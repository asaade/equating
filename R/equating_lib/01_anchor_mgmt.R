# R/equating_lib/01_anchor_mgmt.R
# Responsabilidad: Gestión de Anclas, Depuración CTT/Robust y Detección de Drift
# Versión: v1.2 - Defensive Programming
# Dependencias: MASS, stats

# -----------------------------------------------------------------------------
# 1. ESTADÍSTICAS BÁSICAS
# -----------------------------------------------------------------------------

get_item_stats <- function(df, items) {
  # Validación estricta: solo columnas que existen
  valid_items <- intersect(items, names(df))
  if (length(valid_items) == 0) {
    return(NULL)
  }

  mat <- as.matrix(df[, valid_items, drop = FALSE])
  mode(mat) <- "numeric"

  if (ncol(mat) == 0 || nrow(mat) == 0) {
    return(NULL)
  }

  # Protección contra columnas 100% NA que romperían apply
  n_valid <- colSums(!is.na(mat))
  has_data <- n_valid > 0

  if (sum(has_data) == 0) {
    return(NULL)
  }

  mat_ok <- mat[, has_data, drop = FALSE]

  list(
    p = colMeans(mat_ok, na.rm = TRUE),
    sd = apply(mat_ok, 2, sd, na.rm = TRUE),
    n = n_valid[has_data]
  )
}

# -----------------------------------------------------------------------------
# 2. REFINAMIENTO DE ANCLAS
# -----------------------------------------------------------------------------

refine_anchor <- function(df_src, df_dest, items, config, strictness = TRUE, min_keep_req = 4) {
  # --- Validación Inicial ---
  available_items <- intersect(items, intersect(names(df_src), names(df_dest)))

  if (length(available_items) < min_keep_req) {
    return(NULL)
  }

  current <- available_items
  th <- config$thresholds

  # Parámetros CTT
  p_min <- if (!is.null(th$ctt_p_val_min)) th$ctt_p_val_min[1] else 0.02
  p_max <- if (!is.null(th$ctt_p_val_max)) th$ctt_p_val_max[1] else 0.98
  sd_min <- 0.15

  audit <- data.frame(
    ITEM = current, STATUS = "KEPT", REASON = "OK",
    P_SRC = NA_real_, P_DEST = NA_real_, Z_SCORE = NA_real_,
    stringsAsFactors = FALSE
  )
  rownames(audit) <- current

  # --- FASE 1: Filtrado CTT ---
  s1 <- get_item_stats(df_src, current)
  s2 <- get_item_stats(df_dest, current)

  # Si get_item_stats devuelve NULL o faltan items en la respuesta, ajustamos 'current'
  if (is.null(s1) || is.null(s2)) {
    return(NULL)
  }

  # Intersectar con lo que realmente devolvió stats (por si hubo NAs totales)
  valid_stats_items <- intersect(names(s1$p), names(s2$p))
  current <- intersect(current, valid_stats_items)

  if (length(current) < min_keep_req) {
    return(NULL)
  }

  tryCatch(
    {
      audit[current, "P_SRC"] <- round(s1$p[current], 4)
      audit[current, "P_DEST"] <- round(s2$p[current], 4)

      dead_idx <- which(
        s1$p[current] < p_min | s1$p[current] > p_max |
          s2$p[current] < p_min | s2$p[current] > p_max |
          s1$sd[current] < sd_min | s2$sd[current] < sd_min
      )

      if (length(dead_idx) > 0) {
        dropped <- current[dead_idx]
        current <- setdiff(current, dropped)
        audit[dropped, c("STATUS", "REASON")] <- c("DROPPED", "CTT_THRESHOLD")
      }
    },
    error = function(e) {
      return(NULL)
    }
  )

  if (length(current) < min_keep_req) {
    return(NULL)
  }

  # --- FASE 2: Drift Robust (Iterativo) ---
  min_n_sample <- min(nrow(df_src), nrow(df_dest))
  use_robust <- (min_n_sample >= 50 && length(current) > 5)
  max_iter <- 3

  clamp_min <- 0.001
  clamp_max <- 0.999

  for (iter in 1:max_iter) {
    if (length(current) < min_keep_req) break

    res_try <- tryCatch(
      {
        p1 <- colMeans(df_src[, current, drop = FALSE], na.rm = TRUE)
        p2 <- colMeans(df_dest[, current, drop = FALSE], na.rm = TRUE)
        l1 <- qlogis(pmax(clamp_min, pmin(clamp_max, p1)))
        l2 <- qlogis(pmax(clamp_min, pmin(clamp_max, p2)))

        fit <- if (use_robust) {
          tryCatch(MASS::rlm(l1 ~ l2, method = "M", maxit = 20), error = function(e) lm(l1 ~ l2))
        } else {
          lm(l1 ~ l2)
        }

        resids <- l1 - predict(fit)

        # Escala
        mad_const <- 1.4826
        scale_val <- mad(resids, constant = mad_const)
        if (scale_val < 1e-4) scale_val <- max(sd(resids), 1e-4)

        z_scores <- abs(resids / scale_val)
        names(z_scores) <- current
        list(z = z_scores, ok = TRUE)
      },
      error = function(e) list(ok = FALSE)
    )

    if (!res_try$ok) break

    z_scores <- res_try$z
    audit[current, "Z_SCORE"] <- round(z_scores, 2)

    # Umbral Z
    z_threshold <- if (strictness) 2.5 else 3.0
    if (length(current) < 8) z_threshold <- z_threshold + 0.5

    bad_idx <- which(z_scores > z_threshold)

    if (length(bad_idx) > 0) {
      drops <- names(sort(z_scores[bad_idx], decreasing = TRUE))

      if ((length(current) - length(drops)) < min_keep_req) {
        n_removable <- length(current) - min_keep_req
        if (n_removable > 0) drops <- drops[1:n_removable] else drops <- character(0)
      }

      if (length(drops) > 0) {
        current <- setdiff(current, drops)
        audit[drops, c("STATUS", "REASON")] <- c("DROPPED", paste0("DRIFT_ITER", iter))
      } else {
        break
      }
    } else {
      break
    }
  }

  # --- SALIDA ---
  if (length(current) < min_keep_req) {
    return(NULL)
  }

  val_cor <- tryCatch(
    {
      as.numeric(cor(colMeans(df_src[, current, drop = FALSE], na.rm = TRUE),
        colMeans(df_dest[, current, drop = FALSE], na.rm = TRUE),
        use = "complete.obs"
      ))[1]
    },
    error = function(e) NA_real_
  )

  list(
    final_anchor = current,
    drift_stats = audit,
    correlation = val_cor,
    drift_summary = list(nature = "ANALYZED")
  )
}
