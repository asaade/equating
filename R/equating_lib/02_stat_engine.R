# R/equating_lib/02_stat_engine.R
# Responsabilidad: Motor de Cálculo y Recálculo (Core Stat Engine)
# Versión: v107.3 - Fix Wiggle Scale Bug & Moment Tolerances
# Dependencias: 00_config_defs.R, 01_anchor_mgmt.R, 07_model_selector.R, 08_quality_control.R

source("R/equating_lib/01_anchor_mgmt.R")
source("R/equating_lib/07_model_selector.R")
source("R/equating_lib/08_quality_control.R")

if (!exists("%||%")) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
}

# =============================================================================
# 0. CONFIGURACIÓN
# =============================================================================

normalize_config <- function(cfg) {
  if (missing(cfg) || is.null(cfg) || !is.list(cfg)) cfg <- list()
  if (is.null(cfg$limits)) cfg$limits <- list()
  if (is.null(cfg$equating)) cfg$equating <- list()
  if (is.null(cfg$smoothing)) cfg$smoothing <- list()
  if (is.null(cfg$anchor)) cfg$anchor <- list()

  if (is.null(cfg$limits$critical_min_n)) cfg$limits$critical_min_n <- 30
  if (is.null(cfg$equating$min_anchor_items)) cfg$equating$min_anchor_items <- 10

  if (is.null(cfg$smoothing$wiggle_density_max)) cfg$smoothing$wiggle_density_max <- 0.15
  if (is.null(cfg$smoothing$bias_mean_max)) cfg$smoothing$bias_mean_max <- 0.05
  if (is.null(cfg$smoothing$bias_sd_max)) cfg$smoothing$bias_sd_max <- 0.10
  if (is.null(cfg$smoothing$bias_skew_max)) cfg$smoothing$bias_skew_max <- 0.20
  if (is.null(cfg$smoothing$bias_kurt_max)) cfg$smoothing$bias_kurt_max <- 0.40
  if (is.null(cfg$smoothing$cv_folds)) cfg$smoothing$cv_folds <- 5

  return(cfg)
}

# =============================================================================
# 1. HELPERS DE BAJO NIVEL
# =============================================================================

extract_passport_stats <- function(ft) {
  if (is.null(ft)) {
    return(NULL)
  }

  # Obtenemos el resumen estadístico nativo de equate
  summ_obj <- tryCatch(summary(ft), error = function(e) NULL)
  if (is.null(summ_obj)) {
    return(NULL)
  }

  res <- list()
  if ("total" %in% rownames(summ_obj)) res$total <- as.list(summ_obj["total", ])
  if ("anchor" %in% rownames(summ_obj)) res$anchor <- as.list(summ_obj["anchor", ])

  # La dimensión 1 siempre corresponde al puntaje de interés (Total/X).
  dnames <- dimnames(ft)
  scores <- NULL

  if (!is.null(dnames)) {
    # Extraemos la primera dimensión (Total Score)
    scores <- as.numeric(dnames[[1]])
  } else if (!is.null(names(ft))) {
    # Fallback por si acaso llega un vector simple
    scores <- as.numeric(names(ft))
  }

  # Cálculo seguro de min/max ignorando NAs por conversión
  min_val <- if (!is.null(scores)) min(scores, na.rm = TRUE) else NA
  max_val <- if (!is.null(scores)) max(scores, na.rm = TRUE) else NA

  res$meta <- list(
    design = attr(ft, "design"),
    min_score = min_val,
    max_score = max_val
  )

  return(res)
}

# Cálculo de Wiggliness
calculate_wiggliness_local <- function(ft) {
  # Función interna segura
  calc_rmse_d2 <- function(vec) {
    if (length(vec) < 3) {
      return(0)
    }
    # Importante: vec debe ser densidad (sum=1), no conteos
    d2 <- diff(vec, differences = 2)
    sqrt(mean(d2^2, na.rm = TRUE))
  }

  if (length(dim(ft)) == 2) {
    # BIVARIADO:
    # Error previo: rowSums(mat) devolvía conteos (ej. 50, 100), generando wiggle enorme.
    # Corrección: Normalizar a densidad marginal antes de calcular diferencias.
    mat <- as.matrix(ft)

    row_counts <- rowSums(mat)
    col_counts <- colSums(mat)

    # Evitar división por cero
    row_dens <- if (sum(row_counts) > 0) row_counts / sum(row_counts) else row_counts
    col_dens <- if (sum(col_counts) > 0) col_counts / sum(col_counts) else col_counts

    w_rows <- calc_rmse_d2(row_dens)
    w_cols <- calc_rmse_d2(col_dens)
    return(max(w_rows, w_cols))
  } else {
    # UNIVARIADO:
    vals <- as.numeric(ft)
    dens <- if (sum(vals) > 0) vals / sum(vals) else vals
    return(calc_rmse_d2(dens))
  }
}

clean_missing_data <- function(df, items, config) {
  items <- unique(items)
  if (!all(items %in% names(df))) {
    missing_cols <- setdiff(items, names(df))
    stop(paste("ERROR CRÍTICO: Items no encontrados:", paste(head(missing_cols, 3), collapse = ", ")))
  }
  df_subset <- df[, items, drop = FALSE]
  if (all(complete.cases(df_subset))) {
    return(df)
  }

  df_clean <- df[complete.cases(df_subset), , drop = FALSE]
  min_n <- config$limits$critical_min_n
  if (nrow(df_clean) < min_n) stop(sprintf("ERROR CRÍTICO: N insuficiente (%d < %d).", nrow(df_clean), min_n))
  return(df_clean)
}

check_moment_preservation_native <- function(raw_ft, smooth_ft, config = NULL) {
  s_raw <- summary(raw_ft)
  s_sm <- summary(smooth_ft)

  # Extracción segura manejando formatos univariados/bivariados de equate
  get_stat <- function(summ, stat_name) {
    if ("total" %in% rownames(summ)) summ["total", stat_name] else summ[1, stat_name]
  }

  raw_m <- get_stat(s_raw, "mean")
  raw_sd <- get_stat(s_raw, "sd")
  raw_sk <- get_stat(s_raw, "skew")
  raw_kt <- get_stat(s_raw, "kurt")

  sm_m <- get_stat(s_sm, "mean")
  sm_sd <- get_stat(s_sm, "sd")
  sm_sk <- get_stat(s_sm, "skew")
  sm_kt <- get_stat(s_sm, "kurt")

  # Tolerancias
  lim_mean <- config$smoothing$bias_mean_max %||% 0.05
  lim_sd <- config$smoothing$bias_sd_max %||% 0.10
  lim_sk <- config$smoothing$bias_skew_max %||% 0.20
  lim_kt <- config$smoothing$bias_kurt_max %||% 0.40

  bias_mean <- abs(raw_m - sm_m)
  bias_sd <- abs(raw_sd - sm_sd)
  bias_sk <- abs(raw_sk - sm_sk)
  bias_kt <- abs(raw_kt - sm_kt)

  msgs <- c()
  has_bias <- FALSE

  # Comparación con pequeña tolerancia de punto flotante (1e-5) para evitar falsos positivos
  if (bias_mean > (lim_mean + 1e-5)) {
    has_bias <- TRUE
    msgs <- c(msgs, sprintf("MeanDiff(%.3f>%.2f)", bias_mean, lim_mean))
  }
  if (bias_sd > (lim_sd + 1e-5)) {
    has_bias <- TRUE
    msgs <- c(msgs, sprintf("SDDiff(%.3f>%.2f)", bias_sd, lim_sd))
  }
  if (bias_sk > (lim_sk + 1e-5)) {
    has_bias <- TRUE
    msgs <- c(msgs, sprintf("SkewDiff(%.3f>%.2f)", bias_sk, lim_sk))
  }
  if (bias_kt > (lim_kt + 1e-5)) {
    has_bias <- TRUE
    msgs <- c(msgs, sprintf("KurtDiff(%.3f>%.2f)", bias_kt, lim_kt))
  }

  list(
    has_bias = has_bias, passed = !has_bias,
    reason = if (has_bias) paste(msgs, collapse = "|") else "", metrics = list(d_mean = bias_mean, d_sd = bias_sd, d_skew = bias_sk, d_kurt = bias_kt)
  )
}

estimate_sem_reliability <- function(df, items) {
  tryCatch(
    {
      k <- length(items)
      if (k < 2) {
        return(list(rel = 0, sem = 0))
      }
      mat <- as.matrix(df[, items, drop = FALSE])
      p <- colMeans(mat, na.rm = TRUE)
      sigma_sq_x <- var(rowSums(mat, na.rm = TRUE))
      sum_pq <- sum(p * (1 - p))
      rel <- max(0, min(1, (k / (k - 1)) * (1 - (sum_pq / sigma_sq_x))))
      sem <- sqrt(sigma_sq_x * (1 - rel))
      list(rel = rel, sem = sem, sd = sqrt(sigma_sq_x))
    },
    error = function(e) list(rel = NA, sem = NA, sd = NA)
  )
}

# =============================================================================
# 2. MÓDULO DE SUAVIZADO LOG-LINEAL
# =============================================================================

determine_max_degrees <- function(n_total, is_bivariate, config) {
  if (!is.null(config$equating$max_degree_override)) {
    return(config$equating$max_degree_override)
  }
  if (is_bivariate) {
    inter <- if (n_total >= 3000) 3 else if (n_total >= 1000) 2 else 1
    uni <- if (n_total >= 3000) 6 else if (n_total >= 1000) 4 else 2
    return(list(c(uni, uni - 1), inter))
  } else {
    return(if (n_total >= 3000) 6 else if (n_total >= 1000) 4 else 3)
  }
}

## Lleva a cabo el suavizado de la distribución de puntuaciones
## Utiliza el criterio BIC (revizar: Holland recomienda AIC)
## Solo aplica a modelos no lineales (p. ej. Equipercentil)
## TODOs:
## 1. Confirmar lógica psicométrica de la selección
## 2. Verificar que devuelve información completa para reproducir el proceso
## 3. Revisar que el proceso sea correcto y eficiente (algoritmos, orden de comprobaciones)
## PRECAUCIÓN: Los parámetros de la función equate::presmoothing cambian radicalmente su comportamiento.
##             Se debe ser cuidadoso para no romper la lógica.
optimize_loglinear_smoothing <- function(ft, n_total, config, scale_id = "UNKNOWN", form_tag = "UNKNOWN") {
  is_bivariate <- (length(dim(ft)) == 2)
  max_degrees <- determine_max_degrees(n_total, is_bivariate, config)

  candidate_objects <- tryCatch(
    suppressWarnings(equate::presmoothing(ft, "loglinear",
      degrees = max_degrees,
      stepup = TRUE, compare = FALSE,
      verbose = TRUE, maxit = 2000
    )),
    error = function(e) NULL
  )

  if (is.null(candidate_objects)) {
    return(list(success = FALSE, ft = NULL, info = "FAIL_GLM_GEN"))
  }

  # Normalización de lista de modelos
  model_list <- if (inherits(candidate_objects, "presmoothing") && !is.null(candidate_objects$models)) candidate_objects$models else if (inherits(candidate_objects, "list")) candidate_objects else list(candidate_objects)

  wiggle_tol <- config$smoothing$wiggle_density_max
  #
  history_list <- list()

  for (i in seq_along(model_list)) {
    model <- model_list[[i]]

    if (!inherits(model, "glm") || (!is.null(model$converged) && !model$converged)) {
      history_list[[i]] <- data.frame(
        ModelID = i, Formula = NA, Params_K = NA,
        BIC = Inf, Wiggle = NA, G2 = NA, P_Val = NA,
        Status = "FAIL_CONVERGENCE", stringsAsFactors = FALSE
      )
      next
    }

    fit_vals <- fitted(model)
    fit_tab <- ft
    fit_tab[] <- fit_vals

    g2 <- model$deviance
    k <- model$rank
    df_resid <- model$df.residual
    bic <- g2 + (k * log(n_total))

    # P-Value robustness: Si df_resid es 0 o muy bajo, o g2 es muy bajo, p_val tiende a 1.
    p_val <- if (!is.null(df_resid) && df_resid > 0) (1 - pchisq(g2, df_resid)) else 0
    # ratio G2: útil para ver si el ajuste es atroz, pero con p_val=1.0 suele ser bajo.
    g2_ratio <- if (!is.null(df_resid) && df_resid > 0) g2 / df_resid else 0

    wiggle <- calculate_wiggliness_local(fit_tab)
    mom_check <- check_moment_preservation_native(ft, fit_tab, config)

    status <- "PASS"
    if (is.na(wiggle) || wiggle > wiggle_tol) {
      status <- "REJECT_WIGGLE"
    } else if (!mom_check$passed) {
      status <- paste0("REJECT_MOMENTS:", mom_check$reason)
    } ## Si P-Val es 1.0, NO rechazamos. Solo rechazamos si el ajuste es malo (p-val muy bajo)
    # o si g2_ratio es muy alto.
    else if (g2_ratio > 3.0 && p_val < 0.05) status <- "PASS_WARN_FIT"

    history_list[[i]] <- data.frame(
      ModelID = i, Formula = paste(deparse(formula(model)), collapse = " "),
      Params_K = k, BIC = round(bic, 2), Wiggle = round(wiggle, 5),
      G2 = round(g2, 2), P_Val = round(p_val, 4),
      Status = status, stringsAsFactors = FALSE
    )
  }

  history_df <- do.call(rbind, history_list)
  valid_candidates <- history_df[grepl("PASS", history_df$Status), ]

  if (nrow(valid_candidates) == 0) {
    return(list(success = FALSE, reason = "NO_VALID_SMOOTHING", history = history_df))
  }

  min_bic_val <- min(valid_candidates$BIC)
  tolerance <- config$smoothing$ic_tolerance %||% 2.0
  parsimony_set <- valid_candidates[valid_candidates$BIC <= (min_bic_val + tolerance), ]
  best_row <- parsimony_set[order(parsimony_set$Params_K, parsimony_set$BIC), ][1, ]

  winner_obj <- model_list[[best_row$ModelID]]

  final_ft <- ft
  vals_raw <- fitted(winner_obj)
  norm_factor <- sum(ft) / sum(vals_raw)
  final_ft[] <- vals_raw * norm_factor

  list(
    success = TRUE,
    ft = final_ft,
    info = sprintf("LogLin(K:%d|BIC:%.1f|P:%.3f)", best_row$Params_K, best_row$BIC, best_row$P_Val),
    metrics = list(k = best_row$Params_K, bic = best_row$BIC, wiggle = best_row$Wiggle, p_val = best_row$P_Val),
    model_meta = list(coefficients = coef(winner_obj), formula = best_row$Formula, model_id = best_row$ModelID),
    history = history_df
  )
}

# =============================================================================
# 3. GENERADOR DE FALLBACK
# =============================================================================

generate_identity_object <- function(items_dest, reason) {
  len <- length(items_dest)
  scale_pts <- 0:len
  conc <- data.frame(scale = scale_pts, yx = scale_pts)

  list(
    status = "IDENTITY_APPLIED",
    concordance = conc,
    se = rep(0, length(scale_pts)),
    tre = rep(0, length(scale_pts)),
    meta = list(method = "IDENTITY", n_anc = 0, smoothing = "NONE", decision_reason = reason, bias_warning = TRUE),
    audit = list(
      traffic_light = list(color = "RED", messages = paste("CRITICAL FALLBACK:", reason)),
      provenance = list(generated_at = Sys.time())
    )
  )
}

# =============================================================================
# 4. PIPELINE PRINCIPAL
# =============================================================================

compute_freq_tables <- function(df_src, df_dest, items_src, items_dest, final_anc) {
  scale_src_tot <- 0:length(items_src)
  scale_dest_tot <- 0:length(items_dest)
  scale_anc <- 0:length(final_anc)

  src_s <- data.frame(total = rowSums(df_src[, items_src]), anchor = rowSums(df_src[, final_anc]))
  dest_s <- data.frame(total = rowSums(df_dest[, items_dest]), anchor = rowSums(df_dest[, final_anc]))

  list(
    src_scores = src_s, dest_scores = dest_s,
    ft_src = equate::freqtab(src_s, scales = list(scale_src_tot, scale_anc)),
    ft_dest = equate::freqtab(dest_s, scales = list(scale_dest_tot, scale_anc)),
    n_tot = min(nrow(src_s), nrow(dest_s))
  )
}

step_1_prep_data <- function(df_src, df_dest, items_src, items_dest, min_anc, config) {
  common <- intersect(items_src, items_dest)
  if (length(common) < min_anc) {
    return(list(error = TRUE, reason = "INSUFFICIENT_ANCHOR"))
  }

  df_src_clean <- tryCatch(clean_missing_data(df_src, items_src, config), error = function(e) NULL)
  df_dest_clean <- tryCatch(clean_missing_data(df_dest, items_dest, config), error = function(e) NULL)

  if (is.null(df_src_clean) || is.null(df_dest_clean)) {
    return(list(error = TRUE, reason = "DATA_CLEANING_FAIL"))
  }

  sem_src <- estimate_sem_reliability(df_src_clean, items_src)
  sem_dest <- estimate_sem_reliability(df_dest_clean, items_dest)

  refine <- refine_anchor(df_src_clean, df_dest_clean, common, config = config, strictness = TRUE, min_keep_req = min_anc)
  if (is.null(refine)) {
    return(list(error = TRUE, reason = "ANCHOR_REFINEMENT_FAIL"))
  }

  if (!is.na(refine$correlation) && refine$correlation < (config$anchor$min_cor %||% 0.70)) {
    return(list(error = TRUE, reason = sprintf("LOW_CORRELATION_%.2f", refine$correlation)))
  }

  stats_raw <- compute_freq_tables(df_src_clean, df_dest_clean, items_src, items_dest, refine$final_anchor)

  list(
    error = FALSE, prep = list(df_src = df_src_clean, df_dest = df_dest_clean),
    refine = refine, stats_raw = stats_raw, sem_stats = list(src = sem_src, dest = sem_dest)
  )
}

## Confirmar que se acumula y devuelve la información suficiente para describir el resultdo y
## hacer el proceso reproducible
step_2_smoothing <- function(stats_raw, config, f_src_code, f_dest_code, link_tag, scale_id) {
  smooth_src <- optimize_loglinear_smoothing(stats_raw$ft_src, stats_raw$n_tot, config, scale_id, f_src_code)
  smooth_dest <- optimize_loglinear_smoothing(stats_raw$ft_dest, stats_raw$n_tot, config, scale_id, f_dest_code)

  if (!smooth_src$success || !smooth_dest$success) {
    warn(paste("Smoothing Failed for", link_tag))
    return(NULL)
  }

  bias_src <- check_moment_preservation_native(stats_raw$ft_src, smooth_src$ft, config)
  bias_dest <- check_moment_preservation_native(stats_raw$ft_dest, smooth_dest$ft, config)

  list(
    src = smooth_src, dest = smooth_dest,
    has_bias = (bias_src$has_bias || bias_dest$has_bias),
    bias_details = list(src = bias_src$metrics, dest = bias_dest$metrics)
  )
}

## Confirmar que se acumula y devuelve la información suficiente para describir el resultado y
## hacer el proceso reproducible.
step_3_candidates <- function(stats_raw, smooth_res, config, force_linear = FALSE) {
  cands_raw <- run_candidate_models(stats_raw$ft_src, stats_raw$ft_dest, config, boot_reps = 0, c("TUCKER", "LEVINE"))
  cands_smooth <- list()
  if (!force_linear && !is.null(smooth_res)) {
    cands_circ <- run_candidate_models(stats_raw$ft_src, stats_raw$ft_dest, config, boot_reps = 0, c("CIRCLEARC"))
    cands_equi <- run_candidate_models(smooth_res$src$ft, smooth_res$dest$ft, config, boot_reps = 0, c("EQUIPERCENTILE"))
    cands_smooth <- c(cands_circ, cands_equi)
  }
  Filter(is.list, c(cands_raw, cands_smooth))
}

step_4_finalize <- function(decision, stats_raw, smooth_res, refine, items_src, items_dest, prep, config, env_cache, cache_key) {
  eq_obj <- decision$selected_obj
  selected_name <- decision$selected_name

  if (is.null(eq_obj)) {
    return(NULL)
  }

  # -------------------------------------------------------------------------
  # A. BOOTSTRAP (Cálculo del Error Estándar - SE)
  # -------------------------------------------------------------------------
  is_nonlinear <- selected_name %in% c("EQUIPERCENTILE", "CIRCLEARC", "SYNTHETIC")
  boot_reps_full <- config$equating$boot_reps %||% 300

  if (is_nonlinear && selected_name != "SYNTHETIC" && boot_reps_full > 30) {
    debug(sprintf("[Finalize] Bootstrap %s (%d reps)...", selected_name, boot_reps_full))
    ft_x_use <- if (selected_name == "EQUIPERCENTILE" && !is.null(smooth_res)) smooth_res$src$ft else stats_raw$ft_src
    ft_y_use <- if (selected_name == "EQUIPERCENTILE" && !is.null(smooth_res)) smooth_res$dest$ft else stats_raw$ft_dest

    type <- if (selected_name == "CIRCLEARC") "circle-arc" else "equipercentile"
    method <- if (selected_name == "CIRCLEARC") "levine" else "frequency estimation"

    args_recalc <- list(x = ft_x_use, y = ft_y_use, type = type, method = method, boot = TRUE, reps = boot_reps_full)

    ## Se repite la equiparación para aumentar el número de repeticiones de 'boot' y
    ## mejorar la estimación final
    recalc_obj <- tryCatch(
      {
        do.call(equate::equate, args_recalc)
      },
      error = function(e) eq_obj
    )

    if (!is.null(recalc_obj$concordance)) eq_obj <- recalc_obj
  }

  # -------------------------------------------------------------------------
  # B. ESTANDARIZACIÓN DE CONCORDANCIA
  # -------------------------------------------------------------------------
  theoretical_max <- length(items_dest)
  qc_res <- run_quality_control(eq_obj, max_score = theoretical_max, config = config)
  conc_df <- as.data.frame(if (!is.null(qc_res$final_concordance)) qc_res$final_concordance else eq_obj$concordance)

  if (!all(c("scale", "yx") %in% names(conc_df))) conc_df <- data.frame(scale = 0:theoretical_max, yx = 0:theoretical_max)

  final_se <- extract_se_universal(eq_obj, nrow(conc_df))

  # -------------------------------------------------------------------------
  # C. CÁLCULO DE SESGO (BIAS) Y TRE
  # -------------------------------------------------------------------------
  bias_vec <- rep(0, nrow(conc_df))
  bias_source <- "ASSUMED_ZERO"

  if (!is.null(smooth_res) && selected_name != "EQUIPERCENTILE" && selected_name != "SYNTHETIC") {
    tryCatch(
      {
        ref_obj <- equate::equate(smooth_res$src$ft, smooth_res$dest$ft, type = "equipercentile", method = "frequency estimation", verbose = FALSE)
        # Validación extra de longitudes para evitar el Warning "Fallo calc bias"
        if (!is.null(ref_obj$concordance) && length(ref_obj$concordance$yx) == length(bias_vec)) {
          bias_vec <- conc_df$yx - ref_obj$concordance$yx
          bias_source <- "VS_SMOOTHED_EQUIPERCENTILE"
        }
      },
      error = function(e) {
        # Silencioso o warning suave, ya está inicializado en 0
      }
    )
  }

  final_tre <- sqrt(final_se^2 + bias_vec^2)
  bias_summary_rmsd <- sqrt(mean(bias_vec^2, na.rm = TRUE))

  # -------------------------------------------------------------------------
  # D. PREPARACIÓN DE AUDITORÍA (CORREGIDO)
  # -------------------------------------------------------------------------
  smoothing_audit_list <- NULL

  if (!is.null(smooth_res)) {
    h_src <- smooth_res$src$history
    h_src$Side <- "Source"
    h_dest <- smooth_res$dest$history
    h_dest$Side <- "Target"

    # Esto asegura que la columna exista y tenga la longitud correcta (7 filas de FALSE)
    h_src$Selected <- FALSE
    h_dest$Selected <- FALSE

    # Marcado Seguro en Source
    w_src_id <- smooth_res$src$model_meta$model_id
    # Verificamos que el ID no sea nulo y tenga longitud 1 antes de comparar
    if (!is.null(w_src_id) && length(w_src_id) == 1) {
      h_src$Selected[h_src$ModelID == w_src_id] <- TRUE
    }

    # Marcado Seguro en Target
    w_dest_id <- smooth_res$dest$model_meta$model_id
    if (!is.null(w_dest_id) && length(w_dest_id) == 1) {
      h_dest$Selected[h_dest$ModelID == w_dest_id] <- TRUE
    }

    smoothing_audit_list <- rbind(h_src, h_dest)
  }

  model_params_audit <- if (!is.null(decision$model_params)) decision$model_params else decision$linear_params

  tl_status <- "GREEN"
  if (qc_res$status_flags$global_status != "OK") tl_status <- "YELLOW"
  if (!is.null(smooth_res) && smooth_res$has_bias) tl_status <- "YELLOW"

  # -------------------------------------------------------------------------
  # E. OBJETO FINAL
  # -------------------------------------------------------------------------
  final_res <- list(
    concordance = conc_df[, c("scale", "yx")],
    se = final_se,
    tre = final_tre,
    bias = bias_vec,
    drift = refine$drift_stats,
    meta = list(
      method = selected_name,
      n_anc = length(refine$final_anchor),
      smoothing = if (!is.null(smooth_res)) paste(smooth_res$src$info, smooth_res$dest$info, sep = " // ") else "NONE",
      anchor_cor = refine$correlation,
      bias_rmsd = bias_summary_rmsd,
      bias_source = bias_source,
      max_tre = max(final_tre, na.rm = TRUE)
    ),
    psychometric_passport = list(
      stats_raw_src = extract_passport_stats(stats_raw$ft_src),
      stats_raw_dest = extract_passport_stats(stats_raw$ft_dest),
      stats_smooth_src = if (!is.null(smooth_res)) extract_passport_stats(smooth_res$src$ft) else NULL,
      stats_smooth_dest = if (!is.null(smooth_res)) extract_passport_stats(smooth_res$dest$ft) else NULL
    ),
    audit = list(
      provenance = list(timestamp = Sys.time(), call_params = config),
      input_data = list(ft_src_raw = stats_raw$ft_src, ft_dest_raw = stats_raw$ft_dest),
      model_params = model_params_audit,
      smoothing_history = smoothing_audit_list,
      decision_log = decision$metrics_table,
      traffic_light = list(color = tl_status, flags = qc_res$status_flags$global_status, metrics = qc_res$metrics)
    )
  )

  if (!is.null(env_cache)) assign(cache_key, final_res, envir = env_cache)
  return(final_res)
}

equate_pair_champion <- function(df_src, df_dest, items_src, items_dest, config, is_global, min_anc,
                                 env_cache = NULL, scale_id = "GLOBAL",
                                 allow_identity_fallback = FALSE, force_linear_models = FALSE, logger = NULL) {
  config <- normalize_config(config)
  f_src_code <- as.character(df_src$FORMA[1])
  f_dest_code <- as.character(df_dest$FORMA[1])
  link_tag <- paste(f_src_code, f_dest_code, sep = "->")
  cache_key <- paste(scale_id, f_src_code, "TO", f_dest_code, sep = "_")

  if (!is.null(env_cache) && exists(cache_key, envir = env_cache)) {
    return(get(cache_key, envir = env_cache))
  }

  s1 <- tryCatch(
    step_1_prep_data(df_src, df_dest, items_src, items_dest, min_anc, config),
    error = function(e) list(error = TRUE, reason = e$message)
  )

  if (s1$error) {
    warn(paste("Prep Fail:", s1$reason))
    return(if (allow_identity_fallback) generate_identity_object(items_dest, s1$reason) else NULL)
  }

  s2 <- step_2_smoothing(s1$stats_raw, config, f_src_code, f_dest_code, link_tag, scale_id)

  if (is.null(s2) && !force_linear_models) {
    return(if (allow_identity_fallback) generate_identity_object(items_dest, "SMOOTHING_FAIL") else NULL)
  }

  candidates <- step_3_candidates(s1$stats_raw, s2, config, force_linear = force_linear_models)

  if (length(candidates) == 0) {
    return(if (allow_identity_fallback) generate_identity_object(items_dest, "NO_CANDIDATES") else NULL)
  }

  # Calcular sd_ratio robustamente
  sd_src <- if (!is.null(s1$sem_stats$src$sd) && !is.na(s1$sem_stats$src$sd)) s1$sem_stats$src$sd else NA
  sd_dest <- if (!is.null(s1$sem_stats$dest$sd) && !is.na(s1$sem_stats$dest$sd)) s1$sem_stats$dest$sd else NA
  sd_ratio_val <- if (!is.na(sd_src) && !is.na(sd_dest) && sd_dest > 0) sd_src / sd_dest else 1.0

  decision <- select_best_model(candidates, config,
    sd_ratio = sd_ratio_val,
    anchor_cor = s1$refine$correlation,
    link_tag = link_tag,
    sem_stats = s1$sem_stats,
    n_total = s1$stats_raw$n_tot
  )

  if (is.null(decision)) {
    return(if (allow_identity_fallback) generate_identity_object(items_dest, "SELECTION_FAIL") else NULL)
  }

  final_obj <- step_4_finalize(decision, s1$stats_raw, s2, s1$refine, items_src, items_dest, s1$prep, config, env_cache, cache_key)
  final_obj$status <- "SUCCESS"

  return(final_obj)
}
