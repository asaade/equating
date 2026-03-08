# R/equating_lib/05_data_cleaning.R
# Responsabilidad: Limpieza y validación de datos (Careless Responders & NAs).
# Versión: v1.0
# Dependencias: 00_common, 00_config_defs.R, dplyr

# -----------------------------------------------------------------------------
# 1. HELPERS INTERNOS
# -----------------------------------------------------------------------------

#' Aplica el filtro de valores omitidos (NAs)
apply_na_filter <- function(df_scored, mat_items, item_cols, th_max_na_pct, keep_mask, reasons) {
  if ("FORMA" %in% names(df_scored)) {
    # LÓGICA MULTI-FORMA (Sparse Matrix Support)
    formas <- unique(df_scored$FORMA)

    for (f in formas) {
      idx_f <- which(df_scored$FORMA == f)
      if (length(idx_f) == 0) next

      # 1. Extraer sub-matriz de usuarios de esta forma
      sub_mat <- mat_items[idx_f, , drop = FALSE]

      # 2. Identificar ítems activos (Diseño): Aquellos que NO son 100% NA en este grupo
      # Esto distingue "No presentado" (Estructural) de "No respondido" (Omisión)
      is_active_col <- colSums(!is.na(sub_mat)) > 0
      n_active_items <- sum(is_active_col)

      if (n_active_items == 0) {
        # Caso borde: Forma vacía o mal codificada. No eliminamos para evitar falsos positivos masivos.
        warn(paste("Forma", f, "no tiene ítems activos detectados. Saltando check NA."))
        next
      }

      # 3. Recalcular conteo de NAs SOLO sobre columnas activas
      # (Ignoramos columnas de otras formas que son NA por diseño)
      sub_mat_active <- sub_mat[, is_active_col, drop = FALSE]
      na_counts_local <- rowSums(is.na(sub_mat_active))

      # 4. Calcular porcentaje real de omisión
      pcts <- na_counts_local / n_active_items

      bad_na <- (pcts > th_max_na_pct)

      if (any(bad_na)) {
        global_idx <- idx_f[bad_na]
        to_mark <- global_idx[keep_mask[global_idx]]
        if (length(to_mark) > 0) {
          keep_mask[to_mark] <- FALSE
          reasons[to_mark] <- "HIGH_NA_PCT"
        }
      }
    }
  } else {
    # LÓGICA DE FORMA ÚNICA (Fallback)
    # Aquí sí asumimos que todos deben responder todo
    na_counts <- rowSums(is.na(mat_items))
    pcts <- na_counts / length(item_cols)
    bad_na_idx <- which(pcts > th_max_na_pct)
    if (length(bad_na_idx) > 0) {
      keep_mask[bad_na_idx] <- FALSE
      reasons[bad_na_idx] <- "HIGH_NA_PCT"
    }
  }

  return(list(keep_mask = keep_mask, reasons = reasons))
}

#' Aplica el filtro de patrones planos (varianza 0)
apply_flat_pattern_filter <- function(mat_items, check_flat, keep_mask, reasons) {
  if (check_flat) {
    # Solo evaluamos a los que sobrevivieron el paso 1
    active_ids <- which(keep_mask)
    if (length(active_ids) > 0) {
      # SD por fila
      row_sds <- apply(mat_items[active_ids, , drop = FALSE], 1, sd, na.rm = TRUE)

      # SD < Epsilon indica varianza 0 (todas las respuestas puntuadas iguales)
      bad_flat <- which(row_sds < 1e-9)

      if (length(bad_flat) > 0) {
        global_drop_idx <- active_ids[bad_flat]
        keep_mask[global_drop_idx] <- FALSE
        reasons[global_drop_idx] <- "ZERO_VARIANCE_SCORE"
      }
    }
  }

  return(list(keep_mask = keep_mask, reasons = reasons))
}

#' Calcula la longitud máxima de respuestas consecutivas idénticas
calculate_long_string <- function(row_vec) {
  # Convertir a caracter y eliminar NAs para ver la secuencia real respondida
  vec <- as.character(row_vec[!is.na(row_vec)])
  if (length(vec) == 0) {
    return(0)
  }

  # RLE (Run Length Encoding) para encontrar secuencias
  rle_res <- rle(vec)
  if (length(rle_res$lengths) == 0) {
    return(0)
  }

  return(max(rle_res$lengths))
}

#' Aplica el filtro de careless responders (long strings)
apply_careless_filter <- function(raw_dat, df_scored, item_cols, th_max_invariant, keep_mask, reasons) {
  if (!is.null(raw_dat) && nrow(raw_dat) == nrow(df_scored)) {
    active_ids <- which(keep_mask)
    if (length(active_ids) > 0) {
      raw_item_cols <- intersect(names(raw_dat), item_cols)

      if (length(raw_item_cols) > 5) {
        raw_mat <- as.matrix(raw_dat[active_ids, raw_item_cols, drop = FALSE])
        long_strings <- apply(raw_mat, 1, calculate_long_string)

        bad_pattern <- (long_strings > th_max_invariant)

        if (any(bad_pattern)) {
          global_drop_idx <- active_ids[bad_pattern]
          keep_mask[global_drop_idx] <- FALSE
          reasons[global_drop_idx] <- paste0("INVARIANT_PATTERN(>", th_max_invariant, ")")
        }
      }
    }
  }

  return(list(keep_mask = keep_mask, reasons = reasons))
}

# -----------------------------------------------------------------------------
# 2. FUNCIÓN PRINCIPAL (Entry Point para Orquestador)
# -----------------------------------------------------------------------------

clean_response_data <- function(df_scored, raw_dat, config_cleaning) {
  # Validación de entrada crítica
  if (is.null(df_scored) || nrow(df_scored) == 0) {
    error("Dataframe de puntuaciones vacío.")
    stop("ERROR CRÍTICO: Dataframe de puntuaciones vacío o nulo.")
  }

  n_start <- nrow(df_scored)

  # Inicialización de máscaras y contadores
  keep_mask <- rep(TRUE, n_start)
  reasons <- rep(NA_character_, n_start)

  # Configuración (Prioridad: Config local > Constantes Globales)
  th_max_na_pct <- if (!is.null(config_cleaning$max_na_pct)) config_cleaning$max_na_pct else CONST_CLEAN_MAX_NA_PCT
  check_flat <- if (!is.null(config_cleaning$remove_flat)) config_cleaning$remove_flat else CONST_CLEAN_FLAT_PATTERNS

  # Umbral para Long String (Careless Responders).
  th_max_invariant <- if (!is.null(config_cleaning$max_invariant)) config_cleaning$max_invariant else 10

  # Identificación de Columnas de Ítems (Numéricas y no metadatos)
  meta_cols <- c("ID", "PERSON_ID", "FORMA", "SCORE", "TOTAL", "CASE", "CLUSTER")
  item_cols <- setdiff(names(df_scored)[sapply(df_scored, is.numeric)], meta_cols)

  if (length(item_cols) == 0) {
    warn("No se detectaron columnas de ítems numéricos. Saltando limpieza.")
    return(list(df_clean = df_scored, stats = list(n_start = n_start, n_end = n_start, n_removed = 0, details = NULL)))
  }

  mat_items <- as.matrix(df_scored[, item_cols, drop = FALSE])

  # -------------------------------------------------------
  # PASO 1: FILTRO DE NAs (OMISIONES EXCESIVAS)
  # -------------------------------------------------------
  na_res <- apply_na_filter(df_scored, mat_items, item_cols, th_max_na_pct, keep_mask, reasons)
  keep_mask <- na_res$keep_mask
  reasons <- na_res$reasons

  # -------------------------------------------------------
  # PASO 2: PATRONES PLANOS (VARIANZA 0)
  # -------------------------------------------------------
  flat_res <- apply_flat_pattern_filter(mat_items, check_flat, keep_mask, reasons)
  keep_mask <- flat_res$keep_mask
  reasons <- flat_res$reasons

  # -------------------------------------------------------
  # PASO 3: CARELESS RESPONDERS (LONG STRINGS en RAW)
  # -------------------------------------------------------
  careless_res <- apply_careless_filter(raw_dat, df_scored, item_cols, th_max_invariant, keep_mask, reasons)
  keep_mask <- careless_res$keep_mask
  reasons <- careless_res$reasons

  # -------------------------------------------------------
  # REPORTE Y SALIDA
  # -------------------------------------------------------
  df_clean <- df_scored[keep_mask, , drop = FALSE]
  n_end <- nrow(df_clean)
  n_del <- n_start - n_end

  causes_table <- table(reasons[!keep_mask])
  causes_list <- as.list(causes_table)

  # Logging
  if (n_del > 0) {
    detail_msg <- paste(names(causes_list), as.numeric(causes_list), sep = ":", collapse = ", ")
    msg <- sprintf(
      "Limpieza de Examinados: Inicio=%d, Fin=%d, Eliminados=%d (%.2f%%) [Causas: %s]",
      n_start, n_end, n_del, (n_del / n_start) * 100, detail_msg
    )
    debug(msg)
  } else {
    debug("Limpieza de Examinados: No se detectaron patrones aberrantes.")
  }

  # Advertencia de seguridad estadística
  if (n_end < CONST_CRITICAL_MIN_N) {
    warn(sprintf("ALERTA: N residual (%d) bajo el mínimo crítico (%d).", n_end, CONST_CRITICAL_MIN_N))
  }

  return(list(
    df_clean = df_clean,
    stats = list(n_start = n_start, n_end = n_end, n_removed = n_del, details = causes_list)
  ))
}
