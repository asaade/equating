# R/03_ctt_analysis.R
# Responsabilidad: Análisis Psicométrico CTT y Distractores (Metadata & Config Driven)
# Garantía: Alineación exacta por Forma e Ítem usando el Design Map.
# Versión: v1.0

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, checkmate, lavaan, psych, stats, stringr, matrixcalc)

# ==============================================================================
# 0. UTILIDADES DE NORMALIZACIÓN (CONFIG DRIVEN)
# ==============================================================================

normalize_item_columns <- function(df, metadata, config) {
  # Objetivo: Renombrar columnas posicionales (P_1...) a ITEM_IDs (IT_001...)
  # usando estrictamente la definición de config.yaml y metadata.csv.

  df_cols <- names(df)
  meta_ids <- unique(na.omit(metadata$ITEM_ID))

  intersection <- intersect(df_cols, meta_ids)
  if (length(intersection) > length(meta_ids) * 0.1) {
    return(list(df = df, items = intersection))
  }

  # 2. Obtener prefijo (P_)
  pfx <- config$specs$data_structure$item_prefix
  if (is.null(pfx)) pfx <- "P_"

  # 3. Construir Mapa de Traducción Específico para este Dataset
  # (Cruza: Forma en DF + Columna en Metadata -> Item ID)
  long_map_list <- list()

  for (f_name in names(config$forms)) {
    f_cfg <- config$forms[[f_name]]
    col_meta <- f_cfg$csv_col # Ej: POS_1

    # Validar que la columna exista en metadata
    if (!col_meta %in% names(metadata)) next

    # Extraer mapa: Posición -> ItemID
    sub_m <- metadata[!is.na(metadata[[col_meta]]), c("ITEM_ID", col_meta)]

    if (nrow(sub_m) > 0) {
      # Generar entrada para cada código de forma (ej: "01", "1")
      for (code in f_cfg$codes) {
        long_map_list[[paste(f_name, code, sep = "_")]] <- data.frame(
          FORMA = as.character(code),
          # Nombre esperado en el DF: Prefijo + Posición (Ej: P_1)
          COL_NAME = paste0(pfx, sub_m[[col_meta]]),
          ITEM_ID = sub_m$ITEM_ID,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  map_lookup <- do.call(rbind, long_map_list)
  if (is.null(map_lookup) || nrow(map_lookup) == 0) {
    warn("   [CTT] No se pudo construir mapa. Verifique nombres de col en metadata vs config.")
    return(list(df = df, items = character(0)))
  }

  # 4. Aplicar Normalización (Split por Forma)
  df_forms <- unique(as.character(df$FORMA))
  df_list <- list()

  # Identificar columnas que son ítems para no perderlas
  pos_pattern <- paste0("^", pfx, "[0-9]+$")
  potential_item_cols <- grep(pos_pattern, df_cols, value = TRUE)

  for (f in df_forms) {
    sub_df <- df |>
      dplyr::filter(as.character(FORMA) == f)
    if (nrow(sub_df) == 0) next

    sub_map <- map_lookup |> dplyr::filter(FORMA == f)
    if (nrow(sub_map) == 0) next

    # Columnas base (ID, Score, etc)
    base_cols <- setdiff(names(sub_df), potential_item_cols)
    new_sub_df <- sub_df[, base_cols, drop = FALSE]

    # Renombrar: P_X -> ITEM_ID
    # Solo procesamos columnas que existan tanto en el DF como en el Mapa
    valid_cols <- intersect(names(sub_df), sub_map$COL_NAME)

    if (length(valid_cols) == 0) next

    for (vc in valid_cols) {
      real_id <- sub_map$ITEM_ID[sub_map$COL_NAME == vc]
      # Asignación segura 1 a 1
      if (length(real_id) == 1) {
        new_sub_df[[real_id]] <- sub_df[[vc]]
      }
    }
    df_list[[f]] <- new_sub_df
  }

  final_df <- dplyr::bind_rows(df_list)
  final_items <- intersect(names(final_df), meta_ids)

  list(df = final_df, items = final_items)
}

# ==============================================================================
# 1. FUNCIONES ESTADÍSTICAS CTT
# ==============================================================================

calculate_tetrachoric <- function(item_mat) {
  cor_mat <- tryCatch(
    {
      # Se añade corrección de continuidad para celdas con cero
      res <- psych::tetrachoric(item_mat, correct = 0.5)
      res$rho
    },
    error = function(e) {
      # Fallback a Pearson en caso de error crítico
      cor(item_mat, use = "pairwise.complete.obs")
    }
  )

  # Asegurar que la matriz sea definida positiva (suavizado)
  if (!matrixcalc::is.positive.definite(cor_mat)) {
    cor_mat <- psych::cor.smooth(cor_mat)
  }

  cor_mat
}

calculate_reliability_stats <- function(item_mat) {
  if (is.null(item_mat) || ncol(item_mat) < 2) {
    return(list(alpha = NA_real_, sem = NA_real_))
  }

  debug("  > Calculando confiabilidad.")
  # Alpha de Cronbach
  item_vars <- apply(item_mat, 2, var, na.rm = TRUE)
  total_scores <- rowSums(item_mat, na.rm = TRUE)
  total_var <- var(total_scores, na.rm = TRUE)
  k <- ncol(item_mat)

  if (is.na(total_var) || total_var < 1e-9) {
    return(list(alpha = 0, sem = 0))
  }

  alpha <- (k / (k - 1)) * (1 - (sum(item_vars, na.rm = TRUE) / total_var))
  alpha <- max(0, alpha)

  sem <- sqrt(total_var) * sqrt(max(0, 1 - alpha))
  list(alpha = alpha, sem = sem)
}

calculate_item_stats <- function(item_mat) {
  debug("  > Calculando estadísticos de ítems.")
  # P_VAL (Dificultad)
  p_values <- colMeans(item_mat, na.rm = TRUE)

  # P_BIS (Discriminación Punto Biserial)
  total_scores <- rowSums(item_mat, na.rm = TRUE)
  p_bis <- numeric(ncol(item_mat))
  names(p_bis) <- colnames(item_mat)

  for (i in seq_len(ncol(item_mat))) {
    # Correlación Ítem-Total Corregida (excluyendo el ítem)
    rest_score <- total_scores - item_mat[, i]

    # Evitar error si varianza es 0 (todos contestaron bien o mal)
    if (sd(item_mat[, i], na.rm = TRUE) == 0 || sd(rest_score, na.rm = TRUE) == 0) {
      p_bis[i] <- 0
    } else {
      res <- try(cor(item_mat[, i], rest_score, use = "pairwise.complete.obs"), silent = TRUE)
      p_bis[i] <- if (inherits(res, "try-error")) NA else res
    }
  }

  data.frame(
    ITEM = colnames(item_mat),
    P_VAL = round(p_values, 3),
    P_BIS = round(p_bis, 3),
    stringsAsFactors = FALSE
  )
}

#' Diagnóstico de dimensionalidad
#' @param item_mat Matriz de respuestas dicotómicas (0, 1)
#' @param n_iter Número de iteraciones para el Análisis Paralelo
check_dimensionality <- function(item_mat, threshold_ratio = 3, threshold_var = 0.2) {
  debug("  > Calculando dimensionalidad.")

  k <- ncol(item_mat)
  # Validación de suficiencia de datos
  if (ncol(item_mat) < 5 || nrow(item_mat) < 50) {
    return(list(flag = "INSUFFICIENT_DATA", ratio = NA, var_exp_1 = NA))
  }

  # Cálculo de matriz de correlación tetracórica (específica para datos dicotómicos)
  cor_mat <- calculate_tetrachoric(item_mat)

  # Análisis de autovalores
  eigen_res <- eigen(cor_mat, only.values = TRUE)$values
  v1 <- eigen_res[1]
  v2 <- max(eigen_res[2], 1e-9)

  ratio <- v1 / v2

  # Criterio de Kaiser (autovalores > 1)
  kaiser_factors <- sum(eigen_res > 1)

  # 2. Análisis Bi-factor y cálculo de Omega Jerárquico/ECV
  # Se extraen factores de grupo para determinar la estructura bi-factor
  n_factors <- 3 # max(kaiser_factors, 3)
  om_res <- tryCatch(
    {
      psych::omega(item_mat, nfactors = n_factors, poly = TRUE, plot = FALSE)
    },
    error = function(e) {
      return(NULL)
    }
  )

  omega_h <- if (!is.null(om_res)) om_res$omega_h else NA
  ecv <- if (!is.null(om_res)) om_res$ECV else NA

  # 3. Cálculo del Porcentaje de Correlaciones no Contaminadas (PUC)
  puc <- NA
  if (!is.null(om_res)) {
    # Extracción de cargas de la solución Schmid-Leiman para identificar grupos
    # Se filtran las columnas correspondientes a factores de grupo (F1*, F2*, etc.)
    sl_matrix <- om_res$schmid$sl
    group_cols <- grep("F", colnames(sl_matrix))

    if (length(group_cols) > 0) {
      # Asignación de ítems a factores de grupo basada en la carga máxima
      group_loadings <- sl_matrix[, group_cols, drop = FALSE]
      item_assignments <- apply(group_loadings, 1, which.max)
      group_sizes <- as.vector(table(item_assignments))

      # Aplicación de la fórmula de PUC
      total_correlations <- (k * (k - 1)) / 2
      contaminated_correlations <- sum(sapply(group_sizes, function(s) (s * (s - 1)) / 2))
      puc <- (total_correlations - contaminated_correlations) / total_correlations
    }
  }

  # 5. Evaluación de Unidimensionalidad Esencial (Rodriguez et al., 2016)
  # Criterios:
  # - Si PUC > 0.80, la interpretación unidimensional es robusta.
  # - Si PUC < 0.80, se requiere ECV > 0.60 y OmegaH > 0.70.
  is_unidim <- FALSE
  if (!is.na(puc)) {
    if (puc > 0.80) {
      is_unidim <- TRUE
    } else if (!is.na(ecv) && !is.na(omega_h)) {
      if (ecv > 0.60 && omega_h > 0.70) is_unidim <- TRUE
    }
  }

  list(
    status = if (is_unidim) "ESSENTIAL_UNIDIM_OK" else "MULTIDIMENSIONAL_DETECTED",
    suggested_factors = kaiser_factors,
    puc = round(puc, 3),
    omega_hierarchical = round(omega_h, 3),
    explained_common_variance = round(ecv, 3),
    eigen_ratio = round(ratio, 2),
    U = as.list(psych::unidim(item_mat, cor = "tet")$uni)$u
  )
}


# ==============================================================================
# 2. ANÁLISIS DE DISTRACTORES
# ==============================================================================

analyze_distractor_efficiency <- function(raw_df, scored_df, items) {
  debug("  > Calculando eficiencia de los distractores.")

  # raw_df: Dataframe con respuestas normalizadas (ITEM_ID) y valores "A", "B", "C"...
  # scored_df: Dataframe con puntajes normalizados (ITEM_ID) y valores 0, 1.

  valid_items <- intersect(items, names(scored_df))
  valid_items <- intersect(valid_items, names(raw_df))

  if (length(valid_items) < 3) {
    return(NULL)
  }

  # Calcular Score Total para agrupar (Low/Mid/High)
  # Usamos scored_df para sumar
  score_mat <- as.matrix(scored_df[, valid_items, drop = FALSE])
  total_score <- rowSums(score_mat, na.rm = TRUE)

  # Grupos de desempeño (Terciles)
  breaks <- quantile(total_score, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE)

  n_unique <- if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::uniqueN(breaks)
  } else {
    length(unique(breaks))
  }

  if (n_unique < 4) {
    groups <- cut(total_score, 3, labels = c("Low", "Mid", "High"))
  } else {
    groups <- cut(total_score, breaks = breaks, labels = c("Low", "Mid", "High"), include.lowest = TRUE)
  }

  results_list <- list()

  for (itm in valid_items) {
    resp_vec <- raw_df[[itm]] # Vector A, B, C...

    # Validaciones básicas
    valid_mask <- !is.na(resp_vec) & !is.na(groups)
    if (sum(valid_mask) < 10) next

    # Tabla de contingencia (Opción vs Grupo)
    tbl <- table(resp_vec[valid_mask], groups[valid_mask])
    opts <- rownames(tbl)

    # Correlación P-Biserial por Opción (Opción elegida vs Score Total)
    # Vectorized computation replaces inner loop over options
    dummy_mat <- vapply(opts, function(o) as.integer(resp_vec == o), integer(length(resp_vec)), USE.NAMES = FALSE)

    # Asegurar que sea matriz incluso si solo hay 1 fila
    if (!is.matrix(dummy_mat)) {
      dummy_mat <- matrix(dummy_mat, ncol = length(opts))
    }

    res_cor <- suppressWarnings(cor(dummy_mat, total_score, use = "pairwise.complete.obs"))
    rbis_vec <- as.vector(res_cor)

    if (any(is.na(rbis_vec))) {
      for (k in which(is.na(rbis_vec))) {
        if (sd(dummy_mat[, k], na.rm = TRUE) == 0) {
          rbis_vec[k] <- 0
        }
      }
    }

    # Proporciones
    props <- prop.table(tbl, margin = 2) # Por columna (Grupo)
    prop_total <- table(resp_vec[valid_mask]) / sum(valid_mask)

    # Construcción de fila
    df_itm <- data.frame(
      ITEM = itm,
      OPTION = opts,
      PROP = as.numeric(prop_total),
      PROP_LOW = if ("Low" %in% colnames(props)) props[, "Low"] else NA,
      PROP_MID = if ("Mid" %in% colnames(props)) props[, "Mid"] else NA,
      PROP_HIGH = if ("High" %in% colnames(props)) props[, "High"] else NA,
      R_BIS_OPT = round(rbis_vec, 3)
    )

    results_list[[itm]] <- df_itm
  }

  do.call(rbind, results_list)
}

# ==============================================================================
# 3. ORQUESTADOR CTT
# ==============================================================================

run_ctt_analysis <- function(calib_df, data_obj, config) {
  metadata <- data_obj$meta

  # 1. NORMALIZACIÓN DE SCORED DF (Calibración)
  # Traduce P_XX -> ITEM_ID
  norm_calib <- normalize_item_columns(calib_df, metadata, config)

  if (length(norm_calib$items) < 5) {
    error("CTT Abortado: No se identificaron items válidos tras normalización.")
    return(NULL)
  }
  calib_clean <- norm_calib$df
  all_items <- norm_calib$items

  # 2. NORMALIZACIÓN DE RAW DF (Para Distractores)
  # Traduce P_XX -> ITEM_ID en la data cruda (letras)
  norm_raw <- normalize_item_columns(data_obj$raw_dat, metadata, config)
  raw_clean <- norm_raw$df

  # 3. ANÁLISIS ESTADÍSTICO POR FORMA
  stats_list <- list()
  rel_stats <- list()
  dim_flags <- list()
  forms <- unique(as.character(calib_clean$FORMA))

  debug(sprintf("   Analizando %d formas detectadas...", length(forms)))

  for (f in forms) {
    debug(paste(" >> Analizando forma", as.character(f)))

    sub_df <- calib_clean |> dplyr::filter(as.character(FORMA) == f)

    # Identificar ítems activos en esta forma (columnas no vacías)
    active_cols <- all_items[colSums(!is.na(sub_df[, all_items, drop = FALSE])) > 0]

    if (length(active_cols) < 5) {
      warn(sprintf("   Forma %s con insuficientes ítems activos (%d). Saltando.", f, length(active_cols)))
      next
    }

    mat <- as.matrix(sub_df[, active_cols, drop = FALSE])

    code_map <- get_form_code_map(config)

    # Estadísticos Básicos
    st <- calculate_item_stats(mat)
    st$FORMA <- code_map[f]
    stats_list[[f]] <- st

    # Confiabilidad
    rel <- calculate_reliability_stats(mat)
    rel_stats[[f]] <- data.frame(FORMA = code_map[f], N_ITEMS = ncol(mat), ALPHA = round(rel$alpha, 3), SEM = round(rel$sem, 3))


    # Dimensionalidad
    threshold_var <- config$thresholds$variance_explained
    dim <- check_dimensionality(mat, threshold_var = threshold_var)

    dim_flags[[f]] <- data.frame(
      FORMA = code_map[f],
      DIM_FLAG = dim$status,
      RATIO_1_2 = dim$eigen_ratio,
      VAR_EXP_1 = dim$explained_common_variance,
      Kaiser = dim$suggested_factors,
      Omega = dim$omega_hierarchical,
      PUC = dim$puc,
      U = as.list(psych::unidim(mat, cor = "tet")$uni)$u
    )
  }

  master_stats <- do.call(rbind, stats_list)
  master_rel <- do.call(rbind, rel_stats)
  master_dim <- do.call(rbind, dim_flags)

  # 4. ANÁLISIS DE DISTRACTORES
  common_ids <- intersect(calib_clean$ID, raw_clean$ID)

  if (length(common_ids) == 0) {
    warn("CTT: No hay coincidencia de IDs entre Raw y Scored. Omitiendo distractores.")
    distractors <- NULL
  } else {
    # Alinear dataframes por ID
    calib_aligned <- calib_clean |>
      dplyr::filter(ID %in% common_ids) |>
      dplyr::arrange(ID)
    raw_aligned <- raw_clean |>
      dplyr::filter(ID %in% common_ids) |>
      dplyr::arrange(ID)

    debug(sprintf("   Analizando Distractores (%d casos)...", length(common_ids)))

    # Ejecutar análisis pasando dataframes normalizados
    distractors <- analyze_distractor_efficiency(raw_aligned, calib_aligned, all_items)

    if (!is.null(distractors)) {
      distractors <- distractors |> mutate(across(where(is.numeric), ~ round(.x, 3)))
    }
  }


  # 5. INTEGRACIÓN DE CLAVES Y BANDERAS (FLAGS)
  if (!is.null(distractors) && !is.null(metadata)) {
    keys <- metadata |>
      dplyr::select(ITEM_ID, KEY) |>
      dplyr::distinct()

    distractors <- distractors |>
      dplyr::left_join(keys, by = c("ITEM" = "ITEM_ID")) |>
      dplyr::mutate(
        IS_KEY = (toupper(trimws(OPTION)) == toupper(trimws(KEY))),

        # Banderas de Alerta
        FLAG = dplyr::case_when(
          IS_KEY & (R_BIS_OPT < 0.0) ~ "NEG_KEY", # La clave correlaciona negativamente (Grave)
          IS_KEY & (R_BIS_OPT < config$thresholds$ctt_pbis_min) ~ "LOW_KEY", # La clave no discrimina
          !IS_KEY & (R_BIS_OPT > config$threshold$ctt_distractor_max_pbis) ~ "POS_PBIS", # Distractor atrae a los buenos (Confuso)
          !IS_KEY & (PROP > 0.40) ~ "HIGH_DIST", # Distractor muy popular
          !IS_KEY & (PROP <= 0.05) ~ "LOW_DIST",
          TRUE ~ "OK"
        )
      )
  }

  list(
    stats = master_stats,
    reliability = master_rel,
    dimensionality = master_dim,
    distractors = distractors
  )
}
