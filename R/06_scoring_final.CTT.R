# R/06_scoring_final.R
# Responsabilidad: Asignación de Calificaciones Finales (Scaling & Cutoffs).
# Dependencias: 00_common.R
# Input: df_scored (Respuestas) + conversion_table (Output del Motor CTT)
# Versión: v1.0

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, readr, stats, checkmate)

# ==============================================================================
# 1. UTILIDADES GENERALES
# ==============================================================================

rowSums_robust <- function(x) {
  if (is.null(dim(x))) {
    return(x)
  }
  sums <- rowSums(x, na.rm = TRUE)
  all_na <- rowSums(!is.na(x)) == 0
  sums[all_na] <- NA
  sums
}

# Construye mapa de traducción desde config$forms (ej. "01" -> "FORMA_01")
get_form_code_map <- function(config) {
  if (is.null(config$forms)) {
    warn("SCORING: config$forms es NULL.")
    return(NULL)
  }

  map_list <- lapply(names(config$forms), function(form_name) {
    codes_raw <- config$forms[[form_name]]$codes
    codes <- if (is.list(codes_raw)) unlist(codes_raw) else codes_raw

    if (!is.null(codes) && length(codes) > 0) {
      codes_clean <- trimws(as.character(codes))
      return(structure(rep(form_name, length(codes_clean)), names = codes_clean))
    }
    NULL
  })

  unlist(map_list)
}

get_scaling_params <- function(config) {
  params <- list(slope = 1, intercept = 0)
  if (!is.null(config$scoring$parameters)) {
    p <- config$scoring$parameters
    if (!is.null(p$slope) && !is.null(p$intercept)) {
      slope <- as.numeric(p$slope)
      intercept <- as.numeric(p$intercept)
    }
  }
  params
}

# ==============================================================================
# 2. LÓGICA DE ESCALAMIENTO Y NIVELES
# ==============================================================================

calculate_scaled_score <- function(equated_scores, config) {
  if (is.null(equated_scores)) {
    return(NULL)
  }

  params <- get_scaling_params(config)
  # Transformación Lineal: Y = mX + b
  scaled <- (equated_scores * slope) + intercept

  decimals <- config$scoring$decimals %||% 0
  scaled <- round(scaled, decimals)

  # Clipping (Topes Min/Max)
  if (!is.null(config$scoring$min_score)) scaled <- pmax(scaled, config$scoring$min_score)
  if (!is.null(config$scoring$max_score)) scaled <- pmin(scaled, config$scoring$max_score)

  scaled
}

assign_performance_levels <- function(scores, config) {
  # Validaciones iniciales
  if (is.null(config$scoring) || is.null(config$scoring$performance_levels)) {
    return(rep("N/A", length(scores)))
  }

  levels_cfg <- config$scoring$performance_levels

  # Parseo seguro a Dataframe
  levels_df <- tryCatch(
    {
      # Filtrar solo elementos válidos (deben tener min_score y label)
      valid_levels <- Filter(function(x) {
        !is.null(x$min_score) && !is.null(x$label)
      }, levels_cfg)

      if (length(valid_levels) == 0) stop("No se encontraron niveles válidos (min_score + label).")

      df <- do.call(rbind, lapply(valid_levels, as.data.frame))

      # Asegurar tipos y existencia de columnas
      df$min_score <- suppressWarnings(as.numeric(df$min_score))
      df$label <- as.character(df$label)

      if (any(is.na(df$min_score))) stop("Valores no numéricos en 'min_score'.")

      # Ordenar rigurosamente por puntaje mínimo ascendente
      df[order(df$min_score), ]
    },
    error = function(e) {
      warn(paste("SCORING CONFIG ERROR:", e$message))
      return(NULL)
    }
  )

  if (is.null(levels_df) || nrow(levels_df) == 0) {
    return(rep("N/A", length(scores)))
  }

  # Definición de Cortes (Breaks) para cut()
  # Ejemplo: Min Scores = 0, 1000, 1150 -> Breaks = c(0, 1000, 1150, Inf)
  breaks <- c(levels_df$min_score, Inf)
  labels <- levels_df$label

  # Asignación Vectorizada
  # right = FALSE asegura intervalos [a, b) -> Incluye min, excluye max del siguiente
  # include.lowest = TRUE asegura que el mínimo absoluto esté incluido
  levels_factor <- cut(scores, breaks = breaks, labels = labels, right = FALSE, include.lowest = TRUE)

  # Convertir factor a caracter y manejar NAs (puntajes menores al mínimo configurado)
  levels_char <- as.character(levels_factor)
  levels_char[is.na(levels_char)] <- "N/A"

  levels_char
}

# ==============================================================================
# 3. LOOKUP DE EQUIPARACIÓN
# ==============================================================================

lookup_equated_score <- function(ids, raw_scores, forms_canonical, eq_tables, scale_id) {
  if (is.null(raw_scores) || all(is.na(raw_scores))) {
    return(rep(NA, length(ids)))
  }

  scale_tbl <- eq_tables |>
    dplyr::filter(SCALE_ID == scale_id)

  if (nrow(scale_tbl) == 0) {
    if (scale_id == "GLOBAL") warn(paste("SCORING: No se encontró tabla GLOBAL."))
    return(raw_scores)
  }

  input_df <- data.frame(
    ID_TEMP = ids,
    SOURCE_FORM = trimws(as.character(forms_canonical)),
    RAW_lookup = as.integer(round(raw_scores)),
    stringsAsFactors = FALSE
  )

  merged <- dplyr::left_join(
    input_df,
    scale_tbl |>
      dplyr::select(SOURCE_FORM, RAW_SCORE, EQUATED_ROUNDED, SEE) |>
      dplyr::mutate(
        SOURCE_FORM = trimws(as.character(SOURCE_FORM)),
        RAW_SCORE = as.integer(RAW_SCORE)
      ),
    by = c("SOURCE_FORM" = "SOURCE_FORM", "RAW_lookup" = "RAW_SCORE")
  )

  list(
    eq_score = round(merged$EQUATED_ROUNDED),
    see = merged$SEE
  )
}

# ==============================================================================
# 4. SUBSCORES
# ==============================================================================

calculate_subscores <- function(item_scored_df, metadata, eq_tables, config, canonical_forms) {
  if (!isTRUE(config$scoring$include_subscores)) {
    return(NULL)
  }

  names(metadata) <- toupper(names(metadata))
  group_col <- intersect(names(metadata), c("SUBTEST", "SUBAREA", "AREA", "DOMAIN"))[1]
  item_col <- intersect(names(metadata), c("ITEM", "ITEM_ID", "REACTIVO"))[1]

  if (is.na(group_col) || is.na(item_col)) {
    return(NULL)
  }

  subareas <- unique(na.omit(metadata[[group_col]]))
  subareas <- subareas[subareas != ""]

  subscore_df <- data.frame(ID = as.character(item_scored_df$ID), stringsAsFactors = FALSE)

  for (area in subareas) {
    items_area <- as.character(metadata[[item_col]][metadata[[group_col]] == area])
    items_valid <- intersect(trimws(items_area), names(item_scored_df))

    if (length(items_valid) < 1) next

    safe_name <- toupper(gsub("[^[:alnum:]]", "", area))
    raw_vals <- rowSums_robust(item_scored_df[, items_valid, drop = FALSE])
    subscore_df[[paste0("Raw_", safe_name)]] <- raw_vals
  }
  subscore_df
}

# ==============================================================================
# 5. ENTRY POINT PRINCIPAL
# ==============================================================================

apply_final_scoring <- function(item_scored_df, raw_data_obj, conversion_table, config) {
  # debug("Iniciando Módulo de Calificación Final...")

  # --- 0. Extracción de Tablas ---
  eq_tables <- NULL
  if (is.list(conversion_table) && "tables" %in% names(conversion_table)) {
    eq_tables <- conversion_table$tables
  } else if (is.data.frame(conversion_table)) {
    eq_tables <- conversion_table
  }

  if (is.null(eq_tables)) {
    error("SCORING ERROR: Tablas de equiparación vacías.")
    stop("Critical Scoring Error")
  }


  # --- 1. Mapeo de Formas ---
  # Traducir códigos raw (ej. "01") a canónicos (ej. "FORMA_01")
  code_map <- get_form_code_map(config)
  raw_forms <- trimws(as.character(item_scored_df$FORMA))

  canonical_forms <- raw_forms
  if (!is.null(code_map)) {
    mapped <- code_map[raw_forms]
    canonical_forms <- ifelse(!is.na(mapped), mapped, raw_forms)
  }

  # --- A. Estructura Base ---
  output_df <- data.frame(
    ID = as.character(item_scored_df$ID),
    FORMA = as.character(item_scored_df$FORMA),
    stringsAsFactors = FALSE
  )

  # --- B. Cálculo Raw Global ---
  items_global <- raw_data_obj$items_global
  if (is.null(items_global)) {
    numeric_cols <- names(item_scored_df)[sapply(item_scored_df, is.numeric)]
    items_global <- setdiff(numeric_cols, c("ID", "FORMA", "RAW_TOTAL"))
  }

  raw_global <- rowSums_robust(item_scored_df[, items_global, drop = FALSE])
  output_df$Raw_Global_CTT <- raw_global

  # --- C. Equiparación ---
  lookup_res <- lookup_equated_score(
    output_df$ID,
    output_df$Raw_Global_CTT,
    canonical_forms,
    eq_tables,
    "GLOBAL"
  )

  output_df$Eq_Global_CTT <- lookup_res$eq_score
  output_df$SEE_Global <- lookup_res$see

  if (sum(is.na(output_df$Eq_Global_CTT)) == nrow(output_df)) {
    warn("ALERTA: Todos los puntajes equiparados son NA. Verifique Mapeo de Formas.")
  }

  # --- D. Escalamiento & Niveles ---
  # 1. Escalamiento Lineal (Para reporte numérico final)
  output_df$Score_Final <- calculate_scaled_score(output_df$Eq_Global_CTT, config)

  # 2. Asignación de Niveles (Renombrada)
  # Basado en Eq_Global_CTT / Métrica Referencia
  output_df$Nivel <- assign_performance_levels(output_df$Eq_Global_CTT, config)

  # --- E. Subscores ---
  if (isTRUE(config$scoring$include_subscores)) {
    sub_df <- calculate_subscores(item_scored_df, raw_data_obj$meta, eq_tables, config, canonical_forms)
    if (!is.null(sub_df)) {
      output_df <- dplyr::left_join(output_df, sub_df, by = "ID")
    }
  }

  n_scored <- nrow(output_df)
  avg_score <- mean(output_df$Score_Final, na.rm = TRUE)
  # debug(sprintf("✅ Calificación completada. N=%d, Media=%.2f", n_scored, avg_score))

  output_df
}
