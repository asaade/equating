# R/06_scoring_final.R
# Responsabilidad: Calificación Final Híbrida (CTT/IRT).
# Lógica: Soporta cálculo de Niveles y Score Final usando Theta (IRT) o Puntos (CTT).
# Versión: v5.0 (Simplificada - Solo Escala GLOBAL)

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

get_form_code_map <- function(config) {
  if (is.null(config$forms)) {
    return(NULL)
  }
  map_vec <- c()
  for (form_name in names(config$forms)) {
    codes_raw <- config$forms[[form_name]]$codes
    codes <- if (is.list(codes_raw)) unlist(codes_raw) else codes_raw
    if (!is.null(codes) && length(codes) > 0) {
      codes_clean <- trimws(as.character(codes))
      temp_map <- structure(rep(form_name, length(codes_clean)), names = codes_clean)
      map_vec <- c(map_vec, temp_map)
    }
  }
  map_vec
}

# ==============================================================================
# 2. SISTEMA DE ESCALAMIENTO Y NIVELES
# ==============================================================================

extract_anchors_from_levels <- function(levels_cfg, max_input_val) {
  if (is.null(levels_cfg)) {
    return(NULL)
  }

  pts_list <- list()
  for (lvl in levels_cfg) {
    # Puntos intermedios (Cortes)
    if (!is.null(lvl$min_score) && !is.null(lvl$scaled_score)) {
      pts_list[[length(pts_list) + 1]] <- data.frame(
        input = as.numeric(lvl$min_score),
        target = as.numeric(lvl$scaled_score)
      )
    }
    # Punto final (Techo de la escala)
    if (!is.null(lvl$max_scaled_score)) {
      # Asumimos que el techo de la escala corresponde al máximo input posible
      # (Max Theta para IRT o Max Raw Score para CTT)
      pts_list[[length(pts_list) + 1]] <- data.frame(
        input = as.numeric(max_input_val),
        target = as.numeric(lvl$max_scaled_score)
      )
    }
  }

  if (length(pts_list) == 0) {
    return(NULL)
  }
  do.call(rbind, pts_list)
}

apply_piecewise_mapping <- function(input_vec, config, source_type = "CTT", max_input_val = NULL) {
  sc_cfg <- config$scoring

  # 1. Intentar obtener anchors explícitos (Legacy)
  df_anchors <- NULL
  if (!is.null(sc_cfg$anchors)) {
    df_anchors <- tryCatch(
      {
        do.call(rbind, lapply(sc_cfg$anchors, as.data.frame))
      },
      error = function(e) NULL
    )
  }

  # 2. Intentar derivarlos de performance_levels (Moderno)
  if (is.null(df_anchors) && !is.null(sc_cfg$performance_levels) && !is.null(max_input_val)) {
    df_anchors <- extract_anchors_from_levels(sc_cfg$performance_levels, max_input_val)
  }

  limit_min <- sc_cfg$min_score %||% -Inf
  limit_max <- sc_cfg$max_score %||% Inf
  decimals <- sc_cfg$decimals %||% 0

  # Estrategia A: Piecewise (Spline Monotónico)
  if (!is.null(df_anchors) && nrow(df_anchors) >= 2) {
    df_anchors <- df_anchors[order(df_anchors$input), ]
    df_anchors <- unique(df_anchors)

    interp_fn <- splinefun(x = df_anchors$input, y = df_anchors$target, method = "monoH.FC")
    scaled_values <- interp_fn(input_vec)

    return(round(pmax(limit_min, pmin(limit_max, scaled_values)), decimals))
  }

  # Estrategia B: Lineal (Fallback)
  val <- input_vec
  if (source_type == "IRT") {
    tgt_mean <- sc_cfg$target_mean %||% 0
    tgt_sd <- sc_cfg$target_sd %||% 1
    val <- (val * tgt_sd) + tgt_mean
  } else {
    slope <- sc_cfg$parameters$slope %||% 1
    intercept <- sc_cfg$parameters$intercept %||% 0
    val <- (val * slope) + intercept
  }

  round(pmax(limit_min, pmin(limit_max, val)), decimals)
}

assign_performance_levels <- function(input_scores, config) {
  # NOTA CRÍTICA: input_scores debe estar en la misma métrica que 'min_score' en el config.
  # Si Source=IRT, min_score debe ser Theta (ej. -1.5).
  # Si Source=CTT, min_score debe ser Puntos (ej. 20).

  if (is.null(config$scoring$performance_levels)) {
    return(rep("N/A", length(input_scores)))
  }

  levels_df <- tryCatch(
    {
      clean_list <- lapply(config$scoring$performance_levels, function(x) {
        if (!is.null(x$min_score) && !is.null(x$label)) {
          return(as.data.frame(x[c("min_score", "label")]))
        }
        return(NULL)
      })
      df <- do.call(rbind, clean_list)
      df$min_score <- as.numeric(df$min_score)
      df[order(df$min_score), ]
    },
    error = function(e) NULL
  )

  if (is.null(levels_df) || nrow(levels_df) == 0) {
    return(rep("N/A", length(input_scores)))
  }

  breaks <- c(levels_df$min_score, Inf)
  labels <- levels_df$label

  # cut: include.lowest=TRUE asegura que el valor exacto del corte inferior se incluya en el nivel
  levels_factor <- cut(input_scores, breaks = breaks, labels = labels, right = FALSE, include.lowest = TRUE)

  res <- as.character(levels_factor)
  res[is.na(res)] <- "N/A"
  res
}

# ==============================================================================
# 3. LOOKUP DE EQUIPARACIÓN CTT
# ==============================================================================

lookup_equated_score <- function(ids, raw_scores, forms_canonical, eq_tables, scale_id) {
  if (is.null(raw_scores) || all(is.na(raw_scores))) {
    return(list(eq_score = rep(NA_real_, length(ids)), see = rep(NA_real_, length(ids))))
  }

  scale_tbl <- eq_tables |> dplyr::filter(SCALE_ID == scale_id)

  if (nrow(scale_tbl) == 0) {
    return(list(eq_score = as.numeric(raw_scores), see = rep(NA_real_, length(ids))))
  }

  input_df <- data.frame(
    ID_TEMP = as.character(ids),
    SOURCE_FORM = trimws(as.character(forms_canonical)),
    RAW_lookup = as.integer(round(raw_scores)),
    stringsAsFactors = FALSE
  )

  merged <- dplyr::left_join(
    input_df,
    scale_tbl |>
      dplyr::select(SOURCE_FORM, RAW_SCORE, SEE) |>
      dplyr::mutate(SOURCE_FORM = trimws(as.character(SOURCE_FORM)), RAW_SCORE = as.integer(RAW_SCORE)),
    by = c("SOURCE_FORM" = "SOURCE_FORM", "RAW_lookup" = "RAW_SCORE")
  )

  list(eq_score = merged$EQUATED_SCORE, see = merged$SEE)
}

# ==============================================================================
# 4. ENTRY POINT PRINCIPAL
# ==============================================================================

apply_final_scoring <- function(item_scored_df, irt_scores_df = NULL, raw_data_obj, conversion_table, config) {
  source_method <- toupper(config$scoring$source %||% "CTT")
  message(sprintf("Iniciando Calificación Final (Fuente: %s)...", source_method))

  output_df <- data.frame(
    ID = as.character(item_scored_df$ID),
    FORMA = as.character(item_scored_df$FORMA),
    Metodo_Calculo = source_method,
    # Variable interna agnóstica para almacenar la métrica fuente (Theta o Puntos)
    Score_Input_Metric = NA_real_,
    Score_Final = NA_real_,
    Nivel = "N/A",
    stringsAsFactors = FALSE
  )

  code_map <- get_form_code_map(config)
  raw_forms <- trimws(as.character(output_df$FORMA))
  canonical_forms <- if (!is.null(code_map)) ifelse(!is.na(code_map[raw_forms]), code_map[raw_forms], raw_forms) else raw_forms

  # --- Determinación de Máximos (Techos para interpolación) ---
  items_global <- raw_data_obj$items_global %||% setdiff(names(item_scored_df)[sapply(item_scored_df, is.numeric)], c("ID", "FORMA"))

  max_raw_possible <- length(items_global)
  # Permite configurar el techo Theta (ej. 3.0, 4.0) desde config, default 4.0
  max_theta_possible <- config$scoring$irt_max_theta %||% 4.0

  # --- CTT Processing (Base) ---
  output_df$Raw_Global_CTT <- rowSums_robust(item_scored_df[, items_global, drop = FALSE])

  eq_tables <- if (is.data.frame(conversion_table)) conversion_table else conversion_table$tables
  if (!is.null(eq_tables)) {
    lookup_res <- lookup_equated_score(output_df$ID, output_df$Raw_Global_CTT, canonical_forms, eq_tables, "GLOBAL")
    output_df$Eq_Global_CTT <- lookup_res$eq_score
    output_df$SEE_Global_CTT <- lookup_res$see
  } else {
    output_df$Eq_Global_CTT <- NA_real_
    output_df$SEE_Global_CTT <- NA_real_
  }

  # --- IRT Processing ---
  if (!is.null(irt_scores_df)) {
    irt_clean <- irt_scores_df |>
      dplyr::select(ID, Theta, SE_Theta) |>
      dplyr::mutate(ID = as.character(ID)) |>
      dplyr::distinct(ID, .keep_all = TRUE)
    output_df <- dplyr::left_join(output_df, irt_clean, by = "ID")
  }

  # --- SELECCIÓN DE MÉTRICA FUENTE ---

  # Caso 1: IRT (Theta)
  if (source_method == "IRT") {
    if (!"Theta" %in% names(output_df) || all(is.na(output_df$Theta))) {
      warning("ALERTA: Configuración pide IRT pero no hay datos Theta. Fallback a CTT.")
      source_method <- "CTT"
      output_df$Metodo_Calculo <- "CTT (Fallback)"
    } else {
      # La métrica de entrada es Theta
      output_df$Score_Input_Metric <- output_df$Theta
    }
  }

  # Caso 2: CTT (Equiparado o Crudo)
  if (source_method == "CTT") {
    # Preferencia: Equiparado > Crudo
    if (all(is.na(output_df$Eq_Global_CTT))) {
      output_df$Score_Input_Metric <- output_df$Raw_Global_CTT
    } else {
      output_df$Score_Input_Metric <- output_df$Eq_Global_CTT
    }
  }

  # --- CÁLCULO FINAL Y NIVELES ---

  mask_valid <- !is.na(output_df$Score_Input_Metric)

  if (any(mask_valid)) {
    # Seleccionar el techo adecuado para la interpolación del último tramo
    limit_val <- if (source_method == "IRT") max_theta_possible else max_raw_possible

    # A. Calcular Score Final (Transformación Piecewise/Linear)
    output_df$Score_Final[mask_valid] <- apply_piecewise_mapping(
      output_df$Score_Input_Metric[mask_valid],
      config,
      source_method,
      max_input_val = limit_val
    )

    # B. Asignar Niveles
    # La asignación depende de la métrica de entrada (Theta o Puntos) definida en config$performance_levels
    output_df$Nivel[mask_valid] <- assign_performance_levels(
      output_df$Score_Input_Metric[mask_valid],
      config
    )
  }

  # --- Limpieza Final ---
  cols_tech <- c("Theta", "SE_Theta", "Eq_Global_CTT", "Score_Final")
  for (col in cols_tech) {
    if (!col %in% names(output_df)) output_df[[col]] <- NA_real_
  }

  message(sprintf("✅ Calificación GLOBAL completada. N=%d. Método: %s", nrow(output_df), source_method))
  return(output_df)
}
