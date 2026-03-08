# R/02_scoring_engine.R
# Responsabilidad: Scoring Vectorizado (Matriz Dispersa) con Validación y Limpieza.
# Versión: v1.3

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, checkmate, rlang)

# ==============================================================================
# 1. HELPERS & VALIDACIÓN
# ==============================================================================

# Helper para detectar patrones invariantes (long strings)
# Ignora NAs para detectar si alguien marcó "A A A A" consecutivamente
calculate_long_string <- function(row_vec) {
  vec <- as.character(row_vec[!is.na(row_vec)])
  if (length(vec) == 0) {
    return(0)
  }

  rle_res <- rle(vec)
  if (length(rle_res$lengths) == 0) {
    return(0)
  }

  return(max(rle_res$lengths))
}

validate_input_integrity <- function(raw_df, map) {
  # 1. Verificar que el DataFrame de entrada no esté vacío
  if (nrow(raw_df) == 0) {
    stop("FATAL: DataFrame 'raw_df' está vacío.")
  }

  # 2. Verificar que el Mapa de Diseño no esté vacío
  if (is.null(map) || nrow(map) == 0) {
    stop("FATAL: El Mapa de Diseño ('map') está vacío o es NULL.")
  }

  # 3. Verificar que todas las formas presentes en los datos existan en el mapa
  formas_datos <- unique(as.character(raw_df$FORMA))
  formas_mapa <- unique(as.character(map$FORMA_CODE))
  formas_faltantes <- setdiff(formas_datos, formas_mapa)

  if (length(formas_faltantes) > 0) {
    stop(sprintf(
      "FATAL: Se detectaron formas en los datos que no están definidas en el mapa: %s",
      paste(formas_faltantes, collapse = ", ")
    ))
  }

  # 4. Verificar integridad de columnas
  required_cols <- unique(map$COL_NAME_RAW)
  missing_cols <- setdiff(required_cols, colnames(raw_df))

  if (length(missing_cols) > 0) {
    stop(sprintf(
      "ERROR DE INTEGRIDAD: Faltan %d columnas requeridas (ej. %s)",
      length(missing_cols), paste(head(missing_cols, 3), collapse = ", ")
    ))
  }
  return(TRUE)
}

# ==============================================================================
# 2. MOTOR DE SCORING (Soporte Matriz Dispersa)
# ==============================================================================

process_forms_scoring <- function(raw_df, map, n_subjects, all_items, policy_na_zero, th_long_string) {
  # Matriz Global: Se inicia en NA. Los ítems no presentados se quedarán como NA.
  scored_matrix <- matrix(NA_integer_, nrow = n_subjects, ncol = length(all_items))
  colnames(scored_matrix) <- all_items

  # Vectores para métricas de QC
  qc_na_pct <- rep(1.0, n_subjects) # Se asume 100% omisión hasta demostrar lo contrario
  qc_long_str <- rep(0, n_subjects)

  # 4. Procesamiento por Forma (Mantiene estructura dispersa)
  formas_presentes <- unique(raw_df$FORMA)

  for (f in formas_presentes) {
    # Filtros
    map_f <- map[map$FORMA_CODE == f, ]
    if (nrow(map_f) == 0) next

    idx_subjects <- which(raw_df$FORMA == f)
    if (length(idx_subjects) == 0) next

    # Datos y Claves específicos de esta forma
    item_cols <- map_f$COL_NAME_RAW
    item_ids <- map_f$ITEM_ID
    keys <- map_f$KEY

    raw_sub <- raw_df[idx_subjects, item_cols, drop = FALSE]
    mat_raw <- as.matrix(raw_sub)

    # --- A. CÁLCULO DE MÉTRICAS QC (Sobre datos crudos de esta forma) ---

    # 1. Porcentaje de Omisión (NA Rate)
    n_items_form <- length(item_cols)
    na_counts <- rowSums(is.na(mat_raw))
    qc_na_pct[idx_subjects] <- na_counts / n_items_form

    # 2. Patrones Invariantes (Long String)
    if (!is.null(th_long_string)) {
      qc_long_str[idx_subjects] <- apply(mat_raw, 1, calculate_long_string)
    }

    # --- B. CALIFICACIÓN ---
    is_correct <- t(apply(mat_raw, 1, function(row) row == keys))

    # Conversión a entero (TRUE -> 1, FALSE -> 0, NA -> NA)
    scored_block <- matrix(as.integer(is_correct), nrow = nrow(is_correct), dimnames = dimnames(is_correct))

    # C. IMPUTACIÓN Y GARANTÍA BINARIA (1/0)
    # Los ítems en este bloque son ACTIVOS para la forma actual.
    # Cualquier NA aquí representa una omisión del sustentante, no un ítem no visto.
    # Se imputan a 0 (Falla) para garantizar que todo ítem presentado valga 1 o 0.
    if (policy_na_zero) {
      scored_block[is.na(scored_block)] <- 0L
    }

    # Inserción en matriz global
    # IMPORTANTE: Solo tocamos las columnas item_ids. El resto permanece NA (estructural).
    scored_matrix[idx_subjects, item_ids] <- scored_block
  }

  return(list(
    scored_matrix = scored_matrix,
    qc_na_pct = qc_na_pct,
    qc_long_str = qc_long_str
  ))
}

apply_qc_rules <- function(scored_matrix, n_subjects, qc_na_pct, qc_long_str, th_max_na_pct, th_long_string, th_check_flat) {
  # 5. Ejecución de Reglas de Limpieza (QC)
  mask_keep <- rep(TRUE, n_subjects)
  qc_reasons <- rep(NA_character_, n_subjects)

  # Regla 1: Exceso de Omisiones (High NA Rate)
  bad_na <- qc_na_pct > th_max_na_pct
  if (any(bad_na)) {
    mask_keep[bad_na] <- FALSE
    qc_reasons[bad_na] <- "HIGH_OMISSION_RATE"
  }

  # Regla 2: Patrones Invariantes (Careless Responders)
  if (any(mask_keep) && !is.null(th_long_string)) {
    bad_string <- (qc_long_str > th_long_string) & mask_keep
    if (any(bad_string)) {
      mask_keep[bad_string] <- FALSE
      qc_reasons[bad_string] <- paste0("LONG_STRING_>", th_long_string)
    }
  }

  # Regla 3: Varianza Cero (Patrón plano en aciertos/fallos)
  # INFO: Se calcula sd() con na.rm=TRUE para ignorar los NAs estructurales.
  if (th_check_flat && any(mask_keep)) {
    active_indices <- which(mask_keep)

    # Extraemos solo las filas activas para calcular desviación estándar
    # NOTA: El parámetro na.rm=TRUE es vital aquí: calcula la varianza solo sobre lo que el sujeto vio.
    row_sds <- apply(scored_matrix[active_indices, , drop = FALSE], 1, sd, na.rm = TRUE)

    # Si SD es NA (solo 1 ítem respondido) o < epsilon (todas respuestas iguales), es plano.
    # Nota: Permitimos SD=NA si el test es de 1 solo ítem (raro), pero asumimos test > 1.
    bad_flat <- !is.na(row_sds) & (row_sds < 1e-9)

    if (any(bad_flat)) {
      idx_remove_global <- active_indices[bad_flat]
      mask_keep[idx_remove_global] <- FALSE
      qc_reasons[idx_remove_global] <- "ZERO_VARIANCE_SCORE"
    }
  }

  return(list(
    mask_keep = mask_keep,
    qc_reasons = qc_reasons
  ))
}

format_scoring_output <- function(raw_df, scored_matrix, mask_keep, qc_reasons, n_subjects, all_items) {
  # 6. Consolidación de Resultados
  final_df <- cbind(
    raw_df[, c("ID", "FORMA")],
    as.data.frame(scored_matrix)
  )

  scored_clean <- final_df[mask_keep, ]

  # Logging de exclusiones
  n_excluded <- sum(!mask_keep)
  if (n_excluded > 0) {
    table_causes <- table(qc_reasons[!mask_keep])
    msg_causes <- paste(names(table_causes), as.numeric(table_causes), sep = ":", collapse = ", ")
    # Usar message() para visibilidad en consola
    warn(sprintf("QC: %d sustentantes excluidos. Causas: [%s]", n_excluded, msg_causes))
  }

  return(list(
    scored = scored_clean,
    stats = list(
      n_total = n_subjects,
      n_valid = nrow(scored_clean),
      n_excluded = n_excluded,
      n_items = length(all_items)
    )
  ))
}

score_population <- function(data_obj, config) {
  debug("Iniciando Scoring de Población Base")

  raw_df <- data_obj$raw_dat
  map <- data_obj$design_map

  if (is.null(map)) stop("FATAL: Design Map no encontrado.")

  # 1. Validación de estructura
  validate_input_integrity(raw_df, map)

  # 2. Configuración y Políticas
  # treat_na_as_zero: Solo aplica a omisiones (ítems vistos pero no respondidos)
  policy_na_zero <- if (!is.null(config$scoring$treat_na_as_zero)) config$scoring$treat_na_as_zero else TRUE

  # Umbrales de Limpieza (QC)
  th_max_na_pct <- if (!is.null(config$cleaning$max_na_pct)) config$cleaning$max_na_pct else 0.5
  th_check_flat <- if (!is.null(config$cleaning$remove_flat)) config$cleaning$remove_flat else TRUE
  th_long_string <- if (!is.null(config$cleaning$max_invariant)) config$cleaning$max_invariant else 10

  # 3. Inicialización de Estructuras Globales
  all_items <- unique(map$ITEM_ID)
  n_subjects <- nrow(raw_df)

  scoring_results <- process_forms_scoring(raw_df, map, n_subjects, all_items, policy_na_zero, th_long_string)

  qc_results <- apply_qc_rules(scoring_results$scored_matrix, n_subjects, scoring_results$qc_na_pct, scoring_results$qc_long_str, th_max_na_pct, th_long_string, th_check_flat)

  return(format_scoring_output(raw_df, scoring_results$scored_matrix, qc_results$mask_keep, qc_results$qc_reasons, n_subjects, all_items))
}

# ==============================================================================
# 3. MUESTREO (Opcional)
# ==============================================================================
create_calibration_sample <- function(full_df, config) {
  if (is.null(config$sampling$enabled) || !isTRUE(config$sampling$enabled)) {
    return(full_df)
  }


  set.seed(config$sampling$seed)
  target_n <- config$sampling$size_per_form

  sample_df <- full_df |>
    group_by(FORMA) |>
    sample_n(size = min(n(), target_n)) |>
    ungroup()

  return(sample_df)
}
