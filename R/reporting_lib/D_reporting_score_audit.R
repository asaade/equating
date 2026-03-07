# ==============================================================================
# MÓDULO D: AUDITORÍA DE PUNTUACIONES E IMPACTO
# Versión: v1.0
# Descripción: Puntuaciones finales, tasas de aprobación, precisión (SEE)
#              y análisis de impacto adverso (Fairness).
# ==============================================================================

# --- FUNCIONES AUXILIARES ---
pad_str <- function(x, width, align = "left") {
  x <- as.character(x)
  if (is.na(x) || length(x) == 0) x <- ""
  if (nchar(x) > width) x <- substr(x, 1, width)
  if (align == "right") formatC(x, width = width, flag = "") else formatC(x, width = width, flag = "-")
}

print_header <- function(title) {
  cat(paste0(strrep("=", 80), "\n"))
  cat(paste0("  ", title, "\n"))
  cat(paste0(strrep("=", 80), "\n"))
}

print_section <- function(title) {
  cat(paste0("\n", strrep("-", 80), "\n"))
  cat(paste0(">>> ", title, "\n"))
  cat(paste0(strrep("-", 80), "\n"))
}

# ==============================================================================
# FUNCIÓN PRINCIPAL
# ==============================================================================
audit_score_impact <- function(final_scores, raw_dat = NULL, eq_results = NULL, base_dir, config) {
  if (is.null(final_scores) || nrow(final_scores) == 0) {
    return(NULL)
  }

  # --- FIX: Detección segura de columnas en final_scores (Case-Insensitive) ---
  fs_cols <- names(final_scores)
  fs_cols_up <- toupper(fs_cols)

  col_fs_id    <- fs_cols[na.omit(match(toupper(c("ID", "PERSON_ID", "IDENTIFICADOR")), fs_cols_up))[1]]
  col_fs_form  <- fs_cols[na.omit(match(toupper(c("FORMA", "FORM", "COLECCIÓN")), fs_cols_up))[1]]
  col_fs_raw   <- fs_cols[na.omit(match(toupper(c("Raw_Global_CTT", "RAW_SCORE", "SCORE_RAW", "PUNTAJE_CRUDO")), fs_cols_up))[1]]
  col_fs_eq    <- fs_cols[na.omit(match(toupper(c("Eq_Global_CTT", "EQUATED_SCORE", "SCORE_EQ", "PUNTAJE_EQUIPARADO")), fs_cols_up))[1]]
  col_fs_see   <- fs_cols[na.omit(match(toupper(c("SEE_Global", "SEE", "ERROR_ESTANDAR", "SEM")), fs_cols_up))[1]]
  col_fs_level <- fs_cols[na.omit(match(toupper(c("Nivel", "LEVEL", "PERFORMANCE_LEVEL", "DESEMPEÑO")), fs_cols_up))[1]]

  out_dir <- file.path(base_dir)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  forma_id <- if (!is.na(col_fs_form)) unique(final_scores[[col_fs_form]])[1] else "GLOBAL"
  if (is.na(forma_id)) forma_id <- "GLOBAL"
  report_file <- file.path(out_dir, paste0("AUDIT_SCORES_IMPACT_", forma_id, ".txt"))

  sink(report_file)
  on.exit(sink())

  print_header(paste("REPORTE DE IMPACTO Y PRECISIÓN: FORMA", forma_id))
  cat(paste0("Fecha: ", Sys.time(), "\n"))
  cat(paste0("N Sustentantes: ", nrow(final_scores), "\n"))

  # ----------------------------------------------------------------------------
  # 1. ESTADÍSTICOS Y EFECTO DEL EQUATING
  # ----------------------------------------------------------------------------
  print_section("1. COMPARATIVA DE MÉTRICA (DELTA)")
  if (!is.na(col_fs_raw) && !is.na(col_fs_eq)) {
    raw_m <- mean(final_scores[[col_fs_raw]], na.rm = TRUE)
    eq_m <- mean(final_scores[[col_fs_eq]], na.rm = TRUE)
    delta <- eq_m - raw_m

    cat(paste0("Media Puntuación Cruda  : ", sprintf("%.2f", raw_m), "\n"))
    cat(paste0("Media Puntuación Escala : ", sprintf("%.2f", eq_m), "\n"))
    cat(paste0("Efecto Neto Equating    : ", sprintf("%+.2f", delta), " puntos\n"))

    if (!is.na(delta) && delta > 0) {
      cat(">> Interpretación: La forma fue más DIFÍCIL que la base (Bonificación).\n")
    } else if (!is.na(delta) && delta <= 0) {
      cat(">> Interpretación: La forma fue más FÁCIL que la base (Ajuste negativo).\n")
    }
  }

  # ----------------------------------------------------------------------------
  # 2. TASAS DE APROBACIÓN (PASS RATES)
  # ----------------------------------------------------------------------------
  print_section("2. TASAS DE APROBACIÓN (PASS RATES)")

  # Verificar si tenemos puntos de corte en eq_results
  if (!is.null(eq_results) && !is.null(eq_results$audit_critical)) {
    cuts <- eq_results$audit_critical

    # --- FIX: Detección segura de columnas (Case-Insensitive) ---
    # Mapeamos candidatos a las columnas reales de audit_critical (priorizando engine v2)
    label_candidates <- toupper(c("TARGET_CUT_RAW_REF", "LABEL", "TARGET_CUT_REF", "NOTE"))
    value_candidates <- toupper(c("EQUATED_AT_CUT", "EQUATED", "EST_RAW_CUT", "CUT_SCORE"))
    scale_candidates <- toupper(c("SCALE_ID", "ESCALA", "SCALE"))
    form_candidates  <- toupper(c("FORM", "FORMA", "SOURCE_FORM"))

    col_names <- names(cuts)
    col_names_up <- toupper(col_names)

    # Buscar etiqueta por prioridad
    col_label <- col_names[na.omit(match(label_candidates, col_names_up))[1]]

    # Buscar valor por prioridad
    col_value <- col_names[na.omit(match(value_candidates, col_names_up))[1]]

    # Buscar escala
    col_scale <- col_names[na.omit(match(scale_candidates, col_names_up))[1]]

    # Buscar forma en cuts
    col_form_cuts <- col_names[na.omit(match(form_candidates, col_names_up))[1]]

    if (!is.null(col_label) && !is.null(col_value) && !is.na(col_label) && !is.na(col_value)) {
      cat(paste0(pad_str("PUNTO DE CORTE", 25), pad_str("SCORE REQ.", 12), pad_str("APROBADOS (N)", 15), "TASA (%)\n"))
      cat(paste0(strrep("-", 70), "\n"))

      # Filtrar cortes
      cuts_to_use <- cuts
      if (!is.na(col_scale)) {
        cuts_to_use <- cuts_to_use[toupper(as.character(cuts_to_use[[col_scale]])) == "GLOBAL", ]
      }
      if (!is.na(col_form_cuts) && forma_id != "GLOBAL") {
        # Si tenemos forma específica, filtramos por ella
        cuts_to_use <- cuts_to_use[as.character(cuts_to_use[[col_form_cuts]]) == forma_id, ]
      }

      scores <- if (!is.na(col_fs_eq)) final_scores[[col_fs_eq]] else NULL

      if (!is.null(scores)) {
        # Extraer pares únicos para evitar repeticiones por forma
        cuts_unique <- unique(cuts_to_use[, c(col_label, col_value)])
        names(cuts_unique) <- c("LBL", "VAL")

        for (i in 1:nrow(cuts_unique)) {
        cut_label <- as.character(cuts_unique$LBL[i])
        cut_val <- as.numeric(cuts_unique$VAL[i])

        n_pass <- sum(scores >= cut_val, na.rm = TRUE)
        pct_pass <- (n_pass / length(scores)) * 100

        cat(paste0(
          pad_str(cut_label, 25),
          pad_str(sprintf("%.2f", cut_val), 12),
          pad_str(n_pass, 15),
          sprintf("%.1f%%", pct_pass), "\n"
        ))
        }
      }
    } else {
      cat("No se pudieron identificar las columnas de etiqueta/valor en la tabla de cortes (audit_critical).\n")
      cat("Columnas disponibles: ", paste(names(cuts), collapse = ", "), "\n")
    }
  } else {
    cat("No hay información de puntos de corte disponible para calcular tasas.\n")
  }

  # ----------------------------------------------------------------------------
  # 3. DISTRIBUCIÓN DE NIVELES
  # ----------------------------------------------------------------------------
  print_section("3. DISTRIBUCIÓN DE NIVELES DE DESEMPEÑO")
  if (!is.na(col_fs_level)) {
    lvl_counts <- table(final_scores[[col_fs_level]])
    lvl_props <- prop.table(lvl_counts) * 100

    for (lvl in names(lvl_counts)) {
      cat(paste0(
        pad_str(lvl, 20), "|", strrep("#", round(lvl_props[[lvl]] / 2)),
        " ", sprintf("%.1f%%", lvl_props[[lvl]]), "\n"
      ))
    }
  }

  # ----------------------------------------------------------------------------
  # 4. PRECISIÓN POBLACIONAL (SEE)
  # ----------------------------------------------------------------------------
  print_section("4. PRECISIÓN ESTIMADA DE LAS PUNTUACIONES (SEE)")

  if (!is.na(col_fs_see) && !is.na(col_fs_eq)) {
    avg_see <- mean(final_scores[[col_fs_see]], na.rm = TRUE)
    sd_score <- sd(final_scores[[col_fs_eq]], na.rm = TRUE)

    # Ratio Ruido/Señal: Qué tanto del SD observado es error
    ratio <- if (!is.na(sd_score) && sd_score > 0) avg_see / sd_score else NA

    cat(paste0("Error Estándar Promedio (SEE) : ", sprintf("%.3f", avg_see), "\n"))
    cat(paste0("Desviación Estándar (SD)      : ", sprintf("%.3f", sd_score), "\n"))

    if (!is.na(ratio)) {
      cat(paste0("Ratio Ruido/Señal (SEE/SD)    : ", sprintf("%.2f", ratio), "\n"))

      if (ratio < 0.10) {
        cat(">> DIAGNÓSTICO: Precisión ALTA (El error es menor al 10% de la variabilidad).\n")
      } else if (ratio < 0.30) {
        cat(">> DIAGNÓSTICO: Precisión MODERADA/ACEPTABLE.\n")
      } else {
        cat(">> DIAGNÓSTICO: Precisión BAJA (El error compromete la interpretación).\n")
      }
    }
  } else {
    cat("No se encontró columna SEE_Global para auditar precisión.\n")
  }

  # ----------------------------------------------------------------------------
  # 5. ANÁLISIS DE IMPACTO DIFERENCIAL (FAIRNESS)
  # ----------------------------------------------------------------------------
  print_section("5. EQUIDAD Y ANÁLISIS DE SUBGRUPOS (FAIRNESS)")

  if (!is.null(raw_dat)) {
    # --- FIX: Detección segura de columnas demográficas (Case-Insensitive) ---
    raw_cols <- names(raw_dat)
    raw_cols_up <- toupper(raw_cols)

    # Identificar nombres reales de columnas prioritarias
    col_id_raw  <- raw_cols[match("ID", raw_cols_up)]
    col_sexo    <- raw_cols[match("SEXO", raw_cols_up)]
    col_region  <- raw_cols[match("REGION", raw_cols_up)]

    # Unir datos demográficos (raw_dat) con puntuaciones (final_scores) usando ID
    # Aseguramos que ID sea del mismo tipo
    if (!is.na(col_fs_id)) {
      final_scores[[col_fs_id]] <- as.character(final_scores[[col_fs_id]])

      if (!is.na(col_id_raw)) {
        raw_dat[[col_id_raw]] <- as.character(raw_dat[[col_id_raw]])

        # Columnas a seleccionar (las que existan)
        cols_demog_to_select <- c(col_id_raw, col_sexo, col_region)
        cols_demog_to_select <- cols_demog_to_select[!is.na(cols_demog_to_select)]

        merged_scores <- final_scores |>
          dplyr::left_join(raw_dat |> dplyr::select(dplyr::all_of(cols_demog_to_select)), by = setNames(col_id_raw, col_fs_id))
      } else {
        merged_scores <- final_scores
        # Intentar usar warn si está disponible (según memoria está en R/00_common_base.R)
        if (exists("warn")) {
          warn("No se encontró columna ID en raw_dat para cruce demográfico.", "Audit_Score")
        } else {
          cat(">> ADVERTENCIA: No se encontró columna ID en raw_dat para cruce demográfico.\n")
        }
      }
    } else {
      merged_scores <- final_scores
      # Intentar usar warn si está disponible (según memoria está en R/00_common_base.R)
      if (exists("warn")) {
        warn("No se encontró columna ID en raw_dat para cruce demográfico.", "Audit_Score")
      } else {
        cat(">> ADVERTENCIA: No se encontró columna ID en raw_dat para cruce demográfico.\n")
      }
    }

    # Función interna para analizar un grupo
    analyze_group <- function(data, group_var) {
      if (!group_var %in% names(data)) {
        return(NULL)
      }

      cat(paste0("\n--- ANÁLISIS POR: ", group_var, " ---\n"))

      # Group By dinámico
      stats <- data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_var))) |>
        dplyr::summarise(
          N = dplyr::n(),
          Mean_Eq = mean(.data[[col_fs_eq]], na.rm = TRUE),
          SD_Eq = sd(.data[[col_fs_eq]], na.rm = TRUE),
          # Tasa de aprobación
          Pass_Rate = if (!is.na(col_fs_level)) mean(.data[[col_fs_level]] != "Insuficiente", na.rm = TRUE) * 100 else NA_real_
        ) |>
        dplyr::mutate(
          # Diferencia estandarizada
          Std_Diff = (Mean_Eq - mean(data[[col_fs_eq]], na.rm = TRUE)) / sd(data[[col_fs_eq]], na.rm = TRUE)
        )

      cat(paste0(
        pad_str("GRUPO", 15),
        pad_str("N", 8),
        pad_str("MEDIA", 10),
        pad_str("SD", 8),
        pad_str("%_APROB", 10),
        "STD_DIFF\n"
      ))
      cat(paste0(strrep("-", 65), "\n"))

      for (k in 1:nrow(stats)) {
        grp_name <- as.character(stats[k, group_var])
        if (is.na(grp_name)) grp_name <- "N/A"

        cat(paste0(
          pad_str(grp_name, 15),
          pad_str(stats$N[k], 8),
          pad_str(sprintf("%.2f", stats$Mean_Eq[k]), 10),
          pad_str(sprintf("%.2f", stats$SD_Eq[k]), 8),
          pad_str(sprintf("%.1f%%", stats$Pass_Rate[k]), 10),
          sprintf("%+.2f", stats$Std_Diff[k]), "\n"
        ))
      }

      max_pass <- max(stats$Pass_Rate, na.rm = TRUE)
      min_pass <- min(stats$Pass_Rate, na.rm = TRUE)
      if (!is.na(max_pass) && !is.na(min_pass) && is.finite(max_pass) && is.finite(min_pass) && (max_pass - min_pass) > 10) {
        cat(paste0(
          ">> ALERTA: Brecha de aprobación > 10% detectada en ", group_var,
          " (Posible Impacto Adverso).\n"
        ))
      }
    }

    if (!is.na(col_sexo) && col_sexo %in% names(merged_scores)) analyze_group(merged_scores, col_sexo)
    if (!is.na(col_region) && col_region %in% names(merged_scores)) analyze_group(merged_scores, col_region)
  } else {
    cat("Datos crudos (raw_dat) no disponibles. Se omite análisis demográfico.\n")
  }

  cat("\n>>> FIN DEL REPORTE D <<<\n")
}
