# ==============================================================================
# MÓDULO: GENERACIÓN DE REPORTES TÉCNICOS EN CSV (CTT, EQUATING & BIAS)
# Enfoque: Auditoría técnica, trazabilidad y métricas de decisión.
# Versión: v4.0
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, readr, moments, stats, tidyr, stringr, purrr)

# ==============================================================================
# 0. UTILERÍAS DE EXPORTACIÓN
# ==============================================================================

write_psych_csv <- function(data, base_dir, sub_folder, file_name) {
  if (is.null(data) || (is.data.frame(data) && nrow(data) == 0)) {
    return(FALSE)
  }

  full_path <- file.path(base_dir, "AUDITORIA_TECNICA", sub_folder)
  if (!dir.exists(full_path)) dir.create(full_path, recursive = TRUE, showWarnings = FALSE)

  file_path <- file.path(full_path, file_name)

  tryCatch(
    {
      # Limpieza profunda: Manejo de listas, redondeo forense y strings
      data_clean <- as.data.frame(data) |>
        dplyr::mutate(across(where(is.list), ~ sapply(., function(x) paste(unlist(x), collapse = "; ")))) |>
        dplyr::mutate(across(where(is.numeric), ~ round(., 6))) |>
        dplyr::mutate(across(where(is.character), ~ ifelse(is.na(.), "", .)))

      readr::write_csv(data_clean, file_path, na = "")
      return(TRUE)
    },
    error = function(e) {
      message(paste("Error al exportar:", file_name, "-", sanitize_error_msg(e)))
      return(FALSE)
    }
  )
}

# ==============================================================================
# 1. AUDITORÍA DE ÍTEMS Y DISTRACTORES (TCT)
# ==============================================================================

export_item_audit <- function(ctt_results, meta_data, config, base_dir) {
  if (is.null(ctt_results$stats)) {
    return()
  }

  th_p_min <- config$thresholds$ctt_p_val_min %||% 0.20
  th_p_max <- config$thresholds$ctt_p_val_max %||% 0.90
  th_pbis <- config$thresholds$ctt_pbis_min %||% 0.25

  # 1.1 Auditoría Maestra
  stats_enriched <- ctt_results$stats |>
    dplyr::left_join(meta_data, by = c("ITEM" = "ITEM_ID")) |>
    dplyr::mutate(
      CLASIFICACION_DIFICULTAD = dplyr::case_when(
        P_VAL < th_p_min ~ "MUY DIFÍCIL",
        P_VAL > th_p_max ~ "MUY FÁCIL",
        TRUE ~ "ADECUADA"
      ),
      CLASIFICACION_DISCRIMINACION = dplyr::case_when(
        P_BIS < 0 ~ "NEGATIVA (ERROR)",
        P_BIS < 0.15 ~ "MUY BAJA",
        P_BIS < th_pbis ~ "MARGINAL",
        TRUE ~ "ADECUADA"
      ),
      DIAGNOSTICO_ACCION = dplyr::case_when(
        P_BIS < 0 ~ "REVISAR CLAVE O CONSTRUCCIÓN",
        P_BIS < 0.15 ~ "REVISAR CONTENIDO / AMBIGÜEDAD",
        (P_VAL < 0.10 & P_BIS < 0.15) ~ "ELIMINAR POR BAJA CALIDAD",
        TRUE ~ "CONSERVAR"
      )
    )

  write_psych_csv(stats_enriched, base_dir, "01_ANALISIS_ITEMS", "Auditoria_Maestra_Items.csv")

  # 1.2 Alertas de Contenido (v3.0 Legacy Support)
  alertas <- stats_enriched |> dplyr::filter(DIAGNOSTICO_ACCION != "CONSERVAR")
  write_psych_csv(alertas, base_dir, "01_ANALISIS_ITEMS", "Alertas_Revision_Contenido.csv")

  # 1.3 Análisis de Distractores (v3.0 Legacy Support)
  if (!is.null(ctt_results$distractors)) {
    dist_df <- ctt_results$distractors
    alertas_dist <- dist_df |>
      dplyr::filter(IS_KEY == FALSE, R_BIS_OPT > 0.10) |>
      dplyr::mutate(ALERTA = "DISTRACTOR_ATRACTIVO_SOSPECHOSO")

    write_psych_csv(dist_df, base_dir, "01_ANALISIS_ITEMS", "Analisis_Distractores_Detalle.csv")
    write_psych_csv(alertas_dist, base_dir, "01_ANALISIS_ITEMS", "Alertas_Distractores_Ambiguos.csv")
  }
}

# ==============================================================================
# 2. MÉTRICAS DE CALIDAD GLOBAL (TEST LEVEL)
# ==============================================================================

export_test_metrics <- function(ctt_results, base_dir) {
  # Confiabilidad (Alfa/Omega) y Error Estándar (SEM)
  if (!is.null(ctt_results$reliability)) {
    write_psych_csv(ctt_results$reliability, base_dir, "02_CALIDAD_TEST", "Metricas_Confiabilidad.csv")
  }

  # Dimensionalidad y Estructura Factorial
  if (!is.null(ctt_results$dimensionality)) {
    write_psych_csv(ctt_results$dimensionality, base_dir, "02_CALIDAD_TEST", "Analisis_Dimensionalidad.csv")
  }
}

# ==============================================================================
# 3. TRAZABILIDAD DE EQUIPARACIÓN (FORENSIC EQUATING)
# ==============================================================================

export_equating_audit <- function(eq_results, base_dir) {
  if (is.null(eq_results)) {
    return()
  }
  folder <- "03_EQUIPARACION"

  # 3.1 Operación y Decisiones
  write_psych_csv(eq_results$tables, base_dir, folder, "Tablas_Conversion_Final.csv")
  write_psych_csv(eq_results$coefficients, base_dir, folder, "Parametros_Modelos_Equiparacion.csv")
  write_psych_csv(eq_results$decisions, base_dir, folder, "Trazabilidad_Seleccion_Metodo.csv")
  write_psych_csv(eq_results$executive_summary, base_dir, folder, "Resumen_Ejecutivo_Equiparacion.csv")

  # 3.2 Auditoría de Precisión y QC (v6 Extended)
  write_psych_csv(eq_results$audit_qc, base_dir, folder, "Control_Calidad_QC_Monotonia.csv")
  write_psych_csv(eq_results$audit_critical, base_dir, folder, "Auditoria_Precision_Puntos_Corte.csv")
  write_psych_csv(eq_results$smoothing_history, base_dir, folder, "Historial_Suavizamiento_LogLineal.csv")

  # 3.3 Estabilidad (Drift)
  if (!is.null(eq_results$drift_details)) {
    drift_rep <- eq_results$drift_details |>
      dplyr::mutate(ITEM_ID = rownames(eq_results$drift_details)) |>
      dplyr::mutate(ESTADO_ANCLAJE = ifelse(STATUS == "KEPT", "ESTABLE", "ELIMINADO")) |>
      dplyr::arrange(desc(abs(Z_SCORE)))
    write_psych_csv(drift_rep, base_dir, folder, "Analisis_Estabilidad_Anclajes.csv")
  }

  # 3.4 Red y Topología
  write_psych_csv(eq_results$link_quality, base_dir, folder, "Calidad_Enlaces_Red.csv")
  write_psych_csv(eq_results$topology_plan, base_dir, folder, "Plan_Topologia_Costos.csv")
  write_psych_csv(eq_results$equated_moments, base_dir, folder, "Momentos_Equiparados.csv")
  write_psych_csv(eq_results$psychometric_passport, base_dir, folder, "Pasaporte_Psicometrico.csv")
}

# ==============================================================================
# 4. ANÁLISIS DE EQUIDAD Y SESGO (DIF)
# ==============================================================================

export_equity_audit <- function(dif_results, base_dir) {
  if (is.null(dif_results)) {
    return()
  }

  dif_enriched <- as.data.frame(dif_results) |>
    dplyr::mutate(
      CATEGORIA_ETS = dplyr::case_when(
        abs(as.numeric(ETS_Delta %||% 0)) < 1.0 ~ "A (INSIGNIFICANTE)",
        abs(as.numeric(ETS_Delta %||% 0)) < 1.5 ~ "B (MODERADO)",
        TRUE ~ "C (SEVERO)"
      ),
      ACCION_EQUIDAD = ifelse(grepl("C ", CATEGORIA_ETS), "REVISAR_SESGO", "OK")
    )

  write_psych_csv(dif_enriched, base_dir, "04_EQUIDAD_Y_DIF", "Analisis_DIF_Estandar_ETS.csv")
}

# ==============================================================================
# 5. RESULTADOS FINALES Y NIVELES
# ==============================================================================

export_score_summaries <- function(final_scores, base_dir) {
  if (is.null(final_scores)) {
    return()
  }
  folder <- "05_RESULTADOS_FINALES"

  # 5.1 Distribución de Niveles (v3.0 Legacy)
  if ("Nivel" %in% names(final_scores)) {
    dist_niveles <- final_scores |>
      dplyr::group_by(FORMA, Nivel) |>
      dplyr::summarise(N = n(), .groups = "drop") |>
      dplyr::group_by(FORMA) |>
      dplyr::mutate(PORCENTAJE = (N / sum(N)) * 100)
    write_psych_csv(dist_niveles, base_dir, folder, "Distribucion_Niveles_Desempeno.csv")
  }

  # 5.2 Estadísticos Descriptivos Post-Equiparación
  if ("EQUATED_SCORE" %in% names(final_scores)) {
    stats_summary <- final_scores |>
      dplyr::group_by(FORMA) |>
      dplyr::summarise(
        N = n(),
        Media = mean(EQUATED_SCORE, na.rm = TRUE),
        SD = sd(EQUATED_SCORE, na.rm = TRUE),
        Min = min(EQUATED_SCORE, na.rm = TRUE),
        Max = max(EQUATED_SCORE, na.rm = TRUE),
        Simetria = moments::skewness(EQUATED_SCORE, na.rm = TRUE),
        Curtosis = moments::kurtosis(EQUATED_SCORE, na.rm = TRUE)
      )
    write_psych_csv(stats_summary, base_dir, folder, "Estadisticos_Descriptivos_Puntajes.csv")
  }
}

# ==============================================================================
# PUNTO DE ENTRADA PRINCIPAL
# ==============================================================================

generate_csv_reports <- function(config, ctt_results, eq_results, dif_results, final_scores, meta_data) {
  message(">>> Iniciando persistencia de auditoría técnica v4.0...")
  base <- config$project$output_dir %||% "RESULTADOS_PROCESAMIENTO"

  export_item_audit(ctt_results, meta_data, config, base)
  export_test_metrics(ctt_results, base)
  export_equating_audit(eq_results, base)
  export_equity_audit(dif_results, base)
  export_score_summaries(final_scores, base)

  message("✅ Auditoría completa generada en: ", file.path(base, "AUDITORIA_TECNICA"))
}
