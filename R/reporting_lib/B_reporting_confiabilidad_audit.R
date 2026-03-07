# ==============================================================================
# MÓDULO B: AUDITORÍA DE ESTRUCTURA INTERNA Y CONFIABILIDAD
# Versión: v1.0
# Descripción: Evalúa la consistencia interna (Alfa, SEM) y la dimensionalidad
#              de la prueba contra estándares internacionales (AERA/APA/NCME).
# ==============================================================================

# --- FUNCIONES AUXILIARES DE FORMATEO ---
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
audit_internal_structure <- function(ctt_results, base_dir, config) {
  # 1. Validación de Inputs
  if (is.null(ctt_results$reliability)) {
    error("Faltan datos de confiabilidad en ctt_results.")
    return(NULL)
  }

  out_dir <- file.path(base_dir)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Detectar Forma (para nombre de archivo)
  forma_lbl <- if (!is.null(ctt_results$reliability$FORMA[1])) {
    paste0("_", ctt_results$reliability$FORMA[1])
  } else {
    ""
  }

  report_file <- file.path(out_dir, paste0("AUDIT_STRUCT_RELIABILITY", forma_lbl, ".txt"))

  sink(report_file)
  on.exit(sink())

  # --- ENCABEZADO ---
  print_header("AUDITORÍA DE ESTRUCTURA INTERNA Y CONFIABILIDAD (CTT)")
  cat(paste0("Fecha de Generación : ", Sys.time(), "\n"))
  cat(paste0("Estándar Evaluado   : AERA/APA/NCME (Validez de Estructura Interna)\n"))

  # ============================================================================
  # SECCIÓN 1: CONFIABILIDAD (CONSISTENCIA INTERNA)
  # ============================================================================
  print_section("1. CONFIABILIDAD (ALFA DE CRONBACH & SEM)")

  rel_df <- ctt_results$reliability

  if (nrow(rel_df) == 0) {
    cat(">> ERROR CRÍTICO: Tabla de confiabilidad vacía.\n")
  } else {
    # Umbrales de referencia explícitos para defensa
    cat("Criterios de Evaluación (Nunnally / High Stakes):\n")
    cat("   * Alfa > 0.90 : Excelente (Apto para decisiones individuales críticas)\n")
    cat("   * Alfa > 0.80 : Bueno (Estándar típico en certificación)\n")
    cat("   * Alfa < 0.70 : DEFICIENTE (No apto para high-stakes)\n\n")

    cat(paste0(
      pad_str("FORMA", 10),
      pad_str("N_ITEMS", 8),
      pad_str("ALFA", 8),
      pad_str("SEM", 8),
      pad_str("IC_95%_SCORE", 15),
      "DIAGNÓSTICO\n"
    ))
    cat(paste0(strrep("-", 80), "\n"))

    # Optimización: Vectorización de diagnósticos y cálculos
    rel_df <- rel_df %>%
      mutate(
        quality = case_when(
          is.na(ALPHA) ~ "DEFICIENTE",
          ALPHA >= 0.90 ~ "EXCELENTE",
          ALPHA >= 0.80 ~ "BUENO",
          ALPHA >= 0.70 ~ "ACEPTABLE",
          TRUE ~ "DEFICIENTE"
        ),
        sem_val = ifelse(is.na(SEM), 0, SEM),
        ci_str = paste0("+/- ", sprintf("%.1f", 1.96 * sem_val), " pts")
      )

    cat(paste0(
      vapply(rel_df$FORMA, pad_str, width = 10, FUN.VALUE = character(1), USE.NAMES = FALSE),
      vapply(rel_df$N_ITEMS, pad_str, width = 8, FUN.VALUE = character(1), USE.NAMES = FALSE),
      vapply(ifelse(is.na(rel_df$ALPHA), NA_character_, sprintf("%.3f", rel_df$ALPHA)), pad_str, width = 8, FUN.VALUE = character(1), USE.NAMES = FALSE),
      vapply(ifelse(is.na(rel_df$sem_val), NA_character_, sprintf("%.2f", rel_df$sem_val)), pad_str, width = 8, FUN.VALUE = character(1), USE.NAMES = FALSE),
      vapply(rel_df$ci_str, pad_str, width = 15, FUN.VALUE = character(1), USE.NAMES = FALSE),
      rel_df$quality, "\n",
      collapse = ""
    ))

    # Nota explicativa para audiencia no técnica
    avg_sem <- mean(rel_df$SEM, na.rm = TRUE)
    cat(paste0("\nNOTA FORENSE:\n"))
    cat(paste0("El Error Estándar (SEM) promedio es ", round(avg_sem, 2), ". "))
    cat("Si un sustentante obtiene X puntos,\nsu puntaje verdadero tiene un 95% de probabilidad de estar entre ")
    cat(paste0("X - ", round(1.96 * avg_sem, 1), " y X + ", round(1.96 * avg_sem, 1), ".\n"))
  }

  # ============================================================================
  # SECCIÓN 2: DIMENSIONALIDAD (VALIDACIÓN DE SUPUESTOS)
  # ============================================================================
  print_section("2. ANÁLISIS DE DIMENSIONALIDAD (EVIDENCIA DE UNIDIMENSIONALIDAD)")

  dim_df <- ctt_results$dimensionality

  if (is.null(dim_df) || nrow(dim_df) == 0) {
    cat(">> AVISO: No hay datos de dimensionalidad. Asumiendo unidimensionalidad sin evidencia.\n")
  } else {
    cat("Supuesto: El Equating IRT/CTT requiere que la prueba mida un atributo dominante.\n")
    cat("Criterio: Ratio 1er/2do Eigenvalor > 3.0 o Varianza Explicada Factor 1 > 20%.\n\n")

    cat(paste0(
      pad_str("FORMA", 10),
      pad_str("RATIO_1:2", 12),
      pad_str("VAR_EXP_1", 12),
      pad_str("ESTATUS", 15),
      "CONCLUSIÓN\n"
    ))
    cat(paste0(strrep("-", 80), "\n"))

    # Optimización: Vectorización de diagnósticos de dimensionalidad
    dim_df <- dim_df %>%
      mutate(
        status_flag = case_when(
          is.na(RATIO_1_2) ~ "ALERTA",
          RATIO_1_2 > 3.0 ~ "OK",
          RATIO_1_2 < 3.0 & !is.na(VAR_EXP_1) & VAR_EXP_1 > 0.20 ~ "REV. CONTENIDO",
          TRUE ~ "ALERTA"
        ),
        diag_txt = case_when(
          is.na(RATIO_1_2) ~ "MULTIDIMENSIONAL (?)",
          RATIO_1_2 > 4.0 ~ "UNIDIM. CLARA",
          RATIO_1_2 > 3.0 ~ "UNIDIM. ESENCIAL",
          RATIO_1_2 < 3.0 & !is.na(VAR_EXP_1) & VAR_EXP_1 > 0.20 ~ "UNIDIM. DÉBIL",
          TRUE ~ "MULTIDIMENSIONAL (?)"
        ),
        var_str = ifelse(!is.na(VAR_EXP_1), sprintf("%.1f%%", VAR_EXP_1 * 100), "N/A")
      )

    cat(paste0(
      vapply(dim_df$FORMA, pad_str, width = 10, FUN.VALUE = character(1), USE.NAMES = FALSE),
      vapply(ifelse(is.na(dim_df$RATIO_1_2), NA_character_, sprintf("%.2f", dim_df$RATIO_1_2)), pad_str, width = 12, FUN.VALUE = character(1), USE.NAMES = FALSE),
      vapply(dim_df$var_str, pad_str, width = 12, FUN.VALUE = character(1), USE.NAMES = FALSE),
      vapply(dim_df$status_flag, pad_str, width = 15, FUN.VALUE = character(1), USE.NAMES = FALSE),
      dim_df$diag_txt, "\n",
      collapse = ""
    ))

    cat("\nInterpretación:\n")
    cat("- Ratio 1:2 bajo (< 3.0) sugiere que un segundo factor (ej. velocidad, lectura) está contaminando la medida.\n")
  }

  cat("\n>>> FIN DEL REPORTE B <<<\n")


  debug(paste("Reporte de Estructura Interna generado:", report_file))
}
