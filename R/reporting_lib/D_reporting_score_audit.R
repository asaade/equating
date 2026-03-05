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

  out_dir <- file.path(base_dir)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  forma_id <- unique(final_scores$FORMA)[1]
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
  if ("Raw_Global_CTT" %in% names(final_scores) && "Eq_Global_CTT" %in% names(final_scores)) {
    raw_m <- mean(final_scores$Raw_Global_CTT, na.rm = T)
    eq_m <- mean(final_scores$Eq_Global_CTT, na.rm = T)
    delta <- eq_m - raw_m

    cat(paste0("Media Puntuación Cruda  : ", sprintf("%.2f", raw_m), "\n"))
    cat(paste0("Media Puntuación Escala : ", sprintf("%.2f", eq_m), "\n"))
    cat(paste0("Efecto Neto Equating    : ", sprintf("%+.2f", delta), " puntos\n"))

    if (delta > 0) {
      cat(">> Interpretación: La forma fue más DIFÍCIL que la base (Bonificación).\n")
    } else {
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

    # --- FIX: Detección segura de columnas ---
    # Intentamos mapear las columnas disponibles a LABEL y VALUE

    # Buscar etiqueta
    valid_labels <- intersect(c("LABEL", "TARGET_CUT_RAW_REF", "TARGET_CUT_REF", "NOTE"), names(cuts))
    col_label <- if (length(valid_labels) > 0) valid_labels[1] else NULL

    # Buscar valor
    valid_values <- intersect(c("EQUATED_AT_CUT", "EQUATED", "EST_RAW_CUT", "CUT_SCORE"), names(cuts))
    col_value <- if (length(valid_values) > 0) valid_values[1] else NULL

    # Validar que tenemos columnas y que la columna de puntuación existe
    score_col <- "Eq_Global_CTT"
    has_scores <- score_col %in% names(final_scores)

    if (!is.null(col_label) && !is.null(col_value)) {
      if (has_scores) {
        cat(paste0(pad_str("PUNTO DE CORTE", 25), pad_str("SCORE REQ.", 12), pad_str("APROBADOS (N)", 15), "TASA (%)\n"))
        cat(paste0(strrep("-", 70), "\n"))

        scores <- final_scores[[score_col]]

        # Extraer pares únicos para evitar repeticiones por forma
        cuts_unique <- unique(cuts[, c(col_label, col_value)])
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
      } else {
        cat(paste0("No se encontró la columna de puntuaciones '", score_col, "' para calcular tasas.\n"))
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
  if ("Nivel" %in% names(final_scores)) {
    lvl_counts <- table(final_scores$Nivel)
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

  if ("SEE_Global" %in% names(final_scores)) {
    avg_see <- mean(final_scores$SEE_Global, na.rm = TRUE)
    sd_score <- sd(final_scores$Eq_Global_CTT, na.rm = TRUE)

    # Ratio Ruido/Señal: Qué tanto del SD observado es error
    ratio <- if (sd_score > 0) avg_see / sd_score else NA

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
    # Unir datos demográficos (raw_dat) con puntuaciones (final_scores) usando ID
    # Aseguramos que ID sea del mismo tipo
    final_scores$ID <- as.character(final_scores$ID)
    raw_dat$ID <- as.character(raw_dat$ID)

    # FIX: Usar any_of para evitar error si faltan columnas demográficas
    cols_demog <- c("ID", "SEXO", "REGION")

    merged_scores <- final_scores |>
      dplyr::left_join(raw_dat |> dplyr::select(dplyr::any_of(cols_demog)), by = "ID")

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
          Mean_Eq = mean(Eq_Global_CTT, na.rm = TRUE),
          SD_Eq = sd(Eq_Global_CTT, na.rm = TRUE),
          # Tasa de aprobación
          Pass_Rate = mean(Nivel != "Insuficiente", na.rm = TRUE) * 100
        ) |>
        dplyr::mutate(
          # Diferencia estandarizada
          Std_Diff = (Mean_Eq - mean(data$Eq_Global_CTT, na.rm = T)) / sd(data$Eq_Global_CTT, na.rm = T)
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

      max_pass <- max(stats$Pass_Rate, na.rm = T)
      min_pass <- min(stats$Pass_Rate, na.rm = T)
      if ((max_pass - min_pass) > 10) {
        cat(paste0(
          ">> ALERTA: Brecha de aprobación > 10% detectada en ", group_var,
          " (Posible Impacto Adverso).\n"
        ))
      }
    }

    if ("SEXO" %in% names(merged_scores)) analyze_group(merged_scores, "SEXO")
    if ("REGION" %in% names(merged_scores)) analyze_group(merged_scores, "REGION")
  } else {
    cat("Datos crudos (raw_dat) no disponibles. Se omite análisis demográfico.\n")
  }

  cat("\n>>> FIN DEL REPORTE D <<<\n")
}
