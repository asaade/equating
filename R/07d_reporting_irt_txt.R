# R/08a_reporting_irt_txt.R
# Responsabilidad: Generación de Evidencia Psicométrica, Auditoría y Reportes Técnicos IRT
# Versión: v5.1 (Soporte para Q3, M2 y Varianza Explicada)
# Dependencias: Ninguna externa (usa base y dplyr/tidyr si están cargados)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, readr, tidyr, moments, utils, stats)

# ==============================================================================
# HELPER: Escritura Segura de Archivos
# ==============================================================================

internal_save_csv <- function(data, filepath) {
  tryCatch(
    {
      dir.create(dirname(filepath), recursive = TRUE, showWarnings = FALSE)
      utils::write.csv(data, file = filepath, row.names = FALSE, na = "")
      debug(sprintf("  -> Guardado: %s", basename(filepath)))
    },
    error = function(e) {
      error(sprintf("Error guardando %s: %s", basename(filepath), e$message))
    }
  )
}

# ==============================================================================
# 1. EVIDENCIA DE CALIBRACIÓN (Banco de Ítems + CTT Check + Fit Avanzado)
# ==============================================================================

#' Calcula estadísticos clásicos en matrices dispersas y los compara con IRT
calculate_ctt_checks <- function(model_obj, params_df) {
  dat <- model_obj@Data$data

  # 1. P-values (Dificultad Clásica)
  p_vals <- colMeans(dat, na.rm = TRUE)

  # 2. Correlación Punto-Biserial
  scores <- mirt::fscores(model_obj, method = "EAP", full.scores = TRUE, verbose = FALSE)
  theta <- scores[, 1]

  r_bis <- cor(dat, theta, use = "pairwise.complete.obs") %>% as.vector()

  ctt_df <- data.frame(
    ITEM = colnames(dat),
    CTT_p = round(p_vals, 3),
    CTT_rbis = round(r_bis, 3)
  )

  merged <- params_df %>%
    dplyr::inner_join(ctt_df, by = "ITEM") %>%
    dplyr::mutate(
      FLAG_KEY_ERROR = CTT_rbis < 0,
      FLAG_LOW_DISC  = CTT_rbis < 0.15 & CTT_rbis >= 0,
      FLAG_MISMATCH  = (CTT_p > 0.8 & IRT_b > 1.0) | (CTT_p < 0.2 & IRT_b < -1.0)
    ) %>%
    dplyr::select(ITEM, IRT_a, IRT_b, CTT_p, CTT_rbis, FLAG_KEY_ERROR, FLAG_LOW_DISC, FLAG_MISMATCH, everything())

  return(merged)
}

export_calibration_evidence <- function(model_results, out_dir) {
  calib_dir <- file.path(out_dir, "01_CALIBRATION")
  diag <- model_results$diagnostics

  # A. Parámetros Maestros + Chequeo CTT
  if (!is.null(model_results$parameters) && !is.null(model_results$model_obj)) {
    tryCatch(
      {
        debug("  > Calculando estadísticas clásicas (CTT) para validación cruzada...")
        params_enriched <- calculate_ctt_checks(model_results$model_obj, model_results$parameters)
        internal_save_csv(params_enriched, file.path(calib_dir, "IRT_vs_CTT_Quality_Check.csv"))

        key_errors <- params_enriched %>% dplyr::filter(FLAG_KEY_ERROR == TRUE)
        if (nrow(key_errors) > 0) {
          warn(sprintf("¡ALERTA! Se detectaron %d ítems con correlación negativa.", nrow(key_errors)))
          internal_save_csv(key_errors, file.path(calib_dir, "CRITICAL_Key_Errors_Alert.csv"))
        }
      },
      error = function(e) {
        warn(paste("Fallo cálculo CTT:", e$message))
        internal_save_csv(model_results$parameters, file.path(calib_dir, "IRT_Item_Parameters_Basic.csv"))
      }
    )
  }

  # B. Función de Información del Test (TIF)
  if (!is.null(diag$test_information)) {
    internal_save_csv(diag$test_information, file.path(calib_dir, "IRT_Test_Information_CSEM.csv"))
  }

  # C. Estadísticos de Ajuste por Ítem (S-X2 o Infit/Outfit)
  if (!is.null(diag$fit_statistics)) {
    fit_stats <- diag$fit_statistics
    if ("Infit_MSQ" %in% names(fit_stats)) {
      fit_stats <- fit_stats %>%
        dplyr::mutate(
          INTERPRETATION = case_when(
            Infit_MSQ > 1.4 ~ "Underfit (Ruido)",
            Infit_MSQ < 0.6 ~ "Overfit (Redundancia)",
            TRUE ~ "Aceptable"
          )
        )
    }
    internal_save_csv(fit_stats, file.path(calib_dir, "IRT_Item_Fit_Statistics.csv"))
  }

  # D. Dimensionalidad y Varianza Explicada
  if (!is.null(diag$dimensionality)) {
    internal_save_csv(diag$dimensionality, file.path(calib_dir, "IRT_Dimensionality_Eigenvalues.csv"))
  }
  if (!is.null(diag$variance_explained)) {
    internal_save_csv(diag$variance_explained, file.path(calib_dir, "IRT_Variance_Explained_Factor.csv"))
  }

  # E. Independencia Local (Q3 de Yen)
  if (!is.null(diag$local_independence)) {
    q3_data <- diag$local_independence
    # Guardar solo si hay datos reales (no solo el mensaje de éxito)
    if ("Q3" %in% names(q3_data) || nrow(q3_data) > 0) {
      internal_save_csv(q3_data, file.path(calib_dir, "IRT_Local_Independence_Q3.csv"))
    }
  }

  # F. Ajuste Global (M2)
  if (!is.null(diag$global_fit)) {
    internal_save_csv(diag$global_fit, file.path(calib_dir, "IRT_Global_Fit_M2.csv"))
  }
}

# ==============================================================================
# 2. EVIDENCIA DE EQUIPARACIÓN (Drift Analysis)
# ==============================================================================

export_equating_evidence <- function(model_results, out_dir) {
  drift_dir <- file.path(out_dir, "02_EQUATING_DRIFT")
  drift_obj <- model_results$anchor_validation

  if (is.null(drift_obj)) {
    debug("No se encontró análisis de Drift (Anclaje no activo o sin históricos).")
    return()
  }

  if (!is.null(drift_obj$full_comparison)) {
    full_comp <- drift_obj$full_comparison %>% dplyr::mutate(across(where(is.numeric), ~ round(., 4)))
    internal_save_csv(full_comp, file.path(drift_dir, "IRT_Anchor_Drift_Detailed.csv"))
  }

  if (!is.null(drift_obj$flagged_items) && nrow(drift_obj$flagged_items) > 0) {
    flagged <- drift_obj$flagged_items %>% dplyr::mutate(across(where(is.numeric), ~ round(., 4)))
    internal_save_csv(flagged, file.path(drift_dir, "IRT_Anchor_Drift_ALERTS.csv"))
  }
}

# ==============================================================================
# 3. EVIDENCIA DE PUNTUACIONES (Form Equivalence & Scoring)
# ==============================================================================

export_scoring_evidence <- function(scores_df, out_dir) {
  score_dir <- file.path(out_dir, "03_SCORES_QUALITY")

  if (is.null(scores_df)) {
    return()
  }

  if (!"FORMA" %in% names(scores_df)) scores_df$FORMA <- "GLOBAL"

  # A. Matriz de Equivalencia de Formas
  form_stats <- scores_df %>%
    dplyr::group_by(FORMA) %>%
    dplyr::summarise(
      N_Examinees = n(),
      Mean_Theta = round(mean(Theta, na.rm = TRUE), 3),
      SD_Theta = round(sd(Theta, na.rm = TRUE), 3),
      Min_Theta = round(min(Theta, na.rm = TRUE), 3),
      Max_Theta = round(max(Theta, na.rm = TRUE), 3),
      Mean_SEM = round(mean(SE_Theta, na.rm = TRUE), 3),
      Rel_Marginal_Est = round((var(Theta, na.rm = TRUE) - mean(SE_Theta^2, na.rm = TRUE)) / var(Theta, na.rm = TRUE), 3)
    ) %>%
    dplyr::mutate(Rel_Marginal_Est = pmax(0, Rel_Marginal_Est))

  internal_save_csv(form_stats, file.path(score_dir, "IRT_Form_Equivalence_Matrix.csv"))

  # B. Resumen Estadístico Global
  stats_theta <- scores_df %>%
    dplyr::summarise(
      N = n(),
      Mean_Theta = mean(Theta, na.rm = TRUE),
      SD_Theta = sd(Theta, na.rm = TRUE),
      Mean_SE = mean(SE_Theta, na.rm = TRUE)
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(., 3)))

  internal_save_csv(stats_theta, file.path(score_dir, "IRT_Score_Descriptives_Global.csv"))

  # C. Person Fit
  if ("Zh" %in% names(scores_df)) {
    zh_stats <- scores_df %>%
      dplyr::group_by(FORMA) %>%
      dplyr::summarise(
        Mean_Zh = round(mean(Zh, na.rm = TRUE), 2),
        SD_Zh = round(sd(Zh, na.rm = TRUE), 2),
        Pct_Misfit_Under = round(mean(Zh < -2.0, na.rm = TRUE) * 100, 1),
        Pct_Misfit_Over = round(mean(Zh > 2.0, na.rm = TRUE) * 100, 1)
      )
    internal_save_csv(zh_stats, file.path(score_dir, "IRT_Person_Fit_Analysis_ByForm.csv"))
  }
}

# ==============================================================================
# 4. MEMORANDO TÉCNICO DE AUDITORÍA
# ==============================================================================

generate_audit_memo <- function(model_results, scores_df, config, out_dir) {
  memo_path <- file.path(out_dir, "IRT_Technical_Audit_Memo.txt")
  diag <- model_results$diagnostics

  tryCatch(
    {
      con <- file(memo_path, open = "wt")
      sink(con, type = "output")
      on.exit({
        sink(type = "output")
        close(con)
      })

      cat("==============================================================================\n")
      cat(" AUDITORÍA TÉCNICA: CALIBRACIÓN Y EQUIPARACIÓN IRT\n")
      cat(" FECHA:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
      cat("==============================================================================\n\n")

      # 1. Configuración
      cat("1. CONFIGURACIÓN DEL MODELO\n")
      cat("   Modelo:", config$mirt$model %||% "2PL", "\n")
      cat("   Métrica D:", model_results$D_used, "\n")
      cat("   Convergencia:", ifelse(model_results$model_obj@OptimInfo$converged, "EXITOSA", "FALLIDA"), "\n")
      cat("   Log-Likelihood:", round(model_results$model_obj@Fit$logLik, 2), "\n\n")

      # 2. Confiabilidad
      cat("2. PRECISIÓN DE LA MEDIDA (GLOBAL)\n")
      if (!is.null(scores_df)) {
        rel_marginal <- attr(scores_df, "marginal_reliability")
        if (is.null(rel_marginal)) {
          var_th <- var(scores_df$Theta, na.rm = TRUE)
          mean_se2 <- mean(scores_df$SE_Theta^2, na.rm = TRUE)
          rel_marginal <- round((var_th - mean_se2) / var_th, 4)
        }
        cat("   Confiabilidad Marginal Empírica:", rel_marginal, "\n")
        cat("   Error Estándar Promedio (SE):", round(mean(scores_df$SE_Theta, na.rm = TRUE), 3), "\n")
      }
      cat("\n")

      # 3. Drift
      cat("3. ANÁLISIS DE ESTABILIDAD DE ANCLAJE (DRIFT)\n")
      drift <- model_results$anchor_validation
      if (!is.null(drift)) {
        cat("   Ítems Analizados:", drift$metrics$n_anchors, "\n")
        cat("   Ítems con Drift (>0.3):", drift$metrics$n_flagged, "\n")
        cat("   Shift Poblacional (Constant):", round(drift$metrics$shift_constant, 4), "\n")
        if (drift$metrics$n_flagged > 0) {
          cat("   [ALERTA] Revisar 'IRT_Anchor_Drift_ALERTS.csv'.\n")
        } else {
          cat("   [OK] Parámetros estables.\n")
        }
      } else {
        cat("   No se realizó análisis de Drift.\n")
      }
      cat("\n")

      # 4. Diagnósticos Avanzados (Nuevo en v5.1)
      cat("4. DIAGNÓSTICOS AVANZADOS (INDEPENDENCIA Y AJUSTE)\n")

      # Ajuste Global M2
      if (!is.null(diag$global_fit)) {
        m2 <- diag$global_fit
        if (!is.na(m2$M2)) {
          cat("   [M2 Global Fit]\n")
          cat("     M2:", round(m2$M2, 2), "| df:", m2$df, "| p-val:", round(m2$p, 3), "\n")
          cat("     RMSEA:", round(m2$RMSEA, 3), "\n")
        } else {
          cat("   [M2 Global Fit] No calculado o fallido (Posiblemente datos muy dispersos).\n")
        }
      }

      # Q3 Yen
      if (!is.null(diag$local_independence)) {
        q3 <- diag$local_independence
        if ("Q3" %in% names(q3)) {
          cat(sprintf("   [Independencia Local] ALERTA: %d pares de ítems con Q3 > 0.2 detectados.\n", nrow(q3)))
          cat("     Ver 'IRT_Local_Independence_Q3.csv'.\n")
        } else {
          cat("   [Independencia Local] OK. Sin violaciones detectadas (Q3 < 0.2).\n")
        }
      }

      # Varianza Explicada
      if (!is.null(diag$variance_explained)) {
        ve <- diag$variance_explained
        cat(sprintf("   [Unidimensionalidad] Varianza Explicada: %.1f%%\n", ve$Prop_Var * 100))
      }

      # 5. Calidad CTT
      cat("\n5. CONTROLES DE CALIDAD CLÁSICA (CTT)\n")
      cat("   Se ha generado 'IRT_vs_CTT_Quality_Check.csv'.\n")

      cat("\n==============================================================================\n")
      debug("Memorando técnico generado.")
    },
    error = function(e) {
      error(paste("Error generando memo:", e$message))
    }
  )
}

# ==============================================================================
# ENTRY POINT PRINCIPAL
# ==============================================================================

generate_irt_reports <- function(config, irt_results, final_scores) {
  debug(">>> Generando Reportes y Evidencia IRT (v5.1)...")

  base_dir <- file.path(config$project$output_dir, "IRT_REPORTS")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

  model_res <- if ("model" %in% names(irt_results)) irt_results$model else irt_results

  if (is.null(model_res) || is.null(model_res$parameters)) {
    warn("CRITICAL: Resultados del modelo IRT vacíos. Abortando reportes.")
    return()
  }

  export_calibration_evidence(model_res, base_dir)
  export_equating_evidence(model_res, base_dir)
  export_scoring_evidence(final_scores, base_dir)
  generate_audit_memo(model_res, final_scores, config, base_dir)

  if (!is.null(model_res$model_obj)) {
    tryCatch(
      {
        rds_path <- file.path(base_dir, "04_BINARY_MODEL", "mirt_model_calibrated.rds")
        dir.create(dirname(rds_path), recursive = TRUE, showWarnings = FALSE)
        saveRDS(model_res$model_obj, rds_path)
        debug("  -> Objeto mirt guardado (RDS).")
      },
      error = function(e) warn(paste("Error guardando RDS:", e$message))
    )
  }

  debug("✅ Generación de reportes IRT finalizada.")
}
