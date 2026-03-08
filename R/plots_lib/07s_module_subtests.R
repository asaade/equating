# ==============================================================================
# MÓDULO S: ANÁLISIS DE SUBTESTS Y DOMINIOS
# Versión: v1.0
# Responsabilidad: Auditoría de validez discriminante, consistencia y EQUIPARACIÓN de dominios.
# Contexto: Los subtests son pruebas independientes con puntajes reportados.
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, dplyr, tidyr, reshape2, scales, ggrepel)

if (!exists("execute_safely")) source("00_common.R")

# ==============================================================================
# 1. TEMA GRÁFICO: SUBTESTS (Estilo Matriz)
# ==============================================================================
theme_audit_subtests <- function() {
  theme_bw(base_size = 12) +
    theme(
      text = element_text(color = "black", family = "sans"),
      plot.title = element_text(face = "bold", size = 12, margin = margin(b = 5)),
      plot.subtitle = element_text(size = 10, color = "#2c3e50", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "#666666", hjust = 0, face = "italic"),
      panel.grid.major = element_line(color = "#E0E0E0", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", linewidth = 1),
      strip.background = element_rect(fill = "#F0F0F0", color = "black"),
      strip.text = element_text(face = "bold", size = 9),
      legend.position = "bottom",
      legend.key = element_rect(fill = "white"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# ==============================================================================
# 2. COMPARATIVA DE CONFIABILIDAD (ALPHA BAR CHART)
# ==============================================================================
plot_subtest_reliability <- function(ctt_results) {
  rel_data <- ctt_results$reliability

  if (!any(c("SCALE", "SUBTEST", "SCALE_ID") %in% names(rel_data))) {
    return(NULL)
  }

  if ("SCALE_ID" %in% names(rel_data)) rel_data$SUBTEST <- rel_data$SCALE_ID

  # Filtro opcional: Si queremos excluir GLOBAL para enfocarnos en subtests
  # rel_data <- rel_data |> filter(SUBTEST != "GLOBAL")

  ggplot(rel_data, aes(x = SUBTEST, y = ALPHA)) +
    geom_col(aes(group = FORMA),
      fill = "gray80", color = "black",
      position = position_dodge(width = 0.8), width = 0.7
    ) +
    geom_text(aes(group = FORMA, label = sprintf("%.2f", ALPHA)),
      position = position_dodge(width = 0.8), vjust = -0.5, size = 3
    ) +

    # Línea de referencia ajustada para subtests (suelen ser más cortos -> alpha menor)
    geom_hline(yintercept = 0.7, linetype = "dashed", color = "black", size = 0.4) +
    annotate("text",
      x = 0.5, y = 0.72, label = "Min. (0.70)",
      hjust = 0, size = 3, fontface = "italic"
    ) +
    scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.1)) +
    labs(
      title = "Consistencia Interna por Dominio",
      subtitle = "Coeficiente Alpha de Cronbach por Subtest y Forma",
      x = NULL, y = "Alpha",
      caption = "Nota: Como los puntajes se reportan, la confiabilidad debe ser suficiente para decisiones individuales (Ideal > 0.80)."
    ) +
    theme_audit_subtests()
}

# ==============================================================================
# 3. MATRIZ DE CORRELACIÓN (SIZE ENCODED)
# ==============================================================================
plot_subtest_correlations <- function(final_scores) {
  score_cols <- grep("^(Eq_|Raw_)", names(final_scores), value = TRUE)

  if (any(grepl("^Eq_", score_cols))) {
    score_cols <- grep("^Eq_", score_cols, value = TRUE)
  }

  # Excluir GLOBAL de la matriz de correlación interna (para ver ortogonalidad de dominios)
  score_cols <- score_cols[!grepl("GLOBAL", toupper(score_cols))]

  clean_names <- gsub("^(Eq_|Raw_)|(_CTT|_Score)$", "", score_cols)

  if (length(score_cols) < 2) {
    return(NULL)
  }

  data_for_cor <- final_scores[, score_cols]
  colnames(data_for_cor) <- clean_names

  cor_mat <- cor(data_for_cor, use = "pairwise.complete.obs")
  melted_cormat <- reshape2::melt(cor_mat)

  ggplot(data = melted_cormat, aes(x = Var1, y = Var2)) +
    geom_tile(fill = "white", color = "gray90") +
    geom_point(aes(size = abs(value)), shape = 21, fill = "gray90", color = "black") +
    geom_text(aes(label = sprintf("%.2f", value)), size = 3.5, fontface = "bold") +
    scale_size_continuous(range = c(5, 15), guide = "none") +
    labs(
      title = "Matriz de Correlación entre Dominios",
      subtitle = "Evidencia de Validez Discriminante (Pearson r)",
      x = NULL, y = NULL,
      caption = "Correlaciones moderadas (0.4 - 0.7) son ideales: indican dominios relacionados pero distintos."
    ) +
    theme_audit_subtests() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_blank()
    )
}

# ==============================================================================
# 4. PERFIL DE DESEMPEÑO (LINE TYPES)
# ==============================================================================
plot_subtest_profiles <- function(final_scores) {
  subtest_cols <- grep("^Eq_", names(final_scores), value = TRUE)
  subtest_cols <- subtest_cols[!grepl("Global", subtest_cols, ignore.case = TRUE)]

  if (length(subtest_cols) == 0) {
    return(NULL)
  }

  long_scores <- final_scores |>
    dplyr::select(FORMA, all_of(subtest_cols)) |>
    tidyr::pivot_longer(cols = all_of(subtest_cols), names_to = "Subtest", values_to = "Score") |>
    dplyr::mutate(Subtest = gsub("^(Eq_|Raw_)|(_CTT|_Score)$", "", Subtest))

  summary_stats <- long_scores |>
    dplyr::group_by(FORMA, Subtest) |>
    dplyr::summarise(Mean_Score = mean(Score, na.rm = TRUE), .groups = "drop")

  n_forms <- length(unique(summary_stats$FORMA))
  shapes_pal <- rep(c(16, 17, 15, 18, 1), length.out = n_forms)
  lt_pal <- rep(c("solid", "dashed", "dotted", "dotdash"), length.out = n_forms)

  ggplot(summary_stats, aes(x = Subtest, y = Mean_Score, group = FORMA)) +
    geom_line(aes(linetype = FORMA), linewidth = 0.8, color = "black") +
    geom_point(aes(shape = FORMA), size = 3, fill = "white", color = "black") +
    scale_shape_manual(values = shapes_pal) +
    scale_linetype_manual(values = lt_pal) +
    labs(
      title = "Perfil de Dificultad Relativa",
      subtitle = "Puntaje Promedio por Dominio y Forma",
      x = NULL, y = "Puntaje Promedio Equiparado",
      caption = "Se busca paralelismo entre líneas. Divergencias indican que una forma fue desproporcionadamente difícil en un dominio específico."
    ) +
    theme_audit_subtests()
}

# ==============================================================================
# 5. AUDITORÍA DE SESGO (BIAS) EN EQUIPARACIÓN (SHIFT PLOT)
# ==============================================================================
plot_subtest_equating_bias <- function(eq_results) {
  # Validación
  if (is.null(eq_results$tables)) {
    return(NULL)
  }

  # Filtrar para obtener solo subtests (excluir GLOBAL) y excluir identidad
  plot_data <- eq_results$tables |>
    dplyr::filter(SCALE_ID != "GLOBAL", SOURCE_FORM != TARGET_FORM) |>
    dplyr::mutate(
      Difference = EQUATED_SCORE - RAW_SCORE,
      Form_Label = paste0(SOURCE_FORM, " -> ", TARGET_FORM)
    )

  if (nrow(plot_data) == 0) {
    return(NULL)
  }

  ggplot(plot_data, aes(x = RAW_SCORE, y = Difference)) +
    # Línea base (Cero - Sin Bias)
    geom_hline(yintercept = 0, size = 0.5, color = "black") +

    # Banda de error (SEE)
    geom_ribbon(aes(ymin = Difference - 2 * SEE, ymax = Difference + 2 * SEE, group = SOURCE_FORM),
      fill = "gray80", alpha = 0.4
    ) +

    # Línea de tendencia (Systematic Bias Adjustment)
    geom_line(aes(linetype = SOURCE_FORM), size = 0.8, color = "black") +

    # Faceting: Filas = Subtest
    facet_wrap(~SCALE_ID, scales = "free") +
    labs(
      title = "Ajuste Sistemático de Equiparación (Bias) por Dominio",
      subtitle = "Magnitud del ajuste (Equiparado - Crudo) para corregir diferencias de dificultad",
      x = "Puntuación Cruda",
      y = "Ajuste / Sesgo Corregido (Puntos)",
      caption = "Este gráfico muestra el 'Sesgo de Forma' que fue eliminado. La banda gris es la incertidumbre (2*SEE).\nValores positivos indican que el subtest era más difícil en la forma original."
    ) +
    theme_audit_subtests()
}

# ==============================================================================
# 6. PERFIL DE ERROR DE EQUIPARACIÓN (SEE PROFILE) - *NUEVO*
# ==============================================================================
plot_subtest_see_profile <- function(eq_results) {
  # Validación
  if (is.null(eq_results$tables)) {
    return(NULL)
  }

  # Filtrar para obtener solo subtests
  plot_data <- eq_results$tables |>
    dplyr::filter(SCALE_ID != "GLOBAL", SOURCE_FORM != TARGET_FORM)

  if (nrow(plot_data) == 0) {
    return(NULL)
  }

  ggplot(plot_data, aes(x = RAW_SCORE, y = SEE)) +
    # Línea base
    geom_hline(yintercept = 0, size = 0.5, color = "black") +

    # Curvas de Error
    geom_line(aes(linetype = SOURCE_FORM), size = 0.8, color = "black") +

    # Faceting
    facet_wrap(~SCALE_ID, scales = "free") +
    labs(
      title = "Perfil de Error de Equiparación (SEE) por Dominio",
      subtitle = "Precisión del enlace de equiparación a lo largo de la escala",
      x = "Puntuación Cruda",
      y = "Error Estándar de Equiparación (SEE)",
      caption = "El SEE cuantifica el error aleatorio introducido por el proceso de equiparación.\nSubtests cortos suelen presentar SEE elevado en los extremos debido a escasez de datos."
    ) +
    theme_audit_subtests()
}

# ==============================================================================
# 7. ORQUESTADOR MÓDULO S
# ==============================================================================
export_module_s_subtests <- function(ctt_results, final_scores, eq_results, config, base_dir) {
  debug(">>> Iniciando Módulo S: Análisis de Subtests")

  # S.1. RELIABILITY
  execute_safely(
    expr = {
      p_rel <- plot_subtest_reliability(ctt_results)
      if (!is.null(p_rel)) {
        save_plot_safe(p_rel, file.path(base_dir, "S01_Subtest_Reliability.pdf"),
          width = 8, height = 6
        )
      }
    },
    desc = "Confiabilidad de Subtests"
  )

  # S.2. CORRELACIONES
  execute_safely(
    expr = {
      p_cor <- plot_subtest_correlations(final_scores)
      if (!is.null(p_cor)) {
        save_plot_safe(p_cor, file.path(base_dir, "S02_Inter_Domain_Correlations.pdf"),
          width = 8, height = 8
        )
      }
    },
    desc = "Matriz de Correlaciones"
  )

  # S.3. PERFILES
  execute_safely(
    expr = {
      p_prof <- plot_subtest_profiles(final_scores)
      if (!is.null(p_prof)) {
        save_plot_safe(p_prof, file.path(base_dir, "S03_Form_Profiles.pdf"),
          width = 10, height = 6
        )
      }
    },
    desc = "Perfiles de Forma"
  )

  # S.4. SESGO DE EQUIPARACIÓN (BIAS/SHIFT)
  if (!is.null(eq_results)) {
    execute_safely(
      expr = {
        p_bias_sub <- plot_subtest_equating_bias(eq_results)
        if (!is.null(p_bias_sub)) {
          save_plot_safe(p_bias_sub, file.path(base_dir, "S04_Subtest_Equating_Bias.pdf"),
            width = 10, height = 8
          )
        }
      },
      desc = "Sesgo de Equiparación Subtests"
    )
  }

  # S.5. ERROR DE EQUIPARACIÓN (SEE)
  if (!is.null(eq_results)) {
    execute_safely(
      expr = {
        p_see_sub <- plot_subtest_see_profile(eq_results)
        if (!is.null(p_see_sub)) {
          save_plot_safe(p_see_sub, file.path(base_dir, "S05_Subtest_SEE_Profile.pdf"),
            width = 10, height = 8
          )
        }
      },
      desc = "Error Equiparación (SEE) Subtests"
    )
  }

  debug("<<< Módulo S Finalizado.")
}
