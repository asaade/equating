# ==============================================================================
# MÓDULO D: EVIDENCIA DE EQUIDAD E IMPACTO (FAIRNESS)
# Versión: v1.0
# Responsabilidad: Auditoría de Sesgo Poblacional, DIF y Brechas (Effect Size).
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, dplyr, tidyr, stringr, scales, ggrepel)

if (!exists("execute_safely")) source("00_common.R")

# ==============================================================================
# 1. TEMA GRÁFICO: EQUIDAD (Estilo Técnico)
# ==============================================================================
theme_audit_fairness <- function() {
  theme_bw(base_size = 12) +
    theme(
      text = element_text(color = "black", family = "sans"),
      plot.title = element_text(face = "bold", size = 12, margin = margin(b = 5)),
      plot.subtitle = element_text(size = 10, color = "#2c3e50", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "#666666", hjust = 0, face = "italic"),
      panel.grid.major.x = element_line(color = "#E0E0E0", linewidth = 0.5), # Grid vertical para dot plots
      panel.grid.major.y = element_blank(), # Limpiar grid horizontal para listas
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", linewidth = 1),
      strip.background = element_rect(fill = "#F0F0F0", color = "black"),
      strip.text = element_text(face = "bold", size = 9),
      legend.position = "bottom",
      legend.key = element_rect(fill = "white")
    )
}

# ==============================================================================
# 2. HELPER: CÁLCULO DE D DE COHEN (Manual)
# ==============================================================================
calculate_cohens_d <- function(group1, group2) {
  m1 <- mean(group1, na.rm = TRUE)
  m2 <- mean(group2, na.rm = TRUE)
  s1 <- sd(group1, na.rm = TRUE)
  s2 <- sd(group2, na.rm = TRUE)
  n1 <- sum(!is.na(group1))
  n2 <- sum(!is.na(group2))

  # SD Pooled
  s_pool <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))

  if (is.na(s_pool) || s_pool == 0) {
    return(0)
  }

  d <- (m1 - m2) / s_pool
  return(d)
}

# ==============================================================================
# 3. VISUALIZACIÓN DE IMPACTO (RAINCLOUD PLOTS - MONOCHROME)
# ==============================================================================

plot_demographic_raincloud <- function(merged_data, demo_col, score_col = "Eq_Global_CTT") {
  # Validación
  if (!all(c(demo_col, score_col) %in% names(merged_data))) {
    return(NULL)
  }

  # Limpieza
  plot_data <- merged_data |>
    dplyr::filter(!is.na(!!sym(demo_col)), !is.na(!!sym(score_col))) |>
    dplyr::rename(Group = !!sym(demo_col), Score = !!sym(score_col)) |>
    dplyr::mutate(Group = as.factor(Group))

  # Estadísticas para etiqueta (N y Media)
  stats <- plot_data |>
    dplyr::group_by(Group) |>
    dplyr::summarise(
      Mean = mean(Score),
      N = n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(Label = sprintf("N=%d\n\U03BC=%.1f", N, Mean))

  ggplot(plot_data, aes(x = Group, y = Score)) +
    # 1. VIOLÍN (Distribución de fondo - Gris claro)
    geom_violin(aes(fill = Group), alpha = 0.3, color = NA, trim = FALSE, scale = "width") +

    # 2. BOXPLOT (Cuartiles - Estricto B/N)
    geom_boxplot(width = 0.15, fill = "white", color = "black", outlier.shape = NA, size = 0.4) +

    # 3. MEDIA (Punto Sólido)
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +

    # 4. ETIQUETAS DE GRUPO
    geom_text(
      data = stats, aes(y = min(plot_data$Score) - 2, label = Label),
      size = 3, color = "black", vjust = 1, family = "sans"
    ) +
    scale_fill_grey(start = 0.8, end = 0.6) + # Escala de grises sutil
    coord_flip() + # Horizontal para mejor lectura de etiquetas largas

    labs(
      title = paste("Análisis de Impacto Demográfico:", demo_col),
      subtitle = "Distribución de Puntuaciones Equiparadas por Subgrupo",
      x = NULL, y = "Puntuación Equiparada",
      caption = "Rombo Negro: Media. Barra blanca: Rango Intercuartil (Q1-Q3).\nSombra gris: Densidad de frecuencia."
    ) +
    theme_audit_fairness() +
    theme(legend.position = "none")
}

# ==============================================================================
# 4. DASHBOARD DE BRECHAS (EFFECT SIZE - DOT PLOT)
# ==============================================================================

plot_effect_size_summary <- function(merged_data, demographics, score_col = "Eq_Global_CTT") {
  results_list <- list()

  for (demo in demographics) {
    if (!demo %in% names(merged_data)) next

    # Filtrar NA y validar niveles
    clean_vec <- na.omit(merged_data[[demo]])
    groups <- unique(clean_vec)
    if (length(groups) < 2) next

    # Comparar contra grupo mayoritario (Referencia)
    tbl <- sort(table(clean_vec), decreasing = TRUE)
    ref_group <- names(tbl)[1]

    for (focal_group in names(tbl)[-1]) {
      g_ref <- merged_data[[score_col]][merged_data[[demo]] == ref_group]
      g_foc <- merged_data[[score_col]][merged_data[[demo]] == focal_group]

      d_val <- calculate_cohens_d(g_foc, g_ref) # Focal - Ref

      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = demo,
        Contrast = paste(focal_group, "vs", ref_group),
        Cohen_D = d_val,
        Abs_D = abs(d_val)
      )
    }
  }

  if (length(results_list) == 0) {
    return(NULL)
  }

  plot_df <- do.call(rbind, results_list) |>
    dplyr::mutate(
      # Clasificación de Alerta para Auditoría
      Status_Audit = dplyr::case_when(
        Abs_D >= 0.5 ~ "ALERTA (|d| >= 0.5)",
        Abs_D >= 0.2 ~ "Notoria (|d| >= 0.2)",
        TRUE ~ "Insignificante"
      ),
      Status_Audit = factor(Status_Audit, levels = c("Insignificante", "Notoria (|d| >= 0.2)", "ALERTA (|d| >= 0.5)"))
    )

  ggplot(plot_df, aes(x = Cohen_D, y = Contrast)) +
    # ZONAS DE UMBRAL (Líneas verticales)
    geom_vline(xintercept = 0, color = "black", size = 0.5) +
    geom_vline(xintercept = c(-0.2, 0.2), linetype = "dotted", color = "gray60") +
    geom_vline(xintercept = c(-0.5, 0.5), linetype = "dashed", color = "gray40") +

    # CONECTORES (Lollipop)
    geom_segment(aes(x = 0, xend = Cohen_D, y = Contrast, yend = Contrast), color = "gray70") +

    # PUNTOS (Formas B/N)
    geom_point(aes(shape = Status_Audit, fill = Status_Audit, size = Status_Audit), stroke = 0.8) +
    facet_grid(Variable ~ ., scales = "free_y", space = "free_y") +

    # ESCALAS MANUALES
    scale_shape_manual(values = c("Insignificante" = 21, "Notoria (|d| >= 0.2)" = 21, "ALERTA (|d| >= 0.5)" = 24)) +
    scale_fill_manual(values = c("Insignificante" = "white", "Notoria (|d| >= 0.2)" = "gray80", "ALERTA (|d| >= 0.5)" = "black")) +
    scale_size_manual(values = c("Insignificante" = 2, "Notoria (|d| >= 0.2)" = 2.5, "ALERTA (|d| >= 0.5)" = 3)) +
    scale_x_continuous(limits = c(-1.0, 1.0), breaks = seq(-1, 1, 0.2)) +
    labs(
      title = "Magnitud de Brechas (Effect Size Dashboard)",
      subtitle = "Diferencia Estandarizada (d de Cohen) respecto al grupo mayoritario",
      x = "d de Cohen ( < 0 Favorece Referencia | > 0 Favorece Focal )",
      y = NULL,
      caption = "Líneas punteadas: d=0.2 (Efecto Pequeño). Líneas discontinuas: d=0.5 (Efecto Mediano/Alerta).\nTriángulo Negro: Brecha sustancial que requiere revisión cualitativa."
    ) +
    theme_audit_fairness()
}

# ==============================================================================
# 5. DIF LOLLIPOP (MONOCHROME)
# ==============================================================================

plot_dif_mh_lollipop <- function(dif_results) {
  req_cols <- c("ITEM", "ETS_Delta", "FLAG")
  if (is.null(dif_results) || nrow(dif_results) == 0 || !all(req_cols %in% names(dif_results))) {
    return(NULL)
  }

  plot_data <- dif_results |>
    dplyr::arrange(desc(abs(ETS_Delta))) |>
    dplyr::mutate(
      ITEM = factor(ITEM, levels = unique(ITEM)),
      # Clasificación simplificada B/N
      Flag_Simple = dplyr::case_when(
        grepl("^C", FLAG) ~ "C (Severo)",
        grepl("^B", FLAG) ~ "B (Moderado)",
        TRUE ~ "A (Despreciable)"
      ),
      Is_Flagged = Flag_Simple != "A (Despreciable)"
    )

  ggplot(plot_data, aes(x = ITEM, y = ETS_Delta)) +
    # BANDAS ETS (Líneas horizontales)
    geom_hline(yintercept = c(1, 1.5), linetype = "dotted", color = "gray50") +
    geom_hline(yintercept = c(-1, -1.5), linetype = "dotted", color = "gray50") +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +

    # LOLLIPOP STEM
    geom_segment(aes(x = ITEM, xend = ITEM, y = 0, yend = ETS_Delta),
      color = "gray60", size = 0.5
    ) +

    # PUNTOS (Formas distintas por severidad)
    geom_point(aes(shape = Flag_Simple, fill = Flag_Simple, size = Flag_Simple), stroke = 0.8) +

    # ETIQUETAS PARA ÍTEMS CON FLAG
    ggrepel::geom_text_repel(
      data = subset(plot_data, Is_Flagged),
      aes(label = ITEM),
      size = 3, fontface = "bold", box.padding = 0.5, max.overlaps = 20
    ) +

    # ESCALAS MANUALES
    scale_shape_manual(values = c("A (Despreciable)" = 21, "B (Moderado)" = 22, "C (Severo)" = 24)) +
    scale_fill_manual(values = c("A (Despreciable)" = "white", "B (Moderado)" = "gray60", "C (Severo)" = "black")) +
    scale_size_manual(values = c("A (Despreciable)" = 1.5, "B (Moderado)" = 2.5, "C (Severo)" = 3)) +
    scale_y_continuous(breaks = seq(-3, 3, 0.5)) +
    labs(
      title = "Análisis de Funcionamiento Diferencial (DIF)",
      subtitle = "Magnitud del Sesgo (Delta ETS) por Ítem",
      x = NULL, y = "Delta ETS",
      caption = "Triángulo Negro (C): DIF Severo (|Delta| > 1.5). Cuadrado Gris (B): DIF Moderado.\nValores positivos favorecen al Grupo Focal."
    ) +
    theme_audit_fairness() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

# ==============================================================================
# 6. ORQUESTADOR MÓDULO D
# ==============================================================================

export_module_d_fairness <- function(data_obj, final_scores, config, base_dir) {
  debug(">>> Iniciando Módulo D: Equidad e Impacto")

  # 0. Preparación
  if (is.null(data_obj$raw_dat) || is.null(final_scores)) {
    warn("Faltan datos para análisis de impacto.")
    return()
  }

  merged_df <- final_scores |>
    dplyr::inner_join(data_obj$raw_dat |> dplyr::select(-matches("^P_\\d+"), -any_of(c("FORMA"))), by = "ID")

  # Detección de demográficos (Columnas con pocos niveles)
  potential_demos <- names(merged_df)[sapply(merged_df, function(x) is.character(x) || is.factor(x))]
  potential_demos <- setdiff(potential_demos, c("ID", "FORMA", "Nivel", "Score_Final"))

  # D.1. IMPACTO (RAINCLOUDS)
  execute_safely(
    expr = {
      if (length(potential_demos) > 0) {
        for (var in potential_demos) {
          n_levels <- length(unique(merged_df[[var]]))
          if (n_levels >= 2 && n_levels <= 15) {
            p <- plot_demographic_raincloud(merged_df, var)
            if (!is.null(p)) {
              save_plot_safe(p, file.path(base_dir, "D01_Demographic_Impact.pdf"),
                width = 8, height = 6
              )
            }
          }
        }
      }
    },
    desc = "Gráficos de Impacto Demográfico"
  )

  # D.2. BRECHAS (EFFECT SIZE)
  execute_safely(
    expr = {
      if (length(potential_demos) > 0) {
        p_effect <- plot_effect_size_summary(merged_df, potential_demos)
        if (!is.null(p_effect)) {
          save_plot_safe(p_effect, file.path(base_dir, "D02_Effect_Size_Gaps.pdf"),
            width = 9, height = 7
          )
        }
      }
    },
    desc = "Dashboard de Brechas"
  )

  debug("<<< Módulo D Finalizado.")
}
