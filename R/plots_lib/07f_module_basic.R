# ==============================================================================
# SISTEMA DE REPORTES GRÁFICOS PSICOMÉTRICOS (CTT & EQUATING)
# Versión: 2.7 (Reference Cuts Fix)
# Responsabilidad: Generación de evidencias gráficas para auditoría técnica.
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, dplyr, ggrepel, cowplot, scales, gridExtra, tidyr)

# ==============================================================================
# 0. TEMA GRÁFICO "AUDIT MONOCHROME"
# ==============================================================================
theme_audit <- function() {
  theme_bw(base_size = 11) +
    theme(
      text = element_text(color = "black"),
      plot.title = element_blank(),
      plot.subtitle = element_text(size = 10, color = "#404040", face = "bold"),
      plot.caption = element_text(size = 8, color = "#666666", hjust = 0),
      panel.grid.major = element_line(color = "#E5E5E5", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", linewidth = 0.8),
      strip.background = element_rect(fill = "#F0F0F0", color = "black"),
      strip.text = element_text(face = "bold", size = 10),
      axis.title = element_text(face = "bold", size = 10),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.box.background = element_rect(color = "black", linewidth = 0.2)
    )
}

# ==============================================================================
# 1. MÓDULO A: ANÁLISIS DE ÍTEMS
# ==============================================================================

plot_item_map_multiform <- function(stats_df) {
  if (!"FORMA" %in% names(stats_df)) stats_df$FORMA <- "UNIQUE"

  plot_data <- stats_df |>
    dplyr::mutate(
      Status = dplyr::case_when(
        P_BIS < 0 ~ "FATAL (Invertido)",
        P_BIS < 0.15 ~ "POOR (Revisar)",
        P_VAL < 0.2 | P_VAL > 0.9 ~ "EXTREME (Dif/Fácil)",
        TRUE ~ "OK"
      ),
      Label = ifelse(P_BIS < 0.15 | P_BIS < 0, ITEM, "")
    )

  ggplot(plot_data, aes(x = P_VAL, y = P_BIS)) +
    annotate("rect",
      xmin = 0, xmax = 1, ymin = -1, ymax = 0.15,
      fill = "#EEEEEE", alpha = 0.5
    ) +
    geom_hline(yintercept = 0.15, linetype = "dashed", color = "gray50") +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_point(aes(shape = Status, fill = Status), size = 2, alpha = 0.6) +
    ggrepel::geom_text_repel(aes(label = Label), size = 2.5, max.overlaps = 30, box.padding = 0.3) +
    facet_wrap(~FORMA, ncol = 2) +
    scale_shape_manual(values = c("OK" = 21, "EXTREME" = 21, "POOR" = 24, "FATAL" = 25)) +
    scale_fill_manual(values = c("OK" = "white", "EXTREME" = "gray80", "POOR" = "gray40", "FATAL" = "black")) +
    labs(
      x = "Dificultad (Proporción de Aciertos 'p')",
      y = "Discriminación (Correlación Punto-Biserial)",
      subtitle = "Diagnóstico de Calidad de Ítems (CTT)",
      caption = "Nota: Ítems en zona sombreada tienen discriminación insuficiente (< 0.15)."
    ) +
    theme_audit() +
    theme(legend.position = "bottom")
}

plot_distractor_trace <- function(distractor_data, item_id, key) {
  item_data <- distractor_data |> dplyr::filter(ITEM == item_id)
  if (nrow(item_data) == 0) {
    return(NULL)
  }

  cols_req <- c("PROP_LOW", "PROP_MID", "PROP_HIGH")

  plot_df <- item_data |>
    dplyr::select(OPTION, KEY, all_of(cols_req)) |>
    tidyr::pivot_longer(cols = all_of(cols_req), names_to = "Group", values_to = "Prop") |>
    dplyr::mutate(
      Group_Num = dplyr::case_when(grepl("LOW", Group) ~ 1, grepl("MID", Group) ~ 2, grepl("HIGH", Group) ~ 3),
      Is_Key = (OPTION == KEY),
      Line_Type = ifelse(Is_Key, "Key", "Distractor")
    )

  ggplot(plot_df, aes(x = Group_Num, y = Prop, group = OPTION)) +
    geom_line(aes(linetype = Line_Type, color = Line_Type, linewidth = Line_Type)) +
    geom_point(aes(color = Line_Type), size = 2) +
    geom_text(data = subset(plot_df, Group_Num == 3), aes(label = OPTION), hjust = -0.5, size = 3, fontface = "bold") +
    scale_color_manual(values = c("Key" = "black", "Distractor" = "gray60")) +
    scale_linewidth_manual(values = c("Key" = 1.0, "Distractor" = 0.5)) +
    scale_linetype_manual(values = c("Key" = "solid", "Distractor" = "longdash")) +
    scale_x_continuous(breaks = 1:3, labels = c("Bajo", "Medio", "Alto")) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(
      subtitle = paste("Análisis de Opciones: Ítem", item_id, "| Clave:", key),
      x = "Nivel de Desempeño del Sustentante",
      y = "Probabilidad de Selección"
    ) +
    theme_audit() +
    theme(legend.position = "none", plot.margin = unit(c(0.5, 1, 0.5, 0.5), "cm"))
}

# ==============================================================================
# 2. MÓDULO B: ESTRUCTURA INTERNA
# ==============================================================================

plot_raw_score_multiform <- function(final_scores, reliability_df = NULL, ref_form_id = NULL) {
  if (!"Raw_Global_CTT" %in% names(final_scores)) stop("Column 'Raw_Global_CTT' missing")

  stats_by_form <- final_scores |>
    dplyr::group_by(FORMA) |>
    dplyr::summarise(Mean = mean(Raw_Global_CTT, na.rm = TRUE), SD = sd(Raw_Global_CTT, na.rm = TRUE), N = dplyr::n(), .groups = "drop")

  if (!is.null(reliability_df)) {
    stats_by_form <- stats_by_form |> dplyr::left_join(reliability_df |> dplyr::select(FORMA, ALPHA, SEM), by = "FORMA")
  }

  stats_names <- names(stats_by_form)
  stats_by_form <- stats_by_form |>
    dplyr::mutate(Label = paste0(
      "N: ", N, "\nMedia: ", round(Mean, 1), "\nDE: ", round(SD, 1),
      ifelse("ALPHA" %in% stats_names, paste0("\nAlpha: ", round(ALPHA, 3)), ""),
      ifelse("SEM" %in% stats_names, paste0("\nSEM: ", round(SEM, 2)), "")
    ))

  ref_data <- NULL
  if (!is.null(ref_form_id) && ref_form_id %in% final_scores$FORMA) {
    ref_data <- final_scores |> dplyr::filter(FORMA == ref_form_id)
  }

  p <- ggplot(final_scores, aes(x = Raw_Global_CTT)) +
    {
      if (!is.null(ref_data)) {
        geom_density(
          data = ref_data, aes(x = Raw_Global_CTT),
          fill = "#999999", color = NA, alpha = 0.2, inherit.aes = FALSE
        )
      }
    } +
    geom_histogram(aes(y = after_stat(density)), binwidth = 2, fill = "#E0E0E0", color = "gray60", alpha = 0.8) +
    geom_density(color = "black", linewidth = 0.8) +
    geom_vline(data = stats_by_form, aes(xintercept = Mean), linetype = "dashed", linewidth = 0.6) +
    geom_label(data = stats_by_form, aes(x = Inf, y = Inf, label = Label), hjust = 1.1, vjust = 1.1, size = 2.5, fill = "white", alpha = 0.9) +
    facet_wrap(~FORMA, ncol = 1, scales = "free_y") +
    labs(
      x = "Puntuación Cruda (Total de Aciertos)",
      y = "Densidad de Frecuencia",
      subtitle = "Distribución de Puntajes Crudos",
      caption = if (!is.null(ref_data)) paste("Nota: La sombra gris de fondo representa la distribución de la forma de referencia:", ref_form_id) else NULL
    ) +
    theme_audit()

  return(p)
}

# ==============================================================================
# 3. MÓDULO C: EQUIPARACIÓN
# ==============================================================================

plot_delta_drift <- function(drift_details, form_label) {
  p_to_delta <- function(p) {
    p <- pmax(0.001, pmin(0.999, p))
    13 + 4 * qnorm(1 - p)
  }

  plot_data <- drift_details |>
    dplyr::mutate(Delta_Ref = p_to_delta(P_SRC), Delta_New = p_to_delta(P_DEST), Is_Anchor = STATUS == "OK" | STATUS == "KEPT", Label = ifelse(abs(Z_SCORE) > 1.96, ITEM, ""))

  ggplot(plot_data, aes(x = Delta_Ref, y = Delta_New)) +
    geom_abline(intercept = 1.5, slope = 1, linetype = "dashed", color = "gray70") +
    geom_abline(intercept = -1.5, slope = 1, linetype = "dashed", color = "gray70") +
    geom_abline(intercept = 0, slope = 1, color = "gray40", linewidth = 0.5) +
    geom_point(aes(shape = Is_Anchor, color = abs(Z_SCORE) > 1.96), size = 2, alpha = 0.7) +
    ggrepel::geom_text_repel(aes(label = Label), size = 3, min.segment.length = 0) +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "black")) +
    scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 19)) +
    coord_fixed(ratio = 1, xlim = c(6, 20), ylim = c(6, 20)) +
    labs(
      x = "Dificultad Delta (Referencia)",
      y = paste("Dificultad Delta (", form_label, ")"),
      subtitle = "Análisis de Deriva (Drift) en Ítems Ancla",
      caption = "Puntos vacíos: Deriva significativa detectada (|Z| > 1.96). Banda: +/- 1.5 Delta."
    ) +
    theme_audit() +
    theme(legend.position = "none")
}

plot_equating_function_multiform <- function(eq_table, cuts_df = NULL) {
  plot_data <- eq_table |>
    dplyr::filter(SCALE_ID == "GLOBAL") |>
    dplyr::arrange(SOURCE_FORM, RAW_SCORE)
  if (nrow(plot_data) == 0) {
    return(NULL)
  }

  p_func <- ggplot(plot_data, aes(x = RAW_SCORE, y = EQUATED_SCORE)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray50") +
    geom_line(aes(group = SOURCE_FORM), linewidth = 0.8) +
    {
      if (!is.null(cuts_df)) geom_hline(data = cuts_df, aes(yintercept = EQUATED_AT_CUT), color = "black", linetype = "dashed", alpha = 0.5)
    } +
    facet_wrap(~SOURCE_FORM) +
    coord_fixed(ratio = 1) +
    labs(y = "Puntuación Equiparada", x = NULL, subtitle = "Función de Transformación") +
    theme_audit()

  p_see <- ggplot(plot_data, aes(x = RAW_SCORE, y = SEE)) +
    geom_area(aes(group = SOURCE_FORM), fill = "gray80", alpha = 0.6, color = NA) +
    geom_line(aes(group = SOURCE_FORM), color = "black", linewidth = 0.5) +
    {
      if (!is.null(cuts_df)) geom_vline(data = cuts_df, aes(xintercept = EST_RAW_CUT), color = "black", linetype = "dotted", alpha = 0.5)
    } +
    facet_wrap(~SOURCE_FORM) +
    labs(x = "Puntuación Cruda", y = "SEE", subtitle = "Error Estándar de Equiparación", caption = "Líneas verticales punteadas indican cortes.") +
    theme_audit()

  cowplot::plot_grid(p_func, p_see, ncol = 1, align = "v", rel_heights = c(2, 1))
}

plot_bias_with_bands <- function(eq_table) {
  plot_data <- eq_table |> dplyr::mutate(Diff = EQUATED_SCORE - RAW_SCORE)

  ggplot(plot_data, aes(x = RAW_SCORE, y = Diff)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -0.5, ymax = 0.5, fill = "#F0F0F0", alpha = 0.5) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    geom_ribbon(aes(ymin = Diff - 2 * SEE, ymax = Diff + 2 * SEE), fill = "gray80", alpha = 0.4) +
    geom_line(color = "black", linewidth = 0.8) +
    facet_wrap(~SOURCE_FORM) +
    labs(
      x = "Puntuación Cruda", y = "Sesgo (Equiparada - Cruda)",
      subtitle = "Evaluación de Sesgo e Incertidumbre (RMSD Check)",
      caption = "Sombra gris: Incertidumbre (2*SEE). Banda central clara: Tolerancia práctica (+/- 0.5)."
    ) +
    theme_audit()
}

plot_smoothing_check <- function(smoothing_data) {
  ggplot(smoothing_data, aes(x = Score)) +
    geom_segment(aes(xend = Score, y = 0, yend = Raw_Freq), color = "gray70", linewidth = 0.5) +
    geom_point(aes(y = Raw_Freq), color = "gray50", size = 1.5, shape = 1) +
    geom_line(aes(y = Smooth_Freq), color = "black", linewidth = 0.8) +
    facet_wrap(~Type, scales = "free_y") +
    labs(x = "Puntuación", y = "Probabilidad", subtitle = "Diagnóstico de Suavizado", caption = "Negro: Modelo Suavizado. Gris: Datos Observados.") +
    theme_audit()
}

# ==============================================================================
# 4. MÓDULO D: IMPACTO (VIOLÍN ENRIQUECIDO) - REFERENCIA PRIORITARIA
# ==============================================================================

plot_score_comparison_violin <- function(final_scores, cuts_df = NULL, ref_form_id = NULL) {
  col_eq <- "Eq_Global_CTT"
  if (!col_eq %in% names(final_scores)) stop(paste("Column", col_eq, "missing"))

  # 1. Etiquetas de Eje X con N
  n_table <- final_scores |>
    dplyr::count(FORMA) |>
    dplyr::mutate(Label = paste0(FORMA, "\n(N=", n, ")"))

  plot_data <- final_scores |>
    dplyr::mutate(Eq_Global_CTT = round(Eq_Global_CTT)) |>
    dplyr::left_join(n_table, by = "FORMA")

  # 2. PROCESAMIENTO DE CORTES
  cuts_viz <- NULL
  ann_text <- data.frame()

  if (!is.null(cuts_df) && nrow(cuts_df) > 0) {
    col_form <- if ("FORM" %in% names(cuts_df)) "FORM" else "SOURCE_FORM"

    # A. Selección de Cortes: Prioridad a Referencia, sino Redondeo
    cuts_viz <- cuts_df |>
      dplyr::mutate(EQUATED = round(TARGET_CUT_RAW_REF)) |>
      dplyr::select(EQUATED) |>
      dplyr::distinct()

    ## if (!is.null(ref_form_id) && ref_form_id %in% cuts_df[[col_form]]) {
    ##   # Usar cortes de la referencia como fuente de verdad

    ##   cuts_df |> dplyr::filter(!!sym(col_form) == ref_form_id)
    ## } else {
    ##   # Redondear y unificar si no hay referencia explícita
    ##   if ("LABEL" %in% names(cuts_df)) {
    ##     cuts_viz <- cuts_df |>
    ##       dplyr::mutate(EQUATED = round(EQUATED_AT_CUT)) |>
    ##       dplyr::group_by(LABEL) |>
    ##       dplyr::summarise(EQUATED = mean(TARGET_CUT_RAW_REF, na.rm = TRUE), .groups = "drop")
    ##   } else {
    ##     cuts_viz <- cuts_df |>
    ##       dplyr::mutate(EQUATED = round(TARGET_CUT_RAW_REF)) |>
    ##       dplyr::select(EQUATED) |>
    ##       dplyr::distinct()
    ##   }
    ## }

    # B. Calcular Proporciones usando los cortes unificados
    cut_vals <- sort(unique(cuts_viz$EQUATED))
    breaks <- c(-Inf, cut_vals, Inf)

    props <- plot_data |>
      dplyr::group_by(Label) |>
      dplyr::mutate(Level_Group = cut(!!sym(col_eq), breaks = breaks)) |>
      dplyr::count(Level_Group) |>
      dplyr::mutate(Pct = n / sum(n) * 100, Midpoint = NA) |>
      dplyr::ungroup()

    if (!is.null(config$specs$len_fisica)) {
      range_y <- list(0, config$specs$len_fisica)
    } else {
      range_y <- range(plot_data[[col_eq]], na.rm = TRUE)
    }
    for (i in 1:(length(breaks) - 1)) {
      low <- if (is.infinite(breaks[i])) as.numeric(range_y[1]) else as.numeric(breaks[i])
      high <- if (is.infinite(breaks[i + 1])) as.numeric(range_y[2]) else as.numeric(breaks[i + 1])
      props$Midpoint[as.integer(props$Level_Group) == i] <- (low + high) / 2
    }
    ann_text <- props
  }

  p <- ggplot(plot_data, aes(x = Label, y = !!sym(col_eq))) +
    geom_violin(fill = "#F5F5F5", color = "gray40", scale = "width", width = 0.7) +
    geom_boxplot(width = 0.15, fill = "white", outlier.size = 0.5, outlier.alpha = 0.3) +
    {
      if (!is.null(cuts_viz)) geom_hline(data = cuts_viz, aes(yintercept = EQUATED), linetype = "dotted", linewidth = 0.6)
    } +
    {
      if (nrow(ann_text) > 0) {
        geom_text(
          data = ann_text, aes(x = Label, y = Midpoint, label = sprintf("%.1f%%", Pct)),
          color = "#444444", size = 2.8, fontface = "bold", hjust = -0.6
        )
      }
    } +
    labs(
      x = NULL, y = "Puntuación Equiparada",
      subtitle = "Comparativa de Impacto y Distribución de Niveles",
      caption = "Violín: Densidad. Caja: Mediana/Cuartiles. Porcentajes: Proporción de población entre cortes."
    ) +
    theme_audit()

  return(p)
}

plot_scaling_step <- function(final_scores) {
  col_raw <- "Raw_Global_CTT"
  col_final <- "Eq_Global_CTT"
  lookup_table <- final_scores |>
    dplyr::select(FORMA, Raw = !!sym(col_raw), Final = !!sym(col_final)) |>
    dplyr::distinct() |>
    dplyr::arrange(FORMA, Raw)

  ggplot(lookup_table, aes(x = Raw, y = Final)) +
    geom_step(direction = "vh", linewidth = 0.8) +
    facet_wrap(~FORMA) +
    labs(x = "Puntuación Cruda", y = "Puntuación Final", subtitle = "Tabla de Conversión (Escalera)") +
    theme_audit()
}


# ==============================================================================
# 5. FUNCIÓN MAESTRA
# ==============================================================================

export_module_f_basic <- function(ctt_results, eq_results, final_scores, config, base_dir) {
  # Fail-fast validation
  if (missing(final_scores) || !is.data.frame(final_scores)) stop("ERROR: 'final_scores' must be a valid data.frame")

  debug(paste("Generando gráficos en: ", base_dir))

  # Recuperar Forma de Referencia desde config
  ref_form_id <- tryCatch(config$system$reference_form, error = function(e) NULL)

  # A. MAPAS
  if (!is.null(ctt_results$stats)) {
    save_plot_safe(plot_item_map_multiform(ctt_results$stats), file.path(base_dir, "F01_Item_Quadrants_Map.pdf"), width = 10, height = 8)
  }

  # A2. DISTRACTORES
  if (!is.null(ctt_results$distractors)) {
    bad_items <- ctt_results$stats |>
      dplyr::filter(P_BIS < config$thresholds$ctt_pbis_min) |>
      dplyr::pull(ITEM)
    if (length(bad_items) > 0) {
      pdf(file.path(base_dir, "F02_Distractor_Analysis_FLAGS.pdf"), width = 7, height = 5)
      for (it in bad_items) {
        k <- "A"
        if (!is.null(ctt_results$meta)) {
          k_lookup <- ctt_results$meta |>
            dplyr::filter(ITEM_ID == it) |>
            dplyr::pull(KEY)
          if (length(k_lookup) > 0) k <- k_lookup
        }
        p <- plot_distractor_trace(ctt_results$distractors, it, k)
        if (!is.null(p)) print(p)
      }
      dev.off()
    }
  }

  ## # B. HISTOGRAMAS (Con Referencia Overlay)
  p_hist <- plot_raw_score_multiform(final_scores, ctt_results$reliability, ref_form_id)
  save_plot_safe(p_hist, file.path(base_dir, "F03_Raw_Score_Distribution.pdf"), width = 8, height = 8)

  # C. EQUIPARACIÓN
  if (!is.null(eq_results)) {
    if (!is.null(eq_results$drift_details)) {
      save_plot_safe(plot_delta_drift(eq_results$drift_details, "Destino"), file.path(base_dir, "04_Drift_Delta.pdf"), width = 7, height = 7)
    }

    if (!is.null(eq_results$tables)) {
      cuts <- if (!is.null(eq_results$audit_critical)) eq_results$audit_critical |> dplyr::filter(SCALE_ID == "GLOBAL") else NULL
      p_eq <- plot_equating_function_multiform(eq_results$tables, cuts)
      save_plot_safe(p_eq, file.path(base_dir, "F05_Equating_Functions.pdf"), width = 8, height = 10)

      p_bias <- plot_bias_with_bands(eq_results$tables |> dplyr::filter(SCALE_ID == "GLOBAL"))
      save_plot_safe(p_bias, file.path(base_dir, "F06_Equating_Bias_Check.pdf"), width = 8, height = 6)
    }

    if (!is.null(eq_results$smoothing_data)) {
      save_plot_safe(plot_smoothing_check(eq_results$smoothing_data), file.path(base_dir, "F07_Smoothing_Check.pdf"), width = 9, height = 6)
    }
  }

  # D. IMPACTO (Violín Enriquecido con N y %)
  cuts_global <- if (!is.null(eq_results$audit_critical)) eq_results$audit_critical |> dplyr::filter(SCALE_ID == "GLOBAL") else NULL

  save_plot_safe(plot_score_comparison_violin(final_scores, cuts_global), file.path(base_dir, "F08_Final_Score_Violin.pdf"), width = 10, height = 7)
  save_plot_safe(plot_scaling_step(final_scores), file.path(base_dir, "F09_Scaling_Step_Function.pdf"), width = 8, height = 8)

  debug(">>> Gráficos finalizados.")
}
