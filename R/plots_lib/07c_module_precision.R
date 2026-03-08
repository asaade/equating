# ==============================================================================
# MÓDULO C: EVIDENCIA DE PRECISIÓN DE LA PUNTUACIÓN (THE SCORE)
# Versión: v106.1 - Adapter Pattern para datos consolidados
# Responsabilidad: Error Total (TSE), Shift Global y Trazabilidad de Cortes.
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, dplyr, tidyr, cowplot, ggrepel, scales)

if (!exists("execute_safely")) source("R/00_common_base.R")

theme_audit_precision <- function() {
  theme_bw(base_size = 11) +
    theme(
      text = element_text(color = "black", family = "sans"),
      plot.title = element_text(face = "bold", size = 12),
      panel.grid.major = element_line(color = "#E0E0E0", linewidth = 0.5),
      strip.background = element_rect(fill = "#F0F0F0", color = "black"),
      legend.position = "bottom"
    )
}

# ==============================================================================
# HELPER: ADAPTADOR DE TABLAS
# ==============================================================================
adapt_tables_data <- function(input_data) {
  # Caso 1: Input plano consolidado (eq_results$tables)
  if (!is.null(input_data$tables)) {
    df <- input_data$tables
    # Estandarizar nombres
    return(df |> dplyr::rename(FORM = SOURCE_FORM, RAW = RAW_SCORE, EQUATED = EQUATED_SCORE))
  }

  # Caso 2: Input jerárquico (batch_results)
  return(do.call(rbind, lapply(names(input_data), function(fid) {
    res <- input_data[[fid]]
    conc <- res$concordance
    if (is.null(conc)) {
      return(NULL)
    }

    data.frame(
      FORM = fid,
      RAW = conc$scale,
      EQUATED = conc$yx,
      SEE = res$se
    )
  })))
}

# ==============================================================================
# 2. TRAZABILIDAD DE CORTES (CUT SCORE TRACEABILITY)
# ==============================================================================

plot_cut_score_traceability <- function(data_input, config) {
  # Obtener cortes de referencia (Global Scale)
  perf_levels <- config$scoring$performance_levels
  if (is.null(perf_levels)) {
    return(NULL)
  }

  ref_cuts <- if (is.data.frame(perf_levels)) perf_levels$min_score else sapply(perf_levels, function(x) x$min_score)
  ref_labels <- if (is.data.frame(perf_levels)) perf_levels$label else sapply(perf_levels, function(x) x$label)
  ref_cuts <- as.numeric(unlist(ref_cuts))

  # Obtener datos de curvas
  tables_df <- adapt_tables_data(data_input)
  if (is.null(tables_df)) {
    return(NULL)
  }

  forms <- unique(tables_df$FORM)
  trace_data_list <- list()

  for (f in forms) {
    sub_df <- tables_df[tables_df$FORM == f, ]

    # Interpolador: Global (Equated) -> Raw
    # EQUATED es Y, RAW es X. Queremos hallar X dado Y = Corte
    inv_fun <- approxfun(x = sub_df$EQUATED, y = sub_df$RAW, rule = 2)
    se_fun <- approxfun(x = sub_df$EQUATED, y = sub_df$SEE, rule = 2)

    raw_equiv <- inv_fun(ref_cuts)
    see_vals <- se_fun(ref_cuts)

    trace_data_list[[f]] <- data.frame(
      FORM = f,
      Level = ref_labels,
      Ref_Cut = ref_cuts,
      Est_Raw_Cut = raw_equiv,
      SEE = see_vals
    )
  }
  trace_data <- do.call(rbind, trace_data_list)

  if (is.null(trace_data) || nrow(trace_data) == 0) {
    return(NULL)
  }

  ggplot(trace_data, aes(x = FORM, y = Est_Raw_Cut)) +
    geom_hline(aes(yintercept = Ref_Cut), linetype = "dotted", color = "blue") +
    geom_errorbar(aes(ymin = Est_Raw_Cut - SEE, ymax = Est_Raw_Cut + SEE), width = 0.2) +
    geom_point(size = 3, shape = 21, fill = "white", stroke = 1) +
    geom_text(aes(label = round(Est_Raw_Cut, 1)), vjust = -1.5, size = 3) +
    facet_wrap(~Level, scales = "free_y") +
    labs(
      title = "Trazabilidad del Estándar (Cut Score Traceability)",
      subtitle = "Puntaje Crudo Requerido para Equivalencia con el Estándar",
      y = "Puntaje Crudo Equivalente", x = NULL,
      caption = "Línea Azul: Corte en Escala Base. Barras: Error Estándar (SEE)."
    ) +
    theme_audit_precision()
}

# ==============================================================================
# 3. SHIFT GLOBAL Y BIAS
# ==============================================================================

plot_global_shift <- function(data_input) {
  plot_data <- adapt_tables_data(data_input)
  if (is.null(plot_data)) {
    return(NULL)
  }

  # Filtrar identidad si es necesario (cuando form_src == form_target)
  # Asumimos que adapt_tables_data devuelve solo lo relevante o filtramos aquí
  plot_data <- plot_data |>
    dplyr::mutate(SHIFT = EQUATED - RAW)

  ggplot(plot_data, aes(x = RAW, y = SHIFT)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_ribbon(aes(ymin = SHIFT - 2 * SEE, ymax = SHIFT + 2 * SEE), fill = "gray80", alpha = 0.5) +
    geom_line(aes(color = FORM), linewidth = 1) +
    facet_wrap(~FORM) +
    labs(
      title = "Ajuste Sistemático de Equiparación (Global Shift)",
      subtitle = "Diferencia (Equiparado - Crudo). Valores > 0 indican que la forma fue más difícil.",
      x = "Puntaje Crudo", y = "Ajuste (Puntos)",
      caption = "Banda Gris: +/- 2 SEE."
    ) +
    theme_audit_precision() +
    theme(legend.position = "none")
}

# ==============================================================================
# 4. TÚNEL DE ERROR TOTAL (TSE)
# ==============================================================================

plot_tse_tunnel <- function(data_input) {
  plot_data <- adapt_tables_data(data_input)
  if (is.null(plot_data)) {
    return(NULL)
  }

  # Estimación sintética de CSEM si no tenemos datos de confiabilidad ítem a ítem
  # Usamos aproximación binomial cuadrática: SEM(x) ~ K * sqrt(x * (Max-x))

  max_score <- max(plot_data$RAW, na.rm = TRUE)

  # Inyectar un SEM promedio global (default 3 si no existe info)
  # En producción ideal, esto vendría de $reliability, aquí lo aproximamos para visualización
  sem_avg <- 3.0

  plot_data <- plot_data |>
    mutate(
      # Forma parabólica típica de CSEM
      CSEM = sem_avg * sqrt(RAW * (max_score - RAW)) / (0.5 * max_score),
      CSEM = replace(CSEM, is.na(CSEM) | CSEM == 0, sem_avg), # Evitar ceros en extremos
      TSE = sqrt(CSEM^2 + SEE^2)
    )

  long_data <- plot_data |>
    tidyr::pivot_longer(cols = c("CSEM", "SEE", "TSE"), names_to = "Error_Type", values_to = "Value")

  ggplot(long_data, aes(x = RAW, y = Value, color = Error_Type, linetype = Error_Type)) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~FORM) +
    scale_color_manual(values = c("TSE" = "black", "CSEM" = "blue", "SEE" = "red")) +
    scale_linetype_manual(values = c("TSE" = "solid", "CSEM" = "dashed", "SEE" = "dotted")) +
    labs(
      title = "Descomposición del Error Total (TSE) [Estimado]",
      subtitle = "TSE = sqrt(Error Medición^2 + Error Equiparación^2)",
      x = "Puntuación Cruda", y = "Error Estándar",
      caption = "CSEM: Estimación teórica binomial. SEE: Error de Equiparación (Bootstrap)."
    ) +
    theme_audit_precision()
}

# ==============================================================================
# 5. MÓDULO A: IMPACTO (VIOLÍN ENRIQUECIDO)
# ==============================================================================

prep_violin_stats <- function(final_scores, col_eq, col_lvl) {
  lvl_order <- final_scores |>
    dplyr::group_by(!!sym(col_lvl)) |>
    dplyr::summarise(Median_Score = median(!!sym(col_eq), na.rm = TRUE)) |>
    dplyr::arrange(Median_Score) |>
    dplyr::pull(!!sym(col_lvl))

  stats_summary <- final_scores |>
    dplyr::group_by(FORMA) |>
    dplyr::summarise(
      N = n(),
      Median = median(!!sym(col_eq), na.rm = TRUE),
    ) |>
    dplyr::mutate(
      Label_Stats = sprintf("%s\nN=%d\nMediana=%.1f", as.character(FORMA), N, Median),
      Form_Label = as.character(FORMA)
    )

  plot_data <- final_scores |>
    dplyr::mutate(
      Nivel_Factor = factor(!!sym(col_lvl), levels = lvl_order),
      Form_Label = as.character(FORMA)
    ) |>
    dplyr::left_join(stats_summary, by = c("Form_Label" = "Form_Label"))

  list(plot_data = plot_data, stats_summary = stats_summary)
}

prep_violin_impact <- function(plot_data, stats_summary, col_eq) {
  props <- plot_data |>
    dplyr::group_by(Form_Label, Nivel_Factor) |>
    dplyr::summarise(Count = n(), .groups = "drop_last") |>
    dplyr::mutate(
      Pct = Count / sum(Count),
      Label_Pct = scales::percent(Pct, accuracy = 0.1)
    ) |>
    dplyr::ungroup()

  pos_ref <- plot_data |>
    dplyr::group_by(Form_Label, Nivel_Factor) |>
    dplyr::summarise(Y_Pos = mean(!!sym(col_eq), na.rm = TRUE), .groups = "drop")

  dplyr::inner_join(props, pos_ref, by = c("Form_Label", "Nivel_Factor")) |>
    dplyr::inner_join(stats_summary, by = c("Form_Label" = "Form_Label"))
}

get_violin_cuts <- function(config) {
  perf_levels <- config$scoring$performance_levels
  if (is.null(perf_levels)) return(NULL)

  ref_cuts <- if (is.list(perf_levels)) unlist(lapply(perf_levels, function(x) x$scaled_score)) else sapply(perf_levels, function(x) x$min_score)
  ref_cuts <- as.numeric(unlist(ref_cuts))

  sort(unique(ref_cuts))[-1]
}

plot_score_comparison_violin <- function(final_scores, config = NULL) {
  col_eq <- "Score_Final"
  col_lvl <- "Nivel"

  if (!all(c(col_eq, col_lvl) %in% names(final_scores))) {
    return(NULL)
  }

  prep_data <- prep_violin_stats(final_scores, col_eq, col_lvl)
  plot_data <- prep_data$plot_data
  stats_summary <- prep_data$stats_summary

  ann_text <- prep_violin_impact(plot_data, stats_summary, col_eq)
  cuts_viz <- get_violin_cuts(config)

  p <- ggplot() +
    geom_violin(
      data = plot_data, aes(x = Label_Stats, y = !!sym(col_eq)),
      color = "black", size = 0.5, scale = "width", width = 0.7
    ) +
    geom_boxplot(
      data = plot_data, aes(x = Label_Stats, y = !!sym(col_eq)),
      width = 0.15, fill = "gray90", outlier.shape = 1, outlier.size = 1, alpha = 0.8
    )

  if (!is.null(cuts_viz) && length(cuts_viz) > 0) {
    p <- p + geom_hline(yintercept = cuts_viz, linetype = "dashed", color = "gray20", linewidth = 0.4)
  }

  p <- p + geom_label(
    data = ann_text, aes(x = Label_Stats, y = Y_Pos, label = Label_Pct),
    size = 3, fontface = "bold", fill = "white", linewidth = 0, alpha = 0.7
  ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    labs(
      title = "Distribución de Puntuaciones Equiparadas e Impacto",
      subtitle = "Densidad y Estadísticos Descriptivos por Forma",
      x = NULL,
      y = "Puntuación Equiparada",
      caption = "Líneas punteadas: Cortes de Nivel. Etiquetas: % de población por nivel."
    ) +
    theme_audit_precision() +
    theme(
      axis.text.x = element_text(size = 9, face = "bold"),
      panel.grid.major.x = element_blank()
    )

  p
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
# 5. ORQUESTADOR MÓDULO C
# ==============================================================================

export_module_c_precision <- function(ctt_results, eq_results, final_scores, config, base_dir) {
  if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)
  if (exists("info")) debug(">>> Generando gráficos Módulo C (Precisión)...")

  # C.1. Shift Global
  p1 <- tryCatch(plot_global_shift(eq_results), error = function(e) NULL)
  if (!is.null(p1)) ggsave(file.path(base_dir, "C01_Global_Shift.pdf"), p1, width = 10, height = 6)

  # C.2. Cut Score Trace
  p2 <- tryCatch(plot_cut_score_traceability(eq_results, config), error = function(e) NULL)
  if (!is.null(p2)) ggsave(file.path(base_dir, "C02_Cut_Traceability.pdf"), p2, width = 10, height = 7)

  # C.3. TSE Tunnel
  p3 <- tryCatch(plot_tse_tunnel(eq_results), error = function(e) NULL)
  if (!is.null(p3)) ggsave(file.path(base_dir, "C03_TSE_Structure.pdf"), p3, width = 10, height = 6)

  # 5. MÓDULO F: ANÁLISIS CTT BÁSICO
  # -----------------------------------------------------------
  ref_form_id <- tryCatch(config$system$reference_form, error = function(e) NULL)

  p4 <- plot_score_comparison_violin(final_scores, config)
  if (!is.null(p4)) ggsave(file.path(base_dir, "F08_Final_Score_Violin.pdf"), p4, width = 10, height = 7)
}
