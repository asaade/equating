# R/08b_reporting_irt_viz.R
# Responsabilidad: Generación de gráficos IRT (Validación, Diagnóstico y Scoring)
# Versión: v10.0 (Production Release: Advanced Wright + Cut Point Axis + Full Validation)
# Dependencias: 00_common.R

required_pkgs <- c("ggplot2", "ggthemes", "dplyr", "tidyr", "ggrepel", "scales", "reshape2", "hexbin", "stringr")
suppressPackageStartupMessages({
  invisible(lapply(required_pkgs, function(p) {
    if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
    library(p, character.only = TRUE, quietly = TRUE)
  }))
})

# ==============================================================================
# 1. TEMA GRÁFICO & HELPERS
# ==============================================================================

COLORS_IRT <- list(
  data       = "#2C3E50", # Azul oscuro (Datos principales)
  alert      = "#C0392B", # Rojo oscuro (Alertas/Error)
  warning    = "#F39C12", # Naranja (Advertencias/Cortes)
  ref        = "#95A5A6", # Gris medio (Referencias)
  accent     = "#2980B9", # Azul brillante (Info)
  # Wright Map Específico
  dens_fill  = "#E0E0E0", # Gris claro para densidad
  line_main  = "#000000", # Negro para ejes/líneas principales
  line_sec   = "#505050" # Gris oscuro para items
)

theme_psych_minimal <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(color = "#222222", family = "sans"),
      plot.title = element_blank(),
      plot.subtitle = element_text(color = "#444444", size = rel(1.1), margin = margin(b = 10)),
      plot.caption = element_text(size = rel(0.75), color = "#666666", hjust = 1, margin = margin(t = 10)),
      axis.line = element_line(color = "#333333", linewidth = 0.3),
      axis.ticks = element_line(color = "#333333", linewidth = 0.3),
      axis.title = element_text(size = rel(0.95), face = "bold", color = "#333333"),
      axis.text = element_text(color = "#333333", size = rel(0.9)),
      legend.position = "none",
      strip.text = element_text(face = "bold", size = rel(0.95), hjust = 0),
      panel.grid.major = element_line(color = "#EBEBEB", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(20, 20, 20, 20)
    )
}

# Wrapper seguro para guardar (utiliza la función de 00_common si está cargada)
save_plot <- function(p, name, w, h, logger) {
  if (exists("save_plot_safe")) {
    save_plot_safe(p, name, w, h, 300, logger)
  } else {
    tryCatch(
      {
        dir.create(dirname(name), recursive = TRUE, showWarnings = FALSE)
        ggsave(name, p, width = w, height = h, dpi = 300)
      },
      error = function(e) if (!is.null(logger)) log4r::warn(logger, paste("Plot save failed:", e$message))
    )
  }
}

# Helper para extraer puntos de corte de la configuración (Theta y Escala)
helper_get_cuts_irt <- function(config) {
  cuts <- list(theta = c(), scale = c(), labels = c())
  if (!is.null(config$scoring$anchors)) {
    anchors <- dplyr::bind_rows(config$scoring$anchors)
    # Filtrar Min y Max, nos interesan los cortes internos (C1, C2...)
    mid_points <- anchors %>%
      dplyr::filter(!toupper(label) %in% c("MIN", "MAX"))

    if (nrow(mid_points) > 0) {
      cuts$theta <- mid_points$source_val
      cuts$scale <- mid_points$target_val
      cuts$labels <- mid_points$label
    }
  }
  return(cuts)
}

# ==============================================================================
# 2. RENDERIZADORES: VALIDACIÓN DEL MODELO
# ==============================================================================

# Helper functions for Wright Map
build_base_wright_map <- function(person_df_plot, b_params_plot, y_limits) {
  ggplot() +
    # 1. Densidad de Personas (Izquierda)
    geom_density(
      data = person_df_plot, aes(y = Theta, x = -after_stat(density)),
      fill = COLORS_IRT$dens_fill, color = COLORS_IRT$line_main,
      alpha = 0.6, linewidth = 0.3
    ) +
    # 2. Eje Central
    geom_vline(xintercept = 0, color = COLORS_IRT$line_main, linewidth = 0.8) +

    # 3. Ítems (Rug Plot a la derecha)
    geom_segment(
      data = b_params_plot,
      aes(x = 0.02, xend = 0.2, y = Difficulty, yend = Difficulty),
      color = COLORS_IRT$line_sec, alpha = 1, linewidth = 0.25
    ) +

    # 4. Etiquetas de Ítems
    geom_text_repel(
      data = dplyr::filter(b_params_plot, label_item),
      aes(y = Difficulty, label = Item),
      x = 0.2, hjust = 0, size = 3, color = COLORS_IRT$line_sec, direction = "y",
      nudge_x = 0.15, xlim = c(0.25, 0.8),
      box.padding = 0.25, min.segment.length = 0, max.overlaps = 30
    ) +

    # Configuración de Ejes
    scale_x_continuous(limits = c(-0.75, 0.9), breaks = NULL, name = NULL) +
    scale_y_continuous(name = "Logits (Habilidad / Dificultad)", expand = c(0, 0)) +

    # Títulos de columnas
    annotate("text",
      x = -0.15, y = y_limits[1] + 0.2, label = "Habilidad",
      fontface = "bold", color = COLORS_IRT$line_main, hjust = 1, size = 3.5
    ) +
    annotate("text",
      x = 0.15, y = y_limits[1] + 0.2, label = "Dificultad",
      fontface = "bold", color = COLORS_IRT$line_main, hjust = 0, size = 3.5
    ) +
    theme_psych_minimal() +
    theme(plot.margin = margin(t = 30, r = 10, b = 30, l = 10, unit = "pt")) +
    coord_cartesian(ylim = y_limits, clip = "off")
}

add_wright_map_cuts <- function(p, cuts, y_limits) {
  if (!is.null(cuts$theta) && length(cuts$theta) > 0) {
    cuts_df <- data.frame(Value = cuts$theta, Label = cuts$labels) %>%
      dplyr::filter(Value >= y_limits[1], Value <= y_limits[2])

    if (nrow(cuts_df) > 0) {
      p <- p +
        geom_hline(
          data = cuts_df, aes(yintercept = Value),
          linetype = "dashed", color = COLORS_IRT$warning, linewidth = 0.4
        ) +
        geom_text(
          data = cuts_df, aes(y = Value, label = Label),
          x = -0.65, hjust = 0, vjust = -0.5,
          color = COLORS_IRT$warning, size = 3, fontface = "italic"
        )
    }
  }
  return(p)
}

add_wright_map_alerts <- function(p, n_high, n_low, y_limits) {
  if (n_high > 0) {
    p <- p +
      annotate("segment",
        x = 0.07, xend = 0.07, y = y_limits[2], yend = y_limits[2] + 0.35,
        arrow = arrow(length = unit(0.15, "cm"), type = "closed"), color = COLORS_IRT$alert, linewidth = 0.6
      ) +
      annotate("text",
        x = 0.07, y = y_limits[2] + 0.45, label = paste0(n_high, " ítems (High)"),
        color = COLORS_IRT$alert, size = 2.8, fontface = "bold", vjust = 0
      )
  }

  if (n_low > 0) {
    p <- p +
      annotate("segment",
        x = 0.07, xend = 0.07, y = y_limits[1], yend = y_limits[1] - 0.35,
        arrow = arrow(length = unit(0.15, "cm"), type = "closed"), color = COLORS_IRT$alert, linewidth = 0.6
      ) +
      annotate("text",
        x = 0.07, y = y_limits[1] - 0.45, label = paste0(n_low, " ítems (Low)"),
        color = COLORS_IRT$alert, size = 2.8, fontface = "bold", vjust = 1
      )
  }
  return(p)
}

# A. MAPA DE WRIGHT AVANZADO (Diseño con Densidad Izq. / Rug Der.)
render_wright_map_advanced <- function(scores_subset, b_params_subset, y_limits = c(-4, 4), label_quantiles = c(0.1, 0.9), cuts = NULL) {
  # Filtrar personas dentro del rango visual
  person_df_plot <- scores_subset %>% dplyr::filter(Theta > y_limits[1] & Theta < y_limits[2])
  if (nrow(person_df_plot) == 0) {
    return(NULL)
  }

  # Contar ítems fuera de rango (Alertas)
  n_high <- sum(b_params_subset$Difficulty >= y_limits[2])
  n_low <- sum(b_params_subset$Difficulty <= y_limits[1])

  # Ítems visibles
  b_params_plot <- b_params_subset %>%
    dplyr::filter(Difficulty > y_limits[1], Difficulty < y_limits[2])

  # Etiquetado inteligente de ítems (solo extremos para no saturar)
  if (nrow(b_params_plot) > 0) {
    q_low <- quantile(b_params_plot$Difficulty, label_quantiles[1])
    q_high <- quantile(b_params_plot$Difficulty, label_quantiles[2])
    b_params_plot <- b_params_plot %>%
      dplyr::mutate(label_item = Difficulty <= q_low | Difficulty >= q_high)
  }

  p <- build_base_wright_map(person_df_plot, b_params_plot, y_limits)
  p <- add_wright_map_cuts(p, cuts, y_limits)
  p <- add_wright_map_alerts(p, n_high, n_low, y_limits)

  return(p)
}

# C. INDEPENDENCIA LOCAL (Q3 Heatmap)
render_q3_heatmap <- function(model_results) {
  q3_df <- model_results$diagnostics$local_independence
  if (is.null(q3_df) || nrow(q3_df) == 0 || !all(c("Item_1", "Item_2", "Q3") %in% names(q3_df))) {
    return(NULL)
  }

  q3_top <- q3_df %>%
    dplyr::arrange(desc(abs(Q3))) %>%
    head(30) %>%
    dplyr::mutate(Pair = paste(Item_1, Item_2, sep = " - "))

  ggplot(q3_top, aes(x = reorder(Pair, abs(Q3)), y = Q3)) +
    geom_segment(aes(xend = Pair, yend = 0), color = "#E0E0E0") +
    geom_point(aes(color = abs(Q3) > 0.2), size = 3) +
    coord_flip() +
    geom_hline(yintercept = c(-0.2, 0.2), linetype = "dotted", color = COLORS_IRT$ref) +
    scale_color_manual(values = c("FALSE" = COLORS_IRT$data, "TRUE" = COLORS_IRT$alert)) +
    labs(
      x = NULL, y = "Residuo de Correlación (Q3)",
      subtitle = "Independencia Local (Top 30 Pares)",
      caption = "Puntos Rojos: Posible dependencia local (|Q3| > 0.2)"
    ) +
    theme_psych_minimal()
}

# D. ITEM FIT MAP (Infit vs Dificultad)
render_item_fit_map <- function(model_results, b_params) {
  diag <- model_results$diagnostics$fit
  if (is.null(diag) || is.null(b_params)) {
    return(NULL)
  }

  plot_df <- diag %>%
    dplyr::inner_join(b_params %>% dplyr::select(ITEM = Item, Difficulty), by = "ITEM") %>%
    dplyr::filter(abs(Difficulty) < 5)

  if ("Infit_MSQ" %in% names(plot_df)) {
    ggplot(plot_df, aes(x = Difficulty, y = Infit_MSQ)) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.7, ymax = 1.3, fill = "#F0F9E8", alpha = 0.5) +
      geom_hline(yintercept = c(0.7, 1.3), linetype = "dashed", color = COLORS_IRT$ref) +
      geom_point(aes(color = (Infit_MSQ > 1.3 | Infit_MSQ < 0.7)), alpha = 0.7, size = 2) +
      geom_text_repel(
        data = subset(plot_df, Infit_MSQ > 1.4 | Infit_MSQ < 0.6),
        aes(label = ITEM), size = 2.5, box.padding = 0.4
      ) +
      scale_color_manual(values = c("FALSE" = COLORS_IRT$data, "TRUE" = COLORS_IRT$alert)) +
      labs(
        x = "Dificultad (b)", y = "Infit MSQ",
        subtitle = "Diagnóstico de Ajuste vs. Dificultad",
        caption = "Zona Verde: Ajuste Aceptable [0.7 - 1.3]"
      ) +
      theme_psych_minimal()
  } else {
    return(NULL)
  }
}

# E. PARAMETER RELATION (a vs b)
render_parameter_relation <- function(model_results, b_params) {
  col_a <- dplyr::first(intersect(c("IRT_a", "a", "a1"), names(model_results$parameters)))
  if (is.null(col_a)) {
    return(NULL)
  }

  plot_df <- model_results$parameters %>%
    dplyr::select(ITEM, A = !!sym(col_a)) %>%
    dplyr::inner_join(b_params %>% dplyr::select(ITEM = Item, Difficulty), by = "ITEM") %>%
    dplyr::filter(abs(Difficulty) < 5 & A < 4)

  ggplot(plot_df, aes(x = Difficulty, y = A)) +
    geom_point(color = COLORS_IRT$data, alpha = 0.6) +
    geom_smooth(formula = "y ~ x", method = "loess", color = COLORS_IRT$accent, se = FALSE, linewidth = 0.5, linetype = "dashed") +
    geom_vline(xintercept = 0, color = COLORS_IRT$ref, linewidth = 0.3) +
    labs(x = "Dificultad (b)", y = "Discriminación (a)", subtitle = "Estructura del Test: a vs b") +
    theme_psych_minimal()
}

# ==============================================================================
# 3. RENDERIZADORES: DIAGNÓSTICO PERSONAS
# ==============================================================================

# F. AJUSTE PERSONAS (Zh)
render_person_fit_dist <- function(scores_df) {
  if (!"Zh" %in% names(scores_df)) {
    return(NULL)
  }

  ggplot(scores_df, aes(x = Zh)) +
    geom_histogram(bins = 40, fill = COLORS_IRT$ref, color = "white", linewidth = 0.1) +
    geom_vline(xintercept = c(-2, 2), linetype = "dashed", color = COLORS_IRT$alert, linewidth = 0.8) +
    labs(
      x = "Estadístico de Ajuste (Zh)", y = "Frecuencia",
      subtitle = "Distribución de Ajuste de Personas",
      caption = "Líneas Rojas: Umbrales de Ajuste +/- 2.0"
    ) +
    theme_psych_minimal()
}

# G. THETA VS ZH (Scatter)
render_theta_zh_scatter <- function(scores_df) {
  if (!all(c("Theta", "Zh") %in% names(scores_df))) {
    return(NULL)
  }
  plot_data <- scores_df %>% dplyr::filter(!is.na(Theta) & !is.na(Zh))
  if (nrow(plot_data) == 0) {
    return(NULL)
  }

  p <- ggplot(plot_data, aes(x = Theta, y = Zh))

  if (nrow(plot_data) > 10000) {
    p <- p + geom_hex(bins = 60) +
      scale_fill_gradient(low = "#E0E0E0", high = COLORS_IRT$data, guide = "none")
  } else {
    p <- p + geom_point(alpha = 0.3, color = COLORS_IRT$data, size = 1)
  }

  p + geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = COLORS_IRT$alert) +
    labs(x = "Habilidad (Theta)", y = "Ajuste (Zh)", subtitle = "Relación Habilidad vs Ajuste") +
    theme_psych_minimal()
}

# ==============================================================================
# 4. RENDERIZADORES: SCORING (CORTES EN EJES)
# ==============================================================================

# H. ESCALAMIENTO (Con Cortes Explícitos)
render_irt_scaling_profile <- function(scores_df, config) {
  if (!all(c("Theta", "Score_Final_IRT") %in% names(scores_df))) {
    return(NULL)
  }

  anchors_df <- NULL
  cuts <- helper_get_cuts_irt(config)

  if (!is.null(config$scoring$anchors)) {
    anchors_df <- dplyr::bind_rows(config$scoring$anchors) %>% dplyr::arrange(source_val)
  }

  p <- ggplot(scores_df, aes(x = Theta, y = Score_Final_IRT))

  if (nrow(scores_df) > 5000) {
    p <- p + geom_hex(bins = 60) + scale_fill_gradient(low = "#E0E0E0", high = COLORS_IRT$data, guide = "none")
  } else {
    p <- p + geom_point(alpha = 0.2, color = COLORS_IRT$data, size = 1.2)
  }

  if (!is.null(anchors_df)) {
    p <- p + geom_line(data = anchors_df, aes(x = source_val, y = target_val), color = COLORS_IRT$warning, linewidth = 1, linetype = "dashed") +
      geom_point(data = anchors_df, aes(x = source_val, y = target_val), color = COLORS_IRT$warning, size = 2.5, shape = 18)
  }

  # EJES PERSONALIZADOS CON CORTES
  x_breaks <- sort(unique(c(pretty(scores_df$Theta), cuts$theta)))
  y_breaks <- sort(unique(c(pretty(scores_df$Score_Final_IRT), cuts$scale)))

  p + scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    labs(x = "Theta (Habilidad)", y = "Score Final", subtitle = "Función de Escalamiento") +
    theme_psych_minimal() +
    theme(panel.grid.minor = element_line(color = "#F0F0F0", linewidth = 0.2))
}

# I. TARGETING CON CORTES (TIC)
render_tic_cuts_validation <- function(model_obj, config) {
  if (is.null(model_obj)) {
    return(NULL)
  }

  Theta_grid <- matrix(seq(-4, 4, length.out = 100))
  info <- tryCatch(mirt::testdebug(model_obj, Theta_grid), error = function(e) NULL)
  if (is.null(info)) {
    return(NULL)
  }

  sem <- 1 / sqrt(info)
  plot_df <- data.frame(Theta = as.vector(Theta_grid), Info = info, SEM = sem)
  cuts <- helper_get_cuts_irt(config)
  scale_factor <- max(plot_df$Info) / max(plot_df$SEM) * 0.8

  p <- ggplot(plot_df, aes(x = Theta)) +
    geom_area(aes(y = Info), fill = COLORS_IRT$data, alpha = 0.1) +
    geom_line(aes(y = Info, color = "Información"), linewidth = 1.2) +
    geom_line(aes(y = SEM * scale_factor, color = "Error (SEM)"), linetype = "dashed", linewidth = 0.8) +
    scale_y_continuous(sec.axis = sec_axis(~ . / scale_factor, name = "Error (SEM)")) +
    scale_color_manual(name = "", values = c("Información" = COLORS_IRT$data, "Error (SEM)" = COLORS_IRT$alert)) +
    labs(x = "Theta", y = "Información", subtitle = "Precisión del Test en Puntos de Corte") +
    theme_psych_minimal() +
    theme(legend.position = "top")

  if (length(cuts$theta) > 0) {
    p <- p + geom_vline(xintercept = cuts$theta, color = COLORS_IRT$warning, linetype = "dotted", linewidth = 1) +
      annotate("text",
        x = cuts$theta, y = max(plot_df$Info) * 0.95,
        label = cuts$labels, angle = 90, vjust = -0.5, color = "#444444", size = 3
      ) +
      scale_x_continuous(breaks = sort(unique(c(pretty(plot_df$Theta), cuts$theta)))) # Cortes en eje X
  }
  return(p)
}

# J. DISTRIBUCIÓN DE PUNTUACIONES (Violin con Cortes Y)
render_irt_score_distribution <- function(scores_df, config) {
  if (!"Score_Final_IRT" %in% names(scores_df)) {
    return(NULL)
  }
  cuts <- helper_get_cuts_irt(config)

  p <- ggplot(scores_df, aes(x = FORMA, y = Score_Final_IRT)) +
    geom_violin(fill = "#F9F9F9", color = "#555555", width = 0.7) +
    geom_boxplot(width = 0.1, fill = "white", color = "#222222", outlier.size = 0.5) +
    labs(x = NULL, y = "Score IRT", subtitle = "Distribución de Puntuaciones") +
    theme_psych_minimal()

  if (length(cuts$scale) > 0) {
    p <- p + geom_hline(yintercept = cuts$scale, linetype = "dotted", color = COLORS_IRT$warning) +
      scale_y_continuous(breaks = sort(unique(c(pretty(scores_df$Score_Final_IRT), cuts$scale))))
  }
  return(p)
}

render_level_distribution <- function(scores_df) {
  col_level <- dplyr::first(intersect(c("Nivel_IRT", "Nivel"), names(scores_df)))
  if (is.null(col_level)) {
    return(NULL)
  }
  plot_data <- scores_df %>%
    dplyr::filter(!is.na(!!sym(col_level))) %>%
    dplyr::group_by(Level = !!sym(col_level)) %>%
    dplyr::tally() %>%
    dplyr::mutate(Pct = n / sum(n))

  ggplot(plot_data, aes(x = Level, y = Pct, fill = Level)) +
    geom_col(width = 0.6, alpha = 0.9) +
    geom_text(aes(label = scales::percent(Pct, accuracy = 1)), vjust = -0.5, fontface = "bold") +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
    scale_fill_brewer(palette = "Blues") +
    labs(x = NULL, y = NULL, subtitle = "Distribución por Nivel de Desempeño") +
    theme_psych_minimal() +
    theme(panel.grid.major.x = element_blank())
}

# ==============================================================================
# 5. ORQUESTADOR
# ==============================================================================

generate_irt_visual_reports <- function(config, model_results, scores_df, logger, drift_df = NULL) {
  debug(logger, ">>> Generando Reportes Visuales IRT (v10.0 Final Breaks)...")
  out_dir <- file.path(config$project$output_dir, "IRT", "06_PLOTS")

  if (is.null(model_results) || is.null(scores_df)) {
    warn(logger, "SKIPPING IRT Plots: Data missing.")
    return()
  }

  # Preparar Parámetros B (Dificultad) Robustos
  b_params <- tryCatch(
    {
      coefs <- coef(model_results$model_obj, IRTpars = TRUE, simplify = TRUE)$items
      df_b <- as.data.frame(coefs)
      df_b$Item <- rownames(df_b)
      b_cols <- grep("^b", names(df_b), value = TRUE)
      if (length(b_cols) == 0 && "d" %in% names(df_b)) {
        slope <- if ("a1" %in% names(df_b)) df_b$a1 else 1
        df_b$Difficulty <- -df_b$d / slope
      } else {
        df_b$Difficulty <- rowMeans(df_b[, b_cols, drop = FALSE], na.rm = TRUE)
      }
      df_b
    },
    error = function(e) NULL
  )

  # --- 1. WRIGHT MAPS ---
  if (!is.null(b_params)) {
    cuts <- helper_get_cuts_irt(config)
    forms <- unique(scores_df$FORMA)
    for (f in forms) {
      tryCatch(
        {
          p <- render_wright_map_advanced(scores_df %>% dplyr::filter(FORMA == f), b_params, cuts = cuts)
          if (!is.null(p)) {
            safe_name <- str_replace_all(f, "[^[:alnum:]]", "_")
            save_plot(p, file.path(out_dir, paste0("IRT_Wright_Map_", safe_name, ".png")), 7, 9, logger)
          }
        },
        error = function(e) warn(logger, paste("Wright Map Error:", e$message))
      )
    }
  }

  # --- 2. VALIDACIÓN ---
  tryCatch(save_plot(render_q3_heatmap(model_results), file.path(out_dir, "IRT_Local_Independence_Q3.png"), 8, 7, logger), error = function(e) warn(logger, paste("Q3:", e$message)))
  tryCatch(save_plot(render_item_fit_map(model_results, b_params), file.path(out_dir, "IRT_Item_Fit_Map.png"), 8, 6, logger), error = function(e) warn(logger, paste("Fit:", e$message)))
  tryCatch(save_plot(render_parameter_relation(model_results, b_params), file.path(out_dir, "IRT_Parameter_Relation.png"), 8, 6, logger), error = function(e) warn(logger, paste("Params:", e$message)))

  # --- 3. PERSONAS ---
  tryCatch(save_plot(render_person_fit_dist(scores_df), file.path(out_dir, "IRT_Person_Zh_Dist.png"), 8, 4, logger), error = function(e) warn(logger, paste("Zh:", e$message)))
  tryCatch(save_plot(render_theta_zh_scatter(scores_df), file.path(out_dir, "IRT_Person_Zh_Scatter.png"), 8, 5, logger), error = function(e) warn(logger, paste("Zh Scat:", e$message)))

  # --- 4. SCORING ---
  if (!is.null(model_results$model_obj)) {
    tryCatch(save_plot(render_tic_cuts_validation(model_results$model_obj, config), file.path(out_dir, "IRT_Targeting_Cuts_TIC.png"), 10, 6, logger), error = function(e) warn(logger, paste("TIC:", e$message)))
  }
  tryCatch(save_plot(render_irt_scaling_profile(scores_df, config), file.path(out_dir, "IRT_Scaling_Function_Profile.png"), 8, 6, logger), error = function(e) warn(logger, paste("Scaling:", e$message)))
  tryCatch(save_plot(render_irt_score_distribution(scores_df, config), file.path(out_dir, "IRT_Score_Dist_Violin.png"), 10, 6, logger), error = function(e) warn(logger, paste("Violin:", e$message)))
  tryCatch(save_plot(render_level_distribution(scores_df), file.path(out_dir, "IRT_Level_Distribution.png"), 6, 5, logger), error = function(e) warn(logger, paste("Level:", e$message)))

  debug(logger, "✅ Reportes Visuales IRT finalizados.")
}
