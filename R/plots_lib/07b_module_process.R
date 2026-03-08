# ==============================================================================
# MÓDULO B: EVIDENCIA DE INTEGRIDAD DEL PROCESO (THE PROCESS)
# Versión: v106.1 - Adapter Pattern para datos consolidados
# Responsabilidad: Auditoría de Anclaje, Deriva (Drift) y Selección de Modelos.
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, dplyr, tidyr, ggrepel, gridExtra, scales, stringr)

if (!exists("execute_safely")) source("R/00_common_base.R")

# ==============================================================================
# 1. TEMA GRÁFICO: PROCESO
# ==============================================================================
theme_audit_process <- function() {
  theme_bw(base_size = 11) +
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
      legend.key = element_rect(fill = "white")
    )
}

# ==============================================================================
# HELPER: ADAPTADOR DE DATOS (Flat -> Hierarchical)
# ==============================================================================
adapt_input_data <- function(input_data, type = "drift") {
  # Si ya es jerárquico (lista de listas), lo devuelve tal cual (parcialmente)
  # Pero para drift/history/decisions, las funciones esperan consolidar.

  if (type == "drift") {
    # Si viene el objeto plano 'drift_details', úsalo directo
    if (!is.null(input_data$drift_details)) {
      df <- input_data$drift_details
      if (!"FORM" %in% names(df) && "LINK" %in% names(df)) {
        # Extraer FORM desde LINK (e.g., "FORMA_02->FORMA_01")
        df$FORM <- sub("->.*", "", df$LINK)
      }
      if (!"ITEM_ID" %in% names(df) && "ITEM" %in% names(df)) df$ITEM_ID <- df$ITEM
      return(df)
    }
    # Si es jerárquico batch_results
    return(do.call(rbind, lapply(names(input_data), function(fid) {
      res <- input_data[[fid]]
      if (is.null(res$drift)) {
        return(NULL)
      }
      df <- res$drift
      df$FORM <- fid
      df$ITEM_ID <- rownames(df)
      return(df)
    })))
  }

  if (type == "smoothing") {
    if (!is.null(input_data$smoothing_history)) {
      df <- input_data$smoothing_history
      if (!"FORM" %in% names(df) && "LINK" %in% names(df)) df$FORM <- sub("->.*", "", df$LINK)
      return(df)
    }
    return(do.call(rbind, lapply(names(input_data), function(fid) {
      res <- input_data[[fid]]
      hist <- res$audit$smoothing_history
      if (is.null(hist)) {
        return(NULL)
      }
      hist$FORM <- fid
      return(hist)
    })))
  }

  if (type == "decision") {
    if (!is.null(input_data$decisions)) {
      df <- input_data$decisions
      if (!"FORM" %in% names(df) && "LINK" %in% names(df)) df$FORM <- sub("->.*", "", df$LINK)
      return(df)
    }
    return(do.call(rbind, lapply(names(input_data), function(fid) {
      res <- input_data[[fid]]
      log <- res$decision_log
      if (is.null(log)) {
        return(NULL)
      }
      log$FORM <- fid
      return(log)
    })))
  }
  return(NULL)
}

# ==============================================================================
# 2. ANÁLISIS FORENSE DE DERIVA (ROBUST DELTA PLOT)
# ==============================================================================

plot_drift_forensics <- function(data_input) {
  plot_data <- adapt_input_data(data_input, "drift")

  if (is.null(plot_data) || nrow(plot_data) == 0) {
    return(NULL)
  }

  # Transformación Delta: 13 + 4z (Dificultad ETS)
  p_to_delta <- function(p) {
    p_num <- suppressWarnings(as.numeric(p))
    if (all(is.na(p_num))) return(rep(NA_real_, length(p)))
    p_safe <- pmax(0.001, pmin(0.999, p_num))
    13 + 4 * qnorm(1 - p_safe)
  }

  plot_data <- plot_data |>
    dplyr::filter(!is.na(P_SRC), !is.na(P_DEST)) |>
    dplyr::mutate(
      Delta_Ref = p_to_delta(P_SRC),
      Delta_New = p_to_delta(P_DEST),
      Z_Safe = tidyr::replace_na(as.numeric(Z_SCORE), 0),
      # Clasificación
      Status_Audit = dplyr::case_when(
        STATUS != "KEPT" ~ "ELIMINADO",
        abs(Z_Safe) > 2.5 ~ "ALERTA (Mantenido)",
        TRUE ~ "ESTABLE"
      ),
      Status_Audit = factor(Status_Audit, levels = c("ESTABLE", "ALERTA (Mantenido)", "ELIMINADO"))
    )

  # Resumen de Auditoría por Forma/Link
  group_cols <- intersect(c("FORM", "LINK"), names(plot_data))
  summary_data <- plot_data |>
    dplyr::group_by(across(all_of(group_cols))) |>
    dplyr::summarize(
      n_total = dplyr::n(),
      n_kept = sum(STATUS == "KEPT"),
      r_anchor = NA_real_,
      .groups = "drop"
    )

  # Intentar inyectar correlación si viene en el input consolidado
  if (!is.null(data_input$link_quality)) {
    lq <- data_input$link_quality
    if ("LINK" %in% names(summary_data) && "LINK" %in% names(lq)) {
      summary_data <- summary_data |>
        dplyr::left_join(lq |> dplyr::select(LINK, R_ANCHOR), by = "LINK") |>
        dplyr::mutate(r_anchor = R_ANCHOR)
    } else if ("FORM" %in% names(summary_data)) {
      # Fallback: Extraer de LINK en lq para unir por FORM (Source Form)
      lq_sub <- lq |>
        dplyr::mutate(JOIN_FORM = sub("->.*", "", LINK)) |>
        dplyr::select(JOIN_FORM, R_ANCHOR)
      summary_data <- summary_data |>
        dplyr::left_join(lq_sub, by = c("FORM" = "JOIN_FORM")) |>
        dplyr::mutate(r_anchor = R_ANCHOR)
    }
  }

  summary_data <- summary_data |>
    dplyr::mutate(
      Label = sprintf("N Total: %d\nN Kept: %d%s",
                      n_total, n_kept,
                      ifelse(is.na(r_anchor), "", sprintf("\nCorr: %.3f", r_anchor))),
      # Posicionamiento dinámico (esquina superior izquierda)
      x_pos = -Inf, y_pos = Inf
    )

  ggplot(plot_data, aes(x = Delta_Ref, y = Delta_New)) +
    geom_ribbon(aes(ymin = Delta_Ref - 1.5, ymax = Delta_Ref + 1.5), fill = "#f0f0f0", alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
    geom_point(aes(shape = Status_Audit, fill = Status_Audit, size = Status_Audit), color = "black", stroke = 0.5) +
    ggrepel::geom_text_repel(
      data = subset(plot_data, Status_Audit != "ESTABLE"),
      aes(label = ITEM_ID), size = 2.5, fontface = "bold", max.overlaps = 50
    ) +
    geom_text(
      data = summary_data, aes(x = x_pos, y = y_pos, label = Label),
      hjust = -0.1, vjust = 1.2, size = 3, fontface = "italic", color = "#444444"
    ) +
    facet_wrap(~FORM) +
    scale_shape_manual(values = c("ESTABLE" = 21, "ALERTA (Mantenido)" = 21, "ELIMINADO" = 4)) +
    scale_fill_manual(values = c("ESTABLE" = "white", "ALERTA (Mantenido)" = "#feb24c", "ELIMINADO" = "black")) +
    scale_size_manual(values = c("ESTABLE" = 1.5, "ALERTA (Mantenido)" = 2.5, "ELIMINADO" = 3)) +
    coord_fixed() +
    labs(
      title = "Estabilidad del Ancla (Delta Plot)",
      subtitle = "Detección de Drift mediante Regresión Robusta Iterativa",
      x = "Dificultad Delta (Origen)", y = "Dificultad Delta (Destino)",
      caption = "Zona gris: Tolerancia +/- 1.5 Delta. Cruces: Ítems eliminados por drift excesivo."
    ) +
    theme_audit_process()
}

# ==============================================================================
# 3. AUDITORÍA DE SUAVIZAMIENTO (EVOLUCIÓN BIC/WIGGLE)
# ==============================================================================

plot_smoothing_evolution <- function(data_input) {
  hist_data <- adapt_input_data(data_input, "smoothing")

  if (is.null(hist_data) || nrow(hist_data) == 0) {
    return(NULL)
  }

  # Normalizar métricas para visualización dual
  hist_data <- hist_data |>
    group_by(FORM, Side) |>
    mutate(
      Status_Color = ifelse(grepl("PASS", Status), "PASS", "FAIL")
    ) |>
    ungroup()

  ggplot(hist_data, aes(x = Params_K)) +
    # Línea BIC
    geom_line(aes(y = BIC, group = interaction(FORM, Side), color = "BIC"), linewidth = 0.7) +
    geom_point(aes(y = BIC, shape = Status_Color), size = 2) +
    facet_grid(Side ~ FORM, scales = "free_y") +
    scale_color_manual(values = c("BIC" = "black")) +
    scale_shape_manual(values = c("PASS" = 19, "FAIL" = 4)) +
    labs(
      title = "Forensics: Selección de Modelo Log-Lineal (Step-Up)",
      subtitle = "Compromiso entre Ajuste (BIC) y Complejidad (K)",
      x = "Complejidad (Número de Parámetros K)",
      y = "BIC (Menor es mejor)",
      caption = "Puntos 'X' indican modelos rechazados por Rugosidad o Momentos."
    ) +
    theme_audit_process()
}

# ==============================================================================
# 4. MATRIZ DE DECISIÓN DE MODELO
# ==============================================================================

plot_model_decision_matrix <- function(data_input) {
  dec_data <- adapt_input_data(data_input, "decision")

  if (is.null(dec_data) || nrow(dec_data) == 0) {
    return(NULL)
  }

  dec_data <- dec_data |>
    mutate(
      Label_Text = sprintf("RMSD: %.2f | SEE: %.3f", RMSD_Ref, W_SEE),
      Tile_Color = case_when(
        Selected & Method == "TUCKER" ~ "#e6f5c9", # Verde claro
        Selected & Method != "TUCKER" ~ "#fff2ae", # Amarillo (Alerta complejidad)
        TRUE ~ "white"
      )
    )

  ggplot(dec_data, aes(x = Method, y = FORM)) +
    geom_tile(aes(fill = Tile_Color), color = "gray80") +
    geom_text(aes(label = Label_Text, fontface = ifelse(Selected, "bold", "plain")), size = 3) +
    geom_point(
      data = subset(dec_data, Selected), aes(x = Method, y = FORM),
      shape = 21, size = 12, color = "black", fill = NA, stroke = 1
    ) +
    scale_fill_identity() +
    labs(
      title = "Matriz de Decisión de Modelos de Equiparación",
      subtitle = "Comparativa de Candidatos (Círculo indica el modelo seleccionado)",
      x = NULL, y = NULL,
      caption = "TUCKER es preferido (Parsimonia) salvo que RMSD > DTM."
    ) +
    theme_audit_process() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
}

# ==============================================================================
# 5. ORQUESTADOR MÓDULO B
# ==============================================================================

export_module_b_process <- function(data_input, config, base_dir) {
  if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)

  if (exists("info")) debug(">>> Generando gráficos Módulo B (Proceso)...")

  # B.1. Drift Forensics
  p1 <- tryCatch(plot_drift_forensics(data_input), error = function(e) NULL)
  if (!is.null(p1)) ggsave(file.path(base_dir, "B01_Anchor_Drift_Audit.pdf"), p1, width = 10, height = 7)

  # B.2. Smoothing Evolution
  p2 <- tryCatch(plot_smoothing_evolution(data_input), error = function(e) NULL)
  if (!is.null(p2)) ggsave(file.path(base_dir, "B02_Smoothing_Forensics.pdf"), p2, width = 10, height = 6)

  # B.3. Decision Matrix
  p3 <- tryCatch(plot_model_decision_matrix(data_input), error = function(e) NULL)
  if (!is.null(p3)) ggsave(file.path(base_dir, "B03_Model_Decision_Matrix.pdf"), p3, width = 9, height = 5)
}
