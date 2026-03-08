# ==============================================================================
# MÓDULO D: ANÁLISIS ESTRATÉGICO Y COMPARATIVO
# Versión: v106.1 - Adapter Pattern & Data Robustness
# Responsabilidad: Visualización de decisiones (Trade-offs) y comparación de formas.
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, dplyr, tidyr, ggrepel, scales)

if (!exists("execute_safely")) source("R/00_common_base.R")

theme_audit_analysis <- function() {
  theme_bw(base_size = 11) +
    theme(
      text = element_text(color = "black", family = "sans"),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10, color = "#555555"),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#2c3e50"),
      strip.text = element_text(color = "white", face = "bold"),
      legend.position = "right"
    )
}

# ==============================================================================
# HELPERS: ADAPTADORES DE DATOS (Flat vs Hierarchical)
# ==============================================================================

adapt_decision_data <- function(input_data) {
  # Caso 1: Estructura plana legacy (eq_results$decisions)
  if (!is.null(input_data$decisions)) {
    df <- input_data$decisions
    # Normalización de nombres si es necesario
    if (!"FORM" %in% names(df)) {
      # Intentar extraer de LINK o SOURCE_FORM si existen
      if ("SOURCE_FORM" %in% names(df)) {
        df$FORM <- df$SOURCE_FORM
      } else if ("LINK" %in% names(df)) {
        df$FORM <- sub("->.*", "", df$LINK)
      } else {
        df$FORM <- "UNKNOWN"
      }
    }
    return(df)
  }

  # Caso 2: Estructura jerárquica (eq_results[[id]]$decision_log)
  return(do.call(rbind, lapply(names(input_data), function(fid) {
    res <- input_data[[fid]]
    if (is.null(res$decision_log)) {
      return(NULL)
    }
    log <- res$decision_log
    log$FORM <- fid
    return(log)
  })))
}

adapt_concordance_data <- function(input_data) {
  # Caso 1: Estructura plana legacy (eq_results$tables)
  if (!is.null(input_data$tables)) {
    df <- input_data$tables
    # Mapeo de columnas legacy a estándar
    df <- df |>
      dplyr::rename(
        FORM = SOURCE_FORM,
        RAW = RAW_SCORE,
        EQUATED = EQUATED_SCORE
      )

    # Asegurar columna METHOD si existe en tables
    if (!"METHOD" %in% names(df)) df$METHOD <- "UNKNOWN"
    return(df)
  }

  # Caso 2: Estructura jerárquica (eq_results[[id]]$concordance)
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
      METHOD = res$meta$method %||% "UNKNOWN"
    )
  })))
}

# ==============================================================================
# 1. TRADE-OFF PLOT: PRECISIÓN (SEE) vs. CURVATURA (RMSD)
# ==============================================================================

plot_selection_tradeoff <- function(eq_results, config) {
  tradeoff_data <- adapt_decision_data(eq_results)

  if (is.null(tradeoff_data) || nrow(tradeoff_data) == 0) {
    return(NULL)
  }

  # Validación de columnas críticas
  req_cols <- c("W_SEE", "RMSD_Ref", "Method", "Selected")
  if (!all(req_cols %in% names(tradeoff_data))) {
    warning("[Module D] Faltan columnas críticas en decision data para Trade-off Plot")
    return(NULL)
  }

  # Filtrar NAs
  tradeoff_data <- tradeoff_data[!is.na(tradeoff_data$W_SEE) & !is.na(tradeoff_data$RMSD_Ref), ]
  if (nrow(tradeoff_data) == 0) {
    return(NULL)
  }

  # Umbrales Críticos
  dtm_val <- config$equating$dtm_raw_score %||% 0.4

  # Clasificación visual
  tradeoff_data <- tradeoff_data |>
    mutate(
      Type = ifelse(grepl("TUCKER|LEVINE|IDENTITY", Method), "Lineal (Estable)", "Curvilíneo (Flexible)"),
      Alpha = ifelse(Selected, 1, 0.6),
      Size = ifelse(Selected, 3.5, 2)
    )

  ggplot(tradeoff_data, aes(x = RMSD_Ref, y = W_SEE)) +
    # Zona Verde: RMSD bajo
    annotate("rect",
      xmin = 0, xmax = dtm_val, ymin = -Inf, ymax = Inf,
      fill = "#e6f5c9", alpha = 0.4
    ) +

    # Línea DTM
    geom_vline(xintercept = dtm_val, linetype = "dashed", color = "#4d9221", linewidth = 0.8) +

    # Puntos
    geom_point(aes(shape = Type, color = Selected, size = Size, alpha = Alpha)) +

    # Etiquetas (solo ganadores y lejanos)
    ggrepel::geom_text_repel(
      aes(label = Method, color = Selected),
      size = 3, box.padding = 0.5, max.overlaps = 15,
      fontface = ifelse(tradeoff_data$Selected, "bold", "plain")
    ) +
    facet_wrap(~FORM, scales = "free") +
    scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "#d95f02")) +
    scale_shape_manual(values = c("Lineal (Estable)" = 15, "Curvilíneo (Flexible)" = 19)) +
    scale_size_identity() +
    scale_alpha_identity() +
    labs(
      title = "Análisis de Decisión: Precisión (SEE) vs. Ajuste (RMSD)",
      subtitle = "Justificación de la selección. Preferencia: Izquierda (Menor Sesgo) y Abajo (Menor Error).",
      x = "Indicador de Curvatura/Sesgo (RMSD)",
      y = "Inestabilidad / Error Estándar (Weighted SEE)",
      caption = "Línea punteada: DTM (Difference That Matters). Si RMSD > DTM, se considera curvatura real."
    ) +
    theme_audit_analysis() +
    theme(legend.position = "bottom")
}

# ==============================================================================
# 2. COMPARATIVA MULTI-FORMA (FUNCIÓN DE CONVERSIÓN)
# ==============================================================================

plot_equating_functions_overlay <- function(eq_results) {
  conc_data <- adapt_concordance_data(eq_results)

  if (is.null(conc_data) || nrow(conc_data) == 0) {
    return(NULL)
  }

  ggplot(conc_data, aes(x = RAW, y = EQUATED, color = FORM, group = FORM)) +
    # Identidad
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.8) +
    annotate("text",
      x = max(conc_data$RAW, na.rm = TRUE) * 0.9, y = max(conc_data$RAW, na.rm = TRUE) * 0.9,
      label = "Identidad", angle = 45, color = "gray60", vjust = -0.5
    ) +

    # Curvas
    geom_line(linewidth = 1, alpha = 0.8) +
    coord_fixed() +
    labs(
      title = "Comparativa de Funciones de Equiparación",
      subtitle = "Superposición de conversiones Raw-to-Scale para todas las formas",
      x = "Puntaje Crudo (Raw Score)",
      y = "Puntaje Equiparado (Global Scale)",
      caption = "Curvas superiores: Formas más difíciles. Curvas inferiores: Formas más fáciles."
    ) +
    theme_audit_analysis()
}

# ==============================================================================
# 3. ORQUESTADOR MÓDULO D (Con Shim Legacy)
# ==============================================================================

export_module_d_analysis <- function(eq_results = NULL, config, base_dir = NULL) {
  # --- CAPA DE COMPATIBILIDAD ---

  if (is.null(base_dir)) base_dir <- getwd()

  if (is.null(eq_results)) {
    warning("[Module D] No data provided.")
    return(NULL)
  }

  if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)
  if (exists("info")) debug(">>> Generando gráficos Módulo D (Análisis Estratégico)...")

  # D.1. Trade-off Plot
  execute_safely(
    expr = {
      p1 <- plot_selection_tradeoff(eq_results, config)
      if (!is.null(p1)) ggsave(file.path(base_dir, "D01_Selection_Tradeoff.pdf"), p1, width = 11, height = 7)
    },
    desc = "Gráfico Trade-off"
  )

  # D.2. Overlay Plot
  execute_safely(
    expr = {
      p2 <- plot_equating_functions_overlay(eq_results)
      if (!is.null(p2)) ggsave(file.path(base_dir, "D02_Equating_Functions_Overlay.pdf"), p2, width = 9, height = 8)
    },
    desc = "Gráfico Overlay"
  )
}
