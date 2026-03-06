# ==============================================================================
# MÓDULO DE TOPOLOGÍA DE RED Y ESTRUCTURA DE ENLACES
# Versión: v1.0
# Responsabilidad: Visualizar la arquitectura de conexiones entre formas.
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, ggrepel, igraph, tidyr)

if (!exists("execute_safely")) source("00_common.R")

# ==============================================================================
# 1. TEMA GRÁFICO: REDES (Limpio y sin ejes)
# ==============================================================================
theme_audit_network <- function() {
  theme_void(base_size = 12) +
    theme(
      text = element_text(color = "black", family = "sans"),
      plot.margin = margin(25, 25, 25, 25),
      # Títulos jerárquicos
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 5)),
      plot.subtitle = element_text(size = 11, color = "#404040", hjust = 0.5, margin = margin(b = 20)),
      plot.caption = element_text(size = 9, color = "#666666", hjust = 1, margin = margin(t = 10)),
      legend.position = "bottom"
    )
}

# ==============================================================================
# 2. VISUALIZACIÓN DE LA RED (NETWORK PLOT - B/W OPTIMIZED)
# ==============================================================================

plot_equating_network <- function(eq_results, ref_id = NULL) {
  # 1. Validación de Datos
  if (is.null(eq_results$link_quality) || nrow(eq_results$link_quality) == 0) {
    if (!is.null(logger)) warn("No hay datos de topología para graficar.")
    return(NULL)
  }

  edges_raw <- eq_results$link_quality
  # Normalización de nombres de columnas (Case insensitive)
  names(edges_raw) <- tolower(names(edges_raw))

  # Filtrado y selección de escala GLOBAL
  edge_names <- names(edges_raw)
  plot_data <- edges_raw |>
    dplyr::filter(scale == "GLOBAL") |>
    dplyr::mutate(
      source = from,
      target = to,
      corr = if ("r_anchor" %in% edge_names) r_anchor else (if ("r" %in% edge_names) r else NA),
      n_anc = if ("n_anchor" %in% edge_names) n_anchor else (if ("n_anc" %in% edge_names) n_anc else 0),
      scale = NULL
    ) |>
    dplyr::filter(!is.na(source), !is.na(target))

  if (nrow(plot_data) == 0) {
    return(NULL)
  }

  # 2. Identificación del Nodo Referencia
  final_ref <- ref_id
  if (is.null(final_ref) && !is.null(plot_data$to)) {
    final_ref <- plot_data$to
  }
  # Fallback: Nodo con más conexiones entrantes (in-degree)
  if (is.null(final_ref)) {
    counts <- table(plot_data$target)
    final_ref <- names(counts)[which.max(counts)]
  }

  # 3. Construcción del Grafo y Layout
  g <- igraph::graph_from_data_frame(plot_data, directed = TRUE)

  # Semilla fija para consistencia visual, restaurando estado posterior
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(42)

  # Layout Kamada-Kawai (Fuerza dirigida)
  layout_coords <- as.data.frame(igraph::layout_with_kk(g))

  if (!is.null(old_seed)) {
    assign(".Random.seed", old_seed, envir = .GlobalEnv)
  } else {
    rm(.Random.seed, envir = .GlobalEnv)
  }

  colnames(layout_coords) <- c("x", "y")
  layout_coords$Form <- names(igraph::V(g))

  # Clasificación de Nodos para Estética (Blanco/Negro)
  layout_coords <- layout_coords |>
    dplyr::mutate(
      Type = ifelse(Form == final_ref, "REFERENCIA", "FORMA"),
      # Lógica visual explícita para control total
      Fill_Color = ifelse(Type == "REFERENCIA", "black", "white"),
      Text_Color = ifelse(Type == "REFERENCIA", "white", "black")
    )

  # 4. Preparación de Aristas (Flechas)
  edge_coords <- plot_data |>
    dplyr::left_join(layout_coords, by = c("source" = "Form")) |>
    dplyr::rename(x_start = x, y_start = y) |>
    dplyr::left_join(layout_coords, by = c("target" = "Form")) |>
    dplyr::rename(x_end = x, y_end = y) |>
    dplyr::mutate(
      Mid_X = (x_start + x_end) / 2,
      Mid_Y = (y_start + y_end) / 2,

      # Lógica de Alerta Visual
      Is_Weak = corr < 0.95,
      Label = paste0("r=", round(corr, 3), "\nn=", n_anc),

      # Estilos de Línea
      Line_Type = ifelse(Is_Weak, "dashed", "solid"),
      Line_Width = ifelse(Is_Weak, 0.8, 1.0) # Línea débil un poco más fina o punteada
    )

  # 5. Renderizado Gráfico (GGPLOT2)
  ggplot() +
    # A. ARISTAS (Líneas)
    geom_segment(
      data = edge_coords,
      aes(x = x_start, y = y_start, xend = x_end, yend = y_end, linetype = I(Line_Type)),
      arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
      color = "gray40", # Gris oscuro neutro para impresión
      linewidth = 0.8
    ) +

    # B. ETIQUETAS DE ENLACE (Cajas blancas)
    geom_label(
      data = edge_coords,
      aes(x = Mid_X, y = Mid_Y, label = Label),
      size = 2.5, fill = "white", linewidth = 0.2, alpha = 1.0,
      color = "gray20", family = "sans"
    ) +

    # C. NODOS (Círculos)
    geom_point(
      data = layout_coords,
      aes(x = x, y = y, fill = I(Fill_Color)),
      size = 16, shape = 21, color = "black", stroke = 1.0
    ) +

    # D. ETIQUETAS DE NODO (Texto dentro del círculo)
    geom_text(
      data = layout_coords,
      aes(x = x, y = y, label = Form, color = I(Text_Color)),
      size = 2.0, fontface = "bold"
    ) +
    labs(
      title = "Topología de Red de Equiparación",
      subtitle = paste("Referencia (Ancla):", final_ref),
      caption = "Nota: Líneas discontinuas indican correlación < 0.95 (Enlace débil).\nCírculo Negro: Forma Referencia. Círculo Blanco: Forma Nueva.",
      x = NULL, y = NULL
    ) +
    theme_audit_network()
}

# ==============================================================================
# 2. MATRIZ DE CALIDAD DE ENLACES (TEXT REPORT)
# ==============================================================================

audit_topology_summary <- function(eq_results, base_dir) {
  if (is.null(eq_results$topology_edges)) {
    return()
  }

  out_dir <- file.path(base_dir)
  if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  report_file <- file.path(out_dir, "AUDIT_TOPOLOGY_LINKS.txt")

  sink(report_file)
  on.exit(sink())

  cat("================================================================================\n")
  cat(" AUDITORÍA DE TOPOLOGÍA DE RED Y CALIDAD DE ENLACES\n")
  cat("================================================================================\n\n")

  # Normalización de datos
  edges_raw <- eq_results$link_quality
  names(edges_raw) <- toupper(names(edges_raw))

  # Mapeo robusto de columnas
  if ("R_ANCHOR" %in% names(edges_raw)) edges_raw$R <- edges_raw$R_ANCHOR
  if ("SMD_ANCHOR" %in% names(edges_raw)) edges_raw$SMD <- edges_raw$SMD_ANCHOR
  if ("N_ANCHOR" %in% names(edges_raw)) edges_raw$N_ANC <- edges_raw$N_ANCHOR

  edges <- edges_raw |>
    dplyr::filter(SCALE == "GLOBAL") |>
    dplyr::mutate(
      QUALITY = dplyr::case_when(
        R_ANCHOR >= 0.95 ~ "MUY BUENO",
        R_ANCHOR >= 0.90 ~ "BUENO",
        R_ANCHOR >= 0.85 ~ "ACEPTABLE",
        TRUE ~ "REVISAR"
      )
    )

  cat(sprintf("%-15s -> %-15s | %-6s | %-8s | %-10s\n", "ORIGEN", "DESTINO", "N", "CORR", "CALIDAD"))
  cat(paste(rep("-", 70), collapse = ""), "\n")

  for (i in seq_len(nrow(edges))) {
    row <- edges[i, ]
    cat(sprintf(
      "%-15s -> %-15s | %-6d | %-8.4f | %-10s\n",
      substr(row$FROM, 1, 15), substr(row$TO, 1, 15), row$N_ANC, row$R, row$QUALITY
    ))
  }
}

# ==============================================================================
# 3. WRAPPER DE EXPORTACIÓN
# ==============================================================================

export_topology_audit <- function(eq_results, config, base_dir) {
  if (is.null(eq_results$link_quality$TO)) {
    return()
  }

  ref_id <- tryCatch(config$system$reference_form, error = function(e) NULL)

  debug(">>> Generando Topología de Red...")

  # 1. Gráfico
  execute_safely(
    expr = {
      p <- plot_equating_network(eq_results, ref_id)
      if (!is.null(p)) ggsave(file.path(base_dir, "00_Network_Topology_Map.pdf"), p, width = 8, height = 6)
    },
    desc = "Mapa de Red"
  )

  # 2. Texto
  execute_safely(
    expr = {
      audit_topology_summary(eq_results, base_dir)
    },
    desc = "Reporte Texto Topología"
  )
}
