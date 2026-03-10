# ==============================================================================
# MÓDULO A: EVIDENCIA DE VALIDEZ ESTRUCTURAL (PRE-EQUATING)
# ==============================================================================
# Responsabilidad: Auditoría de Dimensionalidad y Análisis Forense de Ítems.
# Dependencias: 00_common.R
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, dplyr, tidyr, cowplot, ggrepel, scales, psych)

if (!exists("execute_safely")) source("00_common.R")

# ==============================================================================
# 1. TEMA GRÁFICO (Monocromático)
# ==============================================================================
theme_audit_structural <- function() {
  theme_bw(base_size = 11) +
    theme(
      text = element_text(color = "black", family = "sans"),
      plot.title = element_blank(),
      plot.subtitle = element_text(size = 10, color = "#404040", face = "bold"),
      plot.caption = element_text(size = 8, color = "#666666", hjust = 0, margin = margin_auto(t = 10)),
      panel.grid.major = element_line(color = "#E5E5E5", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", linewidth = 0.8),
      strip.background = element_rect(fill = "#F0F0F0", color = "black"),
      strip.text = element_text(face = "bold", size = 9),
      axis.title = element_text(face = "bold", size = 9),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.box.background = element_rect(color = "black", linewidth = 0.2),
      legend.key = element_rect(fill = "white")
    )
}


# ==============================================================================
# 2. VISUALIZACIÓN DE DIMENSIONALIDAD (SCREE PLOTS BÁSICO)
# ==============================================================================

plot_scree_evidence <- function(ctt_results) {
  # REQUISITO: ctt_results debe contener $eigenvalues (data.frame: FORMA, COMPONENT, EIGENVALUE)
  # Si solo existe el resumen ($dimensionality), generamos un gráfico de barras de Ratios.

  if (is.null(ctt_results$eigenvalues) && !is.null(ctt_results$dimensionality)) {
    # Fallback: Gráfico de Ratios si no hay eigenvalores crudos
    return(.plot_dim_ratio_fallback(ctt_results$dimensionality))
  }

  if (is.null(ctt_results$eigenvalues)) {
    return(NULL)
  }

  plot_data <- ctt_results$eigenvalues |>
    dplyr::filter(COMPONENT <= 10) # Enfocamos en los primeros 10 factores

  # Unir con flags para anotaciones
  if (!is.null(ctt_results$dimensionality)) {
    plot_data <- plot_data |>
      dplyr::left_join(
        ctt_results$dimensionality |> dplyr::select(FORMA, RATIO_1_2, DIM_FLAG),
        by = "FORMA"
      )
  }

  ggplot(plot_data, aes(x = COMPONENT, y = EIGENVALUE)) +
    geom_line(color = "black", linewidth = 0.7) +
    geom_point(shape = 21, fill = "white", size = 2) +

    # Criterio de Kaiser (Eigenvalue > 1)
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray60") +
    annotate("text", x = 10, y = 1.1, label = "Kaiser (Ev > 1)", hjust = 1, vjust = 0, size = 2.5, fontface = "italic") +
    facet_wrap(~FORMA, scales = "free_y") +
    geom_text(
      data = .get_unique_dim_labels(plot_data),
      aes(x = Inf, y = Inf, label = Label),
      hjust = 1.1, vjust = 1.5, size = 3, fontface = "bold", inherit.aes = FALSE
    ) +
    scale_x_continuous(breaks = 1:10) +
    labs(
      x = "Componente / Factor",
      y = "Autovalor (Eigenvalue)",
      subtitle = "Evidencia de Unidimensionalidad Esencial (Scree Plot)",
      caption = "Nota: Un codo claro en el componente 1 y un Ratio 1:2 > 3.0 soportan el supuesto de unidimensionalidad."
    ) +
    theme_audit_structural()
}

# Helper interno para etiquetas únicas
.get_unique_dim_labels <- function(df) {
  df |>
    dplyr::select(FORMA, RATIO_1_2, DIM_FLAG) |>
    dplyr::distinct() |>
    dplyr::mutate(
      Label = paste0(
        "Ratio 1:2 = ", round(RATIO_1_2, 1), "\n",
        ifelse(grepl("MULTI", DIM_FLAG), "[!]", "[OK]")
      )
    )
}

.plot_dim_ratio_fallback <- function(dim_summary) {
  # Gráfico alternativo cuando faltan datos crudos
  ggplot(dim_summary, aes(x = FORMA, y = RATIO_1_2)) +
    geom_col(fill = "gray80", color = "black", width = 0.6) +
    geom_hline(yintercept = 3, linetype = "dashed", color = "black") +
    geom_text(aes(label = round(RATIO_1_2, 2)), vjust = -0.5) +
    annotate("text", x = 0.5, y = 3.1, label = "Criterio Recomendado (>3.0)", hjust = 0, size = 3) +
    labs(
      x = NULL, y = "Ratio Primer/Segundo Autovalor",
      subtitle = "Indicador de Dominancia del Primer Factor",
      caption = "Nota: Datos de autovalores crudos no disponibles. Se reporta el ratio de varianza."
    ) +
    theme_audit_structural()
}

# ==============================================================================
# 3. ANÁLISIS FORENSE DE ÍTEMS (TRACE LINES DE DISTRACTORES)
# ==============================================================================

plot_forensic_item_trace <- function(ctt_results, item_id, flag_reason = "") {
  # 1. Extracción y limpieza
  invalid_options <- c(" ", ".", "*")
  dist_data <- ctt_results$distractors |>
    dplyr::filter(ITEM == item_id) |>
    dplyr::filter(!OPTION %in% invalid_options)

  if (nrow(dist_data) == 0) {
    return(NULL)
  }

  key_val <- unique(dist_data$KEY)
  p_val <- tryCatch(ctt_results$stats$P_VAL[ctt_results$stats$ITEM == item_id], error = function(e) NA)
  r_bis <- tryCatch(ctt_results$stats$P_BIS[ctt_results$stats$ITEM == item_id], error = function(e) NA)

  # 2. Preparación de datos (Tidy format)
  cols_req <- c("PROP_LOW", "PROP_MIDLOW", "PROP_MIDHIGH", "PROP_HIGH")
  plot_df <- dist_data |>
    dplyr::select(OPTION, KEY, dplyr::all_of(cols_req)) |>
    tidyr::pivot_longer(cols = dplyr::all_of(cols_req), names_to = "Group", values_to = "Prop") |>
    dplyr::mutate(
      Group_Num = dplyr::case_when(
        Group == "PROP_LOW" ~ 1,
        Group == "PROP_MIDLOW" ~ 2,
        Group == "PROP_MIDHIGH" ~ 3,
        Group == "PROP_HIGH" ~ 4
      ),
      Is_Key = (toupper(trimws(OPTION)) == toupper(trimws(KEY))),
      Line_Type = ifelse(Is_Key, "Clave", "Distractor")
    )

  # 3. Construcción de la gráfica
  g <- ggplot(plot_df, aes(x = Group_Num, y = Prop, group = OPTION)) +
    # Línea de azar teórica
    geom_hline(yintercept = 1 / nrow(dist_data), linetype = "dotted", color = "gray70") +
    # Trazos de las opciones
    geom_line(aes(color = Line_Type, linewidth = Line_Type, linetype = Line_Type)) +
    geom_point(aes(fill = Line_Type), shape = 21, size = 2.5, color = "white", stroke = 0.5) +
    # Etiquetas de opciones (se muestran en el Group_Num == 4)
    geom_text(
      data = dplyr::filter(plot_df, Group_Num == 4),
      aes(label = OPTION, color = Line_Type),
      hjust = -0.5, fontface = "bold", size = 3.5
    ) +
    # Escalas manuales
    scale_color_manual(values = c("Clave" = "#000000", "Distractor" = "#808080")) +
    scale_fill_manual(values = c("Clave" = "#000000", "Distractor" = "#808080")) +
    scale_linewidth_manual(values = c("Clave" = 1.2, "Distractor" = 0.6)) +
    scale_linetype_manual(values = c("Clave" = "solid", "Distractor" = "longdash")) +
    # Corrección de límites y etiquetas del eje X
    scale_x_continuous(
      breaks = 1:4,
      labels = c("Bajo", "M. Bajo", "M. Alto", "Alto"),
      limits = c(0.8, 4.5), # Aumentado para incluir el grupo 4 y etiquetas
      expand = expansion(mult = c(0.05, 0.1))
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent_format(accuracy = 1),
      breaks = seq(0, 1, 0.2)
    ) +
    labs(
      title = paste("Análisis de Distractores - Ítem:", item_id),
      subtitle = sprintf("Clave: %s | p=%.2f | r_bis=%.2f", key_val, p_val, r_bis),
      caption = if (flag_reason != "") paste("ALERTA:", flag_reason) else NULL,
      x = "Nivel de Habilidad (Grupos)",
      y = "Probabilidad de Selección"
    )

  # Aplicación de tema con fallback
  if (exists("theme_audit_structural")) {
    g <- g + theme_audit_structural()
  } else {
    g <- g + theme_minimal()
  }

  g + theme(legend.position = "none") # Las etiquetas de texto reemplazan la leyenda
}

# ==============================================================================
# 4. GRÁFICA SCREE CALCULADA (DATA DRIVEN)
# ==============================================================================

plot_calculated_scree <- function(calib_df, ctt_results = NULL, config = NULL) {
  if (is.null(calib_df)) {
    return(NULL)
  }

  # Detectar columnas de ítems (asumiendo que las columnas de metadatos son fijas o pocas)
  # Excluir columnas ID, FORMA y otras metadatas conocidas
  meta_cols <- c(
    "ID", "PERSON_ID", "IDENTIFICADOR", "FORMA", "SEXO", "GENDER", "SEX",
    "REGION", "ZONA", "UBICACION", "LOCATION", "PROVINCIA",
    "Score_Final", "SCORE", "TOTAL", "CASE", "CLUSTER", "Nivel"
  )
  forms <- unique(calib_df$FORMA)
  plot_data_list <- list()

  for (frm in forms) {
    form_data <- calib_df |> dplyr::filter(FORMA == frm)

    # Identificar items:
    item_cols <- NULL

    # 1. Prioridad: Usar items ya identificados por el motor CTT si están disponibles
    if (!is.null(ctt_results) && !is.null(ctt_results$items)) {
      item_cols <- intersect(names(form_data), ctt_results$items)
    }

    # 2. Fallback 1: Usar prefijo de configuración si está disponible
    if (is.null(item_cols) || length(item_cols) == 0) {
      pfx <- config$specs$data_structure$item_prefix
      if (!is.null(pfx)) {
        pos_pattern <- paste0("^", pfx, "[0-9]+$")
        item_cols <- grep(pos_pattern, names(form_data), value = TRUE)
      }
    }

    # 3. Fallback 2: Lógica actual mejorada con meta_cols extendido
    if (is.null(item_cols) || length(item_cols) == 0) {
      item_cols <- form_data |>
        dplyr::select(where(is.numeric)) |>
        names()
      item_cols <- setdiff(item_cols, meta_cols)
    }

    if (length(item_cols) < 3) next # Necesitamos al menos 3 items

    mat_items <- form_data |>
      dplyr::select(all_of(item_cols)) |>
      as.matrix()

    # Calcular correlación (Pearson es rápido y suficiente para scree inicial)
    # Manejo de NA: pairwise.complete.obs
    cor_mat <- cor(mat_items, use = "pairwise.complete.obs")

    # Calcular Eigenvalues
    # Reemplazar NAs en cor_mat con 0 si quedan (casos raros)
    cor_mat[is.na(cor_mat)] <- 0

    ev <- eigen(cor_mat)$values

    # Preparar data frame para plot (Top 10 factores)
    n_factors <- min(length(ev), 10)

    df_ev <- data.frame(
      FORMA = frm,
      Component = 1:n_factors,
      Eigenvalue = ev[1:n_factors]
    )

    plot_data_list[[frm]] <- df_ev
  }

  full_plot_data <- do.call(rbind, plot_data_list)
  if (is.null(full_plot_data)) {
    return(NULL)
  }

  ggplot(full_plot_data, aes(x = Component, y = Eigenvalue)) +
    geom_line(color = "black", linewidth = 0.8) +
    geom_point(shape = 21, fill = "gray80", size = 2.5) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    facet_wrap(~FORMA, scales = "free_y") +
    scale_x_continuous(breaks = seq(1, 20, 2)) +
    labs(
      x = "Número de Factor",
      y = "Autovalor (Eigenvalue)",
      subtitle = "Análisis de Sedimentación (Scree Plot) Calculado",
      caption = "Cálculo basado en matriz de correlación de ítems respondidos (calib_df).\nLínea punteada: Criterio de Kaiser (Eigenvalue = 1)."
    ) +
    theme_audit_structural()
}


# ==============================================================================
# 5. ORQUESTADOR DEL MÓDULO A (Export Wrapper)
# ==============================================================================

export_module_a_structural <- function(ctt_results, calib_df, config, base_dir) {
  debug(">>> Iniciando Módulo A: Validez Estructural")

  # --- 1. EVIDENCIA DE DIMENSIONALIDAD (PRE-CALCULADA) ---
  execute_safely(
    expr = {
      p_scree <- plot_scree_evidence(ctt_results)
      if (!is.null(p_scree)) {
        save_plot_safe(p_scree, file.path(base_dir, "A01_Dimensionality_Scree.pdf"), width = 8, height = 6)
      } else {
        warn("No hay datos pre-calculados para Scree Plot.")
      }
    },
    desc = "Gráfico Scree Plot (Pre-calculado)"
  )

  # --- 1.b. EVIDENCIA DE DIMENSIONALIDAD (CALCULADA DESDE RAW) ---
  if (!is.null(calib_df)) {
    execute_safely(
      expr = {
        p_scree_calc <- plot_calculated_scree(calib_df, ctt_results, config)
        if (!is.null(p_scree_calc)) {
          save_plot_safe(p_scree_calc, file.path(base_dir, "A01b_Dimensionality_Scree_Calculated.pdf"), width = 8, height = 6)
        }
      },
      desc = "Gráfico Scree Plot (Calculado)"
    )
  }

  # --- 2. AUDITORÍA DE ÍTEMS PROBLEMÁTICOS ---
  execute_safely(
    expr = {
      bad_items_df <- ctt_results$stats |>
        dplyr::filter(P_BIS < 0.15) |>
        dplyr::mutate(Reason = dplyr::case_when(
          P_BIS < 0 ~ "FATAL: Discrim. Negativa",
          P_BIS < 0.15 ~ "ALERTA: Discrim. Baja",
          TRUE ~ "Revisar"
        )) |>
        dplyr::arrange(P_BIS)

      if (nrow(bad_items_df) > 0) {
        debug(sprintf("Generando trazas para %d ítems problemáticos...", nrow(bad_items_df)))

        plot_list <- list()
        top_bad_items <- head(bad_items_df, 20)

        for (i in 1:nrow(top_bad_items)) {
          it_id <- top_bad_items$ITEM[i]
          reason <- top_bad_items$Reason[i]

          p_item <- plot_forensic_item_trace(ctt_results, it_id, reason)
          if (!is.null(p_item)) plot_list[[length(plot_list) + 1]] <- p_item
        }

        if (length(plot_list) > 0) {
          pdf_path <- file.path(base_dir, "A02_Problematic_Items_Audit.pdf")
          pdf(pdf_path, width = 7, height = 5)
          for (p in plot_list) print(p)
          dev.off()
          debug(paste("Saved Multi-page PDF:", basename(pdf_path)))
        }
      }
    },
    desc = "Análisis Forense de Distractores"
  )
}
