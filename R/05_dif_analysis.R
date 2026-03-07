# R/07_dif_analysis.R
# Responsabilidad: Detección de Funcionamiento Diferencial del Ítem (DIF).
# Versión: v1.0
# Dependencias: 00_common.R

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, readr, tidyr)

# ==============================================================================
# LÓGICA DE CÁLCULO
# ==============================================================================

#' Prepara los datos para el análisis DIF
#' @param response_df Dataframe de respuestas.
#' @param demographic_df Dataframe de datos demográficos.
#' @param col_group Nombre de la columna de grupo en datos demográficos.
#' @param ref_grp Grupo de referencia.
#' @param foc_grp Grupo focal.
#' @return Dataframe procesado para DIF o NULL si no es válido.
prepare_dif_data <- function(response_df, demographic_df, col_group, ref_grp, foc_grp) {
  # Validar existencia de columna de grupo en demográficos
  if (!col_group %in% names(demographic_df)) {
    warn(sprintf("SKIPPING DIF: Columna de grupo '%s' no encontrada en datos demográficos.", col_group))
    return(NULL)
  }

  # 2. Fusión de Datos (Join por ID)
  # Seleccionamos solo lo necesario para evitar duplicados
  demo_subset <- demographic_df |>
    dplyr::select(ID, GROUP = !!sym(col_group)) |>
    dplyr::distinct(ID, .keep_all = TRUE)

  # Unir respuestas con grupo
  df_proc <- response_df |>
    dplyr::inner_join(demo_subset, by = "ID") |>
    dplyr::filter(GROUP %in% c(ref_grp, foc_grp))

  if (nrow(df_proc) == 0) {
    warn("SKIPPING DIF: No hay intersección de IDs entre respuestas y demográficos.")
    return(NULL)
  }

  # Validar tamaño de muestra global antes de empezar
  n_counts <- table(df_proc$GROUP)
  if (min(n_counts) < 50) {
    warn(sprintf("SALTANDO DIF: Muestra insuficiente (<50) en grupos: %s", paste(names(n_counts), n_counts, sep = "=", collapse = ", ")))
    return(NULL)
  }

  return(df_proc)
}

#' Crea estratos de score para el dataframe actual
#' @param current_df Dataframe actual con la columna SCORE.
#' @return Dataframe con la columna Bin añadida y filtrado por !is.na(Bin).
create_score_strata <- function(current_df) {
  # Estratificación dinámica
  n_scores <- length(unique(current_df$SCORE))
  if (n_scores > 20) {
    # Deciles con manejo de empates
    current_df$Bin <- as.numeric(cut(current_df$SCORE,
      breaks = unique(quantile(current_df$SCORE, probs = seq(0, 1, 0.1), na.rm = TRUE)),
      include.lowest = TRUE, labels = FALSE
    ))
  } else {
    # Score directo si son pocos niveles
    current_df$Bin <- as.factor(current_df$SCORE)
  }

  return(current_df |> filter(!is.na(Bin)))
}

#' Calcula estadísticas Mantel-Haenszel por ítem
#' @param current_df Dataframe actual con score y Bin.
#' @param item Ítem a analizar.
#' @param ref_grp Grupo de referencia.
#' @param foc_grp Grupo focal.
#' @return Dataframe con 1 fila de resultados MH o NULL.
compute_item_mh_stats <- function(current_df, item, ref_grp, foc_grp) {
  # Evitar items constantes (todos 0 o todos 1)
  if (var(current_df[[item]], na.rm = TRUE) == 0) return(NULL)

  # Tabla de contingencia por estrato
  # Estructura: Bin | Group | R_correct | N_total
  agg <- current_df |>
    group_by(Bin, GROUP) |>
    summarise(
      R = sum(!!sym(item), na.rm = TRUE),
      N = n(),
      .groups = "drop"
    ) |>
    pivot_wider(names_from = GROUP, values_from = c(R, N), values_fill = 0)

  # Nombres dinámicos según grupos
  cr_R <- paste0("R_", ref_grp)
  cf_R <- paste0("R_", foc_grp)
  cr_N <- paste0("N_", ref_grp)
  cf_N <- paste0("N_", foc_grp)

  if (all(c(cr_R, cf_R, cr_N, cf_N) %in% names(agg))) {
    agg <- agg |>
      mutate(
        A = !!sym(cr_R), # Ref Correct
        B = !!sym(cr_N) - !!sym(cr_R), # Ref Wrong
        C = !!sym(cf_R), # Foc Correct
        D = !!sym(cf_N) - !!sym(cf_R), # Foc Wrong
        T = !!sym(cr_N) + !!sym(cf_N) # Total N
      ) |>
      filter(T > 0)

    # Mantel-Haenszel Common Odds Ratio
    # alpha_MH = sum(A*D / T) / sum(B*C / T)
    num <- sum((agg$A * agg$D) / agg$T, na.rm = TRUE)
    den <- sum((agg$B * agg$C) / agg$T, na.rm = TRUE)

    if (den > 0) {
      alpha_mh <- num / den
      delta_mh <- -2.35 * log(alpha_mh) # ETS Delta scale

      # Clasificación ETS
      abs_d <- abs(delta_mh)
      flag <- "A (Negligible)"
      if (abs_d >= 1.5) {
        flag <- "C (Large)"
      } else if (abs_d >= 1.0) flag <- "B (Moderate)"

      return(data.frame(
        ITEM = item,
        MH_Alpha = round(alpha_mh, 4),
        ETS_Delta = round(delta_mh, 4),
        FLAG = flag,
        Favors = ifelse(delta_mh > 0, foc_grp, ref_grp)
      ))
    }
  }
  return(NULL)
}

#' Calcula estadísticas Mantel-Haenszel
#' @param df Dataframe actual.
#' @param items_to_test Ítems a analizar.
#' @param matching_items Ítems a utilizar para el score de matching.
#' @param ref_grp Grupo de referencia.
#' @param foc_grp Grupo focal.
#' @return Dataframe con resultados MH.
calculate_mh_stats <- function(df, items_to_test, matching_items, ref_grp, foc_grp) {
  # Pre-calcular el score base de matching
  base_score <- rowSums(df[, matching_items, drop = FALSE], na.rm = TRUE)

  res_list <- lapply(items_to_test, function(item) {
    item_df <- df

    # Si el ítem es parte de los ítems de matching, su score ya está incluido.
    if (item %in% matching_items) {
      item_df$SCORE <- base_score
    } else {
      # Si no, agregarlo al score base para evitar sesgo de correlación parte-todo
      item_val <- item_df[[item]]
      item_val[is.na(item_val)] <- 0
      item_df$SCORE <- base_score + item_val
    }

    item_df <- create_score_strata(item_df)
    compute_item_mh_stats(item_df, item, ref_grp, foc_grp)
  })

  return(do.call(rbind, res_list))
}

#' Realiza la purificación para el análisis DIF
#' @param df_proc Dataframe procesado.
#' @param stage1_res Resultados de la etapa 1.
#' @param item_names Nombres de los ítems.
#' @param ref_grp Grupo de referencia.
#' @param foc_grp Grupo focal.
#' @return Resultados finales de DIF.
perform_dif_purification <- function(df_proc, stage1_res, item_names, ref_grp, foc_grp) {
# Identificar ítems 'C' (Severos) para excluirlos del score de matching
  bad_items <- stage1_res |>
    filter(FLAG == "C (Large)") |>
    pull(ITEM)

  if (length(bad_items) > 0) {
    debug(sprintf("DIF: Purificación activada. Excluyendo %d ítems con DIF severo del score.", length(bad_items)))

    # Recalcular score SIN ítems severos
    valid_items_for_score <- setdiff(item_names, bad_items)

    # Si nos quedamos sin items para el score, abortamos purificación
    if (length(valid_items_for_score) < 5) {
      warn("DIF: Purificación abortada. Quedan muy pocos ítems (<5) para calcular un score confiable.")
      return(stage1_res)
    }

    # Recalcular DIF para todos los ítems usando el score purificado
    debug("DIF: Etapa 2 (Recálculo Purificado)...")
    final_res <- calculate_mh_stats(df_proc, item_names, valid_items_for_score, ref_grp, foc_grp)
    return(final_res)
  } else {
    debug("DIF: No se detectaron ítems con DIF severo en Etapa 1. Purificación no requerida.")
    return(stage1_res)
  }
}

#' Ejecuta análisis DIF (Mantel-Haenszel) con opción de Purificación
#' @param response_df Dataframe de respuestas.
#' @param demographic_df Dataframe de datos demográficos.
#' @param item_names Vector con los nombres de los ítems a evaluar.
#' @param config Objeto de configuración.
#' @param score_vec Vector de score (no utilizado, preservado por firma).
#' @param purify Booleano. Si TRUE, recalcula el score excluyendo ítems con DIF severo (Nivel C) en una segunda etapa.
#' @return Dataframe con resultados de DIF o NULL.
run_dif_analysis <- function(response_df, demographic_df, item_names, config, score_vec = NULL, purify = TRUE) {
  # 1. Validaciones de Configuración
  if (is.null(config$dif) || !isTRUE(config$dif$enabled)) {
    debug("Análisis DIF deshabilitado en configuración.")
    return(NULL)
  }

  col_group <- config$dif$group_col
  ref_grp <- config$dif$reference_group
  foc_grp <- config$dif$focal_group

  # 2. Fusión de Datos (Join por ID)
  df_proc <- prepare_dif_data(response_df, demographic_df, col_group, ref_grp, foc_grp)
  if (is.null(df_proc)) return(NULL)

  debug(sprintf(
    "Iniciando DIF (MH). Items: %d | N: %d | Grupo: %s (%s vs %s)",
    length(item_names), nrow(df_proc), col_group, ref_grp, foc_grp
  ))

  # ETAPA 1: CÁLCULO INICIAL
  debug("DIF: Etapa 1 (Inicial)...")
  stage1_res <- calculate_mh_stats(df_proc, item_names, item_names, ref_grp, foc_grp)

  if (is.null(stage1_res) || !purify) {
    return(stage1_res)
  }

  # ETAPA 2: PURIFICACIÓN
  return(perform_dif_purification(df_proc, stage1_res, item_names, ref_grp, foc_grp))
}

# ==============================================================================
# 7. DIF EXPORT
# ==============================================================================
export_dif_results <- function(dif_results, config) {
  if (is.null(dif_results) || nrow(dif_results) == 0) {
    return()
  }
  out_dir <- file.path(config$project$output_dir, "AUDITORIA_TECNICA", "04_DIF_ANALYSIS")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Guardar CSV completo
  readr::write_csv(dif_results, file.path(out_dir, "DIF_MantelHaenszel_Full.csv"))
  debug(paste("Resultados DIF exportados a:", out_dir))

  # Guardar alertas si existen
  if ("FLAG" %in% names(dif_results)) {
    severe_dif <- dif_results |> dplyr::filter(FLAG == "C (Large)")
    if (nrow(severe_dif) > 0) {
      readr::write_csv(severe_dif, file.path(out_dir, "DIF_Alerts_Level_C.csv"))
      warn(sprintf("ALERTA DIF: Se detectaron %d ítems con DIF Severo (Nivel C). Ver reporte.", nrow(severe_dif)))
    }
  }
}
