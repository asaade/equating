# R/08_export_results.R
# Responsabilidad: Exportación final de bases de datos de resultados (Sustentantes)
# Versión: v1.0
# Dependencias: 00_common.R

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, readr)

# ==============================================================================
# 1. FUNCIÓN DE GUARDADO SEGURO
# ==============================================================================
save_result_file <- function(df, filename, out_dir) {
  if (is.null(df) || nrow(df) == 0) {
    warn(paste("Dataframe vacío, no se guardó:", filename))
    return()
  }

  path <- file.path(out_dir, filename)
  tryCatch(
    {
      # CSV estándar (UTF-8)
      readr::write_csv(df, path)
      debug(paste("📄 Resultados guardados:", filename))
    },
    error = function(e) {
      error(paste("Error guardando:", filename, "|", e$message))
    }
  )
}

# ==============================================================================
# 2. PREPARACIÓN DE VISTAS DE DATOS
# ==============================================================================

prepare_master_view <- function(scores_df) {
  # Identificar grupos de columnas
  cols_id <- c("ID", "FORMA", "Score_Final", "Nivel")
  cols_ctt <- grep("_CTT$", names(scores_df), value = TRUE)
  cols_sub <- grep("^(Raw_|Eq_|Sub_)", names(scores_df), value = TRUE)

  # Intersección con lo que realmente existe
  cols_ctt <- intersect(cols_ctt, names(scores_df))
  cols_sub <- intersect(cols_sub, names(scores_df))

  # Las que no son ni ID ni las especiales
  cols_other <- setdiff(names(scores_df), c(cols_id, cols_ctt, cols_sub))

  # Reordenar
  scores_df |>
    dplyr::select(
      dplyr::all_of(cols_id),
      dplyr::all_of(cols_ctt),
      dplyr::all_of(cols_sub),
      dplyr::all_of(cols_other)
    ) |>
    dplyr::arrange(ID)
}

prepare_simple_view <- function(scores_df) {
  # Vista ejecutiva: ID, Forma, Calificación Final y Nivel
  req_cols <- c("ID", "FORMA", "Score_Final", "Nivel")

  # Si existen subscores escalados (empiezan con Sub_), incluirlos
  cols_sub_scaled <- grep("^Sub_", names(scores_df), value = TRUE)

  final_cols <- intersect(c(req_cols, cols_sub_scaled), names(scores_df))

  scores_df |>
    dplyr::select(dplyr::all_of(final_cols)) |>
    dplyr::arrange(ID)
}

# ==============================================================================
# 3. ORQUESTADOR DE EXPORTACIÓN
# ==============================================================================

export_final_results <- function(final_scores, config) {
  if (is.null(final_scores) || nrow(final_scores) == 0) {
    error("No hay calificaciones finales para exportar.")
    return()
  }

  # Directorio de Salida
  base_dir <- config$project$output_dir
  out_dir <- file.path(base_dir, "CALIFICACIONES")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # 1. Reporte Maestro (Técnico Completo)
  df_master <- prepare_master_view(final_scores)
  save_result_file(df_master, "Resultados_Maestros_Detallados.csv", out_dir)

  # 2. Reporte Ejecutivo (Simple)
  df_simple <- prepare_simple_view(final_scores)
  save_result_file(df_simple, "Resultados_Calificaciones_Finales.csv", out_dir)

  debug("✅ Exportación de resultados completada exitosamente.")
}
