# ==============================================================================
# ORQUESTADOR PIPELINE PSICOMÉTRICO - PRODUCCIÓN
# Versión: v1.0
# Dependencias: R/00_common_base.R, R/00_config_loader.R
# ==============================================================================

## TODO: Valorar la conveniencia de:
## 1. Repetir el análisis para cada "subtema" o "subárea".
## 2. Coordinar mejor la selección de metodología y sus reportes (CTT o IRT)
## 3. Revisar las alertas y efectos de violaciones a las metodologías
## 4. Flexibilizar la incorporación/selección de variables demográficas
## 5. Revisar la calidad del reporte de auditoría como informe final único
## 6. Incorporar el análisis DIF al proceso (hoy se encuentra aislado).


rm(list = ls())
gc()

# --- 1. ARRANQUE DE INFRAESTRUCTURA ---

# Carga de la base operativa y el gestor de configuración
source("R/00_common_base.R", local = FALSE, encoding = "UTF-8")
source("R/00_config_loader.R", local = FALSE, encoding = "UTF-8")

info("INICIO DEL PIPELINE")

# --- 2. CONFIGURACIÓN Y AUDITORÍA ---

# Carga Unificada (YAML + Defaults Técnicos + Mapeo de Negocio)
config <- execute_safely(
  load_project_config(yaml_path = "config/config.yaml"),
  "Carga de Configuración"
)

if (is.null(config)) {
  fatal("No se pudo cargar la configuración. Abortando.")
  quit(status = 1)
}

# Inicializar Auditoría
audit <- init_audit_trail()

# Limpieza de registros de auditoría
execute_safely(clean_old_artifacts(config, days = 7), "Limpieza de Evidencias")

# Construcción de Mappers (Usando función exportada por 00_config_loader)
config$form_mapper <- execute_safely(
  build_form_mapper(config),
  "Construcción de Mappers"
)

if (is.null(config$form_mapper)) {
  fatal("Error construyendo mapa de formas. Verifique config.yaml.")
  quit(status = 1)
}

# Referencia al logger activo para módulos legacy que requieran el objeto explícito
logger <- setup_logger()
log_instance <- .sys_audit_env$active_logger

# --- 3. CARGA DE MÓDULOS DE NEGOCIO ---
modules <- c(
  "R/01_data_ingest.R",
  "R/02_scoring_engine.R",
  "R/03a_ctt_analysis.R",
  "R/03b_irt_model.R",
  "R/04_equating.R",
  "R/05_dif_analysis.R",
  "R/06_scoring_final.R",
  "R/07a_reporting_ctt_txt.R",
  "R/07b_reporting_ctt_audit.R",
  "R/07b_reporting_ctt_csv.R",
  "R/07c_reporting_ctt_viz.R",
  "R/07d_reporting_irt_txt.R",
  "R/08_export_results.R"
)

modules_ok <- sapply(modules, load_module_safe)

if (!all(modules_ok)) {
  fatal("Fallo en la carga de módulos críticos. Ver log para detalles.")
  quit(status = 1)
} else {
  info("Módulos cargados correctamente.")
}

track_stage(audit, "Configuración", input = NULL, output = config, output_dir_root = config$project$output_dir, save_rds = TRUE)

# --- 4. INICIA PROCESO ---
# ---------------------------------------------------------
# FASE 1: INGESTA
# ---------------------------------------------------------
data_obj <- execute_safely(
  orchestrate_ingestion(config),
  "FASE 1: Ingesta de Datos"
)

if (is.null(data_obj)) {
  fatal("Fallo crítico en Ingesta. Abortando.")
  quit(status = 1)
}

# Reporte de lectura de datos
qa_summary <- tryCatch(
  generate_qa_report(
    data_list   = data_obj,
    config      = config,
    output_path = file.path(config$project$output_dir, "lectura_datos.txt")
  ),
  error = function(e) warn(paste("QA Report Error:", sanitize_error_msg(e)))
)

track_stage(audit, "Ingesta", input = NULL, output = data_obj, output_dir_root = config$project$output_dir)


# ---------------------------------------------------------
# FASE 2: SCORING PRELIMINAR & MUESTREO
# ---------------------------------------------------------
scoring_res <- execute_safely(
  score_population(data_obj, config),
  "FASE 2a: Scoring Preliminar"
)

if (is.null(scoring_res)) {
  fatal("Fallo en Scoring Preliminar.")
  quit(status = 1)
}

scored_df <- if (is.data.frame(scoring_res)) scoring_res else scoring_res$scored
rm(scoring_res)

track_stage(audit, "Scoring_Preliminar", input = data_obj, output = scored_df, output_dir_root = config$project$output_dir)

calib_df <- execute_safely(
  create_calibration_sample(scored_df, config),
  "FASE 2b: Muestreo Análisis"
)

track_stage(audit, "Analysis_Sample", input = scored_df, output = calib_df, output_dir_root = config$project$output_dir, save_rds = TRUE)


# ---------------------------------------------------------
# FASE 3: ANÁLISIS CTT
# ---------------------------------------------------------
ctt_results <- execute_safely(
  run_ctt_analysis(calib_df, data_obj, config),
  "FASE 3: Análisis CTT"
)
track_stage(audit, "Analisis_CTT", input = calib_df, output = ctt_results, output_dir_root = config$project$output_dir)


# ---------------------------------------------------------
# FASE 4: EQUATING
# ---------------------------------------------------------
eq_results <- execute_safely(
  run_equating(
    df_scored = calib_df,
    meta = data_obj$meta,
    raw_dat = data_obj$raw_dat,
    config = config
  ),
  "FASE 4: Equating"
)
track_stage(audit, "Equating_Obj", input = calib_df, output = eq_results, output_dir_root = config$project$output_dir, save_rds = TRUE)

eq_tables_list <- if (!is.null(eq_results)) eq_results$tables else NULL
track_stage(audit, "Equating_Tables", input = calib_df, output = eq_tables_list, output_dir_root = config$project$output_dir, save_rds = FALSE)


# ---------------------------------------------------------
# FASE 3b: ANÁLISIS IRT & SCORING LATENTE
# ---------------------------------------------------------
target_df <- if (isTRUE(config$production_scoring$score_all_population)) scored_df else calib_df

irt_results <- execute_safely(
  {
    irt_calib_data <- prepare_irt_data(calib_df, config)
    model_res <- run_irt_calibration(irt_calib_data, config, data_obj$historical_params)
    irt_full_data <- prepare_irt_data(target_df, config)
    scores <- generate_irt_scores(model_res$model_obj, irt_full_data, target_df, config)

    list(model = model_res, scores = scores)
  },
  "FASE 3b: Modelado IRT (Calibración & Scoring)"
)

track_stage(audit, "IRT_Analysis", input = calib_df, output = irt_results$scores, config = config, output_dir_root = config$project$output_dir, save_rds = FALSE)


# ---------------------------------------------------------
# FASE 5: ANÁLISIS DE SESGO (DIF)
# ---------------------------------------------------------
dif_results <- execute_safely(
  {
    # Identificación inteligente de ítems
    item_names <- if (!is.null(ctt_results$stats)) {
      unique(ctt_results$stats$ITEM)
    } else {
      # Fallback: columnas numéricas ignorando metadatos conocidos
      cols <- names(calib_df)[sapply(calib_df, is.numeric)]
      setdiff(cols, c("ID", "RAW_SCORE", "SCORE"))
    }

    # Intersección real con columnas de calib_df
    item_names <- intersect(item_names, names(calib_df))

    if (length(item_names) > 0) {
      res <- run_dif_analysis(
        response_df = calib_df,
        demographic_df = data_obj$raw_dat,
        item_names = item_names,
        config = config
      )

      if (!is.null(res) && exists("export_dif_results")) {
        export_dif_results(res, config)
      }
      res
    } else {
      warn("No se detectaron ítems válidos para DIF.", "Main")
      NULL
    }
  },
  "FASE 5: Análisis DIF (Differential Item Functioning)"
)

track_stage(audit, "DIF_Analysis", input = calib_df, output = dif_results, output_dir_root = config$project$output_dir)

# ---------------------------------------------------------
# FASE 6: SCORING FINAL (Escalamiento Unificado)
# ---------------------------------------------------------
final_scores <- execute_safely(
  {
    # Decisión de población objetivo

    if (exists("scored_df")) rm(scored_df)
    res <- apply_final_scoring(
      item_scored_df = target_df,
      irt_scores_df = irt_results$scores,
      raw_data_obj = data_obj,
      conversion_table = eq_tables_list,
      config = config
    )
    res
  },
  "FASE 6: Asignación Puntajes Finales"
)

track_stage(audit, "Scoring_Final", input = target_df, output = final_scores, output_dir_root = config$project$output_dir)
if (exists("target_df")) rm(target_df)

# ---------------------------------------------------------
# FASE 7: GENERACIÓN DE REPORTES CTT (Evidencia Técnica)
# ---------------------------------------------------------

# Reportes de Texto
execute_safely(
  run_full_psychometric_audit(data_obj, ctt_results, eq_results, dif_results, final_scores, config),
  "FASE 7a: Reportes Textuales CTT"
)

# Reportes CSV
execute_safely(
  generate_csv_reports(
    config,
    ctt_results,
    eq_results,
    dif_results,
    final_scores,
    data_obj$meta
  ),
  "FASE 7b: Reportes Textuales CSV"
)

execute_safely(
  audit_equating_process(eq_results = eq_results, config),
  "FASE 7b2: Reportes Textuales Equiparación"
)

# Reportes Gráficos
execute_safely(
  run_audit_pipeline(
    ctt_results = ctt_results,
    eq_results = eq_results,
    dif_results = dif_results,
    final_scores = final_scores,
    config = config
  ),
  "FASE 7c: Reportes Gráficos"
)

execute_safely(
  {
    generate_irt_reports(config, irt_results, final_scores)
  },
  "FASE 7d: Guardando datos IRT"
)


# ---------------------------------------------------------
# FASE 8: EXPORTACIÓN DE RESULTADOS (Entrega Final)
# ---------------------------------------------------------
execute_safely(
  export_final_results(final_scores, config),
  "FASE 8: Exportación de Resultados"
)

# --- CIERRE ---
finalize_audit(audit, config$project$output_dir)
info("✅ PROCESO FINALIZADO EXITOSAMENTE")
