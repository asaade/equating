# ==============================================================================
# ORQUESTADOR MAESTRO DE REPORTES DE AUDITORÍA (AUDIT ORCHESTRATOR)
# ==============================================================================
# Responsabilidad: Coordinar la ejecución secuencial de los módulos A, B, C, D.
# Entradas: Objetos en memoria del pipeline de producción.
# Salidas: Generación de evidencias en carpeta AUDIT_PLOTS.
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(log4r, dplyr)

# ------------------------------------------------------------------------------
# 1. CARGA DINÁMICA DE MÓDULOS
# ------------------------------------------------------------------------------
# Intenta cargar los scripts de auditoría si no están ya en memoria.
load_audit_modules <- function(script_dir = "./R/plots_lib") {
  modules <- c(
    "07a_module_structural.R",
    "07b_module_process.R",
    "07c_module_precision.R",
    "07d_module_analysis.R",
    "07e_network_plots.R"
  )

  for (mod in modules) {
    fpath <- file.path(script_dir, mod)
    if (file.exists(fpath)) {
      if (exists("is_safe_r_path") && !is_safe_r_path(fpath)) {
        warn(sprintf("Intento de carga de módulo desde ruta no segura: %s", fpath), "VizLoader")
        next
      }
      tryCatch(
        {
          source(fpath, local = FALSE)
          debug(paste("Módulo cargado:", mod))
        },
        error = function(e) {
          msg <- paste("Error cargando módulo", mod, ":", e$message)
          warning(msg)
        }
      )
    } else {
      msg <- paste("Aviso: No se encontró el módulo", mod, "en", script_dir)
      warning(msg)
    }
  }
}

.prepare_audit_workspace <- function(config) {
  load_audit_modules()

  output_root <- tryCatch(config$project$output_dir, error = function(e) NULL)
  if (is.null(output_root)) output_root <- "./output"

  report_dir <- file.path(output_root, "REPORTES_GRAFICOS")

  if (!dir.exists(report_dir)) {
    dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
    debug(paste("Directorio de reportes creado:", report_dir))
  }

  return(report_dir)
}

# ------------------------------------------------------------------------------
# 2. FUNCIÓN PRINCIPAL DE EJECUCIÓN
# ------------------------------------------------------------------------------

run_audit_pipeline <- function(
  ctt_results,
  eq_results,
  dif_results = NULL,
  final_scores,
  config,
  data_obj = NULL
) {
  # --- A. PREPARACIÓN ---
  debug("=== INICIANDO PIPELINE DE AUDITORÍA PSICOMÉTRICA ===")

  # Verificar dependencias críticas
  if (!exists("execute_safely")) {
    warn("Funciones base (00_common) no detectadas. Intentando cargar...")
    load_audit_modules(script_dir = getwd()) # Asume directorio actual
  }

  base_dir <- .prepare_audit_workspace(config)

  # --- B. EJECUCIÓN SECUENCIAL DE MÓDULOS ---

  # 1. MÓDULO A: VALIDEZ ESTRUCTURAL (Items & Dim)
  # -----------------------------------------------------------
  if (exists("export_module_a_structural")) {
    execute_safely(
      expr = export_module_a_structural(ctt_results, calib_df, config, base_dir),
      desc = "Módulo A (Estructural)"
    )
  } else {
    error("Función 'export_module_a_structural' no encontrada.")
  }

  # 2. MÓDULO B: INTEGRIDAD DEL PROCESO (Drift & Smoothing)
  # -----------------------------------------------------------
  if (exists("export_module_b_process")) {
    execute_safely(
      expr = export_module_b_process(eq_results, config, base_dir),
      desc = "Módulo B (Proceso)"
    )
  }

  # 3. MÓDULO C: PRECISIÓN DE LA PUNTUACIÓN (Error & Cortes)
  # -----------------------------------------------------------
  if (exists("export_module_c_precision")) {
    execute_safely(
      expr = export_module_c_precision(ctt_results, eq_results, final_scores, config, base_dir),
      desc = "Módulo C (Precisión)"
    )
  }

  # 4. MÓDULO D: ANÁLISIS DE MODELOS DE EQUIPARACIÓN
  # -----------------------------------------------------------
  if (exists("export_module_d_analysis")) {
    execute_safely(
      expr = export_module_d_analysis(eq_results, config, base_dir),
      desc = "Módulo D (Modelos)"
    )
  }

  # 5. MÓDULO E: TOPOLOGÍA (Network - Legacy 07e)
  # -----------------------------------------------------------
  if (exists("export_topology_audit")) {
    execute_safely(
      expr = export_topology_audit(eq_results, config, base_dir),
      desc = "Módulo E (Topología)"
    )
  }


  debug("=== PIPELINE DE AUDITORÍA COMPLETADO ===")
}
