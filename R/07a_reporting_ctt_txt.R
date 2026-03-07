# ==============================================================================
# ORQUESTADOR DE AUDITORÍA PSICOMÉTRICA (CTT)
# Versión: v1.0
# ==============================================================================

if (!exists("execute_safely", mode = "function")) {
  common_path <- "R/00_common_base.R"
  if (file.exists(common_path)) {
    source(common_path)
  }
}

# ------------------------------------------------------------------------------
# 1. GESTIÓN DE DEPENDENCIAS
# ------------------------------------------------------------------------------

#' Carga las librerías de auditoría necesarias
#' @keywords internal
.load_audit_libraries <- function() {
  libs <- c(
    "A_reporting_item_audit.R",
    "B_reporting_confiabilidad_audit.R",
    "C_reporting_eq_audit.R",
    "D_reporting_score_audit.R"
  )

  search_paths <- c(
    file.path("R", "reporting_lib"),
    file.path("reporting_lib"),
    file.path("R"),
    "."
  )

  # Carga unificada de módulos
  for (lib in libs) {
    # Saltamos archivos Rmd, solo necesitamos verificar que existan o tenerlos en mente
    if (grepl("\\.Rmd$", lib)) next

    found <- FALSE
    for (path in search_paths) {
      full_path <- file.path(path, lib)
      if (file.exists(full_path)) {
        if (exists("is_safe_r_path") && !is_safe_r_path(full_path)) {
          warn(sprintf("Intento de carga de librería desde ruta no segura: %s", full_path), "AuditLoader")
          next
        }
        tryCatch(
          {
            source(full_path, local = FALSE) # local=FALSE asegura que funciones vayan al GlobalEnv
            found <- TRUE
          },
          error = function(e) {
            warn(paste("Error cargando", lib, ":", e$message))
          }
        )
        break
      }
    }

    # Loguear advertencia si falta una librería (excepto templates)
    if (!found) {
      warn(paste("Librería no encontrada en rutas de búsqueda:", lib))
    }
  }
}

# Carga inicial
.load_audit_libraries()

# ------------------------------------------------------------------------------
# 2. FUNCIONES HELPER (ESPECÍFICAS DE ESTE MÓDULO)
# ------------------------------------------------------------------------------

#' Validación de Inputs Críticos
#' @return TRUE si los datos son válidos, FALSE si hay errores bloqueantes
.validate_audit_inputs <- function(data_obj, ctt_results) {
  errors <- c()

  if (is.null(data_obj$meta)) errors <- c(errors, "Falta metadatos (data_obj$meta)")
  if (is.null(ctt_results$stats)) errors <- c(errors, "Faltan estadísticas CTT (ctt_results$stats)")

  if (length(errors) > 0) {
    error(paste("Validación fallida:", paste(errors, collapse = "; ")))
    return(FALSE)
  }
  return(TRUE)
}

#' Preparación del Espacio de Trabajo
.prepare_audit_workspace <- function(config) {
  output_root <- tryCatch(config$project$output_dir, error = function(e) NULL)
  if (is.null(output_root)) output_root <- "./output"

  report_dir <- file.path(output_root, "REPORTES_AUDITORIA")

  if (!dir.exists(report_dir)) {
    dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
    debug(paste("Directorio de reportes creado:", report_dir))
  }

  return(report_dir)
}

# ------------------------------------------------------------------------------
# 3. FUNCIÓN MAESTRA (ENTRY POINT)
# ------------------------------------------------------------------------------

#' Ejecuta la auditoría psicométrica completa (Texto + Visual)
run_full_psychometric_audit <- function(data_obj,
                                        ctt_results,
                                        eq_results = NULL,
                                        dif_results = NULL,
                                        final_scores = NULL,
                                        config) {
  debug(">>> INICIANDO PROTOCOLO DE AUDITORÍA FORENSE <<<")

  # Fase 1: Validación y Preparación
  if (!.validate_audit_inputs(data_obj, ctt_results)) {
    return(invisible(list(status = "ABORTED_INVALID_INPUT")))
  }

  report_dir <- .prepare_audit_workspace(config)
  exec_states <- list()

  # Función auxiliar para mapear el retorno de execute_safely al estado
  run_mod <- function(expr_block, desc) {
    res <- execute_safely(expr_block, desc)
    if (is.null(res)) "FALLIDO" else res
  }

  # Fase 2: Pipeline de Ejecución (Reportes de Texto / Auditoría)

  # --- Módulo A: Calidad de Ítems & DIF ---
  exec_states$item_audit <- run_mod(
    {
      dif_audit_data <- if (!is.null(dif_results)) dif_results else data.frame(ITEM = character(0))
      audit_item_quality(
        ctt_results = ctt_results,
        dif_results = dif_audit_data,
        meta        = data_obj$meta,
        base_dir    = report_dir,
        config      = config
      )
      "COMPLETADO"
    },
    "Módulo A (Análisis de Ítems)"
  )

  # --- Módulo B: Estructura Interna ---
  exec_states$structure <- run_mod(
    {
      audit_internal_structure(
        ctt_results = ctt_results,
        base_dir    = report_dir,
        config      = config
      )
      "COMPLETADO"
    },
    "Módulo B (Estructura Interna)"
  )

  # --- Módulo C: Equiparación ---
  if (!is.null(eq_results) && !is.null(eq_results$coefficients)) {
    exec_states$equating <- run_mod(
      {
        audit_equating_process(
          eq_results = eq_results,
          base_dir   = report_dir,
          config     = config
        )
        "COMPLETADO"
      },
      "Módulo C (Equiparación)"
    )
  } else {
    debug("Se omite Módulo C: Sin coeficientes de equiparación.")
    exec_states$equating <- "SKIPPED"
  }

  # --- Módulos D & E: Scoring y Subtests ---
  if (!is.null(final_scores) && nrow(final_scores) > 0) {
    exec_states$impact <- run_mod(
      {
        audit_score_impact(
          final_scores = final_scores,
          raw_dat      = data_obj$raw_dat,
          eq_results   = eq_results,
          base_dir     = report_dir,
          config       = config
        )
        "COMPLETADO"
      },
      "Módulo D (Impacto de Puntuaciones)"
    )
  } else {
    debug("Se omiten Módulos D/E: Puntuaciones no disponibles.")
    exec_states$impact <- "SKIPPED"
    exec_states$subtests <- "SKIPPED"
  }

  # Cierre
  debug(">>> PROTOCOLO DE AUDITORÍA COMPLETADO <<<")
  ## invisible(exec_states)
}
