# R/00_config_loader.R
# Responsabilidad: Orquestación de configuración, validación y mapeo de reglas de negocio.
# Dependencias: R/00_common_base.R (para logging), yaml
# Versión: v1.0

# Cargar infraestructura base si no está cargada
if (!exists("setup_logger")) source("R/00_common_base.R")

# Cargar Definiciones Internas (El Factory de Defaults Técnicos)
if (file.exists("R/00_config_defs.R")) {
  source("R/00_config_defs.R")
} else {
  stop("FATAL: No se encuentra 'R/00_config_defs.R'. Integridad del sistema comprometida.")
}

# ==============================================================================
# GESTOR DE CONFIGURACIÓN
# ==============================================================================

#' Carga, unifica y valida la configuración del pipeline
#' @param yaml_path Ruta al archivo de configuración del usuario
#' @param defs_path Ruta al archivo de definiciones por defecto
#' @return Lista final de configuración hidratada y validada
load_project_config <- function(yaml_path = "config.yaml", defs_path = "R/00_config_defs.R") {
  # 1. CARGA E INTEGRIDAD INICIAL
  if (!file.exists(defs_path)) stop("FATAL: Archivo de definiciones base no encontrado.")
  source(defs_path)
  defaults <- get_default_config()

  if (!file.exists(yaml_path)) stop(paste("FATAL: Archivo de configuración no encontrado:", yaml_path))
  user_config <- yaml::read_yaml(yaml_path, eval.expr = FALSE)

  # 2. FUSIÓN RECURSIVA (Deep Merge)
  # El usuario solo sobreescribe lo necesario; el resto se protege con defaults.
  full_config <- utils::modifyList(defaults, user_config)

  # 3. MOTOR DE VALIDACIÓN (Aserciones Lógicas)
  errors <- c()

  # Helper para acumular errores sin detener el flujo inmediatamente
  assert_logic <- function(condition, msg) {
    if (!condition) errors <<- c(errors, msg)
  }

  # A. Validación de Archivos (Inputs)
  for (f in c("dat", "metadata")) {
    path <- full_config$files[[f]]
    assert_logic(!is.null(path) && file.exists(path), sprintf("Archivo crítico '%s' no encontrado en: %s", f, path))
  }

  # B. Consistencia del Formato Fijo (DAT)
  header_info <- full_config$specs$data_structure$header
  if (!is.null(header_info)) {
    total_header_width <- sum(sapply(header_info, function(x) x$width))
    physical_len <- full_config$specs$len_fisica
    item_width <- full_config$specs$data_structure$item_width %||% 1

    remaining_space <- physical_len - total_header_width
    assert_logic(remaining_space > 0, "El ancho del header excede la longitud física (len_fisica).")
    assert_logic(
      remaining_space %% item_width == 0,
      "El espacio para ítems no es múltiplo exacto de 'item_width'."
    )
  }

  # C. Validación de Formas y Diseño de Enlace
  forms <- full_config$forms
  assert_logic(length(forms) > 0, "No se han definido formas en la sección 'forms'.")

  ref_form <- full_config$system$reference_form
  assert_logic(ref_form %in% names(forms), sprintf("La forma de referencia '%s' no existe en 'forms'.", ref_form))

  # Verificar duplicidad de códigos de forma
  all_codes <- unlist(lapply(forms, function(x) x$codes))
  if (any(duplicated(all_codes))) {
    assert_logic(FALSE, paste("Códigos de forma duplicados detectados:", paste(all_codes[duplicated(all_codes)], collapse = ", ")))
  }

  # D. Validación de Escalamiento (Scoring)
  levels <- full_config$scoring$performance_levels
  if (!is.null(levels) && is.list(levels)) {
    # Extraer solo los que tienen min_score (excluyendo el max_scaled_score del final)
    scores <- sapply(levels, function(x) x$min_score)
    scores <- scores[!sapply(scores, is.null)]
    if (length(scores) > 1) {
      assert_logic(
        all(diff(as.numeric(scores)) > 0),
        "Los puntos de corte (min_score) en 'performance_levels' deben ser estrictamente ascendentes."
      )
    }
  }

  # 4. PUENTE DE HIDRATACIÓN (Mapping Bridge)
  # Mapeamos variables del YAML (Negocio) a variables del Motor (Técnico)
  if (!is.null(full_config$thresholds)) {
    th <- full_config$thresholds
    # Inyectamos valores de CTT en la lógica de Anclas si el usuario los definió
    if (!is.null(th$ctt_p_val_min)) full_config$anchor$min_p <- th$ctt_p_val_min
    if (!is.null(th$ctt_p_val_max)) full_config$anchor$max_p <- th$ctt_p_val_max
  }

  # 5. REPORTE FINAL
  if (length(errors) > 0) {
    fatal(paste("[X] ERROR: Fallo en la validación de configuración:\n", "  -", errors, collapse = "\n"))
  }

  # Normalización de rutas para evitar problemas de WD
  if (!is.null(full_config$files$dat)) full_config$files$dat <- normalizePath(full_config$files$dat, mustWork = FALSE)

  message(">>> Configuración validada y cargada: ", full_config$project$name)
  return(full_config)
}


# ==============================================================================
# UTILERÍAS DEPENDIENTES DE CONFIGURACIÓN
# ==============================================================================

setup_parallel <- function(config) {
  use_par <- config$system$use_parallel %||% FALSE
  if (isTRUE(use_par)) {
    req_cores <- config$system$n_cores %||% 1
    avail_cores <- parallel::detectCores(logical = FALSE) - 1
    final_cores <- min(max(1, req_cores), max(1, avail_cores))
    options(mc.cores = final_cores)
    debug(sprintf("Paralelismo activado: %d cores (Solicitados: %s)", final_cores, req_cores), "System")
  } else {
    options(mc.cores = 1)
    debug("Ejecución secuencial (Paralelismo desactivado por config).", "System")
  }
}

get_form_code_map <- function(config) {
  if (is.null(config$forms)) {
    return(NULL)
  }
  map_vec <- c()
  for (form_name in names(config$forms)) {
    codes_raw <- config$forms[[form_name]]$codes
    codes <- if (is.list(codes_raw)) unlist(codes_raw) else codes_raw
    if (!is.null(codes) && length(codes) > 0) {
      codes_clean <- trimws(as.character(codes))
      temp_map <- structure(rep(form_name, length(codes_clean)), names = codes_clean)
      map_vec <- c(map_vec, temp_map)
    }
  }
  map_vec
}

build_form_mapper <- function(config) {
  if (is.null(config$forms)) {
    error("Sección 'forms' faltante en configuración. No se puede construir mapa.", "Mapper")
    return(NULL)
  }

  code_to_name <- c()
  for (fname in names(config$forms)) {
    codes <- config$forms[[fname]]$codes
    for (c in codes) {
      c_str <- as.character(c)
      if (c_str %in% names(code_to_name)) {
        warn(paste("Código de forma duplicado detectado:", c_str), "Mapper")
      }
      code_to_name[c_str] <- fname
    }
  }
  return(list(code_to_name = code_to_name))
}


# Helper seguro para carga de scripts
load_module_safe <- function(path) {
  if (!file.exists(path)) {
    fatal(sprintf("Módulo perdido: %s", path), "Loader")
    return(FALSE)
  }
  tryCatch(
    {
      source(path, local = FALSE, encoding = "UTF-8")
      return(TRUE)
    },
    error = function(e) {
      fatal(sprintf("Sintaxis R rota en '%s': %s", path, e$message), "Loader")
      FALSE
    }
  )
}


# Utilería de Limpieza (Definida localmente para este contexto de ejecución)
clean_old_artifacts <- function(config, days = 7) {
  out_dir <- config$project$output_dir %||% "output"
  root <- file.path(out_dir, "ARTIFACTS")
  if (!dir.exists(root)) {
    return()
  }

  dirs <- list.dirs(root, recursive = FALSE)
  now <- Sys.time()
  count <- 0

  for (d in dirs) {
    mtime <- file.info(d)$mtime
    if (!is.na(mtime) && difftime(now, mtime, units = "days") > days) {
      unlink(d, recursive = TRUE)
      count <- count + 1
    }
  }
  if (count > 0) debug(sprintf("Limpieza: %d carpetas de artefactos eliminadas.", count), "Maint")
}
