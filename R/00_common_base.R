# R/00_common_base.R
# Responsabilidad: Infraestructura Base (Logging Sofisticado, I/O Seguro, Manejo de Errores).
# Versión: v1.0

# ==============================================================================
# 1. DEPENDENCIAS DE SISTEMA
# ==============================================================================
if (!require("pacman")) install.packages("pacman", repos = "https://cloud.r-project.org")

pacman::p_load(
  log4r, # Logging profesional
  checkmate, # Validaciones defensivas
  readr, # I/O optimizado
  dplyr, # Manipulación de datos
  stringr, # Manejo de cadenas
  parallel, # Computación paralela
  digest, # Hashing
  jsonlite, # Serialización
  utils,
  tools,
  ggplot2,
  Cairo,
  testthat
)

# Configuración Regional
Sys.setenv(TZ = "America/Mexico_City")

options(
  stringsAsFactors = FALSE,
  scipen = 9999,
  digits = 4,
  width = 150,
  encoding = "UTF-8",
  warn = 1,
  mc.cores = 6,
  keep.source = TRUE,
  show.error.locations = TRUE, # En versiones recientes de R
  error = NULL
)

# ==============================================================================
# 2. ENTORNO DE AUDITORÍA (Singleton)
# ==============================================================================
if (!exists(".sys_audit_env")) {
  .sys_audit_env <- new.env(parent = emptyenv())
  .sys_audit_env$write_errors <- list()
  .sys_audit_env$active_logger <- NULL
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ==============================================================================
# 3. SISTEMA DE LOGGING CON INTROSPECCIÓN
# ==============================================================================

setup_logger <- function(log_dir = "logs", log_filename = "pipeline.log", level = "INFO") {
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

  log_path <- file.path(log_dir, log_filename)
  layout <- log4r::default_log_layout()

  console_app <- log4r::console_appender(layout = layout)
  file_app <- log4r::file_appender(log_path, append = TRUE, layout = layout)

  logger <- log4r::logger(threshold = level, appenders = list(console_app, file_app))
  .sys_audit_env$active_logger <- logger

  # Mensaje de inicialización directo a consola para confirmar arranque
  cat(sprintf("[SYSTEM] Logger inicializado en: %s\n", log_path))

  return(invisible(logger))
}

#' Recupera el contexto de ejecución real (Función, Archivo, Línea)
#' Ignora funciones wrapper e infraestructura del sistema.
get_context <- function() {
  stack <- sys.calls()
  n <- length(stack)

  # Lista negra expandida
  ignore_list <- c(
    "log_msg", "info", "warn", "error", "fatal", "get_context",
    "tryCatch", "tryCatchList", "tryCatchOne", "doTryCatch",
    "withCallingHandlers", "eval", "source", "execute_safely",
    "standardGeneric", "do.call", "tail", "rev", "seq_len"
  )

  for (i in rev(seq_len(n))) {
    call_obj <- stack[[i]]
    fn_name_raw <- tryCatch(as.character(call_obj[[1]])[1], error = function(e) "")
    fn_name <- gsub("^.*::", "", fn_name_raw)

    if (fn_name != "" && !fn_name %in% ignore_list && !grepl("function|\\{", fn_name)) {
      srcref <- attr(call_obj, "srcref")
      if (!is.null(srcref)) {
        srcfile <- attr(srcref, "srcfile")$filename
        line_num <- as.vector(srcref)[1]
        return(sprintf("%s [%s:%d]", fn_name, basename(srcfile), line_num))
      }
      return(fn_name)
    }
  }
  return("Global")
}

log_msg <- function(level, msg, ctx = NULL) {
  logger <- .sys_audit_env$active_logger

  # Si no se pasó contexto explícito, lo calculamos
  final_ctx <- if (is.null(ctx)) get_context() else ctx

  # Iconos para escaneo visual rápido en logs largos
  icon <- switch(level,
    "info"  = "ℹ️",
    "warn"  = "⚠️",
    "error" = "❌",
    "fatal" = "💀",
    ""
  )

  formatted_msg <- sprintf("%s [%s] %s", icon, final_ctx, msg)

  # Fallback si el logger no está listo
  if (is.null(logger)) {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    cat(sprintf("[%s] %s %s\n", timestamp, toupper(level), formatted_msg))
  } else {
    log_fun <- switch(level,
      "debug"  = log4r::debug,
      "info"  = log4r::info,
      "warn"  = log4r::warn,
      "error" = log4r::error,
      "fatal" = log4r::error,
      log4r::info
    )
    log_fun(logger, formatted_msg)
  }

  if (level == "fatal") stop("FATAL ERROR: A system error occurred. Please check the logs for details.", call. = FALSE)
}

# Wrappers públicos
debug <- function(msg, ctx = NULL) log_msg("debug", msg, ctx)
info <- function(msg, ctx = NULL) log_msg("info", msg, ctx)
warn <- function(msg, ctx = NULL) log_msg("warn", msg, ctx)
error <- function(msg, ctx = NULL) log_msg("error", msg, ctx)
fatal <- function(msg, ctx = NULL) log_msg("fatal", msg, ctx)

# ==============================================================================
# 4. EJECUCIÓN SEGURA (Try-Catch Wrappers)
# ==============================================================================

execute_safely <- function(expr, desc) {
  # Contexto de inicio (quién llama a execute_safely)
  entry_context <- get_context()
  info(sprintf(">>> Iniciando: %s", desc), ctx = entry_context)

  result <- tryCatch(
    {
      withCallingHandlers(
        {
          expr
        },
        warning = function(w) {
          # Captura el contexto real donde se disparó el warning
          actual_ctx <- get_context()
          warn(paste0("En '", desc, "': ", conditionMessage(w)), ctx = actual_ctx)
          invokeRestart("muffleWarning")
        }
      )
    },
    error = function(e) {
      # Captura el contexto real del error
      actual_ctx <- get_context()

      # Opcional: Capturar el traceback simplificado
      tb <- sys.calls()
      error_call <- tail(tb, 2)[[1]]

      error(
        sprintf(
          "FALLO CRÍTICO en '%s': %s (Call: %s)",
          desc, conditionMessage(e), deparse(error_call)
        ),
        ctx = actual_ctx
      )
      return(NULL)
    }
  )
  return(invisible(result))
}

# ==============================================================================
# 5. I/O SEGURO
# ==============================================================================

save_safe <- function(obj, path) {
  if (is.null(obj)) {
    warn(paste("Intento de guardar objeto NULL en:", basename(path)), "IO")
    return(FALSE)
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  tryCatch(
    {
      if (is.data.frame(obj)) {
        readr::write_csv(obj, path, na = "")
      } else {
        warn(paste("Objeto no es data.frame. Tipo:", class(obj)[1]), "IO")
        return(FALSE)
      }
      info(paste("Archivo guardado:", basename(path)), "IO")
      return(TRUE)
    },
    error = function(e) {
      error(paste("Error escritura:", basename(path), e$message), "IO")
      .sys_audit_env$write_errors[[basename(path)]] <- e$message
      return(FALSE)
    }
  )
}

save_plot_safe <- function(plot_obj, filename, width, height) {
  if (is.null(plot_obj)) {
    return(FALSE)
  }
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)

  tryCatch(
    {
      ggplot2::ggsave(filename, plot_obj, width = width, height = height, dpi = 300, bg = "white", device = cairo_pdf)
      info(paste("Gráfico guardado:", basename(filename)), "Plot")
      return(TRUE)
    },
    error = function(e) {
      error(paste("Error gráfico:", basename(filename), e$message), "Plot")
      return(FALSE)
    }
  )
}

# ==============================================================================
# 6. TRAZABILIDAD (Audit Trail)
# ==============================================================================

init_audit_trail <- function() {
  e <- new.env(parent = emptyenv())
  e$id <- format(Sys.time(), "%Y%m%d_%H%M%S")
  e$metrics <- data.frame(
    Stage = character(), Input = character(), Output = character(),
    Artifact = character(), Timestamp = character(), stringsAsFactors = FALSE
  )
  return(e)
}

track_stage <- function(audit, stage, input, output, output_dir_root, config = NULL, save_rds = FALSE) {
  if (!is.environment(audit)) {
    return()
  }

  get_dims <- function(x) if (is.data.frame(x)) paste(dim(x), collapse = "x") else class(x)[1]
  desc_in <- if (is.null(input)) "NULL" else get_dims(input)
  desc_out <- if (is.null(output)) "NULL" else get_dims(output)

  info(sprintf("Stage '%s': %s -> %s", stage, desc_in, desc_out), "Audit")

  artifact_path <- NA

  # Guardado de artefactos para depuración
  if (!is.null(output) && (save_rds || is.data.frame(output))) {
    # Usamos output_dir_root, o intentamos sacarlo de config si no se pasó root
    root <- output_dir_root
    if (missing(root) && !is.null(config)) root <- config$project$output_dir
    if (is.null(root)) root <- "output"

    art_dir <- file.path(root, "ARTIFACTS", audit$id)
    dir.create(art_dir, recursive = TRUE, showWarnings = FALSE)

    # Snapshot de estructura (Debug ligero)
    try(
      {
        capture_file <- file.path(art_dir, paste0(stage, "_str.txt"))
        sink(capture_file)
        str(output, list.len = 10)
        sink()
      },
      silent = TRUE
    )

    if (save_rds) {
      rds_file <- file.path(art_dir, paste0(stage, ".rds"))
      saveRDS(output, rds_file)
      artifact_path <- basename(rds_file)
    }
  }

  new_row <- data.frame(
    Stage = stage, Input = desc_in, Output = desc_out, Artifact = artifact_path,
    Timestamp = format(Sys.time(), "%H:%M:%S"), stringsAsFactors = FALSE
  )
  audit$metrics <- dplyr::bind_rows(audit$metrics, new_row)
}

finalize_audit <- function(audit, output_dir_root) {
  if (!is.environment(audit)) {
    return()
  }
  art_dir <- file.path(output_dir_root, "ARTIFACTS", audit$id)

  if (nrow(audit$metrics) > 0) {
    dir.create(art_dir, recursive = TRUE, showWarnings = FALSE)
    write.csv(audit$metrics, file.path(art_dir, "execution_log.csv"), row.names = FALSE)
    info(paste("Auditoría final guardada en:", art_dir), "Audit")
  }
}
