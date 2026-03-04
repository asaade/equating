# R/01_data_ingest.R
# Responsabilidad: Lectura de archivos DAT y creación del Mapa de Diseño
# Versión: v1.0

if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, stringr, checkmate, purrr)


# ==============================================================================
# A. METADATOS
# ==============================================================================
load_metadata <- function(path, config) {
  if (!file.exists(path)) {
    fatal(sprintf("FATAL: No existe metadata: %s", path))
    stop("Metadata faltante")
  }

  meta <- read_csv(
    path,
    show_col_types = FALSE,
    na = c("", "NA", " ", "null", "NULL"),
    trim_ws = TRUE
  )

  # --- Lógica de Mapeo y Renombrado ---
  final_names <- c(id = "ITEM_ID", key = "KEY")

  # Variantes posibles en el CSV (en mayúsculas para comparar)
  id_variants <- c("ITEM_ID", "ITEM", "ID", "IDENTIFICADOR", "PREGUNTA", "CODIGO")
  key_variants <- c("KEY", "CLAVE", "RESPUESTA", "ANS", "ANSWER", "CORRECTA")

  # Identificar cuáles columnas del CSV coinciden con nuestras variantes
  current_names <- names(meta)

  found_id_col <- current_names[toupper(current_names) %in% id_variants][1]
  found_key_col <- current_names[toupper(current_names) %in% key_variants][1]

  # Validar si logramos identificar ambas
  if (is.null(found_id_col) || is.null(found_key_col)) {
    fatal("Error: No se pudieron identificar las columnas ITEM_ID y KEY (o sus variantes).")
    stop("Estructura de metadata irreconocible.")
  }

  # Renombrar dinámicamente: 'NuevoNombre' = 'NombreEncontrado'
  # Esto garantiza que el objeto resultante SIEMPRE tenga ITEM_ID y KEY
  meta <- meta |>
    rename(
      !!final_names["id"] := all_of(found_id_col),
      !!final_names["key"] := all_of(found_key_col)
    )

  # --- Limpieza de Filas ---
  # Eliminar filas donde el ID sea nulo o esté vacío (incluyendo espacios)
  meta <- meta |>
    filter(!is.na(ITEM_ID) & trimws(as.character(ITEM_ID)) != "")

  # --- Exclusiones ---
  if (!is.null(config$exclusions) && isTRUE(config$exclusions$enabled)) {
    bad_items <- config$exclusions$items
    if (length(bad_items) > 0) {
      meta <- meta |> filter(!ITEM_ID %in% bad_items)
    }
  }

  # --- Validación de integridad de KEY ---
  invalid_keys <- meta |> filter(is.na(KEY) | trimws(as.character(KEY)) == "")
  if (nrow(invalid_keys) > 0) {
    warn(sprintf("⚠️ Se encontraron %d registros con KEY inválida.", nrow(invalid_keys)))
  }

  meta
}

# ==============================================================================
# A.2. MAPA DE DISEÑO (SSOT - Single Source of Truth)
# ==============================================================================
build_design_map <- function(meta, config) {
  debug("Construyendo Mapa de Diseño...")

  # Obtener prefijo de ítem definido en config (Default: "P_")
  pfx <- config$specs$data_structure$item_prefix
  if (is.null(pfx)) pfx <- "P_"

  map_list <- list()

  # Iterar sobre las formas definidas en config
  for (f_name in names(config$forms)) {
    f_conf <- config$forms[[f_name]]

    # Validar que la columna de posición exista en metadata
    pos_col <- f_conf$csv_col
    if (is.null(pos_col) || !pos_col %in% names(meta)) {
      warn(sprintf("Forma %s referencia columna '%s' no hallada en metadata. Saltando.", f_name, pos_col))
      next
    }

    # --- Filtrado Consolidado y Limpieza ---
    sub_meta <- meta |>
      filter(
        # 1. ITEM_ID: No NA, no vacío, no solo espacios
        !is.na(ITEM_ID) & trimws(as.character(ITEM_ID)) != "",
        # 2. POSITION: Debe ser convertible a número y no NA
        !is.na(suppressWarnings(as.numeric(as.character(.data[[pos_col]]))))
      ) |>
      select(ITEM_ID, KEY, POSITION = all_of(pos_col)) |>
      mutate(
        POSITION = as.numeric(as.character(POSITION)),
        FORMA_NAME = f_name,
        COL_NAME_RAW = paste0(pfx, POSITION)
      )

    # --- Validación de Duplicados ---
    # Comprobar si hay más de un ítem asignado a la misma posición en esta forma
    if (any(duplicated(sub_meta$POSITION))) {
      dup_pos <- sub_meta$POSITION[duplicated(sub_meta$POSITION)]
      warn(sprintf("Forma %s tiene posiciones duplicadas: %s", f_name, paste(unique(dup_pos), collapse = ", ")))
    }

    # Expandir para cada código de forma (Ej: "01", "1")
    if (nrow(sub_meta) > 0) {
      for (code in f_conf$codes) {
        tmp <- sub_meta
        tmp$FORMA_CODE <- code
        map_list[[paste(f_name, code, sep = "-")]] <- tmp
      }
    } else {
      warn(sprintf("La forma %s no contiene datos válidos tras el filtrado.", f_name))
    }
  }

  # --- Consolidación Final ---
  if (length(map_list) > 0) {
    design_map <- bind_rows(map_list)
    debug(sprintf("Design Map generado: %d definiciones de ítems activas.", nrow(design_map)))
  } else {
    design_map <- data.frame()
    warn("⚠️ El Design Map está vacío. Revisa la configuración y los datos de entrada.")
  }

  design_map
}

# ==============================================================================
# B. DAT (Lectura Óptica Optimizada)
# ==============================================================================
ingest_raw_dat <- function(path, config) {
  debug(sprintf("Iniciando ingesta DAT: %s", path))

  if (!file.exists(path)) {
    fatal("Archivo DAT no encontrado.")
    stop("Error de ruta DAT")
  }

  struct <- config$specs$data_structure

  # 1. Construcción de Layout
  header_widths <- sapply(struct$header, function(x) x$width)
  header_names <- sapply(struct$header, function(x) x$name)

  len_items <- config$specs$len_fisica
  item_width <- if (!is.null(struct$item_width)) struct$item_width else 1
  item_prefix <- if (!is.null(struct$item_prefix)) struct$item_prefix else "P_"

  item_widths <- rep(item_width, len_items)
  item_names <- paste0(item_prefix, 1:len_items)

  all_widths <- c(header_widths, item_widths)
  all_names <- c(header_names, item_names)

  # 2. Lectura con tipos forzados
  raw_df <- tryCatch(
    {
      read_fwf(
        path,
        col_positions = fwf_widths(all_widths, col_names = all_names),
        col_types = cols(.default = col_character()),
        trim_ws = FALSE,
        progress = FALSE,
        show_col_types = FALSE
      )
    },
    error = function(e) {
      fatal(paste("Error crítico en lectura FWF:", e$message))
      stop(e)
    }
  )

  # 3. Limpieza de columnas SKIP (tipo "_")
  cols_to_keep <- header_names[sapply(struct$header, function(x) !identical(x$type, "_"))]
  raw_df <- raw_df |> select(all_of(cols_to_keep), starts_with(item_prefix))

  # 4. Filtrado de registros íntegros
  # Un registro es válido solo si tiene ID y FORMA (no vacíos)
  before_filter <- nrow(raw_df)
  raw_df <- raw_df |>
    filter(!is.na(ID) & trimws(as.character(ID)) != "") |>
    filter(!is.na(FORMA) & trimws(as.character(FORMA)) != "")

  if (nrow(raw_df) < before_filter) {
    warn(sprintf("Se descartaron %d registros por ID o FORMA faltante.", before_filter - nrow(raw_df)))
  }

  # 5. Deduplicación por ID
  if (any(duplicated(raw_df$ID))) {
    n_dups <- sum(duplicated(raw_df$ID))
    warn(sprintf("Detección de %d IDs duplicados. Manteniendo primera aparición.", n_dups))
    raw_df <- raw_df |> distinct(ID, .keep_all = TRUE)
  }

  debug(sprintf("Ingesta completada: %d registros procesados.", nrow(raw_df)))
  raw_df
}

# ==============================================================================
# C. UTILS (Demográficos + Históricos)
# ==============================================================================
load_demographics <- function(path, config) {
  if (is.null(path) || !file.exists(path)) {
    return(NULL)
  }
  debug("Cargando atributos demográficos...")

  df <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = col_character()))

  # Normalizar nombres para asegurar que exista "ID"
  names(df) <- toupper(names(df))

  if (!"ID" %in% names(df)) {
    warn("El archivo demográfico no tiene columna ID. Se ignorará.")
    return(NULL)
  }

  distinct(df, ID, .keep_all = TRUE)
}

load_historical_params <- function(config) {
  historical_params <- NULL

  cfg <- config$historical_anchoring
  if (is.null(cfg) || !isTRUE(cfg$enabled)) {
    return(NULL)
  }

  if (file.exists(cfg$irt_parameters_file)) {
    irt_df <- read_csv(path, show_col_types = FALSE)
    names(irt_df) <- toupper(names(irt_df))
    if ("ITEM_ID" %in% names(irt_df)) irt_df <- rename(irt_df, ITEM = ITEM_ID)
    historical_params <- irt_df
  } else {
    warn(paste("Missing parameters file: ", cfg$irt_parameters_file))
    return(NULL)
  }

  historical_params
}


generate_qa_report <- function(data_list, config, output_path = "reporte_qa.txt") {
  debug("Generando Reporte de Calidad (QA)...")

  # Extraer objetos del orquestador
  raw_dat <- data_list$raw_dat
  meta <- data_list$meta
  design_map <- data_list$design_map

  # 1. Métricas de Metadata e Ítems
  total_items_meta <- nrow(meta)
  total_items_active <- length(unique(design_map$ITEM_ID))

  # 2. Métricas de Participantes
  total_raw <- nrow(raw_dat)
  conteo_formas <- raw_dat |>
    group_by(FORMA) |>
    summarise(n = n(), .groups = "drop")

  # 3. Detección de posibles problemas de integridad
  # (Simulamos o extraemos de los logs si fuera necesario,
  # pero aquí lo recalculamos para el reporte)
  ids_duplicados <- total_raw - length(unique(raw_dat$ID))

  # 4. Construcción del Cuerpo del Reporte
  report <- c(
    "====================================================",
    "        REPORTE DE CALIDAD DE INGESTA (QA)          ",
    sprintf("        Fecha: %s", Sys.time()),
    "====================================================",
    "",
    "1. RESUMEN DE METADATOS (ITEMS)",
    "----------------------------------------------------",
    sprintf("Items detectados en metadata:     %d", total_items_meta),
    sprintf("Items activos en el diseño:       %d", total_items_active),
    sprintf("Diferencia (Items sin posición):  %d", total_items_meta - total_items_active),
    "",
    "2. RESUMEN DE PARTICIPANTES (DAT)",
    "----------------------------------------------------",
    sprintf("Registros totales procesados:     %d", total_raw),
    sprintf("IDs duplicados eliminados:        %d", ids_duplicados),
    "",
    "Conteo por Forma:",
    capture.output(print(as.data.frame(conteo_formas))),
    "",
    "3. ALERTAS E INTEGRIDAD",
    "----------------------------------------------------"
  )

  # Alertas condicionales
  alerts <- c(
    if (total_items_active == 0) "❌ ERROR CRÍTICO: No hay ítems activos en el diseño.",
    if (any(is.na(raw_dat$FORMA))) "⚠️ ADVERTENCIA: Existen registros con FORMA no identificada.",
    if (nrow(raw_dat) == 0) "❌ ERROR CRÍTICO: El archivo de datos está vacío."
  )

  report <- c(report, alerts, "", "--- --- --- --- ---")

  # 5. Escritura a archivo y log
  tryCatch(
    {
      writeLines(report, output_path)
      debug(sprintf("Reporte QA guardado exitosamente en: %s", output_path))
    },
    error = function(e) {
      warn("No se pudo escribir el archivo de reporte QA.")
    }
  )

  report
}


# ==============================================================================
# D. ORQUESTADOR
# ==============================================================================
orchestrate_ingestion <- function(config) {
  # debug(">>> FASE 1: INGESTA DE DATA <<<")

  # 1. Carga de archivos
  meta <- load_metadata(config$files$metadata, config)
  raw_dat <- ingest_raw_dat(config$files$dat, config)
  attrs <- load_demographics(config$files$attributes, config)

  # 2. Generar el Design Map
  design_map <- build_design_map(meta, config)

  # 3. Unión de Demográficos (con validación de tipo)
  if (!is.null(attrs) && !is.null(raw_dat)) {
    ## debug("Uniendo demográficos...")
    # Asegurar que la llave de unión sea del mismo tipo
    raw_dat$ID <- as.character(raw_dat$ID)
    attrs$ID <- as.character(attrs$ID)

    raw_dat <- raw_dat |> left_join(attrs, by = "ID")
  }

  list(
    raw_dat = raw_dat,
    meta = meta,
    design_map = design_map,
    historical_params = load_historical_params(config)
  )
}
