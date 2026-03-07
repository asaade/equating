# R/04_equating.R
# Responsabilidad: Fachada de Orquestación (Global Only & Sequential)
# Versión: v112.1 - Alignment with Vectorized Engine & Psychometric Passport
# Dependencias: 00_common_base.R, 00_config_loader.R, 03_network.R

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, checkmate, moments, tibble)

# Definición de librerías del núcleo
core_libs <- c(
  "00_data_cleaning.R", "01_anchor_mgmt.R", "08_quality_control.R",
  "07_model_selector.R", "02_stat_engine.R", "03_network.R", "04_validation.R",
  "09_ux_helpers.R"
)

# Carga de dependencias
for (lib in core_libs) {
  path <- file.path("R", "equating_lib", lib)
  if (file.exists(path)) {
    if (is_safe_r_path(path)) {
      source(path, local = FALSE)
    } else {
      warn(sprintf("Omitiendo carga de librería no segura: %s", path), "Equating")
    }
  } else if (file.exists(lib)) {
    if (is_safe_r_path(lib)) {
      source(lib, local = FALSE)
    } else {
      warn(sprintf("Omitiendo carga de librería no segura: %s", lib), "Equating")
    }
  }
}

# =============================================================================
# 1. AUXILIARES DE REPORTE Y AUDITORÍA
# =============================================================================

safe_bind_all <- function(lst) {
  valid_lst <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, lst)
  if (length(valid_lst) == 0) {
    return(NULL)
  }

  all_cols <- unique(unlist(lapply(valid_lst, names)))
  normalized_lst <- lapply(valid_lst, function(df) {
    missing <- setdiff(all_cols, names(df))
    if (length(missing) > 0) df[missing] <- NA
    df
  })

  tryCatch(do.call(rbind, normalized_lst), error = function(e) {
    warn(paste("Error en consolidación:", e$message), "EquatingFacade")
    return(NULL)
  })
}

generate_descriptive_report <- function(df, forms_map, items, scale_id) {
  valid_items <- intersect(items, names(df))
  if (length(valid_items) < 3) {
    return(NULL)
  }

  stats_list <- list()
  for (lbl in names(forms_map)) {
    def <- forms_map[[lbl]]
    if (!is.list(def)) next

    codes <- as.character(def$codes)
    df_form <- df[df$FORMA %in% codes, , drop = FALSE]

    if (nrow(df_form) == 0) next

    scores <- rowSums(df_form[, valid_items, drop = FALSE], na.rm = TRUE)
    k <- length(valid_items)
    var_tot <- var(scores, na.rm = TRUE)
    sum_var_item <- sum(apply(df_form[, valid_items, drop = FALSE], 2, var, na.rm = TRUE), na.rm = TRUE)
    alpha <- if (var_tot > 0) (k / (k - 1)) * (1 - sum_var_item / var_tot) else NA

    stats_list[[length(stats_list) + 1]] <- data.frame(
      SCALE = scale_id, FORMA_LABEL = lbl, N = nrow(df_form),
      Mean = round(mean(scores, na.rm = TRUE), 4),
      SD = round(sd(scores, na.rm = TRUE), 4),
      Skewness = round(moments::skewness(scores, na.rm = TRUE), 4),
      Kurtosis = round(moments::kurtosis(scores, na.rm = TRUE), 4),
      Min = min(scores, na.rm = TRUE), Max = max(scores, na.rm = TRUE),
      Alpha = if (!is.na(alpha)) round(max(0, min(1, alpha)), 4) else NA,
      stringsAsFactors = FALSE
    )
  }

  if (length(stats_list) > 0) do.call(rbind, stats_list) else NULL
}

audit_plan_connectivity <- function(plan_metadata, scale_id, config) {
  if (is.null(plan_metadata) || nrow(plan_metadata) == 0) {
    return(NULL)
  }

  min_anchor_items <- config$equating$min_anchor_items %||% 10
  good_anchor_items <- config$equating$preferred_anchor_items %||% 20
  min_anchor_cor <- config$equating$min_anchor_cor %||% 0.70
  good_anchor_cor <- config$equating$preferred_anchor_cor %||% 0.85

  out <- plan_metadata
  out$SCALE <- scale_id
  out$STATUS <- "OK"

  idx_crit_n <- which(out$n_common < min_anchor_items)
  idx_warn_n <- which(out$n_common >= min_anchor_items & out$n_common < good_anchor_items)
  idx_crit_r <- which(!is.na(out$r_stability) & out$r_stability < min_anchor_cor)
  idx_warn_r <- which(!is.na(out$r_stability) & out$r_stability >= min_anchor_cor & out$r_stability < good_anchor_cor)

  if (length(idx_warn_n) > 0) out$STATUS[idx_warn_n] <- "WARN: Low N"
  if (length(idx_warn_r) > 0) out$STATUS[idx_warn_r] <- paste(out$STATUS[idx_warn_r], "| WARN: Low R")
  if (length(idx_crit_n) > 0) out$STATUS[idx_crit_n] <- "CRITICAL: Very Low N"
  if (length(idx_crit_r) > 0) out$STATUS[idx_crit_r] <- "CRITICAL: Unstable Anchor"

  out$STATUS <- gsub("^OK \\| ", "", out$STATUS)

  out <- out %>%
    rename(FROM = from, TO = to, WEIGHT = weight, N_ANCHOR = n_common, R_ANCHOR = r_stability) %>%
    select(SCALE, FROM, TO, WEIGHT, N_ANCHOR, R_ANCHOR, STATUS)

  return(out)
}

generate_failure_stub <- function(scale_id, forms_map, ref_form, reason) {
  forms <- names(forms_map)
  targets <- setdiff(forms, ref_form)
  if (length(targets) == 0) targets <- c("UNKNOWN")
  rows <- lapply(targets, function(t) {
    data.frame(
      SCALE_ID = as.character(scale_id), SOURCE_FORM = as.character(t),
      TARGET_FORM = as.character(ref_form), RAW_SCORE = -1,
      EQUATED_SCORE = NA, SEE = NA, METHOD = "FAILURE_STUB",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

generate_executive_manifest <- function(tables, drift, qc, config, forms_map) {
  all_forms <- names(forms_map)

  if (is.null(tables)) {
    return(data.frame(SOURCE_FORM = all_forms, Status = "FAIL_NO_DATA", stringsAsFactors = FALSE))
  }

  # Verificación de columnas de Sesgo (Compatibilidad con v2.4 Network)
  cols_avail <- names(tables)
  if (!"BIAS_ACC" %in% cols_avail) tables$BIAS_ACC <- NA
  if (!"BIAS_NET" %in% cols_avail) tables$BIAS_NET <- NA

  # Agregación usando las nuevas métricas vectoriales
  manifest <- aggregate(
    cbind(TRE, SEE, BIAS_ACC, BIAS_NET) ~ SCALE_ID + SOURCE_FORM,
    data = tables,
    FUN = function(x) round(mean(abs(x), na.rm = TRUE), 4)
  )

  # Renombrado para reporte ejecutivo
  names(manifest)[names(manifest) == "TRE"] <- "Mean_TRE"
  names(manifest)[names(manifest) == "SEE"] <- "Mean_SEE"
  names(manifest)[names(manifest) == "BIAS_ACC"] <- "Mean_Abs_Bias"
  names(manifest)[names(manifest) == "BIAS_NET"] <- "Mean_Net_Bias"

  if (!is.null(drift) && nrow(drift) > 0) {
    if (!"CHAIN_ID" %in% names(drift)) drift$CHAIN_ID <- sub("->.*", "", drift$LINK)
    drift_stats <- drift |>
      group_by(SCALE_ID, SOURCE_FORM = CHAIN_ID) |>
      summarise(Items_Dropped = n_distinct(ITEM[STATUS == "DROPPED"]), Drop_Rate = round(Items_Dropped / n_distinct(ITEM), 2), .groups = "drop")
    manifest <- merge(manifest, drift_stats, by = c("SCALE_ID", "SOURCE_FORM"), all.x = TRUE)
  }

  if (!is.null(qc) && nrow(qc) > 0) {
    qc_summ <- qc |>
      group_by(SCALE_ID, SOURCE_FORM = Source_Form) |>
      summarise(QC_Status = paste(unique(Status), collapse = "|"), .groups = "drop")
    manifest <- merge(manifest, qc_summ, by = c("SCALE_ID", "SOURCE_FORM"), all.x = TRUE)
  }

  limit_tre <- if (!is.null(config$equating$tre_threshold)) config$equating$tre_threshold else 0.5
  manifest$Status <- "OK"
  manifest$Status[manifest$Mean_TRE > limit_tre] <- "WARN"
  if ("QC_Status" %in% names(manifest)) manifest$Status[grepl("FAIL|CRITICAL", manifest$QC_Status)] <- "FAIL"

  processed_forms <- unique(manifest$SOURCE_FORM)
  missing_forms <- setdiff(all_forms, processed_forms)
  ref_form <- unique(tables$TARGET_FORM)[1]
  missing_forms <- setdiff(missing_forms, ref_form)

  if (length(missing_forms) > 0) {
    missing_rows <- data.frame(
      SCALE_ID = unique(manifest$SCALE_ID)[1],
      SOURCE_FORM = missing_forms,
      Mean_TRE = NA, Mean_SEE = NA, Mean_Abs_Bias = NA, Mean_Net_Bias = NA,
      Items_Dropped = NA, Drop_Rate = NA, QC_Status = "NO_LINK",
      Status = "DROP/FAIL",
      stringsAsFactors = FALSE
    )
    manifest <- bind_rows(manifest, missing_rows)
  }

  manifest
}

# =============================================================================
# 2. RUNTIME PRINCIPAL (GLOBAL ONLY)
# =============================================================================

# =============================================================================
# ENRIQUECIMIENTO DE PROCEDENCIA Y AUDITORÍA
# =============================================================================

enrich_provenance <- function(res, config, df_scored) {
  # 1. Identificación de Ítems Tóxicos Globales (Global Bad Actors)
  bad_actors <- NULL

  if (!is.null(res$drift_details) && nrow(res$drift_details) > 0) {
    # Trabajamos sobre una copia local para no afectar el objeto original
    df_drift <- res$drift_details

    # NORMALIZACIÓN DE NOMBRES: Convertimos todo a MAYÚSCULAS para evitar errores (drift vs DRIFT)
    names(df_drift) <- toupper(names(df_drift))

    # NORMALIZACIÓN DE CONTENIDO: Convertimos columnas carácter a MAYÚSCULAS para case insensitivity
    char_cols <- sapply(df_drift, is.character)
    if (any(char_cols)) {
      df_drift[char_cols] <- lapply(df_drift[char_cols], toupper)
    }

    # BLINDAJE: Si la columna DRIFT no existe (ej. se llama 'DIFF' o 'D_P'), la buscamos o creamos dummy
    if (!"DRIFT" %in% names(df_drift)) {
      # Intentamos encontrar columnas candidatas comunes
      candidates <- intersect(c("DIFF", "D_VAL", "DELTA", "DRIFT_RAW"), names(df_drift))
      if (length(candidates) > 0) {
        df_drift$DRIFT <- df_drift[[candidates[1]]]
      } else {
        # Si no hay datos de magnitud de drift, ponemos NA para no romper el reporte
        df_drift$DRIFT <- NA_real_
      }
    }

    # BLINDAJE: Aseguramos que existan ITEM, STATUS y LINK
    if (!"ITEM" %in% names(df_drift) && "ITEM_ID" %in% names(df_drift)) df_drift$ITEM <- df_drift$ITEM_ID
    if (!"LINK" %in% names(df_drift)) df_drift$LINK <- "UNKNOWN"
    if (!"STATUS" %in% names(df_drift)) df_drift$STATUS <- "UNKNOWN"

    # Procesamiento Seguro con dplyr
    bad_actors <- df_drift %>%
      filter(STATUS == "DROPPED") %>%
      group_by(ITEM) %>%
      summarise(
        Fail_Count = n(),
        Total_Links_Tested = n_distinct(LINK),
        Fail_Rate = round(n() / n_distinct(LINK), 2),
        # Usamos na.rm = TRUE para tolerar los NAs generados por el blindaje
        Avg_Abs_Drift = round(mean(abs(Z_SCORE), na.rm = TRUE), 3),
        Worst_Link = if (all(is.na(Z_SCORE))) "N/A" else LINK[which.max(abs(Z_SCORE))][1]
      ) %>%
      arrange(desc(Fail_Rate), desc(Fail_Count))
  }

  # 2. Generación de ID de Ejecución
  exec_id <- paste0(
    format(Sys.time(), "%Y%m%d%H%M%S"), "-",
    tryCatch(Sys.debug()[["user"]], error = function(e) "user"), "-",
    sample(1000:9999, 1)
  )

  # 3. Estructura de Procedencia
  provenance <- list(
    execution_id = exec_id,
    timestamp = Sys.time(),
    user = tryCatch(Sys.debug()[["user"]], error = function(e) "unknown"),
    system_version = "v112.3", # Versión incrementada por el fix

    ##config_snapshot = config,
    inputs = list(
      n_examinees = if (!is.null(df_scored)) nrow(df_scored) else 0,
      n_forms = if (!is.null(df_scored) && "FORMA" %in% names(df_scored)) length(unique(df_scored$FORMA)) else 0,
      n_items_global = if (!is.null(res$drift_details)) tryCatch(length(unique(res$drift_details$ITEM)), error = function(e) 0) else 0
    ),
    global_bad_actors = bad_actors
  )

  res$provenance <- provenance
  return(res)
}

# =============================================================================
# RUNTIME PRINCIPAL
# =============================================================================

run_equating <- function(df_scored, meta, config, raw_dat = NULL, ...) {
  if (missing(config) || !is.list(config)) config <- list()
  if (missing(df_scored) || is.null(df_scored)) stop("Dataframe de puntuaciones nulo.")

  debug("Iniciando motor de equiparación (Modo: GLOBAL SECUENCIAL v112.2)...", "EquatingFacade")

  global_items <- unique(as.character(meta$ITEM_ID))
  scale_id <- "GLOBAL"

  forms_map <- config$forms
  if (is.null(forms_map)) {
    if ("FORMA" %in% names(df_scored)) {
      unique_forms <- unique(as.character(df_scored$FORMA))
      forms_map <- setNames(lapply(unique_forms, function(x) list(codes = x)), unique_forms)
      debug("Mapa de formas inferido de los datos.", "EquatingFacade")
    } else {
      stop("ERROR: No se puede definir el mapa de formas.")
    }
  }

  ref_form_label <- as.character(config$system$reference_form)

  # 1. Topología
  debug("1. Construyendo topología de red...", "EquatingFacade")
  linkage <- tryCatch(
    {
      build_linkage_plan(df_scored, forms_map, global_items, config, scale_id)
    },
    error = function(e) {
      warn(paste("Error en topología:", e$message), "EquatingFacade")
      return(NULL)
    }
  )

  if (is.null(linkage)) {
    error("Fallo crítico en topología: No se pudo construir la red.", "EquatingFacade")
    return(list(error = "Topology Failed", tables = generate_failure_stub(scale_id, forms_map, ref_form_label, "Topology Failed")))
  }

  # 2. Resolución
  debug("2. Resolviendo ecuaciones de equiparación...", "EquatingFacade")
  res <- tryCatch(
    {
      # Llamada al resolver (03_network v2.4)
      resolve_network_equating(linkage, df_scored, forms_map, config)
    },
    error = function(e) {
      warn(paste("Error en resolución:", e$message), "EquatingFacade")
      return(NULL)
    }
  )

  if (is.null(res)) {
    error("Fallo crítico en resolución.", "EquatingFacade")
    return(list(error = "Resolution Failed", tables = generate_failure_stub(scale_id, forms_map, ref_form_label, "Resolution Failed")))
  }

  # 3. Enriquecimiento y Consolidación
  debug("3. Generando reportes y auditoría extendida...", "EquatingFacade")

  # Metadatos descriptivos
  res$descriptives <- generate_descriptive_report(df_scored, forms_map, global_items, scale_id)

  # Auditoría de conectividad
  res$link_quality <- audit_plan_connectivity(linkage$audit_metadata, scale_id, config)

  # Plan de Topología (Edges puros)
  if (!is.null(linkage$edges)) {
    res$topology_plan <- linkage$edges
    res$topology_plan$SCALE <- scale_id
  }

  # Consolidación del Grafo: Preferimos el que devuelve resolve_network (si existe), sino el de linkage
  if (is.null(res$network_graph) && !is.null(linkage$graph)) {
    res$network_graph <- linkage$graph
  }

  # Manifiesto Ejecutivo
  res$executive_summary <- generate_executive_manifest(res$tables, res$drift_details, res$audit_qc, config, forms_map)

  # --- NUEVO PASO DE AUDITORÍA FINAL ---
  # Inyectamos la procedencia, snapshot de config y análisis de bad actors
  res <- enrich_provenance(res, config, df_scored)

  debug("✅ Equiparación terminada.", "EquatingFacade")
  res
}
