# R/03_network.R
# Responsabilidad: Gestión de Topología de Red y Resolución de Enlaces
# Versión: v2.5 - Optimized Graph Topology & Psychometric Friction Logic
# Dependencias: 02_stat_engine.R, igraph

if (!require("pacman")) install.packages("pacman")
pacman::p_load(igraph, dplyr, stats, data.table, moments)

# =============================================================================
# 1. HELPERS: MATEMÁTICAS SEGURAS Y AUDITORÍA
# =============================================================================

safe_bind_rows_list <- function(lst) {
  valid_lst <- Filter(function(x) !is.null(x) && is.data.frame(x) && nrow(x) > 0, lst)
  if (length(valid_lst) == 0) {
    return(NULL)
  }

  # Normalización de columnas para evitar errores de bind
  all_cols <- unique(unlist(lapply(valid_lst, names)))
  normalized_lst <- lapply(valid_lst, function(df) {
    missing <- setdiff(all_cols, names(df))
    if (length(missing) > 0) {
      if (is.data.table(df)) {
        for (col in missing) set(df, j = col, value = NA)
      } else {
        df[missing] <- NA
      }
    }
    df
  })

  tryCatch(do.call(rbind, normalized_lst), error = function(e) NULL)
}

safe_approxfun <- function(x, y) {
  if (length(x) < 2 || all(is.na(y))) {
    return(function(v) rep(0, length(v)))
  }
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 2) {
    return(function(v) rep(0, length(v)))
  }
  approxfun(x[ok], y[ok], rule = 2)
}

audit_specific_cuts <- function(cdf, config, form_name, scale_id) {
  if (!is.list(config) || nrow(cdf) == 0) {
    return(NULL)
  }

  cuts <- NULL
  if (!is.null(config$scoring) && !is.null(config$scoring$performance_levels)) {
    pl <- config$scoring$performance_levels
    if (is.data.frame(pl) && "min_score" %in% names(pl)) {
      cuts <- pl$min_score
    } else if (is.list(pl)) cuts <- unlist(lapply(pl, function(x) if (is.list(x)) x$min_score else NA))
  }

  if (is.null(cuts)) {
    return(NULL)
  }
  cuts <- sort(unique(na.omit(as.numeric(cuts))))
  cuts <- cuts[cuts > 0]
  if (length(cuts) == 0) {
    return(NULL)
  }

  audit_rows <- list()
  for (cut_raw in cuts) {
    idx <- which.min(abs(cdf$RAW_SCORE - cut_raw))
    if (length(idx) == 0) next

    row <- cdf[idx, ]
    audit_rows[[length(audit_rows) + 1]] <- data.frame(
      FORM = form_name,
      TARGET_CUT_RAW_REF = cut_raw,
      EST_RAW_CUT = cut_raw,
      EQUATED_AT_CUT = round(row$EQUATED_SCORE, 2),
      SEE_AT_CUT = if (!is.na(row$SEE)) round(row$SEE, 4) else NA,
      TRE_AT_CUT = if (!is.null(row$TRE) && !is.na(row$TRE)) round(row$TRE, 4) else NA,
      SCALE_ID = scale_id,
      NOTE = "Scoring Level Audit",
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, audit_rows)
}

# =============================================================================
# 2. HELPERS: TOPOLOGÍA Y PRE-CÁLCULO (OPTIMIZADO)
# =============================================================================

get_form_items_robust <- function(form_def, df, available_cols) {
  if (!is.list(form_def)) {
    return(character(0))
  }
  if (!is.null(form_def$items) && length(form_def$items) > 0) {
    return(intersect(form_def$items, available_cols))
  }

  codes <- if (!is.null(form_def$codes)) as.character(form_def$codes) else character(0)
  if (length(codes) == 0) return(character(0))

  # Optimización con data.table (v2.6)
  dt <- if (is.data.table(df)) df else as.data.table(df)
  active_cols <- dt[as.character(FORMA) %in% codes, lapply(.SD, function(x) any(!is.na(x))), .SDcols = available_cols]

  if (nrow(active_cols) == 0) return(character(0))
  names(active_cols)[unlist(active_cols)]
}

# Reemplaza a get_topology_metadata con lógica más robusta para el grafo v2
# Optimizado (v2.6): Agregación en una sola pasada para evitar subsets repetitivos
precompute_form_stats <- function(df, forms_map, items_map) {
  dt <- if (is.data.table(df)) df else as.data.table(df)

  # Identificar todos los ítems únicos necesarios
  all_items <- unique(unlist(items_map))
  if (length(all_items) == 0) return(list())
  all_items <- intersect(all_items, names(dt))
  if (length(all_items) == 0) return(list())

  # --- PASO 1: Agregación Global (One-pass) ---
  # Calculamos sumas y conteos por código de FORMA
  # Usamos as.character en el 'by' para evitar mutar el dt original y asegurar tipos
  sums_dt <- dt[, lapply(.SD, sum, na.rm = TRUE), by = .(FORMA = as.character(FORMA)), .SDcols = all_items]
  cnts_dt <- dt[, lapply(.SD, function(x) sum(!is.na(x))), by = .(FORMA = as.character(FORMA)), .SDcols = all_items]
  n_dt <- dt[, .N, by = .(FORMA = as.character(FORMA))]

  setkey(sums_dt, FORMA)
  setkey(cnts_dt, FORMA)
  setkey(n_dt, FORMA)

  stats_cache <- list()
  forms <- names(forms_map)

  # --- PASO 2: Construcción de Caché por Forma ---
  for (f in forms) {
    codes <- if (is.list(forms_map[[f]])) as.character(forms_map[[f]]$codes) else character(0)
    items <- items_map[[f]]

    if (length(items) == 0 || length(codes) == 0) {
      stats_cache[[f]] <- list(n = 0, p_mean = 0, p_items = numeric(0), items = character(0))
      next
    }

    # Consolidar N (sujetos) para los códigos de esta forma
    n_subj <- sum(n_dt[FORMA %in% codes, N], na.rm = TRUE)

    if (n_subj < 10) {
      stats_cache[[f]] <- list(n = 0, p_mean = 0, p_items = setNames(rep(0, length(items)), items), items = items)
    } else {
      # Extraer y agregar estadísticas de ítems
      valid_f_items <- intersect(items, names(sums_dt))
      f_sums <- sums_dt[FORMA %in% codes, ..valid_f_items]
      f_cnts <- cnts_dt[FORMA %in% codes, ..valid_f_items]

      total_sums <- colSums(f_sums, na.rm = TRUE)
      total_cnts <- colSums(f_cnts, na.rm = TRUE)

      # Calcular p-values (medias)
      item_means <- total_sums / total_cnts
      item_means[total_cnts == 0] <- NA

      # Asegurar que el vector tenga todos los ítems (manejo de faltantes)
      if (length(item_means) < length(items)) {
        miss <- setdiff(items, names(item_means))
        item_means <- c(item_means, setNames(rep(NA_real_, length(miss)), miss))[items]
      }

      stats_cache[[f]] <- list(
        n = n_subj,
        p_mean = mean(item_means, na.rm = TRUE),
        p_items = item_means,
        items = items
      )
    }
  }
  return(stats_cache)
}

# ==============================================================================
# 3. LÓGICA DE GRAFO (NUEVA HEURÍSTICA DE FRICCIÓN)
# ==============================================================================

# Reemplaza a compute_quality_matrix y compute_edge_costs
build_equating_graph_optimized <- function(forms, items_cache, form_stats, config) {
  require(igraph)

  # Recuperar configuración o usar defaults seguros
  min_items <- config$anchor$min_common_items_topology %||% config$equating$min_anchor_items %||% 10

  # Penalización por salto configurable (Default sube de 0.1 a 0.25)
  hop_cost <- config$equating$hop_penalty %||% 0.25

  edge_list <- list()

  # Generar pares únicos (triángulo superior)
  pairs <- combn(forms, 2, simplify = FALSE)

  for (p in pairs) {
    f1 <- p[1]
    f2 <- p[2]

    # Validar existencia de datos
    s1 <- form_stats[[f1]]
    s2 <- form_stats[[f2]]
    if (is.null(s1) || is.null(s2) || s1$n < 10 || s2$n < 10) next

    # A. Intersección de Ítems
    items1 <- if (!is.null(items_cache)) items_cache[[f1]] else s1$items
    items2 <- if (!is.null(items_cache)) items_cache[[f2]] else s2$items

    common <- intersect(items1, items2)
    n_comm <- length(common)

    if (n_comm < min_items) next

    # B. Extracción de Vectores de Dificultad
    p1 <- s1$p_items[common]
    p2 <- s2$p_items[common]

    # Chequeo de seguridad: Varianza cero
    if (any(is.na(p1)) || any(is.na(p2)) || sd(p1, na.rm = TRUE) == 0 || sd(p2, na.rm = TRUE) == 0) next

    # C. CÁLCULO DE COMPONENTES (Heurística v2.5)

    # 1. Estabilidad (Correlación r^3) - "Fricción"
    r_raw <- cor(p1, p2, use = "pairwise.complete.obs")
    if (is.na(r_raw)) next
    r_adj <- max(0, r_raw)
    w_r <- r_adj^3

    # 2. Similitud Poblacional (Suavizada) - "Pendiente"
    diff_p <- abs(mean(p1, na.rm = TRUE) - mean(p2, na.rm = TRUE))
    w_pop <- 1 / (1 + (2 * diff_p))

    # 3. Tamaño Efectivo y Ancla - "Caudal"
    # Saturación logarítmica para N y tope para items
    n_eff <- min(s1$n, s2$n)
    w_n <- log10(n_eff + 10) * sqrt(min(n_comm, 40))

    # D. PESO Y COSTO FINAL
    weight <- w_n * w_r * w_pop

    if (weight > 0.001) {
      # COSTO: (Inverso de Calidad) + (Peaje por Salto)
      cost <- (10 / weight) + hop_cost

      edge_entry <- data.frame(
        from = f1,
        to = f2,
        weight = weight,
        cost = cost,
        n_common = n_comm,
        r_stability = r_raw,
        pop_similarity = w_pop, # Guardamos el factor calculado
        n_eff = n_eff,
        stringsAsFactors = FALSE
      )

      edge_list[[length(edge_list) + 1]] <- edge_entry
    }
  }

  if (length(edge_list) == 0) {
    return(NULL)
  }

  edges_df <- do.call(rbind, edge_list)

  # Crear Grafo No Dirigido con pesos y costos
  g <- graph_from_data_frame(edges_df, directed = FALSE)

  # Retornamos una lista con el grafo y el dataframe de auditoría (para compatibilidad)
  list(graph = g, audit = edges_df)
}

resolve_reference_form <- function(g, config_ref, forms) {
  ref <- if (!is.null(config_ref)) as.character(config_ref) else "UNKNOWN"
  if (!ref %in% forms) {
    node_strength <- strength(g, weights = E(g)$weight)
    ref <- names(node_strength)[which.max(node_strength)]
  }
  ref
}

extract_shortest_paths <- function(g, ref_form, forms, scale_id, audit_df) {
  comps <- components(g)
  if (!ref_form %in% names(comps$membership)) {
    return(NULL)
  }

  ref_cluster <- comps$membership[ref_form]

  # Dijkstra usando el costo calculado (Inverso de calidad v2.5)
  paths <- shortest_paths(g, from = ref_form, to = forms, weights = E(g)$cost)
  edges_list <- list()

  for (i in seq_along(forms)) {
    target <- forms[i]
    if (target == ref_form || comps$membership[target] != ref_cluster) next

    v_seq <- paths$vpath[[i]]
    if (length(v_seq) < 2) next

    path_nodes <- V(g)[as.numeric(v_seq)]$name
    for (k in length(path_nodes):2) {
      src <- path_nodes[k]
      dest <- path_nodes[k - 1]
      link_id <- paste(src, dest, sep = "->")

      if (!link_id %in% names(edges_list)) {
        w_idx <- igraph::get_edge_ids(g, c(src, dest))

        # Búsqueda segura en audit_df bidireccional
        meta_row <- NULL
        if (!is.null(audit_df)) {
          meta_row <- audit_df[(audit_df$from == src & audit_df$to == dest) |
            (audit_df$from == dest & audit_df$to == src), ]
        }

        edges_list[[link_id]] <- data.frame(
          from = src, to = dest,
          weight = E(g)$weight[w_idx],
          cost = E(g)$cost[w_idx],
          scale = scale_id,
          n_common = if (!is.null(meta_row)) meta_row$n_common[1] else NA,
          r_stability = if (!is.null(meta_row)) meta_row$r_stability[1] else NA,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(edges_list) == 0) {
    return(NULL)
  }
  do.call(rbind, edges_list)
}

# ==============================================================================
# 4. ORQUESTADOR DE PLAN (PLAN BUILDER)
# ==============================================================================

build_linkage_plan <- function(df, forms_map, items_global, config, scale_id) {
  if (!is.list(config)) config <- list()
  if (length(forms_map) < 2) {
    return(NULL)
  }
  forms <- names(forms_map)

  # 1. Identificación de Ítems (Optimizado v2.6)
  dt <- if (is.data.table(df)) df else as.data.table(df)
  avail <- setdiff(names(dt), c("ID", "PERSON_ID", "FORMA", "SCORE", "TOTAL", "CASE", "CLUSTER", "ESTADO"))
  if (!is.null(items_global)) avail <- intersect(avail, items_global)

  # Pre-calcular presencia de ítems por FORMA para evitar subsets repetitivos
  needs_scan_any <- any(sapply(forms, function(f) is.null(forms_map[[f]]$items) || length(forms_map[[f]]$items) == 0))
  if (needs_scan_any) {
    presence_dt <- dt[, lapply(.SD, function(x) any(!is.na(x))), by = .(FORMA_STR = as.character(FORMA)), .SDcols = avail]
  }

  items_cache <- lapply(forms, function(f) {
    f_def <- forms_map[[f]]
    if (!is.list(f_def)) return(character(0))
    if (!is.null(f_def$items) && length(f_def$items) > 0) return(intersect(f_def$items, avail))

    codes <- as.character(f_def$codes)
    if (length(codes) == 0 || !exists("presence_dt")) return(character(0))

    f_presence <- presence_dt[FORMA_STR %in% codes, ..avail]
    if (nrow(f_presence) == 0) return(character(0))

    # Identificar columnas que tienen al menos un TRUE en los códigos de esta forma
    presence_vec <- if (nrow(f_presence) == 1) unlist(f_presence) else colSums(f_presence) > 0
    names(f_presence)[presence_vec]
  })
  names(items_cache) <- forms

  # 2. Pre-Cálculo de Estadísticas (Nuevo Motor)
  # Esto reemplaza el acceso repetido al dataframe grande
  form_stats <- precompute_form_stats(df, forms_map, items_cache)

  # 3. Construcción del Grafo Optimizado (v2.5)
  graph_result <- build_equating_graph_optimized(forms, items_cache, form_stats, config)

  if (is.null(graph_result)) {
    return(NULL)
  }

  g <- graph_result$graph
  audit_metadata <- graph_result$audit

  # 4. Resolución de Rutas
  ref_form <- resolve_reference_form(g, config$system$reference_form, forms)
  plan_df <- extract_shortest_paths(g, ref_form, forms, scale_id, audit_metadata)

  if (is.null(plan_df)) {
    return(NULL)
  }

  structure(list(
    edges = plan_df,
    graph = g,
    items_map = items_cache,
    ref_form = ref_form,
    scale_id = scale_id,
    audit_metadata = plan_df
  ), class = "LinkagePlan")
}

# =============================================================================
# 5. RESOLUCIÓN DE RED (ENGINE & CHAINING)
# =============================================================================

resolve_network_equating <- function(plan, df, forms_map, config, env_cache = NULL, forced_min_anchor = NULL, ...) {
  if (is.null(plan) || is.null(plan$edges) || nrow(plan$edges) == 0) {
    return(NULL)
  }

  ctx <- init_equating_context(df, config, plan, env_cache, forced_min_anchor)

  # Fase 1: Equiparación por Pares
  pairwise_out <- execute_pairwise_phase(ctx, forms_map, plan)

  # Fase 2: Encadenamiento (Chaining) con propagación vectorial
  chaining_out <- execute_chaining_phase(ctx, pairwise_out$link_map, forms_map, plan)

  all_critical_list <- c(pairwise_out$audit$critical, chaining_out$audit_critical)
  crit_alerts <- safe_bind_rows_list(all_critical_list)

  list(
    tables = safe_bind_rows_list(chaining_out$tables),
    audit_path = safe_bind_rows_list(chaining_out$audit_path),
    audit_qc = safe_bind_rows_list(pairwise_out$audit$qc),
    equated_moments = safe_bind_rows_list(chaining_out$moments),
    audit_critical = crit_alerts,
    drift_details = safe_bind_rows_list(pairwise_out$audit$drift),
    coefficients = safe_bind_rows_list(pairwise_out$audit$coeffs),
    decisions = safe_bind_rows_list(pairwise_out$audit$decisions),
    smoothing_history = safe_bind_rows_list(pairwise_out$audit$anova),
    # Pasaporte Psicométrico
    psychometric_passport = safe_bind_rows_list(pairwise_out$audit$passport),
    network_graph = plan$graph,
    link_meta = safe_bind_rows_list(pairwise_out$audit$link_meta)
  )
}

init_equating_context <- function(df, config, plan, env_cache, forced_min_anchor) {
  if (!is.list(config)) config <- list()
  eq_conf <- config$equating %||% list()
  dt <- if (is.data.table(df)) copy(df) else as.data.table(df)
  if (!"FORMA" %in% names(dt)) dt[, FORMA := as.character(NA)]
  dt[, FORMA := as.character(FORMA)]
  setkey(dt, FORMA)

  list(
    dt = dt, config = config, env_cache = if (is.null(env_cache)) new.env() else env_cache,
    sc_id = plan$scale_id, ref_form = plan$ref_form,
    allow_fallback = eq_conf$allow_identity_fallback %||% FALSE,
    force_linear = eq_conf$force_linear_models %||% FALSE,
    min_anchor = forced_min_anchor %||% eq_conf$min_anchor_items %||% 10,
    chain_penalty = eq_conf$chain_vif_penalty %||% 0.10
  )
}

execute_pairwise_phase <- function(ctx, forms_map, plan) {
  edges <- plan$edges
  unique_links <- unique(edges[, c("from", "to")])
  link_map <- new.env()

  audit_storage <- list(
    decisions = list(), drift = list(), coeffs = list(),
    qc = list(), anova = list(), critical = list(),
    passport = list(), link_meta = list()
  )

  for (i in seq_len(nrow(unique_links))) {
    u <- as.character(unique_links$from[i])
    v <- as.character(unique_links$to[i])
    res <- process_single_link(u, v, ctx, forms_map, plan)

    if (!is.null(res) && is.list(res)) {
      link_map[[paste(u, v, sep = "->")]] <- res
      audit_storage <- collect_audit_logs(audit_storage, res, u, v, ctx$sc_id, ctx$config, length(plan$items_map[[v]]))
    }
  }
  list(link_map = link_map, audit = audit_storage)
}

process_single_link <- function(src, tgt, ctx, forms_map, plan) {
  def_src <- forms_map[[src]]
  def_tgt <- forms_map[[tgt]]
  items_src <- plan$items_map[[src]]
  items_tgt <- plan$items_map[[tgt]]

  common <- intersect(items_src, items_tgt)
  if (length(common) < ctx$min_anchor) {
    return(NULL)
  }

  codes_src <- unique(as.character(def_src$codes))
  codes_tgt <- unique(as.character(def_tgt$codes))
  dt_src <- ctx$dt[FORMA %in% codes_src]
  dt_tgt <- ctx$dt[FORMA %in% codes_tgt]

  if (nrow(dt_src) == 0 || nrow(dt_tgt) == 0) {
    return(NULL)
  }

  equate_pair_champion(
    df_src = as.data.frame(dt_src), df_dest = as.data.frame(dt_tgt),
    items_src = items_src, items_dest = items_tgt,
    config = ctx$config, is_global = (ctx$sc_id == "GLOBAL"), min_anc = ctx$min_anchor,
    env_cache = ctx$env_cache, scale_id = ctx$sc_id,
    allow_identity_fallback = ctx$allow_fallback, force_linear_models = ctx$force_linear
  )
}

# =============================================================================
# 6. CHAINING (PROPAGACIÓN VECTORIAL)
# =============================================================================

execute_chaining_phase <- function(ctx, link_map, forms_map, plan) {
  edges <- plan$edges
  all_nodes <- unique(c(edges$from, edges$to))
  g <- igraph::graph_from_data_frame(d = edges, directed = TRUE)

  if (!ctx$ref_form %in% igraph::V(g)$name) {
    return(list(tables = list(), audit_path = list(), moments = list()))
  }

  valid_nodes <- intersect(all_nodes, igraph::V(g)$name)
  out_tables <- list()
  out_paths <- list()
  out_moments <- list()
  out_critical <- list()

  for (node in valid_nodes) {
    items_node <- plan$items_map[[node]]

    if (node == ctx$ref_form) {
      res <- generate_identity_table(node, items_node, ctx$sc_id)
      out_tables[[length(out_tables) + 1]] <- res
      out_moments[[length(out_moments) + 1]] <- calculate_moments(res, node, items_node, ctx, forms_map)
      next
    }

    path_obj <- tryCatch(igraph::shortest_paths(g, from = node, to = ctx$ref_form, mode = "out", output = "vpath"), error = function(e) NULL)

    if (is.null(path_obj) || length(path_obj$vpath) < 1) {
      out_paths[[length(out_paths) + 1]] <- data.frame(Form = node, Status = "IGRAPH_ERR", Path = "Calc Failed", stringsAsFactors = FALSE)
      next
    }

    v_seq <- path_obj$vpath[[1]]
    path_nodes <- if (length(v_seq) > 0) tryCatch(igraph::as_ids(v_seq), error = function(e) character(0)) else character(0)

    if (length(path_nodes) < 2) {
      out_paths[[length(out_paths) + 1]] <- data.frame(Form = node, Status = "BROKEN", Path = "No Path defined in Plan", stringsAsFactors = FALSE)
      next
    }

    # Llamada a la nueva función de cálculo vectorial
    chain_res <- compute_chain_math_vectorized(path_nodes, link_map, ctx$chain_penalty)

    if (chain_res$valid) {
      cdf <- data.frame(
        RAW_SCORE = chain_res$raw,
        EQUATED_SCORE = chain_res$eq,
        STEP_SCORE = chain_res$step,
        SCALE_ID = ctx$sc_id, SOURCE_FORM = node, TARGET_FORM = ctx$ref_form,
        SEE = chain_res$see,
        TRE = chain_res$tre,
        BIAS_ACC = chain_res$bias,
        BIAS_NET = chain_res$net_bias,
        METHOD = chain_res$method_chain,
        stringsAsFactors = FALSE
      )
      out_tables[[length(out_tables) + 1]] <- cdf
      out_paths[[length(out_paths) + 1]] <- data.frame(Form = node, Status = "OK", Path = paste(path_nodes, collapse = "->"), stringsAsFactors = FALSE)
      out_moments[[length(out_moments) + 1]] <- calculate_moments(cdf, node, items_node, ctx, forms_map)

      cut_chk <- audit_specific_cuts(cdf, ctx$config, node, ctx$sc_id)
      if (!is.null(cut_chk)) out_critical[[length(out_critical) + 1]] <- cut_chk
    } else {
      out_paths[[length(out_paths) + 1]] <- data.frame(Form = node, Status = "BROKEN_LINK", Path = paste(path_nodes, collapse = "->"), stringsAsFactors = FALSE)
    }
  }

  list(tables = out_tables, audit_path = out_paths, moments = out_moments, audit_critical = out_critical)
}

# [CORE] Matemática Vectorial de Chaining
compute_chain_math_vectorized <- function(path_nodes, link_map, penalty_factor) {
  steps <- length(path_nodes) - 1

  raw_base <- NULL
  curr_vec <- NULL
  cum_se2 <- 0
  cum_bias_vec <- 0
  net_bias_vec <- 0
  method_list <- c()

  for (j in 1:steps) {
    src <- path_nodes[j]
    tgt <- path_nodes[j + 1]
    key <- paste(src, tgt, sep = "->")

    if (!exists(key, envir = link_map)) {
      return(list(valid = FALSE))
    }
    lnk <- get(key, envir = link_map)
    tbl <- lnk$concordance
    if (is.null(tbl) || nrow(tbl) == 0) {
      return(list(valid = FALSE))
    }

    method_name <- if (is.list(lnk$meta) && !is.null(lnk$meta$method)) lnk$meta$method else "UNKNOWN"
    method_list <- c(method_list, method_name)

    se_vec_fn <- safe_approxfun(tbl$scale, lnk$se)

    bias_link_vec <- if (!is.null(lnk$bias)) lnk$bias else rep(0, nrow(tbl))
    bias_vec_fn <- safe_approxfun(tbl$scale, bias_link_vec)

    if (j == 1) {
      raw_base <- tbl$scale
      curr_vec <- tbl$yx

      se_vals <- se_vec_fn(raw_base)
      bias_vals <- bias_vec_fn(raw_base)

      cum_se2 <- se_vals^2
      cum_bias_vec <- abs(bias_vals)
      net_bias_vec <- bias_vals
    } else {
      prev_scores <- curr_vec
      curr_vec <- approx(x = tbl$scale, y = tbl$yx, xout = prev_scores, rule = 2)$y

      added_se <- se_vec_fn(prev_scores)
      added_bias <- bias_vec_fn(prev_scores)

      local_slope <- calculate_local_slope(tbl$scale, tbl$yx, prev_scores)

      cum_se2 <- (added_se^2) + ((local_slope^2) * cum_se2)

      cum_bias_vec <- cum_bias_vec + abs(added_bias)
      net_bias_vec <- net_bias_vec + added_bias
    }
  }

  final_see <- sqrt(cum_se2)
  if (steps > 1) final_see <- final_see * (1 + (penalty_factor * (steps - 1)))

  final_tre <- sqrt(final_see^2 + net_bias_vec^2)

  full_method_str <- paste(method_list, collapse = " -> ")
  if (steps > 1) full_method_str <- paste0("CHAIN[", steps, "]: ", full_method_str)

  list(
    valid = TRUE, raw = raw_base, eq = curr_vec, step = curr_vec,
    see = final_see, tre = final_tre,
    bias = cum_bias_vec, net_bias = net_bias_vec,
    steps = steps, method_chain = full_method_str
  )
}

# =============================================================================
# 7. UTILS & AUDIT COLLECTOR
# =============================================================================

calculate_local_slope <- function(scale_vec, yx_vec, query_points) {
  if (length(scale_vec) < 2) {
    return(rep(1, length(query_points)))
  }
  tryCatch(
    {
      fun_spline <- splinefun(scale_vec, yx_vec, method = "natural")
      slopes <- abs(fun_spline(query_points, deriv = 1))
      slopes[slopes < 0.001] <- 0.001
      return(slopes)
    },
    error = function(e) rep(1, length(query_points))
  )
}

generate_identity_table <- function(node, items, sc_id) {
  sc_vec <- 0:length(items)
  data.frame(
    RAW_SCORE = sc_vec, EQUATED_SCORE = sc_vec, STEP_SCORE = sc_vec,
    SCALE_ID = sc_id, SOURCE_FORM = node, TARGET_FORM = node,
    SEE = 0, TRE = 0, BIAS_ACC = 0, BIAS_NET = 0, METHOD = "IDENTITY",
    stringsAsFactors = FALSE
  )
}

calculate_moments <- function(cdf, node, items_node, ctx, forms_map) {
  if (is.null(forms_map[[node]]) || nrow(cdf) == 0) {
    return(data.frame(SCALE_ID = ctx$sc_id, FORM = node, N = 0, MEAN_EQ = NA, SD_EQ = NA))
  }
  codes <- unique(as.character(forms_map[[node]]$codes))
  dt_node <- ctx$dt[FORMA %in% codes]
  if (nrow(dt_node) == 0) {
    return(data.frame(SCALE_ID = ctx$sc_id, FORM = node, N = 0, MEAN_EQ = NA, SD_EQ = NA))
  }

  valid_items <- intersect(items_node, names(dt_node))
  if (length(valid_items) == 0) {
    return(data.frame(SCALE_ID = ctx$sc_id, FORM = node, N = 0, MEAN_EQ = NA, SD_EQ = NA))
  }

  raw_sc <- rowSums(dt_node[, valid_items, with = FALSE], na.rm = TRUE)
  mapped <- approx(x = cdf$RAW_SCORE, y = cdf$EQUATED_SCORE, xout = raw_sc, rule = 2)$y

  data.frame(
    SCALE_ID = ctx$sc_id,
    FORM = node,
    N = length(raw_sc),
    MEAN_EQ = round(mean(mapped, na.rm = TRUE), 4),
    SD_EQ = round(sd(mapped, na.rm = TRUE), 4),
    SKEWNESS_EQ = round(moments::skewness(mapped, na.rm = TRUE), 4),
    KURTOSIS_EQ = round(moments::kurtosis(mapped, na.rm = TRUE), 4),
    MIN_EQ = min(mapped, na.rm = TRUE),
    MAX_EQ = max(mapped, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

collect_audit_logs <- function(store, res, src, tgt, sc_id, config, n_items) {
  link_id <- paste(src, tgt, sep = "->")

  # 1. Semáforo y Decisiones
  d_log <- if (!is.null(res$audit$decision_log)) res$audit$decision_log else res$decision_log

  if (!is.null(d_log)) {
    d <- as.data.frame(d_log)
    d$SCALE_ID <- sc_id
    d$LINK <- link_id

    # Captura de Semáforo
    if (!is.null(res$audit$traffic_light)) {
      d$TRAFFIC_LIGHT <- res$audit$traffic_light$color
      d$TL_FLAGS <- paste(res$audit$traffic_light$flags, collapse = "|")
    }
    store$decisions[[length(store$decisions) + 1]] <- d
  }

  # 2. QC (Control de Calidad)
  if (!is.null(res$audit$traffic_light) && !is.null(res$audit$traffic_light$metrics)) {
    qc_mets <- res$audit$traffic_light$metrics
    qc_flags <- res$audit$traffic_light$flags

    store$qc[[length(store$qc) + 1]] <- data.frame(
      SCALE_ID = sc_id,
      Source_Form = src,
      Target_Form = tgt,
      Violations = qc_mets$violations_fixed %||% 0,
      Status = qc_flags %||% "UNKNOWN",
      MAX_ADJ = qc_mets$max_pava_adj %||% 0,
      stringsAsFactors = FALSE
    )
  }

  # 3. Coeficientes (Model Params)
  mp <- if (!is.null(res$audit$model_params)) res$audit$model_params else res$audit$linear_params
  if (!is.null(mp)) {
    store$coeffs[[length(store$coeffs) + 1]] <- data.frame(
      SCALE_ID = sc_id, LINK_SOURCE = src, LINK_TARGET = tgt,
      SLOPE = mp$slope %||% NA,
      INTERCEPT = mp$intercept %||% NA,
      GAMMA = mp$gamma %||% NA,
      METHOD_USED = res$meta$method %||% "UNKNOWN",
      stringsAsFactors = FALSE
    )
  }

  # 4. Historial de Suavizado
  if (!is.null(res$audit$smoothing_history)) {
    sh <- as.data.frame(res$audit$smoothing_history)
    sh$SCALE_ID <- sc_id
    sh$LINK <- link_id
    store$anova[[length(store$anova) + 1]] <- sh
  }

  # 5. Pasaporte Psicométrico
  if (!is.null(res$psychometric_passport)) {
    pp <- res$psychometric_passport
    get_stat <- function(lst, field) if (!is.null(lst)) lst[[field]] else NA

    pp_entry <- data.frame(
      SCALE_ID = sc_id, LINK = link_id,
      SRC_RAW_MEAN = get_stat(pp$stats_raw_src$total, "mean"),
      SRC_SMOOTH_MEAN = get_stat(pp$stats_smooth_src$total, "mean"),
      SRC_RAW_SD = get_stat(pp$stats_raw_src$total, "sd"),
      SRC_SMOOTH_SD = get_stat(pp$stats_smooth_src$total, "sd"),
      SRC_RAW_SKEW = get_stat(pp$stats_raw_src$total, "skew"),
      SRC_SMOOTH_SKEW = get_stat(pp$stats_smooth_src$total, "skew"),
      SRC_RAW_KURT = get_stat(pp$stats_raw_src$total, "kurt"),
      SRC_SMOOTH_KURT = get_stat(pp$stats_smooth_src$total, "kurt"),
      DEST_RAW_MEAN = get_stat(pp$stats_raw_dest$total, "mean"),
      DEST_SMOOTH_MEAN = get_stat(pp$stats_smooth_dest$total, "mean"),
      DEST_RAW_SD = get_stat(pp$stats_raw_dest$total, "sd"),
      DEST_SMOOTH_SD = get_stat(pp$stats_smooth_dest$total, "sd"),
      DEST_RAW_SKEW = get_stat(pp$stats_raw_dest$total, "skew"),
      DEST_SMOOTH_SKEW = get_stat(pp$stats_smooth_dest$total, "skew"),
      DEST_RAW_KURT = get_stat(pp$stats_raw_dest$total, "kurt"),
      DEST_SMOOTH_KURT = get_stat(pp$stats_smooth_dest$total, "kurt"),
      stringsAsFactors = FALSE
    )
    store$passport[[length(store$passport) + 1]] <- pp_entry
  }

  # 6. Drift Details (Recolección explícita)
  if (!is.null(res$drift)) {
    dr <- as.data.frame(res$drift)
    dr$SCALE_ID <- sc_id
    dr$LINK <- link_id
    store$drift[[length(store$drift) + 1]] <- dr
  }


  if (!is.null(res$meta)) {
    m <- as.data.frame(res$meta, stringsAsFactors = FALSE)
    m$SCALE_ID <- sc_id
    m$LINK <- link_id
    store$link_meta[[length(store$link_meta) + 1]] <- m
  }

  return(store)
}
