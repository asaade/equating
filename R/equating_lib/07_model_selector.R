# R/equating_lib/07_model_selector.R
# Responsabilidad: Selección competitiva de modelos de equiparación.
# Versión: v1.0
# Dependencias: equate

if (!exists("info")) source("R/00_common_base.R")

# =============================================================================
# 1. HELPERS DE EXTRACCIÓN Y CÁLCULO
# =============================================================================

extract_se_universal <- function(eq_obj, n_points) {
  if (!is.list(eq_obj)) {
    return(rep(0, n_points))
  }

  if (!is.null(eq_obj$boot) && is.list(eq_obj$boot) && !is.null(eq_obj$boot$se)) {
    return(as.numeric(eq_obj$boot$se))
  }

  if (!is.null(eq_obj$concordance)) {
    if (is.list(eq_obj$concordance)) {
      if (!is.null(eq_obj$concordance$se.b)) {
        return(as.numeric(eq_obj$concordance$se.b))
      }
      if (!is.null(eq_obj$concordance$se)) {
        se_val <- eq_obj$concordance$se
        if (is.list(se_val) && !is.null(se_val$g)) {
          return(as.numeric(se_val$g))
        }
        if (is.numeric(se_val)) {
          return(as.numeric(se_val))
        }
      }
    }
  }

  if (!is.null(eq_obj$se) && is.numeric(eq_obj$se)) {
    return(as.numeric(eq_obj$se))
  }
  return(rep(0, n_points))
}

# Helper crítico para decidir Tucker vs Levine
calculate_smd_internal <- function(ft_x, ft_y) {
  if (sum(ft_x) < 2 || sum(ft_y) < 2) {
    return(0)
  }

  get_stats <- function(ft) {
    sc <- as.numeric(names(ft))
    fr <- as.numeric(ft)
    n <- sum(fr)
    mu <- sum(sc * fr) / n
    var <- sum(fr * (sc - mu)^2) / (n - 1)
    return(list(mu = mu, var = var))
  }

  st_x <- get_stats(ft_x)
  st_y <- get_stats(ft_y)

  pooled_sd <- sqrt((st_x$var + st_y$var) / 2)
  if (pooled_sd < 1e-6) {
    return(0)
  }

  return((st_x$mu - st_y$mu) / pooled_sd)
}

extract_se_at_point <- function(obj, idx) {
  se_vec <- NULL
  if (!is.null(obj$se)) se_vec <- obj$se
  if (is.null(se_vec) && !is.null(obj$boot$se)) se_vec <- obj$boot$se

  if (!is.null(se_vec) && length(se_vec) >= idx) {
    return(as.numeric(se_vec[idx]))
  }
  return(NA)
}

check_gatekeeper <- function(candidate_obj, linear_obj, cut_scores, sd_target, config) {
  # Si no hay cortes o SD, pasamos (no hay nada que vigilar)
  if (is.null(cut_scores) || length(cut_scores) == 0 || is.na(sd_target)) {
    return(list(pass = TRUE, reason = ""))
  }

  eq_cfg <- config$equating %||% list()

  # Kolen & Brennan sugieren SEE <= 0.1 SD (usamos 0.12 como default prudente)
  see_factor <- eq_cfg$gatekeeper_see_factor %||% 0.12
  max_see <- sd_target * see_factor

  # DTM Local
  local_dtm <- eq_cfg$dtm_raw_score %||% 0.5

  # 1. Mapeo Inverso: Usamos la lineal para ubicar "dónde importa" mirar en la escala cruda
  lin_slope <- linear_obj$slope
  lin_intercept <- linear_obj$intercept

  if (is.null(lin_slope) || is.na(lin_slope) || lin_slope < 1e-3) {
    return(list(pass = TRUE, reason = "Linear invalid for projection"))
  }

  y_crit_raw <- (cut_scores - lin_intercept) / lin_slope
  y_crit_idx <- round(y_crit_raw)

  min_y <- min(candidate_obj$x, na.rm = TRUE)
  max_y <- max(candidate_obj$x, na.rm = TRUE)
  valid_idx <- y_crit_idx >= min_y & y_crit_idx <= max_y
  y_check_points <- y_crit_idx[valid_idx]

  if (length(y_check_points) == 0) {
    return(list(pass = TRUE, reason = "Cuts out of range"))
  }

  # 2. Verificación Punto a Punto
  for (y_pt in y_check_points) {
    idx_loc <- which(candidate_obj$x == y_pt)
    if (length(idx_loc) == 0) next

    y_eq_cand <- candidate_obj$concordance[idx_loc, "yx"]
    see_cand <- extract_se_at_point(candidate_obj, idx_loc)

    # Referencia Lineal (para chequear distorsión local)
    y_eq_lin <- tryCatch(linear_obj$concordance[idx_loc, "yx"], error = function(e) NA)

    # CHECK A: SEE Excesivo (Estabilidad)
    if (!is.na(see_cand) && see_cand > max_see) {
      return(list(
        pass = FALSE,
        reason = sprintf("Gatekeeper: Unstable at Cut (Raw=%d, SEE=%.2f > Limit=%.2f)", y_pt, see_cand, max_see)
      ))
    }

    # CHECK B: Divergencia Injustificada (Distorsión)
    if (!is.null(y_eq_lin) && !is.na(y_eq_lin)) {
      diff_val <- abs(y_eq_cand - y_eq_lin)
      if (!is.na(diff_val) && diff_val > local_dtm) {
        return(list(
          pass = FALSE,
          reason = sprintf("Gatekeeper: Distortion at Cut (Raw=%d, Diff=%.2f > DTM=%.2f)", y_pt, diff_val, local_dtm)
        ))
      }
    }
  }

  return(list(pass = TRUE, reason = ""))
}

calculate_weighted_rmsd <- function(conc_x, conc_y, weights) {
  if (is.null(conc_x) || is.null(conc_y)) {
    return(NA_real_)
  }
  df_x <- as.data.frame(conc_x)
  df_y <- as.data.frame(conc_y)

  merged <- merge(df_x, df_y, by = "scale", suffixes = c("_lin", "_equi"))
  if (nrow(merged) < 2) {
    return(NA_real_)
  }

  idx_char <- as.character(merged$scale)
  if (!is.null(names(weights))) {
    w_final <- weights[idx_char]
  } else {
    warning("calculate_weighted_rmsd: Weights sin nombre. Usando uniformes.")
    w_final <- rep(1, nrow(merged))
  }
  w_final[is.na(w_final)] <- 0
  sum_w <- sum(w_final)
  if (sum_w <= 0) {
    return(NA_real_)
  }

  diffs_sq <- (merged$yx_lin - merged$yx_equi)^2
  sqrt(sum(diffs_sq * (w_final / sum_w), na.rm = TRUE))
}

calculate_weighted_bias <- function(conc_x, conc_y, weights) {
  if (is.null(conc_x) || is.null(conc_y)) {
    return(NA_real_)
  }
  df_x <- as.data.frame(conc_x)
  df_y <- as.data.frame(conc_y)
  merged <- merge(df_x, df_y, by = "scale", suffixes = c("_cand", "_ref"))
  if (nrow(merged) < 2) {
    return(NA_real_)
  }

  idx <- as.character(merged$scale)
  w_final <- if (!is.null(names(weights))) weights[idx] else weights[merged$scale + 1]
  w_final[is.na(w_final)] <- 0
  sum_w <- sum(w_final)
  if (sum_w <= 0) {
    return(NA_real_)
  }

  diffs <- (merged$yx_cand - merged$yx_ref)
  sum(diffs * (w_final / sum_w), na.rm = TRUE)
}

get_cut_scores <- function(config) {
  if (is.null(config$scoring$performance_levels)) {
    return(NULL)
  }
  levels <- config$scoring$performance_levels
  cuts <- if (is.data.frame(levels)) levels$min_score else sapply(levels, function(x) x$min_score)
  cuts <- as.numeric(unlist(cuts))
  unique(sort(cuts[!is.na(cuts) & cuts > 0]))
}

calculate_critical_csee <- function(eq_obj, cuts) {
  if (length(cuts) == 0 || !is.list(eq_obj)) {
    return(NA_real_)
  }
  conc <- if (!is.null(eq_obj$conc)) eq_obj$conc else eq_obj$concordance
  if (is.null(conc)) {
    return(NA_real_)
  }

  se_vec <- extract_se_universal(eq_obj, nrow(conc))
  if (all(is.na(se_vec))) {
    return(NA_real_)
  }

  scale_x <- as.numeric(conc$scale)
  indices <- sapply(cuts, function(c) which.min(abs(scale_x - c)))
  mean(se_vec[unique(indices)], na.rm = TRUE)
}

calculate_wiggliness_yx <- function(equating_obj) {
  yx <- NULL
  if (is.list(equating_obj) && !is.null(equating_obj$concordance)) {
    yx <- equating_obj$concordance$yx
  } else if (is.numeric(equating_obj)) {
    yx <- equating_obj
  }

  if (is.null(yx) || length(yx) < 3) {
    return(NA)
  }
  yx <- yx[!is.na(yx)]
  if (length(yx) < 3) {
    return(NA)
  }

  d2 <- diff(yx, differences = 2)
  sqrt(mean(d2^2, na.rm = TRUE))
}

# =============================================================================
# 2. CONSTRUCCIÓN DE MODELO SINTÉTICO (Model Averaging)
# =============================================================================

build_synthetic_model <- function(lin_obj, equi_obj) {
  if (is.null(lin_obj) || is.null(equi_obj)) {
    return(NULL)
  }
  if (!is.list(lin_obj) || !is.list(equi_obj)) {
    return(NULL)
  }

  c_lin <- if (!is.null(lin_obj$conc)) lin_obj$conc else lin_obj$concordance
  c_eq <- if (!is.null(equi_obj$conc)) equi_obj$conc else equi_obj$concordance

  if (is.null(c_lin) || is.null(c_eq)) {
    return(NULL)
  }
  merged <- merge(c_lin, c_eq, by = "scale", suffixes = c(".lin", ".eq"))
  if (nrow(merged) < 2) {
    return(NULL)
  }

  se_lin <- extract_se_universal(lin_obj, nrow(merged))
  se_eq <- extract_se_universal(equi_obj, nrow(merged))

  # Ponderación por varianza inversa (minimiza varianza del estimador combinado)
  if (all(se_lin == 0) || all(se_eq == 0)) {
    w <- rep(0.5, nrow(merged))
  } else {
    var_lin <- pmax(se_lin^2, 1e-8)
    var_eq <- pmax(se_eq^2, 1e-8)
    w <- var_eq / (var_lin + var_eq)
  }

  yx_syn <- w * merged$yx.lin + (1 - w) * merged$yx.eq
  se_syn <- sqrt(w^2 * var_lin + (1 - w)^2 * var_eq)

  conc_syn <- data.frame(scale = merged$scale, yx = yx_syn)

  list(
    concordance = conc_syn,
    se = se_syn,
    x = lin_obj$x, y = lin_obj$y,
    type = "synthetic", method = "weighted average",
    meta = list(weights = w)
  )
}

# =============================================================================
# 3. EJECUCIÓN DE MODELOS
# =============================================================================

run_candidate_models <- function(ft_src, ft_dest, config, boot_reps = 0,
                                 method_list = c("TUCKER", "LEVINE", "EQUIPERCENTILE", "CIRCLEARC")) {
  candidates <- list()
  cfg_eq <- config$equating %||% list()
  strategy <- cfg_eq$weight_strategy %||% "PROPORTIONAL"

  w_syn <- if (strategy == "EQUAL") {
    0.5
  } else if (strategy == "EXPLICIT") {
    val <- cfg_eq$synthetic_weight
    if (!is.null(val)) as.numeric(unlist(val)[1]) else NULL
  } else {
    NULL
  }

  raw_boot <- boot_reps
  boot_reps_safe <- if (!is.null(raw_boot)) as.integer(unlist(raw_boot)[1]) else 0

  run_unified <- function(type, method, needs_boot) {
    use_boot <- needs_boot
    # Aumentar reps para CircleArc si N es bajo para estabilizar SE
    n_reps <- if (use_boot) max(50, if (boot_reps_safe > 0) min(50, boot_reps_safe) else 30) else 0

    sanitize_ft <- function(ft) {
      if (inherits(ft, "freqtab")) {
        return(ft)
      }
      equate::as.freqtab(ft)
    }

    ft_x <- sanitize_ft(ft_src)
    ft_y <- sanitize_ft(ft_dest)

    if (sum(ft_x) == 0 || sum(ft_y) == 0) {
      return(NULL)
    }

    tryCatch(
      {
        res <- NULL
        if (!is.null(w_syn)) {
          res <- suppressMessages(equate::equate(x = ft_x, y = ft_y, type = type, method = method, boot = use_boot, reps = n_reps, w = w_syn, verbose = TRUE))
        } else {
          res <- suppressMessages(equate::equate(x = ft_x, y = ft_y, type = type, method = method, boot = use_boot, reps = n_reps, verbose = TRUE))
        }

        if (is.list(res)) {
          if (!is.null(res$concordance)) {
            return(res)
          }
        }
        if (is.atomic(res) && is.numeric(res)) {
          scale_vals <- as.numeric(names(ft_x))
          if (length(scale_vals) != length(res)) scale_vals <- 0:(length(res) - 1)
          fake_obj <- list(conc = data.frame(scale = scale_vals, yx = res, se = NA), x = ft_x, y = ft_y, type = type, method = method)
          return(fake_obj)
        }
        return(NULL)
      },
      error = function(e) {
        NULL
      }
    )
  }

  if ("TUCKER" %in% method_list) candidates$TUCKER <- run_unified("linear", "tucker", needs_boot = FALSE)
  if ("LEVINE" %in% method_list) candidates$LEVINE <- run_unified("linear", "levine", needs_boot = FALSE)
  if ("CIRCLEARC" %in% method_list) candidates$CIRCLEARC <- run_unified("circle-arc", "levine", needs_boot = TRUE)
  if ("EQUIPERCENTILE" %in% method_list) candidates$EQUIPERCENTILE <- run_unified("equipercentile", "frequency estimation", needs_boot = TRUE)

  for (m in names(candidates)) {
    obj <- candidates[[m]]
    if (is.list(obj)) {
      conc_name <- "concordance"
      if (is.null(obj[[conc_name]]) && !is.null(obj$conc)) conc_name <- "conc"
      if (!is.null(obj[[conc_name]])) {
        candidates[[m]][[conc_name]]$yx <- round(as.numeric(obj[[conc_name]]$yx), 4)
      } else {
        candidates[[m]] <- NULL
      }
    } else {
      candidates[[m]] <- NULL
    }
  }
  return(candidates)
}

# =============================================================================
# 4. SELECCIÓN DE MODELOS
# =============================================================================

select_best_model <- function(candidates, config, sd_ratio = 1.0, anchor_cor = 1.0,
                              link_tag = "UNKNOWN", dtm = NULL, sem_stats = NULL,
                              n_total = NULL) {
  candidates <- Filter(is.list, candidates)
  if (length(candidates) == 0) {
    return(NULL)
  }

  # 1. Preparación y Cálculo de Efecto (SMD)
  cut_points <- get_cut_scores(config)

  # Calculamos Effect Size (SMD) internamente para robustez
  smd_val <- 0
  first_cand <- candidates[[1]]
  if (!is.null(first_cand$x) && !is.null(first_cand$y)) {
    smd_val <- calculate_smd_internal(first_cand$x, first_cand$y)
  }

  pre_metrics <- compute_candidate_metrics(candidates, cut_points)

  # Elección Lineal Robusta (usando SMD y sd_ratio)
  best_linear_name <- identify_linear_candidate(pre_metrics, config, sd_ratio, anchor_cor, smd_val)

  # Construcción del Sintético si corresponde
  if (!is.null(best_linear_name) && "EQUIPERCENTILE" %in% names(candidates)) {
    syn_obj <- build_synthetic_model(candidates[[best_linear_name]], candidates$EQUIPERCENTILE)
    if (!is.null(syn_obj)) candidates$SYNTHETIC <- syn_obj
  }

  # 2. Definición Dinámica de la Referencia ("Truth") basada en N
  wiggle_max <- config$equating$selector_wiggle_max %||% 0.12
  n_safe <- if (is.null(n_total) || is.na(n_total)) 1000 else n_total

  ref_method <- "NONE"
  has_equi <- "EQUIPERCENTILE" %in% names(candidates)
  has_syn <- "SYNTHETIC" %in% names(candidates)
  has_circ <- "CIRCLEARC" %in% names(candidates)

  wig_equi <- if (has_equi) pre_metrics[pre_metrics$Method == "EQUIPERCENTILE", "Wiggliness"] else 999

  # Selección de la "Verdad"
  if (n_safe >= 1500 && has_equi && wig_equi <= wiggle_max) {
    ref_method <- "EQUIPERCENTILE"
  } else if (n_safe >= 400 && has_syn) {
    ref_method <- "SYNTHETIC"
  } else if (has_circ) {
    ref_method <- "CIRCLEARC"
  } else if (!is.null(best_linear_name)) {
    ref_method <- best_linear_name
  }

  # Cálculo de Métricas contra la Referencia
  metrics <- compute_candidate_metrics(candidates, cut_points)
  metrics$LINK <- link_tag
  metrics$RMSD_Ref <- NA
  metrics$Bias_Ref <- NA

  if (ref_method != "NONE") {
    ref_obj <- candidates[[ref_method]]
    ft_obj <- ref_obj$x
    w_raw <- if (length(dim(ft_obj)) >= 2) apply(ft_obj, 1, sum) else ft_obj
    w_dist <- as.numeric(w_raw)
    names(w_dist) <- names(w_raw)

    c_ref <- if (!is.null(ref_obj$conc)) ref_obj$conc else ref_obj$concordance
    if (!is.null(c_ref)) {
      for (m_name in names(candidates)) {
        m_obj <- candidates[[m_name]]
        c_cand <- if (!is.null(m_obj$conc)) m_obj$conc else m_obj$concordance
        if (!is.null(c_cand)) {
          metrics[metrics$Method == m_name, "RMSD_Ref"] <- calculate_weighted_rmsd(c_cand, c_ref, w_dist)
          metrics[metrics$Method == m_name, "Bias_Ref"] <- calculate_weighted_bias(c_cand, c_ref, w_dist)
        }
      }
      # Limpiar la referencia
      metrics[metrics$Method == ref_method, "RMSD_Ref"] <- 0.0
      metrics[metrics$Method == ref_method, "Bias_Ref"] <- 0.0
    }
  }

  if ("W_SEE" %in% names(metrics)) metrics$W_SEE[is.na(metrics$W_SEE)] <- Inf

  # 3. Arbitraje Jerárquico
  decision <- arbitrate_model_selection(
    best_linear_name, metrics, candidates, config, cut_points,
    dtm, sem_stats, n_total,
    sd_target = NA
  )

  construct_selection_output(decision$selected_name, decision$reason, metrics, candidates)
}

compute_candidate_metrics <- function(candidates, cut_points) {
  metrics <- data.frame(
    Method = names(candidates), W_SEE = NA_real_, Max_SEE = NA_real_,
    Mean_SEE = NA_real_, CSEE_Cuts = NA_real_, Wiggliness = NA_real_,
    RMSD_Ref = NA_real_, Bias_Ref = NA_real_, Selected = FALSE,
    Reason_Tag = "", stringsAsFactors = FALSE
  )

  for (m in names(candidates)) {
    obj <- candidates[[m]]
    if (is.null(obj)) next
    conc <- if (!is.null(obj$conc)) obj$conc else obj$concordance
    if (is.null(conc)) next

    se_vec <- extract_se_universal(obj, nrow(conc))
    freq <- as.numeric(obj$x)
    n <- sum(freq)

    if (all(is.na(se_vec)) || (all(se_vec == 0) && m != "IDENTITY")) {
      metrics[metrics$Method == m, "W_SEE"] <- Inf
    } else {
      w_see <- if (n > 0) sum(se_vec * (freq / n), na.rm = TRUE) else mean(se_vec, na.rm = TRUE)
      metrics[metrics$Method == m, "W_SEE"] <- w_see
      metrics[metrics$Method == m, "Max_SEE"] <- max(se_vec, na.rm = TRUE)
      metrics[metrics$Method == m, "Mean_SEE"] <- mean(se_vec, na.rm = TRUE)
      metrics[metrics$Method == m, "CSEE_Cuts"] <- calculate_critical_csee(obj, cut_points)
    }
    metrics[metrics$Method == m, "Wiggliness"] <- calculate_wiggliness_yx(conc$yx)
  }
  return(metrics)
}

identify_linear_candidate <- function(metrics, config, sd_ratio, anchor_cor, smd_val = 0) {
  linear_methods <- c("TUCKER", "LEVINE")
  rows <- metrics[metrics$Method %in% linear_methods, ]
  if (nrow(rows) == 0) {
    return(NULL)
  }

  cfg_eq <- config$equating %||% list()

  # Criterio 1: Correlación mínima
  raw_min_cor <- cfg_eq$min_anchor_cor
  min_cor <- if (!is.null(raw_min_cor)) as.numeric(unlist(raw_min_cor)[1]) else 0.70

  if (!is.na(anchor_cor) && anchor_cor < min_cor && "LEVINE" %in% rows$Method) {
    return("LEVINE")
  }

  # Criterio 2: Diferencia de Grupos (Effect Size)
  # Si los grupos difieren mucho (> 0.5 SD), Tucker se sesga. Preferir Levine.
  if (abs(smd_val) > 0.5 && "LEVINE" %in% rows$Method) {
    return("LEVINE")
  }

  # Criterio 3: Ratio de Varianzas (Homogeneidad)
  raw_tol <- cfg_eq$var_ratio_tolerance
  tol <- if (!is.null(raw_tol)) as.numeric(unlist(raw_tol)[1]) else 1.25
  is_homo <- !is.na(sd_ratio) && (sd_ratio >= (1 / tol) && sd_ratio <= tol)

  preferred <- if (is_homo) "TUCKER" else "LEVINE"

  if (preferred %in% rows$Method) {
    return(preferred)
  }
  return(rows$Method[which.min(rows$W_SEE)])
}

arbitrate_model_selection <- function(best_linear, metrics, candidates, config, cut_points,
                                      dtm = NULL, sem_stats = NULL, n_total = NULL, sd_target = NA) {
  cfg_eq <- config$equating %||% list()

  # --- Configuración Dinámica DTM ---
  raw_dtm_cfg <- cfg_eq$dtm_raw_score
  default_dtm <- if (!is.null(raw_dtm_cfg)) as.numeric(unlist(raw_dtm_cfg)[1]) else 0.5

  # Si tenemos estadísticas SEM, DTM no debe ser menor a medio SEM
  base_dtm <- if (!is.null(sem_stats) && !is.null(sem_stats$src$sem)) min(default_dtm, 0.5 * sem_stats$src$sem) else default_dtm

  n_safe <- if (is.null(n_total) || is.na(n_total)) 1000 else n_total

  # Definición de TIERS de muestra
  tier <- "MEDIUM"
  if (n_safe < 400) {
    tier <- "LOW"
  } else if (n_safe >= 1500) tier <- "HIGH"

  # Factor de penalización (inflate DTM para N bajo)
  n_penalty <- if (tier == "LOW") 1.5 else if (tier == "MEDIUM") 1.2 else 1.0
  final_dtm <- base_dtm * n_penalty

  wiggle_max <- if (!is.null(cfg_eq$selector_wiggle_max)) as.numeric(cfg_eq$selector_wiggle_max) else 0.12

  # Flags
  has_equi <- "EQUIPERCENTILE" %in% metrics$Method
  has_syn <- "SYNTHETIC" %in% metrics$Method
  has_circ <- "CIRCLEARC" %in% metrics$Method

  # Selección Preliminar por defecto
  prelim_selection <- list(name = best_linear, reason = "Linear parsimony (Default)")
  if (is.null(best_linear)) prelim_selection <- list(name = "NONE", reason = "No Linear Model")

  # Métricas del defensor lineal
  lin_rmsd <- 0
  if (!is.null(best_linear)) {
    val <- metrics[metrics$Method == best_linear, "RMSD_Ref"]
    if (!is.na(val)) lin_rmsd <- val
  }

  is_curved <- lin_rmsd > final_dtm

  # ---------------------------------------------------------
  # LÓGICA JERÁRQUICA POR TIER
  # ---------------------------------------------------------

  if (tier == "LOW") {
    # TIER LOW (< 400): Lineal es Rey.
    # Solo CircleArc puede desafiar si la mejora es MASIVA (> 25% de reducción RMSD).
    if (has_circ && is_curved) {
      circ_rmsd <- metrics[metrics$Method == "CIRCLEARC", "RMSD_Ref"]
      if (!is.na(circ_rmsd) && circ_rmsd < (lin_rmsd * 0.75)) {
        prelim_selection <- list(name = "CIRCLEARC", reason = "Strong Curvature (CircleArc in Low N)")
      }
    }
  } else if (tier == "MEDIUM") {
    # TIER MEDIUM (400-1500): Preferimos Sintético o CircleArc.
    if (is_curved) {
      if (has_syn) {
        prelim_selection <- list(name = "SYNTHETIC", reason = sprintf("Synthetic Compromise (RMSD=%.2f > DTM)", lin_rmsd))
      }
      # Circle Arc alternativo si es MUY bueno
      if (has_circ) {
        circ_rmsd <- metrics[metrics$Method == "CIRCLEARC", "RMSD_Ref"]
        syn_rmsd <- if (has_syn) metrics[metrics$Method == "SYNTHETIC", "RMSD_Ref"] else 999

        if (!is.na(circ_rmsd) && circ_rmsd < syn_rmsd && circ_rmsd < final_dtm) {
          prelim_selection <- list(name = "CIRCLEARC", reason = "Curvature Corrected (CircleArc fit)")
        }
      }
    }
  } else {
    # TIER HIGH (> 1500): Equipercentil permitido.
    if (is_curved && has_equi) {
      wig <- metrics[metrics$Method == "EQUIPERCENTILE", "Wiggliness"]
      if (!is.na(wig) && wig <= wiggle_max) {
        prelim_selection <- list(name = "EQUIPERCENTILE", reason = sprintf("Global Fit (High N, RMSD=%.2f)", lin_rmsd))
      } else if (has_syn) {
        prelim_selection <- list(name = "SYNTHETIC", reason = "Equi Wobbly -> Synthetic Fallback")
      }
    }
  }

  # Fallbacks Absolutos
  if (prelim_selection$name == "NONE" || is.null(prelim_selection$name)) {
    if (has_syn) {
      prelim_selection <- list(name = "SYNTHETIC", reason = "Fallback Available")
    } else if (has_equi) {
      prelim_selection <- list(name = "EQUIPERCENTILE", reason = "Fallback Available")
    } else if (has_circ) prelim_selection <- list(name = "CIRCLEARC", reason = "Fallback Available")
  }

  # ---------------------------------------------------------
  # 4. APLICACIÓN DE GUARDABARRERAS (GATEKEEPERS)
  # ---------------------------------------------------------

  gatekeeper_on <- isTRUE(cfg_eq$gatekeeper_enabled)
  selected_name <- prelim_selection$name

  if (gatekeeper_on && !is.null(selected_name) && selected_name != best_linear && !is.null(best_linear)) {
    cand_obj <- candidates[[selected_name]]
    lin_obj <- candidates[[best_linear]]

    # Inferencia de SD Target si no se proveyó
    eff_sd <- if (!is.na(sd_target)) sd_target else sd(lin_obj$y, na.rm = TRUE)

    gk_result <- check_gatekeeper(cand_obj, lin_obj, cut_points, eff_sd, config)

    if (!gk_result$pass) {
      # FALLÓ EL GATEKEEPER -> Estrategia de recuperación
      if (selected_name == "EQUIPERCENTILE" && has_syn) {
        # Intentar Sintético
        syn_obj <- candidates[["SYNTHETIC"]]
        gk_result_syn <- check_gatekeeper(syn_obj, lin_obj, cut_points, eff_sd, config)
        if (gk_result_syn$pass) {
          return(list(selected_name = "SYNTHETIC", reason = paste(prelim_selection$reason, "-> GK Veto -> Synthetic OK")))
        }
      }
      # Forzar Lineal
      return(list(selected_name = best_linear, reason = paste(prelim_selection$reason, "-> VETOED by Gatekeeper:", gk_result$reason)))
    }
  }

  return(list(selected_name = prelim_selection$name, reason = prelim_selection$reason))
}

construct_selection_output <- function(selected_name, reason, metrics, candidates) {
  if (!is.null(selected_name)) {
    metrics[metrics$Method == selected_name, "Selected"] <- TRUE
    metrics[metrics$Method == selected_name, "Reason_Tag"] <- reason
  }

  obj <- if (!is.null(selected_name)) candidates[[selected_name]] else NULL

  params <- list(
    slope = NA_real_,
    intercept = NA_real_,
    gamma = NA_real_, # Peso Lineal (0=Equi, 1=Lin)
    poly_degree = NA_integer_, # 1=Linear, NA=Others
    model_family = "UNKNOWN"
  )

  if (!is.null(obj)) {
    # 1. Slope/Intercept (Referencia Lineal Aproximada)
    conc <- if (!is.null(obj$conc)) obj$conc else obj$concordance
    if (!is.null(conc) && nrow(conc) > 1) {
      fit <- lm(yx ~ scale, data = conc)
      params$slope <- as.numeric(coef(fit)[2])
      params$intercept <- as.numeric(coef(fit)[1])
    }

    # 2. Parámetros Específicos (Gamma, Poly Degree)
    if (selected_name == "SYNTHETIC") {
      params$model_family <- "SYNTHETIC"
      if (!is.null(obj$meta) && !is.null(obj$meta$weights)) {
        w_raw <- obj$meta$weights
        params$gamma <- mean(w_raw, na.rm = TRUE)
      }
    } else if (selected_name == "EQUIPERCENTILE") {
      params$model_family <- "EQUIPERCENTILE"
      params$gamma <- 0.0
    } else if (selected_name %in% c("TUCKER", "LEVINE", "MEAN", "LINEAR")) {
      params$model_family <- "LINEAR"
      params$gamma <- 1.0
      params$poly_degree <- 1L
    } else if (selected_name == "CIRCLEARC") {
      params$model_family <- "CIRCLEARC"
      params$gamma <- 0.0
    }
  }

  list(
    selected_name = selected_name,
    selected_obj = obj,
    decision_reason = paste0(selected_name, ": ", reason),
    metrics_table = metrics,
    model_params = params
  )
}
