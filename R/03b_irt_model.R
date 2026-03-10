# Responsabilidad: Calibración IRT Unidimensional, Manejo de Métricas (D), Scoring y Diagnósticos Avanzados.
# VERSIÓN: v3.7 (Robust Fit: Eliminado S_X2, Estandarización de Infit/Outfit Manual)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(mirt, dplyr, igraph, checkmate, log4r, tidyr, Matrix, stats)

# ==============================================================================
# SECCIÓN 1: UTILIDADES DE MÉTRICA Y TRANSFORMACIÓN
# ==============================================================================

#' Transforma parámetros de métrica clásica a parametrización mirt
transform_params_to_mirt <- function(params_df, D) {
  if (missing(D) || is.null(D) || !is.numeric(D)) {
    stop("La constante de escalamiento D debe ser proporcionada y numérica.")
  }

  if (is.null(params_df) || nrow(params_df) == 0) {
    return(NULL)
  }

  if (!all(c("a", "b") %in% names(params_df))) {
    warning("El dataframe de parámetros históricos no tiene columnas 'a' y 'b'.")
    return(NULL)
  }

  params_clean <- params_df |>
    dplyr::mutate(
      a = suppressWarnings(as.numeric(a)),
      b = suppressWarnings(as.numeric(b)),
      c = if ("c" %in% names(.)) suppressWarnings(as.numeric(c)) else 0
    ) |>
    dplyr::filter(!is.na(a) & !is.na(b))

  # Transformación: Classic (a,b,c) -> Mirt (a1, d, g)
  params_clean |>
    dplyr::mutate(
      a1 = a * D,
      d  = -1 * a1 * b,
      g  = c
    )
}

extract_and_transform_params <- function(mod_obj, D) {
  if (missing(D) || is.null(D) || !is.numeric(D)) {
    stop("La constante de escalamiento D es obligatoria.")
  }

  raw_coefs <- coef(mod_obj, IRTpars = FALSE, simplify = TRUE)$items
  df_coefs <- as.data.frame(raw_coefs)
  df_coefs$ITEM <- rownames(df_coefs)

  coefs_names <- names(df_coefs)

  df_out <- df_coefs |>
    dplyr::mutate(
      IRT_a = a1 / D,
      IRT_b = -d / a1,
      IRT_c = if ("g" %in% coefs_names) g else 0
    ) |>
    dplyr::select(ITEM, IRT_a, IRT_b, IRT_c)

  df_out
}

# ==============================================================================
# SECCIÓN 2: PREPARACIÓN DE DATOS
# ==============================================================================

prepare_irt_data <- function(scored_df, config) {
  debug(sprintf("Preparando matriz IRT inicial para %d casos...", nrow(scored_df)))

  base_cols <- setdiff(names(scored_df), c("ID", "FORMA", "Raw_Total", "Raw_Global", "Score_Final", "Nivel", "Theta", "SE_Theta", "Zh"))
  valid_cols <- base_cols[colSums(!is.na(scored_df[, base_cols, drop = FALSE])) > 0]

  mat_out <- scored_df[, valid_cols, drop = FALSE]

  min_resp <- config$flags$min_resp_person %||% 0
  if (min_resp > 0) {
    resp_counts <- rowSums(!is.na(mat_out))
    mat_out <- mat_out[resp_counts >= min_resp, , drop = FALSE]
  }

  mat_out <- mat_out |> mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))
  as.data.frame(mat_out)
}

# ==============================================================================
# SECCIÓN 3.1: VALIDACIÓN DE ANCLAJE (DRIFT CON ALINEACIÓN)
# ==============================================================================

validate_anchor_drift <- function(mod_free, historical_params, threshold = 0.3) {
  if (is.null(historical_params) || nrow(historical_params) == 0) {
    return(NULL)
  }

  req_cols <- c("a", "b", "ITEM")
  missing_cols <- setdiff(req_cols, names(historical_params))

  if (length(missing_cols) > 0) {
    warn(paste("Imposible validar Drift. Faltan columnas en históricos:", paste(missing_cols, collapse = ", ")))
    return(NULL)
  }

  current_pars <- coef(mod_free, IRTpars = TRUE, simplify = TRUE)$items |>
    as.data.frame() |>
    tibble::rownames_to_column("ITEM") |>
    dplyr::select(ITEM, a_est = a, b_est = b)

  hist_clean <- historical_params |>
    dplyr::mutate(
      a_hist = suppressWarnings(as.numeric(a)),
      b_hist = suppressWarnings(as.numeric(b))
    ) |>
    dplyr::select(ITEM, a_hist, b_hist) |>
    dplyr::filter(!is.na(a_hist) & !is.na(b_hist))

  common <- current_pars |>
    dplyr::inner_join(hist_clean, by = "ITEM")

  if (nrow(common) < 3) {
    warn("Menos de 3 ítems comunes. No se puede realizar alineación de métrica para Drift.")
    return(NULL)
  }

  # Alineación Mean-Mean
  mean_diff_b <- mean(common$b_est, na.rm = TRUE) - mean(common$b_hist, na.rm = TRUE)

  comparison <- common |>
    dplyr::mutate(
      b_est_aligned = b_est - mean_diff_b,
      diff_b_aligned = b_est_aligned - b_hist,
      drift_flag = abs(diff_b_aligned) > threshold
    ) |>
    dplyr::arrange(desc(abs(diff_b_aligned)))

  problem_items <- comparison |> dplyr::filter(drift_flag == TRUE)

  list(
    full_comparison = comparison,
    flagged_items = problem_items,
    metrics = list(
      n_anchors = nrow(comparison),
      n_flagged = nrow(problem_items),
      shift_constant = mean_diff_b,
      cor_b = cor(comparison$b_est, comparison$b_hist, use = "complete.obs")
    )
  )
}

# ==============================================================================
# SECCIÓN 3: CALIBRACIÓN PRINCIPAL
# ==============================================================================

run_irt_calibration <- function(irt_data, config, historical_params = NULL) {
  CONST_D <- config$mirt$metric_constant %||% 1.0
  model_type <- config$mirt$model %||% "2PL"
  n_cycles <- config$mirt$cycles %||% 2000
  tol <- config$mirt$tol %||% 0.0001
  quadpts <- config$mirt$quadpts %||% 61
  method <- config$mirt$method %||% "EM"
  drift_threshold <- config$mirt$drift_threshold %||% 0.3

  run_drift_check <- isTRUE(config$mirt$drift_analysis) || isTRUE(config$mirt$drift_analisis)

  debug(sprintf("Iniciando Proceso IRT [Modelo: %s | D=%.4f]", model_type, CONST_D))

  item_type_mirt <- switch(model_type,
    "Rasch" = "Rasch",
    "2PL" = "2PL",
    "3PL" = "3PL",
    "2PL"
  )

  mirt_model_obj <- 1
  tech_list <- list(NCYCLES = n_cycles, message = FALSE, warn = FALSE)

  # --- PASO A: DRIFT CHECK ---
  anchor_validation <- NULL
  if (isTRUE(config$historical_anchoring$enabled) && !is.null(historical_params) && run_drift_check) {
    debug("  > Drift Check: Ejecutando calibración libre...")
    mod_free <- tryCatch(
      {
        mirt(irt_data,
          model = mirt_model_obj, itemtype = item_type_mirt,
          method = method, TOL = 0.005, technical = tech_list, SE = FALSE, verbose = FALSE
        )
      },
      error = function(e) {
        warn(e$message)
        return(NULL)
      }
    )

    if (!is.null(mod_free)) {
      anchor_validation <- validate_anchor_drift(mod_free, historical_params, drift_threshold)
      if (!is.null(anchor_validation)) {
        debug(sprintf(
          "  > Alineación completada. Shift: %.3f. Drift en %d ítems.",
          anchor_validation$metrics$shift_constant, anchor_validation$metrics$n_flagged
        ))
      }
    }
  }

  # --- PASO B: CONFIGURACIÓN DE PARÁMETROS ---
  pre_mod_pars <- mirt(irt_data, mirt_model_obj, itemtype = item_type_mirt, pars = "values", technical = tech_list)

  if (isTRUE(config$mirt$constraints$fix_guessing)) {
    guess_val <- config$mirt$constraints$guessing_value %||% 0.2
    idx_g <- which(pre_mod_pars$name == "g")
    if (length(idx_g) > 0) {
      pre_mod_pars[idx_g, "value"] <- guess_val
      pre_mod_pars[idx_g, "est"] <- FALSE
    }
  }

  if (isTRUE(config$historical_anchoring$enabled) && !is.null(historical_params)) {
    hist_items_vec <- if ("ITEM" %in% names(historical_params)) historical_params$ITEM else rownames(historical_params)
    common_items <- intersect(pre_mod_pars$item, hist_items_vec)

    if (length(common_items) > 0) {
      hist_mirt <- transform_params_to_mirt(historical_params |> dplyr::filter(ITEM %in% common_items), CONST_D)
      if (!is.null(hist_mirt)) {
        # Inyección Vectorizada de a1, d, g
        hist_long <- hist_mirt |>
          dplyr::select(item = ITEM, dplyr::any_of(c("a1", "d", "g"))) |>
          tidyr::pivot_longer(cols = dplyr::any_of(c("a1", "d", "g")), names_to = "name", values_to = "hist_val") |>
          dplyr::filter(!is.na(hist_val))

        pre_mod_pars <- pre_mod_pars |>
          dplyr::left_join(hist_long, by = c("item", "name")) |>
          dplyr::mutate(
            value = ifelse(!is.na(hist_val), hist_val, value),
            est = ifelse(!is.na(hist_val), FALSE, est)
          ) |>
          dplyr::select(-hist_val)
      }
    }
  }

  # --- PASO C: ESTIMACIÓN FINAL ---
  mod <- tryCatch(
    {
      mirt(irt_data,
        model = mirt_model_obj, itemtype = item_type_mirt, pars = pre_mod_pars,
        method = method, TOL = tol, quadpts = quadpts, technical = tech_list, SE = TRUE
      )
    },
    error = function(e) {
      fatal(e$message)
      return(NULL)
    }
  )

  if (is.null(mod)) stop("Fallo en calibración final.")

  # --- PASO D: RESULTADOS ---
  params_export <- tryCatch(extract_and_transform_params(mod, CONST_D), error = function(e) NULL)
  diagnostics <- tryCatch(generate_irt_diagnostics(mod, config), error = function(e) NULL)

  list(
    model_obj = mod,
    parameters = params_export,
    diagnostics = diagnostics,
    anchor_validation = anchor_validation,
    D_used = CONST_D
  )
}

# ==============================================================================
# SECCIÓN 4: SCORING (EAP)
# ==============================================================================

generate_irt_scores <- function(mod, irt_matrix, source_df, config) {
  debug("Calculando puntajes IRT (EAP)...")

  model_cols <- colnames(mod@Data$data)
  irt_matrix_ordered <- irt_matrix[, intersect(model_cols, colnames(irt_matrix)), drop = FALSE]

  missing <- setdiff(model_cols, colnames(irt_matrix_ordered))
  if (length(missing) > 0) {
    fill <- matrix(NA, nrow = nrow(irt_matrix_ordered), ncol = length(missing))
    colnames(fill) <- missing
    irt_matrix_ordered <- cbind(irt_matrix_ordered, as.data.frame(fill))
  }
  irt_matrix_numeric <- data.matrix(irt_matrix_ordered[, model_cols])

  scores <- mirt::fscores(mod,
    response.pattern = irt_matrix_numeric, method = "EAP",
    full.scores = TRUE, full.scores.SE = TRUE, verbose = FALSE
  )

  theta_vals <- scores[, 1]
  se_vals <- scores[, 2]

  # Reliabilidad Marginal
  var_theta <- var(theta_vals, na.rm = TRUE)
  mean_sem_sq <- mean(se_vals^2, na.rm = TRUE)
  marginal_rel <- (var_theta - mean_sem_sq) / var_theta

  # Zh Calculation
  n_people <- nrow(irt_matrix_numeric)
  L_obs <- numeric(n_people)
  E_L <- numeric(n_people)
  Var_L <- numeric(n_people)

  mod_items <- mirt::extract.mirt(mod, 'pars')
  theta_matrix <- as.matrix(theta_vals)

  for (i in seq_len(ncol(irt_matrix_numeric))) {
    resp <- irt_matrix_numeric[, i]
    valid_idx <- which(!is.na(resp))
    if (length(valid_idx) == 0) next

    item_obj <- mod_items[[i]]
    theta_sub <- theta_matrix[valid_idx, , drop = FALSE]
    P <- mirt::probtrace(item_obj, theta_sub)
    P <- pmax(P, 1e-16)
    log_P <- log(P)

    term1 <- rowSums(P * log_P)
    term2 <- rowSums(P * (log_P^2))

    E_L[valid_idx] <- E_L[valid_idx] + term1
    Var_L[valid_idx] <- Var_L[valid_idx] + (term2 - term1^2)

    min_val <- if (.hasSlot(item_obj, "min")) item_obj@min else 0
    col_indices <- resp[valid_idx] - min_val + 1
    col_indices <- pmin(pmax(col_indices, 1), ncol(P))
    L_obs[valid_idx] <- L_obs[valid_idx] + log_P[cbind(seq_along(valid_idx), col_indices)]
  }

  sd_L <- sqrt(pmax(1e-9, Var_L))
  zh_vals <- (L_obs - E_L) / sd_L
  zh_vals <- pmax(-5, pmin(5, zh_vals))

  res_df <- data.frame(
    ID = source_df$ID[1:length(theta_vals)],
    Theta = round(theta_vals, 4),
    SE_Theta = round(se_vals, 4),
    Zh = round(zh_vals, 2),
    stringsAsFactors = FALSE
  )

  attr(res_df, "marginal_reliability") <- round(marginal_rel, 4)
  return(res_df)
}

# ==============================================================================
# SECCIÓN 5: DIAGNÓSTICOS AVANZADOS (FIT INDICES & RESIDUALS)
# ==============================================================================

.extract_item_fit_stats <- function(mod, items, data_mat, Ks, theta_mat, P_all) {
  item_fit_list <- list()
  col_idx <- 1

  for (i in seq_along(items)) {
    obs <- data_mat[, i]
    idx <- !is.na(obs)
    K <- Ks[i]

    # Si hay muy pocos datos para este ítem, saltar
    if (sum(idx) < 10) {
      col_idx <- col_idx + K
      next
    }

    P <- P_all[idx, col_idx:(col_idx + K - 1), drop = FALSE]
    col_idx <- col_idx + K
    vals <- 0:(K - 1)

    # Expected Value
    E <- as.vector(P %*% vals)
    obs_vals <- obs[idx]

    # Variance (W)
    W <- numeric(length(E))
    for (k in 1:K) W <- W + P[, k] * (vals[k] - E)^2

    # Residuals
    res <- obs_vals - E
    # Standardized Residuals Squared (Outfit component)
    std_res_sq <- (res^2) / pmax(W, 1e-6)

    item_fit_list[[i]] <- data.frame(
      ITEM = items[i],
      RMSD = round(sqrt(mean(res^2)), 4),
      Infit_MSQ = round(sum(res^2) / sum(W), 3),
      Outfit_MSQ = round(mean(std_res_sq), 3)
    )
  }
  return(do.call(rbind, item_fit_list))
}

.extract_information_criteria <- function(mod) {
  data.frame(
    Index = c("LogLik", "AIC", "BIC", "SABIC"),
    Value = c(mod@Fit$logLik, mod@Fit$AIC, mod@Fit$BIC, mod@Fit$SABIC)
  )
}

.extract_local_independence <- function(mod, data_mat, Ks, P_all, config) {
  local_dep <- NULL
  global_rmsr_proxy <- NA

  if (isTRUE(config$mirt$diagnostics$compute_q3 %||% TRUE)) {
    debug("  > Calculando Q3 de Yen (Independencia Local)...")
    tryCatch(
      {
        # Recalculamos residuos (raw) para la matriz completa
        n_items <- ncol(data_mat)
        n_persons <- nrow(data_mat)
        residuals_mat <- matrix(NA, nrow = n_persons, ncol = n_items)
        col_idx <- 1

        for (i in 1:n_items) {
          K <- Ks[i]
          P <- P_all[, col_idx:(col_idx + K - 1), drop = FALSE]
          col_idx <- col_idx + K
          vals <- 0:(K - 1)
          E <- P %*% vals
          obs <- data_mat[, i]

          # Solo calculamos residuo donde hay dato
          valid_idx <- !is.na(obs)
          if (any(valid_idx)) {
            residuals_mat[valid_idx, i] <- obs[valid_idx] - E[valid_idx]
          }
        }
        colnames(residuals_mat) <- colnames(data_mat)

        cor_res <- cor(residuals_mat, use = "pairwise.complete.obs")

        # A. Pairs Check
        cor_res_diag <- cor_res
        cor_res_diag[lower.tri(cor_res_diag, diag = TRUE)] <- NA
        bad_pairs <- which(abs(cor_res_diag) > 0.2, arr.ind = TRUE)

        if (nrow(bad_pairs) > 0) {
          local_dep <- data.frame(
            Item_1 = rownames(cor_res_diag)[bad_pairs[, 1]],
            Item_2 = colnames(cor_res_diag)[bad_pairs[, 2]],
            Q3 = round(cor_res_diag[bad_pairs], 3)
          ) |> dplyr::arrange(desc(abs(Q3)))
        } else {
          local_dep <- data.frame(Message = "No Q3 > 0.2 detected")
        }

        # B. Global RMSR Proxy
        off_diag <- cor_res[upper.tri(cor_res)]
        off_diag <- na.omit(off_diag)
        if (length(off_diag) > 0) {
          global_rmsr_proxy <- sqrt(mean(off_diag^2))
        }
      },
      error = function(e) {
        warn(paste("Fallo Q3:", e$message))
        NULL
      }
    )
  }
  return(list(local_dep = local_dep, global_rmsr_proxy = global_rmsr_proxy))
}

.extract_global_fit <- function(mod, global_rmsr_proxy, config) {
  global_fit <- NULL
  if (isTRUE(config$mirt$diagnostics$compute_m2 %||% FALSE)) {
    debug("  > Calculando M2 Statistic (Global Fit)...")
    global_fit <- tryCatch(
      {
        res_m2 <- mirt::M2(mod, na.rm = TRUE)
        res_m2$SRMSR_Proxy_Q3 <- round(global_rmsr_proxy, 4)
        res_m2
      },
      error = function(e) {
        warn("M2 statistic falló (Sparsity). Reportando SRMSR Proxy.")
        data.frame(
          M2 = NA, df = NA, p = NA, RMSEA = NA,
          SRMSR_Proxy_Q3 = round(global_rmsr_proxy, 4),
          Note = "M2 Failed. Use SRMSR Proxy."
        )
      }
    )
  } else {
    if (!is.na(global_rmsr_proxy)) {
      global_fit <- data.frame(Metric = "SRMSR_Proxy_Q3", Value = round(global_rmsr_proxy, 4))
    }
  }
  return(global_fit)
}

.extract_tif_csem <- function(mod) {
  theta_grid <- matrix(seq(-3, 3, by = 0.5), ncol = 1)
  info_vals <- mirt::testdebug(mod, theta_grid)
  csem_vals <- 1 / sqrt(info_vals)

  data.frame(
    Theta = theta_grid,
    Information = round(info_vals, 3),
    CSEM = round(csem_vals, 3)
  )
}

.extract_explained_variance <- function(mod) {
  varexp_df <- NULL
  tryCatch(
    {
      summ <- summary(mod, verbose = FALSE)
      if ("rotF" %in% names(summ)) {
        loads <- summ$rotF
        ss_load <- sum(loads^2)
        varexp_df <- data.frame(
          Metric = "Total_Variance_Explained",
          SS_Loadings = round(ss_load, 2),
          Prop_Var = round(ss_load / nrow(loads), 3)
        )
      }
    },
    error = function(e) NULL
  )
  return(varexp_df)
}

.extract_dimensionality_stats <- function(mod) {
  cor_mat <- suppressWarnings(cor(mod@Data$data, use = "pairwise.complete.obs"))
  cor_mat[is.na(cor_mat)] <- 0
  evals <- eigen(cor_mat, only.values = TRUE)$values
  if (length(evals) >= 2) {
    data.frame(Metric = c("Eigen_1", "Eigen_2", "Ratio_1_2"), Value = c(evals[1], evals[2], evals[1] / evals[2]))
  } else {
    NULL
  }
}

generate_irt_diagnostics <- function(mod, config) {
  debug("Generando diagnósticos IRT extendidos...")

  # Precompute essential shared objects
  scores <- mirt::fscores(mod, method = "EAP", full.scores = TRUE, verbose = FALSE)
  theta_mat <- matrix(scores[, 1], ncol = 1)
  data_mat <- mod@Data$data
  items <- colnames(data_mat)
  Ks <- mod@Data$K
  P_all <- mirt::probtrace(mod, theta_mat)

  # 1. Tabla de Ajuste de Ítems (Fit Statistics)
  fit_df <- .extract_item_fit_stats(mod, items, data_mat, Ks, theta_mat, P_all)

  # 2. Criterios de Información (AIC, BIC, SABIC)
  fit_indices <- .extract_information_criteria(mod)

  # 3. Independencia Local (Q3) y Global RMSR Proxy
  local_results <- .extract_local_independence(mod, data_mat, Ks, P_all, config)
  local_dep <- local_results$local_dep
  global_rmsr_proxy <- local_results$global_rmsr_proxy

  # 4. Bondad de Ajuste Global (M2) con Fallback a SRMSR Proxy
  global_fit <- .extract_global_fit(mod, global_rmsr_proxy, config)

  # 5. TIF & CSEM
  tif_df <- .extract_tif_csem(mod)

  # 6. Varianza Explicada
  varexp_df <- .extract_explained_variance(mod)

  # 7. Dimensionality
  dim_df <- .extract_dimensionality_stats(mod)

  list(
    fit_statistics = fit_df,
    fit_indices = fit_indices,
    local_independence = local_dep,
    global_fit = global_fit,
    variance_explained = varexp_df,
    test_information = tif_df,
    dimensionality = dim_df
  )
}
