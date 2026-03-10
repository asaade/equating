# R/00_config_defs.R
# Revisado por Psicómetra Senior

get_default_config <- function() {
  list(
    # -----------------------------------------------------------------------------
    # 0. ORQUESTACIÓN Y SISTEMA
    # -----------------------------------------------------------------------------
    system = list(
      use_parallel = FALSE,
      n_cores = 6,
      reference_form = NULL
    ),

    sampling = list(
      enabled = FALSE,
      seed = 12345,
      size_per_form = 1000
    ),

    ### Análisis CTT
    thresholds = list(
      ctt_p_val_min = 0.05,
      ctt_p_val_max = 0.95,
      ctt_pbis_min = 0.15,
      ctt_distractor_max_pbis = 0.05,
      ctt_alpha = 0.7,
      variance_explained = 0.2
    ),

    ### Análisis IRT
    mirt = list(
      model = "3PL",
      method = "EM",
      cycles = 2000,
      quadpts = 61,
      tol = 0.0001,
      metric_constant = 1.0,
      drift_analysis = TRUE,
      drift_threshold = 0.3,
      diagnostics = list(
        compute_m2 = FALSE,
        compute_q3 = TRUE
      ),
      constraints = list(
        fix_guessing = TRUE,
        guessing_value = 0.2
      )
    ),

    scoring = list(
      treat_na_as_zero = TRUE,
      include_subscores = FALSE,
      irt_max_theta = 4.0
    ),

    dif = list(
      enabled = FALSE,
      group_col = "SEXO",
      reference_group = "H",
      focal_group = "M"
    ),

    # =============================================================================
    # 1. INTEGRIDAD DEL SISTEMA (System Limits)
    # =============================================================================
    limits = list(
      critical_min_n = 150,
      fail_fast = TRUE,
      numeric_tolerance = 1e-5
    ),

    # =============================================================================
    # 2. GESTIÓN DE ANCLAS (Anchor Management)
    # =============================================================================
    anchor = list(
      min_p = 0.2,
      max_p = 0.9,
      min_sd = 0.15,
      min_keep = 10,

      # Drift (Robust Regression)
      rlm_min_n = 50,
      max_iter = 5,
      rlm_maxit = 20,
      mad_constant = 1.4826,
      epsilon = 1e-4,
      min_items_cor = 10,
      strict_z = 2.5,
      z_threshold = 3.0,
      logit_clamp_min = 0.001,
      logit_clamp_max = 0.999,
      min_common_items_topology = 10,

      # Validación Global
      min_cor = 0.70,
      anchor_correlation = 0.70
      # Si es < 0.70, es "fatal error". Entre 0.70-0.80 permite Lineal.
    ),

    # =============================================================================
    # 3. MOTOR DE SUAVIZADO
    # =============================================================================
    smoothing = list(
      max_iter = 200,
      ic_tolerance = 2.0,
      ic_criterion = "AIC",
      wiggle_density_max = 0.1,
      max_skew_diff = 0.5,
      max_kurt_diff = 1.0,
      corr_drop_max = 0.05,
      bias_mean_max = 0.05,
      bias_sd_max = 0.10,
      bias_sd_pct_max = 0.02,
      bias_skew_max = 0.20,
      bias_kurt_max = 0.40,
      cv_folds = 5
    ),

    # =============================================================================
    # 4. SELECTOR DE MODELOS DE EQUIPARACIÓN (Model Selector Logic)
    # =============================================================================
    equating = list(
      # Fase 1: Lineal
      min_anchor_cor = 0.70,
      preferred_anchor_cor = 0.85,
      preferred_anchor_items = 20,
      min_anchor_items = 12,
      var_ratio_tolerance = 1.25,
      see_min_limit = 0.001,
      force_linear_models = FALSE,
      hop_penalty = 0.25,
      max_degree_override = NULL,

      # Fase 2: Estrategia
      weight_strategy = "PROPORTIONAL",
      boot_reps = 300,
      gatekeeper_enabled = TRUE,
      gatekeeper_see_factor = 0.12,
      allow_identity_fallback = TRUE,

      # Umbrales Retador
      dtm_raw_score = 0.4,
      selector_csee_ratio_max = 1.5,
      selector_tail_limit = 4.0,
      selector_wiggle_max = 0.12,
      selector_efficiency_threshold = 0.8,
      rmsd_improvement_threshold = 0.9,
      chain_vif_penalty = 0.10,
      tre_threshold = 0.50,
      cv_folds = 5
    ),

    # =============================================================================
    # 5. CONTROL DE CALIDAD (QC)
    # =============================================================================
    qc = list(
      pava_max_adj = 0.5,
      pava_stress_max = 0.1,
      max_divergence_pct = 0.15,
      sparsity_max = 0.40
    ),

    flags = list(
      min_resp_person = 10
    ),

    cleaning = list(
      max_na_pct = 0.40,
      remove_flat = TRUE,
      max_invariant = 15
    )
  )
}
