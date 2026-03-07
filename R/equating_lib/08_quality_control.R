# R/equating_lib/08_quality_control.R
# Responsabilidad: Auditoría forense post-equiparación (Hard Constraints).
# Refactor: v1.0
# Dependencias: 09_ux_helpers.R, 00_config_defs.R

# Dependencias internas protegidas
if (exists("is_safe_r_path") && !is_safe_r_path("R/equating_lib/09_ux_helpers.R")) {
  stop("Fallo de seguridad: Intento de cargar librería desde ruta no segura (R/equating_lib/09_ux_helpers.R)")
}
source("R/equating_lib/09_ux_helpers.R")

if (exists("is_safe_r_path") && !is_safe_r_path("R/00_config_defs.R")) {
  stop("Fallo de seguridad: Intento de cargar librería desde ruta no segura (R/00_config_defs.R)")
}
source("R/00_config_defs.R")

# -----------------------------------------------------------------------------
# 1. VERIFICACIONES ESTRUCTURALES (Hard Constraints)
# -----------------------------------------------------------------------------

#' @title Auditoría Estricta de Monotonicidad (Isotonic Regression)
audit_monotonicity <- function(yx, se) {
  if (is.null(yx) || length(yx) < 2) {
    return(list(passed = FALSE, critical = FALSE, n_violations = 0, corrected_yx = yx, note = "Empty Data"))
  }

  violations_idx <- which(diff(yx) < 0)
  n_violations <- length(violations_idx)

  if (n_violations == 0) {
    return(list(passed = TRUE, critical = FALSE, n_violations = 0, max_mag = 0, corrected_yx = yx, note = "Mono:OK"))
  }

  iso_fit <- stats::isoreg(yx)
  yx_corrected <- iso_fit$yf
  adjustments <- abs(yx - yx_corrected)
  max_adj <- max(adjustments)
  stress_val <- sqrt(mean(adjustments^2))

  # Uso de Constantes con niveles de severidad
  limit_adj_warn <- 0.10
  limit_adj_fail <- if (exists("CONST_QC_PAVA_MAX_ADJ")) CONST_QC_PAVA_MAX_ADJ else 0.50

  is_minor_fix <- (max_adj <= limit_adj_warn)
  is_major_fix <- (max_adj > limit_adj_warn && max_adj <= limit_adj_fail)
  is_critical <- (max_adj > limit_adj_fail)

  note_msg <- sprintf("PAVA(Viol:%d|MaxAdj:%.4f)", n_violations, max_adj)

  if (is_critical) {
    note_msg <- paste(note_msg, "[CRITICAL FAIL]")
  } else if (is_major_fix) {
    note_msg <- paste(note_msg, "[WARN FIXED]")
  } else {
    note_msg <- paste(note_msg, "[MINOR NOISE]")
  }

  list(
    passed = !is_critical,
    critical = is_critical,
    n_violations = n_violations,
    max_mag = max_adj,
    stress = stress_val,
    corrected_yx = yx_corrected,
    note = note_msg
  )
}

#' @title Auditoría de Rango Físico
audit_score_range <- function(yx, max_theoretical) {
  if (is.null(yx)) {
    return(list(passed = FALSE, range_obs = c(NA, NA), note = "No Data"))
  }
  min_obs <- min(yx, na.rm = TRUE)
  max_obs <- max(yx, na.rm = TRUE)
  out_of_bounds <- (min_obs < -0.01) || (max_obs > (max_theoretical + 0.01))

  note <- "Range:OK"
  if (out_of_bounds) {
    note <- sprintf("Range:CLAMPED(Min:%.2f|Max:%.2f)", min_obs, max_obs)
  }

  list(passed = !out_of_bounds, range_obs = c(min_obs, max_obs), note = note)
}

# -----------------------------------------------------------------------------
# 2. ANÁLISIS FORENSE (Soft Constraints)
# -----------------------------------------------------------------------------

#' @title Auditoría de Divergencia de Identidad
audit_identity_divergence <- function(concordance_df, config = NULL) {
  if (is.null(concordance_df) || is.null(concordance_df$scale)) {
    return(list(risk_level = "UNKNOWN", note = ""))
  }

  diffs <- concordance_df$yx - concordance_df$scale
  max_abs_diff <- max(abs(diffs), na.rm = TRUE)
  scale_len <- max(concordance_df$scale, na.rm = TRUE)

  threshold_pct <- if (exists("CONST_QC_MAX_DIVERGENCE_PCT")) CONST_QC_MAX_DIVERGENCE_PCT else 0.15
  if (!is.null(config$qc$max_divergence)) threshold_pct <- config$qc$max_divergence

  limit_pts <- scale_len * threshold_pct
  risk_flag <- (max_abs_diff > limit_pts)

  note <- sprintf("Div(MaxShift:%.2f)", max_abs_diff)
  if (risk_flag) note <- paste(note, "[HIGH RISK]")

  list(max_change = round(max_abs_diff, 2), risk_level = if (risk_flag) "HIGH" else "NORMAL", note = note)
}

# -----------------------------------------------------------------------------
# 3. WRAPPER PRINCIPAL
# -----------------------------------------------------------------------------

run_quality_control <- function(eq_result, max_score, config = NULL) {
  if (is.null(eq_result) || is.null(eq_result$concordance)) {
    return(NULL)
  }

  conc <- eq_result$concordance
  se_vec <- if (!is.null(eq_result$se)) eq_result$se else rep(0, nrow(conc))

  # 1. Monotonicidad Estricta (PAVA)
  mono_check <- audit_monotonicity(conc$yx, se_vec)

  final_yx <- mono_check$corrected_yx
  final_conc <- conc
  final_conc$yx <- final_yx

  # 2. Rango
  range_check <- audit_score_range(final_yx, max_score)

  # 3. Clamping Final
  ## final_conc <- clamp_scores(final_conc, min_score = 0, max_score = max_score)

  # 4. Divergencia
  div_check <- audit_identity_divergence(final_conc, config)

  # Status Global
  global_status <- "OK"
  if (mono_check$critical) {
    global_status <- "FAIL_MONOTONICITY"
  } else if (div_check$risk_level == "HIGH") {
    global_status <- "RISK_DIVERGENCE"
  } else if (!mono_check$passed) {
    global_status <- "WARN_FIXED_PAVA"
  }

  if (global_status == "OK" && mono_check$max_mag > 0) {
    global_status <- "WARN_FIXED_PAVA"
  }

  # Concatenación de notas para no perder contexto
  all_notes <- c(mono_check$note, range_check$note, div_check$note)
  # Filtramos notas "OK" para limpiar el log, dejamos solo las informativas/alertas
  relevant_notes <- all_notes[!grepl(":OK", all_notes)]
  if (length(relevant_notes) == 0) relevant_notes <- "QC:OK"
  final_note_str <- paste(relevant_notes, collapse = " | ")

  if (!is.null(logger)) {
    if (mono_check$critical) {
      warn(paste("QC_CRITICAL:", final_note_str))
    } else if (global_status != "OK") warn(paste("QC_WARN:", final_note_str))
  }

  list(
    status_flags = list(
      monotonicity_passed = mono_check$passed,
      monotonicity_risk = mono_check$critical,
      divergence_risk = div_check$risk_level,
      global_status = global_status
    ),
    metrics = list(
      violations_fixed = mono_check$n_violations,
      max_pava_adj = mono_check$max_mag,
      max_score_shift = div_check$max_change
    ),
    final_concordance = final_conc,
    note = final_note_str
  )
}
