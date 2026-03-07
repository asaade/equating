# ==============================================================================
# MÓDULO C: AUDITORÍA DE EQUIPARACIÓN (EQUATING) - VERSIÓN FORENSIC v6.0.1
# Fecha: 2026-02-11
# Nivel: Senior Psychometrician / Auditor Externo
# ==============================================================================

# --- CARGA DE UTILERÍAS ---
if (!exists("pad_str", mode = "function")) {
  # Intentar cargar desde rutas relativas comunes
  utils_path <- "R/reporting_lib/utils_report.R"
  if (file.exists(utils_path)) {
    source(utils_path)
  } else if (file.exists("utils_report.R")) {
    source("utils_report.R")
  }
}

# --- FUNCIONES DE SECCIÓN DE AUDITORÍA ---

print_audit_header_section <- function(prov, config, file_suffix, exec_sum, qc_log) {
  print_header(paste("CERTIFICADO DE AUDITORÍA PSICOMÉTRICA (V6) -", file_suffix))
  cat(paste0("Execution ID    : ", prov$execution_id %||% "N/A", "\n"))
  cat(paste0("Timestamp       : ", prov$timestamp %||% format(Sys.time()), "\n"))
  cat(paste0("Auditor/User    : ", prov$user %||% "System", "\n"))
  cat(paste0("System Version  : ", prov$system_version %||% "Unknown", "\n"))
  cat(paste0("Reference Form  : ", config$system$reference_form, "\n"))

  # --- 0.1 RED FLAGS ---
  alerts <- c()
  if (!is.null(exec_sum)) {
    if (any(exec_sum$Status != "OK", na.rm = TRUE)) alerts <- c(alerts, "Fallo en el estado general de una o más formas.")
    if (any(exec_sum$Mean_TRE > 0.5, na.rm = TRUE)) alerts <- c(alerts, "Error Total (TRE) excede el umbral de tolerancia (0.5).")
  }
  if (!is.null(qc_log) && any(qc_log$Status != "OK")) {
    alerts <- c(alerts, paste("Violaciones de QC detectadas en:", paste(qc_log$Source_Form[qc_log$Status != "OK"], collapse = ", ")))
  }

  if (length(alerts) > 0) {
    print_alert_box(alerts, "CRITICAL")
    cat("  ESTADO DE CERTIFICACIÓN: [RIESGO] - Requiere Revisión Manual\n")
  } else {
    cat("\n  ESTADO DE CERTIFICACIÓN: [PASSED] - Integridad Verificada\n")
  }
}

print_audit_moments <- function(moments, desc) {
  print_header("1. IMPACTO Y TAMAÑO DEL EFECTO (EQUATED MOMENTS)")
  cat(paste0(
    pad_str("FORM", 12), pad_str("N", 6, "right"), " | ",
    pad_str("RAW_MN", 8, "right"), pad_str("EQ_MN", 8, "right"), pad_str("DELTA", 8, "right"), " | ",
    pad_str("EFF_SIZE(d)", 11, "right"), pad_str("VISUAL_D", 14), "\n"
  ))
  cat(paste0(strrep("-", 100), "\n"))

  if (!is.null(moments)) {
    for (i in 1:nrow(moments)) {
      m <- moments[i, ]
      d_raw <- desc[desc$FORMA_LABEL == m$FORM, ]
      delta <- m$MEAN_EQ - d_raw$Mean
      cohen_d <- if (d_raw$SD > 0) delta / d_raw$SD else 0
      vis_d <- draw_ascii_bar(cohen_d, limit = 0.8, width = 14, center = TRUE)

      cat(paste0(
        pad_str(m$FORM, 12), fmt_num(m$N, 0, 6), " | ",
        fmt_num(d_raw$Mean, 2, 8), fmt_num(m$MEAN_EQ, 2, 8), fmt_num(delta, 2, 8), " | ",
        fmt_num(cohen_d, 3, 11), pad_str(vis_d, 14), "\n"
      ))
    }
  }
}

print_audit_decisions <- function(decisions) {
  print_header("2. TRAZABILIDAD DE DECISIONES (COMPETING MODELS)")
  cat(paste0(
    pad_str("METHOD", 15), pad_str("SEL", 5), pad_str("TL", 7),
    pad_str("W_SEE", 8, "right"), pad_str("RMSD", 8, "right"),
    pad_str("WIGGLE", 10, "right"), " | ", pad_str("REASON", 25), "\n"
  ))
  cat(paste0(strrep("-", 100), "\n"))

  if (!is.null(decisions)) {
    for (i in 1:nrow(decisions)) {
      dec <- decisions[i, ]
      sel_mark <- if (dec$Selected) "[X]" else "[ ]"
      cat(paste0(
        pad_str(dec$Method, 15), pad_str(sel_mark, 5), pad_str(dec$TRAFFIC_LIGHT, 7),
        fmt_num(dec$W_SEE, 3, 8), fmt_num(dec$RMSD_Ref, 3, 8),
        fmt_num(dec$Wiggliness, 5, 10), " | ", pad_str(dec$Reason_Tag, 25), "\n"
      ))
    }
  }
}

print_audit_coefficients <- function(coeffs, eq_results) {
  print_header("3. PARÁMETROS DEL MODELO SELECCIONADO")
  cat(paste0(
    pad_str("SOURCE", 12), pad_str("TARGET", 12), pad_str("METHOD", 12),
    pad_str("SLOPE", 8, "right"), pad_str("INTCPT", 8, "right"),
    pad_str("GAMMA", 8, "right"), pad_str("R_XY", 8, "right"), "\n"
  ))
  cat(paste0(strrep("-", 100), "\n"))

  if (!is.null(coeffs)) {
    for (i in 1:nrow(coeffs)) {
      co <- coeffs[i, ]
      cat(paste0(
        pad_str(co$LINK_SOURCE, 12), pad_str(co$LINK_TARGET, 12), pad_str(co$METHOD_USED, 12),
        fmt_num(co$SLOPE, 3, 8), fmt_num(co$INTERCEPT, 3, 8),
        fmt_num(co$GAMMA, 3, 8), fmt_num(eq_results$link_quality$R_ANCHOR[i], 3, 8), "\n"
      ))
    }
  }
}

print_audit_drift <- function(drift) {
  print_header("4. ESTABILIDAD DE ANCLAS (FORENSIC DRIFT)")
  if (!is.null(drift) && nrow(drift) > 0) {
    # Filtrar solo anclas sospechosas o una muestra significativa
    drift_top <- drift[order(-abs(drift$Z_SCORE)), ]
    cat(paste0(
      pad_str("ITEM_ID", 12), pad_str("STATUS", 8), pad_str("P_SRC", 7, "right"),
      pad_str("P_DST", 7, "right"), pad_str("Z_SCORE", 8, "right"), " | ",
      pad_str("VISUAL_DRIFT", 14), pad_str("REASON", 15), "\n"
    ))
    cat(paste0(strrep("-", 100), "\n"))

    for (i in 1:min(20, nrow(drift_top))) {
      dr <- drift_top[i, ]
      vis_z <- draw_ascii_bar(dr$Z_SCORE, limit = 3.0, width = 14, center = TRUE)
      cat(paste0(
        pad_str(rownames(dr), 12), pad_str(dr$STATUS, 8), fmt_num(dr$P_SRC, 2, 7),
        fmt_num(dr$P_DEST, 2, 7), fmt_num(dr$Z_SCORE, 2, 8), " | ",
        pad_str(vis_z, 14), pad_str(dr$REASON, 15), "\n"
      ))
    }
  }
}

print_audit_critical <- function(crit_tab) {
  if (!is.null(crit_tab)) {
    print_header("5. PRECISIÓN EN PUNTOS DE CORTE")
    cat(paste0(
      pad_str("FORM", 12), pad_str("CUT", 6, "right"), pad_str("EQ_SCORE", 9, "right"),
      pad_str("SEE", 8, "right"), pad_str("TRE", 8, "right"), " | ",
      pad_str("95% CI (Equated)", 18), "\n"
    ))
    cat(paste0(strrep("-", 100), "\n"))

    for (i in 1:nrow(crit_tab)) {
      ct <- crit_tab[i, ]
      ci_l <- ct$EQUATED_AT_CUT - (1.96 * ct$SEE_AT_CUT)
      ci_u <- ct$EQUATED_AT_CUT + (1.96 * ct$SEE_AT_CUT)
      ci_str <- sprintf("[%5.2f, %5.2f]", ci_l, ci_u)

      cat(paste0(
        pad_str(ct$FORM, 12), fmt_num(ct$TARGET_CUT_RAW_REF, 0, 6), fmt_num(ct$EQUATED_AT_CUT, 2, 9),
        fmt_num(ct$SEE_AT_CUT, 3, 8), fmt_num(ct$TRE_AT_CUT, 3, 8), " | ",
        pad_str(ci_str, 18), "\n"
      ))
    }
  }
}

#' @title Generar Reporte Forense de Auditoría (V6 - Updated Persistence)
audit_equating_process <- function(eq_results = NULL, config, base_dir = getwd(), file_suffix = "GLOBAL") {
  if (is.null(eq_results) || !is.list(eq_results)) {
    return(NULL)
  }

  # Extracción de componentes según nueva estructura
  tables <- eq_results$tables
  coeffs <- eq_results$coefficients
  decisions <- eq_results$decisions
  drift <- eq_results$drift_details
  crit_tab <- eq_results$audit_critical
  moments <- eq_results$equated_moments
  desc <- eq_results$descriptives
  exec_sum <- eq_results$executive_summary
  prov <- eq_results$provenance
  qc_log <- eq_results$audit_qc

  report_file <- file.path(base_dir, paste0("FORENSIC_AUDIT_V6_", file_suffix, ".txt"))
  sink(report_file)
  on.exit(sink())

  print_audit_header_section(prov, config, file_suffix, exec_sum, qc_log)
  print_audit_moments(moments, desc)
  print_audit_decisions(decisions)
  print_audit_coefficients(coeffs, eq_results)
  print_audit_drift(drift)
  print_audit_critical(crit_tab)

  cat(paste0("\n", strrep("=", 100), "\n"))
  cat("FIN DEL REPORTE V6. HASH DE INTEGRIDAD: ", paste(sample(c(0:9, LETTERS), 12), collapse = ""), "\n")
  cat(paste0(strrep("=", 100), "\n"))
}
