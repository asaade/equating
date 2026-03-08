# ==============================================================================
# MÓDULO C: AUDITORÍA DE EQUIPARACIÓN (EQUATING) - VERSIÓN FORENSIC v6.0.1
# Fecha: 2026-02-11
# Nivel: Senior Psychometrician / Auditor Externo
# ==============================================================================

# --- UTILS DE FORMATO BLINDADO ---

pad_str <- function(x, width, align = "left") {
  x <- as.character(x)
  x[is.na(x)] <- ""
  eff_width <- width - 1
  if (any(nchar(x) > eff_width)) {
    long <- nchar(x) > eff_width
    x[long] <- paste0(substr(x[long], 1, eff_width - 2), "..")
  }
  formatted <- if (align == "right") {
    formatC(x, width = eff_width, flag = "")
  } else {
    formatC(x, width = eff_width, flag = "-")
  }
  paste0(formatted, " ")
}

fmt_num <- function(x, digits = 4, width = 8) {
  if (length(x) == 0) return(character(0))
  # Detectar valores no numéricos o NA de forma eficiente
  invalid <- is.na(x) | !vapply(x, is.numeric, logical(1))

  # Formatear todos (los invalid darán "NA" que luego sobreescribiremos)
  txt <- sprintf(paste0("%.", digits, "f"), x)
  res <- pad_str(txt, width, "right")

  if (any(invalid)) {
    res[invalid] <- pad_str("-", width, "right")
  }
  res
}

# --- VECTORIZED UTILS ---

# Optimization rationale: draw_ascii_bar was previously scalar-based and called
# via vapply, incurring significant overhead for each element. By refactoring it
# to use native vectorized operations (pmin, pmax, ifelse, strrep), we leverage
# R's internal efficiency for processing vectors, reducing both CPU time and
# memory allocation overhead during report generation.

draw_ascii_bar <- function(val, limit = 3, width = 10, center = FALSE) {
  if (length(val) == 0) return(character(0))

  res <- rep(pad_str("", width), length(val))
  na_idx <- is.na(val)
  if (all(na_idx)) return(res)

  eff_w <- width - 2
  valid_v <- val[!na_idx]

  if (center) {
    norm_v <- pmax(-1, pmin(1, valid_v / limit))
    center_idx <- floor(eff_w / 2)
    len_v <- floor(abs(norm_v) * (eff_w / 2))

    fill_char_v <- ifelse(valid_v < 0, "<", ">")
    fill_char_v[abs(valid_v) > limit] <- "!"

    chars_v <- rep(strrep(" ", eff_w), length(valid_v))
    left_v <- center_idx - ifelse(valid_v < 0, len_v, 0)
    right_v <- left_v + len_v

    # Vectorized assignment to characters
    substr(chars_v, left_v, right_v) <- strrep(fill_char_v, len_v + 1)

    # Ensure center_idx is valid for substr (R is 1-based)
    c_idx <- pmax(1, center_idx)
    substr(chars_v, c_idx, c_idx) <- "|"

    res[!na_idx] <- paste0("[", chars_v, "]")
  } else {
    mag_v <- pmin(abs(valid_v), limit) / limit
    len_v <- pmax(0, pmin(eff_w, round(mag_v * eff_w)))

    char_v <- ifelse(valid_v > limit * 0.8, "!", "#")

    res[!na_idx] <- paste0("[", strrep(char_v, len_v), strrep(" ", eff_w - len_v), "]")
  }
  res
}

print_header <- function(title) {
  cat(paste0("\n", strrep("=", 100), "\n"))
  cat(paste0("  ", title, "\n"))
  cat(paste0(strrep("=", 100), "\n"))
}

print_alert_box <- function(msgs, type = "CRITICAL") {
  if (length(msgs) == 0) {
    return()
  }
  border_char <- if (type == "CRITICAL") "!" else "*"
  cat(paste0("\n", strrep(border_char, 100), "\n"))
  cat(paste0("  >>> ALERTA DE AUDITORÍA [", type, "]:\n"))
  for (m in msgs) cat(paste0("      - ", m, "\n"))
  cat(paste0(strrep(border_char, 100), "\n"))
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

  if (!is.null(moments) && nrow(moments) > 0) {
    # Match descriptives to moments by Form Label
    idx <- match(moments$FORM, desc$FORMA_LABEL)
    d_raw <- desc[idx, ]

    delta <- moments$MEAN_EQ - d_raw$Mean
    cohen_d <- ifelse(!is.na(d_raw$SD) & d_raw$SD > 0, delta / d_raw$SD, 0)
    vis_d <- draw_ascii_bar(cohen_d, limit = 0.8, width = 14, center = TRUE)

    lines <- paste0(
      pad_str(moments$FORM, 12), fmt_num(moments$N, 0, 6), " | ",
      fmt_num(d_raw$Mean, 2, 8), fmt_num(moments$MEAN_EQ, 2, 8), fmt_num(delta, 2, 8), " | ",
      fmt_num(cohen_d, 3, 11), pad_str(vis_d, 14), "\n"
    )
    cat(paste(lines, collapse = ""))
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

  if (!is.null(decisions) && nrow(decisions) > 0) {
    sel_mark <- ifelse(decisions$Selected, "[X]", "[ ]")
    lines <- paste0(
      pad_str(decisions$Method, 15), pad_str(sel_mark, 5), pad_str(decisions$TRAFFIC_LIGHT, 7),
      fmt_num(decisions$W_SEE, 3, 8), fmt_num(decisions$RMSD_Ref, 3, 8),
      fmt_num(decisions$Wiggliness, 5, 10), " | ", pad_str(decisions$Reason_Tag, 25), "\n"
    )
    cat(paste(lines, collapse = ""))
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

  if (!is.null(coeffs) && nrow(coeffs) > 0) {
    # Asumiendo correspondencia 1:1 entre coeffs y link_quality$R_ANCHOR
    r_anchor <- eq_results$link_quality$R_ANCHOR[1:nrow(coeffs)]
    lines <- paste0(
      pad_str(coeffs$LINK_SOURCE, 12), pad_str(coeffs$LINK_TARGET, 12), pad_str(coeffs$METHOD_USED, 12),
      fmt_num(coeffs$SLOPE, 3, 8), fmt_num(coeffs$INTERCEPT, 3, 8),
      fmt_num(coeffs$GAMMA, 3, 8), fmt_num(r_anchor, 3, 8), "\n"
    )
    cat(paste(lines, collapse = ""))
  }
}

print_audit_drift <- function(drift) {
  print_header("4. ESTABILIDAD DE ANCLAS (FORENSIC DRIFT)")
  if (!is.null(drift) && nrow(drift) > 0) {
    # Filtrar solo anclas sospechosas o una muestra significativa
    drift_top <- head(drift[order(-abs(drift$Z_SCORE)), ], 20)

    cat(paste0(
      pad_str("ITEM_ID", 12), pad_str("STATUS", 8), pad_str("P_SRC", 7, "right"),
      pad_str("P_DST", 7, "right"), pad_str("Z_SCORE", 8, "right"), " | ",
      pad_str("VISUAL_DRIFT", 14), pad_str("REASON", 15), "\n"
    ))
    cat(paste0(strrep("-", 100), "\n"))

    vis_z <- draw_ascii_bar(drift_top$Z_SCORE, limit = 3.0, width = 14, center = TRUE)

    lines <- paste0(
      pad_str(rownames(drift_top), 12), pad_str(drift_top$STATUS, 8), fmt_num(drift_top$P_SRC, 2, 7),
      fmt_num(drift_top$P_DEST, 2, 7), fmt_num(drift_top$Z_SCORE, 2, 8), " | ",
      pad_str(vis_z, 14), pad_str(drift_top$REASON, 15), "\n"
    )
    cat(paste(lines, collapse = ""))
  }
}

print_audit_critical <- function(crit_tab) {
  if (!is.null(crit_tab) && nrow(crit_tab) > 0) {
    # --- FIX: Detección segura de columnas (Case-Insensitive) ---
    col_names <- names(crit_tab)
    col_names_up <- toupper(col_names)

    col_form <- col_names[match(toupper(c("FORM", "FORMA", "SOURCE_FORM")), col_names_up)[1]]
    col_cut  <- col_names[match(toupper(c("TARGET_CUT_RAW_REF", "EST_RAW_CUT", "CUT_SCORE")), col_names_up)[1]]
    col_eq   <- col_names[match(toupper(c("EQUATED_AT_CUT", "EQUATED", "EQUATED_SCORE")), col_names_up)[1]]
    col_see  <- col_names[match(toupper(c("SEE_AT_CUT", "SEE", "ERROR")), col_names_up)[1]]
    col_tre  <- col_names[match(toupper(c("TRE_AT_CUT", "TRE")), col_names_up)[1]]

    # Si faltan columnas críticas, abortar con mensaje
    if (is.na(col_form) || is.na(col_cut) || is.na(col_eq)) {
      cat("\n[WARN] No se pudieron identificar las columnas necesarias en crit_tab para auditoría de cortes.\n")
      return()
    }

    print_header("5. PRECISIÓN EN PUNTOS DE CORTE")
    cat(paste0(
      pad_str("FORM", 12), pad_str("CUT", 6, "right"), pad_str("EQ_SCORE", 9, "right"),
      pad_str("SEE", 8, "right"), pad_str("TRE", 8, "right"), " | ",
      pad_str("95% CI (Equated)", 18), "\n"
    ))
    cat(paste0(strrep("-", 100), "\n"))

    val_eq  <- as.numeric(crit_tab[[col_eq]])
    val_see <- if (!is.na(col_see)) as.numeric(crit_tab[[col_see]]) else rep(NA_real_, nrow(crit_tab))
    val_tre <- if (!is.na(col_tre)) as.numeric(crit_tab[[col_tre]]) else rep(NA_real_, nrow(crit_tab))

    ci_l <- val_eq - (1.96 * val_see)
    ci_u <- val_eq + (1.96 * val_see)
    ci_str <- ifelse(!is.na(val_see), sprintf("[%5.2f, %5.2f]", ci_l, ci_u), "N/A")

    lines <- paste0(
      pad_str(crit_tab[[col_form]], 12),
      fmt_num(crit_tab[[col_cut]], 0, 6),
      fmt_num(val_eq, 2, 9),
      fmt_num(val_see, 3, 8),
      fmt_num(val_tre, 3, 8), " | ",
      pad_str(ci_str, 18), "\n"
    )
    cat(paste(lines, collapse = ""))
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
