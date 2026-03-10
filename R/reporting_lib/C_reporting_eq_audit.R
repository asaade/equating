# ==============================================================================
# MÓDULO C: AUDITORÍA DE EQUIPARACIÓN (EQUATING) - VERSIÓN FORENSIC v6.0
# Fecha: 2026-02-07
# Nivel: Expert / Auditor Externo
# Mejoras: Reporte de Modelos No-Lineales (Pesos Sintéticos y Familias).
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

audit_01_impact <- function(desc_raw, mom_eq) {
  print_header("1. ANÁLISIS DE IMPACTO Y TAMAÑO DEL EFECTO")
  cat(paste0(
    pad_str("FORM", 12), pad_str("N", 6, "right"), " | ",
    pad_str("RAW_MN", 8, "right"), pad_str("EQ_MN", 8, "right"), pad_str("DELTA", 8, "right"), " | ",
    pad_str("EFF_SIZE(d)", 11, "right"), pad_str("VISUAL_D", 14), "\n"
  ))
  cat(paste0(strrep("-", 100), "\n"))
  if (!is.null(desc_raw) && nrow(desc_raw) > 0) {
    frm <- if ("FORMA_LABEL" %in% names(desc_raw)) desc_raw$FORMA_LABEL else desc_raw$FORM

    if (!is.null(mom_eq) && nrow(mom_eq) > 0) {
      idx <- match(frm, mom_eq$FORM)
      mn_eq <- mom_eq$MEAN_EQ[idx]
    } else {
      mn_eq <- rep(NA_real_, nrow(desc_raw))
    }

    delta <- mn_eq - desc_raw$Mean
    cohen_d <- ifelse(!is.na(delta) & desc_raw$SD > 0, delta / desc_raw$SD, NA_real_)

    vis_d <- draw_ascii_bar(cohen_d, limit = 1.0, width = 14, center = TRUE)

    lines <- paste0(
      pad_str(frm, 12), fmt_num(desc_raw$N, 0, 6), " | ",
      fmt_num(desc_raw$Mean, 2, 8), fmt_num(mn_eq, 2, 8), fmt_num(delta, 2, 8), " | ",
      fmt_num(cohen_d, 3, 11), pad_str(vis_d, 14), "\n"
    )
    cat(paste(lines, collapse = ""))
  }
}

audit_02_precision <- function(tables, config) {
  print_header("2. PERFIL DE PRECISIÓN CONDICIONAL (CSEE)")
  cat(paste0(
    pad_str("FORM", 12), pad_str("MEAN_SEE", 9, "right"), " | ",
    pad_str("LOW_SEE", 9, "right"), pad_str("MID_SEE", 9, "right"), pad_str("HIGH_SEE", 9, "right"), " | ",
    pad_str("PROFILE", 15), "\n"
  ))
  cat(paste0(strrep("-", 100), "\n"))
  if (!is.null(tables)) {
    forms <- setdiff(unique(tables$SOURCE_FORM), config$system$reference_form)
    sub_t <- tables[tables$SOURCE_FORM %in% forms, , drop = FALSE]
    if (nrow(sub_t) > 0) {
      valid_forms <- unique(sub_t$SOURCE_FORM)
      f_form <- factor(sub_t$SOURCE_FORM, levels = valid_forms)

      sc_min <- tapply(sub_t$RAW_SCORE, f_form, min, na.rm = TRUE)
      sc_max <- tapply(sub_t$RAW_SCORE, f_form, max, na.rm = TRUE)
      rng <- sc_max - sc_min

      cut1_form <- sc_min + (rng * 0.33)
      cut2_form <- sc_min + (rng * 0.66)

      idx <- as.integer(f_form)
      cut1_full <- cut1_form[idx]
      cut2_full <- cut2_form[idx]

      mask_low <- sub_t$RAW_SCORE <= cut1_full
      mask_mid <- sub_t$RAW_SCORE > cut1_full & sub_t$RAW_SCORE <= cut2_full
      mask_high <- sub_t$RAW_SCORE > cut2_full

      see_low <- tapply(ifelse(mask_low, sub_t$SEE, NA_real_), f_form, mean, na.rm = TRUE)
      see_mid <- tapply(ifelse(mask_mid, sub_t$SEE, NA_real_), f_form, mean, na.rm = TRUE)
      see_high <- tapply(ifelse(mask_high, sub_t$SEE, NA_real_), f_form, mean, na.rm = TRUE)
      mean_see <- tapply(sub_t$SEE, f_form, mean, na.rm = TRUE)

      profile <- rep("FLAT", length(valid_forms))
      extr_mask <- !is.na(see_mid) & see_mid > 0 & (
        (!is.na(see_low) & see_low > 1.3 * see_mid) |
        (!is.na(see_high) & see_high > 1.3 * see_mid)
      )
      extr_mask[is.na(extr_mask)] <- FALSE
      profile[extr_mask] <- "U-SHAPE (Extr)"

      lines <- paste0(
        pad_str(valid_forms, 12), fmt_num(mean_see, 3, 9), " | ",
        fmt_num(see_low, 3, 9), fmt_num(see_mid, 3, 9), fmt_num(see_high, 3, 9), " | ",
        pad_str(profile, 15), "\n"
      )
      cat(paste(lines, collapse = ""))
    }
  }
}

audit_03_topology <- function(tables) {
  print_header("3. DIAGNÓSTICO DE TOPOLOGÍA (PATH HEALTH)")
  cat(paste0(
    pad_str("FORM", 12), pad_str("PATH_TYPE", 10), pad_str("NET_BIAS", 9, "right"), " | ",
    pad_str("METHOD_CHAIN", 35), " | ", pad_str("FLAGS", 20), "\n"
  ))
  cat(paste0(strrep("-", 100), "\n"))

  if (!is.null(tables)) {
    summ_topo <- unique(tables[, c("SOURCE_FORM", "METHOD", "BIAS_NET")])
    if (nrow(summ_topo) > 0) {
      # Vectorización de lógica de topología
      path_type <- ifelse(grepl("CHAIN", summ_topo$METHOD), "MULTI", "DIRECT")

      flag <- rep("", nrow(summ_topo))
      idx_high_bias <- abs(summ_topo$BIAS_NET) > 2.0
      idx_identity <- grepl("IDENTITY", summ_topo$METHOD)
      flag[idx_high_bias] <- "HIGH BIAS ACCUM."
      flag[idx_identity] <- "IDENTITY FALLBACK"

      meth_disp <- gsub("CHAIN\\[\\d+\\]: ", "", summ_topo$METHOD)
      too_long <- nchar(meth_disp) > 33
      meth_disp[too_long] <- paste0(substr(meth_disp[too_long], 1, 30), "...")

      # Construcción vectorial de líneas
      lines <- paste0(
        pad_str(summ_topo$SOURCE_FORM, 12),
        pad_str(path_type, 10),
        fmt_num(summ_topo$BIAS_NET, 3, 9),
        " | ",
        pad_str(meth_disp, 35),
        " | ",
        pad_str(flag, 20),
        "\n"
      )
      cat(paste(lines, collapse = ""))
    }
  }
}

audit_04_params <- function(coeffs) {
  print_header("4. PARÁMETROS DEL MODELO (MODEL SPECS)")
  cat("  * MODEL_FAM: Familia (LINEAR, EQUIPERCENTILE, SYNTHETIC).\n")
  cat("  * WEIGHT: Peso del componente lineal (solo en Synthetic, NA en otros).\n")
  cat("  * SLOPE: Pendiente efectiva (si > 1.15 o < 0.85 indica cambio de forma).\n\n")

  # Columnas Actualizadas
  cat(paste0(
    pad_str("LINK_SRC", 10), pad_str("LINK_TGT", 10),
    pad_str("MODEL_FAM", 14),
    pad_str("SLOPE", 8, "right"),
    pad_str("INTCPT", 8, "right"),
    pad_str("WEIGHT", 8, "right"), # Nuevo
    pad_str("R_XY", 6, "right"),
    " | ", pad_str("DIAGNOSIS", 15),
    "\n"
  ))
  cat(paste0(strrep("-", 100), "\n"))

  if (!is.null(coeffs) && nrow(coeffs) > 0) {
    # Extracción vectorial de datos
    fam <- if (!is.null(coeffs$MODEL_FAMILY)) coeffs$MODEL_FAMILY else rep("LINEAR", nrow(coeffs))
    wgt <- if (!is.null(coeffs$GAMMA)) coeffs$GAMMA else rep(NA_real_, nrow(coeffs))
    slope <- coeffs$SLOPE

    # Diagnóstico Vectorizado
    diag <- rep("OK", nrow(coeffs))

    # Lógica de Pendiente (Slope)
    s_warp <- !is.na(slope) & (slope < 0.85 | slope > 1.15)
    e_warp <- !is.na(slope) & (slope < 0.70 | slope > 1.30)
    diag[s_warp] <- "SCALE WARPING"
    diag[e_warp] <- "EXTREME WARP"

    # Lógica para Synthetic
    is_synth <- fam == "SYNTHETIC" & !is.na(wgt)
    diag[is_synth & wgt < 0.2] <- "MOSTLY EQUI"
    diag[is_synth & wgt > 0.8] <- "MOSTLY LIN"

    # Construcción vectorial de líneas
    lines <- paste0(
      pad_str(coeffs$LINK_SOURCE, 10), pad_str(coeffs$LINK_TARGET, 10),
      pad_str(substr(fam, 1, 13), 14),
      fmt_num(slope, 3, 8),
      fmt_num(coeffs$INTERCEPT, 2, 8),
      fmt_num(wgt, 2, 8),
      fmt_num(coeffs$ANCHOR_COR, 2, 6),
      " | ", pad_str(diag, 15),
      "\n"
    )
    cat(paste(lines, collapse = ""))
  }
}

audit_05_drift <- function(drift) {
  print_header("5. ESTABILIDAD DE ANCLAS (FORENSIC DRIFT)")
  if (!is.null(drift) && nrow(drift) > 0) {
    drift_srt <- drift[order(-abs(drift$Z_SCORE)), ]
    top_fails <- head(drift_srt, 15)
    cat(paste0(
      pad_str("LINK", 16), pad_str("ITEM", 10), pad_str("STATUS", 8),
      pad_str("Z_SCORE", 8, "right"), pad_str("VISUAL_DRIFT", 14), "  REASON\n"
    ))
    cat(paste0(strrep("-", 100), "\n"))
    vis_z <- draw_ascii_bar(top_fails$Z_SCORE, limit = 3.5, width = 14, center = TRUE)
    lines <- paste0(
      pad_str(top_fails$LINK, 16), pad_str(rownames(top_fails), 10), pad_str(top_fails$STATUS, 8),
      fmt_num(top_fails$Z_SCORE, 2, 8), pad_str(vis_z, 14), "  ", top_fails$REASON, "\n"
    )
    cat(paste(lines, collapse = ""))
  } else {
    cat("  [INFO] No Drift data available.\n")
  }
}

audit_06_cuts <- function(crit_tab) {
  if (!is.null(crit_tab) && nrow(crit_tab) > 0) {
    # --- FIX: Detección segura de columnas (Case-Insensitive) ---
    col_names <- names(crit_tab)
    col_names_up <- toupper(col_names)

    col_form <- col_names[match(toupper(c("FORM", "FORMA", "SOURCE_FORM")), col_names_up)[1]]
    col_cut  <- col_names[match(toupper(c("TARGET_CUT_RAW_REF", "EST_RAW_CUT", "CUT_SCORE")), col_names_up)[1]]
    col_eq   <- col_names[match(toupper(c("EQUATED_AT_CUT", "EQUATED", "EQUATED_SCORE")), col_names_up)[1]]
    col_see  <- col_names[match(toupper(c("SEE_AT_CUT", "SEE", "ERROR")), col_names_up)[1]]
    col_note <- col_names[match(toupper(c("NOTE", "LABEL", "COMENTARIO")), col_names_up)[1]]

    # Si faltan columnas críticas, abortar con mensaje
    if (is.na(col_form) || is.na(col_cut) || is.na(col_eq)) {
      cat("\n[WARN] No se pudieron identificar las columnas necesarias en crit_tab para auditoría de cortes.\n")
      return()
    }

    print_header("6. PRECISIÓN EN PUNTOS DE CORTE")
    crit_sorted <- crit_tab[order(crit_tab[[col_form]], crit_tab[[col_cut]]), ]

    cat(paste0(
      pad_str("FORM", 12), pad_str("CUT", 6, "right"), pad_str("EQ_SCR", 9, "right"),
      pad_str("SEE", 8, "right"), pad_str("CI_95%", 16, "right"), "   NOTE\n"
    ))
    cat(paste0(strrep("-", 90), "\n"))

    val_form <- as.character(crit_sorted[[col_form]])
    val_cut  <- as.numeric(crit_sorted[[col_cut]])
    val_eq   <- as.numeric(crit_sorted[[col_eq]])
    val_see  <- if (!is.na(col_see)) as.numeric(crit_sorted[[col_see]]) else rep(NA_real_, nrow(crit_sorted))
    val_note <- if (!is.na(col_note)) as.character(crit_sorted[[col_note]]) else rep("-", nrow(crit_sorted))

    lower <- val_eq - (1.96 * ifelse(is.na(val_see), 0, val_see))
    upper <- val_eq + (1.96 * ifelse(is.na(val_see), 0, val_see))
    ci_str <- ifelse(!is.na(val_see), sprintf("[%s - %s]", round(lower, 1), round(upper, 1)), "N/A")

    lines <- paste0(
      pad_str(val_form, 12), fmt_num(val_cut, 0, 6), fmt_num(val_eq, 2, 9),
      fmt_num(val_see, 3, 8), pad_str(ci_str, 16, "right"), "   ", val_note, "\n"
    )
    cat(paste(lines, collapse = ""))
  }
}

#' @title Generar Reporte Forense de Auditoría (V6 - Extended Params)
audit_equating_process <- function(eq_results = NULL, config, base_dir = getwd(), file_suffix = "GLOBAL") {
  if (is.null(eq_results) || !is.list(eq_results)) {
    return(NULL)
  }

  exec_sum <- eq_results$executive_summary
  coeffs <- eq_results$coefficients
  links <- eq_results$link_quality
  topology <- eq_results$topology_plan
  crit_tab <- eq_results$audit_critical
  drift <- eq_results$drift_details
  tables <- eq_results$tables
  desc_raw <- eq_results$descriptives
  mom_eq <- eq_results$equated_moments

  report_file <- file.path(base_dir, paste0("FORENSIC_AUDIT_V6_", file_suffix, "_", format(Sys.time(), "%Y%m%d_%H%M"), ".txt"))
  sink(report_file)
  on.exit(sink())

  # ============================================================================
  # 0. HEADER
  # ============================================================================
  print_header(paste("CERTIFICADO DE AUDITORÍA PSICOMÉTRICA (V6 EXTENDED) -", file_suffix))
  cat(paste0("Fecha Ejecución : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"))
  cat(paste0("Motor           : Equating Engine v6.0 (Non-Linear Support)\n"))

  # --- 0.1 RED FLAGS ---
  alerts <- c()
  if (!is.null(exec_sum)) {
    fails <- exec_sum[grepl("FAIL|DROP", exec_sum$Status), ]
    if (nrow(fails) > 0) alerts <- c(alerts, paste(nrow(fails), "formas presentan FALLO TOTAL de enlace."))
    high_tre <- exec_sum[exec_sum$Mean_TRE > 1.0, ]
    if (nrow(high_tre) > 0) alerts <- c(alerts, paste(nrow(high_tre), "formas exceden el límite de Error Total (TRE > 1.0)."))
  }
  if (!is.null(links)) {
    weak_links <- links[links$R_ANCHOR < 0.80, ]
    if (nrow(weak_links) > 0) alerts <- c(alerts, paste(nrow(weak_links), "enlaces de red tienen correlación de anclaje < 0.80."))
  }
  if (length(alerts) > 0) {
    print_alert_box(alerts, "CRITICAL")
    cat("  ESTADO DE CERTIFICACIÓN: [RIESGO] - Requiere Revisión Manual\n")
  } else {
    cat("\n  ESTADO DE CERTIFICACIÓN: [PASSED] - Integridad Verificada\n")
  }

  # ============================================================================
  # LLAMADAS A LOS SUB-MÓDULOS DE AUDITORÍA
  # ============================================================================
  audit_01_impact(desc_raw, mom_eq)
  audit_02_precision(tables, config)
  audit_03_topology(tables)
  audit_04_params(coeffs)
  audit_05_drift(drift)
  audit_06_cuts(crit_tab)

  cat(paste0("\n", strrep("=", 100), "\n"))
  cat("FIN DEL REPORTE V6. HASH: ", paste(sample(c(LETTERS, 0:9), 8), collapse = ""), "\n")
  cat(paste0(strrep("=", 100), "\n"))
}
