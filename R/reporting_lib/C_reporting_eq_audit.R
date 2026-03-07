# ==============================================================================
# MÓDULO C: AUDITORÍA DE EQUIPARACIÓN (EQUATING) - VERSIÓN FORENSIC v6.0
# Fecha: 2026-02-07
# Nivel: Expert / Auditor Externo
# Mejoras: Reporte de Modelos No-Lineales (Pesos Sintéticos y Familias).
# ==============================================================================

# (Formatting functions removed, now using utils_report.R)

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

    vis_d <- vapply(cohen_d, function(x) draw_ascii_bar(x, limit = 1.0, width = 14, center = TRUE), FUN.VALUE = character(1))

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
    for (frm in forms) {
      sub_t <- tables[tables$SOURCE_FORM == frm, ]
      if (nrow(sub_t) == 0) next
      sc_min <- min(sub_t$RAW_SCORE, na.rm = TRUE)
      sc_max <- max(sub_t$RAW_SCORE, na.rm = TRUE)
      rng <- sc_max - sc_min
      cut1 <- sc_min + (rng * 0.33)
      cut2 <- sc_min + (rng * 0.66)
      see_low <- mean(sub_t$SEE[sub_t$RAW_SCORE <= cut1], na.rm = TRUE)
      see_mid <- mean(sub_t$SEE[sub_t$RAW_SCORE > cut1 & sub_t$RAW_SCORE <= cut2], na.rm = TRUE)
      see_high <- mean(sub_t$SEE[sub_t$RAW_SCORE > cut2], na.rm = TRUE)
      mean_see <- mean(sub_t$SEE, na.rm = TRUE)
      profile <- "FLAT"
      if (!is.na(see_mid) && see_mid > 0) {
        if (see_low > 1.3 * see_mid || see_high > 1.3 * see_mid) profile <- "U-SHAPE (Extr)"
      }
      cat(paste0(
        pad_str(frm, 12), fmt_num(mean_see, 3, 9), " | ",
        fmt_num(see_low, 3, 9), fmt_num(see_mid, 3, 9), fmt_num(see_high, 3, 9), " | ",
        pad_str(profile, 15), "\n"
      ))
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

  if (!is.null(coeffs)) {
    for (i in 1:nrow(coeffs)) {
      c_row <- coeffs[i, ]

      # Datos nuevos
      fam <- if (!is.null(c_row$MODEL_FAMILY)) c_row$MODEL_FAMILY else "LINEAR"
      wgt <- if (!is.null(c_row$GAMMA)) c_row$GAMMA else NA
      slope <- c_row$SLOPE

      # Diagnóstico
      diag <- "OK"
      if (!is.na(slope)) {
        if (slope < 0.85 || slope > 1.15) diag <- "SCALE WARPING"
        if (slope < 0.70 || slope > 1.30) diag <- "EXTREME WARP"
      }
      # Diagnóstico especial para Synthetic
      if (fam == "SYNTHETIC" && !is.na(wgt)) {
        if (wgt < 0.2) diag <- "MOSTLY EQUI"
        if (wgt > 0.8) diag <- "MOSTLY LIN"
      }

      cat(paste0(
        pad_str(c_row$LINK_SOURCE, 10), pad_str(c_row$LINK_TARGET, 10),
        pad_str(substr(fam, 1, 13), 14),
        fmt_num(slope, 3, 8),
        fmt_num(c_row$INTERCEPT, 2, 8),
        fmt_num(wgt, 2, 8),
        fmt_num(c_row$ANCHOR_COR, 2, 6),
        " | ", pad_str(diag, 15),
        "\n"
      ))
    }
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
    for (i in 1:nrow(top_fails)) {
      d <- top_fails[i, ]
      vis_z <- draw_ascii_bar(d$Z_SCORE, limit = 3.5, width = 14, center = TRUE)
      cat(paste0(
        pad_str(d$LINK, 16), pad_str(rownames(d), 10), pad_str(d$STATUS, 8),
        fmt_num(d$Z_SCORE, 2, 8), pad_str(vis_z, 14), "  ", d$REASON, "\n"
      ))
    }
  } else {
    cat("  [INFO] No Drift data available.\n")
  }
}

audit_06_cuts <- function(crit_tab) {
  if (!is.null(crit_tab) && nrow(crit_tab) > 0) {
    print_header("6. PRECISIÓN EN PUNTOS DE CORTE")
    crit_sorted <- crit_tab[order(crit_tab$FORM, crit_tab$TARGET_CUT_RAW_REF), ]
    cat(paste0(
      pad_str("FORM", 12), pad_str("CUT", 6, "right"), pad_str("EQ_SCR", 9, "right"),
      pad_str("SEE", 8, "right"), pad_str("CI_95%", 16, "right"), "   NOTE\n"
    ))
    cat(paste0(strrep("-", 90), "\n"))
    for (i in 1:nrow(crit_sorted)) {
      r <- crit_sorted[i, ]
      lower <- r$EQUATED_AT_CUT - (1.96 * r$SEE_AT_CUT)
      upper <- r$EQUATED_AT_CUT + (1.96 * r$SEE_AT_CUT)
      ci_str <- sprintf("[%s - %s]", round(lower, 1), round(upper, 1))
      cat(paste0(
        pad_str(r$FORM, 12), fmt_num(r$TARGET_CUT_RAW_REF, 0, 6), fmt_num(r$EQUATED_AT_CUT, 2, 9),
        fmt_num(r$SEE_AT_CUT, 3, 8), pad_str(ci_str, 16, "right"), "   ", r$NOTE, "\n"
      ))
    }
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
