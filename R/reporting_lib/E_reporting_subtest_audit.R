# ==============================================================================
# MÓDULO E: AUDITORÍA DETALLADA DE SUBTESTS (DOMINIOS)
# Versión: v1.0
# Descripción: Análisis de ítems, correlaciones inter-subtest (validez),
#              calidad del enlace y diferencias de grupo por dominio.
# ==============================================================================

# --- FUNCIONES AUXILIARES ---
pad_str <- function(x, width, align = "left") {
  x <- as.character(x)
  x[is.na(x)] <- ""
  if (any(nchar(x) > width)) {
    long <- nchar(x) > width
    x[long] <- substr(x[long], 1, width)
  }
  if (align == "right") {
    formatC(x, width = width, flag = "")
  } else {
    formatC(x, width = width, flag = "-")
  }
}

print_header <- function(title) {
  cat(paste0(strrep("=", 80), "\n"))
  cat(paste0("  ", title, "\n"))
  cat(paste0(strrep("=", 80), "\n"))
}

print_section <- function(title) {
  cat(paste0("\n", strrep("-", 80), "\n"))
  cat(paste0(">>> ", title, "\n"))
  cat(paste0(strrep("-", 80), "\n"))
}

analyze_item_quality <- function(ctt_stats, meta) {
  # Preparación de datos de ítems
  if (!"SUBTEST" %in% names(ctt_stats)) {
    item_data <- ctt_stats |>
      dplyr::left_join(meta |> dplyr::select(ITEM_ID, SUBTEST), by = c("ITEM" = "ITEM_ID"))
  } else {
    item_data <- ctt_stats
  }
  item_data$SUBTEST[is.na(item_data$SUBTEST)] <- "UNDEFINED"

  sub_stats <- item_data |>
    dplyr::group_by(SUBTEST) |>
    dplyr::summarise(
      N_Items = dplyr::n(),
      Mean_Pval = mean(P_VAL, na.rm = TRUE),
      Mean_Pbis = mean(P_BIS, na.rm = TRUE),
      Items_Low_Disc = sum(P_BIS < 0.15, na.rm = TRUE)
    ) |>
    dplyr::arrange(desc(Mean_Pbis))

  cat(paste0(
    pad_str("SUBTEST", 20), pad_str("N_IT", 6), pad_str("DIFF(p)", 10),
    pad_str("DISC(r)", 10), pad_str("BAD_IT", 8), "CALIDAD\n"
  ))
  cat(paste0(strrep("-", 70), "\n"))

  if (nrow(sub_stats) > 0) {
    sub_stats <- sub_stats |>
      dplyr::mutate(
        quality = dplyr::case_when(
          Mean_Pval > 0.85 ~ "MUY FÁCIL",
          Mean_Pval < 0.30 ~ "MUY DIFÍCIL",
          Mean_Pbis < 0.20 ~ "BAJA DISC.",
          TRUE ~ "OK"
        )
      )

    lines <- paste0(
      pad_str(sub_stats$SUBTEST, 20),
      pad_str(sub_stats$N_Items, 6),
      pad_str(sprintf("%.2f", sub_stats$Mean_Pval), 10),
      pad_str(sprintf("%.3f", sub_stats$Mean_Pbis), 10),
      pad_str(sub_stats$Items_Low_Disc, 8),
      sub_stats$quality, "\n"
    )
    cat(paste(lines, collapse = ""))
  }
}

analyze_discriminant_validity <- function(final_scores, raw_cols) {
  if (length(raw_cols) > 1) {
    # Usamos puntuaciones equiparadas para la correlación si existen, sino crudas
    eq_cols_target <- gsub("Raw_", "Eq_", raw_cols)
    valid_eq_cols <- intersect(eq_cols_target, names(final_scores))

    cols_to_cor <- if (length(valid_eq_cols) == length(raw_cols)) valid_eq_cols else raw_cols

    # Calcular matriz de correlación
    cor_mat <- cor(final_scores[, cols_to_cor], use = "pairwise.complete.obs")

    cat("Matriz de Correlación entre Subtests:\n")
    print(round(cor_mat, 3))
    cat("\n")

    # Chequeo de redundancia
    high_cor <- which(cor_mat > 0.90 & cor_mat < 1.0, arr.ind = TRUE)
    if (nrow(high_cor) > 0) {
      cat("ALERTA DE REDUNDANCIA: Se detectaron correlaciones > 0.90.\n")
      cat("Esto sugiere que los subtests miden el mismo constructo y podrían no aportar información diagnóstica única.\n")
    } else {
      cat(">> OK: Las correlaciones sugieren que los subtests aportan información diferenciada (< 0.90).\n")
    }
  } else {
    cat("Menos de 2 subtests detectados. No aplica análisis de correlación.\n")
  }
}

analyze_equating_impact <- function(final_scores, raw_cols) {
  if (length(raw_cols) > 0) {
    cat(paste0(
      pad_str("SUBTEST", 20), pad_str("RAW_MN", 10), pad_str("EQ_MN", 10),
      pad_str("DELTA", 10), "EFECTO\n"
    ))
    cat(paste0(strrep("-", 65), "\n"))

    sub_names <- gsub("Raw_", "", raw_cols)
    e_cols <- paste0("Eq_", sub_names)
    valid_mask <- e_cols %in% names(final_scores)

    if (any(valid_mask)) {
      v_sub_names <- sub_names[valid_mask]
      v_raw_cols <- raw_cols[valid_mask]
      v_e_cols <- e_cols[valid_mask]

      mn_raw <- vapply(v_raw_cols, function(c) mean(final_scores[[c]], na.rm = TRUE), numeric(1))
      mn_eq <- vapply(v_e_cols, function(c) mean(final_scores[[c]], na.rm = TRUE), numeric(1))
      delta <- mn_eq - mn_raw

      effect <- rep("-", length(delta))
      effect[delta > 1.5] <- "BONIF."
      effect[delta < -1.5] <- "CASTIGO"

      lines <- paste0(
        pad_str(v_sub_names, 20),
        pad_str(sprintf("%.1f", mn_raw), 10),
        pad_str(sprintf("%.1f", mn_eq), 10),
        pad_str(sprintf("%+.2f", delta), 10),
        effect, "\n"
      )
      cat(paste(lines, collapse = ""))
    }
  } else {
    cat("No se detectaron columnas pareadas (Raw/Eq) para subtests.\n")
  }
}

analyze_subtest_equity <- function(final_scores, raw_dat, raw_cols) {
  if (!is.null(raw_dat) && length(raw_cols) > 0) {
    print_section("4. DETECCIÓN DE SESGO ESPECÍFICO (EQUIDAD POR DOMINIO)")

    # Unir demográficos
    # Asegurar ID char
    final_scores$ID <- as.character(final_scores$ID)
    raw_dat$ID <- as.character(raw_dat$ID)

    df_merge <- final_scores |>
      dplyr::left_join(raw_dat |> dplyr::select(dplyr::any_of(c("ID", "SEXO", "REGION"))), by = "ID")

    vars_demog <- intersect(names(df_merge), c("SEXO", "REGION"))

    if (length(vars_demog) > 0) {
      for (v in vars_demog) {
        cat(paste0("\n--- Análisis por variable: ", v, " ---\n"))
        cat(paste0(pad_str("SUBTEST", 20), "DIFERENCIA MAX ENTRE GRUPOS (Puntos)\n"))
        cat(paste0(strrep("-", 60), "\n"))

        sub_names <- gsub("Raw_", "", raw_cols)
        e_cols <- paste0("Eq_", sub_names)
        target_cols <- ifelse(e_cols %in% names(df_merge), e_cols, raw_cols)

        diff_max <- vapply(target_cols, function(tc) {
          means_by_group <- tapply(df_merge[[tc]], df_merge[[v]], mean, na.rm = TRUE)
          max(means_by_group) - min(means_by_group)
        }, numeric(1))

        flags <- ifelse(diff_max > 5, "<! ALERTA", "")

        lines <- paste0(
          pad_str(sub_names, 20),
          pad_str(sprintf("%.2f", diff_max), 15),
          flags, "\n"
        )
        cat(paste(lines, collapse = ""))
      }
    }
  }
}

analyze_linkage_quality <- function(eq_results, raw_cols) {
  if (!is.null(eq_results) && !is.null(eq_results$topology_edges)) {
    # ... (Mismo código de la versión anterior para Topology)
    # Lo mantenemos para asegurar la visión completa
    cat(paste0(
      pad_str("SUBTEST (ESCALA)", 20), pad_str("N_ANCLA", 10),
      pad_str("CORR(r)", 10), "ESTADO\n"
    ))
    cat(paste0(strrep("-", 50), "\n"))

    sub_names <- gsub("Raw_", "", raw_cols)

    topo_data <- lapply(sub_names, function(sn) {
      eq_results$topology_edges |>
        dplyr::filter(grepl(paste0("^", sn), SCALE)) |>
        utils::head(1)
    })
    topo_df <- dplyr::bind_rows(topo_data)

    if (nrow(topo_df) > 0) {
      # Mapear de vuelta a sub_names (algunos podrían faltar en topo_df si filter falló)
      # Pero bind_rows mantiene el orden si no hay nulos, sin embargo filter puede retornar 0 rows.
      # Mejor reconstruir una tabla completa para reporte
      report_data <- data.frame(SUBTEST = sub_names, stringsAsFactors = FALSE)

      # Unir con topo_df usando una columna temporal de match
      # O mejor, procesar fila por fila en el lapply y retornar un df con NAs
      topo_list <- lapply(sub_names, function(sn) {
        row <- eq_results$topology_edges |>
          dplyr::filter(grepl(paste0("^", sn), SCALE)) |>
          utils::head(1)
        if (nrow(row) == 0) {
          return(data.frame(SUBTEST = sn, N_Anchor = NA, R = NA, EXISTS = FALSE))
        } else {
          return(data.frame(SUBTEST = sn, N_Anchor = row$N_Anchor, R = row$R, EXISTS = TRUE))
        }
      })
      report_df <- dplyr::bind_rows(topo_list)

      report_df$status <- "OK"
      report_df$status[!is.na(report_df$R) & report_df$R < 0.90] <- "R BAJA"
      report_df$status[!is.na(report_df$N_Anchor) & report_df$N_Anchor < 10] <- "POCOS ANC"

      lines <- ifelse(report_df$EXISTS,
        paste0(
          pad_str(report_df$SUBTEST, 20),
          pad_str(report_df$N_Anchor, 10),
          pad_str(sprintf("%.3f", report_df$R), 10),
          report_df$status, "\n"
        ),
        paste0(pad_str(report_df$SUBTEST, 20), "   (No aplica / Sin equating propio)\n")
      )
      cat(paste(lines, collapse = ""))
    }
  } else {
    cat("Objeto 'eq_results' no disponible o sin topología detallada.\n")
  }
}

# ==============================================================================
# FUNCIÓN PRINCIPAL
# ==============================================================================
audit_subtest_analysis <- function(final_scores, ctt_stats, eq_results = NULL, meta, raw_dat = NULL, base_dir, config) {
  check_config <- isTRUE(config$scoring$include_subscores)
  check_col <- "SUBTEST" %in% names(meta)

  if (!check_config) {
    debug("Módulo E: Omitido por configuración (include_subscores = false).")
    return(NULL)
  }

  if (!check_col) {
    warn("Módulo E: Omitido. Se solicitó análisis pero columna 'SUBTEST' falta en metadata.")
    return(NULL)
  }

  if (is.null(final_scores)) {
    return(NULL)
  }

  out_dir <- file.path(base_dir)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  forma_id <- unique(final_scores$FORMA)[1]
  if (is.na(forma_id)) forma_id <- "GLOBAL"
  report_file <- file.path(out_dir, paste0("AUDIT_SUBTESTS_DETAIL_", forma_id, ".txt"))

  sink(report_file)
  on.exit(sink())

  print_header(paste("ANÁLISIS DE SUBTESTS (DOMINIOS): FORMA", forma_id))
  cat(paste0("N Sustentantes: ", nrow(final_scores), "\n"))

  # Identificar columnas de subtests (Raw_ y Eq_)
  raw_cols <- grep("^Raw_", names(final_scores), value = TRUE)
  raw_cols <- setdiff(raw_cols, "Raw_Global_CTT") # Excluir global

  # 1. CALIDAD DE ÍTEMS AGREGADA
  print_section("1. CALIDAD PSICOMÉTRICA POR DOMINIO (ITEMS)")
  analyze_item_quality(ctt_stats, meta)

  # 2. VALIDEZ DISCRIMINANTE (CORRELACIONES) - NUEVO
  print_section("2. VALIDEZ DISCRIMINANTE (CORRELACIONES INTER-SUBTEST)")
  analyze_discriminant_validity(final_scores, raw_cols)

  # 3. IMPACTO DEL EQUIPARAMIENTO
  print_section("3. IMPACTO DEL EQUIPARAMIENTO (DELTAS POR SUBTEST)")
  analyze_equating_impact(final_scores, raw_cols)

  # 4. EQUIDAD POR SUBTEST (NUEVO)
  analyze_subtest_equity(final_scores, raw_dat, raw_cols)

  # 5. AUDITORÍA TÉCNICA DEL ENLACE
  print_section("5. CALIDAD TÉCNICA DEL ENLACE (ANCLAS POR SUBTEST)")
  analyze_linkage_quality(eq_results, raw_cols)

  cat("\n>>> FIN DEL REPORTE E <<<\n")
  debug(paste("Reporte de Subtests generado:", report_file))
}
