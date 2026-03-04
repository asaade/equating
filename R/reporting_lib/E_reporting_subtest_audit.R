# ==============================================================================
# MÓDULO E: AUDITORÍA DETALLADA DE SUBTESTS (DOMINIOS)
# Versión: v1.0
# Descripción: Análisis de ítems, correlaciones inter-subtest (validez),
#              calidad del enlace y diferencias de grupo por dominio.
# ==============================================================================

# --- FUNCIONES AUXILIARES ---
pad_str <- function(x, width, align = "left") {
  x <- as.character(x)
  if (is.na(x) || length(x) == 0) x <- ""
  if (nchar(x) > width) x <- substr(x, 1, width)
  if (align == "right") formatC(x, width = width, flag = "") else formatC(x, width = width, flag = "-")
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

  # 1. CALIDAD DE ÍTEMS AGREGADA
  print_section("1. CALIDAD PSICOMÉTRICA POR DOMINIO (ITEMS)")

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

  for (i in 1:nrow(sub_stats)) {
    row <- sub_stats[i, ]
    quality <- "OK"
    if (row$Mean_Pbis < 0.20) quality <- "BAJA DISC."
    if (row$Mean_Pval < 0.30) quality <- "MUY DIFÍCIL"
    if (row$Mean_Pval > 0.85) quality <- "MUY FÁCIL"

    cat(paste0(
      pad_str(row$SUBTEST, 20), pad_str(row$N_Items, 6),
      pad_str(sprintf("%.2f", row$Mean_Pval), 10), pad_str(sprintf("%.3f", row$Mean_Pbis), 10),
      pad_str(row$Items_Low_Disc, 8), quality, "\n"
    ))
  }

  # Identificar columnas de subtests (Raw_ y Eq_)
  raw_cols <- grep("^Raw_", names(final_scores), value = TRUE)
  raw_cols <- setdiff(raw_cols, "Raw_Global_CTT") # Excluir global

  # 2. VALIDEZ DISCRIMINANTE (CORRELACIONES) - NUEVO
  print_section("2. VALIDEZ DISCRIMINANTE (CORRELACIONES INTER-SUBTEST)")

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

  # 3. IMPACTO DEL EQUIPARAMIENTO
  print_section("3. IMPACTO DEL EQUIPARAMIENTO (DELTAS POR SUBTEST)")

  if (length(raw_cols) > 0) {
    cat(paste0(
      pad_str("SUBTEST", 20), pad_str("RAW_MN", 10), pad_str("EQ_MN", 10),
      pad_str("DELTA", 10), "EFECTO\n"
    ))
    cat(paste0(strrep("-", 65), "\n"))

    for (r_col in raw_cols) {
      sub_name <- gsub("Raw_", "", r_col)
      e_col <- paste0("Eq_", sub_name)

      if (e_col %in% names(final_scores)) {
        mn_raw <- mean(final_scores[[r_col]], na.rm = TRUE)
        mn_eq <- mean(final_scores[[e_col]], na.rm = TRUE)
        delta <- mn_eq - mn_raw

        effect <- "-"
        if (delta > 1.5) effect <- "BONIF."
        if (delta < -1.5) effect <- "CASTIGO"

        cat(paste0(
          pad_str(sub_name, 20),
          pad_str(sprintf("%.1f", mn_raw), 10),
          pad_str(sprintf("%.1f", mn_eq), 10),
          pad_str(sprintf("%+.2f", delta), 10),
          effect, "\n"
        ))
      }
    }
  } else {
    cat("No se detectaron columnas pareadas (Raw/Eq) para subtests.\n")
  }

  # 4. EQUIDAD POR SUBTEST (NUEVO)
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

        for (r_col in raw_cols) {
          sub_name <- gsub("Raw_", "", r_col)
          e_col <- paste0("Eq_", sub_name)
          target_col <- if (e_col %in% names(df_merge)) e_col else r_col

          # Calcular medias por grupo
          means_by_group <- tapply(df_merge[[target_col]], df_merge[[v]], mean, na.rm = TRUE)
          diff_max <- max(means_by_group) - min(means_by_group)

          flag <- ""
          if (diff_max > 5) flag <- "<! ALERTA" # Umbral arbitrario de 5 puntos

          cat(paste0(
            pad_str(sub_name, 20),
            pad_str(sprintf("%.2f", diff_max), 15),
            flag, "\n"
          ))
        }
      }
    }
  }

  # 5. AUDITORÍA TÉCNICA DEL ENLACE
  print_section("5. CALIDAD TÉCNICA DEL ENLACE (ANCLAS POR SUBTEST)")

  if (!is.null(eq_results) && !is.null(eq_results$topology_edges)) {
    # ... (Mismo código de la versión anterior para Topology)
    # Lo mantenemos para asegurar la visión completa
    cat(paste0(
      pad_str("SUBTEST (ESCALA)", 20), pad_str("N_ANCLA", 10),
      pad_str("CORR(r)", 10), "ESTADO\n"
    ))
    cat(paste0(strrep("-", 50), "\n"))

    subtests_to_check <- gsub("Raw_", "", raw_cols)
    for (sub_name in subtests_to_check) {
      topo_row <- eq_results$topology_edges |>
        dplyr::filter(grepl(paste0("^", sub_name), SCALE)) |>
        utils::head(1)

      if (nrow(topo_row) > 0) {
        status <- "OK"
        if (topo_row$R < 0.90) status <- "R BAJA"
        if (topo_row$N_Anchor < 10) status <- "POCOS ANC"

        cat(paste0(
          pad_str(sub_name, 20),
          pad_str(topo_row$N_Anchor, 10),
          pad_str(sprintf("%.3f", topo_row$R), 10),
          status, "\n"
        ))
      } else {
        cat(paste0(pad_str(sub_name, 20), "   (No aplica / Sin equating propio)\n"))
      }
    }
  } else {
    cat("Objeto 'eq_results' no disponible o sin topología detallada.\n")
  }

  cat("\n>>> FIN DEL REPORTE E <<<\n")
  debug(paste("Reporte de Subtests generado:", report_file))
}
