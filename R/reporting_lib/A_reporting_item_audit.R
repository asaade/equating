# ==============================================================================
# MÓDULO A: AUDITORÍA DE ÍTEMS Y DISTRACTORES (CTT & DIF)
# Versión: v1.0
# Descripción: Audita ítems, distractores y balance del sesgo (DIF).
#              Incluye detección de claves múltiples y ambigüedad.
# ==============================================================================

# --- FUNCIONES AUXILIARES DE FORMATEO ---
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
audit_item_quality <- function(ctt_results, dif_results, meta, base_dir, config) {
  # --- 1. Preparación del Entorno ---
  out_dir <- file.path(base_dir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  forma_id <- unique(ctt_results$stats$FORMA)[1]
  report_file <- file.path(out_dir, paste0("AUDIT_ITEMS_CTT_", forma_id, ".txt"))

  sink(report_file)
  on.exit(sink())

  # --- 2. Fusión de Datos (Data Merge) ---
  # Base: Estadísticas CTT
  items_df <- ctt_results$stats

  # Join Seguro con DIF: Solo si existe y tiene datos
  if (!is.null(dif_results) && is.data.frame(dif_results) && nrow(dif_results) > 0) {
    # Seleccionamos columnas clave de DIF
    cols_dif <- intersect(names(dif_results), c("ITEM", "ETS_Delta", "FLAG", "Favors"))
    dif_subset <- dif_results |> dplyr::select(dplyr::all_of(cols_dif))
    items_df <- items_df |> dplyr::left_join(dif_subset, by = c("ITEM"))
  } else {
    # Si no hay DIF, creamos las columnas vacías para mantener estructura
    items_df$ETS_Delta <- NA
    items_df$FLAG <- NA
    items_df$Favors <- NA
  }

  # Join Seguro con Metadata
  desired_meta_cols <- c("ITEM_ID", "KEY", "SUBTEST")
  items_df <- items_df |>
    dplyr::left_join(
      meta |> dplyr::select(dplyr::any_of(desired_meta_cols)),
      by = c("ITEM" = "ITEM_ID")
    )

  # Normalización de SUBTEST (Evita error de columna inexistente)
  if (!"SUBTEST" %in% names(items_df)) {
    items_df$SUBTEST <- "GLOBAL"
  } else {
    items_df$SUBTEST[is.na(items_df$SUBTEST)] <- "GLOBAL"
  }

  # --- 3. Clasificación de Alertas ---
  th_p_low <- 0.20
  th_p_high <- 0.90
  th_rbis <- 0.15

  items_df <- items_df |>
    dplyr::mutate(
      STATUS_CODE = dplyr::case_when(
        P_BIS < 0 ~ "FATAL",
        P_BIS < th_rbis ~ "LOW_DISC",
        P_VAL < th_p_low ~ "HARD",
        P_VAL > th_p_high ~ "EASY",
        !is.na(FLAG) & FLAG == "C" ~ "DIF_C",
        TRUE ~ "OK"
      )
    )

  # --- 4. Generación del Reporte (Salida de Texto) ---
  print_header(paste("AUDITORIA DE CALIDAD DE ITEMS (CTT): FORMA", forma_id))
  cat(paste0("Fecha: ", Sys.time(), "\n"))
  cat(paste0("N Items: ", nrow(items_df), "\n"))

  # Sección Resumen
  print_section("1. RESUMEN DE SALUD")
  counts <- table(items_df$STATUS_CODE)
  cat(paste0("Items OK : ", sum(items_df$STATUS_CODE == "OK"), "\n"))
  cat(paste0("Alertas  : ", sum(items_df$STATUS_CODE != "OK"), "\n"))

  if (sum(items_df$STATUS_CODE == "FATAL") > 0) {
    cat("\n*** ALERTA CRITICA: Items con correlación negativa detectados. ***\n")
  }

  # Sección Tabla Maestra
  print_section("2. DETALLE DE ITEMS")
  cat(paste0(
    pad_str("ITEM_ID", 15), pad_str("KEY", 5), pad_str("P_VAL", 8),
    pad_str("P_BIS", 8), pad_str("STATUS", 10), "\n"
  ))
  cat(paste0(strrep("-", 50), "\n"))

  items_sorted <- items_df |> dplyr::arrange(P_BIS)

  for (i in 1:nrow(items_sorted)) {
    row <- items_sorted[i, ]
    cat(paste0(
      pad_str(row$ITEM, 15), pad_str(row$KEY, 5),
      pad_str(sprintf("%.2f", row$P_VAL), 8),
      pad_str(sprintf("%.2f", row$P_BIS), 8),
      pad_str(row$STATUS_CODE, 10), "\n"
    ))
  }

  # --- 5. ANÁLISIS DE DISTRACTORES (NUEVA SECCIÓN) ---
  print_section("3. ANÁLISIS DE DISTRACTORES (POSIBLES CLAVES MÚLTIPLES)")

  if (!is.null(ctt_results$distractors)) {
    dist <- ctt_results$distractors

    # Identificar distractores con correlación positiva (competencia con la clave)
    # Criterio: Distractor no clave (IS_KEY == FALSE) con R_BIS > 0.05
    # Nota: Usamos IS_KEY (boolean) o comprobamos si OPTION != KEY

    bad_distractors <- dist |>
      dplyr::filter(IS_KEY == FALSE & R_BIS_OPT > 0.05) |>
      dplyr::select(ITEM, OPTION, PROP, R_BIS_OPT) |>
      dplyr::arrange(dplyr::desc(R_BIS_OPT))

    if (nrow(bad_distractors) > 0) {
      cat("ALERTA: Se detectaron distractores con correlación positiva (> 0.05).\n")
      cat("Esto sugiere ambigüedad, errores de clave o ítems con doble respuesta correcta.\n\n")

      cat(paste0(
        pad_str("ITEM", 15),
        pad_str("OPCION", 8),
        pad_str("PROP(p)", 10),
        pad_str("R_BIS", 10),
        "DIAGNÓSTICO\n"
      ))
      cat(paste0(strrep("-", 60), "\n"))

      for (i in 1:nrow(bad_distractors)) {
        row <- bad_distractors[i, ]

        diag <- "REVISAR CLAVE"
        # Si el distractor correlaciona más de 0.15, es un error grave (posible clave real)
        if (row$R_BIS_OPT > 0.15) diag <- "FATAL ERROR"

        cat(paste0(
          pad_str(row$ITEM, 15),
          pad_str(row$OPTION, 8),
          pad_str(sprintf("%.3f", row$PROP), 10),
          pad_str(sprintf("%.3f", row$R_BIS_OPT), 10),
          diag, "\n"
        ))
      }
    } else {
      cat(">> EXCELENTE: Ningún distractor muestra correlación positiva relevante con el puntaje total.\n")
    }
  } else {
    cat("AVISO: No se encontró el objeto 'distractors' en ctt_results para realizar este análisis.\n")
  }

  cat("\n>>> FIN DEL REPORTE A (ITEMS) <<<\n")

  debug(paste("Reporte A (Items) generado:", report_file))
}
