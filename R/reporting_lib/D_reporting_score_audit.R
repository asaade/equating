# ==============================================================================
# FUNCIÓN PRINCIPAL
# ==============================================================================
audit_score_impact <- function(final_scores, raw_dat = NULL, eq_results = NULL, base_dir, config) {
  if (is.null(final_scores) || nrow(final_scores) == 0) {
    return(NULL)
  }

  # --- FIX: Detección segura de columnas en final_scores (Case-Insensitive) ---
  fs_cols <- names(final_scores)
  fs_cols_up <- toupper(fs_cols)

  col_fs_id    <- fs_cols[na.omit(match(toupper(c("ID", "PERSON_ID", "IDENTIFICADOR")), fs_cols_up))[1]]
  col_fs_form  <- fs_cols[na.omit(match(toupper(c("FORMA", "FORM", "COLECCIÓN")), fs_cols_up))[1]]
  col_fs_raw   <- fs_cols[na.omit(match(toupper(c("Raw_Global_CTT", "RAW_SCORE", "SCORE_RAW", "PUNTAJE_CRUDO")), fs_cols_up))[1]]
  col_fs_eq    <- fs_cols[na.omit(match(toupper(c("Eq_Global_CTT", "EQUATED_SCORE", "SCORE_EQ", "PUNTAJE_EQUIPARADO")), fs_cols_up))[1]]
  col_fs_see   <- fs_cols[na.omit(match(toupper(c("SEE_Global", "SEE", "ERROR_ESTANDAR", "SEM")), fs_cols_up))[1]]
  col_fs_level <- fs_cols[na.omit(match(toupper(c("Nivel", "LEVEL", "PERFORMANCE_LEVEL", "DESEMPEÑO")), fs_cols_up))[1]]

  # Ensure NA values are represented safely
  col_fs_id    <- ifelse(is.na(col_fs_id), NA_character_, col_fs_id)
  col_fs_form  <- ifelse(is.na(col_fs_form), NA_character_, col_fs_form)
  col_fs_raw   <- ifelse(is.na(col_fs_raw), NA_character_, col_fs_raw)
  col_fs_eq    <- ifelse(is.na(col_fs_eq), NA_character_, col_fs_eq)
  col_fs_see   <- ifelse(is.na(col_fs_see), NA_character_, col_fs_see)
  col_fs_level <- ifelse(is.na(col_fs_level), NA_character_, col_fs_level)
}