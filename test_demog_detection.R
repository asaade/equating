library(dplyr)
source("R/reporting_lib/D_reporting_score_audit.R")

# Mock data
final_scores <- data.frame(
  ID = c("1", "2", "3", "4"),
  FORMA = rep("A", 4),
  Raw_Global_CTT = c(10, 15, 20, 25),
  Eq_Global_CTT = c(10.5, 15.5, 20.5, 25.5),
  Nivel = c("Insuficiente", "Suficiente", "Sobresaliente", "Sobresaliente"),
  SEE_Global = rep(0.5, 4),
  stringsAsFactors = FALSE
)

# Test 1: Standard casing
raw_dat_1 <- data.frame(
  ID = c("1", "2", "3", "4"),
  SEXO = c("M", "F", "M", "F"),
  REGION = c("Norte", "Sur", "Norte", "Sur"),
  stringsAsFactors = FALSE
)

# Test 2: Varied casing
raw_dat_2 <- data.frame(
  id = c("1", "2", "3", "4"),
  sexo = c("M", "F", "M", "F"),
  Region = c("Norte", "Sur", "Norte", "Sur"),
  stringsAsFactors = FALSE
)

# Test 3: Missing columns
raw_dat_3 <- data.frame(
  ID = c("1", "2", "3", "4"),
  REGION = c("Norte", "Sur", "Norte", "Sur"),
  stringsAsFactors = FALSE
)

config <- list(scoring = list(performance_levels = list(list(label = "Suficiente", min_score = 15))))
base_dir <- "test_reports"
if (!dir.exists(base_dir)) dir.create(base_dir)

cat("\n--- Running Test 1 (Standard Casing) ---\n")
audit_score_impact(final_scores, raw_dat_1, NULL, base_dir, config)
cat(readLines(file.path(base_dir, "AUDIT_SCORES_IMPACT_A.txt")), sep = "\n")

cat("\n--- Running Test 2 (Varied Casing) ---\n")
audit_score_impact(final_scores, raw_dat_2, NULL, base_dir, config)
cat(readLines(file.path(base_dir, "AUDIT_SCORES_IMPACT_A.txt")), sep = "\n")

cat("\n--- Running Test 3 (Missing Columns) ---\n")
audit_score_impact(final_scores, raw_dat_3, NULL, base_dir, config)
cat(readLines(file.path(base_dir, "AUDIT_SCORES_IMPACT_A.txt")), sep = "\n")

cat("\n--- Test Suite Completed ---\n")
