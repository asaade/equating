# benchmark_topology.R
# Benchmark for audit_03_topology optimization

# Load the target file
# Note: We assume this script is run from the project root
source("R/reporting_lib/C_reporting_eq_audit.R")

# Mock dependencies that might not be available or are too noisy
print_header <- function(title) NULL

# Generate large synthetic data for benchmarking
# We simulate 500 forms, each with 100 raw score entries, to create a substantial 'tables' df
set.seed(42)
n_forms <- 500
n_scores <- 101 # 0 to 100

tables <- expand.grid(
  SOURCE_FORM = paste0("FORM_", 1:n_forms),
  RAW_SCORE = 0:(n_scores-1)
)

# Add METHOD and BIAS_NET that vary or stay constant
# In the real system, BIAS_NET can vary by score
tables$METHOD <- rep(sample(c("DIRECT", "CHAIN[2]: LINEAR -> LINEAR", "IDENTITY", "CHAIN[3]: EQUI -> LIN -> EQUI"), n_forms, replace = TRUE), each = n_scores)
tables$BIAS_NET <- runif(nrow(tables), -3, 3)

# Function to keep a reference to the old implementation for comparison
audit_03_topology_old <- function(tables) {
  print_header("3. DIAGNÓSTICO DE TOPOLOGÍA (PATH HEALTH)")
  cat(paste0(
    pad_str("FORM", 12), pad_str("PATH_TYPE", 10), pad_str("NET_BIAS", 9, "right"), " | ",
    pad_str("METHOD_CHAIN", 35), " | ", pad_str("FLAGS", 20), "\n"
  ))
  cat(paste0(strrep("-", 100), "\n"))
  if (!is.null(tables)) {
    summ_topo <- unique(tables[, c("SOURCE_FORM", "METHOD", "BIAS_NET")])
    for (i in 1:nrow(summ_topo)) {
      src <- summ_topo$SOURCE_FORM[i]
      meth <- summ_topo$METHOD[i]
      bias <- summ_topo$BIAS_NET[i]
      path_type <- if (grepl("CHAIN", meth)) "MULTI" else "DIRECT"
      flag <- ""
      if (abs(bias) > 2.0) flag <- "HIGH BIAS ACCUM."
      if (grepl("IDENTITY", meth)) flag <- "IDENTITY FALLBACK"
      meth_disp <- gsub("CHAIN\\[\\d+\\]: ", "", meth)
      if (nchar(meth_disp) > 33) meth_disp <- paste0(substr(meth_disp, 1, 30), "...")
      cat(paste0(
        pad_str(src, 12), pad_str(path_type, 10), fmt_num(bias, 3, 9), " | ",
        pad_str(meth_disp, 35), " | ", pad_str(flag, 20), "\n"
      ))
    }
  }
}

cat(sprintf("Starting benchmark with %d rows in 'tables'...\n", nrow(tables)))

# Measure Old Version
t1 <- system.time({
  sink("/dev/null")
  audit_03_topology_old(tables)
  sink()
})
cat(sprintf("Old version (Loop-based) took: %.4f seconds\n", t1["elapsed"]))

# Source the file again to get the optimized version (after refactor)
# For initial baseline, this will just be the old one unless refactored.
t2 <- system.time({
  sink("/dev/null")
  audit_03_topology(tables)
  sink()
})
cat(sprintf("Current version (Vectorized) took: %.4f seconds\n", t2["elapsed"]))

if (t1["elapsed"] > 0) {
  cat(sprintf("Improvement: %.2f%%\n", (t1["elapsed"] - t2["elapsed"]) / t1["elapsed"] * 100))
}
