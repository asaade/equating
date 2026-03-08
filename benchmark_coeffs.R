source("R/07b_reporting_ctt_audit.R")

# Mock data
n <- 5000
coeffs <- data.frame(
  LINK_SOURCE = sample(paste0("FORM_", 1:100), n, replace = TRUE),
  LINK_TARGET = sample(paste0("FORM_", 1:100), n, replace = TRUE),
  METHOD_USED = sample(c("LINEAR", "EQUIPERCENTILE", "TUCKER"), n, replace = TRUE),
  SLOPE = runif(n, 0.5, 1.5),
  INTERCEPT = runif(n, -5, 5),
  GAMMA = runif(n, 0, 0.1)
)

eq_results <- list(
  link_quality = data.frame(
    R_ANCHOR = runif(n, 0.8, 1.0)
  )
)

# Original implementation (copy-pasted from file)
print_audit_coefficients_old <- function(coeffs, eq_results) {
  # We omit headers for clean benchmark
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

# Vectorized implementation (to be tested)
print_audit_coefficients_new <- function(coeffs, eq_results) {
  if (!is.null(coeffs)) {
    out <- paste0(
      pad_str(coeffs$LINK_SOURCE, 12),
      pad_str(coeffs$LINK_TARGET, 12),
      pad_str(coeffs$METHOD_USED, 12),
      fmt_num(coeffs$SLOPE, 3, 8),
      fmt_num(coeffs$INTERCEPT, 3, 8),
      fmt_num(coeffs$GAMMA, 3, 8),
      fmt_num(eq_results$link_quality$R_ANCHOR, 3, 8),
      "\n"
    )
    cat(out, sep = "")
  }
}

cat("Benchmarking print_audit_coefficients with n =", n, "\n")

t1 <- system.time({
  sink("/dev/null")
  print_audit_coefficients_old(coeffs, eq_results)
  sink()
})
cat(sprintf("Old version (Row-wise loop) took: %.4f seconds\n", t1["elapsed"]))

t2 <- system.time({
  sink("/dev/null")
  print_audit_coefficients_new(coeffs, eq_results)
  sink()
})
cat(sprintf("New version (Vectorized) took: %.4f seconds\n", t2["elapsed"]))

if (t1["elapsed"] > 0) {
  cat(sprintf("Improvement: %.2f%%\n", (t1["elapsed"] - t2["elapsed"]) / t1["elapsed"] * 100))
}

# Verify output consistency (roughly)
sink("output_old.txt")
print_audit_coefficients_old(coeffs[1:5,], eq_results)
sink()

sink("output_new.txt")
print_audit_coefficients_new(coeffs[1:5,], eq_results)
sink()

if (identical(readLines("output_old.txt"), readLines("output_new.txt"))) {
    cat("Outputs are identical!\n")
} else {
    cat("Outputs DIFFER!\n")
    system("diff output_old.txt output_new.txt")
}
