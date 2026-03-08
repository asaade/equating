source("R/reporting_lib/C_reporting_eq_audit.R")

# Mock pad_str and fmt_num if not present
if (!exists("pad_str")) {
  pad_str <- function(x, width, align="left") formatC(x, width=width, flag=if(align=="right") "" else "-")
  fmt_num <- function(x, digits=4, width=8) pad_str(sprintf(paste0("%.",digits,"f"), x), width, "right")
}

# Create a mock table
set.seed(42)
n_forms <- 100
n_rows_per_form <- 1000
tables <- data.frame(
  SOURCE_FORM = rep(paste0("FORM_", 1:n_forms), each = n_rows_per_form),
  RAW_SCORE = runif(n_forms * n_rows_per_form, 0, 100),
  SEE = runif(n_forms * n_rows_per_form, 0, 5)
)

config <- list(system = list(reference_form = "FORM_0"))

# capture output
sink("/dev/null")
start_time <- Sys.time()
audit_02_precision(tables, config)
end_time <- Sys.time()
sink()

cat("Baseline time:\n")
print(end_time - start_time)
