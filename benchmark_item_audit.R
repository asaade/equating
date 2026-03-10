
library(dplyr)
source("R/00_common_base.R")
source("R/reporting_lib/A_reporting_item_audit.R")

# Mock data generation
create_mock_data <- function(n_items = 1000) {
  items <- paste0("ITEM_", 1:n_items)

  ctt_stats <- data.frame(
    ITEM = items,
    FORMA = "FORMA_A",
    P_VAL = runif(n_items),
    P_BIS = runif(n_items, -0.2, 0.8),
    stringsAsFactors = FALSE
  )

  distractors <- expand.grid(ITEM = items, OPTION = c("A", "B", "C", "D")) %>%
    mutate(
      PROP = runif(n()),
      R_BIS_OPT = runif(n(), -0.1, 0.2),
      IS_KEY = OPTION == "A"
    ) %>%
    group_by(ITEM) %>%
    mutate(PROP = PROP / sum(PROP)) %>%
    ungroup()

  ctt_results <- list(
    stats = ctt_stats,
    distractors = distractors
  )

  dif_results <- data.frame(
    ITEM = items,
    ETS_Delta = runif(n_items),
    FLAG = sample(c("A", "B", "C", NA), n_items, replace = TRUE),
    Favors = sample(c("G1", "G2", NA), n_items, replace = TRUE),
    stringsAsFactors = FALSE
  )

  meta <- data.frame(
    ITEM_ID = items,
    KEY = "A",
    SUBTEST = "TEST_1",
    stringsAsFactors = FALSE
  )

  config <- list(
    thresholds = list(
      ctt_pbis_min = 0.15,
      ctt_distractor_max_pbis = 0.05
    )
  )

  list(ctt_results = ctt_results, dif_results = dif_results, meta = meta, config = config)
}

benchmark_audit <- function(n_items = 100) {
  data <- create_mock_data(n_items)
  base_dir <- tempdir()

  start_time <- Sys.time()
  audit_item_quality(data$ctt_results, data$dif_results, data$meta, base_dir, data$config)
  end_time <- Sys.time()

  return(end_time - start_time)
}

# Run benchmark
cat("Benchmarking with 100 items...\n")
print(benchmark_audit(100))

cat("\nBenchmarking with 1000 items...\n")
print(benchmark_audit(1000))
