# tests/test_config_loader.R
# Unit tests for setup_parallel and configuration utilities

library(testthat)

# Mock dependencies if not already loaded from the pipeline
# Note: we override to ensure we don't use base::debug or other system functions
# and to capture the output if needed.
custom_debug <- function(msg, ctx = "") {
  message(sprintf("DEBUG [%s] %s", ctx, msg))
}
assign("debug", custom_debug, envir = .GlobalEnv)

if (!exists("%||%")) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
}

# Ensure base infrastructure doesn't fail when loading the module
if (!exists("is_safe_r_path")) {
  is_safe_r_path <- function(path) TRUE
}

# Load the file to test
# Assumes execution from project root
if (file.exists("R/00_config_loader.R")) {
  source("R/00_config_loader.R")
}

context("Configuration - setup_parallel")

test_that("setup_parallel disables parallelism by default (mc.cores = 1)", {
  config <- list(system = list(use_parallel = FALSE))

  # Save previous state to avoid affecting other tests
  old_cores <- getOption("mc.cores")
  on.exit(options(mc.cores = old_cores))

  setup_parallel(config)
  expect_equal(getOption("mc.cores"), 1)
})

test_that("setup_parallel handles missing configuration as sequential", {
  config <- list(system = list())

  old_cores <- getOption("mc.cores")
  on.exit(options(mc.cores = old_cores))

  setup_parallel(config)
  expect_equal(getOption("mc.cores"), 1)
})

test_that("setup_parallel enables parallelism and respects hardware limits", {
  # Get available cores dynamically for portability
  avail <- parallel::detectCores(logical = FALSE)
  req_cores <- 2

  config <- list(system = list(use_parallel = TRUE, n_cores = req_cores))

  old_cores <- getOption("mc.cores")
  on.exit(options(mc.cores = old_cores))

  setup_parallel(config)

  # Logic: min(max(1, req_cores), max(1, avail - 1))
  expected_cores <- min(max(1, req_cores), max(1, avail - 1))

  expect_equal(getOption("mc.cores"), expected_cores)
})

test_that("setup_parallel caps n_cores to available cores - 1", {
  avail <- parallel::detectCores(logical = FALSE)
  # Request excessive cores
  req_cores <- avail + 100

  config <- list(system = list(use_parallel = TRUE, n_cores = req_cores))

  old_cores <- getOption("mc.cores")
  on.exit(options(mc.cores = old_cores))

  setup_parallel(config)

  # Should be capped to avail - 1 (minimum 1)
  expected_cores <- max(1, avail - 1)
  expect_equal(getOption("mc.cores"), expected_cores)
})

test_that("setup_parallel ensures at least 1 core if invalid value is requested", {
  config <- list(system = list(use_parallel = TRUE, n_cores = -10))

  old_cores <- getOption("mc.cores")
  on.exit(options(mc.cores = old_cores))

  setup_parallel(config)

  expect_equal(getOption("mc.cores"), 1)
})

test_that("get_form_code_map generates map correctly", {
  config <- list(
    forms = list(
      F1 = list(codes = c("101", "102")),
      F2 = list(codes = list("201", "202"))
    )
  )

  map <- get_form_code_map(config)

  expect_equal(map["101"], c("101" = "F1"))
  expect_equal(map["202"], c("202" = "F2"))
  expect_equal(length(map), 4)
})
