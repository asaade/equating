library(testthat)

# Mock config for testing
create_mock_config <- function(levels) {
  list(
    scoring = list(
      performance_levels = levels
    )
  )
}

test_that("assign_performance_levels returns N/A when config is missing", {
  expect_equal(assign_performance_levels(c(50, 100), list()), c("N/A", "N/A"))
})

test_that("assign_performance_levels handles valid numeric scores", {
  levels <- list(
    list(min_score = 0, label = "Low"),
    list(min_score = 50, label = "Medium"),
    list(min_score = 100, label = "High")
  )
  config <- create_mock_config(levels)

  scores <- c(25, 75, 125)
  expected <- c("Low", "Medium", "High")
  expect_equal(assign_performance_levels(scores, config), expected)
})

test_that("assign_performance_levels handles boundary conditions", {
  levels <- list(
    list(min_score = 0, label = "Low"),
    list(min_score = 50, label = "Medium"),
    list(min_score = 100, label = "High")
  )
  config <- create_mock_config(levels)

  # right = FALSE: [0, 50), [50, 100), [100, Inf)
  scores <- c(0, 50, 100)
  expected <- c("Low", "Medium", "High")
  expect_equal(assign_performance_levels(scores, config), expected)
})

test_that("assign_performance_levels handles NA and out of range scores", {
  levels <- list(
    list(min_score = 10, label = "Valid")
  )
  config <- create_mock_config(levels)

  scores <- c(5, 15, NA)
  # Scores < 10 should be N/A
  expected <- c("N/A", "Valid", "N/A")
  expect_equal(assign_performance_levels(scores, config), expected)
})

test_that("assign_performance_levels is robust to mixed-type config", {
  # This mimics the config.yaml structure that includes max_scaled_score
  levels <- list(
    list(min_score = 0, label = "Insuficiente"),
    list(min_score = 68, label = "Satisfactorio"),
    list(min_score = 95, label = "Sobresaliente"),
    list(max_scaled_score = 1300) # This element lacks min_score and label
  )
  config <- create_mock_config(levels)

  scores <- c(0, 68, 95)
  # If the current code fails, this test will catch it.
  # We expect it to still work by ignoring the invalid element.
  expected <- c("Insuficiente", "Satisfactorio", "Sobresaliente")
  expect_equal(assign_performance_levels(scores, config), expected)
})
