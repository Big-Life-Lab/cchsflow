# ==============================================================================
# Core Infrastructure Tests - Production Ready
# ==============================================================================
#
# Tests for the cleaned, production-ready flexible missing data infrastructure
#
# ==============================================================================

library(testthat)
library(haven)
library(dplyr)

# Source the core infrastructure
source("R/flexible-missing-handler.R")

# =============================================================================
# Core Infrastructure Tests
# =============================================================================

test_that("load_cchs_config loads configuration", {
  config <- load_cchs_config()
  
  expect_true(is.list(config))
  expect_true("pattern_definitions" %in% names(config))
  expect_true("patterns" %in% names(config$pattern_definitions))
})

test_that("get_missing_patterns returns available patterns", {
  patterns <- get_missing_patterns()
  
  expect_true(is.character(patterns))
  expect_true(length(patterns) > 0)
  expect_true("single_digit_missing" %in% patterns)
  expect_true("triple_digit_missing" %in% patterns)
})

test_that("create_missing_handler creates working handler for single_digit_missing", {
  test_data <- c(1, 2, 6, 7, 8, 9)
  
  handler <- create_missing_handler(
    test_data,
    handle_missing_data = "original",
    pattern_type = "single_digit_missing"
  )
  
  expect_true(is.list(handler))
  expect_true("is_missing" %in% names(handler))
  expect_true("is_tag" %in% names(handler))
  expect_true("propagate" %in% names(handler))
})

test_that("handler is_missing works correctly", {
  test_data <- c(1, 2, 6, 7)
  
  handler <- create_missing_handler(
    test_data,
    handle_missing_data = "original", 
    pattern_type = "single_digit_missing"
  )
  
  expect_false(handler$is_missing(1))
  expect_false(handler$is_missing(2))
  expect_true(handler$is_missing(6))   # not_applicable
  expect_true(handler$is_missing(7))   # missing_data
})

test_that("handler is_tag works correctly", {
  test_data <- c(1, 2, 6, 7, 8, 9)
  
  handler <- create_missing_handler(
    test_data,
    handle_missing_data = "original",
    pattern_type = "single_digit_missing"
  )
  
  expect_true(handler$is_tag(6, "not_applicable"))
  expect_false(handler$is_tag(6, "missing_data"))
  expect_true(handler$is_tag(7, "missing_data"))
  expect_true(handler$is_tag(8, "missing_data"))
  expect_true(handler$is_tag(9, "missing_data"))
})

test_that("handler propagate prioritizes correctly", {
  test_data <- c(1, 2, 6, 7)
  
  handler <- create_missing_handler(
    test_data,
    handle_missing_data = "original",
    pattern_type = "single_digit_missing"
  )
  
  # not_applicable (6) should win over missing_data (7)
  result <- handler$propagate(6, 7)
  expect_equal(result, 6)
  
  # missing_data should be returned when no not_applicable
  result2 <- handler$propagate(7, 8, 9)
  expect_true(result2 %in% c(7, 8, 9))
})

test_that("handler works with tagged_na data", {
  test_data <- c(1, 2, tagged_na("a"), tagged_na("b"))
  
  handler <- create_missing_handler(
    test_data,
    handle_missing_data = "tagged_na",
    pattern_type = "single_digit_missing"
  )
  
  expect_false(handler$is_missing(1))
  expect_true(handler$is_missing(tagged_na("a")))
  expect_true(handler$is_missing(tagged_na("b")))
  
  expect_true(handler$is_tag(tagged_na("a"), "not_applicable"))
  expect_true(handler$is_tag(tagged_na("b"), "missing_data"))
})

test_that("triple_digit_missing pattern works", {
  test_data <- c(1.75, 996, 997, 998, 999)
  
  handler <- create_missing_handler(
    test_data,
    handle_missing_data = "original",
    pattern_type = "triple_digit_missing" 
  )
  
  expect_false(handler$is_missing(1.75))
  expect_true(handler$is_missing(996))   # not_applicable
  expect_true(handler$is_missing(997))   # missing_data
  expect_true(handler$is_missing(998))   # missing_data
  expect_true(handler$is_missing(999))   # missing_data
  
  expect_true(handler$is_tag(996, "not_applicable"))
  expect_true(handler$is_tag(997, "missing_data"))
})

test_that("error handling works for invalid patterns", {
  test_data <- c(1, 2, 3)
  
  expect_error(
    create_missing_handler(test_data, pattern_type = "invalid_pattern"),
    "not found in configuration"
  )
})

cat("\n=== Core Infrastructure Tests Complete ===\n")
cat("All core functions validated for production use.\n")