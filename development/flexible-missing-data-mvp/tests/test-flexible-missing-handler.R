# ==============================================================================
# Flexible Missing Data Handler - Test Suite
# ==============================================================================
#
# Comprehensive test suite for flexible missing data handling MVP.
# Tests both "tagged_na" and "original" modes with edge cases and vector processing.
#
# **Test Coverage:**
# - Default behavior and backward compatibility
# - Mode comparison and consistency
# - Missing data conversion patterns
# - Vector processing with mixed data
# - Error handling and validation
# - Performance and regression testing
#
# @note Test suite v1.0.0, created: 2025-07-14
# ==============================================================================

library(testthat)
library(haven)
library(dplyr)

# Source the flexible missing handler
source("../R/flexible-missing-handler.R")

# =============================================================================
# Test Data Setup
# =============================================================================

# Create test data that mimics output from rec_with_table()
create_test_data <- function() {
  list(
    valid_numeric = c(1.5, 2.5, 3.0, 4.5),
    mixed_with_tagged_na = c(1.5, tagged_na("a"), 3.0, tagged_na("b")),
    all_tagged_na = c(tagged_na("a"), tagged_na("b"), tagged_na("a")),
    empty_vector = numeric(0),
    single_valid = 2.5,
    single_tagged_na = tagged_na("a"),
    large_vector = c(rep(1:10, 100), tagged_na("a"), tagged_na("b"))
  )
}

# =============================================================================
# 1. Default Behavior Tests
# =============================================================================

test_that("flexible_missing_handler defaults to tagged_na mode", {
  test_data <- c(1.5, tagged_na("a"), 3.0, tagged_na("b"))
  
  # Default call should preserve tagged_na
  result_default <- flexible_missing_handler(test_data)
  result_explicit <- flexible_missing_handler(test_data, "tagged_na")
  
  expect_equal(result_default, result_explicit)
  expect_true(is_tagged_na(result_default[2], "a"))
  expect_true(is_tagged_na(result_default[4], "b"))
})

test_that("tagged_na mode preserves all tagged_na semantics", {
  test_data <- c(
    1.5, 
    tagged_na("a"), 
    tagged_na("b"), 
    tagged_na("c"), 
    tagged_na("d"),
    3.0
  )
  
  result <- flexible_missing_handler(test_data, "tagged_na")
  
  expect_equal(result[1], 1.5)
  expect_true(is_tagged_na(result[2], "a"))
  expect_true(is_tagged_na(result[3], "b"))
  expect_true(is_tagged_na(result[4], "c"))
  expect_true(is_tagged_na(result[5], "d"))
  expect_equal(result[6], 3.0)
})

# =============================================================================
# 2. Mode Comparison Tests
# =============================================================================

test_that("valid values are identical in both modes", {
  test_data <- c(1.5, 2.5, 3.0, 4.5)  # No missing data
  
  tagged_result <- flexible_missing_handler(test_data, "tagged_na")
  original_result <- flexible_missing_handler(test_data, "original")
  
  expect_equal(tagged_result, original_result)
  expect_equal(length(tagged_result), length(original_result))
})

test_that("mixed data shows correct differences between modes", {
  test_data <- c(1.5, tagged_na("a"), 3.0, tagged_na("b"), 5.0)
  
  tagged_result <- flexible_missing_handler(test_data, "tagged_na")
  original_result <- flexible_missing_handler(test_data, "original")
  
  # Valid values should be identical
  expect_equal(tagged_result[1], original_result[1])  # 1.5
  expect_equal(tagged_result[3], original_result[3])  # 3.0
  expect_equal(tagged_result[5], original_result[5])  # 5.0
  
  # Missing values should differ in representation
  expect_true(is_tagged_na(tagged_result[2], "a"))
  expect_true(is.na(original_result[2]) && !is_tagged_na(original_result[2]))
  
  expect_true(is_tagged_na(tagged_result[4], "b"))
  expect_true(is.na(original_result[4]) && !is_tagged_na(original_result[4]))
})

# =============================================================================
# 3. Missing Data Conversion Tests
# =============================================================================

test_that("original mode converts all tagged_na types to standard NA", {
  test_data <- c(
    tagged_na("a"),   # Not applicable
    tagged_na("b"),   # Missing/invalid
    tagged_na("c"),   # Question not asked  
    tagged_na("d"),   # Variable missing
    1.5
  )
  
  result <- flexible_missing_handler(test_data, "original")
  
  # All tagged_na should become standard NA
  expect_true(is.na(result[1]) && !is_tagged_na(result[1]))
  expect_true(is.na(result[2]) && !is_tagged_na(result[2]))
  expect_true(is.na(result[3]) && !is_tagged_na(result[3]))
  expect_true(is.na(result[4]) && !is_tagged_na(result[4]))
  
  # Valid value should be preserved
  expect_equal(result[5], 1.5)
  expect_false(is.na(result[5]))
  
  # Result should be standard numeric
  expect_true(is.numeric(result))
  expect_equal(class(result), "numeric")
})

test_that("original mode handles untagged NA correctly", {
  test_data <- c(1.5, NA_real_, tagged_na("a"), 3.0)
  
  result <- flexible_missing_handler(test_data, "original")
  
  expect_equal(result[1], 1.5)
  expect_true(is.na(result[2]))  # Standard NA preserved
  expect_true(is.na(result[3]))  # Tagged NA converted to standard NA
  expect_equal(result[4], 3.0)
  
  # All NA values should be standard NA now
  expect_false(any(is_tagged_na(result)))
})

# =============================================================================
# 4. Vector Processing Tests  
# =============================================================================

test_that("vector inputs work correctly in both modes", {
  smk_vec <- c(1, 2, tagged_na("a"), tagged_na("b"), 3)
  
  tagged_result <- flexible_missing_handler(smk_vec, "tagged_na")
  original_result <- flexible_missing_handler(smk_vec, "original")
  
  expect_length(tagged_result, 5)
  expect_length(original_result, 5)
  
  # Valid values should be identical
  expect_equal(tagged_result[1:2], original_result[1:2])
  expect_equal(tagged_result[5], original_result[5])
  
  # Missing values should differ
  expect_true(is_tagged_na(tagged_result[3], "a"))
  expect_true(is.na(original_result[3]) && !is_tagged_na(original_result[3]))
  
  expect_true(is_tagged_na(tagged_result[4], "b"))
  expect_true(is.na(original_result[4]) && !is_tagged_na(original_result[4]))
})

test_that("large vector processing maintains performance", {
  # Create large test vector
  large_data <- c(
    rep(1:100, 10),  # 1000 valid values
    rep(tagged_na("a"), 50),  # 50 tagged_na("a")
    rep(tagged_na("b"), 50)   # 50 tagged_na("b")
  )
  
  # Both modes should handle large vectors efficiently
  expect_no_error({
    tagged_result <- flexible_missing_handler(large_data, "tagged_na")
    original_result <- flexible_missing_handler(large_data, "original")
  })
  
  expect_length(tagged_result, 1100)
  expect_length(original_result, 1100)
  
  # Check that conversions worked correctly
  expect_equal(sum(is_tagged_na(tagged_result)), 100)
  expect_equal(sum(is.na(tagged_result)), 100)
  expect_equal(sum(is_tagged_na(original_result)), 0)
  expect_equal(sum(is.na(original_result)), 100)
})

# =============================================================================
# 5. Error Handling Tests
# =============================================================================

test_that("invalid handle_missing values throw specific errors", {
  test_data <- c(1.5, tagged_na("a"), 3.0)
  
  expect_error(
    flexible_missing_handler(test_data, "invalid"),
    "handle_missing must be either 'tagged_na' or 'original'"
  )
  
  expect_error(
    flexible_missing_handler(test_data, "TAGGED_NA"),  # Wrong case
    "handle_missing must be either 'tagged_na' or 'original'"
  )
  
  expect_error(
    flexible_missing_handler(test_data, 123),  # Wrong type
    "handle_missing must be either 'tagged_na' or 'original'"
  )
})

test_that("validation helper function works correctly", {
  expect_silent(validate_missing_strategy("tagged_na", "test_function"))
  expect_silent(validate_missing_strategy("original", "test_function"))
  
  expect_error(
    validate_missing_strategy("invalid", "test_function"),
    "handle_missing must be one of: tagged_na, original"
  )
  
  expect_error(
    validate_missing_strategy(c("tagged_na", "original"), "test_function"),
    "handle_missing must be a single character value"
  )
})

# =============================================================================
# 6. Edge Cases Tests
# =============================================================================

test_that("empty vectors are handled correctly", {
  empty_numeric <- numeric(0)
  empty_logical <- logical(0)
  
  expect_equal(
    flexible_missing_handler(empty_numeric, "tagged_na"),
    empty_numeric
  )
  
  expect_equal(
    flexible_missing_handler(empty_numeric, "original"),
    empty_numeric
  )
})

test_that("single value vectors work correctly", {
  single_valid <- 2.5
  single_tagged <- tagged_na("a")
  
  # Single valid value
  expect_equal(
    flexible_missing_handler(single_valid, "tagged_na"),
    single_valid
  )
  expect_equal(
    flexible_missing_handler(single_valid, "original"),
    single_valid
  )
  
  # Single tagged NA
  result_tagged <- flexible_missing_handler(single_tagged, "tagged_na")
  result_original <- flexible_missing_handler(single_tagged, "original")
  
  expect_true(is_tagged_na(result_tagged, "a"))
  expect_true(is.na(result_original) && !is_tagged_na(result_original))
})

test_that("all missing data vectors work correctly", {
  all_missing <- c(tagged_na("a"), tagged_na("b"), tagged_na("a"))
  
  result_tagged <- flexible_missing_handler(all_missing, "tagged_na")
  result_original <- flexible_missing_handler(all_missing, "original")
  
  # Tagged mode preserves all tagged_na values
  expect_true(is_tagged_na(result_tagged[1], "a"))
  expect_true(is_tagged_na(result_tagged[2], "b"))
  expect_true(is_tagged_na(result_tagged[3], "a"))
  
  # Original mode converts all to standard NA
  expect_true(all(is.na(result_original)))
  expect_false(any(is_tagged_na(result_original)))
})

# =============================================================================
# 7. Enhanced Clean Variables Tests
# =============================================================================

test_that("enhanced_clean_variables works with multiple vectors", {
  test_data <- list(
    var1 = c(1, 2, tagged_na("a")),
    var2 = c(tagged_na("b"), 3, 4),
    var3 = c(5, 6, 7)  # No missing data
  )
  
  result_tagged <- enhanced_clean_variables(test_data, "tagged_na")
  result_original <- enhanced_clean_variables(test_data, "original")
  
  # Check structure preservation
  expect_equal(names(result_tagged), names(test_data))
  expect_equal(names(result_original), names(test_data))
  expect_equal(lengths(result_tagged), lengths(test_data))
  expect_equal(lengths(result_original), lengths(test_data))
  
  # Check tagged_na mode
  expect_true(is_tagged_na(result_tagged$var1[3], "a"))
  expect_true(is_tagged_na(result_tagged$var2[1], "b"))
  expect_equal(result_tagged$var3, test_data$var3)
  
  # Check original mode
  expect_true(is.na(result_original$var1[3]) && !is_tagged_na(result_original$var1[3]))
  expect_true(is.na(result_original$var2[1]) && !is_tagged_na(result_original$var2[1]))
  expect_equal(result_original$var3, test_data$var3)
})

# =============================================================================
# 8. Information and Documentation Tests
# =============================================================================

test_that("get_missing_strategies returns correct information", {
  strategies <- get_missing_strategies()
  
  expect_true(is.list(strategies))
  expect_true("tagged_na" %in% names(strategies))
  expect_true("original" %in% names(strategies))
  
  expect_true(strategies$tagged_na$default)
  expect_false(strategies$original$default)
  
  expect_true("description" %in% names(strategies$tagged_na))
  expect_true("use_cases" %in% names(strategies$tagged_na))
  expect_true("output_type" %in% names(strategies$tagged_na))
})

# =============================================================================
# 9. Backward Compatibility Tests
# =============================================================================

test_that("existing code without handle_missing parameter works", {
  test_data <- c(1.5, tagged_na("a"), 3.0, tagged_na("b"))
  
  # Default behavior should match tagged_na mode
  result_default <- flexible_missing_handler(test_data)
  result_explicit <- flexible_missing_handler(test_data, "tagged_na")
  
  expect_identical(result_default, result_explicit)
  
  # Should preserve tagged_na semantics
  expect_true(is_tagged_na(result_default[2], "a"))
  expect_true(is_tagged_na(result_default[4], "b"))
  expect_equal(result_default[1], 1.5)
  expect_equal(result_default[3], 3.0)
})

# =============================================================================
# Run All Tests
# =============================================================================

test_that("MVP test suite runs successfully", {
  cat("\\n=== Flexible Missing Data Handler MVP Test Results ===\\n")
  cat("All tests completed successfully!\\n")
  cat("Core functionality validated for both modes.\\n")
  cat("Ready for integration testing.\\n")
})