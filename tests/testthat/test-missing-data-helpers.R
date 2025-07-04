# Comprehensive Missing Data Helpers Tests - v3.0.0 Architecture
# Tests all missing data preprocessing functions and generic helpers

library(testthat)
library(haven)
library(dplyr)

# Test context: Load missing data helper functions
# Note: Functions loaded via devtools::load_all() or source commands

# ==============================================================================
# 1. CORE PREPROCESSING FUNCTION TESTS
# ==============================================================================

test_that("preprocess_cchs_missing_codes() handles standard_response pattern correctly", {
  # Test standard response pattern (6,7,8,9)
  test_data <- c(1, 2, 3, 6, 7, 8, 9, "NA(a)", "NA(b)", NA)
  
  result <- preprocess_cchs_missing_codes(test_data, "standard_response")
  
  expect_equal(length(result), 10)
  expect_equal(result[1], 1)  # Valid response preserved
  expect_equal(result[2], 2)  # Valid response preserved
  expect_equal(result[3], 3)  # Valid response preserved
  expect_true(haven::is_tagged_na(result[4], "a"))  # 6 -> NA(a) 
  expect_true(haven::is_tagged_na(result[5], "b"))  # 7 -> NA(b)
  expect_true(haven::is_tagged_na(result[6], "b"))  # 8 -> NA(b)
  expect_true(haven::is_tagged_na(result[7], "b"))  # 9 -> NA(b)
  expect_true(haven::is_tagged_na(result[8], "a"))  # "NA(a)" -> NA(a)
  expect_true(haven::is_tagged_na(result[9], "b"))  # "NA(b)" -> NA(b)
})

test_that("preprocess_cchs_missing_codes() handles categorical_age pattern correctly", {
  # Test categorical age pattern (96,97,98,99)
  test_data <- c(1, 5, 8, 11, 96, 97, 98, 99, "NA(a)")
  
  result <- preprocess_cchs_missing_codes(test_data, "categorical_age")
  
  expect_equal(length(result), 9)
  expect_equal(result[1], 1)   # Valid response preserved
  expect_equal(result[2], 5)   # Valid response preserved
  expect_equal(result[3], 8)   # Valid response preserved
  expect_equal(result[4], 11)  # Valid response preserved
  expect_true(haven::is_tagged_na(result[5], "a"))  # 96 -> NA(a)
  expect_true(haven::is_tagged_na(result[6], "b"))  # 97 -> NA(b)
  expect_true(haven::is_tagged_na(result[7], "b"))  # 98 -> NA(b)
  expect_true(haven::is_tagged_na(result[8], "b"))  # 99 -> NA(b)
  expect_true(haven::is_tagged_na(result[9], "a"))  # "NA(a)" -> NA(a)
})

test_that("preprocess_cchs_missing_codes() handles continuous_standard pattern correctly", {
  # Test continuous pattern (996,997,998,999)
  test_data <- c(15, 25.5, 996, 997, 998, 999, "NA(b)")
  
  result <- preprocess_cchs_missing_codes(test_data, "continuous_standard")
  
  expect_equal(length(result), 7)
  expect_equal(result[1], 15)    # Valid response preserved
  expect_equal(result[2], 25.5)  # Valid response preserved
  expect_true(haven::is_tagged_na(result[3], "a"))  # 996 -> NA(a)
  expect_true(haven::is_tagged_na(result[4], "b"))  # 997 -> NA(b)
  expect_true(haven::is_tagged_na(result[5], "b"))  # 998 -> NA(b)
  expect_true(haven::is_tagged_na(result[6], "b"))  # 999 -> NA(b)
  expect_true(haven::is_tagged_na(result[7], "b"))  # "NA(b)" -> NA(b)
})

test_that("has_cchs_missing_codes() detects missing codes correctly", {
  # Test standard response pattern
  expect_true(has_cchs_missing_codes(c(1, 2, 6, 7), "standard_response"))
  expect_false(has_cchs_missing_codes(c(1, 2, 3, 4), "standard_response"))
  
  # Test categorical age pattern
  expect_true(has_cchs_missing_codes(c(1, 5, 96, 97), "categorical_age"))
  expect_false(has_cchs_missing_codes(c(1, 5, 8, 11), "categorical_age"))
  
  # Test continuous pattern
  expect_true(has_cchs_missing_codes(c(15, 25, 996, 997), "continuous_standard"))
  expect_false(has_cchs_missing_codes(c(15, 25, 30, 35), "continuous_standard"))
  
  # Test all patterns (NULL pattern_type)
  expect_true(has_cchs_missing_codes(c(1, 2, 6, 7)))    # Standard codes
  expect_true(has_cchs_missing_codes(c(1, 5, 96, 97)))  # Categorical codes
  expect_true(has_cchs_missing_codes(c(15, 996, 997)))  # Continuous codes
  expect_false(has_cchs_missing_codes(c(1, 2, 3, 4)))   # No missing codes
})

test_that("validate_missing_code_preprocessing() validates correctly", {
  # Test valid preprocessing
  original <- c(1, 2, 3, 6, 7, 8, 9)
  processed <- preprocess_cchs_missing_codes(original, "standard_response")
  
  is_valid <- validate_missing_code_preprocessing(original, processed, "standard_response")
  expect_true(is_valid)
  
  # Test with detailed report
  detailed_report <- validate_missing_code_preprocessing(original, processed, "standard_response", report_details = TRUE)
  
  expect_true(is.list(detailed_report))
  expect_true(detailed_report$is_valid)
  expect_true(detailed_report$original_missing_present)
  expect_false(detailed_report$processed_missing_remaining)
  expect_true(detailed_report$length_preserved)
  expect_equal(detailed_report$na_a_count, 1)  # One 6 -> NA(a)
  expect_equal(detailed_report$na_b_count, 3)  # Three codes (7,8,9) -> NA(b)
})

# ==============================================================================
# 2. COMPREHENSIVE INTEGRATION TESTS
# ==============================================================================

test_that("missing data helpers work together in complete workflow", {
  # Test complete workflow: detection -> preprocessing -> validation
  original_data <- c(1, 2, 3, 6, 7, 8, 9, "NA(a)", NA)
  
  # Step 1: Detect if preprocessing needed
  needs_prep <- needs_preprocessing(original_data)
  expect_true(needs_prep)
  
  # Step 2: Check for specific missing codes
  has_missing <- has_cchs_missing_codes(original_data, "standard_response")
  expect_true(has_missing)
  
  # Step 3: Apply preprocessing
  processed_data <- preprocess_cchs_missing_codes(original_data, "standard_response")
  
  # Step 4: Validate preprocessing was successful
  is_valid <- validate_missing_code_preprocessing(original_data, processed_data, "standard_response")
  expect_true(is_valid)
  
  # Step 5: Verify final results
  expect_equal(length(processed_data), length(original_data))
  expect_equal(processed_data[1], 1)  # Valid responses preserved
  expect_equal(processed_data[2], 2)
  expect_equal(processed_data[3], 3)
  expect_true(haven::is_tagged_na(processed_data[4], "a"))  # 6 -> NA(a)
  expect_true(haven::is_tagged_na(processed_data[5], "b"))  # 7 -> NA(b)
  expect_true(haven::is_tagged_na(processed_data[6], "b"))  # 8 -> NA(b)
  expect_true(haven::is_tagged_na(processed_data[7], "b"))  # 9 -> NA(b)
  expect_true(haven::is_tagged_na(processed_data[8], "a"))  # "NA(a)" -> NA(a)
})