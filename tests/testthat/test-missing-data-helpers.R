# Test suite for CCHS missing data preprocessing helpers
# Based on inst/metadata/schemas/cchs/cchs_missing_data.yaml specification

library(testthat)
library(haven)
library(dplyr)

# Source the helper functions
source("../../R/missing-data-helpers.R")

# ============================================================================
# CORE PREPROCESSING FUNCTION TESTS
# ============================================================================

test_that("preprocess_cchs_missing_codes() handles standard response pattern correctly", {
  
  # Test data with original CCHS codes
  test_data <- c(1, 2, 3, 6, 7, 8, 9, "NA(a)", "NA(b)")
  result <- preprocess_cchs_missing_codes(test_data, "standard_response")
  
  # Valid responses preserved as numeric
  expect_equal(result[1], 1)
  expect_equal(result[2], 2) 
  expect_equal(result[3], 3)
  
  # Original missing codes converted correctly
  expect_true(is_tagged_na(result[4], "a"))  # 6 → NA::a (not applicable)
  expect_true(is_tagged_na(result[5], "b"))  # 7 → NA::b (don't know)
  expect_true(is_tagged_na(result[6], "b"))  # 8 → NA::b (refusal)
  expect_true(is_tagged_na(result[7], "b"))  # 9 → NA::b (not stated)
  
  # String-based NA values converted
  expect_true(is_tagged_na(result[8], "a"))  # "NA(a)" → tagged_na("a")
  expect_true(is_tagged_na(result[9], "b"))  # "NA(b)" → tagged_na("b")
})

test_that("preprocess_cchs_missing_codes() handles categorical age pattern correctly", {
  
  # Test data with age category missing codes
  test_data <- c(1, 5, 8, 11, 96, 97, 98, 99, "NA(a)")
  result <- preprocess_cchs_missing_codes(test_data, "categorical_age")
  
  # Valid age categories preserved
  expect_equal(result[1], 1)
  expect_equal(result[2], 5)
  expect_equal(result[3], 8) 
  expect_equal(result[4], 11)
  
  # Extended missing codes converted correctly
  expect_true(is_tagged_na(result[5], "a"))  # 96 → NA::a (not applicable)
  expect_true(is_tagged_na(result[6], "b"))  # 97 → NA::b (don't know)
  expect_true(is_tagged_na(result[7], "b"))  # 98 → NA::b (refusal) 
  expect_true(is_tagged_na(result[8], "b"))  # 99 → NA::b (not stated)
  expect_true(is_tagged_na(result[9], "a"))  # "NA(a)" → tagged_na("a")
})

test_that("preprocess_cchs_missing_codes() handles continuous pattern correctly", {
  
  # Test data with continuous variable missing codes
  test_data <- c(15, 25, 50, 996, 997, 998, 999, "NA(b)")
  result <- preprocess_cchs_missing_codes(test_data, "continuous_standard")
  
  # Valid measurements preserved
  expect_equal(result[1], 15)
  expect_equal(result[2], 25)
  expect_equal(result[3], 50)
  
  # Extended missing codes converted correctly  
  expect_true(is_tagged_na(result[4], "a"))  # 996 → NA::a (not applicable)
  expect_true(is_tagged_na(result[5], "b"))  # 997 → NA::b (don't know)
  expect_true(is_tagged_na(result[6], "b"))  # 998 → NA::b (refusal)
  expect_true(is_tagged_na(result[7], "b"))  # 999 → NA::b (not stated)
  expect_true(is_tagged_na(result[8], "b"))  # "NA(b)" → tagged_na("b")
})

test_that("preprocess_cchs_missing_codes() preserves existing haven::tagged_na values", {
  
  # Test data with existing tagged NA values
  test_data <- c(1, 2, tagged_na("a"), tagged_na("b"), 6, 7)
  result <- preprocess_cchs_missing_codes(test_data, "standard_response")
  
  # Valid responses preserved
  expect_equal(result[1], 1)
  expect_equal(result[2], 2)
  
  # Existing tagged NA values preserved
  expect_true(is_tagged_na(result[3], "a"))
  expect_true(is_tagged_na(result[4], "b"))
  
  # Original missing codes still converted
  expect_true(is_tagged_na(result[5], "a"))  # 6 → NA::a
  expect_true(is_tagged_na(result[6], "b"))  # 7 → NA::b
})

test_that("preprocess_cchs_missing_codes() validates pattern_type parameter", {
  
  test_data <- c(1, 2, 6, 7)
  
  # Valid pattern types should work
  expect_no_error(preprocess_cchs_missing_codes(test_data, "standard_response"))
  expect_no_error(preprocess_cchs_missing_codes(test_data, "categorical_age"))
  expect_no_error(preprocess_cchs_missing_codes(test_data, "continuous_standard"))
  
  # Invalid pattern type should error
  expect_error(preprocess_cchs_missing_codes(test_data, "invalid_pattern"))
})

# ============================================================================
# PATTERN-SPECIFIC HELPER FUNCTION TESTS
# ============================================================================

test_that("preprocess_standard_response() works correctly for SMK_005-type variables", {
  
  # Simulate SMK_005 data (1=daily, 2=occasionally, 3=not at all, 6-9=missing)
  smk_005_data <- c(1, 2, 3, 6, 7, 8, 9, "NA(a)")
  result <- preprocess_standard_response(smk_005_data)
  
  # Valid smoking status preserved
  expect_equal(result[1], 1)  # daily
  expect_equal(result[2], 2)  # occasionally  
  expect_equal(result[3], 3)  # not at all
  
  # Missing codes converted appropriately
  expect_true(is_tagged_na(result[4], "a"))  # 6 = not applicable
  expect_true(is_tagged_na(result[5], "b"))  # 7 = don't know
  expect_true(is_tagged_na(result[6], "b"))  # 8 = refusal
  expect_true(is_tagged_na(result[7], "b"))  # 9 = not stated
  expect_true(is_tagged_na(result[8], "a"))  # "NA(a)" preserved
})

test_that("preprocess_categorical_age() works correctly for SMKG203-type variables", {
  
  # Simulate SMKG203 data (age categories 1-11, 96-99=missing)
  smkg203_data <- c(1, 5, 8, 11, 96, 97, 98, 99)
  result <- preprocess_categorical_age(smkg203_data)
  
  # Valid age categories preserved
  expect_equal(result[1], 1)   # youngest category
  expect_equal(result[2], 5)   # middle category
  expect_equal(result[3], 8)   # another category
  expect_equal(result[4], 11)  # oldest category
  
  # Extended missing codes converted
  expect_true(is_tagged_na(result[5], "a"))  # 96 = not applicable (non-smokers)
  expect_true(is_tagged_na(result[6], "b"))  # 97 = don't know age
  expect_true(is_tagged_na(result[7], "b"))  # 98 = refusal 
  expect_true(is_tagged_na(result[8], "b"))  # 99 = not stated
})

test_that("preprocess_continuous_standard() works correctly for SMK_204-type variables", {
  
  # Simulate SMK_204 data (cigarettes per day: 0-150, 996-999=missing)
  smk_204_data <- c(0, 10, 20, 50, 996, 997, 998, 999)
  result <- preprocess_continuous_standard(smk_204_data)
  
  # Valid cigarette counts preserved
  expect_equal(result[1], 0)   # non-daily (edge case)
  expect_equal(result[2], 10)  # light smoker
  expect_equal(result[3], 20)  # pack per day
  expect_equal(result[4], 50)  # heavy smoker
  
  # Extended missing codes converted  
  expect_true(is_tagged_na(result[5], "a"))  # 996 = not applicable (non-daily)
  expect_true(is_tagged_na(result[6], "b"))  # 997 = don't know count
  expect_true(is_tagged_na(result[7], "b"))  # 998 = refusal
  expect_true(is_tagged_na(result[8], "b"))  # 999 = not stated
})

# ============================================================================
# DOMAIN-SPECIFIC HELPER FUNCTION TESTS
# ============================================================================

test_that("preprocess_smoking_variable() auto-detects patterns correctly", {
  
  # Test standard response variables
  smk_005_data <- c(1, 2, 3, 6, 7)
  result_005 <- preprocess_smoking_variable(smk_005_data, variable_name = "SMK_005")
  expect_true(is_tagged_na(result_005[4], "a"))  # 6 → NA::a
  expect_true(is_tagged_na(result_005[5], "b"))  # 7 → NA::b
  
  # Test categorical age variables
  smkg203_data <- c(1, 5, 96, 97)
  result_203 <- preprocess_smoking_variable(smkg203_data, variable_name = "SMKG203")
  expect_true(is_tagged_na(result_203[3], "a"))  # 96 → NA::a
  expect_true(is_tagged_na(result_203[4], "b"))  # 97 → NA::b
  
  # Test continuous variables
  smk_204_data <- c(15, 20, 996, 997)
  result_204 <- preprocess_smoking_variable(smk_204_data, variable_name = "SMK_204")
  expect_true(is_tagged_na(result_204[3], "a"))  # 996 → NA::a
  expect_true(is_tagged_na(result_204[4], "b"))  # 997 → NA::b
})

test_that("preprocess_smoking_variable() handles manual pattern specification", {
  
  test_data <- c(1, 2, 6, 7)
  
  # Manual pattern override
  result <- preprocess_smoking_variable(test_data, pattern_type = "standard_response")
  expect_true(is_tagged_na(result[3], "a"))  # 6 → NA::a
  expect_true(is_tagged_na(result[4], "b"))  # 7 → NA::b
})

test_that("preprocess_smoking_variable() warns for unknown variables", {
  
  unknown_data <- c(1, 2, 6, 7)
  
  # Should warn but still process with default pattern
  expect_warning(
    result <- preprocess_smoking_variable(unknown_data, variable_name = "UNKNOWN_VAR"),
    "Unknown smoking variable.*using standard_response pattern"
  )
  
  # Should still process correctly with default
  expect_true(is_tagged_na(result[3], "a"))
  expect_true(is_tagged_na(result[4], "b"))
})

test_that("preprocess_smoking_variable() requires either variable_name or pattern_type", {
  
  test_data <- c(1, 2, 6, 7)
  
  # Should error if neither parameter provided
  expect_error(
    preprocess_smoking_variable(test_data),
    "Either variable_name or pattern_type must be specified"
  )
})

# ============================================================================
# UTILITY AND VALIDATION FUNCTION TESTS
# ============================================================================

test_that("validate_missing_code_preprocessing() validates correctly", {
  
  # Test successful preprocessing
  original <- c(1, 2, 3, 6, 7, 8, 9)
  processed <- preprocess_standard_response(original)
  
  # Basic validation should pass
  expect_true(validate_missing_code_preprocessing(original, processed, "standard_response"))
  
  # Detailed validation should provide comprehensive report
  detailed <- validate_missing_code_preprocessing(original, processed, "standard_response", 
                                                 report_details = TRUE)
  expect_true(detailed$is_valid)
  expect_true(detailed$original_missing_present)
  expect_false(detailed$processed_missing_remaining)
  expect_true(detailed$length_preserved)
  expect_equal(detailed$na_a_count, 1)  # One "6" code
  expect_equal(detailed$na_b_count, 3)  # Three "7,8,9" codes
})

test_that("validate_missing_code_preprocessing() detects invalid preprocessing", {
  
  # Test with incomplete preprocessing (original codes still present)
  original <- c(1, 2, 6, 7)
  incomplete_processed <- c(1, 2, 6, tagged_na("b"))  # 6 not converted
  
  # Should fail validation
  expect_false(validate_missing_code_preprocessing(original, incomplete_processed, "standard_response"))
  
  # Test with length mismatch
  length_mismatch <- c(1, 2)  # Shorter than original
  expect_false(validate_missing_code_preprocessing(original, length_mismatch, "standard_response"))
})

test_that("has_cchs_missing_codes() detects missing codes correctly", {
  
  # Standard response codes
  expect_true(has_cchs_missing_codes(c(1, 2, 6, 7)))
  expect_true(has_cchs_missing_codes(c(1, 2, 6, 7), "standard_response"))
  expect_false(has_cchs_missing_codes(c(1, 2, 6, 7), "categorical_age"))
  
  # Categorical age codes
  expect_true(has_cchs_missing_codes(c(1, 5, 96, 97)))
  expect_true(has_cchs_missing_codes(c(1, 5, 96, 97), "categorical_age"))
  expect_false(has_cchs_missing_codes(c(1, 5, 96, 97), "standard_response"))
  
  # Continuous codes
  expect_true(has_cchs_missing_codes(c(15, 25, 996, 997)))
  expect_true(has_cchs_missing_codes(c(15, 25, 996, 997), "continuous_standard"))
  expect_false(has_cchs_missing_codes(c(15, 25, 996, 997), "standard_response"))
  
  # No missing codes
  expect_false(has_cchs_missing_codes(c(1, 2, 3, NA)))
  expect_false(has_cchs_missing_codes(c(1, 2, 3, NA), "standard_response"))
  
  # Test invalid pattern type
  expect_error(has_cchs_missing_codes(c(1, 2, 6), "invalid_pattern"))
})

# ============================================================================
# INTEGRATION TESTS WITH REAL SMOKING FUNCTION PATTERNS
# ============================================================================

test_that("Missing data helpers integrate correctly with smoking function patterns", {
  
  # Test integration with SMKDSTY_fun pattern
  # Simulate mixed input: valid responses + original codes + string NA + haven NA
  mixed_smk_005 <- c(1, 2, 3, 6, 7, 8, 9, "NA(a)", tagged_na("b"))
  mixed_smk_030 <- c(1, 2, 6, 7, 8, 9, "NA(a)", tagged_na("a"), tagged_na("b"))
  mixed_smk_01a <- c(1, 2, 6, 7, 8, 9, "NA(a)", "NA(b)", tagged_na("a"))
  
  # Preprocess all variables
  proc_005 <- preprocess_standard_response(mixed_smk_005)
  proc_030 <- preprocess_standard_response(mixed_smk_030)  
  proc_01a <- preprocess_standard_response(mixed_smk_01a)
  
  # Validate preprocessing worked
  expect_false(has_cchs_missing_codes(proc_005))
  expect_false(has_cchs_missing_codes(proc_030))
  expect_false(has_cchs_missing_codes(proc_01a))
  
  # Verify we have appropriate mix of valid values and tagged NAs
  expect_true(any(!is.na(proc_005) & !is_tagged_na(proc_005)))  # Has valid values
  expect_true(any(is_tagged_na(proc_005, "a")))                # Has NA::a
  expect_true(any(is_tagged_na(proc_005, "b")))                # Has NA::b
})

test_that("Performance is acceptable for typical smoking function data sizes", {
  
  # Test with typical smoking function data size (realistic for CCHS cycles)
  n <- 10000
  large_data <- sample(c(1, 2, 3, 6, 7, 8, 9, "NA(a)", "NA(b)"), n, replace = TRUE)
  
  # Preprocessing should complete quickly (under 1 second for 10k rows)
  start_time <- Sys.time()
  result <- preprocess_standard_response(large_data)
  end_time <- Sys.time()
  processing_time <- as.numeric(end_time - start_time)
  
  expect_lt(processing_time, 1.0)  # Less than 1 second
  expect_equal(length(result), n)   # All values processed
  expect_false(has_cchs_missing_codes(result))  # No original codes remain
})

# ============================================================================
# EDGE CASE AND ERROR HANDLING TESTS
# ============================================================================

test_that("Helper functions handle edge cases gracefully", {
  
  # Empty vector
  expect_equal(length(preprocess_standard_response(c())), 0)
  
  # Single value
  expect_equal(preprocess_standard_response(1), 1)
  expect_true(is_tagged_na(preprocess_standard_response(6), "a"))
  
  # All missing codes
  all_missing <- c(6, 7, 8, 9)
  result <- preprocess_standard_response(all_missing)
  expect_true(is_tagged_na(result[1], "a"))  # 6 → NA::a
  expect_true(all(is_tagged_na(result[2:4], "b")))  # 7,8,9 → NA::b
  
  # Mixed data types (character + numeric)
  mixed_types <- c("1", "2", 6, 7, "NA(a)")
  result <- preprocess_standard_response(mixed_types)
  expect_equal(result[1], 1)  # String "1" converted to numeric
  expect_equal(result[2], 2)  # String "2" converted to numeric
  expect_true(is_tagged_na(result[3], "a"))  # 6 → NA::a
  expect_true(is_tagged_na(result[4], "b"))  # 7 → NA::b
  expect_true(is_tagged_na(result[5], "a"))  # "NA(a)" → tagged_na("a")
})

test_that("Helper functions maintain type safety", {
  
  # All results should be numeric or tagged_na (compatible with case_when)
  test_data <- c(1, 2, 6, 7, "3", "NA(a)", tagged_na("b"))
  result <- preprocess_standard_response(test_data)
  
  # Check that result is compatible with numeric operations
  numeric_part <- result[!is_tagged_na(result)]
  expect_true(is.numeric(numeric_part))
  
  # Check that tagged NA parts are proper haven tagged_na
  na_part <- result[is_tagged_na(result)]
  expect_true(all(is_tagged_na(na_part)))
})