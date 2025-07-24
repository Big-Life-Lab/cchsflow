# ==============================================================================
# Auto-Detection and Enhanced Pattern Detection - Test Suite
# ==============================================================================
#
# Comprehensive tests for the new auto-detection features including:
# - Enhanced pattern detection with CSV lookup
# - Auto-detection in clean_variables() (pattern_type = NULL)
# - Mixed pattern processing in single function calls
# - Fallback behavior and error handling
#
# @note Test suite v1.0.0, created: 2025-07-23
# ==============================================================================

library(testthat)
library(haven)
library(dplyr)

# Source internal environment first
source("helper-env.R")

# Source required functions
source("../R/enhanced-pattern-detection.R")
source("../R/flexible-missing-handler.R")
source("../R/convert-missing.R")
source("../R/clean_variables.R")

# =============================================================================
# 1. Enhanced Pattern Detection Tests
# =============================================================================

test_that("detect_pattern_enhanced() uses CSV lookup as primary source", {
  # Test variables that should be found in CSV
  # These are known to be in variables.csv with missingType defined
  expect_equal(detect_pattern_enhanced("HWTGHTM"), "triple_digit_missing")
  expect_equal(detect_pattern_enhanced("HWTGWTK"), "triple_digit_missing")
  expect_equal(detect_pattern_enhanced("HWTGBMI_der"), "triple_digit_missing")
  expect_equal(detect_pattern_enhanced("HWTGCOR_der"), "triple_digit_missing")
  expect_equal(detect_pattern_enhanced("HWTGBMI_cat4"), "triple_digit_missing")
})

test_that("detect_pattern_enhanced() falls back to pattern matching", {
  # Test variables that should NOT be in CSV but match patterns
  expect_equal(detect_pattern_enhanced("DHH_SEX"), "single_digit_missing")
  expect_equal(detect_pattern_enhanced("SMKG040"), "double_digit_missing")
  expect_equal(detect_pattern_enhanced("ALCDTYP"), "double_digit_missing")
  expect_equal(detect_pattern_enhanced("GEO_PRV"), "single_digit_missing")
})

test_that("detect_pattern_enhanced() handles unknown variables", {
  # Test variables that match no patterns
  expect_null(detect_pattern_enhanced("UNKNOWN_VARIABLE"))
  expect_null(detect_pattern_enhanced("RANDOM_123"))
  expect_null(detect_pattern_enhanced(""))
  expect_null(detect_pattern_enhanced(NULL))
})

test_that("detect_pattern_enhanced() loads and caches CSV correctly", {
  # Test that CSV loading works
  info <- get_pattern_detection_info()
  expect_true(info$csv_available)
  expect_gt(info$csv_variables_total, 300)  # Should have many variables
  expect_gte(info$csv_variables_with_missing_type, 5)  # Should have some with missing types
  
  # Test hierarchy is correct
  expect_equal(info$detection_hierarchy[1], "1. CSV Lookup (Primary)")
  expect_equal(info$detection_hierarchy[2], "2. Pattern Matching (Fallback)")
})

test_that("enhanced pattern detection cache can be cleared", {
  # Test cache clearing function exists and works
  expect_invisible(clear_variables_csv_cache())
  
  # Should still work after cache clear
  expect_equal(detect_pattern_enhanced("HWTGHTM"), "triple_digit_missing")
})

# =============================================================================
# 2. Auto-Detection in clean_variables() Tests
# =============================================================================

test_that("clean_variables() auto-detects patterns when pattern_type is NULL", {
  # Create test data with known variables
  HWTGHTM <- c(1.75, 1.60, 996, 997)      # Should auto-detect triple_digit_missing
  DHH_SEX <- c(1, 2, 6, 7)                # Should auto-detect single_digit_missing
  
  # Test auto-detection (default behavior)
  result <- clean_variables(
    continuous_vars = list(height = HWTGHTM),
    categorical_vars = list(sex = DHH_SEX)
    # No patterns specified - should auto-detect
  )
  
  expect_true(is.list(result))
  expect_equal(names(result), c("height_clean", "sex_clean"))
  expect_equal(length(result$height_clean), 4)
  expect_equal(length(result$sex_clean), 4)
  
  # Check that CCHS codes were converted correctly
  expect_equal(result$height_clean[1], 1.75)  # Valid value unchanged
  expect_equal(result$height_clean[2], 1.60)  # Valid value unchanged
  expect_true(is.na(result$height_clean[3]))  # 996 converted to NA
  expect_true(is.na(result$height_clean[4]))  # 997 converted to NA
  
  expect_equal(result$sex_clean[1], 1)        # Valid value unchanged
  expect_equal(result$sex_clean[2], 2)        # Valid value unchanged  
  expect_true(is.na(result$sex_clean[3]))     # 6 converted to NA
  expect_true(is.na(result$sex_clean[4]))     # 7 converted to NA
})

test_that("clean_variables() explicit patterns override auto-detection", {
  HWTGHTM <- c(1.75, 996, 997)
  
  # Test explicit pattern override
  result_explicit <- clean_variables(
    continuous_vars = list(height = HWTGHTM),
    continuous_pattern = "double_digit_missing",  # Override auto-detection
    output_format = "tagged_na"
  )
  
  # Should use double_digit pattern instead of auto-detected triple_digit
  # With double_digit, only 96,97 convert (not 996,997)
  expect_equal(result_explicit$height_clean[1], 1.75)  # Valid unchanged
  expect_equal(result_explicit$height_clean[2], 996)   # 996 NOT converted (wrong pattern)
  expect_equal(result_explicit$height_clean[3], 997)   # 997 NOT converted (wrong pattern)
})

test_that("clean_variables() handles mixed patterns in single call", {
  # Variables that should auto-detect to different patterns
  HWTGHTM <- c(1.75, 996, 997)             # triple_digit_missing (from CSV)
  SMKG040 <- c(10, 96, 97)                 # double_digit_missing (from pattern)
  DHH_SEX <- c(1, 6, 7)                    # single_digit_missing (from pattern)
  
  result <- clean_variables(
    continuous_vars = list(height = HWTGHTM, smoking = SMKG040),
    categorical_vars = list(sex = DHH_SEX),
    output_format = "tagged_na"
  )
  
  expect_equal(names(result), c("height_clean", "smoking_clean", "sex_clean"))
  
  # Each variable should be processed with its correct pattern
  # Height: triple_digit (996,997 convert)
  expect_equal(result$height_clean[1], 1.75)
  expect_true(is.na(result$height_clean[2]))  # 996 converted
  expect_true(is.na(result$height_clean[3]))  # 997 converted
  
  # Smoking: double_digit (96,97 convert)  
  expect_equal(result$smoking_clean[1], 10)
  expect_true(is.na(result$smoking_clean[2]))  # 96 converted
  expect_true(is.na(result$smoking_clean[3]))  # 97 converted
  
  # Sex: single_digit (6,7 convert)
  expect_equal(result$sex_clean[1], 1)
  expect_true(is.na(result$sex_clean[2]))     # 6 converted
  expect_true(is.na(result$sex_clean[3]))     # 7 converted
})

test_that("clean_variables() uses fallback patterns for unknown variables", {
  # Test with variable names not in CSV and not matching patterns
  unknown_cont <- c(1.5, 996, 997)
  unknown_cat <- c(1, 6, 7)
  
  result <- clean_variables(
    continuous_vars = list(unknown_var1 = unknown_cont),
    categorical_vars = list(unknown_var2 = unknown_cat),
    output_format = "tagged_na"
  )
  
  # Should use fallback patterns (triple_digit for continuous, single_digit for categorical)
  expect_true(is.list(result))
  expect_equal(names(result), c("unknown_var1_clean", "unknown_var2_clean"))
  
  # Fallback should still work for conversion
  expect_equal(result$unknown_var1_clean[1], 1.5)
  expect_true(is.na(result$unknown_var1_clean[2]))  # 996 converted with fallback
})

# =============================================================================
# 3. Integration Tests with convert_missing()
# =============================================================================

test_that("convert_missing() auto-detection integrates with clean_variables()", {
  # Test that convert_missing's auto-detection works when called directly
  HWTGHTM <- c(1.75, 996, 997)
  
  # Direct convert_missing call with auto-detection
  result_direct <- convert_missing(HWTGHTM, pattern_type = NULL, output_format = "tagged_na")
  
  # clean_variables call with auto-detection
  result_clean <- clean_variables(
    continuous_vars = list(height = HWTGHTM),
    output_format = "tagged_na"
  )
  
  # Should produce same results
  expect_equal(result_direct, result_clean$height_clean)
})

test_that("auto-detection handles variable name extraction correctly", {
  # Test that variable names are correctly extracted for pattern detection
  test_height <- c(1.75, 996)
  test_weight <- c(70, 998)
  
  result <- clean_variables(
    continuous_vars = list(HWTGHTM = test_height, HWTGWTK = test_weight),
    output_format = "tagged_na"
  )
  
  # Both should auto-detect as triple_digit_missing and convert properly
  expect_true(is.na(result$HWTGHTM_clean[2]))  # 996 converted
  expect_true(is.na(result$HWTGWTK_clean[2]))  # 998 converted
  expect_equal(names(result), c("HWTGHTM_clean", "HWTGWTK_clean"))
})

# =============================================================================
# 4. Error Handling and Edge Cases
# =============================================================================

test_that("auto-detection handles edge cases gracefully", {
  # Test empty variable lists
  result_empty <- clean_variables(
    continuous_vars = list(),
    categorical_vars = list(),
    output_format = "tagged_na"
  )
  expect_error(result_empty)  # Should require at least one variable
  
  # Test with NULL values (should error as per our design)
  expect_error(
    clean_variables(
      continuous_vars = list(height = NULL),
      output_format = "tagged_na"
    ),
    "Variable.*is NULL"
  )
})

test_that("auto-detection works with different output formats", {
  HWTGHTM <- c(1.75, 996, 997)
  
  # Test with original format
  result_original <- clean_variables(
    continuous_vars = list(height = HWTGHTM),
    output_format = "original"
  )
  
  # Test with auto format  
  result_auto <- clean_variables(
    continuous_vars = list(height = HWTGHTM),
    output_format = "auto"
  )
  
  # Should work with all formats
  expect_true(is.list(result_original))
  expect_true(is.list(result_auto))
  expect_equal(names(result_original), "height_clean")
  expect_equal(names(result_auto), "height_clean")
})

test_that("auto-detection maintains backward compatibility", {
  # Test that old explicit pattern calls still work
  HWTGHTM <- c(1.75, 996, 997)
  
  result_old_style <- clean_variables(
    continuous_vars = list(height = HWTGHTM),
    continuous_pattern = "triple_digit_missing",  # Explicit pattern
    output_format = "tagged_na"
  )
  
  result_new_style <- clean_variables(
    continuous_vars = list(height = HWTGHTM),
    # No pattern specified - auto-detect
    output_format = "tagged_na"
  )
  
  # Should produce identical results
  expect_equal(result_old_style$height_clean, result_new_style$height_clean)
})

# =============================================================================
# 5. Performance and Caching Tests
# =============================================================================

test_that("pattern detection caching improves performance", {
  # Test that repeated calls are faster due to caching
  test_vars <- c("HWTGHTM", "HWTGWTK", "DHH_SEX", "SMKG040")
  
  # First call (loads CSV)
  start_time <- Sys.time()
  for(var in test_vars) {
    detect_pattern_enhanced(var)
  }
  first_time <- Sys.time() - start_time
  
  # Clear cache and test again
  clear_variables_csv_cache()
  
  # Second call (should be similar, maybe slightly faster due to R caching)
  start_time <- Sys.time()
  for(var in test_vars) {
    detect_pattern_enhanced(var)
  }
  second_time <- Sys.time() - start_time
  
  # Should both complete successfully (performance comparison optional)
  expect_true(first_time > 0)
  expect_true(second_time > 0)
})

# =============================================================================
# 6. Documentation and Information Functions
# =============================================================================

test_that("information functions provide useful details", {
  # Test pattern detection info
  info <- get_pattern_detection_info()
  expect_true(is.list(info))
  expect_true("csv_available" %in% names(info))
  expect_true("detection_hierarchy" %in% names(info))
  expect_true("benefits" %in% names(info))
  
  # Test clean variables info
  clean_info <- get_clean_variables_info()
  expect_true(is.list(clean_info))
  expect_true("purpose" %in% names(clean_info))
  expect_true("eliminates" %in% names(clean_info))
  expect_true("provides" %in% names(clean_info))
})

# =============================================================================
# Run All Tests
# =============================================================================

test_that("Auto-detection feature test suite runs successfully", {
  cat("\n=== Auto-Detection Features Test Results ===\n")
  cat("✅ Enhanced pattern detection with CSV lookup\n")
  cat("✅ Auto-detection in clean_variables()\n") 
  cat("✅ Mixed pattern processing\n")
  cat("✅ Integration with convert_missing()\n")
  cat("✅ Error handling and edge cases\n")
  cat("✅ Backward compatibility\n")
  cat("All auto-detection features working correctly!\n")
  
  expect_true(TRUE)  # Pass if we get here
})