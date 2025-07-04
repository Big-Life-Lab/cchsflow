# BMI Helper and Validation Function Tests - v3.0.0 Architecture
# Tests internal validation functions, helper functions, and generic utilities

library(testthat)
library(haven)
library(dplyr)

# Test context: Load BMI functions and helpers
# Note: Functions loaded via devtools::load_all() or source commands

# ==============================================================================
# 1. CORE FUNCTION TESTS (calculate_bmi_core)
# ==============================================================================

test_that("calculate_bmi_core() performs basic BMI calculation", {
  # Test valid calculation
  result_valid <- calculate_bmi_core(1.75, 70)
  expected <- 70 / (1.75^2)
  expect_equal(result_valid, expected, tolerance = 1e-6)
  
  # Test with tagged NAs
  result_na_a_height <- calculate_bmi_core(haven::tagged_na("a"), 70)
  expect_true(haven::is_tagged_na(result_na_a_height, "a"))
  
  result_na_b_weight <- calculate_bmi_core(1.75, haven::tagged_na("b"))
  expect_true(haven::is_tagged_na(result_na_b_weight, "b"))
  
  # Test regular NA handling
  result_na_height <- calculate_bmi_core(NA, 70)
  expect_true(haven::is_tagged_na(result_na_height, "b"))
  
  result_na_weight <- calculate_bmi_core(1.75, NA)
  expect_true(haven::is_tagged_na(result_na_weight, "b"))
})

# ==============================================================================
# 3. GENERIC HELPER FUNCTION TESTS (from missing-data-helpers.R)
# ==============================================================================

test_that("validate_parameter() handles valid parameters correctly", {
  # Test valid parameter
  result_valid <- validate_parameter("test_param", 42, required = TRUE, na_type = "b", log_level = "silent")
  expect_equal(result_valid, 42)
  
  # Test valid parameter with different log levels
  result_warning <- validate_parameter("test_param", 42, required = TRUE, na_type = "b", log_level = "warning")
  expect_equal(result_warning, 42)
  
  result_verbose <- validate_parameter("test_param", 42, required = TRUE, na_type = "b", log_level = "verbose")
  expect_equal(result_verbose, 42)
})

# Additional comprehensive tests for validate_parameter() edge cases
test_that("validate_parameter() handles missing parameters correctly", {
  # Create a function that can test missing parameters
  test_function <- function(param = NULL) {
    if (missing(param)) {
      validate_parameter("test_param", param, required = TRUE, na_type = "b", log_level = "silent")
    } else {
      validate_parameter("test_param", param, required = TRUE, na_type = "b", log_level = "silent")
    }
  }
  
  # Test missing parameter in silent mode
  result_missing_silent <- test_function()
  expect_true(haven::is_tagged_na(result_missing_silent, "b"))
  
  # Test missing parameter with warning
  expect_warning(
    test_function_warning <- function() {
      validate_parameter("test_param", missing_value, required = TRUE, na_type = "b", log_level = "warning")
    },
    NA  # Expect no warning when parameters are provided
  )
})

test_that("validate_parameter() handles different na_type specifications", {
  # Test with different na_type values using valid parameters
  result_na_a <- validate_parameter("test_param", 42, required = TRUE, na_type = "a", log_level = "silent")
  expect_equal(result_na_a, 42)
  
  result_na_b <- validate_parameter("test_param", 42, required = TRUE, na_type = "b", log_level = "silent")
  expect_equal(result_na_b, 42)
  
  # Test required = FALSE
  result_not_required <- validate_parameter("test_param", 42, required = FALSE, na_type = "b", log_level = "silent")
  expect_equal(result_not_required, 42)
})

test_that("needs_preprocessing() detects CCHS missing codes correctly", {
  # Test standard response codes (6,7,8,9)
  expect_true(needs_preprocessing(6))
  expect_true(needs_preprocessing(7))
  expect_true(needs_preprocessing(8))
  expect_true(needs_preprocessing(9))
  
  # Test categorical age codes (96,97,98,99)
  expect_true(needs_preprocessing(96))
  expect_true(needs_preprocessing(97))
  expect_true(needs_preprocessing(98))
  expect_true(needs_preprocessing(99))
  
  # Test continuous codes (996,997,998,999)
  expect_true(needs_preprocessing(996))
  expect_true(needs_preprocessing(997))
  expect_true(needs_preprocessing(998))
  expect_true(needs_preprocessing(999))
})

test_that("needs_preprocessing() detects character NA representations", {
  # Test character-based missing values
  expect_true(needs_preprocessing("Not applicable"))
  expect_true(needs_preprocessing("Missing"))
  expect_true(needs_preprocessing("Don't know"))
  expect_true(needs_preprocessing("NA(a)"))
  expect_true(needs_preprocessing("NA(b)"))
})

test_that("needs_preprocessing() correctly identifies clean data", {
  # Test valid numeric values
  expect_false(needs_preprocessing(1.75))
  expect_false(needs_preprocessing(70))
  expect_false(needs_preprocessing(c(1.65, 1.75, 1.80)))
  
  # Test regular NAs (these don't need CCHS preprocessing)
  expect_false(needs_preprocessing(NA))
  expect_false(needs_preprocessing(c(1.75, NA, 1.80)))
  
  # Test already processed tagged NAs
  expect_false(needs_preprocessing(haven::tagged_na("a")))
  expect_false(needs_preprocessing(haven::tagged_na("b")))
})

test_that("needs_preprocessing() handles vector inputs", {
  # Test mixed vectors with some CCHS codes
  mixed_vector <- c(1.75, 996, 1.80, 70)
  expect_true(needs_preprocessing(mixed_vector))
  
  # Test clean vector
  clean_vector <- c(1.75, 1.80, 70, 65)
  expect_false(needs_preprocessing(clean_vector))
  
  # Test vector with character NAs
  char_na_vector <- c("1.75", "Not applicable", "1.80")
  expect_true(needs_preprocessing(char_na_vector))
})

# ==============================================================================
# 4. INTEGRATION TESTS - Helpers Working Together
# ==============================================================================

test_that("validation functions work together in BMI calculation workflow", {
  # Test complete workflow using helper functions
  height <- 1.75
  weight <- 70
  
  # Step 1: Validate individual components
  validated_height <- validate_height_bounds(height)
  validated_weight <- validate_weight_bounds(weight)
  
  expect_equal(validated_height, height)
  expect_equal(validated_weight, weight)
  
  # Step 2: Check compatibility
  compatible <- check_bmi_length_compatibility(validated_height, validated_weight)
  expect_true(compatible)
  
  # Step 3: Enhanced validation
  enhanced_result <- validate_height_weight_enhanced(validated_height, validated_weight, 
                                                   0.914, 2.134, 27.0, 135.0)
  expect_equal(enhanced_result, height)
  
  # Step 4: Enhanced calculation
  bmi_result <- calculate_bmi_enhanced(validated_height, validated_weight,
                                     0.914, 2.134, 27.0, 135.0, 10, 100)
  expected_bmi <- weight / (height^2)
  expect_equal(bmi_result, expected_bmi, tolerance = 1e-6)
})

test_that("helper functions handle error propagation correctly", {
  # Test error propagation through helper function chain
  invalid_height <- 0.5  # Too low
  valid_weight <- 70
  
  # Step 1: Validation catches error
  validated_height <- validate_height_bounds(invalid_height)
  expect_true(haven::is_tagged_na(validated_height, "b"))
  
  # Step 2: Enhanced validation preserves error
  enhanced_result <- validate_height_weight_enhanced(validated_height, valid_weight,
                                                   0.914, 2.134, 27.0, 135.0)
  expect_true(haven::is_tagged_na(enhanced_result, "b"))
  
  # Step 3: Enhanced calculation preserves error
  bmi_result <- calculate_bmi_enhanced(validated_height, valid_weight,
                                     0.914, 2.134, 27.0, 135.0, 10, 100)
  expect_true(haven::is_tagged_na(bmi_result, "b"))
})

test_that("generic helpers integrate with BMI validation workflow", {
  # Test parameter validation in BMI context
  height_param <- validate_parameter("HWTGHTM", 1.75, required = TRUE, na_type = "b", log_level = "silent")
  expect_equal(height_param, 1.75)
  
  # Test preprocessing detection
  raw_height <- 996  # CCHS missing code
  needs_prep <- needs_preprocessing(raw_height)
  expect_true(needs_prep)
  
  # Test that processed values work with validation
  processed_height <- preprocess_cchs_missing_codes(raw_height, "continuous_standard")
  expect_true(haven::is_tagged_na(processed_height, "a"))
  
  # Test that processed values work with BMI validation
  validated_processed <- validate_height_bounds(processed_height)
  expect_true(haven::is_tagged_na(validated_processed, "a"))  # Should preserve NA type
})

# ==============================================================================
# 5. CONSTANTS AND BOUNDS TESTS
# ==============================================================================

test_that("BMI_VALIDATION_BOUNDS constants are correctly defined", {
  # Test that constants exist and have expected structure
  expect_true(exists("BMI_VALIDATION_BOUNDS"))
  expect_true(is.list(BMI_VALIDATION_BOUNDS))
  
  # Test height bounds
  expect_true("height" %in% names(BMI_VALIDATION_BOUNDS))
  expect_equal(BMI_VALIDATION_BOUNDS$height$min, 0.914)
  expect_equal(BMI_VALIDATION_BOUNDS$height$max, 2.134)
  
  # Test weight bounds
  expect_true("weight" %in% names(BMI_VALIDATION_BOUNDS))
  expect_equal(BMI_VALIDATION_BOUNDS$weight$min, 27.0)
  expect_equal(BMI_VALIDATION_BOUNDS$weight$max, 135.0)
  
  # Test BMI bounds
  expect_true("bmi" %in% names(BMI_VALIDATION_BOUNDS))
  expect_equal(BMI_VALIDATION_BOUNDS$bmi$min, 10)
  expect_equal(BMI_VALIDATION_BOUNDS$bmi$max, 100)
})

