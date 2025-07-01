# Test file for enhanced BMI functions following v3.0.0 development guide
# Tests demonstrate copy-paste functionality, missing data handling, and integration

library(testthat)
library(haven)
library(dplyr)

# Load the BMI functions
source("R/bmi.R")

# ==============================================================================
# 1. BASIC FUNCTIONALITY TESTS
# ==============================================================================

test_that("calculate_bmi() handles basic scalar inputs correctly", {
  # Test valid scalar inputs
  result <- calculate_bmi(1.75, 70)
  expect_equal(result, 70 / (1.75^2), tolerance = 1e-6)
  expect_type(result, "double")
  
  # Test boundary conditions
  result_boundary <- calculate_bmi(0.9, 25)  # Within bounds
  expect_type(result_boundary, "double")
  expect_false(is.na(result_boundary))
  
  # Test out of bounds
  result_invalid <- calculate_bmi(0.5, 70)  # Height too low
  expect_true(haven::is_tagged_na(result_invalid, "b"))
})

test_that("calculate_bmi() handles vector inputs correctly", {
  # Test valid vector inputs
  heights <- c(1.65, 1.75, 1.80)
  weights <- c(60, 70, 80)
  
  results <- calculate_bmi(heights, weights)
  expected <- weights / (heights^2)
  
  expect_equal(results, expected, tolerance = 1e-6)
  expect_equal(length(results), 3)
  expect_type(results, "double")
})

test_that("calculate_bmi() handles scalar/vector combinations", {
  # Scalar height, vector weights
  result1 <- calculate_bmi(1.75, c(60, 70, 80))
  expect_equal(length(result1), 3)
  
  # Vector heights, scalar weight
  result2 <- calculate_bmi(c(1.65, 1.75, 1.80), 70)
  expect_equal(length(result2), 3)
  
  # Both should be numeric
  expect_type(result1, "double")
  expect_type(result2, "double")
})

# ==============================================================================
# 2. MISSING DATA HANDLING TESTS (Critical for guide compliance)
# ==============================================================================

test_that("calculate_bmi() handles raw CCHS missing codes correctly", {
  # Test original CCHS missing codes
  result_6 <- calculate_bmi(6, 70)  # Not applicable
  expect_true(haven::is_tagged_na(result_6, "a"))
  
  result_7 <- calculate_bmi(1.75, 7)  # Don't know
  expect_true(haven::is_tagged_na(result_7, "b"))
  
  result_8 <- calculate_bmi(8, 70)  # Refusal
  expect_true(haven::is_tagged_na(result_8, "b"))
  
  result_9 <- calculate_bmi(1.75, 9)  # Not stated
  expect_true(haven::is_tagged_na(result_9, "b"))
})

test_that("calculate_bmi() handles continuous variable missing codes", {
  # Test continuous missing codes (996, 997, 998, 999)
  result_996 <- calculate_bmi(996, 70)  # Not applicable
  expect_true(haven::is_tagged_na(result_996, "a"))
  
  result_997 <- calculate_bmi(1.75, 997)  # Don't know
  expect_true(haven::is_tagged_na(result_997, "b"))
  
  result_998 <- calculate_bmi(998, 70)  # Refusal
  expect_true(haven::is_tagged_na(result_998, "b"))
  
  result_999 <- calculate_bmi(1.75, 999)  # Not stated
  expect_true(haven::is_tagged_na(result_999, "b"))
})

test_that("calculate_bmi() handles string NA inputs", {
  # Test string-based missing values
  result_na_string <- calculate_bmi("Not applicable", 70)
  expect_true(haven::is_tagged_na(result_na_string, "a"))
  
  result_missing_string <- calculate_bmi(1.75, "Missing")
  expect_true(haven::is_tagged_na(result_missing_string, "b"))
  
  result_dk_string <- calculate_bmi("Don't know", 70)
  expect_true(haven::is_tagged_na(result_dk_string, "b"))
})

test_that("calculate_bmi() preserves haven::tagged_na inputs", {
  # Test that pre-processed tagged NAs are preserved
  result_na_a <- calculate_bmi(haven::tagged_na("a"), 70)
  expect_true(haven::is_tagged_na(result_na_a, "a"))
  
  result_na_b <- calculate_bmi(1.75, haven::tagged_na("b"))
  expect_true(haven::is_tagged_na(result_na_b, "b"))
  
  # Test passthrough behavior
  input_tagged <- haven::tagged_na("a")
  result_tagged <- calculate_bmi(input_tagged, 70)
  expect_identical(class(result_tagged), class(input_tagged))
})

test_that("calculate_bmi() handles mixed missing data scenarios", {
  # Test complex vector with mixed missing types
  heights <- c(1.75, haven::tagged_na("a"), 6, "Not applicable", 997)
  weights <- c(70, 65, haven::tagged_na("b"), 75, "Missing")
  
  results <- calculate_bmi(heights, weights)
  
  expect_equal(length(results), 5)
  expect_false(haven::is_tagged_na(results[1]))  # Valid calculation
  expect_true(haven::is_tagged_na(results[2], "a"))  # Not applicable (height)
  expect_true(haven::is_tagged_na(results[3], "a"))  # Not applicable (height=6)
  expect_true(haven::is_tagged_na(results[4], "a"))  # Not applicable (height string)
  expect_true(haven::is_tagged_na(results[5], "b"))  # Missing (weight string)
})

# ==============================================================================
# 3. INPUT VALIDATION TESTS
# ==============================================================================

test_that("calculate_bmi() validates required parameters", {
  # Test missing required parameters
  expect_error(calculate_bmi(), "Required parameter HWTGHTM must be provided")
  expect_error(calculate_bmi(1.75), "Required parameter HWTGWTK must be provided")
})

test_that("calculate_bmi() validates vector length compatibility", {
  # Test incompatible vector lengths
  expect_error(
    calculate_bmi(c(1.75, 1.80), c(70, 75, 80)), 
    "Input vectors must have compatible lengths"
  )
  
  # Test compatible lengths (equal or scalar)
  expect_silent(calculate_bmi(c(1.75, 1.80), c(70, 75)))  # Equal lengths
  expect_silent(calculate_bmi(1.75, c(70, 75)))  # Scalar + vector
  expect_silent(calculate_bmi(c(1.75, 1.80), 70))  # Vector + scalar
})

test_that("calculate_bmi() provides informative warnings", {
  # Test type validation warnings
  expect_warning(
    calculate_bmi("invalid_string", 70),
    "HWTGHTM contains unexpected values"
  )
  
  # Test all-missing data warning
  expect_warning(
    calculate_bmi(rep(NA, 3), c(70, 75, 80)),
    "All input values are missing"
  )
})

test_that("calculate_bmi() handles edge cases gracefully", {
  # Test all NA inputs
  result_all_na <- suppressWarnings(calculate_bmi(rep(NA, 3), rep(NA, 3)))
  expect_true(all(haven::is_tagged_na(result_all_na, "b")))
  
  # Test extreme but valid values
  result_extreme <- calculate_bmi(2.49, 22.8)  # Just within bounds
  expect_type(result_extreme, "double")
  expect_false(is.na(result_extreme))
})

# ==============================================================================
# 4. VALIDATION MODE TESTS
# ==============================================================================

test_that("calculate_bmi() auto-detects validation mode", {
  # Test auto-detection when bounds are changed
  result_custom <- calculate_bmi(1.75, 70, min_HWTGHTM = 1.0, max_HWTGHTM = 2.0)
  expect_type(result_custom, "double")
  
  # Test validation bounds are applied
  result_invalid <- calculate_bmi(0.9, 70, min_HWTGHTM = 1.0, max_HWTGHTM = 2.0)
  expect_true(haven::is_tagged_na(result_invalid, "b"))
})

test_that("calculate_bmi() applies BMI bounds validation", {
  # Test BMI bounds
  # Create extreme case that would produce very high BMI
  result_high_bmi <- calculate_bmi(1.0, 200, BMI_min = 10, BMI_max = 50)
  expect_true(haven::is_tagged_na(result_high_bmi, "b"))
  
  # Test valid BMI within bounds
  result_valid_bmi <- calculate_bmi(1.75, 70, BMI_min = 10, BMI_max = 50)
  expect_false(is.na(result_valid_bmi))
})

# ==============================================================================
# 5. INTEGRATION TESTS (rec_with_table simulation)
# ==============================================================================

test_that("calculate_bmi() works with data frame contexts", {
  # Simulate rec_with_table() usage
  test_data <- data.frame(
    HWTGHTM = c(1.65, 1.75, 1.80, 6, 997),
    HWTGWTK = c(60, 70, 80, 75, 85),
    stringsAsFactors = FALSE
  )
  
  # Test function application to data frame columns
  test_data$BMI_calculated <- calculate_bmi(test_data$HWTGHTM, test_data$HWTGWTK)
  
  expect_equal(nrow(test_data), 5)
  expect_true("BMI_calculated" %in% names(test_data))
  
  # Check results
  expect_false(is.na(test_data$BMI_calculated[1]))  # Valid calculation
  expect_false(is.na(test_data$BMI_calculated[2]))  # Valid calculation  
  expect_false(is.na(test_data$BMI_calculated[3]))  # Valid calculation
  expect_true(haven::is_tagged_na(test_data$BMI_calculated[4], "a"))  # Not applicable
  expect_true(haven::is_tagged_na(test_data$BMI_calculated[5], "a"))  # Not applicable
})

# ==============================================================================
# 6. BACKWARD COMPATIBILITY TESTS
# ==============================================================================

test_that("calculate_bmi() maintains backward compatibility", {
  # Test against legacy patterns (if we had a legacy function)
  # For now, test consistent behavior with known good values
  
  # Standard BMI calculation
  height <- 1.75
  weight <- 70
  expected_bmi <- weight / (height^2)
  
  result <- calculate_bmi(height, weight)
  expect_equal(result, expected_bmi, tolerance = 1e-10)
  
  # Test with multiple values
  heights <- c(1.65, 1.75, 1.80)
  weights <- c(60, 70, 80)
  expected_bmis <- weights / (heights^2)
  
  results <- calculate_bmi(heights, weights)
  expect_equal(results, expected_bmis, tolerance = 1e-10)
})

# ==============================================================================
# 7. PERFORMANCE AND STRESS TESTS
# ==============================================================================

test_that("calculate_bmi() handles large datasets efficiently", {
  # Test with moderately large dataset
  n <- 10000
  heights <- rnorm(n, 1.7, 0.1)
  weights <- rnorm(n, 70, 15)
  
  # Should complete without error
  expect_silent({
    start_time <- Sys.time()
    results <- calculate_bmi(heights, weights)
    end_time <- Sys.time()
  })
  
  expect_equal(length(results), n)
  expect_type(results, "double")
  
  # Performance should be reasonable (less than 1 second for 10k observations)
  execution_time <- as.numeric(end_time - start_time)
  expect_lt(execution_time, 1.0)
})

# ==============================================================================
# 8. VERSIONING TESTS (Following guide requirements)
# ==============================================================================

test_that("calculate_bmi() has proper version metadata", {
  # Test that function documentation exists and includes version info
  # This would typically be checked during package documentation build
  
  # For now, verify the function exists and is callable
  expect_true(exists("calculate_bmi"))
  expect_true(is.function(calculate_bmi))
  
  # Verify function accepts expected parameters
  formals_names <- names(formals(calculate_bmi))
  expected_params <- c("HWTGHTM", "HWTGWTK", "min_HWTGHTM", "max_HWTGHTM", 
                      "min_HWTGWTK", "max_HWTGWTK", "BMI_min", "BMI_max", 
                      "validate_params")
  
  expect_true(all(expected_params %in% formals_names))
})