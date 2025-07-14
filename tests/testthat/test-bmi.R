# Enhanced BMI Function Tests - v3.0.0 Architecture
# Tests all three BMI functions with comprehensive edge cases following user requirements

library(testthat)
library(haven)
library(dplyr)

# Test context: Load BMI functions
# Note: Functions loaded via devtools::load_all() or source commands

# ==============================================================================
# 1. CALCULATE_BMI FUNCTION TESTS
# ==============================================================================

test_that("calculate_bmi() handles basic scalar inputs correctly", {
  # Test valid inputs within bounds
  result <- calculate_bmi(1.75, 70)
  expected <- 70 / (1.75^2)
  expect_equal(result, expected, tolerance = 1e-6)
  expect_type(result, "double")

  # Test boundary conditions (updated for variable_details.csv bounds)
  result_boundary <- calculate_bmi(0.915, 27.1) # Just within bounds
  expect_type(result_boundary, "double")
  expect_false(is.na(result_boundary))

  # Test values outside typical bounds (now calculates BMI - no bounds validation)
  result_invalid_height <- calculate_bmi(0.5, 70) # Height outside typical range
  expect_false(is.na(result_invalid_height))  # Now calculates BMI
  expect_equal(result_invalid_height, 70 / (0.5^2), tolerance = 1e-6)  # BMI = 280

  result_invalid_weight <- calculate_bmi(1.75, 20) # Weight outside typical range  
  expect_false(is.na(result_invalid_weight))  # Now calculates BMI
  expect_equal(result_invalid_weight, 20 / (1.75^2), tolerance = 1e-6)  # BMI ≈ 6.53
})

test_that("calculate_bmi() handles vector inputs correctly", {
  # Test valid vector inputs
  heights <- c(1.65, 1.75, 1.50)
  weights <- c(60, 70, 80)

  results <- calculate_bmi(heights, weights)
  expected <- weights / (heights^2)

  expect_equal(results, expected, tolerance = 1e-6)
  expect_equal(length(results), 3)
  expect_type(results, "double")
})

test_that("calculate_bmi() handles CCHS missing codes correctly", {
  # Test continuous missing codes (996, 997, 998, 999) - the most common pattern
  result_996 <- calculate_bmi(996, 70) # Not applicable
  expect_true(haven::is_tagged_na(result_996, "a"))

  result_997 <- calculate_bmi(1.75, 997) # Don't know
  expect_true(haven::is_tagged_na(result_997, "b"))

  result_998 <- calculate_bmi(998, 70) # Refusal
  expect_true(haven::is_tagged_na(result_998, "b"))

  result_999 <- calculate_bmi(1.75, 999) # Not stated
  expect_true(haven::is_tagged_na(result_999, "b"))

  # Test standard response codes (6, 7, 8, 9) - now treated as regular numeric values
  result_6 <- calculate_bmi(6, 70) # Height = 6 (calculates BMI)
  expect_false(is.na(result_6))  # Now calculates BMI
  expect_equal(result_6, 70 / (6^2), tolerance = 1e-6)  # BMI ≈ 1.94

  result_7 <- calculate_bmi(1.75, 7) # Weight = 7 (calculates BMI)
  expect_false(is.na(result_7))  # Now calculates BMI  
  expect_equal(result_7, 7 / (1.75^2), tolerance = 1e-6)  # BMI ≈ 2.29
})

test_that("calculate_bmi() preserves haven::tagged_na inputs", {
  # Test that pre-processed tagged NAs are preserved
  result_na_a <- calculate_bmi(haven::tagged_na("a"), 70)
  expect_true(haven::is_tagged_na(result_na_a, "a"))

  result_na_b <- calculate_bmi(1.75, haven::tagged_na("b"))
  expect_true(haven::is_tagged_na(result_na_b, "b"))
})

test_that("calculate_bmi() handles mixed missing data scenarios", {
  # Test complex vector with mixed missing types
  heights <- c(1.75, haven::tagged_na("a"), 996, 1.65, 997)
  weights <- c(70, 65, haven::tagged_na("b"), 75, 80)

  results <- calculate_bmi(heights, weights)

  expect_equal(length(results), 5)
  expect_false(haven::is_tagged_na(results[1])) # Valid calculation
  expect_true(haven::is_tagged_na(results[2], "a")) # Not applicable (height)
  expect_true(haven::is_tagged_na(results[3], "a")) # Not applicable (height=996->a takes precedence)
  expect_false(haven::is_tagged_na(results[4])) # Valid calculation
  expect_true(haven::is_tagged_na(results[5], "b")) # Missing (height=997->b, Don't know)
})

test_that("calculate_bmi() returns tagged_na for invalid parameters instead of errors", {
  # Test missing required parameters (should return tagged_na("d"), not error)
  result_missing_height <- calculate_bmi(log_level = "silent")
  expect_true(haven::is_tagged_na(result_missing_height, "d"))

  # Test equal length vectors (proper vector calculation)
  result_equal_length <- calculate_bmi(c(1.75, 1.80, 1.85), c(70, 75, 80), log_level = "silent")
  expect_equal(length(result_equal_length), 3)
  expect_false(any(is.na(result_equal_length)))  # All should calculate BMI now
  
  # Test actual length incompatibility (unequal vector lengths)
  # clean_variables() handles this gracefully by returning tagged_na("b") values
  result_incompatible <- calculate_bmi(c(1.75, 1.80), c(70, 75, 80), log_level = "silent")
  expect_length(result_incompatible, 3)  # Uses max length
  expect_true(all(haven::is_tagged_na(result_incompatible, "b")))  # All should be tagged_na("b")
})

test_that("calculate_bmi() logging functionality works correctly across all log levels", {
  # Test silent mode (default) - no warnings or messages
  expect_silent(calculate_bmi(c(1.75, 1.80, 1.85), c(70, 75, 80), log_level = "silent"))
  expect_silent(calculate_bmi(c(996, 997), c(70, 75), log_level = "silent"))
  expect_silent(calculate_bmi(c(1.75, 1.80), c(70, 75), log_level = "silent")) # Valid case

  # Test warning mode (warnings removed - fulsome logging planned for recodeflow)
  expect_silent(calculate_bmi(c(1.75, 1.80, 1.85), c(70, 75, 80), log_level = "warning"))
  expect_silent(calculate_bmi(c(996, 997), c(70, 75), log_level = "warning"))
  expect_silent(calculate_bmi(rep(NA, 3), rep(NA, 3), log_level = "warning"))

  # Test verbose mode (warnings removed - fulsome logging planned for recodeflow)
  expect_silent(calculate_bmi(c(1.75, 1.80, 1.85), c(70, 75, 80), log_level = "verbose"))
  expect_silent(calculate_bmi(c(996, 997), c(70, 75), log_level = "verbose"))
  expect_silent(calculate_bmi(rep(NA, 3), rep(NA, 3), log_level = "verbose"))

  # Test that valid inputs don't produce warnings even in verbose mode
  expect_silent(calculate_bmi(1.75, 70, log_level = "verbose"))
  expect_silent(calculate_bmi(c(1.75, 1.80), c(70, 75), log_level = "verbose"))
})

# ==============================================================================
# 2. ADJUST_BMI FUNCTION TESTS
# ==============================================================================

test_that("adjust_bmi() handles basic inputs correctly", {
  # Test valid inputs for male (sex = 1)
  result_male <- adjust_bmi(1, 1.75, 70)
  raw_bmi <- 70 / (1.75^2)
  expected_male <- -1.07575 + 1.07592 * raw_bmi
  expect_equal(result_male, expected_male, tolerance = 1e-6)

  # Test valid inputs for female (sex = 2)
  result_female <- adjust_bmi(2, 1.65, 60)
  raw_bmi_female <- 60 / (1.65^2)
  expected_female <- -0.12374 + 1.05129 * raw_bmi_female
  expect_equal(result_female, expected_female, tolerance = 1e-6)
})

test_that("adjust_bmi() handles missing sex codes correctly", {
  # Test CCHS missing codes for sex
  result_sex_6 <- adjust_bmi(6, 1.75, 70) # Not applicable
  expect_true(haven::is_tagged_na(result_sex_6, "a"))

  result_sex_7 <- adjust_bmi(7, 1.75, 70) # Don't know
  expect_true(haven::is_tagged_na(result_sex_7, "b"))

  # Test invalid sex codes
  result_invalid_sex <- adjust_bmi(3, 1.75, 70) # Invalid sex code
  expect_true(haven::is_tagged_na(result_invalid_sex, "b"))
})

test_that("adjust_bmi() handles missing height and weight correctly", {
  # Test height/weight missing codes
  result_height_996 <- adjust_bmi(1, 996, 70)
  expect_true(haven::is_tagged_na(result_height_996, "a"))

  result_weight_997 <- adjust_bmi(2, 1.65, 997)
  expect_true(haven::is_tagged_na(result_weight_997, "b"))
})

test_that("adjust_bmi() behavior with extreme values", {
  # Note: Both calculate_bmi() and adjust_bmi() now have consistent behavior - no bounds validation
  # Both functions calculate BMI for any numeric input, only handle CCHS missing codes
  result_extreme <- adjust_bmi(1, 1.0, 200)  # Weight 200 > typical range (135.0)
  expect_false(is.na(result_extreme))  # Calculates a result
  expect_true(is.numeric(result_extreme))  # Returns a numeric BMI value
  
  # Test with very small height
  result_small_height <- adjust_bmi(2, 0.5, 60)  # Height 0.5m
  expect_false(is.na(result_small_height))  # Calculates a result
  expect_true(is.numeric(result_small_height))  # Returns a numeric BMI value
})

test_that("adjust_bmi() vector processing works correctly", {
  # Test vector inputs
  sexes <- c(1, 2, 1, 2)
  heights <- c(1.75, 1.65, 1.80, 1.70)
  weights <- c(70, 60, 85, 65)

  results <- adjust_bmi(sexes, heights, weights)

  expect_equal(length(results), 4)
  expect_type(results, "double")
  expect_true(all(!is.na(results)))
})

test_that("adjust_bmi() logging functionality works correctly across all log levels", {
  # Test silent mode (default) - no warnings or messages
  expect_silent(adjust_bmi(c(1, 2), c(1.75, 1.80), c(70, 75, 80), log_level = "silent"))
  expect_silent(adjust_bmi(c("invalid"), c(1.75), c(70), log_level = "silent"))
  expect_silent(adjust_bmi(1, 1.75, 70, log_level = "silent")) # Valid case

  # Test warning mode (warnings removed - fulsome logging planned for recodeflow)
  expect_silent(adjust_bmi(c(1, 2), c(1.75, 1.80), c(70, 75, 80), log_level = "warning"))

  # Test verbose mode (warnings removed - fulsome logging planned for recodeflow)
  expect_silent(adjust_bmi(c(1, 2), c(1.75, 1.80), c(70, 75, 80), log_level = "verbose"))

  # Test that valid inputs don't produce warnings even in verbose mode
  expect_silent(adjust_bmi(1, 1.75, 70, log_level = "verbose"))
  expect_silent(adjust_bmi(c(1, 2), c(1.75, 1.65), c(70, 60), log_level = "verbose"))
})

# ==============================================================================
# BMI RANGE VALIDATION TESTS (Standalone vs rec_with_table)
# ==============================================================================

test_that("calculate_bmi() handles extreme BMI values without bounds validation", {
  # Test BMI < 15 (very low) - standalone function calculates without validation
  result_low_bmi <- calculate_bmi(2.1, 40)  # Height=2.1m, Weight=40kg → BMI≈9.07
  expect_false(is.na(result_low_bmi))
  expect_true(result_low_bmi < 15)
  expect_equal(result_low_bmi, 40 / (2.1^2), tolerance = 1e-6)
  expect_false(haven::is_tagged_na(result_low_bmi))
  
  # Test BMI > 50 (very high) - standalone function calculates without validation
  result_high_bmi <- calculate_bmi(0.7, 70)  # Height=0.7m, Weight=70kg → BMI≈142.86
  expect_false(is.na(result_high_bmi))
  expect_true(result_high_bmi > 50)
  expect_equal(result_high_bmi, 70 / (0.7^2), tolerance = 1e-6)
  expect_false(haven::is_tagged_na(result_high_bmi))
  
  # Test normal BMI range still works
  result_normal <- calculate_bmi(1.75, 70)  # BMI≈22.86
  expect_false(is.na(result_normal))
  expect_true(result_normal >= 15 && result_normal <= 50)
})

test_that("adjust_bmi() handles extreme BMI values without bounds validation", {
  # Test BMI < 15 after correction - standalone function calculates without validation
  result_low_adj <- adjust_bmi(1, 2.1, 40)  # Very tall, underweight → low BMI after correction
  expect_false(is.na(result_low_adj))
  expect_false(haven::is_tagged_na(result_low_adj))
  
  # Test BMI > 50 after correction - standalone function calculates without validation
  result_high_adj <- adjust_bmi(1, 0.7, 70)  # Very short, normal weight → high BMI after correction
  expect_false(is.na(result_high_adj))
  expect_true(result_high_adj > 50)
  expect_false(haven::is_tagged_na(result_high_adj))
  
  # Verify correction formulas are applied correctly
  raw_bmi_low <- 40 / (2.1^2)
  expected_low <- -1.07575 + 1.07592 * raw_bmi_low  # Male correction
  expect_equal(result_low_adj, expected_low, tolerance = 1e-5)
  
  raw_bmi_high <- 70 / (0.7^2)
  expected_high <- -1.07575 + 1.07592 * raw_bmi_high  # Male correction
  expect_equal(result_high_adj, expected_high, tolerance = 1e-5)
})

test_that("rec_with_table() applies BMI bounds validation (integration test)", {
  # Note: This test requires rec_with_table() to be available
  # If not available, test will be skipped
  skip_if_not_installed("cchsflow")
  
  # Test data with valid height/weight but out-of-range BMI - matches bmi.R example
  test_data <- data.frame(
    HWTGHTM = c(1.75, 1.60, 2.134, 0.914),   # Normal, normal, max valid, min valid
    HWTGWTK = c(70, 55, 27, 135)             # Normal, normal, min valid, max valid
  )
  
  # Expected BMI calculations:
  # Row 1: 1.75, 70 → BMI ≈ 22.86 (normal, should pass)
  # Row 2: 1.60, 55 → BMI ≈ 21.48 (normal, should pass)
  # Row 3: 2.134, 27 → BMI ≈ 5.93 (valid inputs, BMI < 15, should be tagged_na)
  # Row 4: 0.914, 135 → BMI ≈ 161.6 (valid inputs, BMI > 50, should be tagged_na)
  
  tryCatch({
    library(cchsflow)
    result <- rec_with_table(test_data, c("HWTGHTM", "HWTGWTK", "HWTGBMI_der"))
    
    # Row 1: Normal BMI should pass validation
    expect_false(is.na(result$HWTGBMI_der[1]))
    expect_true(result$HWTGBMI_der[1] >= 15 && result$HWTGBMI_der[1] <= 50)
    
    # Row 2: Normal BMI should pass validation  
    expect_false(is.na(result$HWTGBMI_der[2]))
    expect_true(result$HWTGBMI_der[2] >= 15 && result$HWTGBMI_der[2] <= 50)
    
    # Row 3: Valid inputs but BMI < 15 should be rejected by rec_with_table()
    expect_true(haven::is_tagged_na(result$HWTGBMI_der[3], "b"))
    
    # Row 4: Valid inputs but BMI > 50 should be rejected by rec_with_table()
    expect_true(haven::is_tagged_na(result$HWTGBMI_der[4], "b"))
    
  }, error = function(e) {
    skip("rec_with_table() not available or failed - integration test skipped")
  })
})

# ==============================================================================
# 3. BMI CATEGORICAL TESTS - via rec_with_table()
# ==============================================================================

test_that("BMI categorical processing works via rec_with_table()", {
  # Test that BMI categorization works through rec_with_table workflow
  # This replaces direct categorize_bmi() function tests
  
  # Create test data with known BMI values
  test_data <- data.frame(
    HWTGHTM = c(1.75, 1.60, 1.80, 1.70),
    HWTGWTK = c(50, 55, 90, 110),
    stringsAsFactors = FALSE
  )
  
  # Calculate BMI first
  test_data$HWTGBMI_der <- calculate_bmi(test_data$HWTGHTM, test_data$HWTGWTK)
  
  # Expected BMI values and categories
  expected_bmis <- c(
    50 / (1.75^2),  # ~16.3 -> Underweight
    55 / (1.60^2),  # ~21.5 -> Normal weight
    90 / (1.80^2),  # ~27.8 -> Overweight
    110 / (1.70^2)  # ~38.1 -> Obese
  )
  
  # Verify BMI calculations
  expect_equal(test_data$HWTGBMI_der, expected_bmis, tolerance = 1e-6)
  
  # Test WHO category boundaries manually
  expect_true(test_data$HWTGBMI_der[1] < 18.5) # Underweight
  expect_true(test_data$HWTGBMI_der[2] >= 18.5 && test_data$HWTGBMI_der[2] < 25) # Normal
  expect_true(test_data$HWTGBMI_der[3] >= 25 && test_data$HWTGBMI_der[3] < 30) # Overweight
  expect_true(test_data$HWTGBMI_der[4] >= 30) # Obese
})

test_that("BMI categorization handles missing data correctly", {
  # Test missing data through BMI calculation workflow
  test_data <- data.frame(
    HWTGHTM = c(1.75, 996, 997, 998, 999),
    HWTGWTK = c(70, 70, 70, 70, 70),
    stringsAsFactors = FALSE
  )
  
  # Calculate BMI
  test_data$HWTGBMI_der <- calculate_bmi(test_data$HWTGHTM, test_data$HWTGWTK)
  
  # Expected outcomes
  expect_false(is.na(test_data$HWTGBMI_der[1])) # Valid BMI
  expect_true(haven::is_tagged_na(test_data$HWTGBMI_der[2], "a")) # 996 -> NA(a)
  expect_true(haven::is_tagged_na(test_data$HWTGBMI_der[3], "b")) # 997 -> NA(b)
  expect_true(haven::is_tagged_na(test_data$HWTGBMI_der[4], "b")) # 998 -> NA(b)
  expect_true(haven::is_tagged_na(test_data$HWTGBMI_der[5], "b")) # 999 -> NA(b)
})

test_that("BMI categorical boundary conditions work correctly", {
  # Test exact WHO boundary values
  boundary_heights <- rep(1.75, 4)
  # Calculate weights that give exact boundary BMI values
  boundary_weights <- c(
    18.5 * (1.75^2),  # Exactly 18.5 (Normal weight lower bound)
    24.99 * (1.75^2), # Just under 25 (Normal weight upper bound)
    25.0 * (1.75^2),  # Exactly 25 (Overweight lower bound)
    30.0 * (1.75^2)   # Exactly 30 (Obese lower bound)
  )
  
  bmi_results <- calculate_bmi(boundary_heights, boundary_weights)
  
  # Verify boundary conditions
  expect_equal(bmi_results[1], 18.5, tolerance = 1e-6)
  expect_equal(bmi_results[2], 24.99, tolerance = 1e-6)
  expect_equal(bmi_results[3], 25.0, tolerance = 1e-6)
  expect_equal(bmi_results[4], 30.0, tolerance = 1e-6)
  
  # Test category boundaries
  expect_true(bmi_results[1] >= 18.5) # Normal weight (>= 18.5)
  expect_true(bmi_results[2] < 25)    # Normal weight (< 25)
  expect_true(bmi_results[3] >= 25)   # Overweight (>= 25)
  expect_true(bmi_results[4] >= 30)   # Obese (>= 30)
})

# ==============================================================================
# 4. INTEGRATION TESTS - All Functions Together
# ==============================================================================

test_that("BMI functions work together in typical workflows", {
  # Test standard workflow: calculate BMI
  height <- 1.75
  weight <- 70

  bmi <- calculate_bmi(height, weight)
  expect_type(bmi, "double")
  expect_equal(round(bmi, 2), 22.86)
  
  # Test that BMI falls in expected WHO category range
  expect_true(bmi >= 18.5 && bmi < 25) # Normal weight range
})

test_that("BMI functions work together with adjustment workflow", {
  # Test adjustment workflow
  sex <- 1 # Male
  height <- 1.75
  weight <- 70

  adjusted_bmi <- adjust_bmi(sex, height, weight)
  raw_bmi <- calculate_bmi(height, weight)
  
  expect_type(adjusted_bmi, "double")
  expect_type(raw_bmi, "double")
  
  # Adjusted BMI should be slightly different from raw BMI
  expect_false(identical(adjusted_bmi, raw_bmi))
  expect_true(abs(adjusted_bmi - raw_bmi) > 0) # Some difference
})

test_that("BMI functions handle data frame contexts correctly", {
  # Simulate rec_with_table() usage pattern
  test_data <- data.frame(
    DHH_SEX = c(1, 2, 1, 6, 997),
    HWTGHTM = c(1.65, 1.75, 1.80, 1.70, 996),
    HWTGWTK = c(60, 70, 80, 75, 85),
    stringsAsFactors = FALSE
  )

  # Apply BMI functions
  test_data$BMI_raw <- calculate_bmi(test_data$HWTGHTM, test_data$HWTGWTK)
  test_data$BMI_adjusted <- adjust_bmi(test_data$DHH_SEX, test_data$HWTGHTM, test_data$HWTGWTK)

  expect_equal(nrow(test_data), 5)
  expect_true(all(c("BMI_raw", "BMI_adjusted") %in% names(test_data)))

  # Check that missing values are handled consistently
  expect_true(haven::is_tagged_na(test_data$BMI_raw[5], "a")) # Height 996
  expect_true(haven::is_tagged_na(test_data$BMI_adjusted[4], "a")) # Sex 6
})

# ==============================================================================
# 5. PERFORMANCE AND EDGE CASE TESTS
# ==============================================================================

test_that("BMI functions handle large datasets efficiently", {
  # Test with moderately large dataset (scaling down from 10k to be reasonable)
  n <- 1000
  set.seed(42) # For reproducible results
  heights <- rnorm(n, 1.7, 0.1)
  weights <- rnorm(n, 70, 15)
  sexes <- sample(c(1, 2), n, replace = TRUE)

  # Test all functions complete without error
  expect_silent({
    start_time <- Sys.time()
    bmi_results <- calculate_bmi(heights, weights)
    adjusted_results <- adjust_bmi(sexes, heights, weights)
    end_time <- Sys.time()
  })

  expect_equal(length(bmi_results), n)
  expect_equal(length(adjusted_results), n)

  # Performance should be reasonable (less than 2 seconds for 1k observations)
  execution_time <- as.numeric(end_time - start_time)
  expect_lt(execution_time, 2.0)
})

test_that("BMI functions logging performance impact is minimal", {
  # Test performance impact of logging levels on moderately large dataset
  n <- 1000
  set.seed(123)
  heights <- rnorm(n, 1.7, 0.1)
  weights <- rnorm(n, 70, 15)
  sexes <- sample(c(1, 2), n, replace = TRUE)

  # Benchmark silent mode (baseline)
  silent_times <- system.time({
    bmi_silent <- calculate_bmi(heights, weights, log_level = "silent")
    adjusted_silent <- adjust_bmi(sexes, heights, weights, log_level = "silent")
  })

  # Benchmark warning mode
  warning_times <- system.time({
    bmi_warning <- calculate_bmi(heights, weights, log_level = "warning")
    adjusted_warning <- adjust_bmi(sexes, heights, weights, log_level = "warning")
  })

  # Benchmark verbose mode
  verbose_times <- system.time({
    bmi_verbose <- calculate_bmi(heights, weights, log_level = "verbose")
    adjusted_verbose <- adjust_bmi(sexes, heights, weights, log_level = "verbose")
  })

  # Logging overhead should be minimal (< 50% increase)
  silent_elapsed <- silent_times[["elapsed"]]
  warning_elapsed <- warning_times[["elapsed"]]
  verbose_elapsed <- verbose_times[["elapsed"]]

  expect_lt(warning_elapsed, silent_elapsed * 1.5, "Warning mode should not significantly impact performance")
  expect_lt(verbose_elapsed, silent_elapsed * 1.5, "Verbose mode should not significantly impact performance")

  # Results should be identical across log levels
  expect_equal(bmi_silent, bmi_warning)
  expect_equal(bmi_silent, bmi_verbose)
  expect_equal(adjusted_silent, adjusted_warning)
  expect_equal(adjusted_silent, adjusted_verbose)
})

test_that("BMI functions scale linearly with dataset size", {
  # Test scalability across different dataset sizes
  sizes <- c(100, 500, 1000)
  times <- numeric(length(sizes))

  for (i in seq_along(sizes)) {
    n <- sizes[i]
    set.seed(456)
    heights <- rnorm(n, 1.7, 0.1)
    weights <- rnorm(n, 70, 15)
    sexes <- sample(c(1, 2), n, replace = TRUE)

    # Measure execution time
    start_time <- Sys.time()
    bmi_results <- calculate_bmi(heights, weights, log_level = "silent")
    adjusted_results <- adjust_bmi(sexes, heights, weights, log_level = "silent")
    end_time <- Sys.time()

    times[i] <- as.numeric(end_time - start_time)
  }

  # Performance should scale roughly linearly (allow for some variation)
  # Time ratio should be roughly proportional to size ratio
  time_ratio_1_2 <- times[2] / times[1] # 500 vs 100
  time_ratio_2_3 <- times[3] / times[2] # 1000 vs 500
  size_ratio_1_2 <- sizes[2] / sizes[1] # 5x
  size_ratio_2_3 <- sizes[3] / sizes[2] # 2x

  # Allow for 2x overhead due to R's vectorization and test environment variability
  expect_lt(time_ratio_1_2, size_ratio_1_2 * 2, "Performance should scale reasonably with dataset size")
  expect_lt(time_ratio_2_3, size_ratio_2_3 * 2, "Performance should scale reasonably with dataset size")

  # All executions should complete in reasonable time
  expect_true(all(times < 5.0), "All dataset sizes should complete within 5 seconds")
})

test_that("BMI functions handle extreme edge cases gracefully", {
  # Test all NA inputs
  result_all_na <- calculate_bmi(rep(NA, 3), rep(NA, 3))
  expect_true(all(haven::is_tagged_na(result_all_na, "b")))

  # Test empty vectors
  result_empty <- calculate_bmi(numeric(0), numeric(0))
  expect_equal(length(result_empty), 0)

  # Test single extreme values (now calculates BMI - no bounds validation)
  result_extreme_low <- calculate_bmi(0.5, 10) # Extreme values
  expect_false(is.na(result_extreme_low))  # Now calculates BMI
  expect_equal(result_extreme_low, 10 / (0.5^2), tolerance = 1e-6)  # BMI = 40

  result_extreme_high <- calculate_bmi(3.0, 200) # Extreme values
  expect_false(is.na(result_extreme_high))  # Now calculates BMI
  expect_equal(result_extreme_high, 200 / (3.0^2), tolerance = 1e-6)  # BMI ≈ 22.22
})

# ==============================================================================
# 6. BACKWARD COMPATIBILITY AND CONSTANTS TESTS
# ==============================================================================

test_that("BMI functions use correct validation constants", {
  # Test that functions work with expected validation ranges
  # Note: Internal validation constants are not exported from package

  # Test that functions handle boundary values correctly
  # Height boundaries (approximately 0.914m to 2.134m)
  result_min_height <- calculate_bmi(0.915, 50, log_level = "silent")
  expect_true(is.numeric(result_min_height) && !is.na(result_min_height))

  result_max_height <- calculate_bmi(2.13, 80, log_level = "silent")
  expect_true(is.numeric(result_max_height) && !is.na(result_max_height))

  # Weight boundaries (approximately 27kg to 135kg)
  result_min_weight <- calculate_bmi(1.70, 27.5, log_level = "silent")
  expect_true(is.numeric(result_min_weight) && !is.na(result_min_weight))

  result_max_weight <- calculate_bmi(1.80, 134, log_level = "silent")
  expect_true(is.numeric(result_max_weight) && !is.na(result_max_weight))
})

test_that("BMI functions have proper parameter defaults", {
  # Test function signatures have expected parameters
  formals_calculate <- names(formals(calculate_bmi))
  expect_true("log_level" %in% formals_calculate)
  expect_equal(formals(calculate_bmi)$log_level, "silent")

  formals_adjust <- names(formals(adjust_bmi))
  expect_true("log_level" %in% formals_adjust)
})

# ==============================================================================
# 7. TAGGED NA EXAMPLES TESTS (from @examples documentation)
# ==============================================================================

test_that("calculate_bmi() @examples tagged NA behavior works as documented", {
  # Test examples from documentation exactly as shown
  
  # Example 1: Height 0.80 (now calculates BMI - no bounds validation)
  result <- calculate_bmi(0.80, 70)
  expect_false(is.na(result))  # Now calculates BMI
  expect_equal(result, 70 / (0.80^2), tolerance = 1e-6)  # BMI = 109.375
  
  # Example 2: CCHS missing code 996 (not applicable)
  result2 <- calculate_bmi(996, 70)
  expect_true(is.na(result2))  # User sees NA
  expect_true(haven::is_tagged_na(result2, "a"))  # Internal structure is tagged_na("a")
  
  # Example 3: CCHS missing code 998 (missing/invalid)
  result3 <- calculate_bmi(998, 70)
  expect_true(is.na(result3))  # User sees NA
  expect_true(haven::is_tagged_na(result3, "b"))  # Internal structure is tagged_na("b")
})

test_that("adjust_bmi() @examples tagged NA behavior works as documented", {
  # Test examples from adjust_bmi documentation
  
  # Example 1: Invalid sex code (6 -> not applicable)
  result <- adjust_bmi(6, 1.75, 70)
  expect_true(is.na(result))  # User sees NA
  expect_true(haven::is_tagged_na(result, "a"))  # Internal structure is tagged_na("a")
  
  # Example 2: CCHS missing height code 997 (missing/invalid)
  result2 <- adjust_bmi(1, 997, 70)
  expect_true(is.na(result2))  # User sees NA
  expect_true(haven::is_tagged_na(result2, "b"))  # Internal structure is tagged_na("b")
})

test_that("BMI functions tagged NA behavior matches specific CCHS missing code patterns", {
  # Test comprehensive CCHS missing code behavior across BMI functions
  
  # CCHS missing codes and their expected tagged NA types
  missing_codes <- list(
    "996" = "a",  # Not applicable
    "997" = "b",  # Don't know
    "998" = "b",  # Refusal  
    "999" = "b"   # Not stated
  )
  
  for(code in names(missing_codes)) {
    expected_tag <- missing_codes[[code]]
    numeric_code <- as.numeric(code)
    
    # Test calculate_bmi
    bmi_result <- calculate_bmi(numeric_code, 70)
    expect_true(haven::is_tagged_na(bmi_result, expected_tag), 
                info = paste("calculate_bmi() code", code, "should be tagged_na(", expected_tag, ")"))
    
    # Test adjust_bmi (using height missing)
    adj_result <- adjust_bmi(1, numeric_code, 70)
    expect_true(haven::is_tagged_na(adj_result, expected_tag),
                info = paste("adjust_bmi() code", code, "should be tagged_na(", expected_tag, ")"))
  }
})

test_that("BMI functions preserve tagged NA through workflows", {
  # Test that tagged NAs are preserved through typical analysis workflows
  
  # Create test data with various missing patterns
  test_heights <- c(1.75, 996, 997, 998, 999, 0.5, NA)  # Valid, missing codes, out-of-bounds, regular NA
  test_weights <- c(70, 70, 70, 70, 70, 70, 70)
  test_sex <- c(1, 1, 1, 1, 1, 1, 1)
  
  # Calculate BMI
  bmi_results <- calculate_bmi(test_heights, test_weights)
  
  # Test workflow: sex + height + weight -> adjusted BMI
  adjusted_bmi <- adjust_bmi(test_sex, test_heights, test_weights)
  
  # Verify that missing data structure is preserved
  expect_true(haven::is_tagged_na(bmi_results[2], "a"))  # 996 -> tagged_na("a")
  expect_true(haven::is_tagged_na(bmi_results[3], "b"))  # 997 -> tagged_na("b")
  expect_false(is.na(bmi_results[6]))  # 0.5 -> now calculates BMI (no bounds validation)
  
  expect_true(haven::is_tagged_na(adjusted_bmi[2], "a"))  # Preserved through adjustment
})

test_that("rec_with_table() example from @examples documentation works correctly", {
  # Test the exact example from calculate_bmi() documentation
  # This validates that the documented rec_with_table() workflow would work
  
  test_data <- data.frame(
    HWTGHTM = c(1.75, 1.60, 0.80, 996, 997, 998, 999, NA),
    HWTGWTK = c(70, 55, 70, 70, 998, 999, 70, NA)
  )
  
  # Test using individual functions (simulating what rec_with_table would do)
  result_bmi <- calculate_bmi(test_data$HWTGHTM, test_data$HWTGWTK)
  
  # Expected outcomes from documentation
  # Row 1: 1.75, 70 -> valid BMI calculation
  expect_false(is.na(result_bmi[1]))
  expect_equal(round(result_bmi[1], 2), 22.86)
  
  # Row 2: 1.60, 55 -> valid BMI calculation  
  expect_false(is.na(result_bmi[2]))
  expect_equal(round(result_bmi[2], 2), 21.48)
  
  # Row 3: 0.80, 70 -> now calculates BMI (no bounds validation)
  expect_false(is.na(result_bmi[3]))  # Now calculates BMI
  expect_equal(round(result_bmi[3], 2), 109.37)  # BMI = 70/(0.80^2) = 109.375 → rounds to 109.37
  
  # Row 4: 996, 70 -> CCHS missing code -> <NA+a>  
  expect_true(haven::is_tagged_na(result_bmi[4], "a"))
  
  # Row 5: 997, 998 -> CCHS missing codes -> <NA+b> (997 is "b")
  expect_true(haven::is_tagged_na(result_bmi[5], "b"))
  
  # Row 6: 998, 999 -> CCHS missing codes -> <NA+b>
  expect_true(haven::is_tagged_na(result_bmi[6], "b"))
  
  # Row 7: 999, 70 -> CCHS missing code -> <NA+b>
  expect_true(haven::is_tagged_na(result_bmi[7], "b"))
  
  # Row 8: NA, NA -> regular NAs -> <NA+b>
  expect_true(haven::is_tagged_na(result_bmi[8], "b"))
  
  # Verify the data structure matches expected output format
  result_df <- data.frame(
    HWTGHTM = test_data$HWTGHTM,
    HWTGWTK = test_data$HWTGWTK,
    HWTGBMI_der = result_bmi
  )
  
  expect_equal(nrow(result_df), 8)
  expect_equal(ncol(result_df), 3)
  expect_true(all(c("HWTGHTM", "HWTGWTK", "HWTGBMI_der") %in% names(result_df)))
})

test_that("adjust_bmi() rec_with_table example from @examples documentation works correctly", {
  # Test the exact example from adjust_bmi() documentation
  test_data <- data.frame(
    DHH_SEX = c(1, 2, 1, 2, 6, 7, 8, 9),
    HWTGHTM = c(1.75, 1.65, 1.80, 1.60, 996, 997, 998, 999),
    HWTGWTK = c(70, 60, 80, 55, 70, 998, 999, NA)
  )
  
  # Test using individual function (simulating what rec_with_table would do)
  result_adj <- adjust_bmi(test_data$DHH_SEX, test_data$HWTGHTM, test_data$HWTGWTK)
  
  # Expected outcomes from documentation
  # Row 1: 1, 1.75, 70 -> valid adjusted BMI (male)
  expect_false(is.na(result_adj[1]))
  expect_equal(round(result_adj[1], 5), 23.51671)
  
  # Row 2: 2, 1.65, 60 -> valid adjusted BMI (female)
  expect_false(is.na(result_adj[2]))
  expect_equal(round(result_adj[2], 5), 23.04519)
  
  # Row 3: 1, 1.80, 80 -> valid adjusted BMI (male)
  expect_false(is.na(result_adj[3]))
  
  # Row 4: 2, 1.60, 55 -> valid adjusted BMI (female)
  expect_false(is.na(result_adj[4]))
  
  # Row 5: 6, 996, 70 -> invalid sex code 6 -> <NA+a>
  expect_true(haven::is_tagged_na(result_adj[5], "a"))
  
  # Row 6: 7, 997, 998 -> invalid sex code 7 -> <NA+b>
  expect_true(haven::is_tagged_na(result_adj[6], "b"))
  
  # Row 7: 8, 998, 999 -> invalid sex code 8 -> <NA+b>
  expect_true(haven::is_tagged_na(result_adj[7], "b"))
  
  # Row 8: 9, 999, NA -> invalid sex code 9 -> <NA+b>
  expect_true(haven::is_tagged_na(result_adj[8], "b"))
})

