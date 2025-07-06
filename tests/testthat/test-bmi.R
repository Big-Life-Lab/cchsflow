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

  # Test values outside bounds (should return tagged_na)
  result_invalid_height <- calculate_bmi(0.5, 70) # Height too low
  expect_true(haven::is_tagged_na(result_invalid_height, "b"))

  result_invalid_weight <- calculate_bmi(1.75, 20) # Weight too low
  expect_true(haven::is_tagged_na(result_invalid_weight, "b"))
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

  # Test standard response codes (6, 7, 8, 9) - treated as out-of-bounds values
  result_6 <- calculate_bmi(6, 70) # Out of bounds height
  expect_true(haven::is_tagged_na(result_6, "b"))

  result_7 <- calculate_bmi(1.75, 7) # Out of bounds weight
  expect_true(haven::is_tagged_na(result_7, "b"))
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

  # Test length incompatibility (should return tagged_na, not error)
  result_length_error <- calculate_bmi(c(1.75, 1.80), c(70, 75, 80), log_level = "silent")
  expect_true(all(haven::is_tagged_na(result_length_error, "b")))
})

test_that("calculate_bmi() logging functionality works correctly across all log levels", {
  # Test silent mode (default) - no warnings or messages
  expect_silent(calculate_bmi(c(1.75, 1.80), c(70, 75, 80), log_level = "silent"))
  expect_silent(calculate_bmi(c("invalid", "data"), c(70, 75), log_level = "silent"))
  expect_silent(calculate_bmi(c(1.75, 1.80), c(70, 75), log_level = "silent")) # Valid case

  # Test warning mode (warnings removed - fulsome logging planned for recodeflow)
  expect_silent(calculate_bmi(c(1.75, 1.80), c(70, 75, 80), log_level = "warning"))
  expect_silent(calculate_bmi(c("invalid", "data"), c(70, 75), log_level = "warning"))
  expect_silent(calculate_bmi(rep(NA, 3), rep(NA, 3), log_level = "warning"))

  # Test verbose mode (warnings removed - fulsome logging planned for recodeflow)
  expect_silent(calculate_bmi(c(1.75, 1.80), c(70, 75, 80), log_level = "verbose"))
  expect_silent(calculate_bmi(c("invalid", "data"), c(70, 75), log_level = "verbose"))
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

test_that("adjust_bmi() validates input bounds before correction", {
  # Test that extremely high weight is flagged during input validation
  result_extreme <- adjust_bmi(1, 1.0, 200, max_HWTGWTK = 150)
  expect_true(haven::is_tagged_na(result_extreme, "b"))
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
# 3. CATEGORIZE_BMI FUNCTION TESTS
# ==============================================================================

test_that("categorize_bmi() handles WHO categories correctly", {
  # Test standard WHO categories with categorical labels (default)
  result_underweight <- categorize_bmi(17)
  expect_equal(result_underweight, "Underweight")

  result_normal <- categorize_bmi(22)
  expect_equal(result_normal, "Normal weight")

  result_overweight <- categorize_bmi(27)
  expect_equal(result_overweight, "Overweight")

  result_obese <- categorize_bmi(32)
  expect_equal(result_obese, "Obese")
})

test_that("categorize_bmi() handles numeric codes correctly", {
  # Test numeric codes (categorical_labels = FALSE)
  result_underweight <- categorize_bmi(17, categorical_labels = FALSE)
  expect_equal(result_underweight, 1L)

  result_normal <- categorize_bmi(22, categorical_labels = FALSE)
  expect_equal(result_normal, 2L)

  result_overweight <- categorize_bmi(27, categorical_labels = FALSE)
  expect_equal(result_overweight, 3L)

  result_obese <- categorize_bmi(32, categorical_labels = FALSE)
  expect_equal(result_obese, 4L)
})

test_that("categorize_bmi() handles missing BMI values correctly", {
  # Test tagged_na preservation with labels
  result_na_a <- categorize_bmi(haven::tagged_na("a"))
  expect_equal(result_na_a, "NA(a)")

  result_na_b <- categorize_bmi(haven::tagged_na("b"))
  expect_equal(result_na_b, "NA(b)")

  # Test tagged_na preservation with numeric codes
  result_na_a_num <- categorize_bmi(haven::tagged_na("a"), categorical_labels = FALSE)
  expect_true(haven::is_tagged_na(result_na_a_num, "a"))

  result_na_b_num <- categorize_bmi(haven::tagged_na("b"), categorical_labels = FALSE)
  expect_true(haven::is_tagged_na(result_na_b_num, "b"))
})

test_that("categorize_bmi() handles CCHS missing codes in BMI values", {
  # Test continuous missing codes preprocessing
  result_996 <- categorize_bmi(996) # Not applicable
  expect_equal(result_996, "NA(a)")

  result_997 <- categorize_bmi(997) # Don't know
  expect_equal(result_997, "NA(b)")
})

test_that("categorize_bmi() handles vector inputs correctly", {
  # Test vector processing
  bmi_values <- c(17, 22, 27, 32, haven::tagged_na("a"), 997)
  results <- categorize_bmi(bmi_values)

  expect_equal(length(results), 6)
  expect_equal(results[1], "Underweight")
  expect_equal(results[2], "Normal weight")
  expect_equal(results[3], "Overweight")
  expect_equal(results[4], "Obese")
  expect_equal(results[5], "NA(a)")
  expect_equal(results[6], "NA(b)") # 997 should become NA(b) after preprocessing (Don't know)
})

test_that("categorize_bmi() boundary conditions work correctly", {
  # Test exact boundary values
  result_18_5 <- categorize_bmi(18.5)
  expect_equal(result_18_5, "Normal weight")

  result_24_9 <- categorize_bmi(24.9)
  expect_equal(result_24_9, "Normal weight")

  result_25_0 <- categorize_bmi(25.0)
  expect_equal(result_25_0, "Overweight")

  result_30_0 <- categorize_bmi(30.0)
  expect_equal(result_30_0, "Obese")
})

test_that("categorize_bmi() logging functionality works correctly across all log levels", {
  # Test silent mode (default) - no warnings or messages
  expect_silent(categorize_bmi(c("invalid", "data"), log_level = "silent"))
  expect_silent(categorize_bmi(c(22, 27), log_level = "silent")) # Valid case
  expect_silent(categorize_bmi(rep(NA, 3), log_level = "silent"))

  # Test warning mode (categorize_bmi has fewer validation warnings since it accepts any numeric input)
  expect_silent(categorize_bmi(c(22, 27), log_level = "warning")) # Valid case shouldn't warn
  expect_silent(categorize_bmi(c(15, 35), log_level = "warning")) # Even extreme values shouldn't warn (just categorize)

  # Test verbose mode (should behave same as warning mode for categorize_bmi)
  expect_silent(categorize_bmi(c(22, 27), log_level = "verbose"))
  expect_silent(categorize_bmi(c(15, 35), log_level = "verbose"))

  # Test that all log levels handle missing data appropriately
  result_silent <- categorize_bmi(haven::tagged_na("a"), log_level = "silent")
  result_warning <- categorize_bmi(haven::tagged_na("a"), log_level = "warning")
  result_verbose <- categorize_bmi(haven::tagged_na("a"), log_level = "verbose")

  expect_equal(result_silent, "NA(a)")
  expect_equal(result_warning, "NA(a)")
  expect_equal(result_verbose, "NA(a)")
})

# ==============================================================================
# 4. INTEGRATION TESTS - All Three Functions Together
# ==============================================================================

test_that("BMI functions work together in typical workflows", {
  # Test standard workflow: calculate -> categorize
  height <- 1.75
  weight <- 70

  bmi <- calculate_bmi(height, weight)
  category <- categorize_bmi(bmi)

  expect_type(bmi, "double")
  expect_type(category, "character")
  expect_equal(category, "Normal weight")
})

test_that("BMI functions work together with adjustment workflow", {
  # Test adjustment workflow: adjust -> categorize
  sex <- 1 # Male
  height <- 1.75
  weight <- 70

  adjusted_bmi <- adjust_bmi(sex, height, weight)
  category <- categorize_bmi(adjusted_bmi)

  expect_type(adjusted_bmi, "double")
  expect_type(category, "character")
  # Adjusted BMI should be slightly different from raw BMI
  raw_bmi <- calculate_bmi(height, weight)
  expect_false(identical(adjusted_bmi, raw_bmi))
})

test_that("BMI functions handle data frame contexts correctly", {
  # Simulate rec_with_table() usage pattern
  test_data <- data.frame(
    DHH_SEX = c(1, 2, 1, 6, 997),
    HWTGHTM = c(1.65, 1.75, 1.80, 1.70, 996),
    HWTGWTK = c(60, 70, 80, 75, 85),
    stringsAsFactors = FALSE
  )

  # Apply all three functions
  test_data$BMI_raw <- calculate_bmi(test_data$HWTGHTM, test_data$HWTGWTK)
  test_data$BMI_adjusted <- adjust_bmi(test_data$DHH_SEX, test_data$HWTGHTM, test_data$HWTGWTK)
  test_data$BMI_category <- categorize_bmi(test_data$BMI_raw)

  expect_equal(nrow(test_data), 5)
  expect_true(all(c("BMI_raw", "BMI_adjusted", "BMI_category") %in% names(test_data)))

  # Check that missing values are handled consistently
  expect_true(haven::is_tagged_na(test_data$BMI_raw[5], "a")) # Height 996
  expect_true(haven::is_tagged_na(test_data$BMI_adjusted[4], "a")) # Sex 6
  expect_equal(test_data$BMI_category[5], "NA(a)") # Missing BMI -> NA(a)
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
    category_results <- categorize_bmi(bmi_results)
    end_time <- Sys.time()
  })

  expect_equal(length(bmi_results), n)
  expect_equal(length(adjusted_results), n)
  expect_equal(length(category_results), n)

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
    category_silent <- categorize_bmi(bmi_silent, log_level = "silent")
  })

  # Benchmark warning mode
  warning_times <- system.time({
    bmi_warning <- calculate_bmi(heights, weights, log_level = "warning")
    adjusted_warning <- adjust_bmi(sexes, heights, weights, log_level = "warning")
    category_warning <- categorize_bmi(bmi_warning, log_level = "warning")
  })

  # Benchmark verbose mode
  verbose_times <- system.time({
    bmi_verbose <- calculate_bmi(heights, weights, log_level = "verbose")
    adjusted_verbose <- adjust_bmi(sexes, heights, weights, log_level = "verbose")
    category_verbose <- categorize_bmi(bmi_verbose, log_level = "verbose")
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
  expect_equal(category_silent, category_warning)
  expect_equal(category_silent, category_verbose)
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
    category_results <- categorize_bmi(bmi_results, log_level = "silent")
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

  # Test single extreme values
  result_extreme_low <- calculate_bmi(0.5, 10) # Outside bounds
  expect_true(haven::is_tagged_na(result_extreme_low, "b"))

  result_extreme_high <- calculate_bmi(3.0, 200) # Outside bounds
  expect_true(haven::is_tagged_na(result_extreme_high, "b"))
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

  formals_categorize <- names(formals(categorize_bmi))
  expect_true("log_level" %in% formals_categorize)
  expect_true("categorical_labels" %in% formals_categorize)
  expect_equal(formals(categorize_bmi)$categorical_labels, TRUE)
})
