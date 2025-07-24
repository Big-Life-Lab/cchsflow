# ==============================================================================
# Refactored BMI Functions - Test Suite
# ==============================================================================
#
# Tests for the new configuration-driven BMI functions (calculate_bmi_v3, etc.)
#
# @note Test suite v1.0.0, created: 2025-07-23
# ==============================================================================

library(testthat)
library(haven)
library(dplyr)

# Source internal environment first
source("helper-env.R")

# Source all required dependencies
source("../R/enhanced-pattern-detection.R")
source("../R/flexible-missing-handler.R")
source("../R/convert-missing.R")
source("../R/clean_variables.R")
source("../R/bmi-refactored.R")

# =============================================================================
# 1. calculate_bmi_v3 Tests
# =============================================================================

test_that("calculate_bmi works with original CCHS codes", {
  height <- c(1.75, 1.60, 996, 997, 998)
  weight <- c(70, 55, 70, 998, 999)
  
  result <- calculate_bmi(height, weight, output_format = "original")
  
  expect_equal(result[1], 22.85714, tolerance = 1e-5)
  expect_equal(result[2], 21.48438, tolerance = 1e-5)
  expect_equal(result[3], 996) # Not applicable
  expect_equal(result[4], 997) # Don't know (propagated from height)
  expect_equal(result[5], 998) # Refusal (propagated from height)
})

test_that("calculate_bmi works with tagged_na data", {
  height <- c(1.75, 1.60, haven::tagged_na("a"))
  weight <- c(70, 55, haven::tagged_na("b"))
  
  result <- calculate_bmi(height, weight, output_format = "tagged_na")
  
  expect_equal(result[1], 22.85714, tolerance = 1e-5)
  expect_true(haven::is_tagged_na(result[3], "a")) # Not applicable wins
})

test_that("calculate_bmi handles mixed data with auto-detection", {
  height <- c(1.75, 996, haven::tagged_na("a"))
  weight <- c(70, 997, haven::tagged_na("b"))
  
  result <- calculate_bmi(height, weight, output_format = "auto")
  
  expect_equal(result[1], 22.85714, tolerance = 1e-5)
  expect_true(haven::is_tagged_na(result[2], "a")) # Not applicable (996) wins
  expect_true(haven::is_tagged_na(result[3], "a")) # Not applicable wins
})

# =============================================================================
# 2. adjust_bmi Tests
# =============================================================================

test_that("adjust_bmi works with valid data", {
  sex <- c(1, 2)
  height <- c(1.75, 1.65)
  weight <- c(70, 60)
  
  result <- adjust_bmi(sex, height, weight)
  
  expect_equal(result[1], 23.51671, tolerance = 1e-5) # Male corrected
  expect_equal(result[2], 23.04519, tolerance = 1e-5) # Female corrected
})

test_that("adjust_bmi handles missing data correctly", {
  sex <-    c(1, 2, 6, 7, 1, 2)
  height <- c(1.75, 1.65, 1.80, 996, 1.70, 1.60)
  weight <- c(70, 60, 80, 70, 998, 55)
  
  result <- adjust_bmi(sex, height, weight, output_format = "tagged_na")
  
  expect_true(haven::is_tagged_na(result[3], "a")) # Invalid sex
  expect_true(haven::is_tagged_na(result[4], "a")) # Not applicable height
  expect_true(haven::is_tagged_na(result[5], "b")) # Missing weight
})

test_that("adjust_bmi propagates missing data with priority", {
  sex <-    c(1, 6)
  height <- c(997, 1.80)
  weight <- c(70, 80)
  
  result <- adjust_bmi(sex, height, weight, output_format = "tagged_na")
  
  # In first case, height is missing_data (b), so that should be the result
  expect_true(haven::is_tagged_na(result[1], "b"))
  # In second case, sex is not_applicable (a), which has higher priority
  expect_true(haven::is_tagged_na(result[2], "a"))
})

test_that("adjust_bmi handles NULL and NA sex values correctly", {
  # Test case 1: DHH_SEX contains a standard NA
  sex_with_na <- c(1, NA, 2)
  height <- c(1.75, 1.65, 1.80)
  weight <- c(70, 60, 80)
  
  # Expect a warning because clean_variables will warn about the NA
  expect_warning(
    result_na <- adjust_bmi(sex_with_na, height, weight, output_format = "tagged_na", log_level = "warning"),
    "Variable 'DHH_SEX' contains NA values that will be converted to tagged_na"
  )
  
  # The NA in sex should be caught and propagated as a tagged_na
  expect_false(is.na(result_na[1]))
  expect_true(haven::is_tagged_na(result_na[2]))
  expect_false(is.na(result_na[3]))

  # Test case 2: DHH_SEX is NULL
  # clean_variables will warn and convert this to NA, which then gets handled.
  expect_warning(
    result_null <- adjust_bmi(NULL, height, weight, output_format = "tagged_na"),
    "Variable 'DHH_SEX' is NULL"
  )
  expect_true(all(haven::is_tagged_na(result_null)))
  expect_equal(length(result_null), 3)
})
