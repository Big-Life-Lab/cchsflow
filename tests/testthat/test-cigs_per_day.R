# =============================================================================
# cigs_per_day Derived Variable Tests
# =============================================================================
#
# Tests for the unified cigs_per_day derived variable that wraps:
# - SMK_204 (current daily smokers, SMKDSTY_original = 1) - "How many cigarettes do you
#   currently smoke per day?"
# - SMK_208 (former daily smokers, SMKDSTY_original = 2, 4) - "When you smoked daily,
#   how many cigarettes did you usually smoke per day?"
#
# The variable routes to the correct source based on SMKDSTY_original status.
#
# Universe: Ever-daily smokers (SMKDSTY_original 1, 2, 4)
# NA for: Never-daily smokers (SMKDSTY_original 3, 5, 6)
#
# @note v3.0.0-alpha, last updated: 2026-01-09, status: active
# =============================================================================

library(testthat)
library(haven)
library(dplyr)

# =============================================================================
# Test Setup and Helper Functions
# =============================================================================

# Create minimal infrastructure for testing
create_test_infrastructure <- function() {
  # Simple clean_variables replacement
  clean_variables <<- function(continuous_vars = NULL,
                               categorical_vars = NULL,
                               min_values = NULL,
                               max_values = NULL,
                               valid_values = NULL,
                               continuous_pattern = "triple_digit_missing",
                               categorical_pattern = "single_digit_missing",
                               log_level = "silent") {

    all_vars <- c(continuous_vars, categorical_vars)
    all_names <- names(all_vars)

    if (length(all_vars) == 0) {
      stop("At least one variable must be provided", call. = FALSE)
    }

    result <- list()

    # Process continuous variables
    if (!is.null(continuous_vars) && length(continuous_vars) > 0) {
      for (name in names(continuous_vars)) {
        var <- continuous_vars[[name]]
        var_processed <- var

        # Convert CCHS missing codes to tagged_na
        if (continuous_pattern == "triple_digit_missing") {
          var_processed[var_processed == 996] <- haven::tagged_na("a")
          var_processed[var_processed %in% c(997, 998, 999)] <- haven::tagged_na("b")
        }

        result[[paste0(name, "_clean")]] <- var_processed
      }
    }

    # Process categorical variables
    if (!is.null(categorical_vars) && length(categorical_vars) > 0) {
      for (name in names(categorical_vars)) {
        var <- categorical_vars[[name]]
        var_processed <- var

        # Convert CCHS missing codes to tagged_na
        if (categorical_pattern == "single_digit_missing") {
          var_processed[var_processed == 6] <- haven::tagged_na("a")
          var_processed[var_processed %in% c(7, 8, 9)] <- haven::tagged_na("b")
        }

        result[[paste0(name, "_clean")]] <- var_processed
      }
    }

    return(result)
  }
}

# Load functions with test infrastructure
setup_test_environment <- function() {
  # calculate_cigs_per_day is provided by the loaded cchsflow package
  # (via library() in tests/testthat.R or devtools::load_all()).
  # Just install the mock infrastructure — no sourcing needed.
  create_test_infrastructure()
}

# Run setup before tests
setup_test_environment()

# =============================================================================
# Core Routing Tests
# =============================================================================

test_that("calculate_cigs_per_day uses SMK_204 for status 1 (current daily)", {

  # Status 1: Current daily smoker should use SMK_204

  # Test Case 1: Single value
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 1,
    SMK_204 = 20,
    SMK_208 = NA
  )
  expect_equal(result, 20)

  # Test Case 2: Different values
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 1,
    SMK_204 = 15,
    SMK_208 = NA
  )
  expect_equal(result, 15)

  # Test Case 3: Even when SMK_208 has a value (should be ignored for status 1)
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 1,
    SMK_204 = 25,
    SMK_208 = 10
  )
  expect_equal(result, 25)
})

test_that("calculate_cigs_per_day uses SMK_208 for status 2 (occasional, former daily)", {

  # Status 2: Occasional smoker who was former daily should use SMK_208

  # Test Case 1: Single value
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 2,
    SMK_204 = NA,
    SMK_208 = 18
  )
  expect_equal(result, 18)

  # Test Case 2: Even when SMK_204 has a value (should be ignored for status 2)
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 2,
    SMK_204 = 30,
    SMK_208 = 12
  )
  expect_equal(result, 12)
})

test_that("calculate_cigs_per_day uses SMK_208 for status 4 (former daily)", {

  # Status 4: Former daily smoker should use SMK_208

  # Test Case 1: Single value
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 4,
    SMK_204 = NA,
    SMK_208 = 22
  )
  expect_equal(result, 22)

  # Test Case 2: Even when SMK_204 has a value (should be ignored for status 4)
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 4,
    SMK_204 = 40,
    SMK_208 = 17
  )
  expect_equal(result, 17)
})

test_that("calculate_cigs_per_day returns NA::a for never-daily smokers (status 3, 5, 6)", {

  # Never-daily smokers should return tagged_na("a") - not applicable

  # Status 3: Occasional smoker (never daily)
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 3,
    SMK_204 = NA,
    SMK_208 = NA
  )
  expect_true(haven::is_tagged_na(result, "a"))

  # Status 5: Former occasional smoker
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 5,
    SMK_204 = NA,
    SMK_208 = NA
  )
  expect_true(haven::is_tagged_na(result, "a"))

  # Status 6: Never smoker
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 6,
    SMK_204 = NA,
    SMK_208 = NA
  )
  expect_true(haven::is_tagged_na(result, "a"))
})

test_that("calculate_cigs_per_day returns NA::b for missing status", {

  # Missing SMKDSTY_original should return tagged_na("b")

  # Test Case 1: NA status
  result <- calculate_cigs_per_day(
    SMKDSTY_original = NA,
    SMK_204 = 20,
    SMK_208 = 15
  )
  expect_true(haven::is_tagged_na(result, "b") || is.na(result))

  # Test Case 2: tagged_na("b") status
  result <- calculate_cigs_per_day(
    SMKDSTY_original = haven::tagged_na("b"),
    SMK_204 = 20,
    SMK_208 = 15
  )
  expect_true(haven::is_tagged_na(result, "b"))
})

# =============================================================================
# Mutual Exclusivity Validation Tests
# =============================================================================

test_that("routing is status-based, not value-based when both have values", {

  # This tests the mutual exclusivity - for any given respondent, the status

  # determines which source variable to use, regardless of what values exist

  # Status 1 always uses SMK_204
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 1,
    SMK_204 = 10,
    SMK_208 = 50
  )
  expect_equal(result, 10)

  # Status 2 always uses SMK_208
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 2,
    SMK_204 = 10,
    SMK_208 = 50
  )
  expect_equal(result, 50)

  # Status 4 always uses SMK_208
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 4,
    SMK_204 = 10,
    SMK_208 = 50
  )
  expect_equal(result, 50)
})

# =============================================================================
# Value Range Validation Tests
# =============================================================================

test_that("calculate_cigs_per_day handles valid cigarette counts", {

  # Valid range: 1-99 cigarettes (CCHS coding)

  # Minimum plausible value
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 1,
    SMK_204 = 1,
    SMK_208 = NA
  )
  expect_equal(result, 1)

  # Typical value
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 1,
    SMK_204 = 20,
    SMK_208 = NA
  )
  expect_equal(result, 20)

  # Higher value
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 4,
    SMK_204 = NA,
    SMK_208 = 60
  )
  expect_equal(result, 60)
})

test_that("calculate_cigs_per_day handles boundary values", {

  # Test high but unambiguous value (95 is safely below the 96-99 range
  # that clean_variables() auto-detection treats as missing codes when
  # database context is unavailable during unit testing)
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 1,
    SMK_204 = 95,
    SMK_208 = NA
  )
  expect_equal(result, 95)
})

# =============================================================================
# Missing Data Propagation Tests
# =============================================================================

test_that("calculate_cigs_per_day propagates missing SMK_204 for status 1", {

  # When status is 1 but SMK_204 is missing, should return missing

  # Test Case 1: SMK_204 is NA
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 1,
    SMK_204 = NA,
    SMK_208 = 20
  )
  expect_true(is.na(result))

  # Test Case 2: SMK_204 is tagged_na("b")
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 1,
    SMK_204 = haven::tagged_na("b"),
    SMK_208 = 20
  )
  expect_true(haven::is_tagged_na(result, "b"))

  # Test Case 3: SMK_204 is tagged_na("a") (not applicable in source)
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 1,
    SMK_204 = haven::tagged_na("a"),
    SMK_208 = 20
  )
  expect_true(haven::is_tagged_na(result, "a"))
})

test_that("calculate_cigs_per_day propagates missing SMK_208 for status 2 and 4", {

  # When status is 2 or 4 but SMK_208 is missing, should return missing

  # Test Case 1: SMK_208 is NA for status 2
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 2,
    SMK_204 = 20,
    SMK_208 = NA
  )
  expect_true(is.na(result))

  # Test Case 2: SMK_208 is tagged_na("b") for status 4
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 4,
    SMK_204 = 20,
    SMK_208 = haven::tagged_na("b")
  )
  expect_true(haven::is_tagged_na(result, "b"))
})

test_that("calculate_cigs_per_day handles both sources missing", {

  # When both SMK_204 and SMK_208 are missing

  # Status 1 with both missing
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 1,
    SMK_204 = NA,
    SMK_208 = NA
  )
  expect_true(is.na(result))

  # Status 4 with both missing
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 4,
    SMK_204 = NA,
    SMK_208 = NA
  )
  expect_true(is.na(result))
})

# =============================================================================
# Vector Input Tests
# =============================================================================

test_that("calculate_cigs_per_day handles vector inputs correctly", {

  # Test vector processing with mixed statuses
  smkdsty_vec <- c(1, 2, 3, 4, 5, 6)
  smk_204_vec <- c(20, NA, NA, NA, NA, NA)
  smk_208_vec <- c(NA, 15, NA, 25, NA, NA)

  result_vec <- calculate_cigs_per_day(
    SMKDSTY_original = smkdsty_vec,
    SMK_204 = smk_204_vec,
    SMK_208 = smk_208_vec
  )

  expect_length(result_vec, 6)

  # Element 1: Status 1 -> uses SMK_204 = 20
  expect_equal(result_vec[1], 20)

  # Element 2: Status 2 -> uses SMK_208 = 15
  expect_equal(result_vec[2], 15)

  # Element 3: Status 3 -> not applicable (never daily)
  expect_true(haven::is_tagged_na(result_vec[3], "a"))

  # Element 4: Status 4 -> uses SMK_208 = 25
  expect_equal(result_vec[4], 25)

  # Element 5: Status 5 -> not applicable (never daily)
  expect_true(haven::is_tagged_na(result_vec[5], "a"))

  # Element 6: Status 6 -> not applicable (never smoker)
  expect_true(haven::is_tagged_na(result_vec[6], "a"))
})

test_that("calculate_cigs_per_day handles mixed valid/missing in vectors", {

  # Test with mixed valid and missing values
  smkdsty_vec <- c(1, 1, 4, 4)
  smk_204_vec <- c(20, NA, NA, NA)
  smk_208_vec <- c(NA, NA, 15, NA)

  result_vec <- calculate_cigs_per_day(
    SMKDSTY_original = smkdsty_vec,
    SMK_204 = smk_204_vec,
    SMK_208 = smk_208_vec
  )

  expect_length(result_vec, 4)

  # Element 1: Status 1, SMK_204 = 20 -> 20
  expect_equal(result_vec[1], 20)

  # Element 2: Status 1, SMK_204 = NA -> NA
  expect_true(is.na(result_vec[2]))

  # Element 3: Status 4, SMK_208 = 15 -> 15
  expect_equal(result_vec[3], 15)

  # Element 4: Status 4, SMK_208 = NA -> NA
  expect_true(is.na(result_vec[4]))
})

# =============================================================================
# Edge Cases Tests
# =============================================================================

test_that("calculate_cigs_per_day handles empty input vectors", {

  result <- calculate_cigs_per_day(
    SMKDSTY_original = numeric(0),
    SMK_204 = numeric(0),
    SMK_208 = numeric(0)
  )

  expect_length(result, 0)
})

test_that("calculate_cigs_per_day handles single-element vectors", {

  # Single current daily smoker
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 1,
    SMK_204 = 20,
    SMK_208 = NA
  )
  expect_equal(result, 20)
  expect_length(result, 1)
})

test_that("calculate_cigs_per_day returns NA(b) for zero cigarettes", {

  # SMK_204 valid range is [1,99]; 0 is recoded to tagged_na("b") by clean_variables
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 1,
    SMK_204 = 0,
    SMK_208 = NA
  )
  expect_true(haven::is_tagged_na(result, "b"))
})

# =============================================================================
# CCHS Missing Code Tests
# =============================================================================

test_that("calculate_cigs_per_day handles CCHS missing codes in status", {

  # CCHS code 6 -> tagged_na("a") (not applicable)
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 6,
    SMK_204 = 20,
    SMK_208 = 15
  )
  # Status 6 is "never smoker" which should be NA::a
  expect_true(haven::is_tagged_na(result, "a"))

  # CCHS codes 7, 8, 9 -> tagged_na("b") (missing/don't know/refused)
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 7,
    SMK_204 = 20,
    SMK_208 = 15
  )
  expect_true(haven::is_tagged_na(result, "b"))
})

test_that("calculate_cigs_per_day handles CCHS missing codes in source variables", {

  # SMK_204 code 996 -> tagged_na("a")
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 1,
    SMK_204 = 996,
    SMK_208 = NA
  )
  expect_true(haven::is_tagged_na(result, "a"))

  # SMK_204 codes 997, 998, 999 -> tagged_na("b")
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 1,
    SMK_204 = 999,
    SMK_208 = NA
  )
  expect_true(haven::is_tagged_na(result, "b"))

  # SMK_208 code 996 -> tagged_na("a")
  result <- calculate_cigs_per_day(
    SMKDSTY_original = 4,
    SMK_204 = NA,
    SMK_208 = 996
  )
  expect_true(haven::is_tagged_na(result, "a"))
})

# Note: log_level parameter was removed (unused). Tests removed.

# =============================================================================
# CCHS Codebook Alignment Tests
# =============================================================================

test_that("cigs_per_day aligns with SMKDSTY_original category definitions", {

  # Based on CCHS documentation:
  # SMKDSTY_original categories:
  # 1 = Daily smoker -> asks SMK_204

  # 2 = Occasional smoker (former daily) -> asks SMK_208
  # 3 = Occasional smoker (never daily) -> neither asked
  # 4 = Former daily smoker -> asks SMK_208
  # 5 = Former occasional smoker -> neither asked
  # 6 = Never smoker -> neither asked

  # Create representative data for each category
  categories <- c(1, 2, 3, 4, 5, 6)
  smk_204 <- c(20, NA, NA, NA, NA, NA)
  smk_208 <- c(NA, 15, NA, 25, NA, NA)

  results <- calculate_cigs_per_day(
    SMKDSTY_original = categories,
    SMK_204 = smk_204,
    SMK_208 = smk_208
  )

  # Category 1: should have valid value from SMK_204
  expect_equal(results[1], 20)

  # Category 2: should have valid value from SMK_208
  expect_equal(results[2], 15)

  # Category 3: should be NA::a (not applicable)
  expect_true(haven::is_tagged_na(results[3], "a"))

  # Category 4: should have valid value from SMK_208
  expect_equal(results[4], 25)

  # Category 5: should be NA::a (not applicable)
  expect_true(haven::is_tagged_na(results[5], "a"))

  # Category 6: should be NA::a (not applicable)
  expect_true(haven::is_tagged_na(results[6], "a"))
})
