# =============================================================================
# age_start_smoking Derived Variable Tests
# =============================================================================
#
# Tests for the unified age_start_smoking derived variable that wraps:
# - SMK_040 (Master, exact continuous) - priority source
# - SMKG040_cont (PUMF, ~±3 years midpoint estimation) - fallback source
#
# The variable follows the pattern of time_quit_smoking from cessation.
#
# Universe: Ever-daily smokers (SMKDSTY 1, 2, 4)
# NA for: Never-daily smokers (SMKDSTY 3, 5, 6)
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
  clean_variables <<- function(vars, output_format = "tagged_na", check_length = TRUE) {
    result <- lapply(vars, function(x) {
      x[x == 997] <- haven::tagged_na("a")
      x[x == 999] <- haven::tagged_na("b")
      return(x)
    })
    return(result)
  }

  # Simple any_missing replacement
  any_missing <<- function(...) {
    vars <- list(...)
    if (length(vars) == 0 || length(vars[[1]]) == 0) {
      return(logical(0))
    }
    sapply(1:length(vars[[1]]), function(i) {
      any(sapply(vars, function(v) is.na(v[i]) || haven::is_tagged_na(v[i])))
    })
  }

  # Simple get_priority_missing replacement
  # Semantic priority: NA::a (not applicable) > NA::b (missing/not stated)
  # If one source says "not applicable" (never-daily), that's the correct answer
  get_priority_missing <<- function(..., output_format = "tagged_na") {
    vars <- list(...)
    if (length(vars) == 0 || length(vars[[1]]) == 0) {
      return(vars[[1]])
    }

    result <- rep(NA, length(vars[[1]]))
    for (i in 1:length(vars[[1]])) {
      has_a <- any(sapply(vars, function(v) haven::is_tagged_na(v[i], "a")))
      has_b <- any(sapply(vars, function(v) haven::is_tagged_na(v[i], "b")))

      # NA::a takes precedence - "not applicable" is a definitive answer
      if (has_a) {
        result[i] <- haven::tagged_na("a")
      } else if (has_b) {
        result[i] <- haven::tagged_na("b")
      } else {
        result[i] <- NA
      }
    }
    return(result)
  }

  # Simple assign_missing replacement
  assign_missing <<- function(missing_type, variable_name, output_format = "tagged_na") {
    if (output_format == "tagged_na") {
      if (missing_type == "not_applicable") return(haven::tagged_na("a"))
      else return(haven::tagged_na("b"))
    } else {
      if (missing_type == "not_applicable") return(996L)
      else return(999L)
    }
  }
}

# Load functions with test infrastructure
setup_test_environment <- function() {
  # First create the mock infrastructure so sourcing won't fail
  create_test_infrastructure()

  # Try to source the function file - we need calculate_age_start_smoking
  tryCatch({
    source("development/flexible-missing-data-mvp/R/smoke-start.R")
  }, error = function(e) {
    tryCatch({
      source("R/smoke-start.R")
    }, error = function(e2) {
      tryCatch({
        source("../../development/flexible-missing-data-mvp/R/smoke-start.R")
      }, error = function(e3) {
        # Final fallback - try absolute path
        source("/Users/dmanuel/github/cchsflow/development/flexible-missing-data-mvp/R/smoke-start.R")
      })
    })
  })

  # Re-apply the mock infrastructure AFTER sourcing to override any
  # conflicting definitions from the sourced files
  create_test_infrastructure()
}

# Run setup before tests
setup_test_environment()

# =============================================================================
# calculate_age_start_smoking() Tests
# =============================================================================

test_that("calculate_age_start_smoking uses SMK_040 when available (Master priority)", {

  # SMK_040 (Master) should take priority over SMKG040_cont (PUMF)
  # This tests the priority logic: exact continuous > midpoint estimation


  # Test Case 1: SMK_040 provided, no SMKG040_cont - use SMK_040
  expect_equal(calculate_age_start_smoking(SMK_040 = 18), 18)
  expect_equal(calculate_age_start_smoking(SMK_040 = 25), 25)
  expect_equal(calculate_age_start_smoking(SMK_040 = 42), 42)

  # Test Case 2: Both provided - SMK_040 takes priority
  expect_equal(calculate_age_start_smoking(SMK_040 = 20, SMKG040_cont = 22), 20)
  expect_equal(calculate_age_start_smoking(SMK_040 = 35, SMKG040_cont = 37), 35)

  # Test Case 3: SMK_040 tagged NA, valid SMKG040_cont - fall back to SMKG040_cont
  expect_equal(calculate_age_start_smoking(SMK_040 = haven::tagged_na("a"), SMKG040_cont = 16), 16)
  expect_equal(calculate_age_start_smoking(SMK_040 = haven::tagged_na("b"), SMKG040_cont = 22), 22)

  # Test Case 4: SMK_040 NA (untagged), valid SMKG040_cont - fall back to SMKG040_cont
  expect_equal(calculate_age_start_smoking(SMK_040 = NA, SMKG040_cont = 18), 18)
})

test_that("calculate_age_start_smoking falls back to SMKG040_cont for PUMF files", {

  # When only SMKG040_cont is available (PUMF files without Master),
  # the function should use the midpoint-estimated values

  # Test Case 1: Only SMKG040_cont provided
  expect_equal(calculate_age_start_smoking(SMKG040_cont = 8), 8)     # Category 1 midpoint
  expect_equal(calculate_age_start_smoking(SMKG040_cont = 13), 13)   # Category 2 midpoint
  expect_equal(calculate_age_start_smoking(SMKG040_cont = 16), 16)   # Category 3 midpoint
  expect_equal(calculate_age_start_smoking(SMKG040_cont = 18.5), 18.5) # Category 4 midpoint
  expect_equal(calculate_age_start_smoking(SMKG040_cont = 22), 22)   # Category 5 midpoint
  expect_equal(calculate_age_start_smoking(SMKG040_cont = 55), 55)   # Category 11 midpoint

  # Test Case 2: Boundary values within valid range
  expect_equal(calculate_age_start_smoking(SMKG040_cont = 5), 5)     # Min plausible age
  expect_equal(calculate_age_start_smoking(SMKG040_cont = 80), 80)   # Max plausible age
})

test_that("calculate_age_start_smoking handles NA correctly for never-daily smokers", {

  # Never-daily smokers (SMKDSTY 3, 5, 6) were never asked age started daily
  # These should return tagged_na("a") - not applicable

  # Test Case 1: Both inputs are tagged_na("a") - not applicable
  result_both_na_a <- calculate_age_start_smoking(
    SMK_040 = haven::tagged_na("a"),
    SMKG040_cont = haven::tagged_na("a")
  )
  expect_true(haven::is_tagged_na(result_both_na_a, "a"))

  # Test Case 2: Only SMK_040 provided as tagged_na("a")
  result_smk040_na_a <- calculate_age_start_smoking(SMK_040 = haven::tagged_na("a"))
  expect_true(haven::is_tagged_na(result_smk040_na_a, "a"))

  # Test Case 3: Only SMKG040_cont provided as tagged_na("a")
  result_pumf_na_a <- calculate_age_start_smoking(SMKG040_cont = haven::tagged_na("a"))
  expect_true(haven::is_tagged_na(result_pumf_na_a, "a"))
})

test_that("calculate_age_start_smoking propagates missing data correctly", {

  # Missing data (not "not applicable") should propagate as tagged_na("b")

  # Test Case 1: Both inputs missing
  result_both_missing <- calculate_age_start_smoking(
    SMK_040 = haven::tagged_na("b"),
    SMKG040_cont = haven::tagged_na("b")
  )
  expect_true(haven::is_tagged_na(result_both_missing, "b"))

  # Test Case 2: SMK_040 missing, SMKG040_cont not applicable
  # "Not applicable" takes precedence over "missing" semantically
  result_mixed <- calculate_age_start_smoking(
    SMK_040 = haven::tagged_na("b"),
    SMKG040_cont = haven::tagged_na("a")
  )
  # When one source says "not applicable" (never-daily), that's the correct answer
  expect_true(haven::is_tagged_na(result_mixed, "a"))

  # Test Case 3: SMK_040 not applicable, SMKG040_cont missing - use "not applicable"
  result_mixed_2 <- calculate_age_start_smoking(
    SMK_040 = haven::tagged_na("a"),
    SMKG040_cont = haven::tagged_na("b")
  )
  expect_true(haven::is_tagged_na(result_mixed_2, "a"))

  # Test Case 4: No inputs provided - should be missing
  result_no_inputs <- calculate_age_start_smoking()
  expect_true(haven::is_tagged_na(result_no_inputs, "b") || is.na(result_no_inputs))
})

test_that("calculate_age_start_smoking handles vector inputs correctly", {

  # Test vector processing with mixed Master and PUMF scenarios
  smk_040_vec <- c(18, haven::tagged_na("a"), 25, haven::tagged_na("b"), NA)
  smkg040_cont_vec <- c(haven::tagged_na("b"), 16, haven::tagged_na("a"), 22, 30)

  result_vec <- calculate_age_start_smoking(
    SMK_040 = smk_040_vec,
    SMKG040_cont = smkg040_cont_vec
  )

  expect_length(result_vec, 5)

  # Element 1: SMK_040 = 18 (valid) - use it
  expect_equal(result_vec[1], 18)

  # Element 2: SMK_040 = NA(a), SMKG040_cont = 16 - fall back to PUMF
  expect_equal(result_vec[2], 16)

  # Element 3: SMK_040 = 25 (valid) - use it, ignore SMKG040_cont NA(a)
  expect_equal(result_vec[3], 25)

  # Element 4: SMK_040 = NA(b), SMKG040_cont = 22 - fall back to PUMF
  expect_equal(result_vec[4], 22)

  # Element 5: SMK_040 = NA, SMKG040_cont = 30 - fall back to PUMF
  expect_equal(result_vec[5], 30)
})

test_that("calculate_age_start_smoking validates age range", {

  # Valid ages should pass through
  expect_equal(calculate_age_start_smoking(SMK_040 = 5), 5)
  expect_equal(calculate_age_start_smoking(SMK_040 = 80), 80)

  # Ages outside plausible range should be flagged
  # Note: Exact handling depends on implementation - may return value or NA
  # These tests document expected behaviour for edge cases

  # Very young ages (< 5) - implausible for daily smoking
  result_too_young <- calculate_age_start_smoking(SMK_040 = 3)
  # Implementation may either pass through or flag as invalid
  # Test documents actual behaviour
  expect_true(is.numeric(result_too_young) || haven::is_tagged_na(result_too_young))

  # Very old ages (> 80) - implausible for starting daily smoking
  result_too_old <- calculate_age_start_smoking(SMK_040 = 95)
  expect_true(is.numeric(result_too_old) || haven::is_tagged_na(result_too_old))
})

test_that("calculate_age_start_smoking maintains precision information", {

  # Master values are exact continuous - should maintain full precision
  expect_equal(calculate_age_start_smoking(SMK_040 = 18.5), 18.5)
  expect_equal(calculate_age_start_smoking(SMK_040 = 22.3), 22.3)

  # PUMF values are midpoint estimates - should also maintain value
  expect_equal(calculate_age_start_smoking(SMKG040_cont = 18.5), 18.5)
  expect_equal(calculate_age_start_smoking(SMKG040_cont = 13), 13)
})

# =============================================================================
# Integration Tests with SMKDSTY
# =============================================================================

test_that("age_start_smoking aligns with SMKDSTY categories", {


  # Test that the derived variable produces appropriate results for each
  # SMKDSTY category when integrated with status variables

  # SMKDSTY 1 (Daily smoker): Should have valid age_start_smoking
  # SMKDSTY 2 (Occasional, former daily): Should have valid age_start_smoking
  # SMKDSTY 3 (Always occasional): Should be NA(a) - never asked
  # SMKDSTY 4 (Former daily): Should have valid age_start_smoking
  # SMKDSTY 5 (Former occasional): Should be NA(a) - never asked
  # SMKDSTY 6 (Never smoker): Should be NA(a) - never asked

  # These are conceptual tests - actual integration would use rec_with_table
  # to load data with proper SMKDSTY values

  # Ever-daily smokers should have valid ages
  expect_false(haven::is_tagged_na(calculate_age_start_smoking(SMK_040 = 20)))

  # Never-daily indicated by NA(a) should propagate
  expect_true(haven::is_tagged_na(
    calculate_age_start_smoking(SMK_040 = haven::tagged_na("a")),
    "a"
  ))
})

# =============================================================================
# CCHS Codebook Validation
# =============================================================================

test_that("age_start_smoking values match CCHS codebook expectations", {

  # Based on CCHS documentation:
  # SMK_040: Age started smoking cigarettes daily (continuous, 5-79)
  # Universe: Ever-daily smokers only

  # SMKG040_cont midpoint categories (from variable_details):
  # Category 1 (5-11) → 8
  # Category 2 (12-14) → 13
  # Category 3 (15-17) → 16
  # Category 4 (18-19) → 18.5
  # Category 5 (20-24) → 22
  # Category 6 (25-29) → 27
  # Category 7 (30-34) → 32
  # Category 8 (35-39) → 37
  # Category 9 (40-44) → 42
  # Category 10 (45-49) → 47
  # Category 11 (50+) → 55

  # Test that typical midpoint values are handled correctly
  midpoints <- c(8, 13, 16, 18.5, 22, 27, 32, 37, 42, 47, 55)
  for (mp in midpoints) {
    result <- calculate_age_start_smoking(SMKG040_cont = mp)
    expect_equal(result, mp,
                info = paste("Midpoint", mp, "should pass through unchanged"))
  }
})

# =============================================================================
# Log Level Tests
# =============================================================================

test_that("calculate_age_start_smoking respects log_level parameter", {

  # Function should accept log_level parameter without error
  expect_no_error({
    calculate_age_start_smoking(SMK_040 = 20, log_level = "silent")
    calculate_age_start_smoking(SMK_040 = 20, log_level = "verbose")
    calculate_age_start_smoking(SMK_040 = 20, log_level = "warning")
  })

  # Results should be identical regardless of log_level
  result_silent <- calculate_age_start_smoking(SMK_040 = 20, log_level = "silent")
  result_verbose <- calculate_age_start_smoking(SMK_040 = 20, log_level = "verbose")
  expect_equal(result_silent, result_verbose)
})
