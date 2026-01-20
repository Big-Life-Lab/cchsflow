# =============================================================================
# time_quit_smoking Derived Variable Tests
# =============================================================================
#
# Tests for the unified time_quit_smoking derived variable that calculates
# years since quit smoking for former smokers.
#
# Source variables (by pathway):
# - SMK_09A_B: Time since quit (recent quitters, categories 1-4)
# - SMKG09C: Time since quit (longer-term, categories 1-3)
#
# The function uses priority logic:
# - SMK_09A_B values 1-3 map directly to 0.5, 1.5, 2.5 years
# - SMK_09A_B value 4 defers to SMKG09C for detail (4, 8, or 12 years)
#
# Universe: Former smokers (SMKDSTY categories 4, 5 or former daily/occasional)
# NA for: Current smokers, never smokers
#
# @note v3.0.0-alpha, last updated: 2026-01-11, status: active
# =============================================================================

library(testthat)
library(haven)
library(dplyr)

# =============================================================================
# Test Setup
# =============================================================================

# Load the function under test
setup_test_environment <- function() {
  tryCatch({
    source("R/smoking.R")
  }, error = function(e) {
    tryCatch({
      source("../../R/smoking.R")
    }, error = function(e2) {
      # Functions should be available via library(cchsflow) in package mode
    })
  })
}

setup_test_environment()

# =============================================================================
# Core Functionality Tests
# =============================================================================

test_that("calculate_time_quit_smoking maps SMK_09A_B categories 1-3 correctly", {

  # Category 1: Less than 1 year ago -> 0.5 years (midpoint)
  expect_equal(calculate_time_quit_smoking(SMK_09A_B = 1, SMKG09C = NA), 0.5)

  # Category 2: 1 to less than 2 years ago -> 1.5 years (midpoint)
  expect_equal(calculate_time_quit_smoking(SMK_09A_B = 2, SMKG09C = NA), 1.5)

  # Category 3: 2 to less than 3 years ago -> 2.5 years (midpoint)
  expect_equal(calculate_time_quit_smoking(SMK_09A_B = 3, SMKG09C = NA), 2.5)
})

test_that("calculate_time_quit_smoking defers to SMKG09C when SMK_09A_B is 4", {

  # Category 4 means "3 or more years" - need SMKG09C for detail

  # SMKG09C category 1: 3 to 5 years ago -> 4 years (midpoint)
  expect_equal(calculate_time_quit_smoking(SMK_09A_B = 4, SMKG09C = 1), 4)

  # SMKG09C category 2: 6 to 10 years ago -> 8 years (midpoint)
  expect_equal(calculate_time_quit_smoking(SMK_09A_B = 4, SMKG09C = 2), 8)

  # SMKG09C category 3: 11 or more years ago -> 12 years (conservative estimate)
  expect_equal(calculate_time_quit_smoking(SMK_09A_B = 4, SMKG09C = 3), 12)
})

test_that("calculate_time_quit_smoking ignores SMKG09C when SMK_09A_B is 1-3", {

  # When SMK_09A_B provides sufficient detail (1-3), SMKG09C should be ignored
  # This tests that SMKG09C values don't override SMK_09A_B

  expect_equal(calculate_time_quit_smoking(SMK_09A_B = 1, SMKG09C = 3), 0.5)
  expect_equal(calculate_time_quit_smoking(SMK_09A_B = 2, SMKG09C = 1), 1.5)
  expect_equal(calculate_time_quit_smoking(SMK_09A_B = 3, SMKG09C = 2), 2.5)

  # Even with tagged_na in SMKG09C, SMK_09A_B 1-3 should work
  expect_equal(calculate_time_quit_smoking(SMK_09A_B = 2, SMKG09C = haven::tagged_na("b")), 1.5)
})

# =============================================================================
# Missing Data Handling Tests
# =============================================================================

test_that("calculate_time_quit_smoking handles not applicable (code 6) correctly", {

  # Code 6 in CCHS typically means "not applicable"
  # For time_quit_smoking, this applies to current smokers and never smokers

  result_na_a <- calculate_time_quit_smoking(SMK_09A_B = 6, SMKG09C = 1)
  expect_true(haven::is_tagged_na(result_na_a))
  expect_equal(haven::na_tag(result_na_a), "a")
})

test_that("calculate_time_quit_smoking handles don't know/refusal (codes 7-9) correctly", {

  # Code 7 = Don't know, Code 8 = Refusal, Code 9 = Not stated
  # All should map to tagged_na("b")

  result_7 <- calculate_time_quit_smoking(SMK_09A_B = 7, SMKG09C = 1)
  expect_true(haven::is_tagged_na(result_7))
  expect_equal(haven::na_tag(result_7), "b")

  result_8 <- calculate_time_quit_smoking(SMK_09A_B = 8, SMKG09C = 1)
  expect_true(haven::is_tagged_na(result_8))
  expect_equal(haven::na_tag(result_8), "b")
})

test_that("calculate_time_quit_smoking propagates SMKG09C missing when needed", {

  # When SMK_09A_B = 4, we need SMKG09C
  # If SMKG09C is missing, that should propagate

  result_smkg_na_a <- calculate_time_quit_smoking(
    SMK_09A_B = 4,
    SMKG09C = haven::tagged_na("a")
  )
  expect_true(haven::is_tagged_na(result_smkg_na_a, "a"))

  result_smkg_na_b <- calculate_time_quit_smoking(
    SMK_09A_B = 4,
    SMKG09C = haven::tagged_na("b")
  )
  expect_true(haven::is_tagged_na(result_smkg_na_b, "b"))
})

test_that("calculate_time_quit_smoking handles invalid codes correctly", {

  # Invalid codes (e.g., 5, 10) should result in tagged_na("b")

  result_invalid <- calculate_time_quit_smoking(SMK_09A_B = 5, SMKG09C = 1)
  expect_true(haven::is_tagged_na(result_invalid))
  expect_equal(haven::na_tag(result_invalid), "b")
})

# =============================================================================
# Vector Input Tests
# =============================================================================

test_that("calculate_time_quit_smoking handles vector inputs correctly", {

  smk_a_vec <- c(1, 2, 3, 4, 4, 4, 6, 7)
  smk_c_vec <- c(NA, NA, NA, 1, 2, 3, NA, NA)

  result_vec <- calculate_time_quit_smoking(
    SMK_09A_B = smk_a_vec,
    SMKG09C = smk_c_vec
  )

  expect_length(result_vec, 8)

  # Check each element
  expect_equal(result_vec[1], 0.5)   # SMK_09A_B = 1
  expect_equal(result_vec[2], 1.5)   # SMK_09A_B = 2
  expect_equal(result_vec[3], 2.5)   # SMK_09A_B = 3
  expect_equal(result_vec[4], 4)     # SMK_09A_B = 4, SMKG09C = 1
  expect_equal(result_vec[5], 8)     # SMK_09A_B = 4, SMKG09C = 2
  expect_equal(result_vec[6], 12)    # SMK_09A_B = 4, SMKG09C = 3
  expect_true(haven::is_tagged_na(result_vec[7], "a"))  # Code 6
  expect_true(haven::is_tagged_na(result_vec[8], "b"))  # Code 7
})

test_that("calculate_time_quit_smoking handles mixed valid and missing vectors", {

  smk_a_vec <- c(1, haven::tagged_na("a"), 4, haven::tagged_na("b"), 2)
  smk_c_vec <- c(NA, NA, 2, NA, haven::tagged_na("b"))

  result_vec <- calculate_time_quit_smoking(
    SMK_09A_B = smk_a_vec,
    SMKG09C = smk_c_vec
  )

  expect_equal(result_vec[1], 0.5)
  expect_true(haven::is_tagged_na(result_vec[2], "a"))
  expect_equal(result_vec[3], 8)
  expect_true(haven::is_tagged_na(result_vec[4], "b"))
  expect_equal(result_vec[5], 1.5)  # SMKG09C ignored for SMK_09A_B = 2
})

# =============================================================================
# CCHS Codebook Validation Tests
# =============================================================================

test_that("time_quit_smoking mappings match CCHS codebook definitions", {

  # Based on CCHS documentation for SMK_09A_B:
  # 1 = "Less than 1 year ago"
  # 2 = "1 year to less than 2 years ago"
  # 3 = "2 years to less than 3 years ago"
  # 4 = "3 or more years ago"

  # Midpoint mappings:
  expect_equal(calculate_time_quit_smoking(1, NA), 0.5,
               info = "Category 1 (< 1 year) midpoint should be 0.5")
  expect_equal(calculate_time_quit_smoking(2, NA), 1.5,
               info = "Category 2 (1-2 years) midpoint should be 1.5")
  expect_equal(calculate_time_quit_smoking(3, NA), 2.5,
               info = "Category 3 (2-3 years) midpoint should be 2.5")

  # Based on CCHS documentation for SMKG09C:
  # 1 = "3 to 5 years ago"
  # 2 = "6 to 10 years ago"
  # 3 = "11 years ago or more"

  expect_equal(calculate_time_quit_smoking(4, 1), 4,
               info = "SMKG09C category 1 (3-5 years) midpoint should be 4")
  expect_equal(calculate_time_quit_smoking(4, 2), 8,
               info = "SMKG09C category 2 (6-10 years) midpoint should be 8")
  expect_equal(calculate_time_quit_smoking(4, 3), 12,
               info = "SMKG09C category 3 (11+ years) should be 12 (conservative)")
})

# =============================================================================
# Mathematical Property Tests
# =============================================================================

test_that("time_quit_smoking results are non-negative", {

  # All valid time values should be positive
  for (smk_09a_b in 1:4) {
    for (smkg09c in c(1, 2, 3)) {
      result <- calculate_time_quit_smoking(smk_09a_b, smkg09c)
      if (!haven::is_tagged_na(result) && !is.na(result)) {
        expect_true(result >= 0,
                    info = paste("Time should be non-negative for SMK_09A_B =",
                                 smk_09a_b, ", SMKG09C =", smkg09c))
      }
    }
  }
})

test_that("time_quit_smoking category ordering is monotonic", {

  # SMK_09A_B categories 1 < 2 < 3 should produce increasing time values
  result_1 <- calculate_time_quit_smoking(1, NA)
  result_2 <- calculate_time_quit_smoking(2, NA)
  result_3 <- calculate_time_quit_smoking(3, NA)

  expect_true(result_1 < result_2,
              "Category 1 time should be less than category 2")
  expect_true(result_2 < result_3,
              "Category 2 time should be less than category 3")

  # SMKG09C categories should also be monotonic
  result_c1 <- calculate_time_quit_smoking(4, 1)
  result_c2 <- calculate_time_quit_smoking(4, 2)
  result_c3 <- calculate_time_quit_smoking(4, 3)

  expect_true(result_c1 < result_c2,
              "SMKG09C category 1 should be less than category 2")
  expect_true(result_c2 < result_c3,
              "SMKG09C category 2 should be less than category 3")
})

test_that("time_quit_smoking transitions correctly at category 4 boundary", {

  # Category 3 max is "less than 3 years" -> midpoint 2.5
  # Category 4 with SMKG09C=1 is "3 to 5 years" -> midpoint 4
  # There should be a logical gap/transition

  result_3 <- calculate_time_quit_smoking(3, NA)  # 2.5 years

  result_4_1 <- calculate_time_quit_smoking(4, 1)  # 4 years

  expect_true(result_4_1 > result_3,
              "Category 4 (with SMKG09C=1) should be greater than category 3")
  expect_true(result_4_1 - result_3 >= 1,
              "Gap between category 3 and 4 should be at least 1 year")
})

# =============================================================================
# Integration with SMKDSTY Categories Tests
# =============================================================================

test_that("time_quit_smoking aligns with smoking status categories", {

  # time_quit_smoking is meaningful only for former smokers
  # SMKDSTY categories 4 (former daily) and 5 (former occasional) should have values
  # SMKDSTY categories 1-3 (current) and 6 (never) should be NA(a)


  # For former smokers, valid time values expected
  expect_false(haven::is_tagged_na(calculate_time_quit_smoking(SMK_09A_B = 2, SMKG09C = NA)))

  # Code 6 (not applicable) represents current/never smokers
  expect_true(haven::is_tagged_na(
    calculate_time_quit_smoking(SMK_09A_B = 6, SMKG09C = NA),
    "a"
  ))
})

# =============================================================================
# Log Level Tests
# =============================================================================

test_that("calculate_time_quit_smoking respects log_level parameter", {

  # Function should accept log_level parameter without error
  expect_no_error({
    calculate_time_quit_smoking(SMK_09A_B = 2, SMKG09C = NA, log_level = "silent")
    calculate_time_quit_smoking(SMK_09A_B = 2, SMKG09C = NA, log_level = "verbose")
    calculate_time_quit_smoking(SMK_09A_B = 2, SMKG09C = NA, log_level = "warning")
  })

  # Results should be identical regardless of log_level
  result_silent <- calculate_time_quit_smoking(SMK_09A_B = 2, SMKG09C = NA, log_level = "silent")
  result_verbose <- calculate_time_quit_smoking(SMK_09A_B = 2, SMKG09C = NA, log_level = "verbose")
  expect_equal(result_silent, result_verbose)
})

# =============================================================================
# Edge Cases Tests
# =============================================================================

test_that("calculate_time_quit_smoking handles empty inputs", {

  # Empty vectors should return empty vectors
  result_empty <- calculate_time_quit_smoking(
    SMK_09A_B = numeric(0),
    SMKG09C = numeric(0)
  )
  expect_length(result_empty, 0)
})

test_that("calculate_time_quit_smoking handles single element inputs", {

  # Single element should work correctly
  expect_equal(calculate_time_quit_smoking(SMK_09A_B = 1, SMKG09C = NA), 0.5)
  expect_equal(calculate_time_quit_smoking(SMK_09A_B = 4, SMKG09C = 2), 8)
})
