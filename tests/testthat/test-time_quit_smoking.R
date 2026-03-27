# =============================================================================
# Smoking cessation derived variable tests
# =============================================================================
#
# Tests for the smoking cessation DV function hierarchy:
#
# Foundational functions (categorical -> continuous midpoint conversion):
#   calculate_SMK_09A_cont(SMK_09A_2003plus, SMK_09C) - former daily smokers
#   calculate_SMK_06A_cont(SMK_06A_cat4, SMKG06C)     - former occasional smokers
#   calculate_SMK_10A_cont(SMK_10A, SMKG10C)           - former daily who continued occasional
#
# Combining functions (priority selection across continuous sources):
#   calculate_time_quit_smoking_complete(SMK_09A_cont, SMK_06A_cont)
#   calculate_time_quit_smoking_daily(SMK_09A_cont, SMK_09C)
#
# =============================================================================

library(testthat)
library(haven)

# =============================================================================
# calculate_SMK_09A_cont - Former daily smoker midpoint conversion
# =============================================================================

test_that("calculate_SMK_09A_cont maps categories 1-3 to midpoints", {

  # Category 1: Less than 1 year ago -> 0.5 years
  expect_equal(
    calculate_SMK_09A_cont(SMK_09A_2003plus =1, SMKG09C = NA),
    0.5
  )

  # Category 2: 1 to less than 2 years ago -> 1.5 years
  expect_equal(
    calculate_SMK_09A_cont(SMK_09A_2003plus =2, SMKG09C = NA),
    1.5
  )

  # Category 3: 2 to less than 3 years ago -> 2.5 years
  expect_equal(
    calculate_SMK_09A_cont(SMK_09A_2003plus =3, SMKG09C = NA),
    2.5
  )
})

test_that("calculate_SMK_09A_cont uses SMKG09C for category 4", {

  # Category 4 with continuous companion -> use continuous value
  expect_equal(
    calculate_SMK_09A_cont(SMK_09A_2003plus =4, SMKG09C = 10.0),
    10.0
  )

  # Category 4 without companion -> fallback to 5.0
  expect_equal(
    calculate_SMK_09A_cont(SMK_09A_2003plus =4, SMKG09C = NA),
    5.0
  )
})

test_that("calculate_SMK_09A_cont ignores SMKG09C for categories 1-3", {

  # SMKG09C values should not affect categories 1-3
  expect_equal(
    calculate_SMK_09A_cont(SMK_09A_2003plus =1, SMKG09C = 10.0),
    0.5
  )
  expect_equal(
    calculate_SMK_09A_cont(SMK_09A_2003plus =2, SMKG09C = 5.0),
    1.5
  )
  expect_equal(
    calculate_SMK_09A_cont(SMK_09A_2003plus =3, SMKG09C = 15.0),
    2.5
  )
})

test_that("calculate_SMK_09A_cont handles vector inputs", {

  smk_cat <- c(1, 2, 3, 4, 4)
  smkg09c <- c(NA, NA, NA, 5.5, NA)

  result <- calculate_SMK_09A_cont(SMK_09A_2003plus =smk_cat, SMKG09C = smkg09c)

  expect_length(result, 5)
  expect_equal(result[1], 0.5)
  expect_equal(result[2], 1.5)
  expect_equal(result[3], 2.5)
  expect_equal(result[4], 5.5)   # Continuous companion used
  expect_equal(result[5], 5.0)   # Fallback for cat 4 without companion
})

test_that("calculate_SMK_09A_cont category ordering is monotonic", {

  result_1 <- calculate_SMK_09A_cont(SMK_09A_2003plus =1, SMKG09C = NA)
  result_2 <- calculate_SMK_09A_cont(SMK_09A_2003plus =2, SMKG09C = NA)
  result_3 <- calculate_SMK_09A_cont(SMK_09A_2003plus =3, SMKG09C = NA)

  expect_true(result_1 < result_2)
  expect_true(result_2 < result_3)
})

# =============================================================================
# calculate_SMK_06A_cont - Former occasional smoker midpoint conversion
# =============================================================================

test_that("calculate_SMK_06A_cont maps categories 1-3 to midpoints", {

  expect_equal(
    calculate_SMK_06A_cont(SMK_06A_cat4 = 1, SMKG06C = NA),
    0.5
  )
  expect_equal(
    calculate_SMK_06A_cont(SMK_06A_cat4 = 2, SMKG06C = NA),
    1.5
  )
  expect_equal(
    calculate_SMK_06A_cont(SMK_06A_cat4 = 3, SMKG06C = NA),
    2.5
  )
})

test_that("calculate_SMK_06A_cont uses SMKG06C for category 4", {

  expect_equal(
    calculate_SMK_06A_cont(SMK_06A_cat4 = 4, SMKG06C = 7.5),
    7.5
  )

  # Fallback without companion
  expect_equal(
    calculate_SMK_06A_cont(SMK_06A_cat4 = 4, SMKG06C = NA),
    5.0
  )
})

# =============================================================================
# calculate_SMK_10A_cont - Quit completely timing (former daily who continued)
# =============================================================================

test_that("calculate_SMK_10A_cont maps categories 1-3 to midpoints", {

  expect_equal(
    calculate_SMK_10A_cont(SMK_10A = 1, SMKG10C = NA),
    0.5
  )
  expect_equal(
    calculate_SMK_10A_cont(SMK_10A = 2, SMKG10C = NA),
    1.5
  )
  expect_equal(
    calculate_SMK_10A_cont(SMK_10A = 3, SMKG10C = NA),
    2.5
  )
})

test_that("calculate_SMK_10A_cont uses SMKG10C for category 4", {

  # Use 12.0 not 8.0 — clean_variables() auto-detection treats integer 8 as a
  # missing code (single-digit pattern) when database context is unavailable
  expect_equal(
    calculate_SMK_10A_cont(SMK_10A = 4, SMKG10C = 12.0),
    12.0
  )

  # Fallback without companion
  expect_equal(
    calculate_SMK_10A_cont(SMK_10A = 4, SMKG10C = NA),
    5.0
  )
})

# =============================================================================
# calculate_time_quit_smoking_complete - Combining function (priority logic)
# =============================================================================

test_that("calculate_time_quit_smoking_complete prioritises SMK_09A_cont", {

  # SMK_09A_cont available -> use it
  result <- calculate_time_quit_smoking_complete(
    SMK_09A_cont = 3.5,
    SMK_06A_cont = NA
  )
  expect_equal(result, 3.5)
})

test_that("calculate_time_quit_smoking_complete falls back to SMK_06A_cont", {

  # SMK_09A_cont missing, SMK_06A_cont available -> use SMK_06A_cont
  result <- calculate_time_quit_smoking_complete(
    SMK_09A_cont = NA,
    SMK_06A_cont = 5.0
  )
  expect_equal(result, 5.0)
})

test_that("calculate_time_quit_smoking_complete handles vector inputs", {

  smk_09a <- c(3.5, NA, NA, 2.0)
  smk_06a <- c(NA, 5.0, NA, NA)

  result <- calculate_time_quit_smoking_complete(
    SMK_09A_cont = smk_09a,
    SMK_06A_cont = smk_06a
  )

  expect_length(result, 4)
  expect_equal(result[1], 3.5)   # SMK_09A_cont used
  expect_equal(result[2], 5.0)   # Fell back to SMK_06A_cont
  expect_true(is.na(result[3]))  # Both missing -> NA
  expect_equal(result[4], 2.0)   # SMK_09A_cont used
})

test_that("calculate_time_quit_smoking_complete results are non-negative", {

  # All valid results should be non-negative
  result <- calculate_time_quit_smoking_complete(
    SMK_09A_cont = c(0.5, 1.5, 2.5, 5.0, 10.0),
    SMK_06A_cont = c(NA, NA, NA, NA, NA)
  )

  valid <- result[!is.na(result)]
  for (i in seq_along(valid)) {
    expect_true(valid[i] >= 0)
  }
})

test_that("calculate_time_quit_smoking_complete handles single element inputs", {

  expect_equal(
    calculate_time_quit_smoking_complete(SMK_09A_cont = 0.5, SMK_06A_cont = NA),
    0.5
  )
  expect_equal(
    calculate_time_quit_smoking_complete(SMK_09A_cont = NA, SMK_06A_cont = 2.5),
    2.5
  )
})

# =============================================================================
# calculate_time_quit_smoking_daily - stub (not yet implemented)
# =============================================================================

test_that("calculate_time_quit_smoking_daily throws not-implemented error", {

  # Function is a stub; must error with an informative message
  expect_error(
    calculate_time_quit_smoking_daily(SMK_09A_cont = 3.5, SMK_09C = NULL),
    "not yet implemented"
  )
})

# =============================================================================
# Legacy function compatibility
# =============================================================================

test_that("time_quit_smoking_fun still exists and works (legacy)", {

  # Legacy function in R/smoking.R should still work
  result <- time_quit_smoking_fun(SMK_09A_B = 1, SMKG09C = NA)
  expect_equal(result, 0.5)

  result <- time_quit_smoking_fun(SMK_09A_B = 4, SMKG09C = 2)
  expect_equal(result, 8)
})
