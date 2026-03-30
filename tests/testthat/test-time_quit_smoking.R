# =============================================================================
# Smoking cessation derived variable tests
# =============================================================================
#
# Tests for the smoking cessation DV function hierarchy:
#
# Foundational functions (categorical -> continuous midpoint conversion):
#   calculate_SMK_06A_cont(SMK_06A_2003plus, SMKG06C) - former occasional smokers
#   SMK_10A_cont: worksheet-only (no R function, SMKG10C does not exist)
#   SMK_09A_cont: worksheet-only direct recode
#
# Combining functions (pathway-aware):
#   calculate_time_quit_smoking_complete(SMKDSTY_cat5, SMK_10_gate, ...)
#   calculate_time_quit_smoking_daily(SMKDSTY_cat5, SMK_09A_cont, SMK_09C)
#
# =============================================================================

library(testthat)
library(haven)

# =============================================================================
# calculate_SMK_06A_cont - Former occasional smoker midpoint conversion
# =============================================================================

test_that("calculate_SMK_06A_cont maps categories 1-3 to midpoints", {

  expect_equal(
    calculate_SMK_06A_cont(SMK_06A_2003plus = 1, SMKG06C = NA),
    0.5
  )
  expect_equal(
    calculate_SMK_06A_cont(SMK_06A_2003plus = 2, SMKG06C = NA),
    1.5
  )
  expect_equal(
    calculate_SMK_06A_cont(SMK_06A_2003plus = 3, SMKG06C = NA),
    2.5
  )
})

test_that("calculate_SMK_06A_cont uses SMKG06C for category 4", {

  expect_equal(
    calculate_SMK_06A_cont(SMK_06A_2003plus = 4, SMKG06C = 7.5),
    7.5
  )

  # Fallback without companion
  expect_equal(
    calculate_SMK_06A_cont(SMK_06A_2003plus = 4, SMKG06C = NA),
    5.0
  )
})

# =============================================================================
# calculate_time_quit_smoking_complete - Pathway-aware combining function
# =============================================================================

test_that("calculate_time_quit_smoking_complete uses SMKDVSTP when available (Master)", {

  # Use 12.0 not 7.0 — clean_variables() auto-detection treats single-digit
  # integers as missing codes when database context is unavailable
  result <- calculate_time_quit_smoking_complete(
    SMKDSTY_cat5 = 3, SMK_10_gate = 1,
    SMK_06A_cont = NA, SMK_09A_cont = 2.5, SMK_10A_cont = NA,
    SMKDVSTP = 12.0
  )
  expect_equal(result, 12.0)
})

test_that("calculate_time_quit_smoking_complete routes former occasional to SMK_06A_cont", {

  result <- calculate_time_quit_smoking_complete(
    SMKDSTY_cat5 = 4, SMK_10_gate = NA,
    SMK_06A_cont = 5.0, SMK_09A_cont = NA, SMK_10A_cont = NA,
    SMKDVSTP = NA
  )
  expect_equal(result, 5.0)
})

test_that("calculate_time_quit_smoking_complete routes direct quitter to SMK_09A_cont", {

  result <- calculate_time_quit_smoking_complete(
    SMKDSTY_cat5 = 3, SMK_10_gate = 1,
    SMK_06A_cont = NA, SMK_09A_cont = 3.5, SMK_10A_cont = NA,
    SMKDVSTP = NA
  )
  expect_equal(result, 3.5)
})

test_that("calculate_time_quit_smoking_complete routes gradual reducer to SMK_10A_cont", {

  result <- calculate_time_quit_smoking_complete(
    SMKDSTY_cat5 = 3, SMK_10_gate = 2,
    SMK_06A_cont = NA, SMK_09A_cont = 5.0, SMK_10A_cont = 2.0,
    SMKDVSTP = NA
  )
  expect_equal(result, 2.0)
})

test_that("calculate_time_quit_smoking_complete uses SMK_09A_cont as 2001 fallback", {

  # 2001: no gate available (NA), falls back to SMK_09A_cont
  result <- calculate_time_quit_smoking_complete(
    SMKDSTY_cat5 = 3, SMK_10_gate = NA,
    SMK_06A_cont = NA, SMK_09A_cont = 4.0, SMK_10A_cont = NA,
    SMKDVSTP = NA
  )
  expect_equal(result, 4.0)
})

test_that("calculate_time_quit_smoking_complete returns NA::a for non-formers", {

  # Current daily smoker
  result <- calculate_time_quit_smoking_complete(
    SMKDSTY_cat5 = 1, SMK_10_gate = NA,
    SMK_06A_cont = NA, SMK_09A_cont = NA, SMK_10A_cont = NA,
    SMKDVSTP = NA
  )
  expect_true(is.na(result))

  # Never smoker
  result <- calculate_time_quit_smoking_complete(
    SMKDSTY_cat5 = 5, SMK_10_gate = NA,
    SMK_06A_cont = NA, SMK_09A_cont = NA, SMK_10A_cont = NA,
    SMKDVSTP = NA
  )
  expect_true(is.na(result))
})

# =============================================================================
# calculate_time_quit_smoking_daily - Former daily smokers
# =============================================================================

test_that("calculate_time_quit_smoking_daily uses SMK_09C when available (Master)", {

  # Use 12.0 not 7.0 — clean_variables() auto-detection treats single-digit
  # integers as missing codes when database context is unavailable
  result <- calculate_time_quit_smoking_daily(
    SMKDSTY_cat5 = 3, SMK_09A_cont = 2.5, SMK_09C = 12.0
  )
  expect_equal(result, 12.0)
})

test_that("calculate_time_quit_smoking_daily falls back to SMK_09A_cont (PUMF)", {

  result <- calculate_time_quit_smoking_daily(
    SMKDSTY_cat5 = 3, SMK_09A_cont = 2.5, SMK_09C = NA
  )
  expect_equal(result, 2.5)
})

test_that("calculate_time_quit_smoking_daily returns NA::a for non-daily formers", {

  # Former occasional (never daily)
  result <- calculate_time_quit_smoking_daily(
    SMKDSTY_cat5 = 4, SMK_09A_cont = NA, SMK_09C = NA
  )
  expect_true(is.na(result))

  # Never smoker
  result <- calculate_time_quit_smoking_daily(
    SMKDSTY_cat5 = 5, SMK_09A_cont = NA, SMK_09C = NA
  )
  expect_true(is.na(result))
})

test_that("calculate_time_quit_smoking_daily works without SMK_09C (NULL)", {

  result <- calculate_time_quit_smoking_daily(
    SMKDSTY_cat5 = 3, SMK_09A_cont = 1.5
  )
  expect_equal(result, 1.5)
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
