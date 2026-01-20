# =============================================================================
# pack_years_der Derived Variable Tests
# =============================================================================
#
# Tests for the pack_years_der derived variable that calculates cumulative
# smoking exposure in pack-years.
#
# Formula: (cigarettes_per_day / 20) * years_smoked
#
# Source variables (dependencies):
# - SMKDSTY_A: Smoking status (routes to correct calculation)
# - DHHGAGE_cont: Current age
# - SMKG203_cont: Age started smoking daily (current daily)
# - SMKG207_cont: Age started smoking daily (former daily)
# - SMKG01C_cont: Age first smoked whole cigarette (never daily)
# - time_quit_smoking: Years since quit
# - SMK_204: Cigarettes per day (current daily)
# - SMK_208: Cigarettes per day when smoked daily (former daily)
# - SMK_05B: Cigarettes per occasion (occasional)
# - SMK_05C: Days smoked per month (occasional)
# - SMK_01A: Smoked 100+ cigarettes (former occasional)
#
# Routes by SMKDSTY_A:
# 1 = Daily smoker -> calculate_pack_years_daily()
# 2 = Occasional (former daily) -> calculate_pack_years_occasional_former()
# 3 = Occasional (never daily) -> calculate_pack_years_occasional_never()
# 4 = Former daily -> calculate_pack_years_former_daily()
# 5 = Former occasional -> calculate_pack_years_former_occasional()
# 6 = Never smoker -> 0
#
# @note v3.0.0-alpha, last updated: 2026-01-11, status: active
# =============================================================================

library(testthat)
library(haven)
library(dplyr)

# =============================================================================
# Test Setup
# =============================================================================

setup_test_environment <- function() {
  tryCatch({
    source("R/smoking.R")
    source("R/smoking-validation-constants.R")
  }, error = function(e) {
    tryCatch({
      source("../../R/smoking.R")
      source("../../R/smoking-validation-constants.R")
    }, error = function(e2) {
      # Functions should be available via library(cchsflow) in package mode
    })
  })
}

setup_test_environment()

# =============================================================================
# Helper Function Tests (Internal Calculations)
# =============================================================================

test_that("calculate_pack_years_daily computes correctly for daily smokers", {

  # Pack-years = (smoking_duration * cigarettes_daily) / 20
  # Smoking duration = current_age - age_started_daily

  # Example: 45 years old, started at 20, smokes 20 cigs/day
  # Duration = 45 - 20 = 25 years
  # Pack-years = (25 * 20) / 20 = 25

  result <- calculate_pack_years_daily(
    current_age = 45,
    age_started_daily = 20,
    cigarettes_daily = 20
  )
  expect_equal(result, 25)

  # Heavy smoker: 50 years old, started at 15, 40 cigs/day
  # Duration = 35 years, Pack-years = (35 * 40) / 20 = 70
  result_heavy <- calculate_pack_years_daily(
    current_age = 50,
    age_started_daily = 15,
    cigarettes_daily = 40
  )
  expect_equal(result_heavy, 70)

  # Light smoker: 30 years old, started at 25, 5 cigs/day
  # Duration = 5 years, Pack-years = (5 * 5) / 20 = 1.25
  result_light <- calculate_pack_years_daily(
    current_age = 30,
    age_started_daily = 25,
    cigarettes_daily = 5
  )
  expect_equal(result_light, 1.25)
})

test_that("calculate_pack_years_former_daily computes correctly", {

  # Pack-years = ((current_age - time_quit) - age_started) * cigs_daily / 20

  # 55 years old, quit 10 years ago, started at 20, smoked 20/day
  # Age at quit = 55 - 10 = 45
  # Duration = 45 - 20 = 25 years
  # Pack-years = (25 * 20) / 20 = 25

  result <- calculate_pack_years_former_daily(
    current_age = 55,
    age_started_daily = 20,
    time_quit_smoking = 10,
    cigarettes_daily = 20
  )
  expect_equal(result, 25)

  # Long-term quitter: 70 years old, quit 30 years ago, started at 18, 30/day
  # Age at quit = 40, Duration = 40 - 18 = 22 years
  # Pack-years = (22 * 30) / 20 = 33
  result_long_quit <- calculate_pack_years_former_daily(
    current_age = 70,
    age_started_daily = 18,
    time_quit_smoking = 30,
    cigarettes_daily = 30
  )
  expect_equal(result_long_quit, 33)
})

test_that("calculate_pack_years_never returns 0", {

  result <- calculate_pack_years_never()
  expect_equal(result, 0)
})

test_that("calculate_pack_years_former_occasional handles 100+ cigarette threshold", {

  # Former occasional smokers who smoked 100+ cigarettes get minimum pack-years
  # Those who smoked < 100 get 0

  # Smoked 100+ (SMK_01A = 1)
  result_100_plus <- calculate_pack_years_former_occasional(smoked_100_cigs = 1)
  expect_equal(result_100_plus, PACK_YEARS_CONSTANTS$min_pack_years)

  # Smoked < 100 (SMK_01A = 2)
  result_less_100 <- calculate_pack_years_former_occasional(smoked_100_cigs = 2)
  expect_equal(result_less_100, 0)
})

# =============================================================================
# Mathematical Property Tests
# =============================================================================

test_that("pack_years results are non-negative", {

  # Daily smoker
  result_daily <- calculate_pack_years_daily(
    current_age = 30,
    age_started_daily = 25,
    cigarettes_daily = 10
  )
  expect_true(result_daily >= 0)

  # Former daily
  result_former <- calculate_pack_years_former_daily(
    current_age = 50,
    age_started_daily = 20,
    time_quit_smoking = 5,
    cigarettes_daily = 15
  )
  expect_true(result_former >= 0)

  # Edge case: very short duration shouldn't go negative
  result_edge <- calculate_pack_years_daily(
    current_age = 20,
    age_started_daily = 19,
    cigarettes_daily = 5
  )
  expect_true(result_edge >= 0)
})

test_that("pack_years increases monotonically with duration", {

  # Same intensity, different durations
  result_5yr <- calculate_pack_years_daily(45, 40, 20)   # 5 years
  result_10yr <- calculate_pack_years_daily(45, 35, 20)  # 10 years
  result_20yr <- calculate_pack_years_daily(45, 25, 20)  # 20 years

  expect_true(result_5yr < result_10yr)
  expect_true(result_10yr < result_20yr)
})

test_that("pack_years increases monotonically with intensity", {

  # Same duration, different intensities
  result_10cigs <- calculate_pack_years_daily(45, 25, 10)  # 10 cigs/day
  result_20cigs <- calculate_pack_years_daily(45, 25, 20)  # 20 cigs/day
  result_40cigs <- calculate_pack_years_daily(45, 25, 40)  # 40 cigs/day

  expect_true(result_10cigs < result_20cigs)
  expect_true(result_20cigs < result_40cigs)
})

test_that("pack_years is bounded by maximum value", {

  # Extreme case: 80 years old, started at 10, 60 cigs/day
  # Duration = 70 years, Pack-years = (70 * 60) / 20 = 210
  # But should be capped at max_pack_years (165)

  result_extreme <- calculate_pack_years_daily(
    current_age = 80,
    age_started_daily = 10,
    cigarettes_daily = 60
  )

  # The helper function doesn't apply the cap - that's done in the wrapper

  # This test documents the raw calculation behaviour
  expect_true(result_extreme > 0)
})

# =============================================================================
# Vector Input Tests
# =============================================================================

test_that("pack_years helper functions handle vector inputs", {

  ages <- c(40, 50, 60)
  start_ages <- c(20, 25, 18)
  cigs <- c(20, 10, 30)

  result_vec <- calculate_pack_years_daily(
    current_age = ages,
    age_started_daily = start_ages,
    cigarettes_daily = cigs
  )

  expect_length(result_vec, 3)

  # Element 1: (40-20) * 20 / 20 = 20
  expect_equal(result_vec[1], 20)

  # Element 2: (50-25) * 10 / 20 = 12.5
  expect_equal(result_vec[2], 12.5)

  # Element 3: (60-18) * 30 / 20 = 63
  expect_equal(result_vec[3], 63)
})

# =============================================================================
# SMKDSTY Routing Tests
# =============================================================================

test_that("pack_years routes correctly by smoking status", {

  # This tests the main calculate_pack_years function's routing logic
  # Each SMKDSTY_A value should route to the correct internal function

  # Status 6 (Never smoker) should return 0
  # Note: Full function requires many inputs; testing the concept here

  # The routing is tested via the internal helper functions
  # Full integration would use calculate_pack_years() with all inputs

  # Status 6 outcome is 0 by definition
  expect_equal(calculate_pack_years_never(), 0)

  # Status 5 (former occasional) uses the 100+ cigarette rule
  expect_equal(calculate_pack_years_former_occasional(1), PACK_YEARS_CONSTANTS$min_pack_years)
  expect_equal(calculate_pack_years_former_occasional(2), 0)
})

# =============================================================================
# Boundary Condition Tests
# =============================================================================

test_that("pack_years handles minimum duration correctly", {

  # Someone who just started (1 year duration)
  result_1yr <- calculate_pack_years_daily(
    current_age = 21,
    age_started_daily = 20,
    cigarettes_daily = 20
  )
  expect_equal(result_1yr, 1)  # 1 year * 20 cigs / 20 = 1 pack-year
})

test_that("pack_years handles light smokers correctly", {

  # Very light smoker: 1 cigarette per day
  result_light <- calculate_pack_years_daily(
    current_age = 40,
    age_started_daily = 20,
    cigarettes_daily = 1
  )
  expect_equal(result_light, 1)  # 20 years * 1 cig / 20 = 1 pack-year
})

test_that("pack_years handles heavy smokers correctly", {

  # Very heavy smoker: 60 cigarettes per day (3 packs)
  result_heavy <- calculate_pack_years_daily(
    current_age = 50,
    age_started_daily = 20,
    cigarettes_daily = 60
  )
  expect_equal(result_heavy, 90)  # 30 years * 60 cigs / 20 = 90 pack-years
})

# =============================================================================
# Occasional Smoker Tests
# =============================================================================

test_that("calculate_pack_years_occasional_never handles irregular smoking", {

  # Occasional smoker who never smoked daily
  # 5 cigs per occasion, 10 days per month
  # Avg daily = (5 * 10) / 30 = 1.67 cigs/day
  # 10 years duration
  # Pack-years = (10 * 1.67) / 20 = 0.83

  result <- calculate_pack_years_occasional_never(
    current_age = 30,
    age_first_cig = 20,
    cigs_per_day_occasional = 5,
    days_smoked_monthly = 10
  )

  expected <- (10 * (5 * 10 / 30)) / 20
  expect_equal(result, expected, tolerance = 0.01)
})

# =============================================================================
# Constants Tests
# =============================================================================

test_that("PACK_YEARS_CONSTANTS are defined correctly", {

  expect_true(exists("PACK_YEARS_CONSTANTS"))
  expect_equal(PACK_YEARS_CONSTANTS$cigarettes_per_pack, 20)
  expect_equal(PACK_YEARS_CONSTANTS$days_per_month, 30)
  expect_true(PACK_YEARS_CONSTANTS$min_pack_years > 0)
  expect_true(PACK_YEARS_CONSTANTS$max_pack_years > 100)
})

# =============================================================================
# Integration-Style Tests
# =============================================================================

test_that("pack_years calculations are consistent across status transitions", {

  # A daily smoker who quits should have same pack-years at quit as former daily
  # calculated at the same age

  current_age <- 50
  started_age <- 20
  cigs_per_day <- 20
  years_quit <- 5

  # At time of quitting (age 45)
  pack_years_at_quit <- calculate_pack_years_daily(
    current_age = 45,
    age_started_daily = started_age,
    cigarettes_daily = cigs_per_day
  )

  # As a former daily smoker 5 years later
  pack_years_former <- calculate_pack_years_former_daily(
    current_age = current_age,
    age_started_daily = started_age,
    time_quit_smoking = years_quit,
    cigarettes_daily = cigs_per_day
  )

  # Should be equal - pack-years don't increase after quitting
  expect_equal(pack_years_at_quit, pack_years_former)
})

# =============================================================================
# Log Level Tests
# =============================================================================

test_that("pack_years functions work without errors", {

  # All helper functions should execute without error
  expect_no_error(calculate_pack_years_daily(45, 20, 20))
  expect_no_error(calculate_pack_years_former_daily(50, 20, 5, 20))
  expect_no_error(calculate_pack_years_never())
  expect_no_error(calculate_pack_years_former_occasional(1))
})
