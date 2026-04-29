# =============================================================================
# pack_years_der Derived Variable Tests
# =============================================================================
#
# Tests for the calculate_pack_years() function (modular architecture) and
# PACK_YEARS_CONSTANTS.
#
# calculate_pack_years() routes by smoking_status:
#   1 = Daily smoker:                (age - age_start) * (cigs / 20)
#   2 = Occasional (former daily):   daily_period + occasional_period
#   3 = Occasional (never daily):    (cigs * days/30) / 20 * duration
#   4 = Former daily:                (age - age_start - time_quit) * (cigs / 20)
#   5 = Former occasional:           min_pack_years or min_pack_years_alt
#   6 = Never smoker:                0
#
# =============================================================================

library(testthat)
library(haven)

# =============================================================================
# PACK_YEARS_CONSTANTS
# =============================================================================

test_that("PACK_YEARS_CONSTANTS are defined correctly", {

  expect_true(exists("PACK_YEARS_CONSTANTS"))
  expect_equal(PACK_YEARS_CONSTANTS$cigarettes_per_pack, 20)
  expect_equal(PACK_YEARS_CONSTANTS$days_per_month, 30)
  expect_true(PACK_YEARS_CONSTANTS$min_pack_years > 0)
  expect_equal(PACK_YEARS_CONSTANTS$min_pack_years, 0.0137)
  expect_equal(PACK_YEARS_CONSTANTS$min_pack_years_alt, 0.007)
  expect_equal(PACK_YEARS_CONSTANTS$max_pack_years, 165)
})

# =============================================================================
# Status 1 - Daily smokers
# =============================================================================

test_that("calculate_pack_years computes correctly for daily smokers (status 1)", {

  # Pack-years = (age - age_start) * (cigs_per_day / 20)
  # 45 years old, started at 20, 20 cigs/day -> (45-20) * 20/20 = 25
  result <- calculate_pack_years(
    smoking_status = 1,
    age = 45,
    age_start_smoking = 20,
    cigs_per_day = 20,
    time_quit_smoking = NA
  )
  expect_equal(result, 25)
})

test_that("calculate_pack_years handles light daily smokers", {

  # 30yo, started at 25, 5 cigs/day -> (30-25) * 5/20 = 1.25
  result <- calculate_pack_years(
    smoking_status = 1,
    age = 30,
    age_start_smoking = 25,
    cigs_per_day = 5,
    time_quit_smoking = NA
  )
  expect_equal(result, 1.25)
})

# =============================================================================
# Status 4 - Former daily smokers
# =============================================================================

test_that("calculate_pack_years computes correctly for former daily smokers (status 4)", {

  # 55yo, started at 20, quit 10 years ago, 20 cigs/day
  # (55 - 20 - 10) * 20/20 = 25
  result <- calculate_pack_years(
    smoking_status = 4,
    age = 55,
    age_start_smoking = 20,
    cigs_per_day = 20,
    time_quit_smoking = 10
  )
  expect_equal(result, 25)
})

# =============================================================================
# Status 5 - Former occasional smokers
# =============================================================================

test_that("calculate_pack_years handles former occasional smokers (status 5)", {

  # SMK_01A = 1 (100+ cigarettes) -> min_pack_years = 0.0137
  result_100_plus <- calculate_pack_years(
    smoking_status = 5,
    age = 50,
    age_start_smoking = NA,
    cigs_per_day = NA,
    time_quit_smoking = NA,
    smoked_100_lifetime = 1
  )
  expect_equal(result_100_plus, PACK_YEARS_CONSTANTS$min_pack_years)

  # SMK_01A = 2 (< 100 cigarettes) -> min_pack_years_alt = 0.007
  result_less_100 <- calculate_pack_years(
    smoking_status = 5,
    age = 50,
    age_start_smoking = NA,
    cigs_per_day = NA,
    time_quit_smoking = NA,
    smoked_100_lifetime = 2
  )
  expect_equal(result_less_100, PACK_YEARS_CONSTANTS$min_pack_years_alt)
})

# =============================================================================
# Status 6 - Never smokers
# =============================================================================

test_that("calculate_pack_years returns 0 for never smokers (status 6)", {

  result <- calculate_pack_years(
    smoking_status = 6,
    age = 50,
    age_start_smoking = NA,
    cigs_per_day = NA,
    time_quit_smoking = NA
  )
  expect_equal(result, 0)
})

# =============================================================================
# Mathematical properties
# =============================================================================

test_that("pack_years results are non-negative for daily smokers", {

  result <- calculate_pack_years(
    smoking_status = 1,
    age = 21,
    age_start_smoking = 20,
    cigs_per_day = 5,
    time_quit_smoking = NA
  )
  expect_true(result >= 0)
})

test_that("pack_years increases monotonically with duration", {

  # Same intensity (20 cigs/day), different durations
  result_5yr <- calculate_pack_years(
    smoking_status = 1, age = 25,
    age_start_smoking = 20, cigs_per_day = 20, time_quit_smoking = NA
  )
  result_10yr <- calculate_pack_years(
    smoking_status = 1, age = 30,
    age_start_smoking = 20, cigs_per_day = 20, time_quit_smoking = NA
  )
  result_20yr <- calculate_pack_years(
    smoking_status = 1, age = 40,
    age_start_smoking = 20, cigs_per_day = 20, time_quit_smoking = NA
  )

  expect_true(result_5yr < result_10yr)
  expect_true(result_10yr < result_20yr)
})

test_that("pack_years increases monotonically with intensity", {

  # Same duration (20 years), different intensities
  result_10 <- calculate_pack_years(
    smoking_status = 1, age = 40,
    age_start_smoking = 20, cigs_per_day = 10, time_quit_smoking = NA
  )
  result_20 <- calculate_pack_years(
    smoking_status = 1, age = 40,
    age_start_smoking = 20, cigs_per_day = 20, time_quit_smoking = NA
  )
  result_40 <- calculate_pack_years(
    smoking_status = 1, age = 40,
    age_start_smoking = 20, cigs_per_day = 40, time_quit_smoking = NA
  )

  expect_true(result_10 < result_20)
  expect_true(result_20 < result_40)
})

# =============================================================================
# Transition consistency
# =============================================================================

test_that("daily smoker at quit matches former daily at same point", {

  # A daily smoker who quits at 45 should have same pack-years
  # as a former daily smoker assessed later
  pack_years_at_quit <- calculate_pack_years(
    smoking_status = 1, age = 45,
    age_start_smoking = 20, cigs_per_day = 20, time_quit_smoking = NA
  )
  pack_years_former <- calculate_pack_years(
    smoking_status = 4, age = 50,
    age_start_smoking = 20, cigs_per_day = 20, time_quit_smoking = 5
  )

  expect_equal(pack_years_at_quit, pack_years_former)
})

# =============================================================================
# Vector inputs
# =============================================================================

test_that("calculate_pack_years handles vector inputs with mixed statuses", {

  # Note: smoking_status = 6 collides with clean_variables() auto-detection
  # (single-digit missing pattern) so only test statuses 1-5 via wrapper.
  result <- calculate_pack_years(
    smoking_status         = c(1,  4,  5),
    age      = c(40, 55, 50),
    age_start_smoking = c(20, 20, NA),
    cigs_per_day      = c(20, 20, NA),
    time_quit_smoking = c(NA, 10, NA),
    smoked_100_lifetime = c(NA, NA, 1)
  )

  expect_length(result, 3)

  # Status 1: (40-20) * 20/20 = 20
  expect_equal(result[1], 20)

  # Status 4: (55-20-10) * 20/20 = 25
  expect_equal(result[2], 25)

  # Status 5 with 100+ cigs: min_pack_years = 0.0137
  expect_equal(result[3], PACK_YEARS_CONSTANTS$min_pack_years)
})

# =============================================================================
# Status 2 - Occasional smokers (former daily)
# =============================================================================

test_that("calculate_pack_years computes correctly for occasional former-daily (status 2)", {

  # Status 2 formula: daily_period + occasional_period
  # daily_period = pmax((age - age_start - time_quit) * (cigs/20), min_pack_years)
  # occasional_period = (pmax(cigs_occ * days/30, 1) / 20) * time_quit
  #
  # 45yo, started daily at 20, quit daily 10 years ago (at 35), 20 cigs/day when daily
  # Now occasional: 5 cigs/occasion, 15 days/month
  # daily_period = (45 - 20 - 10) * (20/20) = 15
  # occasional_period = (pmax(5 * 15/30, 1) / 20) * 10 = (2.5 / 20) * 10 = 1.25
  # total = 15 + 1.25 = 16.25
  result <- calculate_pack_years(
    smoking_status = 2,
    age = 45,
    age_start_smoking = 20,
    cigs_per_day = 20,
    time_quit_smoking = 10,
    cigs_occasional = 5,
    days_per_month = 15
  )
  expect_equal(result, 16.25)
})

test_that("calculate_pack_years status 2 handles light occasional smoking", {

  # 50yo, started at 25, quit daily 5 years ago, 10 cigs/day when daily
  # Occasional: 1 cig/occasion, 2 days/month
  # daily_period = (50 - 25 - 5) * (10/20) = 20 * 0.5 = 10
  # occasional_period = (pmax(1*2/30, 1) / 20) * 5
  #   cigs_occ * days/30 = 1*2/30 = 0.0667, pmax(0.0667, 1) = 1
  #   so (1/20) * 5 = 0.25
  # total = 10 + 0.25 = 10.25
  result <- calculate_pack_years(
    smoking_status = 2,
    age = 50,
    age_start_smoking = 25,
    cigs_per_day = 10,
    time_quit_smoking = 5,
    cigs_occasional = 1,
    days_per_month = 2
  )
  expect_equal(result, 10.25)
})

# =============================================================================
# Status 3 - Occasional smokers (never daily)
# =============================================================================

test_that("calculate_pack_years computes correctly for occasional never-daily (status 3)", {

  # Status 3 formula: (pmax(cigs_occ * days/30, 1) / 20) * (age - age_first_cig)
  # 40yo, first cig at 20, 3 cigs/occasion, 10 days/month
  # effective_daily = pmax(3*10/30, 1) = pmax(1, 1) = 1
  # pack_years = (1/20) * (40 - 20) = 0.05 * 20 = 1.0
  result <- calculate_pack_years(
    smoking_status = 3,
    age = 40,
    age_start_smoking = NA,
    cigs_per_day = NA,
    time_quit_smoking = NA,
    cigs_occasional = 3,
    days_per_month = 10,
    age_first_cigarette = 20
  )
  expect_equal(result, 1.0)
})

test_that("calculate_pack_years status 3 applies pmax floor for very light smoking", {

  # 35yo, first cig at 25, 1 cig/occasion, 1 day/month
  # effective_daily = pmax(1*1/30, 1) = pmax(0.033, 1) = 1  (floor applied)
  # pack_years = (1/20) * (35 - 25) = 0.05 * 10 = 0.5
  result <- calculate_pack_years(
    smoking_status = 3,
    age = 35,
    age_start_smoking = NA,
    cigs_per_day = NA,
    time_quit_smoking = NA,
    cigs_occasional = 1,
    days_per_month = 1,
    age_first_cigarette = 25
  )
  expect_equal(result, 0.5)
})

test_that("calculate_pack_years status 3 handles heavy occasional smoking", {

  # 50yo, first cig at 15, 10 cigs/occasion, 20 days/month
  # effective_daily = pmax(10*20/30, 1) = pmax(6.667, 1) = 6.667
  # pack_years = (6.667/20) * (50 - 15) = 0.3333 * 35 = 11.667
  result <- calculate_pack_years(
    smoking_status = 3,
    age = 50,
    age_start_smoking = NA,
    cigs_per_day = NA,
    time_quit_smoking = NA,
    cigs_occasional = 10,
    days_per_month = 20,
    age_first_cigarette = 15
  )
  expect_equal(result, 10 * 20 / 30 / 20 * 35, tolerance = 1e-6)
})

# =============================================================================
# Legacy compatibility
# =============================================================================

test_that("pack_years_fun still exists and works (legacy)", {

  # Legacy function in R/smoking.R should still be callable
  # Status 6 (never smoker) -> 0
  result <- pack_years_fun(
    SMKDSTY_original = 6, DHHGAGE_cont = 50,
    time_quit_smoking = NA, SMKG203_cont = NA,
    SMKG207_cont = NA, SMK_204 = NA, SMK_05B = NA,
    SMK_208 = NA, SMK_05C = NA, SMKG01C_cont = NA,
    SMK_01A = NA
  )
  expect_equal(result, 0)
})

# =============================================================================
# calculate_pack_years_categorical - 5-category scheme
# =============================================================================

test_that("calculate_pack_years_categorical assigns correct categories", {

  # Category 0: Never smoker (pack-years == 0)
  expect_equal(calculate_pack_years_categorical(0), 0)

  # Category 1: Light (0 < py < 10)
  expect_equal(calculate_pack_years_categorical(0.0137), 1)
  expect_equal(calculate_pack_years_categorical(5.0), 1)
  expect_equal(calculate_pack_years_categorical(9.999), 1)

  # Category 2: Moderate (10 <= py < 20)
  expect_equal(calculate_pack_years_categorical(10.0), 2)
  expect_equal(calculate_pack_years_categorical(15.0), 2)
  expect_equal(calculate_pack_years_categorical(19.999), 2)

  # Category 3: Heavy (20 <= py < 30)
  expect_equal(calculate_pack_years_categorical(20.0), 3)
  expect_equal(calculate_pack_years_categorical(25.0), 3)
  expect_equal(calculate_pack_years_categorical(29.999), 3)

  # Category 4: Very heavy (py >= 30)
  expect_equal(calculate_pack_years_categorical(30.0), 4)
  expect_equal(calculate_pack_years_categorical(100.0), 4)
  expect_equal(calculate_pack_years_categorical(165.0), 4)
})

test_that("calculate_pack_years_categorical handles vector inputs", {

  py <- c(0, 5, 10, 25, 50)
  result <- calculate_pack_years_categorical(py)

  expect_length(result, 5)
  expect_equal(result[1], 0)   # Never
  expect_equal(result[2], 1)   # Light
  expect_equal(result[3], 2)   # Moderate
  expect_equal(result[4], 3)   # Heavy
  expect_equal(result[5], 4)   # Very heavy
})

test_that("calculate_pack_years_categorical boundaries are precise", {

  # Just below and at each boundary
  expect_equal(calculate_pack_years_categorical(9.999), 1)
  expect_equal(calculate_pack_years_categorical(10.0), 2)
  expect_equal(calculate_pack_years_categorical(19.999), 2)
  expect_equal(calculate_pack_years_categorical(20.0), 3)
  expect_equal(calculate_pack_years_categorical(29.999), 3)
  expect_equal(calculate_pack_years_categorical(30.0), 4)
})

test_that("calculate_pack_years_categorical is monotonic", {

  values <- c(0, 0.01, 5, 10, 15, 20, 25, 30, 50, 165)
  result <- calculate_pack_years_categorical(values)

  for (i in 2:length(result)) {
    expect_true(result[i] >= result[i - 1])
  }
})

test_that("calculate_pack_years_categorical handles empty input", {
  expect_length(calculate_pack_years_categorical(numeric(0)), 0)
})

test_that("calculate_pack_years_categorical uses PACK_YEARS_CONSTANTS", {

  breaks <- PACK_YEARS_CONSTANTS$pack_years_cat_breaks
  expect_equal(breaks, c(0, 10, 20, 30))
})
