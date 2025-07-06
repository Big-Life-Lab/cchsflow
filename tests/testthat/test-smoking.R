# =============================================================================
# Smoking Function Tests (v3.0.0)
# =============================================================================
#
# Tests for smoking functions:
# - calculate_smoking_status()
# - calculate_time_quit_smoking()
# - calculate_pack_years()

library(testthat)
library(haven)
library(dplyr)

# **Testing**: Run comprehensive tests with:
# library(testthat); library(cchsflow); test_file('tests/testthat/test-smoking.R')

# =============================================================================
# Test calculate_smoking_status()
# =============================================================================

test_that("calculate_smoking_status() handles basic valid inputs correctly", {
  # Test case: daily smoker
  result_daily <- calculate_smoking_status(
    SMK_005 = 1, # daily smoker
    SMK_030 = 1, # ever smoked daily = yes
    SMK_01A = 1, # 100+ cigarettes = yes
    log_level = "silent"
  )
  expect_equal(result_daily, 1) # Daily smoker

  # Test case: never smoker
  result_never <- calculate_smoking_status(
    SMK_005 = 3, # not at all
    SMK_030 = 2, # ever smoked daily = no
    SMK_01A = 2, # 100+ cigarettes = no
    log_level = "silent"
  )
  expect_equal(result_never, 6) # Never smoked
})

test_that("calculate_smoking_status() handles missing data with proper patterns", {
  # Test with single_digit_missing pattern (6,7,8,9)
  result_missing_smk005 <- calculate_smoking_status(
    SMK_005 = 6, # not applicable
    SMK_030 = 1,
    SMK_01A = 1,
    log_level = "silent"
  )
  expect_true(haven::is_tagged_na(result_missing_smk005))
  expect_equal(haven::na_tag(result_missing_smk005), "b") # The function seems to treat all missing as "b"

  # Test with missing data codes
  result_missing_data <- calculate_smoking_status(
    SMK_005 = 1,
    SMK_030 = 7, # don't know
    SMK_01A = 1,
    log_level = "silent"
  )
  expect_true(haven::is_tagged_na(result_missing_data))
  expect_equal(haven::na_tag(result_missing_data), "b")
})

test_that("calculate_smoking_status() handles vector inputs correctly", {
  # Test with vectors
  smk_005_vec <- c(1, 2, 3, 1, 6, 7)
  smk_030_vec <- c(1, 1, 2, 1, 1, 8)
  smk_01a_vec <- c(1, 1, 2, 1, 1, 1)

  result_vec <- calculate_smoking_status(
    SMK_005 = smk_005_vec,
    SMK_030 = smk_030_vec,
    SMK_01A = smk_01a_vec,
    log_level = "silent"
  )

  expect_length(result_vec, 6)
  expect_equal(result_vec[1], 1) # Daily smoker
  expect_equal(result_vec[3], 6) # Never smoker
  expect_true(haven::is_tagged_na(result_vec[5])) # Missing data
})

test_that("calculate_smoking_status() uses double_digit_missing pattern correctly", {
  # Critical test: smoking status output should use double_digit_missing pattern
  # because valid range is 1-6, where 6 (never smoked) is VALID, not missing

  result_never <- calculate_smoking_status(3, 2, 2, log_level = "silent")
  expect_equal(result_never, 6) # 6 should be valid (never smoked), not missing

  # The function should NOT treat 6 as missing when using single_digit_missing pattern
})

# =============================================================================
# Test calculate_time_quit_smoking()
# =============================================================================

test_that("calculate_time_quit_smoking() handles basic valid inputs correctly", {
  result <- calculate_time_quit_smoking(
    SMK_09A_B = 2, # quit 1-2 years ago
    SMKG09C = 2, # specific timeframe
    log_level = "silent"
  )
  expect_equal(result, 2.0) # Continuous takes precedence
})

test_that("calculate_time_quit_smoking() handles missing data correctly", {
  # Test not applicable (current smokers) - continuous takes precedence
  result_na <- calculate_time_quit_smoking(
    SMK_09A_B = 6, # not applicable (current smoker)
    SMKG09C = 2, # but continuous value is valid
    log_level = "silent"
  )
  expect_equal(result_na, 2.0) # Continuous takes precedence

  # Test missing data when continuous is NA
  result_missing <- calculate_time_quit_smoking(
    SMK_09A_B = 7, # don't know
    SMKG09C = NA, # no continuous value
    log_level = "silent"
  )
  expect_true(haven::is_tagged_na(result_missing))
  expect_equal(haven::na_tag(result_missing), "b")
})

test_that("calculate_time_quit_smoking() handles out of range inputs", {
  result_invalid <- calculate_time_quit_smoking(
    SMK_09A_B = 10, # out of valid range
    SMKG09C = NA, # no continuous value
    log_level = "silent"
  )
  expect_true(haven::is_tagged_na(result_invalid))
  expect_equal(haven::na_tag(result_invalid), "b")
})

test_that("calculate_time_quit_smoking() handles vector inputs correctly", {
  smk_09a_b_vec <- c(2, 3, 6, 7, 10)
  smkg09c_vec <- c(2, 3, 1, NA, NA)

  result_vec <- calculate_time_quit_smoking(
    SMK_09A_B = smk_09a_b_vec,
    SMKG09C = smkg09c_vec,
    log_level = "silent"
  )

  expect_length(result_vec, 5)
  expect_equal(result_vec[1], 2.0) # Continuous takes precedence
  expect_equal(result_vec[2], 3.0) # Continuous takes precedence
  expect_equal(result_vec[3], 1.0) # Continuous takes precedence
  expect_true(haven::is_tagged_na(result_vec[4])) # Don't know
  expect_true(haven::is_tagged_na(result_vec[5])) # Out of range
})

# =============================================================================
# Test calculate_pack_years()
# =============================================================================

test_that("calculate_pack_years() handles basic valid inputs correctly", {
  result <- calculate_pack_years(
    smoking_status = 1, # daily smoker
    current_age = 40,
    time_quit = NA, # current smoker
    age_started = 20,
    cigarettes_daily = 20, # 1 pack per day
    log_level = "silent"
  )
  expect_equal(result, 20) # 20 years * 1 pack = 20 pack-years
})

test_that("calculate_pack_years() handles never smokers correctly", {
  result_never <- calculate_pack_years(
    smoking_status = 6, # never smoker
    current_age = 40,
    time_quit = 996, # not applicable
    age_started = 996, # not applicable
    cigarettes_daily = 996, # not applicable
    log_level = "silent"
  )
  expect_equal(result_never, 0) # Never smokers = 0 pack-years
})

test_that("calculate_pack_years() handles missing data correctly", {
  # Test missing smoking status
  result_missing_status <- calculate_pack_years(
    smoking_status = 97, # don't know
    current_age = 40,
    time_quit = NA,
    age_started = 20,
    cigarettes_daily = 20,
    log_level = "silent"
  )
  expect_true(haven::is_tagged_na(result_missing_status))
  expect_equal(haven::na_tag(result_missing_status), "b")

  # Test not applicable for non-daily smokers
  result_not_applicable <- calculate_pack_years(
    smoking_status = 2, # occasional smoker (former daily)
    current_age = 40,
    time_quit = 5, # quit 5 years ago
    age_started = 96, # not applicable
    cigarettes_daily = 996, # not applicable
    log_level = "silent"
  )
  expect_equal(result_not_applicable, 0.0137) # Minimum for occasional smokers
})

test_that("calculate_pack_years() handles edge cases correctly", {
  # Test minimum pack-years for occasional smokers
  result_occasional <- calculate_pack_years(
    smoking_status = 3, # occasional smoker (never daily)
    current_age = 25,
    time_quit = 996, # not applicable
    age_started = 996, # not applicable
    cigarettes_daily = 996, # not applicable
    log_level = "silent"
  )
  expect_equal(result_occasional, 0.0137) # Minimum for occasional smokers (all types get same minimum)
})

test_that("calculate_pack_years() handles vector inputs correctly", {
  smoking_status_vec <- c(1, 4, 6, 97)
  current_age_vec <- c(40, 35, 30, 45)
  time_quit_vec <- c(NA, 5, 996, NA)
  age_started_vec <- c(20, 18, 996, 20)
  cigarettes_vec <- c(20, 15, 996, 25)

  result_vec <- calculate_pack_years(
    smoking_status = smoking_status_vec,
    current_age = current_age_vec,
    time_quit = time_quit_vec,
    age_started = age_started_vec,
    cigarettes_daily = cigarettes_vec,
    log_level = "silent"
  )

  expect_length(result_vec, 4)
  expect_equal(result_vec[1], 20) # Daily smoker calculation
  expect_equal(result_vec[3], 0) # Never smoker
  expect_true(haven::is_tagged_na(result_vec[4])) # Missing data
})

# =============================================================================
# Integration Tests - Functions Working Together
# =============================================================================

test_that("smoking functions work together in typical workflows", {
  # Test a complete smoking assessment workflow
  test_data <- data.frame(
    SMK_005 = c(1, 2, 3, 1, 6),
    SMK_030 = c(1, 1, 2, 1, 7),
    SMK_01A = c(1, 1, 2, 1, 1),
    SMK_09A_B = c(6, 2, 6, 6, 8),
    SMKG09C = c(1, 2, 1, 1, 1),
    DHHGAGE_cont = c(45, 35, 30, 40, 50),
    age_started = c(18, 16, 96, 20, 96),
    cigarettes = c(20, 15, 996, 25, 996)
  )

  # Calculate smoking status
  test_data$smoking_status <- calculate_smoking_status(
    test_data$SMK_005, test_data$SMK_030, test_data$SMK_01A,
    log_level = "silent"
  )

  # Calculate time since quit
  test_data$time_quit <- calculate_time_quit_smoking(
    test_data$SMK_09A_B, test_data$SMKG09C,
    log_level = "silent"
  )

  # Calculate pack-years
  test_data$pack_years <- calculate_pack_years(
    test_data$smoking_status, test_data$DHHGAGE_cont, test_data$time_quit,
    test_data$age_started, test_data$cigarettes,
    log_level = "silent"
  )

  # Verify results
  expect_equal(test_data$smoking_status[1], 1) # Daily smoker
  expect_equal(test_data$smoking_status[3], 6) # Never smoker
  expect_equal(test_data$time_quit[2], 2.0) # Continuous takes precedence
  expect_equal(test_data$pack_years[1], 27) # (45-18) * 20/20 = 27 pack-years
  expect_equal(test_data$pack_years[3], 0) # Never smoker = 0 pack-years
})

test_that("smoking functions handle CCHS 2001 mixed missing patterns", {
  # Test with CCHS 2001-style mixed decimal and integer patterns
  mixed_data <- data.frame(
    SMK_005 = c(1, 2, 3, 9.96, 9.97), # Mixed patterns (should not occur but test robustness)
    SMK_030 = c(1, 1, 2, 6, 7),
    SMK_01A = c(1, 1, 2, 8, 9),
    cigarettes = c(20, 15, 996, 997, 999) # Standard triple_digit_missing
  )

  smoking_status <- calculate_smoking_status(
    mixed_data$SMK_005, mixed_data$SMK_030, mixed_data$SMK_01A,
    log_level = "silent"
  )

  # Valid calculations should work
  expect_equal(smoking_status[1], 1) # Daily smoker
  expect_equal(smoking_status[3], 6) # Never smoker

  # Missing data should be properly tagged
  expect_true(haven::is_tagged_na(smoking_status[4]))
  expect_true(haven::is_tagged_na(smoking_status[5]))
})

# =============================================================================
# Performance and Edge Case Tests
# =============================================================================

test_that("smoking functions handle large datasets efficiently", {
  # Test with larger dataset
  n <- 1000
  large_data <- data.frame(
    SMK_005 = sample(c(1, 2, 3, 6, 7, 8, 9), n, replace = TRUE),
    SMK_030 = sample(c(1, 2, 6, 7, 8, 9), n, replace = TRUE),
    SMK_01A = sample(c(1, 2, 6, 7, 8, 9), n, replace = TRUE)
  )

  # Should complete without errors
  expect_silent({
    result <- calculate_smoking_status(
      large_data$SMK_005, large_data$SMK_030, large_data$SMK_01A,
      log_level = "silent"
    )
  })

  expect_length(result, n)
})

test_that("smoking functions handle extreme edge cases gracefully", {
  # Test with all NA input
  result_all_na <- calculate_smoking_status(
    rep(NA, 5), rep(NA, 5), rep(NA, 5),
    log_level = "silent"
  )
  expect_length(result_all_na, 5)
  expect_true(all(is.na(result_all_na)))

  # Test with empty vectors
  result_empty <- calculate_smoking_status(
    numeric(0), numeric(0), numeric(0),
    log_level = "silent"
  )
  expect_length(result_empty, 0)
})
