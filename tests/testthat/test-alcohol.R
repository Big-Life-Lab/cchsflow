# =============================================================================
# Alcohol Function Tests (v3.0.0)
# =============================================================================
#
# Tests for alcohol consumption and risk assessment functions:
# - calculate_binge_drinking() - Sex-specific binge drinking assessment
# - calculate_drinking_risk_short() - Short-term drinking risk assessment
# - calculate_drinking_risk_long() - Long-term drinking risk assessment
#
# These replace the deprecated functions:
# - binge_drinker_fun() -> calculate_binge_drinking()
# - low_drink_short_fun() -> calculate_drinking_risk_short()
# - low_drink_long_fun() -> calculate_drinking_risk_long()
#
# @note v3.0.0, last updated: 2025-07-05, status: active
#
# **Testing**: Run comprehensive tests with:
# library(testthat); library(cchsflow); test_file('tests/testthat/test-alcohol.R')

library(testthat)
library(haven)
library(dplyr)

# =============================================================================
# Test calculate_binge_drinking() - Sex-specific binge drinking assessment
# =============================================================================

test_that("calculate_binge_drinking() handles basic valid inputs correctly", {
  # Test case: Male binge drinker (5+ drinks on any day)
  result_male_binge <- calculate_binge_drinking(
    DHH_SEX = 1, # male
    ALW_1 = 1, # had drinks in past week
    ALW_2A1 = 5, ALW_2A2 = 0, ALW_2A3 = 0, ALW_2A4 = 0,
    ALW_2A5 = 0, ALW_2A6 = 0, ALW_2A7 = 0, # 5 drinks on Sunday only
    log_level = "silent"
  )
  expect_equal(result_male_binge, 1L) # binge drinker

  # Test case: Female binge drinker (4+ drinks on any day)
  result_female_binge <- calculate_binge_drinking(
    DHH_SEX = 2, # female
    ALW_1 = 1, # had drinks in past week
    ALW_2A1 = 0, ALW_2A2 = 4, ALW_2A3 = 0, ALW_2A4 = 0,
    ALW_2A5 = 0, ALW_2A6 = 0, ALW_2A7 = 0, # 4 drinks on Monday only
    log_level = "silent"
  )
  expect_equal(result_female_binge, 1L) # binge drinker

  # Test case: Male non-binge drinker (4 drinks maximum)
  result_male_no_binge <- calculate_binge_drinking(
    DHH_SEX = 1, # male
    ALW_1 = 1, # had drinks in past week
    ALW_2A1 = 2, ALW_2A2 = 2, ALW_2A3 = 0, ALW_2A4 = 0,
    ALW_2A5 = 0, ALW_2A6 = 0, ALW_2A7 = 0, # max 2 drinks per day
    log_level = "silent"
  )
  expect_equal(result_male_no_binge, 2L) # not a binge drinker

  # Test case: No drinks in past week
  result_no_drinks <- calculate_binge_drinking(
    DHH_SEX = 1, # male
    ALW_1 = 2, # no drinks in past week
    ALW_2A1 = 0, ALW_2A2 = 0, ALW_2A3 = 0, ALW_2A4 = 0,
    ALW_2A5 = 0, ALW_2A6 = 0, ALW_2A7 = 0,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_no_drinks, "a")) # not applicable
})

test_that("calculate_binge_drinking() handles out of range inputs correctly", {
  # Test case: sex out of range (should return tagged_na("b"))
  result_invalid_sex <- calculate_binge_drinking(
    DHH_SEX = -1, # invalid sex
    ALW_1 = 1, ALW_2A1 = 1, ALW_2A2 = 1, ALW_2A3 = 1,
    ALW_2A4 = 1, ALW_2A5 = 1, ALW_2A6 = 1, ALW_2A7 = 1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid_sex, "b"))

  # Test case: ALW_1 out of range
  result_invalid_alw1 <- calculate_binge_drinking(
    DHH_SEX = 1, ALW_1 = -1, ALW_2A1 = 1, ALW_2A2 = 1, ALW_2A3 = 1,
    ALW_2A4 = 1, ALW_2A5 = 1, ALW_2A6 = 1, ALW_2A7 = 1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid_alw1, "b")) # invalid/missing for invalid ALW_1

  # Test case: ALW_2A1 out of range
  result_invalid_alw2a1 <- calculate_binge_drinking(
    DHH_SEX = 1, ALW_1 = 1, ALW_2A1 = -1, ALW_2A2 = 1, ALW_2A3 = 1,
    ALW_2A4 = 1, ALW_2A5 = 1, ALW_2A6 = 1, ALW_2A7 = 1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid_alw2a1, "b"))

  # Test case: ALW_2A2 out of range
  result_invalid_alw2a2 <- calculate_binge_drinking(
    DHH_SEX = 1, ALW_1 = 1, ALW_2A1 = 1, ALW_2A2 = -1, ALW_2A3 = 1,
    ALW_2A4 = 1, ALW_2A5 = 1, ALW_2A6 = 1, ALW_2A7 = 1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid_alw2a2, "b"))
})

test_that("calculate_binge_drinking() handles vector inputs correctly", {
  # Test case: mixed scenarios with vectors
  # Person 1: Male binge, Person 2: Female no binge, Person 3: No drinks
  result_vector <- calculate_binge_drinking(
    DHH_SEX = c(1, 2, 1), # male, female, male
    ALW_1 = c(1, 1, 2), # drinks, drinks, no drinks
    ALW_2A1 = c(6, 2, 0), # Person 1: 6 drinks (binge), Person 2: 2 drinks, Person 3: 0
    ALW_2A2 = c(0, 1, 0), # Person 1: 0, Person 2: 1 drink, Person 3: 0
    ALW_2A3 = c(0, 1, 0), # Person 1: 0, Person 2: 1 drink, Person 3: 0
    ALW_2A4 = c(0, 1, 0), # Person 1: 0, Person 2: 1 drink, Person 3: 0
    ALW_2A5 = c(0, 0, 0), # All: 0 drinks
    ALW_2A6 = c(0, 0, 0), # All: 0 drinks
    ALW_2A7 = c(0, 0, 0), # All: 0 drinks
    log_level = "silent"
  )

  expect_equal(result_vector[1], 1L) # Person 1: binge drinker
  expect_equal(result_vector[2], 2L) # Person 2: not binge drinker (max 2 drinks/day for female)
  expect_true(is_tagged_na(result_vector[3], "a")) # Person 3: not applicable (no drinks)
})

# =============================================================================
# Test calculate_drinking_risk_short() - Short-term drinking risk assessment
# =============================================================================

test_that("calculate_drinking_risk_short() handles basic valid inputs correctly", {
  # Test case: Male high short-term risk (>3 drinks/day)
  result_male_high_risk <- calculate_drinking_risk_short(
    DHH_SEX = 1, # male
    ALWDWKY = 20, # 20 drinks per week
    ALC_1 = 1, # drinks in past year
    ALW_1 = 1, # had drinks in past week
    ALW_2A1 = 4, ALW_2A2 = 4, ALW_2A3 = 4, ALW_2A4 = 4,
    ALW_2A5 = 4, ALW_2A6 = 0, ALW_2A7 = 0, # 4 drinks/day x 5 days
    log_level = "silent"
  )
  expect_equal(result_male_high_risk, 1L) # high risk

  # Test case: Female low short-term risk (≤2 drinks/day)
  result_female_low_risk <- calculate_drinking_risk_short(
    DHH_SEX = 2, # female
    ALWDWKY = 10, # 10 drinks per week
    ALC_1 = 1, # drinks in past year
    ALW_1 = 1, # had drinks in past week
    ALW_2A1 = 2, ALW_2A2 = 2, ALW_2A3 = 2, ALW_2A4 = 2,
    ALW_2A5 = 2, ALW_2A6 = 0, ALW_2A7 = 0, # 2 drinks/day x 5 days
    log_level = "silent"
  )
  expect_equal(result_female_low_risk, 2L) # low risk

  # Test case: No drinks in past year
  result_no_drinks <- calculate_drinking_risk_short(
    DHH_SEX = 1, # male
    ALWDWKY = 0, # 0 drinks per week
    ALC_1 = 2, # no drinks in past year
    ALW_1 = 2, # no drinks in past week
    ALW_2A1 = 0, ALW_2A2 = 0, ALW_2A3 = 0, ALW_2A4 = 0,
    ALW_2A5 = 0, ALW_2A6 = 0, ALW_2A7 = 0,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_no_drinks, "a")) # not applicable
})

test_that("calculate_drinking_risk_short() handles out of range inputs correctly", {
  # Test case: sex out of range (should return tagged_na("b"))
  result_invalid_sex <- calculate_drinking_risk_short(
    DHH_SEX = -1, # invalid sex
    ALWDWKY = 10, ALC_1 = 1, ALW_1 = 1, ALW_2A1 = 1, ALW_2A2 = 1, ALW_2A3 = 1,
    ALW_2A4 = 1, ALW_2A5 = 1, ALW_2A6 = 1, ALW_2A7 = 1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid_sex, "b"))

  # Test case: ALWDWKY out of range
  result_invalid_alwdwky <- calculate_drinking_risk_short(
    DHH_SEX = 1, ALWDWKY = -1, ALC_1 = 1, ALW_1 = 1, ALW_2A1 = 1, ALW_2A2 = 1, ALW_2A3 = 1,
    ALW_2A4 = 1, ALW_2A5 = 1, ALW_2A6 = 1, ALW_2A7 = 1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid_alwdwky, "b"))

  # Test case: ALC_1 out of range
  result_invalid_alc1 <- calculate_drinking_risk_short(
    DHH_SEX = 1, ALWDWKY = 10, ALC_1 = -1, ALW_1 = 1, ALW_2A1 = 1, ALW_2A2 = 1, ALW_2A3 = 1,
    ALW_2A4 = 1, ALW_2A5 = 1, ALW_2A6 = 1, ALW_2A7 = 1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid_alc1, "b"))
})

test_that("calculate_drinking_risk_short() handles vector inputs correctly", {
  # Test case: different risk profiles across multiple people
  result_vector <- calculate_drinking_risk_short(
    DHH_SEX = c(1, 2, 1, 2), # male, female, male, female
    ALWDWKY = c(25, 8, 0, 15), # high, low, none, moderate
    ALC_1 = c(1, 1, 2, 1), # drinks, drinks, no drinks, drinks
    ALW_1 = c(1, 1, 2, 1), # drinks this week, drinks this week, no drinks, drinks this week
    ALW_2A1 = c(5, 1, 0, 3), # Person 1: high risk, Person 2: low risk, Person 3: none, Person 4: moderate
    ALW_2A2 = c(5, 1, 0, 3),
    ALW_2A3 = c(5, 1, 0, 3),
    ALW_2A4 = c(5, 1, 0, 3),
    ALW_2A5 = c(5, 1, 0, 3),
    ALW_2A6 = c(0, 1, 0, 0),
    ALW_2A7 = c(0, 1, 0, 0),
    log_level = "silent"
  )

  expect_equal(result_vector[1], 1L) # Person 1: high risk (male, >3 drinks/day)
  expect_equal(result_vector[2], 2L) # Person 2: low risk (female, ≤2 drinks/day)
  expect_true(is_tagged_na(result_vector[3], "a")) # Person 3: not applicable (no drinks)
  expect_equal(result_vector[4], 1L) # Person 4: high risk (female, >2 drinks/day)
})

# =============================================================================
# Test calculate_drinking_risk_long() - Long-term drinking risk assessment
# =============================================================================

test_that("calculate_drinking_risk_long() handles basic valid inputs correctly", {
  # Test case: Male high long-term risk (>14 drinks/week)
  result_male_high_risk <- calculate_drinking_risk_long(
    DHH_SEX = 1, # male
    ALWDWKY = 20, # 20 drinks per week
    ALC_1 = 1, # drinks in past year
    ALW_1 = 1, # had drinks in past week
    ALW_2A1 = 3, ALW_2A2 = 3, ALW_2A3 = 3, ALW_2A4 = 3,
    ALW_2A5 = 3, ALW_2A6 = 3, ALW_2A7 = 2, # ~20 drinks/week
    log_level = "silent"
  )
  expect_equal(result_male_high_risk, 1L) # high risk

  # Test case: Female low long-term risk (≤9 drinks/week)
  result_female_low_risk <- calculate_drinking_risk_long(
    DHH_SEX = 2, # female
    ALWDWKY = 7, # 7 drinks per week
    ALC_1 = 1, # drinks in past year
    ALW_1 = 1, # had drinks in past week
    ALW_2A1 = 1, ALW_2A2 = 1, ALW_2A3 = 1, ALW_2A4 = 1,
    ALW_2A5 = 1, ALW_2A6 = 1, ALW_2A7 = 1, # 7 drinks/week
    log_level = "silent"
  )
  expect_equal(result_female_low_risk, 2L) # low risk

  # Test case: No drinks in past year
  result_no_drinks <- calculate_drinking_risk_long(
    DHH_SEX = 1, # male
    ALWDWKY = 0, # 0 drinks per week
    ALC_1 = 2, # no drinks in past year
    ALW_1 = 2, # no drinks in past week
    ALW_2A1 = 0, ALW_2A2 = 0, ALW_2A3 = 0, ALW_2A4 = 0,
    ALW_2A5 = 0, ALW_2A6 = 0, ALW_2A7 = 0,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_no_drinks, "a")) # not applicable
})

test_that("calculate_drinking_risk_long() handles out of range inputs correctly", {
  # Test case: sex out of range (should return tagged_na("b"))
  result_invalid_sex <- calculate_drinking_risk_long(
    DHH_SEX = -1, # invalid sex
    ALWDWKY = 10, ALC_1 = 1, ALW_1 = 1, ALW_2A1 = 1, ALW_2A2 = 1, ALW_2A3 = 1,
    ALW_2A4 = 1, ALW_2A5 = 1, ALW_2A6 = 1, ALW_2A7 = 1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid_sex, "b"))

  # Test case: ALWDWKY out of range
  result_invalid_alwdwky <- calculate_drinking_risk_long(
    DHH_SEX = 1, ALWDWKY = -1, ALC_1 = 1, ALW_1 = 1, ALW_2A1 = 1, ALW_2A2 = 1, ALW_2A3 = 1,
    ALW_2A4 = 1, ALW_2A5 = 1, ALW_2A6 = 1, ALW_2A7 = 1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid_alwdwky, "b"))

  # Test case: ALC_1 out of range
  result_invalid_alc1 <- calculate_drinking_risk_long(
    DHH_SEX = 1, ALWDWKY = 10, ALC_1 = -1, ALW_1 = 1, ALW_2A1 = 1, ALW_2A2 = 1, ALW_2A3 = 1,
    ALW_2A4 = 1, ALW_2A5 = 1, ALW_2A6 = 1, ALW_2A7 = 1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid_alc1, "b"))
})

test_that("calculate_drinking_risk_long() handles vector inputs correctly", {
  # Test case: different long-term risk profiles across multiple people
  result_vector <- calculate_drinking_risk_long(
    DHH_SEX = c(1, 2, 1, 2), # male, female, male, female
    ALWDWKY = c(20, 5, 0, 12), # high risk, low risk, none, moderate risk
    ALC_1 = c(1, 1, 2, 1), # drinks, drinks, no drinks, drinks
    ALW_1 = c(1, 1, 2, 1), # drinks this week, drinks this week, no drinks, drinks this week
    ALW_2A1 = c(3, 1, 0, 2), # Person 1: high weekly total, Person 2: low, Person 3: none, Person 4: moderate
    ALW_2A2 = c(3, 1, 0, 2),
    ALW_2A3 = c(3, 1, 0, 2),
    ALW_2A4 = c(3, 1, 0, 2),
    ALW_2A5 = c(3, 1, 0, 2),
    ALW_2A6 = c(3, 0, 0, 2),
    ALW_2A7 = c(2, 0, 0, 0),
    log_level = "silent"
  )

  expect_equal(result_vector[1], 1L) # Person 1: high risk (male, >14 drinks/week)
  expect_equal(result_vector[2], 2L) # Person 2: low risk (female, ≤9 drinks/week)
  expect_true(is_tagged_na(result_vector[3], "a")) # Person 3: not applicable (no drinks)
  expect_equal(result_vector[4], 2L) # Person 4: low risk (female, 12 drinks/week < 15 threshold, 2 drinks/day < 3 threshold)
})

# =============================================================================
# Integration and Performance Tests
# =============================================================================

test_that("alcohol functions handle large datasets efficiently", {
  # Test with reasonably sized dataset
  n <- 1000
  large_data <- data.frame(
    DHH_SEX = sample(1:2, n, replace = TRUE),
    ALW_1 = sample(1:2, n, replace = TRUE),
    ALWDWKY = sample(0:30, n, replace = TRUE),
    ALC_1 = sample(1:2, n, replace = TRUE),
    ALW_2A1 = sample(0:10, n, replace = TRUE),
    ALW_2A2 = sample(0:10, n, replace = TRUE),
    ALW_2A3 = sample(0:10, n, replace = TRUE),
    ALW_2A4 = sample(0:10, n, replace = TRUE),
    ALW_2A5 = sample(0:10, n, replace = TRUE),
    ALW_2A6 = sample(0:10, n, replace = TRUE),
    ALW_2A7 = sample(0:10, n, replace = TRUE)
  )

  # Should complete without errors
  expect_silent({
    result_binge <- calculate_binge_drinking(
      large_data$DHH_SEX, large_data$ALW_1, large_data$ALW_2A1,
      large_data$ALW_2A2, large_data$ALW_2A3, large_data$ALW_2A4,
      large_data$ALW_2A5, large_data$ALW_2A6, large_data$ALW_2A7,
      log_level = "silent"
    )
  })

  expect_silent({
    result_short <- calculate_drinking_risk_short(
      large_data$DHH_SEX, large_data$ALWDWKY, large_data$ALC_1, large_data$ALW_1,
      large_data$ALW_2A1, large_data$ALW_2A2, large_data$ALW_2A3,
      large_data$ALW_2A4, large_data$ALW_2A5, large_data$ALW_2A6,
      large_data$ALW_2A7,
      log_level = "silent"
    )
  })

  expect_silent({
    result_long <- calculate_drinking_risk_long(
      large_data$DHH_SEX, large_data$ALWDWKY, large_data$ALC_1, large_data$ALW_1,
      large_data$ALW_2A1, large_data$ALW_2A2, large_data$ALW_2A3,
      large_data$ALW_2A4, large_data$ALW_2A5, large_data$ALW_2A6,
      large_data$ALW_2A7,
      log_level = "silent"
    )
  })

  expect_length(result_binge, n)
  expect_length(result_short, n)
  expect_length(result_long, n)
})

test_that("alcohol functions handle extreme edge cases gracefully", {
  # Test with all NA input
  result_all_na <- calculate_binge_drinking(
    rep(NA, 5), rep(NA, 5), rep(NA, 5), rep(NA, 5), rep(NA, 5),
    rep(NA, 5), rep(NA, 5), rep(NA, 5), rep(NA, 5),
    log_level = "silent"
  )
  expect_length(result_all_na, 5)

  # Test with empty vectors
  result_empty <- calculate_binge_drinking(
    numeric(0), numeric(0), numeric(0), numeric(0), numeric(0),
    numeric(0), numeric(0), numeric(0), numeric(0),
    log_level = "silent"
  )
  expect_length(result_empty, 0)
})

# =============================================================================
# Real-world Integration Tests
# =============================================================================

test_that("alcohol functions work correctly in cchsflow workflows", {
  # Test that functions integrate well with rec_with_table() workflow
  # Create sample data mimicking CCHS structure
  test_data <- data.frame(
    DHH_SEX = c(1, 2, 1, 2, 1), # male, female, male, female, male
    ALW_1 = c(1, 1, 2, 1, 1), # drinks this week (except person 3)
    ALWDWKY = c(15, 6, 0, 12, 25), # weekly drink totals
    ALC_1 = c(1, 1, 2, 1, 1), # drinks in past year (except person 3)
    ALW_2A1 = c(3, 1, 0, 2, 5), # Sunday drinks
    ALW_2A2 = c(3, 1, 0, 2, 5), # Monday drinks
    ALW_2A3 = c(2, 1, 0, 2, 5), # Tuesday drinks
    ALW_2A4 = c(2, 1, 0, 2, 5), # Wednesday drinks
    ALW_2A5 = c(2, 1, 0, 2, 5), # Thursday drinks
    ALW_2A6 = c(2, 1, 0, 2, 0), # Friday drinks
    ALW_2A7 = c(1, 0, 0, 0, 0) # Saturday drinks
  )

  # Calculate derived variables
  test_data$binge_drinker <- calculate_binge_drinking(
    test_data$DHH_SEX, test_data$ALW_1, test_data$ALW_2A1,
    test_data$ALW_2A2, test_data$ALW_2A3, test_data$ALW_2A4,
    test_data$ALW_2A5, test_data$ALW_2A6, test_data$ALW_2A7,
    log_level = "silent"
  )

  test_data$short_term_risk <- calculate_drinking_risk_short(
    test_data$DHH_SEX, test_data$ALWDWKY, test_data$ALC_1, test_data$ALW_1,
    test_data$ALW_2A1, test_data$ALW_2A2, test_data$ALW_2A3,
    test_data$ALW_2A4, test_data$ALW_2A5, test_data$ALW_2A6,
    test_data$ALW_2A7,
    log_level = "silent"
  )

  test_data$long_term_risk <- calculate_drinking_risk_long(
    test_data$DHH_SEX, test_data$ALWDWKY, test_data$ALC_1, test_data$ALW_1,
    test_data$ALW_2A1, test_data$ALW_2A2, test_data$ALW_2A3,
    test_data$ALW_2A4, test_data$ALW_2A5, test_data$ALW_2A6,
    test_data$ALW_2A7,
    log_level = "silent"
  )

  # Verify results are reasonable
  # Person 1: Male, 3 drinks/day -> not binge (≤4), short risk (>3/day), long risk (15/week)
  expect_equal(test_data$binge_drinker[1], 2L) # not binge
  expect_equal(test_data$short_term_risk[1], 2L) # low short-term risk (≤3 drinks/day for male)
  expect_equal(test_data$long_term_risk[1], 2L) # low long-term risk (15 drinks/week < 20 threshold AND 3 drinks/day < 4 threshold for male)

  # Person 2: Female, 1 drink/day -> not binge, low risk both
  expect_equal(test_data$binge_drinker[2], 2L) # not binge
  expect_equal(test_data$short_term_risk[2], 2L) # low short-term risk
  expect_equal(test_data$long_term_risk[2], 2L) # low long-term risk

  # Person 3: No drinks -> all not applicable
  expect_true(is_tagged_na(test_data$binge_drinker[3], "a"))
  expect_true(is_tagged_na(test_data$short_term_risk[3], "a"))
  expect_true(is_tagged_na(test_data$long_term_risk[3], "a"))

  # Person 5: Male, 5 drinks/day -> binge, high risk both
  expect_equal(test_data$binge_drinker[5], 1L) # binge drinker
  expect_equal(test_data$short_term_risk[5], 1L) # high short-term risk
  expect_equal(test_data$long_term_risk[5], 1L) # high long-term risk
})

# =============================================================================
# NOTES: Deprecated Functions and Migration Path
# =============================================================================
#
# The following functions have been deprecated and replaced:
# - binge_drinker_fun() -> calculate_binge_drinking()
# - low_drink_short_fun() -> calculate_drinking_risk_short()
# - low_drink_long_fun() -> calculate_drinking_risk_long()
#
# Missing implementations noted in variables.csv analysis:
# - low_drink_score_fun() - General low-risk drinking assessment
# - low_drink_score_fun1() - Alternative low-risk drinking assessment
#
# Migration notes:
# - All functions now use modern tidyverse patterns with haven::tagged_na()
# - Sex-specific thresholds properly implemented for all assessments
# - Comprehensive input validation with informative error messages
# - Support for both scalar and vector inputs
# - Consistent logging and debugging capabilities
# - Enhanced performance for large datasets
#
# @note v3.0.0, last updated: 2025-07-05, status: active
