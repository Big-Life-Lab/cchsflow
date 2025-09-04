# =============================================================================
# ADL Function Tests (v3.0.0)
# =============================================================================
#
# Tests for modernized ADL (Activities of Daily Living) functions following BMI v3.0.0 patterns:
# - assess_adl() - Binary ADL help indicator
# - score_adl() - 5-item ADL score
# - score_adl_6() - 6-item ADL score
#
# @note v3.0.0, last updated: 2025-07-05, status: active
# 
# **Testing**: Run comprehensive tests with:
# library(testthat); library(cchsflow); test_file('tests/testthat/test-adl.R')

library(testthat)
library(haven)
library(dplyr)

# =============================================================================
# Test assess_adl() - Binary ADL help indicator
# =============================================================================

test_that("assess_adl() handles basic valid inputs correctly", {
  # Test case: needs help with all activities
  result_needs_help <- assess_adl(
    ADL_01 = 1, # needs help with personal care
    ADL_02 = 1, # needs help moving around house
    ADL_03 = 1, # needs help with meal preparation
    ADL_04 = 1, # needs help with normal housework
    ADL_05 = 1, # needs help with heavy housework
    log_level = "silent"
  )
  expect_equal(result_needs_help, 1L) # needs help
  
  # Test case: no help needed
  result_no_help <- assess_adl(
    ADL_01 = 2, # no help needed
    ADL_02 = 2, # no help needed
    ADL_03 = 2, # no help needed
    ADL_04 = 2, # no help needed
    ADL_05 = 2, # no help needed
    log_level = "silent"
  )
  expect_equal(result_no_help, 2L) # no help needed
})

test_that("assess_adl() handles out of range inputs correctly", {
  # Test case: ADL_01 out of range (should return tagged_na("b"))
  result_invalid_adl01 <- assess_adl(
    ADL_01 = -1, # invalid value
    ADL_02 = 1, ADL_03 = 1, ADL_04 = 1, ADL_05 = 1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid_adl01, "b"))
  
  # Test case: ADL_02 out of range
  result_invalid_adl02 <- assess_adl(
    ADL_01 = 1, ADL_02 = -1, ADL_03 = 1, ADL_04 = 1, ADL_05 = 1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid_adl02, "b"))
  
  # Test case: ADL_03 out of range
  result_invalid_adl03 <- assess_adl(
    ADL_01 = 1, ADL_02 = 1, ADL_03 = -1, ADL_04 = 1, ADL_05 = 1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid_adl03, "b"))
  
  # Test case: ADL_04 out of range
  result_invalid_adl04 <- assess_adl(
    ADL_01 = 1, ADL_02 = 1, ADL_03 = 1, ADL_04 = -1, ADL_05 = 1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid_adl04, "b"))
  
  # Test case: ADL_05 out of range
  result_invalid_adl05 <- assess_adl(
    ADL_01 = 1, ADL_02 = 1, ADL_03 = 1, ADL_04 = 1, ADL_05 = -1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid_adl05, "b"))
})

test_that("assess_adl() handles vector inputs correctly", {
  # Test case: mixed scenarios with vectors
  # Person 1: needs help with 2 activities, Person 2: no help needed, Person 3: needs help with all
  result_vector <- assess_adl(
    ADL_01 = c(1, 2, 1), # Person 1 needs help, Person 2 no help, Person 3 needs help
    ADL_02 = c(2, 2, 1), # Person 1 no help, Person 2 no help, Person 3 needs help
    ADL_03 = c(1, 2, 1), # Person 1 needs help, Person 2 no help, Person 3 needs help
    ADL_04 = c(2, 2, 1), # Person 1 no help, Person 2 no help, Person 3 needs help
    ADL_05 = c(2, 2, 1), # Person 1 no help, Person 2 no help, Person 3 needs help
    log_level = "silent"
  )
  
  expect_equal(result_vector, c(1L, 2L, 1L)) # needs help, no help, needs help
})

# =============================================================================
# Test score_adl() - 5-item ADL score
# =============================================================================

test_that("score_adl() handles basic valid inputs correctly", {
  # Test case: needs help with all 5 activities
  result_max_score <- score_adl(
    ADL_01 = 1, ADL_02 = 1, ADL_03 = 1, ADL_04 = 1, ADL_05 = 1,
    log_level = "silent"
  )
  expect_equal(result_max_score, 5L) # 5 tasks need help
  
  # Test case: no help needed for any activity
  result_min_score <- score_adl(
    ADL_01 = 2, ADL_02 = 2, ADL_03 = 2, ADL_04 = 2, ADL_05 = 2,
    log_level = "silent"
  )
  expect_equal(result_min_score, 0L) # 0 tasks need help
  
  # Test case: needs help with some activities
  result_partial_score <- score_adl(
    ADL_01 = 1, # needs help
    ADL_02 = 2, # no help
    ADL_03 = 1, # needs help
    ADL_04 = 2, # no help
    ADL_05 = 2, # no help
    log_level = "silent"
  )
  expect_equal(result_partial_score, 2L) # 2 tasks need help
})

test_that("score_adl() handles out of range inputs correctly", {
  # Test case: ADL_01 out of range (should return tagged_na("b"))
  result_invalid <- score_adl(
    ADL_01 = -1, # invalid value
    ADL_02 = 1, ADL_03 = 1, ADL_04 = 1, ADL_05 = 1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid, "b"))
  
  # Test case: ADL_02 out of range
  result_invalid2 <- score_adl(
    ADL_01 = 1, ADL_02 = -1, ADL_03 = 1, ADL_04 = 1, ADL_05 = 1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid2, "b"))
  
  # Test case: ADL_03 out of range
  result_invalid3 <- score_adl(
    ADL_01 = 1, ADL_02 = 1, ADL_03 = -1, ADL_04 = 1, ADL_05 = 1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid3, "b"))
  
  # Test case: ADL_04 out of range
  result_invalid4 <- score_adl(
    ADL_01 = 1, ADL_02 = 1, ADL_03 = 1, ADL_04 = -1, ADL_05 = 1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid4, "b"))
  
  # Test case: ADL_05 out of range
  result_invalid5 <- score_adl(
    ADL_01 = 1, ADL_02 = 1, ADL_03 = 1, ADL_04 = 1, ADL_05 = -1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid5, "b"))
})

test_that("score_adl() handles vector inputs correctly", {
  # Test case: different ADL profiles across multiple people
  result_vector <- score_adl(
    ADL_01 = c(1, 2, 1, 2), # Person 1,3 need help; Person 2,4 don't
    ADL_02 = c(1, 2, 2, 2), # Person 1 needs help; others don't
    ADL_03 = c(2, 2, 1, 2), # Person 3 needs help; others don't
    ADL_04 = c(1, 2, 1, 2), # Person 1,3 need help; Person 2,4 don't
    ADL_05 = c(2, 2, 2, 2), # Nobody needs help
    log_level = "silent"
  )
  
  expect_equal(result_vector, c(3L, 0L, 3L, 0L)) # Person 1: 3 tasks, Person 2: 0 tasks, Person 3: 3 tasks, Person 4: 0 tasks
})

# =============================================================================
# Test score_adl_6() - 6-item ADL score
# =============================================================================

test_that("score_adl_6() handles basic valid inputs correctly", {
  # Test case: needs help with all 6 activities
  result_max_score <- score_adl_6(
    ADL_01 = 1, ADL_02 = 1, ADL_03 = 1, ADL_04 = 1, ADL_05 = 1, ADL_06 = 1,
    log_level = "silent"
  )
  expect_equal(result_max_score, 6L) # 6 tasks need help
  
  # Test case: no help needed for any activity
  result_min_score <- score_adl_6(
    ADL_01 = 2, ADL_02 = 2, ADL_03 = 2, ADL_04 = 2, ADL_05 = 2, ADL_06 = 2,
    log_level = "silent"
  )
  expect_equal(result_min_score, 0L) # 0 tasks need help
  
  # Test case: needs help with some activities
  result_partial_score <- score_adl_6(
    ADL_01 = 1, # needs help
    ADL_02 = 2, # no help
    ADL_03 = 1, # needs help
    ADL_04 = 2, # no help
    ADL_05 = 1, # needs help
    ADL_06 = 2, # no help
    log_level = "silent"
  )
  expect_equal(result_partial_score, 3L) # 3 tasks need help
})

test_that("score_adl_6() handles out of range inputs correctly", {
  # Test case: ADL_06 out of range (should return tagged_na("b"))
  result_invalid <- score_adl_6(
    ADL_01 = 1, ADL_02 = 1, ADL_03 = 1, ADL_04 = 1, ADL_05 = 1, ADL_06 = -1,
    log_level = "silent"
  )
  expect_true(is_tagged_na(result_invalid, "b"))
})

# =============================================================================
# Integration and Performance Tests
# =============================================================================

test_that("ADL functions handle large datasets efficiently", {
  # Test with reasonably sized dataset
  n <- 1000
  large_data <- data.frame(
    ADL_01 = sample(1:2, n, replace = TRUE),
    ADL_02 = sample(1:2, n, replace = TRUE),
    ADL_03 = sample(1:2, n, replace = TRUE),
    ADL_04 = sample(1:2, n, replace = TRUE),
    ADL_05 = sample(1:2, n, replace = TRUE),
    ADL_06 = sample(1:2, n, replace = TRUE)
  )
  
  # Should complete without errors
  expect_silent({
    result_assess <- assess_adl(
      large_data$ADL_01, large_data$ADL_02, large_data$ADL_03, 
      large_data$ADL_04, large_data$ADL_05,
      log_level = "silent"
    )
  })
  
  expect_silent({
    result_score <- score_adl(
      large_data$ADL_01, large_data$ADL_02, large_data$ADL_03, 
      large_data$ADL_04, large_data$ADL_05,
      log_level = "silent"
    )
  })
  
  expect_silent({
    result_score_6 <- score_adl_6(
      large_data$ADL_01, large_data$ADL_02, large_data$ADL_03, 
      large_data$ADL_04, large_data$ADL_05, large_data$ADL_06,
      log_level = "silent"
    )
  })
  
  expect_length(result_assess, n)
  expect_length(result_score, n)
  expect_length(result_score_6, n)
})

test_that("ADL functions handle extreme edge cases gracefully", {
  # Test with all NA input
  result_all_na <- assess_adl(
    rep(NA, 5), rep(NA, 5), rep(NA, 5), rep(NA, 5), rep(NA, 5),
    log_level = "silent"
  )
  expect_length(result_all_na, 5)
  
  # Test with empty vectors
  result_empty <- assess_adl(
    numeric(0), numeric(0), numeric(0), numeric(0), numeric(0),
    log_level = "silent"
  )
  expect_length(result_empty, 0)
})

# =============================================================================
# Real-world Integration Tests
# =============================================================================

test_that("ADL functions work correctly in cchsflow workflows", {
  # Test that functions integrate well with rec_with_table() workflow
  # Create sample data mimicking CCHS structure
  test_data <- data.frame(
    ADL_01 = c(1, 2, 1, 2, 1),
    ADL_02 = c(1, 2, 2, 2, 1),
    ADL_03 = c(2, 2, 1, 2, 1),
    ADL_04 = c(1, 2, 1, 2, 1),
    ADL_05 = c(2, 2, 2, 2, 1),
    ADL_06 = c(1, 2, 1, 2, 1)
  )
  
  # Calculate derived variables
  test_data$adl_help <- assess_adl(
    test_data$ADL_01, test_data$ADL_02, test_data$ADL_03, 
    test_data$ADL_04, test_data$ADL_05,
    log_level = "silent"
  )
  
  test_data$adl_score_5 <- score_adl(
    test_data$ADL_01, test_data$ADL_02, test_data$ADL_03, 
    test_data$ADL_04, test_data$ADL_05,
    log_level = "silent"
  )
  
  test_data$adl_score_6 <- score_adl_6(
    test_data$ADL_01, test_data$ADL_02, test_data$ADL_03, 
    test_data$ADL_04, test_data$ADL_05, test_data$ADL_06,
    log_level = "silent"
  )
  
  # Verify results are reasonable
  expect_true(all(test_data$adl_help %in% c(1L, 2L)))
  expect_true(all(test_data$adl_score_5 %in% 0:5))
  expect_true(all(test_data$adl_score_6 %in% 0:6))
  
  # Person 1: needs help with 3/5 activities, so adl_help = 1, adl_score_5 = 3
  expect_equal(test_data$adl_help[1], 1L)
  expect_equal(test_data$adl_score_5[1], 3L)
  expect_equal(test_data$adl_score_6[1], 4L)
  
  # Person 2: needs help with 0/5 activities, so adl_help = 2, adl_score_5 = 0
  expect_equal(test_data$adl_help[2], 2L)
  expect_equal(test_data$adl_score_5[2], 0L)
  expect_equal(test_data$adl_score_6[2], 0L)
})

# =============================================================================
# NOTES: Deprecated Functions and Migration Path
# =============================================================================
#
# The following functions have been deprecated and replaced:
# - adl_fun() -> assess_adl()
# - adl_score_5_fun() -> score_adl()
# - adl_score_6_fun() -> score_adl_6()
#
# Migration notes:
# - All functions now use modern tidyverse patterns with haven::tagged_na()
# - Comprehensive input validation with informative error messages
# - Support for both scalar and vector inputs
# - Consistent logging and debugging capabilities
# - Enhanced performance for large datasets
#
# @note v3.0.0, last updated: 2025-07-05, status: active