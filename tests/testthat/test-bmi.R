# BMI function tests — silver tier
# Tests all 6 BMI functions: PUMF (calculate_bmi, adjust_bmi, categorize_bmi)
# and Master (calculate_bmi_master, adjust_bmi_master, categorize_bmi_master)

library(testthat)
library(haven)
library(dplyr)

# ===========================================================================
# calculate_bmi() — PUMF BMI from grouped height/weight
# ===========================================================================

test_that("calculate_bmi() computes BMI for valid scalar inputs", {
  result <- calculate_bmi(HWTGHTM = 1.75, HWTGWTK = 70)
  expect_equal(result, 70 / (1.75^2), tolerance = 1e-6)
})

test_that("calculate_bmi() computes BMI for valid vector inputs", {
  heights <- c(1.65, 1.75, 1.80)
  weights <- c(60, 70, 80)
  result <- calculate_bmi(HWTGHTM = heights, HWTGWTK = weights)
  expected <- weights / (heights^2)
  expect_equal(result, expected, tolerance = 1e-6)
  expect_length(result, 3)
})

test_that("calculate_bmi() converts CCHS triple-digit missing codes", {
  result_996 <- calculate_bmi(HWTGHTM = 996, HWTGWTK = 70)
  expect_true(is.na(result_996))

  result_999 <- calculate_bmi(HWTGHTM = 1.75, HWTGWTK = 999)
  expect_true(is.na(result_999))
})

test_that("calculate_bmi() preserves tagged_na inputs", {
  result_a <- calculate_bmi(HWTGHTM = tagged_na("a"), HWTGWTK = 70)
  expect_true(is.na(result_a))

  result_b <- calculate_bmi(HWTGHTM = 1.75, HWTGWTK = tagged_na("b"))
  expect_true(is.na(result_b))
})

test_that("calculate_bmi() handles regular NA inputs", {
  result <- calculate_bmi(HWTGHTM = NA, HWTGWTK = 70)
  expect_true(is.na(result))
})

test_that("calculate_bmi() handles mixed valid and missing in vectors", {
  heights <- c(1.75, 996, NA, 1.80)
  weights <- c(70,   70,  70, 999)
  result <- calculate_bmi(HWTGHTM = heights, HWTGWTK = weights)
  expect_equal(result[1], 70 / (1.75^2), tolerance = 1e-6)
  expect_true(is.na(result[2]))
  expect_true(is.na(result[3]))
  expect_true(is.na(result[4]))
})

test_that("calculate_bmi() supports output_format parameter", {
  result_tagged <- calculate_bmi(HWTGHTM = 996, HWTGWTK = 70,
                                  output_format = "tagged_na")
  expect_true(is.na(result_tagged))

  result_orig <- calculate_bmi(HWTGHTM = 1.75, HWTGWTK = 70,
                                output_format = "original")
  expect_equal(result_orig, 70 / (1.75^2), tolerance = 1e-6)
})

test_that("calculate_bmi() works in dataframe context via mutate", {
  df <- data.frame(
    HWTGHTM = c(1.75, 1.60, 996),
    HWTGWTK = c(70, 55, 70)
  )
  result <- df %>% mutate(bmi = calculate_bmi(HWTGHTM, HWTGWTK))
  expect_equal(result$bmi[1], 70 / (1.75^2), tolerance = 1e-6)
  expect_equal(result$bmi[2], 55 / (1.60^2), tolerance = 1e-6)
  expect_true(is.na(result$bmi[3]))
})
