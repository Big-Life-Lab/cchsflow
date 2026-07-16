# BMI function tests — silver tier (source-agnostic)
# Tests unified BMI functions with semantic parameters.
# Same functions serve both PUMF and Master via worksheet routing.

library(testthat)
library(haven)
library(dplyr)

# ===========================================================================
# calculate_bmi() — BMI from height and weight
# ===========================================================================

test_that("calculate_bmi() computes BMI for valid scalar inputs", {
  result <- calculate_bmi(height_m = 1.75, weight_kg = 70)
  expect_equal(result, 70 / (1.75^2), tolerance = 1e-6)
})

test_that("calculate_bmi() computes BMI for valid vector inputs", {
  heights <- c(1.65, 1.75, 1.80)
  weights <- c(60, 70, 80)
  result <- calculate_bmi(height_m = heights, weight_kg = weights)
  expected <- weights / (heights^2)
  expect_equal(result, expected, tolerance = 1e-6)
  expect_length(result, 3)
})

test_that("calculate_bmi() converts CCHS triple-digit missing codes", {
  result_996 <- calculate_bmi(height_m = 996, weight_kg = 70)
  expect_true(is.na(result_996))

  result_999 <- calculate_bmi(height_m = 1.75, weight_kg = 999)
  expect_true(is.na(result_999))
})

test_that("calculate_bmi() preserves tagged_na inputs", {
  result_a <- calculate_bmi(height_m = tagged_na("a"), weight_kg = 70)
  expect_true(is.na(result_a))

  result_b <- calculate_bmi(height_m = 1.75, weight_kg = tagged_na("b"))
  expect_true(is.na(result_b))
})

test_that("calculate_bmi() handles regular NA inputs", {
  result <- calculate_bmi(height_m = NA, weight_kg = 70)
  expect_true(is.na(result))
})

test_that("calculate_bmi() handles mixed valid and missing in vectors", {
  heights <- c(1.75, 996, NA, 1.80)
  weights <- c(70,   70,  70, 999)
  result <- calculate_bmi(height_m = heights, weight_kg = weights)
  expect_equal(result[1], 70 / (1.75^2), tolerance = 1e-6)
  expect_true(is.na(result[2]))
  expect_true(is.na(result[3]))
  expect_true(is.na(result[4]))
})

test_that("calculate_bmi() supports output_format parameter", {
  result_tagged <- calculate_bmi(height_m = 996, weight_kg = 70,
                                  output_format = "tagged_na")
  expect_true(is.na(result_tagged))

  result_valid <- calculate_bmi(height_m = 1.75, weight_kg = 70,
                                output_format = "original")
  expect_equal(result_valid, 70 / (1.75^2), tolerance = 1e-6)
})

test_that("calculate_bmi() works in dataframe context via mutate", {
  df <- data.frame(
    height_m = c(1.75, 1.60, 996),
    weight_kg = c(70, 55, 70)
  )
  result <- df %>% mutate(bmi = calculate_bmi(height_m, weight_kg))
  expect_equal(result$bmi[1], 70 / (1.75^2), tolerance = 1e-6)
  expect_equal(result$bmi[2], 55 / (1.60^2), tolerance = 1e-6)
  expect_true(is.na(result$bmi[3]))
})

# ===========================================================================
# adjust_bmi() — bias-corrected BMI
# ===========================================================================

test_that("adjust_bmi() applies male correction", {
  raw_bmi <- 70 / (1.75^2)
  expected <- -1.07575 + 1.07592 * raw_bmi
  result <- adjust_bmi(sex = 1, height_m = 1.75, weight_kg = 70)
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("adjust_bmi() applies female correction", {
  raw_bmi <- 60 / (1.65^2)
  expected <- -0.12374 + 1.05129 * raw_bmi
  result <- adjust_bmi(sex = 2, height_m = 1.65, weight_kg = 60)
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("adjust_bmi() handles missing sex codes", {
  result_6 <- adjust_bmi(sex = 6, height_m = 1.75, weight_kg = 70)
  expect_true(is.na(result_6))

  result_9 <- adjust_bmi(sex = 9, height_m = 1.75, weight_kg = 70)
  expect_true(is.na(result_9))
})

test_that("adjust_bmi() handles missing height/weight", {
  result <- adjust_bmi(sex = 1, height_m = 996, weight_kg = 70)
  expect_true(is.na(result))
})

test_that("adjust_bmi() handles invalid sex values", {
  result <- adjust_bmi(sex = 3, height_m = 1.75, weight_kg = 70)
  expect_true(is.na(result))
})

test_that("adjust_bmi() works with vectors", {
  s <- c(1, 2, 6)
  heights <- c(1.75, 1.65, 1.80)
  weights <- c(70, 60, 80)
  result <- adjust_bmi(sex = s, height_m = heights, weight_kg = weights)
  expect_false(is.na(result[1]))
  expect_false(is.na(result[2]))
  expect_true(is.na(result[3]))
})

test_that("adjust_bmi() works in dataframe context via mutate", {
  df <- data.frame(sex = c(1, 2), height_m = c(1.75, 1.65),
                   weight_kg = c(70, 60))
  result <- df %>% mutate(cor_bmi = adjust_bmi(sex, height_m, weight_kg))
  expect_false(is.na(result$cor_bmi[1]))
  expect_false(is.na(result$cor_bmi[2]))
})

# ===========================================================================
# categorize_bmi() — 4-category WHO classification
# ===========================================================================

test_that("categorize_bmi() maps to correct WHO categories", {
  expect_equal(categorize_bmi(bmi = 16.0), 1L)
  expect_equal(categorize_bmi(bmi = 22.0), 2L)
  expect_equal(categorize_bmi(bmi = 27.0), 3L)
  expect_equal(categorize_bmi(bmi = 35.0), 4L)
})

test_that("categorize_bmi() handles WHO boundary values", {
  expect_equal(categorize_bmi(bmi = 18.4), 1L)
  expect_equal(categorize_bmi(bmi = 18.5), 2L)
  expect_equal(categorize_bmi(bmi = 24.9), 2L)
  expect_equal(categorize_bmi(bmi = 25.0), 3L)
  expect_equal(categorize_bmi(bmi = 29.9), 3L)
  expect_equal(categorize_bmi(bmi = 30.0), 4L)
})

test_that("categorize_bmi() handles missing inputs", {
  result_na <- categorize_bmi(bmi = NA)
  expect_true(is.na(result_na))

  result_tagged <- categorize_bmi(bmi = tagged_na("b"))
  expect_equal(result_tagged, "NA(b)")
})

test_that("categorize_bmi() works with vectors", {
  bmi_vals <- c(16, 22, 27, 35, NA)
  result <- categorize_bmi(bmi = bmi_vals)
  expect_equal(result[1], 1L)
  expect_equal(result[2], 2L)
  expect_equal(result[3], 3L)
  expect_equal(result[4], 4L)
  expect_true(is.na(result[5]))
})

test_that("categorize_bmi() works in dataframe context via mutate", {
  df <- data.frame(bmi = c(16.5, 22.0, 27.3, 35.0))
  result <- df %>% mutate(bmi_cat = categorize_bmi(bmi))
  expect_equal(result$bmi_cat, c(1L, 2L, 3L, 4L))
})

# ===========================================================================
# Deprecated aliases — backwards compatibility
# Legacy aliases (bmi_fun, adjusted_bmi_fun, bmi_fun_cat) exist in
# R/legacy/bmi-legacy.R but are not exported. Tests removed as part of
# CRAN submission cleanup (re: #189).
# ===========================================================================
