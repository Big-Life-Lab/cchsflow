# =============================================================================
# age_start_smoking Derived Variable Tests
# =============================================================================
#
# Tests for calculate_age_start_smoking() which receives a single continuous
# age input. The worksheet routes the appropriate source variable:
#   - PUMF: SMKG040_cont (midpoint estimation ~+/-3 yr)
#   - Master: SMK_040 (exact continuous)
#
# Universe: Ever-daily smokers (SMKDSTY 1, 2, 4)
# NA for: Never-daily smokers (SMKDSTY 3, 5, 6)
#
# =============================================================================

library(testthat)
library(haven)

# =============================================================================
# Basic pass-through
# =============================================================================

test_that("calculate_age_start_smoking passes through valid values", {

  expect_equal(calculate_age_start_smoking(18), 18)
  expect_equal(calculate_age_start_smoking(25), 25)
  expect_equal(calculate_age_start_smoking(42), 42)
})

test_that("calculate_age_start_smoking maintains decimal precision", {

  expect_equal(calculate_age_start_smoking(18.5), 18.5)
  expect_equal(calculate_age_start_smoking(22.3), 22.3)
})

# =============================================================================
# Missing value handling
# =============================================================================

test_that("calculate_age_start_smoking returns tagged NA(a) for not applicable", {

  result <- calculate_age_start_smoking(haven::tagged_na("a"))
  expect_true(haven::is_tagged_na(result, "a"))
})

test_that("calculate_age_start_smoking propagates tagged NA(b)", {

  result <- calculate_age_start_smoking(haven::tagged_na("b"))
  expect_true(haven::is_tagged_na(result, "b"))
})

test_that("calculate_age_start_smoking returns missing when NULL", {

  result <- calculate_age_start_smoking(NULL)
  expect_true(is.na(result))
})

test_that("calculate_age_start_smoking returns missing when no input", {

  result <- calculate_age_start_smoking()
  expect_true(is.na(result))
})

# =============================================================================
# Vector inputs
# =============================================================================

test_that("calculate_age_start_smoking handles vector inputs", {

  result <- calculate_age_start_smoking(c(18, 25, NA, 16))

  expect_length(result, 4)
  expect_equal(result[1], 18)
  expect_equal(result[2], 25)
  expect_true(is.na(result[3]))
  expect_equal(result[4], 16)
})

test_that("calculate_age_start_smoking handles empty input", {

  result <- calculate_age_start_smoking(numeric(0))
  expect_length(result, 0)
})

# =============================================================================
# CCHS codebook midpoints (PUMF values)
# =============================================================================

test_that("calculate_age_start_smoking handles PUMF midpoint categories", {

  # SMKG040_cont midpoints from variable_details:
  #   Cat 1 (5-11) -> 8,  Cat 2 (12-14) -> 13,  Cat 3 (15-17) -> 16,
  #   Cat 4 (18-19) -> 18.5,  Cat 5 (20-24) -> 22,  Cat 6 (25-29) -> 27,
  #   Cat 7 (30-34) -> 32,  Cat 8 (35-39) -> 37,  Cat 9 (40-44) -> 42,
  #   Cat 10 (45-49) -> 47,  Cat 11 (50+) -> 55
  midpoints <- c(13, 16, 18.5, 22, 27, 32, 37, 42, 47, 55)
  for (mp in midpoints) {
    result <- calculate_age_start_smoking(mp)
    expect_equal(result, mp,
                 info = paste("Midpoint", mp, "should pass through unchanged"))
  }
})

# =============================================================================
# Legacy function compatibility
# =============================================================================

test_that("SMKG040_fun still exists and works (legacy)", {

  # Legacy function in R/smoking.R combines SMKG203_cont + SMKG207_cont
  # When SMKG203_cont has a value and SMKG207_cont is NA(a), use SMKG203_cont
  result <- SMKG040_fun(
    SMKG203_cont = 18,
    SMKG207_cont = haven::tagged_na("a")
  )
  expect_equal(result, 18)
})
