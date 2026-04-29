# =============================================================================
# SMKDSTY_cat6 Derived Variable Tests
# =============================================================================
#
# Tests for calculate_SMKDSTY_cat6() - the 3-step architecture function that
# reconstructs the 6-category smoking status for 2015+ cycles using:
#   SMK_005 (current status), SMK_030 (ever daily), SMK_01A (100+ cigs)
#
# Logic mapping:
#   1 = Daily smoker:                SMK_005 == 1
#   2 = Occasional (former daily):   SMK_005 == 2, SMK_030 == 1
#   3 = Occasional (never daily):    SMK_005 == 2, SMK_030 == 2 or missing
#   4 = Former daily:                SMK_005 == 3, SMK_030 == 1
#   5 = Former occasional:           SMK_005 == 3, SMK_030 == 2, SMK_01A == 1
#   6 = Never smoked:                SMK_005 == 3, SMK_01A == 2
#
# =============================================================================

library(testthat)
library(haven)

# =============================================================================
# Category 1 - Daily smoker
# =============================================================================

test_that("calculate_SMKDSTY_cat6 returns 1 for daily smokers", {

  result <- calculate_SMKDSTY_cat6(
    SMK_005 = 1, SMK_030 = 1, SMK_01A = 1
  )
  expect_equal(result, 1L)

  # SMK_030 and SMK_01A are irrelevant for daily smokers
  result2 <- calculate_SMKDSTY_cat6(
    SMK_005 = 1, SMK_030 = 2, SMK_01A = 2
  )
  expect_equal(result2, 1L)
})

# =============================================================================
# Category 2 - Occasional (former daily)
# =============================================================================

test_that("calculate_SMKDSTY_cat6 returns 2 for occasional former-daily smokers", {

  result <- calculate_SMKDSTY_cat6(
    SMK_005 = 2, SMK_030 = 1, SMK_01A = 1
  )
  expect_equal(result, 2L)
})

# =============================================================================
# Category 3 - Occasional (never daily)
# =============================================================================

test_that("calculate_SMKDSTY_cat6 returns 3 for occasional never-daily smokers", {

  # SMK_030 == 2 (never smoked daily)
  result <- calculate_SMKDSTY_cat6(
    SMK_005 = 2, SMK_030 = 2, SMK_01A = 1
  )
  expect_equal(result, 3L)
})

# =============================================================================
# Category 4 - Former daily
# =============================================================================

test_that("calculate_SMKDSTY_cat6 returns 4 for former daily smokers", {

  result <- calculate_SMKDSTY_cat6(
    SMK_005 = 3, SMK_030 = 1, SMK_01A = 1
  )
  expect_equal(result, 4L)
})

# =============================================================================
# Category 5 - Former occasional (100+ cigarettes)
# =============================================================================

test_that("calculate_SMKDSTY_cat6 returns 5 for former occasional with 100+ cigs", {

  result <- calculate_SMKDSTY_cat6(
    SMK_005 = 3, SMK_030 = 2, SMK_01A = 1
  )
  expect_equal(result, 5L)
})

# =============================================================================
# Category 6 - Never smoked
# =============================================================================

test_that("calculate_SMKDSTY_cat6 returns 6 for never smokers", {

  result <- calculate_SMKDSTY_cat6(
    SMK_005 = 3, SMK_030 = 2, SMK_01A = 2
  )
  expect_equal(result, 6L)
})

# =============================================================================
# Vector inputs - all 6 categories
# =============================================================================

test_that("calculate_SMKDSTY_cat6 handles vector inputs with all 6 categories", {

  result <- calculate_SMKDSTY_cat6(
    SMK_005 = c(1, 2, 2, 3, 3, 3),
    SMK_030 = c(1, 1, 2, 1, 2, 2),
    SMK_01A = c(1, 1, 1, 1, 1, 2)
  )

  expect_length(result, 6)
  expect_equal(result[1], 1L)  # Daily
  expect_equal(result[2], 2L)  # Occasional (former daily)
  expect_equal(result[3], 3L)  # Occasional (never daily)
  expect_equal(result[4], 4L)  # Former daily
  expect_equal(result[5], 5L)  # Former occasional
  expect_equal(result[6], 6L)  # Never smoked
})

# =============================================================================
# Missing data handling
# =============================================================================

test_that("calculate_SMKDSTY_cat6 handles missing SMK_005", {

  result <- calculate_SMKDSTY_cat6(
    SMK_005 = tagged_na("b"), SMK_030 = 1, SMK_01A = 1
  )
  expect_true(is.na(result))
})

test_that("calculate_SMKDSTY_cat6 handles missing SMK_030 for occasional smoker", {

  # SMK_005 == 2, SMK_030 missing -> category 3 (occasional, never daily)
  # This matches the legacy logic: missing SMK_030 treated same as SMK_030 == 2
  result <- calculate_SMKDSTY_cat6(
    SMK_005 = 2, SMK_030 = tagged_na("a"), SMK_01A = 1
  )
  expect_equal(result, 3L)
})

test_that("calculate_SMKDSTY_cat6 requires all three inputs", {

  expect_error(
    calculate_SMKDSTY_cat6(SMK_005 = 1, SMK_030 = 1),
    "SMK_005, SMK_030, and SMK_01A are required"
  )
})

# =============================================================================
# Legacy alias compatibility
# =============================================================================

test_that("calculate_SMKDSTY_original is a deprecated alias for calculate_SMKDSTY_cat6", {

  expect_warning(
    result <- calculate_SMKDSTY_original(SMK_005 = 1, SMK_030 = 1, SMK_01A = 1),
    "deprecated"
  )
  expect_equal(result, 1L)
})

# =============================================================================
# Consistency with legacy SMKDSTY_fun
# =============================================================================

test_that("calculate_SMKDSTY_cat6 matches legacy SMKDSTY_fun for valid inputs", {

  # Category 5: SMK_005=3, SMK_030=2, SMK_01A=1
  new_result <- calculate_SMKDSTY_cat6(SMK_005 = 3, SMK_030 = 2, SMK_01A = 1)
  legacy_result <- SMKDSTY_fun(3, 2, 1)
  expect_equal(new_result, legacy_result)

  # Category 3: SMK_005=2, SMK_030=2, SMK_01A=NA
  new_result2 <- calculate_SMKDSTY_cat6(SMK_005 = 2, SMK_030 = 2, SMK_01A = tagged_na("a"))
  legacy_result2 <- SMKDSTY_fun(2, 2, NA)
  expect_equal(new_result2, legacy_result2)
})
