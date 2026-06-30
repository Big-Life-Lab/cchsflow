# =============================================================================
# ADL Function Tests (v3.0.0)
# =============================================================================
#
# Tests for modernized ADL (Activities of Daily Living) functions:
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
# 1. assess_adl() - Basic Functionality
# =============================================================================

test_that("assess_adl() handles basic valid inputs correctly", {
  # Needs help with all activities
  result_needs_help <- assess_adl(
    ADL_01 = 1, ADL_02 = 1, ADL_03 = 1, ADL_04 = 1, ADL_05 = 1
  )
  expect_equal(result_needs_help, 1L)

  # No help needed
  result_no_help <- assess_adl(
    ADL_01 = 2, ADL_02 = 2, ADL_03 = 2, ADL_04 = 2, ADL_05 = 2
  )
  expect_equal(result_no_help, 2L)

  # Needs help with one task
  result_one <- assess_adl(1, 2, 2, 2, 2)
  expect_equal(result_one, 1L)

  # Needs help with multiple tasks
  result_multiple <- assess_adl(1, 1, 2, 2, 2)
  expect_equal(result_multiple, 1L)
})

test_that("assess_adl() handles vector inputs correctly", {
  # Person 1: needs help; Person 2: no help; Person 3: needs help
  result_vector <- assess_adl(
    ADL_01 = c(1, 2, 1),
    ADL_02 = c(2, 2, 1),
    ADL_03 = c(1, 2, 1),
    ADL_04 = c(2, 2, 1),
    ADL_05 = c(2, 2, 1)
  )
  expect_equal(result_vector, c(1L, 2L, 1L))
})

test_that("assess_adl() handles scalar/vector combinations", {
  # Scalar ADL_01, vector others
  result1 <- assess_adl(1, c(2, 2, 2), c(2, 2, 2), c(2, 2, 2), c(2, 2, 2))
  expect_equal(result1, c(1L, 1L, 1L))

  # Vector ADL_01, scalar others
  result2 <- assess_adl(c(1, 2, 2), 2, 2, 2, 2)
  expect_equal(result2, c(1L, 2L, 2L))
})

# =============================================================================
# 2. assess_adl() - Missing Data Handling
# =============================================================================

test_that("assess_adl() handles raw CCHS missing codes correctly", {
  # Not applicable (6) -> "NA(a)"
  result_6 <- assess_adl(6, 2, 2, 2, 2)
  expect_equal(result_6, "NA(a)")

  # Don't know (7) -> "NA(b)"
  result_7 <- assess_adl(2, 7, 2, 2, 2)
  expect_equal(result_7, "NA(b)")

  # Refusal (8) -> "NA(b)"
  result_8 <- assess_adl(2, 2, 8, 2, 2)
  expect_equal(result_8, "NA(b)")

  # Not stated (9) -> "NA(b)"
  result_9 <- assess_adl(2, 2, 2, 9, 2)
  expect_equal(result_9, "NA(b)")
})

test_that("assess_adl() handles string NA inputs", {
  result_na_string <- assess_adl("Not applicable", 2, 2, 2, 2)
  expect_equal(result_na_string, "NA(a)")

  result_missing_string <- assess_adl(2, "Missing", 2, 2, 2)
  expect_equal(result_missing_string, "NA(b)")

  result_dk_string <- assess_adl(2, 2, "Don't know", 2, 2)
  expect_equal(result_dk_string, "NA(b)")

  result_refusal_string <- assess_adl(2, 2, 2, "Refusal", 2)
  expect_equal(result_refusal_string, "NA(b)")
})

test_that("assess_adl() preserves haven::tagged_na inputs", {
  result_na_a <- assess_adl(haven::tagged_na("a"), 2, 2, 2, 2)
  expect_equal(result_na_a, "NA(a)")

  result_na_b <- assess_adl(2, haven::tagged_na("b"), 2, 2, 2)
  expect_equal(result_na_b, "NA(b)")

  # Output is now character when tagged NAs are present
  input_tagged <- haven::tagged_na("a")
  result_tagged <- assess_adl(input_tagged, 2, 2, 2, 2)
  expect_equal(result_tagged, "NA(a)")
})

test_that("assess_adl() handles out of range inputs correctly", {
  result_invalid_adl01 <- assess_adl(-1, 1, 1, 1, 1)
  expect_equal(result_invalid_adl01, "NA(b)")

  result_invalid_adl02 <- assess_adl(1, -1, 1, 1, 1)
  expect_equal(result_invalid_adl02, "NA(b)")

  result_invalid_adl03 <- assess_adl(1, 1, -1, 1, 1)
  expect_equal(result_invalid_adl03, "NA(b)")

  result_invalid_adl04 <- assess_adl(1, 1, 1, -1, 1)
  expect_equal(result_invalid_adl04, "NA(b)")

  result_invalid_adl05 <- assess_adl(1, 1, 1, 1, -1)
  expect_equal(result_invalid_adl05, "NA(b)")
})

test_that("assess_adl() handles mixed missing data scenarios", {
  adl_01 <- c(1, haven::tagged_na("a"), 6, "Not applicable", 2)
  adl_02 <- c(2, 2, haven::tagged_na("b"), 2, "Missing")
  adl_03 <- c(2, 2, 2, 2, 2)
  adl_04 <- c(2, 2, 2, 2, 2)
  adl_05 <- c(2, 2, 2, 2, 2)

  results <- assess_adl(adl_01, adl_02, adl_03, adl_04, adl_05)

  expect_equal(length(results), 5)
  expect_equal(results[1], "1")                             # valid - needs help (character due to mixed vector)
  expect_true(is.na(results[2]))                            # tagged_na("a") lost tag in c() coercion → plain NA
  expect_equal(results[3], "NA(a)")                         # code 6 -> not applicable
  expect_equal(results[4], "NA(a)")                         # string -> not applicable
  expect_equal(results[5], "NA(b)")                         # "Missing" string
})

# =============================================================================
# 3. score_adl() - 5-item ADL Score
# =============================================================================

test_that("score_adl() handles basic valid inputs correctly", {
  result_max_score <- score_adl(
    ADL_01 = 1, ADL_02 = 1, ADL_03 = 1, ADL_04 = 1, ADL_05 = 1
  )
  expect_equal(result_max_score, 5L)

  result_min_score <- score_adl(
    ADL_01 = 2, ADL_02 = 2, ADL_03 = 2, ADL_04 = 2, ADL_05 = 2
  )
  expect_equal(result_min_score, 0L)

  result_partial_score <- score_adl(
    ADL_01 = 1, ADL_02 = 2, ADL_03 = 1, ADL_04 = 2, ADL_05 = 2
  )
  expect_equal(result_partial_score, 2L)
})

test_that("score_adl() handles raw CCHS missing codes correctly", {
  result_6 <- score_adl(6, 1, 1, 1, 1)
  expect_equal(result_6, "NA(a)")

  result_7 <- score_adl(1, 7, 1, 1, 1)
  expect_equal(result_7, "NA(b)")

  result_8 <- score_adl(1, 1, 8, 1, 1)
  expect_equal(result_8, "NA(b)")

  result_9 <- score_adl(1, 1, 1, 9, 1)
  expect_equal(result_9, "NA(b)")
})

test_that("score_adl() handles string NA inputs", {
  result_na_string <- score_adl("Not applicable", 1, 1, 1, 1)
  expect_equal(result_na_string, "NA(a)")

  result_missing_string <- score_adl(1, "Missing", 1, 1, 1)
  expect_equal(result_missing_string, "NA(b)")

  result_dk_string <- score_adl(1, 1, "Don't know", 1, 1)
  expect_equal(result_dk_string, "NA(b)")
})

test_that("score_adl() handles tagged_na inputs correctly", {
  # Not applicable should take priority
  result_na_a <- score_adl(haven::tagged_na("a"), 1, 1, 1, 1)
  expect_equal(result_na_a, "NA(a)")

  result_na_b <- score_adl(1, haven::tagged_na("b"), 1, 1, 1)
  expect_equal(result_na_b, "NA(b)")

  # Not applicable takes priority over missing
  result_mixed <- score_adl(haven::tagged_na("a"), haven::tagged_na("b"), 1, 1, 1)
  expect_equal(result_mixed, "NA(a)")
})

test_that("score_adl() handles out of range inputs correctly", {
  result_invalid <- score_adl(-1, 1, 1, 1, 1)
  expect_equal(result_invalid, "NA(b)")

  result_invalid2 <- score_adl(1, -1, 1, 1, 1)
  expect_equal(result_invalid2, "NA(b)")

  result_invalid3 <- score_adl(1, 1, -1, 1, 1)
  expect_equal(result_invalid3, "NA(b)")

  result_invalid4 <- score_adl(1, 1, 1, -1, 1)
  expect_equal(result_invalid4, "NA(b)")

  result_invalid5 <- score_adl(1, 1, 1, 1, -1)
  expect_equal(result_invalid5, "NA(b)")
})

test_that("score_adl() handles vector inputs correctly", {
  result_vector <- score_adl(
    ADL_01 = c(1, 2, 1, 2),
    ADL_02 = c(1, 2, 2, 2),
    ADL_03 = c(2, 2, 1, 2),
    ADL_04 = c(1, 2, 1, 2),
    ADL_05 = c(2, 2, 2, 2)
  )
  expect_equal(result_vector, c(3L, 0L, 3L, 0L))
})

# =============================================================================
# 4. score_adl_6() - 6-item ADL Score
# =============================================================================

test_that("score_adl_6() handles basic valid inputs correctly", {
  result_max_score <- score_adl_6(
    ADL_01 = 1, ADL_02 = 1, ADL_03 = 1, ADL_04 = 1, ADL_05 = 1, ADL_06 = 1
  )
  expect_equal(result_max_score, 6L)

  result_min_score <- score_adl_6(
    ADL_01 = 2, ADL_02 = 2, ADL_03 = 2, ADL_04 = 2, ADL_05 = 2, ADL_06 = 2
  )
  expect_equal(result_min_score, 0L)

  result_partial_score <- score_adl_6(
    ADL_01 = 1, ADL_02 = 2, ADL_03 = 1, ADL_04 = 2, ADL_05 = 1, ADL_06 = 2
  )
  expect_equal(result_partial_score, 3L)
})

test_that("score_adl_6() handles raw CCHS missing codes correctly", {
  result_6 <- score_adl_6(6, 1, 1, 1, 1, 1)
  expect_equal(result_6, "NA(a)")

  result_7 <- score_adl_6(1, 7, 1, 1, 1, 1)
  expect_equal(result_7, "NA(b)")

  result_9 <- score_adl_6(1, 1, 1, 1, 9, 1)
  expect_equal(result_9, "NA(b)")
})

test_that("score_adl_6() handles string NA inputs", {
  result_na_string <- score_adl_6("Not applicable", 1, 1, 1, 1, 1)
  expect_equal(result_na_string, "NA(a)")

  result_missing_string <- score_adl_6(1, "Missing", 1, 1, 1, 1)
  expect_equal(result_missing_string, "NA(b)")
})

test_that("score_adl_6() handles tagged_na inputs correctly", {
  result_na_a <- score_adl_6(2, 2, 2, 2, 2, haven::tagged_na("a"))
  expect_equal(result_na_a, "NA(a)")

  result_na_b <- score_adl_6(1, 1, 1, haven::tagged_na("b"), 1, 1)
  expect_equal(result_na_b, "NA(b)")
})

test_that("score_adl_6() handles out of range inputs correctly", {
  result_invalid <- score_adl_6(1, 1, 1, 1, 1, -1)
  expect_equal(result_invalid, "NA(b)")
})

# =============================================================================
# 5. Input Validation
# =============================================================================

test_that("assess_adl() validates required parameters", {
  expect_error(assess_adl(), "argument \"ADL_01\" is missing")
  expect_error(assess_adl(1), "argument \"ADL_02\" is missing")
  expect_error(assess_adl(1, 2), "argument \"ADL_03\" is missing")
  expect_error(assess_adl(1, 2, 2), "argument \"ADL_04\" is missing")
  expect_error(assess_adl(1, 2, 2, 2), "argument \"ADL_05\" is missing")
})

test_that("assess_adl() validates vector length compatibility", {
  # Incompatible multi-element lengths error rather than silently returning NAs
  expect_error(
    assess_adl(c(1, 2), c(2, 2, 2), c(2, 2), c(2, 2), c(2, 2)),
    "same length"
  )

  # Compatible lengths (including recycled scalars) work silently
  expect_silent(assess_adl(c(1, 2), c(2, 2), c(2, 2), c(2, 2), c(2, 2)))
  expect_silent(assess_adl(1, c(2, 2), c(2, 2), c(2, 2), c(2, 2)))
})

# =============================================================================
# 6. Performance and Large Dataset Tests
# =============================================================================

test_that("ADL functions handle large datasets efficiently", {
  n <- 10000
  large_data <- data.frame(
    ADL_01 = sample(1:2, n, replace = TRUE),
    ADL_02 = sample(1:2, n, replace = TRUE),
    ADL_03 = sample(1:2, n, replace = TRUE),
    ADL_04 = sample(1:2, n, replace = TRUE),
    ADL_05 = sample(1:2, n, replace = TRUE),
    ADL_06 = sample(1:2, n, replace = TRUE)
  )

  expect_silent({
    start_time <- Sys.time()
    result_assess <- assess_adl(
      large_data$ADL_01, large_data$ADL_02, large_data$ADL_03,
      large_data$ADL_04, large_data$ADL_05
    )
    end_time <- Sys.time()
  })

  expect_silent({
    result_score <- score_adl(
      large_data$ADL_01, large_data$ADL_02, large_data$ADL_03,
      large_data$ADL_04, large_data$ADL_05
    )
  })

  expect_silent({
    result_score_6 <- score_adl_6(
      large_data$ADL_01, large_data$ADL_02, large_data$ADL_03,
      large_data$ADL_04, large_data$ADL_05, large_data$ADL_06
    )
  })

  expect_length(result_assess, n)
  expect_length(result_score, n)
  expect_length(result_score_6, n)

  # All assess results should be 1 or 2 (no tagged NAs expected with all-valid input)
  expect_true(all(result_assess %in% c(1L, 2L)))

  # Should complete within 1 second for 10k observations
  execution_time <- as.numeric(end_time - start_time)
  expect_lt(execution_time, 1.0)
})

test_that("ADL functions handle extreme edge cases gracefully", {
  # All NA input
  result_all_na <- assess_adl(
    rep(NA, 5), rep(NA, 5), rep(NA, 5), rep(NA, 5), rep(NA, 5)
  )
  expect_length(result_all_na, 5)

  # Empty vectors
  result_empty <- assess_adl(
    numeric(0), numeric(0), numeric(0), numeric(0), numeric(0)
  )
  expect_length(result_empty, 0)

  # All-missing behavior should return NA strings
  result_missing <- assess_adl(
    rep(NA, 3), rep(NA, 3), rep(NA, 3), rep(NA, 3), rep(NA, 3)
  )
  expect_true(all(is.na(result_missing)))
})

# =============================================================================
# 7. Integration Tests
# =============================================================================

test_that("ADL functions work correctly in cchsflow workflows", {
  test_data <- data.frame(
    ADL_01 = c(1, 2, 2, 6, 7),
    ADL_02 = c(1, 2, 1, 2, 2),
    ADL_03 = c(2, 2, 2, 2, 2),
    ADL_04 = c(1, 2, 1, 2, 2),
    ADL_05 = c(2, 2, 2, 2, 2),
    ADL_06 = c(1, 2, 1, 2, 2),
    stringsAsFactors = FALSE
  )

  test_data$adl_help <- assess_adl(
    test_data$ADL_01, test_data$ADL_02, test_data$ADL_03,
    test_data$ADL_04, test_data$ADL_05
  )

  test_data$adl_score_5 <- score_adl(
    test_data$ADL_01, test_data$ADL_02, test_data$ADL_03,
    test_data$ADL_04, test_data$ADL_05
  )

  test_data$adl_score_6 <- score_adl_6(
    test_data$ADL_01, test_data$ADL_02, test_data$ADL_03,
    test_data$ADL_04, test_data$ADL_05, test_data$ADL_06
  )

  expect_equal(nrow(test_data), 5)

  # Person 1: needs help with 3/5 activities (character due to mixed vector with tagged NAs)
  expect_equal(test_data$adl_help[1], "1")
  expect_equal(test_data$adl_score_5[1], "3")
  expect_equal(test_data$adl_score_6[1], "4")

  # Person 2: no help needed (character due to mixed vector with tagged NAs)
  expect_equal(test_data$adl_help[2], "2")
  expect_equal(test_data$adl_score_5[2], "0")
  expect_equal(test_data$adl_score_6[2], "0")

  # Person 4: ADL_01=6 (not applicable)
  expect_equal(test_data$adl_help[4], "NA(a)")

  # Person 5: ADL_01=7 (don't know)
  expect_equal(test_data$adl_help[5], "NA(b)")
})

# =============================================================================
# 8. Version and Metadata Tests
# =============================================================================

test_that("ADL functions exist with correct signatures", {
  expect_true(exists("assess_adl") && is.function(assess_adl))
  expect_true(exists("score_adl") && is.function(score_adl))
  expect_true(exists("score_adl_6") && is.function(score_adl_6))

  # assess_adl and score_adl should have ADL_01-05 parameters
  expected_params <- c("ADL_01", "ADL_02", "ADL_03", "ADL_04", "ADL_05")
  expect_true(all(expected_params %in% names(formals(assess_adl))))
  expect_true(all(expected_params %in% names(formals(score_adl))))

  # score_adl_6 should also have ADL_06
  expected_6_params <- c(expected_params, "ADL_06")
  expect_true(all(expected_6_params %in% names(formals(score_adl_6))))
})

test_that("R/adl.R has proper @note version metadata", {
  adl_path <- system.file("R", "adl.R", package = "cchsflow")
  skip_if(adl_path == "", "Source file not available in installed package")
  function_content <- readLines(adl_path)
  note_lines <- function_content[grep("@note", function_content)]

  expect_gt(length(note_lines), 0)
  expect_true(any(grepl("v\\d+\\.\\d+\\.\\d+", note_lines)))   # semantic versioning
  expect_true(any(grepl("\\d{4}-\\d{2}-\\d{2}", note_lines)))  # YYYY-MM-DD date
  expect_true(any(grepl("status: (active|deprecated|experimental|legacy)", note_lines)))
  expect_true(any(grepl("v3\\.0\\.0", note_lines)))             # v3.0.0 for modernized functions
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
# @note v3.0.0, last updated: 2025-07-05, status: active
