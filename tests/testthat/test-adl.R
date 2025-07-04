# Updated tests for v3.0.0 - Using new function names and correct tagged_na expectations
library(haven)

# Tests for modernized assess_adl() function (replaces adl_fun)
test_that("assess_adl has expected output when ADL_01 is out of range", {
  result <- assess_adl(-1, 1, 1, 1, 1)
  expect_true(is_tagged_na(result, "b"))
})

test_that("assess_adl has expected output when ADL_02 is out of range", {
  result <- assess_adl(1, -1, 1, 1, 1)
  expect_true(is_tagged_na(result, "b"))
})

test_that("assess_adl has expected output when ADL_03 is out of range", {
  result <- assess_adl(1, 1, -1, 1, 1)
  expect_true(is_tagged_na(result, "b"))
})

test_that("assess_adl has expected output when ADL_04 is out of range", {
  result <- assess_adl(1, 1, 1, -1, 1)
  expect_true(is_tagged_na(result, "b"))
})

test_that("assess_adl has expected output when ADL_05 is out of range", {
  result <- assess_adl(1, 1, 1, 1, -1)
  expect_true(is_tagged_na(result, "b"))
})

test_that("assess_adl has expected output when all values are in range - needs help", {
  expect_equal(assess_adl(1, 1, 1, 1, 1), 1L)
})

test_that("assess_adl has expected output when all values are in range - no help needed", {
  expect_equal(assess_adl(2, 2, 2, 2, 2), 2L)
})

# Tests for modernized score_adl() function (replaces adl_score_5_fun)
test_that("score_adl has expected output when ADL_01 is out of range", {
  result <- score_adl(-1, 1, 1, 1, 1)
  expect_true(is_tagged_na(result, "b"))
})

test_that("score_adl has expected output when ADL_02 is out of range", {
  result <- score_adl(1, -1, 1, 1, 1)
  expect_true(is_tagged_na(result, "b"))
})

test_that("score_adl has expected output when ADL_03 is out of range", {
  result <- score_adl(1, 1, -1, 1, 1)
  expect_true(is_tagged_na(result, "b"))
})

test_that("score_adl has expected output when ADL_04 is out of range", {
  result <- score_adl(1, 1, 1, -1, 1)
  expect_true(is_tagged_na(result, "b"))
})

test_that("score_adl has expected output when ADL_05 is out of range", {
  result <- score_adl(1, 1, 1, 1, -1)
  expect_true(is_tagged_na(result, "b"))
})

test_that("score_adl has expected output when all values need help", {
  expect_equal(score_adl(1, 1, 1, 1, 1), 5L) # 5 tasks need help
})

test_that("score_adl has expected output when no help needed", {
  expect_equal(score_adl(2, 2, 2, 2, 2), 0L) # 0 tasks need help
})

test_that("score_adl has expected output when some need help", {
  expect_equal(score_adl(1, 2, 1, 2, 2), 2L) # 2 tasks need help
})

# Test backward compatibility with deprecated function names
test_that("deprecated adl_fun works with warning", {
  expect_warning(result <- adl_fun(1, 1, 1, 1, 1), "deprecated")
  expect_equal(result, 1L)
})

test_that("deprecated adl_score_5_fun works with warning", {
  expect_warning(result <- adl_score_5_fun(2, 2, 2, 2, 2), "deprecated")
  expect_equal(result, 0L)
})
