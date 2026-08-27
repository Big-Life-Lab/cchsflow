test_that("if_else2 returns TRUE when boolean condition is TRUE", {
  expect_equal(if_else2(1 < 2, TRUE, FALSE), TRUE)
})

test_that("if_else2 returns FALSE when boolean condition is FALSE", {
  expect_equal(if_else2(1 > 2, TRUE, FALSE), FALSE)
})

test_that("if_else2 returns FALSE when the condition evaluates to NA", {
  expect_equal(if_else2(1 < NA, TRUE, FALSE), FALSE)
})

test_that("default_missing_config returns non-empty codes from YAML", {
  config <- default_missing_config()
  expect_true(length(config$na_a_codes) > 0)
  expect_true(length(config$na_b_codes) > 0)
  expect_true(996 %in% config$na_a_codes)
  expect_true(999 %in% config$na_b_codes)
  expect_equal(config$na_a_priority, 1)
  expect_equal(config$na_b_priority, 2)
})

test_that("default_missing_config pattern_only returns just codes", {
  pattern <- default_missing_config(pattern_only = TRUE)
  expect_true(!is.null(pattern$na_a_codes))
  expect_true(!is.null(pattern$na_b_codes))
  expect_null(pattern$na_a_priority)
  expect_null(pattern$na_b_priority)
})

test_that("any_missing works without HWTGBMI_der fallback", {
  # Simulate a DV function calling any_missing with a parameter name
  f <- function(cigs) suppressWarnings(any_missing(cigs))
  result <- f(c(10, NA, 999))
  expect_equal(result, c(FALSE, TRUE, TRUE))
})

test_that("get_priority_missing works without HWTGBMI_der fallback", {
  # Two vectors: one with NA(a), one with NA(b)
  # NA(a) has higher priority (priority=1), so it should win
  result <- suppressWarnings(
    get_priority_missing(
      c(haven::tagged_na("a")),
      c(haven::tagged_na("b"))
    )
  )
  expect_true(haven::is_tagged_na(result[1], "a"))
})

test_that("clean_variables accepts labelled numeric vectors", {
  x <- c(5.5, 10.2, NA)
  attr(x, "label") <- "Time since quit"
  attr(x, "unit") <- "years"
  attr(x, "label_long") <- "SMK_09A continuous"

  expect_no_error(
    suppressWarnings(
      clean_variables(list(SMK_09A_cont = x), output_format = "tagged_na")
    )
  )
})
