test_that("if_else2 returns TRUE when boolean condition is TRUE", {
  expect_equal(if_else2(1 < 2, TRUE, FALSE), TRUE)
})

test_that("if_else2 returns FALSE when boolean condition is FALSE", {
  expect_equal(if_else2(1 > 2, TRUE, FALSE), FALSE)
})

test_that("if_else2 returns FALSE when the condition evaluates to NA", {
  expect_equal(if_else2(1 < NA, TRUE, FALSE), FALSE)
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
