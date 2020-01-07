test_that("if_else2 returns TRUE when boolean condition is TRUE", {
  expect_equal(if_else2(1 < 2,TRUE, FALSE), TRUE)
})

test_that("if_else2 returns FALSE when boolean condition is FALSE", {
  expect_equal(if_else2(1 > 2,TRUE, FALSE), FALSE)
})

test_that("if_else2 returns FALSE when the condition evaluates to NA", {
  expect_equal(if_else2(1 < NA,TRUE, FALSE), FALSE)
})

