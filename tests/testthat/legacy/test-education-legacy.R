test_that("EDUDR04_fun returns 4 for post-secondary graduate", {
  expect_equal(EDUDR04_fun(3, 1, 1, 6), 4)
})

test_that("EDUDR04_fun returns 1 for less than high school", {
  expect_equal(EDUDR04_fun(1, 2, 2, 96), 1)
})

test_that("EDUDR04_fun returns 2 for HS grad", {
  expect_equal(EDUDR04_fun(3, 1, 2, 96), 2)
})
