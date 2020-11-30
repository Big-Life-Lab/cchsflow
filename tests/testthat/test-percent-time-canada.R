test_that("pct_time_fun has expected output when age is out of range", {
  expect_equal(pct_time_fun(-1, 2, 1), tagged_na("b"))
})

test_that("pct_time_fun has expected output when
          immigrant status is out of range", {
  expect_equal(pct_time_fun(20, 3, 1), tagged_na("b"))
})

test_that("pct_time_fun has expected output when
          time in Canada is out of range", {
  expect_equal(pct_time_fun(20, 2, 3), tagged_na("b"))
})

test_that("pct_time_fun has expected output when all arguments are in range", {
  expect_equal(pct_time_fun(20, 2, 1), 22.5)
})

test_that("pct_time_fun has expected output when age is NA", {
  expect_equal(pct_time_fun(NA, 2, 1), tagged_na("b"))
})

test_that("pct_time_fun has expected output when immigrant status is NA", {
  expect_equal(pct_time_fun(20, NA, 1), tagged_na("b"))
})

test_that("pct_time_fun has expected output when time in Canada is NA", {
  expect_equal(pct_time_fun(20, 2, NA), tagged_na("b"))
})

test_that("pct_time_fun has expected output when all arguments are NA", {
  expect_equal(pct_time_fun(NA, NA, NA), tagged_na("b"))
})
