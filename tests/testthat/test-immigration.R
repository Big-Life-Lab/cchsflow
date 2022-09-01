# pct_time_fun

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

# pct_time_fun_cat
test_that("pct_time_fun_cat has expected output when input is out of range", {
  expect_equal(pct_time_fun_cat(-1), "NA(b)")
})

test_that("pct_time_fun_cat has expected output when input is in range", {
  expect_equal(pct_time_fun_cat(1), 1)
})

# immigration_fun
test_that("immigration_fun has expected output when SDCFIMM is out of range", {
  expect_equal(immigration_fun(-1,2, 2, 1), "NA(b)")
})

test_that("immigration_fun has expected output when SDCGCBG is out of range", {
  expect_equal(immigration_fun(1,-2, 2, 1), "NA(b)")
})

test_that("immigration_fun has expected output when SDCGCGT is out of range", {
  expect_equal(immigration_fun(1,2, -2, 1), "NA(b)")
})

test_that("immigration_fun has expected output when SDCGRES is out of range", {
  expect_equal(immigration_fun(1,2, 2, -1), "NA(b)")
})

test_that("immigration_fun has expected output when all values are in range", {
  expect_equal(immigration_fun(1,2, 2, 1), 4)
})