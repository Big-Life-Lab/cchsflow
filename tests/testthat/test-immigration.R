# calculate_pct_time (unified — continuous years in Canada)

test_that("calculate_pct_time returns 100 when born in Canada", {
  expect_equal(calculate_pct_time(27, 1, 4.5), 100)
})

test_that("calculate_pct_time has expected output when age is out of range", {
  expect_equal(calculate_pct_time(-1, 2, 4.5), tagged_na("b"))
})

test_that("calculate_pct_time has expected output when
          immigrant status is out of range", {
  expect_equal(calculate_pct_time(20, 3, 4.5), tagged_na("b"))
})

test_that("calculate_pct_time has expected output when
          time in Canada is NA", {
  expect_equal(calculate_pct_time(20, 2, NA), tagged_na("b"))
})

test_that("calculate_pct_time has expected output with 0-9 year midpoint", {
  expect_equal(calculate_pct_time(20, 2, 4.5), 22.5)
})

test_that("calculate_pct_time has expected output with 10+ year midpoint", {
  expect_equal(calculate_pct_time(20, 2, 15), 75)
})

test_that("calculate_pct_time has expected output with master continuous years", {
  expect_equal(calculate_pct_time(20, 2, 10), 50)
})

test_that("calculate_pct_time has expected output when age is NA", {
  expect_equal(calculate_pct_time(NA, 2, 4.5), tagged_na("b"))
})

test_that("calculate_pct_time has expected output when immigrant status is NA", {
  expect_equal(calculate_pct_time(20, NA, 4.5), tagged_na("b"))
})

test_that("calculate_pct_time has expected output when all arguments are NA", {
  expect_equal(calculate_pct_time(NA, NA, NA), tagged_na("b"))
})

test_that("calculate_pct_time works with vector inputs (PUMF midpoints)", {
  result <- calculate_pct_time(
    age = c(20, 40, 30),
    born_in_canada = c(2, 1, 2),
    years_in_canada = c(4.5, 4.5, 15)
  )
  expect_equal(result[1], 22.5)
  expect_equal(result[2], 100)
  expect_equal(result[3], 50)
})

test_that("calculate_pct_time returns tagged_na(b) when result exceeds 100", {
  # years_in_canada > age → impossible percentage
  expect_equal(calculate_pct_time(20, 2, 25), tagged_na("b"))
})

test_that("calculate_pct_time returns 100 at exact boundary", {
  # years_in_canada == age → exactly 100%
  expect_equal(calculate_pct_time(20, 2, 20), 100)
})

test_that("calculate_pct_time works with vector inputs (master continuous)", {
  result <- calculate_pct_time(
    age = c(20, 40, 30),
    born_in_canada = c(2, 1, 2),
    years_in_canada = c(10, 20, 5)
  )
  expect_equal(result[1], 50)
  expect_equal(result[2], 100)
  expect_length(result, 3)
})

# categorize_pct_time

test_that("categorize_pct_time has expected output when input is out of range", {
  expect_equal(categorize_pct_time(-1), "NA(b)")
})

test_that("categorize_pct_time has expected output when input is in range", {
  expect_equal(categorize_pct_time(1), "1")
})

test_that("categorize_pct_time returns 10 at upper boundary", {
  expect_equal(categorize_pct_time(100), "10")
})

test_that("categorize_pct_time handles tagged_na(a)", {
  expect_equal(categorize_pct_time(tagged_na("a")), "NA(a)")
})

test_that("categorize_pct_time works with vector inputs", {
  result <- categorize_pct_time(c(5, 25, 55, 85, 100))
  expect_equal(result, c("1", "3", "6", "9", "10"))
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
