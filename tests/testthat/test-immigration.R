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


# categorize_immigration

# Out-of-range inputs → NA(b)
test_that("returns NA(b) for out-of-range immigrant_status", {
  expect_equal(categorize_immigration(-1, 2, 2, 4.5), tagged_na("b"))
})

test_that("returns NA(b) for out-of-range born_canada", {
  expect_equal(categorize_immigration(1, -2, 2, 4.5), tagged_na("b"))
})

test_that("returns NA(b) for out-of-range ethnicity", {
  expect_equal(categorize_immigration(1, 2, -2, 4.5), tagged_na("b"))
})

test_that("propagates NA(b) from years", {
  expect_equal(categorize_immigration(1, 2, 2, tagged_na("b")), tagged_na("b"))
})

# NA(a) propagation
test_that("propagates NA(a) from immigrant_status", {
  expect_equal(categorize_immigration(tagged_na("a"), 2, 2, 4.5), tagged_na("a"))
})

test_that("propagates NA(a) from born_canada", {
  expect_equal(categorize_immigration(1, tagged_na("a"), 2, 4.5), tagged_na("a"))
})

test_that("propagates NA(a) from ethnicity", {
  expect_equal(categorize_immigration(1, 2, tagged_na("a"), 4.5), tagged_na("a"))
})

test_that("propagates NA(a) from years", {
  expect_equal(categorize_immigration(1, 2, 2, tagged_na("a")), tagged_na("a"))
})

# All 8 categories — PUMF style (ethnicity 1/2, SDCGRES_cont midpoints 4.5/15)

test_that("category 1: White Canada-born", {
  expect_equal(categorize_immigration(2, 1, 1, 4.5), 1L)
})

test_that("category 2: Visible minority Canada-born", {
  expect_equal(categorize_immigration(2, 1, 2, 4.5), 2L)
})

test_that("category 3: White immigrant, recent (<10 years)", {
  expect_equal(categorize_immigration(1, 2, 1, 4.5), 3L)
})

test_that("category 4: Visible minority immigrant, recent (<10 years)", {
  expect_equal(categorize_immigration(1, 2, 2, 4.5), 4L)
})

test_that("category 5: White immigrant, established (10+ years)", {
  expect_equal(categorize_immigration(1, 2, 1, 15), 5L)
})

test_that("category 6: Visible minority immigrant, established (10+ years)", {
  expect_equal(categorize_immigration(1, 2, 2, 15), 6L)
})

test_that("category 7: White non-immigrant born outside Canada", {
  expect_equal(categorize_immigration(2, 2, 1, tagged_na("a")), 7L)
})

test_that("category 8: Visible minority non-immigrant born outside Canada", {
  expect_equal(categorize_immigration(2, 2, 2, tagged_na("a")), 8L)
})

# Categories 7-8: years value doesn't matter (non-immigrant, so years is irrelevant)

test_that("category 7 works regardless of years value", {
  expect_equal(categorize_immigration(2, 2, 1, tagged_na("b")), 7L)
})

test_that("category 8 works regardless of years value", {
  expect_equal(categorize_immigration(2, 2, 2, tagged_na("b")), 8L)
})

# Master style (SDCDCGT_cat7: ethnicity 1-7, SDCDRES raw continuous years)

test_that("category 2: Visible minority Canada-born (master 7-cat ethnicity)", {
  expect_equal(categorize_immigration(2, 1, 5, 4.5), 2L)
})

test_that("category 4: Visible minority immigrant, recent (master 7-cat ethnicity)", {
  expect_equal(categorize_immigration(1, 2, 3, 5), 4L)
})

test_that("category 6: Visible minority immigrant, established (master 7-cat)", {
  expect_equal(categorize_immigration(1, 2, 7, 12), 6L)
})

test_that("category 8: Visible minority non-immigrant born outside Canada (master 7-cat)", {
  expect_equal(categorize_immigration(2, 2, 5, tagged_na("a")), 8L)
})
