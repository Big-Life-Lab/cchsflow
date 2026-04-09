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

# All 6 categories — PUMF style (ethnicity 1/2, SDCGRES_cont midpoints 4.5/15)
test_that("category 1: White Canada-born", {
  expect_equal(categorize_immigration(2, 1, 1, 4.5), 1L)
})

test_that("category 2: Visible minority Canada-born (PUMF-style ethnicity)", {
  expect_equal(categorize_immigration(2, 1, 2, 4.5), 2L)
})

test_that("category 3: White immigrant, recent", {
  expect_equal(categorize_immigration(1, 2, 1, 4.5), 3L)
})

test_that("category 4: Visible minority immigrant, recent (PUMF-style)", {
  expect_equal(categorize_immigration(1, 2, 2, 4.5), 4L)
})

test_that("category 5: White immigrant, established", {
  expect_equal(categorize_immigration(1, 2, 1, 15), 5L)
})

test_that("category 6: Visible minority immigrant, established (PUMF-style)", {
  expect_equal(categorize_immigration(1, 2, 2, 15), 6L)
})

# Master style (SDCDCGT_cat7: ethnicity 1-7, SDCDRES raw continuous years)
test_that("category 2: Visible minority Canada-born (master 7-cat ethnicity)", {
  expect_equal(categorize_immigration(2, 1, 5, 4.5), 2L)  # cat5 = Japanese/Korean/etc
})

test_that("category 4: Visible minority immigrant, recent (master 7-cat ethnicity)", {
  expect_equal(categorize_immigration(1, 2, 3, 5), 4L)  # cat3 = Chinese, 5 years
})

test_that("category 6: Visible minority immigrant, established (master 7-cat)", {
  expect_equal(categorize_immigration(1, 2, 7, 12), 6L)  # cat7 = South Asian/Arab/West Asian, 12 years
})
