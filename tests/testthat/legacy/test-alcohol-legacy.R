test_that("binge_drinker_fun has expected output when ALW_2A3 is
          out of range", {
  expect_equal(binge_drinker_fun(1, 1, 1, 1, -1, 1, 1, 1, 1), "NA(b)")
})

test_that("binge_drinker_fun has expected output when ALW_2A4 is
          out of range", {
  expect_equal(binge_drinker_fun(1, 1, 1, 1, 1, -1, 1, 1, 1), "NA(b)")
})

test_that("binge_drinker_fun has expected output when ALW_2A5 is
          out of range", {
  expect_equal(binge_drinker_fun(1, 1, 1, 1, 1, 1, -1, 1, 1), "NA(b)")
})

test_that("binge_drinker_fun has expected output when ALW_2A6 is
          out of range", {
  expect_equal(binge_drinker_fun(1, 1, 1, 1, 1, 1, 1, -1, 1), "NA(b)")
})

test_that("binge_drinker_fun has expected output when ALW_2A7 is
          out of range", {
  expect_equal(binge_drinker_fun(1, 1, 1, 1, 1, 1, 1, 1, -1), "NA(b)")
})

test_that("binge_drinker_fun has expected output when all values are
          in range", {
  expect_equal(binge_drinker_fun(1, 1, 1, 1, 1, 1, 1, 1, 1), 2)
})

