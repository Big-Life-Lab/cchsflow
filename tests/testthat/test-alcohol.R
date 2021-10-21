# Binge drinker

test_that("binge_drinker_fun has expected output when sex is out of range", {
  expect_equal(binge_drinker_fun(-1, 1, 1, 1, 1, 1, 1, 1, 1), "NA(b)")
})

test_that("binge_drinker_fun has expected output when ALW_1 is out of range", {
  expect_equal(binge_drinker_fun(1, -1, 1, 1, 1, 1, 1, 1, 1), "NA(a)")
})

test_that("binge_drinker_fun has expected output when ALW_2A1 is
          out of range", {
  expect_equal(binge_drinker_fun(1, 1, -1, 1, 1, 1, 1, 1, 1), "NA(b)")
})

test_that("binge_drinker_fun has expected output when ALW_2A2 is
          out of range", {
  expect_equal(binge_drinker_fun(1, 1, 1, -1, 1, 1, 1, 1, 1), "NA(b)")
})

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

# low_drink_short_fun

test_that("low_drink_short_fun has expected output when sex is out of range", {
  expect_equal(low_drink_short_fun(-1, 1, 1, 1, 1, 1, 1, 1, 1 ,1, 1), "NA(b)")
})

test_that("low_drink_short_fun has expected output when ALWDWKY is out of
          range", {
  expect_equal(low_drink_short_fun(1, -1, 1, 1, 1, 1, 1, 1, 1 ,1, 1), "NA(b)")
})

test_that("low_drink_short_fun has expected output when ALC_1 is out of
          range", {
 expect_equal(low_drink_short_fun(1, 1, -1, 1, 1, 1, 1, 1, 1 ,1, 1), "NA(b)")
})

test_that("low_drink_short_fun has expected output when ALW_2A1 is out of
          range", {
 expect_equal(low_drink_short_fun(1, 1, 1, -1, 1, 1, 1, 1, 1 ,1, 1), "NA(b)")
})

test_that("low_drink_short_fun has expected output when ALW_2A2 is
          out of range", {
 expect_equal(low_drink_short_fun(1, 1, 1, 1, 1, -1, 1, 1, 1 ,1, 1), "NA(b)")
})

test_that("low_drink_short_fun has expected output when ALW_2A3 is
          out of range", {
 expect_equal(low_drink_short_fun(1, 1, 1, 1, 1, 1, -1, 1, 1 ,1, 1), "NA(b)")
})

test_that("low_drink_short_fun has expected output when ALW_2A4 is
          out of range", {
 expect_equal(low_drink_short_fun(1, 1, 1, 1, 1, 1, 1, -1, 1 ,1, 1), "NA(b)")
})

test_that("low_drink_short_fun has expected output when ALW_2A5 is
          out of range", {
 expect_equal(low_drink_short_fun(1, 1, 1, 1, 1, 1, 1, 1, -1 ,1, 1), "NA(b)")
})

test_that("low_drink_short_fun has expected output when ALW_2A6 is
          out of range", {
 expect_equal(low_drink_short_fun(1, 1, 1, 1, 1, 1, 1, 1, 1, -1, 1), "NA(b)")
})

test_that("low_drink_short_fun has expected output when ALW_2A7 is
          out of range", {
 expect_equal(low_drink_short_fun(1, 1, 1, 1, 1, 1, 1, 1, 1 ,1, -1), "NA(b)")
})

test_that("low_drink_short_fun has expected output when all values are
          in range", {
 expect_equal(low_drink_short_fun(1, 1, 1, 1, 1, 1, 1, 1, 1 ,1, 1), 2)
})

# low_drink_long_fun

test_that("low_drink_long_fun has expected output when sex is out of range", {
  expect_equal(low_drink_long_fun(-1, 1, 1, 1, 1, 1, 1, 1, 1 ,1, 1), "NA(b)")
})

test_that("low_drink_long_fun has expected output when ALWDWKY is out of
          range", {
 expect_equal(low_drink_long_fun(1, -1, 1, 1, 1, 1, 1, 1, 1 ,1, 1), "NA(b)")
})

test_that("low_drink_long_fun has expected output when ALC_1 is out of range", {
  expect_equal(low_drink_long_fun(1, 1, -1, 1, 1, 1, 1, 1, 1 ,1, 1), "NA(b)")
})

test_that("low_drink_long_fun has expected output when ALW_2A1 is out of
          range", {
 expect_equal(low_drink_long_fun(1, 1, 1, -1, 1, 1, 1, 1, 1 ,1, 1), "NA(b)")
})

test_that("low_drink_long_fun has expected output when ALW_2A2 is
          out of range", {
 expect_equal(low_drink_long_fun(1, 1, 1, 1, 1, -1, 1, 1, 1 ,1, 1), "NA(b)")
})

test_that("low_drink_long_fun has expected output when ALW_2A3 is
          out of range", {
 expect_equal(low_drink_long_fun(1, 1, 1, 1, 1, 1, -1, 1, 1 ,1, 1), "NA(b)")
})

test_that("low_drink_long_fun has expected output when ALW_2A4 is
          out of range", {
 expect_equal(low_drink_long_fun(1, 1, 1, 1, 1, 1, 1, -1, 1 ,1, 1), "NA(b)")
})

test_that("low_drink_long_fun has expected output when ALW_2A5 is
          out of range", {
 expect_equal(low_drink_long_fun(1, 1, 1, 1, 1, 1, 1, 1, -1 ,1, 1), "NA(b)")
})

test_that("low_drink_long_fun has expected output when ALW_2A6 is
          out of range", {
 expect_equal(low_drink_long_fun(1, 1, 1, 1, 1, 1, 1, 1, 1, -1, 1), "NA(b)")
})

test_that("low_drink_long_fun has expected output when ALW_2A7 is
          out of range", {
 expect_equal(low_drink_long_fun(1, 1, 1, 1, 1, 1, 1, 1, 1 ,1, -1), "NA(b)")
})

test_that("low_drink_long_fun has expected output when all values are
          in range", {
 expect_equal(low_drink_long_fun(1, 1, 1, 1, 1, 1, 1, 1, 1 ,1, 1), 2)
})

# low_drink_score_fun
test_that("low_drink_score_fun has expected output when sex is out of range", {
  expect_equal(low_drink_score_fun(-1, 1), NA)
})

test_that("low_drink_score_fun has expected output when ALWDWKY is out of
          range", {
 expect_equal(low_drink_score_fun(1, -1), NA)
})

test_that("low_drink_score_fun has expected output when all values are
          in range", {
 expect_equal(low_drink_score_fun(1, 1), 1)
})

# low_drink_score_fun1
test_that("low_drink_score_fun1 has expected output when sex is out of range", {
  expect_equal(low_drink_score_fun1(-1, 1, 1, 2), NA)
})

test_that("low_drink_score_fun1 has expected output when ALWDWKY is out of
          range", {
 expect_equal(low_drink_score_fun1(1, -1, 1 ,2), NA)
})

test_that("low_drink_score_fun1 has expected output when ALC_005 is out of
          range", {
  expect_equal(low_drink_score_fun1(1, 1, -1, 2), NA)
})

test_that("low_drink_score_fun1 has expected output when ALC_1 is out of
          range", {
 expect_equal(low_drink_score_fun1(1, 1, 1 ,-1), NA)
})

test_that("low_drink_score_fun1 has expected output when all values are
          in range", {
 expect_equal(low_drink_score_fun1(1, 1, 1 ,2), 1)
})