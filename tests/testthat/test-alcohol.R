# Binge drinker

test_that("assess_binge_drinking has expected output when sex is out of range", {
  expect_equal(assess_binge_drinking(-1, 1, 1, 1, 1, 1, 1, 1, 1), tagged_na("b"))
})

test_that("assess_binge_drinking has expected output when ALW_1 is out of range", {
  expect_equal(assess_binge_drinking(1, -1, 1, 1, 1, 1, 1, 1, 1), tagged_na("a"))
})

test_that("assess_binge_drinking has expected output when ALW_2A1 is
          out of range", {
  expect_equal(assess_binge_drinking(1, 1, -1, 1, 1, 1, 1, 1, 1), tagged_na("b"))
})

test_that("assess_binge_drinking has expected output when ALW_2A2 is
          out of range", {
  expect_equal(assess_binge_drinking(1, 1, 1, -1, 1, 1, 1, 1, 1), tagged_na("b"))
})

test_that("assess_binge_drinking has expected output when ALW_2A3 is
          out of range", {
  expect_equal(assess_binge_drinking(1, 1, 1, 1, -1, 1, 1, 1, 1), tagged_na("b"))
})

test_that("assess_binge_drinking has expected output when ALW_2A4 is
          out of range", {
  expect_equal(assess_binge_drinking(1, 1, 1, 1, 1, -1, 1, 1, 1), tagged_na("b"))
})

test_that("assess_binge_drinking has expected output when ALW_2A5 is
          out of range", {
  expect_equal(assess_binge_drinking(1, 1, 1, 1, 1, 1, -1, 1, 1), tagged_na("b"))
})

test_that("assess_binge_drinking has expected output when ALW_2A6 is
          out of range", {
  expect_equal(assess_binge_drinking(1, 1, 1, 1, 1, 1, 1, -1, 1), tagged_na("b"))
})

test_that("assess_binge_drinking has expected output when ALW_2A7 is
          out of range", {
  expect_equal(assess_binge_drinking(1, 1, 1, 1, 1, 1, 1, 1, -1), tagged_na("b"))
})

test_that("assess_binge_drinking has expected output when all values are
          in range", {
  expect_equal(assess_binge_drinking(1, 1, 1, 1, 1, 1, 1, 1, 1), 2L)
})

# assess_drinking_risk_short

test_that("assess_drinking_risk_short has expected output when sex is out of range", {
  expect_equal(assess_drinking_risk_short(-1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), tagged_na("b"))
})

test_that("assess_drinking_risk_short has expected output when ALWDWKY is out of
          range", {
  expect_equal(assess_drinking_risk_short(1, -1, 1, 1, 1, 1, 1, 1, 1, 1, 1), tagged_na("b"))
})

test_that("assess_drinking_risk_short has expected output when ALC_1 is out of
          range", {
  expect_equal(assess_drinking_risk_short(1, 1, -1, 1, 1, 1, 1, 1, 1, 1, 1), tagged_na("b"))
})

test_that("assess_drinking_risk_short has expected output when ALW_2A1 is out of
          range", {
  expect_equal(assess_drinking_risk_short(1, 1, 1, -1, 1, 1, 1, 1, 1, 1, 1), tagged_na("b"))
})

test_that("assess_drinking_risk_short has expected output when ALW_2A2 is
          out of range", {
  expect_equal(assess_drinking_risk_short(1, 1, 1, 1, 1, -1, 1, 1, 1, 1, 1), tagged_na("b"))
})

test_that("assess_drinking_risk_short has expected output when ALW_2A3 is
          out of range", {
  expect_equal(assess_drinking_risk_short(1, 1, 1, 1, 1, 1, -1, 1, 1, 1, 1), tagged_na("b"))
})

test_that("assess_drinking_risk_short has expected output when ALW_2A4 is
          out of range", {
  expect_equal(assess_drinking_risk_short(1, 1, 1, 1, 1, 1, 1, -1, 1, 1, 1), tagged_na("b"))
})

test_that("assess_drinking_risk_short has expected output when ALW_2A5 is
          out of range", {
  expect_equal(assess_drinking_risk_short(1, 1, 1, 1, 1, 1, 1, 1, -1, 1, 1), tagged_na("b"))
})

test_that("assess_drinking_risk_short has expected output when ALW_2A6 is
          out of range", {
  expect_equal(assess_drinking_risk_short(1, 1, 1, 1, 1, 1, 1, 1, 1, -1, 1), tagged_na("b"))
})

test_that("assess_drinking_risk_short has expected output when ALW_2A7 is
          out of range", {
  expect_equal(assess_drinking_risk_short(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, -1), tagged_na("b"))
})

test_that("assess_drinking_risk_short has expected output when all values are
          in range", {
  expect_equal(assess_drinking_risk_short(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), 2L)
})

# assess_drinking_risk_long

test_that("assess_drinking_risk_long has expected output when sex is out of range", {
  expect_equal(assess_drinking_risk_long(-1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), tagged_na("b"))
})

test_that("assess_drinking_risk_long has expected output when ALWDWKY is out of
          range", {
  expect_equal(assess_drinking_risk_long(1, -1, 1, 1, 1, 1, 1, 1, 1, 1, 1), tagged_na("b"))
})

test_that("assess_drinking_risk_long has expected output when ALC_1 is out of range", {
  expect_equal(assess_drinking_risk_long(1, 1, -1, 1, 1, 1, 1, 1, 1, 1, 1), tagged_na("b"))
})

test_that("assess_drinking_risk_long has expected output when ALW_2A1 is out of
          range", {
  expect_equal(assess_drinking_risk_long(1, 1, 1, -1, 1, 1, 1, 1, 1, 1, 1), tagged_na("b"))
})

test_that("assess_drinking_risk_long has expected output when ALW_2A2 is
          out of range", {
  expect_equal(assess_drinking_risk_long(1, 1, 1, 1, 1, -1, 1, 1, 1, 1, 1), tagged_na("b"))
})

test_that("assess_drinking_risk_long has expected output when ALW_2A3 is
          out of range", {
  expect_equal(assess_drinking_risk_long(1, 1, 1, 1, 1, 1, -1, 1, 1, 1, 1), tagged_na("b"))
})

test_that("assess_drinking_risk_long has expected output when ALW_2A4 is
          out of range", {
  expect_equal(assess_drinking_risk_long(1, 1, 1, 1, 1, 1, 1, -1, 1, 1, 1), tagged_na("b"))
})

test_that("assess_drinking_risk_long has expected output when ALW_2A5 is
          out of range", {
  expect_equal(assess_drinking_risk_long(1, 1, 1, 1, 1, 1, 1, 1, -1, 1, 1), tagged_na("b"))
})

test_that("assess_drinking_risk_long has expected output when ALW_2A6 is
          out of range", {
  expect_equal(assess_drinking_risk_long(1, 1, 1, 1, 1, 1, 1, 1, 1, -1, 1), tagged_na("b"))
})

test_that("assess_drinking_risk_long has expected output when ALW_2A7 is
          out of range", {
  expect_equal(assess_drinking_risk_long(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, -1), tagged_na("b"))
})

test_that("assess_drinking_risk_long has expected output when all values are
          in range", {
  expect_equal(assess_drinking_risk_long(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), 2L)
})

# TODO: Implement missing scoring functions for future phases
# 
# # low_drink_score_fun
# test_that("low_drink_score_fun has expected output when sex is out of range", {
#   expect_equal(low_drink_score_fun(-1, 1), tagged_na("b"))
# })
# 
# test_that("low_drink_score_fun has expected output when ALWDWKY is out of
#           range", {
#   expect_equal(low_drink_score_fun(1, -1), tagged_na("b"))
# })
# 
# test_that("low_drink_score_fun has expected output when all values are
#           in range", {
#   expect_equal(low_drink_score_fun(1, 1), 1)
# })
# 
# # low_drink_score_fun1
# test_that("low_drink_score_fun1 has expected output when sex is out of range", {
#   expect_equal(low_drink_score_fun1(-1, 1, 1, 2), tagged_na("b"))
# })
# 
# test_that("low_drink_score_fun1 has expected output when ALWDWKY is out of
#           range", {
#   expect_equal(low_drink_score_fun1(1, -1, 1, 2), tagged_na("b"))
# })
# 
# test_that("low_drink_score_fun1 has expected output when ALC_005 is out of
#           range", {
#   expect_equal(low_drink_score_fun1(1, 1, -1, 2), tagged_na("b"))
# })
# 
# test_that("low_drink_score_fun1 has expected output when ALC_1 is out of
#           range", {
#   expect_equal(low_drink_score_fun1(1, 1, 1, -2), tagged_na("b"))
# })
# 
# test_that("low_drink_score_fun1 has expected output when all values are
#           in range", {
#   expect_equal(low_drink_score_fun1(1, 1, 1, 2), 2)
# })
