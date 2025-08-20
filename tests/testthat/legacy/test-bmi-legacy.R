test_that("bmi_fun() handles one or more NA arguments properly", {
  expect_equal(bmi_fun(NA, 50), tagged_na("b"))
  expect_equal(bmi_fun(1.7, NA), tagged_na("b"))
  expect_equal(bmi_fun(NA, NA), tagged_na("b"))
})

test_that("bmi_fun() generates a bmi when appropriate parameters are passed", {
  expect_equal(bmi_fun(3, 18), 2)
})

test_that("adjusted_bmi_fun() handles one or more NA arguments properly", {
  expect_equal(adjusted_bmi_fun(NA, 1, 1), tagged_na("b"))
  expect_equal(adjusted_bmi_fun(1, NA, 1), tagged_na("b"))
  expect_equal(adjusted_bmi_fun(1, 1, NA), tagged_na("b"))
  expect_equal(adjusted_bmi_fun(NA, NA, NA), tagged_na("b"))
})

test_that("adjusted_bmi_fun() generates a bmi when appropriate parameters
          are passed", {
  expect_equal(adjusted_bmi_fun(1, 1, 1), 0.00017)
})

test_that("bmi_fun_cat() handles an NA argument properly", {
  expect_equal(bmi_fun_cat(NA), "NA(b)")
})

test_that("bmi_fun_cat() generates a categorical bmi when an appropriate value
          is passed", {
  expect_equal(bmi_fun_cat(25), 3)
})
