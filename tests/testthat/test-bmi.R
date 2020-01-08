test_that("bmi_fun() handles one or more NA arguments properly", {
  expect_equal(bmi_fun(NA, 50), NA)
  expect_equal(bmi_fun(1.7, NA), NA)
  expect_equal(bmi_fun(NA, NA), NA)
})

test_that("bmi_fun() generates a bmi when appropriate parameters are passed", {
  expect_equal(bmi_fun(3, 18), 2)
})
