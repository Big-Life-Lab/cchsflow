# multiple_conditions_fun1
test_that("multiple_conditions_fun1 has expected output when one or more values
           is out of range", {
  expect_equal(multiple_conditions_fun1(-1, 2, 2, 2, 1, 2), 1)
})

test_that("multiple_conditions_fun1 has expected output when all values
           are out of range", {
  expect_equal(
    multiple_conditions_fun1(-1, -2, -2, -2, -1, -2),
    0
  )
})

test_that("multiple_conditions_fun1 has expected output when all values
           are in range", {
  expect_equal(multiple_conditions_fun1(1, 2, 2, 2, 1, 2), 2)
})

test_that("multiple_conditions_fun1 returns '5+' when 5 or more conditions
           are present", {
  expect_equal(multiple_conditions_fun1(1, 1, 1, 1, 1, 1), "5+")
})

test_that("multiple_conditions_fun1 does not count resp_condition_der = 3
           (no respiratory condition)", {
  expect_equal(multiple_conditions_fun1(1, 2, 2, 2, 3, 2), 1)
})

test_that("multiple_conditions_fun1 counts resp_condition_der = 2
           (under 35 with respiratory condition)", {
  expect_equal(multiple_conditions_fun1(2, 2, 2, 2, 2, 2), 1)
})

test_that("multiple_conditions_fun1 treats NA inputs as 0 conditions", {
  expect_equal(multiple_conditions_fun1(NA, 2, 2, 2, 3, 2), 0)
})

# multiple_conditions_fun2
test_that("multiple_conditions_fun2 has expected output when one or more values
          is out of range", {
  expect_equal(multiple_conditions_fun2(-1, 2, 2, 2, 2, 1, 2), 1)
})

test_that("multiple_conditions_fun2 has expected output when all values
          are out of range", {
  expect_equal(
    multiple_conditions_fun2(-1, -2, -2, -2, -2, -1, -2),
    0
  )
})

test_that("multiple_conditions_fun2 has expected output when all values
          are in range", {
  expect_equal(multiple_conditions_fun2(1, 2, 2, 2, 2, 1, 2), 2)
})

test_that("multiple_conditions_fun2 returns '5+' when 5 or more conditions
          are present", {
  expect_equal(multiple_conditions_fun2(1, 1, 1, 1, 1, 1, 1), "5+")
})

test_that("multiple_conditions_fun2 does not count resp_condition_der = 3
          (no respiratory condition)", {
  expect_equal(multiple_conditions_fun2(1, 2, 2, 2, 2, 3, 2), 1)
})

test_that("multiple_conditions_fun2 counts resp_condition_der = 2
          (under 35 with respiratory condition)", {
  expect_equal(multiple_conditions_fun2(2, 2, 2, 2, 2, 2, 2), 1)
})

test_that("multiple_conditions_fun2 treats NA inputs as 0 conditions", {
  expect_equal(multiple_conditions_fun2(NA, 2, 2, 2, 2, 3, 2), 0)
})
