test_that("CCC_091_fun1 returns 1 when either input is 1", {
  expect_equal(CCC_091_fun1(1, 2), 1)
})

test_that("CCC_091_fun1 returns 2 when both inputs are 2", {
  expect_equal(CCC_091_fun1(2, 2), 2)
})

test_that("CCC_091_fun2 returns 1 when any input is 1", {
  expect_equal(CCC_091_fun2(1, 2, 2), 1)
})

test_that("CCC_091_fun2 returns 2 when all inputs are 2", {
  expect_equal(CCC_091_fun2(2, 2, 2), 2)
})
