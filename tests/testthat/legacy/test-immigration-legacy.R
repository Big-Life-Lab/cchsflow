test_that("immigration_fun returns 1 for White Canada-born", {
  expect_equal(immigration_fun(2, 1, 1, 4.5), 1L)
})

test_that("immigration_fun returns 4 for VM immigrant recent", {
  expect_equal(immigration_fun(1, 2, 2, 4.5), 4L)
})

test_that("immigration_fun returns 7 for White non-immigrant born outside Canada", {
  expect_equal(immigration_fun(2, 2, 1, tagged_na("a")), 7L)
})
