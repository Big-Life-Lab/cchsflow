test_that("age_cat_fun performs as expected below lower bound", {
  expect_equal(age_cat_fun(11), NA)
})

test_that("age_cat_fun performs as expected within bounds", {
  expect_equal(age_cat_fun(33), 6)
})

test_that("age_cat_fun performs as expected above upper bound", {
  expect_equal(age_cat_fun(96), 16)
})