test_that("pack_years_fun() has expected outputs when
          SMKDSTY is out of range", {
  expect_equal(pack_years_fun(10, 40, 6, 6, 22, 96, 12, 996, 996, 96, 96, 1),
               NA)
})

test_that("pack_years_fun() has expected outputs when
          DHHGAGE_cont is out of range", {
  expect_equal(pack_years_fun(1, -1, 6, 6, 22, 96, 12, 996, 996, 96, 96, 1),
               NA)
})

test_that("pack_years_fun() has expected outputs when
          SMKG203_cont is out of range", {
  expect_equal(pack_years_fun(1, 40, 6, 6, 100, 96, 12, 996, 996, 96, 96, 1),
               0.0137)
})

test_that("pack_years_fun() has expected outputs when
          all parameters in range", {
  expect_equal(pack_years_fun(1, 40, 6, 6, 22, 96, 12, 996, 996, 96, 96, 1),
               10.8)
})
