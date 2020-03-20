test_that("RACDPAL_fun has expected output when RAC_1 is out of range",{
  expect_equal(RACDPAL_fun(-1, 1, 1, 1), NA)
})

test_that("RACDPAL_fun has expected output when RAC_2A is out of range",{
  expect_equal(RACDPAL_fun(1, -1, 1, 1), NA)
})

test_that("RACDPAL_fun has expected output when RAC_2B is out of range",{
  expect_equal(RACDPAL_fun(1, 1, -1, 1), NA)
})

test_that("RACDPAL_fun has expected output when RAC_2C is out of range",{
  expect_equal(RACDPAL_fun(1, 1, 1, -1), NA)
})

test_that("RACDPAL_fun has expected output when all values are in range",{
  expect_equal(RACDPAL_fun(1, 1, 1, 1), 1)
})