test_that("energy_exp_fun has expected outputs when all inputs are out of
          range", {
        expect_equal(energy_exp_fun(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                     tagged_na("b"))
          })

test_that("energy_exp_fun has expected outputs when all inputs are in
          range", {
        expect_equal(energy_exp_fun(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                     0.0214285714)
          })

test_that("energy_exp_fun_cat returns 1 for inactive", {
  expect_equal(energy_exp_fun_cat(0.5), 1)
})

test_that("energy_exp_fun_cat returns 2 for moderate", {
  expect_equal(energy_exp_fun_cat(2.0), 2)
})

test_that("energy_exp_fun_cat returns 3 for active", {
  expect_equal(energy_exp_fun_cat(4.0), 3)
})
