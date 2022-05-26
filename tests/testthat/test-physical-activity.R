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