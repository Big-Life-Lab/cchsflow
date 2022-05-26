test_that("SPS_5_fun() has expected outputs when
          all parameters in range", {
            expect_equal(SPS_5_fun(1,1,1,1,1),
                         5)
          })

test_that("SPS_5_fun() has expected outputs when
          all parameters in range", {
            expect_equal(SPS_5_fun(1,1,1,1,25),
                         20)
          })

test_that("SPS_5_fun() has expected outputs when
          SPS5_der is out of range", {
            expect_equal(SPS_5_fun(1,1,1,1,NA),
                         tagged_na("b"))
          })
