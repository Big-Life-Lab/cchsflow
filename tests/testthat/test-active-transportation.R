test_that("active_transport1_fun() has expected outputs when
          all parameters are in range", {
            expect_equal(active_transport1_fun(3,4),
                         1)
          })

test_that("active_transport1_fun() has expected outputs when
          PAC_4A_cont is out of range", {
            expect_equal(active_transport1_fun(NA,7),
                         1)
          })

test_that("active_transport2_fun() has expected outputs when
          all parameters are in range", {
            expect_equal(active_transport2_fun(1,1,45,1,1,45),
                         1)
          })

test_that("active_transport2_fun() has expected outputs when
          PAC_7 is out of range", {
            expect_equal(active_transport2_fun(NA,NA,NA,1,1,90),
                         1)
          })

test_that("active_transport2_fun() has expected outputs when
          PAC_8 is out of range", {
            expect_equal(active_transport2_fun(1,1,90,NA,NA,NA),
                         1)
          })

test_that("active_transport2_fun() has expected outputs when
          PAC_7 and PAC_8 are out of range", {
            expect_equal(active_transport2_fun(NA,1,1,NA,1,1),
                         tagged_na("b"))
          })

test_that("active_transport3_fun() has expected outputs when
          all parameters are in range", {
            expect_equal(active_transport3_fun(3,4),
                         1)
          })

test_that("active_transport3_fun() has expected outputs when
          all parameters are in range", {
            expect_equal(active_transport3_fun(NA,7),
                         1)
          })