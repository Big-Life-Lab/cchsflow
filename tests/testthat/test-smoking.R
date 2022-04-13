test_that("time_quit_smoking_fun() has expected outputs when
          SMK_09A_B is out of range", {
            expect_equal(time_quit_smoking_fun(10, 2),
                         tagged_na("b"))
          })

test_that("time_quit_smoking_fun() has expected outputs when
          all parameters in range", {
            expect_equal(time_quit_smoking_fun(2,2),
                         1.5)
          })


test_that("smoke_simple_fun() has expected outputs when
          SMKDSTY is out of range", {
            expect_equal(smoke_simple_fun(100, 2),
                         "NA(b)")
          })

test_that("smoke_simple_fun() has expected outputs when
          all parameters in range", {
            expect_equal(smoke_simple_fun(1,"NA(a)"),
                         1)
          })


test_that("pack_years_fun() has expected outputs when
          SMKDSTY is out of range", {
            expect_equal(pack_years_fun(10, 40, 6, 6, 22, 96, 12, 996, 996, 96, 96),
               tagged_na("b"))
          })

test_that("pack_years_fun() has expected outputs when
          DHHGAGE_cont is out of range", {
            expect_equal(pack_years_fun(1, -1, 6, 6, 22, 96, 12, 996, 996, 96, 96),
                         tagged_na("b"))
          })

test_that("pack_years_fun() has expected outputs when
          all parameters in range", {
            expect_equal(pack_years_fun(1, 40, 96, 16, 96, 10, 5, 996, 996, 96, 96),
                         12)
          })

test_that("pack_years_fun_cat() has expected outputs when
          pack_years_der is out of range", {
            expect_equal(pack_years_fun_cat(-1),
                         "NA(b)")
          })

test_that("pack_years_fun_cat() has expected outputs when
          pack_years_der is in range", {
            expect_equal(pack_years_fun_cat(1),
                         3)
          })

test_that("SMKG040_fun() has expected outputs when
          both inputs are out of range", {
            expect_equal(SMKG040_fun(NA, NA),
                         tagged_na("b"))
          })

test_that("SMKG040_fun() has expected outputs when
          both inputs are in range", {
            expect_equal(SMKG040_fun(1, 1),
                         1)
          })

test_that("SMKG207_fun() has expected outputs when
          DHHGAGE_cont is out of range", {
            expect_equal(pack_years_fun(1, -1, 6, 6, 22, 96, 12, 996, 996, 96, 96),
                         tagged_na("b"))
          })

test_that("SMKDSTY_fun() has expected outputs when
          both inputs are in range",{
            expect_equal(SMKDSTY_fun(2, 2, NA),
                         3)
          })

test_that("SMKDSTY_fun() has expected outputs when
          both inputs are in range",{
            expect_equal(SMKDSTY_fun(3, 2, 1),
                         5)
          })

test_that("SMKDSTY_fun() has expected outputs when
          both inputs are in range",{
            expect_equal(SMKDSTY_fun(NA, NA, NA),
                         tagged_na("b"))
          })

test_that("SMKG203_fun() has expected outputs when
          both inputs are in range",{
            expect_equal(SMKG203_fun(2, 1),
                         tagged_na("b"))
          })

test_that("SMKG203_fun() has expected outputs when
          both inputs are in range",{
            expect_equal(SMKG203_fun(1, 10),
                         47)
          })

test_that("SMKG207_fun() has expected outputs when
          both inputs are in range",{
            expect_equal(SMKG207_fun(2, 1),
                         tagged_na("b"))
          })

test_that("SMKG207_fun() has expected outputs when
          both inputs are in range",{
            expect_equal(SMKG207_fun(1, 10),
                         47)
          })
