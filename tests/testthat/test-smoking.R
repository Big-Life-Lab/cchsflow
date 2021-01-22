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
