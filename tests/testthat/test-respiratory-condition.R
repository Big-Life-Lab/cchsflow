# resp_condition_fun1-------------------------------

test_that("resp_condition_fun1 has expected output when age is out of range", {
            expect_equal(resp_condition_fun1(-1, 1), NA)
          })

test_that("resp_condition_fun1 has expected output when
          COPD/Emphs is out of range", {
            expect_equal(resp_condition_fun1(2, 0), NA)
          })

test_that("resp_condition_fun1 has expected output when age is NA", {
  expect_equal(resp_condition_fun1(NA, 1), NA)
})

test_that("resp_condition_fun1 has expected output when COPD/Emphs is NA", {
  expect_equal(resp_condition_fun1(2, NA), NA)
})

test_that("resp_condition_fun1 has expected output when all arguments are NA", {
            expect_equal(resp_condition_fun1(NA, NA), NA)
          })

test_that("resp_condition_fun1 has expected output when
          all arguments are in range", {
            expect_equal(resp_condition_fun1(40, 1), 1)
          })

# resp_condition_fun2-----------------------

test_that("resp_condition_fun2 has expected output when age is out of range", {
            expect_equal(resp_condition_fun2(-1, 1, 1, 1), NA)
          })

test_that("resp_condition_fun2 has expected output when
          emphysema is out of range", {
            expect_warning(out <- resp_condition_fun2(40, 3, 1, 1),
                           regexp =
                             "In DHHGAGE_cont:40,
                           CCC_91E:3,
                           CCC_91F:1,
                           CCC_91A:1 one or more of the respiratory arguments
                           was outside the 1:2 allowed range however
                           the condition is still calculated")
            expect_equal(out, 1)
          })

test_that("resp_condition_fun2 has expected output when COPD is out of range", {
            expect_warning(out <- resp_condition_fun2(40, 1, 3, 1),
                           regexp =
                             "In DHHGAGE_cont:40,
                           CCC_91E:1,
                           CCC_91F:3,
                           CCC_91A:1 one or more of the respiratory arguments
                           was outside the 1:2 allowed range however
                           the condition is still calculated")
            expect_equal(out, 1)
          })

test_that("resp_condition_fun2 has expected output when
          chronic bronchitis is out of range", {
            expect_warning(out <- resp_condition_fun2(40, 1, 1, 3),
                           regexp =
                             "In DHHGAGE_cont:40,
                           CCC_91E:1,
                           CCC_91F:1,
                           CCC_91A:3 one or more of the respiratory arguments
                           was outside the 1:2 allowed range however
                           the condition is still calculated")
            expect_equal(out, 1)
          })

test_that("resp_condition_fun2 has expected output when
          all parameters are in range", {
            expect_equal(resp_condition_fun2(40, 1, 1, 1), 1)
          })

test_that("resp_condition_fun2 has expected output when
          COPD and bronchitis are negative but emphysema is positive", {
            expect_equal(resp_condition_fun2(40, 1, 2, 2), 1)
          })

test_that("resp_condition_fun2 has expected output when
          emphysema and bronchitis are negative but COPD is positive", {
            expect_equal(resp_condition_fun2(40, 2, 1, 2), 1)
          })

test_that("resp_condition_fun2 has expected output when
          emphysema and COPD are negative but bronchitis is positive", {
            expect_equal(resp_condition_fun2(40, 2, 2, 1), 1)
          })

test_that("resp_condition_fun2 has expected output when
          all parameters are negative", {
            expect_equal(resp_condition_fun2(40, 2, 2, 2), 3)
          })

test_that("resp_condition_fun2 has expected output when
          age is less than 35 and condition positive", {
            expect_equal(resp_condition_fun2(34, 1, 2, 2), 2)
          })

# resp_condition_fun3--------------------------------

test_that("resp_condition_fun3 has expected output when
          age is out of range", {
            expect_equal(resp_condition_fun3(-1, 1, 1), NA)
          })

test_that("resp_condition_fun3 has expected output when
          COPD/Emphys is out of range", {
            expect_warning(out <- resp_condition_fun3(35, 4, 1),
                           regexp =
                             "In DHHGAGE_cont:35,
                           CCC_091:4,
                           CCC_91A:1 one or more of the respiratory arguments
                           was outside the 1:2 allowed range however
                           the condition is still calculated")
            expect_equal(out, 1)
          })

test_that("resp_condition_fun3 has expected output when
          bronchitis is out of range", {
            expect_warning(out <- resp_condition_fun3(35, 1, 4),
                           regexp =
                             "In DHHGAGE_cont:35,
                           CCC_091:1,
                           CCC_91A:4 one or more of the respiratory arguments
                           was outside the 1:2 allowed range however
                           the condition is still calculated")
            expect_equal(out, 1)
          })

test_that("resp_condition_fun3 has expected output when
          all parameters are in range", {
            expect_equal(resp_condition_fun3(35, 1, 1), 1)
          })

test_that("resp_condition_fun3 has expected output when
          COPD/Emphys is negative but bronchitis is positive", {
            expect_equal(resp_condition_fun3(40, 2, 1), 1)
          })

test_that("resp_condition_fun3 has expected output when
          bronchitis is negative but COPD/Emphys is positive", {
            expect_equal(resp_condition_fun3(40, 1, 2), 1)
          })

test_that("resp_condition_fun3 has expected output when
          all parameters are negative", {
            expect_equal(resp_condition_fun3(40, 2, 2), 3)
          })

test_that("resp_condition_fun3 has expected output when
          age is less than 35 and condition positive", {
            expect_equal(resp_condition_fun3(34, 1, 2), 2)
          })
