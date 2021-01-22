# multiple_conditions_fun1
test_that("multiple_conditions_fun1 has expected output when one or more values
           is out of range", {
            expect_equal(multiple_conditions_fun1(-1, 2, 2, 2, 1, 2), 1)
          })

test_that("multiple_conditions_fun1 has expected output when all values
           are out of range", {
             expect_equal(multiple_conditions_fun1(-1, -2, -2, -2, -1, -2),
                          0)
           })

test_that("multiple_conditions_fun has expected output when all values
           are in range", {
             expect_equal(multiple_conditions_fun1(1, 2, 2, 2, 1, 2), 2)
           })

# multiple_conditions_fun2
test_that("multiple_conditions_fun2 has expected output when one or more values
          is out of range", {
            expect_equal(multiple_conditions_fun2(-1, 2, 2, 2, 2, 1, 2), 1)
            })

test_that("multiple_conditions_fun2 has expected output when all values
          are out of range", {
            expect_equal(multiple_conditions_fun2(-1, -2, -2, -2, -2, -1, -2),
                         0)
            })

test_that("multiple_conditions_fun2 has expected output when all values
          are in range", {
            expect_equal(multiple_conditions_fun2(1, 2, 2, 2, 2, 1, 2), 2)
          })
