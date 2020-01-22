# food_insecurity_fun1-------------------------------

test_that("food_insecurity_fun1 has expected output when FINF1 is out of
          range", {
            expect_equal(food_insecurity_fun1(4), NA)
          })

test_that("food_insecurity_fun1 has expected output when FINF1 is in
          range", {
            expect_equal(food_insecurity_fun1(1), 2)
          })

# food_insecurity_fun2-------------------------------

test_that("food_insecurity_fun2 has expected output when FSCDHFS is out of
          range", {
            expect_equal(food_insecurity_fun2(4), NA)
            })

test_that("food_insecurity_fun2 has expected output when FSCDHFS is in
          range", {
            expect_equal(food_insecurity_fun2(1), 2)
            })

# food_insecurity_fun3-------------------------------

test_that("food_insecurity_fun3 has expected output when FSCDHFS2 is out of
          range", {
            expect_equal(food_insecurity_fun3(4), NA)
            })

test_that("food_insecurity_fun3 has expected output when FSCDHFS2 is in
          range", {
            expect_equal(food_insecurity_fun3(1), 2)
            })