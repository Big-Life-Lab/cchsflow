test_that("diet_score_fun has expected output when all inputs are out of
            range", {
            expect_equal(diet_score_fun(NA, NA, NA, NA, NA, NA, NA),
                         tagged_na("b"))
          })

test_that("diet_score_fun has expected output when all inputs are in
            range", {
            expect_equal(diet_score_fun(1, 1, 1, 1, 1, 1, 1),
                         4)
          })

test_that("diet_score_fun_cat returns 1 for poor diet", {
  expect_equal(diet_score_fun_cat(1), 1)
})

test_that("diet_score_fun_cat returns 3 for adequate diet", {
  expect_equal(diet_score_fun_cat(9), 3)
})
