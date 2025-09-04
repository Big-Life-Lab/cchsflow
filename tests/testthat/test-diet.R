# diet_score_fun
test_that("diet_score_fun has expected output when all inputs are out of
          range", {
  expect_equal(
    diet_score_fun(NA, NA, NA, NA, NA, NA, NA),
    tagged_na("b")
  )
})

test_that("diet_score_fun has expected output when all inputs are in
          range", {
  expect_equal(
    diet_score_fun(1, 1, 1, 1, 1, 1, 1),
    5
  )
})

# diet_score_fun_cat
test_that("diet_score_fun_cat has expected output when input is out of
          range", {
  expect_equal(
    diet_score_fun_cat(NA),
    "NA(b)"
  )
})

test_that("diet_score_fun_cat has expected output when input is in
          range", {
  expect_equal(
    diet_score_fun_cat(1),
    1
  )
})
