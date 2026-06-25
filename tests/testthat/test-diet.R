# calculate_diet_score
test_that("calculate_diet_score has expected output when all inputs are out of
          range",{
            expect_equal(calculate_diet_score(NA, NA, NA, NA, NA, NA, NA),
                         tagged_na("b"))
})

test_that("calculate_diet_score has expected output when all inputs are in
          range",{
            expect_equal(calculate_diet_score(1, 1, 1, 1, 1, 1, 1),
                         5)
          })

# categorize_diet_score
test_that("categorize_diet_score has expected output when input is out of
          range",{
            expect_true(is.na(categorize_diet_score(NA)))
          })

test_that("categorize_diet_score has expected output when input is in
          range",{
            expect_equal(categorize_diet_score(1),
                         1)
          })