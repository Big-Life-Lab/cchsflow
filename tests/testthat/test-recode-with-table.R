load(file.path("..", "testdata", "rec_with_table_test_data.RData"))
test_that("rec_with_table matches the standard 2001 recode", {
  expect_equal(suppressWarnings(
    rec_with_table(cchs2001,
                   print_note = FALSE)),
    cchs2001Standard)
})

test_that("rec_with_table matches the standard 2003 recode", {
  expect_equal(suppressWarnings(
    rec_with_table(cchs2003,
                   print_note = FALSE)),
    cchs2003Standard)
})

test_that("rec_with_table matches the standard 2005 recode", {
  expect_equal(suppressWarnings(
    rec_with_table(cchs2005,
                   print_note = FALSE)),
    cchs2005Standard)
})

test_that("rec_with_table matches the standard 20014 recode", {
  expect_equal(suppressWarnings(
    rec_with_table(cchs2014,
                   print_note = FALSE)),
    cchs2014Standard)
})
