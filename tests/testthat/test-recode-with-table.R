load(file.path("..", "testdata", "rec_with_table_test_data.RData"))
test_that("rec_with_table matches the standard 2001 recode", {
  out <- suppressWarnings(
    cchsflow::rec_with_table(cchs2001,
                             print_note = FALSE))
  print(out[1:88,c(1,2,3,4,5,6,7,8,9)])
  print(cchs2001Standard[1:88,c(1,2,3,4,5,6,7,8,9)])
  expect_equivalent(out,
    cchs2001Standard)
})

test_that("rec_with_table matches the standard 2003 recode", {
  out <- suppressWarnings(
    cchsflow::rec_with_table(cchs2003,
                             print_note = FALSE))
  expect_equivalent(out,
                    cchs2003Standard)
})

test_that("rec_with_table matches the standard 2005 recode", {
  out <- suppressWarnings(
    cchsflow::rec_with_table(cchs2005,
                             print_note = FALSE))
  expect_equivalent(out,
                    cchs2005Standard)
})

test_that("rec_with_table matches the standard 2014 recode", {
  out <- suppressWarnings(
    cchsflow::rec_with_table(cchs2014,
                             print_note = FALSE))
  expect_equivalent(out,
                    cchs2014Standard)
})
