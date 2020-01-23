load(file.path("..", "testdata", "rec_with_table_test_data.RData"))
test_that("rec_with_table matches the standard 2001 recode", {
  out <- suppressWarnings(
    cchsflow::rec_with_table(cchs2001,
                             note = FALSE))
  expect_mapequal(out,
    cchs2001Standard)
})

test_that("rec_with_table matches the standard 2003 recode", {
  out <- suppressWarnings(
    cchsflow::rec_with_table(cchs2003,
                             note = FALSE))
  expect_mapequal(out,
                    cchs2003Standard)
})

test_that("rec_with_table matches the standard 2005 recode", {
  out <- suppressWarnings(
    cchsflow::rec_with_table(cchs2005,
                             note = FALSE))
  expect_mapequal(out,
                    cchs2005Standard)
})

test_that("rec_with_table matches the standard 2014 recode", {
  out <- suppressWarnings(
    cchsflow::rec_with_table(cchs2014,
                             note = FALSE))
  expect_mapequal(out,
                    cchs2014Standard)
})
