load(file.path("..", "testdata", "rec_with_table_test_data.RData"))
test_that("rec_with_table matches the standard 2001 recode", {
  out <- suppressWarnings(
    cchsflow::rec_with_table(cchs2001_p, 
                             variables = variables$variable, 
                             variable_details = variable_details,
                             note = FALSE))
  expect_mapequal(out,
    cchs2001Standard)
})

test_that("rec_with_table matches the standard 2003 recode", {
  out <- suppressWarnings(
    cchsflow::rec_with_table(cchs2003_p, 
                             variables = variables$variable, 
                             variable_details = variable_details,
                             note = FALSE))
  expect_mapequal(out,
                    cchs2003Standard)
})

test_that("rec_with_table matches the standard 2005 recode", {
  out <- suppressWarnings(
    cchsflow::rec_with_table(cchs2005_p, 
                             variables = variables$variable, 
                             variable_details = variable_details,
                             note = FALSE))
  expect_mapequal(out,
                    cchs2005Standard)
})

test_that("rec_with_table matches the standard 2015 recode", {
  out <- suppressWarnings(
    cchsflow::rec_with_table(cchs2015_2016_p, 
                             variables = variables$variable, 
                             variable_details = variable_details,
                             note = FALSE))
  expect_mapequal(out,
                  cchs2015Standard)
})