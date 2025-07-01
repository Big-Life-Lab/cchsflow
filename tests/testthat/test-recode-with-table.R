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

test_that("rec_with_table works across all major CCHS cycles", {
  # Test additional major cycles to ensure rec_with_table() functionality
  additional_cycles <- c("cchs2007_2008_p", "cchs2009_2010_p", "cchs2011_2012_p", 
                         "cchs2013_2014_p", "cchs2017_2018_p")
  
  # Core derived variables that should be available across most cycles
  core_variables <- c("BMI_der", "energy_exp")
  
  for (cycle_name in additional_cycles) {
    cycle_data <- get(cycle_name)
    
    # Test that rec_with_table() completes successfully with core variables
    expect_no_error({
      result <- suppressWarnings(
        cchsflow::rec_with_table(cycle_data, 
                                variables = core_variables,
                                database_name = cycle_name,
                                variable_details = variable_details,
                                note = FALSE)
      )
    }, info = paste("rec_with_table failed for cycle:", cycle_name))
    
    # Verify result structure
    result <- suppressWarnings(
      cchsflow::rec_with_table(cycle_data, 
                              variables = core_variables,
                              database_name = cycle_name,
                              variable_details = variable_details,
                              note = FALSE)
    )
    
    expect_true(is.data.frame(result), 
                info = paste("Result not a data.frame for cycle:", cycle_name))
    expect_true(nrow(result) > 0, 
                info = paste("Empty result for cycle:", cycle_name))
  }
})