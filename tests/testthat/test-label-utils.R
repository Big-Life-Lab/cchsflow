load(file.path('..', 'testdata','rec_with_table_test_data.RData'))
test_that("labels are properly applied to random variables", {
  variables$variable <- sapply(variables$variable, trimws)
  varNames <- as.character(unique(names(cchs2001Standard)))
  maxNumOfVars <- length(names(cchs2001Standard))
  listOfVarsToCheck <- sample(1:maxNumOfVars, floor(maxNumOfVars)/2, replace = TRUE)
  labeled2001 <- set_data_labels(cchs2001Standard, variable_details, variables)
  for (var_name_index in (listOfVarsToCheck)) {
    first <- as.character(get_label(labeled2001[[varNames[[var_name_index]]]]))
    second <- as.character(variables[variables$variable == varNames[[var_name_index]], 'label'])
    expect_equal(first,second)
  }
})
