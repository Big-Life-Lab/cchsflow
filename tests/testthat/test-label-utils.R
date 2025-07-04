load(file.path("..", "testdata", "rec_with_table_test_data.RData"))
test_that("labels are properly applied to random variables", {
  variables$variable <- sapply(variables$variable, trimws)
  var_names <- as.character(unique(names(cchs2001Standard)))
  max_num_of_vars <- length(names(cchs2001Standard))
  list_of_vars_to_check <- sample(1:max_num_of_vars,
    floor(max_num_of_vars) / 2,
    replace = TRUE
  )
  labeled2001 <- set_data_labels(
    cchs2001Standard,
    variable_details,
    variables
  )
  for (var_name_index in (list_of_vars_to_check)) {
    first <- as.character(get_label(labeled2001[[var_names[[var_name_index]]]]))
    second <- as.character(variables[
      variables$variable == var_names[[var_name_index]], "label"
    ])
    expect_equal(first, second)
  }
})
