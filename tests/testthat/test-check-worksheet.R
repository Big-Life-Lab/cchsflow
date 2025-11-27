#' Create a CSV file to use in a test
#'
#' @param content a vector containing the content of the file. Each item in the
#' vector should be a row in the CSV file. For example, the first item is the
#' first row, the second item is the second row etc.
#' @param envir DO NOT SET THIS. This is used to properly clean up the created
#' file.
#'
#' @return the path to the created file
#' @examples
#' \dontrun {
#' csv_file_content <- c(
#'   "col_1,col2\n",
#'   "1,2\n"
#' )
#' csv_file_path <- create_test_csv(csv_file_content)
#' }
create_test_csv <- function(content, envir = parent.frame()) {
  test_csv_path <- withr::local_tempfile(fileext = ".csv", .local_envir = envir)
  test_csv_content <- paste(content, collapse = "")
  cat(test_csv_content, file = test_csv_path)
  return(test_csv_path)
}

test_that("Should return an error when the worksheet is not found", {
  non_existent_file_path <- "non-existent.csv"

  expected_result <- list(
    .create_file_not_found_error("variables", non_existent_file_path))

  actual_result <- check_worksheet(non_existent_file_path, "variables")

  expect_equal(actual_result, expected_result)
})

test_that("Should return error(s) when the worksheet has incorrect line
	  endings", {
  variables_sheet_content <- c(
    paste0(
      "variable,label,labelLong,variableType,databaseStart,variableStart,",
      "subject,section,units,notes,description,version,lastUpdated,reviewNotes,",
      "ICES.confirmation,Observation..MD.,status,versionNotes\r\n"
    ),
    paste0(
      "BMI,Body Mass Index,Body Mass Index,Continuous,cchs2013_p,[HWTGBMI],",
      "BMI,Health status,kg/m2,,,1.0.0,2024-01-01,,Yes,,active,\r\n"
    ),
    paste0(
      "Sex,Sex,Sex,Categorical,cchs2013_p,[DHH_SEX],Sex,Demographics,N/A,,,",
      "1.0.0,,2024-01-01,,Yes,,status,\n"
    )
  )
  variables_sheet_path <- create_test_csv(variables_sheet_content)

  expected_result <- list(
    .create_line_ending_crlf_error("variables", variables_sheet_path, 1),
    .create_line_ending_crlf_error("variables", variables_sheet_path, 2)
  )

  actual_result <- check_worksheet(variables_sheet_path, "variables")

  expect_equal(actual_result, expected_result)
})

test_that("Should return error(s) when the worksheet has excessive quoting", {
  header_line <- paste0(
    'variable,label,labelLong,variableType,"databaseStart",',
    '"variableStart",subject,section,units,notes,description,version,lastUpdated,',
    "reviewNotes,ICES.confirmation,Observation..MD.,status,versionNotes\n"
  )
  second_line <- paste0(
    '"BMI","\nBody Mass Index","\r\nBody Mass Index","""Continuous",',
    '",cchs2013_p",[HWTGBMI],BMI,Health status,kg/m2,,,1.0.0,2024-01-01,,Yes,,',
    "active,\n"
  )
  variables_sheet_content <- c(header_line, second_line)
  variables_sheet_path <- create_test_csv(variables_sheet_content)

  expected_result <- list(
    .create_excessive_quoting_error(
      "variables", variables_sheet_path, 1, 5, '"databaseStart"'),
    .create_excessive_quoting_error(
      "variables", variables_sheet_path, 1, 6, '"variableStart"'),
    .create_excessive_quoting_error(
      "variables", variables_sheet_path, 2, 1, '"BMI"')
  )

  actual_result <- check_worksheet(variables_sheet_path, "variables")

  expect_equal(actual_result, expected_result)
})

test_that("Should return error(s) when the columns are not in the right order in
	  the variables sheet", {
  variables_sheet_content <- c(
    paste0(
      "label,variable,labelLong,variableType,databaseStart,variableStart,",
      "subject,section,units,notes,description,version,lastUpdated,reviewNotes,",
      "ICES.confirmation,Observation..MD.,status,versionNotes\n"
    ),
    paste0(
      "Body Mass Index,BMI,Body Mass Index,Continuous,cchs2013_p,[HWTGBMI],",
      "BMI,Health status,kg/m2,,,1.0.0,2024-01-01,,Yes,,active,\n"
    )
  )
  variables_sheet_path <- create_test_csv(variables_sheet_content)

  expected_result <- list(
    .create_column_order_error(
      "variables", variables_sheet_path, "variable", 1, "label"),
    .create_column_order_error(
      "variables", variables_sheet_path, "label", 2, "variable")
  )

  actual_result <- check_worksheet(variables_sheet_path, "variables")

  expect_equal(actual_result, expected_result)
})

test_that("Should return errors when the columns are not in the right
	  order in the variable details sheet", {
  variable_details_sheet_content <- c(
    paste0(
      "dummyVariable,variable,typeEnd,databaseStart,variableStart,",
      "ICES.confirmation,typeStart,recEnd,numValidCat,catLabel,catLabelLong,",
      "units,recStart,catStartLabel,variableStartShortLabel,",
      "variableStartLabel,notes,version,lastUpdated,status,reviewNotes,",
      "versionNotes,review\n"
    ),
    paste0(
      "N/A,BMI,cont,cchs2013_p,[HWTGBMI],,cont,copy,N/A,N/A,N/A,kg/m2,else,",
      "N/A,BMI,BMI,,1.0.0,2024-01-01,active,,,\n"
    )
  )
  variable_details_file_path <- create_test_csv(variable_details_sheet_content)

  expected_errors <- list(
    .create_column_order_error(
      "variable_details", variable_details_file_path, "variable", 1, "dummyVariable"),
    .create_column_order_error(
      "variable_details", variable_details_file_path, "dummyVariable", 2, "variable")
  )

  actual_errors <- check_worksheet(
    variable_details_file_path, "variable_details")

  expect_equal(actual_errors, expected_errors)
})

test_that("Should return column order errors when the worksheet has fewer
	  columns than expected", {
  # Create a variables sheet with 16 columns (missing last 2: status, versionNotes)
  variables_sheet_content <- c(
    paste0(
      "variable,label,labelLong,variableType,databaseStart,variableStart,",
      "subject,section,units,notes,description,version,lastUpdated,reviewNotes,",
      "ICES.confirmation,Observation..MD.\n"
    ),
    paste0(
      "BMI,Body Mass Index,Body Mass Index,Continuous,cchs2013_p,[HWTGBMI],",
      "BMI,Health status,kg/m2,,,1.0.0,2024-01-01,,Yes,\n"
    )
  )
  variables_sheet_path <- create_test_csv(variables_sheet_content)

  # Should get errors for columns 17-18 (positions where actual is NA)
  expected_errors <- list(
    .create_column_order_error(
      "variables", variables_sheet_path, "status", 17, NA_character_),
    .create_column_order_error(
      "variables", variables_sheet_path, "versionNotes", 18, NA_character_)
  )

  actual_errors <- check_worksheet(variables_sheet_path, "variables")

  expect_equal(actual_errors, expected_errors)
})

test_that("Should return the right error when the variables sheet has unsorted
	  rows", {
  variables_sheet_content <- c(
    paste0(
      "variable,label,labelLong,variableType,databaseStart,variableStart,",
      "subject,section,units,notes,description,version,lastUpdated,reviewNotes,",
      "ICES.confirmation,Observation..MD.,status,versionNotes\n"
    ),
    paste0(
      "Weight,Weight,Weight,Continuous,cchs2013_p,[HWTGKG],Weight,",
      "Health status,kg,,,1.0.0,2024-01-01,,Yes,,active,\n",
      "BMI,Body Mass Index,Body Mass Index,Continuous,cchs2013_p,[HWTGBMI],",
      "BMI,Health status,kg/m2,,,1.0.0,2024-01-01,,Yes,,active,\n",
      "Height,Height,Height,Continuous,cchs2013_p,[HWTGM],Height,",
      "Health status,metres,,,1.0.0,2024-01-01,,Yes,,active,\n"
    )
  )
  variables_sheet_path <- create_test_csv(variables_sheet_content)

  expected_result <- list(.create_unsorted_rows_error(
    "variables", variables_sheet_path, "variable"))

  actual_result <- check_worksheet(variables_sheet_path, "variables")

  expect_equal(actual_result, expected_result)
})

test_that("Should return an error when the variables sheet is missing the ID
	  column", {
  # Create a variables sheet with the ID column renamed (from "variable" to
  # "var_name")
  variables_sheet_content <- c(
    paste0(
      "var_name,label,labelLong,variableType,databaseStart,variableStart,",
      "subject,section,units,notes,description,version,lastUpdated,reviewNotes,",
      "ICES.confirmation,Observation..MD.,status,versionNotes\n"
    ),
    paste0(
      "BMI,Body Mass Index,Body Mass Index,Continuous,cchs2013_p,[HWTGBMI],",
      "BMI,Health status,kg/m2,,,1.0.0,2024-01-01,,Yes,,active,\n"
    )
  )
  variables_sheet_path <- create_test_csv(variables_sheet_content)

  expected_result <- list(
    .create_column_order_error(
      "variables", variables_sheet_path, "variable", 1, "var_name"),
    .create_missing_id_column_error(
      "variables", variables_sheet_path, "variable")
  )

  actual_result <- check_worksheet(variables_sheet_path, "variables")

  expect_equal(actual_result, expected_result)
})

test_that("Should not return errors when the variables details sheet rows are
	  unsorted", {
  variable_details_sheet_content <- c(
    paste0(
      "variable,dummyVariable,typeEnd,databaseStart,variableStart,",
      "ICES.confirmation,typeStart,recEnd,numValidCat,catLabel,catLabelLong,",
      "units,recStart,catStartLabel,variableStartShortLabel,",
      "variableStartLabel,notes,version,lastUpdated,status,reviewNotes,",
      "versionNotes,review\n"
    ),
    paste0(
      "Weight,N/A,cont,cchs2013_p,[HWTGKG],,cont,copy,N/A,N/A,N/A,kg,else,N/A,",
      "Weight,Weight,,1.0.0,2024-01-01,active,,,\n"
    ),
    paste0(
      "BMI,N/A,cont,cchs2013_p,[HWTGBMI],,cont,copy,N/A,N/A,N/A,kg/m2,else,",
      "N/A,BMI,BMI,,1.0.0,2024-01-01,active,,,\n"
    )
  )
  variable_details_sheet_file_path <- create_test_csv(
    variable_details_sheet_content)

  expected_result <- list()

  actual_result <- check_worksheet(
    variable_details_sheet_file_path,
    "variable_details"
  )

  expect_equal(actual_result, expected_result)
})

test_that("Should return error(s) when there are empty columns in a
	  worksheet", {
  variables_sheet_content <- c(
    paste0(
      "variable,label,labelLong,variableType,databaseStart,variableStart,",
      "subject,section,units,notes,description,version,lastUpdated,reviewNotes,",
      "ICES.confirmation,Observation..MD.,status,versionNotes,,\n"
    ),
    paste0(
      "BMI,Body Mass Index,Body Mass Index,Continuous,cchs2013_p,[HWTGBMI],",
      "BMI,Health status,kg/m2,,BMI,1.0.0,2024-01-01,,Yes,,active,,,\n"
    )
  )
  variables_sheet_path <- create_test_csv(variables_sheet_content)

  expected_result <- list(
    .create_trailing_empty_columns_error("variables", variables_sheet_path, 19),
    .create_trailing_empty_columns_error("variables", variables_sheet_path, 20)
  )

  actual_result <- check_worksheet(variables_sheet_path, "variables")

  expect_equal(actual_result, expected_result)
})

test_that("Should return no errors for a correctly formatted variables sheet", {
  variables_sheet_content <- c(
    paste0(
      "variable,label,labelLong,variableType,databaseStart,variableStart,",
      "subject,section,units,notes,description,version,lastUpdated,reviewNotes,",
      "ICES.confirmation,Observation..MD.,status,versionNotes\n"
    ),
    paste0(
      "BMI,Body Mass Index,Body Mass Index,Continuous,cchs2013_p,[HWTGBMI],",
      "BMI,Health status,kg/m2,,BMI,1.0.0,2024-01-01,,Yes,,active,\n"
    ),
    paste0(
      "Height,Height,Height,Continuous,cchs2013_p,[HWTGM],Height,",
      "Health status,meters,,,1.0.0,2024-01-01,,Yes,,active,\n"
    )
  )
  variables_sheet_path <- create_test_csv(variables_sheet_content)

  expected_result <- list()

  actual_result <- check_worksheet(variables_sheet_path, "variables")

  expect_equal(actual_result, expected_result)
})

test_that("Should return no errors for a correctly formatted variable details
	  sheet", {
  variable_details_sheet_content <- c(
    paste0(
      "variable,dummyVariable,typeEnd,databaseStart,variableStart,",
      "ICES.confirmation,typeStart,recEnd,numValidCat,catLabel,catLabelLong,",
      "units,recStart,catStartLabel,variableStartShortLabel,",
      "variableStartLabel,notes,version,lastUpdated,status,reviewNotes,",
      "versionNotes,review\n"
    ),
    paste0(
      "BMI,N/A,cont,cchs2013_p,[HWTGBMI],Yes,cont,copy,N/A,N/A,N/A,kg/m2,else,",
      "N/A,BMI,Body Mass Index,,1.0.0,2024-01-01,active,,,approved\n"
    )
  )
  variable_details_sheet_path <- create_test_csv(variable_details_sheet_content)

  expected_result <- list()

  actual_result <- check_worksheet(
    variable_details_sheet_path, "variable_details")

  expect_equal(actual_result, expected_result)
})
