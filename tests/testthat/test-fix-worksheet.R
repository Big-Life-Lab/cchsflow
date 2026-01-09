#' Assert that a worksheet has no errors as reported by the check_worksheet
#' function
#'
#' @param worksheet_path
#' @param worksheet_type One of variables or variable_details
expect_no_errors <- function(worksheet_path, worksheet_type) {
  actual_result <- check_worksheet(worksheet_path, worksheet_type)
  expected_result <- list()
  expect_equal(actual_result, expected_result)
}

test_that("Should return an empty list when there are no errors in the
          worksheet", {
  variables_sheet_content <- c(
    paste0(
      "variable,label,labelLong,variableType,databaseStart,variableStart,",
      "subject,section,units,notes,description,version,lastUpdated,reviewNotes,",
      "ICES.confirmation,Observation..MD.,status,versionNotes\n"
    ),
    paste0(
      "BMI,Body Mass Index,Body Mass Index,Continuous,cchs,[HWTGBMI],",
      "Health status,BMI,kg/m2,,,1.0.0,2024-01-01,,,,active,\n"
    )
  )
  variables_sheet_path <- create_test_csv(variables_sheet_content)

  actual_result <- fix_worksheet(variables_sheet_path, "variables")

  expect_equal(actual_result, list())
})

test_that("Should return the right error object for a file not found error", {
  file_path <- "nonexistent.csv"

  expected_error <- .create_file_not_found_error("variables", file_path)
  expected_error$fixed <- FALSE
  expected_result <- list(expected_error)

  actual_result <- fix_worksheet(file_path, "variables")

  expect_equal(actual_result, expected_result)
})

test_that("Should not fix errors when the worksheet is missing the ID column", {
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

  column_order_error <- .create_column_order_error(
    "variables", variables_sheet_path, "variable", 1, "var_name")
  column_order_error$fixed <- FALSE
  missing_id_error <- .create_missing_id_column_error(
    "variables", variables_sheet_path, "variable")
  missing_id_error$fixed <- FALSE
  expected_result <- list(column_order_error, missing_id_error)

  actual_result <- fix_worksheet(variables_sheet_path, "variables")

  expect_equal(actual_result, expected_result)
})

test_that("Should fix incorrect line endings", {
  variables_sheet_content <- c(
    paste0(
      "variable,label,labelLong,variableType,databaseStart,variableStart,",
      "subject,section,units,notes,description,version,lastUpdated,reviewNotes,",
      "ICES.confirmation,Observation..MD.,status,versionNotes\r\n"
    ),
    paste0(
      "BMI,Body Mass Index,Body Mass Index,Continuous,cchs,[HWTGBMI],",
      "Health status,BMI,kg/m2,,,1.0.0,2024-01-01,,Yes,,active,\r\n"
    ),
    paste0(
      "Sex,Sex,Sex,Categorical,cchs,[DHH_SEX],Sex,Demographics,N/A,,,",
      "1.0.0,2024-01-01,,Yes,,active,\n"
    )
  )
  test_file_path <- create_test_csv(variables_sheet_content)

  line_ending_errors <- list(
    .create_line_ending_crlf_error("variables", test_file_path, 1),
    .create_line_ending_crlf_error("variables", test_file_path, 2)
  )
  expected_result <- lapply(line_ending_errors, function(err) {
    err$fixed <- TRUE
    return(err)
  })

  actual_result <- fix_worksheet(test_file_path, "variables")

  expect_equal(actual_result, expected_result)

  expect_no_errors(test_file_path, "variables")
})

test_that("Should fix excessive quoting", {
  header_line <- paste0(
    'variable,label,labelLong,variableType,"databaseStart",',
    '"variableStart",subject,section,units,notes,description,version,lastUpdated,',
    "reviewNotes,ICES.confirmation,Observation..MD.,status,versionNotes\n"
  )
  second_line <- paste0(
    '"BMI","\nBody Mass Index","\r\nBody Mass Index","""Continuous",',
    '",cchs",[HWTGBMI],Health status,BMI,kg/m2,,,1.0.0,2024-01-01,,Yes,,',
    "active,\n"
  )
  variables_sheet_content <- c(header_line, second_line)
  test_file_path <- create_test_csv(variables_sheet_content)

  excessive_quoting_errors <- list(
    .create_excessive_quoting_error(
      "variables", test_file_path, 1, 5, '"databaseStart"'),
    .create_excessive_quoting_error(
      "variables", test_file_path, 1, 6, '"variableStart"'),
    .create_excessive_quoting_error(
      "variables", test_file_path, 2, 1, '"BMI"')
  )
  expected_result <- lapply(excessive_quoting_errors, function(err) {
    err$fixed <- TRUE
    return(err)
  })

  actual_result <- fix_worksheet(test_file_path, "variables")

  expect_equal(actual_result, expected_result)

  expect_no_errors(test_file_path, "variables")
})

test_that("Should fix column order errors", {
  header_row <- paste0(
    "label,variable,labelLong,variableType,databaseStart,variableStart,",
    "subject,section,units,notes,description,version,lastUpdated,reviewNotes,",
    "ICES.confirmation,Observation..MD.,status,versionNotes\n"
  )
  data_row <- paste0(
    "Body Mass Index,BMI,Body Mass Index,Continuous,cchs2001_p,[HWTGBMI],",
    "Health,Physical,kg/m2,,Calculated BMI,1.0,2024-01-01,,,,",
    "active,\n"
  )
  variables_sheet_content <- c(header_row, data_row)
  variables_sheet_path <- create_test_csv(variables_sheet_content)

  column_order_errors <- list(
    .create_column_order_error(
      "variables", variables_sheet_path, "variable", 1, "label"),
    .create_column_order_error(
      "variables", variables_sheet_path, "label", 2, "variable")
  )
  expected_result <- lapply(column_order_errors, function(err) {
    err$fixed <- TRUE
    return(err)
  })

  actual_result <- fix_worksheet(variables_sheet_path, "variables")

  expect_equal(actual_result, expected_result)

  expect_no_errors(variables_sheet_path, "variables")

  # Verify entire columns are reordered, not just headers
  correct_header_row <- paste0(
    "variable,label,labelLong,variableType,databaseStart,variableStart,",
    "subject,section,units,notes,description,version,lastUpdated,reviewNotes,",
    "ICES.confirmation,Observation..MD.,status,versionNotes\n"
  )
  correct_data_row <- paste0(
    "BMI,Body Mass Index,Body Mass Index,Continuous,cchs2001_p,[HWTGBMI],",
    "Health,Physical,kg/m2,,Calculated BMI,1.0,2024-01-01,,,,",
    "active,\n"
  )
  correct_variables_sheet_content <- c(correct_header_row, correct_data_row)
  correct_variables_sheet_path <- create_test_csv(
    correct_variables_sheet_content)
  expected_fixed_data <- read.csv(
    correct_variables_sheet_path, stringsAsFactors = FALSE)
  actual_fixed_data <- read.csv(variables_sheet_path, stringsAsFactors = FALSE)
  expect_equal(actual_fixed_data, expected_fixed_data)
})
test_that("Should fix columns order errors when there are missing columns", {
  # The notes column is missing
  variables_sheet_content <- c(
    paste0(
      "variable,label,labelLong,variableType,databaseStart,variableStart,",
      "subject,section,units,description,version,lastUpdated,reviewNotes,",
      "ICES.confirmation,Observation..MD.,status,versionNotes\n"
    ),
    paste0(
      "BMI,Body Mass Index,Body Mass Index,Continuous,cchs,[HWTGBMI],",
      "Health status,BMI,kg/m2,,1.0.0,2024-01-01,,,,active,\n"
    )
  )
  variables_sheet_path <- create_test_csv(variables_sheet_content)

  column_order_errors <- list(
    .create_column_order_error(
      "variables", variables_sheet_path, "notes", 10, "description"),
    .create_column_order_error(
      "variables", variables_sheet_path, "description", 11, "version"),
    .create_column_order_error(
      "variables", variables_sheet_path, "version", 12, "lastUpdated"),
    .create_column_order_error(
      "variables", variables_sheet_path, "lastUpdated", 13, "reviewNotes"),
    .create_column_order_error(
      "variables",
      variables_sheet_path,
      "reviewNotes",
      14,
      "ICES.confirmation"
    ),
    .create_column_order_error(
      "variables",
      variables_sheet_path,
      "ICES.confirmation",
      15,
      "Observation..MD."
    ),
    .create_column_order_error(
      "variables", variables_sheet_path, "Observation..MD.", 16, "status"),
    .create_column_order_error(
      "variables", variables_sheet_path, "status", 17, "versionNotes"),
    .create_column_order_error(
      "variables", variables_sheet_path, "versionNotes", 18, NA_character_)
  )
  expected_result <- lapply(column_order_errors, function(err) {
    err$fixed <- TRUE
    return(err)
  })

  actual_result <- fix_worksheet(variables_sheet_path, "variables")

  expect_equal(actual_result, expected_result)
})

test_that("Should fix unsorted variables sheet errors", {
  header_row <- paste0(
    "variable,label,labelLong,variableType,databaseStart,variableStart,",
    "subject,section,units,notes,description,version,lastUpdated,reviewNotes,",
    "ICES.confirmation,Observation..MD.,status,versionNotes\n"
  )
  weight_row <- paste0(
    "Weight,Weight,Body Weight,Continuous,cchs2001_p,[HWTGKG],Health Status,",
    "Weight,kg,,Weight in kilograms,1.0,2024-01-01,,,,active,\n"
  )
  bmi_row <- paste0(
    "BMI,Body Mass Index,Body Mass Index,Continuous,cchs2001_p,[HWTGBMI],",
    "Health Status,BMI,kg/m2,,Calculated BMI,1.0,2024-01-01,,,,active,\n"
  )
  variables_sheet_content <- c(
    header_row,
    weight_row,
    bmi_row
  )
  variables_sheet_path <- create_test_csv(variables_sheet_content)

  unsorted_rows_errors <- list(.create_unsorted_rows_error(
    "variables", variables_sheet_path, "variable"))
  expected_result <- lapply(unsorted_rows_errors, function(err) {
    err$fixed <- TRUE
    err
  })

  actual_result <- fix_worksheet(variables_sheet_path, "variables")

  expect_equal(actual_result, expected_result)

  expect_no_errors(variables_sheet_path, "variables")

  # Verify rows are actually sorted not just the variable column
  correct_variables_sheet_content <- c(header_row, bmi_row, weight_row)
  correct_variables_sheet_path <- create_test_csv(
    correct_variables_sheet_content)
  expected_fixed_data <- read.csv(
    correct_variables_sheet_path, stringsAsFactors = FALSE)
  actual_fixed_data <- read.csv(variables_sheet_path, stringsAsFactors = FALSE)
  expect_equal(actual_fixed_data, expected_fixed_data)
})

test_that("Should remove empty columns", {
  variables_sheet_content <- c(
    paste0(
      "variable,label,labelLong,variableType,databaseStart,variableStart,",
      "subject,section,units,notes,description,version,lastUpdated,reviewNotes,",
      "ICES.confirmation,Observation..MD.,status,versionNotes,,\n"
    ),
    paste0(
      "BMI,Body Mass Index,Body Mass Index,Continuous,cchs2001_p,[HWTGBMI],",
      "Health status,BMI,kg/m2,,,1.0.0,2024-01-01,,,,active,,,\n"
    )
  )
  variables_sheet_path <- create_test_csv(variables_sheet_content)

  trailing_empty_col_errors <- list(
    .create_trailing_empty_columns_error("variables", variables_sheet_path, 19),
    .create_trailing_empty_columns_error("variables", variables_sheet_path, 20)
  )
  expected_result <- lapply(trailing_empty_col_errors, function(err) {
    err$fixed <- TRUE
    return(err)
  })

  actual_result <- fix_worksheet(variables_sheet_path, "variables")

  expect_equal(actual_result, expected_result)

  expect_no_errors(variables_sheet_path, "variables")
})

test_that("Should fix multiple issues", {
  # Create CSV with multiple issues: wrong column order, unsorted, CRLF
  variables_sheet_content <- c(
    paste0(
      "label,variable,labelLong,variableType,databaseStart,variableStart,",
      "subject,section,units,notes,description,version,lastUpdated,reviewNotes,",
      "ICES.confirmation,Observation..MD.,status,versionNotes\r\n"
    ),
    paste0(
      "Weight,Weight,Body Weight,Continuous,cchs2001_p,[HWTGKG],",
      "Health status,Weight,kg,,,1.0.0,2024-01-01,,,,active,\n"
    ),
    paste0(
      "BMI,Body Mass Index,Body Mass Index,Continuous,cchs2001_p,[HWTGBMI],",
      "Health status,BMI,kg/m2,,,1.0.0,2024-01-01,,,,active,\n"
    )
  )
  variables_sheet_path <- create_test_csv(variables_sheet_content)

  all_errors <- list(
    .create_line_ending_crlf_error("variables", variables_sheet_path, 1),
    .create_column_order_error(
      "variables", variables_sheet_path, "variable", 1, "label"),
    .create_column_order_error(
      "variables", variables_sheet_path, "label", 2, "variable"),
    .create_unsorted_rows_error("variables", variables_sheet_path, "variable")
  )
  expected_result <- lapply(all_errors, function(err) {
    err$fixed <- TRUE
    return(err)
  })

  actual_result <- fix_worksheet(variables_sheet_path, "variables")

  expect_equal(actual_result, expected_result)

  expect_no_errors(variables_sheet_path, "variables")
})
