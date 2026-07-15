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

# Schema column order (10 columns):
# variable, label, labelLong, section, subject, variableType, units,
# databaseStart, variableStart, description

test_that("Should return an empty list when there are no errors in the
          worksheet", {
  variables_sheet_content <- c(
    paste0(
      "variable,label,labelLong,section,subject,variableType,units,",
      "databaseStart,variableStart,description\n"
    ),
    paste0(
      "BMI,Body Mass Index,Body Mass Index,BMI,Health status,Continuous,",
      "kg/m2,cchs2001_p,[HWTGBMI],Calculated BMI\n"
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
  variables_sheet_content <- c(
    paste0(
      "var_name,label,labelLong,section,subject,variableType,units,",
      "databaseStart,variableStart,description\n"
    ),
    paste0(
      "BMI,Body Mass Index,Body Mass Index,BMI,Health status,Continuous,",
      "kg/m2,cchs2013_2014_p,[HWTGBMI],Calculated BMI\n"
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
      "variable,label,labelLong,section,subject,variableType,units,",
      "databaseStart,variableStart,description\r\n"
    ),
    paste0(
      "BMI,Body Mass Index,Body Mass Index,BMI,Health status,Continuous,",
      "kg/m2,cchs2001_p,[HWTGBMI],Calculated BMI\r\n"
    ),
    paste0(
      "Sex,Sex,Sex,Demographics,Demographics,Categorical,N/A,",
      "cchs,[DHH_SEX],Sex of respondent\n"
    )
  )
  test_file_path <- create_test_csv(variables_sheet_content)

  # readr::read_lines strips \r on some platforms (macOS); skip if so
  raw <- readr::read_lines(test_file_path)
  skip_if(!any(grepl("\r$", raw)), "Platform strips CR from read_lines output")

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
    'variable,label,labelLong,section,"subject",',
    '"variableType",units,databaseStart,variableStart,description\n'
  )
  second_line <- paste0(
    '"BMI","\nBody Mass Index","\r\nBody Mass Index","""BMI",',
    '",Health status",Continuous,kg/m2,cchs2001_p,[HWTGBMI],Calculated BMI\n'
  )
  variables_sheet_content <- c(header_line, second_line)
  test_file_path <- create_test_csv(variables_sheet_content)

  excessive_quoting_errors <- list(
    .create_excessive_quoting_error(
      "variables", test_file_path, 1, 5, '"subject"'),
    .create_excessive_quoting_error(
      "variables", test_file_path, 1, 6, '"variableType"'),
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
    "label,variable,labelLong,section,subject,variableType,units,",
    "databaseStart,variableStart,description\n"
  )
  data_row <- paste0(
    "Body Mass Index,BMI,Body Mass Index,BMI,Health,Continuous,",
    "kg/m2,cchs2001_p,[HWTGBMI],Calculated BMI\n"
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
    "variable,label,labelLong,section,subject,variableType,units,",
    "databaseStart,variableStart,description\n"
  )
  correct_data_row <- paste0(
    "BMI,Body Mass Index,Body Mass Index,BMI,Health,Continuous,",
    "kg/m2,cchs2001_p,[HWTGBMI],Calculated BMI\n"
  )
  correct_variables_sheet_content <- c(correct_header_row, correct_data_row)
  correct_variables_sheet_path <- create_test_csv(
    correct_variables_sheet_content)
  expected_fixed_data <- read.csv(
    correct_variables_sheet_path, stringsAsFactors = FALSE)
  actual_fixed_data <- read.csv(variables_sheet_path, stringsAsFactors = FALSE)
  expect_equal(actual_fixed_data, expected_fixed_data)
})

test_that("Should fix unsorted variables sheet errors", {
  header_row <- paste0(
    "variable,label,labelLong,section,subject,variableType,units,",
    "databaseStart,variableStart,description\n"
  )
  weight_row <- paste0(
    "Weight,Weight,Body Weight,Weight,Health Status,Continuous,",
    "kg,cchs2001_p,[HWTGKG],Weight in kilograms\n"
  )
  bmi_row <- paste0(
    "BMI,Body Mass Index,Body Mass Index,BMI,Health Status,Continuous,",
    "kg/m2,cchs2001_p,[HWTGBMI],Calculated BMI\n"
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
      "variable,label,labelLong,section,subject,variableType,units,",
      "databaseStart,variableStart,description,,\n"
    ),
    paste0(
      "BMI,Body Mass Index,Body Mass Index,BMI,Health status,Continuous,",
      "kg/m2,cchs2001_p,[HWTGBMI],Calculated BMI,,\n"
    )
  )
  variables_sheet_path <- create_test_csv(variables_sheet_content)

  trailing_empty_col_errors <- list(
    .create_trailing_empty_columns_error("variables", variables_sheet_path, 11),
    .create_trailing_empty_columns_error("variables", variables_sheet_path, 12)
  )
  expected_result <- lapply(trailing_empty_col_errors, function(err) {
    err$fixed <- TRUE
    return(err)
  })

  actual_result <- fix_worksheet(variables_sheet_path, "variables")

  expect_equal(actual_result, expected_result)

  expect_no_errors(variables_sheet_path, "variables")
})

test_that("Should remove extra columns not in schema", {
  variables_sheet_content <- c(
    paste0(
      "variable,label,labelLong,section,subject,variableType,units,",
      "databaseStart,variableStart,description,version,status\n"
    ),
    paste0(
      "BMI,Body Mass Index,Body Mass Index,BMI,Health status,Continuous,",
      "kg/m2,cchs2001_p,[HWTGBMI],Calculated BMI,1.0.0,active\n"
    )
  )
  variables_sheet_path <- create_test_csv(variables_sheet_content)

  actual_result <- fix_worksheet(variables_sheet_path, "variables")

  extra_errors <- Filter(function(e) e$error_type == "extra_column", actual_result)
  expect_equal(length(extra_errors), 2)
  expect_true(all(sapply(extra_errors, function(e) e$fixed)))

  # Verify extra columns are gone
  fixed_data <- read.csv(variables_sheet_path, stringsAsFactors = FALSE,
                         check.names = FALSE)
  expect_false("version" %in% colnames(fixed_data))
  expect_false("status" %in% colnames(fixed_data))
  expect_true("variable" %in% colnames(fixed_data))
  expect_true("description" %in% colnames(fixed_data))

  expect_no_errors(variables_sheet_path, "variables")
})

test_that("Should fix multiple issues", {
  # Create CSV with multiple issues: wrong column order, unsorted
  variables_sheet_content <- c(
    paste0(
      "label,variable,labelLong,section,subject,variableType,units,",
      "databaseStart,variableStart,description\n"
    ),
    paste0(
      "Weight,Weight,Body Weight,Weight,Health status,Continuous,",
      "kg,cchs2001_p,[HWTGKG],Weight in kilograms\n"
    ),
    paste0(
      "BMI,Body Mass Index,Body Mass Index,BMI,Health status,Continuous,",
      "kg/m2,cchs2001_p,[HWTGBMI],Calculated BMI\n"
    )
  )
  variables_sheet_path <- create_test_csv(variables_sheet_content)

  all_errors <- list(
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
