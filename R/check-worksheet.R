#' Check a CSV worksheet for formatting errors
#'
#' @param file_path Path to the CSV file to check
#' @param file_type Type of file being checked. Either "variables" or
#' "variable_details".
#'
#' @return A list of errors found. Each error is a named list containing
#' information about the error.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' check_worksheet("inst/extdata/variables.csv", "variables")
#' }
check_worksheet <- function(
  file_path, file_type = c("variables", "variable_details")) {
  file_type <- match.arg(file_type)

  schema <- load_schema(file_type)
  expected_columns <- schema$expected_column_order

  if (!file.exists(file_path)) {
    return(list(.create_file_not_found_error(file_type, file_path)))
  }

  csv_result <- tryCatch(
    {
      list(
        data = read.csv(
          file_path, stringsAsFactors = FALSE, check.names = FALSE),
        error = NULL
      )
    },
    error = function(e) {
      list(
        data = NULL,
        error = .create_invalid_csv_error(file_type, file_path, e$message)
      )
    }
  )
  if (!is.null(csv_result$error)) {
    return(list(csv_result$error))
  }

  column_order_errors <- .check_column_order(
    csv_result$data,
    expected_columns,
    list(file_path = file_path, file_type = file_type)
  )

  # Check for missing ID column (always) and row sorting (only if multiple rows)
  row_sorting_errors <- if (!is.null(schema$id_column_name)) {
    id_column_name <- schema$id_column_name
    if (!(id_column_name %in% colnames(csv_result$data))) {
      list(.create_missing_id_column_error(
        file_type, file_path, id_column_name
      ))
    } else if (nrow(csv_result$data) > 1) {
      .check_row_sorting(
        csv_result$data,
        id_column_name,
        list(file_path = file_path, file_type = file_type)
      )
    } else {
      list()
    }
  } else {
    list()
  }

  empty_column_errors <- .check_trailing_empty_columns(
    csv_result$data, list(file_path = file_path, file_type = file_type)
  )

  raw_bytes <- readBin(file_path, "raw", n = file.info(file_path)$size)
  csv_text <- rawToChar(raw_bytes)
  parsed_csv <- .parse_csv_text(csv_text)
  line_ending_errors <- .check_line_endings(
    parsed_csv, list(file_path = file_path, file_type = file_type))

  excessive_quote_errors <- .check_excessive_quoting(
    parsed_csv, list(file_path = file_path, file_type = file_type))

  all_errors <- purrr::flatten(list(
    line_ending_errors,
    excessive_quote_errors,
    column_order_errors,
    row_sorting_errors,
    empty_column_errors
  ))

  return(all_errors)
}

#' Check whether a worksheet has the correct line endings
#'
#' @param csv_text A string containing the worksheet contents
#' @param error_ctx Information used when creating the error object. A named
#' list with the following fields:
#' * file_type: The type of worksheet the CSV contains. Can be "variables" or
#'   "variable_details".
#' * file_path: The file path to the worksheet
#'
#' @return The list of line ending errors found in the worksheet
.check_line_endings <- function(parsed_csv, error_ctx) {
  line_ending_errors <- purrr::imap(parsed_csv, function(record, index) {
    last_field <- record[[length(record)]]
    if (substr(last_field, nchar(last_field) - 1, nchar(last_field)) ==
        "\r\n") {
      return(list(.create_line_ending_crlf_error(
        error_ctx$file_type, error_ctx$file_path, index)))
    }
    return(list())
  })

  return(purrr::flatten(line_ending_errors))
}

#' Check the columns order in a worksheet
#'
#' @param csv_data A data.frame containing the worksheet rows
#' @param expected_columns The worksheet column in their expected order
#' @param error_ctx Information used when creating the error object. A named
#' list with the following fields:
#' * file_type: The type of worksheet the CSV contains. Can be "variables" or
#'   "variable_details".
#' * file_path: The file path to the worksheet
#'
#' @return The list of column order errors found in the worksheet
.check_column_order <- function(csv_data, expected_columns, error_ctx) {
  actual_columns <- colnames(csv_data)

  column_order_errors <- 1:length(expected_columns) %>%
    purrr::keep(function(expected_column_index) {
      expected_column <- expected_columns[expected_column_index]
      actual_column <- actual_columns[expected_column_index]
      # Handle case where actual has fewer columns than expected
      if (is.na(actual_column)) {
        return(TRUE)
      }
      return(expected_column != actual_column)
    }) %>%
    purrr::map(function(missing_expected_column_index) {
      expected_column <- expected_columns[missing_expected_column_index]
      actual_column <- actual_columns[missing_expected_column_index]
      # Use NA string if column doesn't exist
      if (is.na(actual_column)) {
        actual_column <- NA_character_
      }
      return(
        .create_column_order_error(
          error_ctx$file_type,
          error_ctx$file_path,
          expected_column,
          missing_expected_column_index,
          actual_column
        )
      )
    })
  return(column_order_errors)
}

#' Check the rows order in a worksheet
#'
#' @param csv_data A data.frame containing the worksheet rows
#' @param id_column_name Name of the column to check for sorting
#' @param error_ctx Information used when creating the error object. A named
#' list with the following fields:
#' * file_type: The type of worksheet the CSV contains. Can be "variables" or
#'   "variable_details".
#' * file_path: The file path to the worksheet
#'
#' @return The list of row order errors found in the worksheet
.check_row_sorting <- function(csv_data, id_column_name, error_ctx) {
  actual_sorting <- csv_data[[id_column_name]]
  expected_sorting <- sort(actual_sorting)
  if (identical(actual_sorting, expected_sorting)) {
    return(list())
  } else {
    return(list(
      .create_unsorted_rows_error(
        error_ctx$file_type, error_ctx$file_path, id_column_name
      )
    ))
  }
}

#' Check a worksheet for trailing empty columns
#'
#' @param csv_data Data.frame containing the worksheet rows
#' @param error_ctx Information used when creating the error object. A named
#' list with the following fields:
#' * file_type: The type of worksheet the CSV contains. Can be "variables" or
#'   "variable_details".
#' * file_path: The file path to the worksheet
#'
#' @return List of errors
.check_trailing_empty_columns <- function(csv_data, error_ctx) {
  col_names <- colnames(csv_data)

  # Count consecutive empty strings from the end
  reversed_names <- rev(col_names)
  trailing_empty_count <- sum(cumsum(reversed_names != "") == 0)

  if (trailing_empty_count == 0) {
    return(list())
  }

  num_cols <- length(col_names)
  purrr::map(1:trailing_empty_count, function(i) {
    col_position <- num_cols - trailing_empty_count + i
    .create_trailing_empty_columns_error(
      error_ctx$file_type, error_ctx$file_path, col_position)
  })
}

#' Check a worksheet for excessive quoting
#'
#' @param csv_text String containing the worksheet contents
#' @param error_ctx Information used when creating the error object. A named
#' list with the following fields:
#' * file_type: The type of worksheet the CSV contains. Can be "variables" or
#'   "variable_details".
#' * file_path: The file path to the worksheet
#'
#' @return list of errors
.check_excessive_quoting <- function(parsed_csv, error_ctx) {
  line_errors <- purrr::imap(parsed_csv, function(record, row_index) {
    field_errors <- purrr::imap(record, function(field, col_index) {
      # Check if field is quoted
      if (nchar(field) >= 2 && substr(field, 1, 1) == '"' &&
        substr(field, nchar(field), nchar(field)) == '"') {
        # Extract the content inside quotes
        content <- substr(field, 2, nchar(field) - 1)
        # Unescape double quotes
        content <- gsub('""', '"', content, fixed = TRUE)

        # Check if the field needs quoting (contains comma, quote, newline, or
        # carriage return)
        needs_quoting <- grepl('[,"\n\r]', content)

        if (!needs_quoting) {
          return(list(.create_excessive_quoting_error(
            error_ctx$file_type,
            error_ctx$file_path,
            row_index,
            col_index,
            field
          )))
        }
      }
      return(list())
    })

    return(purrr::flatten(field_errors))
  })

  return(purrr::flatten(line_errors))
}

#' Create the error object for when the worksheet could not be found
#'
#' @param file_type: The type of worksheet. Can be "variables" or
#' "variable_details".
#' @param file_path The invalid path
#'
#' @return A named list
.create_file_not_found_error <- function(file_type, file_path) {
  return(list(
    error_type = "file_not_found",
    file_type = file_type,
    file_path = file_path,
    message = glue::glue("{.pretty_print_file_type(file_type)} not found at {file_path}.")
  ))
}

#' Create the error for when the worksheet is not valid CSV
#'
#' @param file_type: The type of worksheet. Can be "variables" or
#' "variable_details".
#' @param file_path Path to the worksheet
#' @param error_message Reason(s) for why the worksheet is invalid CSV
#'
#' @return A named list
.create_invalid_csv_error <- function(file_type, file_path, error_message) {
  return(list(
    error_type = "invalid_csv",
    file_type = file_type,
    file_path = file_path,
    message = glue::glue("Invalid {.pretty_print_file_type(file_type)} at path {file_path}: {error_message}")
  ))
}

#' Create an error for when the worksheet has invalid line endings
#'
#' @param file_type: The type of worksheet. Can be "variables" or
#' "variable_details".
#' @param file_path Path to the worksheet
#' @param row_num Index of the row with the invalid line ending
#'
#' @return A named list
.create_line_ending_crlf_error <- function(file_type, file_path, row_num) {
  expected_line_ending <- "LF"
  actual_line_ending <- "CRLF"
  return(list(
    error_type = "line_ending_crlf",
    file_type = file_type,
    file_path = file_path,
    row_num = row_num,
    expected_line_ending = expected_line_ending,
    actual_line_ending = actual_line_ending,
    message <- glue::glue("Error in {.pretty_print_file_type(file_type)} at {file_path}. Row {row_num} has an invalid line ending. Expected {expected_line_ending} but found {actual_line_ending}.")
  ))
}

#' Create an error for when the worksheet has excessive quoting
#'
#' @param file_type: The type of worksheet. Can be "variables" or
#' "variable_details".
#' @param file_path Path to the worksheet
#' @param row_num Row number with excessive quotes
#' @param col_num Column number with excessive quotes
#' @param cell_value Value of the cell with excessive quotes
#'
#' @return A named list
.create_excessive_quoting_error <- function(
  file_type, file_path, row_num, col_num, cell_value) {
  return(list(
    error_type = "excessive_quoting",
    file_type = file_type,
    file_path = file_path,
    row_num = row_num,
    col_num = col_num,
    cell_value = cell_value,
    message = glue::glue("Error in {.pretty_print_file_type(file_type)} at {file_path}. Cell at row {row_num} and column {col_num} with value {cell_value} has excessive quoting.")
  ))
}

#' Create an error for when the worksheet columns are in the wrong order
#'
#' @param file_type: The type of worksheet. Can be "variables" or
#' "variable_details".
#' @param file_path Path to the worksheet
#' @param expected_column The column expected at the offending position
#' @param col_num The position of the column with the wrong header
#' @param actual_column The actual column value
#'
#' @return A named list
.create_column_order_error <- function(
  file_type, file_path, expected_column, col_num, actual_column) {
  return(list(
    error_type = "column_order",
    file_type = file_type,
    file_path = file_path,
    expected_column = expected_column,
    col_num = col_num,
    actual_column = actual_column,
    message = glue::glue("Error in {.pretty_print_file_type(file_type)} at {file_path}. Incorrect column order. Expected column \"{expected_column}\" at column position {col_num} but found \"{actual_column}\".")
  ))
}

#' Create an error for when the worksheet is missing the ID column
#'
#' @param file_type: The type of worksheet. Can be "variables" or
#' "variable_details".
#' @param file_path Path to the worksheet
#' @param id_column_name Name of the expected ID column
#'
#' @return A named list
.create_missing_id_column_error <- function(
  file_type, file_path, id_column_name) {
  return(list(
    error_type = "missing_id_column",
    file_type = file_type,
    file_path = file_path,
    id_column_name = id_column_name,
    message = glue::glue("Error in {.pretty_print_file_type(file_type)} at {file_path}. Missing required ID column \"{id_column_name}\".")
  ))
}

#' Create an error for when the worksheet rows are unsorted
#'
#' @param file_type: The type of worksheet. Can be "variables" or
#' "variable_details".
#' @param file_path Path to the worksheet
#' @param id_column_name Name of the column that should be sorted
#'
#' @return A named list
.create_unsorted_rows_error <- function(file_type, file_path, id_column_name) {
  return(list(
    error_type = "unsorted_rows",
    file_type = file_type,
    file_path = file_path,
    id_column_name = id_column_name,
    message = glue::glue("Error in {.pretty_print_file_type(file_type)} at {file_path}. Rows are not ordered by the {id_column_name} column.")
  ))
}

#' Create an error for when the worksheet has trailing empty columns
#'
#' @param file_type: The type of worksheet. Can be "variables" or
#' "variable_details".
#' @param file_path Path to the worksheet
#' @param col_num Position of the trailing empty column
#'
#' @return A named list
.create_trailing_empty_columns_error <- function(
  file_type, file_path, col_num) {
  return(list(
    error_type = "empty_columns",
    file_type = file_type,
    file_path = file_path,
    col_num = col_num,
    message = glue::glue("Error in {.pretty_print_file_type(file_type)} at {file_path}. Trailing empty column found at position {col_num}.")
  ))
}

#' Parse the worksheet contents
#'
#' @param csv_text String containing the contents of the worksheet
#'
#' @return A list of list of strings. The top level list contains the rows in
#' the worksheet. The second level contains the cell values of each row. For
#' example, for the following CSV file `a,b,c\n1,2,3` the result of this
#' function would be, `list(list('a', 'b', 'c'), list('1', '2', '3'))`
.parse_csv_text <- function(csv_text) {
  if (length(csv_text) == 0 || nchar(csv_text) == 0) {
    return(list())
  }

  is_escaped_quote <- function(curr_pos, curr_is_in_quotes) {
    if (curr_is_in_quotes && curr_pos < nchar(csv_text) &&
      substr(csv_text, curr_pos + 1, curr_pos + 1) == '"') {
      return(TRUE)
    }
    return(FALSE)
  }

  is_in_quotes <- function(curr_pos, curr_is_in_quotes) {
    curr_char <- substr(csv_text, curr_pos, curr_pos)
    if (curr_char == '"') {
      if (is_escaped_quote(curr_pos, curr_is_in_quotes)) {
        return(curr_is_in_quotes)
      } else {
        return(!curr_is_in_quotes)
      }
    }
    return(curr_is_in_quotes)
  }

  parsed_csv <- purrr::reduce(1:nchar(csv_text), function(state, pos) {
    curr_char <- substr(csv_text, pos, pos)

    curr_is_in_quotes <- state$curr_is_escaped_quote ||
      is_in_quotes(pos, state$previous_is_in_quotes)

    next_is_escaped_quote <- curr_char == '"' &&
      is_escaped_quote(pos, state$previous_is_in_quotes)

    is_field_end <- if (!curr_is_in_quotes && curr_char == ",") {
      TRUE
    } else {
      FALSE
    }
    is_record_end <- if (!curr_is_in_quotes && curr_char == "\n") {
      TRUE
    } else {
      FALSE
    }

    new_field <- if (is_field_end || is_record_end) {
      ""
    } else {
      paste0(state$field, curr_char)
    }
    new_records <- if (is_record_end) {
      record_to_append <- c(state$record, list(paste0(state$field, curr_char)))
      .append_to_list(state$records, record_to_append)
    } else {
      state$records
    }
    new_record <- if (is_field_end) {
      c(state$record, list(state$field))
    } else if (is_record_end) {
      list()
    } else {
      state$record
    }
    return(list(
      previous_is_in_quotes = curr_is_in_quotes,
      field = new_field,
      record = new_record,
      records = new_records,
      curr_is_escaped_quote = next_is_escaped_quote
    ))
  },
  .init = list(
    previous_is_in_quotes = FALSE,
    curr_is_escaped_quote = FALSE,
    field = "",
    record = list(),
    records = list()
  )
  )

  return(parsed_csv$records)
}

#' Add an item to a list
#'
#' @param x the list
#' @param item the item to add
#'
#' @return the new list
.append_to_list <- function(x, item) {
  x[[length(x) + 1]] <- item
  return(x)
}

.pretty_print_file_type <- function(file_type) {
  if(file_type == "variables") {
    return("Variables sheet")
  } else {
    return("Variable details sheet")
  }
}
