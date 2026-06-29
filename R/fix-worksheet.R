#' Fix formatting issues in a CSV worksheet
#'
#' @description Automatically corrects formatting violations in a CSV worksheet
#'   based on the errors detected by \code{check_worksheet}.
#'
#' @param file_path Path to the CSV file to fix
#' @param file_type Type of file being fixed. Either "variables" or
#'   "variable_details".
#'
#' @return A list of error objects, each with an added \code{fixed} field:
#'   \itemize{
#'     \item fixed: Boolean indicating if the error was fixed
#'   }
#'   Returns an empty list if no errors were found.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' errors <- fix_worksheet("inst/extdata/variables.csv", "variables")
#' fixed_count <- sum(sapply(errors, function(e) e$fixed))
#' print(sprintf("Fixed %d errors", fixed_count))
#' }
fix_worksheet <- function(
  file_path, file_type = c("variables", "variable_details")
) {
  file_type <- match.arg(file_type)

  errors <- check_worksheet(file_path, file_type)

  if (length(errors) == 0) {
    return(list())
  }

  error_types <- purrr::map_chr(errors, ~ .x$error_type)

  # The following errors can't be fixed
  unfixable_errors <- c("file_not_found", "invalid_csv", "missing_id_column")
  if (any(unfixable_errors %in% error_types)) {
    return(purrr::map(errors, ~ c(.x, fixed = FALSE)))
  }

  initial_csv_data <- tryCatch(
    read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  if (is.null(initial_csv_data)) {
    return(purrr::map(errors, ~ c(.x, fixed = FALSE)))
  }

  empty_columns_fixed_data <- .fix_empty_column_errors(
    initial_csv_data,
    purrr::keep(errors, ~ .x$error_type == "empty_columns")
  )

  extra_columns_fixed_data <- .fix_extra_column_errors(
    empty_columns_fixed_data,
    purrr::keep(errors, ~ .x$error_type == "extra_column")
  )

  column_order_fixed_data <- .fix_column_order_errors(
    extra_columns_fixed_data,
    purrr::keep(errors, ~ .x$error_type == "column_order")
  )

  row_order_fixed_data <- .fix_unsorted_rows_error(
    column_order_fixed_data,
    purrr::keep(errors, ~ .x$error_type == "unsorted_rows")
  )

  # Write CSV content and also fix excessive quoting and line ending errors
  write_result <- tryCatch({
    readr::write_csv(
      row_order_fixed_data,
      file = file_path,
      na = "",
      quote = "needed",
      escape = "double",
      eol = "\n"
    )
    TRUE
  }, error = function(e) FALSE)

  if (!write_result) {
    return(purrr::map(errors, ~ c(.x, fixed = FALSE)))
  }

  fixed_types <- c(
    if ("empty_columns" %in% error_types) "empty_columns",
    if ("extra_column" %in% error_types) "extra_column",
    if ("column_order" %in% error_types) "column_order",
    if ("unsorted_rows" %in% error_types) "unsorted_rows",
    if ("line_ending_crlf" %in% error_types) "line_ending_crlf",
    if ("excessive_quoting" %in% error_types) "excessive_quoting"
  )
  return(purrr::map(errors, ~ c(.x, fixed = .x$error_type %in% fixed_types)))
}

#' Remove empty trailing columns from CSV data
#'
#' @param csv_data A data frame containing the CSV data.
#' @param empty_column_errors A list of empty column error objects from
#'   \code{check_worksheet}.
#'
#' @return The data frame with empty columns removed.
#'
#' @keywords internal
.fix_empty_column_errors <- function(csv_data, empty_column_errors) {
  if (length(empty_column_errors) == 0) {
    return(csv_data)
  }

  empty_col_positions <- purrr::map_int(empty_column_errors, ~ .x$col_num)
  cols_to_keep <- colnames(csv_data)[-empty_col_positions]
  return(csv_data[, cols_to_keep])
}

#' Remove extra columns not defined in the schema
#'
#' @param csv_data A data frame containing the CSV data.
#' @param extra_column_errors A list of extra column error objects from
#'   \code{check_worksheet}.
#'
#' @return The data frame with extra columns removed.
#'
#' @keywords internal
.fix_extra_column_errors <- function(csv_data, extra_column_errors) {
  if (length(extra_column_errors) == 0) {
    return(csv_data)
  }
  extra_col_names <- purrr::map_chr(extra_column_errors, ~ .x$column_name)
  return(csv_data[, !colnames(csv_data) %in% extra_col_names, drop = FALSE])
}

#' Reorder columns to match expected order in a CSV worksheet
#'
#' @param csv_data A data frame containing the CSV data.
#' @param column_order_errors A list of column order error objects from
#'   \code{check_worksheet}.
#'
#' @return The reordered data frame
#'
#' @keywords internal
.fix_column_order_errors <- function(csv_data, column_order_errors) {
  if (length(column_order_errors) == 0) {
    return(csv_data)
  }

  expected_positions <- purrr::map_int(column_order_errors, ~ .x$col_num)
  expected_names <- purrr::map_chr(column_order_errors, ~ .x$expected_column)

  # Add columns that are missing into the sheet
  missing_column_names <- expected_names[
    !expected_names %in% colnames(csv_data)]
  csv_data_with_missing_columns <- purrr::reduce(
    missing_column_names,
    function(new_csv_data, missing_column_name) {
      new_csv_data[[missing_column_name]] <- rep("", nrow(csv_data))
      return(new_csv_data)
    },
    .init = csv_data
  )

  # Build corrected order: place expected columns at their positions, then
  # deduplicate to avoid subsetting with repeated column names.
  corrected_column_order <- purrr::reduce2(
    expected_positions,
    expected_names,
    function(cols, pos, name) {
      cols[pos] <- name
      return(cols)
    },
    .init = colnames(csv_data_with_missing_columns)
  )
  corrected_column_order <- corrected_column_order[
    !duplicated(corrected_column_order)
  ]

  return(csv_data_with_missing_columns[, corrected_column_order])
}

#' Sort rows alphabetically by the ID column
#'
#' @param csv_data A data frame containing the CSV data.
#' @param unsorted_row_errors A list of unsorted row error objects from
#'   \code{check_worksheet}. Each error contains an \code{id_column_name} field.
#'
#' @return The sorted data frame
#'
#' @keywords internal
.fix_unsorted_rows_error <- function(csv_data, unsorted_row_errors) {
  if (length(unsorted_row_errors) == 0) {
    return(csv_data)
  }
  id_column_name <- unsorted_row_errors[[1]]$id_column_name
  return(csv_data[order(csv_data[[id_column_name]]), ])
}
