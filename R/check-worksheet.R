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

  raw_lines <- readr::read_lines(file_path)
  line_ending_errors <- .check_line_endings(
    raw_lines, list(file_path = file_path, file_type = file_type))

  excessive_quote_errors <- .check_excessive_quoting(
    raw_lines, list(file_path = file_path, file_type = file_type))

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
#' Uses vectorised grep on raw lines for performance. The raw file is read
#' with readr::read_lines() which strips LF but preserves trailing CR if
#' present, so CRLF lines end with \\r.
#'
#' @param raw_lines Character vector of lines from readr::read_lines()
#' @param error_ctx Information used when creating the error object. A named
#' list with the following fields:
#' * file_type: The type of worksheet the CSV contains. Can be "variables" or
#'   "variable_details".
#' * file_path: The file path to the worksheet
#'
#' @return The list of line ending errors found in the worksheet
.check_line_endings <- function(raw_lines, error_ctx) {
  crlf_rows <- grep("\r$", raw_lines)
  purrr::map(crlf_rows, function(row_index) {
    .create_line_ending_crlf_error(
      error_ctx$file_type, error_ctx$file_path, row_index)
  })
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
#' Scans raw CSV lines for quoted fields that don't require quoting. A field
#' needs quoting only if it contains a comma, double-quote, newline, or
#' carriage return. Uses vectorised pre-filter: only lines containing a
#' double-quote are inspected field-by-field.
#'
#' @param raw_lines Character vector of lines from readr::read_lines()
#' @param error_ctx Information used when creating the error object. A named
#' list with the following fields:
#' * file_type: The type of worksheet the CSV contains. Can be "variables" or
#'   "variable_details".
#' * file_path: The file path to the worksheet
#'
#' @return list of errors
.check_excessive_quoting <- function(raw_lines, error_ctx) {
  # Pre-filter: only inspect lines that contain a quote character
  has_quote <- grep('"', raw_lines, fixed = TRUE)
  if (length(has_quote) == 0) return(list())

  errors <- list()
  for (row_index in has_quote) {
    # Parse the single line into fields using R's CSV reader
    fields <- tryCatch(
      scan(
        text = raw_lines[row_index], what = "", sep = ",",
        quote = '"', quiet = TRUE, strip.white = FALSE
      ),
      error = function(e) NULL
    )
    if (is.null(fields)) next

    # Now check which fields in the raw line are unnecessarily quoted.
    # Re-split the raw line respecting CSV quoting to get raw field text.
    raw_fields <- .split_csv_line(raw_lines[row_index])

    for (col_index in seq_along(raw_fields)) {
      field <- raw_fields[col_index]
      if (nchar(field) >= 2 &&
          substr(field, 1, 1) == '"' &&
          substr(field, nchar(field), nchar(field)) == '"') {
        content <- substr(field, 2, nchar(field) - 1)
        content <- gsub('""', '"', content, fixed = TRUE)
        if (!grepl('[,"\n\r]', content)) {
          errors <- c(errors, list(.create_excessive_quoting_error(
            error_ctx$file_type, error_ctx$file_path,
            row_index, col_index, field
          )))
        }
      }
    }
  }
  errors
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
    message = glue::glue("Error in {.pretty_print_file_type(file_type)} at {file_path}. Row {row_num} has an invalid line ending. Expected {expected_line_ending} but found {actual_line_ending}.")
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

#' Check variable_details.csv for recode block recStart collisions
#'
#' For variables with multiple recode blocks (distinct variableStart values),
#' checks whether the same recStart value appears in rows from more than one
#' block for the same database. This directly detects the condition that causes
#' rec_with_table() to match duplicate rows and produce incorrect output.
#'
#' Note: databaseStart overlap alone is not sufficient to flag an error because
#' cchsflow legitimately uses parallel PUMF and Master blocks with shared
#' databases but non-overlapping recStart ranges.
#'
#' @param file_path Path to variable_details.csv
#'
#' @return A list of errors found. Each error is a named list containing
#' information about the error.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' check_recode_blocks("inst/extdata/variable_details.csv")
#' }
check_recode_blocks <- function(file_path) {
  if (!file.exists(file_path)) {
    return(list(.create_file_not_found_error("variable_details", file_path)))
  }

  vd <- tryCatch(
    read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  if (is.null(vd)) {
    return(list())
  }

  required_cols <- c("variable", "variableStart", "databaseStart", "recStart")
  if (!all(required_cols %in% names(vd))) {
    return(list())
  }

  errors <- list()
  all_vars <- unique(vd$variable)

  for (var in all_vars) {
    rows <- vd[vd$variable == var, ]

    # Exclude Func:: rows — derived variable routers that legitimately span all
    # databases. Only check actual recode rows.
    recode_rows <- rows[!grepl("^Func::", rows$recEnd), ]
    blocks <- unique(recode_rows$variableStart)

    if (length(blocks) < 2) next

    # For each recode row, expand databaseStart into individual databases and
    # build a lookup: (database, recStart) -> character vector of blocks
    db_recstart_blocks <- list()

    for (vs in blocks) {
      block_rows <- recode_rows[recode_rows$variableStart == vs, ]
      dbs <- trimws(unlist(strsplit(block_rows$databaseStart[1], ",")))

      for (rec in block_rows$recStart) {
        for (db in dbs) {
          key <- paste0(db, "|||", rec)
          db_recstart_blocks[[key]] <- unique(c(db_recstart_blocks[[key]], vs))
        }
      }
    }

    # Flag any (database, recStart) key present in more than one block
    collision_keys <- names(db_recstart_blocks)[
      vapply(db_recstart_blocks, length, integer(1)) > 1
    ]

    if (length(collision_keys) > 0) {
      errors <- c(errors, list(.create_recode_block_collision_error(
        file_path, var, collision_keys, db_recstart_blocks
      )))
    }
  }

  return(errors)
}

#' Create an error for recStart collisions across recode blocks
#'
#' @param file_path Path to the worksheet
#' @param variable_name Name of the variable with the collision
#' @param collision_keys Character vector of "database|||recStart" keys with collisions
#' @param db_recstart_blocks Named list mapping keys to block vectors
#'
#' @return A named list
.create_recode_block_collision_error <- function(
  file_path, variable_name, collision_keys, db_recstart_blocks) {
  # Summarize by block pair: collect distinct recStart values per pair
  pair_recs <- list()
  for (k in collision_keys) {
    blks <- sort(db_recstart_blocks[[k]])
    pair_key <- paste(blks, collapse = " vs ")
    rec <- strsplit(k, "|||", fixed = TRUE)[[1]][2]
    pair_recs[[pair_key]] <- unique(c(pair_recs[[pair_key]], rec))
  }

  pair_summaries <- vapply(names(pair_recs), function(pk) {
    recs <- pair_recs[[pk]]
    n_recs <- length(recs)
    rec_str <- if (n_recs <= 4) {
      paste(recs, collapse = ", ")
    } else {
      paste0(paste(head(recs, 4), collapse = ", "), " ... (", n_recs, " total)")
    }
    paste0(pk, " share recStart: ", rec_str)
  }, character(1))

  detail_str <- paste(pair_summaries, collapse = "; ")
  n_pairs <- length(pair_recs)
  n_collisions <- length(collision_keys)

  return(list(
    error_type = "recode_block_collision",
    file_type = "variable_details",
    file_path = file_path,
    variable = variable_name,
    collision_keys = collision_keys,
    message = glue::glue(
      "Error in Variable details sheet at {file_path}. ",
      "Variable \"{variable_name}\" has {n_collisions} recStart collision(s) ",
      "across {n_pairs} block pair(s): {detail_str}."
    )
  ))
}

#' Split a single CSV line into raw field strings preserving quoting
#'
#' Unlike scan() which strips quotes, this returns each field exactly as it
#' appears in the raw line — quoted fields retain their surrounding quotes.
#' This is needed for detecting excessive quoting.
#'
#' @param line A single CSV line as a character string
#'
#' @return Character vector of raw field strings
.split_csv_line <- function(line) {
  fields <- character()
  n <- nchar(line)
  if (n == 0) return("")
  start <- 1
  in_quotes <- FALSE
  i <- 1
  while (i <= n) {
    ch <- substr(line, i, i)
    if (in_quotes) {
      if (ch == '"') {
        if (i < n && substr(line, i + 1, i + 1) == '"') {
          i <- i + 1 # skip escaped quote
        } else {
          in_quotes <- FALSE
        }
      }
    } else {
      if (ch == '"') {
        in_quotes <- TRUE
      } else if (ch == ",") {
        fields <- c(fields, substr(line, start, i - 1))
        start <- i + 1
      }
    }
    i <- i + 1
  }
  fields <- c(fields, substr(line, start, n))
  fields
}

.pretty_print_file_type <- function(file_type) {
  if(file_type == "variables") {
    return("Variables sheet")
  } else {
    return("Variable details sheet")
  }
}
