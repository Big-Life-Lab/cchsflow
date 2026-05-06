#' Filter worksheets to a subset of variables
#'
#' Creates temporary copies of variables.csv and variable_details.csv
#' containing only the rows matching the specified scope. Scope can be
#' defined by variable names, subject values, or auto-detected from git diff.
#'
#' @param variables_path Path to variables.csv
#' @param variable_details_path Path to variable_details.csv
#' @param variables Character vector of variable names to include, or NULL
#' @param subjects Character vector of subject values to include, or NULL
#'
#' @return A named list with `variables_path` and `variable_details_path`
#' pointing to (possibly temp) files, plus `scope_desc` describing what was
#' filtered, and `scoped` (logical) indicating whether filtering was applied.
#'
#' @export
scope_worksheets <- function(
  variables_path,
  variable_details_path,
  variables = NULL,
  subjects = NULL
) {
  if (is.null(variables) && is.null(subjects)) {
    return(list(
      variables_path = variables_path,
      variable_details_path = variable_details_path,
      scope_desc = "all variables",
      scoped = FALSE
    ))
  }

  if (!file.exists(variables_path)) {
    stop("Variables worksheet not found at ", variables_path)
  }
  if (!file.exists(variable_details_path)) {
    stop("Variable details worksheet not found at ", variable_details_path)
  }

  vars_df <- tryCatch(
    read.csv(variables_path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) stop("Failed to read ", variables_path, ": ", e$message)
  )
  details_df <- tryCatch(
    read.csv(variable_details_path, stringsAsFactors = FALSE,
             check.names = FALSE),
    error = function(e) stop("Failed to read ", variable_details_path, ": ",
                             e$message)
  )

  if (!"variable" %in% colnames(vars_df)) {
    stop("variables.csv is missing the 'variable' column")
  }
  if (!"variable" %in% colnames(details_df)) {
    stop("variable_details.csv is missing the 'variable' column")
  }

  # Build the set of in-scope variable names
  in_scope <- character()

  if (!is.null(variables)) {
    in_scope <- union(in_scope, variables)
  }

  if (!is.null(subjects)) {
    subject_vars <- vars_df$variable[
      trimws(vars_df$subject) %in% subjects
    ]
    in_scope <- union(in_scope, subject_vars)
  }

  # Filter both data frames
  vars_filtered <- vars_df[vars_df$variable %in% in_scope, ]
  details_filtered <- details_df[details_df$variable %in% in_scope, ]

  scope_desc <- if (!is.null(subjects) && !is.null(variables)) {
    paste0(length(in_scope), " variables (subjects: ",
           paste(subjects, collapse = ", "),
           " + explicit: ", paste(variables, collapse = ", "), ")")
  } else if (!is.null(subjects)) {
    paste0(length(in_scope), " variables in subjects: ",
           paste(subjects, collapse = ", "))
  } else {
    paste0(length(in_scope), " variables: ",
           paste(in_scope, collapse = ", "))
  }

  # Write to temp files preserving header structure
  tmp_vars <- tempfile(pattern = "variables_scoped_", fileext = ".csv")
  tmp_details <- tempfile(pattern = "variable_details_scoped_", fileext = ".csv")

  readr::write_csv(vars_filtered, tmp_vars, na = "", quote = "needed",
                    escape = "double", eol = "\n")
  readr::write_csv(details_filtered, tmp_details, na = "", quote = "needed",
                    escape = "double", eol = "\n")

  list(
    variables_path = tmp_vars,
    variable_details_path = tmp_details,
    scope_desc = scope_desc,
    scoped = TRUE
  )
}

#' Parse --variables and --subject CLI arguments
#'
#' Note: The CLI flag `--subject` (singular) maps to the `subjects` parameter
#' (plural) in `scope_worksheets()`.
#'
#' @param args Character vector from commandArgs(trailingOnly = TRUE)
#'
#' @return Named list with `variables` (character vector or NULL) and
#' `subjects` (character vector or NULL)
#'
#' @export
parse_scope_args <- function(args) {
  variables <- NULL
  subjects <- NULL

  var_idx <- which(args == "--variables")
  if (length(var_idx) > 0) {
    if (var_idx[1] >= length(args)) {
      warning("--variables flag provided without a value; ignoring.")
    } else {
      variables <- trimws(unlist(strsplit(args[var_idx[1] + 1], ",")))
    }
  }

  subj_idx <- which(args == "--subject")
  if (length(subj_idx) > 0) {
    if (subj_idx[1] >= length(args)) {
      warning("--subject flag provided without a value; ignoring.")
    } else {
      subjects <- trimws(unlist(strsplit(args[subj_idx[1] + 1], ",")))
    }
  }

  list(variables = variables, subjects = subjects)
}
