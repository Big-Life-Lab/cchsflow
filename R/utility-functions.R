#' @title if_else2
#'
#' @description Custom ifelse function that evaluates missing (NA) values. If
#'  the logical argument (x) compares to a value that is `NA`, it is set to
#'  `FALSE`
#'
#' @details unlike the base ifelse() function, if_else2() is able to evaluate NA
#'  as either a or b. In base ifelse(), anything compared to NA will produce
#'  NA, which can break a function. When dealing with large datasets like the
#'  CCHS, there are many missing (NA) values. That means a special ifelse
#'  function like if_else2() is needed in order for other functions to not break
#'
#' @param x A logical argument
#'
#' @param a value if `x` is `TRUE`
#'
#' @param b value if `x` is `FALSE`
#'
#' @return a or b based on the evaluation of x
#'
#' @examples
#' age <- 12
#' status <- if_else2((age < 18), "child", "invalid age")
#' print(status)
#'
#' age <- NA
#' status <- if_else2((age < 18), "child", "invalid age")
#' print(status)
#' @export
if_else2 <- function(x, a, b) {
  falseifNA <- function(x) {
    ifelse(is.na(x), FALSE, x)
  }
  ifelse(falseifNA(x), a, b)
}


# ==============================================================================
# DerivedVar pass-through helper
# ==============================================================================

#' Pass-through for worksheet-routed derived variables
#'
#' Standard implementation for derived variables where the worksheet handles
#' PUMF/Master source routing. The function receives a single input, cleans it
#' using `clean_variables()` metadata, and returns the result.
#'
#' This eliminates boilerplate for variables like `age_start_smoking`,
#' `age_first_cigarette`, and `smoked_100_lifetime` where the function body
#' is identical: NULL check, empty check, clean, return.
#'
#' @param value Numeric vector. The input value(s) from the worksheet-routed
#'   source variable. NULL if the variable is not in the dataset.
#' @param variable_name Character. The cchsflow variable name (e.g.,
#'   "age_start_smoking"). Used for `clean_variables()` pattern lookup and
#'   `assign_missing()` labelling.
#' @param output_format Character. Output format ("tagged_na" or "original").
#'
#' @return Cleaned numeric vector with missing codes in the requested format.
#'   Returns a single NA::b for NULL input, `numeric(0)` for empty input.
#'
#' @examples
#' \dontrun{
#' # Inside a derived variable function:
#' calculate_age_start_smoking <- function(age_start_smoking = NULL,
#'                                         output_format = "tagged_na") {
#'   derive_passthrough(age_start_smoking, "age_start_smoking", output_format)
#' }
#' }
#'
#' @noRd
derive_passthrough <- function(value, variable_name, output_format = "tagged_na") {

  # NULL input — variable not in dataset
  if (is.null(value)) {
    return(assign_missing("not_stated", variable_name, output_format))
  }

  # Empty input
  if (length(value) == 0) return(numeric(0))

  # Clean using variable_details.csv metadata and return
  cleaned <- clean_variables(
    vars = stats::setNames(list(value), variable_name),
    output_format = output_format
  )

  return(cleaned[[variable_name]])
}