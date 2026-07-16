# ==============================================================================
# LEVEL 3: Worksheet Getters
# ==============================================================================
#
# DEPENDENCY LEVEL: 3 (Universal Worksheet Access Functions)
# DEPENDS ON: Level 1 (file-sourcing.R), Level 2B (worksheet-metadata-loaders.R)
# 
# FUNCTIONS IN THIS FILE:
# - get_variables() - Access variables.csv (1 row per variable)
# - get_variable_details() - Access variable_details.csv (multiple rows per variable)
#
# USED BY LEVEL 4+ FUNCTIONS:
# - get_missing_pattern() (uses get_variable_details)
# - Session cache functions (use get_variable_details)
# - Missing data processing functions
#
# DEVELOPMENT STATUS: Level 3 worksheet getter functions  
# LOCATION: development/flexible-missing-data-mvp/R/worksheet-getters.R
# VERSION: v3.0.0, updated 2025-08-02

# Session-level cache for variable lookup warnings
if (!exists(".variable_warnings_cache")) {
  .variable_warnings_cache <- new.env(parent = emptyenv())
}

# In package context, all R/ files are loaded automatically.
# Dependencies (dplyr) come via DESCRIPTION Depends.

#' Get variable metadata from variables.csv
#' 
#' Access variables.csv with support for scalar, vector inputs.
#' Supports tidyselect patterns like starts_with(), contains(), etc.
#'
#' @param variable_name Character scalar/vector. Variable name(s) to lookup
#' @param ... Tidyselect expressions for field selection (e.g., starts_with("label"))
#' @param use_rdata Logical. Use .RData files if TRUE, CSV if FALSE
#' @return Data.frame with variable metadata (pipe-friendly)
#' @export
#' 
#' @examples
#' # Scalar
#' get_variables("HWTGBMI_der")
#' get_variables("HWTGBMI_der", starts_with("label"))
#' 
#' # Vector
#' get_variables(c("HWTGBMI_der", "ALCDTTM"))
#' get_variables(c("HWTGBMI_der", "ALCDTTM"), contains("label"))
get_variables <- function(variable_name, ..., use_rdata = FALSE) {
  
  # Input validation
  if (missing(variable_name) || is.null(variable_name)) {
    stop("variable_name is required")
  }
  
  if (length(variable_name) == 0) {
    stop("variable_name must not be empty")
  }
  
  if (!is.logical(use_rdata)) {
    stop("use_rdata must be TRUE or FALSE")
  }
  
  variables <- load_worksheet_metadata("variables", use_rdata)
  
  # Handle scalar, vector, or data.frame inputs
  if (is.data.frame(variable_name)) {
    stop("Data.frame input not yet supported. Use variable_name column.")
  }
  
  # Ensure variable_name is character vector
  variable_name <- as.character(variable_name)
  
  # Validate variable names are not empty strings
  if (any(!nzchar(variable_name))) {
    stop("All variable names must be non-empty strings")
  }
  
  # Filter for requested variables
  rows <- variables[variables$variable %in% variable_name, ]
  
  if (nrow(rows) == 0) {
    warning("No variables found in variables.csv: ", paste(variable_name, collapse = ", "))
    return(data.frame())
  }
  
  # Apply tidyselect if provided
  dots <- enquos(...)
  if (length(dots) > 0) {
    rows <- rows %>% dplyr::select(variable, !!!dots)
  }
  
  return(rows)
}

# ==============================================================================
# LEVEL 3 METADATA ACCESS FUNCTIONS (CONSOLIDATED FROM metadata-helpers.R)
# ==============================================================================

#' Get Variable Type from Metadata
#'
#' Determines if a variable is continuous or categorical from variable_details metadata.
#'
#' @param var_name Character. Variable name to check
#' @param variable_details Data.frame. Optional pre-loaded variable_details data
#' @return Character. "continuous" or "categorical"
#' @export
get_variable_type <- function(var_name, variable_details = NULL) {
  
  # Load variable_details if not provided
  if (is.null(variable_details)) {
    variable_details <- get_variable_details(var_name)
  }
  
  var_entries <- variable_details[variable_details$variable == var_name, ]
  
  if (nrow(var_entries) == 0) {
    stop("No metadata found for variable: ", var_name)
  }
  
  type_end <- var_entries$typeEnd[1]
  
  if (type_end == "cont") {
    return("continuous")
  } else if (type_end == "cat") {
    return("categorical")
  } else {
    warning("Unknown typeEnd value '", type_end, "' for variable: ", var_name)
    return("unknown")
  }
}

#' Get Validation Bounds for Continuous Variables
#'
#' Extracts min/max validation limits from variable_details metadata.
#'
#' @param variable_name Character. Variable name
#' @param use_rdata Logical. Whether to use RData format (default TRUE)
#' @return List with min and max values
#' @export
get_variable_limits <- function(variable_name, use_rdata = TRUE) {
  
  # Get variable details using Level 3 infrastructure
  variable_details <- get_variable_details(variable_name, type_filter = "cont")
  
  if (nrow(variable_details) == 0) {
    warning("No validation limits found for variable: ", variable_name)
    return(list(min = -Inf, max = Inf))
  }
  
  # Find rows with validation limits (continuous data ranges)
  limits_rows <- variable_details[
    !is.na(variable_details$recStart) &
    !grepl("NA::|else|Func::|DerivedVar::", variable_details$recEnd),
  ]
  
  if (nrow(limits_rows) == 0) {
    warning("Could not find validation limits for variable: ", variable_name)
    return(list(min = -Inf, max = Inf))
  }
  
  # Parse range from first limits row
  range_spec <- tryCatch({
    parse_range_notation(limits_rows$recStart[1])
  }, error = function(e) {
    warning("Could not parse validation limits for variable: ", variable_name, " - ", e$message)
    return(NULL)
  })
  
  if (is.null(range_spec) || is.null(range_spec$min) || is.null(range_spec$max)) {
    warning("Invalid range specification for variable: ", variable_name)
    return(list(min = -Inf, max = Inf))
  }
  
  return(list(min = range_spec$min, max = range_spec$max))
}

#' Get Variable Bounds (Legacy Alias)
#'
#' @param var_name Character. Variable name
#' @param variable_details Data.frame. Optional pre-loaded variable_details (ignored, uses get_variable_limits)
#' @return List with min and max values
#' @export
get_variable_bounds <- function(var_name, variable_details = NULL) {
  return(get_variable_limits(var_name, use_rdata = FALSE))
}

#' Get variable details from variable_details.csv
#' 
#' Access variable_details.csv with support for scalar, vector inputs.
#' Supports tidyselect patterns and dplyr-friendly filtering.
#'
#' @param variable_name Character scalar/vector. Variable name(s) to lookup
#' @param ... Tidyselect expressions for field selection (e.g., contains("rec"))
#' @param database_filter Character. Filter by databaseStart (e.g., "cchs2001_p")
#' @param type_filter Character. Filter by typeEnd (e.g., "cont", "cat")
#' @param use_rdata Logical. Use .RData files if TRUE, CSV if FALSE
#' @return Data.frame with variable details (pipe-friendly)
#' @export
#' 
#' @examples
#' # Scalar
#' get_variable_details("HWTGBMI_der")
#' get_variable_details("HWTGBMI_der", contains("rec"))
#' 
#' # Vector
#' get_variable_details(c("HWTGBMI_der", "ALCDTTM"))
#' 
#' # With filtering and piping
#' get_variable_details("HWTGBMI_der", type_filter = "cont") %>%
#'   dplyr::filter(databaseStart == "cchs2001_p") %>%
#'   dplyr::select(dplyr::starts_with("rec"))
get_variable_details <- function(variable_name, ..., 
                                database_filter = NULL, type_filter = NULL, 
                                use_rdata = FALSE) {
  
  # Input validation
  if (missing(variable_name) || is.null(variable_name)) {
    stop("variable_name is required")
  }
  
  if (length(variable_name) == 0) {
    stop("variable_name must not be empty")
  }
  
  if (!is.logical(use_rdata)) {
    stop("use_rdata must be TRUE or FALSE")
  }
  
  # Validate filter parameters
  if (!is.null(database_filter) && (!is.character(database_filter) || length(database_filter) != 1)) {
    stop("database_filter must be a single character string or NULL")
  }
  
  if (!is.null(type_filter) && (!is.character(type_filter) || !type_filter %in% c("cont", "cat"))) {
    stop("type_filter must be 'cont', 'cat', or NULL")
  }
  
  variable_details <- load_worksheet_metadata("variable_details", use_rdata)
  
  # Handle scalar, vector, or data.frame inputs
  if (is.data.frame(variable_name)) {
    stop("Data.frame input not yet supported. Use variable_name column.")
  }
  
  # Ensure variable_name is character vector
  variable_name <- as.character(variable_name)
  
  # Validate variable names are not empty strings
  if (any(!nzchar(variable_name))) {
    stop("All variable names must be non-empty strings")
  }
  
  # Filter for requested variables
  rows <- variable_details[variable_details$variable %in% variable_name, ]
  
  if (nrow(rows) == 0) {
    # Only warn once per variable per session
    for (var in variable_name) {
      warning_key <- paste0("var_not_found_", var)
      if (!exists(warning_key, envir = .variable_warnings_cache)) {
        warning("Variable '", var, "' not found in variable_details.csv", call. = FALSE)
        assign(warning_key, TRUE, envir = .variable_warnings_cache)
      }
    }
    return(data.frame())
  }
  
  # Apply filters if specified
  # databaseStart is a comma-separated list (e.g., "cchs2001_p, cchs2003_p").
  # Match rows where database_filter appears as any entry in the list.
  if (!is.null(database_filter)) {
    db_match <- vapply(rows$databaseStart, function(db_list) {
      databases <- trimws(unlist(strsplit(db_list, ",", fixed = TRUE)))
      database_filter %in% databases
    }, logical(1))
    rows <- rows[db_match, ]
  }
  
  if (!is.null(type_filter)) {
    rows <- rows[rows$typeEnd == type_filter, ]
  }
  
  if (nrow(rows) == 0) {
    # Only warn once per variable per session for filtering issues
    warning_key <- paste0("filter_no_rows_", paste(sort(variable_name), collapse = "_"))
    if (!exists(warning_key, envir = .variable_warnings_cache)) {
      warning("No rows found after filtering for variables: ", paste(variable_name, collapse = ", "), call. = FALSE)
      assign(warning_key, TRUE, envir = .variable_warnings_cache)
    }
    return(data.frame())
  }
  
  # Apply tidyselect if provided
  dots <- enquos(...)
  if (length(dots) > 0) {
    rows <- rows %>% dplyr::select(variable, !!!dots)
  }
  
  return(rows)
}

