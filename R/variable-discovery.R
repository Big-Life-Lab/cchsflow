# ==============================================================================
# Variable Discovery Functions
# ==============================================================================
#
# Application-mode helpers for finding and using harmonized variables.
# These functions query cchsflow worksheets (variables.csv) by metadata
# rather than using fragile regex patterns.
#
# Two modes of variable discovery:
#   - Authoring mode: Find CCHS source variables (use R/source-lookups.R)
#   - Application mode: Find harmonized output variables (this file)
#
# USAGE:
#   source("R/variable-discovery.R")
#
#   # Find smoking variables
#   get_harmonized_variables(subject = "Smoking")
#
#   # Find primary recommended variables
#   get_harmonized_variables(subject = "Smoking", recommended = "primary")
#
#   # Get era-specific source mappings
#   get_source_mappings("SMKDSTY_original")
#
#   # Find variable in loaded data (handles era variants)
#   find_variable_in_data(df, "SMKDSTY_original", cycle = "cchs2001_p")
#
# VERSION: 1.0.0
# ==============================================================================

# ==============================================================================
# Configuration
# ==============================================================================

.var_discovery_config <- list(
  # Default path to variables.csv (main branch)
  variables_csv = "inst/extdata/variables.csv",


  # CEP-002 worksheets (development branch)
  cep_worksheets_dir = "ceps/cep-002-smoking"
)

# ==============================================================================
# Core Functions
# ==============================================================================

#' Get harmonized variables by metadata filters
#'
#' Queries variables.csv or CEP worksheets using structured metadata
#' (subject, section, recommended tags) instead of regex patterns.
#'
#' @param subject Character. Filter by subject (e.g., "Smoking", "Alcohol").
#'   Case-insensitive.
#' @param section Character. Filter by section (e.g., "Health behaviour").
#'   Optional.
#' @param recommended Character. Filter by recommendation tag: "primary",
#'   "secondary", or NULL for all. Looks for `{recommended:X}` in notes field.
#' @param source Character. Which CSV to query: "main" (inst/extdata/variables.csv)
#'   or "cep" (CEP worksheets). Default "main".
#' @param cep_path Character. Path to CEP directory if source = "cep".
#' @return Data frame with matching variables
#' @export
#' @examples
#' \dontrun{
#' # Get all smoking variables
#' get_harmonized_variables(subject = "Smoking")
#'
#' # Get primary recommended smoking variables
#' get_harmonized_variables(subject = "Smoking", recommended = "primary")
#'
#' # Get from CEP-002 worksheets
#' get_harmonized_variables(subject = "Smoking", source = "cep",
#'                          cep_path = "ceps/cep-002-smoking")
#' }
get_harmonized_variables <- function(subject = NULL,
                                     section = NULL,
                                     recommended = NULL,
                                     source = "main",
                                     cep_path = NULL) {

  # Load variables based on source
 if (source == "main") {
    csv_path <- .var_discovery_config$variables_csv
    if (!file.exists(csv_path)) {
      stop("variables.csv not found at: ", csv_path,
           "\nRun from cchsflow repo root or specify cep_path")
    }
    vars <- read.csv(csv_path, stringsAsFactors = FALSE)

  } else if (source == "cep") {
    # Load from CEP worksheets
    cep_dir <- cep_path %||% .var_discovery_config$cep_worksheets_dir
    vars <- .load_cep_variables(cep_dir)

  } else {
    stop("source must be 'main' or 'cep'")
  }

  # Apply filters
  if (!is.null(subject)) {
    vars <- vars[tolower(vars$subject) == tolower(subject), ]
  }

  if (!is.null(section)) {
    vars <- vars[tolower(vars$section) == tolower(section), ]
  }

  if (!is.null(recommended)) {
    pattern <- paste0("\\{recommended:", recommended, "\\}")
    # Check in notes or description field
    notes_col <- if ("notes" %in% names(vars)) "notes" else "description"
    if (notes_col %in% names(vars)) {
      vars <- vars[grepl(pattern, vars[[notes_col]], ignore.case = TRUE), ]
    }
  }

  return(vars)
}


#' Get era-specific source variable mappings
#'
#' Parses the variableStart field to extract source variable names
#' for each database/cycle. Returns a named list for easy lookup.
#'
#' @param variable Character. Harmonized variable name (e.g., "SMKDSTY_original")
#' @param source Character. "main" or "cep". Default "main".
#' @param cep_path Character. Path to CEP directory if source = "cep".
#' @return Named list with database names as keys and source variable names
#'   as values. Also includes `default` key for `[VAR]` pattern.
#' @export
#' @examples
#' \dontrun{
#' mappings <- get_source_mappings("SMKDSTY_original")
#' # Returns: list(
#' #   cchs2001_p = "SMKADSTY",
#' #   cchs2003_p = "SMKCDSTY",
#' #   cchs2005_p = "SMKEDSTY",
#' #   cchs2007_2008_p = "SMKDSTY",
#' #   ...
#' #   default = "SMKDSTY"
#' # )
#'
#' # Use in code:
#' cycle <- "cchs2007_2008_p"
#' source_var <- mappings[[cycle]] %||% mappings$default
#' }
get_source_mappings <- function(variable,
                                source = "main",
                                cep_path = NULL) {

  # Get variable row
  vars <- get_harmonized_variables(source = source, cep_path = cep_path)
  var_row <- vars[vars$variable == variable, ]

  if (nrow(var_row) == 0) {
    stop("Variable not found: ", variable)
  }

  if (nrow(var_row) > 1) {
    warning("Multiple rows for variable ", variable, ", using first")
    var_row <- var_row[1, ]
  }

  # Parse variableStart
  vs <- var_row$variableStart
  if (is.na(vs) || vs == "") {
    return(list())
  }

  .parse_variable_start(vs)
}


#' Find variable in loaded data frame
#'
#' Checks for a harmonized variable's source name in a data frame,
#' handling era-specific naming conventions automatically.
#'
#' @param df Data frame. The loaded CCHS data.
#' @param variable Character. Harmonized variable name (e.g., "SMKDSTY_original")
#' @param cycle Character. Optional cycle name (e.g., "cchs2001_p") for
#'   more efficient lookup. If NULL, checks all known variants.
#' @param source Character. "main" or "cep". Default "main".
#' @return Character. The source variable name found in df, or NULL if not found.
#' @export
#' @examples
#' \dontrun{
#' # Find SMKDSTY_original in 2001 data
#' var_name <- find_variable_in_data(cchs_2001, "SMKDSTY_original", "cchs2001_p")
#' # Returns: "SMKADSTY"
#'
#' # Without cycle hint, checks all variants
#' var_name <- find_variable_in_data(df, "SMKDSTY_original")
#' }
find_variable_in_data <- function(df, variable, cycle = NULL, source = "main") {

  # Get source mappings
  mappings <- tryCatch(
    get_source_mappings(variable, source = source),
    error = function(e) list()
  )

  if (length(mappings) == 0) {
    # Fallback: check if variable name itself exists
    if (variable %in% names(df)) {
      return(variable)
    }
    return(NULL)
  }

  # If cycle specified, check that mapping first
  if (!is.null(cycle) && cycle %in% names(mappings)) {
    source_var <- mappings[[cycle]]
    if (source_var %in% names(df)) {
      return(source_var)
    }
  }

  # Check default
  if ("default" %in% names(mappings)) {
    if (mappings$default %in% names(df)) {
      return(mappings$default)
    }
  }

  # Check all mappings
  all_variants <- unique(unlist(mappings))
  found <- intersect(all_variants, names(df))

 if (length(found) > 0) {
    return(found[1])
  }

  return(NULL)
}


#' Get all source variants for a harmonized variable
#'
#' Returns all known source variable names across all eras.
#' Useful for checking if ANY variant exists in data.
#'
#' @param variable Character. Harmonized variable name
#' @param source Character. "main" or "cep". Default "main".
#' @return Character vector of all source variable names
#' @export
#' @examples
#' \dontrun{
#' get_source_variants("SMKDSTY_original")
#' # Returns: c("SMKADSTY", "SMKCDSTY", "SMKEDSTY", "SMKDSTY", "SMKDVSTY")
#' }
get_source_variants <- function(variable, source = "main") {
  mappings <- get_source_mappings(variable, source = source)
  unique(unlist(mappings))
}


# ==============================================================================
# Helper Functions
# ==============================================================================

#' Load variables from CEP worksheets
#' @keywords internal
.load_cep_variables <- function(cep_dir) {
  # Find all variables_*.csv files in subfolders
  pattern <- file.path(cep_dir, "*", "variables_*.csv")
  files <- Sys.glob(pattern)

  if (length(files) == 0) {
    stop("No variables_*.csv files found in: ", cep_dir)
  }

  # Load and combine
  all_vars <- lapply(files, function(f) {
    df <- read.csv(f, stringsAsFactors = FALSE)
    df$source_file <- basename(f)
    df
  })

  do.call(rbind, all_vars)
}


#' Parse variableStart field into named list
#' @keywords internal
.parse_variable_start <- function(vs) {
  result <- list()

  # First, extract DerivedVar::[...] pattern (contains commas inside brackets)
  # Note: [^]] means "not ]" - don't escape ] inside character class in R
  derived_match <- regmatches(vs, regexec("DerivedVar::\\[([^]]+)\\]", vs))[[1]]
  if (length(derived_match) == 2) {
    derived_vars <- trimws(strsplit(derived_match[2], ",")[[1]])
    result$derived_inputs <- derived_vars
    # Remove from string before splitting
    vs <- sub("DerivedVar::\\[[^]]+\\],?\\s*", "", vs)
  }

  # Now split remaining by comma
  parts <- strsplit(vs, ",\\s*")[[1]]

  for (part in parts) {
    part <- trimws(part)
    if (part == "") next

    # Pattern: db::VAR
    if (grepl("::", part)) {
      match <- regmatches(part, regexec("^([^:]+)::(.+)$", part))[[1]]
      if (length(match) == 3) {
        db <- match[2]
        var <- match[3]
        # Remove brackets if present
        var <- gsub("^\\[|\\]$", "", var)
        result[[db]] <- var
      }
    }

    # Pattern: [VAR] (default)
    else if (grepl("^\\[.+\\]$", part)) {
      var <- gsub("^\\[|\\]$", "", part)
      result$default <- var
    }
  }

  return(result)
}


# Null coalescing operator (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}


# ==============================================================================
# Convenience Functions
# ==============================================================================

#' Get smoking variables (convenience wrapper)
#'
#' @param recommended Character. "primary", "secondary", or NULL
#' @param source Character. "main" or "cep"
#' @return Data frame of smoking variables
#' @export
get_smoking_variables <- function(recommended = NULL, source = "main") {
  get_harmonized_variables(
    subject = "Smoking",
    recommended = recommended,
    source = source
  )
}


#' List available subjects in variables.csv
#'
#' @param source Character. "main" or "cep"
#' @return Character vector of unique subjects
#' @export
list_subjects <- function(source = "main") {
  vars <- get_harmonized_variables(source = source)
  sort(unique(vars$subject))
}


# ==============================================================================
# Load message
# ==============================================================================

message("variable-discovery.R loaded. Functions available:
  Query harmonized variables:
    get_harmonized_variables(subject, recommended)  - Filter by metadata
    get_smoking_variables(recommended)              - Convenience for smoking
    list_subjects()                                 - List available subjects

  Source mappings:
    get_source_mappings(variable)     - Get db::VAR mappings from variableStart
    get_source_variants(variable)     - Get all era-specific source names
    find_variable_in_data(df, var)    - Find source var in loaded data frame
")
