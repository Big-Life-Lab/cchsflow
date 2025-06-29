#' Test a derived function created to run within recodeflow
#'
#' @param test_data A data.frame containing the data to test the function
#' against. The columns of the data.frame should contain all the arguments to 
#' the derived function. It should also contain a column called `expected`
#' which contains the expected value and a column called `notes` which is diplayed
#' when the test fails. The notes columns should describe briefly what the 
#' row is testing.
#' @param derived_function the derived function to test
#' @examples
#' BMI <- function(height, weight) {
#'   return(height/(weight*weight))
#' }
#' 
#' test_data <- data.frame(
#'    height = c(185, 160),
#'    weight = c(85, 70),
#'    expected = c(24.8, 27.3),
#'    notes = c("Normal weight", "Overweight")
#' )
#'
#' test_derived_function(test_data, BMI)
test_derived_function <- function(test_data, derived_function) {
  for(i in seq_len(nrow(test_data))) {
    test_datum <- test_data[i, ]
    arguments <- list()
    for(column in colnames(test_datum)) {
      if(column != "expected" & column != "notes") {
        arguments[[column]] <- test_datum[[column]]
      }
    }
    actual <- do.call(derived_function, arguments)

    expect_equal(
      actual,
      test_datum$expected,
      info = paste("Error in row", i, "when testing", test_datum$notes)
    )
  }
}

#' Test a derived function through rec_with_table() workflow
#' 
#' @param test_data A data.frame containing input data and expected results columns
#' @param variable_name The variable name to test (e.g., "HWTGBMI_der")  
#' @param expected_column The column name containing expected results
#' @param database_name The database name for rec_with_table
#' @param variable_details Variable details configuration (optional, loads default if NULL)
#' @param notes_column Column name for test notes (default: "notes")
#' @examples
#' # Test BMI function through rec_with_table
#' bmi_test_data <- read.csv("../testdata/bmi_rec_with_table.csv")
#' test_rec_with_table_function(bmi_test_data, "HWTGBMI_der", "expected_bmi_fun")
test_rec_with_table_function <- function(test_data, variable_name, expected_column, 
                                       database_name = "test_data", variable_details = NULL,
                                       notes_column = "notes") {
  
  # Load actual cchsflow variable_details if not provided
  if (is.null(variable_details)) {
    load('data/variable_details.RData', envir = environment())
  }
  
  # Process test data through rec_with_table
  result <- rec_with_table(
    data = test_data,
    variables = variable_name,
    database_name = database_name,
    variable_details = variable_details,
    log = FALSE,
    notes = FALSE
  )
  
  # Validate results
  for(i in seq_len(nrow(test_data))) {
    actual <- result[[variable_name]][i]
    expected <- test_data[[expected_column]][i]
    test_note <- if(notes_column %in% colnames(test_data)) test_data[[notes_column]][i] else paste("Row", i)
    
    validate_derived_results(
      actual = actual,
      expected = expected,
      info = paste("Error in row", i, "when testing", test_note, "for variable", variable_name)
    )
  }
}

#' Create mock variable_details for derived function testing
#' 
#' @param variable_name The derived variable name
#' @param function_name The function name (e.g., "bmi_fun")
#' @param input_vars Vector of input variable names  
#' @param database_name Database name
#' @return A data.frame with minimal variable_details structure
create_test_variable_details <- function(variable_name, function_name, input_vars, database_name = "test_data") {
  
  # Create the basic structure needed for rec_with_table matching real variable_details.csv
  variable_details <- data.frame(
    variable = variable_name,
    dummyVariable = "N/A",
    typeEnd = "cont",  # Assume continuous for most derived functions
    databaseStart = database_name,
    variableStart = paste0("DerivedVar::[", paste(input_vars, collapse = ", "), "]"),
    `ICES confirmation` = "",
    typeStart = "N/A", 
    recEnd = paste0("Func::", function_name),
    numValidCat = "N/A",
    catLabel = "N/A",
    catLabelLong = "N/A",
    units = "N/A",
    recStart = "N/A",
    catStartLabel = "N/A",
    variableStartShortLabel = variable_name,
    variableStartLabel = variable_name,
    notes = "",
    stringsAsFactors = FALSE,
    check.names = FALSE  # Preserve column names with spaces
  )
  
  return(variable_details)
}

#' Validate results with proper tagged_na() handling
#' 
#' @param actual Actual results
#' @param expected Expected results  
#' @param tolerance Numeric tolerance for comparisons
#' @param info Info message for test failures
validate_derived_results <- function(actual, expected, tolerance = 0.01, info = "") {
  
  # Handle different expected value formats
  if (is.character(expected)) {
    if (expected == "NA(a)") {
      expect_true(haven::is_tagged_na(actual, "a"), info = info)
      return()
    } else if (expected == "NA(b)") {
      expect_true(haven::is_tagged_na(actual, "b"), info = info)
      return()
    } else if (expected == "NA") {
      expect_true(is.na(actual), info = info)
      return()
    } else {
      # Try to convert to numeric if it's a numeric string
      expected_numeric <- suppressWarnings(as.numeric(expected))
      if (!is.na(expected_numeric)) {
        expected <- expected_numeric
      }
    }
  }
  
  # Handle numeric comparisons
  if (is.numeric(expected) && is.numeric(actual)) {
    expect_equal(actual, expected, tolerance = tolerance, info = info)
  } else {
    # Handle exact matches for factors, characters, etc.
    expect_equal(actual, expected, info = info)
  }
}

#' Parse variableStart field using YAML-defined patterns
#' 
#' Extracts dependency information from the variableStart field in variable_details
#' using patterns defined in inst/metadata/schemas/core/variables.yaml. Handles
#' multiple pattern types including derived variables, database mappings, and 
#' simple references.
#' 
#' @param variable_start String from variable_details$variableStart containing
#'   dependency specifications. Supports patterns like:
#'   - "DerivedVar::[VAR1, VAR2]" for derived function inputs
#'   - "database::variable" for raw CCHS variable mappings  
#'   - "[variable]" for simple variable references
#' @param database_name Target database name (e.g., "cchs2015_2016_p") used
#'   to filter database-specific variable mappings
#'   
#' @return Named list with three elements:
#'   \describe{
#'     \item{immediate_dependencies}{Character vector of variables that this 
#'       variable directly depends on (e.g., harmonized variables)}
#'     \item{raw_cchs_variables}{Character vector of raw CCHS variable names
#'       needed from the specified database}
#'     \item{database_mappings}{Named list mapping variable names to their
#'       database-specific raw variable names}
#'   }
#'   
#' @details
#' The function uses regex patterns to identify and extract different types of
#' variable dependencies:
#' 
#' \strong{Pattern Types:}
#' \itemize{
#'   \item \strong{DerivedVar pattern}: Extracts immediate dependencies from
#'     patterns like "DerivedVar::[HEIGHT, WEIGHT]"
#'   \item \strong{Database mapping}: Extracts cycle-specific raw variables from
#'     patterns like "cchs2015_2016_p::RAW_HEIGHT"  
#'   \item \strong{Simple reference}: Extracts variable references from
#'     patterns like "[HARMONIZED_VAR]"
#' }
#' 
#' Complex patterns mixing multiple types are supported, such as:
#' "cchs2015_2016_p::RAW_VAR, cchs2017_2018_p::RAW_VAR2, [HARM_VAR]"
#' 
#' @examples
#' \dontrun{
#' # Parse BMI derived variable dependencies
#' result <- parse_variable_start("DerivedVar::[HWTGHTM, HWTGWTK]", "cchs2015_2016_p")
#' print(result$immediate_dependencies)  # c("HWTGHTM", "HWTGWTK")
#' 
#' # Parse harmonized variable with raw mappings
#' complex_pattern <- "cchs2015_2016_p::HWTDGHTM, cchs2017_2018_p::HWTDGHTM, [HWTGHTM]"
#' result <- parse_variable_start(complex_pattern, "cchs2015_2016_p") 
#' print(result$raw_cchs_variables)      # "HWTDGHTM"
#' print(result$immediate_dependencies)  # "HWTGHTM"
#' }
#' 
#' @seealso \code{\link{get_variable_dependencies}} for higher-level dependency analysis
#' @family dependency-analysis
#' @keywords internal
parse_variable_start <- function(variable_start, database_name) {
  
  # YAML patterns from inst/metadata/schemas/core/variables.yaml
  yaml_patterns <- list(
    derived_variable = "^DerivedVar::\\[([A-Z][A-Z0-9_]*(,\\s*[A-Z][A-Z0-9_]*)*)\\]$",
    database_mapping = "[a-zA-Z0-9_]+::[A-Z][A-Z0-9_]*",
    simple_reference = "\\[[A-Z][A-Z0-9_]*\\]",
    multiple_sources = "^([a-zA-Z0-9_]+::[A-Z][A-Z0-9_]*(,\\s*)?)+$"
  )
  
  result <- list(
    immediate_dependencies = character(0),
    raw_cchs_variables = character(0),
    database_mappings = list()
  )
  
  # Pattern 1: DerivedVar::[var1, var2] - Extract immediate dependencies
  if (grepl("DerivedVar::", variable_start)) {
    # Use simple bracket pattern that works
    if (grepl("\\[.*\\]", variable_start)) {
      match_result <- regmatches(variable_start, regexpr("\\[.*\\]", variable_start))
      # Extract content between brackets using substr
      content <- substr(match_result, 2, nchar(match_result) - 1)
      result$immediate_dependencies <- trimws(unlist(strsplit(content, ",")))
    }
  }
  
  # Pattern 2: Extract database::variable mappings
  db_matches <- regmatches(variable_start, gregexpr(yaml_patterns$database_mapping, variable_start))[[1]]
  if (length(db_matches) > 0) {
    for (mapping in db_matches) {
      parts <- unlist(strsplit(mapping, "::"))
      if (length(parts) == 2) {
        db_name <- parts[1]
        var_name <- parts[2]
        
        # If this mapping is for our target database, record it
        if (db_name == database_name) {
          result$raw_cchs_variables <- c(result$raw_cchs_variables, var_name)
        }
        
        # Store all mappings for reference
        if (is.null(result$database_mappings[[var_name]])) {
          result$database_mappings[[var_name]] <- list()
        }
        result$database_mappings[[var_name]][[db_name]] <- var_name
      }
    }
  }
  
  # Pattern 3: [variable] simple references - extract separately from database mappings
  simple_matches <- regmatches(variable_start, gregexpr(yaml_patterns$simple_reference, variable_start))[[1]]
  if (length(simple_matches) > 0) {
    simple_vars <- gsub("\\[|\\]", "", simple_matches)
    result$immediate_dependencies <- c(result$immediate_dependencies, simple_vars)
  }
  
  # Remove duplicates
  result$immediate_dependencies <- unique(result$immediate_dependencies)
  result$raw_cchs_variables <- unique(result$raw_cchs_variables)
  
  return(result)
}

#' Get variable dependencies with multiple output formats
#' 
#' Analyzes variable dependencies by recursively resolving the complete chain from
#' derived variables down to raw CCHS variables. Supports multiple output formats
#' for different use cases including data extraction, test generation, and 
#' dependency visualization.
#' 
#' @param variable_name Target variable name to analyze (e.g., "HWTGBMI_der").
#'   Must exist in the variable_details data frame.
#' @param database_name Database name to filter dependencies (e.g., "cchs2015_2016_p").
#'   Used to select the appropriate raw variable mappings for the CCHS cycle.
#' @param variable_details Data frame containing variable configuration, typically
#'   loaded from data/variable_details.RData. Must contain columns: variable,
#'   databaseStart, variableStart, recEnd.
#' @param output_format Character string specifying return format. One of:
#'   \describe{
#'     \item{"extraction_list"}{Character vector of raw CCHS variable names (default)}
#'     \item{"dependency_chain"}{Named list with complete dependency information}
#'     \item{"test_data_spec"}{Named list optimized for test data generation}
#'     \item{"processing_order"}{Character vector for complete rec_with_table workflow}
#'   }
#'   
#' @return Return value depends on output_format parameter:
#' 
#' \strong{For "extraction_list" (default):}
#' Character vector of raw CCHS variable names needed for data reads.
#' Example: c("HWTDGHTM", "HWTDGWTK") for BMI calculation.
#' 
#' \strong{For "dependency_chain":}
#' Named list containing:
#' \describe{
#'   \item{target_variable}{The input variable name}
#'   \item{database_name}{The input database name}
#'   \item{immediate_dependencies}{Direct dependencies (harmonized variables)}
#'   \item{raw_cchs_variables}{All raw CCHS variables needed}
#'   \item{derived_functions}{Function names in processing chain}
#'   \item{database_mappings}{Raw variable mappings by database}
#' }
#' 
#' \strong{For "test_data_spec":}
#' Named list optimized for test generation:
#' \describe{
#'   \item{required_columns}{Raw CCHS variables needed in test data}
#'   \item{target_variable}{Variable being tested}
#'   \item{derived_functions}{Functions that will be called}
#'   \item{database_name}{Database context for test}
#' }
#' 
#' \strong{For "processing_order":}
#' Character vector containing the complete variable sequence needed for
#' rec_with_table() workflow, including harmonized and derived variables.
#' 
#' @details
#' The function works by:
#' \enumerate{
#'   \item Finding the primary processing row for the target variable and database
#'   \item Parsing dependency specifications using YAML-defined patterns
#'   \item Recursively resolving dependencies until reaching raw CCHS variables
#'   \item Formatting results according to the requested output format
#' }
#' 
#' \strong{Supported Variable Types:}
#' \itemize{
#'   \item \strong{Derived variables}: Created by functions (recEnd starts with "Func::")
#'   \item \strong{Harmonized variables}: Copied from raw variables (recEnd = "copy")
#'   \item \strong{Raw variables}: Original CCHS survey variables
#' }
#' 
#' The function handles complex dependency chains automatically, such as:
#' BMI categorical → BMI continuous → Height + Weight → Raw CCHS variables
#' 
#' @examples
#' \dontrun{
#' # Load variable configuration
#' load('data/variable_details.RData')
#' 
#' # Get raw variables needed for BMI calculation
#' raw_vars <- get_variable_dependencies("HWTGBMI_der", "cchs2015_2016_p", variable_details)
#' print(raw_vars)  # c("HWTDGHTM", "HWTDGWTK")
#' 
#' # Get complete dependency information
#' deps <- get_variable_dependencies("HWTGBMI_der", "cchs2015_2016_p", 
#'                                  variable_details, "dependency_chain")
#' print(deps$derived_functions)  # "bmi_fun"
#' print(deps$immediate_dependencies)  # c("HWTGHTM", "HWTGWTK")
#' 
#' # Get test data specification
#' test_spec <- get_variable_dependencies("HWTGBMI_der", "cchs2015_2016_p",
#'                                       variable_details, "test_data_spec") 
#' print(test_spec$required_columns)  # Variables needed in test data frame
#' 
#' # Use for CCHS data reading
#' project_vars <- c("HWTGBMI_der", "HWTGBMI_der_cat4")
#' all_raw_vars <- unique(unlist(lapply(project_vars, get_variable_dependencies, 
#'                                     "cchs2015_2016_p", variable_details)))
#' # all_raw_vars contains minimal set of raw variables to read from CCHS
#' }
#' 
#' @seealso 
#' \code{\link{parse_variable_start}} for lower-level pattern parsing
#' \code{\link{test_rec_with_table_function}} for testing derived variables
#' 
#' @family dependency-analysis
#' @export
get_variable_dependencies <- function(variable_name, database_name, variable_details, 
                                    output_format = "extraction_list") {
  
  # Step 1: Find primary processing row
  primary_rows <- variable_details[
    variable_details$variable == variable_name & 
    grepl(database_name, variable_details$databaseStart) &
    grepl("Func::", variable_details$recEnd), 
  ]
  
  if (nrow(primary_rows) == 0) {
    # Try to find non-function rows (copy operations)
    copy_rows <- variable_details[
      variable_details$variable == variable_name &
      grepl(database_name, variable_details$databaseStart) &
      variable_details$recEnd == "copy",
    ]
    
    if (nrow(copy_rows) == 0) {
      stop(paste("Variable", variable_name, "not found for database", database_name))
    }
    
    primary_row <- copy_rows[1, ]
  } else {
    primary_row <- primary_rows[1, ]
  }
  
  # Step 2: Parse dependencies
  parsed <- parse_variable_start(primary_row$variableStart, database_name)
  
  # Step 3: Recursive resolution for immediate dependencies
  all_raw_vars <- parsed$raw_cchs_variables
  derived_functions <- character(0)
  
  # Extract function name if this is a derived variable
  if (grepl("Func::", primary_row$recEnd)) {
    func_name <- gsub("Func::", "", primary_row$recEnd)
    derived_functions <- c(derived_functions, func_name)
  }
  
  # Recursively resolve dependencies
  for (dep_var in parsed$immediate_dependencies) {
    tryCatch({
      dep_result <- get_variable_dependencies(dep_var, database_name, variable_details, "dependency_chain")
      all_raw_vars <- c(all_raw_vars, dep_result$raw_cchs_variables)
      derived_functions <- c(derived_functions, dep_result$derived_functions)
    }, error = function(e) {
      # If dependency resolution fails, it might be a raw variable
      # Check if it has database mappings
      dep_rows <- variable_details[
        variable_details$variable == dep_var &
        grepl(database_name, variable_details$databaseStart),
      ]
      
      if (nrow(dep_rows) > 0) {
        dep_parsed <- parse_variable_start(dep_rows[1, ]$variableStart, database_name)
        all_raw_vars <- c(all_raw_vars, dep_parsed$raw_cchs_variables)
      }
    })
  }
  
  # Remove duplicates
  all_raw_vars <- unique(all_raw_vars)
  derived_functions <- unique(derived_functions)
  
  # Step 4: Format output
  switch(output_format,
    "extraction_list" = {
      return(all_raw_vars)
    },
    "dependency_chain" = {
      return(list(
        target_variable = variable_name,
        database_name = database_name,
        immediate_dependencies = parsed$immediate_dependencies,
        raw_cchs_variables = all_raw_vars,
        derived_functions = derived_functions,
        database_mappings = parsed$database_mappings
      ))
    },
    "test_data_spec" = {
      return(list(
        required_columns = all_raw_vars,
        target_variable = variable_name,
        derived_functions = derived_functions,
        database_name = database_name
      ))
    },
    "processing_order" = {
      # Return complete variable processing sequence for rec_with_table
      processing_vars <- c(parsed$immediate_dependencies, variable_name)
      return(unique(processing_vars))
    },
    stop(paste("Unknown output_format:", output_format))
  )
}
