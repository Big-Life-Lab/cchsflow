#' Recodeflow Schema Validation System
#' @name recodeflow_schema_validation
#' 
#' Comprehensive validation for Recodeflow YAML schemas.

library(yaml)
library(stringr)  # For cross-platform regex consistency (ICU engine)

#' Load and Parse YAML Schema
#'
#' @param schema_path Path to YAML schema file
#' @return Parsed schema object
load_schema <- function(schema_path) {
  if (!file.exists(schema_path)) {
    stop("Schema file not found: ", schema_path)
  }
  
  tryCatch({
    yaml::read_yaml(schema_path)
  }, error = function(e) {
    stop("Error parsing YAML schema: ", e$message)
  })
}

#' Validate CSV Against Schema
#'
#' @param csv_path Path to CSV file to validate
#' @param schema_path Path to YAML schema file
#' @param mode Validation mode: "basic" or "pipeline"
#' @return Validation result object
validate_csv_against_schema <- function(csv_path, schema_path, mode = "basic") {
  # Load schema and data
  schema <- load_schema(schema_path)
  
  if (!file.exists(csv_path)) {
    stop("CSV file not found: ", csv_path)
  }
  
  data <- read.csv(csv_path, stringsAsFactors = FALSE)
  
  # Determine which schema to use
  if ("variables_schema" %in% names(schema)) {
    schema_def <- schema$variables_schema
    schema_type <- "variables"
  } else if ("variable_details_schema" %in% names(schema)) {
    schema_def <- schema$variable_details_schema
    schema_type <- "variable_details"
  } else {
    stop("Unknown schema type in file: ", schema_path)
  }
  
  # Initialize validation results
  validation_result <- list(
    csv_file = basename(csv_path),
    schema_file = basename(schema_path),
    schema_type = schema_type,
    mode = mode,
    timestamp = Sys.time(),
    
    # Summary stats
    total_rows = nrow(data),
    total_columns = ncol(data),
    
    # Validation results
    field_validation = list(),
    column_order_check = list(),
    cross_field_validation = list(),
    pattern_validation = list(),
    enum_validation = list(),
    
    # Issues found
    errors = character(0),
    warnings = character(0),
    info = character(0),
    
    # Summary
    valid = TRUE,
    issues_count = 0
  )
  
  # Get mode-specific requirements
  mode_config <- schema_def$validation_modes[[mode]]
  required_fields <- if (!is.null(mode_config)) mode_config$required_fields else schema_def$fields[sapply(schema_def$fields, function(f) f$constraints$required)]
  
  # 1. Column existence and order validation
  validation_result$column_order_check <- validate_column_order(data, schema_def)
  
  # 2. Field-level validation
  validation_result$field_validation <- validate_fields(data, schema_def, mode_config)
  
  # 3. Pattern validation
  validation_result$pattern_validation <- validate_patterns(data, schema_def)
  
  # 4. Enum validation (skip in basic mode if specified)
  skip_enum <- !is.null(mode_config) && isTRUE(mode_config$skip_enum_validation)
  if (!skip_enum) {
    validation_result$enum_validation <- validate_enums(data, schema_def, schema)
  }
  
  # 5. Cross-field validation (skip in basic mode if specified)
  skip_cross_field <- !is.null(mode_config) && isTRUE(mode_config$skip_cross_field_validation)
  if (!skip_cross_field && !is.null(schema_def$validation_rules$cross_field)) {
    validation_result$cross_field_validation <- validate_cross_field_rules(data, schema_def)
  }
  
  # Collect all issues
  all_validations <- list(
    validation_result$column_order_check,
    validation_result$field_validation,
    validation_result$pattern_validation,
    validation_result$enum_validation,
    validation_result$cross_field_validation
  )
  
  for (validation in all_validations) {
    if (!is.null(validation)) {
      validation_result$errors <- c(validation_result$errors, validation$errors)
      validation_result$warnings <- c(validation_result$warnings, validation$warnings)
      validation_result$info <- c(validation_result$info, validation$info)
    }
  }
  
  # Summary
  validation_result$issues_count <- length(validation_result$errors) + length(validation_result$warnings)
  validation_result$valid <- length(validation_result$errors) == 0
  
  class(validation_result) <- "schema_validation_result"
  return(validation_result)
}

#' Validate Column Order
validate_column_order <- function(data, schema_def) {
  result <- list(errors = character(0), warnings = character(0), info = character(0))
  
  actual_columns <- names(data)
  expected_order <- schema_def$expected_column_order
  
  # Check for missing required columns
  required_columns <- sapply(schema_def$fields, function(f) {
    if (isTRUE(f$constraints$required)) f$name else NULL
  })
  required_columns <- unlist(required_columns)
  
  missing_required <- setdiff(required_columns, actual_columns)
  if (length(missing_required) > 0) {
    result$errors <- c(result$errors, paste("Missing required columns:", paste(missing_required, collapse = ", ")))
  }
  
  # Check column order
  common_columns <- intersect(expected_order, actual_columns)
  actual_order_of_common <- actual_columns[actual_columns %in% common_columns]
  expected_order_of_common <- expected_order[expected_order %in% common_columns]
  
  if (!identical(actual_order_of_common, expected_order_of_common)) {
    result$warnings <- c(result$warnings, "Column order doesn't match schema recommendation")
    result$info <- c(result$info, paste("Expected order:", paste(expected_order_of_common, collapse = ", ")))
    result$info <- c(result$info, paste("Actual order:", paste(actual_order_of_common, collapse = ", ")))
  }
  
  # Check for additional columns
  extra_columns <- setdiff(actual_columns, expected_order)
  if (length(extra_columns) > 0) {
    if (isTRUE(schema_def$allow_additional_columns)) {
      result$info <- c(result$info, paste("Additional columns found (allowed):", paste(extra_columns, collapse = ", ")))
    } else {
      result$warnings <- c(result$warnings, paste("Unexpected additional columns:", paste(extra_columns, collapse = ", ")))
    }
  }
  
  return(result)
}

#' Validate Individual Fields
validate_fields <- function(data, schema_def, mode_config) {
  result <- list(errors = character(0), warnings = character(0), info = character(0))
  
  for (field_def in schema_def$fields) {
    field_name <- field_def$name
    
    if (!field_name %in% names(data)) {
      if (isTRUE(field_def$constraints$required)) {
        result$errors <- c(result$errors, paste("Required field missing:", field_name))
      }
      next
    }
    
    field_data <- data[[field_name]]
    
    # Check required values
    if (isTRUE(field_def$constraints$required)) {
      missing_values <- schema_def$missingValues %||% c("", "NA", "N/A")
      empty_count <- sum(is.na(field_data) | field_data %in% missing_values)
      if (empty_count > 0) {
        result$errors <- c(result$errors, paste("Required field", field_name, "has", empty_count, "empty values"))
      }
    }
    
    # Check unique constraint
    if (isTRUE(field_def$constraints$unique)) {
      duplicates <- sum(duplicated(field_data[!is.na(field_data)]))
      if (duplicates > 0) {
        result$errors <- c(result$errors, paste("Field", field_name, "has", duplicates, "duplicate values (should be unique)"))
      }
    }
    
    # Check pattern constraint
    if (!is.null(field_def$constraints$pattern)) {
      pattern <- field_def$constraints$pattern
      non_empty_data <- field_data[!is.na(field_data) & !field_data %in% (schema_def$missingValues %||% c("", "NA", "N/A"))]
      
      if (length(non_empty_data) > 0) {
        matches <- grepl(pattern, non_empty_data)
        if (!all(matches)) {
          invalid_count <- sum(!matches)
          result$warnings <- c(result$warnings, paste("Field", field_name, "has", invalid_count, "values not matching pattern:", pattern))
          
          # Show some examples
          invalid_examples <- non_empty_data[!matches][1:min(3, sum(!matches))]
          result$info <- c(result$info, paste("Examples of invalid", field_name, "values:", paste(invalid_examples, collapse = ", ")))
        }
      }
    }
  }
  
  return(result)
}

#' Validate Pattern Fields
validate_patterns <- function(data, schema_def) {
  result <- list(errors = character(0), warnings = character(0), info = character(0))
  
  for (field_def in schema_def$fields) {
    field_name <- field_def$name
    
    if (!field_name %in% names(data) || is.null(field_def$patterns)) {
      next
    }
    
    field_data <- data[[field_name]]
    non_empty_data <- field_data[!is.na(field_data) & !field_data %in% (schema_def$missingValues %||% c("", "NA", "N/A"))]
    
    if (length(non_empty_data) == 0) next
    
    # Check each value against all patterns
    for (value in unique(non_empty_data)) {
      matched <- FALSE
      
      for (pattern_def in field_def$patterns) {
        if (grepl(pattern_def$pattern, value)) {
          matched <- TRUE
          break
        }
      }
      
      if (!matched) {
        result$warnings <- c(result$warnings, paste("Value in", field_name, "doesn't match any known pattern:", value))
      }
    }
  }
  
  return(result)
}

#' Validate Enum Fields
validate_enums <- function(data, schema_def, full_schema) {
  result <- list(errors = character(0), warnings = character(0), info = character(0))
  
  # Check schema-level enums
  for (field_def in schema_def$fields) {
    field_name <- field_def$name
    
    if (!field_name %in% names(data) || is.null(field_def$constraints$enum)) {
      next
    }
    
    field_data <- data[[field_name]]
    non_empty_data <- field_data[!is.na(field_data) & !field_data %in% (schema_def$missingValues %||% c("", "NA", "N/A"))]
    
    if (length(non_empty_data) == 0) next
    
    valid_values <- field_def$constraints$enum
    invalid_values <- setdiff(unique(non_empty_data), valid_values)
    
    if (length(invalid_values) > 0) {
      result$warnings <- c(result$warnings, paste("Field", field_name, "has invalid enum values:", paste(invalid_values, collapse = ", ")))
      result$info <- c(result$info, paste("Valid values for", field_name, ":", paste(valid_values, collapse = ", ")))
    }
  }
  
  # Check CCHS-specific enums
  if (!is.null(full_schema$cchs_metadata$field_enums)) {
    cchs_enums <- full_schema$cchs_metadata$field_enums
    
    for (enum_field in names(cchs_enums)) {
      if (enum_field %in% names(data)) {
        field_data <- data[[enum_field]]
        non_empty_data <- field_data[!is.na(field_data) & !field_data %in% (schema_def$missingValues %||% c("", "NA", "N/A"))]
        
        if (length(non_empty_data) > 0) {
          valid_values <- cchs_enums[[enum_field]]$values
          invalid_values <- setdiff(unique(non_empty_data), valid_values)
          
          if (length(invalid_values) > 0) {
            result$info <- c(result$info, paste("Field", enum_field, "has values not in CCHS enum:", paste(invalid_values, collapse = ", ")))
          }
        }
      }
    }
  }
  
  return(result)
}

#' Validate Cross-Field Rules
validate_cross_field_rules <- function(data, schema_def) {
  result <- list(errors = character(0), warnings = character(0), info = character(0))
  
  if (is.null(schema_def$validation_rules$cross_field)) {
    return(result)
  }
  
  for (rule in schema_def$validation_rules$cross_field) {
    # Simple implementation - would need more sophisticated expression evaluation for production
    rule_name <- rule$rule
    condition <- rule$condition
    level <- rule$level %||% "warning"
    message <- rule$message %||% paste("Cross-field validation failed:", rule_name)
    
    # Basic pattern matching for common conditions
    if (grepl("variableType.*==.*Continuous", condition)) {
      continuous_rows <- data$variableType == "Continuous" & !is.na(data$variableType)
      if (any(continuous_rows) && "units" %in% names(data)) {
        missing_units <- continuous_rows & (is.na(data$units) | data$units %in% c("", "N/A"))
        if (any(missing_units)) {
          count <- sum(missing_units)
          msg <- paste("Found", count, "continuous variables without units specified")
          
          if (level == "error") {
            result$errors <- c(result$errors, msg)
          } else if (level == "warning") {
            result$warnings <- c(result$warnings, msg)
          } else {
            result$info <- c(result$info, msg)
          }
        }
      }
    }
    
    if (grepl("typeEnd.*==.*cat", condition)) {
      categorical_rows <- data$typeEnd == "cat" & !is.na(data$typeEnd)
      if (any(categorical_rows) && "dummyVariable" %in% names(data)) {
        missing_dummy <- categorical_rows & (is.na(data$dummyVariable) | data$dummyVariable %in% c("", "N/A"))
        if (any(missing_dummy)) {
          count <- sum(missing_dummy)
          msg <- paste("Found", count, "categorical variables without dummyVariable specified")
          
          if (level == "error") {
            result$errors <- c(result$errors, msg)
          } else if (level == "warning") {
            result$warnings <- c(result$warnings, msg)
          } else {
            result$info <- c(result$info, msg)
          }
        }
      }
    }
  }
  
  return(result)
}

#' Print Schema Validation Results
#'
#' @param x schema_validation_result object
#' @param ... Additional arguments (unused)
print.schema_validation_result <- function(x, ...) {
  cat("Schema Validation Report\n")
  cat("========================\n")
  cat("File:", x$csv_file, "\n")
  cat("Schema:", x$schema_file, "(", x$schema_type, ")\n")
  cat("Mode:", x$mode, "\n")
  cat("Timestamp:", format(x$timestamp), "\n\n")
  
  cat("Data Summary:\n")
  cat("- Rows:", x$total_rows, "\n")
  cat("- Columns:", x$total_columns, "\n\n")
  
  cat("Validation Summary:\n")
  cat("- Valid:", ifelse(x$valid, "YES", "NO"), "\n")
  cat("- Errors:", length(x$errors), "\n")
  cat("- Warnings:", length(x$warnings), "\n")
  cat("- Info:", length(x$info), "\n\n")
  
  if (length(x$errors) > 0) {
    cat("ERRORS:\n")
    for (i in seq_along(x$errors)) {
      cat(sprintf("%d. %s\n", i, x$errors[i]))
    }
    cat("\n")
  }
  
  if (length(x$warnings) > 0) {
    cat("WARNINGS:\n")
    for (i in seq_along(x$warnings)) {
      cat(sprintf("%d. %s\n", i, x$warnings[i]))
    }
    cat("\n")
  }
  
  if (length(x$info) > 0) {
    cat("INFO:\n")
    for (i in seq_along(x$info)) {
      cat(sprintf("%d. %s\n", i, x$info[i]))
    }
    cat("\n")
  }
  
  invisible(x)
}

#' Comprehensive Schema Testing Suite
#'
#' Tests both schemas against real data and generates detailed reports
run_comprehensive_schema_tests <- function() {
  cat("=== Comprehensive Schema Validation Suite ===\n\n")
  
  # Test variables.csv
  cat("1. Testing variables.csv against variables.yaml schema\n")
  cat("-----------------------------------------------------\n")
  
  if (file.exists("variable sheets/variables.csv") && file.exists("inst/metadata/schemas/core/variables.yaml")) {
    result_vars_basic <- validate_csv_against_schema(
      "variable sheets/variables.csv",
      "inst/metadata/schemas/core/variables.yaml",
      mode = "basic"
    )
    print(result_vars_basic)
    
    cat("\n--- Pipeline Mode ---\n")
    result_vars_pipeline <- validate_csv_against_schema(
      "variable sheets/variables.csv",
      "inst/metadata/schemas/core/variables.yaml",
      mode = "pipeline"
    )
    print(result_vars_pipeline)
  } else {
    cat("Skipping variables.csv test - files not found\n")
  }
  
  cat("\n\n2. Testing variable_details.csv against variable_details.yaml schema\n")
  cat("-------------------------------------------------------------------\n")
  
  if (file.exists("variable sheets/variable_details.csv") && file.exists("inst/metadata/schemas/core/variable_details.yaml")) {
    result_details_basic <- validate_csv_against_schema(
      "variable sheets/variable_details.csv",
      "inst/metadata/schemas/core/variable_details.yaml",
      mode = "basic"
    )
    print(result_details_basic)
    
    cat("\n--- Pipeline Mode ---\n")
    result_details_pipeline <- validate_csv_against_schema(
      "variable sheets/variable_details.csv",
      "inst/metadata/schemas/core/variable_details.yaml",
      mode = "pipeline"
    )
    print(result_details_pipeline)
  } else {
    cat("Skipping variable_details.csv test - files not found\n")
  }
  
  cat("\n=== Test Suite Complete ===\n")
  
  # Return results for further analysis
  results <- list()
  if (exists("result_vars_basic")) results$variables_basic <- result_vars_basic
  if (exists("result_vars_pipeline")) results$variables_pipeline <- result_vars_pipeline
  if (exists("result_details_basic")) results$variable_details_basic <- result_details_basic
  if (exists("result_details_pipeline")) results$variable_details_pipeline <- result_details_pipeline
  
  return(invisible(results))
}

# Utility operator for null-coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

#' ============================================================================
#' RECODEFLOW YAML SCHEMA VALIDATION 
#' ============================================================================

#' Validate YAML Schema Files
#'
#' Comprehensive validation for recodeflow YAML schemas including syntax and regex patterns.
#' Validates any recodeflow YAML schema file including variables.yaml, variable_details.yaml,
#' metadata_registry.yaml, and database_metadata.yaml.
#' Uses stringr for cross-platform regex consistency (ICU engine)
#'
#' @param yaml_path Path to YAML file to validate
#' @return List with detailed validation results containing:
#'   \describe{
#'     \item{file}{Base filename of the validated schema}
#'     \item{path}{Full path to the schema file}
#'     \item{valid}{Boolean indicating overall validation success}
#'     \item{errors}{Character vector of validation errors found}
#'     \item{warnings}{Character vector of validation warnings}
#'     \item{patterns_tested}{Number of regex patterns tested}
#'     \item{patterns_valid}{Number of regex patterns that compiled successfully}
#'     \item{timestamp}{Validation timestamp}
#'     \item{regex_engine}{Engine used for pattern validation (stringr/ICU)}
#'     \item{schema_version}{Schema version if detected (e.g., "1.0.0")}
#'     \item{message}{Human-readable validation summary message}
#'   }
#'   This comprehensive result object enables detailed inspection of validation 
#'   outcomes and provides all necessary information for debugging schema issues.
#' @details
#' This function performs comprehensive validation of recodeflow YAML schema files:
#' 
#' **Purpose**: Ensures YAML schema files are syntactically correct and all embedded
#' regex patterns compile successfully across different platforms and R installations.
#' Critical for preventing runtime errors in data harmonization workflows.
#' 
#' **What is validated**:
#' \itemize{
#'   \item YAML syntax and parseability using yaml::read_yaml()
#'   \item All regex patterns found in field constraints (pattern field)
#'   \item Transformation patterns in metadata_registry.yaml shared specifications
#'   \item Cross-platform regex compatibility using stringr/ICU engine
#'   \item Schema version detection and reporting
#' }
#' 
#' **Specific YAML files supported**:
#' \itemize{
#'   \item variables.yaml - Harmonized variable definitions and metadata
#'   \item variable_details.yaml - Value-level transformation rules and recoding logic
#'   \item metadata_registry.yaml - Central registry with shared transformation patterns
#'   \item database_metadata.yaml - Dublin Core compliant database metadata
#'   \item Any recodeflow-compatible YAML schema following the established structure
#' }
#' 
#' @examples
#' \dontrun{
#' # Validate core harmonization schema
#' result <- validate_yaml_schema("inst/metadata/schemas/core/variables.yaml")
#' print(result$message)  # "✓ variables.yaml - YAML syntax valid"
#' cat("Patterns tested:", result$patterns_tested, "Valid:", result$patterns_valid)
#' 
#' # Check for validation errors
#' if (!result$valid) {
#'   cat("Errors found:", paste(result$errors, collapse = "; "))
#' }
#' }
validate_yaml_schema <- function(yaml_path) {
  result <- list(
    file = basename(yaml_path),
    path = yaml_path,
    valid = FALSE,
    errors = character(),
    warnings = character(),
    patterns_tested = 0,
    patterns_valid = 0,
    timestamp = Sys.time(),
    regex_engine = "stringr/ICU",
    schema_version = NA
  )
  
  # Check file exists
  if (!file.exists(yaml_path)) {
    result$errors <- "File does not exist"
    result$message <- paste("✗", basename(yaml_path), "- File not found")
    return(result)
  }
  
  # Test YAML syntax and structure
  tryCatch({
    schema <- yaml::read_yaml(yaml_path)
    result$valid <- TRUE
    result$message <- paste("✓", basename(yaml_path), "- YAML syntax valid")
    
    # Extract schema version if available
    if ("schema_version" %in% names(schema)) {
      result$schema_version <- schema$schema_version
    }
    
    # Test regex patterns with stringr for consistency
    pattern_results <- validate_recodeflow_patterns(schema, yaml_path)
    result$patterns_tested <- pattern_results$patterns_tested
    result$patterns_valid <- pattern_results$patterns_valid
    result$errors <- c(result$errors, pattern_results$errors)
    result$warnings <- c(result$warnings, pattern_results$warnings)
    
    # Overall validity
    result$valid <- result$valid && length(result$errors) == 0
    
  }, error = function(e) {
    result$errors <- e$message
    result$message <- paste("✗", basename(yaml_path), "- YAML syntax error:", e$message)
  })
  
  return(result)
}

#' Validate Recodeflow Schema Patterns
#'
#' Tests regex patterns from recodeflow schemas including metadata_registry patterns
#' @param schema Parsed YAML object  
#' @param yaml_path Path for error reporting
#' @return List with pattern validation results
validate_recodeflow_patterns <- function(schema, yaml_path) {
  result <- list(
    patterns_tested = 0,
    patterns_valid = 0,
    errors = character(),
    warnings = character()
  )
  
  # Extract patterns from recodeflow schema structure
  patterns <- extract_recodeflow_patterns(schema)
  result$patterns_tested <- length(patterns)
  
  if (length(patterns) > 0) {
    for (i in seq_along(patterns)) {
      pattern <- patterns[i]
      
      tryCatch({
        # Test pattern compilation with stringr (ICU engine)
        stringr::str_detect("test", pattern)
        result$patterns_valid <- result$patterns_valid + 1
        
      }, error = function(e) {
        result$errors <- c(result$errors, 
          paste("Invalid regex pattern in", basename(yaml_path), ":", pattern, "-", e$message))
      })
    }
  }
  
  return(result)
}

#' Extract Regex Patterns from Recodeflow Schema Structure
#'
#' Specialized extraction for recodeflow YAML schemas including metadata_registry patterns
#' @param schema Parsed YAML object from recodeflow schema
#' @return Character vector of regex patterns found
extract_recodeflow_patterns <- function(schema) {
  patterns <- character()
  
  # Extract from metadata_registry.yaml structure
  if ("shared_specifications" %in% names(schema)) {
    specs <- schema$shared_specifications
    
    # Extract validation patterns
    if ("validation_patterns" %in% names(specs)) {
      patterns <- c(patterns, extract_pattern_values(specs$validation_patterns))
    }
  }
  
  # Extract from variables.yaml or variable_details.yaml structure  
  if ("variables_schema" %in% names(schema)) {
    patterns <- c(patterns, extract_field_patterns(schema$variables_schema))
  }
  
  if ("variable_details_schema" %in% names(schema)) {
    patterns <- c(patterns, extract_field_patterns(schema$variable_details_schema))
  }
  
  # Fallback: general pattern extraction for any remaining patterns
  patterns <- c(patterns, extract_general_patterns(schema))
  
  return(unique(patterns))
}

#' Extract Pattern Values from Registry Structure
#' @param patterns_section Pattern section from metadata_registry
#' @return Character vector of patterns
extract_pattern_values <- function(patterns_section) {
  patterns <- character()
  
  if (is.list(patterns_section)) {
    for (item in patterns_section) {
      if (is.list(item) && "pattern" %in% names(item)) {
        patterns <- c(patterns, item$pattern)
      } else if (is.character(item)) {
        # Direct pattern values
        if (grepl("\\^|\\$|\\[|\\]|\\*|\\+|\\?|\\{|\\}", item) ||
            grepl("\\\\d|\\\\w|\\\\s", item)) {
          patterns <- c(patterns, item)
        }
      }
    }
  }
  
  return(patterns)
}

#' Extract Patterns from Schema Field Definitions
#' @param schema_section Schema section containing fields
#' @return Character vector of patterns
extract_field_patterns <- function(schema_section) {
  patterns <- character()
  
  if ("fields" %in% names(schema_section)) {
    for (field in schema_section$fields) {
      if ("constraints" %in% names(field)) {
        constraints <- field$constraints
        if ("pattern" %in% names(constraints)) {
          patterns <- c(patterns, constraints$pattern)
        }
      }
    }
  }
  
  return(patterns)
}

#' General Pattern Extraction (Fallback)
#' @param obj YAML object
#' @return Character vector of patterns
extract_general_patterns <- function(obj) {
  patterns <- character()
  
  if (is.list(obj)) {
    for (name in names(obj)) {
      item <- obj[[name]]
      # Only extract from fields explicitly named 'pattern' or ending with 'pattern'
      if (grepl("pattern$", name, ignore.case = TRUE) && is.character(item)) {
        patterns <- c(patterns, item)
      } else if (is.list(item)) {
        patterns <- c(patterns, extract_general_patterns(item))
      }
    }
  }
  
  return(patterns)
}

#' Get Schema Paths from Registry
#'
#' Reads schema file locations from metadata_registry.yaml configuration to enable 
#' flexible file organization and centralized path management. Uses registry 
#' full_path specifications with fallback to default locations.
#' @return List with organized schema paths (core_schemas, registry_schemas) 
#'   enabling dynamic file location management across the package
get_schema_paths <- function() {
  registry_path <- "inst/metadata/documentation/metadata_registry.yaml"
  
  # Default paths in case registry is not available
  default_paths <- list(
    core_schemas = c(
      "inst/metadata/schemas/core/variables.yaml",
      "inst/metadata/schemas/core/variable_details.yaml"
    ),
    registry_schemas = c(
      "inst/metadata/documentation/metadata_registry.yaml",
      "inst/metadata/documentation/database_metadata.yaml"
    )
  )
  
  # Try to read from registry
  if (file.exists(registry_path)) {
    tryCatch({
      registry <- yaml::read_yaml(registry_path)
      
      if ("schema_registry" %in% names(registry)) {
        schema_reg <- registry$schema_registry
        
        # Extract paths from registry
        paths <- list(
          core_schemas = character(),
          registry_schemas = character()
        )
        
        # Core harmonization schemas
        if ("harmonization_schemas" %in% names(schema_reg)) {
          for (schema in schema_reg$harmonization_schemas) {
            if ("full_path" %in% names(schema)) {
              paths$core_schemas <- c(paths$core_schemas, schema$full_path)
            }
          }
        }
        
        # Supporting schemas
        if ("supporting_schemas" %in% names(schema_reg)) {
          for (schema in schema_reg$supporting_schemas) {
            if ("full_path" %in% names(schema)) {
              paths$registry_schemas <- c(paths$registry_schemas, schema$full_path)
            }
          }
        }
        
        return(paths)
      }
    }, error = function(e) {
      cat("Warning: Could not read schema paths from registry, using defaults\n")
    })
  }
  
  return(default_paths)
}

#' Core Schema Validation
#'
#' Tests the two critical schemas required for data harmonization workflows 
#' (variables.yaml and variable_details.yaml) using cross-platform regex validation.
#' Validates master variable definitions and detailed transformation rules.
#' @return Named list containing validation results for both core schemas with 
#'   summary statistics and console output for immediate feedback
validate_core_schemas <- function() {
  cat("=== Core Schema Validation ===\n")
  cat("Testing essential recodeflow schemas with stringr (ICU) for cross-platform consistency\n\n")
  
  # Get schema paths from registry
  schema_paths <- get_schema_paths()
  core_schemas <- schema_paths$core_schemas
  
  results <- list()
  
  for (schema_path in core_schemas) {
    cat("Testing:", basename(schema_path), "... ")
    result <- validate_yaml_schema(schema_path)
    results[[basename(schema_path)]] <- result
    
    # Print immediate feedback
    if (result$valid) {
      cat("✓ PASS")
      if (result$patterns_tested > 0) {
        cat(" (", result$patterns_valid, "/", result$patterns_tested, " patterns valid)", sep = "")
      }
      if (!is.na(result$schema_version)) {
        cat(" [v", result$schema_version, "]", sep = "")
      }
      cat("\n")
    } else {
      cat("✗ FAIL\n")
      for (error in result$errors) {
        cat("  ERROR:", error, "\n")
      }
    }
  }
  
  # Summary
  total_files <- length(results)
  valid_files <- sum(sapply(results, `[[`, "valid"))
  
  cat("\nCore Schema Summary:\n")
  cat("Files tested:", total_files, "\n")
  cat("Valid syntax:", valid_files, "/", total_files, "\n")
  cat("Regex engine: stringr/ICU for cross-platform consistency\n")
  
  return(results)
}

#' Registry and Supporting Schema Validation
#'
#' Tests metadata registry and supporting documentation schemas that provide 
#' shared specifications and database configurations (metadata_registry.yaml 
#' and database_metadata.yaml). Validates central transformation patterns and Dublin Core metadata.
#' @return Named list with validation results for registry schemas plus summary 
#'   statistics for monitoring supporting infrastructure health
validate_registry_schemas <- function() {
  cat("\n=== Registry & Supporting Schema Validation ===\n")
  cat("Testing metadata registry and supporting schema files\n\n")
  
  # Registry and supporting schemas
  registry_schemas <- c(
    "scope-docs/metadata-schema/metadata_registry.yaml",
    "scope-docs/metadata-schema/database_metadata.yaml"
  )
  
  results <- list()
  
  for (schema_path in registry_schemas) {
    if (file.exists(schema_path)) {
      cat("Testing:", basename(schema_path), "... ")
      result <- validate_yaml_schema(schema_path)
      results[[basename(schema_path)]] <- result
      
      # Print immediate feedback
      if (result$valid) {
        cat("✓ PASS")
        if (result$patterns_tested > 0) {
          cat(" (", result$patterns_valid, "/", result$patterns_tested, " patterns valid)", sep = "")
        }
        if (!is.na(result$schema_version)) {
          cat(" [v", result$schema_version, "]", sep = "")
        }
        cat("\n")
      } else {
        cat("✗ FAIL\n")
        for (error in result$errors) {
          cat("  ERROR:", error, "\n")
        }
      }
    } else {
      cat("Skipping:", basename(schema_path), "(file not found)\n")
    }
  }
  
  # Summary
  total_files <- length(results)
  valid_files <- sum(sapply(results, `[[`, "valid"))
  
  cat("\nRegistry Schema Summary:\n")
  cat("Files tested:", total_files, "\n")
  cat("Valid syntax:", valid_files, "/", total_files, "\n")
  
  return(results)
}

#' CCHS Extension Schema Validation
#'
#' Validate CCHS-specific extension schemas (optional)
#' @return List of validation results  
validate_cchs_extension_schemas <- function() {
  cat("\n=== CCHS Extension Schema Validation ===\n")
  cat("Testing CCHS-specific extension schemas\n\n")
  
  # CCHS extension schemas
  cchs_schemas <- c(
    "scope-docs/metadata-schema/variables_cchs_example.yaml",
    "scope-docs/metadata-schema/variable_details_cchs_example.yaml"
  )
  
  results <- list()
  
  for (schema_path in cchs_schemas) {
    if (file.exists(schema_path)) {
      cat("Testing:", basename(schema_path), "... ")
      result <- validate_yaml_schema(schema_path)
      results[[basename(schema_path)]] <- result
      
      # Print immediate feedback
      if (result$valid) {
        cat("✓ PASS")
        if (result$patterns_tested > 0) {
          cat(" (", result$patterns_valid, "/", result$patterns_tested, " patterns valid)", sep = "")
        }
        if (!is.na(result$schema_version)) {
          cat(" [v", result$schema_version, "]", sep = "")
        }
        cat("\n")
      } else {
        cat("✗ FAIL\n")
        for (error in result$errors) {
          cat("  ERROR:", error, "\n")
        }
      }
    } else {
      cat("Skipping:", basename(schema_path), "(file not found)\n")
    }
  }
  
  # Summary
  total_files <- length(results)
  valid_files <- sum(sapply(results, `[[`, "valid"))
  
  cat("\nCCHS Extension Summary:\n")
  cat("Files tested:", total_files, "\n")
  cat("Valid syntax:", valid_files, "/", total_files, "\n")
  
  return(results)
}

#' Complete Recodeflow Schema Validation
#'
#' Run comprehensive validation of all recodeflow YAML schemas
#' @param include_registry Include metadata registry validation (default: TRUE)
#' @param include_cchs_extensions Include CCHS extension validation (default: FALSE)
#' @return Combined results from all validation phases
run_recodeflow_schema_validation <- function(include_registry = TRUE, include_cchs_extensions = FALSE) {
  cat("Starting Recodeflow Schema Validation with stringr for regex consistency...\n\n")
  
  # Core schemas first - these are essential
  core_results <- validate_core_schemas()
  
  # Check if core validation succeeded
  core_all_valid <- all(sapply(core_results, `[[`, "valid"))
  
  results <- list(
    core_schemas = core_results,
    registry_schemas = list(),
    cchs_extensions = list(),
    summary = list(
      core_schemas_valid = core_all_valid,
      registry_schemas_tested = 0,
      cchs_extensions_tested = 0,
      regex_engine = "stringr/ICU",
      timestamp = Sys.time()
    )
  )
  
  if (core_all_valid) {
    # Registry validation if requested
    if (include_registry) {
      registry_results <- validate_registry_schemas()
      results$registry_schemas <- registry_results
      results$summary$registry_schemas_tested <- length(registry_results)
    }
    
    # CCHS extensions if requested
    if (include_cchs_extensions) {
      cchs_results <- validate_cchs_extension_schemas()
      results$cchs_extensions <- cchs_results
      results$summary$cchs_extensions_tested <- length(cchs_results)
    }
    
  } else {
    cat("\n⚠️  Core schemas failed - skipping additional validations\n")
    cat("Fix core schema issues before testing registry and extensions.\n")
  }
  
  cat("\n=== Recodeflow Schema Validation Complete ===\n")
  
  # Overall summary
  total_files <- length(results$core_schemas) + 
                length(results$registry_schemas) + 
                length(results$cchs_extensions)
  
  valid_files <- sum(sapply(results$core_schemas, `[[`, "valid"))
  
  if (length(results$registry_schemas) > 0) {
    valid_files <- valid_files + sum(sapply(results$registry_schemas, `[[`, "valid"))
  }
  
  if (length(results$cchs_extensions) > 0) {
    valid_files <- valid_files + sum(sapply(results$cchs_extensions, `[[`, "valid"))
  }
  
  cat("Total files validated:", total_files, "\n")
  cat("Files passing validation:", valid_files, "/", total_files, "\n")
  cat("Core schemas status:", ifelse(core_all_valid, "✅ PASS", "❌ FAIL"), "\n")
  
  return(results)
}

#' Quick Core Schema Validation
#'
#' Validate only the essential schemas (variables.yaml and variable_details.yaml)
#' @return Results from core schema validation
validate_core_schemas_only <- function() {
  cat("Quick validation of core recodeflow schemas...\n\n")
  
  core_results <- validate_core_schemas()
  core_all_valid <- all(sapply(core_results, `[[`, "valid"))
  
  cat("\n=== Quick Validation Complete ===\n")
  cat("Core schemas status:", ifelse(core_all_valid, "✅ PASS", "❌ FAIL"), "\n")
  
  return(core_results)
}