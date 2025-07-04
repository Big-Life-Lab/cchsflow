# Internal utility functions for CSV standardisation
# Not exported - used internally by standardise_csv()

# Basic validation for training wheels mode
.validate_basic_format <- function(data, file_path) {
  issues <- character(0)

  # Detect file type and required fields
  file_type <- .detect_csv_type(file_path)
  if (file_type == "variables") {
    required_fields <- c("variable", "label", "variableType", "databaseStart", "variableStart")
  } else if (file_type == "variable_details") {
    required_fields <- c("variable", "typeEnd", "databaseStart", "variableStart", "variableStartLabel")
  } else {
    # Unknown file type - minimal validation
    required_fields <- character(0)
  }

  # Check for missing required fields
  missing_fields <- required_fields[!required_fields %in% names(data)]
  if (length(missing_fields) > 0) {
    issues <- c(issues, paste("Missing required fields:", paste(missing_fields, collapse = ", ")))
  }

  return(list(issues = issues))
}

# Enhanced validation for collaboration mode
.validate_enhanced_format <- function(data, file_path) {
  # Start with basic validation
  basic_result <- .validate_basic_format(data, file_path)
  issues <- basic_result$issues

  # Load and apply YAML schema validation
  tryCatch(
    {
      schema <- .load_csv_schema(file_path)
      schema_issues <- .validate_against_schema(data, schema, file_path)
      issues <- c(issues, schema_issues)
    },
    error = function(e) {
      issues <- c(issues, paste("Schema validation error:", e$message))
    }
  )

  return(list(issues = issues))
}

# Apply standardisation fixes to file
.apply_standardisation <- function(file_path) {
  issues_fixed <- character(0)

  # Read file as binary to detect and fix encoding issues
  file_size <- file.info(file_path)$size
  file_content <- readBin(file_path, "raw", n = file_size)

  # Check and remove UTF-8 BOM if present (EF BB BF)
  if (length(file_content) >= 3 &&
    file_content[1] == as.raw(0xEF) &&
    file_content[2] == as.raw(0xBB) &&
    file_content[3] == as.raw(0xBF)) {
    file_content <- file_content[-(1:3)]
    issues_fixed <- c(issues_fixed, "Removed UTF-8 BOM")
  }

  # Convert to character and standardise line endings
  text_content <- rawToChar(file_content)

  # Convert CRLF -> LF
  if (grepl("\r\n", text_content)) {
    text_content <- gsub("\r\n", "\n", text_content)
    issues_fixed <- c(issues_fixed, "Converted CRLF to LF line endings")
  }

  # Convert CR -> LF (old Mac)
  if (grepl("\r", text_content)) {
    text_content <- gsub("\r", "\n", text_content)
    issues_fixed <- c(issues_fixed, "Converted CR to LF line endings")
  }

  # Ensure proper final newline
  if (!grepl("\n$", text_content)) {
    text_content <- paste0(text_content, "\n")
    issues_fixed <- c(issues_fixed, "Added final newline")
  }

  # Standardize column order if schema is available
  column_fixes <- .standardize_column_order(file_path)
  issues_fixed <- c(issues_fixed, column_fixes)

  # Write back with standardised encoding (only if no column fixes were applied)
  if (length(column_fixes) == 0) {
    writeLines(strsplit(text_content, "\n")[[1]], file_path, useBytes = TRUE)
  }

  return(issues_fixed)
}

# Detect CSV file type based on filename patterns
.detect_csv_type <- function(file_path) {
  # Extract filename from path
  filename <- basename(file_path)

  # Check for variable_details patterns (more specific, check first)
  if (grepl("variable_details.*\\.csv$", filename, ignore.case = TRUE) ||
    grepl(".*variable_details.*\\.csv$", filename, ignore.case = TRUE)) {
    return("variable_details")
  }

  # Check for variables patterns
  if (grepl("variables.*\\.csv$", filename, ignore.case = TRUE) ||
    grepl(".*variables.*\\.csv$", filename, ignore.case = TRUE)) {
    return("variables")
  }

  # Unknown type
  return("unknown")
}

# Load YAML schema for CSV file validation
.load_csv_schema <- function(file_path) {
  # Detect file type
  file_type <- .detect_csv_type(file_path)

  # Determine schema file path
  schema_file <- if (file_type == "variables") {
    "inst/metadata/schemas/core/variables.yaml"
  } else if (file_type == "variable_details") {
    "inst/metadata/schemas/core/variable_details.yaml"
  } else {
    stop("Unknown CSV file type for schema loading")
  }

  # Check if schema file exists
  if (!file.exists(schema_file)) {
    stop(paste("Schema file not found:", schema_file))
  }

  # Load YAML schema
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("yaml package required for schema validation. Install with: install.packages('yaml')")
  }

  schema <- yaml::read_yaml(schema_file)
  return(schema)
}

# Validate data against YAML schema
.validate_against_schema <- function(data, schema, file_path) {
  issues <- character(0)
  file_type <- .detect_csv_type(file_path)

  # Get schema section based on file type
  schema_def <- if (file_type == "variables") {
    schema$variables_schema
  } else if (file_type == "variable_details") {
    schema$variable_details_schema
  } else {
    return(c("Unknown file type for schema validation"))
  }

  # Validate field types and constraints
  if (!is.null(schema_def$fields)) {
    for (field in schema_def$fields) {
      field_name <- field$name

      # Skip if field not present and not required
      if (!field_name %in% names(data)) {
        if (isTRUE(field$constraints$required)) {
          issues <- c(issues, paste("Required field missing:", field_name))
        }
        next
      }

      # Get column data
      col_data <- data[[field_name]]

      # Validate enum constraints
      if (!is.null(field$constraints$enum)) {
        invalid_values <- col_data[!col_data %in% c(field$constraints$enum, "", "NA", "N/A")]
        if (length(invalid_values) > 0) {
          unique_invalid <- unique(invalid_values[!is.na(invalid_values)])
          if (length(unique_invalid) > 0) {
            issues <- c(issues, paste0(
              "Invalid values in '", field_name, "': ",
              paste(unique_invalid, collapse = ", "),
              ". Valid values: ",
              paste(field$constraints$enum, collapse = ", ")
            ))
          }
        }
      }

      # Validate pattern constraints
      if (!is.null(field$constraints$pattern)) {
        pattern_violations <- col_data[!is.na(col_data) & col_data != "" &
          !grepl(field$constraints$pattern, col_data)]
        if (length(pattern_violations) > 0) {
          unique_violations <- unique(pattern_violations)
          if (length(unique_violations) > 0) {
            issues <- c(issues, paste0(
              "Pattern violations in '", field_name, "': ",
              paste(unique_violations, collapse = ", ")
            ))
          }
        }
      }
    }
  }

  # Validate cross-field dependencies
  if (!is.null(schema_def$validation_rules$cross_field)) {
    for (rule in schema_def$validation_rules$cross_field) {
      cross_field_issues <- .validate_cross_field_rule(data, rule)
      issues <- c(issues, cross_field_issues)
    }
  }

  # Validate column order
  column_order_issues <- .validate_column_order(data, schema_def, file_path)
  issues <- c(issues, column_order_issues)

  # Validate column completeness (missing/additional columns)
  column_completeness_issues <- .validate_column_completeness(data, schema_def, file_path)
  issues <- c(issues, column_completeness_issues)

  return(issues)
}

# Validate cross-field dependency rules
.validate_cross_field_rule <- function(data, rule) {
  issues <- character(0)

  # Simple implementation for key rules
  if (rule$rule == "units_required_for_continuous") {
    if ("variableType" %in% names(data) && "units" %in% names(data)) {
      continuous_vars <- data$variableType == "Continuous"
      missing_units <- is.na(data$units) | data$units == "" | data$units == "N/A"
      violations <- continuous_vars & missing_units

      if (any(violations, na.rm = TRUE)) {
        var_names <- data$variable[violations]
        var_names <- var_names[!is.na(var_names)]
        if (length(var_names) > 0) {
          issues <- c(issues, paste(
            "Warning: Continuous variables should have units specified:",
            paste(var_names, collapse = ", ")
          ))
        }
      }
    }
  }

  return(issues)
}

# Validate column order against schema expectations
.validate_column_order <- function(data, schema_def, file_path) {
  issues <- character(0)

  if (is.null(schema_def$expected_column_order)) {
    return(issues) # No expected order defined
  }

  current_columns <- names(data)
  expected_order <- schema_def$expected_column_order

  # Only check columns that exist in both current data and expected order
  existing_expected_cols <- expected_order[expected_order %in% current_columns]
  existing_current_order <- current_columns[current_columns %in% expected_order]

  # Check if current order matches expected order for existing columns
  if (!identical(existing_current_order, existing_expected_cols)) {
    issues <- c(issues, paste(
      "Column order doesn't match schema. Expected order for existing columns:",
      paste(existing_expected_cols, collapse = ", "),
      ". Current order:",
      paste(existing_current_order, collapse = ", ")
    ))
  }

  return(issues)
}

# Validate column completeness (missing optional, additional columns)
.validate_column_completeness <- function(data, schema_def, file_path) {
  issues <- character(0)

  if (is.null(schema_def$fields)) {
    return(issues) # No schema fields defined
  }

  current_columns <- names(data)
  schema_columns <- sapply(schema_def$fields, function(field) field$name)

  # Find additional columns (present in data but not in schema)
  additional_columns <- current_columns[!current_columns %in% schema_columns]
  if (length(additional_columns) > 0) {
    issues <- c(issues, paste(
      "Additional columns not in schema:",
      paste(additional_columns, collapse = ", "),
      ". These may be user-defined extensions."
    ))
  }

  # Find missing optional columns (good to know but not errors)
  optional_fields <- schema_def$fields[sapply(schema_def$fields, function(field) {
    !isTRUE(field$constraints$required)
  })]

  if (length(optional_fields) > 0) {
    optional_names <- sapply(optional_fields, function(field) field$name)
    missing_optional <- optional_names[!optional_names %in% current_columns]

    if (length(missing_optional) > 0) {
      # Group by usefulness
      recommended_fields <- c("subject", "section", "units", "notes", "description")
      missing_recommended <- missing_optional[missing_optional %in% recommended_fields]
      missing_other <- missing_optional[!missing_optional %in% recommended_fields]

      if (length(missing_recommended) > 0) {
        issues <- c(issues, paste(
          "Recommended optional columns missing:",
          paste(missing_recommended, collapse = ", "),
          ". Consider adding for better documentation."
        ))
      }

      if (length(missing_other) > 0) {
        issues <- c(issues, paste(
          "Other optional columns available:",
          paste(missing_other, collapse = ", "),
          ". See schema for details."
        ))
      }
    }
  }

  return(issues)
}

# Standardize column order according to schema
.standardize_column_order <- function(file_path) {
  issues_fixed <- character(0)

  tryCatch(
    {
      # Load schema to get expected column order
      schema <- .load_csv_schema(file_path)
      file_type <- .detect_csv_type(file_path)

      # Get expected column order from schema
      schema_def <- if (file_type == "variables") {
        schema$variables_schema
      } else if (file_type == "variable_details") {
        schema$variable_details_schema
      } else {
        return(character(0)) # Unknown type, no reordering
      }

      if (is.null(schema_def$expected_column_order)) {
        return(character(0)) # No expected order defined
      }

      # Read current CSV data
      data <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
      current_columns <- names(data)
      expected_order <- schema_def$expected_column_order

      # Check if reordering is needed
      # Only reorder columns that exist in both current data and expected order
      existing_expected_cols <- expected_order[expected_order %in% current_columns]
      existing_current_order <- current_columns[current_columns %in% expected_order]

      # Check if current order matches expected order for existing columns
      if (!identical(existing_current_order, existing_expected_cols)) {
        # Reorder columns: expected columns first, then any additional columns
        additional_cols <- current_columns[!current_columns %in% expected_order]
        new_order <- c(existing_expected_cols, additional_cols)

        # Reorder the data frame
        data_reordered <- data[, new_order, drop = FALSE]

        # Write back the reordered data
        write.csv(data_reordered, file_path, row.names = FALSE, quote = TRUE, na = "")

        issues_fixed <- c(issues_fixed, "Standardized column order")
      }
    },
    error = function(e) {
      # If column ordering fails, don't break the standardization process
      # Just continue without column ordering
    }
  )

  return(issues_fixed)
}
