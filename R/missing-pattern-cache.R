# ==============================================================================
# Level 4: Missing Pattern Cache System (Database-Agnostic)
# ==============================================================================
#
# Session-level cache system for missing data patterns that works with any database
# by using configuration files instead of hard-coded CCHS-specific logic.
#
# DEPENDENCY LEVEL: 4 (Session Cache - depends on Levels 1-3)
# DEPENDS ON: Level 1 (file-sourcing.R), Level 2A (database-config-loader.R), Level 3 (worksheet-getters.R)
# 
# FUNCTIONS IN THIS FILE:
# - Core cache: has_cached_pattern(), get_cached_pattern(), cache_pattern(), clear_missing_patterns_cache()
# - Database detection: auto_detect_database(), apply_database_heuristics()
# - Pattern extraction: get_missing_pattern(), get_missing_pattern_vector(), get_missing_pattern_bulk()
# - Helper functions: parse_database_lists(), extract_codes_for_pattern()
#
# USED BY LEVEL 5 FUNCTIONS:
# - any_missing() and get_priority_missing() (uses get_missing_pattern)
# - Missing data processing functions
#
# DEVELOPMENT STATUS: Level 4 session cache system (restored missing functions)
# LOCATION: development/flexible-missing-data-mvp/R/missing-pattern-cache.R
# VERSION: v4.0.0, updated 2025-08-03

# Package-level caches
.missing_patterns_cache <- new.env(parent = emptyenv())
.database_warnings_cache <- new.env(parent = emptyenv())

# ==============================================================================
# CORE CACHE INFRASTRUCTURE FUNCTIONS
# ==============================================================================

#' Check if Missing Pattern is Cached
#'
#' Fast O(1) check for whether a missing pattern is already cached
#' for a specific variable-database combination.
#'
#' @param variable_name Character. Variable name to check
#' @param database Character. Database name (required for cache lookup)
#' @return Logical. TRUE if pattern is cached, FALSE otherwise
#' @export
has_cached_pattern <- function(variable_name, database) {
  
  # Input validation
  if (is.null(database)) {
    stop("Database parameter is required for missing pattern cache lookup. ",
         "Database should be the CCHS cycle/type (e.g., 'cchs2001_p', 'cchs2015_p'). ",
         "Use get_variable_details('", variable_name, "') to see available databases.")
  }
  
  if (length(variable_name) != 1 || length(database) != 1) {
    stop("has_cached_pattern() requires scalar inputs. ",
         "Use vectorized functions for multiple variables.")
  }
  
  # Check if variable exists in cache
  if (!exists(variable_name, envir = .missing_patterns_cache)) {
    return(FALSE)
  }
  
  # Check if database exists for this variable
  var_cache <- get(variable_name, envir = .missing_patterns_cache)
  return(database %in% names(var_cache))
}

#' Get Cached Missing Pattern
#'
#' Retrieves cached missing pattern for variable-database combination.
#' Assumes pattern exists - use has_cached_pattern() to check first.
#'
#' @param variable_name Character. Variable name
#' @param database Character. Database name
#' @return List with na_a_codes and na_b_codes elements
#' @export
get_cached_pattern <- function(variable_name, database) {
  
  # Input validation (basic - assumes has_cached_pattern() was called first)
  if (is.null(database)) {
    stop("Database parameter is required for cache retrieval. ",
         "Database should be the CCHS cycle/type (e.g., 'cchs2001_p', 'cchs2015_p').")
  }
  
  # Get variable cache
  if (!exists(variable_name, envir = .missing_patterns_cache)) {
    stop("Variable '", variable_name, "' not found in cache. Use has_cached_pattern() first.")
  }
  
  var_cache <- get(variable_name, envir = .missing_patterns_cache)
  
  # Get database pattern
  if (!database %in% names(var_cache)) {
    stop("Database '", database, "' not found for variable '", variable_name, "' in cache.")
  }
  
  return(var_cache[[database]])
}

#' Cache Missing Pattern
#'
#' Stores missing pattern in session-level cache for fast future retrieval.
#'
#' @param variable_name Character. Variable name
#' @param database Character. Database name  
#' @param pattern List. Pattern with na_a_codes and na_b_codes elements
#' @return Invisible TRUE on success
#' @export
cache_pattern <- function(variable_name, database, pattern) {
  
  # Input validation
  if (is.null(database)) {
    stop("Database parameter is required for caching. ",
         "Database should be the CCHS cycle/type (e.g., 'cchs2001_p', 'cchs2015_p').")
  }
  
  if (!is.list(pattern) || !all(c("na_a_codes", "na_b_codes") %in% names(pattern))) {
    stop("Pattern must be a list with 'na_a_codes' and 'na_b_codes' elements")
  }
  
  # Initialize variable entry if doesn't exist
  if (!exists(variable_name, envir = .missing_patterns_cache)) {
    assign(variable_name, list(), envir = .missing_patterns_cache)
  }
  
  # Get current variable cache
  var_cache <- get(variable_name, envir = .missing_patterns_cache)
  
  # Add database pattern
  var_cache[[database]] <- pattern
  
  # Store back to cache
  assign(variable_name, var_cache, envir = .missing_patterns_cache)
  
  invisible(TRUE)
}

#' Clear Missing Patterns Cache
#'
#' Clears all cached missing patterns. Useful for testing or memory management.
#'
#' @return Invisible TRUE
#' @export
clear_missing_patterns_cache <- function() {
  rm(list = ls(envir = .missing_patterns_cache), envir = .missing_patterns_cache)
  invisible(TRUE)
}

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Parse Database Lists  
#'
#' Parses comma-separated database lists from variable_details.csv format.
#'
#' @param database_lists Character vector. Database list strings
#' @return Character vector. Individual database names
#' @noRd
parse_database_lists <- function(database_lists) {
  
  # Remove NA and empty entries upfront  
  valid_lists <- database_lists[!is.na(database_lists) & nchar(trimws(database_lists)) > 0]
  
  if (length(valid_lists) == 0) return(character(0))
  
  # Vectorized splitting and processing
  all_databases <- unlist(strsplit(valid_lists, ",", fixed = TRUE))
  all_databases <- trimws(all_databases)
  all_databases <- all_databases[nchar(all_databases) > 0]
  
  # Return unique databases
  return(unique(all_databases))
}

#' Map recStart to recEnd with Special Terms Support
#'
#' Enhanced function that processes all recStart→recEnd mappings including 
#' special terms (copy, else, Func::) and missing data patterns.
#'
#' @param mapping_rows Data.frame. All rows with recStart/recEnd data
#' @return List with complete mapping structure including special terms
#' @noRd
map_recStart_to_recEnd <- function(mapping_rows) {
  
  if (nrow(mapping_rows) == 0) {
    return(list(
      na_a_codes = numeric(0),
      na_b_codes = numeric(0),
      copy_mappings = list(),
      else_mappings = list(),
      skip_entries = list()
    ))
  }
  
  # Initialize result structure
  result <- list(
    na_a_codes = numeric(0),      # Missing codes for NA::a (backward compatibility)
    na_b_codes = numeric(0),      # Missing codes for NA::b (backward compatibility)
    copy_mappings = list(),       # recStart → copy directives
    else_mappings = list(),       # else → recEnd mappings
    skip_entries = list()         # Func::, DerivedVar::, etc.
  )
  
  # Process each mapping row
  for (i in 1:nrow(mapping_rows)) {
    row <- mapping_rows[i, ]
    rec_start_raw <- as.character(row$recStart)
    rec_end_raw <- as.character(row$recEnd)
    
    # Skip if either is NA or empty
    if (is.na(rec_start_raw) || is.na(rec_end_raw) || 
        nchar(trimws(rec_start_raw)) == 0 || nchar(trimws(rec_end_raw)) == 0) {
      next
    }
    
    rec_start <- trimws(rec_start_raw)
    rec_end <- trimws(rec_end_raw)
    
    # Handle skip terms (Func::, DerivedVar::, min, max)
    if (should_skip_term(rec_start) || should_skip_term(rec_end)) {
      result$skip_entries[[length(result$skip_entries) + 1]] <- list(
        recStart = rec_start,
        recEnd = rec_end,
        reason = "system_term"
      )
      next
    }
    
    # Handle else directive
    if (rec_start == "else") {
      result$else_mappings[[length(result$else_mappings) + 1]] <- list(
        recEnd = rec_end,
        recEnd_type = parse_recEnd_type(rec_end)
      )
      next
    }
    
    # Handle copy directive
    if (rec_end == "copy") {
      result$copy_mappings[[length(result$copy_mappings) + 1]] <- list(
        recStart = rec_start,
        recStart_values = parse_recStart_values(rec_start)
      )
      next
    }
    
    # Handle missing data patterns (NA::a, NA::b) - backward compatibility
    if (grepl("^NA::", rec_end)) {
      codes <- parse_recStart_values(rec_start)
      
      if (rec_end == "NA::a") {
        result$na_a_codes <- c(result$na_a_codes, codes)
      } else if (rec_end == "NA::b") {
        result$na_b_codes <- c(result$na_b_codes, codes)  
      }
      next
    }
    
    # Handle standard numeric mappings (for future extension)
    # These don't affect missing data detection but could be useful later
  }
  
  # Clean up and sort codes for backward compatibility
  result$na_a_codes <- sort(unique(result$na_a_codes))
  result$na_b_codes <- sort(unique(result$na_b_codes))
  
  return(result)
}

#' Helper: Determine if Term Should be Skipped
#'
#' Identifies system terms that should not be processed as data mappings.
#'
#' @param term Character. Term to check
#' @return Logical. TRUE if term should be skipped
#' @noRd
should_skip_term <- function(term) {
  if (is.null(term) || is.na(term) || nchar(trimws(term)) == 0) {
    return(TRUE)
  }
  
  skip_patterns <- c(
    "^Func::",           # Function calls
    "^DerivedVar::",     # Derived variable references
    "^min$",             # Runtime-dependent minimum
    "^max$",             # Runtime-dependent maximum
    "^N/A$"              # Not applicable (documentation-only rows)
  )
  
  return(any(sapply(skip_patterns, function(p) grepl(p, term))))
}

#' Helper: Parse recStart Values
#'
#' Extracts numeric values from recStart field with range support.
#'
#' @param rec_start Character. recStart value
#' @return Numeric vector. Parsed values
#' @noRd  
parse_recStart_values <- function(rec_start) {
  if (is.null(rec_start) || is.na(rec_start) || nchar(trimws(rec_start)) == 0) {
    return(numeric(0))
  }

  rec_start <- trimws(rec_start)

  # Reject expressions containing alphabetic characters.
  # Valid numeric recStart values are: single numbers ("96"), ranges ("[1,99]"),
  # or comma-separated numbers ("6,7,8"). Cross-variable conditions like
  # "SMKDSTY_original in (3,5,6)" or "is.na(SMK_204)" contain letters and must not
  # be parsed as numeric codes.
  if (grepl("[a-zA-Z]", rec_start)) {
    return(numeric(0))
  }

  # Use existing parse_range_notation if available
  if (exists("parse_range_notation")) {
    parsed <- parse_range_notation(rec_start)
    
    if (!is.null(parsed)) {
      if (parsed$type == "integer" && !is.null(parsed$values)) {
        return(parsed$values)
      } else if (parsed$type == "single_value") {
        return(parsed$value)
      } else if (parsed$type == "continuous") {
        return(c(parsed$min, parsed$max))
      }
    }
  } else {
    # Basic range parsing for [min,max] format
    if (grepl("^\\[\\d+,\\d+\\]$", rec_start)) {
      numbers <- as.numeric(regmatches(rec_start, gregexpr("\\d+", rec_start))[[1]])
      if (length(numbers) == 2) {
        return(seq(numbers[1], numbers[2]))
      }
    }
  }
  
  # Handle comma-separated values
  if (grepl(",", rec_start)) {
    values <- strsplit(rec_start, ",")[[1]]
    numeric_values <- suppressWarnings(as.numeric(trimws(values)))
    return(numeric_values[!is.na(numeric_values)])
  }
  
  # Handle single numeric values
  numeric_val <- suppressWarnings(as.numeric(rec_start))
  if (!is.na(numeric_val)) {
    return(numeric_val)
  }
  
  # Return empty for non-numeric terms
  return(numeric(0))
}

#' Helper: Parse recEnd Type
#'
#' Determines the type of recEnd value for processing.
#'
#' @param rec_end Character. recEnd value
#' @return Character. Type of recEnd value
#' @noRd
parse_recEnd_type <- function(rec_end) {
  if (is.null(rec_end) || is.na(rec_end) || nchar(trimws(rec_end)) == 0) {
    return("empty")
  }
  
  rec_end <- trimws(rec_end)
  
  if (grepl("^NA::", rec_end)) {
    return("tagged_na")
  }
  
  if (!is.na(suppressWarnings(as.numeric(rec_end)))) {
    return("numeric")
  }
  
  return("literal")
}

# ==============================================================================
# PORTABLE AUTO-DETECT DATABASE FUNCTION
# ==============================================================================

#' Auto-detect Database (Database-Agnostic)
#'
#' Automatically determines the appropriate database for a variable using
#' configurable database selection rules instead of hard-coded CCHS logic.
#'
#' @param variable_name Character. Variable name to detect database for
#' @param database_config List. Optional database configuration (auto-detected if NULL)
#' @param variable_metadata Optional. Pre-loaded variable metadata
#' @return Character. Detected database name
#' @export
auto_detect_database <- function(variable_name, database_config = NULL, variable_metadata = NULL) {
  
  # Input validation (database-agnostic)
  if (!is.character(variable_name) || length(variable_name) != 1 || is.na(variable_name)) {
    stop("variable_name must be a single non-NA character string")
  }
  if (nchar(trimws(variable_name)) == 0) {
    stop("variable_name cannot be empty or whitespace-only")
  }
  
  # Try to get available databases for this variable
  tryCatch({
    
    # Use existing metadata accessor
    if (!exists("get_variable_details")) {
      stop("get_variable_details() function not available. ",
           "Please load metadata accessor functions first.")
    }
    
    available_info <- get_variable_details(variable_name)
    
    if (nrow(available_info) == 0) {
      stop("No database information found for variable: '", variable_name, "'. ",
           "Variable may not exist in variable_details.csv.")
    }
    
    # Parse comma-separated database lists from databaseStart field
    database_lists <- unique(available_info$databaseStart)
    database_lists <- database_lists[!is.na(database_lists)]
    
    if (length(database_lists) == 0) {
      stop("No valid database information found for variable: '", variable_name, "'")
    }
    
    # Parse all individual databases from comma-separated lists
    all_databases <- parse_database_lists(database_lists)
    
    if (length(all_databases) == 0) {
      stop("No parseable database entries found for variable: '", variable_name, "'")
    }
    
    # Auto-detect database type if configuration not provided
    if (is.null(database_config)) {
      if (!exists("auto_detect_database_type") || !exists("load_database_config")) {
        warning("Database configuration functions not available. Using default CCHS configuration.")
        # Provide fallback configuration
        database_config <- list(
          priority_databases = c("cchs2001_p", "cchs2003_p", "cchs2005_p"),
          default_database = "cchs2001_p"
        )
        db_type <- "cchs"
      } else {
        db_type <- auto_detect_database_type(all_databases)
        database_config <- load_database_config(db_type)
      }
    }
    
    # If only one database, use it
    if (length(all_databases) == 1) {
      return(all_databases[1])
    }
    
    # Multiple databases - apply configurable heuristics
    selected_db <- apply_database_heuristics(all_databases, variable_name, database_config)
    
    # Only warn once per variable per session
    warning_key <- paste0(variable_name, "_db_selection")
    if (!exists(warning_key, envir = .database_warnings_cache)) {
      db_list <- paste(head(all_databases, 3), collapse = ", ")
      if (length(all_databases) > 3) {
        db_list <- paste0(db_list, "... (", length(all_databases), " total)")
      }
      warning("Multiple databases available for '", variable_name, "': ", db_list, ". ",
              "Auto-selected: '", selected_db, "'. ",
              "Specify database parameter for explicit control.", call. = FALSE)
      assign(warning_key, TRUE, envir = .database_warnings_cache)
    }
    
    return(selected_db)
    
  }, error = function(e) {
    stop("Failed to auto-detect database for variable '", variable_name, "': ", 
         e$message, ". Please specify database parameter explicitly.")
  })
}

# ==============================================================================
# PORTABLE DATABASE SELECTION HEURISTICS
# ==============================================================================

#' Apply Database Selection Heuristics (Database-Agnostic)
#'
#' Applies configurable selection heuristics instead of hard-coded CCHS rules.
#'
#' @param available_dbs Character vector. Available database names
#' @param variable_name Character. Variable name (for context)
#' @param config List. Database configuration object
#' @return Character. Selected database name
#' @export
apply_database_heuristics <- function(available_dbs, variable_name, config) {
  
  # Step 1: Apply exclusion patterns
  if (!is.null(config$exclusion_rules$patterns) && length(config$exclusion_rules$patterns) > 0) {
    for (exclusion in config$exclusion_rules$patterns) {
      pattern <- exclusion$pattern
      case_sensitive <- ifelse(is.null(exclusion$case_sensitive), TRUE, exclusion$case_sensitive)
      
      available_dbs <- available_dbs[!grepl(pattern, available_dbs, ignore.case = !case_sensitive)]
    }
  }
  
  # Step 2: Apply type hierarchy
  if (!is.null(config$type_hierarchy$levels) && length(config$type_hierarchy$levels) > 0) {
    
    # Sort levels by priority (level 1 = highest priority)
    level_names <- names(config$type_hierarchy$levels)
    level_numbers <- as.numeric(level_names)
    sorted_levels <- level_names[order(level_numbers)]
    
    for (level_name in sorted_levels) {
      level_config <- config$type_hierarchy$levels[[level_name]]
      
      if (!is.null(level_config$patterns)) {
        matches <- character(0)
        
        for (pattern in level_config$patterns) {
          pattern_matches <- available_dbs[grepl(pattern, available_dbs)]
          matches <- c(matches, pattern_matches)
        }
        
        if (length(matches) > 0) {
          # Found matches at this priority level
          # Apply year-based selection within this level
          return(select_most_recent_by_year(unique(matches), config))
        }
      }
    }
  }
  
  # Step 3: Fallback - select most recent overall
  return(select_most_recent_by_year(available_dbs, config))
}

# ==============================================================================
# PORTABLE YEAR EXTRACTION
# ==============================================================================

#' Extract Years from Database Names (Database-Agnostic)
#'
#' Extracts years using configurable regex patterns instead of hard-coded CCHS patterns.
#'
#' @param db_names Character vector. Database names
#' @param config List. Database configuration object
#' @return Numeric vector. Extracted years (NA for names without valid years)
#' @export
extract_years_from_database_names <- function(db_names, config) {
  
  years <- numeric(length(db_names))
  
  # Check if year extraction is configured
  if (is.null(config$naming_patterns$primary_pattern)) {
    return(rep(NA, length(db_names)))
  }
  
  for (i in seq_along(db_names)) {
    db_name <- db_names[i]
    years[i] <- NA  # Default
    
    # Try primary pattern first
    if (!is.null(config$naming_patterns$primary_pattern)) {
      match_result <- regexpr(config$naming_patterns$primary_pattern, db_name, ignore.case = TRUE)
      
      if (match_result > 0) {
        # Extract year from the match
        year_match <- regmatches(db_name, match_result)
        year_digits <- gsub("\\D", "", year_match)  # Extract just digits
        
        if (nchar(year_digits) >= 4) {
          year_num <- as.numeric(substr(year_digits, 1, 4))
          
          # Validate against configured year range
          if (!is.na(year_num) && 
              year_num >= config$year_validation$min_year && 
              year_num <= config$year_validation$max_year) {
            years[i] <- year_num
            next  # Found valid year, continue to next database
          }
        }
      }
    }
    
    # Try secondary pattern if primary failed
    if (is.na(years[i]) && !is.null(config$naming_patterns$secondary_pattern)) {
      match_result <- regexpr(config$naming_patterns$secondary_pattern, db_name, ignore.case = TRUE)
      
      if (match_result > 0) {
        year_match <- regmatches(db_name, match_result)
        year_digits <- gsub("\\D", "", year_match)
        
        if (nchar(year_digits) >= 4) {
          year_num <- as.numeric(substr(year_digits, 1, 4))
          
          if (!is.na(year_num) && 
              year_num >= config$year_validation$min_year && 
              year_num <= config$year_validation$max_year) {
            years[i] <- year_num
          }
        }
      }
    }
  }
  
  return(years)
}

#' Select Most Recent Database by Year (Database-Agnostic)
#'
#' Selects the database with the most recent year using configurable year extraction.
#'
#' @param db_names Character vector. Database names to choose from
#' @param config List. Database configuration object
#' @return Character. Selected database name
#' @noRd
select_most_recent_by_year <- function(db_names, config) {
  
  if (length(db_names) == 1) {
    return(db_names[1])
  }
  
  # Extract years using configuration
  years <- extract_years_from_database_names(db_names, config)
  
  # If we have valid years, select the most recent
  if (any(!is.na(years))) {
    max_year_idx <- which.max(years)
    return(db_names[max_year_idx])
  }
  
  # Fallback: lexicographic sorting (most "recent" string)
  sorted_dbs <- sort(db_names, decreasing = TRUE)
  return(sorted_dbs[1])
}

# ==============================================================================
# VECTOR SUPPORT FUNCTIONS
# ==============================================================================

#' Get Missing Pattern for Multiple Variables (Database-Agnostic)
#'
#' Efficiently handles multiple variables using sample-first strategy with
#' configurable consistency assumptions.
#'
#' @param variable_names Character vector. Variable names to get patterns for
#' @param database Character. Database name (optional, will auto-detect)
#' @param database_config List. Database configuration (optional, will auto-detect)
#' @param assume_consistent Logical. Assume all variables have same pattern (default TRUE)
#' @return Named list of missing patterns
#' @noRd
get_missing_pattern_vector <- function(variable_names, database = NULL, database_config = NULL, assume_consistent = TRUE) {
  
  if (assume_consistent) {
    # Efficiency strategy: Sample first variable, assume others match
    sample_variable <- variable_names[1]
    sample_pattern <- get_missing_pattern(sample_variable, database, database_config, assume_consistent = FALSE)
    
    # Use same database for all variables if not specified
    if (is.null(database)) {
      database <- auto_detect_database(sample_variable, database_config)
    }
    
    # Cache all variables with the same pattern (efficiency trade-off)
    result <- vector("list", length(variable_names))
    names(result) <- variable_names
    
    for (var_name in variable_names) {
      # Check cache first
      if (has_cached_pattern(var_name, database)) {
        result[[var_name]] <- get_cached_pattern(var_name, database)
      } else {
        # Assume same pattern as sample (efficiency compromise)
        result[[var_name]] <- sample_pattern
        cache_pattern(var_name, database, sample_pattern)
      }
    }
    
    # Warn user about assumption
    if (length(variable_names) > 1) {
      warning("assume_consistent=TRUE: Using pattern from '", sample_variable, 
              "' for all ", length(variable_names), " variables. ",
              "Set assume_consistent=FALSE for individual pattern detection.")
    }
    
  } else {
    # Accuracy strategy: Process each variable individually
    result <- vector("list", length(variable_names))
    names(result) <- variable_names
    
    for (var_name in variable_names) {
      result[[var_name]] <- get_missing_pattern(var_name, database, database_config, assume_consistent = FALSE)
    }
  }
  
  return(result)
}

#' Get Missing Pattern with Bulk Cache Operations
#'
#' Optimized version for database-level bulk operations where all variables
#' are from the same database.
#'
#' @param variable_names Character vector. Variable names
#' @param database Character. Database name (required for bulk operations)
#' @param database_config List. Database configuration (optional)
#' @return Named list of missing patterns
#' @export
get_missing_pattern_bulk <- function(variable_names, database, database_config = NULL) {
  
  if (is.null(database)) {
    stop("Database parameter is required for bulk operations. ",
         "Use get_missing_pattern() with auto-detection for individual variables.")
  }
  
  # Check which patterns are already cached
  cached_vars <- character(0)
  uncached_vars <- character(0)
  
  for (var_name in variable_names) {
    if (has_cached_pattern(var_name, database)) {
      cached_vars <- c(cached_vars, var_name)
    } else {
      uncached_vars <- c(uncached_vars, var_name)
    }
  }
  
  # Initialize result
  result <- vector("list", length(variable_names))
  names(result) <- variable_names
  
  # Get cached patterns (O(1) lookups)
  for (var_name in cached_vars) {
    result[[var_name]] <- get_cached_pattern(var_name, database)
  }
  
  # Load uncached patterns efficiently
  if (length(uncached_vars) > 0) {
    for (var_name in uncached_vars) {
      result[[var_name]] <- get_missing_pattern(var_name, database, database_config, assume_consistent = FALSE)
    }
  }
  
  message("Bulk operation complete: ", length(cached_vars), " from cache, ", 
          length(uncached_vars), " newly loaded")
  
  return(result)
}

# ==============================================================================
# MAIN PORTABLE GET_MISSING_PATTERN FUNCTION
# ==============================================================================

#' Get Missing Pattern (Database-Agnostic)
#'
#' Main function that extracts missing patterns using configurable database selection
#' instead of hard-coded CCHS logic. Supports both scalar and vector inputs.
#'
#' @param variable_name Character vector. Variable name(s) to get pattern for
#' @param database Character. Database name (optional, will auto-detect using config)
#' @param database_config List. Database configuration (optional, will auto-detect)
#' @param assume_consistent Logical. For vectors, assume all variables have same pattern (default TRUE)
#' @return List with na_a_codes and na_b_codes elements (scalar), or named list of patterns (vector)
#' @export
get_missing_pattern <- function(variable_name, database = NULL, database_config = NULL, assume_consistent = TRUE) {
  
  # Input validation
  if (!is.character(variable_name) || length(variable_name) == 0) {
    stop("variable_name must be a non-empty character vector")
  }
  if (any(is.na(variable_name)) || any(nchar(trimws(variable_name)) == 0)) {
    stop("variable_name cannot contain NA or empty strings")
  }
  
  # Handle vector input
  if (length(variable_name) > 1) {
    return(get_missing_pattern_vector(variable_name, database, database_config, assume_consistent))
  }
  
  # Step 1: Auto-detect database if not provided (using configuration)
  if (is.null(database)) {
    database <- auto_detect_database(variable_name, database_config)
  }
  
  # Step 2: Check cache first (O(1) lookup) - same as original
  if (has_cached_pattern(variable_name, database)) {
    return(get_cached_pattern(variable_name, database))
  }
  
  # Step 3: Load raw metadata - same as original
  tryCatch({
    
    if (!exists("get_variable_details")) {
      stop("get_variable_details() function not available. ",
           "Please load metadata accessor functions first.")
    }
    
    raw_data <- get_variable_details(variable_name)
    
    if (nrow(raw_data) == 0) {
      stop("No metadata found for variable '", variable_name, "' in database '", database, "'")
    }
    
    # Step 4: Process all recStart→recEnd mappings (including special terms)
    mapping_rows <- raw_data[!is.na(raw_data$recStart) & !is.na(raw_data$recEnd), ]
    
    if (nrow(mapping_rows) == 0) {
      # No mapping data defined - return empty pattern
      warning("No recStart→recEnd mappings found for variable '", variable_name, 
              "' in database '", database, "'. Returning empty pattern.")
      pattern <- list(na_a_codes = numeric(0), na_b_codes = numeric(0))
    } else {
      # Step 5: Use enhanced mapping function that handles special terms
      mapping_result <- map_recStart_to_recEnd(mapping_rows)
      
      # Extract backward-compatible missing codes pattern
      pattern <- list(
        na_a_codes = mapping_result$na_a_codes, 
        na_b_codes = mapping_result$na_b_codes
      )
      
      # Store complete mapping info in cache for Level 6 else functionality
      cache_complete_pattern(variable_name, database, mapping_result)
    }
    
    # Step 6: Cache result for future O(1) lookups - same as original
    cache_pattern(variable_name, database, pattern)
    
    return(pattern)
    
  }, error = function(e) {
    stop("Failed to extract missing pattern for variable '", variable_name, 
         "' in database '", database, "': ", e$message)
  })
}

# ==============================================================================
# COMPATIBILITY WRAPPERS
# ==============================================================================

#' Get Missing Pattern with Automatic Configuration Loading
#'
#' Convenience wrapper that maintains the original API while using
#' configurable database selection internally. Supports both scalar and vector inputs.
#'
#' @param variable_name Character vector. Variable name(s) to get pattern for
#' @param database Character. Database name (optional, will auto-detect)
#' @param database_type Character. Database type for configuration (default "cchs")
#' @param assume_consistent Logical. For vectors, assume all variables have same pattern (default TRUE)
#' @return List with na_a_codes and na_b_codes elements (scalar), or named list of patterns (vector)
#' @export
get_missing_pattern_auto <- function(variable_name, database = NULL, database_type = "cchs", assume_consistent = TRUE) {
  
  # Load appropriate configuration
  config <- load_database_config(database_type)
  
  # Call portable version with configuration
  return(get_missing_pattern(variable_name, database, config, assume_consistent))
}

# ==============================================================================
# ENHANCED LEVEL 4: COMPLETE PATTERN ACCESS FOR ELSE FUNCTIONALITY
# ==============================================================================

# Create session-level cache for complete patterns
.complete_patterns_cache <- new.env()

#' Cache Complete Pattern Information
#'
#' Stores complete pattern information (including else/copy mappings) for 
#' Level 6 else functionality while maintaining backward compatibility.
#'
#' @param variable_name Character. Variable name
#' @param database Character. Database name  
#' @param complete_pattern List. Complete pattern from map_recStart_to_recEnd()
#' @noRd
cache_complete_pattern <- function(variable_name, database, complete_pattern) {
  cache_key <- paste(variable_name, database, sep = "::")
  .complete_patterns_cache[[cache_key]] <- complete_pattern
}

#' Get Cached Complete Pattern
#'
#' Retrieves complete pattern information from cache.
#'
#' @param variable_name Character. Variable name
#' @param database Character. Database name
#' @return List with complete pattern or NULL if not cached
#' @noRd
get_cached_complete_pattern <- function(variable_name, database) {
  cache_key <- paste(variable_name, database, sep = "::")
  return(.complete_patterns_cache[[cache_key]])
}

#' Check if Complete Pattern is Cached
#'
#' @param variable_name Character. Variable name
#' @param database Character. Database name
#' @return Logical. TRUE if complete pattern is cached
#' @noRd
has_cached_complete_pattern <- function(variable_name, database) {
  cache_key <- paste(variable_name, database, sep = "::")
  return(exists(cache_key, envir = .complete_patterns_cache))
}

#' Get Complete Variable Pattern
#'
#' Returns complete pattern information including valid ranges and else mappings.
#' This is the main function for Level 6 else functionality.
#'
#' @param variable_name Character. Variable name
#' @param database Character. Optional database specification
#' @return List with complete pattern info (na_a_codes, na_b_codes, copy_mappings, else_mappings)
#' @export
get_complete_pattern <- function(variable_name, database = NULL) {
  
  # Auto-detect database if not provided
  if (is.null(database)) {
    database <- auto_detect_database(variable_name)
  }
  
  # Check complete pattern cache first
  if (has_cached_complete_pattern(variable_name, database)) {
    return(get_cached_complete_pattern(variable_name, database))
  }
  
  # If not cached, extract using existing infrastructure
  tryCatch({
    variable_rows <- get_variable_details(variable_name, database_filter = database)

    if (nrow(variable_rows) == 0) {
      stop("No metadata found for variable: ", variable_name)
    }
    
    # Extract mapping rows
    mapping_rows <- variable_rows[!is.na(variable_rows$recStart) & 
                                 !is.na(variable_rows$recEnd), ]
    
    if (nrow(mapping_rows) == 0) {
      # Return empty complete pattern
      complete_pattern <- list(
        na_a_codes = numeric(0),
        na_b_codes = numeric(0),
        copy_mappings = list(),
        else_mappings = list(),
        skip_entries = list()
      )
    } else {
      # Use existing map_recStart_to_recEnd function
      complete_pattern <- map_recStart_to_recEnd(mapping_rows)
    }
    
    # Cache the complete pattern
    cache_complete_pattern(variable_name, database, complete_pattern)
    
    return(complete_pattern)
    
  }, error = function(e) {
    stop("Failed to get complete pattern for '", variable_name, "': ", e$message)
  })
}

#' Get Copy Mappings (Valid Ranges)
#'
#' Extract valid range specifications from complete pattern.
#'
#' @param variable_name Character. Variable name
#' @param database Character. Optional database specification  
#' @return List of copy mappings with parsed values
#' @export
get_copy_mappings <- function(variable_name, database = NULL) {
  complete_pattern <- get_complete_pattern(variable_name, database)
  return(complete_pattern$copy_mappings)
}

#' Get Else Mappings (Fallback Rules)
#'
#' Extract else rules from complete pattern.
#'
#' @param variable_name Character. Variable name
#' @param database Character. Optional database specification
#' @return List of else mappings with target actions
#' @export  
get_else_mappings <- function(variable_name, database = NULL) {
  complete_pattern <- get_complete_pattern(variable_name, database)
  return(complete_pattern$else_mappings)
}

#' Clear Complete Pattern Cache
#'
#' Clears the complete pattern cache for testing or memory management.
#'
#' @export
clear_complete_patterns_cache <- function() {
  rm(list = ls(.complete_patterns_cache), envir = .complete_patterns_cache)
}

# Note: Original cache functions (has_cached_pattern, get_cached_pattern, etc.) 
# remain unchanged as they are already database-agnostic

# ==============================================================================
# LEVEL 4 PATTERN ANALYSIS FUNCTIONS (CONSOLIDATED FROM metadata-helpers.R)
# ==============================================================================

#' Get Variable Missing Pattern (Legacy Analysis Function)
#'
#' Analyzes missing pattern from variable_details metadata.
#' NOTE: This function is primarily for legacy compatibility.
#' Use get_complete_pattern() for new implementations.
#'
#' @param var_name Character. Variable name
#' @param variable_details Data.frame. Optional pre-loaded variable_details data
#' @return Character. Pattern type identifier
#' @export
get_variable_missing_pattern <- function(var_name, variable_details = NULL) {
  
  # Load variable_details if not provided
  if (is.null(variable_details)) {
    variable_details <- get_variable_details(var_name)
  }
  
  # Find rows with missing code patterns
  missing_entries <- variable_details[
    variable_details$variable == var_name & 
    grepl("NA::", variable_details$recEnd, fixed = TRUE),
  ]
  
  if (nrow(missing_entries) == 0) {
    warning("No missing pattern found for variable: ", var_name)
    return("no_pattern")
  }
  
  # Analyze recStart values to determine pattern
  all_codes <- tryCatch({
    unlist(sapply(missing_entries$recStart, function(x) {
      parsed <- parse_range_notation(x)
      if (!is.null(parsed)) parsed$values else NULL
    }))
  }, error = function(e) {
    warning("Could not parse missing codes for variable: ", var_name, " - ", e$message)
    return(NULL)
  })
  
  if (is.null(all_codes) || length(all_codes) == 0) {
    return("unknown_pattern")
  }
  
  # Classify pattern based on code ranges
  if (any(all_codes >= 996 & all_codes <= 999)) {
    return("triple_digit_missing")
  } else if (any(all_codes >= 6 & all_codes <= 9)) {
    return("single_digit_missing")  
  } else if (any(all_codes >= 96 & all_codes <= 99)) {
    return("double_digit_missing")
  } else {
    return("custom_pattern")
  }
}

