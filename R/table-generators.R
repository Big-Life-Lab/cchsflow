# =============================================================================
# Table Generators for CEP Documentation
# =============================================================================
#
# Functions for generating tables from worksheet metadata.
# Used by Quarto documents to create living documentation from authoritative
# CSV worksheets.
#
# @note v3.0.0-alpha, last updated: 2026-01-09
# =============================================================================

#' Parse databaseStart into PUMF and Master year ranges
#'
#' Extracts year coverage from the databaseStart field, separating PUMF (_p)
#' and Master (_m) file types.
#'
#' @param database_start Character. Comma-separated database list from
#'   databaseStart field (e.g., "cchs2001_p, cchs2003_p, cchs2001_m, cchs2023_m")
#' @return Named list with `pumf` and `master` character strings showing year
#'   ranges (e.g., list(pumf = "2001-2021", master = "2001-2023")). Returns "-"
#'   if no databases of that type.
#' @export
#' @examples
#' parse_database_years("cchs2001_p, cchs2003_p, cchs2021_p, cchs2001_m, cchs2023_m")
#' # Returns: list(pumf = "2001-2021", master = "2001-2023")
#'
#' parse_database_years("cchs2007_2008_p, cchs2009_2010_p")
#' # Returns: list(pumf = "2008-2010", master = "-")
parse_database_years <- function(database_start) {
  if (is.na(database_start) || database_start == "" || is.null(database_start)) {
    return(list(pumf = "-", master = "-"))
  }

  # Split on comma and trim whitespace

databases <- trimws(strsplit(database_start, ",")[[1]])

  # Separate PUMF and Master
  pumf_dbs <- databases[grepl("_p$", databases)]
  master_dbs <- databases[grepl("_m$", databases)]

  # Extract years from database names
  extract_years <- function(dbs) {
    if (length(dbs) == 0) return(NULL)

    years <- sapply(dbs, function(db) {
      # Pattern: cchs[YEAR]_[p|m] or cchs[YEAR]_[YEAR]_[p|m]
      # For dual-year cycles (e.g., cchs2019_2020_p), use the SECOND year
      # For single-year cycles (e.g., cchs2001_p), use that year
      dual_match <- regmatches(db, regexec("cchs(\\d{4})_(\\d{4})_[pm]$", db))[[1]]
      if (length(dual_match) >= 3) {
        # Dual-year cycle: return second year
        return(as.integer(dual_match[3]))
      }
      # Single-year cycle
      single_match <- regmatches(db, regexec("cchs(\\d{4})_[pm]$", db))[[1]]
      if (length(single_match) >= 2) {
        return(as.integer(single_match[2]))
      }
      return(NA)
    })

    years <- years[!is.na(years)]
    if (length(years) == 0) return(NULL)
    return(years)
  }

  format_range <- function(years) {
    if (is.null(years) || length(years) == 0) return("-")
    min_year <- min(years)
    max_year <- max(years)
    if (min_year == max_year) {
      return(as.character(min_year))
    }
    return(paste0(min_year, "-", max_year))
  }

  pumf_years <- extract_years(pumf_dbs)
  master_years <- extract_years(master_dbs)

  list(
    pumf = format_range(pumf_years),
    master = format_range(master_years)
  )
}


#' Parse structured tags from notes field
#'
#' Extracts `{key:value}` tags from the notes field. Tags are used to mark
#' variables as recommended, assign sub-subjects, etc.
#'
#' @param notes Character. The notes field content
#' @return Named list of tag values. Returns empty list if no tags found.
#' @export
#' @examples
#' parse_notes_tags("Universe: all respondents. {recommended:primary} {sub_subject:status}")
#' # Returns: list(recommended = "primary", sub_subject = "status")
#'
#' parse_notes_tags("No tags here.")
#' # Returns: list()
parse_notes_tags <- function(notes) {
  if (is.na(notes) || notes == "" || is.null(notes)) {
    return(list())
  }

  # Extract all {key:value} patterns
  matches <- gregexpr("\\{([^:}]+):([^}]+)\\}", notes, perl = TRUE)
  match_strings <- regmatches(notes, matches)[[1]]

  if (length(match_strings) == 0) {
    return(list())
  }

  # Parse each match into key-value pairs
  result <- list()
  for (match in match_strings) {
    # Extract key and value
    parts <- regmatches(match, regexec("\\{([^:}]+):([^}]+)\\}", match))[[1]]
    if (length(parts) >= 3) {
      key <- trimws(parts[2])
      value <- trimws(parts[3])
      result[[key]] <- value
    }
  }

  return(result)
}


#' Check if variable is a primary recommendation
#'
#' Convenience function to check for {recommended:primary} tag.
#'
#' @param notes Character. The notes field content
#' @return Logical. TRUE if {recommended:primary} tag is present.
#' @export
is_primary_recommendation <- function(notes) {
  tags <- parse_notes_tags(notes)
  !is.null(tags$recommended) && tags$recommended == "primary"
}


#' Get sub-subject from notes tags
#'
#' Extracts the {sub_subject:...} tag value.
#'
#' @param notes Character. The notes field content
#' @return Character or NULL. The sub_subject value if present.
#' @export
get_sub_subject <- function(notes) {
  tags <- parse_notes_tags(notes)
  tags$sub_subject
}


#' Generate Table 1: Primary recommendations by smoking subject
#'
#' Reads all smoking subject worksheet CSVs, filters for variables tagged with
#' `{recommended:primary}`, and generates a summary table showing coverage.
#'
#' @param worksheets_dir Character. Path to CEP-002 worksheets directory
#'   (the directory containing 01-status/, 02-initiation/, etc.)
#' @return Data frame with columns: Subject, Variable, Description, PUMF, Master
#' @export
#' @examples
#' \dontrun{
#' # From within ceps/cep-002-smoking/ directory
#' generate_smoking_summary_table(".")
#' }
generate_smoking_summary_table <- function(worksheets_dir = ".") {

  # Define subject-to-directory mapping and display order
  subjects <- list(
    Status = "01-status",
    Initiation = "02-initiation",
    Cessation = "03-cessation",
    Intensity = "04-intensity",
    `Pack-years` = "05-pack-years"
  )

  # Collect all primary recommendations
  results <- list()

  for (subject_name in names(subjects)) {
    subdir <- subjects[[subject_name]]
    dir_path <- file.path(worksheets_dir, subdir)

    # Find variables CSV file(s) in this directory
    # Pattern matches: variables_smk_*.csv and *_variables.csv
    csv_files <- list.files(
      dir_path,
      pattern = "(^variables_.*\\.csv$|_variables\\.csv$)",
      full.names = TRUE
    )

    for (csv_file in csv_files) {
      if (!file.exists(csv_file)) next

      # Read the CSV
      df <- tryCatch(
        read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE),
        error = function(e) NULL
      )

      if (is.null(df)) next

      # Check required columns exist
      required_cols <- c("variable", "label", "databaseStart", "notes")
      if (!all(required_cols %in% names(df))) next

      # Find rows with {recommended:primary} tag
      for (i in seq_len(nrow(df))) {
        if (is_primary_recommendation(df$notes[i])) {
          # Parse database years
          years <- parse_database_years(df$databaseStart[i])

          # Get sub_subject from tag (or use directory-based subject)
          tags <- parse_notes_tags(df$notes[i])
          display_subject <- if (!is.null(tags$sub_subject)) {
            # Capitalize first letter
            paste0(toupper(substr(tags$sub_subject, 1, 1)),
                   substr(tags$sub_subject, 2, nchar(tags$sub_subject)))
          } else {
            subject_name
          }

          results[[length(results) + 1]] <- data.frame(
            Subject = display_subject,
            Variable = df$variable[i],
            Description = df$label[i],
            PUMF = years$pumf,
            Master = years$master,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (length(results) == 0) {
    # Return empty data frame with correct structure
    return(data.frame(
      Subject = character(),
      Variable = character(),
      Description = character(),
      PUMF = character(),
      Master = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Combine all results
  result_df <- do.call(rbind, results)

  # Order by subject (matching the defined order)
  subject_order <- c("Status", "Initiation", "Cessation", "Intensity", "Pack-years")
  result_df$Subject <- factor(result_df$Subject, levels = subject_order)
  result_df <- result_df[order(result_df$Subject), ]
  result_df$Subject <- as.character(result_df$Subject)

  rownames(result_df) <- NULL
  return(result_df)
}


#' Get recommendation level from notes tags
#'
#' Extracts the recommendation level from {recommended:...} tag.
#'
#' @param notes Character. The notes field content
#' @return Character. "Primary", "Secondary", or "" if no tag.
#' @export
get_recommendation <- function(notes) {
  tags <- parse_notes_tags(notes)
  if (is.null(tags$recommended)) return("")
  # Capitalize first letter
  paste0(toupper(substr(tags$recommended, 1, 1)),
         substr(tags$recommended, 2, nchar(tags$recommended)))
}


#' Generate variable list table from worksheet CSV
#'
#' Reads a variables.csv file and generates a table showing all variables
#' with their coverage and recommendation status.
#'
#' @param csv_path Character. Path to variables_*.csv file
#' @param include_recommendation Logical. Include Recommendation column? Default TRUE.
#' @return Data frame with columns: Variable, Label, Type, PUMF, Master,
#'   and optionally Recommendation
#' @export
#' @examples
#' \dontrun{
#' generate_variable_list_table("01-status/variables_smk_status.csv")
#' }
generate_variable_list_table <- function(csv_path, include_recommendation = TRUE) {
  if (!file.exists(csv_path)) {
    warning("File not found: ", csv_path)
    return(data.frame(
      Variable = character(),
      Label = character(),
      Type = character(),
      PUMF = character(),
      Master = character(),
      stringsAsFactors = FALSE
    ))
  }

  df <- tryCatch(
    read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      warning("Error reading CSV: ", e$message)
      return(NULL)
    }
  )

  if (is.null(df)) {
    return(data.frame(
      Variable = character(),
      Label = character(),
      Type = character(),
      PUMF = character(),
      Master = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Check required columns (variableType is the actual column name)
  required_cols <- c("variable", "label", "databaseStart")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    warning("Missing required columns: ", paste(missing_cols, collapse = ", "))
    return(data.frame(
      Variable = character(),
      Label = character(),
      Type = character(),
      PUMF = character(),
      Master = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Parse databaseStart for each row
  years_list <- lapply(df$databaseStart, parse_database_years)

  # Get type - column may be named 'type' or 'variableType'
  type_col <- if ("variableType" %in% names(df)) df$variableType
              else if ("type" %in% names(df)) df$type
              else rep("", nrow(df))

  result <- data.frame(
    Variable = df$variable,
    Label = df$label,
    Type = type_col,
    PUMF = sapply(years_list, `[[`, "pumf"),
    Master = sapply(years_list, `[[`, "master"),
    stringsAsFactors = FALSE
  )

  # Add recommendation column if requested and notes column exists
  if (include_recommendation && "notes" %in% names(df)) {
    result$Recommendation <- sapply(df$notes, get_recommendation)
    # Format variable names: **bold** for Primary, *italic* for Secondary
    result$Variable <- ifelse(
      result$Recommendation == "Primary",
      paste0("**", result$Variable, "**"),
      ifelse(
        result$Recommendation == "Secondary",
        paste0("*", result$Variable, "*"),
        result$Variable
      )
    )
  }

  return(result)
}


#' Parse databaseStart into individual cycle labels
#'
#' Converts databaseStart field into individual CCHS cycle labels for
#' the coverage matrix display.
#'
#' @param database_start Character. Comma-separated database list
#' @param file_type Character. "pumf" or "master" to filter by file type
#' @return Character vector of cycle labels (e.g., c("01", "03", "07-08"))
#' @keywords internal
parse_database_cycles <- function(database_start, file_type = "pumf") {
  if (is.na(database_start) || database_start == "" || is.null(database_start)) {
    return(character(0))
  }

  suffix <- if (file_type == "pumf") "_p$" else "_m$"
  databases <- trimws(strsplit(database_start, ",")[[1]])
  databases <- databases[grepl(suffix, databases)]

  # Convert database names to cycle labels
  sapply(databases, function(db) {
    # Dual-year cycle: cchs2007_2008_p -> "07-08"
    dual_match <- regmatches(db, regexec("cchs(\\d{4})_(\\d{4})_[pm]$", db))[[1]]
    if (length(dual_match) >= 3) {
      y1 <- substr(dual_match[2], 3, 4)
      y2 <- substr(dual_match[3], 3, 4)
      return(paste0(y1, "-", y2))
    }
    # Single-year cycle: cchs2001_p -> "01"
    single_match <- regmatches(db, regexec("cchs(\\d{4})_[pm]$", db))[[1]]
    if (length(single_match) >= 2) {
      return(substr(single_match[2], 3, 4))
    }
    return(NA_character_)
  }, USE.NAMES = FALSE)
}


#' Generate PUMF cycle coverage matrix
#'
#' Reads all smoking subject worksheet CSVs, filters for variables tagged with
#' `{coverage_matrix:true}`, and generates a matrix showing cycle-by-cycle
#' availability.
#'
#' @param worksheets_dir Character. Path to CEP-002 worksheets directory
#' @param file_type Character. "pumf" or "master". Default "pumf".
#' @return Data frame with Variable column and one column per CCHS cycle
#' @export
#' @examples
#' \dontrun{
#' # From within ceps/cep-002-smoking/ directory
#' generate_coverage_matrix(".")
#' }
generate_coverage_matrix <- function(worksheets_dir = ".", file_type = "pumf") {

  # Define all CCHS cycles in order
  all_cycles <- c("01", "03", "05", "07-08", "09-10", "11-12", "13-14",
                  "15-16", "17-18", "19-20", "21", "22", "23")

  # Define subject-to-directory mapping
  subjects <- list(
    Status = "01-status",
    Initiation = "02-initiation",
    Cessation = "03-cessation",
    Intensity = "04-intensity",
    `Pack-years` = "05-pack-years"
  )

  results <- list()

  for (subject_name in names(subjects)) {
    subdir <- subjects[[subject_name]]
    dir_path <- file.path(worksheets_dir, subdir)

    csv_files <- list.files(
      dir_path,
      pattern = "(^variables_.*\\.csv$|_variables\\.csv$)",
      full.names = TRUE
    )

    for (csv_file in csv_files) {
      if (!file.exists(csv_file)) next

      df <- tryCatch(
        read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE),
        error = function(e) NULL
      )

      if (is.null(df)) next

      required_cols <- c("variable", "databaseStart", "notes")
      if (!all(required_cols %in% names(df))) next

      for (i in seq_len(nrow(df))) {
        tags <- parse_notes_tags(df$notes[i])
        if (!is.null(tags$coverage_matrix) && tags$coverage_matrix == "true") {
          cycles <- parse_database_cycles(df$databaseStart[i], file_type)

          # Create row with checkmarks for available cycles
          row <- data.frame(
            Subject = subject_name,
            Variable = df$variable[i],
            stringsAsFactors = FALSE
          )

          # Add columns for each cycle
          for (cycle in all_cycles) {
            row[[cycle]] <- if (cycle %in% cycles) "\u2713" else "-"
          }

          results[[length(results) + 1]] <- row
        }
      }
    }
  }

  if (length(results) == 0) {
    # Return empty data frame with correct structure
    empty_df <- data.frame(Subject = character(), Variable = character(),
                           stringsAsFactors = FALSE)
    for (cycle in all_cycles) {
      empty_df[[cycle]] <- character()
    }
    return(empty_df)
  }

  result_df <- do.call(rbind, results)

  # Order by subject
  subject_order <- c("Status", "Initiation", "Cessation", "Intensity", "Pack-years")
  result_df$Subject <- factor(result_df$Subject, levels = subject_order)
  result_df <- result_df[order(result_df$Subject), ]
  result_df$Subject <- as.character(result_df$Subject)

  rownames(result_df) <- NULL
  return(result_df)
}


#' Generate subgroup summary table
#'
#' Counts variables by subgroup, separating PUMF+Master from Master-only.
#' Produces a summary table showing Total, PUMF+Master, Master-only, and
#' Description for each subgroup.
#'
#' @param worksheets_dir Character. Path to CEP-002 worksheets directory
#' @return Data frame with columns: Subgroup, Total, PUMF+Master, Master-only, Description
#' @export
#' @examples
#' \dontrun{
#' generate_subgroup_summary_table(".")
#' }
generate_subgroup_summary_table <- function(worksheets_dir = ".") {

  # Define subject metadata
  subjects <- list(
    list(
      dir = "01-status",
      name = "01-status",
      description = "Smoking status classification"
    ),
    list(
      dir = "02-initiation",
      name = "02-initiation",
      description = "Age started smoking"
    ),
    list(
      dir = "03-cessation",
      name = "03-cessation",
      description = "Quit timing and duration"
    ),
    list(
      dir = "04-intensity",
      name = "04-intensity",
      description = "Cigarettes per day"
    ),
    list(
      dir = "05-pack-years",
      name = "05-pack-years",
      description = "Derived cumulative exposure"
    )
  )

  results <- list()

  for (subj in subjects) {
    dir_path <- file.path(worksheets_dir, subj$dir)

    csv_files <- list.files(
      dir_path,
      pattern = "(^variables_.*\\.csv$|_variables\\.csv$)",
      full.names = TRUE
    )

    total <- 0
    pumf_master <- 0
    master_only <- 0

    for (csv_file in csv_files) {
      if (!file.exists(csv_file)) next

      df <- tryCatch(
        read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE),
        error = function(e) NULL
      )

      if (is.null(df) || !"databaseStart" %in% names(df)) next

      for (i in seq_len(nrow(df))) {
        years <- parse_database_years(df$databaseStart[i])
        total <- total + 1

        has_pumf <- years$pumf != "-"
        has_master <- years$master != "-"

        if (has_pumf && has_master) {
          pumf_master <- pumf_master + 1
        } else if (has_pumf) {
          pumf_master <- pumf_master + 1
        } else if (has_master) {
          master_only <- master_only + 1
        }
      }
    }

    results[[length(results) + 1]] <- data.frame(
      Subgroup = subj$name,
      Total = total,
      `PUMF+Master` = pumf_master,
      `Master-only` = master_only,
      Description = subj$description,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  if (length(results) == 0) {
    return(data.frame(
      Subgroup = character(),
      Total = integer(),
      `PUMF+Master` = integer(),
      `Master-only` = integer(),
      Description = character(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  result_df <- do.call(rbind, results)

  # Add totals row
  totals_row <- data.frame(
    Subgroup = "**Total**",
    Total = sum(result_df$Total),
    `PUMF+Master` = sum(result_df$`PUMF+Master`),
    `Master-only` = sum(result_df$`Master-only`),
    Description = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  result_df <- rbind(result_df, totals_row)

  rownames(result_df) <- NULL
  return(result_df)
}


#' Generate coverage matrix for a single subject
#'
#' Reads variables CSV(s) from a subject directory and generates a matrix
#' showing cycle-by-cycle availability for all variables (or filtered by
#' recommendation tag).
#'
#' @param subject_dir Character. Path to subject directory (e.g., "01-status")
#' @param file_type Character. "pumf" or "master". Default "pumf".
#' @param filter_recommended Character or NULL. If "primary" or "secondary",
#'   only include variables with that recommendation tag. NULL includes all.
#' @return Data frame with Variable column and one column per CCHS cycle
#' @export
#' @examples
#' \dontrun{
#' # From within ceps/cep-002-smoking/ directory
#' generate_subject_coverage_matrix("01-status")
#' generate_subject_coverage_matrix("04-intensity", filter_recommended = "primary")
#' }
generate_subject_coverage_matrix <- function(subject_dir,
                                              file_type = "pumf",
                                              filter_recommended = NULL) {

  # Define all CCHS cycles in order
  all_cycles <- c("01", "03", "05", "07-08", "09-10", "11-12", "13-14",
                  "15-16", "17-18", "19-20", "21", "22", "23")

  # Find CSV files in subject directory
  csv_files <- list.files(
    subject_dir,
    pattern = "(^variables_.*\\.csv$|_variables\\.csv$)",
    full.names = TRUE
  )

  results <- list()

  for (csv_file in csv_files) {
    if (!file.exists(csv_file)) next

    df <- tryCatch(
      read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )

    if (is.null(df)) next

    required_cols <- c("variable", "databaseStart")
    if (!all(required_cols %in% names(df))) next

    for (i in seq_len(nrow(df))) {
      # Check recommendation filter if specified
      if (!is.null(filter_recommended) && "notes" %in% names(df)) {
        rec <- get_recommendation(df$notes[i])
        if (tolower(rec) != tolower(filter_recommended)) next
      }

      cycles <- parse_database_cycles(df$databaseStart[i], file_type)

      # Create row with checkmarks for available cycles
      row <- data.frame(
        Variable = df$variable[i],
        stringsAsFactors = FALSE
      )

      # Add columns for each cycle
      for (cycle in all_cycles) {
        row[[cycle]] <- if (cycle %in% cycles) "\u2713" else "-"
      }

      results[[length(results) + 1]] <- row
    }
  }

  if (length(results) == 0) {
    # Return empty data frame with correct structure
    empty_df <- data.frame(Variable = character(), stringsAsFactors = FALSE)
    for (cycle in all_cycles) {
      empty_df[[cycle]] <- character()
    }
    return(empty_df)
  }

  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL
  return(result_df)
}
