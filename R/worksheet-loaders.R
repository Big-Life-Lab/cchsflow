# ==============================================================================
# LEVEL 2B: Worksheet-Level Metadata Loaders
# ==============================================================================
#
# DEPENDENCY LEVEL: 2B (Worksheet metadata - depends on Level 1)
# 
# FUNCTIONS IN THIS FILE:
# - load_worksheet_metadata() - Load CSV worksheets (variables.csv, variable_details.csv)
# - load_worksheet_schemas() - Load YAML schema files (variables.yaml, variable_details.yaml)
#
# USED BY LEVEL 3 FUNCTIONS:
# - get_variables() (uses load_worksheet_metadata)
# - get_variable_details() (uses load_worksheet_metadata)
# - Schema validation functions (use load_worksheet_schemas)
#
# DEVELOPMENT STATUS: Level 2B worksheet metadata functions
# LOCATION: development/flexible-missing-data-mvp/R/worksheet-metadata-loaders.R
# VERSION: v2.0.0, updated 2025-08-02

# Dependencies loaded via DESCRIPTION Depends (dplyr, yaml)
# Path handling uses system.file() instead of here::here()

#' Load worksheet metadata from CSV files
#' 
#' Loads variables.csv or variable_details.csv with proper error handling
#' and standardization. Supports both CSV and RData formats.
#'
#' @param metadata_type Character. Either "variables" or "variable_details"
#' @param use_rdata Logical. Use .RData files if TRUE, CSV if FALSE
#' @return Data frame with metadata
#' @export
load_worksheet_metadata <- function(metadata_type = c("variables", "variable_details"), use_rdata = TRUE) {
  metadata_type <- match.arg(metadata_type)
  
  rdata_file <- system.file("extdata", paste0(metadata_type, ".RData"), package = "cchsflow")
  csv_file <- system.file("extdata", paste0(metadata_type, ".csv"), package = "cchsflow")

  # system.file returns "" if not found; fall back to inst/ path for dev context
  if (!nzchar(rdata_file)) rdata_file <- file.path("inst", "extdata", paste0(metadata_type, ".RData"))
  if (!nzchar(csv_file)) csv_file <- file.path("inst", "extdata", paste0(metadata_type, ".csv"))
  
  # Try loading from RData first if requested (performance optimization)
  if (use_rdata && file.exists(rdata_file) && file.size(rdata_file) > 0) {
    tryCatch({
      e <- new.env()
      load(rdata_file, envir = e)
      if (metadata_type %in% ls(e)) {
        return(get(metadata_type, envir = e))
      }
    }, error = function(e) {
      warning("Failed to load RData file, falling back to CSV: ", e$message, call. = FALSE)
    })
  }
  
  # Load from CSV with file size check
  if (file.exists(csv_file) && file.size(csv_file) > 0) {
    metadata <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
    
    # Auto-save to RData for future performance (from load-variable-details.R feature)
    if (use_rdata) {
      save_metadata_to_rdata(metadata, metadata_type, rdata_file)
    }
    
    return(metadata)
  }
  
  # Final fallback to cached package data (robust fallback from load-variable-details.R)
  if (metadata_type == "variable_details") {
    warning("CSV file not found. Falling back to cached package data.", call. = FALSE)
    e <- new.env()
    data("variable_details", package = "cchsflow", envir = e)
    return(e$variable_details)
  } else {
    stop("Metadata file not found and no package fallback available for: ", metadata_type)
  }
}

#' Load worksheet schema YAML files
#' 
#' Loads worksheet schema files like variables.yaml, variable_details.yaml with proper error handling.
#'
#' @param schema_name Character. Name of schema file (without .yaml extension)
#' @param schema_path Character. Path relative to inst/metadata/schemas/ (default: "core")
#' @return List with parsed YAML schema content
#' @export
load_worksheet_schemas <- function(schema_name, schema_path = "core") {
  yaml_file <- system.file("metadata", "schemas", schema_path, paste0(schema_name, ".yaml"), package = "cchsflow")
  if (!nzchar(yaml_file)) yaml_file <- file.path("inst", "metadata", "schemas", schema_path, paste0(schema_name, ".yaml"))
  
  if (!file.exists(yaml_file)) {
    stop("YAML schema file not found: ", yaml_file)
  }
  
  tryCatch({
    yaml::read_yaml(yaml_file)
  }, error = function(e) {
    stop("Failed to parse YAML file ", yaml_file, ": ", e$message)
  })
}

# ==============================================================================
# HELPER FUNCTIONS (INTERNAL)
# ==============================================================================

#' Save Metadata to RData File
#'
#' Helper function to save CSV metadata to RData format for performance optimization.
#' Merged from load-variable-details.R functionality.
#'
#' @param metadata Data frame. The metadata to save
#' @param metadata_type Character. Type of metadata (for variable naming)
#' @param rdata_path Character. Path where to save the RData file
#' @noRd
save_metadata_to_rdata <- function(metadata, metadata_type, rdata_path) {
  tryCatch({
    # Create environment and assign with correct variable name
    e <- new.env()
    assign(metadata_type, metadata, envir = e)
    
    # Save using the named object
    save(list = metadata_type, file = rdata_path, envir = e)
  }, error = function(e) {
    warning("Failed to save RData file: ", e$message, call. = FALSE)
  })
}

# REMOVED: load_variable_details() function
# This wrapper was deleted to avoid conflicts with production R/utility-functions.R
# Use load_worksheet_metadata("variable_details") directly instead