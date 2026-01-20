# ==============================================================================
# Robust File Sourcing with Centralized Configuration
# ==============================================================================
#
# Configuration-based sourcing that uses here() with a single directory reference.
# Change DEVELOPMENT_R_DIR once to switch between development/production modes.
#
# Usage patterns:
# 1. R-to-R files: Use source_r_robust() (recommended) or source_r_with_fallback() (legacy)
# 2. Metadata files: Use here::here("inst", "extdata", ...)
# 3. Tests: Run from project root with testthat::test_file()
#
# DEVELOPMENT STATUS: Robust sourcing with centralized configuration
# LOCATION: development/flexible-missing-data-mvp/R/file-sourcing.R

library(here)

# ==============================================================================
# CONFIGURATION - Change this single line to switch modes
# ==============================================================================

# Development mode: files in development/flexible-missing-data-mvp/R/
DEVELOPMENT_R_DIR <- here("development", "flexible-missing-data-mvp", "R")

# Production mode (when moved to main R folder): 
# DEVELOPMENT_R_DIR <- here("R")

# Testing mode (from tests/testthat/):
# DEVELOPMENT_R_DIR <- here("..", "..", "R")

# ==============================================================================
# ROBUST SOURCING FUNCTIONS
# ==============================================================================

#' Robust R file sourcing with centralized configuration
#'
#' Sources R files using centralized directory configuration. 
#' Change DEVELOPMENT_R_DIR once to switch between dev/prod/test modes.
#'
#' @param filename Character. Name of R file to source
#' @param check_function Character. Function name to check if already loaded
#' @param verbose Logical. Print sourcing information
#' @export
source_r_robust <- function(filename, check_function = NULL, verbose = FALSE) {
  
  # Skip if function already exists
  if (!is.null(check_function) && exists(check_function)) {
    if (verbose) cat("✓", check_function, "already loaded\n")
    return(TRUE)
  }
  
  # Construct path using centralized configuration
  source_path <- file.path(DEVELOPMENT_R_DIR, filename)
  
  if (file.exists(source_path)) {
    if (verbose) cat("Loading", filename, "from", source_path, "\n")
    source(source_path)
    return(TRUE)
  } else {
    stop("Could not find ", filename, " at ", source_path)
  }
}


