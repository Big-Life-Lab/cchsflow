#' Comprehensive CSV Validation Check
#'
#' Performs a structured validation check similar to R CMD check, providing
#' clear pass/fail status for different validation categories.
#'
#' @param file_path Character. Path to CSV file to validate
#' @param mode Character. Validation mode: "basic" or "collaboration". Default: "collaboration"
#' @param verbose Logical. Show detailed error messages for failures. Default: FALSE
#'
#' @return List with validation results and summary
#' @export
#'
#' @examples
#' \dontrun{
#' # Quick validation summary
#' validate_csv_comprehensive("inst/extdata/variable_details.csv")
#' 
#' # Detailed validation with error messages
#' validate_csv_comprehensive("inst/extdata/variable_details.csv", verbose = TRUE)
#' 
#' # Basic mode validation only
#' validate_csv_comprehensive("inst/extdata/variable_details.csv", mode = "basic")
#' }
validate_csv_comprehensive <- function(file_path, mode = "collaboration", verbose = FALSE) {
  
  # Input validation
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  if (!mode %in% c("basic", "collaboration")) {
    stop("Mode must be 'basic' or 'collaboration'")
  }
  
  # Load required functions (assuming they're already sourced)
  if (!exists("standardise_csv")) {
    stop("Required validation functions not loaded. Please source R/csv-helpers.R and R/csv-utils.R")
  }
  
  # Initialize results tracking
  results <- list()
  issues_details <- list()
  
  # Header
  cat("── Validating", basename(file_path), "────────────────────────\n\n")
  
  # 1. BASIC STRUCTURE CHECK
  cat("◦ Basic structure... ")
  basic_result <- tryCatch({
    standardise_csv(file_path, collaboration = FALSE, validate_only = TRUE)
  }, error = function(e) {
    list(valid = FALSE, issues_remaining = paste("Error:", e$message))
  })
  
  if (basic_result$valid) {
    cat("✅ PASS\n")
    results$basic_structure <- "PASS"
  } else {
    cat("❌ FAIL\n")
    results$basic_structure <- "FAIL"
    issues_details$basic_structure <- basic_result$issues_remaining
    if (verbose) {
      cat("  Issues:", paste(basic_result$issues_remaining, collapse = "; "), "\n")
    }
  }
  
  # If basic structure fails, don't continue with advanced checks
  if (!basic_result$valid) {
    cat("\n🚨 VALIDATION FAILED ──────────────────────────\n")
    cat("❌ Basic structure check failed - fix fundamental issues first\n\n")
    return(list(
      results = results,
      overall_status = "FAIL",
      critical_issues = TRUE,
      details = issues_details
    ))
  }
  
  # 2. SCHEMA COMPLIANCE CHECK (only in collaboration mode)
  if (mode == "collaboration") {
    cat("◦ Schema compliance... ")
    schema_result <- tryCatch({
      standardise_csv(file_path, collaboration = TRUE, validate_only = TRUE)
    }, error = function(e) {
      list(valid = FALSE, issues_remaining = paste("Schema error:", e$message))
    })
    
    # Count different types of pattern issues
    pattern_issues <- sum(grepl("Pattern violations", schema_result$issues_remaining))
    # Exclude dummyVariable issues for now (as requested)
    dummy_var_issues <- sum(grepl("Pattern violations in 'dummyVariable'", schema_result$issues_remaining))
    meaningful_pattern_issues <- pattern_issues - dummy_var_issues
    
    if (meaningful_pattern_issues == 0) {
      cat("✅ PASS\n")
      results$schema_compliance <- "PASS"
    } else {
      cat("🟡 WARNING (", meaningful_pattern_issues, " pattern issues)\n")
      results$schema_compliance <- "WARNING"
      issues_details$schema_compliance <- schema_result$issues_remaining[grepl("Pattern violations", schema_result$issues_remaining) & !grepl("dummyVariable", schema_result$issues_remaining)]
      if (verbose) {
        cat("  Pattern issues in variableStart, recEnd fields\n")
      }
    }
  } else {
    # Basic mode - assume schema compliance passes
    cat("◦ Schema compliance... ⏭️ SKIP (basic mode)\n")
    results$schema_compliance <- "SKIP"
  }
  
  # 3. COLUMN ORDER CHECK
  cat("◦ Column order... ")
  if (mode == "collaboration") {
    order_issues <- sum(grepl("Column order", schema_result$issues_remaining))
    if (order_issues == 0) {
      cat("✅ PASS\n")
      results$column_order <- "PASS"
    } else {
      cat("❌ FAIL\n")
      results$column_order <- "FAIL"
      issues_details$column_order <- schema_result$issues_remaining[grepl("Column order", schema_result$issues_remaining)]
      if (verbose) {
        cat("  Column order doesn't match schema expectations\n")
      }
    }
  } else {
    cat("⏭️ SKIP (basic mode)\n")
    results$column_order <- "SKIP"
  }
  
  # 4. DATA QUALITY CHECK
  cat("◦ Data quality... ")
  if (mode == "collaboration") {
    enum_issues <- sum(grepl("Invalid values|Invalid enum", schema_result$issues_remaining))
    cross_field_issues <- sum(grepl("continuous variables|missing units", schema_result$issues_remaining, ignore.case = TRUE))
    
    total_quality_issues <- enum_issues + cross_field_issues
    
    if (total_quality_issues == 0) {
      cat("✅ PASS\n")
      results$data_quality <- "PASS"
    } else {
      cat("🟡 WARNING (", total_quality_issues, " data issues)\n")
      results$data_quality <- "WARNING"
      issues_details$data_quality <- schema_result$issues_remaining[grepl("Invalid values|continuous variables|missing units", schema_result$issues_remaining, ignore.case = TRUE)]
      if (verbose) {
        if (enum_issues > 0) cat("  ", enum_issues, "enum violations\n")
        if (cross_field_issues > 0) cat("  ", cross_field_issues, "cross-field issues\n")
      }
    }
  } else {
    cat("⏭️ SKIP (basic mode)\n")
    results$data_quality <- "SKIP"
  }
  
  cat("\n")
  
  # SUMMARY REPORTING
  passes <- sum(sapply(results, function(x) x == "PASS"))
  warnings <- sum(sapply(results, function(x) x == "WARNING"))
  fails <- sum(sapply(results, function(x) x == "FAIL"))
  skips <- sum(sapply(results, function(x) x == "SKIP"))
  
  # Determine overall status
  if (fails > 0) {
    cat("🚨 VALIDATION FAILED ──────────────────────────\n")
    cat("❌", fails, "failed")
    if (warnings > 0) cat(", 🟡", warnings, "warnings")
    if (passes > 0) cat(", ✅", passes, "passed")
    if (skips > 0) cat(", ⏭️", skips, "skipped")
    cat("\n\n")
    
    # Provide actionable guidance
    if (results$column_order == "FAIL") {
      cat("🔧 Fix required: Column order issues\n")
    }
    if (results$basic_structure == "FAIL") {
      cat("🔧 Fix required: Basic structure problems\n")
    }
    
    overall_status <- "FAIL"
    critical_issues <- TRUE
    
  } else if (warnings > 0) {
    cat("🟡 VALIDATION PASSED WITH WARNINGS ────────────\n")
    cat("🟡", warnings, "warnings")
    if (passes > 0) cat(", ✅", passes, "passed")
    if (skips > 0) cat(", ⏭️", skips, "skipped")
    cat("\n\n")
    
    # Provide actionable guidance for warnings
    if (results$schema_compliance == "WARNING") {
      cat("💡 Consider: Updating schema patterns for current data\n")
    }
    if (results$data_quality == "WARNING") {
      cat("💡 Consider: Reviewing data quality issues\n")
    }
    
    overall_status <- "WARNING"
    critical_issues <- FALSE
    
  } else {
    cat("🎉 VALIDATION PASSED ───────────────────────────\n")
    cat("✅ All checks passed")
    if (skips > 0) cat(" (⏭️", skips, " skipped in basic mode)")
    cat("\n")
    
    overall_status <- "PASS"
    critical_issues <- FALSE
  }
  
  # Add stepped guidance for getting more information
  if (!verbose && (warnings > 0 || fails > 0)) {
    cat("\n📋 NEED MORE DETAILS?\n")
    cat("  Layer 2: validate_csv_comprehensive('", basename(file_path), "', verbose = TRUE)\n")
    cat("  Layer 3: validate_csv_deep('", basename(file_path), "') # Full investigation\n")
    cat("  Help: validation_help() # Explains what each category checks\n")
  } else if (!verbose) {
    cat("\n💡 For detailed info: verbose = TRUE | For help: validation_help()\n")
  }
  
  # Return comprehensive results
  return(invisible(list(
    results = results,
    overall_status = overall_status,
    critical_issues = critical_issues,
    details = if (verbose || fails > 0 || warnings > 0) issues_details else NULL,
    summary = list(
      file = basename(file_path),
      mode = mode,
      passes = passes,
      warnings = warnings,
      fails = fails,
      skips = skips
    )
  )))
}