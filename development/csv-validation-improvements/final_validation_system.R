# ====================================================================
# FINAL CSV VALIDATION SYSTEM - Simplified Single Function
# ====================================================================

#' Validate Variable Details CSV Files
#'
#' Performs structured validation of variable_details.csv files with different 
#' levels of detail. Uses R CMD check style output with visual icons for 
#' clear status reporting.
#'
#' @param file_path Character. Path to CSV file to validate
#' @param level Character. Detail level: "basic", "verbose", or "full". Default: "basic"
#'   - "basic": Quick summary with icons and guidance (daily workflow)
#'   - "verbose": Enhanced details with specific error descriptions  
#'   - "full": Complete raw validation output for deep investigation
#' @param mode Character. Validation mode: "basic" or "collaboration". Default: "collaboration"
#'
#' @return List with validation results and summary (returned invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' # Daily workflow - quick validation
#' validate_variable_details_csv("inst/extdata/variable_details.csv")
#' 
#' # Enhanced details for investigation
#' validate_variable_details_csv("inst/extdata/variable_details.csv", level = "verbose")
#' 
#' # Complete raw output for troubleshooting
#' validate_variable_details_csv("inst/extdata/variable_details.csv", level = "full")
#' 
#' # Basic structural check only (faster)
#' validate_variable_details_csv("inst/extdata/variable_details.csv", mode = "basic")
#' }
validate_variable_details_csv <- function(file_path, level = "basic", mode = "collaboration") {
  
  # Input validation
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  if (!level %in% c("basic", "verbose", "full")) {
    stop("Level must be 'basic', 'verbose', or 'full'")
  }
  
  if (!mode %in% c("basic", "collaboration")) {
    stop("Mode must be 'basic' or 'collaboration'")
  }
  
  # Load required functions (assuming they're already sourced)
  if (!exists("standardise_csv")) {
    stop("Required validation functions not loaded. Please source R/csv-helpers.R and R/csv-utils.R")
  }
  
  # Handle full level - complete raw validation output
  if (level == "full") {
    return(.validate_csv_full_detail(file_path))
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
    cat("✅ PASS")
    if (level == "verbose") cat(" (required fields present, file readable)")
    cat("\n")
    results$basic_structure <- "PASS"
  } else {
    cat("❌ FAIL\n")
    results$basic_structure <- "FAIL"
    issues_details$basic_structure <- basic_result$issues_remaining
    if (level == "verbose") {
      cat("  Issues:", paste(basic_result$issues_remaining, collapse = "; "), "\n")
    }
  }
  
  # If basic structure fails, don't continue with advanced checks
  if (!basic_result$valid) {
    cat("\n🚨 VALIDATION FAILED ──────────────────────────\n")
    cat("❌ Basic structure check failed - fix fundamental issues first\n")
    if (level == "basic") {
      cat("💡 Need details? Try: validate_variable_details_csv('", file_path, "', level = 'verbose')\n", sep = "")
    }
    cat("\n")
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
      cat("✅ PASS")
      if (level == "verbose") cat(" (all field patterns match schema)")
      cat("\n")
      results$schema_compliance <- "PASS"
    } else {
      cat("🟡 WARNING (", meaningful_pattern_issues, " pattern issues)")
      if (level == "verbose") cat(" (variableStart, recEnd field patterns)")
      cat("\n")
      results$schema_compliance <- "WARNING"
      issues_details$schema_compliance <- schema_result$issues_remaining[grepl("Pattern violations", schema_result$issues_remaining) & !grepl("dummyVariable", schema_result$issues_remaining)]
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
      cat("✅ PASS")
      if (level == "verbose") cat(" (columns in expected schema order)")
      cat("\n")
      results$column_order <- "PASS"
    } else {
      cat("❌ FAIL")
      if (level == "verbose") cat(" (columns not in schema order)")
      cat("\n")
      results$column_order <- "FAIL"
      issues_details$column_order <- schema_result$issues_remaining[grepl("Column order", schema_result$issues_remaining)]
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
      cat("✅ PASS")
      if (level == "verbose") cat(" (all enum values valid, cross-field rules satisfied)")
      cat("\n")
      results$data_quality <- "PASS"
    } else {
      cat("🟡 WARNING (", total_quality_issues, " data issues)")
      if (level == "verbose") {
        if (enum_issues > 0) cat(" (enum violations)")
        if (cross_field_issues > 0) cat(" (cross-field issues)")
      }
      cat("\n")
      results$data_quality <- "WARNING"
      issues_details$data_quality <- schema_result$issues_remaining[grepl("Invalid values|continuous variables|missing units", schema_result$issues_remaining, ignore.case = TRUE)]
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
  
  # Add guidance for getting more information (only for basic level)
  if (level == "basic" && (warnings > 0 || fails > 0)) {
    cat("\n📋 NEED MORE DETAILS?\n")
    cat("  verbose: validate_variable_details_csv('", file_path, "', level = 'verbose')\n", sep = "")
    cat("  full: validate_variable_details_csv('", file_path, "', level = 'full')\n", sep = "")
  } else if (level == "basic") {
    cat("\n💡 For detailed info: level = 'verbose' | For complete output: level = 'full'\n")
  }
  
  # Return comprehensive results
  return(invisible(list(
    results = results,
    overall_status = overall_status,
    critical_issues = critical_issues,
    details = if (level == "verbose" || fails > 0 || warnings > 0) issues_details else NULL,
    summary = list(
      file = basename(file_path),
      level = level,
      mode = mode,
      passes = passes,
      warnings = warnings,
      fails = fails,
      skips = skips
    )
  )))
}

# Internal function for full detail level
.validate_csv_full_detail <- function(file_path) {
  
  # Load required functions (assuming they're already sourced)
  if (!exists("standardise_csv")) {
    stop("Required validation functions not loaded. Please source R/csv-helpers.R and R/csv-utils.R")
  }
  
  cat("🔍 FULL VALIDATION INVESTIGATION\n")
  cat("── File:", basename(file_path), "────────────────────────\n\n")
  
  cat("📋 Running complete validation with full output...\n\n")
  
  # Run both basic and collaboration mode with full output
  cat("=== BASIC MODE VALIDATION ===\n")
  basic_result <- tryCatch({
    standardise_csv(file_path, collaboration = FALSE, validate_only = TRUE)
  }, error = function(e) {
    list(valid = FALSE, issues_remaining = paste("Error:", e$message))
  })
  
  cat("Valid:", basic_result$valid, "\n")
  if (length(basic_result$issues_remaining) > 0) {
    cat("Issues found:\n")
    for (i in seq_along(basic_result$issues_remaining)) {
      cat("  ", i, ".", basic_result$issues_remaining[i], "\n")
    }
  } else {
    cat("No issues found.\n")
  }
  
  cat("\n=== COLLABORATION MODE VALIDATION ===\n")
  collab_result <- tryCatch({
    standardise_csv(file_path, collaboration = TRUE, validate_only = TRUE)
  }, error = function(e) {
    list(valid = FALSE, issues_remaining = paste("Schema error:", e$message))
  })
  
  cat("Valid:", collab_result$valid, "\n")
  if (length(collab_result$issues_remaining) > 0) {
    cat("Issues found:\n")
    for (i in seq_along(collab_result$issues_remaining)) {
      cat("  ", i, ".", collab_result$issues_remaining[i], "\n")
    }
  } else {
    cat("No issues found.\n")
  }
  
  cat("\n=== DETAILED PATTERN VIOLATION ANALYSIS ===\n")
  pattern_issues <- collab_result$issues_remaining[grepl("Pattern violations", collab_result$issues_remaining)]
  if (length(pattern_issues) > 0) {
    cat("Found", length(pattern_issues), "pattern violations:\n\n")
    for (i in seq_along(pattern_issues)) {
      cat("  📋 PATTERN ISSUE", i, ":\n")
      cat("     ", pattern_issues[i], "\n")
      
      # Provide context for common patterns
      if (grepl("variableStart", pattern_issues[i])) {
        cat("     💡 Context: variableStart expects patterns like:\n")
        cat("        - Database references: cchs2001_p::VARIABLE_NAME\n")
        cat("        - Derived variables: DerivedVar::[complex_expression]\n")
        cat("        - Simple values: value_name\n")
      } else if (grepl("recEnd", pattern_issues[i])) {
        cat("     💡 Context: recEnd expects patterns like:\n")
        cat("        - Function calls: Func::function_name\n")
        cat("        - Categorical mappings: category1;category2;category3\n")
        cat("        - Simple values: single_value\n")
      } else if (grepl("dummyVariable", pattern_issues[i])) {
        cat("     💡 Context: dummyVariable patterns (currently relaxed)\n")
      }
      cat("\n")
    }
  } else {
    cat("No pattern violations found.\n")
  }
  
  cat("=== COLUMN ORDER ANALYSIS ===\n")
  order_issues <- collab_result$issues_remaining[grepl("Column order", collab_result$issues_remaining)]
  if (length(order_issues) > 0) {
    cat("Found", length(order_issues), "column order issues:\n")
    for (i in seq_along(order_issues)) {
      cat("  ", i, ".", order_issues[i], "\n")
    }
    cat("\n💡 Expected column order: variable, dummyVariable, typeEnd, databaseStart, variableStart...\n")
  } else {
    cat("Column order is correct.\n")
  }
  
  cat("\n=== DATA QUALITY ANALYSIS ===\n")
  quality_issues <- collab_result$issues_remaining[grepl("Invalid values|Invalid enum|continuous variables|missing units", collab_result$issues_remaining, ignore.case = TRUE)]
  if (length(quality_issues) > 0) {
    cat("Found", length(quality_issues), "data quality issues:\n")
    for (i in seq_along(quality_issues)) {
      cat("  ", i, ".", quality_issues[i], "\n")
    }
  } else {
    cat("No data quality issues found.\n")
  }
  
  cat("\n🎯 SUMMARY\n")
  cat("──────────\n")
  cat("• Basic validation:", ifelse(basic_result$valid, "✅ PASS", "❌ FAIL"), "\n")
  cat("• Collaboration validation:", ifelse(collab_result$valid, "✅ PASS", "❌ FAIL"), "\n")
  cat("• Total issues found:", length(collab_result$issues_remaining), "\n")
  cat("• Pattern violations:", length(pattern_issues), "\n")
  cat("• Column order issues:", length(order_issues), "\n")
  cat("• Data quality issues:", length(quality_issues), "\n")
  
  cat("\n💡 NEXT STEPS:\n")
  if (length(collab_result$issues_remaining) == 0) {
    cat("🎉 No issues found! File is ready for use.\n")
  } else {
    cat("🔧 Fix critical issues (❌ FAIL) first, then address warnings (🟡)\n")
    cat("📋 Use validate_variable_details_csv() for daily workflow validation\n")
  }
  
  # Return comprehensive results
  return(invisible(list(
    basic_result = basic_result,
    collab_result = collab_result,
    pattern_issues = pattern_issues,
    order_issues = order_issues,
    quality_issues = quality_issues,
    all_issues = collab_result$issues_remaining,
    summary = list(
      basic_valid = basic_result$valid,
      collab_valid = collab_result$valid,
      total_issues = length(collab_result$issues_remaining),
      pattern_violations = length(pattern_issues),
      order_issues = length(order_issues),
      quality_issues = length(quality_issues)
    )
  )))
}

#' Show what each validation category checks
#' 
#' Provides detailed explanations of what each validation category checks,
#' helping users understand validation results and know what to fix.
#' 
#' @export
validation_help <- function() {
  cat("📋 CSV VALIDATION CATEGORIES EXPLAINED\n")
  cat("======================================\n\n")
  
  cat("✅❌ BASIC STRUCTURE:\n")
  cat("   • File can be read as valid CSV\n")
  cat("   • Required fields present: variable, typeEnd, databaseStart, variableStart, variableStartLabel\n")
  cat("   • No fundamental parsing errors\n\n")
  
  cat("✅🟡❌ SCHEMA COMPLIANCE:\n") 
  cat("   • Field patterns match schema definitions\n")
  cat("   • variableStart: Database references (cchs2001_p::VAR) and derived variables (DerivedVar::[...])\n")
  cat("   • recEnd: Function references (Func::function_name) and categorical patterns\n")
  cat("   • dummyVariable: Variable naming patterns (currently relaxed)\n\n")
  
  cat("✅❌ COLUMN ORDER:\n")
  cat("   • Columns in expected schema sequence\n")
  cat("   • Expected: variable, dummyVariable, typeEnd, databaseStart, variableStart...\n")
  cat("   • Extra columns allowed but should come after standard columns\n\n")
  
  cat("✅🟡❌ DATA QUALITY:\n")
  cat("   • Enum values match allowed lists\n") 
  cat("   • Cross-field rules (e.g., continuous variables should have units)\n")
  cat("   • Referential integrity between fields\n\n")
  
  cat("💡 VALIDATION LEVELS:\n")
  cat("   • basic: validate_variable_details_csv('file.csv')\n")
  cat("   • verbose: validate_variable_details_csv('file.csv', level = 'verbose')\n")
  cat("   • full: validate_variable_details_csv('file.csv', level = 'full')\n\n")
  
  cat("🔧 COMMON FIXES:\n")
  cat("   • ❌ Basic Structure: Check file format, required columns\n")
  cat("   • 🟡 Schema Compliance: Usually schema definition issues, not data problems\n")
  cat("   • ❌ Column Order: Reorder columns to match schema\n")
  cat("   • 🟡 Data Quality: Check enum values and cross-field rules\n")
}