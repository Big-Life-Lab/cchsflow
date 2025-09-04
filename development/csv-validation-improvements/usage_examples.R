# Usage Examples for validate_csv_comprehensive Function

# Setup (run once)
source("R/csv-helpers.R")
source("R/csv-utils.R")
source("development/csv-validation-improvements/validate_csv_comprehensive.R")

# ===================================================================
# EXAMPLE 1: Quick validation check (most common use case)
# ===================================================================

cat("EXAMPLE 1: Quick validation check\n")
cat("==================================\n")
result <- validate_csv_comprehensive("inst/extdata/variable_details.csv")

# The function returns detailed results, but main output is printed
# You can access specific results:
if (result$overall_status == "PASS") {
  cat("✅ File is ready for use!\n")
} else if (result$overall_status == "WARNING") {
  cat("⚠️ File has warnings but is usable\n")
} else {
  cat("❌ File has critical issues that need fixing\n")
}

cat("\n")

# ===================================================================
# EXAMPLE 2: Detailed investigation of issues
# ===================================================================

cat("EXAMPLE 2: Detailed investigation\n")
cat("==================================\n")
detailed_result <- validate_csv_comprehensive("inst/extdata/variable_details.csv", verbose = TRUE)

# Access specific validation category results:
cat("Basic structure:", detailed_result$results$basic_structure, "\n")
cat("Schema compliance:", detailed_result$results$schema_compliance, "\n")
cat("Column order:", detailed_result$results$column_order, "\n")
cat("Data quality:", detailed_result$results$data_quality, "\n")

cat("\n")

# ===================================================================
# EXAMPLE 3: Basic mode for quick structural check
# ===================================================================

cat("EXAMPLE 3: Basic mode validation\n")
cat("=================================\n")
basic_result <- validate_csv_comprehensive("inst/extdata/variable_details.csv", mode = "basic")

cat("\n")

# ===================================================================
# EXAMPLE 4: Programmatic use in scripts
# ===================================================================

cat("EXAMPLE 4: Programmatic usage\n")
cat("==============================\n")

# Function to validate multiple files
validate_multiple_files <- function(file_paths) {
  results <- list()
  
  for (file_path in file_paths) {
    cat("\nValidating:", basename(file_path), "\n")
    result <- validate_csv_comprehensive(file_path, verbose = FALSE)
    results[[basename(file_path)]] <- result$overall_status
  }
  
  return(results)
}

# Example usage (you can add more CSV files here)
files_to_check <- c("inst/extdata/variable_details.csv")
multi_results <- validate_multiple_files(files_to_check)

cat("\nSummary of multiple file validation:\n")
for (file_name in names(multi_results)) {
  status <- multi_results[[file_name]]
  icon <- switch(status,
                 "PASS" = "✅",
                 "WARNING" = "⚠️", 
                 "FAIL" = "❌")
  cat(sprintf("%s %s: %s\n", icon, file_name, status))
}

cat("\n")

# ===================================================================
# EXAMPLE 5: Integration with existing workflow
# ===================================================================

cat("EXAMPLE 5: Workflow integration\n")
cat("================================\n")

# Function that could be added to package development workflow
check_csv_files <- function() {
  csv_files <- list.files("inst/extdata", pattern = "\\.csv$", full.names = TRUE)
  
  if (length(csv_files) == 0) {
    cat("No CSV files found in inst/extdata/\n")
    return(invisible(TRUE))
  }
  
  all_passed <- TRUE
  
  for (csv_file in csv_files) {
    result <- validate_csv_comprehensive(csv_file)
    if (result$critical_issues) {
      all_passed <- FALSE
    }
    cat("\n")
  }
  
  if (all_passed) {
    cat("🎉 All CSV files passed validation!\n")
  } else {
    cat("⚠️ Some CSV files have critical issues\n")
  }
  
  return(invisible(all_passed))
}

# Run the check
check_csv_files()

cat("\n=== EXAMPLES COMPLETE ===\n")
cat("These examples show how to integrate validate_csv_comprehensive() into your workflow.\n")