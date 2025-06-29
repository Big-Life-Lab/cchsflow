#' Fix CSV files for git collaboration (Getting Started Approach)
#'
#' Eliminates git merge conflicts in CSV files by fixing invisible Excel 
#' corruption issues. Designed for Excel-first workflows with zero learning
#' curve, plus optional enhanced validation for research collaboration.
#'
#' @section Getting Started Design:
#' * **Basic mode (default)**: Just works - fixes git conflicts immediately
#' * **Collaboration mode**: Adds research-quality validation when needed
#' * **Excel compatibility**: Round-trip editing fully preserved
#' * **Project naming**: Works with variables_ProjectName.csv automatically
#'
#' @section Common Issues Fixed:
#' * UTF-8 BOM (invisible bytes that break scripts)
#' * Windows/Mac line endings causing git conflicts  
#' * Inconsistent encoding between team members
#' * File formatting differences from different editors
#' * Column order inconsistencies (reorders to schema-defined standard)
#' * Missing/additional columns (reports and preserves user extensions)
#'
#' @param file_path Character. Path to CSV file (variables.csv, variable_details.csv, or project variants)
#' @param collaboration Logical. FALSE = basic mode (Excel users), TRUE = enhanced validation (research quality). Default: FALSE
#' @param output_path Character. Output path (default: overwrites input file safely)
#' @param validate_only Logical. TRUE = check only without changes, FALSE = fix issues. Default: FALSE
#'
#' @return List with components:
#'   \item{success}{Logical. TRUE if standardisation succeeded}
#'   \item{mode}{Character. "basic" (getting started) or "collaboration" (enhanced)}
#'   \item{issues_fixed}{Character vector. Problems automatically resolved}
#'   \item{issues_remaining}{Character vector. Issues needing attention}
#'   \item{valid}{Logical. TRUE if file passes validation for the selected mode}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Quick fix for git conflicts (most common use case)
#' standardise_csv("variables.csv")
#' 
#' # Check what would be fixed first (safe exploration)
#' result <- standardise_csv("variables.csv", validate_only = TRUE)
#' if (!result$valid) print(result$issues_remaining)
#'
#' # Works with any project naming convention
#' standardise_csv("variables_MyProject.csv")
#' standardise_csv("ProjectName_variable_details.csv")
#' standardise_csv("variables_DemPoRT.csv")
#'
#' # Enhanced validation for research publications
#' standardise_csv("variables.csv", collaboration = TRUE)
#' 
#' # Fix column order issues (automatically reorders to schema standard)
#' # Before: variableType,variable,label -> After: variable,label,variableType
#' standardise_csv("my_variables.csv")
#' 
#' # Column completeness reporting (collaboration mode)
#' result <- standardise_csv("my_file.csv", collaboration = TRUE, validate_only = TRUE)
#' # Reports: "Additional columns: myCustomField", "Missing optional: units, notes"
#' 
#' # Team workflow: validate before sharing
#' result <- standardise_csv("variables.csv", collaboration = TRUE, validate_only = TRUE)
#' if (result$valid) {
#'   cat("✅ Ready for team collaboration!\n")
#' } else {
#'   cat("⚠️ Please address these issues:\n")
#'   for (issue in result$issues_remaining) cat("  -", issue, "\n")
#' }
#' }
#'
#' @seealso
#' For team setup and workflows: CSV_STANDARDISATION_QUICKSTART.md
#' For troubleshooting: Run demo_for_meeting.R for examples
standardise_csv <- function(file_path, collaboration = FALSE, 
                           output_path = file_path, validate_only = FALSE) {
  
  # Read CSV data
  data <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
  
  # Validate data
  if (collaboration) {
    validation_result <- .validate_enhanced_format(data, file_path)
    mode <- "collaboration"
  } else {
    validation_result <- .validate_basic_format(data, file_path)
    mode <- "basic"
  }
  
  issues_remaining <- validation_result$issues
  valid <- length(issues_remaining) == 0
  
  # If validate_only, return without modifying file
  if (validate_only) {
    return(list(
      valid = valid,
      mode = mode,
      issues_remaining = issues_remaining,
      success = valid
    ))
  }
  
  # Apply standardisation to file
  issues_fixed <- .apply_standardisation(file_path)
  
  return(list(
    success = TRUE,
    mode = mode,
    issues_fixed = issues_fixed,
    issues_remaining = issues_remaining,
    valid = valid
  ))
}