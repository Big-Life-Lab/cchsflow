#' Show what each validation category checks (Layer 2: Help System)
#' 
#' Provides detailed explanations of what each validation category checks,
#' helping users understand validation results and know what to fix.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Show help for validation categories
#' validation_help()
#' }
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
  
  cat("💡 GETTING MORE INFORMATION:\n")
  cat("   • Layer 1 (Basic): validate_csv_comprehensive('file.csv')\n")
  cat("   • Layer 2 (Details): validate_csv_comprehensive('file.csv', verbose = TRUE)\n")
  cat("   • Layer 3 (Deep): validate_csv_deep('file.csv') # Full original output\n\n")
  
  cat("🔧 COMMON FIXES:\n")
  cat("   • ❌ Basic Structure: Check file format, required columns\n")
  cat("   • 🟡 Schema Compliance: Usually schema definition issues, not data problems\n")
  cat("   • ❌ Column Order: Reorder columns to match schema\n")
  cat("   • 🟡 Data Quality: Check enum values and cross-field rules\n")
}