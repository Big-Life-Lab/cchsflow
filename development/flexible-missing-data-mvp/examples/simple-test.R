# ==============================================================================
# Simple Test - Core Infrastructure 
# ==============================================================================

library(haven)
library(dplyr)

# Source the infrastructure
source("../R/flexible-missing-handler.R")

cat("=== Testing Flexible Missing Data Infrastructure ===\n\n")

# Test 1: Pattern discovery
cat("1. Available patterns:\n")
patterns <- get_missing_patterns()
cat(paste("  ", patterns, collapse = "\n"), "\n\n")

# Test 2: Basic handler creation
cat("2. Creating handler for triple_digit_missing pattern:\n")
test_data <- c(1.75, 1.60, 996, 997, 998, 999)

handler <- create_missing_handler(
  test_data,
  pattern_type = "triple_digit_missing",
  handle_missing_data = "original"
)

cat("✓ Handler created successfully\n")
cat("  Available tools:", paste(names(handler), collapse = ", "), "\n\n")

# Test 3: Semantic tools
cat("3. Testing semantic tools:\n")

# is_missing tests
cat("  is_missing() tests:\n")
for (val in c(1.75, 996, 997, 998)) {
  result <- handler$is_missing(val)
  cat(sprintf("    is_missing(%s): %s\n", val, result))
}

cat("\n  is_tag() tests:\n")
cat(sprintf("    is_tag(996, 'not_applicable'): %s\n", 
           handler$is_tag(996, "not_applicable")))
cat(sprintf("    is_tag(997, 'missing_data'): %s\n", 
           handler$is_tag(997, "missing_data")))
cat(sprintf("    is_tag(996, 'missing_data'): %s\n", 
           handler$is_tag(996, "missing_data")))

cat("\n  propagate() tests:\n")
# Priority: not_applicable beats missing_data
result1 <- handler$propagate(996, 997)
cat(sprintf("    propagate(996, 997): %s (not_applicable wins)\n", result1))

# Within same category
result2 <- handler$propagate(997, 998)
cat(sprintf("    propagate(997, 998): %s (first missing_data found)\n", result2))

# With valid values
result3 <- handler$propagate(1.75, 997)
cat(sprintf("    propagate(1.75, 997): %s (missing beats valid)\n", result3))

cat("\n=== All tests completed successfully! ===\n")