# AI Development Instructions for cchsflow

*Main instruction file for AI code development assistance*

## Overview

This folder contains development documentation specifically designed for AI assistance in the cchsflow project. These files provide context, track issues, and maintain development continuity across AI sessions.

## Key Files

### 📋 **TODO.md** - Active Development Tasks
- Current priority tasks and completion status
- Session-to-session continuity for AI development
- Technical specifications for ongoing work

### 🐛 **ISSUES_TO_DISCUSS.md** - Technical Issues & Decisions
- Identified technical issues requiring team discussion
- Implementation decisions and rationale
- Research vs. implementation decision tracking

## AI Development Context

### Current Project State
- **v3.0.0 Architecture**: Modern tidyverse-compliant derived variable functions
- **BMI Functions**: Recently modernized with vector-aware helpers and DRY architecture
- **Next Priority**: Smoking and alcohol function modernization using established patterns

### Key Architecture Patterns
1. **Four-tier architecture**: constants → core utilities → specialized helpers → public API
2. **Vector-aware processing**: Functions handle scalars, vectors, and rec_with_table() usage
3. **DRY tagged NA handling**: Reusable helpers for missing data patterns
4. **Tidyverse compliance**: Following community standards and style guide

### Function Naming Conventions
- `calculate_*()` - Mathematical computations
- `categorize_*()` - Classification into groups  
- `score_*()` - Scoring systems
- `adjust_*()` - Data adjustment/correction

### Development Guide Reference
- **Primary**: `@cchsflow/vignettes/derived_variables_development.qmd`
- **Standards**: Tidyverse Style Guide, testthat testing patterns
- **Dependencies**: haven for tagged_na, dplyr for data manipulation

## AI Session Continuity

### Before Starting Work
1. Read TODO.md for current priorities
2. Check ISSUES_TO_DISCUSS.md for context on technical decisions
3. Review recent git commits for implementation patterns

### During Development
1. Update TODO.md with progress (mark in_progress → completed)
2. Add new issues to ISSUES_TO_DISCUSS.md as discovered
3. Follow established helper function patterns from BMI implementation

### Before Ending Session
1. Update TODO.md with current status
2. Document any new technical issues or decisions needed
3. Prepare clear handoff notes for next session

## Code Quality Standards

### Testing
- All functions must have comprehensive testthat tests
- Test both success and failure paths
- Include edge cases and missing data scenarios

### Documentation
- roxygen2 documentation for all exported functions
- Clear parameter descriptions and examples
- Version notes for internal functions

### Architecture Compliance
- Use established helper functions from missing-data-helpers.R
- Follow DRY principles for tagged NA handling
- Maintain backward compatibility with rec_with_table()

## Common Patterns

### Vector-Aware Function Template
```r
my_function <- function(input_var, categorical_labels = TRUE, log_level = "silent") {
  # Use established helpers for preprocessing
  clean_input <- clean_for_categorization(input_var, "continuous_standard", log_level)
  
  # DRY tagged NA handling
  dplyr::case_when(
    !!!generate_tagged_na_conditions(clean_input, categorical_labels),
    # Domain-specific logic here
    .default = if(categorical_labels) "NA(b)" else haven::tagged_na("b")
  )
}
```

### Test Structure Template
```r
test_that("function handles vectors correctly", {
  # Test with vector input
  result <- my_function(c(value1, value2, value3))
  expect_length(result, 3)
  expect_equal(result[1], expected_result)
})
```

## Integration Points

### Critical Integration Tests
- rec_with_table() compatibility across CCHS cycles
- variable_details.csv integration for validation
- Performance with large datasets (>100k records)

### Dependencies to Monitor
- haven package for tagged_na functionality
- dplyr for data manipulation
- vctrs for vector compatibility checking

---

*This file should be updated as development patterns evolve and new standards are established.*