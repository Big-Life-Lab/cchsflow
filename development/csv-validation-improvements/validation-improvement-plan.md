# CSV Validation Improvements Plan

## Overview
Enhance the CSV validation system with a comprehensive validation framework similar to R CMD check/devtools, providing clear, actionable feedback for data quality issues.

## Current State Analysis

### Existing Validation Functions
- ✅ `standardise_csv()` - Main validation entry point
- ✅ Basic mode validation (minimal requirements)
- ✅ Collaboration mode validation (enhanced schema checks)
- ✅ Pattern validation for field constraints
- ✅ Column order validation
- ✅ Cross-field validation rules

### Current Issues
- ❌ Verbose output makes it hard to quickly assess validation status
- ❌ No clear hierarchy of validation issues (critical vs warnings)
- ❌ Limited actionable guidance for fixing issues
- ❌ No summary reporting for multiple files

## Proposed Improvements

### 1. Comprehensive Validation Function
**Goal**: Create `validate_csv_comprehensive()` with structured R CMD check-style output

**Features**:
- Clear PASS/FAIL/WARNING status for each validation category
- Hierarchical issue reporting (critical failures vs warnings)
- Actionable next steps in output
- Verbose mode for detailed error investigation

**Categories to validate**:
1. **Basic Structure** (required fields, file format)
2. **Schema Compliance** (pattern violations, field types)
3. **Column Order** (schema-defined ordering)
4. **Data Quality** (enum violations, cross-field rules)

### 2. Validation Summary Dashboard
**Goal**: Quick validation status across multiple CSV files

**Features**:
- Tabular summary of validation results
- Color-coded status indicators
- File-by-file breakdown
- Overall project validation health

### 3. Schema Rule Updates
**Goal**: Address pattern validation issues in current data

**Identified Issues**:
- `dummyVariable` field patterns too restrictive
- `variableStart` field doesn't handle complex derived variable references
- `recEnd` field doesn't handle function references and complex patterns

**Actions**:
- Review and update YAML schema patterns
- Add support for derived variable syntax (`DerivedVar::[...]`)
- Add support for function references (`Func::function_name`)
- Add support for complex database references (`cchs2001_p::VARIABLE`)

### 4. Column Order Standardization
**Goal**: Fix column ordering issues

**Current Issue**:
- `status` and `lastUpdated` columns are swapped
- Extra columns (`ICES.confirmation`, `review`) not in schema

**Actions**:
- Update schema to include missing columns
- Provide automatic column reordering function
- Clear guidance on expected column order

## Implementation Plan

### Phase 1: Core Validation Function
- [ ] Implement `validate_csv_comprehensive()` function
- [ ] Add to `R/csv-helpers.R`
- [ ] Add proper documentation and examples
- [ ] Export in NAMESPACE
- [ ] Test with existing variable_details.csv

### Phase 2: Schema Updates
- [ ] Review current pattern violations in detail
- [ ] Update `variable_details.yaml` schema patterns
- [ ] Test updated schema against existing data
- [ ] Document new pattern syntax

### Phase 3: Column Order Resolution
- [ ] Add missing columns to schema
- [ ] Implement automatic column reordering
- [ ] Test column standardization
- [ ] Update validation to handle extra columns gracefully

### Phase 4: Testing and Documentation
- [ ] Add comprehensive tests for new validation function
- [ ] Update existing tests if needed
- [ ] Create usage examples and documentation
- [ ] Test against multiple CSV files in the project

### Phase 5: Validation Dashboard (Optional)
- [ ] Implement multi-file validation summary
- [ ] Create reporting functions
- [ ] Add project-wide validation health check

## Success Criteria

### Immediate Goals
1. Users can run `validate_csv_comprehensive("file.csv")` and get clear PASS/FAIL status
2. Column order issues are automatically fixable
3. Schema patterns match actual data usage
4. Validation output is actionable and concise

### Long-term Goals
1. All CSV files in project pass validation
2. New CSV files can be easily validated against standards
3. Schema patterns are comprehensive and maintainable
4. Validation is integrated into development workflow

## Files to Modify

### Core Implementation
- `R/csv-helpers.R` - Add new validation function
- `NAMESPACE` - Export new function
- `man/` - Add documentation files

### Schema Updates
- `inst/metadata/schemas/core/variable_details.yaml` - Update patterns
- Test files as needed

### Testing
- `tests/testthat/test-csv-helpers.R` - Add comprehensive tests
- Create test CSV files with known issues

## Next Steps

1. **Review this plan** with project stakeholders
2. **Implement Phase 1** - Core validation function
3. **Test with current data** - Identify remaining issues
4. **Iterate on schema patterns** - Make validation practical
5. **Integrate into workflow** - Make validation easy to run

## Questions for Discussion

1. Should validation be integrated into package check process?
2. How strict should schema validation be for research vs production use?
3. Should we maintain backward compatibility with existing non-standard patterns?
4. What level of automatic fixing vs manual intervention is appropriate?