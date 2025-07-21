# CSV Validation Improvements - Implementation TODO

## Phase 1: Core Validation Function ⚡ HIGH PRIORITY

### 1.1 Function Implementation
- [ ] Create `validate_csv_comprehensive()` function in `R/csv-helpers.R`
  - [ ] Basic structure validation category
  - [ ] Schema compliance validation category  
  - [ ] Column order validation category
  - [ ] Data quality validation category
  - [ ] Summary reporting with PASS/FAIL/WARNING status
  - [ ] Verbose mode for detailed error messages

### 1.2 Documentation
- [ ] Add roxygen2 documentation for new function
- [ ] Include usage examples in documentation
- [ ] Add function to NAMESPACE for export

### 1.3 Integration Testing
- [ ] Test function with `inst/extdata/variable_details.csv`
- [ ] Verify output format matches R CMD check style
- [ ] Test both verbose and summary modes

## Phase 2: Schema Pattern Updates 🔧 MEDIUM PRIORITY

### 2.1 Current Pattern Issues Analysis
- [ ] Document all pattern violations in `variable_details.csv`
- [ ] Categorize violations by field (`dummyVariable`, `variableStart`, `recEnd`)
- [ ] Identify common patterns in current data

### 2.2 Schema Updates
- [ ] Update `dummyVariable` patterns in `variable_details.yaml`
  - [ ] Support for `N/A` values
  - [ ] Support for categorical dummy patterns (e.g., `ADL_01_cat2_1`)
  - [ ] Support for function references (e.g., `catN/A_Func::function_name`)

- [ ] Update `variableStart` patterns in `variable_details.yaml`
  - [ ] Support for `DerivedVar::[var1, var2, ...]` syntax
  - [ ] Support for database references (e.g., `cchs2001_p::VARIABLE`)
  - [ ] Support for bracketed references (e.g., `[VARIABLE]`)

- [ ] Update `recEnd` patterns in `variable_details.yaml`
  - [ ] Support for `Func::function_name` syntax
  - [ ] Support for `NA::a`, `NA::b` patterns
  - [ ] Support for complex categorical patterns

### 2.3 Schema Testing
- [ ] Test updated schema against current data
- [ ] Verify patterns match intended data types
- [ ] Ensure backward compatibility

## Phase 3: Column Order Resolution 📋 MEDIUM PRIORITY

### 3.1 Schema Column Updates
- [ ] Add missing columns to `variable_details.yaml` schema:
  - [ ] `ICES.confirmation`
  - [ ] `review`
- [ ] Update `expected_column_order` in schema

### 3.2 Column Order Fixing
- [ ] Fix current column order issue:
  - [ ] Move `lastUpdated` before `status` in expected order
  - [ ] OR update CSV file to match schema order
- [ ] Implement automatic column reordering function (optional)

### 3.3 Column Validation Enhancement
- [ ] Update validation to handle extra columns gracefully
- [ ] Provide clear guidance on where to add new columns
- [ ] Test column order validation with fixed schema

## Phase 4: Testing and Quality Assurance ✅ HIGH PRIORITY

### 4.1 Unit Tests
- [ ] Add tests for `validate_csv_comprehensive()` in `test-csv-helpers.R`
  - [ ] Test with valid CSV (all PASS)
  - [ ] Test with column order issues (FAIL)
  - [ ] Test with pattern violations (WARNING)
  - [ ] Test verbose vs summary modes

### 4.2 Integration Tests
- [ ] Test with multiple CSV files in project
- [ ] Test with edge cases (empty files, malformed CSV)
- [ ] Test schema validation after pattern updates

### 4.3 Regression Testing
- [ ] Ensure existing `standardise_csv()` functionality unchanged
- [ ] Verify all existing tests still pass
- [ ] Test backward compatibility

## Phase 5: Documentation and Examples 📚 LOW PRIORITY

### 5.1 Usage Documentation
- [ ] Create usage examples for new validation function
- [ ] Update package vignettes if needed
- [ ] Add troubleshooting guide for common validation issues

### 5.2 Schema Documentation
- [ ] Document new pattern syntax in schema files
- [ ] Create examples of valid patterns for each field
- [ ] Document column order requirements

## Implementation Notes

### Dependencies
- Requires existing validation infrastructure
- May need `yaml` package for schema loading
- Should integrate with existing `csv-helpers.R` and `csv-utils.R`

### Testing Strategy
1. Start with current `variable_details.csv` as test case
2. Create minimal test CSV files for each validation category
3. Test edge cases and error conditions
4. Verify output format consistency

### Rollout Plan
1. Implement and test core function
2. Update schema patterns incrementally
3. Fix column order issues
4. Deploy with comprehensive tests

## Quick Wins (Can Implement Immediately)

### 🚀 Ready to Implement Now
- [ ] `validate_csv_comprehensive()` function skeleton
- [ ] Basic documentation and examples
- [ ] Initial testing with current CSV file

### 🔄 Requires Investigation
- [ ] Exact schema pattern syntax needed
- [ ] Column order preference (fix CSV vs fix schema)
- [ ] Integration points with existing validation

### 🤔 Needs Discussion
- [ ] How strict should pattern validation be?
- [ ] Should we auto-fix issues or just report them?
- [ ] Integration with package development workflow?

## Success Metrics
- [ ] `validate_csv_comprehensive("inst/extdata/variable_details.csv")` shows clear status
- [ ] All validation categories show appropriate PASS/FAIL/WARNING
- [ ] Output is concise and actionable
- [ ] Schema patterns match 95%+ of current data patterns