# cchsflow Development TODO List

*Last updated: 2025-07-04*

## 🎯 Current Priority (Post-BMI v3.0.0)

### Next Function Modernization
- [ ] **Apply BMI v3.0.0 patterns to smoking functions**
  - Use vector-aware helpers from missing-data-helpers.R
  - Implement DRY tagged NA handling
  - Update to calculate_*, categorize_*, score_* naming conventions
  - Status: Ready to begin (BMI patterns established)

- [ ] **Apply BMI v3.0.0 patterns to alcohol functions**
  - Use established helper architecture
  - CSV-driven validation bounds
  - Enhanced testing with rec_with_table() integration
  - Status: Ready to begin

- [ ] **Apply BMI v3.0.0 patterns to ADL functions**
  - Modernize ADL scoring with vector processing
  - Implement consistent missing data handling
  - Update function naming conventions
  - Status: Ready to begin

## 📋 Medium Priority 

### Infrastructure Improvements
- [ ] **Review and consolidate function naming conventions**
  - Current: calculate_, categorize_, score_, adjust_, validate_
  - Consider consolidating categorize_*() and classify_*()
  - Define clear usage patterns for each verb

- [ ] **Enhance CSV-driven validation**
  - Move remaining hard-coded constants to variable_details.csv
  - Implement validation bounds for other domains (smoking, alcohol, ADL)
  - Test performance with large datasets

- [ ] **Documentation improvements**
  - Update vignettes to reflect v3.0.0 patterns
  - Create tutorials for new helper function architecture
  - Document CSV-driven validation approach

## 🔧 Low Priority

### Testing and Quality
- [ ] **Implement comprehensive testing for other domains**
  - Apply BMI testing patterns to smoking, alcohol, ADL
  - Create integration tests with rec_with_table()
  - Performance testing with large datasets

- [ ] **Code quality improvements**
  - Run lintr/styler across entire codebase
  - Ensure all functions have roxygen2 documentation
  - Standardize internal function versioning

---

## ✅ Completed (v3.0.0 BMI Modernization)

*Archived on 2025-07-04 - see git history for detailed progress*

- ✅ BMI functions completely modernized with v3.0.0 architecture
- ✅ Vector-aware processing with DRY helper functions
- ✅ CSV-driven validation (removed hard-coded constants)
- ✅ Comprehensive testing (132 tests passing)
- ✅ rec_with_table() integration verified
- ✅ Development folder organized with AI instructions
- ✅ Version alignment across all schemas (v3.0.0)

---

*Next update: After smoking function modernization begins*