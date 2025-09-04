# Legacy Function Tests Archive

This folder contains test files for legacy functions that have been moved to `R/legacy/`.

## Purpose

These tests are preserved for reference but are **not run** as part of the main test suite. They test the legacy `_fun` functions that have been superseded by modern v3.0.0 implementations.

## Legacy Test Files

### Currently Archived:
- **`test-alcohol-legacy.R`** - Tests for legacy alcohol functions (`binge_drinker_fun`, `low_drink_*_fun`, etc.)
- **`test-bmi-legacy.R`** - Tests for legacy BMI functions (`bmi_fun`, `adjusted_bmi_fun`, `bmi_fun_cat`, etc.)
- **`test-adl-legacy.R`** - Tests for legacy ADL functions (`adl_fun`, `adl_score_5_fun`, etc.)

### Test Failures Expected

⚠️ **These legacy tests will likely fail** because:
1. Legacy functions return string-based missing codes (`"NA(b)"`)
2. Modern missing data handling uses `haven::tagged_na("b")`
3. Legacy functions are no longer exported from the package
4. Dependencies like `if_else2()` are deprecated

## Additional Legacy Functions to Consider

The following legacy functions exist elsewhere in the codebase and may need consolidation:

### From Main R/ Directory:
- **Smoking functions**: `SMK*_fun`, `pack_years_fun`, `time_quit_smoking_fun`
- **Respiratory functions**: `resp_condition_fun*`, `COPD_Emph_der_fun*`
- **Transportation functions**: `active_transport*_fun`
- **Other domain functions**: Various `*_fun` patterns (56+ functions found)

### From Development Branches:
Check `cchsflow-temp` and development branches for additional legacy implementations, particularly:
- `smoking_legacy.R` - Additional smoking function variants
- Other domain-specific legacy implementations

## Migration Strategy

Instead of maintaining these legacy tests:
1. **Focus testing** on modern functions in main `tests/testthat/` directory
2. **Use integration tests** via `rec_with_table()` workflows
3. **Test backward compatibility** through deprecated aliases (if maintained)

## Version History

- **v2.2.0 and earlier**: These were the primary test implementations
- **v3.0.0**: Tests archived, focus shifted to modern function testing
- **v4.0.0 (planned)**: Legacy tests may be removed entirely

## Usage

⚠️ **Do not run these tests directly.** They are preserved for reference and comparison purposes only.

For active testing, use the modern test files in the parent `tests/testthat/` directory.