# Legacy Functions Archive

This folder contains legacy function implementations that have been superseded by modern v3.0.0 functions in the main R/ directory.

## Purpose

These files are preserved for reference and comparison purposes only. They are **not exported** and **not tested** in the main package workflow.

## Legacy Function Files

### Currently Archived:
- **`adl-legacy.R`** - Legacy ADL (Activities of Daily Living) functions
  - `adl_fun()`, `adl_score_5_fun()`, `adl_score_6_fun()`, etc.
  - Replaced by: `assess_adl()`, `score_adl()`, `score_adl_6()`

- **`alcohol-legacy.R`** - Legacy alcohol consumption and risk assessment functions  
  - `binge_drinker_fun()`, `low_drink_short_fun()`, `low_drink_long_fun()`, etc.
  - Replaced by: `calculate_binge_drinking()`, `calculate_drinking_risk_short()`, `calculate_drinking_risk_long()`

- **`bmi-legacy.R`** - Legacy BMI calculation and categorization functions
  - `bmi_fun()`, `adjusted_bmi_fun()`, `bmi_fun_cat()`, etc.  
  - Replaced by: `calculate_bmi()`, `adjust_bmi()`, `categorize_bmi()`

- **`physical-activity-legacy.R`** - Legacy energy expenditure functions
  - `energy_exp_fun()`, `energy_exp_fun_cat()`
  - Replaced by: `calculate_energy_exp()`, `categorize_energy_exp()`

- **`diet-legacy.R`** - Legacy diet score functions
  - `diet_score_fun()`, `diet_score_fun_cat()`
  - Replaced by: `calculate_diet_score()`, `categorize_diet_score()`

- **`education-legacy.R`** - Legacy education derived function
  - `EDUDR04_fun()`
  - Replaced by: `derive_EDUDR04_2015plus()`

- **`respiratory-condition-legacy.R`** - Legacy CCC_091 functions
  - `CCC_091_fun1()`, `CCC_091_fun2()`
  - Replaced by: `derive_CCC_091_2001to2003()`, `derive_CCC_091_2005to2008()`

- **`immigration-legacy.R`** - Legacy immigration function
  - `immigration_fun()`
  - Replaced by: `categorize_immigration()`

- **`percent-time-canada-legacy.R`** - Legacy percent time functions
  - `pct_time_fun()`, `pct_time_fun_cat()`
  - Replaced by: `calculate_pct_time()`, `categorize_pct_time()`

## Modern Function Workflow

The main `rec_with_table()` workflow uses modern functions via CSV metadata:
- `inst/extdata/variable_details.csv` references modern function names (`Func::assess_adl`, not `Func::adl_fun`)
- Users calling `rec_with_table()` automatically get modern implementations
- Legacy functions are completely bypassed in normal workflows

## Key Differences: Legacy vs Modern

| Aspect | Legacy Functions | Modern Functions |
|--------|------------------|------------------|
| Missing Data | String-based (`"NA(b)"`) | `haven::tagged_na("b")` |
| Naming | `_fun` suffix | Verb-first (`assess_`, `calculate_`) |
| Documentation | Minimal | Comprehensive with examples |
| Testing | Basic | Extensive with edge cases |
| Dependencies | `if_else2()` (deprecated) | `dplyr::if_else()` |

## Migration Guide

For users with direct function calls, see `R/deprecated-aliases.R` for backward compatibility wrappers that provide deprecation warnings and redirect to modern functions.

## Version History

- **v2.2.0 and earlier**: Legacy functions were the primary implementation
- **v3.0.0**: Modern functions introduced, legacy functions archived

## Usage

⚠️ **Do not use these functions directly.** They are preserved for reference only.

For active development, use the modern functions in the main R/ directory.