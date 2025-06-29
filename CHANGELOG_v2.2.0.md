# cchsflow v2.2.0 Changelog

**Release Date:** 2025-06-28
**Type:** Major infrastructure enhancement
**Focus:** Addition of `_i` cycle database support and validation infrastructure

---

## 🎯 Release Summary

This release represents a **major infrastructure enhancement** adding support for `_i` cycle databases (master/shared continuous data) across the cchsflow ecosystem.

### Key Metrics
- **28 new variables** added
- **74 variables enhanced** with expanded database support
- **6 new R functions** for validation and utilities
- **34 critical dependencies** reviewed and validated

---

## 📊 Variable Changes

### ✅ New Variables Added (28)

- `ADL_score_6`
- `ADM_MOI_I`
- `ADM_YOI_I`
- `ALCDTYP_A`
- `DHH_MS`
- `DHH_MS_A`
- `GEOGPRV_ONCO`
- `HUI06`
- `HUI07`
- `HUI07A`
- `HUI08`
- `HUI09`
- `HWTDBMI`
- `HWTDHTM`
- `HWTDWTK`
- `MAM_037`
- `PACFLEI`
- `RAC_6D`
- `SDCDGT_A`
- `SDCDGT_B`
- ... and 8 more

### 🔄 Variables Enhanced with _i Database Support (74)

These variables received expanded database coverage through addition of `_i` cycle databases:

- `ADL_01`
- `ADL_02`
- `ADL_03`
- `ADL_04`
- `ADL_05`
- `ADL_06`
- `ADL_07`
- `ALCDTTM`
- `ALW_2A1`
- `ALW_2A2`
- `ALW_2A3`
- `ALW_2A4`
- `ALW_2A5`
- `ALW_2A6`
- `ALW_2A7`
- ... and 59 more

*See detailed analysis for complete enumeration*

### ⚠️ Other Variable Changes Requiring Review (263)

Variables with changes beyond `_i` database additions:

- `active_transport` - Requires individual review
- `ADL_01_A` - Requires individual review
- `ADL_02_A` - Requires individual review
- `ADL_03_A` - Requires individual review
- `ADL_04_A` - Requires individual review
- `ADL_05_A` - Requires individual review
- `ADL_06_A` - Requires individual review
- `ADL_der` - Requires individual review
- `ADL_score_5` - Requires individual review
- `ADLF6R` - Requires individual review

---

## 🔧 Code Changes

### ✅ New R Functions (6)

- `adl_score_6.R`
- `csv-helpers.R`
- `csv-utils.R`
- `DemPoRT_ICES_code.R`
- `schema-validation.R`
- `test code.R`

---

## 🔍 Critical Dependencies

### Variables Affecting Derived Functions (34)

These enhanced variables have dependencies with derived variable functions:

- `active_transport`
- `ADL_der`
- `ADL_score_5`
- `ALWDVLTR_der`
- `ALWDVSTR_der`
- `binge_drinker`
- `COPD_Emph_der`
- `DHHGAGE_C`
- `diet_score`
- `diet_score_cat3`
- `energy_exp`
- `HWTGBMI_der`
- `HWTGBMI_der_cat4`
- `HWTGCOR_der`
- `immigration_der`
- `low_drink_score`
- `low_drink_score1`
- `number_conditions`
- `pack_years_cat`
- `pack_years_der`
- `pct_time_der`
- `pct_time_der_cat10`
- `RACDPAL`
- `resp_condition_der`
- `SMK_09A_B`
- `SMK_09A_cont`
- `SMKDSTY_A`
- `SMKG040_cont`
- `SMKG09C`
- `SMKG203_cont`
- `SMKG207_cont`
- `smoke_simple`
- `SPS5_der`
- `time_quit_smoking`

**✅ Validation Status:** All dependencies reviewed and confirmed compatible.

---

## 📝 Generation Notes

### Change Detection Methodology
This changelog was generated using automated analysis comparing:
- **Source:** cchsflow-temp working files
- **Target:** cchsflow baseline files
- **Analysis date:** 2025-06-28

*This changelog was automatically generated and can serve as a template for future releases.*
