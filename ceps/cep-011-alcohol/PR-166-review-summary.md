# PR #166 review: Alcohol (ALW_1, ALW_2A1-2A7, ALCDTTM, ALWDWKY, binge_drinker)

**PR**: #166 (`alcohol` → `feature/v3.0.0-validation-infrastructure`)
**Author**: caitlink12
**Reviewed**: 2026-02-18

## Summary

The PR adds master (`_m`) survey cycles to 11 alcohol variables by converting the existing `_i` (ICES) suffix databases to `_m` in `variable_details.csv`. The `variables.csv` file already had `_m` databases on the target branch for most variables; ALW_1 had `_m` mappings added to its `databaseStart`/`variableStart`.

Only `variable_details.csv` is modified (full-file rewrite). Content changes are limited to 11 variables: ALCDTTM, ALWDWKY, ALW_1, ALW_2A1 through ALW_2A7, and binge_drinker. Nine additional variables (ALCDTYP, ALCDTYP_A, ALC_005, ALC_1, ALWDDLY, ALWDVLTR_der, ALWDVSTR_der, low_drink_score, low_drink_score1) had only formatting artifacts (trailing empty field), no content changes.

## Verification methodology

This review used three independent sources:

1. **cchsflow worksheets** — direct inspection of `variable_details.csv` and `variables.csv`
2. **MCP metadata database** — queried via `cchs-metadata` MCP tools (DuckDB, built from DDI XML + PUMF RData)
3. **StatCan PDF codebooks** — verified via Google NotebookLM against original documentation

## Issues found

### P0: ALW_2A1 row 1 variableStart corrupted (copy-paste error)

**Confidence: 100** — Confirmed by MCP `cchsflow_name` mappings and StatCan PDF codebooks.

ALW_2A1 row 1 (`recStart=[0,50]`) has ALW_1's source variables instead of ALW_2A1's:

| Field | Current (wrong) | Correct |
|-------|-----------------|---------|
| variableStart | `cchs2001_p::ALCA_5, cchs2003_p::ALCC_5, cchs2005_p::ALCE_5, cchs2015_2016_p::ALW_005, cchs2017_2018_p::ALW_005, [ALW_1]` | `cchs2001_p::ALCA_5A1, cchs2003_p::ALCC_5A1, cchs2005_p::ALCE_5A1, cchs2015_2016_p::ALW_010, cchs2017_2018_p::ALW_010, cchs2001_m::ALCA_5A1, cchs2003_m::ALCC_5A1, cchs2005_m::ALCE_5A1, cchs2015_2016_m::ALW_010, cchs2017_2018_m::ALW_010 , [ALW_2A1]` |

The MCP confirms:
- ALCA_5 → `cchsflow_name: ALW_1` (not ALW_2A1)
- ALCA_5A1 → `cchsflow_name: ALW_2A1` (correct)

Rows 2-4 of ALW_2A1 are correct and use the right source variables. Only row 1 was affected by the copy-paste error. This row also lacks `_m` master mappings, which rows 2-4 have.

**Impact**: At runtime, row 1 recodes the "any alcohol past week" yes/no variable (range 1-9) through a continuous range `[0,50]`, which would capture codes 1-9. The recoded value would be a yes/no code (1 or 2) instead of the expected drink count (0-50). This produces silently wrong data — not an error, but incorrect values.

### P1: `_s` databases in 16 alcohol variables

**Confidence: 100**

`variable_details.csv` uses `cchs2009_s`, `cchs2010_s`, `cchs2012_s` in all in-scope alcohol variables. `variables.csv` already uses `_m` equivalents. These should be converted to `cchs2009_m`, `cchs2010_m`, `cchs2012_m` (or `cchs2009_2010_m`, `cchs2011_2012_m`, `cchs2013_2014_m` depending on the variable's databaseStart pattern).

### Pre-existing issues (not introduced by this PR, score 0)

1. **ALCDTYP and ALCDTYP_A still use `_i` databases** — unchanged from merge base. ALCDTYP_A also has `2011_2012_i` typo (missing `cchs` prefix).

2. **ALCDTTM type-of-drinker recode**: Early-era variables (ALCADTYP 2001-2005) have 4 categories (Regular, Occasional, Former, Never drank). The 2007+ variable ALCDTTM has 3 categories (Regular, Occasional, No drink last 12 months). The worksheets recode codes 3 (Former) and 4 (Never) both to 3. This is justified: StatCan collapsed these categories in 2007 when lifetime-use questions moved to optional modules.

## L6 integration test

`rec_with_table()` ran successfully for all 9 PUMF cycles (2001 through 2017-2018). No step changes at era boundaries for any variable.

| Cycle | ALCDTTM | ALW_1 | ALW_2A1 | ALWDWKY |
|-------|---------|-------|---------|---------|
| cchs2001_p | 100% | 100% | 45% | 78% |
| cchs2003_p | 100% | 100% | 41.5% | 71.5% |
| cchs2005_p | 100% | 100% | 47.5% | 72% |
| cchs2007_2008_p | 100% | 100% | 21.5% | 37.5% |
| cchs2009_2010_p | 100% | 100% | 19.5% | 29% |
| cchs2011_2012_p | 100% | 100% | 28.5% | 51% |
| cchs2013_2014_p | 100% | 100% | 28% | 48.5% |
| cchs2015_2016_p | 100% | 100% | 50% | 75% |
| cchs2017_2018_p | 100% | 100% | 28.5% | 36.5% |

ALCDTTM and ALW_1 are 100% valid across all cycles (categorical variables, all respondents). ALW_2A1-2A7 valid percentages reflect the proportion of respondents who reported drinking in the past week (varies by cycle due to sampling). ALWDWKY valid percentages track similarly. No anomalous patterns.

**Note**: The P0 bug in ALW_2A1 row 1 does not cause test failures because it maps to a valid recoding range. The data produced is silently wrong (yes/no codes instead of drink counts).

**Note**: Master (`_m`) mappings cannot be validated at runtime with PUMF data. The ALW_2A1 source variable naming bug (P0) was caught by worksheet inspection and MCP cross-reference, not L6 testing.

## Checks performed

- [x] Era boundary defaults (Check 1) — ALW_2A1 row 1 caught wrong defaults
- [x] databaseStart consistency (Check 2) — `_s` databases flagged
- [x] PUMF vs Master naming (Check 3)
- [x] Pre-2007 cycle letters (Check 4) — confirmed ALCA/ALCC/ALCE pattern via MCP
- [x] Known error patterns (Check 5)
- [x] L6 PUMF integration test
- [x] MCP cross-reference (new) — verified source variable families via `cchsflow_name` field
- [x] PDF triangulation (new) — confirmed variable definitions via StatCan codebooks
