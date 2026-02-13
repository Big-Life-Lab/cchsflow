# PR #148 review summary: diet score

**PR**: #148 (diet score)
**Author**: caitlink12, rafdoodle (latest commit)
**Target branch**: adl-additions
**Review date**: 2026-02-11
**Commit reviewed**: 16a8f3ab

## Scope

Reviewed 9 derived FVC/diet variables (FVCDFRU, FVCDSAL, FVCDPOT, FVCDCAR, FVCDVEG, FVCDJUI, FVCDTOT, diet_score, diet_score_cat3) and 30 raw FVC variables (FVC_1A through FVC_6E) across PUMF and Master databases, cycles 2001 through 2017-2018.

## Changes in this PR

1. Added `_m` (master) databases for all FVC/diet variables (cchs2001_m through cchs2017_2018_m)
2. Added `_s` (deprecated share) databases
3. Added explicit `variableStart` mappings for `_m` databases (pre-2007 cycle letters, 2015+ renames)
4. Added `units` field to FVCD* variables (times/day)
5. Fixed `cchs20013_2014_m` typo to `cchs2013_2014_m` (FVCDPOT, FVCDJUI)
6. Added `cchs2017_2018_m` to diet_score and diet_score_cat3
7. Expanded FVC_*A-E raw variables from `_s`-only to full master cycle coverage
8. ADL variables also modified (outside stated diet scope, not reviewed)

## Post-approval commit

yulric approved on 2025-12-04 at commit a612bdee. rafdoodle pushed commit 16a8f3ab on 2026-02-10 (after approval), adding cchs2017_2018_m and units fields.

## Checks performed

### L3-L5 worksheet checks

| Check | Result |
|-------|--------|
| Era boundary defaults | PASS - All FVCD* variables have explicit 2015+ and pre-2007 mappings; `[VAR]` default only covers 2007-2014 |
| databaseStart consistency | PASS - variables.csv and variable_details.csv match for all FVC/diet vars |
| PUMF/Master naming | PASS - _m databases use correct ungrouped names |
| Pre-2007 cycle letters | PASS - A (2001), C (2003), E (2005) correctly applied |
| Known error patterns | One issue found (see below) |
| DV specification review | PASS - diet_score_fun() inputs match worksheet; diet_score_fun_cat() correctly chains |
| Unit tests | Exist but minimal (2 tests each for diet_score_fun and diet_score_fun_cat) |

### L6 PUMF integration test

`rec_with_table()` ran successfully for all 12 PUMF cycles. Cross-cycle prevalence:

| Cycle | N | diet_score valid % | diet_score_cat3 distribution |
|-------|---|-------------------|------------------------------|
| cchs2001_p | 200 | 99.5% | 43 poor, 145 fair, 11 adequate |
| cchs2003_p | 200 | 94.0% | 24 poor, 139 fair, 25 adequate |
| cchs2005_p | 200 | 56.0% | 11 poor, 92 fair, 9 adequate (optional content) |
| cchs2007_2008_p | 200 | 94.5% | 26 poor, 137 fair, 26 adequate |
| cchs2009_2010_p | 200 | 93.5% | 21 poor, 142 fair, 24 adequate |
| cchs2011_2012_p | 200 | 88.5% | 15 poor, 128 fair, 34 adequate |
| cchs2013_2014_p | 200 | 91.0% | 14 poor, 136 fair, 32 adequate |
| cchs2015_2016_p | 200 | 95.5% | 7 poor, 161 fair, 23 adequate |
| cchs2017_2018_p | 200 | 1.0% | 2 fair, 198 NA(a) (optional content) |

No step changes at era boundaries. The 2005 and 2017-2018 dips are expected (FVC was optional content in those cycles). The 2014-2015 transition is clean, confirming 2015+ variable renames (FVCDVFRU, FVCDVGRN, etc.) are correctly mapped.

Master (`_m`) mappings validated by worksheet checks only -- no runtime data available for L6 testing.

## Issues found

### Issue 1: `chs` typo in FVC_* database names (confidence: 100)

All 30 raw FVC variables (FVC_1A through FVC_6E) use `chs2011_2012_m` and `chs2013_2014_m` instead of `cchs2011_2012_m` and `cchs2013_2014_m` in both `variables.csv` and `variable_details.csv`. The leading `c` is missing.

- This typo was **introduced by this PR** (the target branch does not have it for FVC_* variables)
- The pattern exists pre-existing in other variables (ADL etc.) on the target branch, which is likely where it was copied from
- Impact: These database names will fail to match any actual CCHS database, causing FVC_* variables to be unavailable when processing master data for 2011-2012 and 2013-2014 cycles
- Fix: Replace `chs2011_2012_m` with `cchs2011_2012_m` and `chs2013_2014_m` with `cchs2013_2014_m` in all 30 FVC_* variables across both CSV files

## CEP artifacts

- `ceps/cep-007-diet/PR-148-review-summary.md` (this file)
- `ceps/cep-007-diet/integration-test-diet.R` (executable PUMF integration test)
- `ceps/cep-007-diet/diet-pumf-integration-test.csv` (test results)
