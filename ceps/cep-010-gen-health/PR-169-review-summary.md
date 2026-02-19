# PR #169 review: General Health (GEN_07, GEN_10)

**PR**: #169 (`gen-health` → `v3.0.0-validation-infrastructure`)
**Author**: caitlink12
**Reviewed**: 2026-02-12

## Summary

The PR adds master (`_m`) survey cycles to GEN_07 and GEN_10 by converting the existing `_i` (ICES) suffix databases to `_m` in `variable_details.csv`. The `variables.csv` file already had `_m` databases on the target branch, so no changes were needed there.

Only `variable_details.csv` is modified (full-file rewrite: 3722+/3722-). Content changes are limited to GEN_07 (8 rows) and GEN_10 (7 rows).

## Issues found

### P0: GEN_10 wrong master source variable names (pre-existing, carried into PR)

**Confidence: 100** — Confirmed against cchsflow-docs data dictionary.

All 7 GEN_10 rows map pre-2007 master databases to the wrong source variable:

| Database | Current (wrong) | Correct |
|----------|-----------------|---------|
| cchs2001_m | GENA_**01** | GENA_**10** |
| cchs2003_m | GENC_**01** | GENC_**10** |
| cchs2005_m | GENE_**01** | GENE_**10** |

- **GENA_01** = "Self-perceived health" (should map to cchsflow GEN_07)
- **GENA_10** = "Sense of belonging to local community" (should map to cchsflow GEN_10)

This bug was pre-existing on the target branch (with `_i` suffix: `cchs2001_i::GENA_01`). The PR converted `_i` → `_m` without correcting the source variable name. At runtime, GEN_10 would return self-perceived health data instead of sense-of-belonging data for master cycles 2001, 2003, and 2005.

**Recommendation**: Fix the source variable names as part of this PR since GEN_10 is already being edited.

### P2: Space after `::` in GEN_07 variableStart (pre-existing)

**Confidence: 50** — Pre-existing on target branch. May or may not affect runtime.

`cchs2017_2018_p:: GEN_020` and `cchs2017_2018_m:: GEN_020` have a space after `::` in all 8 GEN_07 rows. Same pattern existed on target with `_i` suffix.

### Pre-existing issues (not introduced by this PR)

1. **`_s`/`_m` mismatch**: `variable_details.csv` uses `cchs2009_s`, `cchs2010_s`, `cchs2012_s` while `variables.csv` uses `cchs2009_m`, `cchs2010_m`, `cchs2012_m`. This is the known deprecated share file pattern.

2. **dummyVariable colon pattern**: `GEN_07_cat5_NA::a` and `GEN_10_cat4_NA::a` use colons instead of the `_NAa`/`_NAb` convention. Pre-existing.

3. **DHHGAGE_E removed**: 23 rows for this variable exist on the target branch but not in the PR. Out of scope for this review.

## L6 integration test

`rec_with_table()` ran successfully for all 9 PUMF cycles. Since this PR only changes master (`_m`) mappings, PUMF results serve as a regression test confirming the full-file rewrite didn't break existing functionality.

| Cycle | N | GEN_07 valid % | GEN_10 valid % |
|-------|---|---------------|---------------|
| cchs2001_p | 200 | 100% | 100% |
| cchs2003_p | 200 | 100% | 100% |
| cchs2005_p | 200 | 100% | 100% |
| cchs2007_2008_p | 200 | 100% | 100% |
| cchs2009_2010_p | 200 | 100% | 100% |
| cchs2011_2012_p | 200 | 100% | 100% |
| cchs2013_2014_p | 200 | 100% | 100% |
| cchs2015_2016_p | 200 | 100% | 100% |
| cchs2017_2018_p | 200 | 100% | 100% |

Category distributions are stable across all cycles — no era boundary step changes.

**Note**: Master (`_m`) mappings cannot be validated at runtime with PUMF data. The GEN_10 source variable naming bug (P0) can only be caught by worksheet inspection, not L6 testing.

## Checks performed

- [x] Era boundary defaults (Check 1)
- [x] databaseStart consistency (Check 2)
- [x] PUMF vs Master naming (Check 3) — **caught P0**
- [x] Pre-2007 cycle letters (Check 4)
- [x] Known error patterns (Check 5)
- [x] dummyVariable naming (Check 5b)
- [x] Swapped recEnd values (Check 5c)
- [x] L6 PUMF integration test
