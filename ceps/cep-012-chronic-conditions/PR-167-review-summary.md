# PR #167 review: Chronic conditions (CCC_071, CCC_091, CCC_101, CCC_111, CCC_121, CCC_151, CCC_280)

**PR**: #167 (`chron-cond` → `feature/v3.0.0-validation-infrastructure`)
**Author**: caitlink12
**Reviewed**: 2026-02-18

## Summary

The PR adds master (`_m`) survey cycles to 7 chronic condition variables by converting existing `_i` (ICES) suffix databases to `_m` in `variable_details.csv`. Only `variable_details.csv` is modified (full-file rewrite). Content changes are limited to 7 variables: CCC_071, CCC_091, CCC_101, CCC_111, CCC_121, CCC_151, CCC_280. Non-CCC variables had formatting-only artifacts (trailing empty field on 3 OH rows).

## Verification methodology

1. **cchsflow worksheets** — direct inspection of `variable_details.csv` and `variables.csv`
2. **MCP metadata database** — queried via `cchs-metadata` MCP tools
3. **Diff analysis** — Python CSV parsing of merge base vs HEAD

## Issues found

### P1: `_s` databases in all 7 in-scope variables

**Confidence: 100**

All 7 in-scope variables retain `cchs2009_s`, `cchs2010_s`, `cchs2012_s` in `variable_details.csv`. The `variables.csv` already uses `_m` equivalents. These should be converted to `cchs2009_m`, `cchs2010_m`, `cchs2012_m`.

### P1: `_NA::a` / `_NA::b` in dummyVariable (21 rows)

**Confidence: 100**

All 7 in-scope variables use `_NA::a` and `_NA::b` in their `dummyVariable` field (3 rows per variable). The naming convention requires `_NAa` and `_NAb` — colons are invalid in identifiers. Pre-existing on the target branch (906 rows total), but should be fixed in reviewed variables.

### Pre-existing issues (not introduced by this PR, score 0)

1. **CCC_151 typo: `ccsh2009_2010_m`** — The `databaseStart` for CCC_151 contains `ccsh2009_2010_m` (swapped 's' and 'h') instead of `cchs2009_2010_m`. This was `ccsh2009_2010_i` in the merge base; the PR correctly changed the suffix from `_i` to `_m` but the `ccsh` typo was already there.

2. **HWTDWTK typo: `ccsh2015_2016_m`** — Same `ccsh` typo in `variables.csv` for weight variable. Unrelated to this PR.

## Variable review

### Source variable mappings (MCP-confirmed)

| Variable | Description | 2001 | 2003 | 2005 | 2007-2014 | 2015+ |
|---|---|---|---|---|---|---|
| CCC_071 | High blood pressure | CCCA_071 | CCCC_071 | CCCE_071 | CCC_071 (default) | CCC_065 |
| CCC_091 | COPD | CCCA_91B | CCCC_91B | CCCE_91F | CCC_91F (2007), CCC_091 (2009+) | CCC_030 |
| CCC_101 | Diabetes | CCCA_101 | CCCC_101 | CCCE_101 | CCC_101 (default) | CCC_095 |
| CCC_111 | Epilepsy | CCCA_111 | CCCC_111 | CCCE_111 | **dropped** | **dropped** |
| CCC_121 | Heart disease | CCCA_121 | CCCC_121 | CCCE_121 | CCC_121 (default) | CCC_085 |
| CCC_151 | Stroke effects | CCCA_151 | CCCC_151 | CCCE_151 | CCC_151 (default) | CCC_090 |
| CCC_280 | Mood disorder | — | CCCC_280 | CCCE_280 | CCC_280 (default) | CCC_195 |

All source variable names confirmed correct via MCP `cchsflow_name` mappings and variable history.

### Key observations

- **CCC_111 (epilepsy)** correctly has only 3 master cycles (2001-2005). The variable was dropped from CCHS after 2005.
- **CCC_280 (mood disorder)** correctly starts from 2003. The mood disorder question was introduced in cycle 2 (2003).
- **CCC_091 (COPD)** has explicit mappings for 2001-2007 master cycles (where the variable name differs) and uses `[CCC_091]` default for 2009-2014. MCP confirms `CCC_091` exists in those cycles. The 2007 cycle uses `CCC_91F` which has an explicit mapping. This is correct.
- **CCC_091 is missing from 2005 and 2007 PUMF data** — the COPD/emphysema question was optional content in those cycles. The variable_details correctly excludes `cchs2005_p` and `cchs2007_2008_p` from the databaseStart.

## L6 integration test

`rec_with_table()` ran successfully for all 9 PUMF cycles. No errors.

| Cycle | CCC_071 | CCC_091 | CCC_101 | CCC_111 | CCC_121 | CCC_151 | CCC_280 |
|---|---|---|---|---|---|---|---|
| cchs2001_p | 100% | 100% | 100% | 100% | 100% | 100% | MISS |
| cchs2003_p | 100% | 100% | 100% | 100% | 100% | 100% | 100% |
| cchs2005_p | 100% | MISS | 100% | 100% | 100% | 100% | 100% |
| cchs2007_2008_p | 100% | MISS | 100% | MISS | 100% | 100% | 100% |
| cchs2009_2010_p | 100% | 100% | 100% | MISS | 100% | 100% | 100% |
| cchs2011_2012_p | 100% | 100% | 100% | MISS | 100% | 100% | 100% |
| cchs2013_2014_p | 100% | 100% | 100% | MISS | 100% | 100% | 100% |
| cchs2015_2016_p | 100% | 100% | 100% | MISS | 100% | 100% | 100% |
| cchs2017_2018_p | 100% | 100% | 100% | MISS | 100% | 100% | 100% |

- MISS = variable not present in that cycle (expected)
- CCC_111 (epilepsy): only in 2001-2003 (dropped 2005+)
- CCC_091 (COPD): missing from 2005, 2007 (optional content those cycles)
- CCC_280 (mood disorder): missing from 2001 (not yet introduced)
- All present variables recode at 100% valid

## Checks performed

- [x] Era boundary defaults (Check 1) — CCC_091 2007 vs 2009+ checked
- [x] databaseStart consistency (Check 2) — `_s` databases flagged
- [x] PUMF vs Master naming (Check 3) — all mappings verified
- [x] Pre-2007 cycle letters (Check 4) — CCCA/CCCC/CCCE confirmed via MCP
- [x] Known error patterns (Check 5) — `ccsh` typo noted as pre-existing
- [x] L6 PUMF integration test — all cycles pass
- [x] MCP cross-reference — source variable families verified via `cchsflow_name`
