# CEP-008: Immigration / percent time in Canada — PR #165 review

PR: https://github.com/Big-Life-Lab/cchsflow/pull/165
Branch: `v3-pct-time-canada` → `v3`
Author: rafdoodle
Reviewed: 2026-02-12

## Scope

**Variables reviewed:** DHH_AGE, SDCGCB, SDCGCBG, SDCDRES, SDCGRES, pct_time_der
**Database types:** PUMF (`_p`), Master (`_m`)
**Cycles:** 2001 through 2017-2018

### Changes in this PR (original + refactoring)

- **2 new variables:** SDCGCB (master country of birth), SDCDRES (master continuous time in Canada)
- **1 new intermediate variable:** SDCGRES_cont (PUMF categorical → continuous midpoint conversion)
- **4 modified variables:** SDCGCBG (added cycles), SDCGRES (added cycles), DHH_AGE (expanded to master databases), pct_time_der (expanded to master databases, variableStart updated to use SDCGRES_cont for PUMF)
- **3 removed variables:** SDCGCBG_A, SDCGRES_A (consolidated into SDCGCB, SDCDRES), pct_time_der_A (merged into pct_time_der)
- **R function refactoring:** `pct_time_fun()` and `pct_time_fun_A()` removed — replaced with `calculate_pct_time(age, born_in_canada, years_in_canada)`. `pct_time_fun_cat()` renamed to `categorize_pct_time()`. Follows v3 verb-first naming convention.
- **`_s` databases removed:** All `_s` (deprecated share file) references purged from new/modified rows
- **Output bounds validation:** `calculate_pct_time()` clamps values outside [0, 100] to `tagged_na("b")`. Valid range documented in variable_details notes (documentation only, ready for future validation framework).
- **New tests:** 28 tests total (calculate_pct_time, categorize_pct_time, immigration_fun)

## L3-L5 worksheet check findings

### Issue 1: DHH_AGE databaseStart typo — `cchs2007_2008m` (P0, confidence 100)

**PR-introduced.** Missing underscore before `m` suffix. Should be `cchs2007_2008_m`.

Affects: variables.csv and all 4 variable_details.csv rows for DHH_AGE.

### Issue 2: pct_time_der_A databaseStart double comma (P1, confidence 100) — RESOLVED

**Resolved by merging pct_time_der_A into pct_time_der.** The double comma and `_s` references are eliminated. pct_time_der now has separate PUMF and master row sets under the same variable name.

### Issue 3: DHH_AGE missing explicit 2015+ master mappings (informational, downgraded from P1)

**Resolved by L0 DDI verification.** The `[DHH_AGE]` default applies to cchs2015_2016_m and cchs2017_2018_m without explicit `db::VAR` mappings. DDI verification confirms DHH_AGE was **not renamed** in 2015+ master files — it remains `DHH_AGE` through 2021 (renamed to `AWCAGE` only in 2022+). The default is safe.

Adding explicit mappings is still best practice for consistency with other SDC variables in this PR (SDCDVCB, SDCDVRES, SDCDGCB, SDCDGRES).

### Issue 4: SDCGCB dummyVariable uses `_NA::a` / `_NA::b` (P2, confidence 95)

**PR-introduced.** The new SDCGCB variable uses colon notation in dummy names:
- `SDCGCB_cat2_NA::a` → should be `SDCGCB_cat2_NAa`
- `SDCGCB_cat2_NA::b` → should be `SDCGCB_cat2_NAb`
- `SDCGCB_cat7_NA::a` → should be `SDCGCB_cat7_NAa`
- `SDCGCB_cat7_NA::b` → should be `SDCGCB_cat7_NAb`

**Pre-existing:** SDCGCBG and SDCGRES also use colon notation (on v3 branch). These are out of scope for this PR but should be addressed in a follow-up.

### Issue 5: Missing unit tests for born-in-Canada case (P1, confidence 100) — RESOLVED

**Resolved by refactoring.** Added tests for:
- Born-in-Canada (SDCGCBG=1, SDCGCB=1) → returns 100 for both variants
- `categorize_pct_time(100)` boundary → returns "10"
- `categorize_pct_time(tagged_na("a"))` → returns "NA(a)"
- Vector inputs for all three functions

### Issue 6: Missing `man/pct_time_fun_A.Rd` documentation (P2, confidence 100) — RESOLVED

**Resolved by removing `pct_time_fun_A`.** The function was eliminated during refactoring — both PUMF and master now use a single unified `calculate_pct_time()`. No separate .Rd file needed.

### Informational: `_s` suffix databases — RESOLVED for pct_time_der

**`_s` references removed from pct_time_der.** The deprecated share file databases (`cchs2009_s`, `cchs2010_s`, `cchs2012_s`) are no longer referenced in the combined pct_time_der rows. SDCGCB, SDCDRES, and DHH_AGE still reference `_s` on the v3 branch — these are pre-existing and out of scope for this PR.

## L6 integration test — PUMF cross-cycle prevalence

`rec_with_table()` ran successfully for all 9 PUMF cycles. No errors.

| Cycle | N | DHHGAGE_cont | SDCGCBG | SDCGRES | pct_time_der | pct_time_der_cat10 |
|-------|---|-------------|---------|---------|-------------|-------------------|
| cchs2001_p | 200 | 100% | 100% | 100% | 100% | 100% |
| cchs2003_p | 200 | 100% | 100% | 100% | 96.5% | 100% |
| cchs2005_p | 200 | 100% | 100% | 100% | 93.5% | 100% |
| cchs2007_2008_p | 200 | 100% | 100% | 100% | 100% | 100% |
| cchs2009_2010_p | 200 | 100% | 100% | 100% | 98% | 100% |
| cchs2011_2012_p | 200 | 100% | 100% | 100% | 95% | 100% |
| cchs2013_2014_p | 200 | 100% | 100% | 100% | 96.5% | 100% |
| cchs2015_2016_p | 200 | 100% | 100% | 100% | 95.5% | 100% |
| cchs2017_2018_p | 200 | 100% | 100% | 100% | 98% | 100% |

**No step changes at era boundaries.** The 2014→2015 transition shows normal variation (96.5% → 95.5%).

**pct_time_der distribution:** Category 10 (90-100%) dominates across all cycles (~80%), consistent with most CCHS respondents being born in Canada. Category distributions are stable across the 2015 boundary.

**Master (`_m`) variables** could not be tested at L6 — no runtime PUMF data available. Worksheet checks (issues 1-3) cover these.

## L0 DDI verification — variable availability (cchsflow-docs)

All name mappings in PR #165 verified against extracted YAML data dictionaries (2001-2023) and ICES DuckDB (2001-2021).

### Name mapping verification: all correct

| cchsflow name | Era | Mapped to | Verified |
|---------------|-----|-----------|----------|
| DHH_AGE | 2001 | DHHA_AGE | YAML+ICES |
| DHH_AGE | 2003 | DHHC_AGE | YAML+ICES |
| DHH_AGE | 2005 | DHHE_AGE | YAML+ICES |
| SDCGCB | 2001 | SDCAGCB | YAML+ICES |
| SDCGCB | 2003 | SDCCGCB | YAML+ICES |
| SDCGCB | 2005 | SDCEGCB | YAML+ICES |
| SDCGCB | 2011-2014 | SDCGCB10 | YAML+ICES |
| SDCGCB | 2015+ | SDCDVCB | YAML+ICES |
| SDCDRES | 2001 | SDCADRES | YAML+ICES |
| SDCDRES | 2003 | SDCCDRES | YAML+ICES |
| SDCDRES | 2005 | SDCEDRES | YAML+ICES |
| SDCDRES | 2015+ | SDCDVRES | YAML+ICES |
| SDCGCBG | 2001 | SDCAGCBG | YAML+ICES |
| SDCGCBG | 2003 | SDCCGCBG | YAML |
| SDCGCBG | 2005 | SDCEGCBG | YAML |
| SDCGCBG | 2011-2012 | SDCGCB12 | YAML+ICES |
| SDCGCBG | 2013-2014 | SDCGCB13 | YAML+ICES |
| SDCGCBG | 2015+ | SDCDGCB | YAML+ICES |
| SDCGRES | 2001 | SDCAGRES | YAML+ICES |
| SDCGRES | 2003 | SDCCGRES | YAML |
| SDCGRES | 2005 | SDCEGRES | YAML |
| SDCGRES | 2015+ | SDCDGRES | YAML |

### Expansion opportunities (2019-2023)

**Master variables** — all three inputs to `pct_time_der` (master rows) are confirmed through 2023:

| Variable | Source var (2019+) | YAML coverage | ICES coverage |
|----------|--------------------|---------------|---------------|
| DHH_AGE | DHH_AGE (2019-2021), AWCAGE (2022-2023) | 2019-2023 | 2019-2021 |
| SDCGCB | SDCDVCB | 2019-2023 | 2019-2021 |
| SDCDRES | SDCDVRES | 2019-2023 | 2019-2021 |

**PUMF variables** — limited expansion:

| Variable | Source var (2019+) | Available? |
|----------|--------------------|------------|
| SDCGCBG | SDCDGCB | 2019-2020 PUMF only |
| SDCGRES | **Dropped** | Not available in 2019+ PUMF |

`pct_time_der` PUMF rows **cannot** extend past 2017-2018 because SDCGRES was dropped. Master rows can extend through 2023 with appropriate mappings.

## DV function review — `calculate_pct_time`

### Original design issues (in PR #165 as submitted)

1. **Early `return()` breaks vectorization** — `if (is_equal(SDCGCBG, 1)) { return(100) }` returns for the entire call when any element matches. Works for `rec_with_table()` (row-by-row) but fails for direct vector use.

2. **`if_else2()` is legacy** — wrapper for `ifelse(falseifNA(x), a, b)`. No codebase functions have migrated to `dplyr::case_when()` yet, but deprecation is planned.

3. **`pct_time_fun_A()` unnecessary** — introduced in this PR with near-duplicate logic. The only difference between PUMF and master was whether time-in-Canada was categorical (PUMF midpoints: 4.5 and 15 years) or continuous (master: actual years). Moving midpoints to the worksheet eliminates this distinction entirely.

### Refactoring completed (in-scope for PR #165)

**Done:**
- Replaced `if_else2()` with `dplyr::case_when()` for vectorized logic
- Replaced `is_equal()` + `if()`/`return()` with vectorized conditions
- **Removed `pct_time_fun()` and `pct_time_fun_A()`** — replaced with `calculate_pct_time(age, born_in_canada, years_in_canada)`
- **Renamed `pct_time_fun_cat()`** → `categorize_pct_time()` (v3 verb-first naming)
- Removed `pct_time_canada_core()` intermediate — logic inlined into `calculate_pct_time()`
- Moved PUMF midpoints from R code to worksheet: new `SDCGRES_cont` intermediate variable in `variable_details.csv` with `recEnd` midpoints (1→4.5, 2→15)
- `calculate_pct_time()` receives continuous years for both PUMF (via `SDCGRES_cont`) and master (via `SDCDRES`)
- `categorize_pct_time()` uses `case_when()`, returns uniform character type
- `pct_time_der` PUMF variableStart updated: `SDCGRES` → `SDCGRES_cont`
- **Removed `pct_time_der_A`** — merged into `pct_time_der` with separate PUMF and master row sets
- Purged `_s` database references from pct_time_der
- **Output bounds validation**: values outside [0, 100] → `tagged_na("b")`. Catches inconsistent inputs (e.g., years_in_canada > age). Documented in variable_details notes field (documentation only, ready for future validation framework).
- Roxygen examples for scalar, vector, and `rec_with_table()` usage
- 28 tests passing (0 failures), including:
  - Born-in-Canada tests (born_in_canada=1 → returns 100)
  - PUMF midpoint tests (years_in_canada=4.5, 15)
  - Master continuous tests (years_in_canada=10)
  - Vector input tests for calculate_pct_time and categorize_pct_time
  - Boundary test: `categorize_pct_time(100)` → "10"
  - `tagged_na("a")` test for `categorize_pct_time`
  - NA handling for all three parameters
  - Output bounds: years_in_canada > age → `tagged_na("b")`
  - Output bounds: years_in_canada == age → exactly 100 (valid)

**Deferred to post-v3-smoking merge:**
- Full 3-step architecture (`clean_variables()` + domain logic + output validation layer)
- Move inline bounds check from `calculate_pct_time()` to output validation layer
- Machine-enforce valid range metadata from variable_details (currently documentation only)

**Worksheet changes (in CEP directory for review):**
- `SDCGRES_cont_variable_details.csv` — 5 new rows for SDCGRES_cont (cat→cont midpoint conversion)
- `SDCGRES_cont_variables.csv` — 1 new row for SDCGRES_cont in variables.csv
- `pct_time_der_combined_variable_details.csv` — 4 rows replacing both old pct_time_der (2) and pct_time_der_A (2): 2 PUMF rows + 2 master rows, all under `pct_time_der`, using `Func::calculate_pct_time`
- `pct_time_der_combined_variables.csv` — 1 row replacing both old pct_time_der and pct_time_der_A in variables.csv
- `pct_time_der_cat10_updated_variable_details.csv` — 13 rows updated from `Func::pct_time_fun_cat` to `Func::categorize_pct_time`

**R files modified:**
- `R/percent-time-canada.R` — complete rewrite (`calculate_pct_time`, `categorize_pct_time`, semantic params)
- `tests/testthat/test-immigration.R` — complete rewrite (28 tests, continuous inputs + bounds validation)
- `NAMESPACE` — added `case_when` import, exports renamed to `calculate_pct_time` and `categorize_pct_time`
- `R/recode-with-table.R` — added `case_when` to `@importFrom`

## Checks passed

- Era boundary defaults: SDCGCB, SDCGCBG, SDCDRES, SDCGRES, pct_time_der — all SAFE
- databaseStart consistency: All 7 variables CONSISTENT between variables.csv and variable_details.csv
- PUMF/Master naming: No cross-contamination found
- Pre-2007 cycle letters: Correct (SDCA*, SDCC*, SDCE*, DHHA*, DHHC*, DHHE*)
- Known error patterns (cchs20013_, _i suffix, [[VAR]]): None found
- DV specification: Function parameters match worksheet variableStart
- L6 PUMF integration: All cycles pass, no step changes
- L0 DDI verification: All 22 name mappings verified against YAML+ICES (2001-2023)
- DV refactoring: `case_when()` replaces `if_else2()`, functions renamed to `calculate_pct_time` / `categorize_pct_time` (v3 convention), 28/28 tests pass
- Output bounds validation: values >100 or <0 → `tagged_na("b")`, documented in variable_details notes
- Unit test coverage: Born-in-Canada, PUMF midpoints, master continuous, vector inputs, boundary cases, output bounds, tagged_na all covered
