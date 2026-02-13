# CEP-010: PR #171 review summary — ethnicity variables

**PR**: #171 — Ethnicity
**Branch**: `ethnicity` → `v3.0.0-validation-infrastructure`
**Reviewer**: Claude Code (automated)
**Date**: 2026-02-13

## Scope

**In-scope variables**: SDCDCGT_A, SDCDCGT_B, SDCGLNG
**Database types**: Master (`_m`), PUMF (`_p`), Share (`_s`)
**Cycles**: 2001 through 2017-2018 (existing); 2019-2023 (extension)
**PR purpose**: Replace `_i` (ICES internal) database suffixes with `_m` (masterfile) suffixes for SDCDCGT_A and SDCDCGT_B; add `_m` databases for SDCGLNG

## L0: Triage

- **Files changed**: `variables.csv` (379+/379-), `variable_details.csv` (3722+/3722-)
- **Actual row-level changes**: All changes are to `databaseStart` and `variableStart` fields only — no recoding logic was modified
- **Out-of-scope changes**: ADL_01-06 had `_i` databases stripped from `variable_details.csv`; DHHGAGE_E was deleted entirely (23 rows removed from `variable_details.csv`). These are collateral changes outside the stated PR scope.

## L1: Change summary

### SDCDCGT_A (cultural/racial origin — 14 categories)

All 33 rows: replaced `_i` suffix with `_m` suffix across 8 databases (cchs2003 through cchs2017_2018). Source variable names updated correspondingly (e.g., `cchs2003_i::SDCCDRAC` → `cchs2003_m::SDCCDRAC`).

Two eras of category mapping preserved correctly:
- **2003-2014** (rows 0-16): 14 categories + NA, using `[SDCDCGT]` default
- **2015-2018** (rows 17-32): 13 categories remapped to SDCDCGT_A codes, using `[SDCDVCGT]` source

### SDCDCGT_B (cultural/racial origin — 7 collapsed categories)

All 14 rows: same `_i` → `_m` swap as SDCDCGT_A.

### SDCGLNG (language — can converse in)

Added 5 new `_m` databases: `cchs2001_m`, `cchs2003_m`, `cchs2005_m`, `cchs2007_2008_m`, `cchs2009_2010_m`.

Two groups of rows updated:
- **PUMF-era rows (0-6)**: Added `cchs2001_m::SDCAGLNG` alongside existing PUMF references
- **Share-file-era rows (7-15)**: Added `cchs2003_m::SDCCDLNG`, `cchs2005_m::SDCEDLNG`, and 2007-2008/2009-2010 `_m` databases using `[SDCDLNG]` default

## L2: Source variable verification

### SDCDCGT_A/B source variables

| Database | variableStart | Convention | Status |
|----------|--------------|------------|--------|
| cchs2003_m | SDCCDRAC | Cycle prefix `CC` for 2003 | Correct |
| cchs2005_m | SDCEDCGT | Cycle prefix `CE` for 2005 | Correct |
| cchs2007_2008_m | [SDCDCGT] | Default name (no cycle prefix post-2007) | Correct |
| cchs2009_2010_m | [SDCDCGT] | Default | Correct |
| cchs2011_2012_m | [SDCDCGT] | Default | Correct |
| cchs2013_2014_m | [SDCDCGT] | Default | Correct |
| cchs2015_2016_m | [SDCDVCGT] | New derived variable name for 2015+ | Correct |
| cchs2017_2018_m | [SDCDVCGT] | Same as 2015-2016 | Correct |

### SDCGLNG source variables (existing)

| Database | variableStart | Convention | Status |
|----------|--------------|------------|--------|
| cchs2001_m | SDCAGLNG | Cycle prefix `CA` for 2001 | Correct — same as PUMF |
| cchs2003_m | SDCCDLNG | Cycle prefix `CC` + `D` for masterfile | Correct |
| cchs2005_m | SDCEDLNG | Cycle prefix `CE` + `D` for masterfile | Correct |
| cchs2007_2008_m | [SDCDLNG] | Default masterfile name | Correct |
| cchs2009_2010_m | [SDCDLNG] | Default masterfile name | Correct |

Note: PUMF uses `SDCGLNG` (with `G`) while masterfile uses `SDCDLNG` (with `D`). The 2001 cycle uses `SDCAGLNG` in both PUMF and masterfile. The 2003 and 2005 masterfile names correctly differ from PUMF (`SDCCDLNG` vs `SDCCGLNG`).

### SDCGLNG source variables (extension, 2019-2023)

| Database | variableStart | Source | Status |
|----------|--------------|--------|--------|
| cchs2019_m | [SDC_025] | SDC_025: Knowledge of official languages | Confirmed in cchsflow-docs 2019 data dictionary |
| cchs2020_m | [SDC_025] | Same variable | Confirmed in 2020 data dictionary |
| cchs2021_m | [SDC_025] | Same variable | Confirmed in 2021 data dictionary |
| cchs2022_m | [LAN_01] | LAN_01: Knowledge of official languages (renamed) | Confirmed in 2022 data dictionary |
| cchs2023_m | [LAN_01] | Same variable | Confirmed in 2023 data dictionary |

SDC_025 codes: 1=English only, 2=French only, 3=Both, 4=Neither, 7=Don't know, 8=Refusal, 9=Not stated.
LAN_01 codes: 1=English only, 2=French only, 3=Both, 4=Neither, 9=Not stated.

## L3: Category mapping check — PASS

No recoding logic was changed in the PR for existing rows. All `recStart`, `recEnd`, `catLabel`, and `catStartLabel` values are identical between base and PR branch.

New SDCGLNG 2019-2023 rows use the PUMF-era template (codes 1-4 map directly):

| recEnd | recStart | catLabel | SDC_025 code | LAN_01 code |
|--------|----------|----------|-------------|-------------|
| 1 | 1 | English w/ or w/o other | 1 = English only | 1 = English only |
| 2 | 2 | French w/ or w/o other | 2 = French only | 2 = French only |
| 3 | 3 | English & French w/ or w/o other | 3 = Both | 3 = Both |
| 4 | 4 | Neither | 4 = Neither | 4 = Neither |
| NA::a | 6 | not applicable | (no code 6 — row does not fire) | (no code 6) |
| NA::b | [7,9] | missing | 7=DK, 9=Not stated | 9=Not stated |
| NA::b | else | missing | 8=Refusal | (catch-all) |

## L4: Cross-file consistency — FAIL (pre-existing issues)

### Finding 1 (P0, pre-existing): Variable name typo in variables.csv

`variables.csv` uses `SDCDGT_A` and `SDCDGT_B` (missing the second `C`) while `variable_details.csv` correctly uses `SDCDCGT_A` and `SDCDCGT_B`. This mismatch means the two files refer to different variable names.

**Pre-existing** — present on the base branch. Not introduced by this PR.

**Fixed in temp worksheets**: Renamed to `SDCDCGT_A`/`SDCDCGT_B`.

### Finding 2 (P1): SDCDCGT_A/B databaseStart mismatch

SDCDCGT_A/B rows in `variable_details.csv` list 8 `_m` databases, but `variables.csv` (under the misspelled `SDCDGT_A`/`SDCDGT_B`) does not list these databases.

**Fixed in temp worksheets**: `variables.csv` databaseStart updated to match `variable_details.csv`.

### Finding 3 (P1, pre-existing): SDCGLNG `_m`/`_s` databaseStart inconsistency

- `variables.csv` lists `cchs2009_m, cchs2010_m` but `variable_details.csv` uses `cchs2009_s, cchs2010_s`
- This is **pre-existing** on the base branch (264 variables use `_s` for these single-year files vs 3 using `_m`)
- **Partially addressed in temp worksheets**: `variables.csv` now includes both `_m` and `_s` entries. Full resolution deferred.

## L5: Naming conventions — PASS

All newly added `_m` variable names follow the established cycle-prefix conventions:
- 2001: `A` prefix (SDCAGLNG)
- 2003: `CC`/`CD` prefix (SDCCDRAC, SDCCDLNG)
- 2005: `CE`/`ED` prefix (SDCEDCGT, SDCEDLNG)
- 2007+: No cycle prefix, use standard name (SDCDCGT, SDCDLNG)

Extension variables follow post-2007 convention (no cycle prefix): `[SDC_025]`, `[LAN_01]`.

## L6: Implementation validation

### SDCGLNG — `rec_with_table()` results

Tested against merged worksheets (production + CEP-010 temp changes) using all available PUMF and share data.

| Database | N | Valid | Valid % | Distribution | Status |
|----------|---|-------|---------|-------------|--------|
| cchs2001_p | 200 | 200 | 100.0% | 1=173, 3=24, 4=2, 2=1 | PASS |
| cchs2003_p | 200 | 200 | 100.0% | 1=127, 3=38, 2=28, NA(b)=6, 4=1 | PASS |
| cchs2005_p | 200 | 200 | 100.0% | 1=142, 3=32, 2=21, NA(b)=5 | PASS |
| cchs2007_2008_p | 200 | 200 | 100.0% | 1=125, 3=51, 2=22, 4=1, NA(b)=1 | PASS |
| cchs2009_2010_p | 200 | 200 | 100.0% | 1=134, 3=43, 2=17, 4=4, NA(b)=2 | PASS |
| cchs2010_p | 200 | 200 | 100.0% | 1=133, 3=38, 2=27, NA(b)=2 | PASS |
| cchs2009_s | 200 | 200 | 100.0% | 1=133, 3=37, 2=23, 4=4, NA(b)=3 | PASS |
| cchs2010_s | 200 | 200 | 100.0% | 1=142, 3=27, 2=24, NA(b)=4, 4=3 | PASS |
| cchs2011_2012_p | — | — | — | — | SKIP (not in PUMF) |
| cchs2012_p | — | — | — | — | SKIP (not in PUMF) |
| cchs2013_2014_p | — | — | — | — | SKIP (not in PUMF) |
| cchs2014_p | — | — | — | — | SKIP (not in PUMF) |
| cchs2015_2016_p | — | — | — | — | SKIP (not in PUMF) |
| cchs2017_2018_p | — | — | — | — | SKIP (not in PUMF) |

**8 PASS, 6 SKIP, 0 ERROR.**

SDCGLNG was dropped from PUMF after 2010 and is only available in masterfile/share files for 2011+. The 8 testable cycles all return 100% valid with expected distributions (majority English, followed by Both, French, Neither). The extension cycles (2019-2023) use masterfile databases (`_m`) that are not available as PUMF sample data; their source variable mappings are verified at L2.

### SDCDCGT_A — `rec_with_table()` results

| Database | Status | Reason |
|----------|--------|--------|
| All 12 PUMF cycles | SKIP | SDCDCGT_A maps only to `_m` (masterfile) databases; no PUMF data contains the source variables (`SDCCDRAC`, `SDCEDCGT`, `SDCDCGT`, `SDCDVCGT`) |

**0 PASS, 12 SKIP, 0 ERROR.**

SDCDCGT_A is a masterfile-only variable. The PUMF data does not contain the cultural/racial origin source variables. This is by design — Statistics Canada suppresses ethnicity data from public-use files for privacy reasons.

### SDCDCGT_B — `rec_with_table()` results

| Database | Status | Reason |
|----------|--------|--------|
| All 12 PUMF cycles | SKIP | Same as SDCDCGT_A — masterfile-only |

**0 PASS, 12 SKIP, 0 ERROR.**

### L6 summary

No errors. All testable cycles pass. The variables that cannot be tested (SDCDCGT_A/B and SDCGLNG post-2010) are masterfile-only by design. Source variable verification at L2 confirms correct naming.

## Extension analysis: CCHS 2019-2024

### Variable availability in post-2018 cycles

Based on the cchsflow-docs extracted documentation (Statistics Canada masterfile data dictionaries):

| Old variable (through 2017-2018) | New variable (2019+) | Notes |
|----------------------------------|---------------------|-------|
| SDCDCGT / SDCDVCGT | **SDCDVVM** | Visible minority group (derived) |
| — | **SDCDVFLA** | Visible minority flag (binary, derived from SDCDVVM) |
| SDCGLNG (can converse in) | **SDC_025** (2019-2021) / **LAN_01** (2022+) | Knowledge of official languages |
| — | **SDCDVLHM** | Language(s) spoken most often at home (derived) |

### SDCDVVM — Visible minority group of respondent

Available in all single-year masterfiles: 2019, 2020, 2021, 2022, 2023.

**Categories** (consistent across all years):

| Value | Label |
|-------|-------|
| 1 | South Asian |
| 2 | Chinese |
| 3 | Black |
| 4 | Filipino |
| 5 | Latin American |
| 6 | Arab |
| 7 | Southeast Asian |
| 8 | West Asian |
| 9 | Korean |
| 10 | Japanese |
| 11 | Visible minority n.i.e. |
| 12 | Multiple visible minorities |
| 13 | Not a visible minority |
| 99 | Not stated |

**Source variables**: SDC_015 + SDC_020A-K (2019-2021); SDCDVABT + PG_05A-K (2022-2023).

**Key difference from SDCDCGT**: SDCDVVM uses the "visible minority" framework per the Employment Equity Act. Aboriginal/Indigenous persons are coded as "Not a visible minority" (value 13), not as a separate category (SDCDCGT had category 7 = "Aboriginal/Name"). "White" is subsumed under value 13.

### Harmonization feasibility: SDCDCGT_A → SDCDVVM

A direct mapping from SDCDVVM to SDCDCGT_A is **partially possible** but has structural barriers:

| SDCDCGT_A (2003-2014) | SDCDVVM (2019+) | Mapping |
|------------------------|-----------------|---------|
| 1 = White | 13 = Not a visible minority | **Imprecise** — value 13 includes White + Aboriginal |
| 2 = Black | 3 = Black | Direct |
| 3 = Korean | 9 = Korean | Direct |
| 4 = Filipino | 4 = Filipino | Direct |
| 5 = Japanese | 10 = Japanese | Direct |
| 6 = Chinese | 2 = Chinese | Direct |
| 7 = Aboriginal | — | **Not available** — coded as 13 in SDCDVVM |
| 8 = South Asian | 1 = South Asian | Direct |
| 9 = South East Asian | 7 = Southeast Asian | Direct |
| 10 = Arab | 6 = Arab | Direct |
| 11 = West Asian | 8 = West Asian | Direct |
| 12 = Latin American | 5 = Latin American | Direct |
| 13 = Other | 11 = Visible minority n.i.e. | Direct |
| 14 = Multiple origins | 12 = Multiple visible minorities | Direct |

**Barriers**: "White" and "Aboriginal" cannot be distinguished in SDCDVVM — both map to value 13. Clean reverse mapping to SDCDCGT_A categories 1 and 7 is impossible without supplementary Aboriginal identity data.

### CCHS 2024

Statistics Canada lists CCHS 2024 as an active reference period, but no extracted documentation is yet available in the cchsflow-docs repository. Extension to 2024 should wait for data dictionary availability.

## Recommendations

### For PR #171 (current scope)

1. **Fix the `SDCDGT_A`/`SDCDGT_B` typo in `variables.csv`** — rename to `SDCDCGT_A`/`SDCDCGT_B` (P0, pre-existing but blocks correct validation)
2. **Update `variables.csv` databaseStart** for SDCDCGT_A and SDCDCGT_B to include the `_m` databases added to `variable_details.csv`
3. **Resolve the SDCGLNG `_m` vs `_s` inconsistency** in `variables.csv`

### Extension (included in temp worksheets)

4. **Extend SDCGLNG to 2019-2023** — 14 new rows in `variable_details.csv` using `[SDC_025]` (2019-2021) and `[LAN_01]` (2022-2023). Category mapping is 1:1.

### Future work

5. **Create SDCDVVM harmonized variable** — New variable mapping SDCDVVM across 2019-2023 cycles. Cannot be directly mapped to SDCDCGT_A due to White/Aboriginal conflation.
6. **Consider a collapsed SDCDCGT_B-equivalent** for SDCDVVM — The 7-category SDCDCGT_B could be approximated from SDCDVVM (with Aboriginal coded as NA).

### Out-of-scope collateral changes

7. The ADL and DHHGAGE_E changes in this PR appear to be from the base branch merge or parallel work. These should be reviewed separately.

## Artifacts

| File | Description |
|------|-------------|
| `temp-variables.csv` | 3 rows — SDCDCGT_A, SDCDCGT_B, SDCGLNG with all fixes applied |
| `temp-variable_details.csv` | 77 rows — all detail rows for the 3 variables (33+14+30) |
| `l6-integration-test.csv` | Full `rec_with_table()` results across all PUMF/share cycles |
| R scripts | `/tmp/cep010_changes.R`, `/tmp/cep010_l6_test.R` |
| Source documentation | `cchsflow-docs/cchs-extracted/` (2019-2023 data dictionaries and derived variable specs) |
