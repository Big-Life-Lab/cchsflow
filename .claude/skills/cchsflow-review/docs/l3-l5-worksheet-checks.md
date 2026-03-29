# L3-L5 worksheet and testing checks

Run these checks in parallel for the in-scope variables.

## Check 1: Era boundary defaults

The most dangerous class of bug. For each variable:

1. Parse the `databaseStart` field — does it span both 2007-2014 and 2015+ cycles?
2. Parse the `variableStart` field — do 2015+ databases have explicit `db::VAR` mappings?
3. If a `[VAR]` default exists and 2015+ databases lack explicit mappings, the default will apply the wrong variable name at runtime

**Key 2015 renames to check:**
- Smoking categorical: SMK_06A → SMK_060, SMK_09A → SMK_080, SMK_10A → SMK_100
- Smoking continuous: SMK_06C → SMK_070, SMK_09C → SMK_090, SMK_10C → SMK_110
- Smoking derived: SMKDSTY → SMKDVSTY, SMKDSTP → SMKDVSTP
- PUMF grouped: SMKG06C → SMKG070, SMKG09C → SMKG090, SMKG10C → SMKG110
- FVC: FVCDFRU → FVCDVFRU, FVCDSAL → FVCDVGRN, FVCDCAR → FVCDVORA, FVCDPOT → FVCDVPOT, FVCDVEG → FVCDVVEG, FVCDJUI → FVCDVJUI
- ADL: ADL_01-06 → ADL_005-030 (3-digit, 2015-2021), then → ADL_05-30 (2-digit, 2023+)

**Key 2023 renames to check:**
- ADL: ADL_005 → ADL_05, ADL_010 → ADL_10, ADL_015 → ADL_15, ADL_020 → ADL_20, ADL_025 → ADL_25, ADL_030 → ADL_30. This is a new era boundary — `[ADL_005]` defaults will not work for 2023 databases.

## Check 2: databaseStart consistency

For each variable:
1. Extract `databaseStart` from variables.csv
2. Extract all `databaseStart` entries from variable_details.csv for that variable
3. The variables.csv list must equal the union of all variable_details.csv lists
4. Flag any databases present in one file but not the other

For each mismatch found, classify it:
- **PR-introduced**: The mismatch is new (not on target branch) — report as P1
- **Pre-existing**: The mismatch exists on the target branch — document in pre-existing issues
- **`_p` in vd only**: PUMF databases in variable_details but not variables.csv is a known pattern for variables that span both pre-2015 and 2015+ eras (the pre-2015 block includes `_p` databases that the 2015+ block in variables.csv doesn't list). Note but do not flag as a bug.

All mismatches must be explicitly listed in the review summary, even pre-existing ones. Do not silently omit consistency results.

## Check 2b: Multi-block recStart collisions

**Terminology:** A **recode block** is a set of rows in variable_details.csv sharing the same `variableStart` value. A recode block defines how one source variable maps to the harmonized output. Variables that changed source variable names or response category definitions across CCHS cycles require multiple blocks — one per distinct source structure. A single block can span multiple eras when the source variable name and category boundaries were stable across them.

Variables with multiple recode blocks must not have the same `recStart` value appearing in more than one block for the same database. If a `(database, recStart)` pair matches rows from two blocks, `rec_with_table()` will find duplicate rows and produce incorrect output.

Note: `databaseStart` overlap alone (a database appearing in two blocks' lists) is not sufficient to flag an error — cchsflow legitimately uses parallel PUMF and Master blocks that share databases but have non-overlapping `recStart` ranges. The collision must be at the `(database, recStart)` level.

**Automated check:** `exec/check-worksheets.R` runs `check_recode_blocks()` automatically. For manual inspection of a specific variable:

```r
vd_var <- variable_details[variable_details$variable == "VAR", ]
blocks <- split(vd_var, vd_var$variableStart)
db_sets <- lapply(blocks, function(b) {
  trimws(unlist(strsplit(b$databaseStart[1], ",")))
})
# Check all pairwise intersections (overlap is a necessary but not sufficient condition)
pairs <- combn(length(db_sets), 2)
for (i in seq_len(ncol(pairs))) {
  overlap <- intersect(db_sets[[pairs[1,i]]], db_sets[[pairs[2,i]]])
  if (length(overlap) > 0)
    cat("OVERLAP (check recStart too):", paste(overlap, collapse=", "), "\n")
}
```

Flag any confirmed `(database, recStart)` collision as **P0**.

This check is especially important for continuous variables with era-specific midpoint recodes (e.g., SMK_09A_cont, SMK_06A_cont) where different cycles have different category boundaries and require separate recode blocks.

## Check 3: PUMF vs Master naming

For `_m` (master) databases:
- Pre-2007: cycle letter in source variable name (A=2001, C=2003, E=2005)
- 2007-2014: standard naming (no prefix letter)
- 2015+: check for renamed variables

For `_p` (PUMF) databases:
- May use grouped/derived variable names (e.g., SMKG prefix, FVCD prefix)

Verify that `_m` databases don't reference PUMF-only grouped variables, and vice versa.

For variables where PUMF and Master use fundamentally different source types (categorical vs continuous), the required pattern is to split into separate recode blocks — one for PUMF, one for Master — each with its own `databaseStart` and `variableStart`.

For harmonized variable **naming** decisions (when to use `_cont`, `_catN`, era suffixes, etc.), see `docs/variable-naming-conventions.md`.

## Check 4: Pre-2007 cycle letters

For variables with pre-2007 master cycles, verify the cycle letter:
- 2001 (`_m` or `_p`): letter A in the variable name (e.g., SMKA_203, FVCADFRU)
- 2003: letter C (e.g., SMKC_203, FVCCDFRU)
- 2005: letter E (e.g., SMKE_203, FVCEDFRU)

The letter position varies by variable domain but follows a consistent pattern within each domain.

## Check 5: Known error patterns

**Automated check:** `exec/check-worksheets.R` runs `check_invalid_databases()` on both worksheets. Review its output before manual scanning — it catches the first four patterns below automatically.

Scan for:
- `cchs20013_` — extra zero typo (should be `cchs2013_`)
- `chs20` without leading `c` — missing `c` typo (should be `cchs20`). This pattern has been found in ADL and FVC variables (e.g., `chs2011_2012_m` instead of `cchs2011_2012_m`). Check all database names match the `cchs` prefix.
- `_i` suffix databases — deprecated, should be `_m`
- `_s` suffix databases — deprecated, **always convert to `_m`** when found in reviewed variables. Check that a corresponding `_m` entry doesn't already exist (if it does, delete the `_s` row; if not, rename `_s` → `_m`). This applies even if the `_s` is pre-existing on the target branch — if the PR touches these rows, fix the suffix. **Naming convention**: `_s` share files are single-year extracts, so map to the single-year master form: `cchs2009_s` → `cchs2009_m` (not `cchs2009_2010_m`), `cchs2010_s` → `cchs2010_m`, `cchs2012_s` → `cchs2012_m`. Check `variables.csv` to confirm which `_m` form is expected.
- `cchs2021_p`, `cchs2022_p`, `cchs2023_p` — **invalid PUMF databases**. The 2021 CCHS was not released as a standalone PUMF — it was combined with 2022 data into a 2021-2022 PUMF (not yet in cchsflow). The 2022+ smoking variables were restructured into CSS/SPU modules; no standalone PUMF equivalent exists for variables like SMK_09A in those cycles. Remove these from `databaseStart` for PUMF-only or mixed blocks when encountered in reviewed variables.
- `[[VAR]]` — double brackets (invalid notation)
- `[VAR1, VAR2]` without `DerivedVar::` prefix — ambiguous multi-variable input

**Pre-existing typo propagation:** Typo patterns often exist in the target branch for other variables and get copied into new variables through copy-paste. For each typo found, check whether the same pattern exists on the target branch for the same variables — if not, it was introduced by this PR even if the pattern exists elsewhere.

## Check 5b: dummyVariable naming conventions

Verify that `dummyVariable` values follow the naming convention below. (Note: `inst/metadata/documentation/metadata_registry.yaml` is referenced as the authoritative source for these patterns but does not yet exist — this skill section is the current reference.)

**Categorical variables** — regex: `^[a-zA-Z0-9_]+_cat[0-9]+(_[0-9]+|_NA[a-z])$`

| Row type | Pattern | Example |
|----------|---------|---------|
| Valid category | `{variable}_cat{N}_{recEnd}` | `SMK_204_cat4_1`, `FVC_1A_cat5_3` |
| Missing (not applicable) | `{variable}_cat{N}_NAa` | `SMK_204_cat4_NAa` |
| Missing (don't know/refusal) | `{variable}_cat{N}_NAb` | `SMK_204_cat4_NAb` |

**Continuous variables and Func rows** use `N/A` (no naming convention).

**Key rules:**
1. **No colons in dummy names** — use `_NAa` and `_NAb`, not `_NA::a` or `_NA::b`. Colons are invalid in identifiers.
2. **Suffix must match recEnd** — the number after the last underscore should equal the `recEnd` value for that row. A mismatch (e.g., `_cat5_2` with `recEnd=1`) indicates a copy-paste error.
3. **N must match numValidCat** — the number after `_cat` should equal the `numValidCat` value for valid categories of that variable.
4. **Func rows use `N/A`** — derived variable rows (where `recEnd` starts with `Func::`) use `dummyVariable=N/A`.

**What to flag:**
- `_NA::a` or `_NA::b` patterns (should be `_NAa` / `_NAb`)
- Suffix-recEnd mismatches (e.g., `_cat5_2` on a row with `recEnd=1`)
- Func rows with constructed dummy names instead of `N/A`
- Continuous rows with anything other than `N/A`

**DerivedVar block recEnd values:** In `DerivedVar` blocks, `recEnd` documents the *output category codes* produced by the R function, not recode targets. For categorical DVs these will be integers (1, 2, 3, …); for continuous DVs they will be midpoints or numeric outputs. Do **not** flag integer `recEnd` values in a `DerivedVar` block as inconsistent with midpoint values in a sibling direct recode block — the two block types serve different purposes. See `docs/variable-naming-conventions.md` for full explanation.

## Check 5c: Swapped recEnd values

Check for rows where `recEnd` values appear to be swapped between adjacent rows. This is a **P0 data bug** — it produces incorrect values at runtime with no warning.

**Detection pattern:**
1. For each variable, examine rows where `recStart` is a valid data range (e.g., `[1,120]`) and adjacent rows where `recStart` is a not-applicable code (e.g., `996`)
2. The valid data range should map to `recEnd=copy` (or the appropriate output value), not to `NA::a`
3. A not-applicable code should map to `NA::a` or `NA::b`, not to `copy`

**Example (FVC_6D bug found in PR #148):**
```
# WRONG — recEnd values swapped
recStart=[1,120]  recEnd=NA::a   ← valid data being set to missing!
recStart=996      recEnd=copy    ← not-applicable code being copied as data!

# CORRECT
recStart=[1,120]  recEnd=copy    ← valid data copied through
recStart=996      recEnd=NA::a   ← not-applicable code set to missing
```

**When to check:** Always check continuous variables with `copy` and `NA::a`/`NA::b` recEnd values. Swapped values are especially likely for variables added via copy-paste from similar variables.

## Check 5d: Label and metadata consistency

Scan for common metadata quality issues in modified variables:

1. **Double spaces** — check `label`, `labelLong`, `catLabel`, `catLabelLong`, `variableStartShortLabel`, and `variableStartLabel` for consecutive spaces
2. **Spelling errors in labels** — common typos: "consumptoin" (consumption), "freqeuncy" (frequency), "repondent" (respondent)
3. **Trailing punctuation in labelLong** — trailing dashes or incomplete labels (e.g., `"Daily consumption - fruit - (D)"` should be `"Daily consumption - fruit (D)"`)
4. **Missing descriptions** — derived daily frequency variables (FVCD*) and other derived variables should have `description` fields
5. **catLabel propagation** — when a label is fixed in `catLabel`, check that the same fix applies to `catLabelLong`, `variableStartShortLabel`, and `variableStartLabel` where those fields share the same text

These are P2 issues (metadata quality) but are cheap to fix during review and prevent accumulation of inconsistencies.

## Check 5e: Opaque `_A`/`_B` variable name suffixes

When reviewing variables that use `_A` or `_B` suffixes, flag the name as potentially opaque and prompt the reviewer to consider a more descriptive suffix — but **only when the variable is being actively modified** in the current PR or review. Do not propose drive-by renames of untouched variables.

**Smoking variables with `_A`/`_B` suffixes:**

| Current name | Meaning of `_A` | Meaning of `_B` |
|---|---|---|
| SMKDSTY_A / SMKDSTY_B | Pre-2015 6-category structure | 2015+ 6-category structure |
| SMKG01C_A / SMKG01C_B | Pre-2015 grouped categories | 2015+ grouped categories |
| SMKG203_A / SMKG203_B | Pre-2015 grouped categories | 2015+ grouped categories |
| SMKG207_A / SMKG207_B | Pre-2015 grouped categories | 2015+ grouped categories |

The `_A`/`_B` convention consistently encodes era-based category structure splits, but is opaque to users who don't know the convention. Compare with self-documenting suffixes already in use: `_cat3`, `_cat5`, `_cont`.

**When to flag:** If the PR modifies any `_A`/`_B` variable (adds cycles, changes recodes, updates metadata), include a note in the review:

> "SMKDSTY_A uses an opaque `_A` suffix. Consider whether a more descriptive name (e.g., `SMKDSTY_cat6_pre2015`) is warranted as part of this change. A rename requires backward compatibility support (deprecated alias)."

**Backward compatibility:** Renaming a harmonised variable name breaks existing user code that references the old name. Any rename must include a deprecation mechanism — either a wrapper that calls the new name with `.Deprecated()`, or dual entries in `variables.csv` during a transition period. See `docs/variable-naming-conventions.md` for the naming convention and deprecation approach.

**Scoring:** P2 (naming quality). Do not block a PR over this — it is an improvement opportunity, not a correctness issue.

## DV function naming convention (v3)

New or refactored DV functions should use tidyverse-style verb-first names. The `_fun` suffix is legacy and being phased out as functions are refactored.

| Verb | Purpose | Example |
|------|---------|---------|
| `calculate_*()` | Mathematical computation | `calculate_pct_time()`, `calculate_bmi()` |
| `categorize_*()` | Classification into groups | `categorize_pct_time()`, `categorize_bmi()` |
| `assess_*()` | Health risk evaluation | `assess_drinking_risk()` |
| `score_*()` | Scoring systems | `score_adl()` |
| `adjust_*()` | Data correction | `adjust_bmi()` |

Legacy functions (e.g., `bmi_fun()`, `pack_years_fun()`) retain old names until refactored. Worksheets reference functions via `Func::` prefix (e.g., `Func::calculate_pct_time`).

## Worksheet-first principle

`variable_details.csv` `recEnd` is the **source of truth** for value mappings. A DV function (`Func::`) is only warranted when the mapping requires logic that `recStart → recEnd` rows cannot express — for example:

- **Multi-variable computation** (e.g., `pack_years_fun()` combining smoking intensity and duration)
- **Conditional branching** across multiple input variables
- **Date arithmetic** or other transformations not expressible as row-level recodes

Simple categorical-to-midpoint conversions belong in the worksheet as direct recode rows, **not** in R code. The reference pattern is `DHHGAGE_cont`: a continuous variable with era-specific midpoint blocks, entirely worksheet-driven with no R function.

**Why this matters:** When an R function hard-codes midpoints that duplicate (or should duplicate) `recEnd` values, it creates two sources of truth. If the worksheet is updated but the function is not (or vice versa), the pipeline silently produces wrong values. Eliminating redundant functions removes this class of bugs entirely.

## Check 6: L4 — derived variable specification review

If the in-scope variables include derived variables (functions in `R/`):

1. **Input consistency**: Read the DV function (e.g., `calculate_pct_time()` in `R/percent-time-canada.R`) and verify that the input variable names it expects match those listed in `variable_details.csv` for the derived variable
2. **Category coverage**: Verify the function handles all category values that the worksheet's `recFrom` maps to — no unhandled cases that would silently produce NA
3. **Output consistency**: Verify the function's return values match the `recTo` values in the worksheet
4. **No hard-coded worksheet values**: Check that the DV function does not contain literal midpoints or category values (e.g., `~ 0.5`, `~ 1.5`, `~ 4`) that duplicate or should duplicate `recEnd` values in `variable_details.csv`. If a function hard-codes values that the worksheet already expresses (or could express) as `recStart → recEnd` rows, flag as **P1** — the function should be refactored to read from the worksheet or eliminated entirely. Reference: the `DHHGAGE_cont` pattern.
5. **Output bounds validation**: For continuous DVs, check whether the function validates output range. Values outside the valid domain (e.g., percentage >100 or <0) indicate inconsistent inputs and should return `tagged_na("b")`. The valid range should be documented in the `notes` field of the Func row in variable_details (documentation only for now, ready for future validation framework). If the DV lacks bounds checking, flag as P1.
6. **Documentation**: Check roxygen docs match the actual function signature
7. **Necessity check** (worksheet-first): Before reviewing function logic, verify that the `Func::` DerivedVar block is actually needed. Check whether the same mapping could be expressed as direct recode rows (`recStart → recEnd`). If the DerivedVar input uses the same categorical scale as an existing direct recode block and the function only maps categories to output values, the function is redundant — flag as **P1** and recommend converting to direct recode rows. See "Worksheet-first principle" above.

## Check 7: Unit tests (L5)

If the PR includes or modifies test files in `tests/testthat/`:
- Verify category coverage (all output categories have test cases)
- Check edge cases (missing data, boundary values)
- Verify cross-cycle consistency

If the PR lacks tests for new derived variables, flag this.
