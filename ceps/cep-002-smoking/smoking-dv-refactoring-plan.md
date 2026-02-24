# Smoking DV refactoring plan

**Date**: 2026-02-22 (revised 2)
**Branch**: v3-smoking (from skills/review-validation)
**Scope**: Variable naming, recoding logic, and DV function consolidation for smoking cessation

## Problem statement

Two separate issues need addressing:

1. **Variable naming**: The v2.1.0 `_A`/`_B` suffix convention is opaque. `SMK_06A_A` and `SMK_06A_B` don't communicate what they distinguish. They need clear, self-documenting names for v3.0.

2. **Recoding architecture**: There are two parallel mechanisms producing similar output (passthrough `rec_with_table()` and DV functions with hard-coded midpoints). These need to be clarified, not duplicated.

## Key architectural principle

**Two distinct recoding patterns exist — don't conflate them:**

### Pattern 1: Passthrough recoding via `rec_with_table()`

For variables where the worksheet rows define the complete recoding:
- `rec_with_table()` reads `variable_details.csv` and applies recStart→recEnd mappings
- **No DV function needed** — the worksheets are the single source of truth
- Examples: `SMK_06A_cat4` (1→1, 2→2, 3→3, 4→4), `SMK_06A_cont` (1→0.5, 2→1.5, 3→2.5, 4→4.0)
- The 2001 vs 2003+ midpoint differences are handled by separate worksheet row groups with different databaseStart — `rec_with_table()` handles this automatically

**v2.1 verification**: Confirmed that `SMK_06A_cont`, `SMK_09A_cont`, `SMK_10A_cont` have no `customFunction` in `variables.csv` and no `Func::` entries in their own `recEnd` values. They are pure worksheet passthrough — `rec_with_table()` converts the string `"0.5"` to numeric via `as.numeric()` (recode-with-table.R line 819). The same pattern is used for `SMKDGSTP_cont` PUMF midpoints.

### Pattern 2: Derived variable functions

For variables that require runtime logic beyond simple recoding:
- DV functions use the 3-step pattern: `clean_variables()` → domain logic → `clean_variables()`
- These functions combine multiple inputs, apply conditional logic, or compute formulas
- The `get_category_rules()` / `apply_category_rules()` functions (in `development/`) support DV functions like pack-years that need to look up category boundaries at runtime
- Examples: `calculate_pack_years()`, `calculate_time_quit_smoking()` (priority combining), `calculate_smoke_simple()` (threshold logic)

### What this means for cessation

The foundational cessation functions (`calculate_SMK_06A_cont`, `calculate_SMK_09A_cont`, `calculate_SMK_10A_cont`) are **Pattern 1 disguised as Pattern 2**. They duplicate what `rec_with_table()` already does — except for the category 4 continuous companion logic (using SMKG06C/09C/10C when available). That companion logic is already handled by the PUMF/Master worksheet split:

- **PUMF rows**: midpoint imputation for all categories (including cat 4 → fixed estimate)
- **Master rows**: cats 1-3 get midpoints; cat 4 gets `copy` from the continuous companion variable

So `rec_with_table()` already handles everything these DV functions do. The DV functions exist as an alternative entry point but are redundant for the passthrough case.

**Decision**: The foundational cessation functions should be kept — they serve as the DV function entry point when called by combining functions like `calculate_time_quit_smoking()`. But `rec_with_table()` is the primary mechanism for end users. The real work is in the worksheets.

## Phase 1: Variable naming rationalisation

**Goal**: Replace the opaque `_A`/`_B` suffix convention with self-documenting names.

### Naming rules

1. If original categories are preserved → use bare StatCan name (e.g., `SMK_10A`)
2. If original categories can't be preserved (e.g., 2001 differs from 2003+) → use `_cat4` suffix
3. Continuous versions → keep `_cont` suffix

### Proposed renames

| Current name | New name | Rationale |
|-------------|----------|-----------|
| `SMK_06A_A` | `SMK_06A_cat4` | 2001 categories differ from 2003+; `_cat4` flags this |
| `SMK_06A_B` | *merge into `SMK_06A_cat4`* | Identical output (recEnd=1,2,3,4); consolidate |
| `SMK_06A_cont` | `SMK_06A_cont` | No change |
| `SMK_09A_A` | `SMK_09A_cat4` | Same 2001 discrepancy as SMK_06A |
| `SMK_09A_B` | *merge into `SMK_09A_cat4`* | Consolidate |
| `SMK_09A` | Keep as source variable | It's referenced as `variableStart` by other variables |
| `SMK_09A_cont` | `SMK_09A_cont` | No change |
| `SMK_10A_B` | `SMK_10A` | No 2001 discrepancy; bare name OK |
| `SMK_10A_cont` | `SMK_10A_cont` | No change |

### What "merge" means

`SMK_06A_A` and `SMK_06A_B` have non-overlapping databaseStart (different cycle coverage) but produce identical output (recEnd = 1, 2, 3, 4). Merging means:

1. In `variables.csv`: create one `SMK_06A_cat4` row with the union of both databaseStart lists
2. In `variable_details.csv`: rename the `variable` column on all rows from both `_A` and `_B`
3. The row groups remain separate (different databaseStart), which is correct — they cover different cycles

### Implementation steps

1. Write R script to perform the renames in temp files
2. Verify: new names exist, old names gone, databaseStart coverage correct
3. Check that `SMK_09A` (bare source variable) is NOT renamed
4. Check that no `variableStart` references break
5. Apply to production worksheets

## Phase 2: Consolidate duplicate DV functions

### smoke-stop.R vs smoking-cessation.R

Two pairs of functions are duplicated:
- `calculate_SMK_09A_cont()` — in both files
- `calculate_time_quit_smoking()` — in both files

**Action**: Convert the smoke-stop.R versions to documentation-only stubs (matching the pattern already used for `calculate_SMK_06A_B`, `calculate_SMK_09A_B`, `calculate_SPU_25I` in that file). The smoking-cessation.R versions remain as the canonical implementations.

### Foundational cessation functions in smoking-cessation.R

These functions (`calculate_SMK_06A_cont`, `calculate_SMK_09A_cont`, `calculate_SMK_10A_cont`) duplicate what `rec_with_table()` does via the worksheet rows. They exist because combining functions (like `calculate_time_quit_smoking()`) call them in R code.

**Decision**: Keep them — note in the docstring that `rec_with_table()` is the primary mechanism and these functions are an alternative path for use by other DV functions.

**Future consideration**: If and when the L7 `get_category_rules()` is promoted to `R/`, the hard-coded midpoints in these functions could be replaced with worksheet lookups. Not urgent — `rec_with_table()` is the correct mechanism for end users.

## Phase 3: Update parameter names in DV functions

After the worksheet renames, update function parameter names to match:
- `SMK_06A_cat` → `SMK_06A_cat4`
- `SMK_09A_B` (parameter) → `SMK_09A_cat4`
- `SMK_10A_cat` → `SMK_10A`

Update docstrings and `@param` tags accordingly.

## Phase 4: Documentation

1. Update smoke-stop.R header comments with new variable names
2. Update smoking-cessation.R docstrings
3. Note in CEP-002 that v3.0 renames these variables
4. Document the two recoding patterns (passthrough vs derived) distinction

## Out of scope

- **Promoting L7 `get_category_rules()` to R/**: Not needed for passthrough variables. Relevant for derived functions like pack-years — defer to when that refactoring is tackled.
- **Replacing hard-coded midpoints in foundational cessation functions**: These functions are secondary to `rec_with_table()`. If/when L7 is promoted, this becomes a natural follow-up.
- **Legacy `_fun()` functions in smoking.R**: Keep for backward compatibility.
- **Pack-year domain constants**: The constants file is appropriate for domain formulas.
- **Extending to 2022-2023**: Blocked on metadata DB update.

## Skill update notes

Track observations during implementation that should update the cchsflow-worksheets or cchsflow-review skills.

### Midpoint recoding (Pattern 1)

How it works:
1. `variable_details.csv` rows define `recStart` (categorical code) → `recEnd` (numeric midpoint string, e.g., `"0.5"`)
2. `rec_with_table()` reads these rows, matches source data to `recStart`, and assigns `recEnd`
3. Numeric conversion happens automatically: `as.numeric("0.5")` → `0.5` (recode-with-table.R line 819)
4. Era-specific midpoints (e.g., 2001 vs 2003+) are handled by separate row groups with different `databaseStart` — no code logic needed
5. No `customFunction` or `Func::` entry needed in the worksheets

When to use a DV function instead (`Func::` in recEnd):
- The variable requires combining multiple source variables (e.g., `calculate_time_quit_smoking()`)
- The source variable names change across eras and need conditional dispatch (e.g., `calculate_SMKG203_continuous()`)
- The variable requires runtime computation beyond simple lookup (e.g., `calculate_pack_years()`)

### Naming convention (_catN suffix)

When harmonised categories differ from the original StatCan categories (e.g., 2001 has different interval boundaries than 2003+), use `_catN` suffix where N = number of harmonised categories. If original categories are preserved across all eras, use the bare StatCan name.

## Execution order

1. **Phase 1** (naming) — worksheet changes, no code dependencies
2. **Phase 2** (consolidate duplicates) — removes redundant code in smoke-stop.R
3. **Phase 3** (parameter names) — follows from Phase 1
4. **Phase 4** (documentation) — after all changes
