# PUMF vs Master harmonization

This document describes how to author worksheet rows when PUMF and Master databases require different recoding logic for the same harmonized variable.

## CCHS PUMF availability by cycle

Before authoring `_p` database entries, verify which PUMF files actually exist. StatsCan PUMF releases have been irregular, particularly around the COVID-19 period.

| Cycle | PUMF status | cchsflow database name |
|-------|-------------|------------------------|
| 2001–2017/18 | Released | `cchs2001_p` … `cchs2017_2018_p` |
| 2019-2020 | Released | `cchs2019_2020_p` |
| 2021 | **Not released standalone** — combined with 2022 into a 2021–2022 PUMF | No `cchs2021_p`; no `cchs2021_2022_p` yet |
| 2022 | Combined with 2021 (see above) | No standalone `cchs2022_p` |
| 2023 | Status uncertain | Check before authoring |

**Key rule**: `cchs2021_p` is an **invalid database name** — do not use it. The 2021 PUMF data is only available as part of the combined 2021-2022 file, which cchsflow has not yet added as a database.

**For variables with `cchs2021_p` in databaseStart**: This is a branch-wide error introduced during v3-smoking development. Remove `cchs2021_p` from `databaseStart` and `variableStart` for any variable you are working on. The broader fix is tracked separately.

**Future PUMF uncertainty**: StatsCan has signalled that Master data collection will continue but PUMF scope and release frequency may change. When adding new `_p` databases, confirm availability with the cchs-metadata MCP or GN before authoring.

## When to split rows by database type

Most cchsflow variables use the same source variable on both PUMF and Master, so `_p` and `_m` databases share the same worksheet rows. A **split** is needed when:

1. **PUMF has a grouped/categorical variable** while **Master has the ungrouped continuous version** of the same concept
2. **The recoding logic differs** between the two — typically midpoint imputation from categorical (PUMF) vs continuous pass-through (Master)
3. **DerivedVar rows with different feeder sets** — when a derived variable uses different feeder variables for PUMF vs Master (e.g., `DHHGAGE_cont` on PUMF vs `DHH_AGE` on Master), the DerivedVar rows must be split accordingly (see [DerivedVar row splitting](#derivedvar-row-splitting) below)

### Common triggers

| PUMF source | Master source | Concept |
|-------------|---------------|---------|
| SMKG06C (grouped categorical) | SMK_06C (continuous years) | Years since stopped occasional smoking |
| SMKG09C (grouped categorical) | SMK_09C (continuous years) | Years since stopped daily smoking |
| SMKDGSTP (grouped 5-category) | SMKDVSTP (continuous 0-88) | Years since quit completely |
| SMK_06A (4-category) | SMK_06A + SMK_06C | Quit timing (categorical + continuous companion) |

### Continuous companion variable names by era

The continuous companion variables follow the standard CCHS era naming conventions:

| Concept | 2001 | 2003 | 2005 | 2007-2014 | 2015-2021 | 2022-2023 |
|---------|------|------|------|-----------|-----------|-----------|
| Age started smoking | — | SMKC_06C | SMKE_06C | SMK_06C | SMK_070 | N/A |
| Time since quit daily | — | SMKC_09C | SMKE_09C | SMK_09C | SMK_090 | SPU_25A/B (restructured) |
| Time since last smoked | — | SMKC_10C | SMKE_10C | SMK_10C | SMK_110 | N/A |

**Key notes:**
- 2001 has no continuous companions (PUMF rows apply to both `_p` and `_m`)
- 2022-2023 restructured smoking into CSS/SPU modules — timing variables changed from a single continuous value (years) to month+year pair (SPU_25A/SPU_25B). These are Master-only.
- Use the MCP `get_variable_history()` to verify existence and era names for any variable

### Naming convention for harmonized Master continuous variables

The cchsflow convention for harmonized variable names uses the **2007-2014 StatCan name** as the canonical form (matching the existing pattern: SMK_09A not SMK_080). Therefore:

| Harmonized name | StatCan source (2007-2014) | StatCan source (2015+) |
|-----------------|---------------------------|------------------------|
| **SMK_06C** | SMK_06C | SMK_070 |
| **SMK_09C** | SMK_09C | SMK_090 |
| **SMK_10C** | SMK_10C | SMK_110 |

These are newly introduced in cchsflow (not in v2.1.0) because Master data was not previously harmonized.

### When NOT to split

- **Same variable on both files**: When PUMF and Master use the same variable name and coding (e.g., SMK_10 gate question) — share rows.
- **Same categories, different names**: When PUMF and Master have different variable names but identical category codes — use separate `variableStart` mappings in the same row group (strategy 2 below).
- **No continuous companion**: When only the categorical variable exists on Master (e.g., cchs2001 has no `_C` continuous variables) — use shared PUMF/Master rows for **recoding rows**. Note: this exception does not apply to DerivedVar rows where the feeder variable differs by database type (e.g., `cchs2001_m` still uses `DHH_AGE`, not `DHHGAGE_cont`).

## Harmonization strategies

### Identifying which strategy to use

Work through these questions in order for each variable:

```
Q1: Do PUMF and Master use the same source variable name and coding?
    YES → Strategy 1 (shared rows)
    NO  → Continue to Q2

Q2: Do the source variables have the same type (both categorical, or both continuous)?
    YES → Strategy 2 (different names, same recoding)
    NO  → Continue to Q3

Q3: Does Master have a continuous source where PUMF only has categorical?
    YES → Strategy 3 (different recoding logic — PUMF/Master split)
    NO  → Investigate further (may need a derived function)
```

**Important**: Strategies compound. A single variable often requires strategy 2 (era naming) AND strategy 3 (PUMF/Master split) simultaneously. See "Compounding strategies" below.

### Strategy 1: Same source, same recoding

PUMF and Master use the same variable name and coding. They share rows — no split needed.

```
databaseStart: cchs2007_2008_p, cchs2007_2008_m
variableStart: [SMK_10]
recStart: 1    recEnd: 1
```

This is the most common case. The `_p` and `_m` databases appear together in `databaseStart`.

### Strategy 2: Different names, same recoding

The source variable name differs (across eras or between PUMF and Master), but the category codes and recoding logic are identical. Use explicit `db::VAR` mappings within the same row group — **no row duplication needed**.

**Example — era naming differences within one row group:**

SMK_09A has different names across eras (SMKA_09A in 2001, SMKC_09A in 2003, etc.) but the same 4 categories with the same codes. One set of rows handles all eras:

```
databaseStart: cchs2001_m, cchs2003_m, cchs2005_m, cchs2007_2008_m, ..., cchs2013_2014_m
variableStart: cchs2001_m::SMKA_09A, cchs2003_m::SMKC_09A, cchs2005_m::SMKE_09A, [SMK_09A]
recStart: 1      recEnd: 0.5
recStart: 2      recEnd: 1.5
recStart: 3      recEnd: 2.5
...
```

The `[SMK_09A]` default applies to 2007-2014 databases where the name is stable. Each database resolves to its correct source variable via the explicit `db::VAR` mappings.

**When row groups ARE needed (era boundary with rename):**

If `databaseStart` spans the 2015 rename boundary, you must split into separate row groups because the `[VAR]` default cannot safely span both eras. The recoding logic is still identical — only the `variableStart` mappings change:

```
# Row group 1: 2007-2014
databaseStart: cchs2007_2008_m, ..., cchs2013_2014_m
variableStart: [SMK_09A]
recStart: 1    recEnd: 0.5

# Row group 2: 2015+
databaseStart: cchs2015_2016_m, ..., cchs2021_m
variableStart: cchs2015_2016_m::SMK_080, cchs2017_2018_m::SMK_080, ...
recStart: 1    recEnd: 0.5
```

Same `recStart`/`recEnd` in both groups. The split is driven by naming safety, not by different recoding rules.

### Strategy 3: Different recoding logic (PUMF/Master split)

PUMF and Master need genuinely different recoding — typically midpoint imputation from categorical (PUMF) vs `copy` pass-through from continuous (Master). Must split into separate row groups with different `typeStart`, `recStart`, `recEnd`.

This strategy is required when:
- The harmonized variable has `typeEnd=cont` (continuous output)
- PUMF only has categorical source variables
- Master has both categorical AND continuous companion variables
- You want to preserve the continuous precision available on Master

### Compounding strategies: era naming + PUMF/Master split

Real variables frequently require both strategy 2 and strategy 3 at once. SMK_06A_cont is a good example — it needs:
- **Strategy 2** because the source variable name changes across eras (SMKC_06A → SMKE_06A → SMK_06A → SMK_060)
- **Strategy 3** because Master has a continuous companion (SMK_06C) that PUMF lacks

The result is multiple row groups, each addressing a combination of era and database type:

```
Group 1: PUMF all eras (midpoint for all categories including cat 4)
  databaseStart: cchs2001_p, cchs2001_m, cchs2003_p, ..., cchs2017_2018_p
  variableStart: cchs2001_p::SMKA_06A, cchs2001_m::SMKA_06A, cchs2003_p::SMKC_06A, ...
  recStart: 1→0.5, 2→1.5, 3→2.5, 4→4, 6→NA::a, [7,9]→NA::b, else→NA::b

Group 2: Master 2003-2014 (midpoint cats 1-3, copy cat 4 from continuous)
  databaseStart: cchs2003_m, cchs2005_m, ..., cchs2013_2014_m
  variableStart (cat rows): cchs2003_m::SMKC_06A, cchs2005_m::SMKE_06A, [SMK_06A]
  variableStart (copy row): cchs2003_m::SMKC_06C, cchs2005_m::SMKE_06C, [SMK_06C]
  recStart: 1→0.5, 2→1.5, 3→2.5, 6→NA::a, [7,9]→NA::b, else→NA::b, copy→copy

Group 3: Master 2015+ (same logic as group 2, but different variable names)
  databaseStart: cchs2015_2016_m, ..., cchs2021_m
  variableStart (cat rows): cchs2015_2016_m::SMK_060, ...
  variableStart (copy row): cchs2015_2016_m::SMK_070, ...
  recStart: 1→0.5, 2→1.5, 3→2.5, 6→NA::a, [7,9]→NA::b, else→NA::b, copy→copy
```

Note that:
- Group 1 includes `cchs2001_m` because 2001 has no continuous companion — it stays with PUMF
- Groups 2 and 3 are split from each other (strategy 2) because the variable was renamed in 2015
- Groups 2 and 3 are split from group 1 (strategy 3) because Master needs different recoding for cat 4

## Working across eras

### The era-walking workflow

Harmonization typically proceeds chronologically — start at 2001 and work forward through each era boundary. At each boundary, reassess whether the existing strategy still applies:

```
2001: Establish baseline. What source variables exist? PUMF and Master same or different?
      → Choose initial strategy.

2003-2005: Pre-2007 era. Variable names change by cycle letter but categories are usually stable.
      → Likely extends with strategy 2 (explicit db::VAR mappings).

2007: Standard naming era begins. Check whether new variables were introduced
      (e.g., continuous companions like SMK_06C appear on Master).
      → May need to add strategy 3 and revisit 2003-2005 row groups.

2015: Major redesign. Variable names renumbered, PUMF may switch to grouped versions,
      question wording may change.
      → Re-evaluate: can you extend, or is this a semantic break?

2022+: Module restructuring (e.g., smoking → substance use). Variable names may change domain prefix.
      → Same evaluation as 2015.
```

At each boundary, the key question is: **can I extend the existing harmonized variable, or does this boundary require a new one?**

### One variable vs multiple variables

**Prefer one variable with multiple row groups** when the output means the same thing across eras, even if the source variables, precision, or strategies differ. Internal row groups handle the complexity — the researcher sees a single consistent variable.

**Create separate harmonized variables** when the output semantics genuinely change:
- Categories were redefined (not just renamed or renumbered)
- The question wording changed to ask something different
- The population filter changed (e.g., "all respondents" → "daily smokers only")
- Combining eras would mislead researchers about comparability

### Grey areas

Some boundaries are judgement calls:

- **Category collapse/expansion**: If 2015+ added a 5th category, you could collapse to 4 across all eras (one variable) or provide both versions. Consider what researchers need — a single long time series, or era-specific detail?

- **Precision differences**: PUMF midpoint (4 values) vs Master continuous (actual years) is a precision difference, not a semantic break. One variable with internal PUMF/Master splits is appropriate. Document the precision difference in `notes`.

- **Partial availability**: If a variable exists on PUMF in some cycles but not others, it can still be one variable — the `databaseStart` simply doesn't include the missing cycles. But if PUMF availability is sparse enough to be misleading, consider separate variables with clear coverage.

### Naming harmonized variables

When separate harmonized variables are needed, the name should communicate **why the split exists**:

| Pattern | Use when | Example |
|---------|----------|---------|
| `_cont` / `_cat` / `_catN` | Output type differs | `SMK_06A_cont`, `SMK_06A_cat`, `SMKDSTY_cat5` |
| `_pre2015` / `_post2015` | Semantic break at era boundary | Where question meaning changed |
| Descriptive qualifier | Concept-specific | `SMKDVSTP` (Master continuous) vs `SMKDGSTP` (PUMF grouped) |

**Avoid** the legacy `_A` / `_B` convention — the letter suffix doesn't communicate what it distinguishes. When you encounter existing `_A`/`_B` variables, check whether a more descriptive name would reduce cognitive load.

**Don't encode year ranges in variable names** (e.g., `_2003_2014`) — these become stale when new cycles are added. Instead, document the cycle coverage in `databaseStart` and `notes`.

## The categorical + continuous hybrid pattern

This is the core pattern for cessation timing variables (SMK_06A_cont, SMK_09A_cont, SMK_10A_cont).

### The source data structure

The CCHS asks former smokers "when did you stop?" with a 4-category response:

| Category | Label | Midpoint |
|----------|-------|----------|
| 1 | Less than 1 year ago | 0.5 years |
| 2 | 1 to less than 2 years | 1.5 years |
| 3 | 2 to less than 3 years | 2.5 years |
| 4 | 3 or more years ago | **Open-ended** |

On **PUMF**, only the categorical variable exists (e.g., SMK_06A). Category 4 gets a conservative fixed estimate (4 years).

On **Master**, StatsCan also provides a **continuous companion** (e.g., SMK_06C) with the actual number of years. Category 4 respondents can get their true value.

### Why categories 1-3 use the same recoding

Categories 1-3 are bounded intervals where both PUMF and Master have the same information (the categorical response). Midpoint imputation is appropriate for both:
- Cat 1 → 0.5 (midpoint of 0-1)
- Cat 2 → 1.5 (midpoint of 1-2)
- Cat 3 → 2.5 (midpoint of 2-3)

Although Master has the continuous value, `rec_with_table()` processes rows sequentially — the categorical source is used for categories 1-3 (matching on `recStart`), and the continuous source is only invoked for the separate `copy` row.

### Why category 4 needs the split

Category 4 is open-ended ("3+ years"). On PUMF, the best we can do is assign a fixed value (4 years — conservative). On Master, the continuous variable gives the actual years (3, 5, 12, 27...). The split preserves this precision.

### Row structure for Master database groups

Each Master database group produces 7 rows:

```
Row 1: typeStart=cat  recStart=1      recEnd=0.5    (from categorical source, e.g., SMK_06A)
Row 2: typeStart=cat  recStart=2      recEnd=1.5    (from categorical source)
Row 3: typeStart=cat  recStart=3      recEnd=2.5    (from categorical source)
Row 4: typeStart=cat  recStart=6      recEnd=NA::a  (not applicable)
Row 5: typeStart=cat  recStart=[7,9]  recEnd=NA::b  (missing)
Row 6: typeStart=cat  recStart=else   recEnd=NA::b  (catch-all)
Row 7: typeStart=cont recStart=copy   recEnd=copy   (from continuous source, e.g., SMK_06C)
```

**Key**: Row 7 uses a **different `variableStart`** — the continuous companion variable — and `typeStart=cont` instead of `typeStart=cat`.

### Row structure for PUMF database groups

PUMF rows are identical except category 4 uses a fixed midpoint:

```
Row 1: typeStart=cat  recStart=1      recEnd=0.5
Row 2: typeStart=cat  recStart=2      recEnd=1.5
Row 3: typeStart=cat  recStart=3      recEnd=2.5
Row 4: typeStart=cat  recStart=4      recEnd=4      (fixed estimate — no continuous source)
Row 5: typeStart=cat  recStart=6      recEnd=NA::a
Row 6: typeStart=cat  recStart=[7,9]  recEnd=NA::b
Row 7: typeStart=cat  recStart=else   recEnd=NA::b
```

## Reference implementation: SMKDGSTP_cont

This variable unifies years since quit across PUMF and Master with three distinct pathways:

| Database type | Source | Strategy |
|---------------|--------|----------|
| Master all cycles | SMKCDSTP/SMKEDSTP/SMKDSTP/SMKDVSTP | `recStart=[0,79]`, `recEnd=copy` — true continuous pass-through |
| PUMF 2007-2008 | SMKDSTP | `recStart=[0,82]`, `recEnd=copy` — continuous available on early PUMF |
| PUMF 2015+ | SMKDGSTP | Categorical midpoint: 0→0.5, 1→1.5, 2→4.0, 3→8.0, 4→15.0 |

Note that PUMF 2007-2008 uses `copy` because the continuous variable happened to be available on that PUMF file. This is a reminder to check each cycle individually — PUMF availability varies.

**Location**: `inst/extdata/variable_details.csv`, SMKDGSTP_cont rows.

## Reference implementation: SMK_06A_cont (cessation fix)

See `ceps/cep-002-smoking/03-cessation/smk_quit_fix_variable_details.csv` for the 49 Master-only rows and `generate_smk_quit_fix.R` for the generation script.

**Before the split** (existing state): SMK_06A_cont has rows with both `_p` and `_m` databases mixed together, all using midpoint imputation including a fixed value of 4 for category 4.

**After the split**:
- Existing rows → PUMF-only (`_p` databases only, plus cchs2001_m)
- New rows → Master-only (`_m` databases from 2003+), with `copy` pass-through for category 4

**Exception**: cchs2001_m stays with the PUMF rows because the 2001 cycle has no continuous companion variable (SMKA_06C doesn't exist).

## How `rec_with_table()` handles `copy`

The `copy` keyword is recognised in two code paths in `R/recode-with-table.R`:

**Path 1** (lines 521-526): When `recStart=else` and `recEnd=copy`, all unmatched values are copied from the source column:
```r
if (is_equal(else_value, "copy")) {
  recoded_data[variable_being_checked] <- data[data_variable_being_checked]
}
```

**Path 2** (lines 628-631): When a specific `recFrom` range matches and `recEnd=copy`, the matching source values are copied directly:
```r
if (is_equal(value_recorded, "copy")) {
  value_recorded <- data[valid_row_index, data_variable_being_checked]
}
```

For the cessation pattern, Path 2 is used: `recStart=copy` means "match all values" and `recEnd=copy` means "pass through as-is".

## Step-by-step workflow for applying a split

### Step 1: Identify databases with continuous sources

Check DDI or use `R/source-lookups.R` to find which cycles have the continuous companion variable:

```r
source("R/source-lookups.R")
# Does SMK_06C exist on Master?
variable_exists_in_database("SMK_06C", "cchs2007_2008_m")  # TRUE
# Does it exist on PUMF?
variable_exists_in_database("SMK_06C", "cchs2007_2008_p")  # FALSE (PUMF has SMKG06C instead)
```

### Step 2: Group databases by type

| Group | Databases | Strategy |
|-------|-----------|----------|
| PUMF all cycles | `_p` databases | Midpoint for all categories |
| Master with continuous | `_m` databases (2003+) | Midpoint cats 1-3, `copy` cat 4 |
| Master without continuous | `cchs2001_m` | Stays with PUMF rows |

### Step 3: Create PUMF-only rows

Remove `_m` databases from the existing mixed rows. The `databaseStart` and `variableStart` should reference only `_p` databases (plus any `_m` databases that lack continuous sources).

### Step 4: Create Master-only rows

For each Master database group:
1. Create 6 categorical rows using the **categorical source** (e.g., SMK_06A)
2. Create 1 continuous row using the **continuous source** (e.g., SMK_06C)
3. Set `typeStart=cont`, `recStart=copy`, `recEnd=copy` on the continuous row

Mind the era naming: the continuous variable was renamed in 2015 (e.g., SMK_06C → SMK_070). Use explicit `db::VAR` mappings.

### Step 5: Verify consistency

```r
# Check that variables.csv databaseStart = union of all detail rows
# Use /cchsflow-validation to run all checks
```

### Step 6: Handle exceptions

Document any databases that don't follow the pattern in the `reviewNotes` field:
```
reviewNotes: "cchs2001_m stays with PUMF rows (no continuous variable in 2001)"
```

## DerivedVar row splitting

The PUMF/Master split obligation applies equally to `DerivedVar::` rows, not just recoding rows. When a derived variable uses **different feeder variables** for PUMF vs Master, the DerivedVar rows must be split by database type.

### The rule

> A DerivedVar row must not mix `_p` and `_m` databases if those databases use different feeder variables.

This is invisible to `rec_with_table()` — it will silently process both row groups for every database, using whichever feeder happens to be present. No error is raised. The bug only becomes visible through output comparison or dependency resolution tools.

### Common case: age variable

The most common DerivedVar split is the age feeder:

| Database type | Age feeder | Notes |
|---------------|------------|-------|
| PUMF (`_p`) | `DHHGAGE_cont` | PUMF-only midpoint-imputed continuous age |
| Master (`_m`) | `DHH_AGE` | Master true continuous age, exists in all cycles including 2001 |

**`DHHGAGE_cont` is PUMF-only.** It does not exist on Master. If a DerivedVar row lists both `_p` and `_m` databases with `DHHGAGE_cont` as a feeder, `rec_with_table()` will fail silently for Master databases.

**`cchs2001_m` uses `DHH_AGE`**, not `DHHGAGE_cont`. The "2001 Master stays with PUMF rows" exception applies only to recoding rows where the continuous companion variable doesn't exist in 2001 (e.g., `SMK_06C`). For age, the Master variable `DHH_AGE` exists in all cycles including 2001 — so `cchs2001_m` belongs with the Master `DHH_AGE` rows.

### Example: pack_years_der

**Before (incorrect):** All 6 DerivedVar rows list all `_p` and `_m` databases together:

```
# Rows 1-3 (wrong — _m databases should not be here)
databaseStart: cchs2001_p, ..., cchs2023_p, cchs2001_m, ..., cchs2023_m
variableStart: DerivedVar::[SMKDSTY_A, DHHGAGE_cont, age_start_smoking, ...]

# Rows 4-6 (wrong — _p databases should not be here)
databaseStart: cchs2001_p, ..., cchs2023_p, cchs2001_m, ..., cchs2023_m
variableStart: DerivedVar::[SMKDSTY_A, DHH_AGE, age_start_smoking, ...]
```

**After (correct):** Rows split cleanly by database type:

```
# Rows 1-3: PUMF only
databaseStart: cchs2001_p, cchs2003_p, ..., cchs2023_p
variableStart: DerivedVar::[SMKDSTY_A, DHHGAGE_cont, age_start_smoking, ...]

# Rows 4-6: Master only (including cchs2001_m)
databaseStart: cchs2001_m, cchs2003_m, ..., cchs2023_m
variableStart: DerivedVar::[SMKDSTY_A, DHH_AGE, age_start_smoking, ...]
```

### How to check

Use `resolve_dependencies()` from `variable-tools.R` with a `databases` filter and verify that the feeder list matches expectations for that database type:

```r
devtools::load_all()
vd <- read.csv("inst/extdata/variable_details.csv", stringsAsFactors = FALSE)

# Should show DHHGAGE_cont, not DHH_AGE
deps_p <- resolve_dependencies("pack_years_der", variable_details = vd,
                                databases = "cchs2001_p")
deps_p$graph[["pack_years_der"]]$feeders

# Should show DHH_AGE, not DHHGAGE_cont
deps_m <- resolve_dependencies("pack_years_der", variable_details = vd,
                                databases = "cchs2001_m")
deps_m$graph[["pack_years_der"]]$feeders
```

If both return the same (combined) feeder list, the rows need splitting.

## Common errors

| Error | Consequence | Prevention |
|-------|-------------|------------|
| Forgetting to remove `_m` from original rows after adding Master rows | Duplicate processing — variable gets recoded twice for Master databases | Always update the PUMF rows' `databaseStart` when adding Master rows |
| Using `[SMK_09C]` default for 2015+ | Variable not found — `SMK_09C` was renamed to `SMK_090` in 2015 | Use explicit `db::VAR` mappings for all eras (see [variableStart-databaseStart-authoring.md](variableStart-databaseStart-authoring.md)) |
| Splitting cchs2001_m when no continuous variable exists | `copy` row references a non-existent source variable | Check DDI for each cycle before assuming continuous exists |
| Using `typeStart=cat` on the `copy` row | `rec_with_table()` treats values as category codes instead of continuous | The `copy` row must have `typeStart=cont` |
| Inconsistent midpoint values between PUMF and Master rows | Different output for same category depending on database | Use identical midpoints for categories 1-3 on both PUMF and Master rows |
| Mixing `_p` and `_m` databases in DerivedVar rows with different feeder sets | Silent wrong output — `rec_with_table()` processes both row groups for every database | Split DerivedVar rows by database type whenever feeder sets differ; verify with `resolve_dependencies()` |
| Using `DHHGAGE_cont` as age feeder in a row that includes `_m` databases | Master databases use PUMF age variable — wrong age values or silent failure | `DHHGAGE_cont` is PUMF-only; use `DHH_AGE` for all `_m` databases including `cchs2001_m` |

## Related documentation

- [variableStart-databaseStart-authoring.md](variableStart-databaseStart-authoring.md) — era-specific naming and the dangerous default pattern
- [harmonization-workflow.md](harmonization-workflow.md) — L0-L6 staged workflow (identify PUMF vs Master differences at L2)
