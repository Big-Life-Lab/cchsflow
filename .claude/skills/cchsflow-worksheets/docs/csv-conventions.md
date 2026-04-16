# CSV structural conventions

This document covers ordering, naming, and structural rules for `variables.csv` and `variable_details.csv` that apply across all variables — independent of any specific harmonization workflow.

---

## 1. Alphabetical variable ordering

All variables in both `variables.csv` and `variable_details.csv` must be sorted **alphabetically by the `variable` column** (first column). When adding a new variable, insert it at the correct alphabetical position — do not append to the end.

---

## 2. Canonical databaseStart ordering

**All _p cycles first (chronological), then all _m cycles (chronological).** Single-year child cycles follow immediately after their two-year parent.

Full canonical order:
```
cchs2001_p, cchs2003_p, cchs2005_p, cchs2007_2008_p,
cchs2009_2010_p, cchs2010_p,
cchs2011_2012_p, cchs2012_p,
cchs2013_2014_p, cchs2014_p,
cchs2015_2016_p, cchs2017_2018_p, cchs2019_2020_p, cchs2022_p, cchs2023_p,
cchs2001_m, cchs2003_m, cchs2005_m, cchs2007_2008_m,
cchs2009_2010_m, cchs2009_m, cchs2010_m,
cchs2011_2012_m, cchs2012_m,
cchs2013_2014_m, cchs2014_m,
cchs2015_2016_m, cchs2017_2018_m, cchs2019_2020_m, cchs2021_m, cchs2022_m, cchs2023_m
```

This ordering applies to both `variables.csv` (databaseStart column) and every era block's databaseStart in `variable_details.csv`.

**Note on 2021 PUMF**: `cchs2021_p` is **not a valid database name** and must never appear in either CSV. StatsCan did not release a standalone 2021 PUMF — the 2021 and 2022 data were combined into a single file, which cchsflow names `cchs2022_p`. The Master file is released annually, so `cchs2021_m` is valid. If you encounter `cchs2021_p` in any databaseStart or variableStart, remove it.

---

## 3. Single-year cycle parent-child rules

Single-year cycles are sub-releases of two-year surveys. They share the same source variables as the parent and are only valid alongside it.

| Parent cycle | Child cycle(s) |
|---|---|
| `cchs2009_2010_p` | `cchs2010_p` |
| `cchs2011_2012_p` | `cchs2012_p` |
| `cchs2013_2014_p` | `cchs2014_p` |
| `cchs2009_2010_m` | `cchs2009_m`, `cchs2010_m` |
| `cchs2011_2012_m` | `cchs2012_m` |
| `cchs2013_2014_m` | `cchs2014_m` |

### Rules

| Condition | Action |
|---|---|
| `cchs2009_2010_m` in databaseStart | Also add `cchs2009_m` and `cchs2010_m` |
| `cchs2011_2012_m` in databaseStart | Also add `cchs2012_m` |
| `cchs2013_2014_m` in databaseStart | Also add `cchs2014_m` |
| `cchs2009_2010_p` in databaseStart | Also add `cchs2010_p` |
| `cchs2011_2012_p` in databaseStart | Also add `cchs2012_p` |
| `cchs2013_2014_p` in databaseStart | Also add `cchs2014_p` |
| Child present **without** parent | Remove the child |

### variableStart for child cycles

Child cycles inherit the same source variable as their parent. When adding a child's token to variableStart, copy the parent's source:

```
# Parent: cchs2009_2010_m::SMK_01C
# Children inherit the same source:
cchs2009_m::SMK_01C, cchs2010_m::SMK_01C
```

If the era block uses `[VAR]` pass-through, children are already covered — no explicit token needed.

---

## 4. Union rule: variables.csv ↔ variable_details.csv

- `variables.csv databaseStart` = union of all `variable_details.csv databaseStart` values for that variable
- `variables.csv variableStart` = union of all explicit `cycle::SOURCE` tokens and `[VAR]` / `DerivedVar::` patterns across all era blocks

No cycle should appear in `variables.csv databaseStart` without a corresponding era block in `variable_details.csv` (exception: cycles explicitly deferred as "UNMATCHED" pending new era blocks).

---

## 5. Era block collapsing (variable_details.csv)

An era block is a group of rows that share the same `databaseStart` and `variableStart`. **Redundant era-split blocks with identical recoding must be collapsed into a single block.**

### When to collapse

Collapse two blocks into one when:
- They have the same `recStart`/`recEnd`/`catLabel` structure (identical recoding logic)
- The only difference is that each block lists different cycles for the same source variable

### How to collapse

Replace per-cycle `db::SOURCE` tokens with the shared `[SOURCE]` pass-through (or explicit multi-cycle listing in one block):

```
# Before: two separate blocks (wrong when recoding is identical)
# Block A
databaseStart: cchs2005_p, cchs2007_2008_p
variableStart: cchs2005_p::SMKE_06A, cchs2007_2008_p::SMK_06A

# Block B
databaseStart: cchs2009_2010_p, cchs2011_2012_p
variableStart: cchs2009_2010_p::SMK_06A, cchs2011_2012_p::SMK_06A

# After: one block using [VAR] pass-through (correct)
databaseStart: cchs2005_p, cchs2007_2008_p, cchs2009_2010_p, cchs2011_2012_p
variableStart: cchs2005_p::SMKE_06A, [SMK_06A]
```

The `[VAR]` default applies to all cycles in `databaseStart` not listed with an explicit `db::VAR` mapping.

### When NOT to collapse

Do not collapse blocks when:

- **Source variable renamed across eras**: e.g., `SMK_06A` → `SMK_060` in 2015+. A `[VAR]` pass-through would incorrectly apply the old name to all cycles in the merged databaseStart. Keep separate blocks with explicit `db::VAR` tokens for each era.
- **Recoding structure differs**: different number of categories, different midpoints, different labels — the rows themselves cannot be shared, so there is nothing to collapse.
- **Mixed computation types**: one block uses direct recoding (plain `recStart`/`recEnd` rows) and the other uses `DerivedVar::`/`Func::`. These are fundamentally different row types and must stay in separate blocks.

---

## 6. Row sort order within era blocks

Rows within each era block must be sorted in this order:

### Categorical variables
1. Numerical category rows — ascending by `recStart` value
2. `NA::a` rows
3. `NA::b` rows (non-else)
4. `NA::b` `else` row

### Continuous variables (copy/passthrough)
1. `copy` rows — ascending by `recStart` value
2. `NA::a` rows
3. `NA::b` rows (non-else)
4. `NA::b` `else` row

### Derived variables (Func:: / DerivedVar::)
1. `Func::` row — always first
2. `NA::a` rows (if present)
3. `NA::b` rows (non-else, if present)
4. `NA::b` `else` row (if present)

`DerivedVar::` blocks typically contain only the `Func::` row with no NA rows alongside — the derivation function handles all output values internally.

---

## 7. dummyVariable naming convention

For **categorical** `variable_details.csv` rows:

```
{variable}_cat{N}_{x}
```

Where:
- `N` = value of `numValidCat` for that variable (number of valid categories)
- `x` = category index: `1` through `N` for value rows, then `NAa`, `NAb`

Examples:
```
SMK_01A_cat2_1        ← first valid category
SMK_01A_cat2_2        ← second valid category
SMK_01A_cat2_NAa      ← NA::a row
SMK_01A_cat2_NAb      ← NA::b rows (all NA::b rows share same dummyVariable)
```

For **continuous** variables: `dummyVariable = N/A`

---

## 8. variables.csv label alignment

The `label` and `labelLong` columns in `variables.csv` must be consistent with the corresponding `variable_details.csv` columns:

| variables.csv column | Matches variable_details.csv column |
|---|---|
| `label` | `variableStartShortLabel` (and `catLabel`) |
| `labelLong` | `variableStartLabel` |

Example — DHHGAGE_cont:
```
variables.csv:        label = "Age",   labelLong = "Converted categorical age"
variable_details.csv: variableStartShortLabel = "Age",
                      variableStartLabel = "Converted categorical age"
```

These must match exactly across all rows for that variable.

---

## Related documentation

- [variableStart-databaseStart-authoring.md](variableStart-databaseStart-authoring.md) — detailed rules for `variableStart`/`databaseStart` coordination, era-specific naming, and the dangerous `[VAR]` default pattern
- [pumf-master-harmonization.md](pumf-master-harmonization.md) — PUMF availability by cycle; when and how to split rows by database type
