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

### Collapsing Func:: era blocks

The same collapsing rule applies to `Func::` blocks. Two consecutive Func:: era blocks must be merged when they call the **same function with the same feeder list**:

```
# Before: two separate Func:: blocks (wrong when function + feeders are identical)
# Block A
databaseStart: "cchs2015_2016_m, cchs2017_2018_m, cchs2019_2020_m"
variableStart: "DerivedVar::[SMK_005, SMK_040]"
recEnd: Func::calculate_SMK_203

# Block B
databaseStart: "cchs2021_m, cchs2022_m, cchs2023_m"
variableStart: "DerivedVar::[SMK_005, SMK_040]"
recEnd: Func::calculate_SMK_203

# After: one merged Func:: block (correct)
databaseStart: "cchs2015_2016_m, cchs2017_2018_m, cchs2019_2020_m, cchs2021_m, cchs2022_m, cchs2023_m"
variableStart: "DerivedVar::[SMK_005, SMK_040]"
recEnd: Func::calculate_SMK_203
```

Each Func:: block always stays 3 rows after merging (Func:: + NA::a + NA::b). Keep separate blocks only when the feeder list changes across eras (different source variables routing to the function).

### When NOT to collapse

Do not collapse blocks when:

- **Source variable renamed across eras**: e.g., `SMK_06A` → `SMK_060` in 2015+. A `[VAR]` pass-through would incorrectly apply the old name to all cycles in the merged databaseStart. Keep separate blocks with explicit `db::VAR` tokens for each era.
- **Recoding structure differs**: different number of categories, different midpoints, different labels — the rows themselves cannot be shared, so there is nothing to collapse.
- **Mixed computation types**: one block uses direct recoding (plain `recStart`/`recEnd` rows) and the other uses `DerivedVar::`/`Func::`. These are fundamentally different row types and must stay in separate blocks.
- **Feeder list changes across eras**: two Func:: blocks call the same function but with different `DerivedVar::` inputs — keep them separate.

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

Row count depends on `typeEnd` and on whether the underlying R function can return `NA(a)` / `NA(b)`.

**Continuous output** (`typeEnd=cont`) — up to 3 rows:
1. `Func::` row — `typeStart=N/A`, `recStart=N/A`, `catLabel=N/A`, `recEnd=Func::function_name`
2. `NA::a` row — `recEnd=NA::a`, `recStart=N/A`, `catLabel=not applicable`
3. `NA::b` row — `recEnd=NA::b`, `recStart=N/A`, `catLabel=missing`

**Categorical output** (`typeEnd=cat`) — Func:: row, then N category rows, then NA rows:
1. `Func::` row — `typeStart=N/A`, `recStart=N/A`, `catLabel=N/A`, `recEnd=Func::function_name`
2. Category rows 1…N — `recEnd=1`, `recEnd=2`, … ascending (same as direct recoding)
3. `NA::a` row
4. `NA::b` row

**NA::a / NA::b row exception**: an `NA::a` row may be **omitted** if the underlying R function never returns `tagged_na("a")` (i.e. never produces `NA(a)`). Likewise for `NA::b`. To verify before omitting, inspect the function body for `tagged_na("a")`, `tagged_na("b")`, `assign_missing("not_applicable", …)`, `assign_missing("not_stated", …)`, or string returns of `"NA(a)"` / `"NA(b)"`. If none of those paths exist, the corresponding row is by-design absent and is not a convention violation.

**Never add a `NA::b else` row to a Func:: block** (either type). The else/catchall pattern belongs only to direct recoding blocks. `recStart=N/A` and `catLabel=N/A` on the Func:: row itself — the function determines all valid output values.

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

---

## 9. Derived variable inputs in variables.csv variableStart

A derived variable's `variables.csv variableStart` summarizes which cchsflow variables feed it. The format depends on the structure of its `variable_details.csv` era blocks.

### When to use `DerivedVar::[union]` (preferred for purely-derived variables)

Use a single `DerivedVar::[union of all feeders across era blocks]` token in `variables.csv variableStart` when **all** of the following conditions hold for the variable's `variable_details.csv` rows:

1. Every era block specifies its inputs via either:
   - `DerivedVar::[X, Y, …]` in a Func:: row (functional derivation), OR
   - `cycle::[X, Y, …]` per-cycle multi-variable token.
2. **No** era block uses a `[VAR]` default pass-through.
3. **No** era block uses a singular `cycle::SOURCE` mapping.

The union must list every cchsflow variable that appears as a feeder in any era block, with each name appearing exactly once.

```
# Example — pack_years_der (functional derivation across PUMF/Master era blocks)
# variable_details.csv:
#   Block 1 (PUMF):   variableStart = "DerivedVar::[SMKDSTY_original, DHHGAGE_cont, age_start_smoking, ...]"
#   Block 2 (Master): variableStart = "DerivedVar::[SMKDSTY_original, DHH_AGE,      age_start_smoking, ...]"

# variables.csv (correct — single DerivedVar::[union])
variableStart = "DerivedVar::[SMKDSTY_original, DHHGAGE_cont, DHH_AGE, age_start_smoking, ...]"
```

```
# Example — immigration_der (multi-source pass-through, cycle::[X, Y] per cycle)
# variable_details.csv era blocks all use cycle::[X, Y, ...] tokens:
#   cchs2001_m::[SDCFIMM, SDCGCB, SDCDCGT_cat7, SDCDRES],
#   cchs2003_m::[SDCFIMM, SDCGCB, SDCDCGT_cat7, SDCDRES], ...

# variables.csv (correct — collapse to DerivedVar::[union], not per-cycle tokens)
variableStart = "DerivedVar::[SDCFIMM, SDCGCB, SDCDCGT_cat7, SDCDRES, SDCGCBG, SDCGCGT, SDCGRES_cont]"
```

This applies to both:
- **Functional derived variables** (e.g., `pack_years_der`, `time_quit_smoking_complete`) whose era blocks use `DerivedVar::[…]` in Func:: rows.
- **Multi-source pass-through variables** (e.g., `immigration_der`, `COPD_Emph_der`) whose era blocks use `cycle::[X, Y]` per-cycle multi-var tokens.

### When to keep cycle-specific tokens

Use cycle-specific tokens in `variables.csv variableStart` only when the union format cannot represent the structure — i.e., when **any** of these is true:
- An era block uses a `[VAR]` default pass-through.
- An era block uses a singular `cycle::SOURCE` mapping.

In these cases preserve the per-cycle tokens and `[VAR]` patterns directly so the variableStart still satisfies the union rule (§4).

### Names inside brackets must be cchsflow names

Whatever format is used, names inside `[…]` brackets in `variables.csv variableStart` must be **cchsflow harmonized variable names**, not raw CCHS source names:

```
# Wrong — raw CCHS era-specific names inside brackets
cchs2001_m::[SMKA_203, SMKA_207], cchs2003_m::[SMKC_203, SMKC_207]

# Correct — cchsflow harmonized variable names inside brackets
cchs2001_m::[SMK_203, SMK_207], cchs2003_m::[SMK_203, SMK_207]
```

Era-specific source name mappings (SMKA_203, SMKC_203, etc.) belong in `variable_details.csv` rows. `variables.csv` is a summary registry — its inputs declare which cchsflow variables feed a derived variable, not which raw CCHS variables supply the data.

### Consistency across eras

When consecutive eras call the same function with the same cchsflow feeders (even if the underlying raw sources differ per era), use the same feeder names for all those cycles. Under the `DerivedVar::[union]` format this is automatic; under per-cycle cycle-specific tokens, write each cycle with the same bracket contents:

```
# All post-2015 cycles use the same cchsflow feeders — consistent across eras
cchs2015_2016_m::[VAR_A, VAR_B], cchs2017_2018_m::[VAR_A, VAR_B],
cchs2022_m::[VAR_A, VAR_B], cchs2023_m::[VAR_A, VAR_B]
```

Even if 2022+ maps to different raw CCHS sources, `variables.csv` still references the same cchsflow feeder names because that's what `rec_with_table()` resolves.

---

## 10. Category label and units consistency

### catLabel and catStartLabel

`catStartLabel` must describe the value at `recStart` for that specific row — it must not carry over a label from a preceding row. Rules by row type:

| Row type | catStartLabel |
|---|---|
| Single-value integer category | Same as `catLabel` |
| Range category (`[lo, hi]`) | Label for the lower-bound value |
| Last category in an ascending series | Same as `catLabel` (it is its own start) |
| Copy / pass-through row | Describe the data type (e.g., `"Age in years"`) |

**Common error**: the last category row inherits `catStartLabel` from the row above (the second-to-last category). Each row's `catStartLabel` must reflect its own `recStart`.

### units

`units` must be consistent across all rows in a variable block — value rows, NA::a rows, NA::b rows. If any row has `units=years`, all rows in that block must also have `units=years`. Use `N/A` for categorical variables with no meaningful unit.

---

---

## 11. NA row label conventions

The `catLabel`, `catLabelLong`, and `catStartLabel` fields for `NA::a` and `NA::b` rows must follow fixed conventions — no free-text variation allowed.

### NA::a rows

| Field | Required value |
|---|---|
| `catLabel` | `not applicable` |
| `catLabelLong` | `not applicable` |
| `catStartLabel` | `not applicable` |

All three fields are identical and always lowercase.

### NA::b else rows (`recStart = else`)

| Field | Required value |
|---|---|
| `catLabel` | `missing` |
| `catLabelLong` | `missing` |
| `catStartLabel` | `else` |

### NA::b non-else rows (`recStart` is a code range)

| Field | Required value |
|---|---|
| `catLabel` | `missing` |
| `catLabelLong` | `missing` |
| `catStartLabel` | derived from `recStart` code family (see below) |

**catStartLabel format** — read the lower bound of `recStart` to determine the code family:

| recStart example | catStartLabel |
|---|---|
| `[7,9]` or `7` | `don't know (7); refusal (8); not stated (9)` |
| `[97,99]` or `97` | `don't know (97); refusal (98); not stated (99)` |
| `[997,999]` or `997` | `don't know (997); refusal (998); not stated (999)` |
| `N/A` (Func:: block) | `missing` |

The trailing digit of the lower bound is always 7 (don't know). Refusal is +1, not stated is +2.

### Category value rows (non-NA)

`catLabel` and `catLabelLong` for Yes/No response categories must have the first letter capitalised: `Yes`, `No`. Never `yes`, `no`.

### Common errors to avoid

| Wrong | Correct |
|---|---|
| `catLabelLong = "Not applicable"` | `catLabelLong = "not applicable"` |
| `catLabelLong = "Don't know/Refusal/Not stated"` | `catLabelLong = "missing"` |
| `catLabelLong = "Catch-all missing"` | `catLabelLong = "missing"` |
| `catStartLabel = "Valid skip"` on NA::a | `catStartLabel = "not applicable"` |
| `catStartLabel = "DK/Refused/NS"` | `catStartLabel = "don't know (97); refusal (98); not stated (99)"` |
| `catStartLabel = "97-99"` | `catStartLabel = "don't know (97); refusal (98); not stated (99)"` |
| `catStartLabel = "Else"` (capital E) | `catStartLabel = "else"` |

---

## Related documentation

- [variableStart-databaseStart-authoring.md](variableStart-databaseStart-authoring.md) — detailed rules for `variableStart`/`databaseStart` coordination, era-specific naming, and the dangerous `[VAR]` default pattern
- [pumf-master-harmonization.md](pumf-master-harmonization.md) — PUMF availability by cycle; when and how to split rows by database type
