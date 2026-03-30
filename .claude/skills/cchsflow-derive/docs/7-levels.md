# Function levels (L1-L7)

A taxonomy of reusable function complexity. Higher levels compose lower
levels. Understanding the level helps you write the right amount of code
and reuse existing infrastructure.

## Level definitions

| Level | Name | Purpose | Example |
|-------|------|---------|---------|
| L1 | Foundational utility | Low-level missing data, cleaning, pattern detection | `any_missing()`, `clean_variables()`, `assign_missing()` |
| L2 | Midpoint mapping | Convert categorical ranges to continuous values via lookup table | `smkg_age_midpoint()` |
| L3 | Single-source pass-through | Wrap and clean a single input, worksheet handles routing | `calculate_age_start_smoking()` |
| L4 | Categorical-to-continuous conversion | Apply midpoint imputation with domain logic | `calculate_SMK_06A_cont()` |
| L5 | Filter/route by status | Extract subset of input based on status filtering | `calculate_SMKG203_cont()`, `assess_quit_pathway()` |
| L6 | Multi-source combining | Route multiple sources with priority hierarchy | `calculate_time_quit_smoking_complete()` |
| L7 | Complex multi-source unification | Full decision tree combining multiple inputs | `calculate_SMKDSTY_cat6()`, `calculate_pack_years()` |

## Decision tree

Use this to classify your function:

```
Does your function just pass through a single source?
  → YES → L3 (pass-through)
  → NO ↓

Does it convert categories to continuous values?
  → YES, using a lookup table only → L2 (midpoint mapping)
  → YES, with domain logic → L4 (cat-to-continuous)
  → NO ↓

Does it filter/extract based on a status variable?
  → YES, single source filtered by status → L5 (filter/route)
  → NO ↓

Does it combine multiple sources with priority?
  → YES, with pathway-aware routing → L6 (combining)
  → NO ↓

Does it have a complex decision tree with multiple inputs?
  → YES → L7 (complex unification)
```

## How levels compose

Pack-years demonstrates the full stack:

```
calculate_pack_years (L7)
├── clean_variables() (L1)
├── any_missing() + get_priority_missing() (L1)
├── SMKDSTY_A (L7: calculate_SMKDSTY_cat6)
├── age_start_smoking (L3: calculate_age_start_smoking)
│   └── derive_passthrough() (L1)
├── time_quit_smoking (L6: calculate_time_quit_smoking_complete)
│   ├── calculate_SMK_06A_cont() (L4)
│   │   └── smkg_age_midpoint() (L2)
│   └── pathway logic with SMK_10_gate (L5: assess_quit_pathway)
├── cigs_per_day (L7: calculate_cigs_per_day)
│   └── status-based routing (L5 pattern)
└── age (L3: via worksheet routing)
```

## Level-by-level guidance

### L1: Foundational utilities

These are shared infrastructure. You rarely write new L1 functions — you
use them. Key functions to know:

- `clean_variables(vars, variable_details, output_format)` — step 1 and 3
- `any_missing(var1, var2, ...)` — vectorised missing detection
- `get_priority_missing(var1, var2, ...)` — NA::b wins over NA::a
- `assign_missing(type, var_name, variable_details)` — create typed missing
- `derive_passthrough(value, variable_name, variable_details, output_format)` — L3 helper

### L2: Midpoint mapping

A lookup table that converts categorical codes to continuous values.
Typically a simple named vector or small helper function.

```r
smkg_age_midpoint <- function(category) {
  midpoints <- c(8, 13, 16, 18.5, 22, 27, 32, 37, 42, 47, 55)
  midpoints[category]
}
```

### L3: Single-source pass-through

Minimal wrapper around `derive_passthrough()`. The worksheet handles
which source variable to feed in.

```r
calculate_age_start_smoking <- function(
    age_start_smoking, variable_details = NULL, output_format = "tagged_na") {
  derive_passthrough(age_start_smoking, "age_start_smoking",
                     variable_details, output_format)
}
```

### L4-L7: See pattern docs

These levels correspond to specific patterns:

- L4 → `patterns/cat-to-continuous.md`
- L5 → `patterns/multi-source-routing.md` (filter variant)
- L6 → `patterns/multi-source-routing.md` or `patterns/pathway-branching.md`
- L7 → `patterns/formula-calculation.md` or `patterns/category-grouping.md`

## Existing function inventory

See `function-inventory.md` for a complete mapping of all current DV
functions to their levels and patterns.
