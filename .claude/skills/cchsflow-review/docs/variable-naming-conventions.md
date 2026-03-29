# Variable naming conventions

This document governs how harmonized variable names are chosen in cchsflow. It
applies both when authoring new variables and when reviewing PRs that introduce
new names.

## Core principle: preserve StatCan names

Use the common harmonized StatCan variable name (e.g., `SMK_09A`, `DHHGAGE`)
unless a suffix is specifically required to distinguish a transformation or
structural variant. Avoid decorative suffixes — only add what is necessary to
disambiguate.

## Suffix rules

### `_cont` — categorical-to-continuous recode only

Add `_cont` when the source variable is **categorical** and the harmonized
variable applies midpoint imputation to produce a continuous output.

```
SMK_09A   →  categorical (codes 1–4)
SMK_09A_cont  →  midpoint-imputed continuous (0.5, 1.5, 2.5, 4)
```

**Do not** add `_cont` if the source variable is already continuous. In that
case, keep the StatCan name unchanged.

### `_catN` — category-count change or derived clarification

Add `_catN` (where N = number of output categories) only when:

1. The number of categories **changes** from the source (collapsing or
   expanding), or
2. The variable is **derived** and the suffix clarifies the output structure
   for users.

**Do not** add `_catN` if the harmonized categories are identical to the source
variable's categories. A variable that recodes unchanged 4-category responses
is still just the source name — it does not become `_cat4`.

### Era/cycle suffixes — use descriptive names, not letters

When a variable has a genuine structural break across cycles (different question
wording, different category boundaries, or a different source variable with
incompatible categories), use an **era-based suffix** rather than an abstract
letter.

| Avoid | Use instead | When |
|-------|-------------|------|
| `_A`  | `_2001`, `_pre2003` | 2001-only variant (cycle 1.1) |
| `_B`  | `_2003plus` | 2003+ variant |
| `_A`, `_B` | `_pre2007`, `_2007plus` | Pre/post 2007 restructuring |

Existing `_A`/`_B` suffixes are deprecated. Replace them with era-based names
when a variable is refactored or reviewed, unless the refactor is out of scope.

### Other clarifying suffixes

Add clarifying suffixes as needed when disambiguation is genuinely required and
the above rules do not apply. Keep suffixes short and self-explanatory. A
reviewer should be able to infer the meaning of a suffix without consulting the
worksheet.

## DerivedVar block `recEnd` semantics

In `variable_details.csv`, `DerivedVar` blocks document the output of a custom
R function. The `recEnd` values in these blocks are **output category codes**
produced by the function — not recode targets and not midpoints.

- Categorical DV output: `recEnd` values are integers (1, 2, 3, …) matching
  the function's return values
- Continuous DV output: `recEnd` values are midpoints or numeric outputs
  matching the function's return values

This differs from direct recode blocks, where `recEnd` is the target value that
`rec_with_table()` writes into the output. Do not flag integer `recEnd` values
in a `DerivedVar` block as inconsistent with midpoint values in a sibling direct
recode block — they serve different purposes.

## Examples

| Variable | Name chosen | Rationale |
|----------|-------------|-----------|
| `SMK_09A` (categorical, unchanged) | `SMK_09A` | No transformation; keep StatCan name |
| `SMK_09A` → midpoint imputed | `SMK_09A_cont` | Categorical → continuous recode |
| `SMK_09A` collapsed to 4 cats | `SMK_09A_cat4` | Category count change from source |
| `SMK_09A` 2001 variant (different categories) | `SMK_09A_cat4_2001` | Era suffix for structural break |
| `SMK_09A` 2003+ variant | `SMK_09A_cat4_2003plus` | Era suffix, more readable than `_B` |
| Continuous source variable (no transform) | `DHHGAGE` | Already continuous; no `_cont` |

## Relationship to `dummyVariable` naming

`dummyVariable` values follow a separate convention (see Check 5b in the
review skill) and are derived from the harmonized variable name. The suffix
rules above govern the harmonized name itself, which then propagates into
`dummyVariable` values.
