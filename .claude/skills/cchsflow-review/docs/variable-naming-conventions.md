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

When a variable has a **harmonization break** across cycles, use an **era-based
suffix** rather than an abstract letter. A harmonization break is a point where
a variable can no longer consistently measure the same exposure across cycles
due to:

- **Category changes**: response categories added, removed, or restructured
  (e.g., a 10-category age grouping split into 11 categories at 2005)
- **Wording changes**: question text changed enough to alter measurement
  (e.g., "Did you ever smoke daily?" vs "Have you smoked at least 100
  cigarettes in your lifetime?")
- **Measurement breaks**: the underlying construct changed (e.g., self-reported
  vs derived, or a different routing/skip pattern)

Not every change warrants a break. Minor wording adjustments or label
refinements that do not materially affect measurement can be harmonized under
one name, with a note in the worksheet documenting the concern. Whether a change
constitutes a break is a **team judgment call** informed by domain expertise.

**The suffix year should reflect where harmonization fails** — the cycle
boundary at which consistent measurement can no longer be maintained. This may
differ from when StatCan renamed the variable. For example, if StatCan
introduced SMKDVSTY in 2015 but cchsflow can reconstruct the new scheme from
SMKDSTY data back to 2009, the harmonization boundary is 2009 (where
reconstruction becomes possible), not 2015 (where the name changed).

| Avoid | Use instead | When |
|-------|-------------|------|
| `_A`  | `_2001`, `_pre2005` | Earlier variant with different categories |
| `_B`  | `_2005plus` | Later variant after category restructure |
| `_A`, `_B` | `_pre2009`, `_2009plus` | Pre/post harmonization boundary |

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
