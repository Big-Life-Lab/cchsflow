---
name: cchsflow-derive
description: Write and review derived variable functions for cchsflow. Use when implementing new DV functions (calculate_*, assess_*, categorize_*), upgrading existing functions to v3 architecture, reviewing DV code for correctness, or preparing DV changes for commit. Covers the 3-step architecture, source-agnostic design, quality tiers, patterns, testing, and package-level validation.
allowed-tools: Bash(Rscript:*), Bash(R:*), Bash(git:*), Read, Glob, Grep
---

# cchsflow derived variable development

Write, review, and validate derived variable functions using the v3 3-step architecture.

## Usage

```
/cchsflow-derive                    # general guidance (reads foundations)
/cchsflow-derive calculate_bmi      # review/write a specific function
/cchsflow-derive --check            # run done criteria checks
```

## Before you start

### Required reading

Before writing or reviewing a DV function, read these docs (in this skill's `docs/` folder):

1. **[foundations.md](docs/foundations.md)** — 3-step architecture, missing data handling, quality tiers, coding standards, anti-patterns. Read this first.
2. **The pattern doc** that matches your function (see "Choose a pattern" below)

### Choose a pattern

Identify which pattern your function follows, then read the corresponding doc:

| Pattern | When to use | Doc |
|---------|-------------|-----|
| **Formula calculation** | Compute a value from inputs (BMI, pack-years) | [formula-calculation.md](docs/patterns/formula-calculation.md) |
| **Category grouping** | Map values to categories (BMI categories, smoking status) | [category-grouping.md](docs/patterns/category-grouping.md) |
| **Pass-through** | Clean and forward a single variable | [pass-through.md](docs/patterns/pass-through.md) |
| **Cat-to-continuous** | Midpoint imputation from categorical ranges | [cat-to-continuous.md](docs/patterns/cat-to-continuous.md) |
| **Multi-source routing** | Choose best source with priority chain | [multi-source-routing.md](docs/patterns/multi-source-routing.md) |
| **Pathway branching** | Complex decision tree with gate variables | [pathway-branching.md](docs/patterns/pathway-branching.md) |

### Reference material

- **[7-levels.md](docs/7-levels.md)** — function complexity taxonomy (L1-L7)
- **[function-inventory.md](docs/function-inventory.md)** — all existing DV functions with pattern, level, and tier
- **[testing.md](docs/testing.md)** — unit test and golden fixture patterns, common failure diagnostics

## Development workflow

### 1. Write tests

Follow the test tier matching your function's quality tier (see [testing.md](docs/testing.md)):

- **Bronze**: Happy path + one missing input
- **Silver**: + out-of-range, vectors, dataframe via `mutate()`
- **Gold**: + every `case_when()` branch, tagged NA type verification, `output_format` parameter

### 2. Write the function

Follow the pattern template from the appropriate pattern doc. Key principles:

- **Source-agnostic**: Semantic parameter names (`height_m`, `weight_kg`), not CCHS variable names. ONE function for both PUMF and Master; the worksheet routes different source variables to the same parameters.
- **3-step**: `clean_variables(output_format = "tagged_na")` → `case_when()` logic → `clean_variables(output_format = output_format)`
- **Step 1 always uses `"tagged_na"`**: Never pass the user's `output_format` to Step 1 — `any_missing()` in Step 2 won't detect numeric missing codes.
- **Namespace-qualify**: `dplyr::case_when()`, `haven::tagged_na()` — functions must work standalone.

### 3. Write roxygen documentation

Silver and gold tier require the full template (see foundations.md § Documentation):

```r
#' @title [verb phrase]
#' @description [1-2 sentences]
#' @details [implementation notes, PUMF vs Master table if source-agnostic]
#' @param var1 [description]
#' @param output_format Output missing data format: "tagged_na" (default) or "original".
#' @param ... Arguments passed from deprecated aliases.
#' @return [type and range]
#' @examples
#' # Scalar
#' # Vector
#' # Dataframe
#' # Standalone with rec_with_table (in \dontrun{})
#' @references
#' @seealso
#' @export
```

**`@param ...` rule**: If deprecated aliases use `@rdname` pointing to your function and their signature is `function(...)`, you MUST add `@param ... Arguments passed from deprecated aliases.` to your roxygen. Otherwise R CMD check will report "Undocumented arguments in Rd file: '...'".

### 4. Write deprecated aliases (if renaming)

If the function replaces an older function name, add aliases in `R/deprecated-aliases.R`:

```r
#' @rdname new_function_name
#' @export
old_function_name <- function(...) {
  .Deprecated("new_function_name",
    msg = "old_function_name() is deprecated. Use new_function_name() instead.")
  new_function_name(...)
}
```

### 5. Update worksheets (if needed)

If the function is referenced from `variable_details.csv` via `Func::`:

- Update `recEnd` to point to the new function name
- Update `dummyVariable` if function name changed
- Run `Rscript exec/fix-worksheets.R` after any CSV modification
- Rebuild RData if worksheet structure changed (see cchsflow-worksheets skill)

## Done criteria

**Before committing DV function changes, ALL of these must pass.** Run them in order — earlier checks are faster and catch different issues.

### Check 1: Unit tests pass

```r
# From the project root (or worktree root)
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-<domain>.R")'
```

Verify: 0 failures for in-scope tests. Pre-existing failures in other test files are acceptable (note them but don't block on them).

### Check 2: R CMD check passes

```r
# Quick check — catches NAMESPACE, roxygen, imports (skips tests/examples)
Rscript -e 'devtools::check(document = FALSE, args = "--no-tests --no-examples --no-vignettes --no-manual")'

# Full check — recommended before PR
Rscript -e 'devtools::check()'
```

Verify: 0 **new** errors/warnings/notes compared to the branch baseline. Common issues caught only here:

- Undocumented `...` from `@rdname` aliases
- Missing NAMESPACE exports
- Broken `@examples`
- Undeclared imports in DESCRIPTION

### Check 3: Worksheet validation (if worksheets changed)

Invoke the `cchsflow-validation` skill, or run manually:

```r
Rscript exec/fix-worksheets.R
```

### Check 4: Roxygen checklist

Verify manually against the template in Step 3 above:

- [ ] `@title`, `@description`, `@details` present
- [ ] All `@param` documented (including `...` if aliases exist)
- [ ] `@examples` includes scalar, vector, dataframe, and `rec_with_table()`
- [ ] `@return` describes type and range
- [ ] `@export` present
- [ ] `@seealso` links related functions

### Check 5: Test coverage checklist

- [ ] Every `case_when()` branch has a test
- [ ] Scalar, vector, and dataframe inputs tested
- [ ] Missing inputs tested (NA, tagged_na("a"), tagged_na("b"))
- [ ] Boundary values tested (for categorization functions)
- [ ] Deprecated aliases tested (expect deprecation warning + correct delegation)

## Cross-references

### Related cchsflow skills

- **cchsflow-review** — PR review of worksheet changes (L0-L6 process). Lives on `skills/review-validation` branch.
- **cchsflow-validation** — programmatic worksheet validation. Lives on `skills/review-validation` branch.
- **cchsflow-worksheets** — worksheet authoring guidance. Lives on `skills/review-validation` branch.

### External references

- R CMD check guidance: `~/github/ai-infrastructure/context/domains/r_packages.md` § "Local verification before committing"
- V3 coding standards: project memory `project_derive_function_standards.md`
- Reference implementations: `calculate_bmi()` in `R/bmi.R` (formula), `calculate_pack_years()` in `R/smoke-pack-years.R` (complex)
