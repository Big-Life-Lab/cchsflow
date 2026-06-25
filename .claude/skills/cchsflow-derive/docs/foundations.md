# Foundations

Core concepts that apply to all derived variable functions in cchsflow.

## Why the 3-step architecture?

CCHS data arrives with raw numeric missing codes (6, 7, 8, 9 for
single-digit variables; 996, 997, 998, 999 for triple-digit). These codes
are embedded in the same numeric column as valid data — a `9` might be a
real value or "not stated" depending on the variable.

Before v3, functions had to know which codes were valid and which were
missing for each variable, leading to hardcoded values scattered throughout
the codebase. The 3-step architecture solves this by extracting missing
code definitions from `variable_details.csv` and handling them uniformly.

v3 uses the `haven` package's `tagged_na()` to represent missing data.
Tagged NAs behave like regular `NA` in most R operations but carry a tag
("a" for not applicable, "b" for not stated) that preserves the reason
the data is missing. This is important for downstream analysis — a
researcher needs to know whether a value is missing because the question
didn't apply (never-smoker asked about quit date) or because the
respondent declined to answer.

## When to use each tier

Problems identified in legacy code, grouped by which tier addresses them.

### All tiers (even bronze must avoid these)

- **String vs object NA** — comparing `NA(a)` as a string instead of using `haven::tagged_na()` objects
- **Missing codes treated as data** — `9` passes numeric comparisons silently when it means "not stated"
- **Mixed return types** — functions returning character in some branches, numeric in others
- **No output validation** — domain logic can produce out-of-range values with no check
- **Pathway confusion** — unclear which respondents should get NA::a vs a calculated value

### Silver adds (no hardcoding, standalone, documented)

- **Hardcoded missing codes** — `if (x == 9)` breaks when a variable uses `99` or `996`
- **Duplicated lookup tables** — same midpoint map copy-pasted across functions
- **Deep if-else nesting** — 4+ levels of `if_else2()` obscure the logic
- **Not standalone** — functions that can't be copy-pasted into a researcher's script

### Gold adds (full 3-step, priority missing, clean_variables)

- **Separate PUMF/Master functions** — duplicated code for the same formula with different variable names. Use ONE source-agnostic function with semantic params; let the worksheet route
- **Vectorisation ambiguity** — unclear whether a function handles vectors or scalars only
- **No input validation** — out-of-range inputs silently produce wrong results
- **Sophisticated pathway awareness** — complex decision trees (e.g., quit timing) need explicit pathway routing with gate variables

## The 3-step architecture in detail

### Step 1: Clean inputs

```r
cleaned <- clean_variables(
  vars = list(
    SMK_005 = SMK_005,
    SMK_030 = SMK_030
  ),
  output_format = "tagged_na"
)
```

**What this does:**

1. Looks up each variable's missing codes in `variable_details.csv` via
   `get_complete_pattern()`. For `SMK_005`, this might return
   `na_a_codes = c(6)` and `na_b_codes = c(7, 8, 9)`.
2. Converts those raw codes to tagged NAs: `6` → `tagged_na("a")`,
   `7/8/9` → `tagged_na("b")`.
3. Validates that all input vectors have the same length.
4. Returns a named list of cleaned vectors.

**CRITICAL: Step 1 must always use `output_format = "tagged_na"`.** The
user's requested format should only be passed to step 3. If step 1 uses
`"original"`, `clean_variables()` converts missing codes back to numeric
values (e.g., 999), and `any_missing()` in step 2 will not detect them —
it sees 999 as valid data. This is a confirmed bug in several existing
functions that pass `output_format` through to step 1.

**The `output_format` parameter** controls whether missing codes are
represented as `haven::tagged_na()` values ("tagged_na") or kept as their
original numeric codes ("original"). Use "tagged_na" for step 2 logic,
then pass the user's requested format in step 3.

**Why `cleaned$SMK_005` instead of just `SMK_005`?** After step 1, the
raw value `9` (which looked like valid data) has been converted to
`tagged_na("b")`. The `cleaned$` prefix accesses the cleaned version
where missing codes are now detectable by `any_missing()`. Using the
uncleaned input in step 2 would miss these hidden missing values.

**No hardcoded missing codes.** The pattern (which codes mean "missing")
comes from `variable_details.csv`, not from the function. If a variable
uses different codes in different cycles, the metadata handles it.

### Step 2: Domain logic

```r
result <- dplyr::case_when(
  # Check for missing data first — always the first arm
  any_missing(cleaned$SMK_005) ~
    get_priority_missing(cleaned$SMK_005, cleaned$SMK_030),

  # Domain logic with cleaned values
  cleaned$SMK_005 == 1 ~ 1L,  # Daily smoker
  cleaned$SMK_005 == 2 ~ 2L,  # Occasional smoker
  cleaned$SMK_005 == 3 ~ 3L,  # Former smoker

  # Catch-all: anything that didn't match
  .default = assign_missing("not_applicable", "SMKDSTY_cat3", output_format)
)
```

**Why check missing first?** If `SMK_005` is `tagged_na("b")` (not stated),
comparing it to `1` returns `NA` (not `FALSE`), and `case_when()` would
skip that arm. By checking `any_missing()` first, we catch all missing
values before they fall through the logic.

**Why `get_priority_missing()` and not just `NA`?** When multiple inputs
are missing, we want the most informative missing code. If one input is
NA::a (not applicable) but another is NA::b (not stated), the output
should be NA::b — because the data was collected but missing, which is
different from the question not applying.

### Step 3: Clean outputs

```r
output_clean <- clean_variables(
  vars = list(SMKDSTY_cat3 = result),
  output_format = output_format  # Pass through user's requested format
)
output_clean$SMKDSTY_cat3
```

**What this does:**

1. Validates that output values fall within the expected range defined in
   `variable_details.csv` for the output variable.
2. Converts the output to the user's requested format. If the user asked
   for "original" codes (numeric 6/7/8/9), tagged NAs are converted back.

**Why clean outputs?** It catches bugs where domain logic produces a value
outside the expected range, and it respects the user's choice of missing
data representation.

## Missing data handling

### Why tagged NAs?

Base R has only one `NA` type. The CCHS has four missing categories:

| CCHS code | Meaning | cchsflow tagged_na |
|-----------|---------|-------------------|
| 6 | Not applicable | `tagged_na("a")` |
| 7 | Don't know | `tagged_na("b")` |
| 8 | Refusal | `tagged_na("b")` |
| 9 | Not stated | `tagged_na("b")` |

cchsflow collapses these to two: NA::a (not applicable) and NA::b
(missing/not stated). The `haven` package makes this work — tagged NAs
behave like regular NAs in arithmetic and comparisons, but the tag is
preserved for downstream analysis.

See `vignette("tagged_na_usage")` for more background.

### Key functions

| Function | Purpose | Returns |
|----------|---------|---------|
| `any_missing(var1, var2, ...)` | Detect if any input has a missing value | Logical vector |
| `get_priority_missing(var1, var2, ...)` | Return highest-priority missing code | NA::b wins over NA::a |
| `assign_missing(type, var_name, output_format)` | Create a missing value of the right type | Tagged NA or numeric code |

### Missing data priority

NA::b (not stated) has higher priority than NA::a (not applicable). If any
input is NA::b, the output should be NA::b. This reflects that "data was
collected but missing" is more informative than "question didn't apply."

### Pattern in code

```r
# Always check missing first in case_when()
dplyr::case_when(
  any_missing(cleaned$var1, cleaned$var2) ~
    get_priority_missing(cleaned$var1, cleaned$var2),
  # ... domain logic ...
  .default = assign_missing("not_applicable", "output_var", output_format)
)
```

## NULL input handling

Functions with optional parameters (variables not present in certain CCHS
cycles) use `= NULL` defaults. The `expand_null_inputs()` helper in
`R/clean-variables.R` converts NULLs to NA vectors of the correct length.

### All-NULL convention

When **all** inputs are NULL — meaning the entire variable set wasn't
collected in that survey cycle — the function returns `haven::tagged_na("c")`.
This is distinct from NA::a (not applicable at respondent level) and NA::b
(question asked but unanswered). NA::c means "not collected" at the survey
level.

```r
# Pattern for functions with optional parameters
my_function <- function(var1 = NULL, var2 = NULL, output_format = "tagged_na") {
  # All-NULL: variable not collected in this cycle
  if (is.null(var1) && is.null(var2)) {
    return(haven::tagged_na("c"))
  }
  # Expand remaining NULLs to NA vectors
  n <- max(length(var1), length(var2))
  optional <- expand_null_inputs(list(var1 = var1, var2 = var2), n)
  var1 <- optional$var1
  var2 <- optional$var2
  # ... Step 1/2/3 continues ...
}
```

### When to add NULL defaults

- **Add** `= NULL` when a parameter represents a variable that may not exist
  in all CCHS cycles (optional feeder)
- **Don't add** when all parameters are required for the calculation to
  produce any meaningful result

## Quality tiers

### Bronze — ship it

Minimum for working code:

- Correct output for valid inputs
- Basic missing data handling (at minimum, NA passthrough)
- May use `if`/`else` or `if_else2()`
- May hardcode midpoint values or thresholds
- Roxygen with `@title`, `@param`, `@return`, basic `@examples`

### Silver — solid

Everything in bronze, plus:

- No hardcoded values (use constants, lookup tables, or recEnd)
- Comprehensive roxygen with executable examples for:
  - Scalar input
  - Vector input
  - Dataframe input (via `mutate()`)
- Standalone `rec_with_table()` examples in documentation
- `case_when()` instead of if-else chains

### Gold — reference

Everything in silver, plus:

- Full 3-step architecture (clean_variables → logic → clean_variables)
- `any_missing()` / `get_priority_missing()` for missing data
- `assign_missing()` for explicit not-applicable returns
- Missing codes extracted from `variable_details.csv` — no hardcoded codes
- `haven::tagged_na()` for missing value representation
- Tidyverse naming conventions (verb-based function names)
- Tidyverse dependencies (`dplyr::case_when()`, etc.)
- Function works standalone (copy-paste without full cchsflow install)

## Coding standards

### Source-agnostic functions

Functions use semantic parameter names (`height_m`, `weight_kg`, `age`)
not CCHS variable names (`HWTGHTM`, `HWTDHTM`). One function serves
both PUMF and Master — the worksheet routes different source variables
to the same parameters. This makes functions portable (copy-paste into
other systems) and eliminates code duplication.

Inside `clean_variables()` Step 1, map semantic names to a representative
CCHS variable for pattern lookup:

```r
cleaned <- clean_variables(
  vars = list(HWTGHTM = height_m, HWTGWTK = weight_kg),
  output_format = "tagged_na"
)
```

### Naming

- Function names use verbs: `calculate_`, `assess_`, `derive_`
- Follow pattern: `calculate_<VARIABLE_NAME>()`
- Use snake_case for all function and parameter names
- Parameter names are semantic (descriptive of the concept, not the CCHS variable)

### Tidyverse

- `dplyr::case_when()` replaces nested if-else chains
- `haven::tagged_na()` for missing value coding
- Do NOT use `if_else2()` — it is deprecated. Use `dplyr::if_else()` if
  needed, but prefer `case_when()` for multi-branch logic.

### Standalone functions

Functions should work without the full cchsflow package installed. A
researcher should be able to copy-paste a function and its dependencies
into their own script. This means:

- Use `dplyr::case_when()` not `case_when()` (namespace-qualify)
- Document which packages are needed
- Include self-contained examples

### Input types

Every function must work on:

- **Scalar**: `calculate_foo(var1 = 1, var2 = 2)`
- **Vector**: `calculate_foo(var1 = c(1, 2, 3), var2 = c(4, 5, 6))`
- **Dataframe**: via `mutate()` — `df %>% mutate(result = calculate_foo(col1, col2))`

This is achieved naturally by using `case_when()` which is vectorised.

### Documentation (roxygen)

Silver and gold tier require the template below. See also anti-patterns.

```r
#' @title Calculate [variable description]
#'
#' @description
#' [What the function does, in 1-2 sentences]
#'
#' @details
#' [Implementation approach, source variables, coverage notes]
#'
#' @param var1 [description]
#' @param var2 [description]
#'
#' @return [description of output type and range]
#'
#' @examples
#' # Scalar
#' calculate_foo(var1 = 1, var2 = 2)
#'
#' # Vector
#' calculate_foo(var1 = c(1, 2, 3), var2 = c(4, 5, 6))
#'
#' # Dataframe
#' library(dplyr)
#' df <- data.frame(var1 = c(1, 2), var2 = c(3, 4))
#' df %>% mutate(result = calculate_foo(var1, var2))
#'
#' # Standalone with rec_with_table
#' result <- rec_with_table(
#'   cchs2015_2016_p,
#'   variables = variables,
#'   variable_details = variable_details,
#'   log = TRUE
#' )
#'
#' @export
```

## Anti-patterns

Common bugs found during review. Check for these when reviewing or
writing gold-tier functions.

### Step 1 output_format pass-through

**Wrong:**
```r
cleaned <- clean_variables(vars = list(...), output_format = output_format)
```

**Right:**
```r
# Step 1: always tagged_na so any_missing() works in Step 2
cleaned <- clean_variables(vars = list(...), output_format = "tagged_na")
# Step 3: user's format
output_clean <- clean_variables(vars = list(...), output_format = output_format)
```

### Joint missing check on gate + source variables

When a function filters by status then passes through a source variable,
checking both together in the first `any_missing()` arm short-circuits
the domain logic for respondents where the source is expected to be NA::a.

**Wrong:**
```r
any_missing(cleaned$SMK_005, cleaned$SMKG040_cont) ~
  get_priority_missing(cleaned$SMK_005, cleaned$SMKG040_cont, ...)
```

A non-daily smoker (SMK_005=2) has SMKG040_cont=NA::a by design. The
joint check catches this before the status routing, returning NA::a via
`get_priority_missing()` instead of the `.default` arm. Result is the
same but the logic path is wrong and fragile.

**Right:** Check the gate variable first, then check source within its
applicable arm:
```r
any_missing(cleaned$SMK_005) ~
  get_priority_missing(cleaned$SMK_005, ...),
cleaned$SMK_005 == 1 & !any_missing(cleaned$SMKG040_cont) ~
  cleaned$SMKG040_cont,
cleaned$SMK_005 == 1 & any_missing(cleaned$SMKG040_cont) ~
  get_priority_missing(cleaned$SMKG040_cont, ...),
.default = assign_missing("not_applicable", ...)
```

### Dead code after any_missing() catches all NAs

`any_missing(status)` catches all tagged NAs including NA::a. A later
arm like `haven::is_tagged_na(status, "a")` is unreachable — the first
arm already matched. Remove redundant arms to avoid confusion.

### source() in package files

Do not use `tryCatch { source("R/helper.R") }` in R package files.
Package functions are loaded via NAMESPACE. Conditional `source()` blocks
are an anti-pattern that can double-load functions or mask package
versions. For standalone use, document dependencies in `@details`.
