# Testing derived variable functions

How to write, maintain, and diagnose tests for cchsflow derived variables.

## Test types

cchsflow uses two kinds of tests for derived variables:

| Type | Location | What it checks |
|------|----------|----------------|
| **Unit tests** | `tests/testthat/test-<domain>.R` | Single function, scalar inputs, expected outputs |
| **Golden fixture tests** | `tests/testthat/test-recode-with-table.R` | Full `rec_with_table()` pipeline against saved RData snapshots |

### Unit tests

Each DV function should have unit tests covering:

1. **Valid inputs** — representative values for each branch of the `case_when()`
2. **Out-of-range inputs** — values outside valid range, verify correct missing type
3. **Missing inputs** — `NA`, `tagged_na("a")`, `tagged_na("b")` as appropriate
4. **Edge cases** — boundary values, zero-length vectors

Example (gold tier):

```r
test_that("calculate_my_var returns correct value for status 1", {
  result <- calculate_my_var(smoking_status = 1, age = 45, value = 20)
  expect_equal(result, 25.0)
})

test_that("calculate_my_var returns NA::a for never smokers", {
  result <- calculate_my_var(smoking_status = 6, age = 50, value = NA)
  expect_true(haven::is_tagged_na(result, "a"))
})

test_that("calculate_my_var returns NA::b when input is missing", {
  result <- calculate_my_var(smoking_status = 1, age = 45,
                             value = tagged_na("b"))
  expect_true(haven::is_tagged_na(result, "b"))
})
```

### Golden fixture tests

`test-recode-with-table.R` runs `rec_with_table()` on sample PUMF data
(200-row `_p` datasets in `data/`) and compares every column against saved
"standard" RData files in `tests/testdata/rec_with_table_test_data.RData`.

These are **regression tests** — they catch unintended changes but must be
regenerated when functions intentionally change.

## Common failure patterns

### Pattern 1: v2 string vs v3 tagged_na

**Symptom**: `actual is a character vector ('NA(b)')`, `expected is a double vector (NA)`

**Cause**: The function uses the v2 pattern (returns string `"NA(b)"`) but the
test expects v3 behaviour (`tagged_na("b")`).

**Which is wrong?** Depends on the function's tier:
- **v2 (bronze/silver)** functions: the test expectation is aspirational.
  Either downgrade the test to match v2 output, or upgrade the function to
  gold tier.
- **v3 (gold)** functions: the function should return `tagged_na("b")` via
  `assign_missing()`. If it returns string `"NA(b)"`, the function has a bug.

**Example**: `low_drink_score_fun(-1, 1)` returns `"NA(b)"` (string) but
test expects `tagged_na("b")`. The function is v2; the test was written for
a v3 future that hasn't been implemented.

### Pattern 2: Missing type distinction (NA::a vs NA::b)

**Symptom**: `actual: "NA(a)"`, `expected: "NA(b)"` in golden fixtures.

**Cause**: v3 infrastructure correctly distinguishes "not applicable" (the
question doesn't apply to this respondent) from "not stated" (the respondent
didn't answer). Old golden fixtures used `NA(b)` for both.

**Which is wrong?** Usually the fixture is wrong. A never-smoker should get
`NA(a)` for pack-years, not `NA(b)`. Regenerate the fixtures after verifying
the function logic is correct.

**Common variables affected**: `pack_years_cat`, `diet_score_cat3`, and any
derived variable whose input universe excludes certain respondent groups.

### Pattern 3: Scoring range shift

**Symptom**: Values are systematically offset (e.g., actual=0 where expected=5,
actual=2 where expected=3).

**Cause**: The function's scoring logic was changed (e.g., from independence
score to needs-help count) but the unit test expectations and golden fixtures
weren't updated.

**How to diagnose**: Check `git log --oneline -10 -- R/<file>.R` to see if
the function was recently modified. Compare the current logic against the
test expectation to determine which is semantically correct.

**Fix**: Update both the unit test AND the golden fixtures. These must stay
in sync.

### Pattern 4: Calculation formula changes

**Symptom**: Continuous values differ (e.g., actual=0.95, expected=1.90).

**Cause**: The calculation formula was intentionally changed (e.g., fixing a
factor-of-2 error, changing midpoint imputation). Golden fixtures reflect the
old formula.

**How to diagnose**: Check the git history for the calculation function.
Verify the new formula is correct against the CEP or design documentation.

**Fix**: Regenerate golden fixtures after confirming the new formula is
correct.

### Pattern 5: Golden fixture level padding

**Symptom**: Factor levels differ in whitespace (e.g., `"1 "` vs `"1    "`).

**Cause**: Factor level widths are determined by the longest level string.
When new levels are added (like `"NA(b)"` becoming `"NA(b)"` instead of
`"NA"`), padding changes for all levels in the column.

**This is cosmetic** — usually accompanies a real change (patterns 1-4).

## When to regenerate golden fixtures

Regenerate `rec_with_table_test_data.RData` when:

1. A DV function's logic intentionally changed (new formula, scoring range)
2. Missing data handling upgraded from v2 to v3 (string to tagged_na)
3. New variables added to `variable_details.csv`
4. Worksheet changes alter how `rec_with_table()` routes variables

**Do NOT regenerate** to paper over unexpected failures. First confirm the
function change is correct.

### How to regenerate

```r
# Load current worksheets and sample data
variables <- read.csv("inst/extdata/variables.csv")
variable_details <- read.csv("inst/extdata/variable_details.csv")

# Generate new standards for each cycle
cchs2001Standard <- suppressWarnings(
  rec_with_table(cchs2001_p,
                 variables = variables$variable,
                 variable_details = variable_details,
                 note = FALSE)
)
# Repeat for cchs2003_p, cchs2005_p, cchs2015_2016_p

# Save
save(variables, variable_details,
     cchs2001Standard, cchs2003Standard,
     cchs2005Standard, cchs2015Standard,
     file = "tests/testdata/rec_with_table_test_data.RData")
```

**Always review the diff** between old and new fixtures. Every changed value
should be explainable by a known function change.

## Writing tests for new DV functions

### Bronze tier

At minimum, test the happy path and one missing input:

```r
test_that("calculate_my_var returns expected value", {
  expect_equal(calculate_my_var(input = 25), 25)
})
```

### Silver tier

Add out-of-range, missing, and vector tests:

```r
test_that("calculate_my_var handles vector input", {
  result <- calculate_my_var(input = c(10, 20, NA))
  expect_equal(result[1:2], c(10, 20))
  expect_true(is.na(result[3]))
})
```

### Gold tier

Test every branch of the `case_when()`, verify missing type tags, and test
`output_format` parameter:

```r
test_that("calculate_my_var returns NA::a for not-applicable status", {
  result <- calculate_my_var(smoking_status = 6, value = NA)
  expect_true(haven::is_tagged_na(result, "a"))
})

test_that("calculate_my_var returns NA::b when required input missing", {
  result <- calculate_my_var(smoking_status = 1,
                             value = tagged_na("b"))
  expect_true(haven::is_tagged_na(result, "b"))
})

test_that("calculate_my_var respects output_format = 'numeric'", {
  result <- calculate_my_var(smoking_status = 6, value = NA,
                             output_format = "numeric")
  # Should return the numeric missing code, not tagged_na
  expect_false(haven::is_tagged_na(result))
})
```

### Testing the gate vs source pattern

For functions with gate variables (e.g., smoking status) and source
variables (e.g., cigarettes per day), test these combinations:

| Gate | Source | Expected |
|------|--------|----------|
| Valid, applies | Valid | Calculated value |
| Valid, applies | NA::b | NA::b (missing source) |
| Valid, does not apply | NA::a | NA::a (not applicable) |
| NA::b | Any | NA::b (missing gate) |
| NA::a | Any | NA::a (not applicable gate) |

This catches the **joint missing check bug** where checking
`any_missing(gate, source)` together short-circuits for respondents where
the source is legitimately NA::a.

## Current test debt (as of 2026-03)

The following pre-existing failures reflect the v2→v3 transition in
progress. They are tracked here so developers don't waste time
investigating known issues:

| Tests | Category | Status |
|-------|----------|--------|
| `test-adl.R:46` (1 failure) | ADL scoring range changed (0-based vs 1-based) | Needs: decide correct scoring, update test |
| `test-alcohol.R:164-194` (6 failures) | Functions return v2 string `"NA(b)"`, tests expect v3 `tagged_na("b")` | Needs: upgrade functions to gold OR downgrade test expectations |
