# Three-Step Architecture Review

**Date:** June 10, 2026
**Status:** Background document for v4 scoping
**Scope:** Deep review of the v3 3-step derived-variable architecture, its
supporting functions, and the case for a modular `rec_with_table()` refactor.

## Purpose

This review answers three questions ahead of the v4 refactor:

1. How well does the 3-step pattern, as built in v3, meet its design goals?
2. Which supporting functions are general-purpose enough to become the core
   of a modular `rec_with_table()`?
3. What did the ADL/alcohol repair teach us about the architecture's
   remaining gaps?

It draws on a full read of the v3 engine and derived-variable code, the test
suite, the worksheet metadata, and the design history recovered from the
abandoned v3.0.0 branch lineage (PR #137/#143, branch `3-step-tidyverse`).

## Design history in brief

Two modernization efforts ran in parallel during 2025--2026:

- **The big-bang lineage** (PR #137, July 2025; copied to PR #143 for gradual
  merge). This produced the formal flexible-missing-data specification, the
  architecture analyses, and the prototype code in `development/`. Its
  support documents were deleted in cleanup commits on that branch; they
  remain recoverable from git history (see appendix).
- **The incremental lineage** (the v3 branch). Domain-by-domain CEP work that
  shipped. The big-bang implementation code crossed over (missing-pattern
  cache, `clean_variables()`, the smoking suite); its design documentation
  did not.

The recovered `LESSONS_LEARNED.md` records four architectural phases tried
and rejected before the final design: a YAML handler factory, an enhanced
`clean_variables_2()`, missing-safe arithmetic operations, and finally the
two-helper (`any_missing()` + `get_priority_missing()`) approach that
shipped. v4 should take the original specification's scope with the final
phase's simplicity.

## The 3-step pattern as built

The canonical pattern (skill: `cchsflow-derive`):

1. **Step 1** -- `clean_variables(vars, output_format = "tagged_na")`:
   metadata-driven conversion of missing codes to `haven::tagged_na()`,
   using `variable_details.csv` patterns. Always tagged in step 1 so step 2
   can detect missing values.
2. **Step 2** -- `dplyr::case_when()` with `any_missing()` as the first arm,
   returning `get_priority_missing()` (not applicable beats not stated).
3. **Step 3** -- `clean_variables()` on the derived variable with the user's
   requested `output_format`, validating output against the derived
   variable's metadata.

### Verdict against the design goals

The pattern was meant to fix specific v2 gaps: hardcoded missing codes,
no output validation, unclear missing-data semantics, and deep `if_else2()`
nesting. Assessment by layer:

**Function layer: goal met.** The fully modernized functions (three BMI,
eleven smoking) follow the pattern consistently. Verified findings:

- Step 1 correctly hardcodes `"tagged_na"` in every modernized function;
  the step-1 `output_format` bug described in the skill docs is absent from
  this branch.
- The `any_missing()`-first rule is followed without exception.
- Output validation (step 3) is in place for formula functions -- the v2
  BMI-range gap is closed where the pattern is applied.
- `calculate_pack_years()` is the best exemplar: semantic parameters, an
  internal core separating arithmetic from cleaning, defensive handling of
  optional inputs.
- The constants pattern (`smoking-validation-constants.R`) cleanly separates
  epidemiological thresholds (code) from data-cleaning bounds (worksheet).

**Engine layer: goal not yet met.** `rec_with_table()` predates the pattern
and does not use it. Specifics in the integration section below. Until the
engine adopts the same layers, users of the standard workflow get neither
the output validation nor the consistent missing-data semantics that the
modernized functions provide.

**NA-patterns-as-metadata: partially met.** Missing-code patterns are
derived from `variable_details.csv` at runtime (the `recStart`/`recEnd`
rows), cached per variable. This is metadata-driven in substance. What is
missing for the stated goal -- flexible NA patterns defined in a schema --
is a first-class declaration: patterns are inferred from recode rows rather
than declared, the priority hierarchy ships as a code fallback because the
YAML it looks for (`missing_priority_rules.yaml`) does not exist in
`inst/metadata/schemas/`, and only `NA::a`/`NA::b` are supported. The
`output_format = "original"` parameter exists on every modernized function
but cannot be requested through `rec_with_table()`, so original-format
support is standalone-only until the engine refactor.

### Three generations of missing-data idiom

Production code on the v3 branch contained three idioms simultaneously:

1. **Legacy `if_else2()` chains** -- 264 calls across 15 files (diet,
   physical activity, respiratory, RACDPAL, social provision, education and
   others). Deliberately deferred per the team's scope decision.
2. **The abandoned splice idiom** -- `!!!generate_tagged_na_conditions()` in
   adl.R and alcohol.R (July 2025 era). Its dependency file
   (`R/missing-data-helpers.R`) never reached v3, so these functions failed
   at runtime; a silent `tryCatch(source(...))` masked the missing file at
   load time. Removed by the June 2026 repair (below).
3. **The canonical 3-step** -- smoking, BMI, and (post-repair) ADL and
   alcohol.

v4's convergence target: one idiom. Retire `if_else2()` domain by domain;
the splice idiom is already gone.

### Honest criticisms

- About half of each modernized function is step-1/step-3 scaffolding. The
  team has accepted this as the price of readable, self-contained functions;
  the engine refactor should absorb the pattern once rather than ask every
  function author to retype it. (Standalone copy-paste purity has been
  deprioritized as a design constraint -- AI-assisted refactoring makes
  extraction easy -- but clear domain logic remains required.)
- Parameter naming splits between semantic (`height_m`, `smoking_status`)
  and CCHS-coded (`SMK_005`, `SMKDSTY_cat5`). Pick one convention for v4.
- The skill documentation shows step 2 keeping values tagged and step 3
  converting format; several implementations instead pass `output_format`
  into `get_priority_missing()` in step 2. Both work when input and output
  missing-code families match, but the documented form is safer when they
  differ. The repair used the documented form.

## The integration layer

The strongest evidence for the modular refactor is at the boundary between
`rec_with_table()` and the derived-variable functions.

- **Row-at-a-time dispatch.** `DerivedVar::` rows are dispatched through
  `rowwise() %>% do(...)` and `do.call(get(fn), row_values)`
  (`R/recode-with-table.R` ~lines 937--983). Every vectorized function is
  called once per row -- about 100,000 scalar calls per CCHS cycle -- and
  `rowwise()`/`do()` are deprecated dplyr idioms.
- **Positional, unnamed arguments.** `unname(row_values)` means worksheet
  `DerivedVar::[...]` order must exactly match each function's parameter
  order. A drift produces wrong numbers silently. `output_format` can never
  be passed.
- **Tagged NAs do not survive categorical coercion.** Derived categorical
  results pass through `as.factor(unlist(...))`, collapsing
  `tagged_na("a")` vs `tagged_na("b")` to plain `NA`. Directly recoded
  categoricals instead carry the string `"NA(b)"` as a factor level (from
  `recode_variable_NA_formating()`). One output dataframe can therefore
  hold three missing-data representations depending on how each column was
  produced.

## Duplication map

Five concerns are implemented once in the legacy engine and once in the v3
stack. In each case the v3 implementation is the stronger one.

| Concern | Legacy engine | v3 stack | Risk |
|---|---|---|---|
| Missing code to NA | `recode_variable_NA_formating()` (hardcoded `NA::` parsing; strings for categoricals) | `convert_input_to_tagged_na()` + pattern cache | High -- output semantics differ today |
| Range parsing `[18.5,25)` | `compare_value_based_on_interval()` | `parse_range_notation()` (typed, open/closed/inf) | High -- legacy mishandles continuous ranges |
| Worksheet row access | `recode_columns()` reads the dataframe directly | `get_variable_details()` + session cache | Moderate |
| Output format | Hardcoded by variable type | Explicit `output_format` parameter | Moderate |
| `else`/`copy` logic | Inline in the recode loop | `apply_else_logic()` with copy/value/else mappings | Moderate |

## Supporting functions: engine API candidates

Classification of the v3 supporting stack for the modular refactor.

**Promote to the public harmonization-engine API:**

- `get_variables()`, `get_variable_details()` -- metadata access (level 3);
  exported, tidyselect-aware
- `get_missing_pattern()`, `get_complete_pattern()` and the cache functions
  -- pattern layer (level 4)
- `any_missing()`, `get_priority_missing()`, `assign_missing()` -- universal
  helpers (level 5)
- `clean_variables()` -- preprocessing and validation (level 6); the
  keystone
- `parse_range_notation()` -- currently internal; export it and have
  `compare_value_based_on_interval()` delegate to it
- `normalize_input_lengths()` -- added during the repair; scalar recycling
  plus explicit error on true length mismatch
- label utilities (`set_data_labels()` and friends) -- output layer

**Keep internal until rebuilt:** the recode pipeline internals
(`recode_columns()`, `get_data_variable_name()`,
`recode_variable_NA_formating()`).

**Dev tooling, not runtime:** `check_worksheet()`, `fix_worksheet()`,
`load_schema()`, and the YAML schemas under `inst/metadata/` (consulted only
by worksheet QA, not by harmonization). `variable-discovery.R` assumes a
repository working directory and breaks in an installed package; mark it
dev-only or fix the paths.

**Known weaknesses to carry into the v4 spec:**

- The session caches have no invalidation when worksheets change mid-session
  and are process-local (fresh per parallel worker).
- `any_missing()`/`get_priority_missing()` guess the variable name from the
  call stack (`extract_variable_name()`); in practice the guess fails and a
  BMI fallback pattern is used. Harmless after step-1 tagging, but fragile.
- The level-5 helpers loop element-wise in interpreted R; vectorize during
  the engine refactor.
- Warning hygiene: pattern and database-selection warnings are cached per
  session, but several first-call warnings remain noisy for new users.

## Case study: the ADL/alcohol repair (June 2026)

**What happened.** adl.R and alcohol.R on the v3 branch called helpers
(`generate_tagged_na_conditions()`, `clean_categorical_variables()`, and a
different-signature `clean_variables()`) defined in
`R/missing-data-helpers.R` -- a file that exists on the `adl-additions` and
`alcohol` feature branches but never reached v3. A
`tryCatch(source(...), error = function(e) {})` header hid the missing file
at load time, so the package loaded cleanly and six exported functions
failed only when called. test-adl.R had 24 errors, test-alcohol.R had 12,
and 12 worksheet rows routed `rec_with_table()` through the broken
functions.

**The repair** rewrote both files onto the canonical 3-step stack (function
names and parameter order unchanged for worksheet compatibility), replaced
the `min_*`/`max_*`/`log_level` parameter style with
`output_format`, and removed the `library()`/`source()` headers. Test
counts after repair: test-adl.R 98 passing, test-alcohol.R 51 passing.

**Infrastructure changes made during the repair:**

- `clean_variables()` now maps recognized CCHS label strings ("Not
  applicable", "Don't know", "NA(b)", ...) to tagged NAs during
  character/factor coercion, so the a/b distinction survives factor
  round-trips from `rec_with_table()`.
- `apply_else_logic()` was vectorized (specs parsed once per variable, not
  once per element).
- Categorical variables now have an enumerable valid set: numeric `recEnd`
  codes of `typeEnd == "cat"` rows join `copy_mappings` as the valid input
  set, so out-of-range values receive the worksheet's else rule at step 1.
- One repeated warning (database configuration fallback) is now emitted
  once per session.
- `normalize_input_lengths()` recycles scalars and errors on true length
  mismatches instead of silently returning all-NA results.

**The lesson for v4.** The first attempt at the valid-set change used
`recStart` domains and broke pack-years: `DHHGAGE_cont`'s worksheet rows map
category codes 1--5 to midpoint ages, so its `recStart` domain is in the
source coding while cleaned data is in the target coding. The fix keyed on
`typeEnd`: only categorical targets enumerate a valid set. The general
point: worksheet rows describe transformations between two coordinate
systems, and any metadata-driven validation must state which system it
applies to. The v4 schema design (NA patterns and valid ranges as declared
metadata) should make the source-domain/target-domain distinction explicit
rather than inferred.

## Recommended v4 sequence

Ordered by payoff against risk; each step is verifiable by golden-output
comparison against the current engine on the bundled sample data, because
the worksheet contract does not change.

1. **Vectorize the dispatch.** Replace `rowwise()`/`do()` with one
   named-argument `do.call(fn, as.list(columns))` per derived variable;
   pass `output_format` through. Small diff, large win.
2. **Unify range parsing** -- one parser, both callers.
3. **Route legacy NA formatting through the pattern cache**, making
   missing-data representation consistent package-wide. The `"NA(b)"`
   factor-level behaviour needs an explicit compatibility decision.
4. **Adopt `apply_else_logic()` and cached worksheet access in
   `recode_columns()`.**
5. **Decompose `rec_with_table()`** into database-detection, recode,
   derive, and label stages built on the public primitives -- at which
   point it is a thin orchestrator.

In parallel, domain-by-domain `if_else2()` retirement can proceed as CEP
work, using the repaired adl.R/alcohol.R as templates.

## Appendix: recovered design documents

The deleted support files from the big-bang lineage are recoverable from
git history (commits `38202456^`, `91cd84d0^`, `0bf0337e^`, `52e0a133^`).
Key items: `flexible-missing-data-specs.md` (the formal specification,
v3.1), `LESSONS_LEARNED.md` (the four architectural phases),
`OPINIONATED-MISSING-DECISION.md` (mixed missing types in original format),
`simplified-derived-variable-target.md` (the distilled 3-step target),
`DERIVED-VARIABLE-WORKFLOWS.md` (the level-7 categorization design), and
the architecture analyses in `archive/`. The surviving simplified reference
documents live on branch `3-step-tidyverse` under
`development/flexible-missing-data-mvp/`.
