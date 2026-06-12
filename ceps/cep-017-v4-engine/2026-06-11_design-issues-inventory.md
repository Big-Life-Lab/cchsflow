# Design Issues Inventory

**Date:** June 11, 2026
**Status:** Draft -- six of ten planned sweep dimensions complete (see
"Coverage and verification status" below)
**Method:** Multi-agent evidence sweep of the cchsflow working tree (v3 +
the ADL/alcohol repair), each finding cited to file:line and, where noted,
reproduced live. Full machine-readable findings in `evidence/*.json`;
high-severity details with evidence and fixes in
`evidence/high-severity-details.txt`.

This is the "design issues that never made a lot of sense" inventory
requested for v4 scoping: 78 confirmed-style findings across six
dimensions, ranked by severity, each with a candidate fix and effort
estimate (S/M/L).

## Headline findings

The ten most consequential, across dimensions:

1. **List-mode recoding silently corrupts data.** When `rec_with_table()`
   receives a list of dataframes, the loop passes the full database vector
   instead of the loop variable, so the first database's rules are applied
   to every database (`R/recode-with-table.R:188-203`; reproduced live with
   distinct per-database rules). One-line fix; the worst defect found.
2. **The interval parser silently mangles set syntax.** `recStart`
   `"[7,8,9]"` is parsed as interval [7,8] with the third value dropped to
   the else rule (`recode-with-table.R:571-601`; reproduced). The engine
   has no recStart grammar validation at all.
3. **Three NA representations in one output dataframe.** Recoded
   continuous: `tagged_na`. Recoded categorical: literal factor level
   `"NA(b)"` -- `is.na()` returns FALSE on missing observations, and
   `haven::write_sav()` errors on export. Derived categorical:
   plain `NA` after `as.factor()` destroys tags. Missing-data semantics is
   the package's core value proposition, and what a missing value looks
   like depends on which internal code path produced the column.
4. **The shipped missing-data priority is the inverse of the documented
   one.** `missing_priority_rules.yaml` was authored on the
   `3-step-tidyverse` branch (commit `370b673b`, "Not Stated wins") but
   never merged; the built-in fallback gives "Not Applicable wins"
   (`R/missing-data-functions.R:194-198`). Two contradictory priorities
   are documented in different places in the codebase. This is an open
   methodological decision, not just a bug -- see "Design decisions
   raised" below.
5. **`rec_with_table()` fails on any tibble input.** `label_data()` uses
   single-bracket indexing nine times (`R/label-utils.R:184-211`);
   tibbles -- what `haven::read_sav()` and `readr::read_csv()` return --
   error with "'list' object cannot be coerced". Open issue #159
   misdiagnoses this as a haven_labelled problem. Every modern import path
   hits it.
6. **14 per cent of the public API is fake.** Eighteen exported
   `calculate_*` functions have `stop("DOCUMENTATION ONLY...")` as their
   entire body -- errors used as a documentation channel for
   worksheet-routed variables.
7. **No Imports field exists.** All nine runtime dependencies sit in
   Depends, attaching ~996 symbols to the user's search path and masking
   `stats::filter`, `haven::read_sas` and others. `glue` and `stats` are
   used but undeclared (CRAN blocker); `cli` is a hard dependency never
   called from package code.
8. **The metadata-driven Level 4-5 integration is largely dead code in
   practice.** `extract_variable_name()` fails for 100 per cent of
   in-package call sites and silently falls back to `HWTGBMI_der` (whose
   pattern is empty); the Level 2A database-config loader was never
   migrated, so exported `get_missing_pattern_auto()` always errors. The
   3-step functions work because inputs are pre-cleaned, not because the
   advertised metadata integration operates.
9. **Three competing runtime sources of worksheet metadata.** Lazy
   `data/*.RData` (a 16-column projection), `inst/extdata/*.csv` (23
   columns), and a runtime-written RData cache inside the installed
   package directory (a CRAN policy violation). No `data-raw/` rebuild
   pipeline exists; the projection is tribal knowledge.
10. **844MB of git history**, ~420MB of which is 431 committed versions of
    `variable_details.csv` (Excel-induced whole-file churn), plus 51MB of
    once-committed DDI XML. Clone and CI cost grow monotonically.

## Fast fixes that should not wait for v4

These are S-effort, independent of the refactor, and worth folding into
the v3 release window:

| Fix | Evidence | Effort |
|---|---|---|
| List-mode loop bug: pass `data_name`, not `database_name` | recode-with-table.R:188-203 | S |
| Declare `glue` + `stats` in Imports; `withr` in Suggests | check-worksheet.R:274ff; alcohol.R:93 | S |
| `rec_with_table()` roxygen documents wrong defaults for `append_to_data` and `notes` | recode-with-table.R:113-118 vs 160-162 | S |
| Tibble support: `[[` instead of `[ , ]` in `label_data()` | label-utils.R:184-211; closes the real cause of #159 | S |
| Ship `missing_priority_rules.yaml` (after the priority decision below) | missing-data-functions.R:168-198 | S |
| `set_data_labels()` hard-stop on heterogeneous `variableStartLabel` -> warning + first label | label-utils.R:132-148; crashes on shipped worksheets | S |
| `data-raw/` rebuild script for the RData projection + CI assertion | no data-raw/ exists | S |
| pkgdown reference regeneration (25 deleted topics listed; ~60 new exports missing) | _pkgdown.yml:61-92 | S |

## Per-dimension summaries

### Engine internals (21 issues: 4 high, 11 medium, 6 low)

Beyond headline items 1-3: a plain dataframe is mis-routed into the list
branch when `ncol(data) == length(database_name)`; database matching is
unanchored substring `grepl()` (no `fixed=TRUE`); `log=`/`notes=` are
silently ignored for derived variables; `custom_function_path` is
`source()`d into the global environment despite a comment claiming
otherwise; the per-variable recode loop is quadratic (repeated dataframe
filtering per category); the roxygen documents sjmisc features the engine
never implemented; dead code includes a `map::` row split that is never
consumed. The derived-variable dispatch (deprecated `rowwise()+do()`,
unnamed positional `do.call`) was documented in the architecture review
and is confirmed here with the additional finding that nothing checks
feeder order against `formals()`.

### Missing-data architecture (9 issues: 5 high, 4 medium)

Headline items 4 and 8, plus: `NA::a`/`NA::b` are hardcoded across ~6
sites while the recodeflow scoping requires `NA::<any lowercase>`; no
cache invalidation exists when worksheets are edited mid-session (the
team's own development workflow); caches are process-local under parallel
workers; `clean_variables()` and `get_missing_config()` use two
contradictory fallback patterns for unknown variables; the Level 5
helpers run element-wise R loops (~0.3-0.6 s per call per 100k rows, ~14
calls per derived variable).

### Labels and metadata layer (11 issues: 4 high, 6 medium, 1 low)

The dimension furthest from its own scoping targets. Headline items 3 and
5, plus: value labels are attached in a nonstandard sjlabelled-on-factor
form (character-coded `labels` attribute, custom `labels_long`/`unit`
attributes) that neither `labelled` nor `haven` can consume -- metadata
cannot flow to gtsummary, codebook generators, or SPSS export;
`variables.csv`'s `label`/`labelLong` columns are dead (labels actually
come from per-row source-variable labels in variable_details, which is
also why `set_data_labels()` crashes on era-varying labels); labelling is
fused to recoding with hidden type coercion and no opt-out; labels do not
survive `bind_rows` or base subsetting.

### API surface and UX (15 issues: 7 high, 7 medium, 1 low)

Headline item 6, plus: three API generations coexist (43 `calculate_*`,
27 legacy `*_fun`, 6 functions named identically to CCHS variables) with
no deprecation markers and no shims for the deleted v2 names;
`rec_with_table()` has 11 parameters of which at least 4 are unused in the
package's own vignettes and tests (recodeflow's copy has grown to 15);
91 `stop()` calls are base-R, frequently unactionable, and in two places
functionally buggy (vectorized `paste()`, wrong variable in message);
`output_format` exists on 41 of 51 DV functions with drifting vocabulary;
near-collision names (`calculate_SMKG203_cont` vs `_continuous` vs
`_from_combined`).

### Dependencies and namespace (12 issues: 3 high, 6 medium, 3 low)

Headline item 7, plus: `pkg.globals` carries 84 string constants of which
~47-53 are dead bllflow leftovers; mutable state lives in five top-level
namespace environments with inconsistent construction (one without
`parent = emptyenv()`); `R/legacy/`, `R/docs/`, and `R/README.md` are
never-loaded content inside R/ that R CMD check flags; no
`globalVariables()` declaration for NSE column references.

### Data artifacts (10 issues: 4 high, 4 medium, 2 low)

Headline items 9 and 10, plus: shipped sample data covers none of the
2019-2023 cycles v3 newly harmonizes, so the headline feature cannot be
exercised end-to-end by users or CI; the 200-row samples have no committed
generation script, seed, or sampling method; three orphaned synthetic
share datasets carry copy-paste provenance errors; installed size is
dominated by a 3MB logo PDF and the worksheets shipped twice.

## Design decisions raised (for the team, not unilateral fixes)

1. **Missing-data priority order.** The unshipped YAML says Not Stated
   wins (refusal is more informative than structural non-applicability);
   the live fallback, the foundations doc, the OPINIONATED decision doc,
   and the repaired ADL/alcohol tests all encode Not Applicable wins.
   Decide once, document the epidemiological rationale, ship the YAML,
   and add a regression test. (If the answer is "Not Stated wins," the
   repair tests change too.)
2. **One NA representation at the engine boundary.** The evidence
   strongly favours haven semantics end-to-end: `haven_labelled` numeric
   codes + `tagged_na` for missing, `as_factor()` as an explicit opt-in,
   `"NA(x)"` strings retained only as a deprecated compatibility shim.
   This is the single decision that unblocks SPSS export, `is.na()`
   correctness, and the labels-ecosystem integration at once.
3. **Worksheet grammar formalization.** The engine accepts-and-mangles
   syntax (`[7,8,9]`) that authors believed valid. A formal recStart/
   recEnd grammar, validated in `check_worksheet()` so bad syntax never
   reaches the engine, is a prerequisite for the v4 metadata schema.
4. **API tiering.** Of 125 exports: ~10 engine API, 46 worksheet-referenced
   DV functions, the rest internal or fake. The v4 surface should be
   deliberate; every kept export is a compatibility promise.
5. **Repository hygiene.** Fresh repo vs `git filter-repo` for the 844MB
   history; either way, stop committing build products and adopt
   delta-friendly worksheet discipline.

## Open-issue triage (complete; verified June 11)

| Issue | Verdict |
|---|---|
| #184 R CMD INSTALL fails (stale NAMESPACE) | Resolved by the fix branch (NAMESPACE regenerated in d636573a; R CMD check now 0 errors). Close on merge. |
| #185 smoking Func::/feeders out of sync | Resolved by the fix branch (worksheet repair d636573a; all 70+ Func:: targets verified to resolve; ADL/alcohol feeder order verified). Close on merge. |
| #132 adl_score_5_fun counts 2s instead of 1s | Resolved in v3: rewritten `score_adl()` counts 1=needs help; tests assert direction. Close at release with migration note. |
| #138 negative pack years | **Still real -- correction to the June 10 triage.** Reproduced live: never-daily status 3, age 52, age-first-cigarette midpoint 55 returns -0.15; the duration term in `.calculate_pack_years_core()` has no pmax guard. Fix: `pmax(age - age_first_cig, 0)` plus the regression case. v3 blocker. |
| #139 immigration_der misclassification | **Still real, verified.** Non-immigrants born outside Canada fall through to NA::b; needs two case_when arms before the NA-propagation block. v3 blocker. |
| #159 haven_labelled input | Real bug, wrong diagnosis (verified): tibble single-bracket indexing in label_data(), not haven_labelled itself. v3 patch: `[[` indexing (optionally zap_labels at entry); v4: a typed input-normalization stage. |
| #173 NULL-handling standardization | Valid; standardize via a `prepare_inputs()` helper across DV functions. v3.x/v4. |
| #178 SDCGLHM/SDCDFOLS mappings | Valid worksheet gap (P2); needs Gem verification before rows are written. |
| #182, #175, #172, #131 | Resolved by v3 -- close on merge. |
| #135 switch to recodeflow utilities | Answered by the reunification recommendation (option B): yes, in v4. See [2026-06-11_engine-comparison-and-reunification.md](2026-06-11_engine-comparison-and-reunification.md). |
| #99 sedentary, #69 survey year, #57 frequency tool, #179 SDCD* successors | v4/backlog inputs (CEP candidates; #69 is worksheet-only, low effort). |

## Coverage and verification status (updated June 11, evening)

The evidence program is complete: all ten sweep dimensions, the
adversarial verification pass, the engine comparison, and the ecosystem
benchmark.

- **All ten dimensions on disk** (`evidence/inventory-*.json`), including
  the June 11 additions: worksheet-schema (19 issues, e.g. the
  rec_with_table docstring still documents the pre-rename column names
  toType/fromType/recFrom/recTo), testing-ci (root causes of the
  pre-existing test errors), docs-vignettes, and the full issues triage.
- **Adversarial verification: complete.** 41 verdicts
  (`evidence/verification-verdicts.json`): 38 findings confirmed real,
  3 refuted and removed from consideration (two worksheet-schema claims --
  the YAML-nesting crash and the DerivedVar-silently-ignored claim -- and
  the haven_labelled framing of #159). All 35 findings from the June 10
  sweep were confirmed. Severity rankings can now be treated as reviewed.
- **Engine comparison: complete** (`evidence/engines-*.md`), synthesized
  in [2026-06-11_engine-comparison-and-reunification.md](2026-06-11_engine-comparison-and-reunification.md).
- **Ecosystem benchmark: complete** (`evidence/bench-*.md` with skeptic
  verdicts in `evidence/skeptic-*.json`; skeptics confirmed the
  load-bearing claims, refuting only minor factual details). Synthesis
  into the consolidated v4 requirements is the remaining writing task.
