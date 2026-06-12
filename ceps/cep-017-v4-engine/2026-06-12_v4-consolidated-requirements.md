# v4 Consolidated Requirements

**Date:** June 12, 2026 (revised same day after the six-lens adversarial
review panel; panel reports in `evidence/review-*.md`)
**Status:** Draft for team review -- supersedes the scope outline's open
questions where a verdict is stated; the engine track is detailed in
[2026-06-11_engine-comparison-and-reunification.md](2026-06-11_engine-comparison-and-reunification.md)
**Supersession note:** this document and the reunification recommendation
retire the June 10 scope outline's non-goal "no cchsflow/recodeflow
package split" and replace the architecture review's five-phase
in-cchsflow engine sequence. What changed: the engine comparison showed
recodeflow dev is already ahead on the engine core, making consolidation
there cheaper than duplication. The June 10 documents remain as history.

## The balance principle

v4 must do two things at once: **fix deficits** (the verified inventory --
things that are wrong today) and **stage for the future** (the scoping
ambitions -- catalog, versioning, dictionaries, interoperability). Tracks
1-3 are deficit-driven, tracks 5-8 are staging, track 4 straddles. Each
requirement is tagged v3.x (can land before or alongside the release
train, no engine dependency), v4 (the main programme), or v5+ (deferred).

## Disposition of the recodeflow scoping (direct answer to "off mark?")

None of the scoping areas were off the mark. They were not yet
synthesized because the engine question was decision-blocking. Verdicts:

| Scope doc | Verdict | Lands in |
|---|---|---|
| missing-data.qmd | Current; v3 under-implements it (`NA::<any lowercase>` vs the shipped a/b-only) | Track 3 |
| labels.qmd (esp. Doug's 524-line original) | Current and ahead of the implementation; the labels layer is the dimension furthest from its own scoping | Track 4 |
| versioning.qmd | The strongest document of the set; nothing stale | Track 5 |
| logging.qmd (+ logging-refactor original) | Direction current; detail now superseded by the benchmark's concrete cli/rlang design | Track 6 |
| metadata.qmd + catalog.qmd (dropped from the simplified set) | Current; recodeflow already prototyped Dublin Core metadata (`pbc_metadata`) | Track 7 |
| derived-variables.qmd | Mostly settled by v3 practice (vectorized inputs; metadata-driven bounds superseded the min/max-parameter line); the untagged-NA warning requirement is unbuilt and lands in Track 1 | Tracks 1, 3 |
| out-of-scope.qmd | Still correct (no splines/dummies/roles) | unchanged |
| index/scope framing | Current; the three user complaints (logging, Divio docs, complex API) map to Tracks 6, 8, and 1 | -- |

## Governance and release sequencing (added on panel review)

The programme's critical path runs through recodeflow, which today is at
**v0.1.2 with no defined v1.0.0** and with its own undeclared CRAN
dependencies (checkmate, purrr, glue used via `::` but absent from
Imports; sjlabelled in Depends). Requirements:

1. **Define recodeflow v1.0.0 minimally:** the three shared-defect fixes
   (NA representation, vectorized named dispatch, recStart/recEnd
   grammar) plus what is already merged on dev. Logging, versioning,
   catalog, and any generic missing-data layer are **v1.1+ and do not
   gate** cchsflow v4's dependency.
2. **recodeflow CRAN pre-flight is an explicit milestone** with its own
   checklist: declare checkmate/purrr/glue in Imports, move sjlabelled
   out of Depends, audit the do/rowwise importFrom entries, review
   integration-test data for CRAN suitability, 0 errors/warnings on
   R CMD check. No cchsflow v4 integration work starts against an
   unpublishable engine.
3. **Authorship is assigned, not assumed:** engine-defect fixes and
   v1.0.0 scope are Yulric's lane; cchsflow-side adaptation PRs come from
   the cchsflow team with Yulric reviewing. (Team to confirm.)
4. **The L3-6 "generic migration" is deferred to recodeflow v1.1.**
   cchsflow v4 keeps its missing-data layer, refactored behind a clean
   internal API boundary so the later migration is an extraction, not a
   rewrite. (Panel verdict: the generic form is L-effort with a
   configuration-interface design problem; do not put it on the v4
   critical path.)

## Track 0 -- Testing and CI pre-conditions (deficit; before Track 1 starts)

Added on panel review; the testing-ci sweep found three high-severity
gaps that make refactoring unsafe today:

1. The engine's golden tests stand at **zero passing** (the frozen
   worksheet references function names that moved to R/legacy/), and no
   regeneration script exists. Requirement: a fixture-generator script,
   a regenerated committed baseline, and golden-output comparison wired
   into CI **before** any engine transformation begins.
2. The v3 lineage has had **no R CMD check or test-suite CI for a year**
   (it diverged one day before R-CMD-check.yaml landed on main).
   Requirement: restore R-CMD-check + testthat CI on working branches
   now (v3.x).
3. `check_worksheet()` **crashes on variable_details.csv** (schema YAML
   nesting mismatch vs `load_schema()`; adjudicated by direct execution
   June 12 -- note this overturns one verification verdict that had
   refuted the claim), so the branch's only CI gate errors rather than
   diagnoses. Requirement: flatten the YAML (or fix the loader) -- S
   effort, v3.x.
4. covr coverage baseline recorded in CEP-017; synthetic fixtures
   covering 2019-2023 PUMF- and Master-shaped frames for every supported
   era (connects to the data-artifacts findings).

## Track 1 -- Engine core (deficit; v4)

Per the reunification recommendation: consolidate in recodeflow v1.0.0
(as minimally defined above); cchsflow v4 consumes. The three
shared-defect fixes land once -- noting that **vectorized named-argument
dispatch is net-new work for both repos** (recodeflow's loop refactor
removed `do()` but still dispatches one scalar call per row) -- plus the
engine-level warning when a derived function returns untagged NA (from
derived-variables.qmd). Absorb recodeflow's existing advances (template
variables, feeder overrides, constants-as-feeders,
`parse_variables_sheet()`, `get_start_variables()`).

Also in this track:

- **API tier table is a deliverable, not a slogan:** enumerate the
  retained first-class exports, the deprecated shims (with lifecycle
  stage and removal target), and the removals -- before any code moves.
- **Parameter-naming convention decided here** (scope outline Q5):
  semantic vs CCHS-coded names for DV functions, one rule, documented.
- **Worksheet column-convention alignment** between the repos (the
  pkg.env/pkg.globals label-mapping divergence) named as an explicit
  migration item with a decided winner.
- This track supersedes the five-phase in-cchsflow sequence in the June
  10 architecture review; those transformations happen in recodeflow.

**Sequencing constraint (panel):** Track 3 and Track 4 engine-boundary
changes happen **after** Track 1 consolidation -- they touch the same
code path (recode loop -> NA formatting -> label attachment). Doing them
in cchsflow first means doing them twice. Only the v3.x bug subsets are
independent.

## Track 2 -- Worksheet schema and data integrity (deficit; starts in v3.x)

The databaseStart problem, verified end to end: malformed tokens ship
today (DEN_132 carries `cchs2007_2008p` / `cchs2007_2008m`); the engine
matches databases by unanchored regex substring; the same column name
carries different semantics in the two worksheets; the default database
name comes from `deparse(substitute(data))`.

1. **v3.x:** fix the DEN_132 tokens; database-token registry in
   `inst/metadata/` + a `check_worksheet()` rule validating every
   databaseStart token against it.
2. **v3.x:** tokenized exact matching in the engine (`strsplit` + `%in%`)
   replacing unanchored `grepl()`; validate requested database_name
   against the registry, error with candidates.
3. **v4:** formal column schema following Frictionless Table Schema
   (types, required columns, enums for typeEnd/typeStart/status, the
   variable_details -> variables foreign key, missingValues), imitating
   LinkML design patterns. Replaces the column-order-only YAML and its
   current defects.
4. **v4:** resolve the dual-semantics columns (variables.csv
   databaseStart -> databaseCoverage or equivalent); explicit rowRole
   for DerivedVar label-definition rows.
5. **v3.x:** `data-raw/rebuild-worksheets.R` with the explicit RData
   projection and a CI assertion.

## Track 3 -- Missing-data semantics (deficit; v4 core, after Track 1)

1. **One representation at the engine boundary:** haven_labelled numeric
   codes + `tagged_na()`; `as_factor()` as explicit opt-in (an
   `output_format = "factor"` option is retained for one major version
   as the compatibility path); `"NA(x)"` strings as a deprecated shim.
2. **`output_format = "original"` becomes a deprecated alias** for the
   new `labelled_spss` output (codes preserved *with* missingness
   declaration via `labelled::tagged_na_to_user_na()`); `"tagged_na"`
   remains the default. `declared` was evaluated and rejected
   (experimental, two reverse imports, no vctrs integration).
3. **General `NA::<any lowercase>` contract**, restated effort: ~6
   engine/config sites (S) plus ~30 DV modules whose case_when logic
   hardcodes "a"/"b" (M, mechanical).
4. **Priority order: the decision and the YAML ship in v3.x** (one
   decision, documented epidemiological rationale, regression test; the
   NEWS entry must characterize this as a methodological-output change,
   not a bug fix). v4 adds schema-declared enforcement.
5. **Schema-declared patterns with explicit source-vs-target domains**
   (the DHHGAGE_cont lesson); cache keyed by worksheet fingerprint.
6. **if_else2() retirement homed here** (scope outline goal 1): CEP-by-
   CEP migration of the remaining ~15 legacy-style domains onto the
   3-step pattern, using the repaired adl.R/alcohol.R as templates.
   Spans v3.x-v4 as capacity allows.
7. **Level-7 disposition** (recovered 2025 specs): the generic
   continuous-to-categorical conversion is absorbed into Track 1's
   engine boundary as the categorization stage; per-domain
   `categorize_*()` functions remain the interim pattern.

## Track 4 -- Labels and object attributes (deficit + staging; v4, after Track 1)

**Panel flag: the highest-effort v4 track.** Sub-deliverables:

(a) **v3.x bug fixes [S]:** tibble `[[` indexing; heterogeneous-label
hard-stop downgraded to warning.
(b) **sjlabelled -> labelled migration [L]:** value labels as named
numeric vectors on haven_labelled vectors, `labelled::var_label()` for
variable labels. The bespoke attributes (labels_long, label_long, unit)
are deprecated with a one-cycle warning period, their content served via
a metadata accessor.
(c) **Label source switch [M]:** harmonized labels from variables.csv
(one row per variable); variableStartLabel becomes per-row provenance.
(d) **Transform survival [M]:** labels survive bind_rows/subsetting --
largely free once (b) lands, which is the point of adopting the
ecosystem class.

Plus: `labelled::look_for()` wired into variable discovery (CEP-015).

## Track 5 -- Versioning and migration (staging; design in v4)

1. Three decoupled levels (package/worksheet/variable; the existing
   version/lastUpdated columns gain types and rules via the Track-2
   schema).
2. Explicit breaking-change policy per level (Hyrum's law), in
   CONTRIBUTING.
3. Version stamping on recoded outputs as attributes (connects to the
   Track-7 catalog).
4. **Migration requirements (expanded on panel review):** enumerate
   which v2 `*_fun` exports get shims vs removal; lifecycle package in
   Imports with badges on every legacy export; a **user-code migration
   table** keyed to the four breaking patterns -- scripts testing
   `== "NA(b)"`, scripts relying on `is.na()` returning FALSE for
   categorical missing, downstream packages consuming the bespoke label
   attributes, and SPSS/Stata export workflows -- each with before/after
   recipes in a migration vignette.
5. **Managed-environment commitment** (ICES/RDC/data-centre users):
   recodeflow's CRAN publication precedes any cchsflow v4 release;
   cchsflow + recodeflow are requested together for environment imports;
   DV functions remain copy-paste runnable without the engine (their
   only hard dependency being haven/dplyr, as today).

## Track 6 -- Logging and conditions (deficit + staging; v4)

1. rlang classed conditions + cli formatting; every engine condition
   classed (`cchsflow_missing_variable`, ...) with variable/database/row
   metadata.
2. `.frequency = "once"` replaces the three hand-rolled warning caches.
3. Package-level verbosity (none/inform/debug per rOpenSci) replacing
   the dead log=/notes= parameters.
4. End-of-run summary via a condition collector around the recode loop.
5. **v3.x:** the two functionally buggy stop() messages (vectorized
   paste; undefined `row`) are plain bug fixes now.

## Track 7 -- Catalog and dictionaries (staging; v4 foundation)

1. **Catalog object** per the preserved catalog.qmd: DCAT/Dublin Core
   fields, catalog.csv sidecar, set_catalog()/get_catalog()/print/
   summary; recodeflow's pbc_metadata is the seed. JSON-LD export v5.
2. **`export_ddi()` moves to v5** (panel feasibility verdict); the v4
   foundation is the catalog plus the worksheet-to-dictionary data
   model.
3. Living-dictionary and rules-as-objects concepts imitated from
   pointblank/validate (GPL-3 caution documented); Croissant ignored.

## Track 8 -- Documentation (staging; v3.x start, v4 completion)

Added on panel review; the docs-vignettes sweep has five high-severity
findings with no previous home:

1. **v3.x:** regenerate the pkgdown reference index (25 deleted topics
   listed, ~60 new exports missing -- the site cannot build); fix
   get_started.Rmd and the other vignettes still teaching deleted v2
   functions; `pkgdown::check_pkgdown()` in CI.
2. **v4:** Divio restructure (tutorials / how-to / reference /
   explanation) -- one of the three named user complaints in the
   recodeflow scoping; the worksheet-authoring how-to absorbs the
   undocumented conventions catalogued by the worksheet-schema sweep.
3. **v4:** NEWS.md backfilled (2.1.0 gap) and maintained under the
   Track-5 policy; migration vignette (Track 5 item 4) lives here.

## Staging summary

| When | What |
|---|---|
| v3.x (no engine dependency) | Track 0 items 2-3 (CI restoration; check_worksheet crash fix); databaseStart token fix + registry check; tokenized database matching; data-raw pipeline; tibble fix; label hard-stop downgrade; the two buggy stop() messages; #138 pmax guard; #139 immigration arms; priority-order decision + YAML (with methodological NEWS note); glue/stats/withr declarations; rec_with_table roxygen default fixes; pkgdown regeneration + vignette sweep. List-mode loop bug: **already fixed** (63450ba3); ports to recodeflow with Track 1. |
| Gate | **recodeflow v1.0.0 CRAN-ready** (minimal scope + pre-flight checklist above) |
| v4 (the programme) | Track 0 item 1 + 4 (golden baseline, fixtures) then Track 1 (engine consolidation, API tiers, naming decision); Track 2 schema redesign; Track 3 NA semantics (after Track 1); Track 4 labels (after Track 1; highest effort); Track 5 versioning + migration vignette; Track 6 conditions/logging; Track 7 catalog; Track 8 Divio |
| v5+ | export_ddi(); DCAT JSON-LD; LinkML toolchain proper; ontology (Tier 3); living-dictionary automation; recodeflow v1.1 generic missing-data layer |

The deficit/staging balance: Tracks 0-3 fix what the inventory proved
wrong; Tracks 5-8 build what the scoping promised; Track 4 does both at
once -- adopting the ecosystem's attribute conventions simultaneously
fixes today's label bugs and stages tomorrow's dictionary and export
features.
