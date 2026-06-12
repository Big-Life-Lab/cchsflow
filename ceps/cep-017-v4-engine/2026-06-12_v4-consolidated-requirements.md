# v4 Consolidated Requirements

**Date:** June 12, 2026
**Status:** Draft for team review -- supersedes the scope outline's open
questions where a verdict is stated; the engine track is detailed in
[2026-06-11_engine-comparison-and-reunification.md](2026-06-11_engine-comparison-and-reunification.md)
**Sources:** the verified design-issues inventory (evidence/), the
ecosystem benchmark with skeptic passes (evidence/bench-*.md), recodeflow
PR #43 scoping (both the simplified set and Doug's preserved originals),
and the recovered 2025 cchsflow specifications.

## The balance principle

v4 must do two things at once: **fix deficits** (the verified inventory --
things that are wrong today) and **stage for the future** (the scoping
ambitions -- catalog, versioning, dictionaries, interoperability). The
track structure below makes the balance explicit: tracks 1-3 are
deficit-driven, tracks 5-7 are staging, track 4 straddles. Each
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
| index/scope framing | Current; the three user complaints (logging, Divio docs, complex API) map to Tracks 6, docs work, and Track 1 respectively | -- |

## Track 1 -- Engine core (deficit; v4)

Per the reunification recommendation (option B): consolidate in
recodeflow v1.0.0; cchsflow v4 consumes. The three shared-defect fixes
land once: single NA representation at the boundary, vectorized
named-argument derived-variable dispatch (plus the engine-level warning
when a derived function returns untagged NA, from derived-variables.qmd),
and a validated recStart/recEnd grammar. Absorb recodeflow's existing
advances (template variables, feeder overrides, constants-as-feeders,
`parse_variables_sheet()`, `get_start_variables()`). API shrinks from 125
exports to a deliberate tier.

## Track 2 -- Worksheet schema and data integrity (deficit; starts in v3.x)

The databaseStart problem, verified end to end: malformed tokens ship
today (DEN_132 carries `cchs2007_2008p` / `cchs2007_2008m`, missing the
underscore -- silent dead rows); the engine matches databases by
unanchored regex substring (`cchs2015` matches `cchs2015_2016_p`); the
same column name carries different semantics in the two worksheets
(coverage summary vs per-row filter); and the default database name comes
from `deparse(substitute(data))`, silently producing garbage for
non-symbol arguments.

Requirements:

1. **v3.x:** fix the DEN_132 tokens; add a database-token registry (an
   enumerated list in `inst/metadata/`) and a `check_worksheet()` rule
   validating every databaseStart token against it. Fails fast on the
   next typo.
2. **v3.x:** tokenized exact matching in the engine (`strsplit` + `%in%`)
   replacing the unanchored `grepl()`; validate the requested
   database_name against the registry and error with candidates.
3. **v4:** formal column schema following Frictionless Table Schema
   (`datapackage.json`: types, required columns, enums for
   typeEnd/typeStart/status, the foreign key variable_details.variable ->
   variables.variable, missingValues declaration), imitating LinkML
   design patterns (explicit enumerations, per-column purpose/range/
   cardinality in schema YAML). This replaces the column-order-only YAML
   and fixes its current defects (phantom templateVariable entry,
   inconsistent nesting between the two files).
4. **v4:** resolve the dual-semantics columns (rename variables.csv
   databaseStart -> databaseCoverage or equivalent); define the
   explicit rowRole distinction for DerivedVar label-definition rows.
5. **v3.x:** `data-raw/rebuild-worksheets.R` with the explicit RData
   projection and a CI assertion (the Rafidul rebuild episode is the
   case in point).

## Track 3 -- Missing-data semantics (deficit; v4 core)

1. **One representation at the engine boundary:** haven_labelled numeric
   codes + `tagged_na()` for missing; `as_factor()` as explicit opt-in;
   `"NA(x)"` strings only as a deprecated compatibility shim. (Benchmark
   verdict: keep tagged_na primary -- ecosystem-standard,
   Stata/SAS-export-safe, already woven through v3.)
2. **Replace the lossy "original" output format** with optional
   `labelled_spss` output (codes preserved *with* their missingness
   declaration), via `labelled::tagged_na_to_user_na()` -- a strict
   upgrade on returning bare 996s. The `declared` package was evaluated
   and rejected (experimental, two reverse imports, no vctrs
   integration; its correctness advantage is already covered).
3. **Implement the scoping's general `NA::<any lowercase>` contract**,
   removing the a/b hardcoding (~6 sites).
4. **Decide and ship the priority order** (the inverted-YAML finding):
   one decision, documented epidemiological rationale, regression test.
5. **Schema-declared patterns with explicit domains:** NA patterns and
   valid ranges declared in the Track-2 schema, stating source-domain vs
   target-domain (the DHHGAGE_cont lesson); cache keyed by worksheet
   fingerprint (fixes invalidation and the parallel-worker gap).

## Track 4 -- Labels and object attributes (deficit + staging; v4)

What "object attributes like other libraries" concretely means:

1. **Adopt the labelled/haven attribute conventions wholesale:** value
   labels as named numeric vectors on haven_labelled vectors, variable
   labels via `labelled::var_label()` -- replacing the bespoke
   sjlabelled-on-factor form (character-coded labels, custom
   labels_long/label_long/unit attributes) that no other package reads.
2. **Phase out sjlabelled for labelled** (active, 41 reverse imports,
   vctrs-integrated) -- also removes one Depends entry.
3. **Labels survive transforms** (bind_rows, subsetting) per the labels
   scoping -- haven_labelled + vctrs gives this largely for free, which
   is the point of adopting the ecosystem class instead of bespoke
   attributes.
4. **Harmonized labels come from variables.csv** (one row per variable),
   ending the per-row variableStartLabel conflation that crashes
   `set_data_labels()` today; variableStartLabel becomes provenance
   metadata.
5. **`labelled::look_for()`** wired into the variable-discovery module
   (CEP-015) as the Stata-lookfor analogue.
6. **v3.x bug fixes** independent of all the above: tibble `[[`
   indexing; the heterogeneous-label hard-stop downgraded to a warning.

## Track 5 -- Versioning (staging; design in v4, enforce by v5)

The versioning scoping survives intact; requirements:

1. Three decoupled levels: package version, worksheet version, variable
   version -- worksheet and variable versions carried in the worksheets
   themselves (version/lastUpdated columns already exist; today they are
   unvalidated free text -- the Track-2 schema gives them types and
   rules).
2. An explicit breaking-change policy per level (Hyrum's law), published
   in CONTRIBUTING.
3. **Version stamping on outputs:** recoded dataframes carry package +
   worksheet versions as attributes (connects to the Track-7 catalog;
   this is the scoping's "recodeflow functions extract the version
   identifier and attach it to the recoded dataset").
4. v3.0.0 itself follows the policy: shims (option B), migration table
   in NEWS.

## Track 6 -- Logging and conditions (deficit + staging; v4)

The benchmark turned the logging scoping into a concrete design:

1. **rlang classed conditions + cli formatting**: every engine condition
   gets a class (`cchsflow_missing_variable`, `cchsflow_db_ambiguity`,
   ...) carrying variable/database/row metadata -- machine-catchable,
   which is the scoping's "structured log objects".
2. **`.frequency = "once"`** replaces the three hand-rolled
   warning-cache environments.
3. **Package-level verbosity** (option/env var; levels none/inform/
   debug per rOpenSci) replacing the dead log=/notes= parameters.
4. **End-of-run summary** via a withCallingHandlers collector around the
   recode loop: n recoded, n skipped, missing-data counts per variable
   -- the scoping's summary requirement.
5. **v3.x:** the two functionally buggy stop() messages (vectorized
   paste, undefined `row`) are plain bug fixes that need not wait.

## Track 7 -- Catalog and data dictionaries (staging; v4 foundation, v5 polish)

1. **Implement the catalog object** per Doug's preserved catalog.qmd:
   DCAT/Dublin Core-aligned fields, stored as catalog.csv sidecar,
   `set_catalog()`/`get_catalog()`/print/summary; recodeflow's
   pbc_metadata prototype is the seed. JSON-LD export deferred to v5.
2. **DDI Codebook as the dictionary export target:** `export_ddi()` via
   DDIwR mapping worksheets to DDI XML (variable -> var, catLabel rows ->
   catgry, NA:: rows -> missing="Y", units -> varFormat). IPUMS is the
   existence proof that DDI + harmonization scale together.
3. **Living data dictionary**: imitate pointblank's informant concept
   (computed stats embedded in column docs, YAML-stored, regenerated on
   demand) and validate's rules-as-objects pattern for step-3 output
   specs -- imitate, not adopt (GPL-3 caution; both verdicts in
   evidence/bench-dictionaries-validation.md).
4. Croissant: ignore. codebook/datadictionary/codebookr: ignore (one
   borrowable concept each at most).

## Staging summary

| When | What |
|---|---|
| v3.x (no engine dependency) | databaseStart token fix + registry check; tokenized database matching; data-raw pipeline; tibble fix; label hard-stop downgrade; the two buggy stop() messages; #138 pmax guard; #139 immigration arms; priority-order decision + YAML |
| v4 (the programme) | Engine consolidation in recodeflow (Track 1); schema redesign (Track 2); single NA representation + general NA::<type> (Track 3); labelled adoption (Track 4); versioning design + policy (Track 5); conditions/logging system (Track 6); catalog + export_ddi foundation (Track 7) |
| v5+ | recodeflow/cchsflow split polish; DCAT JSON-LD export; LinkML toolchain proper; ontology (Tier 3 roadmap); living-dictionary automation |

The deficit/staging balance: roughly, Tracks 1-3 fix what the inventory
proved wrong; Tracks 5-7 build what the scoping promised; Track 4 does
both at once and is the best early demonstration that the two goals
reinforce rather than compete -- adopting the ecosystem's attribute
conventions simultaneously fixes today's label bugs and stages
tomorrow's dictionary and export features.
