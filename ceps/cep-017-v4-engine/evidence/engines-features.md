# Feature Inventory: recodeflow vs cchsflow

**Date:** 2026-06-11
**Branch analyzed:** cchsflow fix/v3-smoking-worksheet-sync, recodeflow dev
**Task:** Capabilities each repo has that the other lacks

---

## 1. recodeflow capabilities cchsflow lacks

### 1.1 `parse_variables_sheet()` — validated variables-sheet object

`R/parse-variables-sheet.R` (exported). Returns a typed `variables_sheet`
S3 object after running structural validation:
- Checks required columns exist
- Validates that derived variables (`DerivedVar::`) reference only
  non-derived or other-derived variables, never raw database columns
Returns a structured error list (type + message + context fields) rather
than stopping, enabling programmatic error handling.

cchsflow has `check_worksheet()` which validates format/column order of
the *file on disk*; it does not validate the in-memory object or
cross-validate derived-variable inputs.

### 1.2 `get_start_variables()` — typed start-variable graph

`R/get-start-variables.R` (exported). Given a variable name + database,
returns a typed list of `{name, type}` records classifying each feeder as
`"database"`, `"non-derived"`, `"derived"`, or `"table"`. This makes the
dependency graph explicit and machine-traversable before any recoding
runs.

cchsflow has `get_feeder_vars()` (from recodeflow) but no function that
returns the typed classification.

### 1.3 `is_table_feeder_var()` / `get_table_name()` + `tables=` parameter

`R/variable-start.R` (both exported). The `$table:name` syntax in
`variableStart` allows a named data.frame to be passed as a feeder
argument to a derived function. `rec_with_table()` accepts `tables =
list()` and dispatches table feeders correctly.

cchsflow's `rec_with_table()` calls the recodeflow copy and therefore has
the parameter, but the `is_table_feeder_var()` / `get_table_name()` utilities
are not independently exported by cchsflow.

### 1.4 Template-variable expansion (`templateVariable` column)

`R/recode-with-table.R:337-` `expand_template_variables()`. When a row in
`variable_details` sets `templateVariable = "Yes"`, the engine treats that
row set as a reusable recode template. Other variables reference the
template with `templateVariable = <name>` and inherit its recode rules
without duplication. This reduces worksheet verbosity for variables that
share identical category structure (e.g., two survey items both using the
same language-code schema).

cchsflow has no template-variable mechanism.

### 1.5 Scalar and string constants as feeder arguments

`R/recode-with-table.R:1260-1277`. `is_start_var_numeric()` and
`is_start_var_string()` detect when a `variableStart` value is a literal
(e.g., `42` or `'English'`) rather than a column name. The literal is
passed directly as the named argument to the derived function. This lets
worksheets parameterize derived functions without adding a column.

Documented in the recodeflow changelog entry "new feature where the start
variable for a derived variable can be constants" (commit 7cb372f).

cchsflow DV functions receive constants via YAML (`smoking-validation-constants.R`
and `PACK_YEARS_CONSTANTS`), not via the worksheet engine.

### 1.6 `id_role_name` / `append_non_db_columns` parameters

`rec_with_table()` in recodeflow accepts:
- `id_role_name`: generates a composite ID column from role-defined
  feeder columns (via `create_id_row()`/`select_vars_by_role()`).
- `append_non_db_columns`: when TRUE, appends columns absent from this
  database as NA columns, enabling `bind_rows()` across cycles without
  downstream NA-filling.

cchsflow's local `rec_with_table()` does not expose these.

### 1.7 Dataset-level metadata (Dublin Core / `pbc_metadata` pattern)

`inst/extdata/pbc_metadata.yaml` + `R/data.R` ship a `pbc_metadata` list
following Dublin Core (title, creator, subject, description, publisher,
date, type, format, identifier, source, language, rights, references).
The scoping doc (`/tmp/recodeflow-pr43/metadata.qmd`) formalizes this as
the planned "catalog" object using DCAT/Dublin Core as interoperable
standard.

cchsflow has no dataset-level metadata structure.

### 1.8 Formal scoping documents (`scope-docs/`)

`scope-docs/dist/` contains rendered HTML scope documents for:
derived-variables, labels, logging, metadata, missing-data, versioning,
and out-of-scope. These are v2.0 scope specifications driving the next
recodeflow release.

Highlights from scoping that are recodeflow-only design targets:
- **Logging**: structured log objects with machine-readable metadata per
  event; verbosity levels; per-variable label in error messages.
- **Variable versioning**: decoupled package / worksheet / variable
  versions so users can detect which variables changed between releases.
- **Catalog object**: DCAT-aligned dataset metadata, data-dictionary
  generation, print/summary methods.
- **Label persistence**: labels must survive `bind_rows` and base
  subsetting; `haven_labelled` + `as_factor()` as explicit opt-in.

cchsflow has the CEP process, but the CEPs are domain-specific (smoking,
alcohol, etc.). No equivalent technology-scoping document set exists for
the engine layer.

### 1.9 YAML schema for `variable_details` / `variables` (branches)

`archive/variable_details.yaml`, `archive/variables.yaml`,
`archive/metadata_architecture_framework.md` (branch `variables-metadata`);
`scope-docs/yaml-schema-validation-plan.md` (branch `variables-metadata`).
A three-file metadata architecture with YAML schemas for both worksheets,
including a shared `metadata_registry` for cross-referencing
transformation patterns.

cchsflow has YAML *column-order* schemas in
`inst/metadata/schemas/core/variables.yaml` and `variable_details.yaml`,
but these only list `expected_column_order` + `id_column_name` — not a
full schema with types, constraints, or cross-file references.

### 1.10 `get_start_variables` utility (exported; graph traversal)

See 1.2. Also note the `scoping-doc-catalog` branch adds the `catalog`
object concept plus the response to Yulric's questions in `scope.qmd`.

### 1.11 Integration test harness with RData snapshot diffing

`tests/testthat/test-integration.R` uses a full HUIPoRT dataset
(`integration-assets/data/*.RData`), a custom function file, and RData
snapshot comparison (`expect_snapshot_file`) with a `compare_snapshot()`
helper. This is an end-to-end regression test, not just unit tests.

cchsflow has no integration test that runs `rec_with_table()` against
real multi-cycle data.

---

## 2. cchsflow capabilities recodeflow lacks

### 2.1 Three-step derived-variable architecture

`R/clean-variables.R`, `R/missing-data-functions.R`,
`R/missing-pattern-cache.R`. The 3-step pattern:
1. `clean_variables()` — converts raw missing codes to detectable format,
   supports `output_format = "tagged_na" | "original"`
2. `any_missing()` / `get_priority_missing()` in `case_when()` — unified
   missing detection with priority hierarchy
3. Output validation in `clean_variables()` or inline assertions

This is the core cchsflow v3 DV architecture. 830 + 492 + 1077 = ~2400
lines of infrastructure supporting it. recodeflow has no equivalent; its
DV dispatch is a `rowwise() + do()` positional call into the global
environment.

### 2.2 Worksheet check/fix infrastructure + YAML column-order schemas

`R/check-worksheet.R` (527 lines), `R/fix-worksheet.R` (160 lines),
`exec/check-worksheets.R`, `exec/fix-worksheets.R`.

Validates on disk:
- Column order against `inst/metadata/schemas/core/*.yaml`
- Row sort order by `id_column_name`
- Line endings (CR vs LF)
- Trailing empty columns
- Excessive quoting (minimal-quoting enforcement)

Fixes in place or to a new file with auto-repair of the above.

recodeflow `parse_variables_sheet()` validates column presence and
derived-variable structure, but has no file-level format checks, no CSV
quoting enforcement, no line-ending checks, no column-order enforcement.

### 2.3 419-variable CCHS-harmonized content

`inst/extdata/variables.csv` (~419 rows) + `inst/extdata/variable_details.csv`
covering 2001–2023 CCHS master and PUMF cycles across ~30 domains
(smoking, alcohol, BMI, ADL, physical activity, diet, immigration, chronic
conditions, education, demographics, active transport, etc.).

recodeflow ships only the PBC demo dataset (`pbc_variables.csv`, 24 rows;
`pbc_variable_details.csv`, 69 rows) plus the HUIPoRT integration-test
fixtures.

### 2.4 Domain-specific derived-variable functions (30+ modules)

`R/smoking.R`, `R/alcohol.R`, `R/bmi.R`, `R/adl.R`, `R/diet.R`,
`R/education.R`, `R/immigration.R`, `R/physical-activity.R`,
`R/respiratory-condition.R`, `R/depression.R`, `R/number-conditions.R`,
`R/social-provision.R`, `R/food-insecurity.R`, `R/life-satisfaction.R`,
`R/occupation.R`, `R/active-transportation.R`, etc.

recodeflow ships only `R/example_der_function.R` (1 function).

### 2.5 CEP (Codebase Enhancement Proposal) process

`ceps/` directory with 15+ domain-specific CEPs documenting variable
decisions, coverage evidence, and design rationale. CEP-017 (on another
branch, commit c777e1c8) documents the v4 engine modernization planning
with a 78-finding design-issues inventory.

recodeflow has no equivalent structured process for documenting variable-
or feature-level decisions.

### 2.6 Variable-discovery API

`R/variable-discovery.R`. Query `variables.csv` by subject, section,
recommendation tag (`{recommended:primary}`), or source. Returns
harmonized variable metadata without requiring regex patterns against
column names. Designed for application-mode consumers of cchsflow output.

recodeflow has no variable-discovery API.

### 2.7 Worksheet-getter API with tidyselect support

`R/worksheet-getters.R`. `get_variables(variable_name, ..., use_rdata =
FALSE)` and `get_variable_details(...)` with tidyselect column selection
(`starts_with("label")`, `contains("label")`). Provides pipe-friendly
single-variable or vector lookup against both RData and CSV sources.

recodeflow has no programmatic worksheet-query API; worksheets are passed
as dataframes directly to `rec_with_table()`.

### 2.8 Missing-pattern cache (`R/missing-pattern-cache.R`)

1077-line session-level cache (`cache_pattern()`, `get_complete_pattern()`,
`get_missing_pattern()`, `apply_database_heuristics()`, `auto_detect_database()`).
Caches extracted missing-code patterns by variable + database to avoid
repeated CSV scans during multi-variable harmonization runs.

recodeflow has no caching layer; it re-reads worksheets on each call.

### 2.9 Legacy/deprecation layer

`R/legacy/` with `adl-legacy.R`, `alcohol-legacy.R`, `bmi-legacy.R`:
shim wrappers preserving v2 function names. `R/NAMESPACE` exports both
new and legacy names during the transition.

recodeflow has no version-to-version migration shims (though the scoping
doc calls for them).

### 2.10 Multi-cycle PUMF sample data for 12 survey cycles

`data/cchs2001_p.RData` through `data/cchs2017_2018_p.RData` (12 files,
200 rows each). These enable users to run vignette examples against real
survey structure.

recodeflow ships only the PBC clinical trial dataset.

### 2.11 `assign_missing()` and `apply_else_logic()` primitives

Exported utilities that feed the 3-step architecture and can be used
independently by downstream consumers. recodeflow has `is_equal()` as its
only comparable low-level exported utility.

### 2.12 `label_data()` / `set_data_labels()` with sjlabelled integration

`R/label-utils.R`. Attaches variable labels and value labels drawn from
worksheet metadata to a recoded dataframe. Although the design-issues
inventory flags bugs (tibble support, heterogeneous `variableStartLabel`
crash), the intent is to automate label attachment as a post-recoding
step.

recodeflow has `set_data_labels()` (re-exported), but the comprehensive
per-domain label integration is cchsflow-only.

---

## 3. Architectural divergence summary

| Dimension | recodeflow | cchsflow v3 |
|---|---|---|
| Engine | Monolithic `rec_with_table()` (1277 lines) | Same engine (983-line copy) + 3-step DV layer on top |
| DV dispatch | `rowwise() + do()` positional `do.call()` | Case_when + `any_missing()` / `get_priority_missing()` |
| Missing data | Haven tagged_na; recodeflow scoping targets one representation | Three representations (tagged_na / "NA(b)" factor / plain NA) — design issue noted in CEP-017 |
| Worksheet validation | `parse_variables_sheet()` (in-memory, structural) | `check_worksheet()` + `fix_worksheet()` (on-disk, format + structure) |
| Content | PBC demo (24 vars / 69 DV rows) | 419 vars / multi-thousand DV rows, 2001–2023 |
| Metadata | Dublin Core `pbc_metadata` YAML (prototype) | Column-order YAML schemas only |
| Scoping | 7 formal HTML scope documents, 3 research branches | 15+ domain CEPs; no engine-layer scope docs (CEP-017 adds this) |
| API surface | 6 exports (rec_with_table, parse_variables_sheet, get_start_variables, set_data_labels, is_equal, is_table_feeder_var/get_table_name) | 125 exports (domain functions, 3-step primitives, worksheet tools, discovery) |
| Tests | 5 test files (unit + 1 integration snapshot) | 28 test files (domain-specific unit tests, worksheet checks) |
| Vignettes | 9 vignettes (general + advanced topics) | 9 vignettes (CCHS-specific workflow) |
| Legacy layer | None | R/legacy/ + deprecated-aliases |
| Constants | Worksheet-inline (string/numeric literals in variableStart) | YAML files + R constants objects |

---

## 4. Findings relevant to v4 scoping

### Features recodeflow has that v4 should absorb into cchsflow

1. `parse_variables_sheet()` typed object — v4 should validate the
   in-memory worksheet, not only the on-disk file.
2. `get_start_variables()` typed graph — prerequisite for vectorized
   DV dispatch (Phase 1 of v4 outline).
3. Template-variable expansion — reduces worksheet verbosity for
   CCHS variables that share category schemas across survey modules.
4. Scalar/string constants in variableStart — lets worksheets pass
   threshold and conversion constants without an extra column.
5. `append_non_db_columns` — needed for multi-cycle `bind_rows` workflows.
6. Scoped logging design (recodeflow scope doc) — structured log objects
   with variable-level metadata; better than the 91 `stop()` calls
   documented in the CEP-017 sweep.
7. Variable + worksheet versioning (scoping doc) — decoupled package /
   worksheet / variable versions.
8. Catalog / Dublin Core dataset metadata (scope doc) — foundation for
   the data-dictionary generation planned in the documentation roadmap.

### cchsflow features that v4 must preserve

1. 3-step DV architecture: `clean_variables()`, `any_missing()`,
   `get_priority_missing()` — the core missing-data value proposition.
2. `check_worksheet()` / `fix_worksheet()` — worksheet quality tooling
   that recodeflow lacks entirely.
3. Variable-discovery API and worksheet-getter API — consumer-facing
   tools for application developers.
4. Missing-pattern cache — performance optimization needed at CCHS scale
   (419 variables, 12+ cycles).
5. Domain-derived-variable library — the 30+ functional modules are the
   primary deliverable of cchsflow; must survive the v4 engine refactor.

### Features neither repo has (gap opportunities for v4)

- Formal recStart/recEnd grammar with parse-time validation
  (prevents the set-notation mangling bug found in CEP-017).
- Cache invalidation on worksheet change.
- `data-raw/` rebuild pipeline for RData projections.
- Parallel-worker-safe caching (process-local caches fail under
  `mclapply` / `future`).
- Codebook / data-dictionary generation from worksheets + catalog.
