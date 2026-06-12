# Benchmark: Harmonization Platforms — Maelstrom/Rmonize and Peers
# For cchsflow v4 engine scoping
# Researched: 2026-06-11

---

## 1. Family overview

This document covers the "harmonization platforms" family: systems that encode
transformation rules in structured metadata tables (rather than ad-hoc scripts)
and apply them to produce reproducible harmonized datasets. The primary focus is
Maelstrom Research's Rmonize/Opal/Mica ecosystem, with comparison to psHarmonize,
retroharmonize, and IPUMS as contrasting reference points.

---

## 2. Maelstrom Research ecosystem

### 2.1 Rmonize (R package)

- **CRAN:** https://cran.r-project.org/web/packages/Rmonize/index.html
- **GitHub:** https://github.com/maelstrom-research/Rmonize
- **Docs:** https://maelstrom-research.github.io/Rmonize-documentation/
- **Current version:** 2.0.0
- **Published on CRAN:** 2025-06-30 (2.0.0 release July 21, 2025 per NEWS.md)
- **R requirement:** >= 3.5
- **License:** GPL-3
- **Key dependencies:** dplyr (>=1.1.0), rlang, stringr, tidyr, haven, fabR (>=2.0.0),
  madshapR (>=2.0.0)
- **Status:** Actively maintained by Maelstrom Research. 224 commits, 3 releases.
  Breaking API changes in 2.0.0 (function/parameter renames), but no stability
  concerns equivalent to retroharmonize's CRAN-archival event.

### 2.2 How Rmonize encodes harmonization rules

The harmonization process is defined by two user-authored structured tables:

**DataSchema** — describes the harmonized output
- A list of two data frames: `Variables` and `Categories`
- `Variables` must contain at minimum: `name`, `label`, `valueType`
  (text/integer/decimal/boolean/date), and optionally `unit`, `taxonomy` terms
- `Categories` (if present) must contain: `variable`, `name`, `label` columns
- Stored as Excel or CSV; loaded with `as_dataschema()`

**Data Processing Elements (DPE)** — one row per DataSchema variable per input dataset
Five mandatory columns:
1. `input_dataset` — name of the source dataset (must match dossier keys)
2. `dataschema_variable` — target harmonized variable name
3. `input_variables` — source variable name(s), semicolon-separated;
   `__BLANK__` if no input needed
4. `harmo_rule` (or equivalent column; "algorithm" in older docs) — one of
   eight rule-type keywords (see below)
5. The rule algorithm string (inline R code or keyword)

**Eight rule types** (specified in `harmo_rule`/algorithm column):
- `id_creation` — establishes unique row identifiers; must be first rule
- `direct_mapping` — copy source variable without transformation
- `recode` — categorical recode using `=` and `;` delimiters (e.g. `1=Yes;2=No`)
- `case_when` — conditional logic using `~` and `;` (inline R case_when syntax)
- `paste` — assign a constant; `__BLANK__` as input_variables
- `operation` — arithmetic/statistical expression on input variable(s)
- `other` — arbitrary R script; `<<-` double-assignment for environment side effects
- `impossible` / `undetermined` — marks variable as unavailable or pending;
  allows `harmo_process()` to run without error

Additional documentation columns may be added freely but are not processed by
the engine — they pass through as-is for analyst notes.

**Execution engine:** `harmo_process(dossier, data_proc_elem)` iterates the DPE,
dispatches each row to the appropriate rule handler, and returns a
"harmonized dossier" — a named list of harmonized datasets (one per input dataset)
with associated metadata extracted from the DataSchema and DPE. Error/warning
annotations are added back to the DPE output rows.

Source: https://maelstrom-research.github.io/Rmonize-documentation/dpe/index.html
Source: https://cran.r-project.org/web/packages/Rmonize/vignettes/b-Data-processing-elements.html

### 2.3 Provenance model in Rmonize

- **No dedicated version-control layer** within Rmonize itself.
- The DPE table is the provenance artifact: it is meant to be stored in version
  control (Excel/CSV in a repo) alongside the harmonized outputs.
- `harmo_process()` annotates output dossier metadata with the algorithm applied
  to each variable per dataset, allowing downstream audit of which rule generated
  which value.
- The broader Maelstrom process guidelines (IJE 2017, PMC5407152) prescribe
  documentation of every step and decision in a structured format, but this is
  process-level guidance rather than software-enforced provenance.
- **No row-level lineage** (i.e., no tracking of which input observation produced
  which output row beyond the join key).

### 2.4 Versioning in Rmonize

- DPE tables are versioned externally (by analyst, in version control).
- `harmo_process()` adds processing metadata to the returned dossier object,
  but does not stamp a pipeline version or schema version on output rows.
- Summary/evaluation reports (`harmonized_dossier_summarize()`,
  `harmonized_dossier_evaluate()`) support auditing after the fact.

### 2.5 Opal (server application)

- **Docs:** https://opaldoc.obiba.org/en/latest/
- **GitHub:** https://github.com/obiba/opal
- **License:** GPL-3 (Java/JavaScript web application by OBiBa)
- Purpose: server-side data management, harmonization, and dissemination hub

**Rule encoding in Opal:**
- Variables are organized as "views" (virtual tables)
- Derived variables = variables whose values are computed by scripts
- Scripts are written in the **Magma JavaScript API** (Opal's built-in JS engine)
- A comprehensive JS utility library (unit conversions, date manipulation, etc.)
  is bundled
- R integration available for complex statistical derivations

**Provenance/VCS in Opal:**
- Opal has an **embedded version control system** for derived variable scripts
- Every script edit creates a new VCS commit with author, date, and optional comment
- REST API exposes:
  - `GET /datasource/{d}/view/{v}/vcs/commits` — full commit history for a view
  - `GET /datasource/{d}/view/{v}/vcs/variable/{var}/commits` — per-variable history
  - `GET /datasource/{d}/view/{v}/vcs/variable/{var}/commit/{id}` — diff at commit
  - `GET /datasource/{d}/view/{v}/vcs/variable/{var}/blob/{id}` — script at commit
- This is a **first-class audit trail at the variable-script level** — a capability
  cchsflow/recodeflow does not have.
- DataSchema is uploaded to each Opal server instance; processing algorithms are
  developed locally per study using Opal's view/derived-variable system.

Source: https://github.com/obiba/opal/issues/1816
Source: https://opaldoc.obiba.org/en/dev/data-harmonization.html

### 2.6 Mica (web portal)

- Purpose: build searchable web portals that expose study and variable metadata
- Integrates with Opal for federated real-time summary statistics
- Not directly relevant to rule encoding; primarily a dissemination layer

### 2.7 DataSHIELD

- Purpose: privacy-preserving co-analysis; sends analysis requests to distributed
  Opal servers; raw data never leaves host servers
- Relevant for cchsflow context: federated analysis across CCHS cycles on secure
  servers (e.g. ICES) would use DataSHIELD; harmonization still done via Opal/Rmonize

---

## 3. psHarmonize

- **CRAN:** https://cran.r-project.org/web/packages/psHarmonize/index.html
- **GitHub:** https://github.com/NUDACC/psHarmonize
- **Published:** 2025-10-16 (v0.3.6); peer-reviewed paper in Patterns (2024)
- **License:** not specified in CRAN summary
- **R requirement:** >= 2.10
- **Authors:** Stephen JJ, Carolan P, Krefman AE et al. (Northwestern NUDACC)
- **Paper:** https://www.cell.com/patterns/fulltext/S2666-3899(24)00128-4

**Rule encoding — the harmonization sheet:**
A single flat table (CSV or Excel) with one row per variable per study:

| Column | Purpose |
|---|---|
| `study` | Source cohort/dataset identifier |
| `item` | Harmonized variable name |
| `source_item` | Source variable name(s); semicolons for multi-input |
| `source_dataset` | Source dataset name |
| `id_var` | Identifier variable |
| `code_type` | Rule type: `"recode category"`, `"function"`, blank (direct copy) |
| `code1` | Rule definition |
| `domain` / `subdomain` | Subject domain classification |
| `coding_notes` | Human-readable documentation of the coding decision |
| `possible_range` | Expected value range for QC |
| `visit` | Visit number for longitudinal data |

**Rule types:**
- `"recode category"` — `original_value = harmonized_value; ...` (same semicolon
  syntax as Rmonize `recode`)
- `"function"` — R expression; single input referenced as `x`; multiple inputs
  as `x1, x2, ...` (semicolon-separated `source_item`)
- Blank `code_type` — direct copy

**`previous_dataset`** — special `source_dataset` value allowing a row to reference
a previously harmonized variable from the same pipeline (enables chaining).

**Provenance:** `coding_notes` column provides human-readable documentation inline
with the rule. No formal VCS or machine-readable lineage.

**Versioning:** External (analyst responsibility). No built-in version stamping.

**Output:** Produces long and wide harmonized datasets; descriptive statistics
via RMarkdown.

Source: https://cran.r-project.org/web/packages/psHarmonize/vignettes/Harmonization_sheet.html

---

## 4. retroharmonize

- **CRAN:** https://cran.r-project.org/web/packages/retroharmonize/
- **Current version:** 0.2.8, published 2026-05-21
- **License:** GPL-3; maintained by Daniel Antal (dataobservatory.eu)
- **Stability risk:** Archived from CRAN 2026-01-30 to 2026-05-21 due to
  cascading dependency failure (`dataset` package archived). Restored after
  refactoring.
- **Primary focus:** Ex-post harmonization of Eurobarometer, Afrobarometer,
  and similar comparative survey waves; DDI-compliant metadata.

**Rule encoding:** Unlike Rmonize/psHarmonize, retroharmonize does NOT use a
single rule-table. Instead:
- Rules are expressed as R functions operating on `labelled_spss_survey` objects
- The `labelled_spss_survey` S3 class extends haven's `labelled_spss`:
  preserves original variable names, labels, value codes alongside harmonized ones
- Crosswalk tables define value-code mappings between survey waves
- `merge_waves()` combines multiple survey waves after harmonization
- `create_codebook()` generates metadata summaries (currently has known bugs)

**Provenance:** Original variable names and value labels are preserved as
attributes on the harmonized vector — so the "before" state is embedded in
the output object. No formal audit trail beyond this.

**Key difference from Rmonize/psHarmonize:** retroharmonize is metadata-first —
it works from DDI codebooks and SPSS/SAV files natively; rules are expressed
programmatically rather than in a declarative table.

Source: https://dataobservatory.eu/software/retroharmonize/
Source: bench-retroharmonize-ddi.md (sibling file in /tmp/v4-research/)

---

## 5. IPUMS

- **Website:** https://www.ipums.org/
- **R package:** `ipumsr` v0.10.0 (March 2026); https://github.com/ipums/ipumsr
- **Approach:** Curator-managed harmonization; rules are not exposed to end users

**Rule encoding:** IPUMS harmonization is done by professional curators at the
Minnesota Population Center, not by end-user configuration tables. The "rules"
are embedded in the distributed microdata files and documented in DDI codebooks.
`ipumsr` is a _reader_ package, not a rule-encoding engine.

**Provenance:** Rich metadata in DDI XML (variable descriptions, comparability
notes, code changes across years, universe statements). The DDI is the provenance
artifact.

**Versioning:** Each IPUMS extract has a dataset version identifier; users cite
exact version in methods. No automated VCS within the package.

**Key lesson for cchsflow v4:** IPUMS's variable-level comparability metadata
(explicit warnings when a variable changes meaning across years) is a model for
cchsflow's era-split design. The DDI `<catgry>` and comparability note fields
parallel what cchsflow tries to express in `variable_details.csv` notes.

Source: bench-ipumsr-ipums.md (sibling file in /tmp/v4-research/)

---

## 6. EHRs Data Harmonization Platform (recodeflow-based Shiny app)

- **Paper:** https://arxiv.org/html/2411.10342v1 (2024)
- A Shiny GUI wrapper over recodeflow for clinical EHR harmonization
- Uses the same `variable_details` / `variables` worksheet model as cchsflow
- Adds: web-based GUI for constructing worksheets; multi-format data loading;
  automatic documentation generation
- Acknowledged limitations: struggles with large datasets (>20 GB CSV/RDS/SAS)
  without SQLite conversion
- Direct evidence that the recodeflow/cchsflow worksheet model is being adopted
  in clinical settings beyond population health surveys

---

## 7. General Primer for Data Harmonization (Cheng et al. 2024, Scientific Data)

- **PMC:** https://pmc.ncbi.nlm.nih.gov/articles/PMC10831085/
- Field-agnostic review; derives general principles from COVID-19 PHSM harmonization
- Key principles relevant to cchsflow v4:
  1. **Transparent documentation** of all methodological decisions at every step
  2. **Harmonization-information tradeoff** — explicitly acknowledge what granularity
     is lost in each recoding decision
  3. **False equivalence avoidance** — document when variables cannot be combined
     across waves (maps to cchsflow's `impossible`/`undetermined` concept)
  4. **FAIR alignment** — rules and provenance should be Findable, Accessible,
     Interoperable, Reusable
  5. **Measurement error propagation** — document that harmonization may introduce
     new errors, not just inherit old ones

---

## 8. Maelstrom Research guidelines (IJE 2017)

- **PMC:** https://pmc.ncbi.nlm.nih.gov/articles/PMC5407152/
- Six-step framework for retrospective epidemiological data harmonization:
  1. Define research questions and protocol
  2. Assemble information and select studies
  3. Define variables and evaluate potential (create DataSchema)
  4. Process data (apply DPE algorithms)
  5. Estimate quality (validate harmonized outputs)
  6. Disseminate and preserve

- **Processing models** (relevant to v4 rule types):
  - Algorithmic transformation (categorical recoding)
  - Calibration (unit conversion, continuous variables)
  - Standardization (different scales, no calibration available)
  - Latent variable models (bridging items across scales)
  - Multiple imputation (overlapping scales)

- **Lesson:** These five models map closely to what cchsflow does in
  `variable_details.csv`: rows with `DerivedVar:`, `Rec:`, `Calc:` prefixes
  represent distinct transformation families, but they are not formally
  distinguished as model types in the worksheet schema.

---

## 9. Direct comparison: Rmonize vs cchsflow/recodeflow

| Dimension | Rmonize 2.0 | cchsflow v3 / recodeflow |
|---|---|---|
| Rule storage | DPE table (Excel/CSV, 5 mandatory cols) | variable_details.csv (22 cols) + variables.csv |
| Rule dispatch | `harmo_rule` enum (8 types) | `recTo`/`recFrom` positional fields + DerivedVar: prefix in function_name |
| Inline code | Yes: `case_when`, `operation`, `other` columns embed R expressions directly | No: code is in external R functions; worksheet stores only the function name |
| Cross-dataset | One DPE row per (dataset × variable); explicit `impossible`/`undetermined` | database_start / database_end date range; no explicit "impossible" marker |
| DataSchema | Explicit: `Variables` + `Categories` data frames with valueType | Implicit: variables.csv is the schema; no formal valueType enforcement |
| VCS / audit trail | None in Rmonize; Opal has full per-variable VCS | None; rely on Git history for CSV files |
| Provenance at row level | No | No |
| Output validation | `harmonized_dossier_evaluate()` generates quality reports | check-worksheets.R validates format; no semantic quality report |
| Missing value handling | `NA_integer_` / `NA_character_` typed NAs; `__BLANK__` placeholder | Custom: 996/997/998/999 + NA sentinel codes; `get_priority_missing()` |
| Federated analysis | Via Opal + DataSHIELD | Not supported |
| GUI | None (R only) | Shiny wrapper exists (EHR platform) |
| Versioning of schema | External; no built-in | External; CEP documents serve as design provenance |

---

## 10. Key lessons for cchsflow v4

1. **Explicit rule taxonomy pays off.** Rmonize's 8-type enum (`direct_mapping`,
   `recode`, `case_when`, `operation`, `other`, `impossible`, `undetermined`)
   and psHarmonize's 3-type system (`recode category`, `function`, blank) both
   show that naming the rule type separately from the algorithm text makes
   programmatic dispatch cleaner and validation possible. cchsflow currently
   embeds rule type implicitly (DerivedVar: prefix, function_name field,
   recTo/recFrom structure). A v4 `rule_type` column would close this gap.

2. **Inline algorithm text vs. external functions.** Rmonize embeds R code
   directly in the DPE cell (the `algorithm` field). cchsflow externalizes code
   to R functions and references them by name. The tradeoff: inline code is more
   portable and auditable from the worksheet alone; external functions are more
   testable and maintainable for complex logic. A hybrid approach — inline for
   simple recodes, external function references for 3-step derived variables —
   is defensible and is already partially what cchsflow does.

3. **Explicit DataSchema is missing from cchsflow.** Rmonize separates the
   target output specification (DataSchema) from the transformation rules (DPE).
   cchsflow blends them: variables.csv is both the schema and the harmonized
   variable registry. Separating these concerns would allow independent validation
   of the target variable definition and the rules that generate it.

4. **`impossible`/`undetermined` as first-class states.** Rmonize marks
   variables that cannot be generated for a given dataset as `impossible` or
   `undetermined` — allowing `harmo_process()` to run end-to-end without
   treating missing mappings as failures. cchsflow handles this implicitly
   through database_start/database_end date ranges but has no explicit "this
   variable cannot be generated from this survey cycle" marker. This creates
   silent gaps rather than explicit documentation.

5. **Opal's per-variable VCS is the gold standard for audit trails.** The
   ability to retrieve the JavaScript script for any derived variable at any
   historical commit, with author and timestamp, is a capability no R-only tool
   in this ecosystem matches. For cchsflow v4, even light-weight provenance
   (hash of the algorithm used to generate a variable, stored in output data
   as an attribute) would be a meaningful improvement over nothing.

6. **Missing value architecture diverges.** All other platforms use standard R
   NA types (possibly typed NA_integer_ etc.). cchsflow's numeric sentinel codes
   (996-999) are idiosyncratic and not interoperable with other tools. This is a
   known design issue (CEP inventory) but worth emphasising: adopting typed NAs
   would make cchsflow output directly compatible with Rmonize, psHarmonize, and
   haven-based workflows.

7. **Federated analysis is not in scope for v4 but worth acknowledging.** The
   Opal/DataSHIELD pathway shows where platforms go when they need to operate on
   data that can't be extracted from secure servers. ICES use cases already
   constrain cchsflow to non-exportable data contexts; DataSHIELD-style co-
   analysis is a natural extension if cchsflow ever needs to support multi-site
   analysis without data movement.

8. **psHarmonize's `coding_notes` column models inline decision documentation.**
   Having a human-readable rationale field in the same row as the rule is better
   than separate documentation documents. cchsflow's `notes` column in
   variable_details is under-utilised; a structured `rationale` column (free text,
   machine-ignored but human-required) would help.

---

## Sources

- https://cran.r-project.org/web/packages/Rmonize/index.html
- https://maelstrom-research.github.io/Rmonize-documentation/
- https://maelstrom-research.github.io/Rmonize-documentation/dpe/index.html
- https://cran.r-project.org/web/packages/Rmonize/vignettes/b-Data-processing-elements.html
- https://github.com/maelstrom-research/Rmonize/blob/main/NEWS.md
- https://www.maelstrom-research.org/page/software
- https://opaldoc.obiba.org/en/dev/data-harmonization.html
- https://github.com/obiba/opal/issues/1816
- https://pmc.ncbi.nlm.nih.gov/articles/PMC5407152/
- https://pmc.ncbi.nlm.nih.gov/articles/PMC5837212/
- https://academic.oup.com/ije/article/46/5/1372/4102813
- https://cran.r-project.org/web/packages/psHarmonize/index.html
- https://cran.r-project.org/web/packages/psHarmonize/vignettes/Harmonization_sheet.html
- https://www.cell.com/patterns/fulltext/S2666-3899(24)00128-4
- https://cran.r-project.org/web/packages/retroharmonize/
- https://dataobservatory.eu/software/retroharmonize/
- https://arxiv.org/html/2411.10342v1
- https://pmc.ncbi.nlm.nih.gov/articles/PMC10831085/
