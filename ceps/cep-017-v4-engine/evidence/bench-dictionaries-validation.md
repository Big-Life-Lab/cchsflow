# Ecosystem Benchmark: Dictionaries & Validation Packages

**Date:** 2026-06-11
**Branch context:** cchsflow fix/v3-smoking-worksheet-sync; v4 scoping underway
**Task:** Evaluate codebook, datadictionary, pointblank, validate for potential adoption vs. imitation vs. ignore in cchsflow v4

---

## 1. Package inventory

### 1.1 validate (data-cleaning/validate)
- **CRAN version:** 1.1.7 (2025-12-10)
- **License:** GPL-3
- **Stars:** 431, open issues: 49
- **Source:** https://cran.r-project.org/package=validate
- **GitHub:** https://github.com/data-cleaning/validate
- **Cookbook:** https://data-cleaning.github.io/validate/
- **JSS paper:** https://arxiv.org/pdf/1912.09759

**What it does:**
`validator()` creates a portable set of validation rules expressed as R logical expressions. Rules are first-class objects: they can be named, labelled, described, imported/exported as YAML or data frames, visualised, and checked for internal consistency. `confront(data, rules)` executes the rules and returns a confrontation object with per-row, per-variable pass/fail/NA counts. Results can be summarised, plotted, or exported.

**Rule types:**
- Per-field: type, range, missingness, code-list membership
- In-record: conditional restrictions, forbidden combinations
- Cross-record: aggregation checks, balance equations
- Cross-dataset: comparing multiple versions of the same dataset
- Statistical: group-level summaries, time-series checks
- SDMX: rules derived from SDMX DSD files

**YAML rule file format (illustrative):**
```yaml
- expr: age >= 0 & age <= 120
  name: age_range
  label: "Age plausibility"
  description: "Age must be between 0 and 120"
```

**Key strengths for cchsflow:**
- Rules can be imported/exported from YAML files that live alongside variable_details.csv — aligns with cchsflow's sidecar-file pattern.
- Cross-dataset comparison directly applies to comparing pre- and post-recode outputs (Step 3 validation).
- Lightweight dependencies (methods, stats, graphics, settings, yaml — no tidyverse).
- GPL-3 is compatible with cchsflow's GPL-2+ licence (check this; GPL-3 is not fully compatible with GPL-2-only).
- No concept of worksheets, allowed-value sets from a codebook, or survey-specific missing codes.

**Key gaps for cchsflow:**
- No awareness of survey-specific missing codes (NA::a, NA::b, tagged NA) — rules would need manual specification of these.
- No integration with variable_details.csv recFrom/recTo ranges. The allowed-value sets and bounds that cchsflow already encodes in variable_details.csv would need to be extracted and converted to validator() rules — doable but manual.
- No codebook or data dictionary output. Purely a validation engine.

---

### 1.2 pointblank (posit-dev/pointblank)
- **CRAN version:** 0.12.3 (2025-11-28)
- **License:** MIT
- **Stars:** ~1,000, open issues: 109
- **Source:** https://cran.r-project.org/package=pointblank
- **GitHub:** https://github.com/rstudio/pointblank (moved to https://github.com/posit-dev/pointblank)
- **Docs:** https://rstudio.github.io/pointblank/

**What it does:**
Three workflows:
1. **Agent + validation plan** — chainable validation steps (col_vals_between, col_vals_in_set, col_vals_not_null, col_schema_match, rows_distinct, etc.) attached to an agent object. The agent collects results and can generate interactive HTML reports via the gt package, raise warnings/errors, or write log entries.
2. **Pipeline validation** — same functions used directly inside dplyr-style pipelines without an agent; raises R warnings/errors when thresholds are exceeded.
3. **Informant (information management)** — `create_informant()` builds a living data dictionary. Column-level descriptions, table-level notes, dynamic snippets (snip_list, snip_stats, snip_lowest, snip_highest) that auto-update from data. YAML serialisation via yaml_write()/yaml_read_informant().

**draft_validation()** scans a data frame and auto-generates a validation script using column inference: type checks (col_schema_match), range checks (col_vals_between with observed min/max), distinctness (rows_distinct). Intended as a starting template, not production-ready.

**col_vals_in_set()** — checks column values against a literal R vector. The `set` parameter is a vector; it cannot reference an external dictionary directly. Values would need to be extracted from variable_details.csv programmatically.

**Key strengths for cchsflow:**
- The informant workflow is the best-fit for generating a human-readable data dictionary from variables.csv + variable_details.csv. Column descriptions, type information, and dynamic snippets (e.g., snip_list of allowed codes) could reflect the existing metadata.
- YAML round-trip for informant objects means dictionary could be versioned alongside CSVs.
- MIT licence — no licence compatibility concerns.
- Well-maintained (Posit-backed), broad database support (DuckDB, SQLite, PostgreSQL, etc.) — relevant if cchsflow ever validates against a cchs-metadata DuckDB instance directly.
- Interactive HTML reports would be useful for users checking harmonisation outputs.

**Key gaps for cchsflow:**
- Heavy dependency surface: gt, dplyr, tibble, tidyr, DBI, dbplyr, glue, yaml + many suggests. Adds ~20+ packages to cchsflow's dependency tree.
- The informant is manually authored; it has no mechanism to ingest variable_details.csv and auto-populate column descriptions. Building that bridge is non-trivial custom code.
- Validation rules are not portable as standalone text files in the same way validate's YAML is.
- No concept of survey missing codes, tagged NA, or interval notation (recFrom/recTo).
- col_vals_in_set() set parameter cannot be a programmatic reference to a codebook row — would need a wrapper function.

---

### 1.3 codebook (rubenarslan/codebook)
- **CRAN version:** 0.10.1 (2026-03-03)
- **License:** MIT
- **Source:** https://cran.r-project.org/package=codebook
- **GitHub:** https://github.com/rubenarslan/codebook
- **Docs:** https://rubenarslan.github.io/codebook/
- **Paper:** Arslan RC (2019), Advances in Methods and Practices in Psychological Science

**What it does:**
Generates interactive HTML/PDF/Word codebooks from R data frames. Reads metadata from R attributes (variable labels via var_label(), value labels via val_labels(), scale structure). Computes distributions, frequency tables, Cronbach's alpha for scales, reverse-coding support. Exports JSON-LD metadata compatible with schema.org / Google Dataset Search.

**Key strengths for cchsflow:**
- Reads `haven::labelled` value labels — directly relevant if cchsflow outputs are labelled (it does attach labels via set_data_labels()/label_data()).
- dict_to_list() converts a spreadsheet dictionary into R label lists — a thin bridge from variables.csv to labelled output.
- JSON-LD export would make harmonised CCHS datasets discoverable — valuable for the open-science mission.
- Rmarkdown-based output integrates with existing vignette/pkgdown workflow.

**Key gaps for cchsflow:**
- Designed for psychometric survey scales (Cronbach's alpha, reverse-coding, item-level analysis). These features are irrelevant to epidemiological health surveys like CCHS.
- Focused on single-dataset documentation. cchsflow documents ~200 source variables per survey cycle across 20+ cycles — codebook's per-variable distribution plots would produce enormous HTML files.
- No validation capability — purely documentation output.
- Depends heavily on rmarkdown rendering pipeline; less useful as a library function.
- Not designed to document harmonisation mappings (recFrom/recTo, interval notation, missing code taxonomy).

---

### 1.4 datadictionary (DoctorBJones/datadictionary)
- **CRAN version:** 1.0.1 (2025-07-22)
- **License:** MIT
- **Stars:** 12, forks: 0
- **Source:** https://cran.r-project.org/package=datadictionary
- **GitHub:** https://github.com/DoctorBJones/datadictionary

**What it does:**
Minimal package — two exported functions: create_dictionary() and summarise_variable(). Takes a data frame, produces a tibble with columns: item, label, class, summary, value. Supports optional var_labels (named vector), output to Excel. Summary varies by type (numeric: mean/median/min/max; factor/logical: level counts; character: unique counts). No validation.

**Key gaps for cchsflow:**
- Very limited. Essentially a fancier str() + summary(). No mapping metadata, no allowed-value sets, no harmonisation documentation.
- 12 stars, no forks, no active development — not a mature dependency.
- No relevance to cchsflow's specific needs.

---

### 1.5 codebookr (brad-cannell/codebookr)
- **CRAN version:** 0.1.9 (2026-02-13)
- **License:** MIT
- **Source:** https://cran.r-project.org/package=codebookr
- **GitHub:** https://github.com/brad-cannell/codebookr
- **Docs:** https://brad-cannell.github.io/codebookr/

**What it does:**
Generates codebooks from data frames using flextable + officer (Word document output). Depends on haven for labelled data. More output-format focused than codebook package.

**Key gaps for cchsflow:**
- Word document output is useful for reporting but not for programmatic validation.
- haven + flextable dependency adds bulk.
- No validation capability; no harmonisation mapping documentation.

---

### 1.6 retroharmonize (antaldaniel/retroharmonize)
- **CRAN version:** 0.2.8 (2026-05-21)
- **License:** GPL-3
- **Source:** https://cran.r-project.org/package=retroharmonize
- **Docs:** https://ropengov.github.io/retroharmonize/

**What it does:**
Ex-post harmonisation of survey data across waves/studies. Uses labelled_spss_survey S3 class (extends haven::labelled_spss) to preserve variable names, labels, value codes, and missing value ranges across source surveys. Tools for crosswalk tables, value-label harmonisation, missing-value alignment. Case studies: Afrobarometer, Arab Barometer, Eurobarometer.

**Relevance to cchsflow:**
conceptually overlaps with cchsflow's mission (cross-survey harmonisation), but the approach is orthogonal. retroharmonize works at the labelled-data/codelist level, treating harmonisation as label unification. cchsflow works at the variable-specification level, treating harmonisation as a recoding-rule specification problem encoded in variable_details.csv. No Canadian data coverage, no CCHS-specific functionality.

**Key gaps for cchsflow:**
- GPL-3 licence complexity.
- No awareness of cchsflow's worksheet schema or missing-code taxonomy.
- Would require substantial adaptation to be useful.

---

## 2. Comparison against cchsflow's actual validation needs

### 2.1 check_worksheet (existing)
`R/check-worksheet.R` (528 lines) validates the *format* of variables.csv and variable_details.csv:
- File existence and parsability
- Column order vs. schema
- ID column presence
- Row sorting (alphabetical by variable name)
- Trailing empty columns (the Excel-export artefact)
- Line endings (LF vs. CRLF)
- Excessive quoting (minimal quoting rule)

None of the benchmarked packages touch this domain. This is worksheet hygiene validation, not data content validation. The existing implementation is appropriate and well-suited to its purpose. The validate or pointblank packages add no value here.

### 2.2 Step-3 output validation (planned for v4)
The 3-step derived-variable architecture uses clean_variables() at step 3 to validate that the output of a derive function conforms to the bounds declared in variable_details.csv (recStart/recEnd, allowed categorical codes). This is content validation of the *output* data frame.

**validate package fit:**
- Could be used: validator rules generated from variable_details.csv rows, confronted against the output data frame.
- Programmatic rule generation: `variable_details %>% filter(variable == output_var) %>% summarise(rules)` → `validator(...)`.
- The interval-notation recStart/recEnd format in variable_details.csv maps well to validator's range check syntax.
- Cross-dataset check: validator can compare pre-recode source columns against post-recode output — directly applicable to integration tests.
- BUT: survey missing codes (NA::a, NA::b, tagged NA) are not understood by validate; rules must explicitly exclude them. This is a non-trivial complication for cchsflow's missing-data architecture.

**pointblank fit:**
- col_vals_between() and col_vals_in_set() are functionally equivalent to what step-3 clean_variables() already does. Using pointblank for step-3 would mean replacing the current clean_variables() output validation with pointblank validation steps — possible but adds heavy dependencies for functionality already present.
- The informant workflow could expose step-3 results as interactive reports, which is genuinely useful for QA review.

### 2.3 Data dictionary / codebook generation (planned via recodeflow catalog)
The recodeflow catalog spec (catalog.qmd) envisions combining catalog + variables + variable_details into a user-facing data dictionary. The recodeflow metadata.qmd doc explicitly calls for Dublin Core / DCAT alignment.

**pointblank informant fit:**
- The informant's column-level info_columns() + info_section() + dynamic snippets is the closest match for auto-generating a living data dictionary from variables.csv.
- BUT: it requires manually authoring the informant or writing a variables.csv → informant converter. There is no built-in mechanism to ingest a codebook CSV.
- YAML serialisation means the dictionary can be versioned and regenerated.

**codebook fit:**
- dict_to_list() bridges a dictionary spreadsheet to R label attributes — directly applicable if cchsflow wants to enrich output data frames with variable/value labels from variables.csv.
- Psychometric features are irrelevant noise for CCHS.
- JSON-LD output is useful for open-data discovery.

**Custom implementation:**
The existing variables.csv + variable_details.csv already contain all the information needed for a data dictionary. A bespoke function that reads these CSVs and renders an HTML summary (possibly using gt, knitr, or flextable) would be simpler than wrapping pointblank informant and would avoid the heavy dependency.

---

## 3. Recommendation matrix

| Package | Current fit | v4 action | Rationale |
|---|---|---|---|
| validate | Medium | Imitate | Core concept (rules as first-class objects, YAML export, confront()) is the right pattern for step-3 validation. Adopt concept, not package — missing-code handling gap is too deep. |
| pointblank | Low-medium | Wrap (informant only) | MIT, well-maintained, informant workflow is worth using for QA report generation. Validation functions duplicate clean_variables(). Only wrap if interactive HTML QA reports are a v4 deliverable. |
| codebook | Low | Ignore | Psychometric framing, rmarkdown-heavy, not designed for harmonisation mapping. dict_to_list() concept is worth imitating (1-2 lines of code). |
| datadictionary | None | Ignore | Too minimal, low adoption, adds no value. |
| codebookr | None | Ignore | Word output focus, no validation, not relevant. |
| retroharmonize | Conceptual only | Ignore | GPL-3 complication, no CCHS support, orthogonal approach. |

---

## 4. Design lessons for v4

1. **Rules as first-class objects (from validate):** cchsflow's step-3 validation should treat allowed-value specs as objects that can be introspected, tested, and version-controlled — not just implicit in clean_variables() logic. A lightweight `cchsflow_rule` or `output_spec` object (derived from variable_details.csv rows) enables this without adopting validate's GPL-3.

2. **YAML-serialisable rule sets (from validate):** Storing derived-variable output specs as YAML alongside the worksheets enables validation without re-reading the CSV on every run. The cchs-metadata DuckDB already stores this metadata — v4 should expose it as a queryable rule set.

3. **Living data dictionary with dynamic snippets (from pointblank informant):** The concept of embedding computed statistics (e.g., observed range, frequency distribution) directly into column documentation, stored as YAML and regenerated on demand, is directly applicable to cchsflow's QA workflow. No need to adopt the full pointblank stack.

4. **Sidecar metadata as the source of truth (already in cchsflow/recodeflow):** The recodeflow catalog spec and the existing worksheet architecture correctly identify that metadata should live in open, version-controlled CSV/YAML files, not encoded in package internals. This principle is validated by the validate, codebook, and pointblank packages — all use external files (YAML, CSV) as the canonical metadata store.

5. **GPL-3 licence caution:** validate is GPL-3. cchsflow is GPL-2+. If GPL-2+ is interpreted as "GPL-2 or any later version", GPL-3 is compatible; if it means "GPL-2 only" it is not. Verify licence terms before adopting validate as a dependency. If ambiguous, imitating validate's rule architecture in native code avoids the issue entirely.

---

## 5. Sources

- https://cran.r-project.org/package=pointblank (v0.12.3, 2025-11-28)
- https://cran.r-project.org/package=validate (v1.1.7, 2025-12-10)
- https://cran.r-project.org/package=codebook (v0.10.1, 2026-03-03)
- https://cran.r-project.org/package=datadictionary (v1.0.1, 2025-07-22)
- https://cran.r-project.org/package=codebookr (v0.1.9, 2026-02-13)
- https://cran.r-project.org/package=retroharmonize (v0.2.8, 2026-05-21)
- https://rstudio.github.io/pointblank/ (main docs)
- https://rstudio.github.io/pointblank/reference/create_informant.html
- https://rstudio.github.io/pointblank/reference/draft_validation.html
- https://rstudio.github.io/pointblank/reference/col_vals_in_set.html
- https://data-cleaning.github.io/validate/ (validate cookbook)
- https://github.com/data-cleaning/validate (431 stars)
- https://github.com/rstudio/pointblank (~1000 stars)
- https://github.com/rubenarslan/codebook
- https://github.com/DoctorBJones/datadictionary (12 stars)
- https://rubenarslan.github.io/codebook/
- https://ropengov.github.io/retroharmonize/
- /Users/dmanuel/github/cchsflow/R/check-worksheet.R (local)
- /tmp/recodeflow-pr43/doug-originals/catalog.qmd (local)
- /tmp/recodeflow-pr43/metadata.qmd (local)
