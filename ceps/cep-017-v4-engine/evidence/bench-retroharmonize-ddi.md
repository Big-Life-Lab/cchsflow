# Benchmark: retroharmonize + DDI ecosystem

**Date researched:** 2026-06-11
**Sources verified:** CRAN, GitHub API, package vignettes, academic paper

---

## 1. Package inventory

### 1a. retroharmonize

- **CRAN page:** https://cran.r-project.org/web/packages/retroharmonize/
- **GitHub:** https://github.com/rOpenGov/retroharmonize (canonical repo; mirrored at dataobservatory-eu/retroharmonize)
- **Current version:** 0.2.8, published 2026-05-21
- **Maintainer:** Daniel Antal (dataobservatory.eu)
- **License:** GPL-3
- **R requirement:** >= 3.5.0
- **Dependencies:** assertthat, cli, dataset, dplyr (>=1.0.0), fs, glue, haven, here, labelled, magrittr, purrr, rlang, snakecase, stringr, tibble, tidyr, tidyselect, vctrs
- **CRAN archival history:** Archived 2026-01-30 because its dependency `dataset` was itself archived. Restored via 0.2.8 release (2026-05-21) after refactoring vctrs integration and removing/replacing the dataset dependency.
- **Recent commits (2026):** S3 class cleanup (March 2026), CRAN compatibility fixes (May 2026). No feature additions—maintenance-only mode.
- **Open GitHub issues (as of 2026-06-11):** 13 open issues, oldest from 2021. Key open issues:
  - #33 (2023-12-01): `.data` in tidyselect deprecated — unfixed
  - #30 (2022-06-02): `create_codebook()` examples fail
  - #29 (2022-06-02): Inconsistent use of import_path/survey_path/export_path
  - #28 (2022-06-02): `file_id` undefined in `get_survey_no_ctable()`
  - #21 (2022-01-28): `crosswalk_table_create()` loses `val_label_orig`
  - #24 (2022-01-28): `metadata_create()` gives many warnings
  - #6 (2021-06-18): `merge_waves()` loses id attribute
- **Key design fact:** Was temporarily broken (CRAN-archived) from Jan–May 2026 due to cascading dependency failure. This is a material stability risk for dependent workflows.

### 1b. DDIwR

- **CRAN page:** https://cran.r-project.org/web/packages/DDIwR/
- **GitHub:** https://github.com/dusadrian/DDIwR
- **Current version:** 0.19 (released 2024-12-10, July 2025 PDF manual)
- **Maintainer:** Adrian Dusa (University of Bucharest)
- **License:** GPL (>= 3)
- **Monthly CRAN downloads (approx.):** ~271
- **Open issues:** 2 (low activity repo, 20 stars)
- **Purpose:** Convert to/from DDI Codebook XML (version 2.6); convert between SPSS, Stata, SAS, R, Excel, CSV with metadata preservation. Uses the `declared` package for extended missing value handling.
- **Key functions:** `getCodebook()`, `exportCodebook()`, `convert()`, `setupfile()`, `recodeMissings()`, `makeElement()`, `makeCategories()`, `testValid()`, `showDetails()`
- **Scope:** Metadata exchange format converter. Does NOT do harmonization (no crosswalk, no multi-survey merging). Intended as building block, not standalone harmonization tool.
- **Relationship to retroharmonize:** Mentioned by retroharmonize developer as a planned integration ("new dependencies: DDIwR, dataset and declared" — from dev notes). The 0.2.8 refactor brings these closer together.

---

## 2. What retroharmonize solves

### Core design philosophy

retroharmonize is an **ex-post harmonization tool for survey microdata**: given N survey files with different variable names, different value codings, and different missing value schemes, it produces a single tidy joined data frame with consistent codes and labels.

The mental model is:
1. **Read** survey files (SPSS .sav, Stata .dta, CSV, RDS)
2. **Inventory** variable metadata across all files (`metadata_create()`)
3. **Plan** harmonization via crosswalk table (`crosswalk_table_create()`)
4. **Execute** renaming, subsetting, recoding (`harmonize_var_names()`, `harmonize_values()`, `merge_surveys()`)
5. **Output** a unified tidy data frame, with full provenance attributes

The `labelled_spss_survey` class is the key innovation: it extends `haven::labelled_spss` to carry survey-level provenance (survey id, original variable name, original value labels) as vector attributes, so cross-survey concatenation is possible even when the original codings diverge.

### What it handles well

1. **Rename variables across survey waves:** crosswalk table maps `var_name_orig` → `var_name_target` per file
2. **Recode categorical value codes:** e.g., female=2 in Survey A → female=0 to match Survey B
3. **Normalize value labels:** regex-based `harmonize_values()` maps label strings to target labels and numeric codes
4. **Standardize missing value handling:** distinguishes "not applicable" from "refused" / "don't know"; uses the 99997-99999 range convention; converts SPSS-style missing ranges to explicit NA_real_
5. **Memory-efficient sequential processing:** can work with file paths rather than loading all surveys into memory (important for 80+ Eurobarometer waves)
6. **Provenance tracking:** labelled_spss_survey attributes preserve original survey metadata alongside harmonized values
7. **Tested survey programs:** documented case studies with ~80 Eurobarometer files, 5 Afrobarometer rounds, Arab Barometer

---

## 3. The crosswalk table format

The retroharmonize crosswalk table is a data frame with these columns:

| Column | Required | Purpose |
|--------|----------|---------|
| id | yes | Survey identifier |
| filename | yes | Source file path |
| var_name_orig | yes | Original variable name |
| var_name_target | yes | Harmonized output name |
| val_numeric_orig | no | Source numeric code |
| val_numeric_target | no | Target numeric code |
| val_label_orig | no | Source value label |
| val_label_target | no | Target value label |
| na_numeric_orig | no | Source missing code |
| na_numeric_target | no | Target missing code |
| na_label_orig | no | Source missing label |
| na_label_target | no | Target missing label |
| class_orig | no | Source R class |
| class_target | no | Target R class |

This is created programmatically via `crosswalk_table_create()` from a metadata data frame, then edited (dplyr pipelines or spreadsheet). Known bug: `crosswalk_table_create()` loses `val_label_orig` in some cases (open issue #21).

---

## 4. Comparison: retroharmonize crosswalk vs cchsflow variable_details

### cchsflow variable_details schema (23 columns, 3,839 rows for v3)

Key columns:
- `variable`: output variable name
- `dummyVariable`: per-category dummy for cat variables
- `typeEnd` / `typeStart`: cat or cont
- `databaseStart`: which CCHS surveys contain this variable
- `variableStart`: source variable name(s), with survey-prefix syntax for name changes; `DerivedVar::[]` for multi-input variables
- `recEnd`: target value, or `NA::a`, `NA::b`, `else`, `copy`, or `Func::functionName`
- `recStart`: source value/range, using interval notation `[1,4]`
- `catLabel` / `catLabelLong`: output labels
- `catStartLabel`: source label (DDI-derived)
- `variableStartLabel` / `variableStartShortLabel`: variable description
- `units`, `notes`, `version`, `lastUpdated`, `status`

### Structural comparison

| Dimension | retroharmonize crosswalk | cchsflow variable_details |
|-----------|--------------------------|--------------------------|
| **Unit of a row** | One source variable × one survey | One category value × one set of databases |
| **Value mapping grain** | One row per source code | One row per output category; input range encoded as `[lo,hi]` string |
| **Interval ranges** | Not supported; exact code match only | Fully supported via `[lo,hi]` interval notation in `recStart` |
| **Variable name mapping** | Yes, per-file explicit | Yes, via survey-prefix syntax; default-name `[VAR]` shorthand |
| **Multi-input derived variables** | Not supported | `DerivedVar::[var1,var2,...]` + `Func::functionName` |
| **Arbitrary compute functions** | Not supported | Supported via `Func::` dispatch to R functions |
| **Missing value taxonomy** | 99997/99998/99999 convention | Tagged NA: `NA::a` (not applicable), `NA::b` (missing) |
| **Type change (cat→cont)** | Supported via `class_target` | Supported via `typeStart`/`typeEnd` columns |
| **Data source** | SPSS .sav, Stata .dta, CSV | Any R data frame; designed for CCHS CSV/RData |
| **Provenance in metadata** | labelled_spss_survey attributes | `catStartLabel`, `variableStartLabel`, `notes`, `version` columns |
| **Machine-executable** | Yes, via harmonize_values() etc. | Yes, via `rec_with_table()` |
| **Human-readable in spreadsheet** | Moderate (more rows per variable) | Yes (designed for spreadsheet editing) |
| **Derived/computed variables** | **None** | **Core feature** (299 DerivedVar rows, 64 Func:: rows) |
| **Era-split variables** | Manual: separate rows per survey | Explicit: `databaseStart` comma list per row |

### Critical gap: derived/computed variables

retroharmonize has **no mechanism** for derived or computed variables. The function reference explicitly does not list any functions for creating new variables from combinations of existing ones. The Kołczyńska (2022) review similarly shows this is a gap in the crosswalk-table paradigm generally.

In cchsflow, the `DerivedVar::` + `Func::` system is how BMI, pack-years, ADL scores, active transport, and all smoking cessation timing variables are computed. These represent 299/3,839 rows (7.8%) of variable_details, but they are conceptually the most complex and valuable part of the package.

This is the single most important architectural difference.

---

## 5. What retroharmonize does NOT do (vs cchsflow v3 needs)

1. **No derived/computed variables.** Cannot express BMI = weight/height², pack-years = (cigarettes/day/20) × years-smoked, ADL score = sum of 5 binary items. cchsflow v3 handles all of these.

2. **No interval-range recoding.** retroharmonize works with exact numeric codes; it cannot express "all values from 30 to 99 recode to category 3". cchsflow's `[lo,hi]` interval notation in `recStart` handles continuous-to-categorical conversions like age grouping and BMI categorization.

3. **No multi-database era logic in a single row.** cchsflow's `databaseStart` lists which surveys contain a variable; one row can apply to 10 databases simultaneously. retroharmonize's crosswalk requires one row per survey × variable combination.

4. **No survey-neutral design.** retroharmonize is SPSS/Stata-centric (haven dependency, labelled_spss_survey class). cchsflow works with plain R data frames.

5. **No `else` catch-all recode.** cchsflow has explicit `else` / `copy` semantics for unmatched values. retroharmonize has no analogous construct.

6. **Fragile dependency chain.** Was CRAN-archived 2026-01-30 to 2026-05-21 due to `dataset` package cascading failure. Not a one-off: this happened because retroharmonize depends on a research-software package (`dataset`) with its own instability. cchsflow's dependency footprint (haven, dplyr, sjmisc) is more stable.

---

## 6. What retroharmonize does well that cchsflow lacks

1. **Memory-efficient large-file processing.** The sequential file-path API (not requiring all surveys in memory at once) is purpose-built for datasets like Eurobarometer with 80+ waves. cchsflow/recodeflow loads everything into R data frames.

2. **Provenance-aware vector class.** labelled_spss_survey vectors carry the original survey's labels and codes as attributes, enabling reconstruction of what was done after the fact. cchsflow doesn't carry original metadata inside the output vector — it's in the worksheet rows.

3. **Codebook generation.** `create_codebook()` and `document_surveys()` produce structured survey documentation. cchsflow has no equivalent output format.

4. **Multi-survey metadata inventory.** `metadata_create()` produces a tabular inventory of all variables across all survey files — useful for discovery before writing mappings. cchsflow's MCP server provides this for CCHS specifically but it is CCHS-specific.

5. **Standard missing-value taxonomy.** The 99997/99998/99999 convention for do_not_know/declined/inap is clear and consistent. cchsflow's `NA::a`/`NA::b` tagged NA approach is R-only and can cause problems when exporting to SPSS/Stata.

6. **Programmatic crosswalk creation.** `crosswalk_table_create()` auto-generates the crosswalk skeleton from metadata — less manual than cchsflow where every row must be hand-crafted.

---

## 7. DDIwR: specific lessons

DDIwR's role is narrower: it is a **DDI Codebook XML reader/writer**, not a harmonization engine. Its `convert()` function moves data between SPSS, Stata, SAS, R, and Excel with metadata preservation. The `getCodebook()` function extracts variable/value labels and missing value info from DDI XML or SPSS files.

For cchsflow v4, the DDIwR-derived lesson is: **a well-structured metadata extraction step at ingestion time, using standard DDI XML, could replace the manual `catStartLabel` / `variableStartLabel` population** that currently requires hand-copying from Statistics Canada data dictionaries. The CCHS DDI codebooks (PUMF 2001–2018) already exist and are referenced in cchsflow memory notes.

The `declared` package (DDIwR's missing value handler) provides a third approach to extended missing values (beyond haven's tagged NA and retroharmonize's 99997-99999), using declared NAs with typed reasons. Worth monitoring but not mature enough for adoption.

---

## 8. Key lessons for cchsflow v4

1. **The crosswalk-table model is the right paradigm for pass-through/recode variables** (3,540/3,839 rows). retroharmonize validates this. The cchsflow `variable_details` schema is already more capable (interval notation, era logic, labels).

2. **Derived/computed variables need a separate layer.** Neither retroharmonize nor any other package in this ecosystem provides a general solution. cchsflow's `Func::` dispatch is the right architectural direction; v4 should formalize and test it, not replace it with something from this ecosystem.

3. **The `labelled_spss_survey` lesson is about atomic provenance.** Carrying original-survey metadata at the vector level (rather than only in the worksheet) enables downstream reconstruction and debugging. v4 could consider whether output variables should carry provenance attributes.

4. **Dependency stability matters.** retroharmonize's 4-month CRAN archive from a cascading dependency failure is a concrete lesson. cchsflow's dependency chain should remain minimal and stable. The `dataset` and `declared` packages are research-software with high volatility — do not adopt them.

5. **Programmatic crosswalk skeleton generation** is worth adding to cchsflow tooling: given a new survey wave, auto-generate the variable_details row stubs from a DDI/SPSS/CSV metadata scan, then let the curator fill in the recode logic. This is what `crosswalk_table_create()` does for retroharmonize — the equivalent for cchsflow would read a Statistics Canada DDI XML and output pre-populated variable_details rows.

6. **Missing value taxonomy unification.** The retroharmonize 99997/99998/99999 convention is more portable than tagged NA. cchsflow v4 should consider whether the `NA::a`/`NA::b` system is the right long-term choice, especially for ICES/SPSS export.

7. **retroharmonize does not solve the "what year does this variable exist?" problem** in the way cchsflow's `databaseStart` column does. The CCHS's structural complexity (Master vs PUMF, annual vs biennial, module opt-ins) requires the row-per-era architecture that cchsflow uses. retroharmonize's crosswalk would require one row per survey × variable which would explode to thousands of rows.

---

## 9. CRAN/maintenance status summary

| Package | Version | Last CRAN | Status |
|---------|---------|-----------|--------|
| retroharmonize | 0.2.8 | 2026-05-21 | Active (restored after 4-month archive) |
| DDIwR | 0.19 | 2024-12-10 | Active, low traffic (~271/month) |
| declared | (dependency) | 2025 | Low maturity, monitor only |
| dataset | (dependency) | Archived | Do not use |

---

## Sources

- https://cran.r-project.org/web/packages/retroharmonize/
- https://retroharmonize.dataobservatory.eu/articles/crosswalk.html
- https://retroharmonize.dataobservatory.eu/articles/labelled_spss_survey.html
- https://retroharmonize.dataobservatory.eu/articles/survey_harmonization.html
- https://retroharmonize.dataobservatory.eu/articles/afrobarometer.html
- https://retroharmonize.dataobservatory.eu/reference/index.html
- https://github.com/rOpenGov/retroharmonize (issue tracker, releases)
- https://cran.r-project.org/web/packages/DDIwR/
- https://www.rdocumentation.org/packages/DDIwR/versions/0.19
- https://github.com/dusadrian/DDIwR
- https://rdrr.io/cran/retroharmonize/
- https://journals.sagepub.com/doi/full/10.1177/20597991221077923 (Kołczyńska 2022, crosswalk methodology)
- https://arxiv.org/html/2411.10342v1 (EHR harmonization using recodeflow)
- https://big-life-lab.github.io/cchsflow/articles/variable_details.html
- /Users/dmanuel/github/cchsflow/vignettes/variable_details.Rmd (local)
- /Users/dmanuel/github/cchsflow/inst/extdata/variable_details.csv (local, 3,839 rows verified)
