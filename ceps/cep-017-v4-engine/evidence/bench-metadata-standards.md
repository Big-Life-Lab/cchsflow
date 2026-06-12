# Benchmark: Metadata Standards for the cchsflow v4 Schema

Prepared: 2026-06-11  
Branch context: fix/v3-smoking-worksheet-sync / cep-017-v4-engine scoping

---

## 1. What are we aligning to?

The v4 schema involves two structured artefacts:

1. **`variable_details.csv`** (23 columns) — one row per variable-database-category combination. Key columns: `variable`, `databaseStart`, `variableStart`, `recStart`, `recEnd`, `typeStart`, `typeEnd`, `catLabel`, `catLabelLong`, `units`, `notes`, `version`, `status`.

2. **`variables.csv`** (18 columns) — one row per harmonised variable. Key columns: `variable`, `label`, `labelLong`, `variableType`, `databaseStart`, `variableStart`, `subject`, `section`, `units`, `description`, `version`, `status`.

Together they constitute: (a) a variable-level codebook, (b) a cross-database mapping/recode table, and (c) a dataset-level catalog placeholder (the `catalog` object scoped in the recodeflow PR43 docs).

The five candidate standards are: DDI Codebook/Lifecycle, DCAT v3 + Dublin Core, Croissant, LinkML, and Frictionless Table Schema.

---

## 2. Standard-by-standard assessment

### 2.1 DDI Codebook 2.6 / Lifecycle 3.x

**What it is.** The Data Documentation Initiative is a long-standing XML-based standard for documenting socioeconomic surveys and censuses. DDI Codebook (v 2.5/2.6) targets single datasets; DDI Lifecycle (v 3.x/4.0-beta) covers the full data lifecycle from study design through archiving. DDI 4.0 beta was released in 2025 and adds RDF and JSON representations alongside XML.
Source: https://ddialliance.org/ddi-codebook

**Variable-level coverage.** The `<var>` element captures: variable name, label, interview question text, universe, coding instructions, value categories (`<catgry>` with label, missing-value flag), valid range, and summary statistics. `<varGrp>` groups related variables (equivalent to cchsflow's `section`/`subject` columns).
Source: https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/catgry.html

**Alignment with cchsflow worksheets.**

| cchsflow column | DDI element |
|---|---|
| `variable` | `<var name="">` |
| `label` / `catLabel` | `<labl>` on `<var>` / `<catgry>` |
| `catLabelLong` | `<txt>` inside `<catgry>` |
| `recStart` / `recEnd` | No direct equivalent; partially `<catgry><catValu>` for coded values |
| `databaseStart` | Study-level `<dataAccs>` / file citation; no per-variable database list |
| `variableStart` | No direct equivalent; harmonization mapping is not a DDI concept |
| `units` | `<varFormat>` decimal attribute, `<measUnit>` |
| `notes` | `<notes>` on `<var>` |
| `version` / `status` | Study-level versioning only; no per-variable version field |
| Missing codes (NA::a, NA::b) | `<catgry missing="Y">` |

**Gap.** DDI has no mechanism for the cchsflow "cross-database mapping" concept: mapping a source variable (e.g., SMK_005 in cchs2001_p) to a harmonised variable (SMKDSTY) via a recode expression. That belongs to PMML (a separate standard that recodeflow already uses for legacy dispatch), not DDI.

**R tooling.**
- `DDIwR` v0.19 (CRAN, Dec 2024, maintained by Adrian Dusa): imports/exports DDI Codebook 2.6, handles value labels, missing codes, SPSS/Stata/SAS conversion. 153 functions including `exportDDI()`, `getCodebook()`, `makeCategories()`, `recodeMissings()`.
- `ipumsr` (CRAN, actively maintained): reads IPUMS DDI XML extracts; uses DDI to carry variable labels, value labels, universe. Real-world proof that DDI + harmonization coexist (IPUMS does harmonization above DDI).
- `rddi` (GitHub, Global-TIES-for-Children): lightweight DDI Codebook 2.5 builder; not on CRAN as of Jun 2026.

**Assessment for cchsflow v4.** DDI is the strongest match for the *codebook* layer of `variables.csv`/`variable_details.csv` — labels, value codes, missing code flags, question text, universe. It does not model the recode/mapping rows. Alignment requires exporting DDI for the codebook facet; the mapping DSL (recStart/recEnd/DerivedVar::) remains a cchsflow-specific extension. IPUMS is the existence proof: rich harmonization alongside DDI metadata. Effort to add DDI export: moderate — DDIwR already handles the element mapping; the main work is building an `export_ddi()` function from the worksheet.

---

### 2.2 DCAT v3 + Dublin Core

**What it is.** DCAT (Data Catalog Vocabulary) v3 became a W3C Recommendation on 22 August 2024. It is an RDF vocabulary for describing datasets and data services in a catalog. Dublin Core Terms (`dcterms:`) provide title, description, creator, publisher, issued, modified. DCAT adds `dcat:Dataset`, `dcat:Distribution`, `dcat:Catalog`, `dcat:DatasetSeries`.
Source: https://www.w3.org/TR/vocab-dcat-3/

**What it covers.**
- Catalog level: title, description, publisher, license, rights, temporal/spatial coverage.
- Dataset level: keyword, theme, distribution access URL, media type, format.
- Distribution level: download URL, byte size, checksum.
- No variable-level metadata — DCAT does not descend to the codebook level.

**Alignment with cchsflow worksheets.** DCAT maps onto the proposed `catalog` object from the recodeflow PR43 scoping doc (title, description, creator, publisher, date_created, date_modified, version, license, contact_point). The `catalog` schema proposed in that document is essentially a subset of `dcat:Dataset` + `dcterms:` properties.

| catalog field | DCAT/Dublin Core term |
|---|---|
| `title` | `dcterms:title` |
| `description` | `dcterms:description` |
| `creator` | `dcterms:creator` |
| `publisher` | `dcterms:publisher` |
| `date_created` | `dcterms:created` |
| `date_modified` | `dcterms:modified` |
| `version` | `dcat:version` (DCAT v3 new) |
| `license` | `dcterms:license` |
| `contact_point` | `dcat:contactPoint` |

**Gap.** DCAT does not model variables or value codes. It would sit above the worksheet layer, not replace it.

**R tooling.** No dedicated R DCAT package as of Jun 2026. The `dataset` package (CRAN) partially implements Dublin Core for data frames. JSON-LD serialisation can be written manually or via `jsonld`. The `catalog.qmd` scoping doc explicitly named DCAT and proposed a CSV-first implementation that could later emit DCAT-aligned JSON-LD.

**Assessment for cchsflow v4.** DCAT is the right standard for the *dataset/catalog layer*, not the variable layer. Alignment is straightforward: add a `catalog.csv` or `catalog.yaml` sidecar whose fields map 1-to-1 to DCAT/Dublin Core terms. No R package is required immediately; the mapping can be documented in CLAUDE.md and a future `export_catalog_dcat()` function can emit JSON-LD. Low effort, high interoperability payoff for data portal discovery (e.g., data.gc.ca, Open Canada portal).

---

### 2.3 Croissant

**What it is.** Croissant is a metadata format for ML-ready datasets, published by MLCommons in March 2024. It extends `schema.org/Dataset` with four layers: dataset metadata, resources (FileObject/FileSet), structure (RecordSets with Fields), and semantic layer (ML-specific annotations like train/test splits).
Source: https://research.google/blog/croissant-a-metadata-format-for-ml-ready-datasets/
Canonical paper: https://arxiv.org/html/2403.19546v1

**What it covers.**
- Dataset-level discovery metadata (same DCAT/Dublin Core space).
- File-level resource description.
- Field-level typing and semantic annotations.
- ML-specific: splits (train/test/validation), label columns, bounding boxes.
- Adopted by Hugging Face, Kaggle, OpenML, Google Dataset Search (~400k+ datasets collectively).

**Alignment with cchsflow worksheets.** Croissant's RecordSet/Field layer maps loosely to the variable layer but is designed for ML feature engineering, not survey codebooks. It lacks: value categories with labels, missing code taxonomy, recode mappings, database availability lists.

**R tooling.** None confirmed. Croissant is Python-first (TensorFlow Datasets, JAX, PyTorch). A CKAN plugin was released in 2024, but no R package exists as of Jun 2026.

**Assessment for cchsflow v4.** Croissant is mismatched to cchsflow's use case. cchsflow outputs are harmonized population health survey datasets, not ML training sets. The Croissant semantic layer is built around ML labels (classification target columns, splits), not survey value-code taxonomies or cross-cycle harmonization mappings. The DCAT/Dublin Core overlap with Croissant is best served by implementing DCAT directly. Croissant adds no value for cchsflow's primary users (epidemiologists, public health analysts).

**Recommendation: ignore.**

---

### 2.4 LinkML

**What it is.** LinkML (Linked Data Modeling Language) is a YAML-based schema framework that generates 30+ output formats: JSON Schema, JSON-LD, OWL, SHACL, Python dataclasses, SQL DDL, Excel/CSV templates, Markdown documentation. It is Python-first, actively developed, with a GigaScience paper published Dec 2025. It is used by NCI CCDH, NMDC, and Alliance of Genome Resources.
Source: https://linkml.io/
GigaScience paper: https://academic.oup.com/gigascience/article/doi/10.1093/gigascience/giaf152/8378082

**What it covers.**
- Class/slot/enum hierarchies with range constraints and inheritance.
- Semantic mapping to ontology terms via `mappings:` property.
- Enum definitions with permissible values — the closest parallel to cchsflow value codes.
- Schema-level metadata.
- No survey-specific constructs (question text, interviewer instructions, universe, cross-database availability).
- Schemasheets tool allows domain experts to author schemas in spreadsheet format → LinkML YAML.

**Alignment with cchsflow worksheets.**
- `variables.csv`: maps well to a LinkML class definition (name = slot name, label/labelLong = `title`/`description`, variableType = range, subject/section = domain).
- `variable_details.csv` value codes: map to LinkML enums with permissible values and `meaning` (ontology URI).
- The recode/harmonization logic (recStart, recEnd, DerivedVar::, Func::) has no LinkML equivalent.
- Missing codes (NA::a, NA::b) could be represented as enum entries, but LinkML has no native "user-defined missing" concept.

**R tooling.** No R package exists for LinkML as of Jun 2026. The porting guide acknowledges this gap. Usage from R would require `reticulate` to call Python tooling, which is a significant dependency burden for an R package.

**Assessment for cchsflow v4.** LinkML is the strongest option for *defining the schema of the worksheets themselves* (meta-schema), not as a runtime format. If the v4 redesign formalises what columns `variable_details.csv` must have, and what values are permissible, LinkML YAML could serve as the authoritative schema definition and generate: JSON Schema validators, documentation, and ontology links. The Schemasheets tool is directly relevant — the worksheets are already spreadsheet-based. However, there is no R runtime tooling, and generating LinkML from scratch requires Python. The practical recommendation is to imitate LinkML's design patterns (enum definitions, slot ranges, semantic mapping annotations) when designing the v4 column schema, without depending on the Python toolchain.

**Recommendation: imitate** (design patterns, not toolchain).

---

### 2.5 Frictionless Table Schema

**What it is.** Frictionless Data Table Schema is a JSON specification for describing tabular data: field names, types, constraints (required, unique, enum, min/max, pattern), missing values, and foreign keys. It is part of the broader Data Package standard (also Frictionless). DCAT v3 references Frictionless Data Package as a compatible distribution format.
Sources:
- https://specs.frictionlessdata.io//table-schema/
- https://docs.ropensci.org/frictionless/articles/table-schema.html

**What it covers.**
- Field types: string, integer, number, boolean, date, datetime, array, object, geopoint.
- Constraints: required, unique, enum, minLength, maxLength, minimum, maximum, pattern.
- Missing values: `missingValues` array of strings (e.g., `["", "NA", "96", "997"]`).
- Foreign keys: link between tables.
- `title` and `description` on fields (human-readable but not value labels).

**Alignment with cchsflow worksheets.**

| cchsflow concept | Frictionless Table Schema |
|---|---|
| Column names in worksheets | `fields[].name` |
| Column types | `fields[].type` |
| Valid categories for a column | `constraints.enum` |
| Missing value codes | `missingValues` (string-level, not per-variable) |
| Value labels (catLabel) | Not supported; `title`/`description` are for the field, not individual values |
| Cross-table links (variableStart ↔ variable) | `foreignKeys` |
| Per-variable documentation | No; field descriptors describe the *column*, not each row |

**R tooling.**
- `frictionless` (CRAN, v1.2.1, May 2025, rOpenSci): reads/writes Data Packages; uses `schema$fields` and `schema$missingValues` for type parsing; supports `constraints$enum` for factor levels. Does NOT propagate title, description, or most constraints at read time.
- `tableschema.r` (CRAN, Apr 2025): validates Table Schema descriptors; more complete constraint support.
- Both are well-maintained rOpenSci/Frictionless ecosystem packages.

**Gap for cchsflow.** Frictionless Table Schema describes the *structure of a CSV file* (column names and types) — it is not a codebook. It cannot represent: value labels for specific codes (e.g., "1 = Male"), multi-code missing taxonomies (NA::a vs NA::b), recode mappings, or database availability. It is suitable for validating that `variable_details.csv` conforms to its expected column schema (column names, types, required-ness), but it cannot replace the content within those columns.

**Assessment for cchsflow v4.** Frictionless Table Schema is appropriate as a *machine-readable schema for the worksheets themselves* (i.e., a `datapackage.json` that validates the CSV format of `variable_details.csv` and `variables.csv`). This is the narrowest alignment scope but is achievable immediately with existing R tooling. It would catch structural errors (wrong column order, unexpected types, missing required columns) and enable the `check_worksheet()` family of functions to delegate to a standards-based validator. The `frictionless` R package is rOpenSci-maintained and stable.

**Recommendation: wrap** (use for worksheet structural validation; Frictionless Table Schema as the schema of the schema).

---

## 3. Cross-standard comparison matrix

| Standard | Level | Variable metadata | Value codes | Missing codes | Recode/mapping | R tooling | Effort |
|---|---|---|---|---|---|---|---|
| DDI Codebook 2.6 | Variable/codebook | Rich | Yes (`<catgry>`) | Yes (`missing="Y"`) | No | DDIwR v0.19 | Moderate |
| DDI Lifecycle 3.x | Study lifecycle | Rich | Yes | Yes | Partial | DDIwR (planned) | High |
| DCAT v3 + DC | Dataset/catalog | No | No | No | No | None (manual JSON-LD) | Low |
| Croissant | Dataset + ML features | Partial | Partial (ML labels) | No | No | None | N/A |
| LinkML | Meta-schema | Yes (class/slot) | Yes (enum) | Partial | No | None (Python only) | High |
| Frictionless Table Schema | CSV structure | No (column schema only) | No (enum = column constraint) | Partial (missingValues) | No | frictionless, tableschema.r | Low |

---

## 4. Concrete recommendations for v4

### 4.1 Adopt DDI Codebook as the export target for the codebook layer

The `variables.csv` + `variable_details.csv` codebook content (variable labels, value labels, missing codes, units, question text when added) maps cleanly to DDI Codebook 2.6 via `DDIwR`. Add `export_ddi()` as a v4 function that converts the worksheet to DDI XML. IPUMS is proof-of-concept that DDI + harmonization coexist in a major public health survey data project.

What alignment requires:
1. Map `variable` → `<var name="">`.
2. Map `label`/`labelLong` → `<labl>` short/long.
3. Map each `catLabel` row → `<catgry><catValu>` + `<labl>`.
4. Flag NA::a / NA::b rows → `<catgry missing="Y">`.
5. Map `units` → `<varFormat measUnit="">`.
6. Map `notes` → `<var><notes>`.
7. `databaseStart` has no DDI equivalent; document in a study-level `<notes>` or custom extension.

DDIwR's `makeCategories()` and `exportDDI()` functions can do most of this programmatically. The main gap is that cchsflow has multiple rows per variable (one per database era), which requires collapsing to the canonical harmonised variable before export.

### 4.2 Wrap Frictionless Table Schema for worksheet structural validation

Create a `datapackage.json` in `inst/extdata/` that declares schemas for `variable_details.csv` and `variables.csv`:
- Field names and types.
- Required columns.
- `enum` constraints on `typeEnd`, `typeStart`, `status`.
- Foreign key linking `variable_details.variable` → `variables.variable`.
- `missingValues: ["", "NA", "N/A"]`.

The `frictionless` package can then validate worksheets against this descriptor via `read_package()` + `read_resource()`. This replaces ad hoc column-count checks in `check_worksheet()` with a standards-compliant approach.

### 4.3 Align catalog to DCAT v3 / Dublin Core

Implement the `catalog` object (already scoped in the recodeflow PR43 docs) with fields that map 1-to-1 to DCAT/Dublin Core terms. Store in `catalog.csv` (consistent with worksheet pattern). Add a future `export_catalog_dcat()` that emits JSON-LD using the `dcat:` namespace. No R package dependency required initially.

### 4.4 Imitate LinkML design patterns for the v4 column schema

When redesigning `variable_details.csv` columns for v4, apply LinkML design principles:
- Define allowed values for controlled-vocabulary columns (typeEnd, typeStart, status) as explicit enumerations.
- Define each column's purpose, range, and cardinality in a schema YAML (can be inside the package as `inst/metadata/schemas/worksheet-schema.yaml`).
- Map columns to ontology URIs where applicable (e.g., `units` → UCUM codes, `subject` → MeSH).
Do not adopt the Python LinkML toolchain; imitate the design patterns.

### 4.5 Ignore Croissant

No alignment value for cchsflow's use case. Croissant is ML-dataset-first with no survey-specific constructs, no R tooling, and no benefit beyond the DCAT layer already covered.

---

## 5. Sources

- DDI Alliance: https://ddialliance.org/ddi-codebook
- DDI Codebook 2.5 catgry element: https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation_files/schemas/codebook_xsd/elements/catgry.html
- DCAT v3 W3C Recommendation (Aug 2024): https://www.w3.org/TR/vocab-dcat-3/
- DCAT v3 announcement: https://www.w3.org/news/2024/data-catalog-vocabulary-dcat-version-3-is-a-w3c-recommendation/
- Croissant announcement (Mar 2024): https://mlcommons.org/2024/03/croissant_metadata_announce/
- Croissant paper (ACM DEEM 2024): https://dl.acm.org/doi/10.1145/3650203.3663326
- Croissant arxiv preprint: https://arxiv.org/html/2403.19546v1
- LinkML home: https://linkml.io/
- LinkML GigaScience paper (Dec 2025): https://academic.oup.com/gigascience/article/doi/10.1093/gigascience/giaf152/8378082
- Frictionless Table Schema spec: https://specs.frictionlessdata.io//table-schema/
- frictionless R package (rOpenSci): https://docs.ropensci.org/frictionless/articles/table-schema.html
- frictionless CRAN: https://cran.r-project.org/web/packages/frictionless/index.html (v1.2.1, May 2025)
- tableschema.r CRAN: https://cran.r-project.org/web/packages/tableschema.r/index.html (Apr 2025)
- DDIwR CRAN: https://cran.r-project.org/package=DDIwR (v0.19, Dec 2024)
- ipumsr DDI reading: https://rdrr.io/cran/ipumsr/man/read_ipums_ddi.html
- rddi package intro: https://cran.r-project.org/web/packages/rddi/vignettes/rddi_intro.html
- recodeflow metadata scoping doc: /tmp/recodeflow-pr43/doug-originals/catalog.qmd
- recodeflow labels scoping doc: /tmp/recodeflow-pr43/doug-originals/labels.qmd
- cchsflow v4 architecture reference: /tmp/cchsflow-recovered-specs/current-simplified-specs/ARCHITECTURE-REFERENCE.md
- cchsflow variable_details.csv: /Users/dmanuel/github/cchsflow/inst/extdata/variable_details.csv
- cchsflow variables.csv: /Users/dmanuel/github/cchsflow/inst/extdata/variables.csv
