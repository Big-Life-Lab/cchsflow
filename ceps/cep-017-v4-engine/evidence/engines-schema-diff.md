# Worksheet Schema Diff: recodeflow vs cchsflow

**Analysis date:** 2026-06-11  
**Repos compared:**  
- recodeflow: `/Users/dmanuel/github/recodeflow` (dev branch)  
- cchsflow: `/Users/dmanuel/github/cchsflow` (fix/v3-smoking-worksheet-sync branch)

---

## 1. File inventory

### recodeflow worksheet specimens

| File | Columns | Rows | Notes |
|------|---------|------|-------|
| `inst/extdata/tester_variable_details.csv` | 16 | 119 | Primary test fixture |
| `inst/extdata/tester_variables.csv` | 11 | 24 | Primary test fixture |
| `inst/extdata/pbc_variable_details.csv` | 16 | ~34 | Mayo PBC clinical example |
| `inst/extdata/pbc_variables.csv` | 11 | 19 | Mayo PBC clinical example |
| `inst/no-template-variable-variable-details.csv` | 14 | — | Template system test (no templateVariable col) |
| `inst/template-variable-variable-details.csv` | 15 | — | Template system test (with templateVariable col) |

Schema/documentation YAMLs:
- `archive/variable_details.yaml` (v2.2.0, CCHS-focused)
- `archive/variables.yaml` (v2.2.0, CCHS-focused)
- `archive/variable_details_cchs.yaml` (CCHS-specific supplement)
- `archive/variables_cchs.yaml` (CCHS-specific supplement)
- `archive/variables_recodeflow_pr70.yaml` (PR70 proposal, v0.1)
- `inst/extdata/pbc_metadata.yaml` (Dublin Core dataset metadata, not schema)

### cchsflow worksheet files

| File | Columns | Rows | Notes |
|------|---------|------|-------|
| `inst/extdata/variable_details.csv` | 23 | 3,839 | Production worksheet |
| `inst/extdata/variables.csv` | 18 | 418 | Production worksheet |
| `inst/metadata/schemas/core/variable_details.yaml` | 21 cols defined | — | Active schema (v1.0.0, 2025-06-22) |
| `inst/metadata/schemas/core/variables.yaml` | 18 cols defined | — | Active schema (minimal, just column list) |
| `inst/metadata/documentation/metadata_registry.yaml` | — | — | Shared specifications registry |

---

## 2. Column-set diff: variable_details

### 2a. Core 16-column baseline (shared between repos)

Both repos' production and test worksheets share these 16 columns as the common core:

```
variable, dummyVariable, typeEnd, databaseStart, variableStart, typeStart,
recEnd, numValidCat, catLabel, catLabelLong, units, recStart, catStartLabel,
variableStartShortLabel, variableStartLabel, notes
```

**Column order differs** in the core 16 between repos:

| Position | recodeflow tester order | cchsflow CSV order | cchsflow core schema order |
|----------|------------------------|-------------------|---------------------------|
| 1 | variable | variable | variable |
| 2 | dummyVariable | dummyVariable | dummyVariable |
| 3 | typeEnd | typeEnd | typeEnd |
| 4 | typeStart | databaseStart | databaseStart |
| 5 | databaseStart | variableStart | variableStart |
| 6 | variableStart | **ICES.confirmation** | typeStart |
| 7 | variableStartLabel | typeStart | recEnd |
| 8 | numValidCat | recEnd | numValidCat |
| 9 | recEnd | numValidCat | catLabel |
| 10 | catLabel | catLabel | catLabelLong |
| 11 | catLabelLong | catLabelLong | units |
| 12 | units | units | recStart |
| 13 | recStart | recStart | catStartLabel |
| 14 | catStartLabel | catStartLabel | variableStartShortLabel |
| 15 | variableStartShortLabel | variableStartShortLabel | variableStartLabel |
| 16 | notes | variableStartLabel | notes |
|    |        | notes (pos 17) | |

Key ordering divergences:
- recodeflow puts `typeStart` at position 4 (before `databaseStart`); cchsflow schema puts it at position 6 (after `variableStart`)
- recodeflow puts `variableStartLabel` before `numValidCat`; both cchsflow versions put it near the end of the core block
- cchsflow production CSV inserts `ICES.confirmation` at position 6, disrupting the core field sequence
- cchsflow CSV and core schema don't match each other: `check_worksheet` will report column-order errors if run against the production CSV

### 2b. Extension columns

| Column | recodeflow tester | recodeflow archive schema | cchsflow CSV (production) | cchsflow core schema |
|--------|:-----------------:|:------------------------:|:------------------------:|:-------------------:|
| `ICES.confirmation` | — | `ICES confirmation` (space) | YES (pos 6) | — |
| `templateVariable` | — (test file) | YES (optional) | — | YES (extension) |
| `version` | — | YES | YES | YES |
| `lastUpdated` | — | YES | YES | YES |
| `harmonizationStatus` | — | YES | — | — |
| `status` | — | — | YES | YES |
| `reviewNotes` | — | YES | YES | YES |
| `versionNotes` | — | — | YES | — |
| `review` | — | — | YES | — |

**Key differences:**
1. **`harmonizationStatus` vs `status`**: recodeflow archive schema uses `harmonizationStatus` (enum: development/active/not_harmonizable/pending_review). cchsflow uses `status` with a different enum (adds "deprecated", "discontinued"). These are semantically equivalent fields with different names and enumerations.
2. **`ICES.confirmation` vs `ICES confirmation`**: cchsflow CSV uses a dot-separated R-safe column name (`ICES.confirmation`); recodeflow archive schema uses the original with a space (`ICES confirmation`). These are the same field with a name normalization difference.
3. **`versionNotes` and `review`**: cchsflow-only columns not present in any recodeflow schema or specimen. These appear to be internal workflow columns (71% populated in production CSV).
4. **`templateVariable`**: Defined in recodeflow archive schema and present in the template-test CSV specimen, but absent from cchsflow's production variable_details.csv. It appears in cchsflow's core schema as an "extension" field but isn't in the actual data.

### 2c. cchsflow production CSV vs its own core schema

Running `check_worksheet()` on the production CSV would report:
- Column order error at position 6 (`ICES.confirmation` found where `typeStart` expected)
- `ICES.confirmation` not in schema column list (positions 6-21 all shift)
- `templateVariable` in schema but not in CSV
- `versionNotes` and `review` in CSV but not in schema (schema allows `allow_additional_columns: true`)

---

## 3. Column-set diff: variables

### 3a. Shared core columns (both repos)

```
variable, label, labelLong, variableType, databaseStart, variableStart,
subject, section, units, notes, description
```

**Column order:** Both repos share the same logical set but with ordering differences:
- recodeflow tester: `subject, section` come before `variableType` (positions 4-5 vs 6 in cchsflow)
- cchsflow schema: `variableType` at position 4 (after labelLong), then `databaseStart, variableStart, subject, section`

### 3b. Extension columns comparison

| Column | recodeflow tester | recodeflow archive schema | cchsflow CSV (production) | cchsflow core schema |
|--------|:-----------------:|:------------------------:|:------------------------:|:-------------------:|
| `version` | — | YES | YES | YES |
| `lastUpdated` | — | YES | YES | YES |
| `harmonizationStatus` | — | YES | — | — |
| `status` | — | — | YES | YES |
| `reviewNotes` | — | YES | YES | YES |
| `ICES.confirmation` | — | `ICES confirmation` | YES | YES (as `ICES.confirmation`) |
| `Observation..MD.` | — | `Observation (MD)` | YES | YES (as `Observation..MD.`) |
| `versionNotes` | — | — | YES | YES |
| `notes` | YES (0% populated) | — | YES (93%) | — |
| `fileRowId` | — | PR70 proposed only | — | — |

**Key differences:**
1. `harmonizationStatus` (recodeflow archive) vs `status` (cchsflow): same field divergence as in variable_details
2. `Observation (MD)` → `Observation..MD.`: same R-name normalization issue as ICES field
3. `versionNotes` is in cchsflow but not in recodeflow
4. `notes` appears in recodeflow tester_variables (but is 100% empty) and in cchsflow (93% populated). The recodeflow archive schema doesn't list `notes` in variables.yaml. The cchsflow core schema also doesn't list it - but the actual cchsflow CSV has it.
5. `fileRowId` was proposed in recodeflow PR70 but never merged; not present anywhere in production.

### 3c. cchsflow variables.csv vs its own core schema

The cchsflow core `variables.yaml` is minimal (only `expected_column_order` and `id_column_name`). The actual CSV matches the schema column order exactly (18 columns, identical sequence).

---

## 4. recEnd vocabulary diff

### recEnd values in production

| Pattern | recodeflow tester | cchsflow production | recodeflow archive schema |
|---------|:-----------------:|:-------------------:|:------------------------:|
| Integer codes (`1`, `2`, ...) | YES | 2,094 rows | YES |
| Decimal values (`1.5`, `2.75`) | YES | 87 rows | YES |
| `NA::a` (valid skip) | YES | 515 rows | YES |
| `NA::b` (don't know/refusal) | YES | 980 rows | YES |
| `Func::function_name` | `Func::example_der_fun` | 64 rows | YES |
| `copy` | YES | 99 rows | YES |
| `N/A` (not applicable) | — | 0 rows | YES (schema allows) |
| `number+` (e.g., `5+`) | — | 0 rows | YES (schema lists) |
| Text strings (`m`, `f`) | YES (`m`, `f` for sex) | 0 rows | — |
| Wrong-case `Na::b` | YES (1 occurrence) | 0 rows | — |

**Notable divergences:**
1. **Text-based recEnd**: recodeflow tester contains `m` and `f` as recEnd values (for sex variable). cchsflow uses integer codes (1, 2) for all categorical variables. The recodeflow engine (`recode_with_table`) supports string values; cchsflow does not use this feature.
2. **`Na::b` (wrong case)**: The recodeflow tester CSV contains `Na::b` (capital N, lowercase a) in the `agegrp5` variable. This is a data quality issue in the test fixture; the canonical form is `NA::b`. The archive schema only validates `NA::` (capital). cchsflow production has no such error.
3. **`N/A` as recEnd**: The recodeflow archive schema explicitly documents this for "not applicable" rows, but cchsflow has 0 occurrences. cchsflow uses `N/A` only in recStart (for derived variable / Func rows).
4. **`copy` in both recStart and recEnd**: cchsflow has 3 rows where both recStart and recEnd are `copy` (SMK_06C, SMK_09C, SMK_10C - continuous smoking variables). This is a cchsflow-specific pattern for pass-through continuous variables. recodeflow documentation mentions `copy` only as a recEnd value paired with `else` in recStart.

---

## 5. recStart vocabulary diff

### recStart patterns

| Pattern | recodeflow tester | cchsflow production | recodeflow archive schema |
|---------|:-----------------:|:-------------------:|:------------------------:|
| Integer values | YES | 2,278 rows | YES |
| Decimal values | YES | 27 rows | YES |
| Closed intervals `[a,b]` | YES | 690 rows | YES |
| Half-open `[a,b)` or `(a,b]` | — | 150 rows | YES |
| Open intervals `(a,b)` | — | 0 rows | YES |
| Complex negative intervals `[-a,b)` | — | YES (HUI variables) | YES |
| `NA::a` / `NA::b` | — | 12 rows | YES |
| `else` | YES | 455 rows | YES |
| `N/A` | — | 224 rows | YES |
| Text strings (`m`, `f`) | YES | 0 rows | YES (schema allows) |
| Incomplete range `[20,` | — | 0 rows | YES (documented as data quality issue) |
| `***` | — | 0 rows | YES (documented) |
| `copy` | — | 3 rows | — (not in schema) |

**Notable divergences:**
1. **`copy` as recStart**: cchsflow uses `copy` in both recEnd and recStart for 3 continuous variables. This is undocumented in recodeflow's schema. The engine code in `recode_with_table.R` does handle `copy` in recEnd but the recStart=`copy` case is not explicitly described in any schema.
2. **`N/A` in recStart**: 224 cchsflow rows use `N/A` in recStart for derived variables (Func:: rows). The recodeflow archive schema documents this as valid. The cchsflow core schema also documents it.
3. **Text-based recStart** (`m`, `f`): Supported in recodeflow tester; absent in cchsflow.
4. **Interval completeness**: cchsflow has zero open intervals `(a,b)` and zero malformed intervals. The recodeflow archive schema documents incomplete/malformed patterns as "data quality issues found in real validation" — these existed in older cchsflow data and have since been cleaned.

---

## 6. Validation approach diff

### recodeflow validation

Located in: `R/parse-variables-sheet.R` and `R/recode-with-table.R`

**Approach:** Programmatic runtime validation when data is loaded into `rec_with_table()`:
- `parse_variables_sheet()` validates the variables sheet at load time
- Required columns: only `variable` and `variableStart` are required
- Validates that derived variables (`DerivedVar::`) don't reference database columns directly
- Column presence check uses `checkmate::test_names()` with `must.include`
- No CSV formatting validation (line endings, quoting, column order, trailing empty columns)
- No schema file: validation rules are hardcoded in R functions
- Template variable expansion via `expand_template_variables()` before processing

**Validation errors produced:**
- `invalid_input_type`: if input is not a data.frame
- `missing_required_columns`: if `variable` or `variableStart` missing
- `invalid_dependency`: if derived variable references database::column directly

### cchsflow validation

Located in: `R/check-worksheet.R` and `R/load-schema.R`

**Approach:** Standalone CSV file validation via `check_worksheet()`:
- Reads schema from YAML files in `inst/metadata/schemas/core/`
- Validates the CSV file on disk, not the in-memory data frame
- Uses a custom CSV parser (character-by-character) to preserve raw formatting
- Schema-driven: expected columns and id column come from YAML

**Validation checks:**
1. File exists (returns `file_not_found` error if not)
2. Valid CSV (returns `invalid_csv` error if unparseable)
3. Column order: compares actual vs schema's `expected_column_order` positionally
4. Row sorting: rows must be sorted by the id column (`dummyVariable` for variable_details, `variable` for variables)
5. Trailing empty columns: detects Excel-artifact blank columns at right margin
6. Line endings: enforces LF only (no CRLF)
7. Excessive quoting: fields should only be quoted if they contain comma, quote, or newline

**Validation errors produced (named list with error_type):**
- `file_not_found`
- `invalid_csv`
- `column_order`
- `missing_id_column`
- `unsorted_rows`
- `empty_columns`
- `line_ending_crlf`
- `excessive_quoting`

### Key approach divergences

1. **When validation runs**: recodeflow validates at runtime when data is processed. cchsflow validates the CSV file as a pre-flight/CI check, separate from data processing.
2. **What is validated**: recodeflow validates logical structure (derived variable dependencies). cchsflow validates physical CSV formatting (column order, quoting, line endings, sorting). Neither repo validates field-level content (enum values, patterns) at this time.
3. **Schema externalization**: cchsflow externalizes schema to YAML files loaded at runtime. recodeflow hardcodes required columns in R code.
4. **Row identity**: cchsflow enforces alphabetical sorting by `dummyVariable` (variable_details) and `variable` (variables). recodeflow has no row ordering requirement.
5. **Template expansion**: recodeflow's `rec_with_table()` calls `expand_template_variables()` before processing - this is a pre-processing step not present in cchsflow's engine.

---

## 7. Summary: schema compatibility status

### Production CSV compatibility with recodeflow engine

cchsflow's production `variable_details.csv` is **compatible** with recodeflow's `rec_with_table()` engine because:
- All 16 core columns are present
- recodeflow only requires `variable` and `variableStart` at parse time
- `rec_with_table()` accesses columns by name (via `pkg.env$columns.*` constants), not position
- Extra cchsflow-only columns (`ICES.confirmation`, `review`, etc.) are ignored

The engine reads columns by the names in `strings.R`:
- `recTo` → `recEnd`
- `recFrom` → `recStart`
- `toType` → `typeEnd`
- `fromType` → `typeStart`

### Incompatibilities and risks

1. **Column name normalization**: `ICES confirmation` (recodeflow) vs `ICES.confirmation` (cchsflow, R-name-normalized). If recodeflow validation ever checked for `ICES confirmation` by name, the cchsflow CSV would fail. Currently not an issue since this field isn't in the required set.
2. **`harmonizationStatus` vs `status`**: Any tooling that reads the status field by name would need to know which repo's convention to use.
3. **`dummyVariable` not required in recodeflow**: recodeflow's tester files include `dummyVariable` but it's not a required column. cchsflow's `check_worksheet()` would flag it as missing if not present.
4. **Row sorting enforcement**: cchsflow's `check_worksheet()` requires alphabetical sort by `dummyVariable`. recodeflow has no such requirement. A recodeflow user's worksheet would fail cchsflow's validator unless sorted.
5. **Column order enforcement**: cchsflow's `check_worksheet()` enforces positional column order from YAML schema. The cchsflow production CSV itself doesn't match its own schema at position 6 (ICES.confirmation inserted before typeStart). This is an existing v3 inconsistency.
6. **`templateVariable` field**: Present in recodeflow's extension schema and processed by `expand_template_variables()`. Not in cchsflow's production CSV. If cchsflow wanted to use template variables, the column would need to be added.

---

## 8. Reference: column presence matrix

### variable_details.csv

| Column | rf tester | rf no-template | rf template | rf archive schema | cc CSV | cc core schema |
|--------|:---------:|:--------------:|:-----------:|:-----------------:|:------:|:--------------:|
| variable | Y | Y | Y | Y | Y | Y |
| dummyVariable | Y | N | N | Y | Y | Y |
| typeEnd | Y | Y | Y | Y | Y | Y |
| typeStart | Y | Y | Y | Y | Y | Y |
| databaseStart | Y | Y | Y | Y | Y | Y |
| variableStart | Y | Y | Y | Y | Y | Y |
| variableStartLabel | Y | Y | Y | Y | Y | Y |
| numValidCat | Y | Y | Y | Y | Y | Y |
| recEnd | Y | Y | Y | Y | Y | Y |
| catLabel | Y | Y | Y | Y | Y | Y |
| catLabelLong | Y | Y | Y | Y | Y | Y |
| units | Y | Y | Y | Y | Y | Y |
| recStart | Y | Y | Y | Y | Y | Y |
| catStartLabel | Y | Y | Y | Y | Y | Y |
| variableStartShortLabel | Y | N | N | Y | Y | Y |
| notes | Y | N | N | Y | Y | Y |
| templateVariable | N | N | Y | Y | N | Y (extension) |
| ICES.confirmation | N | N | N | Y (space name) | Y (pos 6) | N |
| version | N | N | N | Y | Y | Y |
| lastUpdated | N | N | N | Y | Y | Y |
| harmonizationStatus | N | N | N | Y | N | N |
| status | N | N | N | N | Y | Y |
| reviewNotes | N | N | N | Y | Y | Y |
| versionNotes | N | N | N | N | Y | N |
| review | N | N | N | N | Y | N |

### variables.csv

| Column | rf tester | rf archive schema | rf PR70 schema | cc CSV | cc core schema |
|--------|:---------:|:-----------------:|:--------------:|:------:|:--------------:|
| variable | Y | Y | Y | Y | Y |
| label | Y | Y | Y | Y | Y |
| labelLong | Y | Y | Y | Y | Y |
| variableType | Y | Y | Y | Y | Y |
| databaseStart | Y | Y | Y | Y | Y |
| variableStart | Y | Y | Y | Y | Y |
| subject | Y | Y | Y | Y | Y |
| section | Y | Y | Y | Y | Y |
| units | Y | Y | Y | Y | Y |
| notes | Y | N | Y | Y | N |
| description | Y | N | Y | Y | Y |
| version | N | Y | Y | Y | Y |
| lastUpdated | N | Y | Y | Y | Y |
| harmonizationStatus | N | Y | Y | N | N |
| status | N | N | N | Y | Y |
| reviewNotes | N | Y | Y | Y | Y |
| ICES.confirmation | N | Y (space) | N | Y (dot) | Y (dot) |
| Observation..MD. | N | Y (parens) | N | Y (dots) | Y (dots) |
| versionNotes | N | N | N | Y | Y |
| fileRowId | N | N | Y (proposed) | N | N |
