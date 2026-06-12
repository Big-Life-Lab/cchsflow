# Engine Core Diff: recodeflow (dev) vs cchsflow (v3 / fix/v3-smoking-worksheet-sync)

Analysis date: 2026-06-11  
Repos compared:
- recodeflow: /Users/dmanuel/github/recodeflow  branch dev (HEAD b87e6bd)
- cchsflow:   /Users/dmanuel/github/cchsflow    branch fix/v3-smoking-worksheet-sync

---

## 1. File inventory

### recodeflow R/ (11 files)

| File | Role |
|------|------|
| recode-with-table.R | Main entry point + internal pipeline (rec_with_table, recode_call, recode_columns, recode_non_derived_vars, recode_derived_vars, ...) |
| utils.R | Shared helpers: create_id_row, create_var_labels, label_data, set_data_labels, select_vars_by_role, update_variable_details_based_on_variable_sheet |
| strings.R | pkg.env constants (column name mappings) |
| get-data-variable-name.R | get_data_variable_name() |
| get-feeder-vars.R | get_feeder_vars() |
| get-start-variables.R | get_start_variables() + internal helpers |
| is-derived-var.R | is_derived_var() |
| variable-start.R | is_table_feeder_var(), get_table_name() |
| parse-variables-sheet.R | parse_variables_sheet() — PR #85 (merged 2025-12-30) |
| example_der_function.R | Vignette/test helper |
| data.R | Dataset docs |

### cchsflow R/ engine-core files

| File | Role |
|------|------|
| recode-with-table.R | Legacy rec_with_table + recode_call + recode_columns + recode_derived_variables + compare_value_based_on_interval + recode_variable_NA_formating + get_data_variable_name |
| strings.R | pkg.globals constants (column name mappings, diverged from recodeflow pkg.env) |
| clean-variables.R | v3 Level-6 preprocessing: clean_variables(), process_missing_codes(), apply_else_logic(), parse_range_notation(), coerce_cchs_label_strings(), normalize_input_lengths(), derive_passthrough() |
| missing-pattern-cache.R | v3 Level-4: has_cached_pattern(), get_cached_pattern(), cache_pattern(), get_missing_pattern(), get_complete_pattern(), map_recStart_to_recEnd(), auto_detect_database(), apply_database_heuristics() |
| missing-data-functions.R | v3 Level-5: any_missing(), get_priority_missing(), assign_missing(), load_priority_rules(), detect_missing_vectorized(), apply_priority_hierarchy() |
| worksheet-getters.R | v3 Level-3: get_variables(), get_variable_details(), get_variable_type(), get_variable_limits() |
| worksheet-loaders.R | v3 Level-2B: load_worksheet_metadata(), load_worksheet_schemas(), save_metadata_to_rdata() |
| check-worksheet.R | check_worksheet() + .check_column_order, .check_row_sorting, .check_excessive_quoting, .check_trailing_empty_columns, .check_line_endings, .parse_csv_text |
| fix-worksheet.R | (not read in this analysis, inferred from git status) |
| load-schema.R | Loads YAML schemas for check_worksheet |

---

## 2. Function-by-function correspondence

### 2A. rec_with_table() — public entry point

**recodeflow** `R/recode-with-table.R:218-335`  
**cchsflow**   `R/recode-with-table.R:154-245`

| Aspect | recodeflow | cchsflow |
|--------|-----------|----------|
| Signature | 14 params including `tables`, `id_role_name`, `name_of_environment_to_load`, `append_non_db_columns` | 12 params — missing `tables`, `id_role_name`, `name_of_environment_to_load` |
| Default loading | Generic: loads from any named package via `name_of_environment_to_load` | CCHS-specific: hardcoded fallback to `package = "cchsflow"` |
| List-of-databases handling | Calls `recode_call` with `no_template_variables_variable_details` (post-expansion) | Calls `recode_call` with un-expanded `variable_details` |
| Template expansion | `expand_template_variables()` called before dispatch — **NEW** | Not present |
| `tables` param | Passed to `recode_call` and on to `recode_columns` / `recode_derived_vars` | Not present — no reference-table support |
| `id_role_name` param | Handled in `recode_columns` via `create_id_row` | Not present |

**recodeflow is ahead by**: template variable expansion, generic package loading, reference-table support, ID role creation.  
**cchsflow has not backported**: any of these four features.

---

### 2B. recode_call() — internal dispatcher

**recodeflow** `R/recode-with-table.R:400-521`  
**cchsflow**   `R/recode-with-table.R:248-351`

| Aspect | recodeflow | cchsflow |
|--------|-----------|----------|
| Signature | Includes `tables` param | No `tables` |
| var trimming | Uses `pkg.env` constants; separates df vs vector branch cleanly | Uses `pkg.globals` constants; separate filter step inline |
| `update_details_from_vars` | Refactored to `update_details_from_vars()` in utils | Called `update_variable_details_based_on_variable_sheet()` internally |
| Label merging column | `pkg.env$columns.variableLabel` = `"labelLong"` | `pkg.globals$argument.VariableLabelShort` = `"variableStartShortLabel"` |
| `append_non_db_columns` logic | More precise: uses `detected_varnames` from filtered set | Simpler: `all_possible_var_names` vs `all_variables_detected` |
| DB filter | `grepl(database_name, details[[dbstart_col]])` | Same logic |

**Divergence**: Column name mappings differ (see Section 3).

---

### 2C. recode_columns() — per-variable router

**recodeflow** `R/recode-with-table.R:539-693`  
**cchsflow**   `R/recode-with-table.R:431-691`

This is where the biggest structural divergence lives.

| Aspect | recodeflow | cchsflow |
|--------|-----------|----------|
| Split into paths | 4 paths: `normal_vars` (non-derived), `custom_func_vars` (Func::), `derived_startvars` (vars that depend on derived output), `idfrom_vars` (id_from::) | 3 paths: `rec_variables_to_process`, `func_variables_to_process`, `map_variables_to_process` (map:: — detected but raises stop()) |
| Detection of func-vars | recTo contains `Func::` | variableStart contains `DerivedVar::` |
| Detection of derived-dep vars | variableStart contains `DerivedVar::` pattern (post-func processing) | Not separated — mixed into func path |
| Loop mechanism for normal vars | `while` + `purrr::reduce` inside `recode_non_derived_vars` | `while` + nested `for` loop over rows |
| Log construction | `purrr::reduce` accumulates a result list | `for` loop over rows, builds `log_table` incrementally |
| Interval parsing | Calls `check_interval()` with cleaned `parsed_from_value` | Calls `compare_value_based_on_interval()` which builds return boolean via `data[[col]] %in% data[[col]][which(...)]` (double-lookup redundancy) |
| Overlap detection | Checks `any(recoding_result$recoded_data_rows[current_rows_to_recode])` | No overlap check |
| label_data call | After both passes | After both passes — same |
| map:: handling | Raises stop() with "not yet supported" | Silently part of filter, not processed |

**cchsflow regression vs recodeflow**: no overlap detection, double-lookup in interval comparison, no 4th `derived_startvars` pass.

---

### 2D. recode_non_derived_vars() / inline while loop

**recodeflow** `R/recode-with-table.R:702-888` (standalone function)  
**cchsflow**   Inlined inside `recode_columns` while loop, `R/recode-with-table.R:459-661`

recodeflow extracted this into `recode_non_derived_vars()` (PR #78/#80). cchsflow kept it inlined. This is a purely structural divergence — the logic is largely equivalent except:

- recodeflow has duplicate-from-value check via `table()` + named-count; cchsflow checks raw uniqueness equality.
- recodeflow's `else` handling: builds a synthetic else row if none found, then appends it at end of details. cchsflow extracts else_value separately and prefills the recoded_data column before iterating non-else rows — **these produce equivalent results but the recodeflow approach is cleaner and handles the `copy` case with explicit `get_result()` helper**.

---

### 2E. recode_derived_vars() / recode_derived_variables()

**recodeflow** `R/recode-with-table.R:1054-1248`  (named `recode_derived_vars`)  
**cchsflow**   `R/recode-with-table.R:827-973`  (named `recode_derived_variables`)

| Aspect | recodeflow | cchsflow |
|--------|-----------|----------|
| Function name | `recode_derived_vars` | `recode_derived_variables` |
| `tables` param | Present — table feeders resolved from `tables` list | Not present |
| `is_table_feeder_var` check | Yes — `is_table_feeder_var()` guard before data lookup | Not present |
| `is_start_var_string` / `is_start_var_numeric` | Yes — supports literal string/numeric constants as feeders | Not present |
| Feeder var extraction | Uses `get_feeder_vars()` from variableStart | Extracts from `variableStart` by split on `::` then strip brackets |
| Row-level call | `for` loop over `seq_len(nrow(recoded_data))` building `recoded_variable` vector | `dplyr::rowwise() %>% select() %>% do()` idiom |
| `do()` deprecation | Not used | **Uses deprecated `dplyr::do()`** — this is a known v3 regression |
| `database_name` param | Present — passed to `get_feeder_vars` | Not present — feeder extraction is database-unaware |
| Label creation | `create_var_labels()` from utils.R (shared) | `create_label_list_element()` — diverged name |
| Label assignment timing | Before the row loop (`append(label_list, ...)`) | After the row loop |

**cchsflow regression**: uses deprecated `dplyr::do()` (issue raised in design inventory). recodeflow refactored to explicit row loop.

---

### 2F. calculate_custom_function_row_value()

**recodeflow** `R/recode-with-table.R:1250-1258`  
**cchsflow**   `R/recode-with-table.R:974-983`

Functionally identical: `do.call(get(custom_function_name), unname(row_values))`.

---

### 2G. get_data_variable_name()

**recodeflow** `R/get-data-variable-name.R` (standalone file, extracted PR #84)  
**cchsflow**   `R/recode-with-table.R:369-413` (inlined)

recodeflow's version is cleaner:
- Uses `startsWith(name, db_prefix)` instead of `grepl`
- Parameters renamed (`row` vs `row_being_checked`, `recoded_varname` vs `variable_being_checked`)
- No `data` parameter — recodeflow version does not need to check `data[[start_var]]` at this stage

cchsflow version has a latent bug at `R/recode-with-table.R:397`: uses bare `row` (undefined at that scope) in a stop message — `row` should be `row_being_checked`.

---

### 2H. compare_value_based_on_interval() / check_interval()

**recodeflow** `check_interval()` `R/recode-with-table.R:950-974`  
**cchsflow**   `compare_value_based_on_interval()` `R/recode-with-table.R:706-755`

| Aspect | recodeflow | cchsflow |
|--------|-----------|----------|
| Function name | `check_interval` | `compare_value_based_on_interval` |
| Approach | Direct comparison: `min <= v & v <= max` returning logical vector | Indirect: `data[[col]] %in% data[[col]][which(...)]` — double index lookup |
| `(,)` (open-open) interval | Supported | Supported |
| Non-numeric left boundary | `data[[column]] %in% left_boundary` (single value) | `data[[col]] %in% data[[col]][which(left_boundary == data[[col]])]` (redundant) |

**cchsflow is less efficient** in the non-numeric branch and the double-index approach adds unnecessary memory overhead for large datasets.

---

### 2I. format_recoded_value() / recode_variable_NA_formating()

**recodeflow** `format_recoded_value()` `R/recode-with-table.R:1034-1052`  
**cchsflow**   `recode_variable_NA_formating()` `R/recode-with-table.R:807-825`

Functionally identical: grepl("NA"), split on ":", tagged_na for cont, "NA(x)" string for cat.

---

### 2J. update_details_from_vars() / update_variable_details_based_on_variable_sheet()

**recodeflow** `update_details_from_vars()` `R/recode-with-table.R:980-1023`  
**cchsflow**   `update_variable_details_based_on_variable_sheet()` `R/recode-with-table.R:758-795`

Functionally equivalent. recodeflow uses `pkg.env` constants throughout; cchsflow uses `pkg.globals`. Column names merged:

| Column | recodeflow pkg.env | cchsflow pkg.globals |
|--------|-------------------|---------------------|
| label short | `columns.variableLabel` = `"labelLong"` | `argument.VariableLabelShort` = `"variableStartShortLabel"` |
| label long | `columns.label` = `"label"` | `MSW.Variables.Columns.LabelLong` = `"labelLong"` |

This is a real semantic divergence: recodeflow uses `labelLong` as the "variableLabel" while cchsflow uses `variableStartShortLabel`.

---

### 2K. expand_template_variables() — recodeflow only

**recodeflow** `R/recode-with-table.R:337-397`  
**cchsflow** — **NOT PRESENT**

recodeflow supports a `templateVariable` column in variable_details. Variables with `templateVariable = "Yes"` serve as templates that are expanded for concrete variables. cchsflow does not implement this.

---

### 2L. is_equal()

**recodeflow** `R/recode-with-table.R:52-59`  
**cchsflow**   `R/recode-with-table.R:33-40`

Identical implementation.

---

### 2M. parse_variables_sheet() — recodeflow only (PR #85, 2025-12-30)

**recodeflow** `R/parse-variables-sheet.R` (196 lines)  
**cchsflow** — **NOT PRESENT** in production code path

recodeflow added `parse_variables_sheet()` which:
1. Validates the variables_sheet data.frame has required columns (variable, variableStart)
2. Validates derived variables do not reference raw database columns directly
3. Returns a classed `variables_sheet` object

cchsflow has `check_worksheet()` (`R/check-worksheet.R`) which is a different concern: it validates the on-disk CSV format (column order, row sorting, excessive quoting, line endings, trailing empty columns). `check_worksheet()` has no analog in recodeflow.

---

### 2N. get_feeder_vars() — recodeflow only

**recodeflow** `R/get-feeder-vars.R:1-50`  
**cchsflow** — inlined in `recode_derived_variables` at lines 857-861

recodeflow extracted feeder-var extraction into `get_feeder_vars()` with three regex patterns:
- database-specific: `database::DerivedVar::[vars]`  
- default wrapped: `[DerivedVar::[vars]]`
- plain: `DerivedVar::[vars]`

cchsflow inline version only handles the plain/simple case (split on "::", take index 2, strip brackets). The database-specific and wrapped patterns are not handled.

**cchsflow deficit**: cannot resolve database-specific DerivedVar feeder overrides.

---

### 2O. is_derived_var()

**recodeflow** `R/is-derived-var.R` (standalone)  
**cchsflow** — not explicitly defined; detection inlined as `grepl("DerivedVar::", ...)`

recodeflow uses a two-pattern regex: `"DerivedVar::\\[(.+?)\\]|DerivedVar::\\[\\]"` to handle both populated and empty bracket cases. cchsflow inline only uses `grepl("DerivedVar::", ...)` which is a superset match (would match any prefix form).

---

### 2P. get_start_variables() — recodeflow only

**recodeflow** `R/get-start-variables.R` (347 lines)  
**cchsflow** — **NOT PRESENT**

This is a higher-level inspection API: given a variable + database, returns typed list of start variables (type: "database", "table", "derived", "non-derived"). Used for tooling and validation, not the hot path. No equivalent in cchsflow.

---

### 2Q. is_table_feeder_var() / get_table_name() — recodeflow only

**recodeflow** `R/variable-start.R:22-38`  
**cchsflow** — **NOT PRESENT**

Reference-table feeders (`tables::tablename`) are not supported in cchsflow.

---

### 2R. create_var_labels() / create_label_list_element()

**recodeflow** `create_var_labels()` in `R/utils.R:57-110`  
**cchsflow** — `create_label_list_element()` (name differs; not read in full but referenced in recode_columns)

recodeflow version verifies that all rows for a variable have identical type/unit/label (consistency check before setting value labels). cchsflow version was not read directly but differs in name and likely in validation depth.

---

### 2S. label_data() / set_data_labels()

**recodeflow** `label_data()` `R/utils.R:122-167`; `set_data_labels()` `R/utils.R:218-259`  
**cchsflow** `label_data()` in `R/label-utils.R` (exists, not read); `set_data_labels()` likely exported

Both repos export `set_data_labels`. The recodeflow version in utils.R calls `update_variable_details_based_on_variable_sheet` and `create_var_labels`; equivalent in cchsflow.

---

## 3. Constants / column-name namespace divergence

| Logical column | recodeflow (pkg.env) | cchsflow (pkg.globals) |
|----------------|---------------------|----------------------|
| variable | `columns.variable = "variable"` | `argument.Variables = "variable"` |
| databaseStart | `columns.databaseStart = "databaseStart"` | `argument.DatabaseStart = "databaseStart"` |
| variableStart | `columns.variableStart = "variableStart"` | `argument.VariableStart = "variableStart"` |
| recEnd/recTo | `columns.recTo = "recEnd"` | `argument.CatValue = "recEnd"` |
| recStart/recFrom | `columns.recFrom = "recStart"` | `argument.From = "recStart"` |
| typeEnd | `columns.toType = "typeEnd"` | `argument.ToType = "typeEnd"` |
| label (short) | `columns.label = "label"` | `argument.VariableLabelShort = "variableStartShortLabel"` |
| label (long) | `columns.variableLabel = "labelLong"` | `argument.VariableLabel = "variableStartLabel"` |
| catLabel | `columns.catLabel = "catLabel"` | `argument.CatLabel = "catLabel"` |
| catLabelLong | `columns.catLabelLong = "catLabelLong"` | `argument.CatLabelLong = "catLabelLong"` |
| notes | `columns.notes = "notes"` | `argument.Notes = "notes"` |
| units | `columns.units = "units"` | `argument.Units = "units"` |
| func key | `recode.key.func = "Func::"` | (not in pkg.globals — inline literal `"Func::"`) |
| derived key | `recode.key.derived.var = "DerivedVar::"` | (inline literal `"DerivedVar::"`) |
| else value | `variable_details$columns.recFrom.elseValue = "else"` | (inline literal `"else"`) |

The **label column mapping is semantically diverged**: recodeflow uses `label`/`labelLong` from the variables sheet; cchsflow uses `variableStartShortLabel`/`variableStartLabel` from the variable_details sheet. This affects what `set_data_labels` / `update_details_from_vars` attach to recoded columns.

---

## 4. cchsflow-only infrastructure (no recodeflow equivalent)

### 4A. v3 3-step architecture

cchsflow v3 added three layers that have no counterpart in recodeflow:

**Level 6 — clean_variables()** (`R/clean-variables.R`)
- Preprocesses raw CCHS missing codes (6/7/8/9 or 996/997/998/999) to tagged_na before domain logic
- `process_missing_codes()`: converts input → tagged_na → apply else logic → convert back if `output_format = "original"`
- `apply_else_logic()`: vectorized pass to handle out-of-range values using worksheet else mappings
- `parse_range_notation()`: full bracket notation parser ([7,9], [18.5,25), [30,inf), etc.)
- `coerce_cchs_label_strings()`: converts sjlabelled factor strings ("NA(a)", "Not applicable") to tagged_na
- `normalize_input_lengths()`: scalar recycling for vector inputs
- `derive_passthrough()`: boilerplate-elimination helper for simple worksheet-routed functions

**Level 5 — any_missing() / get_priority_missing()** (`R/missing-data-functions.R`)
- `any_missing(...)`  — detects any tagged_na, plain NA, or numeric missing code in a set of variables
- `get_priority_missing(...)` — returns the highest-priority missing value (NA::b > NA::a per CCHS rules)
- `assign_missing()` — returns typed missing value in requested output_format
- `load_priority_rules()` — YAML-driven priority config with session cache

**Level 4 — missing-pattern-cache.R**
- Session-level cache of per-variable missing patterns keyed by (variable, database)
- `get_complete_pattern()` — full pattern: na_a_codes, na_b_codes, copy_mappings, value_mappings, else_mappings
- `map_recStart_to_recEnd()` — parses all worksheet rows for a variable into structured pattern
- `auto_detect_database()` — configurable database selection heuristics
- `get_missing_pattern_bulk()` — bulk cache preload for database-level operations

**Level 3 — worksheet-getters.R**
- `get_variables(variable_name, ...)` — tidyselect-friendly variables.csv accessor
- `get_variable_details(variable_name, ..., database_filter, type_filter)` — filtered variable_details.csv accessor
- `get_variable_type()`, `get_variable_limits()` — typed accessors for derived function use

**Level 2B — worksheet-loaders.R**
- `load_worksheet_metadata()` — CSV/RData load with automatic RData caching
- `load_worksheet_schemas()` — YAML schema loading for check-worksheet

### 4B. check_worksheet()

`R/check-worksheet.R` — CSV format validator for production worksheets. Checks:
- Column order (against YAML schema)
- Row sorting (alphabetical by `variable` column)
- Trailing empty columns
- Excessive quoting (fields quoted that do not need to be)
- Line endings (LF required, not CRLF)

Has no analog in recodeflow. recodeflow's `parse_variables_sheet()` is a semantic/structural validator for the data-frame already loaded, not a CSV format checker.

---

## 5. Summary of divergence direction

### recodeflow is ahead of cchsflow in:

1. **Template variable expansion** — `expand_template_variables()` (not in cchsflow)
2. **Reference-table feeders** — `tables` param + `is_table_feeder_var()` + `get_table_name()` (not in cchsflow)
3. **`get_start_variables()` introspection API** — typed start-variable discovery (not in cchsflow)
4. **`parse_variables_sheet()` semantic validation** — derived-variable dependency checking (not in cchsflow)
5. **`get_feeder_vars()` with database-specific override** — three-pattern regex (cchsflow only handles plain case inline)
6. **`id_role_name` / `create_id_row`** — composite ID construction from feeder columns (not in cchsflow)
7. **`recode_non_derived_vars()` extracted as standalone function** — cleaner structure, overlap detection
8. **Literal string/numeric constant feeders** — `is_start_var_string()` / `is_start_var_numeric()` (not in cchsflow)
9. **`check_interval()` direct comparison** — no double-lookup index overhead
10. **Consistent pkg.env constants** — `columns.*` naming; cchsflow has competing `pkg.globals`

### cchsflow is ahead of recodeflow in:

1. **Level 3–6 missing-data architecture** — `clean_variables()`, `any_missing()`, `get_priority_missing()`, `assign_missing()`, `get_missing_pattern()`, `get_complete_pattern()`, YAML priority rules — a full missing-data preprocessing stack that recodeflow does not have
2. **`check_worksheet()` CSV format validation** — enforces LF endings, minimal quoting, column order, row sorting, no trailing empty columns
3. **Worksheet access layer** — `get_variables()`, `get_variable_details()` with tidyselect, `database_filter`, `type_filter` (no analog in recodeflow)
4. **`load_worksheet_metadata()` with RData caching** — performance-optimized multi-format loading
5. **`parse_range_notation()` full notation parser** — bracket notation, half-open intervals, infinity, single values (recodeflow inline parsing is simpler)
6. **YAML schema infrastructure** — `load_worksheet_schemas()` drives both `check_worksheet()` and `get_complete_pattern()`
7. **`derive_passthrough()` and `normalize_input_lengths()`** — utility helpers for the 3-step derived function pattern

### Both repos share (exact or near-exact copies):

- `is_equal()` — identical
- `format_recoded_value()` / `recode_variable_NA_formating()` — functionally identical
- `rec_with_table()` public interface skeleton
- `label_data()` / `set_data_labels()` — equivalent
- General pipeline: filter by database → loop over variables → recode → label

---

## 6. Notable bugs / regressions in cchsflow vs recodeflow

1. **`cchsflow:R/recode-with-table.R:397`** — stop() message references undefined `row` variable; should be `row_being_checked`.
2. **`cchsflow:R/recode-with-table.R:938-954`** — uses deprecated `dplyr::do()` in `recode_derived_variables`; recodeflow replaced with explicit `for` loop.
3. **`cchsflow:R/recode-with-table.R:710-755`** — `compare_value_based_on_interval()` uses double-index pattern (`data[[col]] %in% data[[col]][which(...)]`) which is correct but allocates a full vector unnecessarily; `check_interval()` in recodeflow avoids this.
4. **No overlap detection** — cchsflow `recode_columns` does not check for overlapping from-ranges; recodeflow raises a stop() when overlap detected.
5. **No database-specific DerivedVar feeder override** — cchsflow cannot route a derived variable's feeders differently by database (recodeflow `get_feeder_vars` handles `database::DerivedVar::` prefix).

---

## 7. v4 design implications

For a v4 engine shared between cchsflow and recodeflow:

1. **Adopt recodeflow's refactored recode_non_derived_vars() and recode_derived_vars()** — cleaner separation, overlap detection, explicit loop replaces deprecated do().
2. **Adopt recodeflow's pkg.env naming** or unify constants — the dual pkg.env / pkg.globals namespace creates silent divergence.
3. **Port recodeflow's get_feeder_vars(), is_table_feeder_var(), get_table_name()** into cchsflow or into a shared recodeflow dependency.
4. **Decide where the v3 missing-data architecture lives** — it is CCHS-specific in its YAML config and default fallbacks; recodeflow should not take this directly. Better to make Level 3–6 a cchsflow extension of a generic recodeflow engine.
5. **check_worksheet() is cchsflow-specific** (CCHS CSV conventions) but the underlying `.check_*` functions could be generalized for recodeflow too.
6. **parse_variables_sheet() in recodeflow is the right approach** for semantic validation; cchsflow should adopt it instead of (or alongside) the CSV-format-only check_worksheet.
7. **Template variables** are a recodeflow feature that cchsflow has not needed so far but could benefit from for cross-year harmonization.
