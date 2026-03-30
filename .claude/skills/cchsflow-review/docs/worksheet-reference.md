# Understanding cchsflow worksheets

A reference for reading, writing, and validating the cchsflow harmonisation worksheets. This document covers both the CCHS survey context needed to judge whether a mapping is *correct* and the cchsflow worksheet mechanics needed to judge whether a mapping is *well-formed*.

**Audience:** Human contributors, LLM reviewers (Claude Code, Gemini/NotebookLM), and anyone who needs to understand how cchsflow encodes variable harmonisation.

**Design spec:** `docs/superpowers/specs/2026-03-26-worksheet-reference-design.md`

------------------------------------------------------------------------

## Part 1: CCHS foundations

This section provides just enough survey context for a reviewer to understand what cchsflow is harmonising. For comprehensive CCHS terminology, see the [cchsflow-docs glossary](https://github.com/Big-Life-Lab/cchsflow-docs/blob/main/docs/glossary.md).

### What is the CCHS?

The **Canadian Community Health Survey** (CCHS) is a national cross-sectional health survey conducted by Statistics Canada. It collects data on health status, healthcare utilisation, and health determinants from approximately 65,000 respondents per cycle. The survey has run annually since 2001, with over 20 years of data now available.

cchsflow harmonises variables across these cycles so that researchers can pool or compare data longitudinally despite changes in variable names, response categories, and questionnaire structure.

### File types

Statistics Canada releases CCHS data in several file types:

| File type | Access | Content | cchsflow suffix |
|----------------|----------------|----------------|-------------------------|
| **PUMF** (Public Use Microdata File) | Public download | Grouped/suppressed values for privacy | `_p` |
| **Master** | Restricted (Research Data Centres) | Exact values, full variable set | `_m` |
| **Share** | Deprecated | Legacy public-use subset | `_s` (convert to `_m`) |

**Key difference for harmonisation:** PUMF files often group continuous variables into categories (e.g., exact age → age groups) and may suppress rare values. Master files retain exact values. This means the same conceptual variable may need different recode rules for PUMF and Master — which is why cchsflow worksheets use separate blocks for `_p` and `_m` databases.

#### The PUMF-Master variable family pattern

**Key rule: If a continuous measure exists on Master, always expect only a categorical (grouped) version on PUMF.** This is not occasional — it is systematic across CCHS. Every continuous demographic, health behaviour, and health outcome variable on PUMF is grouped into categories for privacy protection.

A single health concept (e.g., "respondent age") therefore typically requires a **family** of harmonized variables in cchsflow:

| Variable | Type | File type | Worksheet pattern |
|----------|------|-----------|-------------------|
| `DHH_AGE` | Continuous passthrough | Master only | `[12,102]→copy` |
| `DHHGAGE_B`\* | Categorical (grouped bins) | PUMF only | `1→1, 2→2, ...16→16` |
| `DHHGAGE_cont` | Midpoint imputation | PUMF (+ Master passthrough) | `1→13, 2→16, ...` |

\* `DHHGAGE_B` is a **StatCan-assigned name** — the `_B` denotes the 2005+ category structure (16 age groups), not the cchsflow era-split convention. `DHHGAGE_A` (15 age groups, 2001-2003) is similarly a StatCan name. These are candidates for year-based renaming (e.g., `DHHGAGE_pre2005`, `DHHGAGE_2005plus`) but are out of scope unless being refactored.

The `_cont` suffix convention bridges PUMF categorical data to pseudo-continuous values via midpoint imputation. For Master data, the `_cont` variable typically passes through the true continuous value unchanged. This pattern applies broadly — smoking duration, consumption frequency, BMI, income, and most other continuous measures follow it.

**Common errors from not understanding this pattern:**

- Adding PUMF databases to a Master-only continuous variable (e.g., putting `_p` databases on `DHH_AGE`'s copy row — PUMF has no single-year age)
- Missing the `_cont` bridging variable when adding a new continuous measure
- Assuming a DerivedVar feeder (e.g., `DHH_AGE` in `pack_years_der`) works on PUMF when it's Master-only — the PUMF pipeline needs to use `DHHGAGE_cont` instead
- Creating a continuous Master variable without a corresponding PUMF categorical variable, leaving PUMF users with no access to the measure

**When reviewing or authoring:** For any continuous variable, check that the worksheet has the full family: Master continuous, PUMF categorical, and `_cont` bridge. Missing any piece means incomplete coverage for that file type.

### Cycle naming

| Era | Naming | Examples |
|------------------|--------------------------|-----------------------------|
| Early cycles (2001-2005) | Single-year, labelled as "Cycle N.1" | 2001 (Cycle 1.1), 2003 (Cycle 2.1), 2005 (Cycle 3.1) |
| Transition (2007) | First dual-year collection | 2007-2008 |
| Annual period (2008-2023) | Annual or dual-year | 2009-2010, 2011-2012, ..., 2022, 2023 |

**Exception:** The 2021 CCHS was not released as a standalone PUMF. It was combined with 2022 data into a 2021-2022 PUMF. Any `cchs2021_p` database reference is invalid in the current release structure.

### Database identifiers

cchsflow uses a consistent naming convention for databases:

```         
cchs{year}_{type}
```

- `cchs2001_p` — 2001 PUMF
- `cchs2007_2008_m` — 2007-2008 Master
- `cchs2022_p` — 2022 PUMF

The `_s` (share file) suffix is **deprecated**. Share files map to single-year Master databases: `cchs2009_s` → `cchs2009_m`, `cchs2010_s` → `cchs2010_m`, `cchs2012_s` → `cchs2012_m`.

**Single-year vs dual-year databases** are separate databases, not aliases. `cchs2009_m` and `cchs2009_2010_m` are distinct — some variables only appear in the single-year file, and Statistics Canada may drop those variables from the combined dual-year file. cchsflow supports both where available; when both exist, single-year databases are generally the primary focus.

### StatCan variable naming system

Statistics Canada uses systematic naming conventions to distinguish how a variable was measured and processed. Understanding these is essential for interpreting `variableStart` entries in the worksheets.

**Module prefixes** identify the survey section (e.g., `SMK` = Smoking, `ALC` = Alcohol, `GEN` = General Health, `DHH` = Demographics).

**Naming patterns within a module:**

| Pattern | Meaning | Example |
|------------------------|------------------------|------------------------|
| `MOD_NNN` | Base survey question — direct questionnaire response | `SMK_005` (Have you smoked 100 cigarettes?) |
| `MOD_NNNA` | Lettered sub-question | `SMK_09A` (When did you stop daily?) |
| `MODG_NNN` or `MODGNNN` | Grouped/categorical version | `SMKG005` (grouped smoking frequency) |
| `MODDXXX` | StatCan-derived variable (computed from others) | `SMKDSTY` (smoking status, derived) |
| `MOD_NNNC` | Continuous companion (exact values alongside categorical) | `SMK_09C` (exact years since quit) |

**Variables rename across cycles.** The same concept may have different names in different survey years due to questionnaire redesign:

- `SMK_09C` (2003-2014) → `SMK_090` (2015-2021) → `SPU_25` (2022-2023)
- `SMK_045` (pre-2022) → `CSS_25` (2022-2023)

This is why `variableStart` in the worksheets supports era-specific aliases — the harmonised variable needs to map to the correct source name for each cycle.

### PUMF vs Master: implications for harmonisation

| Aspect                | PUMF                          | Master                |
|--------------------------|--------------------|--------------------------|
| Continuous variables  | Often grouped into categories | Exact values retained |
| Rare values           | Suppressed or top-coded       | Present               |
| Variable availability | Subset of Master variables    | Full variable set     |
| Missing codes         | Same encoding (6, 7, 8, 9)    | Same encoding         |
| Derived variables     | May differ from Master        | StatCan-computed      |

**Harmonisation consequence:** A single cchsflow variable often requires separate worksheet blocks for PUMF and Master databases. For example, `SMKG09C` uses a direct recode block for PUMF (mapping categorical codes) and a range-based `[SMK_09C]` block for Master (grouping continuous years into categories).

------------------------------------------------------------------------

## Part 2: Worksheet schema — `variables.csv`

`variables.csv` is the **variable registry**. Each row defines one harmonised variable: its name, label, which databases contain it, and the source variable names used in each database.

**Current dimensions:** \~384 rows, 18 columns.

### Column reference

| Column | Type | Description |
|---------------------|------------------|----------------------------------|
| `variable` | text | **Primary key.** Harmonised variable name (e.g., `SMK_01A`, `DHHGAGE_cont`). Must be unique. |
| `label` | text | Short label (≤40 characters). Used in output datasets. |
| `labelLong` | text | Descriptive label. Human-readable explanation of the variable. |
| `variableType` | text | Output data type: `Categorical` or `Continuous`. |
| `databaseStart` | text | Comma-separated list of databases where this variable is available (e.g., `cchs2001_p, cchs2003_p, cchs2007_2008_m`). |
| `variableStart` | text | Comma-separated source variable names with optional `db::name` aliases for era-specific names (e.g., `cchs2003_p::SMKCG09C, cchs2005_p::SMKEG09C, SMKG09C`). Plain names apply to all unlisted databases. |
| `subject` | text | Domain classification (e.g., `Smoking`, `Physical Activity`, `Demographics`). |
| `section` | text | Sub-domain or module section. |
| `units` | text | Measurement units (e.g., `years`, `cigarettes/day`, `score`). |
| `notes` | text | Free-text notes about the variable. |
| `description` | text | Extended description of the variable's purpose and derivation. |
| `version` | text | Version when the variable was added or last modified (e.g., `3.0.0-alpha`). |
| `lastUpdated` | date | Date of last modification (YYYY-MM-DD). |
| `reviewNotes` | text | Notes from review process. |
| `ICES.confirmation` | text | ICES review status or confirmation notes. Temporary column for development. This will be depreciated on the final version 3.0 |
| `Observation..MD.` | text | Observations from MD (medical doctor) review. Temporary column for devcelopment. |
| `status` | text | Lifecycle state: `active`, `deprecated`, or `draft`. |
| `versionNotes` | text | Notes about version changes. |

### Key relationships

- `variable` in `variables.csv` is the foreign key referenced by `variable_details.csv`
- `databaseStart` must be a superset of all databases appearing in `variable_details.csv` for that variable
- `variableStart` names must match the source variable names used in `variable_details.csv` blocks

------------------------------------------------------------------------

## Part 3: Worksheet schema — `variable_details.csv`

`variable_details.csv` is the **recode specification**. Each row defines one mapping rule: for a given variable, in a given set of databases, map a source value to a target value. Rows group into blocks that collectively define how a variable is recoded for a set of databases.

**Current dimensions:** \~3,664 rows, 23 columns.

### Column reference

| Column | Type | Description |
|---------------------|------------------|----------------------------------|
| `variable` | text | **Foreign key** to `variables.csv`. The harmonised variable this row belongs to. |
| `dummyVariable` | text | Row identifier. Convention: `{variable}_{typeEnd}{numValidCat}_{sequence}` (e.g., `SMK_01A_cat2_1`). Values repeat across blocks for the same variable when different blocks produce the same output categories. See [naming conventions](#part-7-naming-conventions). |
| `typeEnd` | text | Output data type for this row: `cat` (categorical) or `cont` (continuous). |
| `databaseStart` | text | Comma-separated databases this row applies to. A database must appear in exactly one block for each variable. |
| `variableStart` | text | Source variable specification. Supports four notations (see below). |
| `ICES.confirmation` | text | ICES review confirmation. |
| `typeStart` | text | Source variable data type: `cat` or `cont`. Determines recode behaviour: `cat` sources use value mapping (including midpoint imputation); `cont` sources may use range-based binning or copy. A single variable can have blocks with different `typeStart` values for different databases (e.g., PUMF categorical vs Master continuous). |
| `recEnd` | text | **Target value** — the output of the recode. See special values below. |
| `numValidCat` | text | Number of valid (non-missing) categories. For `_cont` variables derived from categorical sources, this reflects the source category count, not the number of distinct continuous output values. |
| `catLabel` | text | Short category label for the `recEnd` value. |
| `catLabelLong` | text | Long category label. |
| `units` | text | Measurement units. |
| `recStart` | text | **Source value** — the input to be recoded. See special values below. |
| `catStartLabel` | text | Label for the source value (`recStart`). |
| `variableStartShortLabel` | text | Short label of the source variable. |
| `variableStartLabel` | text | Full label of the source variable. |
| `notes` | text | Free-text notes. |
| `version` | text | Version when added or modified. |
| `lastUpdated` | date | Date of last modification. |
| `status` | text | Lifecycle state: `active`, `deprecated`, `draft`. |
| `reviewNotes` | text | Review notes. |
| `versionNotes` | text | Notes about version changes. |
| `review` | text | Review status or reviewer. |

### `variableStart` notations

The `variableStart` column supports four distinct notations, each serving a different purpose:

**1. Plain name** — the source variable has the same name across all listed databases:

```         
SMKG09C
```

**2. Database-qualified alias** — era-specific source names when the variable was renamed across cycles:

```         
cchs2003_p::SMKCG09C, cchs2005_p::SMKEG09C, [SMKG09C]
```

This means: use `SMKCG09C` in `cchs2003_p`, `SMKEG09C` in `cchs2005_p`, and `SMKG09C` (resolved via bracket notation) in all other listed databases.

**3. Bracket notation** — `[VARIABLE_NAME]` resolves to whatever source name that variable uses in each database, as defined in `variables.csv`:

```         
[SMK_09C]
```

This is useful when the source variable itself has era-specific aliases defined in `variables.csv`. The bracket notation delegates name resolution to the variable registry rather than hard-coding names.

**4. DerivedVar** — inputs to an R function that computes the output:

```         
DerivedVar::[SMK_09A_cont, SMK_06A_cont]
```

The variables listed in brackets are passed as arguments to the function specified in the block's `recEnd=Func::function_name` row.

### `recStart` special values

| Value | Meaning | Example |
|---------------------|--------------------------|--------------------------|
| Integer or decimal | Literal source value to match | `1`, `2`, `6`, `3.5` |
| `[min,max)` | Half-open interval (includes min, excludes max) | `[3,6)` matches 3, 4, 5 |
| `[min,max]` | Closed interval (includes both endpoints) | `[11,82]` matches 11 through 82 |
| `else` | Catch-all: matches any value not matched by other rows | Maps unmatched values (typically to `NA::b`) |
| `N/A` | Not applicable — used in DerivedVar blocks where the R function handles all recoding | Always paired with DerivedVar `variableStart` |
| R-like expression | Conditional expression referencing input variables — used in DerivedVar output rows | `SMKDSTY_A in (3,5,6)`, `is.na(SMK_204)` |

### `recEnd` special values

| Value | Meaning | Example |
|---------------------|--------------------------|--------------------------|
| Integer or decimal | Target output value | `1`, `2`, `0.5`, `8` |
| `copy` | Pass-through: output equals input value unchanged | Used for continuous variables that need no transformation |
| `Func::function_name` | DerivedVar header: delegates recoding to the named R function | `Func::calculate_SMK_06A_cont` |
| `NA::a` | Not applicable — legitimate skip (e.g., non-smoker asked about quitting) | Maps source missing code `6` or equivalent |
| `NA::b` | Missing — refusal, don't know, or not stated | Maps source codes `7`, `8`, `9` (or `97`, `98`, `99`) |

------------------------------------------------------------------------

## Part 4: Block structure

Block structure is the most important concept for understanding how cchsflow worksheets work. A **block** is a group of contiguous rows in `variable_details.csv` that share the same `variable`, `variableStart`, and `databaseStart` values. Together, the rows in a block define the complete recode specification for that variable in those databases.

### Block types

#### Direct recode blocks

The most common type. Each row maps a `recStart` value to a `recEnd` value. `rec_with_table()` applies these mappings directly.

```         
variable    variableStart    databaseStart           recStart  recEnd
SMK_01A     [SMK_01A]        cchs2001_p, cchs2003_p  1         1
SMK_01A     [SMK_01A]        cchs2001_p, cchs2003_p  2         2
SMK_01A     [SMK_01A]        cchs2001_p, cchs2003_p  6         NA::a
SMK_01A     [SMK_01A]        cchs2001_p, cchs2003_p  [7,9]     NA::b
SMK_01A     [SMK_01A]        cchs2001_p, cchs2003_p  else      NA::b
```

The block above says: for `SMK_01A` in `cchs2001_p` and `cchs2003_p`, take the harmonised source `SMK_01A` (which maps to era-specific StatCan names via the full `variableStart` aliases), pass values 1 and 2 through, map 6 to not-applicable, and map everything else to missing.

#### DerivedVar blocks

Used when the recode logic requires computation that worksheets cannot express (multi-variable input, conditional branching, date arithmetic). The first row is a header with `Func::function_name` in `recEnd`; subsequent rows document the function's possible output values.

```         
variable       variableStart                                  recStart                          recEnd
cigs_per_day   DerivedVar::[SMK_204, SMK_208, SMKDSTY_A]     N/A                               Func::calculate_cigs_per_day
cigs_per_day   DerivedVar::[SMK_204, SMK_208, SMKDSTY_A]     [1,99]                            copy
cigs_per_day   DerivedVar::[SMK_204, SMK_208, SMKDSTY_A]     SMKDSTY_A in (3,5,6)              NA::a
cigs_per_day   DerivedVar::[SMK_204, SMK_208, SMKDSTY_A]     is.na(SMK_204) & is.na(SMK_208)   NA::b
cigs_per_day   DerivedVar::[SMK_204, SMK_208, SMKDSTY_A]     else                              NA::b
```

The `recEnd` values after the `Func::` row are **output documentation** — they describe the values the function can produce and conditions for missing values. This differs from direct recode blocks where `recEnd` *is* the target. Note that DerivedVar blocks may use conditional `recStart` expressions that reference input variables (e.g., `SMKDSTY_A in (3,5,6)`).

#### Range-based blocks

Map continuous source values to categorical or continuous targets using interval notation. Common for Master file variables where exact values are available.

```         
variable    variableStart  databaseStart                    recStart   recEnd
SMKG09C     [SMK_09C]      cchs2003_m, cchs2005_m, ...     [3,6)      1
SMKG09C     [SMK_09C]      cchs2003_m, cchs2005_m, ...     [6,11)     2
SMKG09C     [SMK_09C]      cchs2003_m, cchs2005_m, ...     [11,82]    3
SMKG09C     [SMK_09C]      cchs2003_m, cchs2005_m, ...     996        NA::a
SMKG09C     [SMK_09C]      cchs2003_m, cchs2005_m, ...     [997,999]  NA::b
SMKG09C     [SMK_09C]      cchs2003_m, cchs2005_m, ...     else       NA::b
```

This groups exact years-since-quit from Master's `SMK_09C` into three categories: 1 (3-5 years), 2 (6-10 years), 3 (11+ years).

#### Copy blocks

Pass source values through unchanged. Used for continuous variables that already have the correct scale.

```         
variable       variableStart  databaseStart                       recStart   recEnd
SMKG09C_cont   [SPU_25]       cchs2019_2020_p, cchs2022_p, ...   [0,121]    copy
SMKG09C_cont   [SPU_25]       cchs2019_2020_p, cchs2022_p, ...   996        NA::a
SMKG09C_cont   [SPU_25]       cchs2019_2020_p, cchs2022_p, ...   [997,999]  NA::b
```

### Multi-block variables

Most variables have more than one block because they need different recode logic for different databases. Common reasons:

1.  **PUMF vs Master split** — PUMF has categorical source, Master has continuous source. Each needs its own block with different `recStart` patterns.

2.  **Era-specific sources** — the source variable was renamed or restructured between cycles. Each era gets a block with the appropriate `variableStart`.

3.  **File type availability** — some variables exist only on Master or only on PUMF for certain cycles.

**Example:** `SMKG09C` has three blocks: - A direct recode block for older PUMF databases (categorical → categorical) - A range-based `[SMK_09C]` block for Master databases (continuous → categorical) - A direct recode block for recent PUMF databases (categorical → categorical, different source names)

### Block precedence

When `rec_with_table()` processes a variable for a specific database, it selects the block whose `databaseStart` includes that database. **Each database should appear in exactly one block per variable.** If a database appears in multiple blocks, the behaviour is undefined and likely indicates a worksheet error.

### The worksheet-first principle

Worksheet `recEnd` values are the **source of truth** for value mappings. R functions (`Func::` in DerivedVar blocks) should only be used when the recode logic genuinely cannot be expressed in worksheet rows:

- Multi-variable computation (combining inputs from several source variables)
- Conditional branching that depends on runtime values
- Date arithmetic or other calculations

Simple categorical-to-midpoint conversions belong in worksheet rows, not R code. The reference implementation for worksheet-only continuous variables is `DHHGAGE_cont`, which converts age groups to midpoints entirely through `recStart → recEnd` mappings with no R function.

**Anti-pattern:** The deleted `calculate_SMK_09A_cont()` function hard-coded midpoint values (0.5, 1.5, 2.5, 4.0) that duplicated the worksheet's own `recEnd` values. This redundancy created maintenance risk — changes to the worksheet would not propagate to the function, or vice versa.

### How DerivedVar blocks invoke R functions

When `rec_with_table()` encounters a DerivedVar block, it:

1. **Processes feeder variables first.** The variables listed in `DerivedVar::[var1, var2, ...]` are recursively processed through their own worksheet blocks before the function is called. The function receives already-harmonised values, not raw source data.
2. **Calls the function by position.** Arguments are passed in the order listed in `DerivedVar::[...]`, not by parameter name. This means the **count and order** of inputs must match the function's parameter count exactly.
3. **Operates row-wise.** The function is called once per row in the dataset.

**Practical constraints for writing DerivedVar blocks and functions:**

- A function with 2 parameters needs exactly 2 inputs in `DerivedVar::[a, b]`
- Reordering inputs in the DerivedVar list changes which value goes to which parameter
- The function can assume its inputs are clean harmonised values (not raw StatCan codes)

For detailed technical documentation of the `rec_with_table()` engine, see the cchsflow-review skill.

------------------------------------------------------------------------

## Part 5: Recode patterns

This section shows real examples from the current worksheets. Each illustrates a distinct pattern with the actual rows used by `rec_with_table()`.

### Pattern 1: Simple categorical passthrough

**Variable:** `SMK_01A` (ever smoked a whole cigarette)

The simplest pattern — source values map directly to the same output values.

| variable | variableStart | databaseStart (abbreviated) | recStart | recEnd |
|-------------|-------------|-----------------------|-------------|-------------|
| SMK_01A | cchs2001_p::SMKA_01A, ..., \[SMK_01A\] | cchs2001_m, cchs2001_p, ... | 1 | 1 |
| SMK_01A | cchs2001_p::SMKA_01A, ..., \[SMK_01A\] | cchs2001_m, cchs2001_p, ... | 2 | 2 |
| SMK_01A | cchs2001_p::SMKA_01A, ..., \[SMK_01A\] | cchs2001_m, cchs2001_p, ... | 6 | NA::a |
| SMK_01A | cchs2001_p::SMKA_01A, ..., \[SMK_01A\] | cchs2001_m, cchs2001_p, ... | \[7,9\] | NA::b |
| SMK_01A | cchs2001_p::SMKA_01A, ..., \[SMK_01A\] | cchs2001_m, cchs2001_p, ... | else | NA::b |

**What makes it distinctive:** Values 1 and 2 pass through unchanged. The `variableStart` uses era-qualified aliases (e.g., `cchs2001_p::SMKA_01A`) for cycles where StatCan renamed the source, with `[SMK_01A]` as the default for unlisted databases. Source missing codes (6 = not applicable, 7-9 = refusal/don't know/not stated) are mapped to `NA::a` and `NA::b`. The `else` row catches any unexpected values.

### Pattern 2: Era-specific source names

**Variable:** `SMKG09C` (years since quit, grouped — former daily smoker)

Different CCHS cycles use different variable names for the same concept.

| variable | variableStart | databaseStart (abbreviated) | recStart | recEnd |
|-------------|-------------|-----------------------|-------------|-------------|
| SMKG09C | cchs2003_p::SMKCG09C, cchs2005_p::SMKEG09C, cchs2015_2016_p::SMKG090, cchs2017_2018_p::SMKG090, \[SMKG09C\] | cchs2003_p, cchs2005_p, cchs2007_2008_p, ... | 1 | 1 |
| SMKG09C | (same) | (same) | 2 | 2 |
| SMKG09C | (same) | (same) | 3 | 3 |
| SMKG09C | (same) | (same) | 6 | NA::a |
| SMKG09C | (same) | (same) | \[7,9\] | NA::b |
| SMKG09C | (same) | (same) | else | NA::b |

**What makes it distinctive:** The `variableStart` field maps specific databases to their era-specific source names: `SMKCG09C` in 2003, `SMKEG09C` in 2005, `SMKG090` in 2015-2018, and `SMKG09C` (via bracket resolution) for all other listed databases.

### Pattern 3: Categorical to continuous midpoint (worksheet-only)

**Variable:** `SMK_09A_cont` (years since stopped daily, midpoint-imputed)

Categorical source values are mapped to continuous midpoint values entirely through worksheet `recEnd` values — no R function needed.

| variable | variableStart | databaseStart (abbreviated) | recStart | recEnd |
|-------------|-------------|-----------------------|-------------|-------------|
| SMK_09A_cont | cchs2003_p::SMKC_09A, ..., \[SMK_09A\] | cchs2003_p, cchs2005_p, ... | 1 | 0.5 |
| SMK_09A_cont | cchs2003_p::SMKC_09A, ..., \[SMK_09A\] | cchs2003_p, cchs2005_p, ... | 2 | 1.5 |
| SMK_09A_cont | cchs2003_p::SMKC_09A, ..., \[SMK_09A\] | cchs2003_p, cchs2005_p, ... | 3 | 2.5 |
| SMK_09A_cont | cchs2003_p::SMKC_09A, ..., \[SMK_09A\] | cchs2003_p, cchs2005_p, ... | 4 | 4 |
| SMK_09A_cont | cchs2003_p::SMKC_09A, ..., \[SMK_09A\] | cchs2003_p, cchs2005_p, ... | 6 | NA::a |
| SMK_09A_cont | cchs2003_p::SMKC_09A, ..., \[SMK_09A\] | cchs2003_p, cchs2005_p, ... | \[7,9\] | NA::b |

**What makes it distinctive:** This is the **worksheet-first principle** in action. The midpoint values (0.5, 1.5, 2.5, 4.0) are encoded directly in `recEnd`. No DerivedVar block or R function is needed. This follows the `DHHGAGE_cont` pattern — the reference implementation for worksheet-only continuous variables.

### Pattern 4: DerivedVar with R function

**Variable:** `cigs_per_day` (cigarettes smoked per day)

When multiple source variables must be combined with conditional logic, a DerivedVar block delegates to an R function.

| variable | variableStart | databaseStart (abbreviated) | recStart | recEnd |
|-------------|-------------|-----------------------|-------------|-------------|
| cigs_per_day | DerivedVar::\[SMK_204, SMK_208, SMKDSTY_A\] | cchs2001_p, cchs2003_p, ... | N/A | Func::calculate_cigs_per_day |
| cigs_per_day | DerivedVar::\[SMK_204, SMK_208, SMKDSTY_A\] | cchs2001_p, cchs2003_p, ... | \[1,99\] | copy |
| cigs_per_day | DerivedVar::\[SMK_204, SMK_208, SMKDSTY_A\] | cchs2001_p, cchs2003_p, ... | SMKDSTY_A in (3,5,6) | NA::a |
| cigs_per_day | DerivedVar::\[SMK_204, SMK_208, SMKDSTY_A\] | cchs2001_p, cchs2003_p, ... | is.na(SMK_204) & is.na(SMK_208) | NA::b |
| cigs_per_day | DerivedVar::\[SMK_204, SMK_208, SMKDSTY_A\] | cchs2001_p, cchs2003_p, ... | else | NA::b |

**What makes it distinctive:** The first row's `recEnd=Func::calculate_cigs_per_day` tells `rec_with_table()` to call the R function `calculate_cigs_per_day()`, passing `SMK_204`, `SMK_208`, and `SMKDSTY_A` as inputs. Subsequent rows document output value ranges and missing value conditions. This pattern uses conditional `recStart` expressions (e.g., `SMKDSTY_A in (3,5,6)`) that evaluate against the input variables.

### Pattern 5: Range-based Master recode

**Variable:** `SMKG09C_cont` (years since quit, continuous — former daily smoker, Master)

Exact continuous values from Master files are grouped into broader categories using interval notation.

| variable     | variableStart | databaseStart (abbreviated) | recStart    | recEnd |
|-------------|-------------|-----------------------|-------------|-------------|
| SMKG09C_cont | \[SMK_09C\]   | cchs2003_m, cchs2005_m, ... | \[3,6)      | 4      |
| SMKG09C_cont | \[SMK_09C\]   | cchs2003_m, cchs2005_m, ... | \[6,11)     | 8      |
| SMKG09C_cont | \[SMK_09C\]   | cchs2003_m, cchs2005_m, ... | \[11,82\]   | 12     |
| SMKG09C_cont | \[SMK_09C\]   | cchs2003_m, cchs2005_m, ... | 996         | NA::a  |
| SMKG09C_cont | \[SMK_09C\]   | cchs2003_m, cchs2005_m, ... | \[997,999\] | NA::b  |
| SMKG09C_cont | \[SMK_09C\]   | cchs2003_m, cchs2005_m, ... | else        | NA::b  |

**What makes it distinctive:** The `recStart` column uses interval notation (`[3,6)` means 3 ≤ value \< 6). The `recEnd` values are midpoints of the ranges (4, 8, 12), converting exact years to grouped midpoints. This is the Master counterpart to a PUMF direct recode block.

### Pattern 6: PUMF/Master split

**Variable:** `SMKG09C` — shows how one variable uses completely different blocks for PUMF and Master.

**PUMF block** (direct recode — categorical source):

| variableStart | databaseStart | recStart | recEnd |
|-------------------|---------------------|----------------|----------------|
| cchs2003_p::SMKCG09C, ... \[SMKG09C\] | cchs2003_p, cchs2005_p, ... | 1 | 1 |
| (same) | (same) | 2 | 2 |
| (same) | (same) | 3 | 3 |

**Master block** (range-based — continuous source):

| variableStart | databaseStart               | recStart  | recEnd |
|---------------|-----------------------------|-----------|--------|
| \[SMK_09C\]   | cchs2003_m, cchs2005_m, ... | \[3,6)    | 1      |
| (same)        | (same)                      | \[6,11)   | 2      |
| (same)        | (same)                      | \[11,82\] | 3      |

**What makes it distinctive:** Same output categories (1, 2, 3) but completely different source variables and recode logic. PUMF has pre-grouped categorical input; Master has continuous years that must be range-mapped. Both blocks produce the same harmonised output.

### Pattern 7: Copy pass-through

**Variable:** `SMKG09C_cont` — the `[SPU_25]` block for recent PUMF databases.

| variable | variableStart | databaseStart | recStart | recEnd |
|---------------|---------------|---------------|---------------|---------------|
| SMKG09C_cont | \[SPU_25\] | cchs2019_2020_p, cchs2021_p, cchs2022_p, cchs2023_p | \[0,121\] | copy |
| SMKG09C_cont | \[SPU_25\] | cchs2019_2020_p, cchs2021_p, cchs2022_p, cchs2023_p | 996 | NA::a |
| SMKG09C_cont | \[SPU_25\] | cchs2019_2020_p, cchs2021_p, cchs2022_p, cchs2023_p | \[997,999\] | NA::b |

**What makes it distinctive:** `recEnd=copy` means the source value passes through unchanged. Valid values (0-121 months) are copied as-is; only missing codes are recoded. This is used when the source variable is already in the desired format.

------------------------------------------------------------------------

## Part 6: Missing values

### cchsflow missing value encoding

cchsflow uses two missing value codes throughout the worksheets:

| Code | Meaning | Typical source codes |
|-----------------|-----------------|---------------------------------------|
| `NA::a` | **Not applicable** — the question does not apply to this respondent (legitimate skip) | `6`, `96`, `996` (StatCan "not applicable") |
| `NA::b` | **Missing** — refusal, don't know, or not stated | `7`/`8`/`9`, `97`/`98`/`99`, `997`/`998`/`999` |

This two-category system simplifies StatCan's more detailed missing codes while preserving the critical distinction between "does not apply" and "data is missing."

### StatCan source missing codes

Statistics Canada uses consistent missing code conventions, but the specific values depend on the variable's range:

| Variable range | Not applicable | Refusal | Don't know | Not stated |
|----------------|----------------|---------|------------|------------|
| 1-5            | 6              | 7       | 8          | 9          |
| 1-95           | 96             | 97      | 98         | 99         |
| 1-995          | 996            | 997     | 998        | 999        |

### Tagged NAs in R

In R, cchsflow uses the `haven` package's `tagged_na()` function to preserve missing value types. Tagged NAs look like regular `NA` to standard R functions but carry a hidden tag (`"a"` or `"b"`) that can be inspected with `haven::is_tagged_na()` and `haven::na_tag()`.

This allows downstream analysis to distinguish between "not applicable" and "truly missing" when needed, while still behaving as `NA` for standard operations like `mean(x, na.rm = TRUE)`.

### The `clean_variables()` function

cchsflow's `clean_variables()` function auto-detects missing codes when database context is unavailable. It uses single-digit pattern matching (values 6-9 for narrow-range variables, 96-99 for wider ranges) to identify and convert source missing codes to tagged NAs.

**Caution:** Auto-detection can misclassify legitimate values as missing codes when the variable's valid range overlaps with missing code ranges. This is more likely with exact continuous values (e.g., a value of 8 could be either "8 years" or "don't know"). The worksheet's explicit `recStart → recEnd` mappings take precedence over auto-detection.

------------------------------------------------------------------------

## Part 7: Naming conventions {#part-7-naming-conventions}

cchsflow uses specific naming conventions for harmonised variables, functions, and row identifiers. The authoritative reference is `.claude/skills/cchsflow-review/docs/variable-naming-conventions.md`.

> **v3 status:** The conventions below reflect v3 decisions. Some are universally agreed upon (new variables use tidyverse verbs); others are being adopted incrementally during refactoring (renaming legacy `_A`/`_B` suffixes, renaming `_fun` functions). Legacy code may not yet follow all conventions.

### Harmonised variable names

**Selecting the base name:** cchsflow generally uses the CCHS 2007-2014 StatCan variable name as the harmonised name, harmonising other years to that form. For example, `SMK_09A` uses the 2003-2014 name even though 2001 used `SMKC_09A` and 2022-2023 uses `SPU_25A`.

**When to create a new name:** When three or more StatCan names exist for the same concept and none is clearly dominant, cchsflow creates a new descriptive name rather than arbitrarily picking one era's name. The new name should be more meaningful than any of the StatCan names. Example: `cigs_per_day` rather than picking among `SMK_204`, `SMK_045`, `CSS_25`.

### Variable suffixes

| Suffix | When to use | Example |
|--------------------|-------------------------------|----------------------|
| `_cont` | Categorical source → continuous midpoint output (PUMF) | `SMK_09A_cont` |
| `_C` (no underscore) | Master continuous companion using StatCan naming | `SMK_09C`, `SMK_06C` |
| `_catN` | Number of output categories changes from source | `SMK_09A_cat4` |
| `_2001`, `_2003plus` | Structural break across cycles (different category boundaries) | `SMK_09A_2001`, `SMK_09A_2003plus` |
| `_A`, `_B` (deprecated) | Legacy era-split suffixes — replace with descriptive names when refactoring | `SMKG01C_A` (2001-2003), `SMKG01C_B` (2005+) |

**`_cont` vs `_C` distinction:** `_cont` means midpoint imputation from a categorical PUMF source (e.g., `SMK_09A_cont` maps codes 1-4 to midpoints 0.5-4.0). `_C` reuses the StatCan `MOD_NNNC` naming for continuous companions that exist on Master files (e.g., `SMK_09C` = exact years from Master's `SMK_09C` variable). The `_C` variables are new in v3 (Master was not in v2).

**Do not** add `_cont` if the source is already continuous. **Do not** add `_catN` if the categories are unchanged from the source.

### Function naming conventions

DerivedVar blocks reference R functions via `Func::function_name`. v3 uses tidyverse-style verb prefixes:

| Prefix | Meaning | Example |
|--------------|----------------------------------------------|-------------------------------|
| `calculate_` | Numeric computation from inputs | `calculate_pack_years` |
| `score_`     | Index or scale scoring | `score_depression_scale` |

**Legacy functions** use a `variable_fun` convention (e.g., `SMKDSTY_fun`). These are left in place unless the variable family is being refactored — when doing a major block rewrite (e.g., smoking v3), rename the function to the new convention and add a re-exported alias if needed for backward compatibility.

**New variables** must use the tidyverse verb convention. This is a firm team decision.

### `dummyVariable` convention

`dummyVariable` values follow the pattern `{variable}_{typeEnd}{numValidCat}_{sequence}`:

- `SMK_01A_cat2_1` — first row of a 2-category categorical variable
- `DHHGAGE_cont_1` — first row of a continuous variable
- `SMK_01A_cat2_2` — second row

Values may repeat across blocks for the same variable. Some v3 variables use `N/A` as a placeholder when the convention has not yet been applied.

------------------------------------------------------------------------

## Part 8: Validation rules

A valid set of cchsflow worksheets satisfies all of the following rules. These are expressed declaratively — they describe what must be true, not how to check it. The current worksheets have known violations of some rules (particularly rules 3-6 and 12) due to legacy data and ongoing migration. New variables should satisfy all rules; existing violations are tracked for resolution.

### Structural rules

1.  **Unique primary key.** Every `variable` value in `variables.csv` must be unique.

2.  **Consistent row identifiers.** Every non-`N/A` `dummyVariable` value in `variable_details.csv` must follow the naming convention `{variable}_{typeEnd}{numValidCat}_{sequence}`. Values may repeat across blocks for the same variable when different blocks produce the same output categories (e.g., two era blocks both mapping to categories 1 and 2).

3.  **Foreign key integrity.** Every `variable` in `variable_details.csv` must have a corresponding row in `variables.csv`.

4.  **Database coverage agreement.** The set of databases in a variable's `databaseStart` in `variables.csv` must be a superset of the databases listed across all of that variable's blocks in `variable_details.csv`.

### Block rules

5.  **Exclusive database assignment.** Each database in a variable's coverage must appear in exactly one block in `variable_details.csv`. If a database appears in multiple blocks for the same variable, the behaviour is undefined.

6.  **Complete missing value rows.** Every direct recode and range-based block must include rows for `NA::a` and `NA::b` in `recEnd`. DerivedVar blocks should include NA rows to document possible missing outputs, but the R function handles missing values internally. Copy-only blocks may omit NA rows if the source variable's missing values pass through unchanged.

7.  **Catch-all row.** Every direct categorical recode block should include an `else` row in `recStart` to handle unexpected source values. Copy blocks and range-based blocks with exhaustive ranges are exempt — the range notation already constrains valid values.

### DerivedVar rules

8.  **Function existence.** A DerivedVar block's `Func::function_name` must correspond to an exported function in the package's `NAMESPACE`.

9.  **Input availability.** The variables listed in `DerivedVar::[var1, var2, ...]` must themselves be defined in `variables.csv` and available in the databases listed in the block's `databaseStart`.

10. **Source variable existence.** For DerivedVar blocks, the source variable (`SPU_25`, `SMK_09A`, etc.) must actually exist in the databases listed in `databaseStart`. A DerivedVar block should not list databases where the source variable does not exist.

### Value rules

11. **Type consistency.** In direct recode blocks, `recEnd` values must be consistent with `typeEnd`: integers for `cat`, numeric (including decimals) for `cont`.

12. **No deprecated databases.** The `_s` database suffix must not appear. Share files should be mapped to their Master equivalents (`_m`).

13. **Mutually exclusive recStart.** Within a direct recode block, `recStart` values must not overlap. Each source value should match at most one row (before the `else` catch-all).

### Cross-worksheet rules

14. **Source name consistency.** Source variable names in `variableStart` in `variable_details.csv` must be consistent with `variableStart` in `variables.csv`. Era-specific aliases must match.

15. **Database list consistency.** Databases listed in `variable_details.csv` blocks must use the standard `cchs{year}_{type}` naming convention and correspond to known CCHS releases.

------------------------------------------------------------------------

## Part 9: Common patterns and anti-patterns

### Pattern: Worksheet-first principle

**When to use worksheet recodes:** Simple value mappings — categorical passthrough, midpoint imputation, range-based grouping, copy. These should always be expressed as `recStart → recEnd` rows in the worksheet.

**When to use DV functions:** Multi-variable computation (e.g., `cigs_per_day` combines `SMK_204`, `SMK_208`, and `SMKDSTY_A` with conditional logic), conditional branching that depends on runtime values, or date arithmetic.

**Reference implementation:** `DHHGAGE_cont` converts age group categories to midpoint values entirely through worksheet `recEnd` mappings. No R function exists or is needed.

### Pattern: PUMF-Master bridging variables

Some cchsflow variables exist specifically to provide a unified interface across PUMF and Master data, allowing researchers to move seamlessly between file types. These bridging variables combine:

- **PUMF blocks:** Midpoint imputation from categorical sources (e.g., `SMK_09A_cont` maps categories 1-4 to midpoints 0.5, 1.5, 2.5, 4.0)
- **Master blocks:** Direct continuous values or copy from exact-value sources (e.g., `SMK_09C` on Master provides exact years since quit)

The result is a single harmonised variable that produces comparable continuous values regardless of whether the researcher uses PUMF or Master data. The PUMF values carry inherent imprecision from midpoint estimation (~15-20% relative error), while Master values are exact.

**Combining functions** like `time_quit_smoking` and `cigs_per_day` take these pre-computed continuous values as DerivedVar inputs and apply priority logic (e.g., prefer former-daily over former-occasional timing) to produce a single output. This two-layer architecture — worksheet midpoint recodes feeding DerivedVar combining functions — keeps the simple mappings in worksheets while delegating multi-variable routing to R functions.

**Key bridging variables:** `SMK_09A_cont`, `SMK_06A_cont`, `SMK_10A_cont`, `SMKG203_cont`, `SMKG207_cont`, `SMKG040_cont`, `DHHGAGE_cont`.

### Pattern: Variables.csv stubs (planned variables)

A `variables.csv` entry can exist without corresponding `variable_details.csv` rows. These stubs serve as architectural markers for planned work — the variable's metadata (label, subject, description) is defined, but the actual recode mappings have not yet been created.

**Example:** `time_quit_smoking_complete` and `time_quit_smoking_daily` exist in `variables.csv` as pathway-specific variants of `time_quit_smoking`, but have zero `variable_details` rows. They are planned for v3 but not yet implemented.

**Validation:** A worksheet check should flag variables.csv entries with zero variable_details rows as **warnings** (not errors). These are intentional stubs, not accidental omissions.

### Anti-pattern: Orphaned DerivedVar blocks

When an R function is deleted but its `Func::` reference remains in `variable_details.csv`, `rec_with_table()` will fail at runtime for any database that matches the orphaned block.

**Example:** `calculate_SMK_09A_cont()` was deleted because it was redundant (worksheet-first principle). The `SMKG09C` and `SMKG09C_cont` variables had DerivedVar blocks still referencing it. These blocks were also redundant — every database they listed was already covered by other blocks — and were deleted.

**Prevention:** Before deleting an R function, search `variable_details.csv` for `Func::function_name` and either delete or convert all matching DerivedVar blocks.

### Anti-pattern: Database over-specification

A DerivedVar block lists databases where the source variable does not exist.

**Example:** A `DerivedVar::[SPU_25]` block listed `cchs2003_p` through `cchs2023_p`, but `SPU_25` only exists in 2022-2023 databases. The block would fail for all earlier databases (and was redundant because other blocks already covered them).

**Prevention:** Verify that the source variables in a DerivedVar block actually exist in every database listed in `databaseStart`.

### Anti-pattern: Mismatched feeder variable names

A DerivedVar block references feeder variables by names that don't match their `variables.csv` entries. This causes silent resolution failures because `rec_with_table()` looks up feeders by their harmonised name.

**Example:** `DerivedVar::[SMKG005, SMKG040]` referenced `SMKG005`, but the harmonised variable is actually `SMK_005` (with underscore). The `G` prefix in `SMKG005` was the StatCan PUMF name, not the cchsflow harmonised name. The block should read `DerivedVar::[SMK_005, SMKG040]`.

**Prevention:** DerivedVar feeder names must exactly match the `variable` column in `variables.csv`. When copying variableStart patterns from source variable names (e.g., StatCan DDI names), verify the harmonised equivalent.

### Anti-pattern: Out-of-scope modifications

When writing `variable_details.csv` programmatically (e.g., with R), operations like `gsub()` on the full data frame can modify rows for variables outside the intended scope.

**Example:** A global `gsub("_s", "_m", ...)` intended to fix deprecated share file suffixes for smoking variables instead modified hundreds of unrelated variables.

**Prevention:** Always subset to in-scope variables before applying transformations. Write to a temporary file for review before overwriting the main worksheet.

------------------------------------------------------------------------

## Appendix A: Glossary cross-reference

| cchsflow term | cchsflow-docs glossary term | Ontology concept (future) |
|------------------|-----------------------------|--------------------------|
| Database (`cchs2001_p`) | Dataset | Instance context |
| Variable (`SMK_01A`) | Variable | Represented Variable |
| Source variable (`SMK_005`, `SMKCG09C`) | — | Instance Variable |
| Block | — | — (worksheet-specific) |
| `recStart → recEnd` | — | Relationship (recoded) |
| `Func::` function | — | Derivation rule |
| `NA::a` / `NA::b` | — | Missing data classification |
| PUMF / Master | Master Files / Share Files | File type context |
| Era suffix (`_2001`, `_2003plus`) | Cycle | Temporal boundary |

The ontology prototype in `cchsflow-docs/development/ontology/examples/smoking_variables.yaml` implements the DDI Variable Cascade model (Conceptual → Represented → Instance), which provides a formal framework for the relationships that cchsflow worksheets encode procedurally. See `cchsflow-docs/development/ontology/REQUIREMENTS.md` for the full specification.

------------------------------------------------------------------------

## Appendix B: Relationship to other documentation

| Resource | Location | Relationship |
|----------------------|----------------------|----------------------------|
| YAML column schemas | `inst/metadata/schemas/core/` | This document adds semantics; schemas define column order |
| Naming conventions | `.claude/skills/cchsflow-review/docs/variable-naming-conventions.md` | Authoritative source for naming rules; summarised in Part 7 |
| cchsflow-review skill | `.claude/skills/cchsflow-review/SKILL.md` | Procedural review process; this document provides the declarative knowledge it references |
| cchsflow-docs glossary | `../cchsflow-docs/docs/glossary.md` | CCHS terminology; Part 1 draws from it |
| cchsflow-docs architecture | `../cchsflow-docs/docs/architecture.md` | Database schema and metadata infrastructure |
| Ontology prototype | `../cchsflow-docs/development/ontology/` | Formal variable relationship model; Appendix A bridges to it |
| CEP documents | `ceps/` | Variable-specific harmonisation specs; this document is general |
| Package documentation | `man/`, pkgdown site | R function API reference |