# Benchmark: ipumsr / IPUMS Harmonization Model
# For cchsflow v4 engine scoping
# Researched: 2026-06-11

## 1. What IPUMS Is

IPUMS (Integrated Public Use Microdata Series) is the world's largest harmonized
microdata infrastructure, maintained by the Minnesota Population Center. It covers
US decennial census data (IPUMS USA), Current Population Survey (IPUMS CPS),
international census data from ~100 countries (IPUMS International), and a growing
number of health surveys (NHIS, MEPS, DHS, ATUS). As of 2026 the system holds
hundreds of millions of person-records spanning 150+ years.

Source: https://www.ipums.org/mission-purpose

## 2. The ipumsr R Package

**Package**: `ipumsr` on CRAN; GitHub at https://github.com/ipums/ipumsr  
**Current version**: 0.10.0 (released March 2026 per GitHub releases page)  
**R requirement**: >= 4.1.0  
**Maintainer**: Minnesota Population Center

Source: https://github.com/ipums/ipumsr/releases

### 2.1 Core capabilities

- `read_ipums_ddi()` — parse DDI XML codebook into R `ipums_ddi` object
- `read_ipums_micro()` — read fixed-width microdata with metadata pre-attached
- `ipums_var_info()`, `ipums_val_labels()`, `ipums_var_desc()` — inspect variable metadata
- `ipums_view()` — launch static HTML browser of variable metadata
- `lbl_*` family — manipulate haven-labelled vectors for harmonization workflows
- `define_extract_micro()`, `submit_extract()`, `download_extract()` — programmatic API access
- `var_spec()` — per-variable extract configuration (case selections, data quality flags, attached characteristics)
- `ipums_bind_rows()` — combine data frames while preserving labelled attributes
- `get_metadata_catalog()`, `get_metadata()` — query NHGIS/IHGIS source metadata without downloading data

Source: https://tech.popdata.org/ipumsr/reference/index.html

### 2.2 The `ipums_ddi` object (var_info tibble columns)

When `read_ipums_ddi()` parses a DDI XML file, the resulting object's `var_info`
tibble has these columns:

| Column | Content |
|---|---|
| `var_name` | Variable name (harmonized, consistent across years) |
| `var_label` | Short human label |
| `var_desc` | Longer prose description |
| `val_labels` | List-column: tibble of val/lbl pairs per variable |
| `code_instr` | Coding instructions from documentation |
| `start`, `end` | Column positions for fixed-width parsing |
| `imp_decim` | Implied decimal places |
| `var_type` | Data type |
| `rectypes` | Record type(s) for hierarchical (multi-level) extracts |

Sources: https://tech.popdata.org/ipumsr/reference/read_ipums_ddi.html,
https://czep.net/21/ipums-into-postgres.html

### 2.3 lbl_* label helpers for harmonization

IPUMS uses `haven::labelled` vectors (not R factors) to preserve both the raw
numeric code and a human-readable label. This allows:
- Partial labeling: not all values need labels
- Non-sequential codes that preserve original meaning (e.g., AGE=90 means "90+")
- Conversion to NA via pattern matching, not hard-coded sentinels

Key functions:

```r
# Convert out-of-universe / refusal codes to NA before numeric coercion
lbl_na_if(x, ~ .val == 999999999)
lbl_na_if(x, ~ .lbl %in% c("NIU", "Not in universe", "Don't know"))

# Collapse detailed codes to broad categories (e.g., 4-digit → first digit)
lbl_collapse(x, ~ (.val %/% 10) * 10)

# Manually remap labels across years
lbl_relabel(x, lbl(1, "Employed") ~ .val %in% c(1, 10, 11, 12))

# Clean up orphaned labels after collapsing
lbl_clean(x)

# Strip labels entirely for analysis
zap_labels(x)
```

The value label is _stored as an attribute on the vector_, not in a separate
lookup table. This means the metadata travels with the data through pipelines,
survives joins, and can be restored with `set_ipums_var_attributes()` if
accidentally stripped.

Sources: https://tech.popdata.org/ipumsr/reference/lbl_relabel.html,
https://rdrr.io/cran/ipumsr/f/vignettes/value-labels.Rmd

## 3. The IPUMS Harmonization Architecture

### 3.1 Core philosophy: metadata-driven transformation

IPUMS's central design principle is that harmonization rules live in
**correspondence tables** (metadata), not in bespoke code. As the infrastructure
paper (Ruggles et al., Scientific Data, 2018) states:

> "Most IPUMS data transformations are performed using variable harmonization
> tables that specify how each value in the source data is recoded. Some variables
> also require programming logic in addition to the harmonization table."

The pipeline is:
1. Staff maintain correspondence tables mapping `(source_variable, source_code)` → `(harmonized_code)` for each country-year-variable combination.
2. Custom software reads those tables and executes transformations at extract-generation time.
3. Users receive harmonized data plus a DDI codebook reflecting the harmonized scheme.

This contrasts with cchsflow's current approach, where harmonization logic
is embedded in R functions (ADL, alcohol, BMI, etc.) or in the positional
`recode-with-table.R` engine, with the CSV worksheets acting as partial
configuration. IPUMS's correspondence tables _are_ the authoritative specification;
cchsflow's CSVs are necessary but insufficient (they reference R functions by name
without encoding the logic).

Sources: https://pmc.ncbi.nlm.nih.gov/articles/PMC5827695/,
https://international.ipums.org/international/harmonization.shtml

### 3.2 Composite coding: hierarchical codes preserve detail

IPUMS uses a tiered digit scheme for categorical variables that have different
detail levels across years:

> "The first one or two digits of the code provide information available across
> all samples. The next one or two digits provide additional information available
> in a broad subset of samples. Finally, trailing digits provide detail only rarely
> available." (IPUMS International harmonization page)

Example (marital status): Code 1 = "married" works everywhere. Code 11 = "married,
polygamous" only where Kenya data distinguishes this. Users can aggregate by
truncating trailing digits — no recoding scripts needed.

This is directly analogous to how cchsflow could treat, say, immigration status
with trailing digits distinguishing "number of years in Canada" precision levels.
The difference: IPUMS encodes the digit hierarchy _in the metadata_; cchsflow
currently encodes it in R case_when logic.

Source: https://international.ipums.org/international/harmonization.shtml

### 3.3 "Not in universe" (NIU) as a first-class concept

IPUMS encodes NIU as a labelled value, not a missing code:
- NIU records are given a **common cross-time code** within each variable
- The "universe" statement (which respondents the question applied to) is
  documented per country-year in the DDI and on the variable's documentation page
- Users call `lbl_na_if(x, ~ .lbl == "NIU")` to convert to NA explicitly

This is more explicit than cchsflow's current approach, where "not applicable"
values are encoded as numeric sentinels (-7, -8, etc.) with their meaning
embedded in `variable_details.csv` category labels, but no machine-readable
universe statement attached.

Sources: https://cps.ipums.org/cps-action/faq,
https://rdrr.io/cran/ipumsr/f/vignettes/value-labels.Rmd

### 3.4 Harmonized vs. unharmonized variables: the dual-track model

IPUMS CPS offers both:

**Harmonized variables** (e.g., `RACE`): Consistent coding across all years.
IPUMS staff apply their own recoding decisions, with unavoidable "anachronism" risk.

**Unharmonized variables** (e.g., `UH_RACE_1` through `UH_RACE_5`): One
unharmonized variable per distinct coding epoch. When the original coding scheme
changes, a new unharmonized variable is created. They preserve source codes almost
entirely (only minor usability recodes: blank→NIU, string→numeric).

Key design implication: when a variable's coding scheme changes across time, IPUMS
creates a _new variable name_ rather than silently folding the change into an
existing variable. This makes versioning explicit in the variable namespace.

Source: https://cps.ipums.org/cps/unharmonized_variables.shtml,
https://blog.popdata.org/cpsunharmonized/

**Analogy to cchsflow**: The era-split naming in cchsflow v3 (`_pre2015`,
`_2015plus`, `_2003plus`) follows a similar philosophy — epoch-specific names for
variables whose source question/coding changed. IPUMS systematizes this pattern.

### 3.5 Variable versioning and revision tracking

IPUMS CPS maintains a public revision log (https://cps.ipums.org/cps-action/revisions)
that records:
- Edited / added / expanded variables by date
- Exact affected years
- Root cause and number of records affected
- Occupation/industry code transitions across Census Bureau classification schemes

IPUMS USA and International maintain separate revision logs. This is a form of
data versioning that IPUMS ships as first-class documentation.

cchsflow has `version` and `lastUpdated` columns in `variable_details.csv` and
`variables.csv`, plus `versionNotes` and `reviewNotes`, but no machine-readable
changelog analogous to IPUMS's revision log. The current version field is a single
semver string, not a per-row history.

Source: https://cps.ipums.org/cps-action/revisions

### 3.6 Comparability documentation: structured prose attached to variables

Every IPUMS variable has a "Comparability" tab in its documentation that
explicitly flags:
- Universe differences across years (e.g., age restriction changed)
- Coding scheme changes that limit longitudinal comparability
- Country-specific caveats for international datasets
- Questionnaire wording differences

This is human-authored prose, but it is structured _per variable_ and is
surfaced prominently. The DDI object carries a `var_desc` field that can include
this material.

cchsflow has `notes` and `description` columns in `variables.csv`, and `notes`
in `variable_details.csv`, but no formal "comparability" section per variable.
These fields are sparsely populated in practice.

Sources: https://international.ipums.org/international-action/variables/173949,
https://pmc.ncbi.nlm.nih.gov/articles/PMC5827695/

### 3.7 Occupation/industry harmonization: crosswalk-based, fixed benchmarks

For variables that evolve across multiple classification schemes (occupation codes
changed in 1950, 1970, 1990, 2000, 2010), IPUMS maintains:

- `OCC` / `IND`: original codes for each year
- `OCC1950` / `OCC1990`: harmonized to a fixed classification scheme
- Census Bureau crosswalk tables as authoritative mapping source
- Explicit "plurality weighting" for ambiguous mappings

The key lesson: IPUMS does not attempt to harmonize everything into one code
scheme. They maintain multiple scheme-specific harmonizations (`OCC1950`,
`OCC1990`, `OCC2010`) so researchers can pick the scheme that suits their
analytical span. When two schemes conflict, both exist.

Source: https://usa.ipums.org/usa/chapter4/chapter4.shtml

## 4. The DDI Standard as Metadata Container

DDI (Data Documentation Initiative) is an XML standard for social science
metadata (https://www.ddialliance.org/). IPUMS adopts it for:
- Fixed-width parsing instructions (start/end columns, decimal places)
- Variable and value labels
- Universe statements
- General extract information (terms of use, sample identifiers)

The DDI file **travels with the data**: an IPUMS extract is always a (DDI.xml,
data.dat.gz) pair. This ensures the metadata is never separated from the
data file.

**ipumspy (Python)** also reads DDI: the `Codebook` class parses the XML into a
`VariableDescription` per variable with `codes` (dict of label→value) and
`description` fields. Non-extract collections use YAML instead of DDI XML, but
the resulting objects are interface-compatible.

Source: https://ipumspy.readthedocs.io/en/latest/reading_data.html

## 5. Programmatic API: extract-as-code

IPUMS offers a REST API (v2) wrapped by ipumsr and ipumspy. Key capabilities:

```r
# Define an extract programmatically
ext <- define_extract_micro(
  collection = "cps",
  description = "Income analysis 2018-2020",
  samples = c("cps2018_03s", "cps2019_03s", "cps2020_03s"),
  variables = list(
    var_spec("INCTOT",
             case_selections = c("1"),   # employed only
             data_quality_flags = TRUE),
    "AGE", "SEX", "RACE"
  )
)

# Submit, wait, download
submitted <- submit_extract(ext)
wait_for_extract(submitted)
download_extract(submitted)
```

Variable specifications (`var_spec()`) can include:
- `case_selections`: filter to specific code values (with general vs detailed mode)
- `data_quality_flags`: include flag variables for edited/allocated values
- `attached_characteristics`: add household member variables (mother, father, spouse, head)

The extract definition is **serializable** (can be retrieved, shared, re-run) via
`get_extract_info()` and `get_extract_history()`.

Sources: https://tech.popdata.org/ipumsr/articles/ipums-api-micro.html,
https://tech.popdata.org/ipumsr/reference/define_extract_micro.html

## 6. Concrete Comparisons to cchsflow

### 6.1 What IPUMS does better

| Dimension | IPUMS | cchsflow (current) |
|---|---|---|
| Harmonization rules | Correspondence tables (metadata); software interprets | R functions (code); CSVs reference by name only |
| Value labels | `haven::labelled` vectors; metadata travels with data | Numeric sentinels; labels in separate CSV lookup |
| Missing/NIU | First-class labelled value with universe statements | Numeric codes (-7, -8) with labels in category rows |
| Variable versioning | Public revision log per variable; epoch-specific names for scheme changes | `version` field + `versionNotes` text; no changelog |
| Comparability docs | Structured "Comparability" tab per variable | `notes` column; sparsely populated |
| Dual-track harmonization | Harmonized + unharmonized (source) variables side-by-side | Era-split names (`_pre2015`, `_2015plus`) — similar intent, less systematic |
| Composite coding | Digit hierarchy encodes resolution level in the code itself | Resolution expressed in separate `typeStart`/`typeEnd` columns |
| Metadata portability | DDI XML ships with every extract; always paired | CSVs live in `inst/extdata/`; decoupled from data delivery |

### 6.2 What cchsflow does better (or is better suited for)

| Dimension | IPUMS | cchsflow (current) |
|---|---|---|
| Derived variables | Limited: mostly recoding source vars | First-class: computed from multiple sources via R functions |
| Complex logic | Correspondence tables for recoding; limited for multi-source arithmetic | Arbitrary R functions — handles pack_years, BMI, ADL score, etc. |
| Scale | 100+ countries, census/survey, millions of records | One national survey (CCHS), dozens of waves, ~100 variables |
| Open-source | Package is open; metadata infrastructure is proprietary | Both package and metadata are open |
| Database separation | Single harmonized output | Explicit per-database tracking (cchs2001_p vs cchs2001_m vs cchs2007_2008_m) |

### 6.3 Key design pattern: cchsflow already has an analogue for each IPUMS pattern

- **IPUMS correspondence tables** ↔ **cchsflow variable_details.csv** (but cchsflow's rows reference R functions instead of encoding the recode logic inline)
- **IPUMS harmonized vs unharmonized** ↔ **cchsflow era-split names** (less systematic: era suffix `_pre2015` vs IPUMS's `UH_RACE_1..5`)
- **IPUMS NIU labelled value** ↔ **cchsflow `-7` / `-8` sentinels with category labels** (same concept, different implementation)
- **IPUMS revision log** ↔ **cchsflow `version` + `versionNotes` columns** (IPUMS is queryable; cchsflow is free text)
- **IPUMS DDI XML** ↔ **cchsflow variable_details.csv** (both bundle metadata; DDI is a community standard, cchsflow is bespoke)

## 7. Lessons for cchsflow v4

1. **Encode recoding logic in metadata, not only in function names.** The `recEnd`
   field in `variable_details.csv` currently references `Func::bmi_fun` — it names
   the function but does not encode the mapping. For simple categorical recodes
   (passthrough, constant, direct code substitution), the recode table itself
   should be machine-readable without referencing external R code. Complex derived
   vars (BMI, pack-years, ADL score) still need R functions, but simple ones
   should not.

2. **Adopt `haven::labelled` for output rather than bare integers.** The `lbl_*`
   API provides a clean, composable way to handle NIU/missing/refused codes
   without hard-coding sentinels. A cchsflow v4 `clean_variables()` step could
   return labelled vectors that preserve the code-label mapping, making downstream
   NA conversion explicit and auditable.

3. **Formalise "universe" per variable-era.** The IPUMS universe statement
   ("persons age 10+ who were employed") is the missing piece in cchsflow's
   `notes` column. A structured `universe` field per row in `variable_details.csv`
   would make the `-7` (not applicable) logic machine-readable.

4. **Make the harmonized/unharmonized split explicit at the schema level.** IPUMS's
   `UH_RACE_N` convention is more systematic than cchsflow's era-suffix convention.
   A v4 schema could include a `harmonization_class` column (passthrough | recode |
   derived | era_split) to make this explicit and queryable.

5. **Ship a machine-readable revision log.** IPUMS's CPS revision log is a query
   target. cchsflow's `version` + `versionNotes` fields are there but not indexed.
   A v4 changelog table (variable, date, change_type, description) would support
   reproducibility auditing.

6. **Comparability notes as a first-class column, not free-text `notes`.** IPUMS's
   Comparability tab is surfaced prominently and per-variable. A v4 cchsflow schema
   could add a `comparability_notes` column to `variables.csv` (or as a separate
   table) to document limits on cross-era pooling — a common user error.

7. **Don't abandon era-split naming; systematize it.** The IPUMS unharmonized
   variable approach validates cchsflow's era-split naming (`_pre2015`, `_2015plus`).
   v4 should standardize the naming convention and add a `source_epoch` metadata
   field instead of encoding the epoch only in the name.

8. **The composite digit coding scheme is worth borrowing for category codes.**
   IPUMS's leading-digit-as-broad, trailing-digit-as-detail approach would help
   cchsflow handle variables where some CCHS cycles have finer response categories.
   Instead of dropping detail, encode broad categories in leading digits and let
   users truncate.

## 8. What IPUMS does NOT solve for cchsflow

- IPUMS has no equivalent to cchsflow's **derived variable functions** (pack-years,
  BMI, ADL score, active transport in minutes). IPUMS recodes source variables; it
  does not compute new measures from combinations of source variables the way
  cchsflow's derive functions do.
- IPUMS's **scale** is orders of magnitude larger; their infrastructure investments
  (custom software, staff, server infrastructure) are not replicated by an R package
  alone.
- IPUMS does not handle the **PUMF vs Master data split** (CCHS-specific issue).
- IPUMS's **harmonization is curated centrally** by subject-matter experts; cchsflow
  is community-maintained and needs a leaner verification workflow.

## 9. Sources

- ipumsr CRAN: https://rdrr.io/cran/ipumsr/
- ipumsr reference: https://tech.popdata.org/ipumsr/reference/index.html
- ipumsr releases: https://github.com/ipums/ipumsr/releases
- Reading IPUMS data: https://tech.popdata.org/ipumsr/articles/ipums-read.html
- IPUMS value labels vignette: https://rdrr.io/cran/ipumsr/f/vignettes/value-labels.Rmd
- lbl_relabel docs: https://tech.popdata.org/ipumsr/reference/lbl_relabel.html
- IPUMS CPS API micro: https://tech.popdata.org/ipumsr/articles/ipums-api-micro.html
- define_extract_micro: https://tech.popdata.org/ipumsr/reference/define_extract_micro.html
- IPUMS International harmonization: https://international.ipums.org/international/harmonization.shtml
- IPUMS USA occupation codes: https://usa.ipums.org/usa/chapter4/chapter4.shtml
- IPUMS CPS unharmonized variables: https://cps.ipums.org/cps/unharmonized_variables.shtml
- IPUMS CPS unharmonized blog: https://blog.popdata.org/cpsunharmonized/
- IPUMS CPS revision log: https://cps.ipums.org/cps-action/revisions
- IPUMS CLASSWK variable docs: https://international.ipums.org/international-action/variables/173949
- ipumspy reading data: https://ipumspy.readthedocs.io/en/latest/reading_data.html
- Ruggles et al. 2018 (PMC): https://pmc.ncbi.nlm.nih.gov/articles/PMC5827695/
- Harmonization in Social Sciences (Wiley): https://onlinelibrary.wiley.com/doi/10.1002/9781119712206.ch12
