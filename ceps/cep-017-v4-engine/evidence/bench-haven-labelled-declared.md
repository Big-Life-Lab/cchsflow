# Benchmark: haven / labelled / declared ecosystem
## Relevance to cchsflow v4 engine

Written: 2026-06-11
Agent: bench-haven-labelled-declared

---

## 1. Package inventory and current status

### 1.1 haven (tidyverse)
- **Version**: 2.5.5 (published 2025-05-30)
- **Maintainer**: Hadley Wickham / Posit PBC
- **Source**: https://cran.r-project.org/web/packages/haven/index.html
- **Imports**: cli, forcats, hms, lifecycle, methods, readr, rlang, tibble, tidyselect, vctrs
- **Reverse imports**: ~225 packages (tidyverse, rio, labelled, retroharmonize…)
- **Role**: Import/export for SPSS, Stata, SAS. Defines the foundational classes `haven_labelled` and `haven_labelled_spss`. Provides `tagged_na()` and SPSS user-NA via `labelled_spss()`.

### 1.2 labelled (Larmarange)
- **Version**: 2.16.0 (published 2025-10-22)
- **Maintainer**: Joseph Larmarange
- **Source**: https://cran.r-project.org/web/packages/labelled/index.html
- **Imports**: haven (≥ 2.4.1), cli, dplyr (≥ 1.1.0), lifecycle, rlang, vctrs, stringr, tidyr (≥ 1.3.0), tidyselect
- **Reverse imports**: 41 packages
- **Role**: Ergonomic layer on top of haven_labelled. Variable/value label manipulation, SPSS-style user-NA management, data dictionary (`look_for()`), conversion utilities.

### 1.3 declared (Dusa)
- **Version**: 0.26 (published 2026-04-02)
- **Maintainer**: Adrian Dusa (Univ. Bucharest)
- **Source**: https://cran.r-project.org/web/packages/declared/index.html
- **Imports**: zero dependencies
- **Reverse imports**: DDIwR, QCA (niche social-science tools)
- **Lifecycle**: "experimental"
- **Role**: Alternative class for social-science microdata. Stores original numeric codes but marks certain NAs as "declared" (displayed as `NA(-91)`). Bundles weighted statistical methods.

### 1.4 sjlabelled (Lüdecke)
- **Version**: current as of 2025-07 (latest CRAN PDF dated 2025-07-23)
- **Note**: cchsflow currently depends on sjlabelled for `set_label()` / `set_labels()` / `set_label<-()`. These are used in `label_data()` (R/label-utils.R). The recodeflow v4 spec (labels.qmd) explicitly targets haven + labelled as the forward-looking foundation.

### 1.5 retroharmonize (Antal)
- **Version**: 0.2.8 (published 2026-05-21)
- **Imports**: dplyr, tidyr, tibble, haven, stringr, purrr, rlang, labelled, snakecase
- **Role**: Survey-wave harmonization. Introduces `labelled_spss_survey()`, a subclass of haven's `labelled_spss` that carries a wave/source identifier. Closely related use case to cchsflow. Active as of 2026.

---

## 2. Missing-value architecture comparison

### 2.1 tagged_na (haven)

**Mechanism**: Hijacks an IEEE-754 NaN payload byte to store a single lowercase letter tag (a–z). The value passes `is.na()` = TRUE and behaves identically to regular NA in all base-R and tidyverse operations; the tag is only visible via `haven::na_tag()` / `haven::print_tagged_na()`.

**Constraints**:
- Double vectors only. Cannot tag integer, character, or factor vectors.
- Tag space: 26 lowercase letters (a–z). cchsflow uses only "a" (not-applicable) and "b" (missing/not-stated).
- Most R functions are tag-unaware; tags are preserved through dplyr/tidyr operations due to vctrs integration in modern haven.

**cchsflow current usage**:
- Pervasive: `tagged_na("a")` and `tagged_na("b")` used in recode-with-table.R (line 814), all 3-step DV functions (smoking-status.R, smoke-stop.R, smoking-cessation.R, bmi.R, alcohol.R, immigration.R, percent-time-canada.R, physical-activity.R, active-transportation.R, adl.R).
- `output_format = "tagged_na"` is the default / standard output.
- `is_tagged_na()` used in `detect_missing_vectorized()` in missing-data-functions.R.

**Upside**: Already the cchsflow standard. Tidyverse-native. SAS/Stata round-trip compatible. No class overhead on the vector.

**Downside**: Double-only constraint means integer variables need coercion. Tags can be stripped silently by packages that don't know about them. Requires `haven::is_tagged_na()` to discriminate tags in case_when.

### 2.2 labelled_spss (haven subclass via labelled)

**Mechanism**: Values retain their original numeric codes. Separate attributes (`na_values`, `na_range`) declare which codes are "user missing." `is.na()` returns TRUE for user-NA values, but the values are still present in the underlying vector. Most base-R functions do NOT treat them as missing—only haven-aware functions do.

**labelled package extension**: `labelled::na_values()`, `labelled::set_na_values()`, `labelled::na_range()`, `labelled::is_user_na()`, `labelled::user_na_to_na()`, `labelled::user_na_to_tagged_na()`, `labelled::tagged_na_to_user_na()`.

**Contradiction noted in the declared package docs** (https://cran.r-project.org/web/packages/declared/declared.pdf search result): "Package haven introduces an `haven_labelled_spss` class to deal with user defined missing values in a similar way as SPSS... `is.na()` returns TRUE for user-NA values, but such values are still present in the vector and other packages do not know these values should be treated as missing."

**cchsflow current usage**: Not used directly. The legacy recode-with-table.R engine does not produce `labelled_spss` objects. The 3-step derived-variable architecture does not use user-NA.

**Upside**: Preserves exact original CCHS codes (6, 9, 996, 999, etc.) alongside missingness metadata. Natural bridge to SPSS round-trip. `user_na_to_tagged_na()` provides a bridge to tagged_na if needed.

**Downside**: Most downstream R functions (lm, mean, table, etc.) will include user-NA values in calculations unless explicitly converted first with `user_na_to_na()`. This creates a correctness trap for downstream users. Requires careful use of `zap_missing()` or `as_factor()` in analysis pipelines.

### 2.3 declared class (Dusa)

**Mechanism**: Stores original numeric codes in the vector but converts them to NA internally and stores an index of "declared" NAs as an attribute. The display format is `NA(-91)` rather than plain `NA`. Base R's `is.na()` returns TRUE, but the declared attribute preserves what value underlies each NA for display and recovery.

**Key distinction from labelled_spss**: 
- labelled_spss: value is stored as -91, `is.na()` returns TRUE only by haven convention, other packages see -91 as a real value.
- declared: value is stored as NA at the R level (so all R functions correctly exclude it), but the original code (-91) is preserved in an index attribute and shown as `NA(-91)`.

**Concrete example from README**:
```r
x1 <- labelled_spss(c(1:5, -91), labels = c(DK = -91), na_values = -91)
mean(x1)  # -12.67 -- because -91 is still in the vector, mean() doesn't know it's missing

x2 <- declared(c(1:5, -91), labels = c(DK = -91), na_values = -91)
mean(x2)  # 3 -- -91 is stored as NA at the R level, excluded from mean()
```

**Constructor**:
```r
declared(x, labels = NULL, na_values = NULL, na_range = NULL,
         label = NULL, measurement = NULL, llevels = FALSE, ...)
```

**Key functions**:
- `declared()`, `as.declared()`, `is.declared()`, `anyNAdeclared()`
- `undeclare()`, `as.haven()` -- coerce back to haven/labelled objects
- `missing_values()`, `missing_range()`, `is.empty()`, `anyNAempty()`
- `w_mean()`, `w_sd()`, `w_table()` -- weighted statistics that correctly handle declared NAs
- `drop_na()` -- drop/undeclare labelled objects

**Compatibility**: Has `as.haven()` to convert back to haven objects. Maintains compatibility with haven/labelled for import workflows.

**Zero dependencies**: Can be used without pulling in haven, dplyr, etc. But this is a double-edged sword—it means declared objects don't integrate with vctrs-based type stability, which can cause issues in tidyverse pipelines.

**Adoption**: Low. Only 2 reverse imports (DDIwR, QCA)—both niche social-science packages. Experimental lifecycle. 439 commits suggests active development but limited ecosystem uptake.

---

## 3. Concrete comparison table

| Dimension | tagged_na (haven) | labelled_spss (haven/labelled) | declared |
|-----------|-------------------|-------------------------------|----------|
| Storage | NA with tag byte | Original value + na_values attr | NA in vector + index attr |
| is.na() | TRUE | TRUE (by haven convention) | TRUE (genuine NA) |
| base R mean/lm | Excludes (as NA) | INCLUDES (bug trap) | Excludes (correct) |
| Type constraint | Double only | Numeric/integer/char | Numeric |
| Tag/code space | 26 letters | Any numeric codes | Any numeric codes |
| Original code recovery | No | Yes (in vector) | Yes (in attr index) |
| Display | NA, na_tag("a") shows "a" | -91 (invisible missingness) | NA(-91) (explicit) |
| Tidyverse integration | Excellent (vctrs-native) | Good | Limited (no vctrs) |
| as_factor() support | Yes (haven) | Yes (haven) | Via as.haven() |
| SAS/Stata export | Yes (haven round-trip) | Yes | Via as.haven() |
| CRAN maturity | Stable | Stable | Experimental |
| Reverse imports | 225+ | (part of haven) | 2 |
| Zero-dependency | No | No | Yes |
| cchsflow current use | Core standard | Not used | Not used |

---

## 4. Overlap with cchsflow

### 4.1 What cchsflow currently does with this ecosystem

**haven (tagged_na)**: The foundation. Every DV function emits `tagged_na("a")` or `tagged_na("b")`. The `output_format` parameter ("tagged_na" vs "original") controls whether to emit tagged NAs or raw numeric codes. The `any_missing()` / `get_priority_missing()` / `clean_variables()` pipeline is built around `haven::is_tagged_na()`.

**sjlabelled**: Used in `label_data()` for attaching variable labels and value labels (`set_label<-`, `set_labels`). This is the labelling layer, separate from the missing-value layer.

**labelled**: Not currently used directly, but labelled's `labelled_spss` class underlies haven's handling when `user_na = TRUE`. The recodeflow v4 spec (labels.qmd) explicitly names haven + labelled as the forward target, suggesting sjlabelled will be phased out.

**declared**: Not used anywhere in cchsflow v3 or v4 plans.

### 4.2 The CCHS integer-type problem

CCHS source variables are often integer (codes 6, 7, 8, 9). `tagged_na()` requires double. This forces silent coercion in the recode-with-table.R engine (line 814: `tagged_na(as.character(na_value_list[[3]]))`). The 3-step DV functions handle this in `clean_variables()`. This is a known limitation that v4 will need to address explicitly.

**labelled_spss** could preserve integer types with SPSS-style user-NA attributes, but the correctness trap (mean/lm including codes) makes it a worse default.

**declared** also converts to NA at the R level but stores indices; it similarly handles the integer issue if the underlying vector holds codes before conversion—but the package's zero-dependency philosophy and experimental status are risks.

### 4.3 The output_format bridge

The `output_format = "tagged_na" | "original"` parameter in v3 3-step functions is a partial workaround for the integer problem: "original" returns the numeric code (e.g., 996) instead of `tagged_na("b")`. The recodeflow v4 spec wants this to continue (missing-data.qmd: "this behaviour should be maintained").

---

## 5. Lessons for v4 design

1. **Keep tagged_na as the primary output format.** It is the only approach that is (a) standard across the R ecosystem, (b) Stata/SAS export-compatible, (c) already woven through cchsflow. The recodeflow v4 spec (missing-data.qmd) explicitly recommends continuing with haven.

2. **Consider adding labelled_spss as an optional output format** for users who need to preserve original codes with their missingness annotation (SPSS round-trip, debugging). The `user_na_to_tagged_na()` bridge in labelled makes this low-risk. This would replace the "original" mode's lossy numeric-code approach.

3. **Do not adopt declared.** Rationale: (a) experimental lifecycle with minimal adoption (2 reverse imports); (b) no vctrs integration means type instability in tidyverse pipelines; (c) the correctness advantage over labelled_spss (mean properly excludes declared NAs) is already achieved by tagged_na; (d) as.haven() bridge exists but adds complexity; (e) zero-dependency is a feature for a standalone tool but a fragility signal for an ecosystem package.

4. **Phase out sjlabelled dependency** in favour of labelled (as recodeflow v4 spec proposes). The labelled package is actively maintained (2.16.0, Oct 2025) with 41 reverse imports and is the standard haven companion. sjlabelled overlaps heavily but is less integrated with vctrs.

5. **Solve the double-only constraint explicitly.** Either (a) document that all continuous CCHS variables must be stored as double (not integer) in cchsflow output, or (b) add an `integer_tagged_na()` utility that coerces and tags in one step.

6. **Use labelled::look_for() for variable discovery.** cchsflow v4 has a variable-discovery module (R/variable-discovery.R). labelled's `look_for()` is a well-tested, data-dictionary-generating function that searches variable labels, value labels, and factor levels. It is the Stata `lookfor` analogue and directly relevant to the CEP-015 consumer-side tools.

7. **Consider labelled::user_na_to_tagged_na() in the engine.** If the v4 engine ingests data that has already been labelled_spss (e.g., directly from `read_spss(user_na=TRUE)`), this function is the right conversion path into the tagged_na standard.

8. **retroharmonize is the closest ecosystem peer.** Its `labelled_spss_survey()` class solves a similar problem (multi-wave source tracking) to cchsflow's per-database mapping. Worth examining for design patterns, not as a dependency.

---

## 6. Recommendation summary

**Adopt** (deepen use): `haven` tagged_na + `labelled` package (variable/value label management, user_na conversion utilities, `look_for()`).

**Wrap / Imitate** (selective borrowing): `labelled_spss` as an optional secondary output_format. The `user_na_to_tagged_na()` and `tagged_na_to_user_na()` conversion pair is worth exposing as v4 output options.

**Ignore**: `declared`. Experimental, minimal adoption, no vctrs integration, correctness advantage already covered by tagged_na.

---

## 7. Source URLs

- haven CRAN: https://cran.r-project.org/web/packages/haven/index.html
- labelled CRAN: https://cran.r-project.org/web/packages/labelled/index.html
- labelled function reference: https://larmarange.github.io/labelled/reference/index.html
- labelled missing values vignette: https://cran.r-project.org/web/packages/labelled/vignettes/missing_values.html
- declared CRAN: https://cran.r-project.org/web/packages/declared/index.html
- declared GitHub: https://github.com/dusadrian/declared
- declared function reference: https://dusadrian.github.io/declared/reference/declared.html
- haven semantics vignette: https://haven.tidyverse.org/articles/semantics.html
- retroharmonize CRAN: https://cran.r-project.org/web/packages/retroharmonize/
- recodeflow v4 missing-data spec: /tmp/recodeflow-pr43/missing-data.qmd
- recodeflow v4 labels spec: /tmp/recodeflow-pr43/labels.qmd
- cchsflow tagged_na vignette: /Users/dmanuel/github/cchsflow/vignettes/tagged_na_usage.Rmd
- cchsflow missing-data-functions.R: /Users/dmanuel/github/cchsflow/R/missing-data-functions.R
- cchsflow clean-variables.R: /Users/dmanuel/github/cchsflow/R/clean-variables.R
- cchsflow label-utils.R: /Users/dmanuel/github/cchsflow/R/label-utils.R
- cchsflow recode-with-table.R tagged_na usage: line 814
- cchsflow DESCRIPTION: /Users/dmanuel/github/cchsflow/DESCRIPTION
