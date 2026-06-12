# Benchmark: R Package Design Practices vs cchsflow gaps
Generated: 2026-06-11
Agent: bench-pkg-design-practices

---

## Scope

This benchmark compares cchsflow (branch fix/v3-smoking-worksheet-sync, working tree) against
four authoritative sources on R package design:

1. **R Packages 2e** (Wickham & Bryan) -- https://r-pkgs.org/
   - Ch. 9 DESCRIPTION: https://r-pkgs.org/description.html
   - Ch. 11 Dependencies in Practice: https://r-pkgs.org/dependencies-in-practice.html
   - Ch. 21 Lifecycle: https://r-pkgs.org/lifecycle.html
2. **lifecycle package** (r-lib) -- https://lifecycle.r-lib.org/
   - Stages: https://lifecycle.r-lib.org/articles/stages.html
   - Communicate: https://lifecycle.r-lib.org/articles/communicate.html
3. **rOpenSci Packages Guide** -- https://devguide.ropensci.org/pkg_building.html
4. **Tidy design principles** (tidyverse) -- https://design.tidyverse.org/
   - Type stability: https://design.tidyverse.org/out-type-stability.html
   - Function names: https://design.tidyverse.org/function-names.html

---

## 1. Depends vs Imports -- CRITICAL gap

### Norm (R Packages 2e)
"Prior to the roll-out of namespaces in R 2.14.0 in 2011, Depends was the only way to 'depend'
on another package. Now, despite the name, you should almost always use Imports, not Depends."
Source: https://r-pkgs.org/description.html#sec-description-depends-imports-suggests

"The most legitimate current use of Depends is to state a minimum version for R itself, e.g.
Depends: R (>= 4.0.0)."
Source: https://r-pkgs.org/dependencies-in-practice.html

goodpractice::gp() check `no_description_depends` specifically flags this.
Source: https://cran.r-project.org/web/packages/goodpractice/vignettes/goodpractice.html

### cchsflow reality
DESCRIPTION has NO Imports field. All 9 runtime packages in Depends:
  R (>= 3.5.0), haven (>= 1.1.2), dplyr (>= 0.8.2), sjlabelled (>= 1.0.17), stringr (>= 1.2.0),
  magrittr, yaml, readr, purrr, cli

This attaches ~996 exported symbols to every user's search path on library(cchsflow):
- dplyr masks 6 base/stats names: filter, lag, intersect, union, setdiff, setequal
- sjlabelled masks haven::read_stata, read_spss, read_sas, write_sas, as_factor, zap_labels
- purrr masks base::%||% (R >= 4.4) and magrittr::set_names
- cli is in Depends but has ZERO calls in R/ (only used in exec/ developer scripts)

Installed versions show the pin gap: haven>=1.1.2 (installed 2.5.5), dplyr>=0.8.2 (installed 1.2.0).
dplyr 0.8.2 predates dplyr 1.0.0 (released June 2020) which overhauled rowwise/group_by.

Evidence: inventory-dependencies-namespace.json (existing CEP-017 evidence file)

### Gap severity: HIGH -- violates the most emphatic "almost always" rule in R Packages 2e.

---

## 2. Lifecycle deprecation -- if_else2 and _fun family

### Norm (lifecycle, R Packages 2e)
R Packages 2e Ch. 21 (https://r-pkgs.org/lifecycle.html):
  - `lifecycle::deprecate_warn()` is preferred over `.Deprecated()` because it limits warnings
    to once per 8 hours, provides structured `when/what/with` messaging, and works with
    `lifecycle::is_present()` for argument handling.
  - Base `.Deprecated()` is the minimum; lifecycle package is the standard for mature packages.
  - Lifecycle stages: experimental → stable → superseded (better alt exists, no warning) →
    deprecated (will be removed, warning) → defunct (error)

lifecycle package (https://lifecycle.r-lib.org/reference/deprecate_soft.html):
  - `deprecate_soft()` warns only if the deprecated function is called directly (not by package
    internals) -- ideal for the first deprecation cycle.
  - `deprecate_warn()` warns unconditionally.
  - Add lifecycle badge to @description block: `` `r lifecycle::badge("deprecated")` ``

rOpenSci guide (https://devguide.ropensci.org/pkg_building.html):
  - Functions and arguments should have consistent naming; "add @keywords internal to remove from
    documentation index" for internal helpers.

### cchsflow reality

**if_else2 (263 call sites in R/, 210 in non-legacy):**
- Exported from NAMESPACE: yes (`export(if_else2)`)
- Deprecation signalling: NONE. No `.Deprecated()`, no lifecycle badge, no NEWS entry.
- Project memory explicitly records the v3 decision to "deprecate if_else2() in favour of
  dplyr::case_when in the 3-step architecture" but this is not communicated to users.
- Distribution by file: smoking.R (86), number-conditions.R (25), respiratory-condition.R (21),
  diet.R (19), active-transportation.R (18), age-categorical.R (16), physical-activity.R (11),
  education.R (6), social-provision.R (4), RACDPAL.R (4) -- all live, non-legacy files.
- The equivalent in the v3 layer IS done: calculate_SMKDSTY_original() uses .Deprecated() and
  redirects to calculate_SMKDSTY_cat6(). That is the pattern, but it is not applied to if_else2.

**_fun family (27 exported functions):**
- Old naming: pack_years_fun, CCC_091_fun1, multiple_conditions_fun1/2/3, resp_condition_fun,
  smoke_simple_fun, time_quit_smoking_fun, SMKDSTY_fun, SMKG040_fun, etc.
- Zero deprecation signals on any of these; zero lifecycle badges.
- Per inventory-api-surface-ux.json: both generations exported simultaneously for several
  functions (e.g. pack_years_fun AND calculate_pack_years; smoke_simple_fun AND calculate_smoke_simple).
- The *_fun names predate the verb-first convention and are inconsistent with rOpenSci/tidyverse
  guidance (see Section 3 below).

**Only one function properly deprecated in the codebase:**
  calculate_SMKDSTY_original() → .Deprecated("calculate_SMKDSTY_cat6")
  Source: R/smoking-status.R

### Gap severity: HIGH -- 263 internal if_else2 calls continue with no user warning; 27 _fun exports
have no deprecation signals at all, inviting new external dependencies on legacy API.

---

## 3. Function naming -- three coexisting conventions

### Norm

**Tidyverse design guide (https://design.tidyverse.org/function-names.html):**
  "In general, prefer verbs. Use imperative mood: mutate() not mutated(), mutates(), or mutating()."
  "Use prefixes to group functions together based on common input or common purpose, as prefixes
  are better than suffixes because of auto-complete."
  "Err on the side of too long rather than too short."

**rOpenSci guide (https://devguide.ropensci.org/pkg_building.html):**
  "object_verb() naming scheme for functions operating on common data types"
  "snake_case over all other styles"
  "argument naming and order should be consistent across functions that use similar inputs"

**R Journal naming convention survey (2012, https://journal.r-project.org/archive/2012/RJ-2012-018/RJ-2012-018.pdf):**
  Snake_case + verb-noun is the modern community consensus. Adding generic suffixes like _fun,
  _der, or numbered variants (_fun1/_fun2) conveys no information.

### cchsflow reality

Three generations coexist in the exported API (125 exports):
1. Legacy _fun family (27 exports): pack_years_fun, age_cat_fun, diet_score_fun, CCC_091_fun1,
   multiple_conditions_fun1/2/3, resp_condition_fun, SMKDSTY_fun, SMKG040_fun, SMKG203_fun,
   SMKG207_fun, SPS_5_fun, active_transport1_fun, active_transport2_fun, active_transport3_fun,
   RACDPAL_fun, EDUDR04_fun, energy_exp_fun, pack_years_fun_cat, smoke_simple_fun, time_quit_smoking_fun
2. Verb-first family (43 calculate_ + 8 assess_/categorize_/score_/adjust_)
3. Raw CCHS codes as function names (DPSDPP, DPSDSF, GEN_02A2, LBFA_31A, LBFA_31A_a, LBFA_31A_b)
   plus _der suffix (food_insecurity_der)

Numbered variants are especially opaque: CCC_091_fun1 vs CCC_091_fun2, multiple_conditions_fun1/2/3,
COPD_Emph_der_fun1/2. These encode era-specific logic in a numeric suffix users cannot interpret.

Users cannot predict a function name: is BMI bmi_fun, calculate_bmi, or HWTGBMI? Three answers
have all been valid at different points in the package history.

Evidence: NAMESPACE (grep), inventory-api-surface-ux.json

### Gap severity: HIGH -- naming inconsistency impedes discoverability, is inconsistent with all
three naming standards cited, and makes the worksheet Func:: references a maintenance burden.

---

## 4. Type stability -- if_else2 and mixed output types

### Norm (Tidy design guide, https://design.tidyverse.org/out-type-stability.html)
"The less you need to know about a function's inputs to predict the type of its output, the better."
"A function achieves type-stability when: (1) output type can be predicted from input types alone,
(2) when using ..., argument order doesn't affect output type."
"ifelse() violates type-stability: output type depends on actual values, not just input types.
When the condition is NA, the result type differs from when it's TRUE or FALSE."

The document explicitly flags base::ifelse() as a canonical type-stability violation.

### cchsflow reality

if_else2 wraps ifelse() with an NA→FALSE conversion:
```r
if_else2 <- function(x, a, b) {
  falseifNA <- function(x) { ifelse(is.na(x), FALSE, x) }
  ifelse(falseifNA(x), a, b)
}
```
This inherits ifelse()'s type-instability: the output type depends on the runtime values of a and b,
not just their declared types. In a health data context where missing codes (tagged NAs, numeric,
character strings "NA(a)") mix in the same variable, this is a correctness risk.

The v3 standard (project_derive_function_standards.md) recognized this and mandates dplyr::case_when
instead, which is type-stable (always returns the same type determined by the RHS expressions, not
by runtime values). But 210 non-legacy calls to if_else2 remain, and the function is still exported
and undeprecated.

### Gap severity: MEDIUM-HIGH -- the type-instability is a real downstream correctness risk for
derived variable calculations, not just a style issue; most critical in the 86 calls in smoking.R.

---

## 5. Dependency version pins -- stale and over-pinning

### Norm (R Packages 2e, rOpenSci)
"Specify minimum versions only when genuinely necessary." (rOpenSci)
"you should not specify an unnecessarily broad or narrow range of acceptable package versions."

### cchsflow reality

Minimum pins are deeply stale:
- dplyr >= 0.8.2 (released ~May 2019); current is 1.2.0. The package imports dplyr::do() which has
  been **superseded** since dplyr 1.0.0 (June 2020). The pin promises compatibility with a version
  that predates the supersession of core dispatch functions.
- haven >= 1.1.2 (released ~2017); current is 2.5.5.
- sjlabelled >= 1.0.17; current is 1.2.0.
- R >= 3.5.0 while the team's own standard (CLAUDE.local.md) sets a 4.2.0 floor.

NAMESPACE imports dplyr::do and dplyr::rowwise -- functions that are **superseded** in dplyr
(as confirmed by ?dplyr::do: "*[Superseded]* 'do()' is superseded as of dplyr 1.0.0"). The pin
claims compatibility with dplyr >= 0.8.2 but the dispatch model is already marked for removal.

A future dplyr release removing do() would break every DerivedVar in the worksheets without
cchsflow changing a line of code.

### Gap severity: MEDIUM -- stale pins mislead; the dplyr::do supersession is a ticking clock on
the most critical code path.

---

## 6. Exported internals / API surface discipline

### Norm (R Packages 2e)
"export a function only if it is part of the API you are committing to... the fewer exports,
the easier it is to change things later."
Use `@keywords internal` or `@noRd` for internal helpers; they remain callable via :::

rOpenSci guide: functions should be documented consistently; internal helpers should not appear
in the reference index.

### cchsflow reality

125 exports, of which ~36 are infrastructure plumbing that leaked via blanket @export:
- 10 cache exports: cache_pattern, has_cached_pattern, get_cached_pattern, get_complete_pattern,
  get_missing_pattern, get_missing_pattern_auto, get_missing_pattern_bulk,
  get_variable_missing_pattern, clear_missing_patterns_cache, clear_complete_patterns_cache
- Engine micro-utils: is_equal, if_else2, apply_database_heuristics, auto_detect_database,
  extract_years_from_database_names, find_variable_in_data, assign_missing
- 18 DOCUMENTATION ONLY stubs (functions that always stop())
- 2 _stub exports (calculate_time_quit_smoking_complete_stub, calculate_time_quit_smoking_daily_stub)

Every exported symbol is a compatibility promise the v4 refactor must honour or break.
The cache and heuristics layers are precisely what v4 plans to redesign.

Evidence: inventory-dependencies-namespace.json, inventory-api-surface-ux.json

### Gap severity: HIGH -- freezes v4 freedom; bloats documented API; contradicts R Packages 2e advice.

---

## 7. Superseded dplyr functions in NAMESPACE importFrom

### Norm (rOpenSci, dplyr documentation)
"Import specific functions rather than using Depends." (rOpenSci)
dplyr::do() is marked [Superseded] in the dplyr documentation since 1.0.0 (2020).
dplyr::rowwise() used in the core DerivedVar dispatch pattern.
tidyselect: passing character vectors to select() without all_of() is deprecated.

### cchsflow reality
NAMESPACE: `importFrom(dplyr,do)` and `importFrom(dplyr,rowwise)`
R/recode-with-table.R:938-946:
  recoded_data %>% rowwise() %>% select(used_feeder_vars) %>% do(column_being_added = ...)
  
The `select(used_feeder_vars)` pattern (character vector without all_of()) was deprecated in
tidyselect and will error in future versions. The rowwise/do dispatch is the core DerivedVar
execution path -- it runs for every derived variable in every worksheet call.

This is a dependency management problem: the package pins dplyr >= 0.8.2 while relying on
patterns that dplyr has publicly flagged for removal since 1.0.0.

### Gap severity: MEDIUM -- identified as v4 scope but represents a real breakage risk.

---

## 8. Undeclared dependencies -- R CMD check WARNINGs

### Norm (CRAN policy, R Packages 2e)
Every package accessed via :: must appear in Imports (or Depends). Every package used only in
tests must appear in Suggests. An undeclared Import causes R CMD check WARNING.

### cchsflow reality (per existing inventory)
- `glue` used in R/check-worksheet.R (8 calls) but not in DESCRIPTION
- `stats` used in R/alcohol.R and R/clean-variables.R via stats::setNames() but not declared
- `withr` used in tests/testthat/helper-create-test-csv.R but not in Suggests

These are hard CRAN blockers (R CMD check WARNING class), not just style issues.

### Gap severity: HIGH for CRAN submission.

---

## 9. pkgdown / documentation hygiene

### Norm (rOpenSci, R Packages 2e)
- _pkgdown.yml reference index must reflect actual NAMESPACE exports
- Internal helpers should use @noRd to avoid generating man/*.Rd
- Examples should be runnable (not suppressWarnings-wrapped) and cover the public API

### cchsflow reality
- _pkgdown.yml still indexes 15 deleted v2 functions (bmi_fun, adl_fun, etc.) whose man/*.Rd
  are deleted -- pkgdown::build_site() fails
- 90 of 125 current exports absent from the reference index
- 20 man/dot-*.Rd pages for internal helpers (from check-worksheet.R) that lack @noRd
- 32 exports with no examples; 49 with only \dontrun{}
- 23 man/*.Rd wrap examples in suppressWarnings()

### Gap severity: MEDIUM -- documentation site cannot build from this tree.

---

## Summary priority ranking

| # | Gap | Source | Severity | Effort |
|---|-----|--------|----------|--------|
| 1 | All 9 packages in Depends (not Imports) | R Pkgs 2e, CRAN policy | HIGH | M |
| 2 | Undeclared deps: glue, stats, withr | CRAN policy | HIGH | S |
| 3 | 27 _fun + if_else2 exports with zero deprecation signalling | lifecycle, R Pkgs 2e Ch.21 | HIGH | S-M |
| 4 | 125 exports including ~36 internals | R Pkgs 2e API discipline | HIGH | M |
| 5 | Three naming conventions coexist (_fun, calculate_, bare CCHS) | tidyverse design, rOpenSci | HIGH | L |
| 6 | if_else2 type-instability (ifelse wrapper) | Tidy design type-stability | MED-HIGH | M |
| 7 | Stale version pins; dplyr::do() superseded | rOpenSci dep management | MED | S |
| 8 | pkgdown index broken; 90 exports unindexed | R Pkgs 2e, rOpenSci | MED | S |
| 9 | DESCRIPTION metadata stale (v2.1.0, 2022, R>=3.5.0) | R Pkgs 2e Ch.9 | LOW | S |

---

## Lessons for v4

1. **Open an Imports field immediately**: Move 9 Depends → Imports; the code already uses qualified
   calls or importFrom for all of them. This is S effort but touches DESCRIPTION only.
   
2. **Add lifecycle package to Imports**: Then apply `lifecycle::deprecate_warn()` to if_else2
   and all 27 *_fun exports in one pass. This gives users one release of warning before v4 removal.
   Use `deprecate_soft()` initially (warns only on direct external calls, not package-internal).
   
3. **Superseded vs deprecated distinction**: The *_fun family that will survive but be replaced
   (e.g. pack_years_fun while calculate_pack_years is the preferred path) should get
   lifecycle::badge("superseded") not "deprecated" -- no warning, but documented intent.
   if_else2 which the team explicitly plans to remove should get "deprecated".
   
4. **API triage before v4 freeze**: The 36 plumbing exports should move to @keywords internal
   now, before v4, so the API surface is minimal when v4 breaks compatibility.
   
5. **dplyr::do() migration is a v4 prerequisite**: The rowwise/do dispatch in recode-with-table.R
   is flagged superseded by dplyr. Replacing it with vectorized dispatch (the v3 DV functions
   are already vectorized by design standard) is a prerequisite for any future dplyr update.

---

## Sources

- R Packages 2e: https://r-pkgs.org/
- R Packages 2e, DESCRIPTION chapter: https://r-pkgs.org/description.html
- R Packages 2e, Dependencies in Practice: https://r-pkgs.org/dependencies-in-practice.html  
- R Packages 2e, Lifecycle chapter: https://r-pkgs.org/lifecycle.html
- lifecycle package: https://lifecycle.r-lib.org/
- lifecycle stages article: https://lifecycle.r-lib.org/articles/stages.html
- lifecycle communicate article: https://lifecycle.r-lib.org/articles/communicate.html
- lifecycle deprecate_soft reference: https://lifecycle.r-lib.org/reference/deprecate_soft.html
- rOpenSci Packages Guide: https://devguide.ropensci.org/pkg_building.html
- Tidy design principles, type stability: https://design.tidyverse.org/out-type-stability.html
- Tidy design principles, function names: https://design.tidyverse.org/function-names.html
- Tidyverse style guide, syntax: https://style.tidyverse.org/syntax.html
- goodpractice CRAN checks: https://cran.r-project.org/web/packages/goodpractice/vignettes/goodpractice.html
- CRAN Repository Policy: https://cran.r-project.org/web/packages/policies.html
- dplyr changelog (do() superseded): https://dplyr.tidyverse.org/news/index.html

---

## Cross-reference with existing CEP-017 inventories

This benchmark uses and extends findings in:
- /tmp/v4-research/inventory-dependencies-namespace.json (Depends/Imports, undeclared deps, exports bloat)
- /tmp/v4-research/inventory-api-surface-ux.json (naming families, doc stubs, error messages)

Key new additions not in those inventories:
- Type-stability framing for if_else2 (cited against Tidy design principles)
- Superseded vs deprecated distinction for _fun family
- lifecycle::deprecate_soft() as the right tool for _fun initial phase
- Version pin gap quantified (dplyr 0.8.2 → 1.2.0, haven 1.1.2 → 2.5.5)
- goodpractice::no_description_depends check as an additional authority
