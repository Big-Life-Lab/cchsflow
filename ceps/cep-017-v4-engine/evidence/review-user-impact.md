# CCHS-User Impact Review: v4 Planning Documents

**Reviewer lens:** Epidemiologist with existing cchsflow scripts, secure data-centre analyst (ICES, Statistics Canada RDC) without package-install rights, standalone/copy-paste user.
**Documents reviewed:**
- `review-targets/reunification.md` (engine comparison + recommendation)
- `review-targets/requirements.md` (consolidated v4 requirements, 7 tracks)
- `review-targets/inventory.md` (verified design-issues inventory)
**Evidence base:** `inventory-*.json`, `bench-*.md`, `engines-*.md`, `skeptic-*.json` in `/tmp/v4-research/`

---

## Summary verdict

The planning documents are technically rigorous and methodologically honest. The evidence sweep is thorough (38 of 41 findings confirmed real after adversarial verification), the track structure is coherent, and the deficit/staging framing is exactly right. But read from the user seat, the documents have a significant blind spot: they catalogue what is wrong with the implementation and specify what will replace it, without specifying how existing users get from here to there. Five of the seven v4 tracks change things that user scripts currently depend on -- NA representation, label attributes, DV function names, `output_format`, the API surface -- yet the migration story for each is either a single sentence, deferred to a future artefact, or absent entirely. The one exception is the versioning track (Track 5), which explicitly names the policy requirement. The data-centre / secure-environment use case, mentioned obliquely in two evidence files, receives no treatment in either planning document despite being one of the most common CCHS analyst contexts.

---

## Problem 1 (blocking): NA representation switch has no concrete migration path for existing scripts

**Claim (requirements.md, Track 3, §1):** "One representation at the engine boundary: haven_labelled numeric codes + tagged_na() for missing; as_factor() as explicit opt-in; 'NA(x)' strings only as a deprecated compatibility shim."

**Claim (Track 4, §1):** "Adopt the labelled/haven attribute conventions wholesale ... replacing the bespoke sjlabelled-on-factor form."

**What breaks for existing users:**

The current package emits two output types that many user scripts are already written against:

1. **Categorical variables** arrive as `factor` with levels including `"NA(b)"`. Analysts who wrote `df$DHH_SEX == "NA(b)"` to filter missing, or who used `table(df$DHH_SEX)` expecting `"NA(b)"` to appear, or who passed the factor to a model expecting character-coded levels -- all of these break when the output becomes `haven_labelled` with `tagged_na("b")`.

2. **`output_format = "original"`** in DV functions returns raw numeric codes (e.g., 996). Requirements Track 3 §2 says this will be "replaced" by optional `labelled_spss` output. That is a different class with different behaviour: analysts who relied on `output_format = "original"` getting a plain numeric and testing `df$BMI > 0` get a `labelled_spss` vector whose missingness behaviour differs across base-R functions. The evidence (bench-haven-labelled-declared.md §2.2) documents this explicitly: `mean(labelled_spss_vector)` includes user-NA values, `mean(tagged_na_vector)` excludes them.

**What the documents say about migration:** Track 3 §1 mentions `"NA(x)"` strings as "a deprecated compatibility shim." Track 5 §4 says "v3.0.0 itself follows the policy: shims (option B), migration table in NEWS." That is the entire migration story across both documents. There is no specification of: what the shim does (is `output_format = "original"` retained? does it return plain numerics or `labelled_spss`?), how long the shim lasts (one release? two?), what code analysts should write instead, or whether a migration helper (e.g., `convert_to_v3_output()`) will be provided.

**Evidence the gap is real:** The inventory confirms three NA representations currently coexist in one output dataframe (inventory.md headline item 3; missing-data-architecture.json §3). A data-centre analyst with a script built on `is.na(df$AGE)` returning `FALSE` for `"NA(b)"` factor levels is currently getting wrong results -- but they have never been told this, and the v4 plan does not specify how their script changes after migration.

**Suggestion:** Requirements Track 3 must specify the shim contract: which old output modes are preserved, at what deprecation level, for how long, and what the concrete code change looks like for each. A one-paragraph migration table keyed by: (a) scripts using `output_format = "original"`, (b) scripts testing `== "NA(b)"`, (c) scripts passing cchsflow output to `haven::write_sav()`, and (d) scripts using `is.na()` on categorical output. The v4 plan has good evidence for the correct design; it lacks a bridge for users who are not reading the design documents.

---

## Problem 2 (blocking): Labels overhaul breaks every downstream consumer without a stated compatibility period

**Claim (requirements.md, Track 4, §1-3):** Phase out `sjlabelled`-on-factor; adopt `labelled/haven` conventions; emit `haven_labelled` vectors; drop `labels_long`, `label_long`, `unit` column attributes.

**What breaks:**

The labels layer is documented as "the dimension furthest from its own scoping targets" (requirements.md, Track 4 intro). That means it is also the dimension where the gap between current output and v4 output is widest. Current cchsflow output carries:
- `labels` attribute: named character vector with code strings as values (e.g., `c(Male = "1", Female = "2", "not applicable" = "NA(a)")`)
- `labels_long`, `label_long`, `unit` custom attributes
- Column class: `factor` with levels as code strings

Any user script that:
- calls `attr(df$SEX, "labels")` to extract the code-label mapping
- calls `sjlabelled::as_label(df$SEX)` to decode
- checks `levels(df$SEX)` expecting `"1"`, `"2"`
- uses `labelled::val_labels()` (currently returns `NULL` on cchsflow output -- confirmed in labels-metadata-layer.json §3 live repro)
- passes columns to `gtsummary::tbl_summary()` expecting factor behaviour

...all behave differently under v4. The labels-metadata-layer.json evidence (§4) confirms `labelled::val_labels()` returns `NULL` on current output and that `sjlabelled::as_label()` emits `"NAs introduced by coercion"` warnings. So some of these uses are already broken; v4 fixes them but breaks the workarounds.

**What the documents say about migration:** Track 4 §6 lists "v3.x bug fixes independent of all the above" (tibble indexing, label hard-stop downgrade). Track 5 §4 says "shims (option B), migration table in NEWS." No further migration text for the label attribute changes.

**Suggestion:** At minimum, specify: (a) whether the custom `labels_long`/`unit` attributes are available via an accessor function or are simply gone, (b) whether `set_data_labels()` and `label_data()` remain callable as-is, (c) whether `output_format = "factor"` is a v4-supported mode for users who need factor output for downstream model code. The evidence shows `as_factor()` is the intended opt-in; the documents should say explicitly that `rec_with_table(..., output_format = "factor")` remains supported for at least one release.

---

## Problem 3 (important): API shrink from 125 exports has no shim specification

**Claim (reunification.md, §B, point 4):** "The 125-export surface shrinks to the deliberate tier (engine re-exports, DV functions, discovery tools)."

**Claim (requirements.md, Track 1):** "API shrinks from 125 exports to a deliberate tier."

**What breaks for existing users:**

The 125-export count includes:
- ~30 infrastructure functions (cache layer, heuristics) users may have imported via `:::` or explicitly
- 27 legacy `*_fun` / `*_fun1` / `*_fun2` DV functions that some scripts still call directly (the worksheet `Func::` column references both generations; any user who calls `pack_years_fun()` directly is in the legacy tier)
- 18 documentation-only stubs that unconditionally `stop()` -- removing these is unambiguously good, but any script autocompleted against them and written as `rec_with_table(data, "SMKDSTY_cat5")` will be fine, while a script that accidentally called the stub will now get a different error
- Parameter removals: `log=`, `notes=`, `else_value=`, `append_to_data=`, `custom_function_path=` are candidates for removal (inventory-api-surface-ux.json §3); all have zero observed usage in the repo but could be in user scripts outside the repo

**What the documents say about migration:** Track 5 §4 says "shims (option B), migration table in NEWS." The inventory candidate fix for naming generations (inventory-api-surface-ux.json §2) says "Keep *_fun as deprecated shims for one release with lifecycle::deprecate_warn." This is the correct direction but it appears in the evidence file, not the requirements doc. The requirements document should adopt it explicitly.

**Suggestion:** Requirements Track 1 should include a concrete API tier table: what stays as a first-class export, what becomes a deprecated shim (with which deprecation level and for how long), and what is removed without a shim (the 18 stubs and 6 empty bodies, since calling them already fails). The distinction matters because users have `pkg_check()` on their scripts and a `deprecate_warn` is more tolerable than `stop("DOCUMENTATION ONLY")`.

---

## Problem 4 (important): recodeflow as an Imports dependency is unaddressed for data-centre / secure-environment users

**Claim (reunification.md, §B, point 4):** "cchsflow v4 depends on recodeflow (>= 1.0.0) via Imports."

**The data-centre context:**

A significant fraction of CCHS analysts work in Statistics Canada Research Data Centres (RDCs), ICES analytic environments, or other secure data facilities where:
- Package installation is managed by IT administrators, not analysts
- Packages are approved and installed by request, sometimes months in advance
- Internet access from the analysis environment is typically blocked
- A new `Imports` dependency means the analyst must request approval for *two* packages (cchsflow and recodeflow) rather than one

This is not a hypothetical scenario. The inventory (inventory.md headline item 7) documents that cchsflow currently puts all nine runtime dependencies in `Depends` rather than `Imports`, attaching ~996 symbols to the user's search path -- a CRAN review finding that also has install implications. Adding recodeflow as a new mandatory dependency increases the install-approval burden.

**What the documents say:** Neither planning document mentions data centres, RDCs, secure environments, or the `Imports` dependency burden for managed environments. The reunification document discusses CRAN sequencing as a risk to manage ("recodeflow must publish before cchsflow v4") but entirely from the package-author perspective, not the user-environment perspective.

**What a standalone / copy-paste user needs to know:** Some CCHS analysts at secure facilities use cchsflow by copying function source files and the worksheets, bypassing the package entirely. The reunification changes the structure of what they need to copy: they now need the recodeflow source as well as cchsflow source, and the split between the two repos is not yet specified in the planning documents (which generic infrastructure migrates to recodeflow and which CCHS extensions stay in cchsflow is described only at a high level in reunification.md §B point 3).

**Suggestion:** Requirements should include a one-paragraph treatment of the managed-environment user. Minimally: (a) state whether a CRAN release of recodeflow is a hard prerequisite before cchsflow v4 reaches users (yes, per reunification §B point 4's risks section), (b) document the expected lag (recodeflow v1.0.0 CRAN submission → cchsflow v4 CRAN submission), and (c) state whether an installation bundle or a "fat" cchsflow option (vendor recodeflow engine inline) will be provided for environments that cannot install two packages. The equity principle from the project's constitution makes this more than a convenience question: data-centre analysts at regional RDCs often work with exactly the populations (lower-income, rural, Indigenous) whose health data is most sensitive; blocking their access to v4 for months is a methodological equity problem.

---

## Problem 5 (important): output_format parameter on DV functions disappears without a stated plan for callers

**Claim (requirements.md, Track 3, §2):** "Replace the lossy 'original' output format with optional labelled_spss output."

**Claim (Track 4, §1):** `as_factor()` becomes "explicit opt-in."

**What breaks:**

The `output_format` parameter currently exists on 41 of 51 DV functions and on `rec_with_table()`. Users who specify `output_format = "original"` are requesting raw numeric codes (pre-tagging). Users who specify `output_format = "factor"` (where available) are requesting labelled factors. Under v4 Track 3 §2 the "original" mode is "replaced" by `labelled_spss` output, and Track 4 §1 makes factors an "explicit opt-in" via `as_factor()`.

These are two different things:
- `output_format = "original"` → `labelled_spss`: different class, different `is.na()` behaviour for user-NAs (confirmed in bench-haven-labelled-declared.md §2.2: `mean(labelled_spss_vector)` includes user-NA values)
- `output_format = "factor"` → `as_factor(data)` post-recode: an extra pipeline step, not a function parameter

Additionally, 10 DV functions lack `output_format` entirely (inventory-api-surface-ux.json §7), so there is already inconsistency. The v4 plan should state clearly whether `output_format` is centralised at the engine level (removing it from individual DV function signatures) or retained on each DV function, and what happens to scripts that pass `output_format` to the functions that will no longer accept it.

**Suggestion:** Track 3 should specify: (a) whether `output_format = "original"` is a deprecated alias for `labelled_spss` output or whether it is removed, (b) whether `output_format = "tagged_na"` (currently the default) still works as a parameter name in v4, (c) whether the parameter moves to `rec_with_table()`-level only (matching the staged-pipeline design), and (d) for the 10 DV functions that currently lack `output_format`, whether they acquire it or remain type-fixed.

---

## Problem 6 (important): Missing-data priority change is a silent methodological breakage for existing results

**Claim (requirements.md, Track 3, §4):** "Decide and ship the priority order (the inverted-YAML finding): one decision, documented epidemiological rationale, regression test."

**What breaks:**

This is not a code change that breaks at runtime -- it is a change that produces different numbers silently. The inventory (inventory.md headline item 4; missing-data-architecture.json §2) confirms that the current built-in fallback gives "Not Applicable wins" (`na_a` priority = 1) while the unshipped YAML and the roxygen documentation say "Not Stated wins" (`na_b` priority = 1). Every derived variable whose inputs include both `NA::a` and `NA::b` source rows produces the wrong missing-data priority today on every installation, without any diagnostic other than a startup warning about a missing YAML file.

When the correct YAML ships (if "Not Stated wins" is the decision), existing analyses that relied on the current (wrong) behaviour will produce different derived-variable values. The analysis does not crash; the results just change. For a health-data harmonization package, this is the most dangerous class of change: silent retrospective disagreement with published results.

**What the documents say:** Track 3 §4 calls for "one decision, documented epidemiological rationale, regression test." The staging table includes "priority-order decision + YAML" as a v3.x fix. Neither document addresses what users who ran analyses under the current (wrong) priority should do to verify their results, or whether a NEWS entry will be written explaining the change.

**Suggestion:** The fix is correct and should not be blocked. But requirements should additionally specify: (a) that the NEWS entry for this change explicitly names it as a methodological-output change (not just a bug fix), so users who have published results with the package know to check, and (b) whether a `get_priority_order()` function or similar will let users confirm which priority their version uses. The inventory notes zero tests currently cover `get_priority_missing()`; a regression test as specified is necessary but not sufficient -- a user-facing diagnostic is also needed.

---

## Problem 7 (minor): Column-rename (databaseStart → databaseCoverage) and schema column additions are worksheet-format breaking changes

**Claim (requirements.md, Track 2, §4):** "resolve the dual-semantics columns (rename variables.csv databaseStart -> databaseCoverage or equivalent); define the explicit rowRole distinction for DerivedVar label-definition rows."

**What breaks:**

Any user who maintains their own derived variables using a custom worksheet -- which is the documented recodeflow use case and a subset of cchsflow users -- will have worksheets with the old column names. A v4 engine that expects `databaseCoverage` will silently ignore their `databaseStart` rows (or error, depending on implementation).

The Track 2 §3 schema addition (Frictionless Table Schema `datapackage.json`, new `rowRole` column) is additive, but Track 2 §4's column rename is destructive for existing custom worksheets.

**What the documents say:** No migration path stated for worksheet-format changes. Track 5 §1 notes that "worksheet and variable versions" will be carried in the worksheets themselves, implying worksheet-version-aware loading. But that mechanism is a v4 design item, and the rename is also a v4 item -- the documents do not connect these.

**Suggestion:** Track 2 should specify whether old column names are supported as aliases during a transition period, or whether a `migrate_worksheet()` utility function will be provided. Given that the inventory calls this a v4 item (not v3.x), worksheet authors have lead time, but they need a concrete migration target.

---

## Problem 8 (minor): API shrink removes 18 stubs that were serving as documentation, with no stated documentation replacement

**Claim (requirements.md, Track 1):** "API shrinks from 125 exports to a deliberate tier." The inventory (inventory-api-surface-ux.json §1) notes the candidate fix is "document worksheet-implemented variables as data/topic docs (roxygen @name + @docType or pkgdown articles auto-generated from variables.csv), not exported closures."

**What breaks:**

Users who discovered variables via `?SMKDSTY_cat5` or `?calculate_SMKDSTY_cat5` currently get an (admittedly misleading) help page. The inventory candidate fix proposes `@name` pages so `?SMKDSTY_cat5` still works. But the requirements document does not adopt this explicitly -- it says "API shrinks" without specifying what replaces the documentation channel.

**Suggestion:** Track 1 should explicitly include: "worksheet-implemented variables remain discoverable via `?variable_name` help pages (roxygen @name pages auto-generated from variables.csv) even though the exported stub functions are removed." This is a user-facing documentation commitment, not just an implementation detail.

---

## Strengths of the planning documents

1. **The track structure correctly separates deficit fixes from staging work.** Users reading Track 1-3 understand what is wrong today; reading Tracks 5-7 they understand what is being built for the future. This is honest framing.

2. **v3.x fast-fixes are correctly identified and separated.** The tibble indexing fix (#159), the label hard-stop downgrade, the priority-order YAML, the two buggy stop() messages, and the pmax guard for negative pack years (issue #138) are all v3.x items that do not require the v4 engine. Getting these to users before v4 is the right call -- these are bugs that affect every current user.

3. **The missing-data priority finding is named explicitly as a design decision.** inventory.md §"Design decisions raised" item 1 frames this correctly: not a unilateral fix, but a team decision with epidemiological rationale required. The requirements document inherits this framing.

4. **The reunification recommendation is conservative where it matters.** Option B ships v3 as-is (no engine dependency change in the current release), which is the correct choice for users already waiting for v3. The engine work that affects users lands in v4, after recodeflow v1.0.0 is published.

5. **The `declared` package rejection is correct from a user perspective.** The bench-haven-labelled-declared.md evidence is cited accurately: no vctrs integration means type instability in exactly the tidyverse pipelines CCHS analysts use (bind_rows across cycles, mutate, filter).

6. **Track 6 (logging) directly addresses the top user complaint.** Replacing the per-row vectorized warning mash with classed conditions, aggregated per-variable counts, and a verbosity option is the single change most likely to make analysts trust the output. The connection to "why did my variable not recode" diagnostics is present in the requirements.

---

## Cross-cutting gap: no user migration guide is planned as a deliverable

Neither document mentions a migration guide, upgrade vignette, or `?cchsflow-migration` help topic as a deliverable. For an R package making simultaneous changes to: NA representation, label attributes, DV function names, `output_format` semantics, API surface, worksheet column names, and priority-order results -- a migration guide is not optional documentation; it is a prerequisite for users adopting v4 at all.

The versioning track (Track 5 §2) requires "an explicit breaking-change policy per level, published in CONTRIBUTING." That is the policy; the migration guide is the per-release artefact. Track 5 §4 says "migration table in NEWS" but a migration table buried in `NEWS.md` is not discoverable by a user who opens their script after a `update.packages()` and sees unexpected output.

**Suggestion:** Add to requirements: "A `vignette('cchsflow-migration')` or equivalent artefact documenting v3 → v4 code changes for the five breaking-change areas (NA representation, labels, API surface, output_format, worksheet schema) is a release blocker, not post-release documentation." This is standard practice for tidyverse-ecosystem packages; `tidyr::gather()` → `pivot_longer()` is the familiar template.

---

*Prepared 2026-06-12. Evidence sources: /tmp/v4-research/. Document versions: reunification.md (2026-06-11), requirements.md (2026-06-12), inventory.md (2026-06-11).*
