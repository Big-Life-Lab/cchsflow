# Review: Engine Comparison and Reunification Recommendation
## Lens: Lead Developer (Yulric) Perspective

**Reviewer:** Subagent standing in for Yulric Sequeira  
**Date:** 2026-06-12  
**Documents reviewed:**
- `review-targets/reunification.md` (engine comparison + option B recommendation)
- `review-targets/requirements.md` (v4 consolidated requirements, 7 tracks)
- `review-targets/inventory.md` (verified design-issues inventory)
**Evidence cross-checked:** `engines-core-diff.md`, `engines-features.md`, recodeflow repo at `/Users/dmanuel/github/recodeflow` (dev branch, HEAD b87e6bd, latest commit 2026-05-26)

---

## Summary verdict

The reunification doc is technically careful and the option B recommendation is defensible. But from Yulric's chair it has three problems that will cause friction in the review meeting. First, it describes recodeflow as "ahead on the engine" without naming what the path to v1.0.0 actually is -- there is no release plan, no scope-to-code mapping, and no timeline anchor. If Doug asks "when will recodeflow be ready?", neither this doc nor any other document in the repo answers that question. Second, the claim that cchsflow's L3-6 migrates "down in generic form" is stated as if it is a small configuration exercise; it is not. The migration is a substantial engineering effort whose scope is not defined. Third, the PR #43 disposition ("harvest and close") is correct in intent but the framing may read as dismissal rather than promotion -- Yulric will want to know that the scoping investment is actually load-bearing in v1.0.0, not just acknowledged and archived.

The requirements doc is stronger. The scoping-to-track mapping table is exactly the kind of synthesis Yulric has been waiting for. The residual problem there is Track 1: it lists outcomes ("three shared-defect fixes land once") without a work plan.

---

## 1. What the doc gets right (genuine strengths)

### 1.1 The "ahead" characterisation is accurate

The function-by-function diff in `engines-core-diff.md` confirms the doc's claims. recodeflow dev (b87e6bd) does have: `recode_non_derived_vars()` extracted as a standalone function (PR #78); explicit `for` loop replacing `rowwise()+do()` for non-derived vars (PR #80); overlap detection; `parse_variables_sheet()` with structured error returns (PR #85, merged 2025-12-30); `get_start_variables()` typed dependency graph (PR #84); `get_feeder_vars()` with three-pattern regex for database-specific overrides; template variable expansion; scalar/string constants as feeders (PR commit 7cb372f); `is_table_feeder_var()`/`get_table_name()` reference-table support; and an end-to-end integration test with RData snapshot diffing. The doc does not exaggerate.

The most recent dev commits are from 2026-05-26 (PR #87: databaseStart separator standardisation) and 2026-05-19 (PR #86: validate databaseStart references in variableStart). Yulric is actively working the engine. The characterisation of "2025 progress" undersells the 2026 activity.

### 1.2 The "both share the same defects" framing is correct and productive

Identifying that `recode_variable_NA_formating()` is byte-identical in both repos is the strongest single argument for option B. Any team member who checks the diff will confirm it. This framing turns a turf question into a coordination problem, which is resolvable.

### 1.3 The scope-to-track mapping (requirements doc) is the clearest synthesis in either document

The table mapping each scoping doc to a verdict and a track (requirements.md lines 26-36) does what PR #43 could not complete while the engine decision was open. Yulric will recognise that the missing-data scoping, labels scoping, and versioning scoping are all finding concrete homes in Tracks 1-5. This is the section most likely to get a positive response.

### 1.4 PR #43 disposition is accurate

"Harvest the scoping (both layers, including the preserved catalog.qmd) into recodeflow's v1.0.0 requirements, then close it -- it has done its job" is the right call. The scoping branch exists solely to inform a version release; it is not itself a deliverable. Yulric will agree with the logic even if the phrasing feels abrupt.

---

## 2. Problems Yulric will push back on

### 2.1 "recodeflow v1.0.0" has no release plan and no scope definition (blocking)

**Claim (reunification.md lines 70-84):** "v4 engine work happens in recodeflow, which becomes v1.0.0. The three shared-defect fixes land there once..."

**What the repo shows:** recodeflow is currently at v0.1.2 (DESCRIPTION file). The CHANGELOG shows only one [Unreleased] entry: `get_start_variables`. NEWS.md has two entries, the most recent being 0.1.2 (tibble crash fix). There is no milestone, no issue label for "v1.0.0", no feature list, no timeline. The scoping documents live on a separate `scoping-doc` branch that was never merged to `dev` or `main`. The scope-docs are rendered HTML in `scope-docs/dist/` but the source `.qmd` files are also only on `scoping-doc`.

**Yulric's likely question:** "You say recodeflow becomes v1.0.0, but what exactly is in v1.0.0, and who decides what scope is included before cchsflow v4 can depend on it? You've listed three defects to fix, but the scoping document has seven dimensions. Are all seven in scope for v1.0.0? If I have to ship logging, versioning, catalog, and the label overhaul before cchsflow v4 can depend on me, that's a multi-year project."

The reunification doc does not answer this. It lists fixes that need to happen ("three shared-defect fixes") without specifying the full v1.0.0 scope. As written, cchsflow v4 development cannot start until recodeflow defines and ships v1.0.0, but v1.0.0 is undefined.

**Suggested addition:** The doc needs a minimal v1.0.0 scope: the engine fixes that cchsflow v4 requires as Imports. Everything else (logging, versioning, catalog) can be v1.1+ and developed in parallel with cchsflow v4. The doc should explicitly say that cchsflow v4's Imports dependency requires only the engine-fix subset, not all seven scoping dimensions.

### 2.2 The L3-6 migration is understated as a "generic form" configuration exercise (blocking)

**Claim (reunification.md lines 77-82):** "cchsflow's levels 3-6 migrate down in generic form -- the pattern cache, cleaning mechanics, and range parser take a configuration object instead of CCHS defaults; cchsflow supplies the CCHS configuration and keeps check_worksheet()'s CCHS conventions, the DV library, content, and CEPs."

**What the code shows:** Levels 3-6 are 830 (`clean-variables.R`) + 492 (`missing-data-functions.R`) + 1,077 (`missing-pattern-cache.R`) = ~2,400 lines. The CCHS-specific assumptions are not cleanly separated into a configuration layer. From `engines-features.md` section 2.1: the 3-step architecture processes CCHS missing codes (6/7/8/9 or 996/997/998/999), manages YAML priority rules with CCHS rationale, uses session-level caches keyed by (variable, database), auto-detects databases via CCHS heuristics, and applies database-specific heuristics. The `auto_detect_database()` function and `apply_database_heuristics()` are not generic primitives with CCHS configuration -- they are CCHS-first implementations.

**Yulric's likely question:** "What does 'parameterized so CCHS-specific configuration stays in cchsflow' actually mean in practice? Does recodeflow v1.0.0 grow 2,400 lines of new infrastructure? Who writes and maintains that? The missing-data architecture is cchsflow's most complex subsystem. Moving it to recodeflow in 'generic form' is a major effort -- how is that not just duplicating work a second time, which is what option A was criticised for?"

The doc treats this migration as a design decision ("the natural moment to resolve them") but does not quantify the effort or specify who does the work. If Yulric is responsible for recodeflow v1.0.0 and the expectation is that he absorbs cchsflow's missing-data architecture in generic form, that is a very different workload than "fix three shared defects."

**Suggested fix:** Either (a) defer the L3-6 generic migration to v1.1 and have cchsflow v4 keep its own missing-data layer for the initial release, adding a clean internal API boundary without full extraction, or (b) explicitly state that Doug / the cchsflow team authors the generic form in cchsflow first and only opens PRs to recodeflow when the separation is proved stable.

### 2.3 "CRAN sequencing" is treated as a risk to manage, not a prerequisite gate (important)

**Claim (reunification.md lines 87-89):** "CRAN sequencing (recodeflow must publish before cchsflow v4) ... Risks to manage."

**The actual situation:** recodeflow is currently at v0.1.2 and has never been released on CRAN (the DESCRIPTION, NAMESPACE, and test suite would not pass current CRAN checks -- `checkmate` appears as an undeclared dependency in `parse-variables-sheet.R`, and the exported API is 6 functions, which is fine, but the integration test depends on HUIPoRT data that is not suitable for CRAN distribution in its current form). Getting recodeflow to a CRAN-publishable v1.0.0 requires: declaring all dependencies correctly, resolving the `dplyr::do()` import (still in NAMESPACE, still called from the cchsflow copy, though recodeflow dev has already replaced it for non-derived vars), ensuring the integration-test data is publicly distributable, adding a `data-raw/` pipeline for test fixtures, and achieving 0 errors/warnings on R CMD check.

This is not a risk to "manage" at the end -- it is a hard gate that should be on the critical path. If recodeflow cannot reach CRAN before cchsflow v4, option B fails. The doc names this as a risk but does not describe the path.

**Yulric's likely question:** "Has anyone run R CMD check on recodeflow dev recently? Is it CRAN-ready? What's the gap to CRAN acceptance, and whose job is it to close that gap?"

### 2.4 The "ahead" claim omits that both repos still share the dplyr::do() defect at the level of cchsflow consumption (important)

**Claim (reunification.md line 23):** recodeflow "swapped do() for a for loop."

**What the code shows:** recodeflow's `recode_non_derived_vars()` does use an explicit for loop. But the NAMESPACE file still imports `dplyr::do` and `dplyr::rowwise`. Looking at `engines-core-diff.md` section 2E: the `recode_derived_vars` function in recodeflow still uses a `for` loop over `seq_len(nrow(recoded_data))` building a scalar call per row -- it is an explicit loop, not `rowwise()+do()`, but it is still a per-row scalar dispatch pattern. The do() removal was partial: the non-derived path was refactored, but the derived-variable path still calls `calculate_custom_function_row_value()` per row using `do.call(get(custom_function_name), unname(row_values))`. This is not vectorized named-argument dispatch.

The requirement in Track 1 for "vectorized named-argument derived-variable dispatch" is a new thing neither repo has. The doc implies recodeflow is closer to this goal than cchsflow, but neither has implemented it. Yulric may push back on the implication that this work is small because recodeflow has "already refactored."

**Suggested correction:** State clearly that the vectorized dispatch is new work in both repos, not an extension of recodeflow's existing refactor. Estimate effort separately.

### 2.5 The column-name divergence is noted but its migration cost is not addressed (important)

**From `engines-core-diff.md` section 3:** The label column mapping is semantically diverged. recodeflow uses `label`/`labelLong` from the variables sheet; cchsflow uses `variableStartShortLabel`/`variableStartLabel` from the variable_details sheet. When cchsflow v4 adopts recodeflow as Imports, `set_data_labels()` will need to be reconciled -- but which convention wins? If recodeflow's convention wins, cchsflow's worksheets (419 variables, 3,839 detail rows) need to be updated. If cchsflow's convention wins, recodeflow's worksheet examples, tests, and integration fixtures need updating. This is a breaking change in either direction.

The doc does not name this migration cost. Yulric will ask: "If I'm the engine maintainer and cchsflow v4 depends on recodeflow, do I have to restructure my worksheet schema to match cchsflow's, or does cchsflow restructure to match mine? Who decides, and when?"

### 2.6 No mention of the `get_start_variables` API already exported (minor)

The doc says recodeflow has "a typed start-variable dependency graph (`get_start_variables()`)" as a recent development. This is accurate -- it was added in PR #84 (commit 6ae70e6, 2025-07-31). But the NAMESPACE shows it is not currently exported. `parse_variables_sheet`, `rec_with_table`, `set_data_labels`, `is_equal`, `is_table_feeder_var`, `get_table_name` are exported. `get_start_variables` is not. Whether this is intentional (internal-only for now) or an oversight affects how the doc should characterise this as a v1.0.0 deliverable.

---

## 3. Questions Yulric will ask that the doc does not answer

### Q1. What is the minimum viable recodeflow v1.0.0 scope that unblocks cchsflow v4?

The doc lists the v4 engine work in recodeflow: three defect fixes + absorption of existing advances. But "absorption of existing advances" includes template variables, feeder overrides, semantic validation, constants as feeders, and the typed dependency graph -- features already in dev. Does v1.0.0 mean "tag what's on dev, fix the three defects, and ship"? Or does it mean "also absorb the L3-6 generic form, implement vectorized dispatch, and build the condition/logging system"? These are very different scopes. The doc does not say.

### Q2. What is the timeline, and what is blocking it?

The doc mentions recodeflow's release cadence becomes cchsflow's critical path, with the mitigation being "Yulric owns the engine lane." But Yulric works on recodeflow alongside other commitments. What is the expected timeline for recodeflow v1.0.0? Six months? Two years? Is there funding? Is there a GitHub milestone?

### Q3. Who authors the generic L3-6 layer?

The doc says L3-6 "migrates down in generic form" but does not say who does the migration. This is a 2,400-line codebase. If it is Yulric's responsibility, he needs to know. If it is Doug's team, Yulric needs to be able to review and accept PRs without being blocked by CCHS domain knowledge he does not have.

### Q4. How does the `databaseStart` separator divergence get resolved?

The most recent commit to recodeflow dev (2026-05-26, PR #87) standardises the databaseStart separator to commas in tests. cchsflow's worksheets use a different separator (space-separated, from the schema diff analysis). If cchsflow v4 imports recodeflow, `rec_with_table()` will use recodeflow's separator expectation. This is a breaking change for every cchsflow user. The doc does not mention this.

### Q5. What happens to recodeflow's existing users and the HUIPoRT project during the v1.0.0 transition?

The HUIPoRT integration test in recodeflow uses `id_role_name = "id"` and `append_non_db_columns = TRUE`, which are features recodeflow has but cchsflow does not. If recodeflow v1.0.0 is the shared engine, HUIPoRT workflows need to continue working. Is HUIPoRT a constraint on recodeflow v1.0.0's API? The doc does not say.

### Q6. Does option B require a version lock between cchsflow v4 and recodeflow v1.0.0?

If cchsflow v4 declares `Imports: recodeflow (>= 1.0.0)`, what happens when recodeflow ships v1.1.0 with a breaking change? The versioning scoping addresses this at the level of worksheets and variables, but not at the package-dependency API level. Does recodeflow commit to a stable engine API for cchsflow's sake? Who arbitrates?

---

## 4. Specific doc claims that need correction or qualification

### 4.1 "recodeflow dev is ahead on the engine... Since 2025 it has: extracted and refactored the recode loops... overlap detection... database-specific DerivedVar:: feeder overrides... template variables... scalar and string constants as worksheet feeders; reference-table feeders..." (reunification.md lines 14-26)

**Factual accuracy:** All confirmed correct by cross-checking the repo. The commit dates: overlap detection and extracted `recode_non_derived_vars` arrived in 2025 (PRs #78-#80); `get_start_variables` in 2025 (PR #84); `parse_variables_sheet` merged 2025-12-30 (PR #85); most recent engine work in 2026 (PRs #86, #87). "Since 2025" is accurate but slightly undersells the 2026 activity.

**Suggested revision:** Add "and through 2026" or cite the two 2026 PRs to give a complete picture.

### 4.2 "a Dublin Core dataset-metadata prototype" (reunification.md line 24)

**Factual accuracy:** Correct. `inst/extdata/pbc_metadata.yaml` exists on dev and contains Dublin Core fields. But this is a data object shipped with an example dataset, not a functional API. The word "prototype" is appropriate, but calling it part of what makes recodeflow "ahead" on the engine may be misleading -- it is a data file, not a function.

### 4.3 "the formal engine scope documents (PR #43)" (reunification.md line 25)

**Factual status:** PR #43 does not appear to exist as a GitHub PR in the recodeflow repository based on available information. The scope documents live on the `scoping-doc` branch (never merged), and the `scoping-doc-catalog` branch (the in-progress responses to Yulric's questions from Doug). The most recent substantive commit to `scoping-doc-catalog` was 2024-12-04 ("in-progress -- responses to Yulric questions in scope.qmd"). There is no evidence of a PR #43 in the recodeflow issue/PR history visible from the git log. If PR #43 refers to a GitHub PR that is open but unmerged, the doc should clarify its status. If it refers to the scoping-doc branch, it should name the branch.

**Impact:** Yulric will be confused if the doc references "PR #43" and he cannot find it.

### 4.4 "an end-to-end integration test with snapshot diffing" (reunification.md line 24)

**Factual accuracy:** Correct. `tests/testthat/test-integration.R` exists with `expect_snapshot_file` and a custom `compare_snapshot()` helper that uses `waldo::compare`. But this test uses HUIPoRT data, not CCHS data. The doc's framing suggests this is comparable to cchsflow's integration-testing gap; it is, but Yulric should note that this test does not cover any of cchsflow's content.

### 4.5 The effort estimate for absorbing cchsflow's L3-6

The doc implies this is M-effort ("parameterized so CCHS-specific configuration stays in cchsflow"). Based on the code: `clean-variables.R` is 830 lines of domain logic; `missing-pattern-cache.R` is 1,077 lines with session-level environments, invalidation logic, and CCHS heuristics; `missing-data-functions.R` is 492 lines with YAML-driven priority rules. Making this "generic" requires: (a) designing a configuration interface that is not CCHS-specific; (b) separating the CCHS-specific logic (hardcoded NA::a/NA::b, 996-999 missing codes, database heuristics) from the generic logic (cache keying, pattern extraction, range parser); (c) testing the generic form against non-CCHS data; (d) having cchsflow adopt the new configuration-object API. This is L-effort, not M-effort. The doc should say so.

---

## 5. What Yulric will likely accept without pushback

- The diagnosis of shared defects (NA representations, dispatch, grammar) as fixes that belong once in recodeflow -- this is architecturally sound.
- The "v3.0.0 ships as-is" provision (step 1 of the recommendation) -- no one is asking for a mid-flight engine swap.
- The PR #43 closure framing, provided the scoping investment is visibly honoured in v1.0.0 requirements.
- The division of inventory issues by repo in the final section -- it respects ownership boundaries.
- Track 2 (worksheet schema) requirements -- the DEN_132 tokenisation fix and exact-match database lookup are long-overdue, well-scoped, and do not require cross-repo coordination.
- Track 5 (versioning) and Track 6 (logging/conditions) -- these map cleanly to the recodeflow scoping docs and Yulric will recognise his own design targets.

---

## 6. Recommendations before sharing with Yulric

1. **Add a minimal v1.0.0 scope definition.** Specify the minimum recodeflow v1.0.0 that makes option B viable: fix the three shared defects + ship what is already on dev (PRs #78-#87). Explicitly defer the L3-6 generic migration and the seven full scoping dimensions to v1.1+. This makes the timeline tractable.

2. **Be explicit about who does the L3-6 generic migration and when.** If it is Doug's team writing the PRs against recodeflow, say so. If it is deferred to v1.1 and cchsflow v4 ships with its own missing-data layer under a clean API boundary, say that. Do not leave it as a design decision to be resolved "during the natural moment."

3. **Address the databaseStart separator divergence.** PR #87 (recodeflow dev, 2026-05-26) standardised separators in tests. cchsflow uses a different convention. Name this as a migration requirement for cchsflow worksheets or as a compatibility shim in the engine.

4. **Clarify PR #43's identity.** If the doc references a GitHub PR, verify it exists and link it. If it refers to the scoping-doc branch, name the branch.

5. **Add a CRAN pre-flight checklist for recodeflow.** The doc names CRAN sequencing as a risk; mitigating it requires naming what is needed (undeclared `checkmate` dependency, `do.call` pattern review, integration-test data review, R CMD check pass). This can be a separate item on the shared backlog.

6. **Acknowledge the 2026 recodeflow activity.** The doc says "Since 2025" but PRs #86-#87 are 2026. Updating this is a small change that shows Yulric's recent work is visible and valued.

---

## 7. The question the doc implicitly answers but never states explicitly

The real question for Yulric is: "Am I being asked to do more work or less work under option B than under the status quo?"

The doc should answer this directly. Under option B:
- Yulric is not asked to absorb cchsflow's content, DV library, or CEP process. Those stay in cchsflow.
- Yulric is asked to ship recodeflow v1.0.0 with the three engine fixes and the features already on dev. This is close to tagging what already exists.
- The L3-6 generic migration is additional work (Doug's team authors, Yulric reviews) whose scope and timeline need agreement.
- The seven scoping dimensions are the roadmap for v1.1+, not blocking v1.0.0.

If this is the actual plan, stating it plainly will get option B accepted quickly. The current draft buries this message under architecture language about "migration" and "parameterisation" that makes the effort sound larger than the minimum viable case actually is.
