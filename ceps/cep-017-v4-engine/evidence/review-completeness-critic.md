# Completeness Critic Review: v4 Planning Documents

**Reviewer lens:** Completeness critic  
**Date:** 2026-06-12  
**Documents reviewed:**
- `/tmp/v4-research/review-targets/requirements.md` (v4 consolidated requirements, 7 tracks)
- `/tmp/v4-research/review-targets/reunification.md` (engine comparison + recommendation)
- `/tmp/v4-research/review-targets/inventory.md` (design-issues inventory)
**Evidence base consulted:** all `inventory-*.json`, `engines-*.md`, `bench-*.md`, `skeptic-*.json`, `verify-*.json`, `engines-schema-diff.md`, `/tmp/cchsflow-recovered-specs/`

---

## Executive summary

The three planning documents are technically sound on the content they cover. The
requirements correctly disposition the recodeflow scoping, the inventory findings are
accurately represented, and the reunification recommendation is well-reasoned. However,
the requirements document has five significant completeness gaps:

1. **Testing and CI** is the most consequential omission. The inventory produced a full
   dimension (`inventory-testing-ci.json`) with three high-severity and five lower findings.
   None appear in any track; "testing" is mentioned once in passing. For a project whose
   primary purpose is a v4 engine refactor, having no test strategy in the requirements is
   a serious oversight.

2. **Documentation** — the Divio overhaul promised as a named recodeflow scoping driver
   gets one parenthetical mention. The `inventory-docs-vignettes.json` has four
   high-severity findings (broken pkgdown build, vignettes teaching removed functions,
   vignettes failing on v3 worksheets, Divio gap). No track owns documentation work.

3. **Migration and compatibility for existing users** is not addressed as a track concern.
   The v2→v3 rename wave is unannounced to users; v4 will be a second wave. The only
   treatment is a brief mention of "shims" in Track 5 versioning.

4. **CRAN sequencing and governance of the two-repo split** receives one sentence in
   Risks (reunification doc) and none in the requirements. This is a concrete blocking
   dependency with no owner.

5. **The recovered 2025 specs** promised a Level 7 (`convert_cont_to_cat`) as a first-
   class architectural piece. v4 silently drops it with no disposition.

Several schema-diff findings the synthesis author did not read in full also have no home.

---

## Gap 1: Testing strategy has no track

**Evidence:** `inventory-testing-ci.json` is a full sweep dimension with 9 findings, 3
of which are high severity:

- The engine's only end-to-end regression suite (4 golden-output tests) is completely
  dead: 0 of 4 tests pass because the frozen worksheet references legacy function names
  moved into `R/legacy/` (a directory R never sources). No regeneration script exists.
  The team is heading into a v4 engine rewrite with no working baseline to diff against.
  (`evidence` field: "Rscript run... [ FAIL 4 | WARN 0 | SKIP 0 | PASS 0 ]")

- `check_worksheet()` crashes on every `variable_details` sheet (purrr::keep error on
  `logical(0)`), including the production CSV, because PR #148 nested the YAML schema
  under a `variable_details_schema:` key without updating `load_schema()`. The branch's
  only CI gate (`check-csv.yml`) therefore errors rather than producing diagnostics.
  (`evidence` field: "3 errors at lines 120, 235, 312... purrr::keep '.p() must return
  a single TRUE or FALSE, not an empty logical vector'")

- No R-CMD-check or test-suite CI exists on the v3 lineage. The v3 branch diverged from
  main on 2025-06-03, one day before `R-CMD-check.yaml` landed on main, and never
  received it. A year of active v3 development (smoking, BMI, ADL, alcohol rewrites) has
  run with zero automated test execution.

**The requirements document** mentions "a CI assertion (the Rafidul rebuild episode)"
once (Track 2, item 5 for the RData rebuild script) and references "regression test" for
the priority-order decision (Track 3, item 4). That is the full treatment.

**What is missing:**

a. No requirement to restore a working test baseline before the v4 engine refactor begins.
   Without it, "tests pass after refactor" is an empty claim.

b. No requirement to regenerate the golden-data fixture and commit a generator script.
   The inventory is explicit that the v3 engine rename invalidated the frozen worksheet
   silently (function names in the frozen fixture include `adl_fun`, `bmi_fun` etc. which
   no longer exist).

c. No requirement to add R-CMD-check CI to v3/v4 working branches.

d. No requirement for code-coverage baseline before the refactor. The inventory flags
   that without a coverage baseline, the refactor team cannot know which engine branches
   the existing tests actually exercise.

e. No requirement for synthetic fixtures covering 2019-2023 era and Master files. The
   current sample data stops at cchs2017_2018_p; every v3 mapping for 2019-2023 (the
   headline feature) has no executable regression artifact.

f. No requirement for snapshot tests on `check_worksheet()` structured error lists, which
   would have caught the schema-loading regression immediately.

**Severity: blocking.** The v4 engine refactor is the centrepiece of the programme.
Executing it without a working test suite risks the same silent regression accumulation
that the inventory found on the v3 lineage.

---

## Gap 2: Documentation overhaul has no track

**Evidence:** `inventory-docs-vignettes.json` has 4 high-severity findings:

- `_pkgdown.yml` lists 28 reference topics with no man page; pkgdown build fails.
  90 of 125 current exports are absent from the reference index.
  ("28 _pkgdown.yml reference items have no man/ topic; 90 of 125 NAMESPACE exports...
  missing from the reference index")

- Vignettes teach removed v2 functions (`bmi_fun`, `if_else2`/`ifelse2`) and the
  deprecated monolith DV pattern. `how_to_add_variables.Rmd` is the contributor
  template; it trains contributors to submit v2-style nested-ifelse functions.

- `get_started.Rmd` Example 4 silently produces empty output because `DHHGAGE_A`/`_B`
  were removed from variables.csv; the warning is suppressed by `{r, warning=FALSE}`.

- The Divio gap is explicit in the recodeflow scoping (`index.qmd:37-38`: "The
  documentation not catering to different types of users (not using the divio style of
  documentation)"). The inventory confirms: three distinct audiences (consumers,
  contributors, developers) share one undifferentiated article list; 90 of 125 v3
  exports appear in zero vignettes; architecture rationale lives only in CEPs and
  untracked `docs/`.

**The requirements document** references "Divio docs" once, in the disposition table
(`index/scope framing | ... Divio docs, complex API...`), described only as mapping to
"docs work" with no track assigned. No track contains documentation requirements.

**What is missing:**

a. No track for the Divio restructure: consumer / contributor / developer articles, with
   migration guide (v2→v3 renames, v3→v4 API changes) as a first-class deliverable.

b. No requirement to fix the pkgdown build (currently fails due to deleted man pages in
   the reference index) or to add `pkgdown::check_pkgdown()` to CI.

c. No requirement to rewrite the vignettes that actively teach the replaced architecture
   (`derived_variables.Rmd`, `how_to_add_variables.Rmd`). These are not cosmetic: they
   define the contribution model for external authors.

d. No requirement to add `VignetteBuilder: knitr` to DESCRIPTION and build vignettes in
   CI. The inventory confirms vignettes have never shipped in the installed package (no
   `VignetteBuilder` field in any version including CRAN); `browseVignettes('cchsflow')`
   is empty. This directly contradicts the project's local-first principle.

e. No requirement to update `NEWS.md`, which is frozen at the v2.1.0 release (mislabelled
   as `2.0.1`) with no entry covering four years of breaking changes.

**Severity: important.** The Divio gap is a named user-complaint in the scoping document
that triggered the entire v4 programme. Assigning it to "docs work" without a track means
it has no owner and no requirements to complete.

---

## Gap 3: Migration/compatibility for existing users has no specification

**Evidence:**

- The inventory (`inventory-api-surface-ux.json`) documents three coexisting naming
  generations across 125 exports with no deprecation markers: 27 legacy `*_fun` names,
  51 verb-prefixed v3 names, 6 bare CCHS-name doc placeholders. No `lifecycle::` usage
  anywhere.

- The docs inventory confirms no migration guide exists for the v2→v3 mass renames
  (bmi_fun→calculate_bmi, adl_fun→assess_adl, etc.). Nothing in vignettes, README, or
  NEWS.md mentions any rename.

- DESCRIPTION shows Version 2.1.0 / Date 2022-05-05, unchanged on the v3 branch that
  exports the v3 surface. Users and `renv` lockfiles cannot distinguish the legacy API.

**The requirements document** mentions:
- Track 5, item 4: "v3.0.0 itself follows the policy: shims (option B), migration table
  in NEWS."
- The staging summary: "v3.x ... #138 pmax guard; #139 immigration arms; priority-order
  decision + YAML"

**What is missing:**

a. No specification of which v3 renames require shims and for how long. "Shims (option B)"
   references the versioning policy but does not identify which of the 27 `*_fun` exports
   get shims and which are cut.

b. No requirement to add `lifecycle` to Imports and badge legacy exports as
   `deprecated()` before v4. Without lifecycle badges, every v4 removal appears as
   unannounced breakage.

c. No requirement for a v2→v3 migration document (the renames happened; users deserve a
   mapping table). The v4 requirements inherit this debt.

d. No explicit compatibility promise for the worksheet format. Users who have custom
   worksheets using the v2 column schema (toType/fromType/recFrom/recTo — names that the
   current roxygen still documents) need a migration path.

e. No requirement to bump DESCRIPTION version to 3.0.0-dev before the v4 programme so
   that renv lockfiles start to distinguish the pre- and post-refactor surfaces.

**Severity: important.** Track 5 covers the versioning policy framework but not the
concrete compat work for the existing user base.

---

## Gap 4: CRAN sequencing and two-repo governance have no requirements

**Evidence:** The reunification document (option B) states: "CRAN sequencing (recodeflow
must publish before cchsflow v4)" as a risk, with mitigation "Yulric owns the engine lane
and the scoping is his." The requirements document does not mention CRAN at all.

**What is missing:**

a. No requirement that recodeflow v1.0.0 must reach CRAN (or at minimum a tagged release)
   before cchsflow v4 can be submitted. This is a hard dependency and a potential blocking
   path of months.

b. No governance model for the two-repo split: who owns the cross-repo issue tracker,
   how CEPs span both repos, who has write access to recodeflow, what the release cadence
   agreement is. The inventory mentions "CEP-017 issue inventory becomes a shared backlog
   with explicit repo assignment" — but this is in the reunification doc's recommendation,
   not in requirements.

c. No requirement for a shared test harness. The reunification doc says cchsflow's
   generic infrastructure "migrates down" to recodeflow — but there is no requirement for
   recodeflow to adopt cchsflow-style worksheet validation or for the integration test to
   span both repos.

d. No requirement for license compatibility review. recodeflow's license (GPL-3 is
   explicitly flagged in the benchmark document for `pointblank` and `validate`) needs
   to be confirmed before cchsflow depends on it for CRAN submission.

**Severity: important.** The two-repo architecture is a load-bearing dependency whose
governance is entirely unspecified in the requirements.

---

## Gap 5: Level 7 (convert_cont_to_cat) is silently dropped with no disposition

**Evidence:** The recovered 2025 specs (`/tmp/cchsflow-recovered-specs/`) describe a
Level 7 (`continuous-to-categorical.R`, `convert_cont_to_cat()`) as a first-class
architectural piece:

- `current-simplified-specs/README.md`: "Level 7: Continuous-to-categorical conversion
  (`convert_cont_to_cat()`)" listed as a core infrastructure file alongside Levels 1-6.

- `current-simplified-specs/ARCHITECTURE-REFERENCE.md`: Level 7 has a full specification
  with three exported functions (`get_category_rules`, `apply_category_rules`,
  `convert_cont_to_cat`), integration with Levels 3-6, and the canonical "3-step +
  categorization pattern."

- `current-simplified-specs/MIGRATION-GUIDE.md`: "Level 1-7 Infrastructure -- COMPLETE...
  100+ tests passing, performance validated." Level 7 is in the migration roadmap.

- `lessons/LESSONS_LEARNED.md` describes four iterations leading to the 3-step + Level 7
  pattern as the settled architecture.

**The requirements document** does not mention `convert_cont_to_cat`, Level 7, or
continuous-to-categorical conversion. Track 3 covers missing-data semantics and Track 1
covers the engine, but neither addresses the categorization step that the 2025 specs built
as the extension of the 3-step pattern.

**The inventory** also flags a related gap: `clean_variables()` Step 3 output validation
currently discards out-of-range values (maps to NA) rather than clamping, and the
category-boundary logic has boundary errors on half-open intervals.

**What is missing:**

a. An explicit disposition for `convert_cont_to_cat` / Level 7. Is it absorbed into
   Track 1 engine work? Merged into Track 3 missing-data semantics? Deferred to v5?
   The silence means a complete, tested piece of work from 2025 may be abandoned without
   a decision.

b. No requirement for the metadata-driven categorization boundary logic
   (`get_category_rules()` from variable_details.csv) that the 2025 specs validated. If
   v4 keeps the 3-step pattern, the "step 3" categorization needs specification.

**Severity: important.** The 2025 specs represent completed development investment that
v4 silently ignores.

---

## Gap 6: Schema-diff findings not addressed in requirements

The synthesis author explicitly noted the `engines-schema-diff.md` was not read in full.
The following findings in that document have no home in any track:

**6a. harmonizationStatus vs status field divergence** (schema-diff §2b, §3b):
recodeflow archive schema uses `harmonizationStatus` (enum: development/active/
not_harmonizable/pending_review); cchsflow uses `status` with a different enum
(adds "deprecated", "discontinued"). Track 2 requires schema unification but does not
address this enum-level divergence that will require an explicit migration decision when
check_worksheet is shared.

**6b. The cchsflow production CSV fails its own check_worksheet** (schema-diff §2c):
`ICES.confirmation` appears at position 6 in the production CSV but is absent from the
core schema's `expected_column_order`. Running `check_worksheet()` on the production
`variable_details.csv` — if it were not already crashing on the YAML-nesting bug — would
report a column-order error. Track 2 requires "formal column schema" but does not call
out that the current production CSV already violates its own schema.

**6c. copy in both recStart and recEnd is undocumented** (schema-diff §5, item 1):
Three continuous smoking variables (SMK_06C, SMK_09C, SMK_10C) have `copy` in both
recStart and recEnd. This is a cchsflow-specific pattern for pass-through continuous
variables. The recodeflow schema documents `copy` only as a recEnd value. Track 2's
grammar formalization requirement does not mention the `copy`-as-recStart case, which
will need an explicit grammar rule in the Frictionless Table Schema.

**6d. Validation timing divergence** (schema-diff §6):
recodeflow validates at runtime (when data is processed); cchsflow validates the CSV
file as a pre-flight/CI check. "Neither repo validates field-level content (enum values,
patterns) at this time." Track 2 requires formal column schema with enums — but does not
specify when validation runs (CI pre-flight, load time, or runtime) or who enforces it
(the schema YAML, `check_worksheet`, `parse_variables_sheet`, or the engine).

**Severity: minor for 6a/6b/6c/6d individually; collectively important** because Track
2 is where the schema redesign lives and these are the concrete migration decisions it
must make.

---

## Gap 7: High-severity inventory items with only partial track coverage

### 7a. List-mode recoding silently corrupts data (inventory headline #1)

The inventory calls this "the worst defect found": when `rec_with_table()` receives a
list of dataframes, the loop passes the full database vector instead of the loop variable,
so the first database's rules are applied to every database (`R/recode-with-table.R:
188-203`; reproduced live). It is in the "Fast fixes" table and in the v3.x staging
summary. It is **not in any track requirement**. Track 1 says "consolidate in recodeflow
v1.0.0; cchsflow v4 consumes" but does not list the list-mode bug as a named item to
fix. If recodeflow v1.0.0 inherits the same loop bug from the common ancestor, it will
ship the same corruption.

**Recommendation:** Track 1 should explicitly list the list-mode loop bug as a named
item in the shared-defect fixes, with a test reproducing the distinct-per-database-rules
scenario.

### 7b. Level 2A database-config-loader never migrated (inventory missing-data #3)

`get_missing_pattern_auto()` always errors with "could not find function
load_database_config" — it is an exported function that can never succeed. Track 3 says
"cache keyed by worksheet fingerprint (fixes invalidation and the parallel-worker gap)"
but does not mention the dead Level 2A loader. The dead Level 2A loader is distinct from
the cache-invalidation issue: it means the auto-detection heuristic permanently degrades
to lexicographic sort.

**Recommendation:** Track 3 should add an explicit item: decide and ship or delete
`get_missing_pattern_auto()` and the Level 2A database-config infrastructure.

### 7c. Parallel-worker cache failure (inventory missing-data #4 / Track 3 item 5)

Track 3 item 5 says "cache keyed by worksheet fingerprint (fixes invalidation and the
parallel-worker gap)." The inventory is more specific: caches are process-local under
parallel workers; `use_rdata=TRUE` writes RData into the installed package directory
at runtime (a write race under parallel workers, and a CRAN policy violation). The
requirements mention the parallel-worker gap but not the CRAN policy violation from
writing to the package install directory. This needs explicit language.

### 7d. API stub exports (inventory headline #6)

18 exported `calculate_*` functions have `stop("DOCUMENTATION ONLY...")` as their
entire body. The requirements mention reducing the API surface ("shrinks from 125 exports
to a deliberate tier" in Track 1) but do not explicitly address how worksheet-implemented
variables will be documented after the stubs are removed. The inventory recommends
data/topic docs (`@name` + `@docType` pages). Without a concrete specification, the stub
problem may be deferred into v5.

### 7e. 844MB git history (inventory headline #10)

The inventory's data-artifacts finding on git history ("~420MB of which is 431 committed
versions of `variable_details.csv` (Excel-induced whole-file churn), plus 51MB of
once-committed DDI XML. Clone and CI cost grow monotonically") is in the inventory but
has no track. The requirements staging summary only mentions "stop committing build
products and adopt delta-friendly worksheet discipline" in the design-decisions section,
not as a requirement. Fresh repo vs `git filter-repo` is a one-time decision that should
be made before v4 adds more history.

---

## Gap 8: Performance vectorization requirement is underspecified

**Evidence:** The inventory (`inventory-missing-data-architecture.json`) measures: "element-wise R loops make `any_missing` 0.28s and `get_priority_missing` 0.71s per 100k rows per call vs 0.007s vectorized (~40-100x), with up to ~14 calls per derived variable." A fully vectorized implementation would be 40-100x faster on the hot path.

**Track 1** says "vectorized named-argument derived-variable dispatch" and Track 3 says "Schema-declared patterns with explicit domains." Neither track specifies vectorization of `any_missing()` / `get_priority_missing()` themselves — only the dispatch of DV functions. The 40-100x performance gap on the Level 5 helpers is a user-visible regression for any dataset of realistic size.

**Severity: minor** — the gap is partially addressed, but the Level 5 vectorization should be named explicitly.

---

## What is present and well-specified (strengths)

For balance, the following areas are well-covered:

- **Track 2 (worksheet schema):** The DEN_132 token fix, tokenized matching, Frictionless
  Table Schema, dual-semantics column rename, and RData rebuild are all concrete and
  evidence-cited.

- **Track 3 (missing-data):** The priority-order decision, the three-NA-representations
  problem, and the general NA::<type> extension are all correctly identified and cited to
  specific evidence.

- **Track 4 (labels):** The sjlabelled→labelled migration, the variable-label source-of-
  truth fix, and the v3.x bug fixes (tibble indexing, hard-stop downgrade) are all
  specific and actionable.

- **Reunification recommendation:** The option B recommendation is well-argued, the risks
  are named, and the division of inventory findings between repos is explicit.

- **Recodeflow scoping disposition:** The seven-row table is clear and complete; no scoping
  area is left undisposed.

---

## Summary of missing requirements by urgency

| Priority | What is missing | Evidence source |
|---|---|---|
| **Blocking** | Testing strategy: restore working golden tests; CI on v3/v4 branches; synthetic fixtures for 2019-2023 | `inventory-testing-ci.json` (3 high findings) |
| **Important** | Documentation track: Divio restructure, pkgdown CI, vignette rewrites, VignetteBuilder, NEWS.md | `inventory-docs-vignettes.json` (4 high findings) |
| **Important** | Migration/compat: lifecycle badges, shim inventory, v2→v3 migration doc, worksheet schema compat | `inventory-api-surface-ux.json`, `inventory-docs-vignettes.json` |
| **Important** | CRAN sequencing: recodeflow must reach CRAN first; governance model for two-repo split; license review | `reunification.md` risks |
| **Important** | Level 7 / convert_cont_to_cat disposition: accept, adapt, or explicitly defer | `/tmp/cchsflow-recovered-specs/` |
| **Minor** | Schema-diff items: harmonizationStatus/status enum; copy-as-recStart grammar; validation timing spec | `engines-schema-diff.md` §2b, §5, §6 |
| **Minor** | Specific inventory items: list-mode bug named in Track 1; dead Level 2A loader dispositioned; CRAN policy violation from runtime RData writes | `inventory-missing-data-architecture.json`, `inventory-testing-ci.json` |
| **Minor** | Performance vectorization of Level 5 helpers explicitly named | `inventory-missing-data-architecture.json` |

---

*Full structured summary returned via StructuredOutput tool.*
