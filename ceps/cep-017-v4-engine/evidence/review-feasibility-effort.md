# Feasibility and Effort Review: v4 Planning Documents

**Date:** 2026-06-12  
**Lens:** Feasibility / effort -- staging realism, hidden dependencies, effort calibration, minimum viable scope  
**Documents reviewed:** requirements.md, reunification.md, inventory.md  
**Code examined:** cchsflow fix/v3-smoking-worksheet-sync, recodeflow dev (b87e6bd)

---

## Summary verdict

The planning documents are ambitious but not unrealistic for the engine tracks, and the
v3.x list is mostly achievable with one correctable ambiguity. The realistic risk is not
that any single track is impossible, but that the two-repo programme has two structural
vulnerabilities: a critical-path dependency that the documents acknowledge but do not
resolve at the scheduling level, and several tracks whose apparent S/M effort ratings
disguise L/XL coordination costs when examined against the actual code. Three tracks in
particular -- Track 4 (labels), the missing-data architecture migration, and the
two-repo API surface reduction -- are substantially underestimated. The minimum viable
v4, if the full programme cannot be sustained, is outlined in section 5.

---

## 1. Is the v3.x list achievable without the engine move?

The staging table (requirements.md, "Staging summary") lists ten v3.x items. Assessed
against the code:

**Achievable, roughly as stated:**

- databaseStart token fix for DEN_132: Confirmed real. Both variables.csv (line 84)
  and variable_details.csv (lines 534--542) carry `cchs2007_2008p` and `cchs2007_2008m`
  instead of `cchs2007_2008_p` and `cchs2007_2008_m`. Fix is a single-variable
  worksheet edit, plus the registry check added to `check_worksheet()`. S effort.

- Tokenized exact matching in the engine: The unanchored `grepl(database_name, ...)` at
  recode-with-table.R:320 is a one-line change to `strsplit` + `%in%`. Engine change
  only; no worksheet migration needed. S effort.

- `data-raw/` rebuild script: Nothing exists. A clean R script writing the RData
  projections from the CSVs is a half-day task; the CI assertion is another line in the
  existing test suite. S effort.

- Tibble fix (label-utils.R:184--211): Nine occurrences of `data_to_label[, variable_name]`
  must become `data_to_label[[variable_name]]`. Mechanical; confirmed at label-utils.R
  lines 184, 192, 193, 195, 196, 198, 199, 207, 209. S effort, closes issue #159.

- Label hard-stop downgrade (label-utils.R:132--148): The `stop()` on heterogeneous
  `variableLabel` is real (confirmed in code). Downgrading to a warning with first-label
  fallback is a two-line change. S effort.

- The two buggy stop() messages: recode-with-table.R:397 references undefined `row`
  (confirmed in engines-core-diff.md section 2G); the vectorized paste in the other
  location. S effort each.

- `#138 pmax guard` for pack years: Confirmed real in inventory (negative pack years
  reproduced live). `pmax(age - age_first_cig, 0)` plus one regression test. S effort.

- `#139 immigration arms`: Two missing `case_when` arms. S effort, but requires
  Gem/MCP verification of the correct StatCan codes before writing rows.

**Ambiguous or harder than stated:**

- **Priority-order decision + YAML (requirements.md Track 3, item 4; staging table):**
  Listed as v3.x. The code confirms the problem: `load_priority_rules()` at
  missing-data-functions.R:194--198 has `rules <- list(na_a = 1, na_b = 2)` (Not
  Applicable wins) as the built-in fallback, while the unshipped YAML on the
  3-step-tidyverse branch says Not Stated wins. The documents correctly frame this as
  a team decision, not a unilateral fix. However, it is not purely a v3.x item in
  effort terms: once the decision is made, all derived-variable tests must be audited
  for the direction assumed, and if "Not Stated wins" is chosen, the repair tests
  for ADL/alcohol change too (inventory.md, Design decisions #1). The code change is
  S; the test audit and epidemiological rationale documentation are M. The staging
  table understates this by treating it as a single bullet.

**One item that is not achievable without clarification:**

- **`pkgdown` reference regeneration:** listed in the inventory's fast-fixes table
  but not the requirements staging table. Worth noting that 25 deleted topics +
  ~60 new exports in `_pkgdown.yml` is not S effort -- it is M, involving decisions
  about which of the 18 `DOCUMENTATION ONLY` stub functions (confirmed in
  smoke-stop.R:74,120,153,183; smoke-start.R:102,163,304,546,592;
  smoke-intensity.R:209,250,286,322; smoking-status.R:93,145,197,250,306) should
  appear in reference docs vs be hidden. The stub pattern itself is not resolved in
  v3.x.

**Verdict on v3.x list:** 8 of 10 items are S-effort and achievable without the engine
move. The priority-order item is M and requires a team decision first. The stub-function
documentation is not in the staging table but is a blocking CRAN issue that should be.

---

## 2. Hidden dependencies between tracks

### 2A. Track 4 (labels) depends on Track 1 (engine boundary)

The requirements document (Track 4, items 1--3) calls for replacing sjlabelled-on-factor
with haven_labelled + labelled. This is presented as independent of the engine move
("starts in v3.x" for the bug fixes; "v4" for the ecosystem migration).

The dependency is real and is not mentioned: `label_data()` at label-utils.R:183--215 is
called from inside `recode_columns()` (the engine). Replacing sjlabelled's
`set_labels()` / `set_label<-` with labelled's `labelled::set_value_labels()` /
`labelled::var_label<-` requires touching both the engine loop and the post-engine
label attachment step. If the engine is consolidated in recodeflow (Track 1) before the
labels migration (Track 4), the labels work happens once in the right place. If Track 4
is attempted in cchsflow first, it must be re-done or merged after the engine move.
The requirements doc does not sequence these two tracks against each other.

Additionally: sjlabelled is in cchsflow's Depends (DESCRIPTION line 41), not Imports.
Removing it as a Depends entry is a breaking change that affects the user's search path
immediately. recodeflow's DESCRIPTION also has sjlabelled in Imports (line 15). Both
repos will need coordinated sjlabelled removal, and recodeflow's parse-variables-sheet.R
already uses checkmate and purrr without declaring them in DESCRIPTION -- an undeclared
dependency problem that mirrors cchsflow's glue/stats issue (confirmed:
parse-variables-sheet.R:43 uses `checkmate::test_data_frame`; DESCRIPTION Imports does
not list checkmate or purrr). This needs fixing in recodeflow before recodeflow v1.0.0
can go to CRAN.

### 2B. Track 3 (NA representation) depends on Track 1 and Track 4 simultaneously

Replacing `"NA(b)"` factor-level strings with tagged_na throughout requires changes in:
(a) the engine's `recode_variable_NA_formating()` (shared between repos, byte-identical
per engines-core-diff.md section 2I); (b) `clean_variables()` which currently handles
the sjlabelled-on-factor string form via `coerce_cchs_label_strings()` at
clean-variables.R:541ff; (c) all 30+ DV modules that call `clean_variables()` and
pattern-match on `NA::a` / `NA::b` strings in their `case_when` logic. The
requirements doc lists Track 3 as "v4 core" without noting that it is sequenced after
both Track 1 (engine boundary fix) and Track 4 (factor-to-haven_labelled migration),
since the three-representation problem is partly produced by the categorical factor path
in the engine and partly by the sjlabelled attribute form. All three tracks must advance
in concert to avoid a state where the NA representation is partially migrated and the
clean_variables() preprocessing is inconsistent with engine output.

### 2C. Track 2 (worksheet schema) depends on Track 1 for the recStart/recEnd grammar

Track 2 item 3 specifies a Frictionless Table Schema / datapackage.json with "enums for
typeEnd/typeStart/status" and items 3--4 call for the recStart/recEnd grammar
formalization. The inventory confirms the set-notation bug ([7,8,9] drops third value;
recode-with-table.R:571--601). But validating recStart grammar in `check_worksheet()`
is only useful if the engine also enforces it -- otherwise check_worksheet rejects rows
the engine accepts and vice versa. The grammar formalization is therefore a joint Track
1 / Track 2 deliverable. The requirements doc presents it as a Track 2 item with a
Track 1 allusion but does not make the joint dependency explicit.

### 2D. CRAN sequencing for two-repo programme

The reunification recommendation (Option B, step 4) correctly identifies that
"recodeflow must publish before cchsflow v4." What this means concretely:

1. recodeflow must achieve CRAN cleanliness: currently has undeclared checkmate/purrr
   dependencies (confirmed above); sjlabelled in Depends (should be Imports at minimum
   for CRAN); glue used but not declared in parse-variables-sheet.R:118.
2. recodeflow must reach version 1.0.0 -- the current dev HEAD (b87e6bd) has version
   0.1.2 per DESCRIPTION. The gap between 0.1.2 and 1.0.0 includes all Track 1 engine
   work.
3. cchsflow v4 cannot enter submission until recodeflow 1.0.0 is on CRAN.

This sequencing is acknowledged in reunification.md but is not reflected in the staging
table or the track descriptions as a first-order constraint. The realistic consequence
is that cchsflow v4 cannot ship until recodeflow v1.0.0 ships, and recodeflow v1.0.0
requires completing all of Track 1. The staging table should treat recodeflow v1.0.0
as a separate deliverable with its own checklist (including the undeclared dependency
fixes), not merely note it as a "mitigation."

---

## 3. Which v4 items are underestimated?

### Track 4 -- Labels migration: rated as a single v4 track; actual effort is L/XL

The track calls for (1) replacing sjlabelled-on-factor with haven_labelled end-to-end,
(2) phasing out sjlabelled entirely, (3) ensuring labels survive transforms, (4) making
harmonized labels come from variables.csv, and (5) wiring labelled::look_for() into the
discovery module. Each item is independently M--L:

- Replacing `set_labels()` / `set_label<-` (label-utils.R:193,207) with labelled
  equivalents is not just two function calls: it means every downstream consumer that
  uses `sjlabelled::get_label()` or inspects the `labels` attribute directly will get
  different output. The cchsflow vignettes explicitly call `library(sjlabelled)` and use
  `get_label()`. A compatibility shim layer is needed.

- Making labels come from variables.csv (item 4) requires audit of all ~419 variables
  for whether `variableStartLabel` is consistent or era-varying (the inventory notes
  this as the crash cause). The per-variable audit at 419 variables is M effort just
  in verification, before any code changes.

- "Labels survive bind_rows and base subsetting" (item 3) is a property of haven
  _labelled_ that requires the vectors to already be haven_labelled class. But the
  engine currently produces factors (categorical) and numeric (continuous). The
  conversion from factor to haven_labelled vectors changes every column class in every
  output dataframe -- a broad change that tests throughout all 28 test files will need
  to accommodate.

The requirements doc says Track 4 "does both at once" (fixes deficit and stages
dictionary). This is accurate but undersells that doing both at once makes the track
larger and riskier, not smaller. Estimate: Track 4 alone is an XL track (weeks, not
days) requiring careful regression testing across the full 419-variable content.

### Track 3 -- Missing-data architecture migration: rated as v4 core; the CCHS-configuration 
  extraction is underspecified

The requirements doc correctly notes that the pattern cache and clean_variables()
mechanics should migrate in "generic form" to recodeflow, with CCHS-specific
configuration (missing-code families, database heuristics, priority rules) staying in
cchsflow. What is not specified:

- The pattern cache (missing-pattern-cache.R, 1,077 lines) depends on directly reading
  variable_details.csv to extract recStart/recEnd pairs. Making this "generic" means
  defining a configuration interface that recodeflow can accept. The cache currently
  contains `auto_detect_database()` with CCHS-specific heuristics (line patterns for
  `_m`, `_p` suffixes). These need to be parameterized.

- The `NA::a` / `NA::b` hardcodings span not just the engine but all DV functions via
  their `case_when` logic. The inventory lists ~6 sites in the engine; the actual
  occurrence count in DV code is higher (grep finds 64 lines mentioning `NA::a` or
  `NA::b` in R/ files). Replacing these with a general `NA::<any lowercase>` mechanism
  requires a parser change AND updates to every DV function that pattern-matches the
  specific letters.

- `detect_missing_vectorized()` at missing-data-functions.R:248--293 uses an
  element-wise R loop (confirmed in code: `for (i in 1:n) { for (var in vars) {...}}`).
  The inventory correctly notes ~0.3--0.6s per call per 100k rows. Vectorizing this is
  M effort independent of the CCHS migration.

### Track 7 -- DDI export: rated v4 foundation; the mapping work alone is L

The requirements doc specifies `export_ddi()` via DDIwR, mapping worksheets to DDI XML
(variable -> var, catLabel rows -> catgry, NA:: rows -> missing="Y"). DDIwR is not
currently a dependency of either repo, and adding an XML-generating dependency is a
non-trivial CRAN submission consideration. More substantively: the mapping from a
cchsflow worksheet row to DDI 2.5 is not mechanical -- DDI has required elements
(codebook, dataDscr, var/@name, var/@ID) whose values must be derived from the
worksheet columns, and the cchsflow worksheet currently lacks a formal variable ID
column (version/lastUpdated exist but are unvalidated free text per the requirements
doc itself). The "DDI Codebook as dictionary export target" is a v5-complexity item
mis-staged as v4 foundation. The pbc_metadata.yaml prototype is a simple YAML file with
15 keys; it is not a DDI codebook generator. The gap between the prototype and a
functioning `export_ddi()` for 419 variables across 23+ databases is L--XL.

### Track 6 -- Conditions/logging: underestimated due to rlang/cli interplay

The requirements doc calls for classed conditions with machine-catchable metadata and
`.frequency = "once"` replacing three hand-rolled warning-cache environments. The
three existing environments (confirmed: `.priority_rules_cache` in
missing-data-functions.R:151; `.cchsflow_cache` in clean-variables.R:14;
`.get_pattern_warnings_cache()` at clean-variables.R:18) have inconsistent construction
(one lacks `parent = emptyenv()` per the inventory). Replacing them with rlang conditions
requires deciding which conditions are informational vs warnings vs errors, writing the
classed condition constructors, and updating every `stop()` call (91 confirmed in the
inventory). Updating 91 stop() calls is M effort even at a mechanical level, and the
per-condition class naming ("cchsflow_missing_variable", "cchsflow_db_ambiguity") is
design work, not just coding.

---

## 4. Is the two-repo programme realistic for a small team?

**Context from the code:** cchsflow has one maintainer (Kitty Chen per DESCRIPTION),
two regular contributors (Yulric, Doug per git log), and a version number of 2.1.0 with
a Date of 2022-05-05 (the description has not been updated despite v3 work). recodeflow
has Yulric as primary (DESCRIPTION version 0.1.2, dev branch). The PR comment in
memory ("PR #43 scoping is Yulric's") and the reunification doc ("Yulric owns the
engine lane") confirm that the two-repo programme effectively divides into:

- **recodeflow lane** (Yulric): Track 1 engine work, CRAN cleanup, v1.0.0
- **cchsflow lane** (Doug + team): Tracks 2--7, content, CEPs

This division is clear in intent. The realism question is whether the two lanes can
proceed in parallel or whether the cchsflow lane is blocked while waiting for
recodeflow v1.0.0.

**The critical-path gap:** The requirements doc implicitly assumes that cchsflow v4
work on Tracks 2--7 can proceed in parallel with Track 1. For most tracks this is
true in principle: worksheet schema redesign (Track 2), the missing-data priority
decision (Track 3 subset), label bug fixes (Track 4 v3.x), versioning design (Track
5), and conditions design (Track 6) can all be designed and partially implemented
without the engine move. But none of them can be tested end-to-end against the engine
without a stable engine API. The integration tests for Track 4 (labels survive
transforms) require a haven_labelled output from rec_with_table(), which requires Track
1's NA-representation fix. The integration tests for Track 3 (general NA::<type>)
require the engine to accept and emit the new representation. Track 6's classed
conditions wrap rec_with_table() calls.

In practice, a small team working two repos in parallel will encounter one of two
failure modes:
- Work accumulates in cchsflow v4 branches that cannot be integration-tested until
  recodeflow 1.0.0 ships, creating a large merge event with unknown breakage.
- The team serializes the work (wait for recodeflow 1.0.0, then do cchsflow v4), which
  is lower risk but roughly doubles the calendar time.

Neither failure mode is fatal, but neither is acknowledged in the planning documents.

**Verdict on two-repo realism:** The programme is realistic if and only if recodeflow
v1.0.0 is treated as a hard dependency with a published target date, and cchsflow v4
scope for the parallel window is limited to items that can be fully tested without the
new engine API (worksheets, missing-data priority, versioning design, v3.x bug fixes).
As currently written, the staging table does not enforce this constraint.

---

## 5. Minimum viable v4 if the full programme cannot be sustained

If team capacity or the recodeflow critical path proves harder than expected, the minimum
set that delivers a defensible v4 release (addresses the real-world failures users hit)
is:

1. **Engine consolidation in recodeflow** (Track 1): the list-mode loop bug, deprecated
   `do()`, and NA representation fix are the three items with documented data-corruption
   or breakage consequences. The full Track 1 (template variables, grammar, feeder
   overrides) is v4 aspiration; the three data-correctness fixes are non-negotiable.

2. **databaseStart registry and tokenized matching** (Track 2, items 1--2): the
   DEN_132 bug and its class (unanchored grepl matching) are silent data errors that
   ship today. The registry check and exact-match fix are each S effort, engine-engine
   independent, and directly protect data quality.

3. **tibble fix + set_data_labels hard-stop** (Track 4 v3.x): these are the most-hit
   user-facing bugs (issue #159 is open and misdiagnosed; the hard-stop crashes on the
   shipped worksheets). S effort each.

4. **data-raw rebuild pipeline** (Track 2, item 5): prevents future silent RData
   projection drift. S effort; no engine dependency.

5. **Priority-order decision + YAML** (Track 3, item 4): the epidemiological decision
   must be made before v4 ships regardless of which other tracks land.

These five items (all grounded in confirmed data-correctness bugs) constitute a minimum
v4. They can be completed independently of recodeflow v1.0.0 except for item 1 (the
three data-correctness engine fixes, which the reunification doc correctly assigns to
recodeflow). Everything in Tracks 5--7 is genuinely v5 if capacity is constrained.

Track 4's labelled ecosystem adoption (beyond the v3.x bug fixes) and Track 6's full
conditions system are both high value and correctly sequenced for v4 -- but they are M--L
effort tracks that should be explicitly scheduled rather than listed at the same level
as S fixes.

---

## 6. Specific corrections to the staging table

The staging table in requirements.md (final section) should be read with the following
caveats:

1. "priority-order decision + YAML" is listed as v3.x but requires an M-effort test
   audit in addition to the configuration fix; it belongs in the v3.x list only if the
   team decision is made before v3.0.0 ships.

2. recodeflow v1.0.0 itself needs a CRAN-compliance pass before it can serve as
   cchsflow's dependency: undeclared checkmate, purrr, glue in recodeflow (confirmed
   from code). This is not in the staging table at all.

3. Track 4 "labelled adoption" is listed at the same tier as Track 2 and Track 3 in
   the v4 block, but it is XL and requires the most careful rollout plan (shims,
   vignette updates, sjlabelled removal from Depends). It should be called out as the
   highest-effort single track.

4. Track 7 "export_ddi foundation" is listed as v4, but the mapping work from
   cchsflow worksheets (with their current schema deficiencies) to DDI 2.5 XML is
   more realistically v5 unless a working draft of the Track 2 formal schema is in
   place first and the variable ID question is resolved.

5. The staging table does not include a "recodeflow v1.0.0 ships" milestone as a
   gate between the v4 parallel-work phase and the v4 integration phase. Adding this
   gate makes the two-repo coordination explicit and avoids the large-merge failure
   mode.

---

## 7. Minor documentation inconsistencies

- inventory.md credits the list-mode loop bug as "One-line fix; the worst defect
  found." The reunification.md and requirements.md both correctly assign it to Track 1
  (recodeflow engine fix). But the inventory's "fast fixes" table (section: "Fast fixes
  that should not wait for v4") also includes it as an S-effort item, implying it can
  be fixed in cchsflow independently. These two framings are in tension: if the loop
  bug is fixed in cchsflow now, that fix must later be redone in recodeflow. The
  requirements doc should clarify whether to patch cchsflow's copy or wait for the
  engine move. Given how small the fix is, patching both is the right call.

- requirements.md Track 2 item 3 mentions "phantom templateVariable entry" in the
  current YAML schemas as a defect. The actual YAML schemas at
  inst/metadata/schemas/core/ only list `expected_column_order` and `id_column_name`
  (confirmed). The "phantom templateVariable" refers to a column cchsflow's current
  worksheet does not have but recodeflow's engine does. The wording implies a bug in
  an existing schema; it is actually a missing schema entry for a future feature. Low
  severity but worth clarifying to avoid confusion during Track 2 implementation.

- The benchmark evidence (engines-features.md section 3 architectural summary) lists
  recodeflow API surface as "6 exports." Examination of the NAMESPACE confirms this is
  approximately correct (rec_with_table, parse_variables_sheet, get_start_variables,
  set_data_labels, is_equal, is_table_feeder_var/get_table_name). cchsflow's "125
  exports" is confirmed (NAMESPACE: 125 export lines). The claim that "the 125-export
  surface shrinks to the deliberate tier" (reunification.md) is correct in direction but
  the target number is not specified anywhere in the requirements doc. API tiering
  design (inventory.md Design decision #4) is mentioned but not scheduled as a track
  deliverable.
