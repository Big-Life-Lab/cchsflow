# Internal Consistency Review: CEP-017 v4 Planning Documents

**Reviewer:** Internal consistency lens
**Date:** June 12, 2026
**Documents reviewed:**
- `review-targets/reunification.md` (2026-06-11)
- `review-targets/requirements.md` (2026-06-12)
- `review-targets/inventory.md` (2026-06-11)
- CEP-017 earlier docs (v4-planning branch):
  - `2026-06-10_v4-scope-outline.md`
  - `2026-06-10_three-step-architecture-review.md`

---

## Blocking problems

### 1. The scope outline's v4 non-goal directly contradicts the requirements' core recommendation

**Scope outline (June 10) non-goals:**
> "A cchsflow/recodeflow package split -- revisit after the engine is modular."

**Requirements Track 1 (June 12):**
> "Per the reunification recommendation (option B): consolidate in recodeflow v1.0.0; cchsflow v4 consumes."

**Reunification (June 11):**
> "cchsflow v4 = content + DV library + CCHS extensions, with recodeflow as an Imports dependency."

**Requirements v5+ staging:**
> "recodeflow/cchsflow split polish"

The scope outline explicitly defers the package split to "after the engine is modular." Option B in the requirements makes cchsflow v4 a downstream consumer of recodeflow -- that is the split, happening in v4. The v5+ entry "recodeflow/cchsflow split polish" then tags polish for v5+, implying the split itself is v4, directly contradicting the scope outline's non-goal.

The documents have superseded each other without saying so. The requirements should explicitly retire this non-goal with a dated rationale, not leave it standing in the earlier doc.

---

### 2. The arch review's five-phase engine sequence is for cchsflow; Track 1 redirects the same work to recodeflow -- but does not acknowledge or supersede the phases

**Three-step arch review (June 10), recommended v4 sequence:**
1. Vectorize the dispatch -- "Replace `rowwise()`/`do()` with one named-argument `do.call(fn, as.list(columns))` per derived variable."
2. Unify range parsing.
3. Route legacy NA formatting through the pattern cache.
4. Adopt `apply_else_logic()` and cached worksheet access in `recode_columns()`.
5. Decompose `rec_with_table()` into stages.

Every one of these five steps modifies `recode-with-table.R` and `recode_columns()` in cchsflow.

**Scope outline (June 10), phased sequence:** Same five phases, same cchsflow targets.

**Requirements Track 1 (June 12):**
> "Per the reunification recommendation (option B): consolidate in recodeflow v1.0.0; cchsflow v4 consumes."

The engine work for the same three shared defects (dispatch, NA representation, grammar) now lands in recodeflow, not cchsflow. The arch review's five phases are logically invalidated by Track 1, but the arch review is never amended or cross-referenced to say so. A reader following the arch review's sequence would build a v4 cchsflow engine that Track 1 says should not exist.

The requirements must state explicitly: "the five-phase sequence in the arch review is superseded; those transformations happen in recodeflow's v1.0.0 milestone."

---

### 3. The list-mode data-corruption bug is simultaneously a "fast fix that should not wait for v4" and assigned to recodeflow as v4 work

**Inventory, fast-fix table (the worst defect found):**
> "List-mode loop bug: pass `data_name`, not `database_name` | recode-with-table.R:188-203 | S [effort]"

The inventory section preamble states these fixes are "worth folding into the v3 release window."

**Reunification, inventory division:**
> "the engine defects (NA representations, dispatch, grammar, **list-mode class of bugs**) are fixed in recodeflow"

**Requirements v3.x staging table:** List-mode bug is absent.

These cannot both be correct. Either the one-line fix ships now in cchsflow (inventory's verdict), or it waits for the recodeflow engine consolidation (reunification's assignment). The reunification lumps it with architectural defects it calls "list-mode class of bugs," but the actual fix is a single variable-name correction in a cchsflow loop that will exist until v4 ships. The requirements staging table omits it from v3.x without explanation. The current result is that the #1 ranked defect -- "silent data corruption," reproduced live -- has no home in v3.x and a deferred home in recodeflow.

---

### 4. The priority-order decision appears in both the v3.x staging table and inside a v4 track, assigning it to two release windows simultaneously

**Requirements, staging summary:**
> "v3.x (no engine dependency) | ... priority-order decision + YAML"

**Requirements Track 3 header:** `## Track 3 -- Missing-data semantics (deficit; v4 core)`

**Requirements Track 3, item 4:**
> "4. Decide and ship the priority order (the inverted-YAML finding): one decision, documented epidemiological rationale, regression test."

Track 3 carries no v3.x marker on individual items; its header says "v4 core." Item 4 inside that track is the same work the staging table places in v3.x. The two-release-window assignment is irresolvable as written. The team needs a single home: the decision and YAML ship do not depend on the engine and should be v3.x; but if the rationale requires the schema redesign from Track 2 (which is v4), that sequencing constraint must be stated.

---

## Important problems

### 5. The inventory status header is stale: it says "six of ten planned sweep dimensions complete" but the coverage section confirms all ten are done

**Inventory status header (line 4):**
> "Status: Draft -- six of ten planned sweep dimensions complete"

**Inventory coverage section (last section):**
> "The evidence program is complete: all ten sweep dimensions, the adversarial verification pass, the engine comparison, and the ecosystem benchmark."
> "All ten dimensions on disk (evidence/inventory-*.json)"

The header was written mid-sweep and never updated. A reader stopping at the header believes 40% of the evidence is missing. The "78 confirmed-style findings across six dimensions" count in the body is also stale: the ten JSON files contain 131 total findings (76 from the six dimensions documented in the body + 55 from the four additional dimensions). The body text should be updated to acknowledge all ten dimensions and revise the headline count.

---

### 6. "option B" refers to two completely different decisions across documents, with no disambiguation

**Scope outline, decision log:**
> "v3.0.0 release takes option B -- deprecation shims (`R/deprecated-aliases.R`) for renamed v2 functions, removed in v4."

**Reunification doc:**
> "B. Engine consolidates in recodeflow v1.0.0; cchsflow v4 consumes it."

**Requirements Track 5.4:**
> "v3.0.0 itself follows the policy: shims (option B), migration table in NEWS."

Here "option B" almost certainly means the shim convention from the scope outline, not the engine option from the reunification doc -- but a reader of the requirements in isolation cannot tell. The reunification's option B is the architectural decision that governs all of Track 1; the scope outline's option B governs backward compatibility in v3. Using the same label for both in adjacent documents is a genuine comprehension hazard.

---

### 7. The scope outline's goal "users can request original codes through the standard workflow" is contradicted by requirements Track 3.2

**Scope outline, goal 3:**
> "users can request original codes through the standard workflow."

**Requirements Track 3.2:**
> "Replace the lossy 'original' output format with optional `labelled_spss` output (codes preserved with their missingness declaration), via `labelled::tagged_na_to_user_na()`."

The scope outline preserves `output_format = "original"` as a user-facing option. Requirements Track 3.2 replaces it with `labelled_spss`. These are incompatible: the scope outline's commitment is that the `original` request mechanism continues to work; Track 3.2 removes the named format and replaces the mechanic. The requirements should say explicitly that `output_format = "original"` is deprecated in favour of the new parameter value, and whether a compatibility shim is provided.

---

### 8. Three inventory fast-fixes that "should not wait for v4" are absent from the v3.x staging table, with no rationale for the omission

The inventory fast-fix table marks eight items as S-effort and "worth folding into the v3 release window." The requirements v3.x staging table captures only some of them. Missing from v3.x:

| Fast fix | Inventory location | Severity |
|---|---|---|
| List-mode loop bug (data corruption) | recode-with-table.R:188-203 | High -- #1 headline finding |
| Declare `glue` + `stats` in Imports (CRAN blocker) | check-worksheet.R:274ff | High -- CRAN submission blocker |
| `rec_with_table()` roxygen wrong defaults for `append_to_data` and `notes` | recode-with-table.R:113-118 | Medium |
| pkgdown reference regeneration (25 deleted topics, ~60 new exports missing) | _pkgdown.yml:61-92 | High (docs) |

The CRAN blocker (`glue` and `stats` undeclared) is a CRAN submission prerequisite. It appears nowhere in any requirements track. The list-mode bug is discussed under problem 3 above. The pkgdown regeneration and the roxygen default mismatch are similarly absent.

---

### 9. The scope outline's v4 Goal 1 ("Retire `if_else2()`") has no counterpart in any requirements track

**Scope outline, goal 1:**
> "Retire `if_else2()` (264 calls, 15 files); every derived-variable function follows the canonical 3-step pattern."

**Scope outline, phased sequence:**
> "Parallel | `if_else2()` retirement, CEP by CEP, using repaired adl.R/alcohol.R as templates"

**Arch review:** Confirms 264 calls across 15 files; states "v4's convergence target: one idiom."

**Requirements:** `if_else2()` does not appear anywhere in any track or the staging summary.

The requirements acknowledge Track 1 absorbs "the derived-variable dispatch" problem, but the 264-call `if_else2()` retirement is a CEP-by-CEP content migration that is independent of the engine refactor. It was the first stated v4 goal in the scope outline. Its absence from the requirements is likely an oversight rather than a deliberate drop, but the requirements should state whether it lands in a track (Track 3 is the natural home), in v3.x CEP work, or v5.

---

### 10. The docs/vignettes dimension (5 high-severity, 13 total findings) is not assigned to any track

**Inventory JSON `inventory-docs-vignettes.json`:** 5 high-severity findings including a broken pkgdown index (90 of 125 current exports unindexed), vignettes that teach removed v2 functions, get_started.Rmd examples that fail against v3 worksheets, and missing VignetteBuilder in DESCRIPTION (vignettes have never shipped in the installed package).

**Requirements:** The only mention is a table cell noting "Divio docs" maps to "docs work" with a dash in the Lands column. No track, no staging tier, no requirements.

The five high-severity docs findings include things that prevent the package from being installed with working vignettes. These need a home in the staging plan.

---

### 11. Two scope outline open questions are not addressed in the requirements

**Q5 -- Parameter naming:** "Semantic (`height_m`) or CCHS-coded (`SMK_005`) for function signatures -- one convention, documented." The arch review repeats this. Requirements do not answer it or assign it to a track.

**Q6 -- Level-7 categorization:** "The recovered `convert_cont_to_cat()` design was never implemented; v3 uses per-domain `categorize_*()` functions. Generalize or keep per-domain?" Requirements contain no reference to level-7 categorization or `convert_cont_to_cat()`.

Q1 (schema design), Q3 (NA(b) factor levels), and Q4 (cache semantics) are answered by Tracks 2, 3.1, and 3.5 respectively. Q2 (priority rules) is answered but split across two release windows (see problem 4). Q5 and Q6 remain open with no stated disposition.

---

## Minor problems

### 12. The inventory's per-dimension issue counts do not match the JSON evidence files

The inventory body states counts based on the initial six-dimension sweep:

| Dimension | Text count | JSON count |
|---|---|---|
| API surface and UX | 15 | 12 |
| Data artifacts | 10 | 11 |

The API surface text says "15 issues: 7 high, 7 medium, 1 low"; the JSON has 12 issues (6 high, 5 medium, 1 low). The data artifacts text says "10 issues: 4 high, 4 medium, 2 low"; the JSON has 11 issues (3 high, 7 medium, 1 low). These discrepancies arose because the JSON files were updated after the text was written (or refactored). The body counts should be corrected to match the final evidence.

---

### 13. The CRAN policy violation (runtime RData write) is not addressed in any requirements track or v3.x staging

**Inventory headline finding 9:**
> "a runtime-written RData cache inside the installed package directory (a CRAN policy violation)"

The v3.x staging table includes the "data-raw pipeline" (the RData rebuild script), which addresses the no-pipeline aspect of finding 9. But the runtime-write violation itself -- the session cache that writes to the installed package's directory -- is not mentioned in the requirements. It is a CRAN submission blocker distinct from the rebuild pipeline.

---

### 14. The arch review's golden-output verification strategy assumes sample data that does not exist for v3's headline new content

**Arch review and scope outline:** Both describe "golden-output comparison on bundled sample data" as the verification strategy for engine refactor phases.

**Inventory (data artifacts):** "Newly harmonized cycles (2019-2023) have no sample data, so their worksheet rows are untestable in-package or CI."

The verification strategy is sound for the 2001-2018 cycles. It has no coverage for the 2019-2023 cycles that are v3's stated contribution. This is not a contradiction between the planning documents per se -- the arch review was written before the inventory confirmed the gap -- but the requirements should acknowledge that the verification strategy requires either new sample data or an alternative approach for the new cycles.

---

## Strengths

1. **The three new documents (inventory, reunification, requirements) are internally coherent with each other.** The inventory's findings map cleanly to requirement tracks; the reunification's option B is adopted consistently by Track 1; the staging summary faithfully reflects the track structure.

2. **The scope outline's open questions Q1, Q3, and Q4 are cleanly answered.** Schema design lands in Tracks 2 and 3.5; NA(b) factor levels get an explicit compatibility shim in Track 3.1; cache semantics (invalidation and parallel-worker behaviour) are addressed in Track 3.5.

3. **The recodeflow scoping disposition table is the best-structured section in the requirements.** Each scoping document gets an explicit verdict, a "Lands in" assignment, and a rationale.

4. **The inventory's design decisions section correctly flags the priority-order question as a methodological decision requiring the team, not a unilateral fix.** This saves the team from a silent inconsistency reaching production.

5. **The requirement tracks are well-balanced between deficit-fixing and staging**, and the Track 4 argument (labels adoption simultaneously fixes bugs and stages the dictionary/export future) is the clearest example of the two goals reinforcing each other.

---

## Summary table

| # | Problem | Severity | Documents in conflict |
|---|---|---|---|
| 1 | Package split non-goal (scope outline) vs option B adoption (requirements) | blocking | scope-outline vs requirements+reunification |
| 2 | Arch review 5-phase sequence targets cchsflow; Track 1 redirects same work to recodeflow, without superseding the phases | blocking | arch-review+scope-outline vs requirements |
| 3 | List-mode bug: S-effort fast fix (inventory) vs deferred to recodeflow (reunification); absent from v3.x staging | blocking | inventory vs reunification vs requirements |
| 4 | Priority-order decision placed in v3.x (staging summary) AND inside a v4 track (Track 3.4) | blocking | requirements internal |
| 5 | Inventory status header says 6/10 dims complete; body says 78 findings; coverage section says all 10 done with 131 findings | important | inventory internal |
| 6 | "option B" names two unrelated decisions across the document set | important | scope-outline vs reunification vs requirements |
| 7 | Scope outline preserves `output_format = "original"`; Track 3.2 replaces it with no shim statement | important | scope-outline vs requirements |
| 8 | 4 S-effort fast-fixes (including CRAN blocker and #1 data-corruption bug) absent from v3.x staging | important | inventory vs requirements |
| 9 | `if_else2()` retirement (v4 Goal 1 in scope outline) has no track in requirements | important | scope-outline vs requirements |
| 10 | Docs/vignettes dimension (5 high-severity findings) has no track or staging tier | important | inventory vs requirements |
| 11 | Q5 (parameter naming) and Q6 (level-7 categorization) unresolved in requirements | important | scope-outline vs requirements |
| 12 | Per-dimension issue counts in inventory body do not match JSON files | minor | inventory internal |
| 13 | Runtime RData cache (CRAN violation) not addressed in any track or v3.x staging | minor | inventory vs requirements |
| 14 | Golden-output verification strategy has no coverage for 2019-2023 cycles | minor | arch-review/scope-outline vs inventory |
