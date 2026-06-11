# v4 Scope Outline

**Date:** June 10, 2026
**Status:** Draft for team discussion
**Companion:** [2026-06-10_three-step-architecture-review.md](2026-06-10_three-step-architecture-review.md)

## Goals

1. **One missing-data idiom.** Retire `if_else2()` (264 calls, 15 files);
   every derived-variable function follows the canonical 3-step pattern.
2. **A modular harmonization engine.** `rec_with_table()` becomes a thin
   orchestrator over the level 3--6 primitives the 3-step stack already
   provides. Vectorized dispatch, named arguments, `output_format` passed
   through.
3. **Consistent missing-data semantics end to end.** One representation per
   output dataframe; tagged NAs survive categorical handling; users can
   request original codes through the standard workflow.
4. **NA patterns and validation rules as declared metadata.** Move from
   patterns inferred from recode rows to patterns declared in schema, with
   explicit source-domain vs target-domain semantics (see the
   `DHHGAGE_cont` lesson in the companion review). Extend beyond
   `NA::a`/`NA::b` as needed.
5. **Output validation everywhere.** Step-3 validation for every derived
   variable, including worksheet rows that give derived variables real
   missing-code and range declarations.

## Non-goals for v4

- LinkML schemas and the formal ontology (tier 2/3 of the documentation
  roadmap) -- v5 material, informed by the v4 schema work.
- A cchsflow/recodeflow package split -- revisit after the engine is
  modular.
- Standalone copy-paste purity of derived-variable functions -- explicitly
  deprioritized; clear and robust domain logic remains required.

## Phased sequence

| Phase | Work | Verification |
|---|---|---|
| 1 | Vectorize `DerivedVar::` dispatch; named arguments; pass `output_format` | Golden-output comparison on bundled sample data |
| 2 | Single range parser (`parse_range_notation()`) for engine and stack | Existing recode tests |
| 3 | Legacy NA formatting routed through the pattern cache; decide `"NA(b)"` factor-level compatibility | Golden-output + explicit migration note |
| 4 | `recode_columns()` adopts `apply_else_logic()` and cached worksheet access | Golden-output |
| 5 | Decompose `rec_with_table()` into detect / recode / derive / label stages on public primitives | Full suite + vignette rebuild |
| Parallel | `if_else2()` retirement, CEP by CEP, using repaired adl.R/alcohol.R as templates | Per-domain test files |

## Engine API shortlist

Promote: `get_variables()`, `get_variable_details()`,
`get_missing_pattern()`, `get_complete_pattern()`, `any_missing()`,
`get_priority_missing()`, `assign_missing()`, `clean_variables()`,
`parse_range_notation()`, `normalize_input_lengths()`, label utilities.

Keep internal: recode pipeline internals until rebuilt. Dev-only:
`check_worksheet()`, `fix_worksheet()`, `load_schema()`,
variable-discovery.

## Open questions for the team

1. **Schema design.** Where do declared NA patterns live -- worksheet
   columns, YAML in `inst/metadata/schemas/`, or the cchs-metadata
   database? How do declarations state source vs target domain? (Deferred
   from v3; the schemas currently shipped are consulted only by worksheet
   QA.)
2. **Priority rules.** Ship the missing `missing_priority_rules.yaml` or
   fold priorities into the schema above?
3. **`"NA(b)"` factor levels.** Compatibility break or transition shim when
   the engine moves to tagged NAs for categoricals?
4. **Cache semantics.** Invalidation on worksheet change; behaviour under
   parallel workers (`mclapply`, future, targets).
5. **Parameter naming.** Semantic (`height_m`) or CCHS-coded (`SMK_005`)
   for function signatures -- one convention, documented.
6. **Level-7 categorization.** The recovered `convert_cont_to_cat()` design
   was never implemented; v3 uses per-domain `categorize_*()` functions.
   Generalize or keep per-domain?

## Decision log

- **June 2026:** v3.0.0 release takes option B -- deprecation shims
  (`R/deprecated-aliases.R`) for renamed v2 functions, removed in v4.
- **June 2026:** Claude Code skills stay on the skills branch
  (`skills/review-validation`, PR #183); the v3-to-main merge carries no
  skill content.
- **June 2026:** The abandoned splice idiom
  (`generate_tagged_na_conditions()`) does not reach main; adl.R and
  alcohol.R rewritten onto the canonical 3-step before release.
- **March 2026 (memory):** smoking + BMI received full v3 treatment;
  other domains deliberately parsimonious until v4.
