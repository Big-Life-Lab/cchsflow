# Engine Comparison and Reunification Recommendation

**Date:** June 11, 2026 (revised June 12 after the adversarial review
panel; see `evidence/review-yulric-perspective.md` and
`evidence/review-feasibility-effort.md`)
**Status:** Draft recommendation for team decision (issue #135, recodeflow PR #43)
**Evidence:** `evidence/engines-core-diff.md`, `evidence/engines-features.md`,
`evidence/engines-schema-diff.md` (function-by-function comparison of
recodeflow dev `b87e6bd` vs cchsflow `fix/v3-smoking-worksheet-sync`)

## The finding that reframes the question

The two repos have built complementary halves of one system, with little
overlap in their progress since the engines forked:

**recodeflow dev is ahead on the engine.** Since 2025 it has: extracted
and refactored the recode loops (`recode_non_derived_vars()`, explicit row
loop replacing the deprecated `rowwise()+do()` that cchsflow still uses);
overlap detection on from-ranges; database-specific `DerivedVar::` feeder
overrides (`get_feeder_vars()` -- cchsflow cannot do this); template
variables (`templateVariable` column -- reusable recode rule sets); scalar
and string constants as worksheet feeders; reference-table feeders
(`tables=`); semantic worksheet validation returning structured errors
(`parse_variables_sheet()`, PR #85); a typed start-variable dependency
graph (`get_start_variables()`); an end-to-end integration test with
snapshot diffing; a Dublin Core dataset-metadata prototype; and the formal
engine scope documents (PR #43).

**cchsflow v3 is ahead on everything around the engine.** The 3-step
missing-data architecture (levels 3-6: pattern cache, `clean_variables()`,
`any_missing()`/`get_priority_missing()`, ~2,400 lines recodeflow has no
equivalent of); on-disk worksheet QA (`check_worksheet()`/
`fix_worksheet()`); the 419-variable CCHS content and 30+ derived-variable
modules; the CEP process; discovery and getter APIs; the deprecation
layer.

**Both share the same core defects**, inherited from the common ancestor:
the literal-`"NA(b)"`-string vs `tagged_na` split (the NA-formatting
function is functionally identical in both repos), per-row positional
dispatch of derived-variable functions (recodeflow replaced `do()` with a
`for` loop, but vectorized named-argument dispatch is net-new work for
both repos), and no formal recStart/recEnd grammar. The list-mode
database bug is already fixed on the cchsflow side (63450ba3) and ports
to recodeflow with the consolidation.

## Options

**A. Engine modernized in cchsflow; recodeflow follows or is deprecated.**
Duplicates work Yulric has already completed in recodeflow (template
variables, refactored loops, semantic validation), deepens the fork, and
strands the recodeflow scoping investment. Effort concentrates in
re-implementing; risk is social as much as technical. Not recommended.

**B. Engine consolidates in recodeflow v1.0.0; cchsflow v4 consumes it.**
The shared defects get fixed once, in the repo whose engine is already
cleaner. cchsflow's generic infrastructure migrates down where it
generalizes (pattern cache, `clean_variables()` mechanics, range parser,
worksheet QA primitives), parameterized so CCHS-specific configuration
(missing-code families, database heuristics, priority rules) stays in
cchsflow. cchsflow v4 = content + DV library + CCHS extensions, with
recodeflow as an Imports dependency. Directly answers issue #135 and
gives recodeflow PR #43's scoping a concrete release target.

**C. Staged hybrid: modernize inside cchsflow v4 behind a clean internal
API, extract to recodeflow in v5.** Defers the coordination cost but
guarantees double-handling: cchsflow would first adopt recodeflow's
improvements locally (a port), then later donate everything back (a second
port). Only attractive if recodeflow's release cadence cannot be relied
on.

## Recommendation: B, with a bridge

1. **v3.0.0 ships as-is** on cchsflow's local engine -- no dependency
   change in the release that is already staged.
2. **v4 engine work happens in recodeflow**, which becomes v1.0.0 (per
   Doug's versioning comment on PR #43). Panel correction: **v1.0.0 does
   not exist yet** -- recodeflow is at v0.1.2 with no milestone -- so the
   recommendation defines it minimally: the three shared-defect fixes
   (single NA representation at the boundary: haven_labelled +
   tagged_na, factors as explicit opt-in; vectorized named-argument
   dispatch; validated recStart/recEnd grammar) plus what is already
   merged on dev. Logging, versioning, catalog, and the generic
   missing-data layer are v1.1+ and do not gate cchsflow v4. A CRAN
   pre-flight is part of the milestone: recodeflow currently has
   undeclared Imports (checkmate, purrr, glue) and sjlabelled in Depends;
   these must clear R CMD check before any cchsflow dependency lands.
3. **cchsflow v4 keeps its levels 3-6 missing-data layer**, refactored
   behind a clean internal API boundary; the migration "down in generic
   form" (configuration object instead of CCHS defaults) is **deferred to
   recodeflow v1.1** as an extraction rather than a rewrite. (Panel
   verdict: the generic form is L-effort with an unsolved
   configuration-interface design; it does not belong on the v4 critical
   path. Authorship when it happens: cchsflow team authors, Yulric
   reviews -- team to confirm.) cchsflow keeps `check_worksheet()`'s CCHS
   conventions, the DV library, content, and CEPs throughout.
4. **cchsflow v4 depends on recodeflow (>= 1.0.0)** via Imports. The
   125-export surface shrinks to a deliberate tier, enumerated as a
   Track-1 deliverable (retained exports, shims with lifecycle stages,
   removals) before any code moves.

**Risks to manage:** recodeflow's release cadence becomes cchsflow's
critical path (mitigation: Yulric owns the engine lane and the scoping is
his); CRAN sequencing (recodeflow must publish before cchsflow v4);
CCHS-specific assumptions hiding in "generic" layers (mitigation: the
DHHGAGE_cont coordinate-system lesson from the repair is the test case --
configuration must declare source vs target domains explicitly);
coordination overhead across two repos (mitigation: the CEP-017 issue
inventory becomes a shared backlog with explicit repo assignment).

**What this means for PR #43:** harvest the scoping (both layers,
including the preserved catalog.qmd) into recodeflow's v1.0.0
requirements, then close it -- it has done its job.

**Supersession:** this recommendation replaces the five-phase
in-cchsflow engine sequence in
[2026-06-10_three-step-architecture-review.md](2026-06-10_three-step-architecture-review.md);
those transformations now happen inside the recodeflow v1.0.0 milestone.
The worksheet column-convention divergence between the repos (the
pkg.env/pkg.globals label-mapping split documented in
`evidence/engines-core-diff.md` section 3) is an explicit migration item
of the consolidation, with one convention chosen and the worksheet
migration path documented.

## Division of the confirmed design-issue inventory

Of the inventory's high-severity findings, under option B: the engine
defects (NA representations, dispatch, grammar, list-mode class of bugs)
are fixed in recodeflow; the packaging, labels-layer, data-artifact, and
worksheet-content issues are fixed in cchsflow; the missing-data
architecture issues (priority rules, cache invalidation, dead config
loader) are fixed during the generic-form migration -- which is the
natural moment to resolve them rather than patching twice.
