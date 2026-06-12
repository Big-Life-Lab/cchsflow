# Engine Comparison and Reunification Recommendation

**Date:** June 11, 2026
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
function is byte-identical in both repos), per-row positional dispatch of
derived-variable functions (recodeflow swapped `do()` for a `for` loop but
it is still one scalar call per row), and no formal recStart/recEnd
grammar.

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
   Doug's versioning comment on PR #43). The three shared-defect fixes land
   there once: a single NA representation at the engine boundary
   (haven_labelled + tagged_na, factors as explicit opt-in), vectorized
   named-argument dispatch for derived variables, and a validated
   recStart/recEnd grammar.
3. **cchsflow's levels 3-6 migrate down in generic form** -- the pattern
   cache, cleaning mechanics, and range parser take a configuration object
   instead of CCHS defaults; cchsflow supplies the CCHS configuration and
   keeps `check_worksheet()`'s CCHS conventions, the DV library, content,
   and CEPs.
4. **cchsflow v4 depends on recodeflow (>= 1.0.0)** via Imports. The
   125-export surface shrinks to the deliberate tier (engine re-exports,
   DV functions, discovery tools).

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

## Division of the confirmed design-issue inventory

Of the inventory's high-severity findings, under option B: the engine
defects (NA representations, dispatch, grammar, list-mode class of bugs)
are fixed in recodeflow; the packaging, labels-layer, data-artifact, and
worksheet-content issues are fixed in cchsflow; the missing-data
architecture issues (priority rules, cache invalidation, dead config
loader) are fixed during the generic-form migration -- which is the
natural moment to resolve them rather than patching twice.
