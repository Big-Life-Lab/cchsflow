# CEP-017: v4 Engine Modernization

Background material and scope for the cchsflow v4 refactor (engine
modularization and missing-data metadata).

| Document | Purpose |
|---|---|
| [2026-06-10_three-step-architecture-review.md](2026-06-10_three-step-architecture-review.md) | Deep review of the v3 3-step architecture, the `rec_with_table()` integration gaps, the duplication map, and the ADL/alcohol repair case study. Background for v4 scope, specifications, and requirements. |
| [2026-06-10_v4-scope-outline.md](2026-06-10_v4-scope-outline.md) | Draft v4 goals, non-goals, phased sequence, engine API shortlist, open questions, and decision log. |
| [2026-06-11_design-issues-inventory.md](2026-06-11_design-issues-inventory.md) | The "design issues that never made sense" inventory: 78 evidence-cited findings across six sweep dimensions, headline top 10, fast-fix list for the v3 window, design decisions raised, and partial open-issue triage. Machine-readable findings in [evidence/](evidence/). |

Pending (interrupted by org spend limit; resumable from the cached
workflow run `wf_5d1616cd-804`): the remaining four inventory dimensions,
the adversarial verification pass, the recodeflow-vs-cchsflow engine
comparison with reunification options, and the eight-family ecosystem
benchmark (retroharmonize, ipumsr, declared, codebook/pointblank, cli,
DDI/DCAT/LinkML, packaging norms, Maelstrom).

Provenance: the review synthesizes a June 2026 code review of the v3 branch,
the test suite, worksheet metadata, and design documents recovered from the
abandoned v3.0.0 lineage (PR #137/#143, branch `3-step-tidyverse`; deletion
commits `38202456`, `91cd84d0`, `0bf0337e`, `52e0a133`). Recovered originals
remain available from git history on those commits' parents.
