# CEP-017: v4 Engine Modernization

Background material and scope for the cchsflow v4 refactor (engine
modularization and missing-data metadata).

| Document | Purpose |
|---|---|
| [2026-06-10_three-step-architecture-review.md](2026-06-10_three-step-architecture-review.md) | Deep review of the v3 3-step architecture, the `rec_with_table()` integration gaps, the duplication map, and the ADL/alcohol repair case study. Background for v4 scope, specifications, and requirements. |
| [2026-06-10_v4-scope-outline.md](2026-06-10_v4-scope-outline.md) | Draft v4 goals, non-goals, phased sequence, engine API shortlist, open questions, and decision log. |
| [2026-06-11_design-issues-inventory.md](2026-06-11_design-issues-inventory.md) | The "design issues that never made sense" inventory: 78 evidence-cited findings across six sweep dimensions, headline top 10, fast-fix list for the v3 window, design decisions raised, and partial open-issue triage. Machine-readable findings in [evidence/](evidence/). || [2026-06-11_engine-comparison-and-reunification.md](2026-06-11_engine-comparison-and-reunification.md) | Function-by-function recodeflow-vs-cchsflow comparison and the costed reunification recommendation (option B: engine consolidates in recodeflow v1.0.0; cchsflow v4 consumes it). Answers issue #135 and sets the disposition for recodeflow PR #43. |
| [2026-06-12_v4-consolidated-requirements.md](2026-06-12_v4-consolidated-requirements.md) | The balanced v4 programme: seven tracks (engine, worksheet schema incl. databaseStart remediation, missing-data semantics, labels/attributes, versioning, logging, catalog/dictionaries) with v3.x / v4 / v5 staging and the disposition verdict on each recodeflow scoping document. |

The evidence program is complete (all ten inventory dimensions,
41-verdict adversarial verification, engine comparison, eight-family
ecosystem benchmark with skeptic passes -- raw material in evidence/).
The consolidated requirements draft is written; remaining work is team
review and the decisions it calls out (priority order, NA representation,
the recodeflow consolidation).

Provenance: the review synthesizes a June 2026 code review of the v3 branch,
the test suite, worksheet metadata, and design documents recovered from the
abandoned v3.0.0 lineage (PR #137/#143, branch `3-step-tidyverse`; deletion
commits `38202456`, `91cd84d0`, `0bf0337e`, `52e0a133`). Recovered originals
remain available from git history on those commits' parents.
