# NotebookLM prompt — 2022–2023 Master continuous quit-time variables

**Date:** 2026-03-17
**Context:** v3-smoking branch; investigating whether continuous "years since stopped smoking
daily" is available on the 2022 and 2023 Master files, and what variables are needed to
construct `time_quit_smoking_daily` for those cycles.

---

In earlier CCHS cycles (2003–2021), former daily smokers who answered "3 or more years ago"
to the categorical cessation question (`SMK_09A`, `SMK_080`) were routed to a continuous
follow-up: "How many years ago was it?" — captured as `SMK_09C` (2003–2014) or `SMK_090`
(2015–2021). These variables provide the actual number of years since the respondent stopped
smoking daily.

In 2022–2023, the questionnaire restructured into CSS (Current Smoker Supplement) and SPU
(Smoking Past Use) modules. The categorical cessation question is now:

- **2022 Master:** `SPU_25` does NOT appear in the MCP metadata for 2022; instead, `SPU_25A`
  (month stopped) and `SPU_25B` (year stopped, numeric 1940–2022) are present.
- **2023 Master:** `SPU_25` is confirmed as a 4-category variable (same 1–4 scale as 2003–2021).

Please investigate and answer the following:

1. **2022 Master — is there a categorical "when stopped daily" variable?** The MCP shows
   `SPU_25A` (month) and `SPU_25B` (year) but not a categorical `SPU_25` with codes 1–4 for
   2022. Did the 2022 cycle skip the categorical question and go directly to month/year? Or
   does `SPU_25` (categorical) also exist on the 2022 Master?

2. **2023 Master — is there a continuous "years since stopped daily" follow-up?** For
   respondents who answered code 4 ("3 or more years ago") to `SPU_25` in 2023, is there a
   continuous follow-up variable giving the actual number of years? Or does the 2023 design
   use only the categorical `SPU_25` with no continuous companion?

3. **`SMKDVSTP` coverage:** This StatCan derived variable (years since stopped smoking
   completely) is confirmed on the 2022 Master but the MCP does not surface it for 2023. Does
   `SMKDVSTP` exist on the 2023 Master, or was it discontinued after 2022?

4. **Constructing continuous years from `SPU_25B`:** If 2022 provides `SPU_25B` (year stopped
   as a 4-digit year), can continuous "years since stopped daily" be derived by subtracting
   `SPU_25B` from the interview year? Is this the intended approach for 2022 Master, replacing
   the older "how many years ago" follow-up?

5. **`SMKG09C` in 2022:** Our cchsflow worksheet lists `SMKG09C` (a categorical "years since
   quit daily" grouping variable) as covering `cchs2022_m`. Does `SMKG09C` appear on the 2022
   Master, and if so, what is its source — is it derived from `SPU_25B`, or is it an
   independent question?

Please summarize the continuous variable options available for 2022 and 2023 Master for the
"years since stopped smoking daily" concept, and flag any gaps.

---

## Findings (NotebookLM response, 2026-03-17)

**Q1 — 2022 Master categorical variable:**
No categorical `SPU_25` in 2022 Master. The 2022 EQ format skipped the 4-category question
and went directly to `SPU_25A` (month) + `SPU_25B` (4-digit year stopped). Categorical
equivalent must be derived manually if needed.

**Q2 — 2023 Master continuous follow-up:**
No continuous follow-up exists. After `SPU_25` (categorical), respondents route directly to
`SPU_C30` — no year or "how many years ago" question. **It is impossible to construct
continuous "years since stopped daily" for 2023 Master.**

**Q3 — `SMKDVSTP` in 2023:**
`SMKDVSTP` discontinued after 2022. In 2022 it was calculable from `SPU_25B`/`SPU_35B`
exact-year variables. In 2023 those variables don't exist, so the DV cannot be computed.
`SMKDVSTP` does not appear in 2023 Master documentation.

**Q4 — Deriving continuous years from `SPU_25B` in 2022:**
Confirmed: `ADM_YOI - SPU_25B` is the intended approach. StatCan's 2022 DV specifications
use this formula. `ADM_MOI` is used to resolve month-level precision.

**Q5 — `SMKG09C` in 2022:**
`SMKG09C` does NOT exist on 2022 Master. It is a legacy PUMF variable name. Its presence
in our worksheet mapped to `cchs2022_m` is an error — needs to be corrected.

### Coverage implications for cchsflow

All four variables cover PUMF+Master 2001–2023 (with `time_quit_smoking_complete` excluding
2001 Master only). The 2023 Master does not create a coverage gap — it creates a **precision
reduction**: midpoint imputation from `SMK_09A_2003plus` is used instead of exact years,
consistent with PUMF methodology across all years.

| Variable | Coverage | 2023 Master approach | Precision note |
|---|---|---|---|
| `time_quit_smoking_daily` | PUMF+Master 2001–2023 | Midpoints from `SMK_09A_2003plus` | Reduced vs 2001–2022 Master (exact years) |
| `time_quit_smoking_complete` | PUMF+Master 2003–2023; PUMF 2001 | Midpoints from `SMK_09A_2003plus` + `SMK_06A_2003plus` | Reduced vs 2003–2022 Master (`SMKDVSTP`) |
| `SMK_09A_2003plus` | PUMF+Master 2003–2023 | Categorical pass-through (1–4) | Full coverage |
| `SMK_09A_cont` | PUMF 2001–2023 | DerivedVar via `calculate_SMK_09A_cont()` | Midpoint throughout |

For **2022 Master**, `time_quit_smoking_daily` uses `ADM_YOI - SPU_25B` — a new DerivedVar
block (not a pass-through). Highest precision of any era.

For **2023 Master**, midpoint imputation from `SMK_09A_2003plus` is the only option.
Flag reduced precision in `reviewNotes` for both `time_quit_smoking_daily` and
`time_quit_smoking_complete`.
