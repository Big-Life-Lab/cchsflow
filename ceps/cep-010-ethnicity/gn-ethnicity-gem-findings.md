# Gem verification findings — ethnicity, language, and migration variables

**Date:** 2026-03-31
**Prompt:** `gn-ethnicity-variables-prompt.md` (20 tiered questions)
**Input data:** `gn-ethnicity-variables.csv` (13 rows), `gn-ethnicity-variable_details.csv` (186 rows)
**Verification tool:** Google NotebookLM Gem (cchsflow worksheet reviewer)
**Source documents:** ~250 StatCan PDFs (data dictionaries, questionnaires, DV specifications)

---

## Summary table

| # | Variable | Tier 1 (coverage) | Tier 2 (names) | Tier 3 (categories) | Tier 4 (consistency) | Notes |
|---|----------|--------------------|-----------------|----------------------|----------------------|-------|
| 1 | SDCGCGT | OK | OK | OK | — | PUMF ethnicity ends at 2018 |
| 2 | SDCDCGT_cat13 | OK | OK | OK | — | Master ethnicity ends at 2018 |
| 3 | SDCDCGT_cat7 | OK | OK | OK | — | Master ethnicity ends at 2018 |
| 4 | SDCDVABT | OK | OK | OK | — | Aboriginal/Indigenous |
| 5 | SDCGLNG | OK | OK | OK | See #18 | Language, 2001-2010 PUMF |
| 6 | SDC_5A_1 | OK | OK | OK | See #18 | Language, 2011+ |
| 7 | SDCGLHM | OK | OK | — | — | Language at home (PUMF) |
| 8 | SDCGLHM_A | OK | OK | — | — | Language at home (Master) |
| 9 | SDCDFOLS | Issue | OK | — | See #20 | Master coverage gap |
| 10 | SDCFIMM | OK | OK | OK | — | Immigrant status |
| 11 | SDCGCBG | OK | OK | OK | See #19 | Country of birth |
| 12 | SDCGCBG_A | OK | OK | — | See #19 | Country of birth (Master) |
| 13 | SDCGRES | OK | OK | OK | — | Time in Canada |

---

## Detailed findings

### Finding 1: SDCDCGT_cat13/cat7 ends at 2018m — no 2019+ Master ethnicity

**Status:** By design
**Tier:** 1 (coverage)
**Variables:** SDCDCGT_cat13, SDCDCGT_cat7

The Master ethnicity variable was restructured after 2018. Successor variables (SDCDVFLA, SDCDVVM) use different constructs (visible minority, etc.) that are not directly mappable to the pre-2019 cultural/racial origin groupings. The worksheet correctly stops at 2018m. Successor variables are tracked in issue #179.

### Finding 2: SDCGLNG / SDC_5A_1 overlap on 2019+ Master

**Status:** By design
**Tier:** 4 (consistency), question 18
**Variables:** SDCGLNG, SDC_5A_1

Both variables map SDC_025 (2015-2018) and LAN_01 (2022-2023) on Master files. This is intentional: SDCGLNG represents the historical lineage from 2001 (multi-language checklist era) while SDC_5A_1 represents the 2011+ direct-question lineage. Both produce the same 4-category output (English only / French only / both / neither). The overlap reflects different historical contexts mapping to the same construct.

### Finding 3: SDCGCGT is binary (White/non-White) on PUMF

**Status:** Correct
**Tier:** 3 (categories), question 13
**Variables:** SDCGCGT

The PUMF version is a 2-category grouping (1=White, 2=Non-White) rather than the 13-category Master version. This is by design — StatCan suppresses detailed ethnicity on PUMF for confidentiality. The 13-category and 7-category versions are available through the Master-only SDCDCGT_cat13 and SDCDCGT_cat7 variables.

### Finding 4: SDCDFOLS Master coverage limited to cchs2012_m

**Status:** Pre-existing gap
**Tier:** 1 (coverage) / 4 (consistency), question 20
**Variables:** SDCDFOLS

The First Official Language Spoken variable exists on PUMF for 2011-2018 but only covers cchs2012_m on Master. Gem could not confirm whether SDCDFOLS exists on 2011-2012 combined Master or 2013-2014 Master. This is a pre-existing coverage gap — extension to additional Master databases is a follow-up opportunity, not a PR #171 issue.

### Finding 5: cchs2023_p not in SDCDVABT or SDCFIMM

**Status:** Not actionable
**Tier:** 1 (coverage)
**Variables:** SDCDVABT, SDCFIMM

The 2023 PUMF likely contains both Aboriginal identity and immigrant status variables, but `cchs2023_p` is not yet configured as a database in cchsflow. Adding it is a separate infrastructure task, not specific to these variables.

---

## Classification summary

| Classification | Count | Description |
|---------------|-------|-------------|
| By design | 2 | Coverage boundaries are intentional (ethnicity 2018 cutoff, SDCGLNG/SDC_5A_1 overlap) |
| Correct | 1 | PUMF ethnicity binary confirmed accurate |
| Pre-existing gap | 1 | SDCDFOLS Master coverage incomplete |
| Not actionable | 1 | 2023 PUMF not yet in cchsflow |
| **Blocking issues** | **0** | — |

## Action taken

- All findings reconciled — no blocking issues for PR #171 merge
- Successor variables tracked in issue #179
- PR comment posted with findings summary
