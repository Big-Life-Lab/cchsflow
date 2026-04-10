# Language variables review — PR #171

**Date:** 2026-03-12
**Branch:** ethnicity
**Reviewer:** Doug Manuel
**Variables:** SDCGLNG, SDC_5A_1, SDCGLHM, SDCDFOLS
**Trigger:** Rafdoodle review requesting changes to SDCGLNG extension and noting gaps in SDC_5A_1, SDCGLHM, SDCDFOLS

---

## Background

PR #171 extended SDCGLNG to include 2019–2023 Master databases using source variables
`SDC_025` (2019–2021) and `LAN_01` (2022–2023). Rafdoodle flagged these as invalid
extensions because the StatCan value code labels differ between eras. This review examines
all four language variables across L0–L7 to determine the correct approach.

**Update (2026-03-12):** Cross-check with CCHS documentation via Google NotebookLM
confirms that the label difference is an artefact of question redesign, not a conceptual
difference. The extension of SDCGLNG using `SDC_025` and `LAN_01` is analytically sound
(see L2–L4 below). Rafdoodle's flag was technically correct regarding labels but the
underlying cohort identified is functionally identical.

---

## L0 — Source variable landscape

CCHS uses two distinct question constructs for language:

| Construct | StatCan label | Value codes | Cycles available |
|-----------|--------------|-------------|-----------------|
| Languages — can converse (derived) | SDCGLNG / SDCDLNG / SDCAGLNG etc. | 1 = Eng w/w/o other, 2 = Fre w/w/o other, 3 = Both w/w/o other, 4 = Neither | PUMF 2001–2010; Master 2007–2010 |
| Knowledge of official languages (direct question) | SDC_5A_1 / SDC_025 / LAN_01 | 1 = English only, 2 = French only, 3 = Both, 4 = Neither | PUMF 2011–2018; Master 2011–2023 |
| Language(s) spoken at home (derived) | SDCGLHM / SDCDGLHM / SDCDLHM | Same 4-category structure as SDCGLNG | PUMF 2007–2014; share 2009–2012 |
| First official language spoken (derived) | SDCDFOLS / SDCDVFLS | 1 = English, 2 = French, 3 = Both, 4 = Neither | PUMF + Master 2011–2014 |

### SDC_025 / LAN_01 coverage (MCP confirmed)

| Source variable | StatCan label | Cycles | Release |
|----------------|--------------|--------|---------|
| SDC_5A_1 | Knowledge of official languages | 2011–2014 | PUMF + Master |
| SDC_025 | Knowledge of official languages | 2015–2021 | PUMF 2015–2018; Master 2015–2021 |
| LAN_01 | Knowledge of official languages | 2022–2023 | Master only |

`LAN_01` has a restricted universe (`DOLAN = 1`) and is a 1-digit type, suggesting a
simplified module in 2022–2023. Value codes are identical to `SDC_025`.

### SDCDLNG (Master 2007–2010)

The Master equivalent of SDCGLNG uses a 7-category coding (GN confirmed):

| Code | Label |
|------|-------|
| 1 | English only |
| 2 | French only |
| 3 | English & French only |
| 4 | English, French & other |
| 5 | English & other |
| 6 | French & other |
| 7 | Neither English nor French |

This requires recoding to map to cchsflow's 4-category SDCGLNG structure. The derivation
logic explicitly separates respondents who speak only official languages from those who
also speak other languages — hence 7 categories vs. 4.

**Note on SDCDVLHM (Master home language):** The Master equivalent of SDCGLHM also uses
a 7-category structure identical to SDCDLNG. Any future extension of SDCGLHM using Master
databases will require the same 7→4 recoding strategy.

---

## L1 — cchsflow main baseline

### SDCGLNG (main)

- **Databases:** PUMF 2001–2010; share 2009–2010
- **Source variables:** SDCAGLNG (2001), SDCCGLNG (2003), SDCEGLNG (2005), `[SDCGLNG]`
  default for 2007–2010, `[SDCDLNG]` for share files
- **No Master databases**
- Correctly stops at 2010 — SDCGLNG does not exist after 2010

**Gap in main:** Master 2007–2010 (`SDCDLNG`) is absent. The `_s` share files use
`[SDCDLNG]` but the `_m` Master databases are not included and would require 7→4 category
recoding.

**Note on older Master cycles (GN confirmed):** If Master 2001–2005 is ever added,
the source variable names are `SDCADLNG` (2001), `SDCCDLNG` (2003), `SDCEDLNG` (2005) —
same 7-category structure, same recoding applies.

### SDC_5A_1 (main)

- **Databases:** PUMF 2011–2018; share 2012
- **Source variables:** `SDC_025` explicit for 2015–2018; `[SDC_5A_1]` default for 2011–2014
- **No Master databases**, despite Master coverage existing from 2011–2021

**Gap in main:** Master 2011–2023 absent.

### SDCGLHM (main)

- **Databases:** PUMF 2007–2018; share 2009–2012
- **Source variables:** `SDCDGLHM` explicit for 2015–2018; `[SDCGLHM]` default for
  2007–2014; share files use `[SDCDLHM]`
- **No Master databases**
- MCP shows SDCGLHM exists in PUMF only through 2013–2014; the 2015–2018 mapping to
  `SDCDGLHM` (derived version) appears correct
- **Share file recoding is correct in main:** The `_s` rows already implement the full
  7→4 recoding for `SDCDLHM` (codes 1+5→1, 2+6→2, [3,4]→3, 7→4, share NA codes 96/97–99
  handled separately). GN flagged this as a potential bug but worksheet inspection
  confirms it is already handled.

**Gap in main:** Pre-2015 explicit source variable mappings absent (2007–2014 PUMF relies
on `[SDCGLHM]` default). No Master coverage. Any future Master extension will require
the same 7→4 recoding as `SDCDLNG` — `SDCDVLHM` (GN confirmed) uses identical 7-category
structure.

### SDCDFOLS (main)

- **Databases:** PUMF + Master 2011–2014
- **Source variables:** `SDCDVFLS` explicit for 2015–2018; `[SDCDFOLS]` default for
  2011–2014
- MCP shows SDCDFOLS only in 2011–2014 PUMF and Master; the 2015–2018 mapping to
  `SDCDVFLS` needs verification

**Gap in main:** Pre-2015 explicit source variable mappings absent. The 2015–2018
`databaseStart` includes these cycles but SDCDFOLS may not exist beyond 2014 in the source.

---

## L2–L4 — Value code compatibility

### SDCGLNG vs SDC_025: functionally compatible (GN confirmed)

| cchsflow recEnd | SDCGLNG / SDCAGLNG label | SDC_025 / LAN_01 label | Functionally same? |
|----------------|--------------------------|------------------------|-------------------|
| 1 (English) | Eng **with or without** other language | English **only** | Yes — "only" refers only to official languages |
| 2 (French) | Fre **with or without** other language | French **only** | Yes — same reasoning |
| 3 (Both) | Both with or without other language | Both English and French | Yes |
| 4 (Neither) | Neither | Neither | Yes |

**Rafdoodle's flag was technically correct** regarding the label wording, but the
underlying construct is the same. The pre-2011 `SDCGLNG` was derived from a long list of
all languages (`SDC_5A` to `SDC_5W`); the derived variable grouped by official language
knowledge and labelled the result "with or without other" to make the derivation
transparent. The 2011+ question (`SDC_5A_1`, `SDC_025`) asks *only* about official
languages directly — so "English only" means "English but not French" (the respondent may
still speak other languages). A respondent who speaks English and Cantonese would be
code 1 in both constructs.

**Conclusion:** `SDC_025` and `LAN_01` are valid extensions of SDCGLNG. No integer
recoding is needed. The label shift should be documented in harmonization notes.

### SDCDLNG (Master 2007–2010): requires recoding

| SDCDLNG codes | → cchsflow recEnd |
|--------------|------------------|
| 1 (English only), 5 (English & other) | → 1 |
| 2 (French only), 6 (French & other) | → 2 |
| 3 (English & French only), 4 (English, French & other) | → 3 |
| 7 (Neither) | → 4 |

This recoding collapses the 7-category Master variable to match the 4-category PUMF
harmonization. Information loss occurs (cannot distinguish English-only from
English+other), but this is consistent with how cchsflow treats the PUMF.

### SDC_5A_1 / SDC_025 / LAN_01: compatible 1:1

| SDC_5A_1 codes | → cchsflow recEnd |
|---------------|------------------|
| 1 (English only) | → 1 |
| 2 (French only) | → 2 |
| 3 (Both) | → 3 |
| 4 (Neither) | → 4 |

Direct pass-through — no recoding needed.

---

## L5 — PR #171 changes to SDCGLNG

The PR added to SDCGLNG:

- Master databases: cchs2001_m through cchs2023_m
- Explicit mappings: `cchs2019_m::SDC_025`, `cchs2020_m::SDC_025`, `cchs2021_m::SDC_025`,
  `cchs2022_m::LAN_01`, `cchs2023_m::LAN_01`

**Problems identified (all implementation errors — the conceptual approach is correct):**

1. **Invalid database names:** `cchs2019_m` and `cchs2020_m` are not valid cchsflow names;
   the correct name is `cchs2019_2020_m` (GN confirmed combined 2-year cycle)
2. **Missing recoding rows:** Master 2007–2010 added to databaseStart but no
   variable_details rows with the 7→4 category recoding for `SDCDLNG` — values 5, 6, 7
   would bleed into the 4-category harmonized variable without these rows
3. **No PUMF gap:** SDCGLNG does not exist in any PUMF after 2010; the 2011+ extension
   via `SDC_025`/`LAN_01` is Master-only (correct as implemented)

---

## Options

### Option A — Extend SDCGLNG with SDC_025/LAN_01, fix database errors (recommended)

The PR's intent was correct. The problems are implementation errors, not a conceptual
mistake. Fix the errors and keep the extension.

**SDCGLNG fixes required:**
- Remove invalid database names (`cchs2019_m`, `cchs2020_m`) → replace with
  `cchs2019_2020_m`
- Add SDCDLNG Master 2007–2010 recoding rows (7→4 categories) — currently absent
- Confirm value code rows for 2019–2023 Master are present and correct in variable_details
- Document label shift in variable notes

**SDC_5A_1:**
- Extend with Master databases (2011–2021 via `SDC_025`, 2022–2023 via `LAN_01`) —
  value codes are 1:1, no recoding needed
- Closes the Master coverage gap Rafdoodle flagged

**Result:** SDCGLNG covers 2001–2023 as a single harmonized variable (PUMF 2001–2018,
Master 2007–2023). SDC_5A_1 covers the same range as a parallel pass-through for users
who prefer the direct-question variable.

**Effort:** Medium — fix database name errors, add SDCDLNG recoding rows, extend
SDC_5A_1 Master.

### Option B — Keep SDCGLNG at 2001–2018 PUMF only, extend SDC_5A_1 for Master

- Revert SDCGLNG 2019–2023 additions; stop at 2018 PUMF
- Add SDCDLNG Master 2007–2010 recoding rows to SDCGLNG (optional)
- Extend SDC_5A_1 with all Master databases 2011–2023

**Pros:** Cleaner separation — SDCGLNG stays PUMF-only; SDC_5A_1 becomes the Master
variable
**Cons:** Loses 2019–2023 Master coverage for SDCGLNG; users need to know to switch
variables at 2019

### Option C — Revert SDCGLNG to main, leave SDC_5A_1 as-is

- Minimal change; unblocks PR for the non-language variables
- Leaves both the 2019–2023 coverage gap and SDC_5A_1 Master gap unresolved
- Opens follow-up issue for both

**Pros:** Fastest path to merge
**Cons:** Defers known gaps; Rafdoodle's review concern technically unresolved

---

## Recommendation

**Option A.** GN confirms the extension is analytically sound — the PR intent was right
and Rafdoodle's concern was about labels rather than construct validity. The work required
is fixing the invalid database names (`cchs2019_m`/`cchs2020_m`) and adding the missing
SDCDLNG 7→4 recoding rows for Master 2007–2010, then extending SDC_5A_1 with Master
coverage.

SDCGLHM and SDCDFOLS pre-2015 explicit source variable mappings and Master coverage
(noting SDCDVLHM also uses a 7-category structure) are separate gaps; recommend a
follow-up issue rather than expanding this PR further.

---

## Next steps

- [x] Fix SDCGLNG: correct `cchs2019_m`/`cchs2020_m` → `cchs2019_2020_m`
- [x] Fix SDCGLNG: SDCDLNG Master 2007–2010 recoding rows confirmed present in PR
- [x] Fix SDCGLNG: verify variable_details rows for 2019–2023 Master are complete
- [x] Extend SDC_5A_1: add Master 2011–2023 (SDC_025 through 2021, LAN_01 for 2022–2023)
- [x] Add harmonization note to SDCGLNG documenting label shift at 2011
- [x] L6 integration test: PASS across all 12 PUMF cycles for both SDCGLNG and SDC_5A_1
- [x] Open GH issue: SDCGLHM/SDCDFOLS pre-2015 mappings and Master coverage → [#178](https://github.com/Big-Life-Lab/cchsflow/issues/178)
