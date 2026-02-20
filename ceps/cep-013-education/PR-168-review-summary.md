# CEP-013: Education variables review (PR #168)

**PR**: #168 (Education)
**Author**: caitlink12
**Target**: feature/v3.0.0-validation-infrastructure
**Review date**: 2026-02-20

## Scope

Two education derived variables (EDUDR03, EDUDR04) with ICES survey cycle additions.

| Variable | Description | Categories | Cycles |
|---|---|---|---|
| EDUDR03 | Highest education (3-level) | Less than HS, HS grad, Post-secondary grad | 2001-2018 (all PUMF + master) |
| EDUDR04 | Highest education (4-level) | Less than HS, HS grad, Some post-sec, Post-sec grad | 2001-2014 only |

## Changes introduced by PR

1. **EDUDR03**: Added master databases (`cchs2001_m` through `cchs2017_2018_m`) and cycle-specific variableStart for 2001-2005 master (`cchs2001_m::EDUADR04`, `cchs2003_m::EDUCDR04`, `cchs2005_m::EDUEDR04`). 2015-2018 master databases added to EHG2DVR3 row.
2. **EDUDR04**: Added master databases for 2001-2014. Removed `cchs2015_2016_m` and `cchs2017_2018_m` (correct — EHG2DVR3 is 3-level, doesn't map to 4-level EDUDR04).
3. **EDUDR03 for 2015-2016**: Separate row block using `[EHG2DVR3]` with 3 categories (direct 1:1 mapping rather than collapsing 4→3).

## MCP source variable verification

| variableStart | Expected cycle | MCP confirmed | Notes |
|---|---|---|---|
| EDUADR04 | cchs2001 | Yes | 4-level education derived |
| EDUCDR04 | cchs2003 | Yes | 4-level education derived |
| EDUEDR04 | cchs2005 | Yes | 4-level education derived |
| EDUDR04 | 2007-2014 | Yes | Default name from 2007+ |
| EHG2DVR3 | 2015-2018 | Yes | 3-level education derived (new naming) |

## L6 integration results

### EDUDR03 (all 9 PUMF cycles pass)

| Cycle | Status | Distribution |
|---|---|---|
| cchs2001_p | OK | 1:42 2:22 3:136 |
| cchs2003_p | OK | 1:56 2:41 3:102 NA(b):1 |
| cchs2005_p | OK | 1:47 2:41 3:106 NA(b):6 |
| cchs2007_2008_p | OK | 1:48 2:36 3:114 NA(b):2 |
| cchs2009_2010_p | OK | 1:45 2:25 3:126 NA(b):4 |
| cchs2011_2012_p | OK | 1:48 2:25 3:115 NA(b):12 |
| cchs2013_2014_p | OK | 1:46 2:37 3:114 NA(b):3 |
| cchs2015_2016_p | OK | 1:37 2:46 3:114 NA(b):3 |
| cchs2017_2018_p | OK | 1:40 2:40 3:118 NA(b):2 |

### EDUDR04 (7 cycles pass, 2 correctly MISS)

| Cycle | Status | Distribution |
|---|---|---|
| cchs2001_p | OK | 1:42 2:22 3:31 4:105 |
| cchs2003_p | OK | 1:56 2:41 3:10 4:92 NA(b):1 |
| cchs2005_p | OK | 1:47 2:41 3:15 4:91 NA(b):6 |
| cchs2007_2008_p | OK | 1:48 2:36 3:16 4:98 NA(b):2 |
| cchs2009_2010_p | OK | 1:45 2:25 3:19 4:107 NA(b):4 |
| cchs2011_2012_p | OK | 1:48 2:25 3:11 4:104 NA(b):12 |
| cchs2013_2014_p | OK | 1:46 2:37 3:4 4:110 NA(b):3 |
| cchs2015_2016_p | MISS | Correct — EHG2DVR3 is 3-level |
| cchs2017_2018_p | MISS | Correct — EHG2DVR3 is 3-level |

## Design note: EDUDR03 recoding

For 2001-2014, EDUDR03 collapses the 4-level source (EDUDR04/variants) into 3 categories: categories 3 ("some post-secondary") and 4 ("post-secondary graduate") are combined into category 3 ("post-secondary graduate"). `recStart: [3,4]` handles this.

For 2015-2018, EHG2DVR3 is already 3-level, so a direct 1:1 mapping is used. The MCP database maps `EHG2DVR3.cchsflow_name` = `EDUDR04`, but this appears incorrect — the variable is structurally 3-level and correctly mapped to EDUDR03 in the worksheets.

## Issues found

### P1: Trailing empty columns in variable_details.csv (19 extra columns)

The CSV header expanded from 22 to 41 columns with empty column names. This adds trailing commas to nearly all rows. Likely introduced by Excel editing. **Fixed in review commit.**

### P2: dummyVariable `::` identifiers (9 rows)

`EDUDR03_cat3_NA::a` and `EDUDR03_cat3_NA::b` (6 rows), plus `EDUDR04_cat4_NA::a` and `EDUDR04_cat4_NA::b` (3 rows). Colons are invalid in R identifiers. **Fixed: `_NAa`/`_NAb`.**

### Pre-existing (not introduced by this PR)

- `cchs2014_m` not used anywhere in the project (0 variables reference it). This is a project-wide pattern, not specific to this PR.
- Base file already had 3296 rows with trailing commas (pre-existing formatting debt).
