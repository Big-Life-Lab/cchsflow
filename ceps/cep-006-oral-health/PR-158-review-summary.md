# PR #158 Review Summary: DEN_132 (Oral Health)

## Current status

**PR #158** adds `DEN_132` (Last time visited dental professional) for Master files.

**Current configuration in PR:**
```
databaseStart: cchs2001_m, cchs2003_m, cchs2005_m, cchs2009_2010_m, cchs2011_2012_m, cchs2013_2014_m
variableStart: cchs2001_m::DENA_132, cchs2003_m::DENC_132, cchs2005_m::DENE_132, [DEN_132]
```

## Verified from actual PUMF data (integration test)

We ran `rec_with_table()` integration tests on actual CCHS PUMF data to verify availability:

| Cycle | Variable | National N | National Valid | National % | Ontario N | Ontario Valid | Ontario % |
|-------|----------|------------|----------------|------------|-----------|---------------|-----------|
| 2001 | DENA_132 | 130,880 | 46,722 | 35.7% | 39,278 | 34,449 | **87.7%** |
| 2003 | DENC_132 | 134,072 | 78,613 | 58.6% | 42,777 | 28,658 | **67.0%** |
| 2005 | DENE_132 | 132,221 | 44,265 | 33.5% | 41,766 | 35,303 | **84.5%** |
| 2007-2008 | DEN_132 | 131,061 | 13,202 | 10.1% | 43,958 | 0 | **0.0%** |
| 2009-2010 | DEN_132 | 124,188 | 42,363 | 34.1% | 42,495 | 37,002 | **87.1%** |
| 2011-2012 | DEN_132 | 124,929 | 18,362 | 14.7% | 42,915 | 0 | **0.0%** |
| 2013-2014 | DEN_132 | 127,462 | 36,885 | 28.9% | 42,553 | 36,885 | **86.7%** |
| 2015-2016 | DEN_035 | 109,659 | 15,205 | 13.9% | 32,928 | 0 | **0.0%** |
| 2017-2018 | DEN_035 | 113,290 | 30,990 | 27.4% | 33,511 | 29,478 | **88.0%** |
| 2019-2020 | None | - | - | - | - | - | - |
| 2022 | None | - | - | - | - | - | - |

**Source:** `ceps/cep-006-oral-health/integration-test-den.R` using `rec_with_table()` with harmonized GEOGPRV province variable.

## Key findings

### 1. Variable naming evolution (confirmed from PUMF DDI)
- 2001: DENA_132
- 2003: DENC_132
- 2005: DENE_132
- 2007-2014: DEN_132
- **2015-2018: DEN_035** (renamed, same concept)

### 2. Ontario availability pattern
- **Available (67-88%)**: 2001, 2003, 2005, 2009-2010, 2013-2014, 2017-2018
- **NOT available (0%)**: 2007-2008, 2011-2012, 2015-2016

### 3. DEN_035 starts in 2015-2016, not 2017-2018
The PUMF DDI and actual data confirm DEN_035 exists in 2015-2016 (15,205 valid nationally), contradicting GoogleLM which claimed only CHP_080 was available.

## Documentation inconsistencies

| Source | Claim | Actual PUMF |
|--------|-------|-------------|
| GoogleLM | 2015-2016 has only CHP_080 | **DEN_035 exists** (14% national valid) |
| ICES Dictionary | DEN_035 not in Ontario Linked 2017-2018 | **Needs verification** - seems inconsistent |
| ICES Dictionary | DEN_132 in 2007-2008 Ontario Share (2008 only) | **0% Ontario valid in PUMF** - confirms exclusion |

**Recommendation:** Verify ICES Linked files directly to confirm DEN_035 availability, since documentation appears incomplete.

## Recommendations for PR #158

Based on verified PUMF data:

### Cycles to add
1. **Add 2007-2008**: `cchs2007_2008_m::DEN_132` - exists nationally (10%), Ontario excluded
2. **Add 2015-2016**: `cchs2015_2016_m::DEN_035` - exists nationally (14%), Ontario excluded
3. **Add 2017-2018**: `cchs2017_2018_m::DEN_035` - exists nationally (27%), Ontario available (88%)

### Notes to add
- Add description note: "Dental module was Optional Content. Ontario excluded in 2007-2008, 2011-2012, and 2015-2016."
- Variable renamed from DEN_132 to DEN_035 in 2015-2016

### Updated configuration (proposed)
```
databaseStart: cchs2001_m, cchs2003_m, cchs2005_m, cchs2007_2008_m, cchs2009_2010_m,
               cchs2011_2012_m, cchs2013_2014_m, cchs2015_2016_m, cchs2017_2018_m
variableStart: cchs2001_m::DENA_132, cchs2003_m::DENC_132, cchs2005_m::DENE_132,
               cchs2015_2016_m::DEN_035, cchs2017_2018_m::DEN_035, [DEN_132]
```

## Proposed PR comment

```markdown
## Review summary

Reviewed DEN_132 worksheet with PUMF integration testing. Found additional cycles available.

### Verified from actual PUMF data

Ran `rec_with_table()` integration tests across all CCHS PUMF cycles:

| Cycle | Variable | National % Valid | Ontario % Valid |
|-------|----------|------------------|-----------------|
| 2001 | DENA_132 | 35.7% | 87.7% ✓ |
| 2003 | DENC_132 | 58.6% | 67.0% ✓ |
| 2005 | DENE_132 | 33.5% | 84.5% ✓ |
| 2007-2008 | DEN_132 | 10.1% | 0% (excluded) |
| 2009-2010 | DEN_132 | 34.1% | 87.1% ✓ |
| 2011-2012 | DEN_132 | 14.7% | 0% (excluded) |
| 2013-2014 | DEN_132 | 28.9% | 86.7% ✓ |
| 2015-2016 | DEN_035 | 13.9% | 0% (excluded) |
| 2017-2018 | DEN_035 | 27.4% | 88.0% ✓ |

### Findings

1. **Variable renamed**: DEN_132 → DEN_035 starting in 2015-2016
2. **Additional cycles available**: 2007-2008, 2015-2016, 2017-2018 exist in PUMF
3. **Ontario gaps**: 0% valid in 2007-2008, 2011-2012, 2015-2016 (Optional Content not selected)

### Questions

1. Should we add the additional cycles (2007-2008, 2015-2016, 2017-2018) to this PR?
2. If yes, need to verify DEN_035 category values match DEN_132
3. Should we add a note about Ontario availability gaps?

### Next steps

If expanding scope:
- Add `cchs2007_2008_m::DEN_132`
- Add `cchs2015_2016_m::DEN_035`
- Add `cchs2017_2018_m::DEN_035`
- Add Optional Content note to variable description
```

## Files created for this review

- `ceps/cep-006-oral-health/integration-test-den.R` - PUMF integration test script
- `ceps/cep-006-oral-health/den-pumf-integration-test.csv` - Integration test results
- `ceps/cep-006-oral-health/google-lm-crosscheck-prompt.md` - Cross-check prompt
- `ceps/cep-006-oral-health/ontario-oral-health-availability.md` - ICES Dictionary analysis
- `ceps/cep-006-oral-health/PR-158-review-summary.md` - This summary

## Next steps

1. **Post comment to PR #158** with integration test findings
2. **Verify ICES Linked files** - extract actual DEN_035 from ICES data to resolve documentation inconsistency
3. **Wait for Rafidul's response** on expanding scope
4. **Then approve or request changes**
