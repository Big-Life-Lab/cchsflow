# Issues to discuss with team

*Last updated: 2025-07-04*

## Critical issues requiring team discussion

### 1. Missing data strategy inconsistency

#### Issue: `tagged_na()` vs `"NA()"` inconsistency across functions
**Discussion points**:
- Some functions return `haven::tagged_na("a")`, others return `"NA(a)"` strings
- OK for a consistent `tagged_na()` approach?
- **NEW**: Review usage of `tagged_na("d")` - unable to calculate derived variable because missing start variables in data. The challenge of failing derived functions for this reason has been consistently flagged. Is this tagged_na approach appropriate? I'd prefer to have log warnings, which is on the recodeflow development plan.

### 2. Namespace consistency - incomplete implementation

#### Issue: Inconsistent package namespacing
**Discussion points**:
- Some functions use `tagged_na()` without `haven::`
- Inconsistent `dplyr::` usage across functions
- Should ALL functions use full namespacing for stability?

**Recommendation**: Implement full `haven::` and `dplyr::` namespacing in all functions

### 3. Validation boundaries - need clinical review

#### Alcohol functions: physiologically implausible thresholds
```r
# Current: up to 995 drinks per day
ALW_2A1 %in% (0:995) & ALW_2A2 %in% (0:995)
```
**Team review needed**: Are these CCHS-specified or should we implement realistic limits?

#### BMI functions: validation ranges need verification
**Team review needed**: Current height (0.914-2.134m) and weight (27-135kg) ranges

#### Smoking functions: age boundaries need team decisions
**Research-confirmed**: Age 8 minimum (Holford et al.)

**Age maximums (not specified in research)**:
```r
SMOKING_AGE_BOUNDS <- list(
  SMKG203_cont = list(min = 8, max = 84),    # ❓ Max age decision
  SMKG207_cont = list(min = 8, max = 84),    # ❓ Max age decision  
  SMKG040_cont = list(min = 8, max = 95),    # ❓ Max age decision
  current_age = list(min = 12, max = 102)    # ❓ CCHS-specific
)

TIME_QUIT_BOUNDS <- list(
  max = 82     # ❓ Maximum time since quitting - not research-specified
)
```

**Team decisions needed**: 
- Maximum age of smoking cessation (currently 84)
- Upper bound for time since quit smoking (currently 82 years)
- CCHS categorical-to-continuous age mappings

**Discussion points**:
1. **Max cessation age**: Research doesn't specify upper bounds - should we use 84, 95, or CCHS max age (102)?
2. **Time since quit maximum**: 82 years may exclude elderly former smokers - is this appropriate?
3. **Categorical mappings**: CCHS-specific conversions (e.g., category 3 = age 16 vs 17 across cycles)

**Documentation**: Comprehensive analysis available in `/cchsflow-temp/scope-docs/smoking-constants-comprehensive-review.md`

**Recommendation**: Clinical team review of all validation boundaries

### 4. Function naming conventions - standardization needed

#### Issue: Multiple verb patterns need consolidation
**Current verbs**: calculate_, assess_, categorize_, score_, classify_, adjust_, validate_

**Discussion points**:
- Can we consolidate `categorize_*()` and `classify_*()`?
- Should `adjust_*()` be more specific (correct_, calibrate_)?
- Remove deprecated `assess_*()` pattern?

**Recommendation**: Define clear usage guidelines for each verb pattern

---

## Recently resolved (v3.0.0)

- ✅ rec_with_table() compatibility confirmed
- ✅ Function organization strategy implemented (R/function.R + R/legacy/)
- ✅ BMI function modernization completed with vector processing

## Next steps

1. **Team decisions** on the 4 critical issues above
2. **Apply decisions** to smoking, alcohol, ADL function modernization
3. **Update development guide** with final standards

---

*This list focuses on team decisions needed - implementation details tracked in TODO.md*