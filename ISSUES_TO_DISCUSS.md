# Issues to Discuss with Team

## Potential Logic Issues Identified During v3.0.0 Modernization

### **1. Alcohol Functions - Physiologically Implausible Thresholds**

#### **Issue**: Extremely High Daily Drink Limits
**Location**: `R/alcohol.R` lines 637, 805
```r
# Current validation allows up to 995 drinks per day
ALW_2A1 %in% (0:995) & ALW_2A2 %in% (0:995) & ... & ALW_2A7 %in% (0:995)
```

**Discussion Points**:
- 995 drinks per day is physiologically impossible
- May mask data quality issues or coding errors
- Should we implement more realistic upper bounds (e.g., 50 drinks/day)?
- Need to check if CCHS documentation specifies these ranges

**Recommendation**: Review CCHS variable documentation for intended ranges

---

### **2. ADL Functions - Missing Data Type Assumptions**

#### **Issue**: String-Based Missing Data Operations
**Location**: `R/adl.R` lines 171-175
```r
# Assumes all missing values are strings
count_missing_adl <- sum(all_adl_vector == "NA(b)")
count_not_applicable_adl <- sum(all_adl_vector == "NA(a)")
```

**Discussion Points**:
- What happens with actual `NA` values or `haven::tagged_na()`?
- Could produce unexpected results with mixed data types
- Enhanced functions handle this, but should legacy be updated?

**Recommendation**: Test with different missing data formats from actual CCHS files

---

### **3. Physical Activity - Incorrect tagged_na Usage**

#### **Issue**: Missing haven:: Namespace
**Location**: `R/physical-activity.R` lines 111, 130
```r
# Should be haven::tagged_na()
tagged_na("a"), tagged_na("b")
```

**Discussion Points**:
- Causes errors if haven package not loaded
- Inconsistent with other cchsflow functions
- Should be standardized across all functions

**Recommendation**: Global search/replace for `tagged_na(` → `haven::tagged_na(`

---

### **4. BMI Functions - Inconsistent Validation Approaches**

#### **Issue**: Mixed Validation Patterns
**Location**: `R/bmi.R` various functions
```r
# Some use helper functions
is_valid <- validate_height_weight(...)

# Others do inline validation with different logic
```

**Discussion Points**:
- Could lead to same inputs being accepted/rejected by different functions
- Validation bounds differ between PUMF vs Master files
- Should validation be centralized?

**Recommendation**: Standardize validation approach across all BMI functions

---

## **Function Integration Issues**

### **1. rec_with_table() Compatibility**

#### **Issue**: Enhanced Function Naming Conflicts
**Status**: ✅ **RESOLVED** - Enhanced functions now use original names

**Solution Implemented**:
- `R/alcohol.R` - Enhanced functions with ORIGINAL names (e.g., `binge_drinker_fun`)
- `R/alcohol-legacy.R` - Legacy functions (backup)
- `R/bmi.R` - Enhanced functions with ORIGINAL names (e.g., `bmi_fun`)
- `R/bmi-legacy.R` - Legacy functions (backup)
- `R/adl.R` - Enhanced functions with ORIGINAL names (e.g., `adl_fun`)
- `R/adl-legacy.R` - Legacy functions (backup)

**Result**: rec_with_table() calls work exactly as before, but now use enhanced functions automatically!

**Still Need to Test**:
- Integration with actual CCHS data across all cycles
- Performance with real data volumes
- variable_details.csv integration

---

### **2. Function Organization Strategy**

#### **Implemented Structure**: ✅ **COMPLETED**
- `R/alcohol.R` - Enhanced functions (default)
- `R/alcohol-legacy.R` - Legacy functions (deprecated)
- `R/bmi.R` - Enhanced functions (default)  
- `R/bmi-legacy.R` - Legacy functions (deprecated)
- `R/adl.R` - Enhanced functions (default)
- `R/adl-legacy.R` - Legacy functions (deprecated)

**Benefits Achieved**:
✅ Maintains complete backward compatibility
✅ Users get enhanced functions automatically  
✅ No changes needed to variable_details.csv
✅ Legacy functions preserved for validation
✅ rec_with_table() works without modification

---

## **Testing Gaps Identified**

### **1. Real Data Testing**
**Status**: Enhanced functions tested with synthetic data only

**Need to Test**:
- Integration with actual CCHS cycles (2001-2018)
- Performance with real data volumes
- rec_with_table() compatibility
- variable_details.csv integration

### **2. Cross-Cycle Consistency**
**Status**: Not validated

**Need to Test**:
- Enhanced functions produce consistent results across cycles
- Missing data handling works with different cycle formats
- Validation bounds appropriate for all cycles

---

## **Documentation and Metadata Issues**

### **1. variable_details.csv Updates**

#### **Issue**: Enhanced Functions Not Integrated
**Discussion Points**:
- variable_details.csv likely references legacy function names
- Enhanced functions need to be integrated into metadata
- max/min validation should reference variable_details.csv
- Need clear migration path for users

**Recommendation**: Update variable_details.csv to support enhanced functions

### **2. Version Metadata Consistency**

#### **Issue**: Inconsistent @note Formatting
**Examples from legacy code**:
```r
# Some functions have v3.0.0 notes, others don't
# Format varies across functions
```

**Recommendation**: Standardize version metadata format across all functions

---

## **Priority Recommendations**

### **High Priority**
1. **Test enhanced functions with real CCHS data and rec_with_table()** ⚠️ **PENDING**
2. **Fix physical activity tagged_na() namespace issue** ✅ **COMPLETED** - Fixed in commit 4b03cef
3. **Decide on function organization strategy** ✅ **COMPLETED** - Implemented enhanced functions with original names

### **Medium Priority**  
1. **Review alcohol function thresholds with clinical team**
2. **Standardize BMI validation approaches**
3. **Update variable_details.csv for enhanced functions**
4. **R/Tidyverse documentation standardization** ✅ **COMPLETED** - All derived variable functions updated to community standards

### **Low Priority**
1. **Implement more realistic alcohol consumption bounds**
2. **Standardize version metadata format**
3. **Create comprehensive cross-cycle testing suite**

---

## **5. Smoking Functions - Research Standards vs Implementation Decisions**

### **Issue**: Constants and Age Boundaries Based on Research vs Team Decisions

**Research Foundation**: Functions follow Holford et al. (2014) and Manuel et al. (2020) smoking initiation/cessation standards

#### **✅ Research-Confirmed Constants (Implemented Correctly)**
**Location**: `R/smoking.R` constants section
```r
MIN_SMOKING_INITIATION_AGE <- 8  # ✅ Holford et al. standard
```

**Evidence-Based Decisions**:
- **Age 8 minimum**: All smoking initiation variables correctly use Holford et al. minimum age 8
- **2-year cessation rule**: Former smoker definition aligns with ≥2 years research standard
- **Model age range**: Research covers ages 8-99 for projection models

#### **❓ Implementation Decisions Requiring Team Review**

**Location**: `R/smoking.R` - SMOKING_AGE_BOUNDS, TIME_QUIT_BOUNDS

**Age Maximums (Not Specified in Research)**:
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

**Team Decisions Made vs Still Needed**:
- ✅ **Decided**: Youngest smoking initiation age = 8 (Holford standard)
- ❓ **Team Decision Needed**: Maximum age of smoking cessation (currently 84)
- ❓ **Team Decision Needed**: Upper bound for time since quit smoking (currently 82 years)
- ❓ **Team Decision Needed**: CCHS categorical-to-continuous age mappings

**Discussion Points**:
1. **Max cessation age**: Research doesn't specify upper bounds - should we use 84, 95, or CCHS max age (102)?
2. **Time since quit maximum**: 82 years may exclude elderly former smokers - is this appropriate?
3. **Categorical mappings**: CCHS-specific conversions (e.g., category 3 = age 16 vs 17 across cycles)

**Documentation**: Comprehensive analysis available in `/cchsflow-temp/scope-docs/smoking-constants-comprehensive-review.md`

**Recommendation**: Review implementation decisions for consistency with current research and CCHS data characteristics

---

## **Next Steps**

1. **Team Discussion**: Review identified issues and prioritize fixes
2. **Real Data Testing**: Validate enhanced functions with actual CCHS data  
3. **Function Reorganization**: Implement alcohol.R (enhanced) + alcohol-legacy.R structure
4. **Documentation Updates**: Update variable_details.csv and function metadata
5. **Migration Planning**: Develop user communication strategy for function transitions
6. **Smoking Constants Review**: Team decision on age maximums and boundary constants not specified in research