# **A Configuration-Driven Architecture for Flexible Missing Data Handling**

**Document version:** 3.1
**Date:** 2025-07-22
**Author:** CCHS Flow team
**Status:** Final Specification


## **Executive Summary**

This document outlines a revised, modern architecture for handling missing data within the cchsflow package. The previous approach, while functional, tightly coupled the transformation logic with the missing data rules, leading to code that was difficult to maintain and extend.

This new specification details a **configuration-driven, handler-based architecture**. The core of this design is to decouple the business logic of a derived variable (e.g., how to calculate BMI) from the complex, version-specific rules for handling missing data. This is achieved by introducing two key components:



1. A centralized **YAML configuration file** that acts as the single source of truth for all missing data patterns.
2. A **"Missing Data Handler"** factory function (create_missing_handler) that reads the configuration and provides tools for derived variable functions to use.

This new architecture makes the derived variable functions clean, semantic, and readable. It allows the system to intelligently adapt to the format of the input data (either original numeric codes or haven::tagged_na values) and makes the entire system significantly more robust, maintainable, and scalable for future CCHS cycles.


## **1. Introduction and Goal**


### **1.1. The Challenge**

The CCHS uses numerous, often inconsistent, codes for missing data (e.g., "Not applicable," "Don't know," "Refusal"). These codes can be numeric (6, 997) or based on haven::tagged_na(). Hard-coding this logic within each derived variable function is brittle and creates significant maintenance overhead.


### **1.2. Project Goal**

The goal of this project is to refactor our missing data logic into a flexible, configuration-driven system. This system will automatically detect the format of input data and apply the correct rules from a central configuration file. This will make our functions cleaner, more robust, and easier to maintain and extend.


## **2. The New Architecture: The "Missing Data Handler"**

The cornerstone of the new approach is a "Missing Data Handler" system. This system is composed of two primary parts: a central YAML configuration and an R function that acts as a "handler factory."


### **2.1. Central Configuration File (inst/metadata/cchs_missing_data.yaml)**

All missing data rules will be defined in a single YAML file. This file will be the single source of truth, making updates for new survey cycles simple and centralized. It will be structured by pattern type (e.g., single_digit_missing).

**YAML Structure (Updated to match implementation):**

```yaml
# inst/metadata/schemas/cchs/cchs_missing_data.yaml

schema_version: "1.0.0"
schema_date: "2025-07-22"  
description: "CCHS missing data patterns and transformation rules"

pattern_definitions:
  patterns:
    single_digit_missing:
      description: "Binary/ternary variables with valid responses 1-5"
      transformation_map:
        6: "haven::tagged_na('a')"  # Not applicable
        7: "haven::tagged_na('b')"  # Don't know
        8: "haven::tagged_na('b')"  # Refusal
        9: "haven::tagged_na('b')"  # Not stated
      priority_hierarchy:
        not_applicable:
          priority: 1
          label: "Not applicable"
          original_codes: [6]
          tagged_na: ["a"]
        missing_data:
          priority: 2
          label: "Missing data"
          original_codes: [7, 8, 9]
          tagged_na: ["b"]
    
    triple_digit_missing:
      description: "Continuous measurements with variable-specific valid ranges"
      transformation_map:
        996: "haven::tagged_na('a')"  # Not applicable
        997: "haven::tagged_na('b')"  # Don't know
        998: "haven::tagged_na('b')"  # Refusal
        999: "haven::tagged_na('b')"  # Not stated
        999.6: "haven::tagged_na('a')" # Not applicable (decimal)
        999.7: "haven::tagged_na('b')" # Don't know (decimal)
        999.8: "haven::tagged_na('b')" # Refusal (decimal)
        999.9: "haven::tagged_na('b')" # Not stated (decimal)
      priority_hierarchy:
        not_applicable:
          priority: 1
          label: "Not applicable"
          original_codes: [996]
          decimal_codes: [999.6]
          tagged_na: ["a"]
        missing_data:
          priority: 2
          label: "Missing data"
          original_codes: [997, 998, 999]
          decimal_codes: [999.7, 999.8, 999.9]
          tagged_na: ["b"]

```


### **2.2. The Handler Factory (create_missing_handler)**

This R function is the engine of the new system. It inspects the input data, reads the appropriate section from the YAML configuration, and returns a list of helper functions ("tools") that are tailored to the specific data format it detected.

**Key Responsibilities:**



* **Auto-detection:** Automatically determines if the input data is using "original" numeric codes or haven::tagged_na values by checking inherits(..., "haven_labelled"). **Critical Rule**: When multiple vectors are provided with mixed formats (some haven_labelled, some numeric), the system defaults to "tagged_na" mode for consistency.
* **Parameterization:** Accepts handle_missing_data ("auto", "original", "tagged_na") to allow for manual override and pattern_type to select the correct block from the YAML file.
* **Tool Generation:** Returns a list of helper functions, such as:
    * is_tag(x, ...): Checks if a value x matches one or more semantic tags (e.g., "not_applicable", "missing_data").
    * is_missing(x): Checks if a value is any type of missing value defined in the configuration.
    * propagate(...): Returns the highest-priority missing value from a set of inputs using the priority_hierarchy defined in the YAML configuration.


## **3. Technical Specifications**


### **3.1. Core Implementation**


<table>
  <tr>
   <td><strong>ID</strong>
   </td>
   <td><strong>Specification Description</strong>
   </td>
  </tr>
  <tr>
   <td>TS-1
   </td>
   <td>Create a new helper function create_missing_handler(..., handle_missing_data = "auto", pattern_type) in a new file, R/utils-missing.R, that implements the handler factory logic.
   </td>
  </tr>
  <tr>
   <td>TS-2
   </td>
   <td>Create a helper function load_cchs_config() that reads and parses the cchs_missing_data.yaml file.
   </td>
  </tr>
  <tr>
   <td>TS-3
   </td>
   <td>Refactor all derived variable functions (e.g., calculate_bmi, calculate_smoking_status_detailed) to use the create_missing_handler at the beginning of the function call.
   </td>
  </tr>
  <tr>
   <td>TS-4
   </td>
   <td>Remove all hard-coded missing value checks (e.g., == 9, is_tagged_na(x, "a")) from the derived variable functions and replace them with calls to the handler's tools (e.g., handler$is_tag(x, "refusal")).
   </td>
  </tr>
</table>



### **3.2. Refactored Function Examples**

The derived variable functions become clean, declarative, and easy to read. They state *what* they are checking for, and the handler determines *how* to check for it.

**Example: calculate_bmi_core**

```R
calculate_bmi_core <- function(height, weight, handle_missing_data) { 
  # 1. Get the handler for the correct pattern 
  handler <- create_missing_handler( 
    height, weight, 
    handle_missing_data = handle_missing_data, 
    pattern_type = "triple_digit_missing" 
  ) 
 
  # 2. Use the handler's tools to perform the logic 
  dplyr::case_when( 
    handler$is_missing(height) | handler$is_missing(weight) ~ handler$propagate(height, weight), 
    .default = weight / (height^2) 
  ) 
} 
```

**Example: calculate_smoking_status_detailed**

```R
calculate_smoking_status_detailed <- function(SMK_005, SMK_030, SMK_01A, handle_missing_data) { 
  # 1. Get the handler for the correct pattern 
  handler <- create_missing_handler( 
    SMK_005, SMK_030, SMK_01A, 
    handle_missing_data = handle_missing_data, 
    pattern_type = "single_digit_missing" 
  ) 
   
  # Aliases for readability 
  s005 <- SMK_005; s030 <- SMK_030; s01a <- SMK_01A 
 
  # 2. The logic is now semantic and format-agnostic 
  dplyr::case_when( 
    handler$is_missing(s005) ~ handler$propagate(s005), 
    s005 == 1 ~ 1L, 
    s005 == 2 & s030 == 1 ~ 2L, 
    s005 == 2 & (s030 == 2 | handler$is_tag(s030, "not_applicable", "missing_data")) ~ 3L, 
    # ... more rules ... 
    .default = handler$propagate(s005, s030, s01a) 
  ) 
} 
```


## **4. Testing Requirements**

The testing strategy must be updated to validate the new architecture.


<table>
  <tr>
   <td><strong>ID</strong>
   </td>
   <td><strong>Requirement Description</strong>
   </td>
  </tr>
  <tr>
   <td>TR-1
   </td>
   <td>Create unit tests for create_missing_handler itself, ensuring it returns the correct tools for each mode (original vs. tagged_na) and for different pattern_type values.
   </td>
  </tr>
  <tr>
   <td>TR-2
   </td>
   <td>For each derived variable function, create tests that pass in data with <strong>original numeric codes</strong> and confirm that the output correctly propagates the appropriate numeric code (e.g., inputting 9 results in 9).
   </td>
  </tr>
  <tr>
   <td>TR-3
   </td>
   <td>For each derived variable function, create tests that pass in data with <strong>haven::tagged_na values</strong> and confirm that the output correctly propagates the appropriate tagged_na value.
   </td>
  </tr>
  <tr>
   <td>TR-4
   </td>
   <td>Create tests for the "auto" mode, providing mixed haven_labelled and numeric vectors to ensure the handler correctly defaults to the "tagged_na" mode.
   </td>
  </tr>
  <tr>
   <td>TR-5
   </td>
   <td>Ensure all existing tests continue to pass after the refactoring to guarantee no regressions in the core calculation logic.
   </td>
  </tr>
</table>



## **5. Error Handling and Robustness**

The system must fail gracefully with informative error messages in error conditions:

**EH-1: Missing Configuration File**
- If `cchs_missing_data.yaml` is missing, the system should provide a clear error message with suggested file locations and basic troubleshooting steps.

**EH-2: Malformed YAML**
- Invalid YAML syntax should trigger an error with the specific line number and nature of the parsing error.

**EH-3: Invalid Pattern Types**
- Requesting a `pattern_type` that doesn't exist should list available patterns from the configuration file.

**EH-4: Configuration Validation**
- The system should validate that required fields (transformation_map, priority_hierarchy) exist for each pattern.

**EH-5: Graceful Degradation**
- If configuration loading fails, the system should optionally fall back to hardcoded patterns with a warning message.

## **6. Scope and Integration**



* **In Scope:** The creation of the create_missing_handler and load_cchs_config utilities, the creation of the cchs_missing_data.yaml file, and the complete refactoring of all derived variable functions to use this new system.
* **Out of Scope:** Direct changes to rec_with_table().
* **Integration with rec_with_table():** This architecture simplifies integration. Since the derived variable functions are now format-agnostic, they will work seamlessly whether they receive tagged_na data from rec_with_table or raw data with original numeric codes directly from a user. There is no conflict.

This new architecture provides a robust, maintainable, and scalable foundation for all current and future data processing work in the cchsflow package.