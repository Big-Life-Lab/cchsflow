# Flexible Missing Data Infrastructure - Development TODO

This document outlines the remaining tasks and improvements for the flexible missing data infrastructure, prioritized from most critical to least.

## Phase 1: Critical Architectural Fixes (Highest Priority)

These issues fundamentally break R package conventions, lead to unpredictable behavior, and make the code fragile and hard to maintain/test.

1.  **Refactor Dependency Management (Eliminate `source()` calls):**
    *   **Problem:** The pervasive use of `source()` with relative paths (e.g., `../R/`, `flexible-missing-handler.R`) within `clean-variables-enhanced.R`, `convert-missing.R`, and `flexible-missing-handler.R` is non-idiomatic for R packages. It leads to fragility, conflicts, and makes package building/testing difficult.
    *   **Development Proposal:** During active development within the `development/flexible-missing-data-mvp/` directory, the `source()` calls within the R files can remain *temporarily* as a visual reminder of internal dependencies. However, the primary method for running and testing this code should be by loading the entire development module as a package using `devtools::load_all()`. This ensures all functions are correctly loaded and available in the environment.
    *   **Production Action:** When this code is ready to be integrated into the main `R/` directory of the `cchsflow` package, *all* `source()` calls within these R files *must* be removed. Dependencies will then be managed solely through `roxygen2` tags (`@importFrom`) and the standard R package build process.
    *   **Status:** **DONE**. All `source()` calls have been removed from `clean_variables.R`, `convert-missing.R`, and `flexible-missing-handler.R`. The test environment now uses a dedicated helper file (`helper-env.R`) to manage the internal environment.
    *   **Impact:** This is foundational. Until this is done, other fixes might be masked or behave inconsistently.

2.  **Centralize and Isolate Global State/Caching:**
    *   **Status:** **DONE**. Caching mechanisms (`.cchs_config_cache` and `.variables_csv_cache`) have been refactored to use a dedicated internal package environment (`._flexible_missing_data_env`) defined in `R/zzz.R`.
    *   **Impact:** Improves robustness and avoids potential side effects in the global environment.

## Phase 2: Core Function Robustness & Clarity (High Priority)

These issues affect the robustness or correctness of core functions and their predictable behavior.

3.  **Standardize `check_vector_compatibility` Dependency:**
    *   **Status:** **DONE**. The `check_vector_compatibility` function has been defined directly within `clean_variables.R`, making it a private, self-contained helper.
    *   **Impact:** Ensures `clean_variables` behaves predictably and consistently, regardless of the environment.

4.  **Improve `clean_variables` NULL Input Handling:**
    *   **Status:** **DONE**. `clean_variables()` now replaces `NULL` inputs with `NA_real_` and issues a warning, allowing for graceful propagation of missing values instead of stopping execution.
    *   **Impact:** Prevents `stop()` calls for `NULL` inputs, allowing for more flexible data pipelines.

## Phase 3: Refinements & Optimizations (Medium Priority)

These items focus on minor redundancies, efficiency gains, or areas where clarity could be improved without breaking core functionality.

5.  **Optimize Pattern Detection Redundancy:**
    *   **Status:** **ADDRESSED BY DESIGN**. `clean_variables` correctly determines and passes the `pattern_type` to `convert_missing`. The auto-detection logic within `convert_missing` is necessary for its standalone use as an exported function. No problematic redundancy exists.
    *   **Impact:** Clarified design intent.

6.  **Refine `variables.csv` Path Resolution:**
    *   **Status:** **DONE**. The `load_variables_csv` function now primarily uses `system.file()` for robust package data loading, and `here::here()` for development environments, aligning with project conventions.
    *   **Impact:** More reliable loading of metadata, especially when the package is installed or during development.

## Phase 4: Documentation & Code Clarity (Lower Priority)

These are important for long-term maintainability and developer experience.

7.  **Update Code Comments:**
    *   **Status:** **DONE**. Comments in `bmi-refactored.R` have been reviewed and updated to reflect current logic and design.
    *   **Impact:** Improves code readability and maintainability for future developers.
