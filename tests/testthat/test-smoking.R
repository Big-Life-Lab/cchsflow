# =============================================================================
# Smoking Function Tests - Comprehensive Test Suite (v3.1.0)
# =============================================================================
#
# Consolidated test suite for smoking functions with CSV-driven implementation:
# - Core functionality tests (priority logic, mappings, vector processing)
# - CCHS codebook validation (official documentation compliance)
# - Mathematical property tests (non-negative results, ordering, consistency)
# - Missing data handling patterns (CCHS codes → tagged_na)
# - Edge cases and integration patterns
#
# This file consolidates tests from multiple previous test files to provide
# comprehensive coverage while focusing on the calculate_time_quit_smoking function.
#
# @note v3.1.0, last updated: 2025-07-13, status: active

library(testthat)
library(haven)
library(dplyr)

# Functions are loaded by the testthat helper.R

# =============================================================================
# calculate_smoking_status_detailed() Tests (SMKDSTY_A)
# =============================================================================

test_that("calculate_smoking_status_detailed correctly implements SMKDSTY_A classification", {
  
  # Test Case 1: Daily smoker (SMK_005 = 1)
  expect_equal(calculate_smoking_status_detailed(SMK_005 = 1, SMK_030 = 1, SMK_01A = 1), 1L)
  expect_equal(calculate_smoking_status_detailed(SMK_005 = 1, SMK_030 = 2, SMK_01A = 2), 1L)
  expect_equal(calculate_smoking_status_detailed(SMK_005 = 1, SMK_030 = 6, SMK_01A = 6), 1L)
  
  # Test Case 2: Occasional smoker (former daily) - SMK_005 = 2 & SMK_030 = 1
  expect_equal(calculate_smoking_status_detailed(SMK_005 = 2, SMK_030 = 1, SMK_01A = 1), 2L)
  expect_equal(calculate_smoking_status_detailed(SMK_005 = 2, SMK_030 = 1, SMK_01A = 2), 2L)
  
  # Test Case 3: Occasional smoker (never daily) - SMK_005 = 2 & SMK_030 ≠ 1
  expect_equal(calculate_smoking_status_detailed(SMK_005 = 2, SMK_030 = 2, SMK_01A = 1), 3L)
  expect_equal(calculate_smoking_status_detailed(SMK_005 = 2, SMK_030 = 2, SMK_01A = 2), 3L)
  
  # Test Case 4: Former daily smoker - SMK_005 = 3 & SMK_030 = 1
  expect_equal(calculate_smoking_status_detailed(SMK_005 = 3, SMK_030 = 1, SMK_01A = 1), 4L)
  expect_equal(calculate_smoking_status_detailed(SMK_005 = 3, SMK_030 = 1, SMK_01A = 2), 4L)
  
  # Test Case 5: Former occasional smoker - SMK_005 = 3 & SMK_030 = 2 & SMK_01A = 1
  expect_equal(calculate_smoking_status_detailed(SMK_005 = 3, SMK_030 = 2, SMK_01A = 1), 5L)
  
  # Test Case 6: Never smoked - SMK_005 = 3 & SMK_01A = 2
  expect_equal(calculate_smoking_status_detailed(SMK_005 = 3, SMK_030 = 2, SMK_01A = 2), 6L)
  expect_equal(calculate_smoking_status_detailed(SMK_005 = 3, SMK_030 = 6, SMK_01A = 2), 6L)
})

test_that("calculate_smoking_status_detailed handles missing data correctly", {
  
  # Test SMK_005 missing codes (should propagate)
  expect_true(haven::is_tagged_na(calculate_smoking_status_detailed(SMK_005 = 6, SMK_030 = 1, SMK_01A = 1), "a"))
  expect_true(haven::is_tagged_na(calculate_smoking_status_detailed(SMK_005 = 7, SMK_030 = 1, SMK_01A = 1), "b"))
  expect_true(haven::is_tagged_na(calculate_smoking_status_detailed(SMK_005 = 8, SMK_030 = 1, SMK_01A = 1), "b"))
  expect_true(haven::is_tagged_na(calculate_smoking_status_detailed(SMK_005 = 9, SMK_030 = 1, SMK_01A = 1), "b"))
  
  # Test SMK_030 missing codes in occasional smoker context (should result in category 3)
  expect_equal(calculate_smoking_status_detailed(SMK_005 = 2, SMK_030 = 6, SMK_01A = 1), 3L)
  expect_equal(calculate_smoking_status_detailed(SMK_005 = 2, SMK_030 = 7, SMK_01A = 1), 3L)
  expect_equal(calculate_smoking_status_detailed(SMK_005 = 2, SMK_030 = 8, SMK_01A = 1), 3L)
  
  # Test invalid codes result in tagged_na("b")
  expect_true(haven::is_tagged_na(calculate_smoking_status_detailed(SMK_005 = 5, SMK_030 = 1, SMK_01A = 1), "b"))
  expect_true(haven::is_tagged_na(calculate_smoking_status_detailed(SMK_005 = 10, SMK_030 = 1, SMK_01A = 1), "b"))
})

test_that("calculate_smoking_status_detailed handles vector inputs correctly", {
  
  # Test vector processing with complete classification spectrum
  smk_005_vec <- c(1, 2, 2, 3, 3, 3, 6, 7)
  smk_030_vec <- c(1, 1, 2, 1, 2, 2, 1, 1)
  smk_01a_vec <- c(1, 1, 1, 1, 1, 2, 1, 1)
  
  expected_vec <- c(1L, 2L, 3L, 4L, 5L, 6L, NA, NA)
  
  result_vec <- calculate_smoking_status_detailed(
    SMK_005 = smk_005_vec,
    SMK_030 = smk_030_vec, 
    SMK_01A = smk_01a_vec
  )
  
  expect_length(result_vec, 8)
  
  # Check valid results
  expect_equal(result_vec[1], 1L)  # Daily smoker
  expect_equal(result_vec[2], 2L)  # Occasional (former daily)
  expect_equal(result_vec[3], 3L)  # Occasional (never daily)
  expect_equal(result_vec[4], 4L)  # Former daily
  expect_equal(result_vec[5], 5L)  # Former occasional
  expect_equal(result_vec[6], 6L)  # Never smoked
  
  # Check missing data patterns
  expect_true(haven::is_tagged_na(result_vec[7], "a"))  # Code 6 → "a"
  expect_true(haven::is_tagged_na(result_vec[8], "b"))  # Code 7 → "b"
})

test_that("calculate_smoking_status_detailed edge cases and complex logic", {
  
  # Test edge case: Former smoker classification depends on SMK_01A
  # SMK_005=3, SMK_030=2 can be either category 5 or 6 based on SMK_01A
  expect_equal(calculate_smoking_status_detailed(SMK_005 = 3, SMK_030 = 2, SMK_01A = 1), 5L) # Former occasional
  expect_equal(calculate_smoking_status_detailed(SMK_005 = 3, SMK_030 = 2, SMK_01A = 2), 6L) # Never smoked
  
  # Test that SMK_030 and SMK_01A missing codes don't affect daily smokers
  expect_equal(calculate_smoking_status_detailed(SMK_005 = 1, SMK_030 = 6, SMK_01A = 6), 1L)
  expect_equal(calculate_smoking_status_detailed(SMK_005 = 1, SMK_030 = 7, SMK_01A = 7), 1L)
  
  # Test unhandled combinations default to tagged_na("b")
  expect_true(haven::is_tagged_na(calculate_smoking_status_detailed(SMK_005 = 4, SMK_030 = 1, SMK_01A = 1), "b"))
})

# =============================================================================
# calculate_smoking_status() Alias Function Tests
# =============================================================================

test_that("calculate_smoking_status alias works correctly", {
  
  # Test that alias produces identical results to detailed function
  test_cases <- list(
    list(SMK_005 = 1, SMK_030 = 1, SMK_01A = 1, expected = 1L),
    list(SMK_005 = 2, SMK_030 = 1, SMK_01A = 1, expected = 2L),
    list(SMK_005 = 2, SMK_030 = 2, SMK_01A = 1, expected = 3L),
    list(SMK_005 = 3, SMK_030 = 1, SMK_01A = 1, expected = 4L),
    list(SMK_005 = 3, SMK_030 = 2, SMK_01A = 1, expected = 5L),
    list(SMK_005 = 3, SMK_030 = 2, SMK_01A = 2, expected = 6L)
  )
  
  for (case in test_cases) {
    result_detailed <- calculate_smoking_status_detailed(
      SMK_005 = case$SMK_005, 
      SMK_030 = case$SMK_030, 
      SMK_01A = case$SMK_01A
    )
    result_alias <- calculate_smoking_status(
      SMK_005 = case$SMK_005, 
      SMK_030 = case$SMK_030, 
      SMK_01A = case$SMK_01A
    )
    
    expect_equal(result_detailed, result_alias)
    expect_equal(result_alias, case$expected)
  }
})

# =============================================================================
# CCHS Codebook Validation for Smoking Status
# =============================================================================

test_that("smoking status classifications match CCHS codebook definitions", {
  
  # Based on CCHS smoking variable definitions:
  # SMK_005: 1=daily, 2=occasional, 3=not at all
  # SMK_030: 1=yes (ever daily), 2=no (never daily) 
  # SMK_01A: 1=yes (≥100 cigarettes), 2=no (<100 cigarettes)
  
  # SMKDSTY_A categories:
  # 1 = Daily smoker
  # 2 = Occasional smoker (former daily)
  # 3 = Occasional smoker (never daily)  
  # 4 = Former daily smoker
  # 5 = Former occasional smoker
  # 6 = Never smoked
  
  # Test all documented category combinations
  expect_equal(calculate_smoking_status_detailed(1, 1, 1), 1L, 
              info = "Daily smoker: SMK_005=1 should always → category 1")
  expect_equal(calculate_smoking_status_detailed(1, 2, 2), 1L,
              info = "Daily smoker: SMK_005=1 regardless of other variables → category 1")
              
  expect_equal(calculate_smoking_status_detailed(2, 1, 1), 2L,
              info = "Occasional (former daily): SMK_005=2 & SMK_030=1 → category 2")
              
  expect_equal(calculate_smoking_status_detailed(2, 2, 1), 3L,
              info = "Occasional (never daily): SMK_005=2 & SMK_030=2 → category 3")
              
  expect_equal(calculate_smoking_status_detailed(3, 1, 1), 4L,
              info = "Former daily: SMK_005=3 & SMK_030=1 → category 4")
              
  expect_equal(calculate_smoking_status_detailed(3, 2, 1), 5L,
              info = "Former occasional: SMK_005=3 & SMK_030=2 & SMK_01A=1 → category 5")
              
  expect_equal(calculate_smoking_status_detailed(3, 2, 2), 6L,
              info = "Never smoked: SMK_005=3 & SMK_01A=2 → category 6")
})

# =============================================================================
# Legacy Compatibility Tests
# =============================================================================

test_that("smoking status function matches legacy SMKDSTY_fun behavior", {
  
  # Test cases that replicate the exact legacy function logic
  # From smoking-legacy-v2-1-0.R SMKDSTY_fun
  
  # Test complete decision tree
  legacy_test_cases <- data.frame(
    SMK_005 = c(1, 2, 2, 2, 3, 3, 3, 3, 6, 7),
    SMK_030 = c(1, 1, 2, 6, 1, 2, 2, 6, 1, 1), 
    SMK_01A = c(1, 1, 1, 1, 1, 1, 2, 1, 1, 1),
    expected = c(1L, 2L, 3L, 3L, 4L, 5L, 6L, 6L, NA, NA),
    description = c(
      "Daily smoker",
      "Occasional smoker (former daily)",
      "Occasional smoker (never daily)",
      "Occasional smoker (never daily - missing SMK_030)",
      "Former daily smoker", 
      "Former occasional smoker",
      "Never smoked",
      "Never smoked (missing SMK_030)",
      "Missing SMK_005 (code 6)",
      "Missing SMK_005 (code 7)"
    )
  )
  
  for (i in 1:nrow(legacy_test_cases)) {
    result <- calculate_smoking_status_detailed(
      SMK_005 = legacy_test_cases$SMK_005[i],
      SMK_030 = legacy_test_cases$SMK_030[i],
      SMK_01A = legacy_test_cases$SMK_01A[i]
    )
    
    if (is.na(legacy_test_cases$expected[i])) {
      expect_true(haven::is_tagged_na(result) || is.na(result),
                 info = paste("Test case", i, ":", legacy_test_cases$description[i]))
    } else {
      expect_equal(result, legacy_test_cases$expected[i],
                  info = paste("Test case", i, ":", legacy_test_cases$description[i]))
    }
  }
})

# =============================================================================
# calculate_time_quit_smoking() Tests
# =============================================================================

test_that("calculate_time_quit_smoking correctly applies priority logic and mappings", {
  
  # Test Case 1: SMK_09A_B has priority (values 1, 2, 3)
  expect_equal(calculate_time_quit_smoking(SMK_09A_B = 1, SMKG09C = 999), 0.5)
  expect_equal(calculate_time_quit_smoking(SMK_09A_B = 2, SMKG09C = 1), 1.5)
  expect_equal(calculate_time_quit_smoking(SMK_09A_B = 3, SMKG09C = 2), 2.5)
  
  # Test Case 2: Logic defers to SMKG09C when SMK_09A_B is 4
  expect_equal(calculate_time_quit_smoking(SMK_09A_B = 4, SMKG09C = 1), 4)
  expect_equal(calculate_time_quit_smoking(SMK_09A_B = 4, SMKG09C = 2), 8)
  expect_equal(calculate_time_quit_smoking(SMK_09A_B = 4, SMKG09C = 3), 12)
  
  # Test Case 3: Missing data propagation
  # Tagged NA in priority variable (should propagate)
  expect_true(haven::is_tagged_na(calculate_time_quit_smoking(SMK_09A_B = 6, SMKG09C = 1), "a"))
  expect_true(haven::is_tagged_na(calculate_time_quit_smoking(SMK_09A_B = 7, SMKG09C = 1), "b"))
  
  # Tagged NA in secondary variable when it's needed (should propagate)
  expect_true(haven::is_tagged_na(calculate_time_quit_smoking(SMK_09A_B = 4, SMKG09C = 6), "a"))
  expect_true(haven::is_tagged_na(calculate_time_quit_smoking(SMK_09A_B = 4, SMKG09C = 997), "b"))
  
  # Tagged NA in secondary variable when it's NOT needed (should be ignored)
  expect_equal(calculate_time_quit_smoking(SMK_09A_B = 2, SMKG09C = 997), 1.5)
  
  # Test Case 4: Invalid codes result in tagged_na('b')
  expect_true(haven::is_tagged_na(calculate_time_quit_smoking(SMK_09A_B = 5, SMKG09C = 1), "b"))
  expect_true(haven::is_tagged_na(calculate_time_quit_smoking(SMK_09A_B = 4, SMKG09C = 5), "b"))
  
  # Test Case 5: Vector processing
  smk_a_vec <- c(1, 2, 4, 4, 7, 4, 5)
  smk_c_vec <- c(99, 99, 1, 3, 99, 997, 2)
  
  result_vec <- calculate_time_quit_smoking(SMK_09A_B = smk_a_vec, SMKG09C = smk_c_vec)
  
  # Check results element by element
  expect_equal(result_vec[1], 0.5)
  expect_equal(result_vec[2], 1.5)
  expect_equal(result_vec[3], 4)
  expect_equal(result_vec[4], 12)
  expect_true(haven::is_tagged_na(result_vec[5], "b"))
  expect_true(haven::is_tagged_na(result_vec[6], "b"))
  expect_true(haven::is_tagged_na(result_vec[7], "b"))
})


# =============================================================================
# General Missing Data Pattern Tests
# =============================================================================

test_that("smoking functions handle CCHS missing codes correctly", {
  
  # Test single-digit missing pattern for calculate_time_quit_smoking
  # Code 6 (not applicable) → tagged_na("a")
  result_na_a <- calculate_time_quit_smoking(SMK_09A_B = 6, SMKG09C = 1)
  expect_true(haven::is_tagged_na(result_na_a))
  expect_equal(haven::na_tag(result_na_a), "a")
  
  # Code 7 (don't know) → tagged_na("b")
  result_na_b <- calculate_time_quit_smoking(SMK_09A_B = 7, SMKG09C = 1)
  expect_true(haven::is_tagged_na(result_na_b))
  expect_equal(haven::na_tag(result_na_b), "b")
})

test_that("smoking functions handle triple-digit missing codes correctly", {
  
  # Test triple-digit missing codes in calculate_time_quit_smoking
  result_997 <- calculate_time_quit_smoking(SMK_09A_B = 4, SMKG09C = 997)
  expect_true(haven::is_tagged_na(result_997, "b"))
  
  result_998 <- calculate_time_quit_smoking(SMK_09A_B = 4, SMKG09C = 998)
  expect_true(haven::is_tagged_na(result_998, "b"))
  
  result_999 <- calculate_time_quit_smoking(SMK_09A_B = 4, SMKG09C = 999)
  expect_true(haven::is_tagged_na(result_999, "b"))
})

# =============================================================================
# Vector Input Support Tests  
# =============================================================================

test_that("smoking functions handle vector inputs correctly", {
  
  # Test calculate_time_quit_smoking with vectors
  smk_a_vec <- c(1, 2, 4, 4, 6, 7, 8)
  smk_c_vec <- c(99, 99, 1, 3, 99, 99, 99)
  
  result_vec <- calculate_time_quit_smoking(
    SMK_09A_B = smk_a_vec,
    SMKG09C = smk_c_vec
  )
  
  expect_length(result_vec, 7)
  
  # Check results
  expect_equal(result_vec[1], 0.5)  # SMK_09A_B=1 → 0.5
  expect_equal(result_vec[2], 1.5)  # SMK_09A_B=2 → 1.5  
  expect_equal(result_vec[3], 4)    # SMK_09A_B=4, SMKG09C=1 → 4
  expect_equal(result_vec[4], 12)   # SMK_09A_B=4, SMKG09C=3 → 12
  
  # Check missing data pattern preservation in vectors
  expect_equal(haven::na_tag(result_vec[5]), "a") # Code 6 → "a"
  expect_equal(haven::na_tag(result_vec[6]), "b") # Code 7 → "b"
  expect_equal(haven::na_tag(result_vec[7]), "b") # Code 8 → "b"
})

# =============================================================================
# Edge Case Logic Tests
# =============================================================================

test_that("smoking functions handle edge cases correctly", {
  
  # Test invalid codes result in tagged_na("b")
  result_invalid <- calculate_time_quit_smoking(SMK_09A_B = 5, SMKG09C = 1)
  expect_true(haven::is_tagged_na(result_invalid, "b"))
  
  # Test secondary variable missing when not needed
  result_ignore_missing <- calculate_time_quit_smoking(SMK_09A_B = 2, SMKG09C = 997)
  expect_equal(result_ignore_missing, 1.5) # Should ignore missing SMKG09C
})

test_that("function priority logic works correctly", {
  
  # SMK_09A_B should take priority over SMKG09C when both valid
  result_priority <- calculate_time_quit_smoking(SMK_09A_B = 2, SMKG09C = 1)
  expect_equal(result_priority, 1.5) # Should use SMK_09A_B=2, not SMKG09C=1
  
  # Only when SMK_09A_B=4 should function defer to SMKG09C
  result_defer <- calculate_time_quit_smoking(SMK_09A_B = 4, SMKG09C = 2)
  expect_equal(result_defer, 8) # Should use SMKG09C=2 value
})

# =============================================================================
# Integration Pattern Tests
# =============================================================================

test_that("smoking functions maintain consistent missing data patterns", {
  
  # Test that tagged_na patterns are consistent
  result_6 <- calculate_time_quit_smoking(SMK_09A_B = 6, SMKG09C = 1)
  result_7 <- calculate_time_quit_smoking(SMK_09A_B = 7, SMKG09C = 1)
  
  # Code 6 should consistently produce tagged_na("a")
  expect_true(haven::is_tagged_na(result_6, "a"))
  
  # Code 7 should consistently produce tagged_na("b") 
  expect_true(haven::is_tagged_na(result_7, "b"))
  
  # Different missing codes should produce appropriate tagged_na types
  expect_false(haven::is_tagged_na(result_6, "b")) # 6 is not "b" type
  expect_false(haven::is_tagged_na(result_7, "a")) # 7 is not "a" type
})

# =============================================================================
# Log Level Tests
# =============================================================================

test_that("smoking functions respect log_level parameter", {
  
  # calculate_time_quit_smoking should accept log_level parameter without error
  expect_no_error({
    calculate_time_quit_smoking(SMK_09A_B = 2, SMKG09C = 3, log_level = "silent")
    calculate_time_quit_smoking(SMK_09A_B = 2, SMKG09C = 3, log_level = "verbose")
    calculate_time_quit_smoking(SMK_09A_B = 2, SMKG09C = 3, log_level = "warning")
  })
})

# =============================================================================
# CCHS Codebook Validation Tests
# =============================================================================

test_that("categorical mappings match CCHS codebook definitions", {
  
  # Based on CCHS documentation:
  # SMK_09A_B categories (time since quit smoking - recent quitters)
  # 1 = "Less than 1 year ago" → midpoint = 0.5 years
  # 2 = "1 year to less than 2 years ago" → midpoint = 1.5 years  
  # 3 = "2 years to less than 3 years ago" → midpoint = 2.5 years
  # 4 = "3 or more years ago" → use SMKG09C for detail
  
  expect_equal(calculate_time_quit_smoking(1, NA), 0.5, 
              info = "SMK_09A_B=1 should map to 0.5 years (< 1 year midpoint)")
  expect_equal(calculate_time_quit_smoking(2, NA), 1.5,
              info = "SMK_09A_B=2 should map to 1.5 years (1-2 year midpoint)")
  expect_equal(calculate_time_quit_smoking(3, NA), 2.5,
              info = "SMK_09A_B=3 should map to 2.5 years (2-3 year midpoint)")
  
  # SMKG09C categories (time since quit smoking - detailed)
  # 1 = "3 to 5 years ago" → midpoint = 4 years
  # 2 = "6 to 10 years ago" → midpoint = 8 years
  # 3 = "11 years ago or more" → conservative estimate = 12 years
  
  expect_equal(calculate_time_quit_smoking(4, 1), 4,
              info = "SMKG09C=1 should map to 4 years (3-5 year midpoint)")
  expect_equal(calculate_time_quit_smoking(4, 2), 8,
              info = "SMKG09C=2 should map to 8 years (6-10 year midpoint)")
  expect_equal(calculate_time_quit_smoking(4, 3), 12,
              info = "SMKG09C=3 should map to 12 years (11+ year estimate)")
})

test_that("missing data codes match CCHS standards", {
  
  # CCHS missing data codes (documented):
  # 6 = "Not applicable" → tagged_na("a")
  # 7 = "Don't know" → tagged_na("b")  
  # 8 = "Refusal" → tagged_na("b")
  # 9 = "Not stated" → tagged_na("b")
  # 997 = "Don't know" → tagged_na("b")
  # 998 = "Refusal" → tagged_na("b") 
  # 999 = "Not stated" → tagged_na("b")
  
  # Test single-digit codes
  result_6 <- calculate_time_quit_smoking(6, 1)
  expect_true(haven::is_tagged_na(result_6, "a"))
  
  result_7 <- calculate_time_quit_smoking(7, 1)
  expect_true(haven::is_tagged_na(result_7, "b"))
  
  # Test triple-digit codes (should propagate through SMKG09C)
  result_997 <- calculate_time_quit_smoking(4, 997)
  expect_true(haven::is_tagged_na(result_997, "b"))
})

# =============================================================================
# Mathematical Property Tests
# =============================================================================

test_that("time_quit_smoking satisfies mathematical properties", {
  
  # Property 1: Results should be non-negative when valid
  for (smk_09a_b in 1:4) {
    for (smkg09c in c(1, 2, 3)) {  # Only test valid codes
      result <- calculate_time_quit_smoking(smk_09a_b, smkg09c)
      if (!haven::is_tagged_na(result) && !is.na(result)) {
        expect_true(result >= 0, 
                   info = paste("Negative time for SMK_09A_B =", smk_09a_b, ", SMKG09C =", smkg09c))
      }
    }
  }
  
  # Property 2: Categorical ordering should be preserved
  # SMK_09A_B categories 1 < 2 < 3 should give increasing time values
  result_1 <- calculate_time_quit_smoking(1, NA)  # 0.5
  result_2 <- calculate_time_quit_smoking(2, NA)  # 1.5  
  result_3 <- calculate_time_quit_smoking(3, NA)  # 2.5
  
  expect_true(result_1 < result_2, "SMK_09A_B ordering: category 1 should < category 2")
  expect_true(result_2 < result_3, "SMK_09A_B ordering: category 2 should < category 3")
  
  # Property 3: SMKG09C categorical ordering should be preserved  
  result_cat1 <- calculate_time_quit_smoking(4, 1)  # Should be 4
  result_cat2 <- calculate_time_quit_smoking(4, 2)  # Should be 8
  result_cat3 <- calculate_time_quit_smoking(4, 3)  # Should be 12
  
  expect_true(result_cat1 < result_cat2, "SMKG09C ordering: category 1 should < category 2")
  expect_true(result_cat2 < result_cat3, "SMKG09C ordering: category 2 should < category 3")
})

test_that("missing data patterns are consistent", {
  
  # Property: Missing data codes should produce tagged_na consistently
  missing_codes_single <- c(6, 7, 8, 9)
  missing_codes_triple <- c(997, 998, 999)
  
  for (code in missing_codes_single) {
    result <- calculate_time_quit_smoking(code, 1)
    expect_true(haven::is_tagged_na(result) || is.na(result),
               info = paste("Single-digit missing code", code, "should produce tagged_na"))
  }
  
  for (code in missing_codes_triple) {
    result <- calculate_time_quit_smoking(4, code)
    expect_true(haven::is_tagged_na(result) || is.na(result),
               info = paste("Triple-digit missing code", code, "should produce tagged_na"))
  }
})
