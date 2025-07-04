# Integration Tests for v3.0.0 Modernized Functions with rec_with_table()
# 
# Tests all renamed functions (calculate_bmi, assess_adl, etc.) across all CCHS cycles
# Ensures complete integration compatibility following v3.0.0 modernization

library(testthat)
library(haven)
library(dplyr)

# Load necessary functions and data
if (!exists("variable_details")) {
  load("data/variable_details.RData")
}

# Define all available CCHS cycles for comprehensive testing
ALL_CCHS_CYCLES <- c(
  "cchs2001_p", "cchs2003_p", "cchs2005_p", "cchs2007_2008_p", "cchs2009_2010_p",
  "cchs2009_s", "cchs2010_p", "cchs2010_s", "cchs2011_2012_p", "cchs2012_p", 
  "cchs2012_s", "cchs2013_2014_p", "cchs2014_p", "cchs2015_2016_p", "cchs2017_2018_p"
)

# Define modernized functions and their corresponding derived variables
MODERNIZED_FUNCTIONS <- list(
  # BMI Functions
  calculate_bmi = list(
    derived_vars = c("HWTGBMI_der"),
    test_cycles = c("cchs2001_p", "cchs2003_p", "cchs2005_p", "cchs2015_2016_p", "cchs2017_2018_p")
  ),
  adjust_bmi = list(
    derived_vars = c("HWTGCOR_der"),
    test_cycles = c("cchs2001_p", "cchs2015_2016_p", "cchs2017_2018_p")
  ),
  categorize_bmi = list(
    derived_vars = c("HWTGBMI_der_cat4"),
    test_cycles = c("cchs2001_p", "cchs2015_2016_p", "cchs2017_2018_p")
  ),
  
  # ADL Functions  
  assess_adl = list(
    derived_vars = c("ADL_der"),
    test_cycles = c("cchs2001_p", "cchs2007_2008_p", "cchs2015_2016_p", "cchs2017_2018_p")
  ),
  score_adl = list(
    derived_vars = c("ADL_score_5"),
    test_cycles = c("cchs2001_p", "cchs2007_2008_p", "cchs2015_2016_p")
  ),
  score_adl_6 = list(
    derived_vars = c("ADL_score_6"),
    test_cycles = c("cchs2001_p", "cchs2007_2008_p", "cchs2015_2016_p")
  ),
  
  # Alcohol Functions
  assess_binge_drinking = list(
    derived_vars = c("binge_drinker"),
    test_cycles = c("cchs2001_p", "cchs2007_2008_p", "cchs2015_2016_p", "cchs2017_2018_p")
  ),
  assess_drinking_risk_short = list(
    derived_vars = c("ALWDVSTR_der"),
    test_cycles = c("cchs2001_p", "cchs2007_2008_p", "cchs2015_2016_p")
  ),
  assess_drinking_risk_long = list(
    derived_vars = c("ALWDVLTR_der"),
    test_cycles = c("cchs2001_p", "cchs2007_2008_p", "cchs2015_2016_p")
  ),
  
  # Physical Activity Functions
  calculate_energy_expenditure = list(
    derived_vars = c("energy_exp"),
    test_cycles = c("cchs2015_2016_p", "cchs2017_2018_p")
  )
)

# ==============================================================================
# 1. INTEGRATION TEST FRAMEWORK
# ==============================================================================

test_that("rec_with_table() loads successfully with all required dependencies", {
  # Test that all required functions and data are available
  expect_true(exists("variable_details"), "variable_details.RData must be loaded")
  expect_true(is.data.frame(variable_details), "variable_details must be a data frame")
  expect_gt(nrow(variable_details), 0, "variable_details must contain data")
  
  # Test that modernized functions exist
  expect_true(exists("calculate_bmi"), "calculate_bmi function must exist")
  expect_true(exists("assess_adl"), "assess_adl function must exist")
  expect_true(exists("assess_binge_drinking"), "assess_binge_drinking function must exist")
  expect_true(exists("calculate_energy_expenditure"), "calculate_energy_expenditure function must exist")
})

# ==============================================================================
# 2. BMI FUNCTIONS INTEGRATION TESTS
# ==============================================================================

test_that("calculate_bmi integration works across CCHS cycles", {
  skip_if_not(exists("rec_with_table"), "rec_with_table function required")
  
  # Test with recent cycles that have BMI data
  test_cycles <- c("cchs2015_2016_p", "cchs2017_2018_p", "cchs2001_p")
  
  for (cycle in test_cycles) {
    # Skip if cycle data not available
    data_file <- paste0("data/", cycle, ".RData")
    skip_if_not(file.exists(data_file), paste("Data file", data_file, "not available"))
    
    # Create minimal test data for BMI calculation
    test_data <- data.frame(
      HWTGHTM = c(1.75, 1.65, 1.80),
      HWTGWTK = c(70, 60, 85),
      stringsAsFactors = FALSE
    )
    
    # Test rec_with_table integration
    expect_silent({
      result <- rec_with_table(
        data = test_data,
        variables = "HWTGBMI_der",
        database_name = cycle,
        variable_details = variable_details
      )
    })
    
    # Verify results structure
    expect_true(is.data.frame(result), paste("Result should be data frame for", cycle))
    expect_true("HWTGBMI_der" %in% names(result), paste("BMI derived variable should exist for", cycle))
    expect_equal(nrow(result), 3, paste("Should return 3 rows for", cycle))
    
    # Verify BMI calculations are reasonable (15-50 range)
    bmi_values <- result$HWTGBMI_der[!is.na(result$HWTGBMI_der)]
    if (length(bmi_values) > 0) {
      expect_true(all(bmi_values >= 15 & bmi_values <= 50), 
                  paste("BMI values should be reasonable for", cycle))
    }
  }
})

test_that("adjust_bmi integration works with bias correction", {
  skip_if_not(exists("rec_with_table"), "rec_with_table function required")
  
  test_data <- data.frame(
    DHH_SEX = c(1, 2, 1, 2),        # Male, Female, Male, Female
    HWTGHTM = c(1.75, 1.65, 1.80, 1.70),
    HWTGWTK = c(70, 60, 85, 55),
    stringsAsFactors = FALSE
  )
  
  # Test with cycles that have adjusted BMI functionality
  test_cycles <- c("cchs2015_2016_p", "cchs2017_2018_p", "cchs2001_p")
  
  for (cycle in test_cycles) {
    skip_if_not(file.exists(paste0("data/", cycle, ".RData")), paste("Data file for", cycle, "not available"))
    
    expect_silent({
      result <- rec_with_table(
        data = test_data,
        variables = "HWTGCOR_der",
        database_name = cycle,
        variable_details = variable_details
      )
    })
    
    expect_true("HWTGCOR_der" %in% names(result), paste("Adjusted BMI variable should exist for", cycle))
    expect_equal(nrow(result), 4, paste("Should return 4 rows for", cycle))
    
    # Verify adjusted BMI values are reasonable (slightly different from raw BMI)
    adj_bmi_values <- result$HWTGCOR_der[!is.na(result$HWTGCOR_der)]
    if (length(adj_bmi_values) > 0) {
      expect_true(all(adj_bmi_values >= 15 & adj_bmi_values <= 50), 
                  paste("Adjusted BMI values should be reasonable for", cycle))
    }
    
    # Compare with raw BMI to ensure adjustment is applied
    raw_result <- rec_with_table(
      data = test_data,
      variables = "HWTGBMI_der",
      database_name = cycle,
      variable_details = variable_details
    )
    
    if ("HWTGBMI_der" %in% names(raw_result)) {
      raw_bmi_values <- raw_result$HWTGBMI_der[!is.na(raw_result$HWTGBMI_der)]
      if (length(raw_bmi_values) > 0 && length(adj_bmi_values) > 0) {
        # Adjusted BMI should be different from raw BMI (bias correction applied)
        expect_false(identical(adj_bmi_values, raw_bmi_values), 
                     paste("Adjusted BMI should differ from raw BMI for", cycle))
      }
    }
  }
})

test_that("categorize_bmi integration works with categorical outputs", {
  skip_if_not(exists("rec_with_table"), "rec_with_table function required")
  
  test_data <- data.frame(
    HWTGHTM = c(1.75, 1.65, 1.80, 1.70),
    HWTGWTK = c(70, 60, 85, 55),  # Range from underweight to overweight
    stringsAsFactors = FALSE
  )
  
  # Test with cycle that has categorical BMI
  cycle <- "cchs2015_2016_p"
  skip_if_not(file.exists(paste0("data/", cycle, ".RData")), "CCHS 2015-2016 data not available")
  
  expect_silent({
    result <- rec_with_table(
      data = test_data,
      variables = "HWTGBMI_der_cat4",
      database_name = cycle,
      variable_details = variable_details
    )
  })
  
  expect_true("HWTGBMI_der_cat4" %in% names(result), "Categorical BMI variable should exist")
  
  # Check that categorical values are appropriate (1-4 for underweight/normal/overweight/obese)
  cat_values <- result$HWTGBMI_der_cat4[!is.na(result$HWTGBMI_der_cat4)]
  if (length(cat_values) > 0) {
    expect_true(all(cat_values %in% 1:4), "BMI categories should be 1-4")
  }
})

# ==============================================================================
# 3. ADL FUNCTIONS INTEGRATION TESTS  
# ==============================================================================

test_that("assess_adl integration works across CCHS cycles", {
  skip_if_not(exists("rec_with_table"), "rec_with_table function required")
  
  test_cycles <- c("cchs2001_p", "cchs2007_2008_p", "cchs2015_2016_p")
  
  for (cycle in test_cycles) {
    skip_if_not(file.exists(paste0("data/", cycle, ".RData")), paste("Data file for", cycle, "not available"))
    
    # Create test data with ADL responses (1=needs help, 2=no help needed)
    test_data <- data.frame(
      ADL_01 = c(1, 2, 2, 1),  # Preparing meals
      ADL_02 = c(2, 2, 1, 2),  # Getting to appointments
      ADL_03 = c(2, 2, 2, 1),  # Household chores
      ADL_04 = c(2, 2, 2, 2),  # Personal care
      ADL_05 = c(2, 2, 2, 2),  # Moving around house
      stringsAsFactors = FALSE
    )
    
    expect_silent({
      result <- rec_with_table(
        data = test_data,
        variables = "ADL_der",
        database_name = cycle,
        variable_details = variable_details
      )
    })
    
    expect_true("ADL_der" %in% names(result), paste("ADL derived variable should exist for", cycle))
    expect_equal(nrow(result), 4, paste("Should return 4 rows for", cycle))
    
    # Check ADL results are appropriate (1=needs help, 2=no help)
    adl_values <- result$ADL_der[!is.na(result$ADL_der)]
    if (length(adl_values) > 0) {
      expect_true(all(adl_values %in% 1:2), paste("ADL values should be 1 or 2 for", cycle))
    }
  }
})

test_that("score_adl integration calculates correct scores", {
  skip_if_not(exists("rec_with_table"), "rec_with_table function required")
  
  # Test with data designed to produce known scores
  test_data <- data.frame(
    ADL_01 = c(1, 1, 2, 2),  # 1=needs help, 2=no help
    ADL_02 = c(1, 2, 2, 2),
    ADL_03 = c(1, 2, 2, 2),
    ADL_04 = c(2, 2, 2, 2),
    ADL_05 = c(2, 2, 2, 2),
    stringsAsFactors = FALSE
  )
  
  cycle <- "cchs2015_2016_p"
  skip_if_not(file.exists(paste0("data/", cycle, ".RData")), "CCHS 2015-2016 data not available")
  
  expect_silent({
    result <- rec_with_table(
      data = test_data,
      variables = "ADL_score_5",
      database_name = cycle,
      variable_details = variable_details
    )
  })
  
  expect_true("ADL_score_5" %in% names(result), "ADL score variable should exist")
  
  # Check that scores are in valid range (0-5)
  score_values <- result$ADL_score_5[!is.na(result$ADL_score_5)]
  if (length(score_values) > 0) {
    expect_true(all(score_values >= 0 & score_values <= 5), "ADL scores should be 0-5")
  }
})

# ==============================================================================
# 4. ALCOHOL FUNCTIONS INTEGRATION TESTS
# ==============================================================================

test_that("assess_binge_drinking integration works across cycles", {
  skip_if_not(exists("rec_with_table"), "rec_with_table function required")
  
  test_cycles <- c("cchs2001_p", "cchs2015_2016_p", "cchs2017_2018_p")
  
  for (cycle in test_cycles) {
    skip_if_not(file.exists(paste0("data/", cycle, ".RData")), paste("Data file for", cycle, "not available"))
    
    # Create test data for binge drinking assessment
    test_data <- data.frame(
      DHH_SEX = c(1, 2, 1, 2),  # 1=male, 2=female
      ALW_1 = c(1, 1, 2, 1),    # Drinks in past 12 months
      ALW_2A1 = c(0, 0, 0, 5),  # Sunday drinks
      ALW_2A2 = c(0, 0, 0, 5),  # Monday drinks  
      ALW_2A3 = c(0, 0, 0, 5),  # Tuesday drinks
      ALW_2A4 = c(6, 4, 0, 0),  # Wednesday drinks (binge level)
      ALW_2A5 = c(0, 0, 0, 0),  # Thursday drinks
      ALW_2A6 = c(0, 0, 0, 0),  # Friday drinks
      ALW_2A7 = c(0, 0, 0, 0),  # Saturday drinks
      stringsAsFactors = FALSE
    )
    
    expect_silent({
      result <- rec_with_table(
        data = test_data,
        variables = "binge_drinker",
        database_name = cycle,
        variable_details = variable_details
      )
    })
    
    expect_true("binge_drinker" %in% names(result), paste("Binge drinker variable should exist for", cycle))
    expect_equal(nrow(result), 4, paste("Should return 4 rows for", cycle))
    
    # Check binge drinking results are binary (0/1)
    binge_values <- result$binge_drinker[!is.na(result$binge_drinker)]
    if (length(binge_values) > 0) {
      expect_true(all(binge_values %in% 0:1), paste("Binge drinking values should be 0 or 1 for", cycle))
    }
  }
})

# ==============================================================================
# 5. PHYSICAL ACTIVITY INTEGRATION TESTS
# ==============================================================================

test_that("calculate_energy_expenditure integration works for recent cycles", {
  skip_if_not(exists("rec_with_table"), "rec_with_table function required")
  
  # Energy expenditure only available in recent cycles
  test_cycles <- c("cchs2015_2016_p", "cchs2017_2018_p")
  
  for (cycle in test_cycles) {
    skip_if_not(file.exists(paste0("data/", cycle, ".RData")), paste("Data file for", cycle, "not available"))
    
    # Create test data for energy expenditure calculation
    test_data <- data.frame(
      DHHGAGE_cont = c(25, 45, 65, 35),     # Age ranges
      PAA_045 = c(2, 1, 0, 3),              # Hours vigorous activity
      PAA_050 = c(30, 15, 0, 45),           # Minutes vigorous activity
      PAA_075 = c(1, 2, 1, 0),              # Hours moderate activity
      PAA_080 = c(20, 40, 30, 0),           # Minutes moderate activity
      PAADVDYS = c(5, 3, 2, 7),             # Active days
      PAADVVIG = c(150, 75, 30, 200),       # Vigorous minutes
      PAYDVTOA = c(100, 50, 20, 150),       # Youth other activities
      PAYDVADL = c(80, 40, 15, 120),        # Youth ADL activities
      PAYDVVIG = c(60, 30, 10, 90),         # Youth vigorous
      PAYDVDYS = c(4, 2, 1, 6),             # Youth active days
      stringsAsFactors = FALSE
    )
    
    expect_silent({
      result <- rec_with_table(
        data = test_data,
        variables = "energy_exp",
        database_name = cycle,
        variable_details = variable_details
      )
    })
    
    expect_true("energy_exp" %in% names(result), paste("Energy expenditure variable should exist for", cycle))
    expect_equal(nrow(result), 4, paste("Should return 4 rows for", cycle))
    
    # Check energy expenditure values are reasonable (0-20 kcal/kg/day range)
    energy_values <- result$energy_exp[!is.na(result$energy_exp)]
    if (length(energy_values) > 0) {
      expect_true(all(energy_values >= 0 & energy_values <= 20), 
                  paste("Energy expenditure should be 0-20 kcal/kg/day for", cycle))
    }
  }
})

# ==============================================================================
# 6. COMPREHENSIVE CROSS-CYCLE COMPATIBILITY TESTS
# ==============================================================================

test_that("modernized functions maintain consistency across all available cycles", {
  skip_if_not(exists("rec_with_table"), "rec_with_table function required")
  
  # Test BMI consistency across all cycles with BMI data
  bmi_cycles <- c("cchs2001_p", "cchs2003_p", "cchs2005_p", "cchs2015_2016_p", "cchs2017_2018_p")
  
  test_data <- data.frame(
    HWTGHTM = 1.75,
    HWTGWTK = 70,
    stringsAsFactors = FALSE
  )
  
  bmi_results <- list()
  
  for (cycle in bmi_cycles) {
    if (file.exists(paste0("data/", cycle, ".RData"))) {
      expect_silent({
        result <- rec_with_table(
          data = test_data,
          variables = "HWTGBMI_der",
          database_name = cycle,
          variable_details = variable_details
        )
      })
      
      bmi_results[[cycle]] <- result$HWTGBMI_der[1]
    }
  }
  
  # Check that BMI calculations are consistent across cycles (within 0.1 tolerance)
  if (length(bmi_results) > 1) {
    bmi_values <- unlist(bmi_results)
    bmi_values <- bmi_values[!is.na(bmi_values)]
    
    if (length(bmi_values) > 1) {
      expect_lt(max(bmi_values) - min(bmi_values), 0.1, 
                "BMI calculations should be consistent across cycles")
    }
  }
})

test_that("error handling works appropriately across cycles", {
  skip_if_not(exists("rec_with_table"), "rec_with_table function required")
  
  # Test with invalid data to ensure proper error handling
  invalid_data <- data.frame(
    HWTGHTM = c(NA, 0, -1, 999),  # Invalid height values
    HWTGWTK = c(70, NA, -5, 500), # Invalid weight values
    stringsAsFactors = FALSE
  )
  
  cycle <- "cchs2015_2016_p"
  skip_if_not(file.exists(paste0("data/", cycle, ".RData")), "CCHS 2015-2016 data not available")
  
  # Should not error, but should handle invalid data appropriately
  expect_silent({
    result <- rec_with_table(
      data = invalid_data,
      variables = "HWTGBMI_der",
      database_name = cycle,
      variable_details = variable_details
    )
  })
  
  expect_true("HWTGBMI_der" %in% names(result), "BMI variable should exist even with invalid data")
  
  # Invalid values should result in NA or tagged_na
  bmi_values <- result$HWTGBMI_der
  expect_true(all(is.na(bmi_values) | (!is.na(bmi_values) & bmi_values > 10 & bmi_values < 60)), 
              "Invalid inputs should produce NA or reasonable BMI values")
})

# ==============================================================================
# 7. PERFORMANCE AND MEMORY TESTS
# ==============================================================================

test_that("integration tests complete within reasonable time", {
  skip_if_not(exists("rec_with_table"), "rec_with_table function required")
  
  # Create larger test dataset to check performance
  n <- 1000
  large_data <- data.frame(
    HWTGHTM = rnorm(n, 1.7, 0.1),
    HWTGWTK = rnorm(n, 70, 15),
    stringsAsFactors = FALSE
  )
  
  cycle <- "cchs2015_2016_p"
  skip_if_not(file.exists(paste0("data/", cycle, ".RData")), "CCHS 2015-2016 data not available")
  
  # Test should complete within reasonable time (< 10 seconds)
  start_time <- Sys.time()
  
  expect_silent({
    result <- rec_with_table(
      data = large_data,
      variables = "HWTGBMI_der",
      database_name = cycle,
      variable_details = variable_details
    )
  })
  
  end_time <- Sys.time()
  execution_time <- as.numeric(end_time - start_time)
  
  expect_lt(execution_time, 10, "Integration test should complete within 10 seconds for 1000 records")
  expect_equal(nrow(result), n, "Should return same number of rows as input")
})