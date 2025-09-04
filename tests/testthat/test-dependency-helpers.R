test_that("parse_variable_start extracts DerivedVar dependencies", {
  # Test pattern: DerivedVar::[HWTGHTM, HWTGWTK]
  variable_start <- "DerivedVar::[HWTGHTM, HWTGWTK]"
  result <- parse_variable_start(variable_start, "cchs2015_2016_p")

  expect_equal(sort(result$immediate_dependencies), sort(c("HWTGHTM", "HWTGWTK")))
  expect_equal(length(result$raw_cchs_variables), 0) # No direct raw mappings
})

test_that("parse_variable_start extracts database mappings", {
  # Test pattern: cchs2015_2016_p::HWTDGHTM, cchs2017_2018_p::HWTDGHTM, [HWTGHTM]
  variable_start <- "cchs2015_2016_p::HWTDGHTM, cchs2017_2018_p::HWTDGHTM, [HWTGHTM]"
  result <- parse_variable_start(variable_start, "cchs2015_2016_p")

  expect_true("HWTDGHTM" %in% result$raw_cchs_variables)
  expect_true("HWTGHTM" %in% result$immediate_dependencies)
  expect_true("cchs2015_2016_p" %in% names(result$database_mappings$HWTDGHTM))
})

test_that("get_variable_dependencies returns BMI extraction list", {
  # Load actual variable_details
  load("data/variable_details.RData")

  # Test BMI basic case
  result <- get_variable_dependencies("HWTGBMI_der", "cchs2015_2016_p", variable_details, "extraction_list")
  expected <- c("HWTDGHTM", "HWTDGWTK")

  expect_equal(sort(result), sort(expected))
})

test_that("get_variable_dependencies returns BMI dependency chain", {
  # Load actual variable_details
  load("data/variable_details.RData")

  # Test BMI dependency chain
  result <- get_variable_dependencies("HWTGBMI_der", "cchs2015_2016_p", variable_details, "dependency_chain")

  expect_equal(result$target_variable, "HWTGBMI_der")
  expect_equal(result$database_name, "cchs2015_2016_p")
  expect_true("bmi_fun" %in% result$derived_functions)
  expect_true("HWTDGHTM" %in% result$raw_cchs_variables)
  expect_true("HWTDGWTK" %in% result$raw_cchs_variables)
})

test_that("get_variable_dependencies handles harmonized variables", {
  # Load actual variable_details
  load("data/variable_details.RData")

  # Test harmonized variable (copy operation)
  result <- get_variable_dependencies("HWTGHTM", "cchs2015_2016_p", variable_details, "extraction_list")

  expect_true("HWTDGHTM" %in% result)
})

test_that("get_variable_dependencies returns test data spec", {
  # Load actual variable_details
  load("data/variable_details.RData")

  # Test BMI test data spec
  result <- get_variable_dependencies("HWTGBMI_der", "cchs2015_2016_p", variable_details, "test_data_spec")

  expect_equal(result$target_variable, "HWTGBMI_der")
  expect_equal(result$database_name, "cchs2015_2016_p")
  expect_true("HWTDGHTM" %in% result$required_columns)
  expect_true("HWTDGWTK" %in% result$required_columns)
  expect_true("bmi_fun" %in% result$derived_functions)
})

test_that("get_variable_dependencies handles categorical derived variables", {
  # Load actual variable_details
  load("data/variable_details.RData")

  # Test BMI categorical (two-step derivation)
  result <- get_variable_dependencies("HWTGBMI_der_cat4", "cchs2015_2016_p", variable_details, "extraction_list")
  expected <- c("HWTDGHTM", "HWTDGWTK") # Should trace back to raw variables

  expect_equal(sort(result), sort(expected))
})

test_that("get_variable_dependencies handles missing variables gracefully", {
  # Load actual variable_details
  load("data/variable_details.RData")

  # Test non-existent variable
  expect_error(
    get_variable_dependencies("NONEXISTENT_VAR", "cchs2015_2016_p", variable_details),
    "Variable NONEXISTENT_VAR not found"
  )
})

test_that("YAML patterns work correctly", {
  # Test derived variable pattern
  derived_pattern <- "^DerivedVar::\\[([A-Z][A-Z0-9_]*(,\\s*[A-Z][A-Z0-9_]*)*)\\]$"
  expect_true(grepl(derived_pattern, "DerivedVar::[HWTGHTM, HWTGWTK]"))
  expect_false(grepl(derived_pattern, "cchs2015_2016_p::HWTDGHTM"))

  # Test database mapping pattern
  db_pattern <- "[a-zA-Z0-9_]+::[A-Z][A-Z0-9_]*"
  expect_true(grepl(db_pattern, "cchs2015_2016_p::HWTDGHTM"))
  expect_false(grepl(db_pattern, "DerivedVar::[HWTGHTM]"))
})

test_that("parse_variable_start handles complex mixed patterns", {
  # Real example from variable_details
  variable_start <- "cchs2005_p::HWTEGHTM, cchs2015_2016_p::HWTDGHTM, cchs2017_2018_p::HWTDGHTM, [HWTGHTM]"
  result <- parse_variable_start(variable_start, "cchs2015_2016_p")

  expect_true("HWTDGHTM" %in% result$raw_cchs_variables)
  expect_true("HWTGHTM" %in% result$immediate_dependencies)
  expect_true(length(result$database_mappings) > 0)
})

test_that("get_variable_dependencies handles processing_order format", {
  # Load actual variable_details
  load("data/variable_details.RData")

  # Test BMI processing order
  result <- get_variable_dependencies("HWTGBMI_der", "cchs2015_2016_p", variable_details, "processing_order")

  expect_type(result, "character")
  expect_true("HWTGBMI_der" %in% result)
  expect_true("HWTGHTM" %in% result)
  expect_true("HWTGWTK" %in% result)
  expect_true(length(result) >= 3) # Should include harmonized + derived variables
})
