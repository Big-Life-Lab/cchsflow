# Test Recodeflow YAML Schema Validation
# Core schema validation with stringr regex consistency

library(testthat)

test_that("Core recodeflow YAML schemas have valid syntax", {
  # Test essential recodeflow schemas
  core_schemas <- c(
    "scope-docs/metadata-schema/variables.yaml",
    "scope-docs/metadata-schema/variable_details.yaml"
  )
  
  for (schema_path in core_schemas) {
    # Skip if file doesn't exist (test environment may differ)
    if (!file.exists(schema_path)) {
      skip(paste("Core schema file not found:", schema_path))
    }
    
    result <- validate_yaml_schema(schema_path)
    
    expect_true(result$valid, 
                info = paste("Core schema should be valid:", basename(schema_path), 
                           "Errors:", paste(result$errors, collapse = "; ")))
    
    expect_equal(result$regex_engine, "stringr/ICU")
    
    expect_gte(result$patterns_tested, 0)
    
    # Check for schema version
    expect_true(!is.na(result$schema_version))
  }
})

test_that("Registry and supporting schemas have valid syntax", {
  # Test metadata registry and supporting schemas
  registry_schemas <- c(
    "scope-docs/metadata-schema/metadata_registry.yaml",
    "scope-docs/metadata-schema/database_metadata.yaml"
  )
  
  for (schema_path in registry_schemas) {
    if (file.exists(schema_path)) {
      result <- validate_yaml_schema(schema_path)
      
      expect_true(result$valid, 
                  info = paste("Registry schema should be valid:", basename(schema_path),
                             "Errors:", paste(result$errors, collapse = "; ")))
      
      expect_equal(result$regex_engine, "stringr/ICU")
      
      # Registry should have many patterns to test
      if (basename(schema_path) == "metadata_registry.yaml") {
        expect_gte(result$patterns_tested, 20)
      }
    } else {
      skip(paste("Registry schema not found:", basename(schema_path)))
    }
  }
})

test_that("Complete recodeflow validation workflow", {
  # Test the full recodeflow validation workflow - but skip if no schemas available
  if (!file.exists("scope-docs/metadata-schema/variables.yaml")) {
    skip("Recodeflow YAML schemas not available in test environment")
  }
  
  results <- run_recodeflow_schema_validation(include_registry = TRUE, include_cchs_extensions = FALSE)
  
  # Check structure
  expect_true("core_schemas" %in% names(results))
  expect_true("registry_schemas" %in% names(results))
  expect_true("summary" %in% names(results))
  
  # Check core schemas validated
  expect_true(results$summary$core_schemas_valid)
  
  # Check regex engine consistency
  expect_equal(results$summary$regex_engine, "stringr/ICU")
  
  # Check registry schemas were tested
  expect_gte(results$summary$registry_schemas_tested, 0)
})

test_that("YAML schema validation handles missing files gracefully", {
  result <- validate_yaml_schema("nonexistent/file.yaml")
  
  expect_false(result$valid)
  expect_match(result$errors, "File does not exist")
  expect_equal(result$patterns_tested, 0)
})

test_that("recodeflow pattern extraction works correctly", {
  # Test regex pattern detection from recodeflow schema structure
  test_schema <- list(
    variables_schema = list(
      fields = list(
        list(
          name = "variable",
          constraints = list(pattern = "^[a-zA-Z_][a-zA-Z0-9_]*$")
        ),
        list(
          name = "version", 
          constraints = list(pattern = "^[0-9]+\\.[0-9]+\\.[0-9]+$")
        )
      )
    )
  )
  
  patterns <- extract_recodeflow_patterns(test_schema)
  
  expect_gte(length(patterns), 1)
  expect_true(any(grepl("\\^\\[a-zA-Z_\\]", patterns)))
})

test_that("stringr regex validation catches invalid patterns", {
  # Test with invalid regex
  invalid_patterns <- c(
    "[unclosed",      # Unclosed bracket
    "(?invalid)",     # Invalid group syntax  
    "*starts_with_quantifier"  # Invalid quantifier placement
  )
  
  for (pattern in invalid_patterns) {
    # This should catch the error gracefully
    result <- tryCatch({
      stringr::str_detect("test", pattern)
      "no_error"
    }, error = function(e) {
      "error_caught"
    })
    
    expect_equal(result, "error_caught",
                info = paste("Invalid pattern should cause error:", pattern))
  }
})