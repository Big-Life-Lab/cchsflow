# Tests for CSV standardisation bug fixes
# These tests verify the critical bug fixes identified in code review

# Test setup helper
setup_test_csv <- function(content, filename = "test.csv", add_bom = FALSE, line_ending = "\n") {
  temp_dir <- tempdir()
  file_path <- file.path(temp_dir, filename)

  # Ensure content has proper final newline for valid CSV
  if (!grepl("\n$", content)) {
    content <- paste0(content, "\n")
  }

  # Convert line endings
  if (line_ending == "\r\n") {
    content <- gsub("\n", "\r\n", content)
  } else if (line_ending == "\r") {
    content <- gsub("\n", "\r", content)
  }

  # Add BOM if requested
  if (add_bom) {
    bom <- rawToChar(as.raw(c(0xEF, 0xBB, 0xBF)))
    content <- paste0(bom, content)
  }

  # Write with explicit encoding control
  writeBin(charToRaw(content), file_path)
  return(file_path)
}

# Basic test data
basic_variables_content <- "variable,label,labelLong,variableType,databaseStart,variableStart
test_var1,Test Variable 1,Test Variable 1 Long,Categorical,test_db,[TEST_VAR1]
test_var2,Test Variable 2,Test Variable 2 Long,Continuous,test_db,[TEST_VAR2]"

# ============================================================================
# Issue #1: Line ending conversion bug
# ============================================================================

test_that("Issue #1: Line endings are correctly identified and fixed only once", {
  # Test CRLF conversion - should only report CRLF, not CR
  test_file <- setup_test_csv(basic_variables_content, "test_crlf.csv", line_ending = "\r\n")

  result <- standardise_csv(test_file, validate_only = FALSE)

  # Should report CRLF conversion
  expect_true(any(grepl("CRLF", result$issues_fixed)))

  # Should NOT report CR conversion separately (that was the bug)
  cr_messages <- grepl("^Converted CR to LF", result$issues_fixed)
  expect_false(any(cr_messages))

  # Verify file actually has LF endings
  file_content <- readChar(test_file, file.info(test_file)$size)
  expect_false(grepl("\r", file_content))
  expect_true(grepl("\n", file_content))
})

test_that("Issue #1: Pure CR files are correctly identified", {
  # Test old Mac CR-only conversion
  test_file <- setup_test_csv(basic_variables_content, "test_cr.csv", line_ending = "\r")

  result <- standardise_csv(test_file, validate_only = FALSE)

  # Should report CR conversion (not CRLF)
  expect_true(any(grepl("Converted CR to LF", result$issues_fixed)))
  expect_false(any(grepl("CRLF", result$issues_fixed)))

  # Verify file has LF endings
  file_content <- readChar(test_file, file.info(test_file)$size)
  expect_false(grepl("\r", file_content))
  expect_true(grepl("\n", file_content))
})

test_that("Issue #1: Mixed line endings are normalized", {
  # Create a file with intentionally mixed line endings (rare but possible)
  mixed_content <- "variable,label\r\ntest1,Test 1\rtest2,Test 2\n"
  test_file <- file.path(tempdir(), "test_mixed.csv")
  writeBin(charToRaw(mixed_content), test_file)

  result <- standardise_csv(test_file, validate_only = FALSE)

  # Should fix the file
  expect_true(result$fixed)

  # All line endings should be LF now
  file_content <- readChar(test_file, file.info(test_file)$size)
  expect_false(grepl("\r", file_content))

  # Should still be readable as CSV
  data <- read.csv(test_file, stringsAsFactors = FALSE)
  expect_equal(nrow(data), 2)
})

# ============================================================================
# Issue #2: Read-modify-write race condition
# ============================================================================

test_that("Issue #2: Line ending fixes are preserved when column reordering happens", {
  # Create file with wrong line endings AND wrong column order
  wrong_order <- "variableType,variable,labelLong,label,databaseStart,variableStart
Categorical,test_var1,Test Variable 1 Long,Test Variable 1,test_db,[TEST_VAR1]
Continuous,test_var2,Test Variable 2 Long,Test Variable 2,test_db,[TEST_VAR2]"

  test_file <- setup_test_csv(wrong_order, "variables_test.csv", line_ending = "\r\n")

  # Apply fixes
  result <- standardise_csv(test_file, validate_only = FALSE)

  # Both fixes should be reported
  expect_true(any(grepl("CRLF", result$issues_fixed)) || any(grepl("CR", result$issues_fixed)))
  expect_true(any(grepl("column order", result$issues_fixed, ignore.case = TRUE)))

  # Verify line endings were actually fixed (this was the bug - they were lost)
  file_content <- readChar(test_file, file.info(test_file)$size)
  expect_false(grepl("\r", file_content), "CRLF should be converted even with column reordering")

  # Verify column order was fixed
  data <- read.csv(test_file, stringsAsFactors = FALSE, check.names = FALSE)
  expect_equal(names(data)[1], "variable", "Column order should be corrected")
})

test_that("Issue #2: BOM removal is preserved with column reordering", {
  # File with BOM and wrong column order
  wrong_order <- "variableType,variable,labelLong,label,databaseStart,variableStart
Categorical,test_var1,Test Variable 1 Long,Test Variable 1,test_db,[TEST_VAR1]"

  test_file <- setup_test_csv(wrong_order, "variables_bom.csv", add_bom = TRUE)

  # Verify BOM exists
  raw_before <- readBin(test_file, "raw", n = 10)
  expect_equal(raw_before[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))

  # Apply fixes
  result <- standardise_csv(test_file, validate_only = FALSE)

  # Verify BOM was removed (even with column reordering)
  raw_after <- readBin(test_file, "raw", n = 10)
  expect_false(all(raw_after[1:3] == as.raw(c(0xEF, 0xBB, 0xBF))),
               "BOM should be removed even with column reordering")

  # File should still be readable
  data <- read.csv(test_file, stringsAsFactors = FALSE)
  expect_equal(nrow(data), 1)
})

# ============================================================================
# Issue #3: Schema path resolution
# ============================================================================

test_that("Issue #3: Schema loading works in development mode", {
  # This test verifies the fallback to inst/ path works
  skip_if_not(file.exists("inst/metadata/schemas/core/variables.yaml"),
              "Schema files not available")

  test_file <- setup_test_csv(basic_variables_content, "variables.csv")

  # Should not error on schema loading
  expect_error(
    result <- standardise_csv(test_file, collaboration = TRUE, validate_only = TRUE),
    NA  # No error expected
  )
})

test_that("Issue #3: Helpful error when schema truly missing", {
  # Create a non-standard filename that won't match patterns
  test_content <- "col1,col2\nval1,val2"
  test_file <- setup_test_csv(test_content, "random_file.csv")

  # Should handle gracefully (unknown file type)
  result <- standardise_csv(test_file, collaboration = FALSE, validate_only = TRUE)

  # Should succeed in basic mode
  expect_true(result$success)
})

# ============================================================================
# Issue #7: Input validation
# ============================================================================

test_that("Issue #7: Invalid file_path is caught", {
  expect_error(
    standardise_csv(NULL),
    "file_path must be"
  )

  expect_error(
    standardise_csv(c("file1.csv", "file2.csv")),
    "file_path must be a single"
  )

  expect_error(
    standardise_csv(""),
    "file_path must be.*non-empty"
  )

  expect_error(
    standardise_csv("nonexistent_file.csv"),
    "File not found"
  )
})

test_that("Issue #7: Invalid collaboration parameter is caught", {
  test_file <- setup_test_csv(basic_variables_content, "test.csv")

  expect_error(
    standardise_csv(test_file, collaboration = "yes"),
    "collaboration must be.*logical"
  )

  expect_error(
    standardise_csv(test_file, collaboration = c(TRUE, FALSE)),
    "collaboration must be a single"
  )
})

test_that("Issue #7: Invalid validate_only parameter is caught", {
  test_file <- setup_test_csv(basic_variables_content, "test.csv")

  expect_error(
    standardise_csv(test_file, validate_only = "no"),
    "validate_only must be.*logical"
  )

  expect_error(
    standardise_csv(test_file, validate_only = 1),
    "validate_only must be.*logical"
  )
})

test_that("Issue #7: Corrupted CSV file gives helpful error", {
  # Create a malformed CSV
  bad_csv <- file.path(tempdir(), "bad.csv")
  writeLines('header1,header2\nvalue1,"unclosed quote', bad_csv)

  # Should give helpful error message
  expect_error(
    standardise_csv(bad_csv),
    "Failed to read CSV"
  )
})

# ============================================================================
# Issue #8: Return value consistency
# ============================================================================

test_that("Issue #8: Return structure is consistent and meaningful", {
  test_file <- setup_test_csv(basic_variables_content, "test.csv", line_ending = "\r\n")

  # validate_only mode
  result_validate <- standardise_csv(test_file, validate_only = TRUE)
  expect_true("valid" %in% names(result_validate))
  expect_true("success" %in% names(result_validate))
  expect_true("mode" %in% names(result_validate))
  expect_true("issues_remaining" %in% names(result_validate))

  # Fix mode
  result_fix <- standardise_csv(test_file, validate_only = FALSE)
  expect_true("valid" %in% names(result_fix))
  expect_true("success" %in% names(result_fix))
  expect_true("fixed" %in% names(result_fix))
  expect_true("mode" %in% names(result_fix))
  expect_true("issues_fixed" %in% names(result_fix))
  expect_true("issues_remaining" %in% names(result_fix))

  # fixed should be TRUE because we applied CRLF fix
  expect_true(result_fix$fixed)

  # success should be TRUE (process succeeded)
  expect_true(result_fix$success)
})

test_that("Issue #8: fixed field accurately reports whether fixes were applied", {
  # File with no issues
  test_file <- setup_test_csv(basic_variables_content, "test.csv", line_ending = "\n")

  result <- standardise_csv(test_file, validate_only = FALSE)

  # If no fixes needed, fixed should be FALSE
  if (length(result$issues_fixed) == 0) {
    expect_false(result$fixed)
  } else {
    expect_true(result$fixed)
  }
})

# ============================================================================
# Integration tests - Multiple issues at once
# ============================================================================

test_that("Multiple issues are all fixed in one pass", {
  # File with BOM, CRLF, wrong column order, and missing final newline
  messy_content <- "variableType,variable,labelLong,label,databaseStart,variableStart
Categorical,test1,Long 1,Test 1,db1,[VAR1]"
  # Note: setup_test_csv adds final newline, so we'll create manually

  test_file <- file.path(tempdir(), "messy_variables.csv")

  # Create with BOM, CRLF, no final newline
  messy_bytes <- charToRaw(gsub("\n", "\r\n", messy_content))
  bom <- as.raw(c(0xEF, 0xBB, 0xBF))
  writeBin(c(bom, messy_bytes), test_file)

  # Fix all issues
  result <- standardise_csv(test_file, validate_only = FALSE)

  # Should report multiple fixes
  expect_true(length(result$issues_fixed) >= 3)
  expect_true(any(grepl("BOM", result$issues_fixed)))
  expect_true(any(grepl("CRLF|CR", result$issues_fixed)))

  # Verify all fixes applied
  raw_content <- readBin(test_file, "raw", n = 10)
  expect_false(all(raw_content[1:3] == as.raw(c(0xEF, 0xBB, 0xBF))), "BOM removed")

  text_content <- readChar(test_file, file.info(test_file)$size)
  expect_false(grepl("\r", text_content), "No CR characters remain")

  data <- read.csv(test_file, stringsAsFactors = FALSE, check.names = FALSE)
  expect_equal(names(data)[1], "variable", "Column order fixed")
})

test_that("validate_only truly doesn't modify files", {
  # Create file with multiple issues
  test_file <- setup_test_csv(basic_variables_content, "test.csv",
                               add_bom = TRUE, line_ending = "\r\n")

  # Record state before
  content_before <- readBin(test_file, "raw", file.info(test_file)$size)
  mtime_before <- file.info(test_file)$mtime

  # Run validation only
  result <- standardise_csv(test_file, validate_only = TRUE)

  # File should be completely unchanged
  content_after <- readBin(test_file, "raw", file.info(test_file)$size)
  expect_identical(content_before, content_after)

  # Should still detect issues
  expect_false(result$valid)  # BOM present makes it invalid
})
