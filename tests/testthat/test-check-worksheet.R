test_that("check_worksheet returns empty list for valid CSV", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  df <- data.frame(
    variable = c("A", "B"),
    label = c("Label A", "Label B"),
    labelLong = c("Long A", "Long B"),
    section = c("s1", "s1"),
    subject = c("Sub", "Sub"),
    variableType = c("cont", "cat"),
    units = c("years", ""),
    databaseStart = c("cchs2001_p", "cchs2001_p"),
    variableStart = c("VAR_A", "VAR_B"),
    description = c("desc A", "desc B"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(df, tmp, na = "", quote = "needed", eol = "\n")
  errors <- check_worksheet(tmp, "variables")
  expect_equal(length(errors), 0)
})

test_that("check_worksheet returns file_not_found for missing file", {
  errors <- check_worksheet("/nonexistent/path.csv", "variables")
  expect_equal(length(errors), 1)
  expect_equal(errors[[1]]$error_type, "file_not_found")
})

test_that("check_worksheet detects CRLF line endings", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  # Write CRLF directly via writeBin to ensure actual \r\n in file
  lines <- paste0(
    "variable,label,labelLong,section,subject,variableType,units,",
    "databaseStart,variableStart,description\r\n",
    "A,L,LL,s,S,cont,y,cchs2001_p,V,d\r\n"
  )
  writeBin(charToRaw(lines), tmp)
  # readr::read_lines strips \r on some platforms (macOS); skip if so
  raw <- readr::read_lines(tmp)
  skip_if(!any(grepl("\r$", raw)), "Platform strips CR from read_lines output")
  errors <- check_worksheet(tmp, "variables")
  crlf_errors <- Filter(function(e) e$error_type == "line_ending_crlf", errors)
  expect_true(length(crlf_errors) > 0)
})

test_that("check_worksheet detects wrong column order", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  # Swap label and labelLong columns
  df <- data.frame(
    variable = "A", labelLong = "LL", label = "L", section = "s",
    subject = "S", variableType = "cont", units = "y",
    databaseStart = "cchs2001_p", variableStart = "V", description = "d",
    stringsAsFactors = FALSE
  )
  readr::write_csv(df, tmp, na = "", quote = "needed", eol = "\n")
  errors <- check_worksheet(tmp, "variables")
  col_errors <- Filter(function(e) e$error_type == "column_order", errors)
  expect_true(length(col_errors) > 0)
})

test_that("check_worksheet detects unsorted rows", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  df <- data.frame(
    variable = c("B", "A"),
    label = c("L", "L"),
    labelLong = c("LL", "LL"),
    section = c("s", "s"),
    subject = c("S", "S"),
    variableType = c("cont", "cont"),
    units = c("", ""),
    databaseStart = c("cchs2001_p", "cchs2001_p"),
    variableStart = c("V", "V"),
    description = c("d", "d"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(df, tmp, na = "", quote = "needed", eol = "\n")
  errors <- check_worksheet(tmp, "variables")
  sort_errors <- Filter(function(e) e$error_type == "unsorted_rows", errors)
  expect_equal(length(sort_errors), 1)
})

test_that("check_worksheet detects trailing empty columns", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  # Write a CSV with an extra empty column
  lines <- c(
    "variable,label,labelLong,section,subject,variableType,units,databaseStart,variableStart,description,",
    "A,L,LL,s,S,cont,y,cchs2001_p,V,d,"
  )
  writeLines(lines, tmp, sep = "\n")
  errors <- check_worksheet(tmp, "variables")
  empty_errors <- Filter(function(e) e$error_type == "empty_columns", errors)
  expect_true(length(empty_errors) > 0)
})

test_that("check_worksheet detects extra non-empty columns", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  lines <- c(
    "variable,label,labelLong,section,subject,variableType,units,databaseStart,variableStart,description,extra_col",
    "A,L,LL,s,S,cont,y,cchs2001_p,V,d,extra_value"
  )
  writeLines(lines, tmp, sep = "\n")
  errors <- check_worksheet(tmp, "variables")
  extra_errors <- Filter(function(e) e$error_type == "extra_column", errors)
  expect_equal(length(extra_errors), 1)
  expect_equal(extra_errors[[1]]$column_name, "extra_col")
})

test_that("check_worksheet detects excessive quoting", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  # Manually write with unnecessary quotes
  lines <- c(
    "variable,label,labelLong,section,subject,variableType,units,databaseStart,variableStart,description",
    '"A","L","LL","s","S","cont","y","cchs2001_p","V","d"'
  )
  writeLines(lines, tmp, sep = "\n")
  errors <- check_worksheet(tmp, "variables")
  quote_errors <- Filter(function(e) e$error_type == "excessive_quoting", errors)
  expect_true(length(quote_errors) > 0)
})

test_that("check_worksheet skips row sorting for single-row CSV", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  df <- data.frame(
    variable = "A", label = "L", labelLong = "LL", section = "s",
    subject = "S", variableType = "cont", units = "y",
    databaseStart = "cchs2001_p", variableStart = "V", description = "d",
    stringsAsFactors = FALSE
  )
  readr::write_csv(df, tmp, na = "", quote = "needed", eol = "\n")
  errors <- check_worksheet(tmp, "variables")
  sort_errors <- Filter(function(e) e$error_type == "unsorted_rows", errors)
  expect_equal(length(sort_errors), 0)
})

# --- check_recode_blocks ---

test_that("check_recode_blocks returns file_not_found for missing file", {
  errors <- check_recode_blocks("/nonexistent/path.csv")
  expect_equal(length(errors), 1)
  expect_equal(errors[[1]]$error_type, "file_not_found")
})

test_that("check_recode_blocks reports error for invalid CSV", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines("not,valid\n\"unclosed", tmp)
  errors <- check_recode_blocks(tmp)
  expect_true(length(errors) > 0)
  expect_true(errors[[1]]$error_type %in% c("invalid_csv", "missing_required_columns"))
})

test_that("check_recode_blocks reports missing required columns", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  df <- data.frame(variable = "A", other = "val", stringsAsFactors = FALSE)
  readr::write_csv(df, tmp, na = "", quote = "needed", eol = "\n")
  errors <- check_recode_blocks(tmp)
  expect_equal(length(errors), 1)
  expect_equal(errors[[1]]$error_type, "missing_required_columns")
})

test_that("check_recode_blocks finds no collisions for single-block variable", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  df <- data.frame(
    variable = c("A", "A"),
    variableStart = c("[A]", "[A]"),
    databaseStart = c("cchs2001_p", "cchs2001_p"),
    recStart = c("1", "2"),
    recEnd = c("1", "2"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(df, tmp, na = "", quote = "needed", eol = "\n")
  errors <- check_recode_blocks(tmp)
  expect_equal(length(errors), 0)
})

test_that("check_recode_blocks detects recStart collision across blocks", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  df <- data.frame(
    variable = c("A", "A", "A", "A"),
    variableStart = c("[A]", "[A]", "[B]", "[B]"),
    databaseStart = c("cchs2001_p", "cchs2001_p", "cchs2001_p", "cchs2001_p"),
    recStart = c("1", "2", "1", "3"),
    recEnd = c("1", "2", "1", "3"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(df, tmp, na = "", quote = "needed", eol = "\n")
  errors <- check_recode_blocks(tmp)
  expect_equal(length(errors), 1)
  expect_equal(errors[[1]]$error_type, "recode_block_collision")
})

test_that("check_recode_blocks uses all rows' databaseStart, not just first", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  # Block [A] has row 1 for db1, row 2 for db2.
  # Block [B] has row 3 for db2 with same recStart as block [A] row 2.
  # If only first row's databaseStart is used, db2 collision is missed.
  df <- data.frame(
    variable = c("A", "A", "A"),
    variableStart = c("[A]", "[A]", "[B]"),
    databaseStart = c("cchs2001_p", "cchs2003_p", "cchs2003_p"),
    recStart = c("1", "2", "2"),
    recEnd = c("1", "2", "2"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(df, tmp, na = "", quote = "needed", eol = "\n")
  errors <- check_recode_blocks(tmp)
  expect_equal(length(errors), 1)
  expect_equal(errors[[1]]$error_type, "recode_block_collision")
})

# --- .split_csv_line ---

test_that(".split_csv_line handles simple fields", {
  result <- cchsflow:::.split_csv_line("a,b,c")
  expect_equal(result, c("a", "b", "c"))
})

test_that(".split_csv_line handles quoted field with comma", {
  result <- cchsflow:::.split_csv_line('"a,b",c')
  expect_equal(result, c('"a,b"', "c"))
})

test_that(".split_csv_line handles escaped quotes", {
  result <- cchsflow:::.split_csv_line('"a""b",c')
  expect_equal(result, c('"a""b"', "c"))
})

test_that(".split_csv_line handles empty fields", {
  result <- cchsflow:::.split_csv_line("a,,c")
  expect_equal(result, c("a", "", "c"))
})

test_that(".split_csv_line handles empty string", {
  result <- cchsflow:::.split_csv_line("")
  expect_equal(result, "")
})

test_that(".split_csv_line handles single field", {
  result <- cchsflow:::.split_csv_line("a")
  expect_equal(result, "a")
})

test_that(".split_csv_line handles trailing comma", {
  result <- cchsflow:::.split_csv_line("a,b,")
  expect_equal(result, c("a", "b", ""))
})

# --- fix_worksheet ---

test_that("fix_worksheet returns empty list for clean file", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  df <- data.frame(
    variable = c("A", "B"),
    label = c("L", "L"),
    labelLong = c("LL", "LL"),
    section = c("s", "s"),
    subject = c("S", "S"),
    variableType = c("cont", "cont"),
    units = c("", ""),
    databaseStart = c("cchs2001_p", "cchs2001_p"),
    variableStart = c("V", "V"),
    description = c("d", "d"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(df, tmp, na = "", quote = "needed", eol = "\n")
  result <- fix_worksheet(tmp, "variables")
  expect_equal(length(result), 0)
})

test_that("fix_worksheet sorts unsorted rows", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  df <- data.frame(
    variable = c("B", "A"),
    label = c("L", "L"),
    labelLong = c("LL", "LL"),
    section = c("s", "s"),
    subject = c("S", "S"),
    variableType = c("cont", "cont"),
    units = c("", ""),
    databaseStart = c("cchs2001_p", "cchs2001_p"),
    variableStart = c("V", "V"),
    description = c("d", "d"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(df, tmp, na = "", quote = "needed", eol = "\n")
  result <- fix_worksheet(tmp, "variables")
  sort_errors <- Filter(function(e) e$error_type == "unsorted_rows", result)
  expect_true(all(sapply(sort_errors, function(e) e$fixed)))

  # Verify file is now sorted
  fixed_df <- read.csv(tmp, stringsAsFactors = FALSE)
  expect_equal(fixed_df$variable, c("A", "B"))
})

test_that("fix_worksheet returns fixed=FALSE for missing file", {
  result <- fix_worksheet("/nonexistent/path.csv", "variables")
  expect_equal(length(result), 1)
  expect_false(result[[1]]$fixed)
})

# --- load_schema ---

test_that("load_schema returns expected structure for variables", {
  schema <- load_schema("variables")
  expect_true("expected_column_order" %in% names(schema))
  expect_true("id_column_name" %in% names(schema))
  expect_equal(schema$id_column_name, "variable")
})

test_that("load_schema returns expected structure for variable_details", {
  schema <- load_schema("variable_details")
  expect_true("expected_column_order" %in% names(schema))
  expect_equal(schema$id_column_name, "variable")
})

test_that("load_schema rejects invalid file_type", {
  expect_error(load_schema("invalid_type"))
})

# --- scope_worksheets ---

test_that("scope_worksheets returns unscoped for NULL args", {
  result <- scope_worksheets(
    "inst/extdata/variables.csv",
    "inst/extdata/variable_details.csv"
  )
  expect_false(result$scoped)
  expect_equal(result$variables_path, "inst/extdata/variables.csv")
})

# --- parse_scope_args ---

test_that("parse_scope_args parses --variables", {
  result <- parse_scope_args(c("--variables", "A,B,C"))
  expect_equal(result$variables, c("A", "B", "C"))
  expect_null(result$subjects)
})

test_that("parse_scope_args parses --subject", {
  result <- parse_scope_args(c("--subject", "Smoking"))
  expect_null(result$variables)
  expect_equal(result$subjects, "Smoking")
})

test_that("parse_scope_args returns NULL for no args", {
  result <- parse_scope_args(character(0))
  expect_null(result$variables)
  expect_null(result$subjects)
})

test_that("parse_scope_args warns on --variables without value", {
  expect_warning(
    parse_scope_args(c("--variables")),
    "without a value"
  )
})

test_that("parse_scope_args parses both args", {
  result <- parse_scope_args(c("--variables", "A,B", "--subject", "Smoking"))
  expect_equal(result$variables, c("A", "B"))
  expect_equal(result$subjects, "Smoking")
})
