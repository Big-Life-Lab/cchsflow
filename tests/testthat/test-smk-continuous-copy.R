# =============================================================================
# SMK_09C, SMK_06C, SMK_10C — Continuous copy regression tests
# =============================================================================
#
# Regression: commit 8ac6fc4 placed numeric ranges in recEnd instead of
# recStart, causing all valid values to fall through to NA(b).
# Fixed in 405bf14 (column swap) and verified bounds against primary
# dictionaries across all supported cycles (2003–2021).
#
# Bounds rationale: each variable's range was verified against primary
# CCHS data dictionaries (cchsflow-docs v2.1.0) across all supported
# cycles. Per-variable bounds reflect observed min/max:
#   SMK_09C: [3, 76]   (max 76 in 2019)
#   SMK_06C: [3, 88]   (max 88 in 2015)
#   SMK_10C: [3, 68]   (max 68 in 2015)
# Min of 3 reflects survey routing (< 3 years goes through categorical).
#
# These tests verify:
#   1. variable_details.csv has correct column placement (recEnd=copy,
#      recStart=[min, max]) for each Master continuous variable
#   2. Recoding engine produces correct output for valid durations,
#      boundaries, valid skips, and non-response
#   3. End-to-end SMK_09C -> time_quit_smoking_daily -> smoke_simple route
# =============================================================================

library(testthat)
library(haven)

# Helper: run rec_with_table for a single variable in one database.
# source_col overrides the input column name when the source variable
# differs from the harmonized name (e.g., SMK_070 -> SMK_06C in 2015+).
recode_smk <- function(var_name, values, db = "cchs2007_2008_m",
                        source_col = var_name) {
  df <- data.frame(X = values)
  names(df) <- source_col
  dat <- list(df)
  names(dat) <- db
  result <- suppressWarnings(suppressMessages(
    rec_with_table(
      data = dat,
      variables = var_name,
      database_name = db,
      log = FALSE, notes = FALSE
    )
  ))
  result[[db]][[var_name]]
}

# =============================================================================
# Task 4a: CSV column regression — recStart must contain the numeric range,
#          recEnd must contain "copy"
# =============================================================================

test_that("SMK_09C continuous copy row has correct recStart/recEnd columns", {
  vd <- read.csv(
    system.file("extdata", "variable_details.csv", package = "cchsflow"),
    stringsAsFactors = FALSE
  )
  row <- vd[vd$variable == "SMK_09C" & vd$recEnd == "copy", ]
  expect_true(nrow(row) >= 1, info = "No copy row found for SMK_09C")
  expect_equal(row$recEnd[1], "copy")
  expect_match(row$recStart[1], "^\\[3,\\s*76\\]$")
})

test_that("SMK_06C continuous copy row has correct recStart/recEnd columns", {
  vd <- read.csv(
    system.file("extdata", "variable_details.csv", package = "cchsflow"),
    stringsAsFactors = FALSE
  )
  row <- vd[vd$variable == "SMK_06C" & vd$recEnd == "copy", ]
  expect_true(nrow(row) >= 1, info = "No copy row found for SMK_06C")
  expect_equal(row$recEnd[1], "copy")
  expect_match(row$recStart[1], "^\\[3,\\s*88\\]$")
})

test_that("SMK_10C continuous copy row has correct recStart/recEnd columns", {
  vd <- read.csv(
    system.file("extdata", "variable_details.csv", package = "cchsflow"),
    stringsAsFactors = FALSE
  )
  row <- vd[vd$variable == "SMK_10C" & vd$recEnd == "copy", ]
  expect_true(nrow(row) >= 1, info = "No copy row found for SMK_10C")
  expect_equal(row$recEnd[1], "copy")
  expect_match(row$recStart[1], "^\\[3,\\s*68\\]$")
})

# =============================================================================
# Task 4b: Recoding engine tests — SMK_09C (years since stopped daily)
#
# Uses rec_with_table with database_name = "cchs2007_2008_m" where source
# variable is SMK_09C (from [SMK_09C] default in variableStart).
# =============================================================================

test_that("SMK_09C: ordinary valid durations are copied through", {
  out <- recode_smk("SMK_09C", c(8, 20, 45))
  expect_equal(out, c(8, 20, 45), ignore_attr = TRUE)
})

test_that("SMK_09C: boundary values 3 and 76 are accepted", {
  out <- recode_smk("SMK_09C", c(3, 76))
  expect_equal(out, c(3, 76), ignore_attr = TRUE)
})

test_that("SMK_09C: valid skip 996 maps to NA(a)", {
  out <- recode_smk("SMK_09C", 996)
  expect_true(is.na(out))
  expect_equal(haven::na_tag(out), "a")
})

test_that("SMK_09C: non-response 997/998/999 maps to NA(b)", {
  out <- recode_smk("SMK_09C", c(997, 998, 999))
  expect_true(all(is.na(out)))
  expect_equal(haven::na_tag(out), c("b", "b", "b"))
})

# =============================================================================
# Task 4c: Recoding engine tests — SMK_06C (years since stopped occasional)
# =============================================================================

test_that("SMK_06C: ordinary valid durations are copied through", {
  out <- recode_smk("SMK_06C", c(5, 40, 83))
  expect_equal(out, c(5, 40, 83), ignore_attr = TRUE)
})

test_that("SMK_06C: value 88 accepted (boundary, was rejected by old [0, 82])", {
  # In 2015+, source variable is SMK_070 (not SMK_06C)
  out <- recode_smk("SMK_06C", 88, db = "cchs2015_2016_m", source_col = "SMK_070")
  expect_equal(out, 88, ignore_attr = TRUE)
})

test_that("SMK_06C: boundary values 3 and 88 are accepted", {
  out <- recode_smk("SMK_06C", c(3, 88))
  expect_equal(out, c(3, 88), ignore_attr = TRUE)
})

test_that("SMK_06C: valid skip 996 maps to NA(a)", {
  out <- recode_smk("SMK_06C", 996)
  expect_true(is.na(out))
  expect_equal(haven::na_tag(out), "a")
})

test_that("SMK_06C: non-response 997/998/999 maps to NA(b)", {
  out <- recode_smk("SMK_06C", c(997, 998, 999))
  expect_true(all(is.na(out)))
  expect_equal(haven::na_tag(out), c("b", "b", "b"))
})

# =============================================================================
# Task 4d: Recoding engine tests — SMK_10C (years since quit completely)
# =============================================================================

test_that("SMK_10C: ordinary valid durations are copied through", {
  out <- recode_smk("SMK_10C", c(3, 30, 68))
  expect_equal(out, c(3, 30, 68), ignore_attr = TRUE)
})

test_that("SMK_10C: boundary values 3 and 68 are accepted", {
  out <- recode_smk("SMK_10C", c(3, 68))
  expect_equal(out, c(3, 68), ignore_attr = TRUE)
})

test_that("SMK_10C: valid skip 996 maps to NA(a)", {
  out <- recode_smk("SMK_10C", 996)
  expect_true(is.na(out))
  expect_equal(haven::na_tag(out), "a")
})

test_that("SMK_10C: non-response 997/998/999 maps to NA(b)", {
  out <- recode_smk("SMK_10C", c(997, 998, 999))
  expect_true(all(is.na(out)))
  expect_equal(haven::na_tag(out), c("b", "b", "b"))
})

# =============================================================================
# Task 5: Full pipeline — SMK_09C -> time_quit_smoking_daily -> smoke_simple
#
# An eligible former daily smoker who quit 8 years ago should reach
# smoke_simple category 3 (former daily, quit > 5 years).
# =============================================================================

test_that("Former daily smoker who quit 8 years ago reaches smoke_simple cat 3", {
  tqsd <- calculate_time_quit_smoking_daily(
    SMKDSTY_cat5 = 3, SMK_09A_cont = 2.5, SMK_09C = 8.0
  )
  expect_equal(tqsd, 8.0)

  result <- smoke_simple_fun(SMKDSTY_cat5 = 3, time_quit_smoking = tqsd)
  expect_equal(as.numeric(result), 3)
})

test_that("Former daily smoker who quit 4 years ago reaches smoke_simple cat 2", {
  tqsd <- calculate_time_quit_smoking_daily(
    SMKDSTY_cat5 = 3, SMK_09A_cont = 1.5, SMK_09C = 4.0
  )
  expect_equal(tqsd, 4.0)

  result <- smoke_simple_fun(SMKDSTY_cat5 = 3, time_quit_smoking = tqsd)
  expect_equal(as.numeric(result), 2)
})

test_that("PUMF fallback: former daily with SMK_09A_cont = 5.0 gets cat 2", {
  tqsd <- calculate_time_quit_smoking_daily(
    SMKDSTY_cat5 = 3, SMK_09A_cont = 5.0, SMK_09C = NA
  )
  expect_equal(tqsd, 5.0)

  result <- smoke_simple_fun(SMKDSTY_cat5 = 3, time_quit_smoking = tqsd)
  expect_equal(as.numeric(result), 2)
})

test_that("2001 behaviour preserved: SMK_09A_cont = 15.0 reaches cat 3", {
  tqsd <- calculate_time_quit_smoking_daily(
    SMKDSTY_cat5 = 3, SMK_09A_cont = 15.0, SMK_09C = NA
  )
  expect_equal(tqsd, 15.0)

  result <- smoke_simple_fun(SMKDSTY_cat5 = 3, time_quit_smoking = tqsd)
  expect_equal(as.numeric(result), 3)
})

test_that("Former occasional smoker is hardcoded to smoke_simple cat 2", {
  result <- smoke_simple_fun(SMKDSTY_cat5 = 4, time_quit_smoking = tagged_na("a"))
  expect_equal(as.numeric(result), 2)
})

test_that("Never smoker reaches smoke_simple cat 0", {
  result <- smoke_simple_fun(SMKDSTY_cat5 = 5, time_quit_smoking = tagged_na("a"))
  expect_equal(as.numeric(result), 0)
})

test_that("Current daily smoker reaches smoke_simple cat 1", {
  result <- smoke_simple_fun(SMKDSTY_cat5 = 1, time_quit_smoking = tagged_na("a"))
  expect_equal(as.numeric(result), 1)
})

# Boundary: exactly 5 years is category 2 (quit <= 5)
test_that("Former daily smoker quit exactly 5 years ago reaches cat 2", {
  result <- smoke_simple_fun(SMKDSTY_cat5 = 3, time_quit_smoking = 5.0)
  expect_equal(as.numeric(result), 2)
})

# Boundary: 5.01 years is category 3 (quit > 5)
test_that("Former daily smoker quit 5.01 years ago reaches cat 3", {
  result <- smoke_simple_fun(SMKDSTY_cat5 = 3, time_quit_smoking = 5.01)
  expect_equal(as.numeric(result), 3)
})

# Current occasional smoker also gets category 1
test_that("Current occasional smoker reaches smoke_simple cat 1", {
  result <- smoke_simple_fun(SMKDSTY_cat5 = 2, time_quit_smoking = tagged_na("a"))
  expect_equal(as.numeric(result), 1)
})
