# Regression test: list-mode rec_with_table() must apply each database's own
# recode rules. Before the fix, recode_call() received the full database_name
# vector, so grepl() matched only the first database's rows and every
# database in the list was recoded with the first database's rules.

library(testthat)

test_that("list-mode rec_with_table applies per-database rules", {
  variables <- data.frame(
    variable = "TEST_VAR",
    label = "Test variable",
    labelLong = "Test variable (long)",
    section = "test",
    subject = "test",
    variableType = "Categorical",
    databaseStart = "db_one, db_two",
    units = "N/A",
    variableStart = "db_one::SRC, db_two::SRC",
    stringsAsFactors = FALSE
  )

  variable_details <- data.frame(
    variable = rep("TEST_VAR", 4),
    typeEnd = rep("cat", 4),
    typeStart = rep("cat", 4),
    databaseStart = c("db_one", "db_one", "db_two", "db_two"),
    variableStart = c("db_one::SRC", "db_one::SRC", "db_two::SRC", "db_two::SRC"),
    dummyVariable = rep("N/A", 4),
    recStart = c("1", "2", "1", "2"),
    recEnd = c("11", "12", "21", "22"),
    catLabel = c("one-1", "one-2", "two-1", "two-2"),
    catLabelLong = c("one-1", "one-2", "two-1", "two-2"),
    numValidCat = rep("2", 4),
    units = rep("N/A", 4),
    stringsAsFactors = FALSE
  )

  db_one <- data.frame(SRC = c(1, 2))
  db_two <- data.frame(SRC = c(1, 2))

  out <- suppressWarnings(suppressMessages(rec_with_table(
    data = list(db_one = db_one, db_two = db_two),
    variables = "TEST_VAR",
    database_name = c("db_one", "db_two"),
    variable_details = variable_details,
    log = FALSE,
    notes = FALSE
  )))

  # Each database must be recoded with its own rules
  expect_equal(as.character(out$db_one$TEST_VAR), c("11", "12"))
  expect_equal(as.character(out$db_two$TEST_VAR), c("21", "22"))
})
