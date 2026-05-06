#!/usr/bin/env Rscript

# Programmatic worksheet row rebuilder for variable_details.csv
#
# Provides reusable functions for generating worksheet rows from templates.
# Designed for bulk variable coverage expansion where manual CSV editing
# is error-prone.
#
# Usage (as library — source and call functions):
#   source("exec/rebuild-rows.R")
#   rows <- binary_block("HUI06", dbs, varstart)
#   write_hearing_rows(rows)
#
# Usage (standalone — rebuild a variable group):
#   Rscript exec/rebuild-rows.R --variable HUI06 --preview
#   Rscript exec/rebuild-rows.R --group hearing --apply
#
# Functions exported:
#   make_row()       - Create a single variable_details row from a template
#   binary_block()   - Standard binary recode block (1=Yes, 2=No, 6=NA::a, [7,8,9]=NA::b)
#   wdm_block()      - Washington Group 4-point to binary block
#   likert4_block()   - Generic 4-point Likert to binary block
#   rebuild_variable() - Replace all rows for a variable with new rows
#   preview_rows()   - Print rows for review without writing

suppressPackageStartupMessages(library(cli))

VD_PATH <- "inst/extdata/variable_details.csv"
V_PATH <- "inst/extdata/variables.csv"

# ── Read current worksheet ───────────────────────────────────────────────────

read_vd <- function(path = VD_PATH) {
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

read_v <- function(path = V_PATH) {
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

# ── Template row creation ────────────────────────────────────────────────────

#' Create a single variable_details row
#'
#' @param template A row from variable_details to use as template (inherits
#'   columns not explicitly set)
#' @param variable Harmonised variable name
#' @param dummy dummyVariable value
#' @param dbs databaseStart value
#' @param varstart variableStart value
#' @param rec_start recStart value
#' @param rec_end recEnd value
#' @param cat_label catLabel (and catLabelLong)
#' @param cat_start_label catStartLabel
#' @param type_start typeStart (default "cat")
#' @param type_end typeEnd (default "cat")
#' @param num_valid_cat numValidCat (default "2")
#' @return A single-row data.frame
make_row <- function(template, variable, dummy, dbs, varstart,
                     rec_start, rec_end, cat_label, cat_start_label,
                     type_start = "cat", type_end = "cat",
                     num_valid_cat = "2") {
  row <- template
  row$variable <- variable
  row$dummyVariable <- dummy
  row$databaseStart <- dbs
  row$variableStart <- varstart
  row$recStart <- rec_start
  row$recEnd <- rec_end
  row$typeStart <- type_start
  row$typeEnd <- type_end
  row$numValidCat <- num_valid_cat
  row$catLabel <- cat_label
  row$catLabelLong <- cat_label
  row$catStartLabel <- cat_start_label
  row
}

# ── Block generators ─────────────────────────────────────────────────────────

#' Generate a standard binary recode block
#'
#' Creates 5 rows: 1=Yes, 2=No, 6=NA::a, [7,8,9]=NA::b, else=NA::b.
#' Suitable for HUI hearing questions and similar binary variables.
#'
#' @param template Template row from variable_details
#' @param variable Harmonised variable name
#' @param dbs databaseStart string
#' @param varstart variableStart string
#' @param yes_label catStartLabel for the "Yes" row (default "Yes")
#' @param no_label catStartLabel for the "No" row (default "No")
#' @return Data frame with 5 rows
binary_block <- function(template, variable, dbs, varstart,
                         yes_label = "Yes", no_label = "No") {
  prefix <- paste0(variable, "_cat2")
  rbind(
    make_row(template, variable, paste0(prefix, "_1"), dbs, varstart,
             "1", "1", "Yes", yes_label),
    make_row(template, variable, paste0(prefix, "_2"), dbs, varstart,
             "2", "2", "No", no_label),
    make_row(template, variable, paste0(prefix, "_NAa"), dbs, varstart,
             "6", "NA::a", "Not applicable", "Not applicable"),
    make_row(template, variable, paste0(prefix, "_NAb"), dbs, varstart,
             "[7,8,9]", "NA::b", "Missing",
             "Don't know / Refusal / Not stated"),
    make_row(template, variable, paste0(prefix, "_NAb"), dbs, varstart,
             "else", "NA::b", "Missing", "Not stated")
  )
}

#' Generate a Washington Group 4-point to binary recode block
#'
#' Maps: 1 (No difficulty) -> 1 (Yes/able), 2 (Some) -> 1, 3 (A lot) -> 1,
#' 4 (Cannot do) -> 2 (No/unable). Plus NA rows.
#'
#' This mapping reflects that "some difficulty" and "a lot of difficulty"
#' still indicate ability (e.g., able to hear), while only "cannot do at all"
#' maps to inability.
#'
#' @param template Template row from variable_details
#' @param variable Harmonised variable name
#' @param dbs databaseStart string
#' @param varstart variableStart string
#' @return Data frame with 7 rows
wdm_block <- function(template, variable, dbs, varstart) {
  prefix <- paste0(variable, "_cat2")
  rbind(
    make_row(template, variable, paste0(prefix, "_1"), dbs, varstart,
             "1", "1", "Yes", "No difficulty"),
    make_row(template, variable, paste0(prefix, "_1"), dbs, varstart,
             "2", "1", "Yes", "Some difficulty"),
    make_row(template, variable, paste0(prefix, "_1"), dbs, varstart,
             "3", "1", "Yes", "A lot of difficulty"),
    make_row(template, variable, paste0(prefix, "_2"), dbs, varstart,
             "4", "2", "No", "Cannot do at all"),
    make_row(template, variable, paste0(prefix, "_NAa"), dbs, varstart,
             "6", "NA::a", "Not applicable", "Not applicable"),
    make_row(template, variable, paste0(prefix, "_NAb"), dbs, varstart,
             "[7,8,9]", "NA::b", "Missing",
             "Don't know / Refusal / Not stated"),
    make_row(template, variable, paste0(prefix, "_NAb"), dbs, varstart,
             "else", "NA::b", "Missing", "Not stated")
  )
}

#' Generate a generic 4-point Likert to binary recode block
#'
#' Maps: 1,2,3 -> 1 (threshold at "a lot of difficulty"), 4 -> 2.
#' Customise labels as needed.
#'
#' @param template Template row
#' @param variable Harmonised variable name
#' @param dbs databaseStart string
#' @param varstart variableStart string
#' @param labels Named list with keys: l1, l2, l3, l4 for the 4 source labels
#' @return Data frame with 7 rows
likert4_block <- function(template, variable, dbs, varstart,
                          labels = list(
                            l1 = "No difficulty",
                            l2 = "Some difficulty",
                            l3 = "A lot of difficulty",
                            l4 = "Cannot do at all"
                          )) {
  prefix <- paste0(variable, "_cat2")
  rbind(
    make_row(template, variable, paste0(prefix, "_1"), dbs, varstart,
             "1", "1", "Yes", labels$l1),
    make_row(template, variable, paste0(prefix, "_1"), dbs, varstart,
             "2", "1", "Yes", labels$l2),
    make_row(template, variable, paste0(prefix, "_1"), dbs, varstart,
             "3", "1", "Yes", labels$l3),
    make_row(template, variable, paste0(prefix, "_2"), dbs, varstart,
             "4", "2", "No", labels$l4),
    make_row(template, variable, paste0(prefix, "_NAa"), dbs, varstart,
             "6", "NA::a", "Not applicable", "Not applicable"),
    make_row(template, variable, paste0(prefix, "_NAb"), dbs, varstart,
             "[7,8,9]", "NA::b", "Missing",
             "Don't know / Refusal / Not stated"),
    make_row(template, variable, paste0(prefix, "_NAb"), dbs, varstart,
             "else", "NA::b", "Missing", "Not stated")
  )
}

# ── Worksheet operations ─────────────────────────────────────────────────────

#' Preview rows without writing
#'
#' @param rows Data frame of new rows to preview
preview_rows <- function(rows) {
  for (v in unique(rows$variable)) {
    vrows <- rows[rows$variable == v, ]
    cli_h3("{v} ({nrow(vrows)} rows)")

    # Group by databaseStart
    for (db in unique(vrows$databaseStart)) {
      block <- vrows[vrows$databaseStart == db, ]
      cli_alert_info("Block: {substr(db, 1, 50)}...")
      cli_alert_info("  varStart: {substr(block$variableStart[1], 1, 60)}")
      for (i in seq_len(nrow(block))) {
        r <- block[i, ]
        cli_text("  recS={r$recStart}  recE={r$recEnd}  dummy={r$dummyVariable}  catStart={r$catStartLabel}")
      }
    }
  }
}

#' Replace all rows for specified variables with new rows
#'
#' Removes existing rows for the given variables and inserts new rows
#' at the correct alphabetical position.
#'
#' @param new_rows Data frame of new rows
#' @param variables Character vector of variable names to replace
#' @param vd_path Path to variable_details.csv
#' @param dry_run If TRUE, return the result without writing (default TRUE)
#' @return The modified data frame (invisibly)
rebuild_variable <- function(new_rows, variables, vd_path = VD_PATH,
                             dry_run = TRUE) {
  vd <- read_vd(vd_path)
  old_count <- sum(vd$variable %in% variables)

  non_target <- vd[!vd$variable %in% variables, ]

  # Find insertion point (alphabetical)
  insert_idx <- which(non_target$variable > min(variables))[1]
  if (is.na(insert_idx)) insert_idx <- nrow(non_target) + 1

  result <- rbind(
    non_target[seq_len(insert_idx - 1), ],
    new_rows,
    non_target[insert_idx:nrow(non_target), ]
  )

  cli_alert_info("Rows: {old_count} -> {nrow(new_rows)} for {paste(variables, collapse = ', ')}")
  cli_alert_info("Total: {nrow(vd)} -> {nrow(result)}")

  if (!dry_run) {
    write.csv(result, vd_path, row.names = FALSE, na = "")
    cli_alert_success("Written to {vd_path}")
    cli_alert_warning("Run exec/fix-worksheets.R to standardise formatting")
  } else {
    cli_alert_info("Dry run — use dry_run = FALSE to write")
  }

  invisible(result)
}

# ── CLI mode ─────────────────────────────────────────────────────────────────

if (!interactive() && length(commandArgs(trailingOnly = TRUE)) > 0) {
  args <- commandArgs(trailingOnly = TRUE)

  if ("--help" %in% args || "-h" %in% args) {
    cat("
Worksheet row rebuilder

Usage:
  source('exec/rebuild-rows.R')  # Load as library
  Rscript exec/rebuild-rows.R --help

Functions available when sourced:
  make_row()          - Single row from template

  binary_block()      - Binary recode (1=Yes, 2=No, 6=NA::a, [7,8,9]=NA::b)
  wdm_block()         - WDM 4-point to binary (1,2,3->1, 4->2)
  likert4_block()     - Generic 4-point to binary
  preview_rows()      - Print rows for review
  rebuild_variable()  - Replace variable rows in CSV (dry_run=TRUE default)

Example:
  source('exec/rebuild-rows.R')
  vd <- read_vd()
  template <- vd[vd$variable == 'HUI06', ][1, ]
  rows <- binary_block(template, 'HUI06', 'cchs2001_m, cchs2003_m', '[HUI_06]')
  preview_rows(rows)
  rebuild_variable(rows, 'HUI06', dry_run = FALSE)
")
  }
}
