#!/usr/bin/env Rscript

# Query cchs-metadata database from R
#
# Wrapper around the cchs-metadata CLI (Python) with a DuckDB fallback.
# Use when the MCP server is unavailable during reviews.
#
# Usage (CLI mode):
#   Rscript exec/query-metadata.R search smoking
#   Rscript exec/query-metadata.R history SMKDSTY
#   Rscript exec/query-metadata.R detail SMK_204
#   Rscript exec/query-metadata.R codes SMK_204
#   Rscript exec/query-metadata.R compare SMKDSTY 2015-2016
#   Rscript exec/query-metadata.R dataset cchs-2015d-p-can
#   Rscript exec/query-metadata.R coverage HUI06 HUI07 HUI08
#
# Usage (as library):
#   source("exec/query-metadata.R")
#   meta_search("smoking")
#   meta_history("SMKDSTY")
#   meta_detail("SMK_204")
#   meta_coverage(c("HUI06", "HUI07"))  # variable-by-cycle matrix
#
# Database location (searched in order):
#   1. CCHS_DB_PATH environment variable
#   2. ../cchsflow-docs/database/cchs_metadata.duckdb (sibling repo)
#   3. ~/github/cchsflow-docs/database/cchs_metadata.duckdb

suppressPackageStartupMessages(library(cli))

# ── Locate database ─────────────────────────────────────────────────────────

find_db <- function() {
  candidates <- c(
    Sys.getenv("CCHS_DB_PATH", ""),
    file.path("..", "cchsflow-docs", "database", "cchs_metadata.duckdb"),
    file.path(Sys.getenv("HOME"), "github", "cchsflow-docs", "database",
              "cchs_metadata.duckdb")
  )
  candidates <- candidates[candidates != ""]

  for (p in candidates) {
    if (file.exists(p)) return(normalizePath(p))
  }

  cli_abort(c(
    "cchs-metadata database not found",
    "i" = "Searched: {paste(candidates, collapse = ', ')}",
    "i" = "Set CCHS_DB_PATH or clone cchsflow-docs alongside cchsflow"
  ))
}

# ── Locate Python CLI ───────────────────────────────────────────────────────

find_cli <- function() {
  candidates <- c(
    file.path("..", "cchsflow-docs", "mcp-server", "cli.py"),
    file.path(Sys.getenv("HOME"), "github", "cchsflow-docs", "mcp-server",
              "cli.py")
  )
  for (p in candidates) {
    if (file.exists(p)) return(normalizePath(p))
  }
  NULL
}

# ── Query via Python CLI ─────────────────────────────────────────────────────

run_cli <- function(args) {
  cli_path <- find_cli()
  if (is.null(cli_path)) return(NULL)

  db_path <- find_db()
  cmd <- sprintf(
    "python3 '%s' --db '%s' %s 2>&1",
    cli_path, db_path, paste(args, collapse = " ")
  )

  tryCatch(
    system(cmd, intern = TRUE),
    error = function(e) NULL
  )
}

# ── Query via DuckDB directly (fallback) ─────────────────────────────────────

query_db <- function(sql) {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    cli_abort(c(
      "duckdb R package not installed",
      "i" = "Install with: install.packages('duckdb')",
      "i" = "Or use the Python CLI: python3 ../cchsflow-docs/mcp-server/cli.py"
    ))
  }

  db_path <- find_db()
  con <- duckdb::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE))
  duckdb::dbGetQuery(con, sql)
}

# ── High-level query functions ───────────────────────────────────────────────

#' Search variables by name or label
meta_search <- function(query) {
  result <- run_cli(c("search", shQuote(query), "--json"))
  if (!is.null(result)) {
    return(jsonlite::fromJSON(paste(result, collapse = "\n")))
  }
  # DuckDB fallback
  query_db(sprintf(
    "SELECT variable_name, label_statcan, cchsflow_name
     FROM variables
     WHERE variable_name ILIKE '%%%s%%'
        OR label_statcan ILIKE '%%%s%%'
     LIMIT 50",
    query, query
  ))
}

#' Get variable history (which cycles contain it)
meta_history <- function(variable_name) {
  result <- run_cli(c("history", variable_name, "--json"))
  if (!is.null(result)) {
    return(jsonlite::fromJSON(paste(result, collapse = "\n")))
  }
  query_db(sprintf(
    "SELECT * FROM v_variable_history
     WHERE variable_name = '%s'
     ORDER BY dataset_id",
    variable_name
  ))
}

#' Get full detail for a variable
meta_detail <- function(variable_name) {
  result <- run_cli(c("detail", variable_name, "--json"))
  if (!is.null(result)) {
    return(jsonlite::fromJSON(paste(result, collapse = "\n")))
  }
  query_db(sprintf(
    "SELECT * FROM variables WHERE variable_name = '%s'",
    variable_name
  ))
}

#' Get value codes for a variable
meta_codes <- function(variable_name) {
  result <- run_cli(c("codes", variable_name, "--json"))
  if (!is.null(result)) {
    return(jsonlite::fromJSON(paste(result, collapse = "\n")))
  }
  query_db(sprintf(
    "SELECT * FROM value_codes
     WHERE variable_name = '%s'
     ORDER BY dataset_id, code",
    variable_name
  ))
}

#' Compare Master vs PUMF for a variable in a cycle
meta_compare <- function(variable_name, cycle) {
  result <- run_cli(c("compare", variable_name, cycle, "--json"))
  if (!is.null(result)) {
    return(jsonlite::fromJSON(paste(result, collapse = "\n")))
  }
  query_db(sprintf(
    "SELECT vd.*, d.file_type
     FROM variable_datasets vd
     JOIN datasets d ON vd.dataset_id = d.dataset_id
     WHERE vd.variable_name = '%s'
       AND d.cycle = '%s'
     ORDER BY d.file_type",
    variable_name, cycle
  ))
}

#' Generate a variable-by-cycle coverage matrix
#'
#' Useful during reviews to see which cycles contain which variables.
#' Not available in the Python CLI — this is R-only.
#'
#' @param variable_names Character vector of variable names to check
#' @param file_type Filter to "master" or "pumf" (default: both)
#' @return Data frame with variables as rows, cycles as columns, "x" for present
meta_coverage <- function(variable_names, file_type = NULL) {
  where_ft <- if (!is.null(file_type)) {
    sprintf("AND d.file_type = '%s'", file_type)
  } else {
    ""
  }

  var_list <- paste(sprintf("'%s'", variable_names), collapse = ", ")
  df <- query_db(sprintf(
    "SELECT vd.variable_name, d.cycle, d.file_type
     FROM variable_datasets vd
     JOIN datasets d ON vd.dataset_id = d.dataset_id
     WHERE vd.variable_name IN (%s) %s
     ORDER BY vd.variable_name, d.cycle",
    var_list, where_ft
  ))

  if (nrow(df) == 0) {
    cli_alert_warning("No results found")
    return(data.frame())
  }

  # Pivot to matrix
  cycles <- sort(unique(df$cycle))
  mat <- data.frame(variable = variable_names, stringsAsFactors = FALSE)
  for (cyc in cycles) {
    mat[[cyc]] <- ifelse(
      variable_names %in% df$variable_name[df$cycle == cyc],
      "x", ""
    )
  }
  mat
}

# ── CLI mode ─────────────────────────────────────────────────────────────────

if (!interactive() && length(commandArgs(trailingOnly = TRUE)) > 0) {
  args <- commandArgs(trailingOnly = TRUE)
  subcmd <- args[1]
  rest <- args[-1]

  if (subcmd == "coverage") {
    # R-native coverage matrix
    mat <- meta_coverage(rest)
    print(mat, row.names = FALSE)
  } else {
    # Delegate to Python CLI
    output <- run_cli(args)
    if (!is.null(output)) {
      cat(paste(output, collapse = "\n"), "\n")
    } else {
      cli_alert_warning("Python CLI not available, falling back to DuckDB")
      if (subcmd == "search") {
        print(meta_search(rest[1]))
      } else if (subcmd == "history") {
        print(meta_history(rest[1]))
      } else if (subcmd == "detail") {
        print(meta_detail(rest[1]))
      } else if (subcmd == "codes") {
        print(meta_codes(rest[1]))
      } else if (subcmd == "compare") {
        print(meta_compare(rest[1], rest[2]))
      } else {
        cli_abort("Unknown subcommand: {subcmd}")
      }
    }
  }
}
