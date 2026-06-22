#!/usr/bin/env Rscript

# Compare worksheet CSVs between git branches (content-based, not line-based)
#
# Groups rows by variable, compares key fields, and reports only content
# changes — ignoring formatting differences (quoting, whitespace, column order).
#
# Usage:
#   Rscript exec/diff-worksheets.R                          # diff HEAD vs working tree
#   Rscript exec/diff-worksheets.R --ref origin/main        # diff origin/main vs working tree
#   Rscript exec/diff-worksheets.R --ref HEAD~3             # diff 3 commits ago vs working tree
#   Rscript exec/diff-worksheets.R --variables "HUI06,HUI07,HUI07A"  # filter to specific vars
#   Rscript exec/diff-worksheets.R --file variable_details  # only variable_details.csv
#
# Output: per-variable summary of added/removed/changed rows with field-level detail.

suppressPackageStartupMessages(library(cli))

# ── Parse arguments ──────────────────────────────────────────────────────────

args <- commandArgs(trailingOnly = TRUE)

parse_args <- function(args) {
  opts <- list(
    ref = "HEAD",
    variables = NULL,
    file = "both"
  )

  i <- 1
  while (i <= length(args)) {
    if (args[i] == "--ref" && i < length(args)) {
      opts$ref <- args[i + 1]
      i <- i + 2
    } else if (args[i] == "--variables" && i < length(args)) {
      opts$variables <- trimws(unlist(strsplit(args[i + 1], ",")))
      i <- i + 2
    } else if (args[i] == "--file" && i < length(args)) {
      opts$file <- args[i + 1]
      i <- i + 2
    } else {
      i <- i + 1
    }
  }
  opts
}

opts <- parse_args(args)

# ── Helper: read CSV from git ref ────────────────────────────────────────────

read_csv_from_ref <- function(ref, csv_path) {
  cmd <- sprintf("git show %s:%s 2>/dev/null", ref, csv_path)
  result <- tryCatch(
    {
      con <- pipe(cmd, "r")
      on.exit(close(con))
      read.csv(con, stringsAsFactors = FALSE, check.names = FALSE)
    },
    error = function(e) NULL
  )
  result
}

# ── Key fields for comparison ────────────────────────────────────────────────

VD_KEY_FIELDS <- c(
  "variable", "dummyVariable", "databaseStart", "variableStart",
  "recStart", "recEnd", "typeStart", "typeEnd", "numValidCat",
  "catLabel", "catLabelLong", "catStartLabel"
)

V_KEY_FIELDS <- c(
  "variable", "databaseStart", "variableStart", "variableType",
  "subject", "label", "labelLong", "units"
)

# ── Compare one file ─────────────────────────────────────────────────────────

compare_file <- function(ref, csv_path, key_fields, label, filter_vars = NULL) {
  cli_h2("{label}")

  old <- read_csv_from_ref(ref, csv_path)
  if (is.null(old)) {
    cli_alert_warning("Could not read {csv_path} from {ref}")
    return(invisible(NULL))
  }
  new <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)

  # Use only fields present in both
  fields <- intersect(key_fields, intersect(names(old), names(new)))

  old <- old[, fields, drop = FALSE]
  new <- new[, fields, drop = FALSE]

  # Normalise: trim whitespace, convert NA to ""
  normalise <- function(df) {
    df[] <- lapply(df, function(x) {
      x <- as.character(x)
      x[is.na(x)] <- ""
      trimws(x)
    })
    df
  }

  old <- normalise(old)
  new <- normalise(new)

  # Filter variables if requested
  if (!is.null(filter_vars) && "variable" %in% names(old)) {
    old <- old[old$variable %in% filter_vars, , drop = FALSE]
    new <- new[new$variable %in% filter_vars, , drop = FALSE]
  }

  # Group by variable
  old_vars <- if ("variable" %in% names(old)) unique(old$variable) else "ALL"
  new_vars <- if ("variable" %in% names(new)) unique(new$variable) else "ALL"
  all_vars <- sort(union(old_vars, new_vars))

  added_vars <- setdiff(new_vars, old_vars)
  removed_vars <- setdiff(old_vars, new_vars)
  common_vars <- intersect(old_vars, new_vars)

  changes_found <- FALSE

  if (length(added_vars) > 0) {
    changes_found <- TRUE
    cli_alert_success("New variables: {paste(added_vars, collapse = ', ')}")
    for (v in added_vars) {
      n <- sum(new$variable == v)
      cli_alert_info("  {v}: {n} rows added")
    }
  }

  if (length(removed_vars) > 0) {
    changes_found <- TRUE
    cli_alert_danger("Removed variables: {paste(removed_vars, collapse = ', ')}")
  }

  # Compare common variables
  for (v in common_vars) {
    old_v <- old[old$variable == v, , drop = FALSE]
    new_v <- new[new$variable == v, , drop = FALSE]

    # Create row signatures for comparison
    make_sig <- function(df) {
      apply(df, 1, function(row) paste(row, collapse = "|"))
    }

    old_sigs <- make_sig(old_v)
    new_sigs <- make_sig(new_v)

    if (identical(sort(old_sigs), sort(new_sigs))) next

    changes_found <- TRUE

    added_rows <- sum(!new_sigs %in% old_sigs)
    removed_rows <- sum(!old_sigs %in% new_sigs)

    cli_h3("{v}")
    cli_alert_info("Rows: {nrow(old_v)} -> {nrow(new_v)} ({cli::col_green('+{added_rows}')} / {cli::col_red('-{removed_rows}')})")

    # Show field-level changes for modified rows
    # Match rows by dummyVariable + databaseStart if available
    if (all(c("dummyVariable", "databaseStart") %in% fields)) {
      make_key <- function(df) {
        paste(df$dummyVariable, df$databaseStart, sep = "@@")
      }
      old_keys <- make_key(old_v)
      new_keys <- make_key(new_v)

      common_keys <- intersect(old_keys, new_keys)
      for (k in common_keys) {
        o <- old_v[old_keys == k, , drop = FALSE][1, ]
        n <- new_v[new_keys == k, , drop = FALSE][1, ]
        diffs <- fields[o != n]
        diffs <- diffs[!diffs %in% c("variable")]
        if (length(diffs) > 0) {
          for (d in diffs) {
            cli_alert_warning(
              "  {d}: \"{o[[d]]}\" -> \"{n[[d]]}\""
            )
          }
        }
      }

      new_only <- new_keys[!new_keys %in% old_keys]
      if (length(new_only) > 0) {
        for (k in new_only) {
          row <- new_v[new_keys == k, , drop = FALSE][1, ]
          cli_alert_success("  + {row$dummyVariable} (db: {substr(row$databaseStart, 1, 40)}...)")
        }
      }
    } else if ("databaseStart" %in% fields) {
      # variables.csv: compare by variable name directly
      for (f in setdiff(fields, "variable")) {
        if (old_v[[f]][1] != new_v[[f]][1]) {
          cli_alert_warning(
            "  {f}: \"{substr(old_v[[f]][1], 1, 60)}\" -> \"{substr(new_v[[f]][1], 1, 60)}\""
          )
        }
      }
    }
  }

  if (!changes_found) {
    cli_alert_success("No content changes detected")
  }

  invisible(changes_found)
}

# ── Main ─────────────────────────────────────────────────────────────────────

cli_h1("Worksheet content diff: {opts$ref} vs working tree")

if (!is.null(opts$variables)) {
  cli_alert_info("Filtering to: {paste(opts$variables, collapse = ', ')}")
}

if (opts$file %in% c("both", "variable_details")) {
  compare_file(
    opts$ref, "inst/extdata/variable_details.csv",
    VD_KEY_FIELDS, "variable_details.csv",
    opts$variables
  )
}

if (opts$file %in% c("both", "variables")) {
  compare_file(
    opts$ref, "inst/extdata/variables.csv",
    V_KEY_FIELDS, "variables.csv",
    opts$variables
  )
}
