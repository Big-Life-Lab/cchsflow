# CEP-007 Diet: PUMF Integration Test for PR #148
# Tests FVCDFRU, FVCDSAL, FVCDPOT, FVCDCAR, FVCDVEG, FVCDJUI,
# diet_score, and diet_score_cat3 across all PUMF cycles.
#
# This script validates that rec_with_table() correctly harmonizes
# FVC and diet variables. The cross-cycle prevalence summary helps
# identify era boundary issues (e.g., 2014 -> 2015 naming changes).

# Use devtools::load_all() because PR modifies R/diet.R
devtools::load_all()
library(dplyr)

# Load worksheet from the branch under review
variable_details <- read.csv("inst/extdata/variable_details.csv",
                             stringsAsFactors = FALSE)

# Variables to test: FVC inputs + derived diet variables
fvc_inputs <- c("FVCDFRU", "FVCDSAL", "FVCDPOT", "FVCDCAR", "FVCDVEG",
                "FVCDJUI")
diet_vars <- c("diet_score", "diet_score_cat3")
# DHH_SEX is needed as input to diet_score_fun but is not an FVC variable
all_vars <- c(fvc_inputs, "DHH_SEX", diet_vars)

# PUMF cycles from databaseStart
cycles <- c("cchs2001_p", "cchs2003_p", "cchs2005_p",
            "cchs2007_2008_p", "cchs2009_2010_p",
            "cchs2011_2012_p", "cchs2013_2014_p",
            "cchs2015_2016_p", "cchs2017_2018_p")

# Also test annual cycles that are listed in databaseStart
annual_cycles <- c("cchs2010_p", "cchs2012_p", "cchs2014_p")

results <- data.frame()
cat_distributions <- list()

for (cycle in c(cycles, annual_cycles)) {
  rdata_file <- file.path("data", paste0(cycle, ".RData"))
  if (!file.exists(rdata_file)) {
    cat("SKIP", cycle, "- file not found\n")
    next
  }

  cat("\n=== ", cycle, " ===\n")
  load(rdata_file)
  df <- get(cycle)
  n_total <- nrow(df)

  result <- tryCatch({
    rec_with_table(
      data = df,
      variables = all_vars,
      database_name = cycle,
      variable_details = variable_details,
      log = FALSE
    )
  }, error = function(e) {
    cat("ERROR in", cycle, ":", e$message, "\n")
    NULL
  })

  if (!is.null(result)) {
    # Report for each variable
    for (v in setdiff(names(result), "ADM_RNO")) {
      valid <- sum(!is.na(result[[v]]))
      cat(cycle, v, ": valid =", valid, "/", n_total,
          "(", round(100 * valid / n_total, 1), "%)\n")

      results <- rbind(results, data.frame(
        cycle = cycle, variable = v,
        n = n_total, valid = valid,
        valid_pct = round(100 * valid / n_total, 1),
        stringsAsFactors = FALSE
      ))
    }

    # Category distribution for diet_score_cat3
    if ("diet_score_cat3" %in% names(result)) {
      cat("\n  diet_score_cat3 distribution:\n")
      freq <- table(result$diet_score_cat3, useNA = "ifany")
      print(freq)
      cat_distributions[[cycle]] <- freq
    }

    # Summary statistics for diet_score (continuous)
    if ("diet_score" %in% names(result)) {
      ds <- result$diet_score[!is.na(result$diet_score)]
      if (length(ds) > 0) {
        cat("\n  diet_score summary:\n")
        print(summary(ds))
      }
    }
  }

  rm(list = cycle)  # free memory
}

# Cross-cycle prevalence summary
cat("\n\n=== CROSS-CYCLE SUMMARY ===\n")
for (v in unique(results$variable)) {
  cat("\n", v, ":\n")
  sub <- results[results$variable == v, ]
  print(sub[, c("cycle", "n", "valid", "valid_pct")], row.names = FALSE)
}

# Save results
write.csv(results,
          "ceps/cep-007-diet/diet-pumf-integration-test.csv",
          row.names = FALSE)
cat("\nResults saved to ceps/cep-007-diet/diet-pumf-integration-test.csv\n")
