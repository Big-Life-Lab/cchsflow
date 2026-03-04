library(cchsflow)
library(dplyr)

# Load worksheet from the branch under review
variable_details <- read.csv("inst/extdata/variable_details.csv",
                             stringsAsFactors = FALSE)

# In-scope variables for PR #166
variables_to_test <- c("ALCDTTM", "ALWDWKY", "ALW_1",
                       "ALW_2A1", "ALW_2A2", "ALW_2A3", "ALW_2A4",
                       "ALW_2A5", "ALW_2A6", "ALW_2A7",
                       "binge_drinker")

# PUMF cycles
cycles <- c("cchs2001_p", "cchs2003_p", "cchs2005_p",
            "cchs2007_2008_p", "cchs2009_2010_p", "cchs2011_2012_p",
            "cchs2013_2014_p", "cchs2015_2016_p", "cchs2017_2018_p")

results <- data.frame()

for (cycle in cycles) {
  rdata_file <- file.path("data", paste0(cycle, ".RData"))
  if (!file.exists(rdata_file)) {
    cat("SKIP", cycle, "- file not found\n")
    next
  }

  load(rdata_file)
  df <- get(cycle)

  result <- tryCatch({
    rec_with_table(
      data = df,
      variables = variables_to_test,
      database_name = cycle,
      variable_details = variable_details,
      log = FALSE
    )
  }, error = function(e) {
    cat("ERROR in", cycle, ":", e$message, "\n")
    NULL
  })

  if (!is.null(result)) {
    n <- nrow(result)
    for (v in setdiff(names(result), "ADM_RNO")) {
      valid <- sum(!is.na(result[[v]]))
      cat(cycle, v, ": valid =", valid, "/", n,
          "(", round(100 * valid / n, 1), "%)\n")

      results <- rbind(results, data.frame(
        cycle = cycle, variable = v,
        n = n, valid = valid,
        valid_pct = round(100 * valid / n, 1),
        stringsAsFactors = FALSE
      ))
    }
  }

  rm(list = cycle)
}

# Cross-cycle prevalence summary
cat("\n=== CROSS-CYCLE SUMMARY ===\n")
for (v in unique(results$variable)) {
  cat("\n", v, ":\n")
  sub <- results[results$variable == v, ]
  print(sub[, c("cycle", "n", "valid", "valid_pct")], row.names = FALSE)
}

# Save results
write.csv(results, "/tmp/alcohol-pumf-integration-test.csv", row.names = FALSE)
cat("\nResults saved to /tmp/alcohol-pumf-integration-test.csv\n")
