# PR #169 Integration test: GEN_07 and GEN_10
# Tests PUMF data only (master changes cannot be tested locally)

library(cchsflow)
library(dplyr)

variable_details <- read.csv("inst/extdata/variable_details.csv",
                             stringsAsFactors = FALSE)

variables_to_test <- c("GEN_07", "GEN_10")
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
    for (v in variables_to_test) {
      if (v %in% names(result)) {
        valid <- sum(!is.na(result[[v]]))
        cat(cycle, v, ": valid =", valid, "/", n,
            "(", round(100 * valid / n, 1), "%)\n")

        freq <- table(result[[v]], useNA = "ifany")
        print(freq)

        results <- rbind(results, data.frame(
          cycle = cycle, variable = v,
          n = n, valid = valid,
          valid_pct = round(100 * valid / n, 1),
          stringsAsFactors = FALSE
        ))
      } else {
        cat(cycle, v, ": NOT IN OUTPUT\n")
      }
    }
  }

  rm(list = cycle)
}

cat("\n=== CROSS-CYCLE SUMMARY ===\n")
for (v in unique(results$variable)) {
  cat("\n", v, ":\n")
  sub <- results[results$variable == v, ]
  print(sub[, c("cycle", "n", "valid", "valid_pct")], row.names = FALSE)
}

write.csv(results, "/tmp/gen-health-pumf-integration-test.csv",
          row.names = FALSE)
cat("\nResults saved to /tmp/gen-health-pumf-integration-test.csv\n")
