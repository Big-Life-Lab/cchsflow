library(cchsflow)
library(dplyr)

variable_details <- read.csv("inst/extdata/variable_details.csv", stringsAsFactors = FALSE)

# Test PAC_4A/4B family (2001-2005 PUMF)
variables_era1 <- c("PAC_4A", "PAC_4A_cont", "PAC_4B", "PAC_4B_cont")
cycles_era1 <- c("cchs2001_p", "cchs2003_p", "cchs2005_p")

# Test PAC_7/8 family (2007-2014 PUMF)
variables_era2 <- c("PAC_7", "PAC_7A", "PAC_7B", "PAC_7B_cont", "PAC_8", "PAC_8A", "PAC_8B", "PAC_8B_cont")
cycles_era2 <- c("cchs2007_2008_p", "cchs2009_2010_p", "cchs2010_p", "cchs2011_2012_p", "cchs2012_p", "cchs2013_2014_p", "cchs2014_p")

# Test active_transport (all eras with PUMF data)
variables_dv <- c("active_transport")
cycles_dv <- c("cchs2001_p", "cchs2003_p", "cchs2005_p",
               "cchs2007_2008_p", "cchs2009_2010_p", "cchs2010_p",
               "cchs2011_2012_p", "cchs2012_p", "cchs2013_2014_p", "cchs2014_p",
               "cchs2015_2016_p", "cchs2017_2018_p")

results <- data.frame()

run_test <- function(variables, cycles, era_label) {
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
        variables = variables,
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
        cat(era_label, cycle, v, ": valid =", valid, "/", n,
            "(", round(100 * valid / n, 1), "%)\n")
        
        if (v %in% c("PAC_4A", "PAC_7", "PAC_7B", "PAC_8", "PAC_8B")) {
          freq <- table(result[[v]], useNA = "ifany")
          cat("  Distribution:", paste(names(freq), freq, sep="=", collapse=", "), "\n")
        }
        
        results <<- rbind(results, data.frame(
          era = era_label, cycle = cycle, variable = v,
          n = n, valid = valid,
          valid_pct = round(100 * valid / n, 1),
          stringsAsFactors = FALSE
        ))
      }
    }
    rm(list = cycle)
  }
}

cat("=== ERA 1: PAC_4A/4B (2001-2005) ===\n")
run_test(variables_era1, cycles_era1, "ERA1")

cat("\n=== ERA 2: PAC_7/8 (2007-2014) ===\n")
run_test(variables_era2, cycles_era2, "ERA2")

cat("\n=== DERIVED: active_transport (all eras) ===\n")
run_test(variables_dv, cycles_dv, "DV")

cat("\n=== CROSS-CYCLE SUMMARY ===\n")
for (v in unique(results$variable)) {
  cat("\n", v, ":\n")
  sub <- results[results$variable == v, ]
  print(sub[, c("cycle", "n", "valid", "valid_pct")], row.names = FALSE)
}

write.csv(results, "/tmp/active-transport-integration-test.csv", row.names = FALSE)
