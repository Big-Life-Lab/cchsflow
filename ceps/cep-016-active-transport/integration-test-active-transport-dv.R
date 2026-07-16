library(cchsflow)
suppressPackageStartupMessages(library(dplyr))

variable_details <- read.csv("inst/extdata/variable_details.csv", stringsAsFactors = FALSE)

# All variables together so feeders get resolved
all_vars <- c("PAC_4A", "PAC_4A_cont", "PAC_4B", "PAC_4B_cont",
              "PAC_7", "PAC_7A", "PAC_7B", "PAC_7B_cont",
              "PAC_8", "PAC_8A", "PAC_8B", "PAC_8B_cont",
              "active_transport")

cycles <- c("cchs2001_p", "cchs2003_p", "cchs2005_p",
            "cchs2007_2008_p", "cchs2009_2010_p", "cchs2010_p",
            "cchs2011_2012_p", "cchs2012_p", "cchs2013_2014_p", "cchs2014_p",
            "cchs2015_2016_p", "cchs2017_2018_p")

results <- data.frame()

for (cycle in cycles) {
  rdata_file <- file.path("data", paste0(cycle, ".RData"))
  if (!file.exists(rdata_file)) { next }
  load(rdata_file)
  df <- get(cycle)
  
  result <- tryCatch(
    suppressWarnings(rec_with_table(
      data = df, variables = all_vars,
      database_name = cycle, variable_details = variable_details, log = FALSE
    )),
    error = function(e) { cat("ERROR in", cycle, ":", e$message, "\n"); NULL }
  )
  
  if (!is.null(result) && "active_transport" %in% names(result)) {
    n <- nrow(result)
    valid <- sum(!is.na(result$active_transport))
    cat(cycle, ": active_transport valid =", valid, "/", n,
        "(", round(100 * valid / n, 1), "%)\n")
    
    # Distribution summary for continuous DV
    if (valid > 0) {
      vals <- result$active_transport[!is.na(result$active_transport)]
      cat("  min=", min(vals), " median=", median(vals), " mean=", round(mean(vals),1),
          " max=", max(vals), "\n")
    }
    results <- rbind(results, data.frame(cycle=cycle, n=n, valid=valid,
                                          valid_pct=round(100*valid/n, 1)))
  } else if (!is.null(result)) {
    cat(cycle, ": active_transport NOT in result columns\n")
    cat("  Available:", paste(names(result), collapse=", "), "\n")
  }
  rm(list = cycle)
}

cat("\n=== CROSS-CYCLE active_transport ===\n")
print(results, row.names = FALSE)
