# Integration test for DEN_132/DEN_035 using rec_with_table()
# Check Ontario vs National availability

library(dplyr)
library(here)
library(cchsflow)

# Set project root
project_root <- here::here()

# Load worksheets as data frames (from development version)
variable_details <- read.csv(file.path(project_root, "inst/extdata/variable_details.csv"), stringsAsFactors = FALSE)

# PUMF data directory
rdata_dir <- file.path(project_root, "working_data_and_documentation/pumf-rdata")

# Ontario code
ONTARIO <- 35

# Cycles to test
cycles <- c(
  "cchs2001_p", "cchs2003_p", "cchs2005_p", "cchs2007_2008_p",

  "cchs2009_2010_p", "cchs2011_2012_p", "cchs2013_2014_p",
  "cchs2015_2016_p", "cchs2017_2018_p"
)

# Map cycle names to RData files
cycle_to_file <- list(
  "cchs2001_p" = "CCHS_2001.RData",
  "cchs2003_p" = "CCHS_2003.RData",
  "cchs2005_p" = "CCHS_2005.RData",
  "cchs2007_2008_p" = "CCHS_2007_2008.RData",
  "cchs2009_2010_p" = "CCHS_2009_2010.RData",
  "cchs2011_2012_p" = "CCHS_2011_2012.RData",
  "cchs2013_2014_p" = "CCHS_2013_2014.RData",
  "cchs2015_2016_p" = "CCHS_2015_2016.RData",
  "cchs2017_2018_p" = "CCHS_2017_2018.RData"
)

# DEN variables to check (source names by cycle)
den_vars <- list(
  "cchs2001_p" = "DENA_132",
  "cchs2003_p" = "DENC_132",
  "cchs2005_p" = "DENE_132",
  "cchs2007_2008_p" = "DEN_132",
  "cchs2009_2010_p" = "DEN_132",
  "cchs2011_2012_p" = "DEN_132",
  "cchs2013_2014_p" = "DEN_132",
  "cchs2015_2016_p" = "DEN_035",
  "cchs2017_2018_p" = "DEN_035"
)

results <- data.frame(
  cycle = character(),
  den_var = character(),
  national_n = integer(),
  national_valid = integer(),
  national_pct = numeric(),
  ontario_n = integer(),
  ontario_valid = integer(),
  ontario_pct = numeric(),
  stringsAsFactors = FALSE
)

for (cycle in cycles) {
  cat("\n=== ", cycle, " ===\n")

  # Load data
  rdata_file <- file.path(rdata_dir, cycle_to_file[[cycle]])
  if (!file.exists(rdata_file)) {
    cat("  File not found:", rdata_file, "\n")
    next
  }

  load(rdata_file)
  df <- table
  rm(table)

  # Get DEN variable for this cycle
  den_var <- den_vars[[cycle]]

  if (!den_var %in% names(df)) {
    cat("  ", den_var, "not found in data\n")
    next
  }

  # Use rec_with_table to harmonize province
  cat("  Harmonizing GEOGPRV...\n")
  df_harm <- tryCatch({
    rec_with_table(
      data = df,
      variables = "GEOGPRV",
      database_name = cycle,
      variable_details = variable_details,
      append_to_data = TRUE,
      log = FALSE
    )
  }, error = function(e) {
    cat("  Error harmonizing province:", e$message, "\n")
    NULL
  })

  if (is.null(df_harm)) {
    # Fall back to checking raw province variable
    cat("  Using raw province variable\n")
    df_harm <- df
    df_harm$GEOGPRV <- NA
  }

  # Calculate stats
  national_n <- nrow(df_harm)
  # Valid = values 1-5 (response categories, not skip/DK/refused)
  national_valid <- sum(!is.na(df_harm[[den_var]]) & df_harm[[den_var]] >= 1 & df_harm[[den_var]] <= 5, na.rm = TRUE)
  national_pct <- round(100 * national_valid / national_n, 1)

  # Ontario subset
  ontario_df <- df_harm[!is.na(df_harm$GEOGPRV) & df_harm$GEOGPRV == ONTARIO, ]
  ontario_n <- nrow(ontario_df)
  ontario_valid <- sum(!is.na(ontario_df[[den_var]]) & ontario_df[[den_var]] >= 1 & ontario_df[[den_var]] <= 5, na.rm = TRUE)
  ontario_pct <- if (ontario_n > 0) round(100 * ontario_valid / ontario_n, 1) else NA

  cat("  ", den_var, ": National=", national_valid, "/", national_n, " (", national_pct, "%)",
      " | Ontario=", ontario_valid, "/", ontario_n, " (", ontario_pct, "%)\n")

  results <- rbind(results, data.frame(
    cycle = cycle,
    den_var = den_var,
    national_n = national_n,
    national_valid = national_valid,
    national_pct = national_pct,
    ontario_n = ontario_n,
    ontario_valid = ontario_valid,
    ontario_pct = ontario_pct,
    stringsAsFactors = FALSE
  ))
}

cat("\n\n=== SUMMARY ===\n")
print(results)

# Save results
write.csv(results, here::here("ceps/cep-006-oral-health/den-pumf-integration-test.csv"),
          row.names = FALSE)
cat("\nResults saved to ceps/cep-006-oral-health/den-pumf-integration-test.csv\n")
