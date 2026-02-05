# Check DEN_132 and DEN_035 availability in actual PUMF data
# Also check Ontario vs National availability

library(dplyr)

rdata_dir <- here::here("working_data_and_documentation/pumf-rdata")

# Province variable names by cycle
prov_vars <- list(
  "CCHS_2001" = "GEOAGPRV",
  "CCHS_2003" = "GEOGPRV",
  "CCHS_2005" = "GEOGPRV",
  "CCHS_2007_2008" = "GEOGPRV",
  "CCHS_2009_2010" = "GEOGPRV",
  "CCHS_2011_2012" = "GEOGPRV",
  "CCHS_2013_2014" = "GEOGPRV",
  "CCHS_2015_2016" = "GEO_PRV",
  "CCHS_2017_2018" = "GEO_PRV",
  "CCHS_2019_2020" = "GEO_PRV",
  "CCHS_2022" = "GEO_PRV"
)

# Ontario code
ONTARIO <- 35

results <- data.frame(
  cycle = character(),
  den_var = character(),
  national_n = integer(),
  national_valid = integer(),
  ontario_n = integer(),
  ontario_valid = integer(),
  stringsAsFactors = FALSE
)

files <- list.files(rdata_dir, pattern = "\\.RData$", full.names = TRUE)

for (f in files) {
  cycle_name <- gsub("\\.RData$", "", basename(f))
  cat("\n=== ", cycle_name, " ===\n")

  load(f)

  # The RData files use "table" as the object name
  if (!exists("table")) {
    cat("  No 'table' object found\n")
    next
  }

  df <- table
  cols <- names(df)

  # Find DEN variables
  den_cols <- cols[grepl("^DEN_|^DENA_|^DENC_|^DENE_", cols, ignore.case = FALSE)]

  if (length(den_cols) == 0) {
    cat("  No DEN variables found\n")
    cat("  All columns starting with DEN: ",
        paste(cols[grepl("^DEN", cols, ignore.case = TRUE)], collapse = ", "), "\n")
  } else {
    cat("  DEN variables: ", paste(den_cols, collapse = ", "), "\n")

    # Check province variable
    prov_var <- prov_vars[[cycle_name]]
    has_prov <- prov_var %in% cols

    for (dv in den_cols) {
      # Focus on the dental visit variables
      if (!grepl("132|035", dv)) next

      national_n <- nrow(df)
      national_valid <- sum(!is.na(df[[dv]]) & df[[dv]] < 6, na.rm = TRUE)

      if (has_prov) {
        ont_df <- df[df[[prov_var]] == ONTARIO, ]
        ontario_n <- nrow(ont_df)
        ontario_valid <- sum(!is.na(ont_df[[dv]]) & ont_df[[dv]] < 6, na.rm = TRUE)
      } else {
        ontario_n <- NA
        ontario_valid <- NA
      }

      cat("  ", dv, ": National valid=", national_valid, "/", national_n,
          " | Ontario valid=", ontario_valid, "/", ontario_n, "\n")

      results <- rbind(results, data.frame(
        cycle = cycle_name,
        den_var = dv,
        national_n = national_n,
        national_valid = national_valid,
        ontario_n = ontario_n,
        ontario_valid = ontario_valid,
        stringsAsFactors = FALSE
      ))
    }
  }

  # Clean up
  rm(table)
}

cat("\n\n=== SUMMARY ===\n")
print(results)

# Save results
write.csv(results, here::here("ceps/cep-006-oral-health/den-variable-availability.csv"),
          row.names = FALSE)
cat("\nResults saved to ceps/cep-006-oral-health/den-variable-availability.csv\n")
