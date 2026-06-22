#!/usr/bin/env Rscript
# Generate smk_quit_fix_variable_details.csv
#
# Purpose: Create Master-only rows for _cont smoking cessation variables.
# Master databases need continuous source variables for category 4 ("3+ years")
# instead of midpoint imputation used by PUMF.
#
# These rows should be ADDED to variable_details.csv, and the existing rows
# need _m databases (2003+) REMOVED from their databaseStart/variableStart.
# Exception: cchs2001_m stays with PUMF rows (no continuous variable in 2001).
#
# Continuous source variables by era (Master only):
#   2001: No continuous variables exist (stays midpoint)
#   2003: SMKC_06C, SMKC_09C, SMKC_10C
#   2005: SMKE_06C, SMKE_09C, SMKE_10C
#   2007-2014: SMK_06C, SMK_09C, SMK_10C
#   2015-2021: SMK_070, SMK_090, SMK_110
#
# Usage: Rscript ceps/cep-002-smoking/03-cessation/generate_smk_quit_fix.R

library(readr)

make_row <- function(variable, databaseStart, variableStart, typeStart,
                     recStart, recEnd, catLabel, catLabelLong,
                     catStartLabel, notes = "", numValidCat = 4,
                     shortLabel = "", longLabel = "") {
  data.frame(
    variable = variable,
    dummyVariable = "N/A",
    typeEnd = "cont",
    databaseStart = databaseStart,
    variableStart = variableStart,
    ICES.confirmation = "",
    typeStart = typeStart,
    recEnd = recEnd,
    numValidCat = numValidCat,
    catLabel = catLabel,
    catLabelLong = catLabelLong,
    units = "years",
    recStart = recStart,
    catStartLabel = catStartLabel,
    variableStartShortLabel = shortLabel,
    variableStartLabel = longLabel,
    notes = notes,
    version = "3.0.0-alpha",
    lastUpdated = "2026-02-21",
    status = "active",
    reviewNotes = "Master: continuous source for cat 4 (3+ years).",
    versionNotes = "",
    review = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# Standard category rows for Master (cats 1-3, 6, 7/9, else)
standard_cats <- list(
  list(recStart = "1", recEnd = "0.5", catLabel = "<1 year",
       catLabelLong = "Less than 1 year ago", catStartLabel = "Less than 1 year",
       notes = "Midpoint conversion: cat 1 -> 0.5 years"),
  list(recStart = "2", recEnd = "1.5", catLabel = "1-2 years",
       catLabelLong = "1 to less than 2 years ago", catStartLabel = "1-2 years",
       notes = "Midpoint conversion: cat 2 -> 1.5 years"),
  list(recStart = "3", recEnd = "2.5", catLabel = "2-3 years",
       catLabelLong = "2 to less than 3 years ago", catStartLabel = "2-3 years",
       notes = "Midpoint conversion: cat 3 -> 2.5 years"),
  list(recStart = "6", recEnd = "NA::a", catLabel = "Not applicable",
       catLabelLong = "Not applicable", catStartLabel = "Not applicable",
       notes = ""),
  list(recStart = "[7,9]", recEnd = "NA::b", catLabel = "Missing",
       catLabelLong = "Don't know/Refusal/Not stated", catStartLabel = "Missing",
       notes = ""),
  list(recStart = "else", recEnd = "NA::b", catLabel = "Catch-all",
       catLabelLong = "Catch-all missing", catStartLabel = "else",
       notes = "")
)

# -----------------------------------------------------------------------
# Master database groups with categorical + continuous source variables
# -----------------------------------------------------------------------

master_groups <- list(

  # === SMK_06A_cont: Former occasional smoker quit timing ===
  list(
    variable = "SMK_06A_cont",
    shortLabel = "Yrs quit (occ)",
    longLabel = "When stopped smoking - former occasional",
    groups = list(
      # 2003-2014 Master
      list(
        master_dbs = "cchs2003_m, cchs2005_m, cchs2007_2008_m, cchs2009_2010_m, cchs2011_2012_m, cchs2013_2014_m",
        master_cat_src = "cchs2003_m::SMKC_06A, cchs2005_m::SMKE_06A, [SMK_06A]",
        master_cont_src = "cchs2003_m::SMKC_06C, cchs2005_m::SMKE_06C, [SMK_06C]"
      ),
      # 2015-2021 Master
      list(
        master_dbs = "cchs2015_2016_m, cchs2017_2018_m, cchs2019_2020_m, cchs2021_m",
        master_cat_src = "cchs2015_2016_m::SMK_060, cchs2017_2018_m::SMK_060, cchs2019_2020_m::SMK_060, cchs2021_m::SMK_060",
        master_cont_src = "cchs2015_2016_m::SMK_070, cchs2017_2018_m::SMK_070, cchs2019_2020_m::SMK_070, cchs2021_m::SMK_070"
      )
    )
  ),

  # === SMK_09A_cont: Former daily smoker stopped daily timing ===
  list(
    variable = "SMK_09A_cont",
    shortLabel = "Yrs quit daily",
    longLabel = "When stopped smoking daily - former daily",
    groups = list(
      # 2003-2014 Master
      list(
        master_dbs = "cchs2003_m, cchs2005_m, cchs2007_2008_m, cchs2009_2010_m, cchs2011_2012_m, cchs2013_2014_m",
        master_cat_src = "cchs2003_m::SMKC_09A, cchs2005_m::SMKE_09A, [SMK_09A]",
        master_cont_src = "cchs2003_m::SMKC_09C, cchs2005_m::SMKE_09C, [SMK_09C]"
      ),
      # 2015-2021 Master
      list(
        master_dbs = "cchs2015_2016_m, cchs2017_2018_m, cchs2019_2020_m, cchs2021_m",
        master_cat_src = "cchs2015_2016_m::SMK_080, cchs2017_2018_m::SMK_080, cchs2019_2020_m::SMK_080, cchs2021_m::SMK_080",
        master_cont_src = "cchs2015_2016_m::SMK_090, cchs2017_2018_m::SMK_090, cchs2019_2020_m::SMK_090, cchs2021_m::SMK_090"
      )
    )
  ),

  # === SMK_10A_cont: Former daily smoker quit completely timing ===
  list(
    variable = "SMK_10A_cont",
    shortLabel = "Yrs quit complete",
    longLabel = "When quit smoking completely - former daily",
    groups = list(
      # 2003-2005 Master
      list(
        master_dbs = "cchs2003_m, cchs2005_m",
        master_cat_src = "cchs2003_m::SMKC_10A, cchs2005_m::SMKE_10A",
        master_cont_src = "cchs2003_m::SMKC_10C, cchs2005_m::SMKE_10C"
      ),
      # 2007-2014 Master
      list(
        master_dbs = "cchs2007_2008_m, cchs2009_2010_m, cchs2011_2012_m, cchs2013_2014_m",
        master_cat_src = "[SMK_10A]",
        master_cont_src = "[SMK_10C]"
      ),
      # 2015-2021 Master
      list(
        master_dbs = "cchs2015_2016_m, cchs2017_2018_m, cchs2019_2020_m, cchs2021_m",
        master_cat_src = "cchs2015_2016_m::SMK_100, cchs2017_2018_m::SMK_100, cchs2019_2020_m::SMK_100, cchs2021_m::SMK_100",
        master_cont_src = "cchs2015_2016_m::SMK_110, cchs2017_2018_m::SMK_110, cchs2019_2020_m::SMK_110, cchs2021_m::SMK_110"
      )
    )
  )
)

# -----------------------------------------------------------------------
# Generate Master rows
# -----------------------------------------------------------------------
all_rows <- list()

for (cfg in master_groups) {
  variable <- cfg$variable
  shortLabel <- cfg$shortLabel
  longLabel <- cfg$longLabel

  for (grp in cfg$groups) {
    # Cats 1-3, 6, 7/9, else: use categorical source variable
    for (cat in standard_cats) {
      all_rows[[length(all_rows) + 1]] <- make_row(
        variable = variable,
        databaseStart = grp$master_dbs,
        variableStart = grp$master_cat_src,
        typeStart = "cat",
        recStart = cat$recStart,
        recEnd = cat$recEnd,
        catLabel = cat$catLabel,
        catLabelLong = cat$catLabelLong,
        catStartLabel = cat$catStartLabel,
        notes = cat$notes,
        shortLabel = shortLabel,
        longLabel = longLabel
      )
    }

    # Cat 4: continuous pass-through from Master continuous variable
    all_rows[[length(all_rows) + 1]] <- make_row(
      variable = variable,
      databaseStart = grp$master_dbs,
      variableStart = grp$master_cont_src,
      typeStart = "cont",
      recStart = "copy",
      recEnd = "copy",
      catLabel = "3+ years (continuous)",
      catLabelLong = "Actual years from continuous source variable",
      catStartLabel = "3+ years",
      notes = "Master: pass-through from continuous source variable",
      shortLabel = shortLabel,
      longLabel = longLabel
    )
  }
}

result <- do.call(rbind, all_rows)

# Write to same directory as this script
output_path <- "ceps/cep-002-smoking/03-cessation/smk_quit_fix_variable_details.csv"
write_csv(result, output_path, na = "", quote = "needed", escape = "double", eol = "\n")

cat(sprintf("Generated %d Master-only rows for %d variables\n",
    nrow(result), length(unique(result$variable))))
cat(sprintf("Variables: %s\n", paste(unique(result$variable), collapse = ", ")))
cat(sprintf("Written to %s\n", output_path))
cat("\n--- To apply ---\n")
cat("1. Remove _m databases (2003+) from existing PUMF rows in variable_details.csv\n")
cat("2. Add these Master rows to variable_details.csv\n")
cat("3. Exception: cchs2001_m stays with PUMF rows (no continuous variable)\n")
