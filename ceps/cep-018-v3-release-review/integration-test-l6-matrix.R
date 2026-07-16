suppressMessages(devtools::load_all("/Users/dmanuel/github/cchsflow", quiet = TRUE))
suppressWarnings({

# --- #139 repro: categorize_immigration(non-immigrant, born outside Canada, white) ---
im <- tryCatch(categorize_immigration(2, 2, 1), error = function(e) paste("ERR:", conditionMessage(e)))
cat("#139 categorize_immigration(2,2,1):", format(im),
    if (is.numeric(im) && haven::is_tagged_na(im, "b")) "-> STILL NA::b (unfixed)" else "-> classified/other\n", "\n")

# --- L6 matrix ---
vd <- read.csv("inst/extdata/variable_details.csv", stringsAsFactors = FALSE)
known <- unique(vd$variable)
vars_l6 <- intersect(c(
  "sedentary_der","SBE_005","ALCDTTM_former","DHH_OWN","CCCG102_2005plus",
  "ADL_der","ADL_score_5","binge_drinker","pack_years_der","HWTGBMI_der",
  "SLP_10","INCGHH_cont","SMKDVSTY","SMKDGSTP_cont","HUIDVHSI","DEN_132","FSCDVHFS"
), known)
cat("L6 variables (", length(vars_l6), "):", paste(vars_l6, collapse=", "), "\n\n")

cycles <- paste0("cchs", c("2001","2003","2005","2007_2008","2009_2010",
                           "2011_2012","2013_2014","2015_2016","2017_2018"), "_p")
res <- list()
for (cy in cycles) {
  df <- tryCatch(get(cy), error = function(e) NULL)
  if (is.null(df)) next
  out <- tryCatch(
    suppressMessages(rec_with_table(df, vars_l6, database_name = cy)),
    error = function(e) conditionMessage(e)
  )
  if (is.character(out)) { res[[cy]] <- paste("ERROR:", substr(out, 1, 70)); next }
  pct <- sapply(vars_l6, function(v) {
    if (!v %in% names(out)) return(NA_real_)
    x <- out[[v]]
    if (is.factor(x)) x <- as.character(x)
    valid <- if (is.numeric(x)) sum(!is.na(x)) else sum(!is.na(x) & !grepl("^NA\\(", x))
    round(100 * valid / nrow(out), 1)
  })
  res[[cy]] <- pct
}
ok <- res[!sapply(res, is.character)]
if (length(ok)) {
  mat <- do.call(rbind, ok); rownames(mat) <- names(ok)
  cat("=== L6 valid% matrix ===\n"); print(mat)
}
for (e in names(res)[sapply(res, is.character)]) cat("CYCLE ERROR", e, ":", res[[e]], "\n")
})
