suppressMessages(devtools::load_all("/Users/dmanuel/github/cchsflow", quiet = TRUE))
suppressWarnings({
for (cy in c("cchs2007_2008_p","cchs2011_2012_p","cchs2015_2016_p","cchs2017_2018_p")) {
  df <- get(cy)
  # find the raw source column for ADL_01 in this cycle's data
  src <- intersect(c("ADL_01","ADLC_01","ADL_005","ADLDCLS"), names(df))
  raw_info <- if (length(src)) {
    x <- df[[src[1]]]
    sprintf("raw %s: %d/%d non-NA, values={%s}", src[1], sum(!is.na(x)), nrow(df),
            paste(head(sort(unique(x)), 8), collapse=","))
  } else sprintf("no ADL source col found; ADL-ish cols: %s",
                 paste(head(grep("ADL", names(df), value=TRUE), 5), collapse=","))
  out <- tryCatch(suppressMessages(rec_with_table(df, c("ADL_01","ADL_der"), database_name = cy)),
                  error = function(e) NULL)
  rec01 <- if (!is.null(out) && "ADL_01" %in% names(out)) {
    x <- as.character(out$ADL_01)
    sprintf("recoded ADL_01: %d valid, %d NA(a), %d NA(b)",
            sum(x %in% c("1","2")), sum(x=="NA(a)", na.rm=TRUE), sum(x=="NA(b)", na.rm=TRUE))
  } else "recode failed"
  cat(cy, "\n  ", raw_info, "\n  ", rec01, "\n")
}
})
