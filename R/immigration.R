#' @title Calculate immigration category
#'
#' @description Derives a 6-category immigration classification combining
#' immigrant status, country of birth, ethnicity (white / visible minority),
#' and recency of arrival. Works for both PUMF and master CCHS files; the
#' worksheet supplies database-appropriate feeder variables for each cycle block.
#'
#' @param immigrant_status Immigrant status: 1 = immigrant, 2 = non-immigrant
#'   (SDCFIMM; covers both PUMF and master).
#'
#' @param born_canada Country of birth: 1 = Canada, 2 = outside Canada.
#'   Use SDCGCBG for PUMF cycles; SDCGCB for master cycles.
#'
#' @param ethnicity Ethnicity: 1 = White, any value > 1 = visible minority.
#'   Use SDCGCGT (2-category) for PUMF cycles or SDCDCGT_cat7 (7-category)
#'   for master cycles — both use 1 = White, so the function handles either.
#'
#' @param years Time in Canada in years (continuous). Values < 10 are treated
#'   as recent; values >= 10 as established. Use SDCGRES_cont (midpoints 4.5
#'   or 15 derived from categorical SDCGRES) for PUMF cycles; SDCDRES (raw
#'   continuous 0-97) for master cycles. Not used for Canada-born respondents
#'   (categories 1 and 2).
#'
#' @return Integer 1-6, \code{tagged_na("a")} (not applicable), or
#'   \code{tagged_na("b")} (missing / unmatched):
#'   \itemize{
#'     \item 1 — White, Canada-born
#'     \item 2 — Visible minority, Canada-born
#'     \item 3 — White immigrant, 0-9 years in Canada
#'     \item 4 — Visible minority immigrant, 0-9 years in Canada
#'     \item 5 — White immigrant, 10+ years in Canada
#'     \item 6 — Visible minority immigrant, 10+ years in Canada
#'   }
#'
#' @examples
#' library(cchsflow)
#'
#' # PUMF usage
#' immigration2009_2010 <- rec_with_table(
#'   cchs2009_2010_p,
#'   c("SDCFIMM", "SDCGCBG", "SDCGCGT", "SDCGRES_cont", "immigration_der")
#' )
#' head(immigration2009_2010)
#'
#' # Scalar usage — PUMF style (SDCGCGT 2-cat, SDCGRES_cont midpoints 4.5/15)
#' categorize_immigration(
#'   immigrant_status = 1, born_canada = 2, ethnicity = 2, years = 4.5
#' ) # -> 4 (visible minority immigrant, recent)
#'
#' # Scalar usage — master style (SDCDCGT_cat7 7-cat, SDCDRES raw continuous)
#' categorize_immigration(
#'   immigrant_status = 1, born_canada = 2, ethnicity = 5, years = 15
#' ) # -> 6 (visible minority immigrant, established)
#'
#' @export
categorize_immigration <- function(immigrant_status, born_canada, ethnicity,
                                  years) {
  dplyr::case_when(
    # Canada-born (non-immigrant): years not consulted
    immigrant_status == 2 & born_canada == 1 & ethnicity == 1 ~ 1L,
    immigrant_status == 2 & born_canada == 1 & ethnicity >  1 ~ 2L,
    # Immigrant, born outside Canada, recent (< 10 years)
    immigrant_status == 1 & born_canada == 2 & ethnicity == 1 & years <  10 ~ 3L,
    immigrant_status == 1 & born_canada == 2 & ethnicity >  1 & years <  10 ~ 4L,
    # Immigrant, born outside Canada, established (10+ years)
    immigrant_status == 1 & born_canada == 2 & ethnicity == 1 & years >= 10 ~ 5L,
    immigrant_status == 1 & born_canada == 2 & ethnicity >  1 & years >= 10 ~ 6L,
    # NA(a) propagation
    haven::is_tagged_na(immigrant_status, "a") |
      haven::is_tagged_na(born_canada,       "a") |
      haven::is_tagged_na(ethnicity,          "a") |
      haven::is_tagged_na(years,             "a") ~ tagged_na("a"),
    TRUE ~ tagged_na("b")
  )
}
