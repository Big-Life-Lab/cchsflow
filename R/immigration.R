#' @title Categorize immigration status
#'
#' @description Derives an 8-category immigration classification combining
#'  immigrant status, country of birth, ethnicity (white / visible minority),
#'  and recency of arrival. Works for both PUMF and Master CCHS files; the
#'  worksheet supplies database-appropriate feeder variables for each cycle block.
#'
#' @param immigrant_status Immigrant status: 1 = immigrant, 2 = non-immigrant
#'   (SDCFIMM; covers both PUMF and Master).
#' @param born_canada Country of birth: 1 = Canada, 2 = outside Canada.
#'   Use SDCGCBG for PUMF cycles; SDCGCB for Master cycles.
#' @param ethnicity Ethnicity: 1 = White, any value > 1 = visible minority.
#'   Use SDCGCGT (2-category) for PUMF cycles or SDCDCGT_cat7 (7-category)
#'   for Master cycles — both use 1 = White, so the function handles either.
#' @param years Time in Canada in years (continuous). Values < 10 are treated
#'   as recent; values >= 10 as established. Use SDCGRES_cont (midpoints 4.5
#'   or 15 derived from categorical SDCGRES) for PUMF cycles; SDCDRES (raw
#'   continuous 0-97) for Master cycles. Not used for Canada-born respondents.
#' @param output_format Output missing data format: "tagged_na" (default) or "original".
#'
#' @return Integer 1-8, or tagged NA:
#'   \itemize{
#'     \item 1 — White, Canada-born
#'     \item 2 — Visible minority, Canada-born
#'     \item 3 — White immigrant, 0-9 years in Canada
#'     \item 4 — Visible minority immigrant, 0-9 years in Canada
#'     \item 5 — White immigrant, 10+ years in Canada
#'     \item 6 — Visible minority immigrant, 10+ years in Canada
#'     \item 7 — White, non-immigrant born outside Canada
#'     \item 8 — Visible minority, non-immigrant born outside Canada
#'   }
#'
#' @examples
#' # PUMF style
#' categorize_immigration(1, 2, 2, 4.5)  # -> 4 (VM immigrant, recent)
#'
#' # Master style
#' categorize_immigration(1, 2, 5, 15)   # -> 6 (VM immigrant, established)
#'
#' # Non-immigrant born outside Canada
#' categorize_immigration(2, 2, 1, haven::tagged_na("a"))  # -> 7
#'
#' @export
categorize_immigration <- function(immigrant_status, born_canada, ethnicity,
                                   years, output_format = "tagged_na") {
  # Note: ethnicity intentionally NOT cleaned via clean_variables() because
  # it can be either PUMF 2-cat or Master 7-cat — metadata-driven range
  # validation would reject valid Master values. Missing detection uses
  # direct tagged_na checks instead.

  suppressWarnings({
    immigrant_status <- as.numeric(immigrant_status)
    born_canada <- as.numeric(born_canada)
    ethnicity <- as.numeric(ethnicity)
    years <- as.numeric(years)
  })

  result <- dplyr::case_when(
    # Canada-born (non-immigrant): years not consulted
    immigrant_status == 2 & born_canada == 1 & ethnicity == 1 ~ 1L,
    immigrant_status == 2 & born_canada == 1 & ethnicity >  1 ~ 2L,
    # Immigrant, born outside Canada, recent (< 10 years)
    immigrant_status == 1 & born_canada == 2 & ethnicity == 1 & years <  10 ~ 3L,
    immigrant_status == 1 & born_canada == 2 & ethnicity >  1 & years <  10 ~ 4L,
    # Immigrant, born outside Canada, established (10+ years)
    immigrant_status == 1 & born_canada == 2 & ethnicity == 1 & years >= 10 ~ 5L,
    immigrant_status == 1 & born_canada == 2 & ethnicity >  1 & years >= 10 ~ 6L,
    # Non-immigrant, born outside Canada
    immigrant_status == 2 & born_canada == 2 & ethnicity == 1 ~ 7L,
    immigrant_status == 2 & born_canada == 2 & ethnicity >  1 ~ 8L,
    # NA(a) propagation
    haven::is_tagged_na(immigrant_status, "a") |
      haven::is_tagged_na(born_canada, "a") |
      haven::is_tagged_na(ethnicity, "a") |
      haven::is_tagged_na(years, "a") ~ haven::tagged_na("a"),
    TRUE ~ haven::tagged_na("b")
  )
  prep_cat_output(result)
}
