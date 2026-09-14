#' @title Categorize immigration status
#'
#' @description
#' Derives an 8-category immigration classification combining immigrant
#' status, country of birth, ethnicity (not a visible minority / visible
#' minority), and recency of arrival.
#'
#' @details
#' Works for both PUMF and Master CCHS files; the worksheet supplies
#' database-appropriate feeder variables for each cycle block. Ethnicity
#' uses 1 = not a visible minority regardless of source granularity (PUMF
#' 2-category or Master 7-category). For pre-2019 cycles, category 1
#' corresponds to "White"; for 2019+ cycles, SDCDVVM code 13 ("Not a
#' visible minority") includes White, Indigenous, and some multiple
#' population-group responses. Years in Canada splits immigrants into
#' recent (< 10 years) and established (>= 10 years); this parameter is
#' not used for Canada-born respondents. Ethnicity is intentionally not
#' cleaned via \code{clean_variables()} because it can be either the PUMF
#' 2-category or Master 7-category variable.
#'
#' @param immigrant_status Immigrant status (1 = immigrant,
#'   2 = non-immigrant).
#' @param born_canada Country of birth (1 = Canada, 2 = outside Canada).
#' @param ethnicity Ethnicity (1 = not a visible minority, > 1 = visible
#'   minority). Pre-2019: 1 = White. 2019+: 1 = not a visible minority
#'   (includes White and Indigenous).
#' @param years Time in Canada in years (continuous). Values < 10 are
#'   recent; values >= 10 are established.
#' @param output_format Output missing data format: "tagged_na" (default)
#'   or "original".
#'
#' @return Integer vector: 1 = not a visible minority, Canada-born;
#'   2 = visible minority, Canada-born; 3 = not a visible minority,
#'   immigrant, recent; 4 = visible minority, immigrant, recent;
#'   5 = not a visible minority, immigrant, established; 6 = visible
#'   minority, immigrant, established; 7 = not a visible minority,
#'   non-immigrant, foreign-born; 8 = visible minority, non-immigrant,
#'   foreign-born. Missing data: \code{haven::tagged_na("a")} (not
#'   applicable), \code{haven::tagged_na("b")} (not stated/invalid).
#'
#' @examples
#' # Scalar
#' categorize_immigration(1, 2, 2, 4.5)  # 4 (VM immigrant, recent)
#' categorize_immigration(2, 1, 1, 0)    # 1 (not a visible minority, Canada-born)
#'
#' @seealso \code{\link{calculate_pct_time}}
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
    immigrant_status == 2 & born_canada == 1 & ethnicity == 1 ~ 1L,  # not VM
    immigrant_status == 2 & born_canada == 1 & ethnicity >  1 ~ 2L,  # VM
    # Immigrant, born outside Canada, recent (< 10 years)
    immigrant_status == 1 & born_canada == 2 & ethnicity == 1 & years <  10 ~ 3L,
    immigrant_status == 1 & born_canada == 2 & ethnicity >  1 & years <  10 ~ 4L,
    # Immigrant, born outside Canada, established (10+ years)
    immigrant_status == 1 & born_canada == 2 & ethnicity == 1 & years >= 10 ~ 5L,
    immigrant_status == 1 & born_canada == 2 & ethnicity >  1 & years >= 10 ~ 6L,
    # Non-immigrant, born outside Canada
    immigrant_status == 2 & born_canada == 2 & ethnicity == 1 ~ 7L,  # not VM
    immigrant_status == 2 & born_canada == 2 & ethnicity >  1 ~ 8L,  # VM
    # NA(a) propagation
    haven::is_tagged_na(immigrant_status, "a") |
      haven::is_tagged_na(born_canada, "a") |
      haven::is_tagged_na(ethnicity, "a") |
      haven::is_tagged_na(years, "a") ~ haven::tagged_na("a"),
    TRUE ~ haven::tagged_na("b")
  )
  prep_cat_output(result)
}
