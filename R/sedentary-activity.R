#' @title Total hours - sedentary activity (age 20+)
#'
#' @description Calculates total hours spent doing sedentary activities:
#'  computer use, video games, watching TV/videos, and reading.
#'  Time spent at school or work is excluded. Limited to ages 20+.
#'
#' @param age Continuous age (DHHGAGE_cont for PUMF, DHH_AGE for Master)
#' @param SAC_1 Hours on computer
#' @param SAC_2 Hours playing video games
#' @param SAC_3 Hours watching TV or videos
#' @param SAC_4 Hours reading
#'
#' @return Total sedentary hours for age 20+, or tagged NA
#'
#' @examples
#' sedentary_activity_fun(25, 5, 3, 10, 7)  # 25 hours
#' sedentary_activity_fun(18, 5, 3, 10, 7)  # NA(a) - under 20
#'
#' @export
sedentary_activity_fun <- function(age, SAC_1, SAC_2, SAC_3, SAC_4) {
  if_else2(!is.na(SAC_1) & !is.na(SAC_2) & !is.na(SAC_3) &
           !is.na(SAC_4) & age >= 20,
    SAC_1 + SAC_2 + SAC_3 + SAC_4,
    if_else2(!is.na(SAC_1) & !is.na(SAC_2) & !is.na(SAC_3) &
             !is.na(SAC_4) & age < 20, tagged_na("a"),
      if_else2(SAC_1 == "NA(a)" | SAC_2 == "NA(a)" |
               SAC_3 == "NA(a)" | SAC_4 == "NA(a)",
        tagged_na("a"), tagged_na("b"))))
}

#' @title Total hours - sedentary activity excluding reading (age 20+)
#'
#' @description Calculates total hours spent doing sedentary activities
#'  excluding reading: computer use, video games, and watching TV/videos.
#'  Time spent at school or work is excluded. Limited to ages 20+.
#'
#' @param age Continuous age (DHHGAGE_cont for PUMF, DHH_AGE for Master)
#' @param SAC_1 Hours on computer
#' @param SAC_2 Hours playing video games
#' @param SAC_3 Hours watching TV or videos
#'
#' @return Total sedentary hours (excl. reading) for age 20+, or tagged NA
#'
#' @examples
#' sedentary_activity2_fun(25, 5, 3, 10)  # 18 hours
#' sedentary_activity2_fun(18, 5, 3, 10)  # NA(a) - under 20
#'
#' @export
sedentary_activity2_fun <- function(age, SAC_1, SAC_2, SAC_3) {
  if_else2(!is.na(SAC_1) & !is.na(SAC_2) & !is.na(SAC_3) & age >= 20,
    SAC_1 + SAC_2 + SAC_3,
    if_else2(!is.na(SAC_1) & !is.na(SAC_2) & !is.na(SAC_3) &
             age < 20, tagged_na("a"),
      if_else2(SAC_1 == "NA(a)" | SAC_2 == "NA(a)" |
               SAC_3 == "NA(a)",
        tagged_na("a"), tagged_na("b"))))
}

#' @title Weekly leisure screen time
#'
#' @description Calculates total weekly hours of screen time outside of
#'  school and/or work from daily screen time.
#'
#' @param SBE_010 Daily screen time (hours/day) outside of school and work
#'
#' @return Weekly screen time (hours/week) or tagged NA
#'
#' @examples
#' weekly_screen_time_fun(2)  # 14 hours/week
#' weekly_screen_time_fun(0)  # 0
#'
#' @export
weekly_screen_time_fun <- function(SBE_010) {
  if_else2(!is.na(SBE_010), SBE_010 * 7,
    if_else2(SBE_010 == "NA(a)",
      tagged_na("a"), tagged_na("b")))
}

#' @title Weekly leisure screen time - adult (age 20+)
#'
#' @description Calculates total weekly screen time outside of school/work
#'  for respondents aged 20 and over. Returns NA(a) for age under 20.
#'
#' @param age Continuous age (DHHGAGE_cont for PUMF, DHH_AGE for Master)
#' @param SBE_010 Daily screen time (hours/day) outside of school and work
#'
#' @return Weekly screen time (hours/week) for age 20+, or tagged NA
#'
#' @export
weekly_screen_time_adult_fun <- function(age, SBE_010) {
  if_else2(!is.na(SBE_010) & age >= 20, SBE_010 * 7,
    if_else2(!is.na(SBE_010) & age < 20, tagged_na("a"),
      if_else2(SBE_010 == "NA(a)",
        tagged_na("a"), tagged_na("b"))))
}

#' @title Weekly leisure screen time - youth (age under 20)
#'
#' @description Calculates total weekly screen time outside of school/work
#'  for respondents aged under 20. Returns NA(a) for age 20 and over.
#'
#' @param age Continuous age (DHHGAGE_cont for PUMF, DHH_AGE for Master)
#' @param SBE_010 Daily screen time (hours/day) outside of school and work
#'
#' @return Weekly screen time (hours/week) for age under 20, or tagged NA
#'
#' @export
weekly_screen_time_youth_fun <- function(age, SBE_010) {
  if_else2(!is.na(SBE_010) & age < 20, SBE_010 * 7,
    if_else2(!is.na(SBE_010) & age >= 20, tagged_na("a"),
      if_else2(SBE_010 == "NA(a)",
        tagged_na("a"), tagged_na("b"))))
}
