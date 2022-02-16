#' @title Weekly leisure screen time
#' 
#' @description This function creates a derived variable (weekly_screen)
#'  that calculates the total hours in a week spent using a screen outside of 
#'  school and/or work.
#'  
#' @param SBE_010 Daily screen time (hours/day) outside of school and work day for
#' CCHS 2017-2018
#' 
#' @return value for screen time (hours/week) outside of school and work day
#' 
#' @examples 
#' # Using weekly_screen_time_fun() to create values for weekly screen time 
#' # across CCHS cycles.
#' # weekly_screen_time_fun() is specified in variable_details.csv along with 
#' # the CCHS variables and cycles included.
#'
#' # To transform weekly_screen across cycles, use rec_with_table() for each
#' # CCHS cycle and specify weekly_screen, along with the daily screen time
#' # variable.Then by using merge_rec_data(), you can combine 
#' # weekly_screen across cycles.
#' 
#' library(cchsflow)
#' 
#' weekly_st2017_2018 <- rec_with_table(
#'   cchs2017_2018_p, c(
#'     "SBE_010", "weekly_screen"
#'   )
#' )
#'
#' head(weekly_st2017_2018)
#' tail(weekly_st2017_2018)
#' @export

weekly_screen_time_fun <-
  function(SBE_010) {
    if_else2(!is.na(SBE_010), SBE_010*7, 
             if_else2(SBE_010 == "NA(a)", 
                      tagged_na("a"), tagged_na("b")))
}

#' @title Weekly leisure screen time (age 20+)
#' 
#' @description This function creates a derived variable (weekly_screen_A)
#'  that calculates the total hours in a week spent using a screen outside of 
#'  school and/or work. This is limited to age 20+.
#'  
#' @param DHHGAGE_cont Continuous age
#' 
#' @param SBE_010 Daily screen time (hours/day) outside of school and work day for
#' CCHS 2017-2018
#' 
#' @return value for screen time (hours/week) outside of school and work day
#' 
#' @examples 
#' # Using weekly_screen_time_age1_fun() to create values for weekly screen time 
#' # across CCHS cycles.
#' # weekly_screen_time_age1_fun() is specified in variable_details.csv along with 
#' # the CCHS variables and cycles included.
#'
#' # To transform weekly_screen_A, use rec_with_table() for each
#' # CCHS cycle and specify weekly_screen_A, along with the daily screen time
#' # and continuous age variable.Then by using merge_rec_data(), you can combine 
#' # weekly_screen_A across cycles.
#' 
#' library(cchsflow)
#' 
#' weekly_st_A_2017_2018 <- rec_with_table(
#'   cchs2017_2018_p, c(
#'     "DHHGAGE_cont",SBE_010", "weekly_screen_A"
#'   )
#' )
#'
#' head(weekly_st_A_2017_2018)
#' tail(weekly_st_A_2017_2018)
#' @export

weekly_screen_time_age1_fun <-
  function(DHHGAGE_cont, SBE_010) {
    if_else2(!is.na(SBE_010) & DHHGAGE_cont >=20, SBE_010*7, 
             if_else2(SBE_010 == "NA(a)", 
                      tagged_na("a"), tagged_na("b")))
  }

#' @title Weekly leisure screen time (age <20)
#' 
#' @description This function creates a derived variable (weekly_screen_B)
#'  that calculates the total hours in a week spent using a screen outside of 
#'  school and/or work. This is limited to age less than 20.
#'  
#' @param DHHGAGE_cont Continuous age
#' 
#' @param SBE_010 Daily screen time (hours/day) outside of school and work day for
#' CCHS 2017-2018
#' 
#' @return value for screen time (hours/week) outside of school and work day
#' 
#' @examples 
#' # Using weekly_screen_time_age2_fun() to create values for weekly screen time 
#' # across CCHS cycles.
#' # weekly_screen_time_age2_fun() is specified in variable_details.csv along with 
#' # the CCHS variables and cycles included.
#'
#' # To transform weekly_screen_B, use rec_with_table() for each
#' # CCHS cycle and specify weekly_screen_B, along with the daily screen time
#' # and continuous age variable.Then by using merge_rec_data(), you can combine 
#' # weekly_screen_B across cycles.
#' 
#' library(cchsflow)
#' 
#' weekly_st_B_2017_2018 <- rec_with_table(
#'   cchs2017_2018_p, c(
#'     "DHHGAGE_cont",SBE_010", "weekly_screen_B"
#'   )
#' )
#'
#' head(weekly_st_B_2017_2018)
#' tail(weekly_st_B_2017_2018)
#' @export
#' 
weekly_screen_time_age2_fun <-
  function(DHHGAGE_cont, SBE_010) {
    if_else2(!is.na(SBE_010) & DHHGAGE_cont <20, SBE_010*7, 
             if_else2(SBE_010 == "NA(a)", 
                      tagged_na("a"), tagged_na("b")))
  }

#' @title Video games (physical and non-physical activity)
#' 
#' @description This function creates a derived variable (SAC_2D)
#'  that calculates the total hours in a week spent playing video games (both
#'  physical and non-physical activity). 
#'  
#' @param SAC_2D_A No./hours in past week playing video games - physical 
#' activity for CCHS 2015-2016
#' 
#' @param SAC_2D_B No./hours in past week playing video games - non-physical 
#' activity for CCHS 2015-2016
#' 
#' @return value for hours/week playing video games
#' 
#' @examples 
#' # Using video_game_fun() to create values for hours/week playing video games 
#' # across CCHS cycles.
#' # video_game_fun() is specified in variable_details.csv along with 
#' # the CCHS variables and cycles included.
#'
#' # To transform SAC_2D, use rec_with_table() for each
#' # CCHS cycle and specify SAC_2D, along with SAC_2D_A and SAC_2D_B
#' # variables.Then by using merge_rec_data(), you can combine 
#' # SAC_2D for CCHS 2015-2016.
#' 
#' library(cchsflow)
#' 
#' vg2015_2016 <- rec_with_table(
#'   cchs2015_2016_p, c(
#'     "SAC_2D_A","SAC_2D_B", "SAC_2D"
#'   )
#' )
#'
#' head(vg2015_2016)
#' tail(vg2015_2016)
#' @export

video_game_fun <-
  function(SAC_2D_A, SAC_2D_B){
    if_else2(!is.na(SAC_2D_A) & !is.na(SAC_2D_B),
             SAC_2D_A + SAC_2D_B,
             if_else2(SAC_2D_A == "NA(a)"| SAC_2D_B == "NA(a)",
                      tagged_na("a"), tagged_na("b")))
  }

#' @title Time spent playing video games (age 20+)
#' 
#' @description This function creates a derived variable (SAC_2_A)
#'  that calculates the total hours in a week spent playing video games for age
#'  20+. 
#'  
#' @param DHHGAGE_cont Continuous age
#' 
#' @param SAC_2D No./hours in past week playing video games for CCHS 2015-2016
#' 
#' @return value for hours/week playing video games
#' 
#' @examples 
#' # Using video_game_age1_fun() to create values for hours/week playing video 
#' # games across CCHS cycles.
#' # video_game_age1_fun() is specified in variable_details.csv along with 
#' # the CCHS variables and cycles included.
#'
#' # To transform SAC_2_A across cycles, use rec_with_table() for each
#' # CCHS cycle and specify SAC_2_A, along with the required variables
#' # variables. For CCHS 2009-2014, only DHHGAGE_cont and SAC_2_A need to be 
#' # specified in the transformation. For CCHS 2015-2016, DHHGAGE_cont, 
#' # SAC_2_A, and the associated variables for SAC_2D need to be 
#' # specified in the transformation. Then by using merge_rec_data(), you can 
#' # combine SAC_2_A across cycles.
#' 
#' library(cchsflow)
#' 
#' vg_A_2013_2014 <- rec_with_table(
#'   cchs2013_2014_p, c(
#'     "DHHGAGE_cont", "SAC_2_A"
#'   )
#' )
#' 
#' head(vg_A_2013_2014)
#' 
#' vg_A_2015_2016 <- rec_with_table(
#'   cchs2015_2016_p, c(
#'     "SAC_2D_A","SAC_2D_B", "SAC_2D", "DHHGAGE_cont", "SAC_2_A"
#'   )
#' )
#'
#' tail(vg_A_2015_2016)
#' 
#' combined_vg_A <- suppressWarnings(merge_rec_data(vg_A_2013_2014,
#' vg_A_2015_2016))
#'
#' head(combined_vg_A)
#' tail(combined_vg_A)
#' @export

video_game_age1_fun <- function(DHHGAGE_cont, SAC_2D){
  if_else2(DHHGAGE_cont >=20, SAC_2D, if_else2(SAC_2D == "NA(a)",
                                               tagged_na("a"), tagged_na("b")))
}

#' @title Time spent playing video games (age <20)
#' 
#' @description This function creates a derived variable (SAC_2_B)
#'  that calculates the total hours in a week spent playing video games for age
#'  less than 20. 
#'  
#' @param DHHGAGE_cont Continuous age
#' 
#' @param SAC_2D No./hours in past week playing video games for CCHS 2015-2016
#' 
#' @return value for hours/week playing video games
#' 
#' @examples 
#' # Using video_game_age2_fun() to create values for hours/week playing video 
#' # games across CCHS cycles.
#' # video_game_age2_fun() is specified in variable_details.csv along with 
#' # the CCHS variables and cycles included.
#'
#' # To transform SAC_2_B across cycles, use rec_with_table() for each
#' # CCHS cycle and specify SAC_2_B, along with the required variables
#' # variables. For CCHS 2009-2014, only DHHGAGE_cont and SAC_2_B need to be 
#' # specified in the transformation. For CCHS 2015-2016, DHHGAGE_cont, 
#' # SAC_2_B, and the associated variables for SAC_2D need to be 
#' # specified in the transformation. Then by using merge_rec_data(), you can 
#' # combine SAC_2_B across cycles.
#' 
#' library(cchsflow)
#' 
#' vg_B_2013_2014 <- rec_with_table(
#'   cchs2013_2014_p, c(
#'     "DHHGAGE_cont", "SAC_2_B"
#'   )
#' )
#' 
#' head(vg_B_2013_2014)
#' 
#' vg_B_2015_2016 <- rec_with_table(
#'   cchs2015_2016_p, c(
#'     "SAC_2D_A","SAC_2D_B", "SAC_2D", "DHHGAGE_cont", "SAC_2_B"
#'   )
#' )
#'
#' tail(vg_B_2015_2016)
#' 
#' combined_vg_B <- suppressWarnings(merge_rec_data(vg_B_2013_2014,
#' vg_B_2015_2016))
#'
#' head(combined_vg_B)
#' tail(combined_vg_B)
#' @export

video_game_age2_fun <- function(DHHGAGE_cont, SAC_2D){
  if_else2(DHHGAGE_cont <20, SAC_2D, if_else2(SAC_2D == "NA(a)",
                                              tagged_na("a"), tagged_na("b")))
}

#' @title Total hours/week - sedentary activity (age 20+)
#' 
#' @description This function creates a derived variable (sedentary_activity)
#'  that calculates the total number of hours in a week spent doing sedentary 
#'  activities such as using a computer, using the internet, playing video 
#'  games, watching television or videos, and reading. For all activities, the 
#'  time spent at school or work is excluded. This is limited to ages 20+.
#'  
#' @note This is limited to ages 20+ for CCHS 2009-2016.
#'  
#' @param SAC_1 number of hours/week - spent on computer
#' 
#' @param SAC_2_A number of hours/week - playing video games (age 20+)
#' 
#' @param SAC_3 number of hours/week - watching TV or videos
#' 
#' @param SAC_4 number of hours/week - spent reading
#' 
#' @param DHHGAGE_cont continuous age
#' 
#' @return value for total hours/week doing sedentary activities
#' 
#' @examples 
#' # Using sedentary_activity_fun() to create values for total hours per week 
#' # spent doing sedentary activities across CCHS cycles.
#' # sedentary_activity_fun() is specified in variable_details.csv along with 
#' # the CCHS variables and cycles included.
#'
#' # To transform sedentary_activity across cycles, use rec_with_table() for each
#' # CCHS cycle and specify sedentary_activity, along with the required base
#' # variables.Then by using merge_rec_data(), you can combine 
#' # sedentary_activity across cycles.
#' 
#' library(cchsflow)
#' 
#' sed_act2009_2010 <- rec_with_table(
#'   cchs2009_2010_p, c(
#'     "SAC_1", "SAC_2_A","SAC_3","SAC_4","DHHGAGE_cont","sedentary_activity"
#'   )
#' )
#'
#' head(sed_act2009_2010)
#' 
#' sed_act2015_2016 <- rec_with_table(
#'   cchs2015_2016_p, c(
#'     "SAC_1", "SAC_2D_A","SAC_2D_B","SAC_2D","DHHGAGE_cont","SAC_2_A",
#'     "SAC_3","SAC_4","sedentary_activity"
#'   )
#' )
#' tail(sed_act2015_2016)
#' 
#' combined_sed_act <- suppressWarnings(merge_rec_data(sed_act2009_2010,
#'  sed_act2015_2016))
#'
#' head(combined_sed_act)
#' tail(combined_sed_act)
#' @export

sedentary_activity_fun <-
  function(DHHGAGE_cont, SAC_1, SAC_2_A, SAC_3, SAC_4){
    if_else2(!is.na(SAC_1) & !is.na(SAC_2_A) & !is.na(SAC_3) & 
             !is.na(SAC_4) & DHHGAGE_cont >=20,
             SAC_1 + SAC_2_A + SAC_3 + SAC_4,
             if_else2(SAC_1 == "NA(a)"| SAC_2_A == "NA(a)"| 
                      SAC_3 == "NA(a)"| SAC_4== "NA(a)",
                      tagged_na("a"), tagged_na("b")))
  }


#' @title Total hours/week - sedentary activity excluding reading (age 20+)
#' 
#' @description This function creates a derived variable (sedentary_activity2)
#'  that calculates the total number of hours in a week spent doing sedentary 
#'  activities such as using a computer, using the internet, playing video 
#'  games, and watching television or videos. For all activities, the 
#'  time spent at school or work is excluded. Time spent in reading is not 
#'  included. This is limited to ages 20+.
#'  
#' @param SAC_1 number of hours/week - spent on computer
#' 
#' @param SAC_2_A number of hours/week - playing video games (age 20+)
#' 
#' @param SAC_3 number of hours/week - watching TV or videos
#' 
#' @param DHHGAGE_cont continuous age
#' 
#' @return value for total hours/week doing sedentary activities (excluding 
#' reading)
#' 
#' @examples 
#' # Using sedentary_activity2_fun() to create values for total hours per week 
#' # spent doing sedentary activities (excluding reading) across CCHS cycles.
#' # sedentary_activity2_fun() is specified in variable_details.csv along with 
#' # the CCHS variables and cycles included.
#'
#' # To transform sedentary_activity2 across cycles, use rec_with_table() for each
#' # CCHS cycle and specify sedentary_activity2, along with the required base 
#' # variables.Then by using merge_rec_data(), you can combine 
#' # sedentary_activity2 across cycles.
#' 
#' library(cchsflow)
#' 
#' sed_act2_2009_2010 <- rec_with_table(
#'   cchs2009_2010_p, c(
#'     "SAC_1", "SAC_2","SAC_3","SAC_4","DHHGAGE_cont,"sedentary_activity2"
#'   )
#' )
#'
#' head(sed_act2_2009_2010)
#' 
#' sed_act2_2015_2016 <- rec_with_table(
#'   cchs2015_2016_p, c(
#'     "SAC_1", "SAC_2D_A","SAC_2D_B","SAC_2D","DHHGAGE_cont","SAC_2_A",
#'     "SAC_3","SAC_4","sedentary_activity2"
#'   )
#' )
#' tail(sed_act2_2015_2016)
#' 
#' combined_sed_act2 <- suppressWarnings(merge_rec_data(sed_act2_2009_2010,
#'  sed_act2_2015_2016))
#'
#' head(combined_sed_act2)
#' tail(combined_sed_act2)
#' @export

sedentary_activity2_fun <-
  function(DHHGAGE_cont, SAC_1, SAC_2_A, SAC_3){
    if_else2(!is.na(SAC_1) & !is.na(SAC_2_A) & !is.na(SAC_3) & DHHGAGE_cont >=20,
             SAC_1 + SAC_2_A + SAC_3,
              if_else2(SAC_1 == "NA(a)"| SAC_2_A == "NA(a)"| SAC_3 == "NA(a)",
                       tagged_na("a"), tagged_na("b")))
  }