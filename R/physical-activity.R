#' @title Daily energy expenditure in leisure activity 
#' 
#' @description This function creates a derived variable for daily leisure 
#' energy expenditure.A MET is a conceptual value that represents energy 
#' expended during physical activity. The volume of activity is calculated by 
#' multiplying the amount of minutes of activity (by level of intensity) by the 
#' MET value associated with that intensity. A MET (metabolic equivalent) is 
#' the energy cost of activity expressed as kilocalories expended per kilogram 
#' of body weight per hour of activity.
#'
#' In CCHS 2001-2014, PACDEE is the variable used to determine the daily 
#' expenditure of leisure activity for all ages. In CCHS 2015-2018, ages 12-17 
#' and 18+ years old have separate activity variables, where 12-17 year olds 
#' use PAY_XXX and 18+ year olds use PAA_XXX. Leisure activity is not directly 
#' measured. We used the derived variable, PAADVVOL, and removed active 
#' transportation in the new function. With this function, we combined leisure 
#' activity for ages 12+. We calculate the daily energy expenditure which uses 
#' the frequency and duration per session of the physical activity as well as 
#' the MET value (3 METS for leisure and 6 METS for vigorous activity).
#' 
#' EE (Daily Energy Expenditure) = ((N X D X METvalue) / 60)/7
#' Where: 
#' N = the number of times a respondent engaged in an activity over a 7 day period
#' D = the average duration in minutes of the activity
#' MET value = the energy cost of the activity expressed as kilocalories 
#' expended per kilogram of body 
#' weight per hour of activity (kcal/kg per hour)
#'  
#'
#' @param age continuous age variable.
#' 
#' @param PAA_045 number of hours of sports, fitness, or recreational activity 
#' that make you sweat or breathe harder for CCHS 2015-2018 for 18+ years old.
#' 
#' @param PAA_050 number of minutes of sports, fitness, or recreational activity 
#' that make you sweat or breathe harder for CCHS 2015-2018 for 18+ years old.
#' 
#' @param PAA_075 number of hours of other physical activity while at work, 
#' home or volunteering for CCHS 2015-2018 for 18+ years old.
#' 
#' @param PAA_080 number of minutes of other physical activity while at work, 
#' home or volunteering for CCHS 2015-2018 for 18+ years old.
#' 
#' @param PAADVVIG number of minutes of vigorous activity over 7 days or CCHS 
#' 2015-2018 for 18+ years old. 
#' 
#' @param PAADVDYS number of active days - 7 day for CCHS 2015-2018 for 18+ 
#' years old.
#' 
#' @param PAYDVTOA total minutes of other activities - 7 day for CCHS 2015-2018 
#' for 12-17 years old.
#' 
#' @param PAYDVADL total minutes of physical activity - leisure - 7 day for 
#' CCHS 2015-2018 for 12-17 years old.
#' 
#' @param PAYDVVIG total minutes - vigorous physical activity - 7 d for CCHS 
#' 2015-2018 for 12-17 years old.
#' 
#' @param PAYDVDYS total days physically active - 7 day for CCHS 2015-2018 for 
#' 12-17 years old.
#' 
#' @return Continuous variable for energy expenditure (energy_exp)
#' 
#' @examples
#' # Using energy_exp_fun() to create energy expenditure values across CCHS 
#' # cycles
#' # energy_exp_fun() is specified in variable_details.csv along with the CCHS
#' # variables and cycles included.
#'
#' # To transform energy_exp across cycles, use rec_with_table() for each
#' # CCHS cycle and specify energy_exp, along with each activity variable.
#' # Then by using merge_rec_data(), you can combine energy_exp across
#' # cycles
#' 
#' library(cchsflow)
#' energy_exp2015_2016 <- rec_with_table(
#'   cchs2015_2016_p, c(
#'     "DHHGAGE_cont", "PAA_045", "PAA_050", "PAA_075", "PAA_080", "PAADVDYS", 
#'     "PAADVVIG", "PAYDVTOA", "PAYDVADL", "PAYDVVIG", "PAYDVDYS", "energy_exp"
#'   )
#' )
#' 
#' head(energy_exp2015_2016)
#' 
#' energy_exp2017_2018 <- rec_with_table(
#'   cchs2017_2018_p, c(
#'     "DHHGAGE_cont", "PAA_045", "PAA_050", "PAA_075", "PAA_080", "PAADVDYS", 
#'     "PAADVVIG", "PAYDVTOA", "PAYDVADL", "PAYDVVIG", "PAYDVDYS", "energy_exp"
#'   )
#' )
#' 
#' tail(energy_exp2015_2016)
#' 
#' combined_energy_exp <- suppressWarnings(merge_rec_data(energy_exp2015_2016,
#'  energy_exp2017_2018))
#'
#' head(combined_energy_exp)
#' tail(combined_energy_exp)
#' @export

calculate_energy_exp <-
  function(age, PAA_045, PAA_050, PAA_075, PAA_080, PAADVDYS,
           PAADVVIG, PAYDVTOA, PAYDVADL, PAYDVVIG, PAYDVDYS,
           output_format = "tagged_na") {
    # === STEP 1: DATA CLEANING ===
    cleaned <- clean_variables(vars = list(
      DHHGAGE_cont = age,
      PAA_045 = PAA_045, PAA_050 = PAA_050,
      PAA_075 = PAA_075, PAA_080 = PAA_080,
      PAADVDYS = PAADVDYS, PAADVVIG = PAADVVIG,
      PAYDVTOA = PAYDVTOA, PAYDVADL = PAYDVADL,
      PAYDVVIG = PAYDVVIG, PAYDVDYS = PAYDVDYS
    ), output_format = "tagged_na")

    age_c <- cleaned$DHHGAGE_cont

    # === STEP 2: CORE CALCULATION ===
    # Adult leisure activity (age >= 18)
    leisure_adult <- dplyr::case_when(
      age_c >= 18 & !any_missing(cleaned$PAA_045, cleaned$PAA_050,
                                  cleaned$PAA_075, cleaned$PAA_080) ~
        cleaned$PAA_045 * 60 + cleaned$PAA_050 +
        cleaned$PAA_075 * 60 + cleaned$PAA_080,
      any_missing(cleaned$PAA_045, cleaned$PAA_050,
                  cleaned$PAA_075, cleaned$PAA_080) ~
        get_priority_missing(cleaned$PAA_045, cleaned$PAA_050,
                             cleaned$PAA_075, cleaned$PAA_080,
                             output_format = output_format),
      .default = assign_missing("not_applicable", "energy_exp", output_format)
    )

    # Youth leisure activity (age < 18)
    leisure_youth <- dplyr::case_when(
      age_c < 18 & !any_missing(cleaned$PAYDVTOA, cleaned$PAYDVADL) ~
        cleaned$PAYDVTOA + cleaned$PAYDVADL,
      any_missing(cleaned$PAYDVTOA, cleaned$PAYDVADL) ~
        get_priority_missing(cleaned$PAYDVTOA, cleaned$PAYDVADL,
                             output_format = output_format),
      .default = assign_missing("not_applicable", "energy_exp", output_format)
    )

    # Energy expenditure: youth path first, then adult
    result <- dplyr::case_when(
      # Youth path
      !any_missing(cleaned$PAYDVVIG, leisure_youth, cleaned$PAYDVDYS) ~
        ((leisure_youth - cleaned$PAYDVVIG) * 3 +
         cleaned$PAYDVVIG * 6) / 7 * cleaned$PAYDVDYS / 60,
      # Adult path
      !any_missing(cleaned$PAADVDYS, cleaned$PAADVVIG, leisure_adult) ~
        ((leisure_adult - cleaned$PAADVVIG) * 3 +
         cleaned$PAADVVIG * 6) / 7 * cleaned$PAADVDYS / 60,
      # Missing propagation
      any_missing(leisure_youth, cleaned$PAYDVDYS, cleaned$PAYDVVIG,
                  leisure_adult, cleaned$PAADVDYS, cleaned$PAADVVIG) ~
        get_priority_missing(leisure_youth, leisure_adult,
                             output_format = output_format),
      .default = assign_missing("not_stated", "energy_exp", output_format)
    )

    # === STEP 3: OUTPUT VALIDATION ===
    output_cleaned <- clean_variables(vars = list(
      energy_exp = result
    ), output_format = output_format)

    return(output_cleaned$energy_exp)
  }

#' @title Categorize energy expenditure into 3 activity levels
#'
#' @description Categorizes the continuous energy expenditure variable
#'   (energy_exp) into 3 physical activity levels using CCHS cutpoints:
#'   inactive (< 1.5), moderately active (1.5 to < 3.0), and active (>= 3.0
#'   kcal/kg/day).
#'
#' @param energy_exp Continuous energy expenditure in kcal/kg/day.
#'
#' @return A categorical variable with 3 levels:
#'   \enumerate{
#'     \item Inactive (< 1.5 kcal/kg/day)
#'     \item Moderately active (1.5 to < 3.0 kcal/kg/day)
#'     \item Active (>= 3.0 kcal/kg/day)
#'   }
#'   Returns \code{"NA(a)"} if energy_exp is tagged NA(a) (not applicable),
#'   or \code{"NA(b)"} otherwise (missing).
#'
#' @examples
#' energy_exp_fun_cat(0.5)  # 1 (Inactive)
#' energy_exp_fun_cat(2.0)  # 2 (Moderately active)
#' energy_exp_fun_cat(4.0)  # 3 (Active)
#'
#' @export
categorize_energy_exp <- function(energy_exp, output_format = "tagged_na") {
  # === STEP 1: DATA CLEANING ===
  cleaned <- clean_variables(vars = list(
    energy_exp = energy_exp
  ), output_format = "tagged_na")

  ee <- cleaned$energy_exp

  # === STEP 2: CATEGORIZATION ===
  result <- dplyr::case_when(
    any_missing(ee) ~
      get_priority_missing(ee, output_format = output_format),
    ee >= 0 & ee < 1.5 ~ 1,
    ee >= 1.5 & ee < 3 ~ 2,
    ee >= 3 ~ 3,
    .default = assign_missing("not_stated", "energy_exp_cat3", output_format)
  )

  # === STEP 3: OUTPUT VALIDATION ===
  output_cleaned <- clean_variables(vars = list(
    energy_exp_cat3 = result
  ), output_format = output_format)

  return(prep_cat_output(output_cleaned$energy_exp_cat3))
}
