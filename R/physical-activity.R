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
#' @param DHHGAGE_cont continuous age variable.
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
#' # Scalar usage - Adult with moderate activity levels
#' calculate_energy_expenditure(
#'   DHHGAGE_cont = 35, PAA_045 = 1, PAA_050 = 30, PAA_075 = 2, PAA_080 = 20,
#'   PAADVDYS = 5, PAADVVIG = 150, PAYDVTOA = 0, PAYDVADL = 0, 
#'   PAYDVVIG = 0, PAYDVDYS = 0
#' )
#'
#' # Scalar usage - Youth with high activity levels  
#' calculate_energy_expenditure(
#'   DHHGAGE_cont = 16, PAA_045 = 0, PAA_050 = 0, PAA_075 = 0, PAA_080 = 0,
#'   PAADVDYS = 0, PAADVVIG = 0, PAYDVTOA = 200, PAYDVADL = 100, 
#'   PAYDVVIG = 150, PAYDVDYS = 6
#' )
#'
#' # Vector processing with missing data and different age groups
#' ages <- c(25, 45, 16, 65, NA)                    # Mixed adults and youth
#' paa_045 <- c(2, 1, 0, 0, 1)                     # Hours vigorous (adults)
#' paa_050 <- c(30, 15, 0, 0, 20)                  # Minutes vigorous (adults)
#' paa_075 <- c(1, 2, 0, 1, "NA(a)")              # Hours moderate with missing
#' paa_080 <- c(20, 40, 0, 30, 25)                 # Minutes moderate
#' paadvdys <- c(5, 3, 0, 2, 4)                    # Active days
#' paadvvig <- c(150, 75, 0, 30, 120)              # Vigorous minutes
#' paydvtoa <- c(0, 0, 180, 0, 0)                  # Youth other activities
#' paydvadl <- c(0, 0, 90, 0, 0)                   # Youth ADL activities
#' paydvvig <- c(0, 0, 120, 0, 0)                  # Youth vigorous
#' paydvdys <- c(0, 0, 5, 0, 0)                    # Youth active days
#' 
#' calculate_energy_expenditure(ages, paa_045, paa_050, paa_075, paa_080,
#'                             paadvdys, paadvvig, paydvtoa, paydvadl, 
#'                             paydvvig, paydvdys)
#'
#' # rec_with_table() integration for production use across CCHS cycles
#' \dontrun{
#' library(cchsflow)
#' 
#' # CCHS 2015-2016 energy expenditure
#' energy_exp2015_2016 <- rec_with_table(
#'   data = cchs2015_2016_p,
#'   variables = "energy_exp",
#'   database_name = "cchs2015_2016_p"
#' )
#' 
#' # CCHS 2017-2018 energy expenditure
#' energy_exp2017_2018 <- rec_with_table(
#'   data = cchs2017_2018_p,
#'   variables = "energy_exp", 
#'   database_name = "cchs2017_2018_p"
#' )
#' 
#' # Combine across cycles using merge_rec_data()
#' combined_energy_exp <- merge_rec_data(energy_exp2015_2016, energy_exp2017_2018)
#' summary(combined_energy_exp$energy_exp)
#' }
#' @note v3.0.0, last updated: 2025-01-01, status: active, Note: Function renamed from energy_exp_fun to calculate_energy_expenditure following R conventions
#' 
#' @export

calculate_energy_expenditure <-
  function(DHHGAGE_cont, PAA_045, PAA_050, PAA_075, PAA_080, PAADVDYS, 
           PAADVVIG, PAYDVTOA, PAYDVADL, PAYDVVIG, PAYDVDYS){
    # Leisure activity for adults
    leisure_adult <- 
      if_else2(DHHGAGE_cont >= 18 & !is.na(PAA_045) & !is.na(PAA_050) & 
               !is.na(PAA_075) & !is.na(PAA_080),
                ((PAA_045)*60 +(PAA_050) +(PAA_075)*60 +(PAA_080)), 
               if_else2(PAA_045 == "NA(a)"|PAA_050 == "NA(a)"|
                        PAA_075 == "NA(a)"|PAA_080 == "NA(a)", 
                        haven::tagged_na("a"), haven::tagged_na("b")))
    
    # Leisure activity for youth
    leisure_youth <- 
      if_else2(DHHGAGE_cont < 18 & !is.na(PAYDVTOA) & !is.na(PAYDVADL), 
               ((PAYDVTOA) + (PAYDVADL)),
               if_else2(PAYDVTOA == "NA(a)"|PAYDVADL == "NA(a)", 
                        haven::tagged_na("a"), haven::tagged_na("b")))
    
    # Energy expenditure calculation
    physical_activity <-
      if_else2(!is.na(PAYDVVIG) & !is.na(leisure_youth) & !is.na(PAYDVDYS),
               (((leisure_youth) - (PAYDVVIG))*3 + (PAYDVVIG)*6)/7*PAYDVDYS/60,
      if_else2(!is.na(PAADVDYS) & !is.na(PAADVVIG) & !is.na(leisure_adult),
               ((((leisure_adult) - (PAADVVIG))*3 + (PAADVVIG)*6)/7*(PAADVDYS)/60), 
                    if_else2(leisure_youth == "NA(a)"|
                             PAYDVDYS == "NA(a)"|PAYDVVIG == "NA(a)"|
                             leisure_adult == "NA(a)"|PAADVDYS == "NA(a)"|
                             PAADVVIG == "NA(a)", 
                             haven::tagged_na("a"), haven::tagged_na("b"))))
    return(physical_activity)
  }


