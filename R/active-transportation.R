#' @title Daily active transportation (2001-2005)
#' 
#' @description This function creates a derived variable for daily time spent 
#' traveling in active ways. This includes walking and biking. This function is 
#' used for CCHS 2001-2005.
#' 
#' @param PAC_4A_cont number of hours walk work/school in week in the past 
#' 3 months.
#' 
#' @param PAC_4B_cont number of hours bike work/school in week in the past 
#' 3 months.
#' 
#' @return Continuous variable for active transportation (active_transport)
#' 
#' @examples
#' # Using active_transport1_fun() to determine daily time spent 
#' # traveling in active ways values across CCHS 2001-2005.
#' 
#' # active_transport1_fun() is specified in variable_details.csv along with the CCHS
#' # variables and cycles included.
#'
#' # To transform active_transport across cycles, use rec_with_table() for each
#' # CCHS cycle and specify active_transport, along with each activity variable.
#' # Then by using merge_rec_data(), you can combine active_transport across
#' # cycles
#' 
#' library(cchsflow)
#' active_transport2001 <- rec_with_table(
#'   cchs2001_p, c(
#'     "PAC_4A_cont", PAC_4B_cont", "active_transport"
#'   )
#' )
#' 
#' head(active_transport2001)
#' 
#' active_transport2005 <- rec_with_table(
#'   cchs2005_p, c(
#'     "PAC_4A_cont", PAC_4B_cont", "active_transport"
#'   )
#' )
#' 
#' tail(active_transport2005)
#' 
#' combined_active_transport <- suppressWarnings(merge_rec_data(active_transport2001,
#'  active_transport2005))
#'
#' head(combined_active_transport)
#' tail(combined_active_transport)
#' @export

active_transport1_fun <-
  function(PAC_4A_cont, PAC_4B_cont){
    if_else2(!is.na(PAC_4A_cont) & !is.na(PAC_4B_cont),
             (PAC_4A_cont + PAC_4B_cont)/7,
             if_else2(is.na(PAC_4A_cont), PAC_4B_cont/7,
                      if_else2(is.na(PAC_4B_cont), PAC_4A_cont/7,
                               if_else2(PAC_4A_cont == "NA(a)" & PAC_4B_cont == "NA(a)"),
                               tagged_na("a"), tagged_na("b"))))
  }

#' @title Daily active transportation (2007-2014)
#' 
#' @description This function creates a derived variable for daily time spent 
#' traveling in active ways. This includes walking and biking. This function is 
#' used for CCHS 2007-2014.
#' 
#' @param PAC_7 have walked to work or school in the past 3 months?
#' 
#' @param PAC_7A number of times walked to work/school in the past 3 months. 
#' 
#' @param PAC_7B_cont number of minutes walk to work/school.
#' 
#' @param PAC_8 have biked to work or school in the past 3 months?
#' 
#' @param PAC_8A number of times biked to work/school in the past 3 months. 
#' 
#' @param PAC_8B_cont number of minutes bike to work/school.
#' 
#' @return Continuous variable for active transportation (active_transport)
#' 
#' @examples
#' # Using active_transport2_fun() to determine daily time spent 
#' # traveling in active ways values across CCHS 2007-2014.
#' 
#' # active_transport2_fun() is specified in variable_details.csv along with the CCHS
#' # variables and cycles included.
#'
#' # To transform active_transport across cycles, use rec_with_table() for each
#' # CCHS cycle and specify active_transport, along with each activity variable.
#' # Then by using merge_rec_data(), you can combine active_transport across
#' # cycles
#' 
#' library(cchsflow)
#' active_transport2007_2008 <- rec_with_table(
#'   cchs2007_2008_p, c(
#'     "PAC_7", "PAC_7A", "PAC_7B_cont", "PAC_8", "PAC_8A", "PAC_8B_cont", 
#'     "active_transport"
#'   )
#' )
#' 
#' head(active_transport2007_2008)
#' 
#' active_transport2013_2014 <- rec_with_table(
#'   cchs2013_2014_p, c(
#'     "PAC_7", "PAC_7A", "PAC_7B_cont", "PAC_8", "PAC_8A", "PAC_8B_cont", 
#'     "active_transport"
#'   )
#' )
#' 
#' tail(active_transport2013_2014)
#' 
#' combined_active_transport <- suppressWarnings(merge_rec_data(
#'  active_transport2007_2008, active_transport2013_2014))
#'
#' head(combined_active_transport)
#' tail(combined_active_transport)
#' @export

active_transport2_fun <-
  function(PAC_7, PAC_7A, PAC_7B_cont, PAC_8, PAC_8A, PAC_8B_cont){
    # Walk
    walk <-
    if_else2(PAC_7 == 2, 0,
             if_else2(PAC_7 == 1 & !is.na(PAC_7A) & !is.na(PAC_7B_cont),
                      (PAC_7A)/90*PAC_7B_cont,
                      if_else2(PAC_7 == "NA(a)", tagged_na("a"), tagged_na("b"))))
    
    # Bike
    bike <-
      if_else2(PAC_8 == 2, 0,
               if_else2(PAC_8 == 1 & !is.na(PAC_8A) & !is.na(PAC_8B_cont),
                        (PAC_8A)/90*PAC_8B_cont,
                        if_else2(PAC_8 == "NA(a)", tagged_na("a"), tagged_na("b"))))
    
    # Active transportation
    active <-
      if_else2(!is.na(walk) & !is.na(bike),
               walk + bike,
               if_else2(is.na(walk), bike,
                        if_else2(is.na(bike), walk,
                                 if_else2(walk == "NA(a)" & bike == "NA(a)",
                                          tagged_na("a"), tagged_na("b")))))
    
    return(active)
  
                      
  }

#' @title Daily active transportation (2015-2018)
#' 
#' @description This function creates a derived variable for daily time spent 
#' traveling in active ways. This includes walking and biking. This function is 
#' used for CCHS 2015-2018.
#' 
#' @param PAYDVTTR number of minutes of active transportation in a week 
#' for 12-17 years old.
#' 
#' @param PAADVTRV number of minutes of active transportation in a week 
#' for 18+ years old.
#' 
#' @return Continuous variable for active transportation (active_transport)
#' 
#' @examples
#' # Using active_transport3_fun() to determine daily time spent 
#' # traveling in active ways values across CCHS 2015-2018.
#' 
#' # active_transport3_fun() is specified in variable_details.csv along with the CCHS
#' # variables and cycles included.
#'
#' # To transform active_transport across cycles, use rec_with_table() for each
#' # CCHS cycle and specify active_transport, along with each activity variable.
#' # Then by using merge_rec_data(), you can combine active_transport across
#' # cycles
#' 
#' library(cchsflow)
#' active_transport2015_2016 <- rec_with_table(
#'   cchs2015_2016_p, c(
#'     "PAYDVTTR", "PAADVTRV","active_transport"
#'   )
#' )
#' 
#' head(active_transport2015_2016)
#' 
#' active_transport2017_2018 <- rec_with_table(
#'   cchs2017_2018_p, c(
#'     "PAYDVTTR", "PAADVTRV","active_transport"
#'   )
#' )
#' 
#' tail(active_transport2017_2018)
#' 
#' combined_active_transport <- suppressWarnings(merge_rec_data(
#'  active_transport2015_2016, active_transport2017_2018))
#'
#' head(combined_active_transport)
#' tail(combined_active_transport)
#' @export

active_transport3_fun <-
  function(PAYDVTTR, PAADVTRV) {
    if_else2(!is.na(PAYDVTTR) & !is.na(PAADVTRV),
             (PAYDVTTR + PAADVTRV)/7,
             if_else2(is.na(PAYDVTTR), PAADVTRV/7,
                      if_else2(is.na(PAADVTRV), PAYDVTTR/7,
                               if_else2(PAYDVTTR == "NA(a)" & PAADVTRV == "NA(a)",
                               tagged_na("a"), tagged_na("b")))))
  }