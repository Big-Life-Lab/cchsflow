% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/physical-activity.R
\name{energy_exp_fun}
\alias{energy_exp_fun}
\title{Daily energy expenditure in leisure activity}
\usage{
energy_exp_fun(
  DHHGAGE_cont,
  PAA_045,
  PAA_050,
  PAA_075,
  PAA_080,
  PAADVDYS,
  PAADVVIG,
  PAYDVTOA,
  PAYDVADL,
  PAYDVVIG,
  PAYDVDYS
)
}
\arguments{
\item{DHHGAGE_cont}{continuous age variable.}

\item{PAA_045}{number of hours of sports, fitness, or recreational activity 
that make you sweat or breathe harder for CCHS 2015-2018 for 18+ years old.}

\item{PAA_050}{number of minutes of sports, fitness, or recreational activity 
that make you sweat or breathe harder for CCHS 2015-2018 for 18+ years old.}

\item{PAA_075}{number of hours of other physical activity while at work, 
home or volunteering for CCHS 2015-2018 for 18+ years old.}

\item{PAA_080}{number of minutes of other physical activity while at work, 
home or volunteering for CCHS 2015-2018 for 18+ years old.}

\item{PAADVDYS}{number of active days - 7 day for CCHS 2015-2018 for 18+ 
years old.}

\item{PAADVVIG}{number of minutes of vigorous activity over 7 days or CCHS 
2015-2018 for 18+ years old.}

\item{PAYDVTOA}{total minutes of other activities - 7 day for CCHS 2015-2018 
for 12-17 years old.}

\item{PAYDVADL}{total minutes of physical activity - leisure - 7 day for 
CCHS 2015-2018 for 12-17 years old.}

\item{PAYDVVIG}{total minutes - vigorous physical activity - 7 d for CCHS 
2015-2018 for 12-17 years old.}

\item{PAYDVDYS}{total days physically active - 7 day for CCHS 2015-2018 for 
12-17 years old.}
}
\value{
Continuous variable for energy expenditure (energy_exp)
}
\description{
This function creates a derived variable for daily leisure 
energy expenditure.A MET is a conceptual value that represents energy 
expended during physical activity. The volume of activity is calculated by 
multiplying the amount of minutes of activity (by level of intensity) by the 
MET value associated with that intensity. A MET (metabolic equivalent) is 
the energy cost of activity expressed as kilocalories expended per kilogram 
of body weight per hour of activity.

In CCHS 2001-2014, PACDEE is the variable used to determine the daily 
expenditure of leisure activity for all ages. In CCHS 2015-2018, ages 12-17 
and 18+ years old have separate activity variables, where 12-17 year olds 
use PAY_XXX and 18+ year olds use PAA_XXX. Leisure activity is not directly 
measured. We used the derived variable, PAADVVOL, and removed active 
transportation in the new function. With this function, we combined leisure 
activity for ages 12+. We calculate the daily energy expenditure which uses 
the frequency and duration per session of the physical activity as well as 
the MET value (3 METS for leisure and 6 METS for vigorous activity).

EE (Daily Energy Expenditure) = ((N X D X METvalue) / 60)/7
Where: 
N = the number of times a respondent engaged in an activity over a 7 day period
D = the average duration in minutes of the activity
MET value = the energy cost of the activity expressed as kilocalories 
expended per kilogram of body 
weight per hour of activity (kcal/kg per hour)
}
\examples{
# Using energy_exp_fun() to create energy expenditure values across CCHS 
# cycles
# energy_exp_fun() is specified in variable_details.csv along with the CCHS
# variables and cycles included.

# To transform energy_exp across cycles, use rec_with_table() for each
# CCHS cycle and specify energy_exp, along with each activity variable.
# Then by using merge_rec_data(), you can combine energy_exp across
# cycles

library(cchsflow)
energy_exp2015_2016 <- rec_with_table(
  cchs2015_2016_p, c(
    "DHHGAGE_cont", "PAA_045", "PAA_050", "PAA_075", "PAA_080", "PAADVDYS", 
    "PAADVVIG", "PAYDVTOA", "PAYDVADL", "PAYDVVIG", "PAYDVDYS", "energy_exp"
  )
)

head(energy_exp2015_2016)

energy_exp2017_2018 <- rec_with_table(
  cchs2017_2018_p, c(
    "DHHGAGE_cont", "PAA_045", "PAA_050", "PAA_075", "PAA_080", "PAADVDYS", 
    "PAADVVIG", "PAYDVTOA", "PAYDVADL", "PAYDVVIG", "PAYDVDYS", "energy_exp"
  )
)

tail(energy_exp2015_2016)

combined_energy_exp <- suppressWarnings(merge_rec_data(energy_exp2015_2016,
 energy_exp2017_2018))

head(combined_energy_exp)
tail(combined_energy_exp)
}
