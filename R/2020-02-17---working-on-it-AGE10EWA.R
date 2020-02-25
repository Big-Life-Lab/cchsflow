pacman::p_load(devtools, tidyverse, cchsflow, openxlsx, here, readxl)

# reworkLOC <- here('../cchsflow/R/age-rework-ex.R')


# Excel trickery ----------------------------------------------------------

# wb <- createWorkbook()
# addWorksheet(wb, 'variable_details')
# addWorksheet(wb, 'variables')
# writeData(wb, 'variable_details', variable_details)
# writeData(wb, 'variables', variables)
# saveWorkbook(wb, 'myVariablesSheet.xlsx', overwrite = TRUE)

# rec game3 ---------------------------------------------------------------

getwd()

# get the variables sheet into R session;
variable_details2 <- read_excel("PoRT MSW.xlsx",sheet = "cchsflow variable_details.csv")
variables2 <- read_excel("PoRT MSW.xlsx",sheet = "cchsflow variables.csv")


# trying the newwly derived varibles

rec_with_table(data = cchs2001, 
               variables = c('DHHGAGE_A', 'DHHGAGE_A10', 'DHHGAGE_D'),
               variable_details = variable_details2
               # custom_function_path = reworkLOC
)


rec_with_table(data = cchs2005, 
               variables = c('DHHGAGE_B', 'DHHGAGE_B10', 'DHHGAGE_D'),
               variable_details = variable_details2
               # custom_function_path = reworkLOC
)




write.csv(variable_details2, 'variable_details.csv', row.names = FALSE)
write.csv(variables2, 'variables.csv', row.names = FALSE)


