updateData <- function(variables, dataName, pathToData) {
  mockList <- list()
  mockList[[dataName]] <- bllflow::ReadData(variables, dataName, pathToData)
  save(mockList[[1]], file = file.path(getwd(), "data",paste(dataName,".RData",sep = "" )))
}
