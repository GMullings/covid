setwd("~/R/covid")
.libPaths(c("C:/Users/geoff/Documents/R/win-library/3.6"))
filename <- "Rates_of_COVID-19_Cases_or_Deaths_by_Age_Group_and_Vaccination_Status.csv"
dataset <- read.csv(filename)
dim(dataset)
sapply(dataset, class)
levels(dataset$outcome)
