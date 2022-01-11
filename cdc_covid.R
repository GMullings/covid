setwd("~/R/covid")
.libPaths(c("C:/Users/geoff/Documents/R/win-library/3.6"))
filename <- "Rates_of_COVID-19_Cases_or_Deaths_by_Age_Group_and_Vaccination_Status.csv"
dataset <- read.csv(filename)
dim(dataset)
sapply(dataset, class)
levels(dataset$outcome)
levels(dataset$Age.group)
sum(is.na.data.frame(dataset))

allagesvaxtypes = dataset[dataset$Vaccine.product == "all_types" & dataset$Age.group == "all_ages_adj",]
deaths = allagesvaxtypes[allagesvaxtypes$outcome == "death",]
cases = allagesvaxtypes[allagesvaxtypes$outcome == "case",]

sum(is.na.data.frame(cases))
sum(is.na.data.frame(deaths))

vaxdeathrate = deaths[6]/deaths[7]
unvaxdeathrate = deaths[8]/deaths[9]
colnames(vaxdeathrate) = "Vaccinated.death.rate"
colnames(unvaxdeathrate) = "Unvaccinated.death.rate"
deathsrate = cbind.data.frame(deaths,unvaxdeathrate,vaxdeathrate)
vaxcaserate = cases[6]/cases[7]
unvaxcaserate = cases[8]/cases[9]
colnames(vaxcaserate) = "Vaccinated.case.rate"
colnames(unvaxcaserate) = "Unvaccinated.case.rate"
caserate = cbind.data.frame(cases,unvaxcaserate,vaxcaserate)

# Should transform case and death rates into %s

# Need to plot case and death rates over time

# Need to get CDC data on hospitalizations and death characteristics. Over time would be excellent

