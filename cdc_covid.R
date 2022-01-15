setwd("~/R/covid")
.libPaths(c("C:/Users/geoff/Documents/R/win-library/3.6"))
install.packages("scales")
library("scales")

filename <- "Rates_of_COVID-19_Cases_or_Deaths_by_Age_Group_and_Vaccination_Status.csv"
dataset <- read.csv(filename)
dim(dataset)
sapply(dataset, class)
levels(dataset$outcome)
levels(dataset$Age.group)
sum(is.na.data.frame(dataset))

# All Ages
allagesvaxtypes = dataset[dataset$Vaccine.product == "all_types" & dataset$Age.group == "all_ages_adj",]
deaths = allagesvaxtypes[allagesvaxtypes$outcome == "death",]
cases = allagesvaxtypes[allagesvaxtypes$outcome == "case",]

sum(is.na.data.frame(cases))
sum(is.na.data.frame(deaths))

colnames(deaths)
vaxdeathrate = deaths[6]/deaths[7]
unvaxdeathrate = deaths[8]/deaths[9]
colnames(vaxdeathrate) = "Vaccinated.death.rate"
colnames(unvaxdeathrate) = "Unvaccinated.death.rate"
deathsrate = cbind.data.frame(deaths,unvaxdeathrate,vaxdeathrate)
colnames(cases)
vaxcaserate = cases[6]/cases[7]
unvaxcaserate = cases[8]/cases[9]
colnames(vaxcaserate) = "Vaccinated.case.rate"
colnames(unvaxcaserate) = "Unvaccinated.case.rate"
caserate = cbind.data.frame(cases,unvaxcaserate,vaxcaserate)

unvaxcaseratepercent = percent(caserate$Unvaccinated.case.rate)
vaxcaseratepercent = percent(caserate$Vaccinated.case.rate)
unvaxdeathratepercent = percent(deathsrate$Unvaccinated.death.rate)
vaxdeathratepercent = percent(deathsrate$Vaccinated.death.rate)

unvaxcaseratepercent = as.data.frame(unvaxcaseratepercent)
colnames(unvaxcaseratepercent) = "Unvaccinated.case.percent"
vaxcaseratepercent = as.data.frame(vaxcaseratepercent)
colnames(vaxcaseratepercent) = "Vaccinated.case.percent"
unvaxdeathratepercent = as.data.frame(unvaxdeathratepercent)
colnames(unvaxdeathratepercent) = "UnVaccinated.death.percent"
vaxdeathratepercent = as.data.frame(vaxdeathratepercent)
colnames(vaxdeathratepercent) = "Vaccinated.death.percent"
caserate = cbind.data.frame(caserate, unvaxcaseratepercent, vaxcaseratepercent)
deathsrate = cbind.data.frame(deathsrate, unvaxdeathratepercent, vaxdeathratepercent)

dim(caserate)
dim(deathsrate)

snippedcaserate = head(caserate, -3)
unvaxcasefatalityratio = deathsrate[8]/snippedcaserate[8]
vaxcasefatalityratio = deathsrate[6]/snippedcaserate[6]
colnames(unvaxcasefatalityratio) = "Unvaccinated"
colnames(vaxcasefatalityratio) = "Vaccinated"
casefatalityratios = cbind.data.frame(unvaxcasefatalityratio, vaxcasefatalityratio)
casefatalityratios = cbind.data.frame(snippedcaserate[2:5],casefatalityratios)
casefatalityratios = casefatalityratios[,-4]

deathshundredk = deathsrate[17:18]*100000
colnames(deathshundredk) = c("Unvaccinated","Vaccinated")
deathshundredk = cbind.data.frame(deathsrate[2:5], deathshundredk)
deathshundredk = deathshundredk[,-4]

# Teens, 12-17
teensvaxtypes = dataset[dataset$Vaccine.product == "all_types" & dataset$Age.group == "12-17",]
teensdeaths = teensvaxtypes[teensvaxtypes$outcome == "death",]
teenscases = teensvaxtypes[teensvaxtypes$outcome == "case",]

sum(is.na.data.frame(teenscases))
sum(is.na.data.frame(teensdeaths))
# Age-adjustments seem to be NA in specific ages.

colnames(teensdeaths)
teensvaxdeathrate = teensdeaths[6]/teensdeaths[7]
teensunvaxdeathrate = teensdeaths[8]/teensdeaths[9]
colnames(teensvaxdeathrate) = "Vaccinated.death.rate"
colnames(teensunvaxdeathrate) = "Unvaccinated.death.rate"
teensdeathsrate = cbind.data.frame(teensdeaths,teensunvaxdeathrate,teensvaxdeathrate)
colnames(teenscases)
teensvaxcaserate = teenscases[6]/teenscases[7]
teensunvaxcaserate = teenscases[8]/teenscases[9]
colnames(teensvaxcaserate) = "Vaccinated.case.rate"
colnames(teensunvaxcaserate) = "Unvaccinated.case.rate"
teenscaserate = cbind.data.frame(teenscases,teensunvaxcaserate,teensvaxcaserate)

dim(teenscaserate)
dim(teensdeathsrate)

snippedteenscaserate = head(teenscaserate, -3)
teensunvaxcasefatalityratio = teensdeathsrate[8]/snippedteenscaserate[8]
teensvaxcasefatalityratio = teensdeathsrate[6]/snippedteenscaserate[6]
colnames(teensunvaxcasefatalityratio) = "Unvaccinated"
colnames(teensvaxcasefatalityratio) = "Vaccinated"
teenscasefatalityratios = cbind.data.frame(teensunvaxcasefatalityratio, teensvaxcasefatalityratio)
teenscasefatalityratios = cbind.data.frame(snippedteenscaserate[2:5],teenscasefatalityratios)

teensdeathshundredk = teensdeathsrate[17:18]*100000
colnames(teensdeathshundredk) = c("Unvaccinated","Vaccinated")
teensdeathshundredk = cbind.data.frame(teensdeathsrate[2:5], teensdeathshundredk)

# Young Adults, 18-29
yngadltsvaxtypes = dataset[dataset$Vaccine.product == "all_types" & dataset$Age.group == "18-29",]
yngadltsdeaths = yngadltsvaxtypes[yngadltsvaxtypes$outcome == "death",]
yngadltscases = yngadltsvaxtypes[yngadltsvaxtypes$outcome == "case",]

sum(is.na.data.frame(yngadltscases))
sum(is.na.data.frame(yngadltsdeaths))
# Age-adjustments seem to be NA in specific ages.

colnames(yngadltsdeaths)
yngadltsvaxdeathrate = yngadltsdeaths[6]/yngadltsdeaths[7]
yngadltsunvaxdeathrate = yngadltsdeaths[8]/yngadltsdeaths[9]
colnames(yngadltsvaxdeathrate) = "Vaccinated.death.rate"
colnames(yngadltsunvaxdeathrate) = "Unvaccinated.death.rate"
yngadltsdeathsrate = cbind.data.frame(yngadltsdeaths,yngadltsunvaxdeathrate,yngadltsvaxdeathrate)
colnames(yngadltscases)
yngadltsvaxcaserate = yngadltscases[6]/yngadltscases[7]
yngadltsunvaxcaserate = yngadltscases[8]/yngadltscases[9]
colnames(yngadltsvaxcaserate) = "Vaccinated.case.rate"
colnames(yngadltsunvaxcaserate) = "Unvaccinated.case.rate"
yngadltscaserate = cbind.data.frame(yngadltscases,yngadltsunvaxcaserate,yngadltsvaxcaserate)

dim(yngadltscaserate)
dim(yngadltsdeathsrate)

snippedyngadltscaserate = head(yngadltscaserate, -3)
yngadltsunvaxcasefatalityratio = yngadltsdeathsrate[8]/snippedyngadltscaserate[8]
yngadltsvaxcasefatalityratio = yngadltsdeathsrate[6]/snippedyngadltscaserate[6]
colnames(yngadltsunvaxcasefatalityratio) = "Unvaccinated"
colnames(yngadltsvaxcasefatalityratio) = "Vaccinated"
yngadltscasefatalityratios = cbind.data.frame(yngadltsunvaxcasefatalityratio, yngadltsvaxcasefatalityratio)
yngadltscasefatalityratios = cbind.data.frame(snippedyngadltscaserate[2:5],yngadltscasefatalityratios)

yngadltsdeathshundredk = yngadltsdeathsrate[17:18]*100000
colnames(yngadltsdeathshundredk) = c("Unvaccinated","Vaccinated")
yngadltsdeathshundredk = cbind.data.frame(yngadltsdeathsrate[2:5], yngadltsdeathshundredk)

# Mid Ages, 30-49
mdagesvaxtypes = dataset[dataset$Vaccine.product == "all_types" & dataset$Age.group == "30-49",]
mdagesdeaths = mdagesvaxtypes[mdagesvaxtypes$outcome == "death",]
mdagescases = mdagesvaxtypes[mdagesvaxtypes$outcome == "case",]

sum(is.na.data.frame(mdagescases))
sum(is.na.data.frame(mdagesdeaths))
# Age-adjustments seem to be NA in specific ages.

colnames(mdagesdeaths)
mdagesvaxdeathrate = mdagesdeaths[6]/mdagesdeaths[7]
mdagesunvaxdeathrate = mdagesdeaths[8]/mdagesdeaths[9]
colnames(mdagesvaxdeathrate) = "Vaccinated.death.rate"
colnames(mdagesunvaxdeathrate) = "Unvaccinated.death.rate"
mdagesdeathsrate = cbind.data.frame(mdagesdeaths,mdagesunvaxdeathrate,mdagesvaxdeathrate)
colnames(mdagescases)
mdagesvaxcaserate = mdagescases[6]/mdagescases[7]
mdagesunvaxcaserate = mdagescases[8]/mdagescases[9]
colnames(mdagesvaxcaserate) = "Vaccinated.case.rate"
colnames(mdagesunvaxcaserate) = "Unvaccinated.case.rate"
mdagescaserate = cbind.data.frame(mdagescases,mdagesunvaxcaserate,mdagesvaxcaserate)

dim(mdagescaserate)
dim(mdagesdeathsrate)

snippedmdagescaserate = head(mdagescaserate, -3)
mdagesunvaxcasefatalityratio = mdagesdeathsrate[8]/snippedmdagescaserate[8]
mdagesvaxcasefatalityratio = mdagesdeathsrate[6]/snippedmdagescaserate[6]
colnames(mdagesunvaxcasefatalityratio) = "Unvaccinated"
colnames(mdagesvaxcasefatalityratio) = "Vaccinated"
mdagescasefatalityratios = cbind.data.frame(mdagesunvaxcasefatalityratio, mdagesvaxcasefatalityratio)
mdagescasefatalityratios = cbind.data.frame(snippedmdagescaserate[2:5],mdagescasefatalityratios)

mdagesdeathshundredk = mdagesdeathsrate[17:18]*100000
colnames(mdagesdeathshundredk) = c("Unvaccinated","Vaccinated")
mdagesdeathshundredk = cbind.data.frame(mdagesdeathsrate[2:5], mdagesdeathshundredk)

# Old, 50-64
oldvaxtypes = dataset[dataset$Vaccine.product == "all_types" & dataset$Age.group == "50-64",]
olddeaths = oldvaxtypes[oldvaxtypes$outcome == "death",]
oldcases = oldvaxtypes[oldvaxtypes$outcome == "case",]

sum(is.na.data.frame(oldcases))
sum(is.na.data.frame(olddeaths))
# Age-adjustments seem to be NA in specific ages.

colnames(olddeaths)
oldvaxdeathrate = olddeaths[6]/olddeaths[7]
oldunvaxdeathrate = olddeaths[8]/olddeaths[9]
colnames(oldvaxdeathrate) = "Vaccinated.death.rate"
colnames(oldunvaxdeathrate) = "Unvaccinated.death.rate"
olddeathsrate = cbind.data.frame(olddeaths,oldunvaxdeathrate,oldvaxdeathrate)
colnames(oldcases)
oldvaxcaserate = oldcases[6]/oldcases[7]
oldunvaxcaserate = oldcases[8]/oldcases[9]
colnames(oldvaxcaserate) = "Vaccinated.case.rate"
colnames(oldunvaxcaserate) = "Unvaccinated.case.rate"
oldcaserate = cbind.data.frame(oldcases,oldunvaxcaserate,oldvaxcaserate)

dim(oldcaserate)
dim(olddeathsrate)

snippedoldcaserate = head(oldcaserate, -3)
oldunvaxcasefatalityratio = olddeathsrate[8]/snippedoldcaserate[8]
oldvaxcasefatalityratio = olddeathsrate[6]/snippedoldcaserate[6]
colnames(oldunvaxcasefatalityratio) = "Unvaccinated"
colnames(oldvaxcasefatalityratio) = "Vaccinated"
oldcasefatalityratios = cbind.data.frame(oldunvaxcasefatalityratio, oldvaxcasefatalityratio)
oldcasefatalityratios = cbind.data.frame(snippedoldcaserate[2:5],oldcasefatalityratios)

olddeathshundredk = olddeathsrate[17:18]*100000
colnames(olddeathshundredk) = c("Unvaccinated","Vaccinated")
olddeathshundredk = cbind.data.frame(olddeathsrate[2:5], olddeathshundredk)

# Golden, 65-79
goldenvaxtypes = dataset[dataset$Vaccine.product == "all_types" & dataset$Age.group == "65-79",]
goldendeaths = goldenvaxtypes[goldenvaxtypes$outcome == "death",]
goldencases = goldenvaxtypes[goldenvaxtypes$outcome == "case",]

sum(is.na.data.frame(goldencases))
sum(is.na.data.frame(goldendeaths))
# Age-adjustments seem to be NA in specific ages.

colnames(goldendeaths)
goldenvaxdeathrate = goldendeaths[6]/goldendeaths[7]
goldenunvaxdeathrate = goldendeaths[8]/goldendeaths[9]
colnames(goldenvaxdeathrate) = "Vaccinated.death.rate"
colnames(goldenunvaxdeathrate) = "Unvaccinated.death.rate"
goldendeathsrate = cbind.data.frame(goldendeaths,goldenunvaxdeathrate,goldenvaxdeathrate)
colnames(goldencases)
goldenvaxcaserate = goldencases[6]/goldencases[7]
goldenunvaxcaserate = goldencases[8]/goldencases[9]
colnames(goldenvaxcaserate) = "Vaccinated.case.rate"
colnames(goldenunvaxcaserate) = "Unvaccinated.case.rate"
goldencaserate = cbind.data.frame(goldencases,goldenunvaxcaserate,goldenvaxcaserate)

dim(goldencaserate)
dim(goldendeathsrate)

snippedgoldencaserate = head(goldencaserate, -3)
goldenunvaxcasefatalityratio = goldendeathsrate[8]/snippedgoldencaserate[8]
goldenvaxcasefatalityratio = goldendeathsrate[6]/snippedgoldencaserate[6]
colnames(goldenunvaxcasefatalityratio) = "Unvaccinated"
colnames(goldenvaxcasefatalityratio) = "Vaccinated"
goldencasefatalityratios = cbind.data.frame(goldenunvaxcasefatalityratio, goldenvaxcasefatalityratio)
goldencasefatalityratios = cbind.data.frame(snippedgoldencaserate[2:5],goldencasefatalityratios)

goldendeathshundredk = goldendeathsrate[17:18]*100000
colnames(goldendeathshundredk) = c("Unvaccinated","Vaccinated")
goldendeathshundredk = cbind.data.frame(goldendeathsrate[2:5], goldendeathshundredk)

# End of Life, 80+
eolvaxtypes = dataset[dataset$Vaccine.product == "all_types" & dataset$Age.group == "80+",]
eoldeaths = eolvaxtypes[eolvaxtypes$outcome == "death",]
eolcases = eolvaxtypes[eolvaxtypes$outcome == "case",]

sum(is.na.data.frame(eolcases))
sum(is.na.data.frame(eoldeaths))
# Age-adjustments seem to be NA in specific ages.

colnames(eoldeaths)
eolvaxdeathrate = eoldeaths[6]/eoldeaths[7]
eolunvaxdeathrate = eoldeaths[8]/eoldeaths[9]
colnames(eolvaxdeathrate) = "Vaccinated.death.rate"
colnames(eolunvaxdeathrate) = "Unvaccinated.death.rate"
eoldeathsrate = cbind.data.frame(eoldeaths,eolunvaxdeathrate,eolvaxdeathrate)
colnames(eolcases)
eolvaxcaserate = eolcases[6]/eolcases[7]
eolunvaxcaserate = eolcases[8]/eolcases[9]
colnames(eolvaxcaserate) = "Vaccinated.case.rate"
colnames(eolunvaxcaserate) = "Unvaccinated.case.rate"
eolcaserate = cbind.data.frame(eolcases,eolunvaxcaserate,eolvaxcaserate)

dim(eolcaserate)
dim(eoldeathsrate)

snippedeolcaserate = head(eolcaserate, -3)
eolunvaxcasefatalityratio = eoldeathsrate[8]/snippedeolcaserate[8]
eolvaxcasefatalityratio = eoldeathsrate[6]/snippedeolcaserate[6]
colnames(eolunvaxcasefatalityratio) = "Unvaccinated"
colnames(eolvaxcasefatalityratio) = "Vaccinated"
eolcasefatalityratios = cbind.data.frame(eolunvaxcasefatalityratio, eolvaxcasefatalityratio)
eolcasefatalityratios = cbind.data.frame(snippedeolcaserate[2:5],eolcasefatalityratios)

eoldeathshundredk = eoldeathsrate[17:18]*100000
colnames(eoldeathshundredk) = c("Unvaccinated","Vaccinated")
eoldeathshundredk = cbind.data.frame(eoldeathsrate[2:5], eoldeathshundredk)

# Janssen All Ages
allagesjajvax = dataset[dataset$Vaccine.product == "Janssen" & dataset$Age.group == "all_ages_adj",]
jajdeaths = allagesjajvax[allagesjajvax$outcome == "death",]
jajcases = allagesjajvax[allagesjajvax$outcome == "case",]

sum(is.na.data.frame(jajcases))
sum(is.na.data.frame(jajdeaths))

colnames(jajdeaths)
jajvaxdeathrate = jajdeaths[6]/jajdeaths[7]
colnames(jajvaxdeathrate) = "Janssen.death.rate"
colnames(jajcases)
jajvaxcaserate = jajcases[6]/jajcases[7]
colnames(jajvaxcaserate) = "Janssen.case.rate"

jajvaxcaseratepercent = percent(jajvaxcaserate$Janssen.case.rate)
jajvaxdeathratepercent = percent(jajvaxdeathrate$Janssen.death.rate)

jajvaxcaseratepercent = as.data.frame(jajvaxcaseratepercent)
colnames(jajvaxcaseratepercent) = "Janssen.case.percent"
jajvaxdeathratepercent = as.data.frame(jajvaxdeathratepercent)
colnames(jajvaxdeathratepercent) = "Janssen.death.percent"

dim(jajvaxcaserate)
dim(jajvaxdeathrate)

snippedjajvaxcaserate = head(jajvaxcaserate, -3)
jajcasefatalityratio = jajvaxdeathrate/snippedjajvaxcaserate
colnames(jajcasefatalityratio) = "Janssen"

jajdeathshundredk = jajvaxdeathrate*100000

# Moderna All Ages
allagesmdrnavax = dataset[dataset$Vaccine.product == "Moderna" & dataset$Age.group == "all_ages_adj",]
mdrnadeaths = allagesmdrnavax[allagesmdrnavax$outcome == "death",]
mdrnacases = allagesmdrnavax[allagesmdrnavax$outcome == "case",]

sum(is.na.data.frame(mdrnacases))
sum(is.na.data.frame(mdrnadeaths))

colnames(mdrnadeaths)
mdrnavaxdeathrate = mdrnadeaths[6]/mdrnadeaths[7]
colnames(mdrnavaxdeathrate) = "Moderna.death.rate"
colnames(mdrnacases)
mdrnavaxcaserate = mdrnacases[6]/mdrnacases[7]
colnames(mdrnavaxcaserate) = "Moderna.case.rate"

mdrnavaxcaseratepercent = percent(mdrnavaxcaserate$Moderna.case.rate)
mdrnavaxdeathratepercent = percent(mdrnavaxdeathrate$Moderna.death.rate)

mdrnavaxcaseratepercent = as.data.frame(mdrnavaxcaseratepercent)
colnames(mdrnavaxcaseratepercent) = "Moderna.case.percent"
mdrnavaxdeathratepercent = as.data.frame(mdrnavaxdeathratepercent)
colnames(mdrnavaxdeathratepercent) = "Moderna.death.percent"

dim(mdrnavaxcaserate)
dim(mdrnavaxdeathrate)

snippedmdrnavaxcaserate = head(mdrnavaxcaserate, -3)
mdrnacasefatalityratio = mdrnavaxdeathrate/snippedmdrnavaxcaserate
colnames(mdrnacasefatalityratio) = "Moderna"

mdrnadeathshundredk = mdrnavaxdeathrate*100000

# Pfizer All Ages
allagespfzrvax = dataset[dataset$Vaccine.product == "Pfizer" & dataset$Age.group == "all_ages_adj",]
pfzrdeaths = allagespfzrvax[allagespfzrvax$outcome == "death",]
pfzrcases = allagespfzrvax[allagespfzrvax$outcome == "case",]

sum(is.na.data.frame(pfzrcases))
sum(is.na.data.frame(pfzrdeaths))

colnames(pfzrdeaths)
pfzrvaxdeathrate = pfzrdeaths[6]/pfzrdeaths[7]
colnames(pfzrvaxdeathrate) = "Pfizer.death.rate"
colnames(pfzrcases)
pfzrvaxcaserate = pfzrcases[6]/pfzrcases[7]
colnames(pfzrvaxcaserate) = "Pfizer.case.rate"

pfzrvaxcaseratepercent = percent(pfzrvaxcaserate$Pfizer.case.rate)
pfzrvaxdeathratepercent = percent(pfzrvaxdeathrate$Pfizer.death.rate)

pfzrvaxcaseratepercent = as.data.frame(pfzrvaxcaseratepercent)
colnames(pfzrvaxcaseratepercent) = "Pfizer.case.percent"
pfzrvaxdeathratepercent = as.data.frame(pfzrvaxdeathratepercent)
colnames(pfzrvaxdeathratepercent) = "Pfizer.death.percent"

dim(pfzrvaxcaserate)
dim(pfzrvaxdeathrate)

snippedpfzrvaxcaserate = head(pfzrvaxcaserate, -3)
pfzrcasefatalityratio = pfzrvaxdeathrate/snippedpfzrvaxcaserate
colnames(pfzrcasefatalityratio) = "Pfizer"

pfzrdeathshundredk = pfzrvaxdeathrate*100000

# Combining vaccinated deaths per 100ks for all ages
deathshundredk = cbind.data.frame(deathshundredk,jajdeathshundredk,mdrnadeathshundredk,pfzrdeathshundredk)
casefatalityratios = cbind.data.frame(casefatalityratios, jajcasefatalityratio, mdrnacasefatalityratio, pfzrcasefatalityratio)

datasetcasefatalityratio = cbind.data.frame(head(datasetcases, -30),datasetdeaths[,6:9])
datasetcasefatalityratio = datasetcasefatalityratio[,-18]
datasetcasefatalityratio = datasetcasefatalityratio[,-19]
datasetcasefatalityratio$Vaccinated.case.fatality.ratio = datasetcasefatalityratio[,17]/datasetcasefatalityratio[,6]
datasetcasefatalityratio$Unvaccinated.case.fatality.ratio = datasetcasefatalityratio[,18]/datasetcasefatalityratio[,8]

# Plotting case fatality ratios for vaccines
datasetvaxcasefatalityratio = subset(datasetcasefatalityratio, Vaccine.product %in% c("Janssen","Moderna","Pfizer"))
datasetunvaxcasefatalityratio = datasetcasefatalityratio[datasetcasefatalityratio$Vaccine.product == "all_types",]

g <- ggplot(datasetvaxcasefatalityratio, aes(x=Vaccine.product,y=Vaccinated.case.fatality.ratio))
g + geom_boxplot(varwidth=T, fill="plum") +
labs(title="Box plot of Case Fatality Ratios",
subtitle="By Vaccination Status and Product Type",
caption="Source: CDC",
x="Status/Vaccine Type",
y="Case Fatality Ratio")

# Case Fatality Ratios are much higher for the non-J&J jabs.

g <- ggplot(datasetunvaxcasefatalityratio, aes(x=Vaccine.product,y=Unvaccinated.case.fatality.ratio))
g + geom_boxplot(varwidth=T, fill="plum") +
labs(title="Box plot of Case Fatality Ratios",
subtitle="By Vaccination Status and Product Type",
caption="Source: CDC",
x="Status/Vaccine Type",
y="Case Fatality Ratio")

# Are the case fatality ratios stable between vaccine types? T-Test
# Are vaccinated and unvaccinated statistically different on deaths per 100k and case fatality ratios?
# Are the deaths per 100k or the case fatality ratios stable across age groups within vaccinated vs unvaccinated?

# Need to plot case and death rates over time

# Need to get CDC data on hospitalizations and death characteristics. Over time would be excellent

