setwd("~/R/covid")
.libPaths(c("C:/Users/geoff/Documents/R/win-library/3.6"))
install.packages("ggplot2")
install.packages("scales")
install.packages("ggpubr")
library("ggplot2")
library("scales")
library("ggpubr")

filename <- "Rates_of_COVID-19_Cases_or_Deaths_by_Age_Group_and_Vaccination_Status.csv"
dataset <- read.csv(filename)
dim(dataset)
sapply(dataset, class)
levels(dataset$outcome)
levels(dataset$Age.group)
sum(is.na.data.frame(dataset))

# All Ages
allagesvaxtypes = dataset[dataset$Vaccine.product == "all_types" & dataset$Age.group == "all_ages_adj",]
deaths = dataset[dataset$outcome == "death",]
cases = dataset[dataset$outcome == "case",]

sum(is.na.data.frame(cases))
sum(is.na.data.frame(deaths))

colnames(deaths)
vaxdeathrate = deaths[6]/deaths[7]
unvaxdeathrate = deaths[8]/deaths[9]
colnames(vaxdeathrate) = "Death.rate"
vaxdeathrate$Vaccinated = "Yes"
colnames(unvaxdeathrate) = "Death.rate"
unvaxdeathrate$Vaccinated = "No"
vaxdeathrate = cbind.data.frame(deaths,vaxdeathrate)
unvaxdeathrate = cbind.data.frame(deaths,unvaxdeathrate)
deathsrate = rbind.data.frame(unvaxdeathrate,vaxdeathrate)
colnames(cases)
vaxcaserate = cases[6]/cases[7]
unvaxcaserate = cases[8]/cases[9]
colnames(vaxcaserate) = "Case.rate"
vaxcaserate$Vaccinated = "Yes"
colnames(unvaxcaserate) = "Case.rate"
unvaxcaserate$Vaccinated = "No"
vaxcaserate = cbind.data.frame(cases,vaxcaserate)
unvaxcaserate = cbind.data.frame(cases,unvaxcaserate)
caserate = rbind.data.frame(unvaxcaserate,vaxcaserate)

caserate$Case.rate = as.data.frame(percent(caserate$Case.rate))

unvaxcaserate$Case.rate = percent(caserate$Case.rate)
vaxcaseratepercent = percent(caserate$Case.rate)
unvaxdeathratepercent = percent(deathsrate$Unvaccinated.death.rate)
vaxdeathratepercent = percent(deathsrate$Vaccinated.death.rate)
vaxtypedeathratepercent = percent(deathsratevaxtype$Death.rate)

dim(deaths)
dim(cases)

snippedcases = head(cases, -30)
unvaxcasefatalityratio = deaths$Unvaccinated.with.outcome/snippedcases$Unvaccinated.with.outcome
unvaxcasefatalityratio = cbind.data.frame(snippedcases[2:5], unvaxcasefatalityratio)
unvaxcasefatalityratio$Vaccinated = "No"
colnames(unvaxcasefatalityratio) = c("month","MMWR.week","Age.group","Vaccine.product","Case.fatality.ratio","Vaccinated")
vaxcasefatalityratio = deaths$Vaccinated.with.outcome/snippedcases$Vaccinated.with.outcome
vaxcasefatalityratio = cbind.data.frame(snippedcases[2:5], vaxcasefatalityratio)
vaxcasefatalityratio$Vaccinated = "Yes"
colnames(vaxcasefatalityratio) = c("month","MMWR.week","Age.group","Vaccine.product","Case.fatality.ratio","Vaccinated")

deathsrate$Deaths.per.100k = deathsrate$Death.rate*100000
caserate$Cases.per.100k = caserate$Case.rate*100000

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

# Plotting case fatality ratios for vaccines
vaxtypecasefatalityratio = subset(vaxcasefatalityratio, Vaccine.product %in% c("Janssen","Moderna","Pfizer"))
casefatalityratio = rbind.data.frame(unvaxcasefatalityratio, vaxcasefatalityratio)
graphcfr = casefatalityratio[casefatalityratio$Vaccine.product == "all_types",]

v <- ggplot(vaxtypecasefatalityratio, aes(x=Vaccine.product,y=Case.fatality.ratio)) + geom_boxplot(varwidth=T, fill="plum") +
labs(title="Box plot of Vaccinated Case Fatality Ratios",
subtitle="By Vaccination Product Type",
caption="Source: CDC",
x="Vaccine Type",
y="Case Fatality Ratio")

u <- ggplot(graphcfr, aes(x=Vaccinated,y=Case.fatality.ratio)) + geom_boxplot(varwidth=T, fill="plum", aes(color=Age.group)) +
  labs(title="Box plot of Case Fatality Ratios by Vaccination Status",
       subtitle="Yes = Vaccinated",
       caption="Source: CDC",
       x="Vaccination Status",
       y="Case Fatality Ratio")

genfigure = ggarrange(v, u,
                   ncol = 2)
genfigure

# Plotting Deaths per 100k for vaccines
vaxdeathrate = deathsrate[deathsrate$Vaccinated == "Yes",]
vaxdeathshundredtype = subset(vaxdeathrate, Vaccine.product %in% c("Janssen","Moderna","Pfizer"))

v100k <- ggplot(vaxdeathshundredtype, aes(x=Vaccine.product,y=Deaths.per.100k)) + geom_boxplot(varwidth=T, fill="plum") +
  labs(title="Box plot of Vaccinated Deaths Per 100k",
       subtitle="By Vaccination Product Type",
       caption="Source: CDC",
       x="Vaccine Type",
       y="Deaths Per 100k")

u100k <- ggplot(deathsrate, aes(x=Vaccinated,y=Deaths.per.100k)) + geom_boxplot(varwidth=T, fill="plum", aes(color=Vaccine.product)) +
  labs(title="Box plot of Deaths Per 100k by Vaccination Status",
       subtitle="Yes = Vaccinated",
       caption="Source: CDC",
       x="Vaccination Status",
       y="Deaths Per 100k")

age100k <- ggplot(deathsrate, aes(x=Vaccinated,y=Deaths.per.100k)) + geom_boxplot(varwidth=T, fill="plum", aes(color=Age.group)) +
  labs(title="Box plot of Deaths Per 100k by Vaccination Status",
       subtitle="Yes = Vaccinated",
       caption="Source: CDC",
       x="Vaccination Status",
       y="Deaths Per 100k")

gen100kfig = ggarrange(v100k, u100k, age100k,
                       ncol = 2)
gen100kfig

# Plotting weekly Case Fatality Ratios, seems seasonal
allagescfr = casefatalityratio[casefatalityratio$Age.group == "all_ages_adj",]
ln <-ggplot(allagescfr, aes(x=MMWR.week, y=Case.fatality.ratio, group=Vaccine.product)) +
geom_line(aes(linetype=Vaccine.product, color=Vaccine.product))+
geom_point(aes(shape=Vaccine.product, color=Vaccine.product)) +
  labs(title="Graph of Weekly Case Fatality Ratios by Vaccine Product",
       subtitle="All ages, adjusted",
       caption="Source: CDC",
       x="MMWR Week",
       y="Case Fatality Ratio")
ln

ln1 <-ggplot(allagescfr, aes(x=MMWR.week, y=Case.fatality.ratio, group=Vaccinated)) +
  geom_line(aes(linetype=Vaccinated, color=Vaccinated))+
  geom_point(aes(shape=Vaccinated, color=Vaccinated)) +
  labs(title="Graph of Weekly Case Fatality Ratios by Vaccine Status",
       subtitle="All ages, adjusted",
       caption="Source: CDC",
       x="MMWR Week",
       y="Case Fatality Ratio")
ln1

ln2 <-ggplot(casefatalityratio, aes(x=MMWR.week, y=Case.fatality.ratio, group=Age.group)) +
  geom_line(aes(linetype=Age.group, color=Age.group))+
  geom_point(aes(shape=Age.group, color=Age.group)) +
  labs(title="Graph of Weekly Case Fatality Ratios by Age",
       subtitle="All ages, adjusted",
       caption="Source: CDC",
       x="MMWR Week",
       y="Case Fatality Ratio")

# Plotting weekly Deaths Per 100k
allagesd100k = deathsrate[deathsrate$Age.group == "all_ages_adj",]
vaxallages100k = allagesd100k[allagesd100k$Vaccinated == "Yes",]
ln3 <-ggplot(vaxallages100k, aes(x=MMWR.week, y=Deaths.per.100k, group=Vaccine.product)) +
  geom_line(aes(linetype=Vaccine.product, color=Vaccine.product))+
  geom_point(aes(shape=Vaccine.product, color=Vaccine.product)) +
  labs(title="Graph of Weekly Deaths Per 100k by Vaccine Product",
       subtitle="All ages, adjusted",
       caption="Source: CDC",
       x="MMWR Week",
       y="Deaths Per 100k")
ln3

ln4 <-ggplot(allagesd100k, aes(x=MMWR.week, y=Deaths.per.100k, group=Vaccinated)) +
  geom_line(aes(linetype=Vaccinated, color=Vaccinated))+
  geom_point(aes(shape=Vaccinated, color=Vaccinated)) +
  labs(title="Graph of Weekly Deaths Per 100k by Vaccine Status",
       subtitle="All ages, adjusted",
       caption="Source: CDC",
       x="MMWR Week",
       y="Deaths Per 100k")
ln4

ln5 <-ggplot(deathsrate, aes(x=MMWR.week, y=Deaths.per.100k, group=Age.group)) +
  geom_line(aes(linetype=Age.group, color=Age.group))+
  geom_point(aes(shape=Age.group, color=Age.group)) +
  labs(title="Graph of Weekly Deaths Per 100k by Age",
       subtitle="All ages, adjusted",
       caption="Source: CDC",
       x="MMWR Week",
       y="Deaths Per 100k")
ln5

# Are the case fatality ratios stable between vaccine types? The distribution of case fatalities seems normal enough to use a two-sample T-test.

pfizercfr = allagescfr[allagescfr$Vaccine.product == "Pfizer",]
mdrnacfr = allagescfr[allagescfr$Vaccine.product == "Moderna",]
jajcfr = allagescfr[allagescfr$Vaccine.product == "Janssen",]
pfizercfr = pfizercfr[pfizercfr$Vaccinated == "Yes",]
mdrnacfr = mdrnacfr[mdrnacfr$Vaccinated == "Yes",]
jajcfr = jajcfr[jajcfr$Vaccinated == "Yes",]
dim(pfizercfr)
dim(mdrnacfr)
dim(jajcfr)
# Not sure if the vaccinated CFRs are actually normally distributed. Moderna and Pfizer seem skewed, particularly compared to Janssen. 
shapiro.test(jajcfr$Case.fatality.ratio) #J&J not normally distributed.
shapiro.test(mdrnacfr$Case.fatality.ratio) #Moderna normally distributed.
shapiro.test(pfizercfr$Case.fatality.ratio) #Pfizer not normally distributed.

# Let's see the Wilcox test.
wilcox.test(mdrnacfr$Case.fatality.ratio, pfizercfr$Case.fatality.ratio, paired=TRUE) # Low p-value indicates significant difference. Moderna and Pfizer's CFRs are significantly different
wilcox.test(mdrnacfr$Case.fatality.ratio, pfizercfr$Case.fatality.ratio, paired=TRUE, alternative="greater") # Moderna's CFR is significantly higher than Pfizer's - why?
wilcox.test(jajcfr$Case.fatality.ratio, pfizercfr$Case.fatality.ratio, paired=TRUE) # J&J and Pfizer have similar CFRs.
wilcox.test(jajcfr$Case.fatality.ratio, mdrnacfr$Case.fatality.ratio, paired=TRUE) # Significant difference between Moderna and J&J
wilcox.test(jajcfr$Case.fatality.ratio, mdrnacfr$Case.fatality.ratio, paired=TRUE, alternative="less") # Janssen's CFR is significantly lower than Moderna's

# Are the vaccinated and unvaccinated statistically different on case fatality ratios?
shapiro.test(vaxcasefatalityratio$Case.fatality.ratio)
shapiro.test(unvaxcasefatalityratio$Case.fatality.ratio)
shapiro.test(subset(vaxcasefatalityratio, Age.group == "12-17")$Case.fatality.ratio)
shapiro.test(subset(unvaxcasefatalityratio, Age.group == "12-17")$Case.fatality.ratio)
shapiro.test(subset(vaxcasefatalityratio, Age.group == "18-29")$Case.fatality.ratio)
shapiro.test(subset(unvaxcasefatalityratio, Age.group == "18-29")$Case.fatality.ratio) # Not normally distributed, will use a wilcox test.
shapiro.test(subset(vaxcasefatalityratio, Age.group == "30-49")$Case.fatality.ratio)
shapiro.test(subset(unvaxcasefatalityratio, Age.group == "30-49")$Case.fatality.ratio) # Not normally distributed, will use a wilcox test.
shapiro.test(subset(vaxcasefatalityratio, Age.group == "50-64")$Case.fatality.ratio)
shapiro.test(subset(unvaxcasefatalityratio, Age.group == "50-64")$Case.fatality.ratio) # Not normally distributed, will use a wilcox test.
shapiro.test(subset(vaxcasefatalityratio, Age.group == "65-79")$Case.fatality.ratio) # Not normally distributed
shapiro.test(subset(unvaxcasefatalityratio, Age.group == "65-79")$Case.fatality.ratio) # Not normally distributed
shapiro.test(subset(vaxcasefatalityratio, Age.group == "80+")$Case.fatality.ratio) # Not normally distributed
shapiro.test(subset(unvaxcasefatalityratio, Age.group == "80+")$Case.fatality.ratio) # Not normally distributed

# All that are normally distributed are given a T-test.
t.test(unvaxcasefatalityratio$Case.fatality.ratio, vaxcasefatalityratio$Case.fatality.ratio, paired=TRUE) # Significant difference between vaccinated and unvaccinated CFRs.
t.test(subset(unvaxcasefatalityratio, Age.group == "12-17")$Case.fatality.ratio, subset(vaxcasefatalityratio, Age.group == "12-17")$Case.fatality.ratio, paired=TRUE) # Significant difference between vaccinated and unvaccinated CFRs.

# Wilcox test for the abnormals
wilcox.test(subset(unvaxcasefatalityratio, Age.group == "18-29")$Case.fatality.ratio, subset(vaxcasefatalityratio, Age.group == "18-29")$Case.fatality.ratio, paired=TRUE) # Significant difference
wilcox.test(subset(unvaxcasefatalityratio, Age.group == "18-29")$Case.fatality.ratio, subset(vaxcasefatalityratio, Age.group == "18-29")$Case.fatality.ratio, paired=TRUE, alternative="greater") # Significantly higher rates among unvaccinated.
wilcox.test(subset(unvaxcasefatalityratio, Age.group == "30-49")$Case.fatality.ratio, subset(vaxcasefatalityratio, Age.group == "30-49")$Case.fatality.ratio, paired=TRUE) # Significant difference
wilcox.test(subset(unvaxcasefatalityratio, Age.group == "30-49")$Case.fatality.ratio, subset(vaxcasefatalityratio, Age.group == "30-49")$Case.fatality.ratio, paired=TRUE, alternative="greater") # Significantly higher rates among unvaccinated.
wilcox.test(subset(unvaxcasefatalityratio, Age.group == "50-64")$Case.fatality.ratio, subset(vaxcasefatalityratio, Age.group == "50-64")$Case.fatality.ratio, paired=TRUE) # Significant difference
wilcox.test(subset(unvaxcasefatalityratio, Age.group == "50-64")$Case.fatality.ratio, subset(vaxcasefatalityratio, Age.group == "50-64")$Case.fatality.ratio, paired=TRUE, alternative="greater") # Significantly higher rates among unvaccinated.
wilcox.test(subset(unvaxcasefatalityratio, Age.group == "65-79")$Case.fatality.ratio, subset(vaxcasefatalityratio, Age.group == "65-79")$Case.fatality.ratio, paired=TRUE) # Significant difference
wilcox.test(subset(unvaxcasefatalityratio, Age.group == "65-79")$Case.fatality.ratio, subset(vaxcasefatalityratio, Age.group == "65-79")$Case.fatality.ratio, paired=TRUE, alternative="greater") # Significantly higher rates among unvaccinated.
wilcox.test(subset(unvaxcasefatalityratio, Age.group == "80+")$Case.fatality.ratio, subset(vaxcasefatalityratio, Age.group == "80+")$Case.fatality.ratio, paired=TRUE) # Significant difference
wilcox.test(subset(unvaxcasefatalityratio, Age.group == "80+")$Case.fatality.ratio, subset(vaxcasefatalityratio, Age.group == "80+")$Case.fatality.ratio, paired=TRUE, alternative="greater") # Significantly higher rates among unvaccinated.

# Not sure if the vaccinated Deaths Per 100k are actually normally distributed. Moderna and Pfizer seem skewed, particularly compared to Janssen. 
shapiro.test(subset(vaxdeathrate, Vaccine.product == "Janssen")$Deaths.per.100k)
shapiro.test(subset(vaxdeathrate, Vaccine.product == "Moderna")$Deaths.per.100k)
shapiro.test(subset(vaxdeathrate, Vaccine.product == "Pfizer")$Deaths.per.100k)

# T-Test is appropriate
t.test(subset(vaxdeathrate, Vaccine.product == "Janssen")$Deaths.per.100k, subset(vaxdeathrate, Vaccine.product == "Moderna")$Deaths.per.100k, paired=TRUE) # Significant difference
t.test(subset(vaxdeathrate, Vaccine.product == "Janssen")$Deaths.per.100k, subset(vaxdeathrate, Vaccine.product == "Moderna")$Deaths.per.100k, paired=TRUE, alternative="greater") # Janssen deaths per 100k are significantly higher.
t.test(subset(vaxdeathrate, Vaccine.product == "Pfizer")$Deaths.per.100k, subset(vaxdeathrate, Vaccine.product == "Moderna")$Deaths.per.100k, paired=TRUE) # Significant difference
t.test(subset(vaxdeathrate, Vaccine.product == "Pfizer")$Deaths.per.100k, subset(vaxdeathrate, Vaccine.product == "Moderna")$Deaths.per.100k, paired=TRUE, alternative = "greater") # Pfizer deaths per 100k are significantly higher.
t.test(subset(vaxdeathrate, Vaccine.product == "Pfizer")$Deaths.per.100k, subset(vaxdeathrate, Vaccine.product == "Janssen")$Deaths.per.100k, paired=TRUE) # Significant difference
t.test(subset(vaxdeathrate, Vaccine.product == "Pfizer")$Deaths.per.100k, subset(vaxdeathrate, Vaccine.product == "Janssen")$Deaths.per.100k, paired=TRUE, alternative = "less") # Pfizer deaths per 100k are significantly lower.

# Are the vaccinated and unvaccinated statistically different on Deaths per 100k?
unvaxdeathrate = deathsrate[deathsrate$Vaccinated == "No",]
shapiro.test(vaxdeathrate$Deaths.per.100k)
shapiro.test(unvaxdeathrate$Deaths.per.100k)
shapiro.test(subset(vaxdeathrate, Age.group == "12-17")$Deaths.per.100k)
shapiro.test(subset(unvaxdeathrate, Age.group == "12-17")$Deaths.per.100k)
shapiro.test(subset(vaxdeathrate, Age.group == "18-29")$Deaths.per.100k)
shapiro.test(subset(unvaxdeathrate, Age.group == "18-29")$Deaths.per.100k)
shapiro.test(subset(vaxdeathrate, Age.group == "30-49")$Deaths.per.100k) # Not normally distributed, will use a wilcox test.
shapiro.test(subset(unvaxdeathrate, Age.group == "30-49")$Deaths.per.100k)
shapiro.test(subset(vaxdeathrate, Age.group == "50-64")$Deaths.per.100k)
shapiro.test(subset(unvaxdeathrate, Age.group == "50-64")$Deaths.per.100k) 
shapiro.test(subset(vaxdeathrate, Age.group == "65-79")$Deaths.per.100k)
shapiro.test(subset(unvaxdeathrate, Age.group == "65-79")$Deaths.per.100k)
shapiro.test(subset(vaxdeathrate, Age.group == "80+")$Deaths.per.100k)
shapiro.test(subset(unvaxdeathrate, Age.group == "80+")$Deaths.per.100k) # Not normally distributed.

# T-Test where appropriate
t.test(unvaxdeathrate$Deaths.per.100k, vaxdeathrate$Deaths.per.100k, paired=TRUE) # Significantly different death rates
t.test(subset(unvaxdeathrate, Age.group == "12-17")$Deaths.per.100k, subset(vaxdeathrate, Age.group == "12-17")$Deaths.per.100k, paired=TRUE)
t.test(subset(unvaxdeathrate, Age.group == "12-17")$Deaths.per.100k, subset(vaxdeathrate, Age.group == "12-17")$Deaths.per.100k, paired=TRUE, alternative="greater") #Unvaxxed rate is significantly higher
t.test(subset(unvaxdeathrate, Age.group == "18-29")$Deaths.per.100k, subset(vaxdeathrate, Age.group == "18-29")$Deaths.per.100k, paired=TRUE)
t.test(subset(unvaxdeathrate, Age.group == "18-29")$Deaths.per.100k, subset(vaxdeathrate, Age.group == "18-29")$Deaths.per.100k, paired=TRUE, alternative = "greater") #Unvaxxed rate is significantly higher
t.test(subset(unvaxdeathrate, Age.group == "50-64")$Deaths.per.100k, subset(vaxdeathrate, Age.group == "50-64")$Deaths.per.100k, paired=TRUE)
t.test(subset(unvaxdeathrate, Age.group == "50-64")$Deaths.per.100k, subset(vaxdeathrate, Age.group == "50-64")$Deaths.per.100k, paired=TRUE, alternative = "greater") #Unvaxxed rate is significantly higher
t.test(subset(unvaxdeathrate, Age.group == "65-79")$Deaths.per.100k, subset(vaxdeathrate, Age.group == "65-79")$Deaths.per.100k, paired=TRUE)
t.test(subset(unvaxdeathrate, Age.group == "65-79")$Deaths.per.100k, subset(vaxdeathrate, Age.group == "65-79")$Deaths.per.100k, paired=TRUE, alternative = "greater") #Unvaxxed rate is significantly higher

# Wilcox tests for the abnormals
wilcox.test(subset(unvaxdeathrate, Age.group == "30-49")$Deaths.per.100k, subset(vaxdeathrate, Age.group == "30-49")$Deaths.per.100k, paired=TRUE)
wilcox.test(subset(unvaxdeathrate, Age.group == "30-49")$Deaths.per.100k, subset(vaxdeathrate, Age.group == "30-49")$Deaths.per.100k, paired=TRUE, alternative = "greater") #Unvaxxed rate is significantly higher
wilcox.test(subset(unvaxdeathrate, Age.group == "80+")$Deaths.per.100k, subset(vaxdeathrate, Age.group == "80+")$Deaths.per.100k, paired=TRUE)
wilcox.test(subset(unvaxdeathrate, Age.group == "80+")$Deaths.per.100k, subset(vaxdeathrate, Age.group == "80+")$Deaths.per.100k, paired=TRUE, alternative="greater")

# Plotting deaths by age (Vax vs Unvax) with Chi-sq test of deaths vs population
d = sum(subset(deaths, Age.group == "12-17")$Vaccinated.with.outcome)
d1 = sum(subset(deaths, Age.group == "18-29")$Vaccinated.with.outcome)
d2 = sum(subset(deaths, Age.group == "30-49")$Vaccinated.with.outcome)
d3 = sum(subset(deaths, Age.group == "50-64")$Vaccinated.with.outcome)
d4 = sum(subset(deaths, Age.group == "65-79")$Vaccinated.with.outcome)
d5 = sum(subset(deaths, Age.group == "80+")$Vaccinated.with.outcome)
deathagepie = data.frame(deaths=c(d,d1,d2,d3,d4,d5), ages= c("12-17","18-29","30-49","50-64","65-79","80+"))

da = mean(subset(deaths, Age.group == "12-17")$Fully.vaccinated.population)
da1 = mean(subset(deaths, Age.group == "18-29")$Fully.vaccinated.population)
da2 = mean(subset(deaths, Age.group == "30-49")$Fully.vaccinated.population)
da3 = mean(subset(deaths, Age.group == "50-64")$Fully.vaccinated.population)
da4 = mean(subset(deaths, Age.group == "65-79")$Fully.vaccinated.population)
da5 = mean(subset(deaths, Age.group == "80+")$Fully.vaccinated.population)
vaxpopdist = data.frame(Population=c(da,da1,da2,da3,da4,da5), ages= c("12-17","18-29","30-49","50-64","65-79","80+"), Vaccinated="Yes")
vaxpopprob = c(da,da1,da2,da3,da4,da5)/sum(da,da1,da2,da3,da4,da5)

p1 = ggplot(deathagepie, aes(x="", y=deaths, fill=ages)) +
  geom_col() +
  coord_polar(theta = "y") +
  geom_text(aes(label = deaths),
            position = position_stack(vjust = 0.5)) +
  labs(title="Vaccinated deaths by age",
                   caption="Source: CDC")+
  theme_void()

p2 = ggplot(vaxpopdist, aes(x="", y=Population, fill=ages)) +
  geom_col() +
  coord_polar(theta = "y") +
  labs(title="Vaccinated by age",
       caption="Source: CDC")+
  theme_void()

vaxpie = ggarrange(p1, p2,
          ncol = 2)
vaxpie

chisq.test(deathagepie$deaths, vaxpopprob) # The distribution of vaccinated deaths were significantly skewed toward the elderly vs. vaccinated population distribution.


du = sum(subset(deaths, Age.group == "12-17")$Unvaccinated.with.outcome)
du1 = sum(subset(deaths, Age.group == "18-29")$Unvaccinated.with.outcome)
du2 = sum(subset(deaths, Age.group == "30-49")$Unvaccinated.with.outcome)
du3 = sum(subset(deaths, Age.group == "50-64")$Unvaccinated.with.outcome)
du4 = sum(subset(deaths, Age.group == "65-79")$Unvaccinated.with.outcome)
du5 = sum(subset(deaths, Age.group == "80+")$Unvaccinated.with.outcome)
unvaxdeathagepie = data.frame(deaths=c(du,du1,du2,du3,du4,du5), ages= c("12-17","18-29","30-49","50-64","65-79","80+"))

dau = mean(subset(deaths, Age.group == "12-17")$Unvaccinated.population)
dau1 = mean(subset(deaths, Age.group == "18-29")$Unvaccinated.population)
dau2 = mean(subset(deaths, Age.group == "30-49")$Unvaccinated.population)
dau3 = mean(subset(deaths, Age.group == "50-64")$Unvaccinated.population)
dau4 = mean(subset(deaths, Age.group == "65-79")$Unvaccinated.population)
dau5 = mean(subset(deaths, Age.group == "80+")$Unvaccinated.population)
unvaxpopdist = data.frame(Population=c(dau,dau1,dau2,dau3,dau4,dau5), ages= c("12-17","18-29","30-49","50-64","65-79","80+"), Vaccinated="No")
unvaxpopprob = c(dau,dau1,dau2,dau3,dau4,dau5)/sum(dau,dau1,dau2,dau3,dau4,dau5)

p3 = ggplot(unvaxdeathagepie, aes(x="", y=deaths, fill=ages)) +
  geom_col() +
  coord_polar(theta = "y") +
  geom_text(aes(label = deaths),
            position = position_stack(vjust = 0.5)) +
  labs(title="Unvaccinated deaths by age",
       caption="Source: CDC")+
  theme_void()

p4 = ggplot(unvaxpopdist, aes(x="", y=Population, fill=ages)) +
  geom_col() +
  coord_polar(theta = "y") +
  labs(title="Unvaccinated by age",
       caption="Source: CDC")+
  theme_void()

unvaxpie = ggarrange(p3, p4,
                   ncol = 2)
unvaxpie

chisq.test(unvaxdeathagepie$deaths, unvaxpopprob) # The distribution of unvaccinated deaths were also skewed older, although not as much.

# Need to plot case and death rates over time

# Need to get CDC data on hospitalizations and death characteristics. Over time would be excellent

