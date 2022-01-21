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

