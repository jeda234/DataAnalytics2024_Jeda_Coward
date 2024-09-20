####### Data Analytics Fall 2024 Lab 01 ######

library(ggplot2)

### read in data
epi.results <- read.csv("C:\\Users\\cowarj\\Documents\\DataAnalytics\\Lab2\\epi2024results06022024.csv")
epi.weights <- read.csv("C:\\Users\\cowarj\\Documents\\DataAnalytics\\Lab2\\epi2024weights.csv")

View(epi.results)
View(epi.weights)

#### Exploratory Analysis ####

epi.results$EPI.new

epi.results[1,5]

attach(epi.results)

EPI.new

EPI.new[1]

## NA values
na.indices <- is.na(EPI.new) 

## drop NAs
Epi.new.compl <- EPI.new[!na.indices]

## convert to data frame and add country
Epi.new.compl <- data.frame(Country = country[!na.indices], EPI = EPI.new[!na.indices])

## summary stats
summary(EPI.new)

fivenum(EPI.new,na.rm=TRUE)

## histograms
hist(EPI.new)

hist(EPI.new, seq(20., 80., 2.0), prob=TRUE)

rug(EPI.new)

lines(density(EPI.new,na.rm=TRUE,bw=1))
lines(density(EPI.new,na.rm=TRUE,bw="SJ"))

x <- seq(20., 80., 1.0)
qn<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,0.4*qn)

qn<- dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,0.12*qn)

##################

### Comparing distributions of 2 variables

boxplot(EPI.old, EPI.new, names=c("EPI.old","EPI.new"))

### Comparing distributions of 3 variables

boxplot(MKP.new, MHP.new, MPE.new, names=c("MKP.new","MHP.new", "MPE.new"))

### Quantile-quantile plots

qqnorm(EPI.new)
qqline(EPI.new)

x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)),EPI.new)
qqline(EPI.new)

qqplot(rnorm(1000),EPI.new)
qqline(EPI.new)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d1)
qqline(d1)

### Quantile-quantile plots - MKP
qqplot(qnorm(ppoints(200)),MKP.new)
qqline(MKP.new)
summary(MKP.new)

qqplot(rnorm(1000),MKP.new)
qqline(MKP.new)

### Quantile-quantile plots - MHP
qqplot(qnorm(ppoints(200)),MHP.new)
qqline(MHP.new)
summary(MHP.new)

qqplot(rnorm(1000),MHP.new)
qqline(MHP.new)

### Quantile-quantile plots - MPE
qqplot(qnorm(ppoints(200)),MPE.new)
qqline(MPE.new)
summary(MPE.new)

qqplot(rnorm(1000),MPE.new)
qqline(MPE.new)

### Empirical Cumulative Distribution Function
plot(ecdf(EPI.new), do.points=FALSE) 

plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE, main="Norm Dist vs. EPI.new ECDF")
lines(ecdf(EPI.new))

plot(ecdf(EPI.old), do.points=FALSE, main="EPI.old vs. EPI.new ECDF")
lines(ecdf(EPI.new))

### Empirical Cumulative Distribution Function - MKP
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE, main="Norm Dist vs. MKP.new ECDF")
lines(ecdf(MKP.new))

### Empirical Cumulative Distribution Function - MHP
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE, main="Norm Dist vs. MHP.new ECDF")
lines(ecdf(MHP.new))

### Empirical Cumulative Distribution Function - MPE
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE, main="Norm Dist vs. MPE.new ECDF")
lines(ecdf(MPE.new))

#### Populations Dataset ####

## read data
populations_2023 <- read.csv("C:\\Users\\cowarj\\Documents\\DataAnalytics\\Lab2\\countries_populations_2023.csv")

## drop country populations that don't exist in epi results
populations <- populations_2023[-which(!populations_2023$Country %in% epi.results$country),]

## sort populations by country name
populations <- populations[order(populations$Country),]

## drop country results that don't exist in populations
epi.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]

## sort results by country name
epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]

## only keep relevant columns
epi.results.sub <- epi.results.sub[,c("country","MKP.old","MKP.new")]

## convert to mnumeric
epi.results.sub$population <- as.numeric(populations$Population)

## compute population log
epi.results.sub$population_log <- log10(epi.results.sub$population)

boxplot(epi.results.sub$population_log)

attach(epi.results.sub)

## created linear model of EPI.new = a(population_log) + b
lin.mod.mkpnew <- lm(MKP.new~population_log,epi.results.sub)

plot(EPI.new~population_log)
abline(lin.mod.mkpnew)

summary(lin.mod.mkpnew)

plot(lin.mod.mkpnew)


ggplot(epi.results.sub, aes(x = population_log, y = MKP.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.mkpnew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')


## another lm
lin.mod.pop <- lm(population_log~MKP.new,epi.results.sub)
plot(population_log~MKP.old)
abline(lin.mod.pop)

summary(lin.mod.pop)

plot(lin.mod.pop)


ggplot(epi.results.sub, aes(x = MKP.old, y = population_log)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.pop, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')