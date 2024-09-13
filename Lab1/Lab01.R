# LAB 1 - Jeda Coward

# Setting Up
EPI <- read.csv("C:\\Users\\cowarj\\Documents\\DataAnalytics\\Lab1\\epi2024results06022024.csv")
View(EPI)
attach(EPI)
fix(EPI)
EPI.new

# Exercise 1
summary(EPI.new)
fivenum(EPI.new,na.rm=TRUE)
stem(EPI.new)
hist(EPI.new)
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new, na.rm=TRUE,bw="SJ"))
rug(EPI.new)

boxplot(EPI.new, APO.new)

x<-seq(20,80,1)
q<-dnorm(x,mean=42,sd=5,log=FALSE)
lines(x,q)
lines(x,.4*q)
q<-dnorm(x,mean=65,sd=5,log=FALSE)
lines(x,.12*q)

# Exercise 2
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE)
qqnorm(EPI.new); qqline(EPI.new)
qqplot(rnorm(ppoints(250)), EPI.new, xlab="Q-Q plot for norm dsn")
qqline(EPI.new)
qqplot(rt(ppoints(250), df=5), EPI.new, xlab="Q-Q plot for t dsn")
qqline(EPI.new)

# Exercise 2a - For ECO
plot(ecdf(ECO.new), do.points=FALSE, verticals=TRUE)
qqnorm(ECO.new); qqline(EPI.new)
qqplot(rnorm(ppoints(250)), ECO.new, xlab="ECO.new Q-Q plot for norm dsn")
qqline(ECO.new)
qqplot(rt(ppoints(250), df=5), ECO.new, xlab="ECO.new Q-Q plot for t dsn")
qqline(ECO.new)

# Exercise 2a - For PAR
plot(ecdf(PAR.new), do.points=FALSE, verticals=TRUE)
qqnorm(PAR.new); qqline(PAR.new)
qqplot(rnorm(ppoints(250)), PAR.new, xlab="PAR.new Q-Q plot for norm dsn")
qqline(PAR.new)
qqplot(rt(ppoints(250), df=5), PAR.new, xlab="PAR.new Q-Q plot for t dsn")
qqline(PAR.new)
