####### Data Analytics Fall 2024 Lab 02 Part 2 ######

abalone <- read.csv("C:\\Users\\cowarj\\Documents\\DataAnalytics\\Lab2Part2\\abalone\\abalone.data")
abalone.names <- read.csv("C:\\Users\\cowarj\\Documents\\DataAnalytics\\Lab2Part2\\abalone\\abalone.names")
iris <- read.csv("C:\\Users\\cowarj\\Documents\\DataAnalytics\\Lab2Part2\\iris.csv")

install.packages("e1071")
library(e1071)

# COLUMN NAMES
colnames(abalone) <-c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght',
                      'viscera_wieght', 'shell_weight', 'rings' ) 

abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels=c("young", "adult", "old"))

# NAIVE BAYES IRIS DATASET

#Train classifier
classifier<-naiveBayes(iris[,1:4], iris[,5])

# evaluate classification
table(predict(classifier, iris[,-5]), iris[,5], dnn=list('predicted','actual'))

# examine class means and standard deviations for petal length
classifier$tables$Petal.Length

# plot normal distributions at the means of the classes
# one class
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species")

# another class
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")

# the final class
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green")


# EXERCISE 1: ABALONE DATASET
classifier1<-naiveBayes(abalone[,2:7], abalone[,10])

# evaluate classification
table(predict(classifier1, abalone[,2:7]), abalone[,10], dnn=list('predicted','actual'))

# examine class means and standard deviations for length
classifier1$apriori
classifier1$tables$length

# plot normal distributions at the means of the classes
# one class
plot(function(x) dnorm(x, 0.4209915, 0.11137474), 0, 1, ylim=c(0,5), col="red", main="Length distribution for old, young, and adult")
# another class
curve(dnorm(x, 0.5707182, 0.08740980), add=TRUE, col="orange")
# the final class
curve(dnorm(x, 0.5868542, 0.08100644), add=TRUE, col = "pink")

classifier2<-naiveBayes(abalone[,-10], abalone[,10])

# evaluate classification
table(predict(classifier2, abalone[,2:7]), abalone[,10], dnn=list('predicted','actual'))

# examine class means and standard deviations for shell weight
classifier2$apriori
classifier2$tables$shell_weight

# plot normal distributions at the means of the classes
# one class
plot(function(x) dnorm(x, 0.3425532, 0.13673132), -.5, 1, ylim=c(0,5), col="lightblue", main="Shell weight distribution for old, young, and adult")
# another class
curve(dnorm(x, 0.1213945, 0.08096481), add=TRUE, col="turquoise")
# the final class
curve(dnorm(x, 0.2752133, 0.10944254), add=TRUE, col = "blue")

classifier3<-naiveBayes(abalone[,-10], abalone[,10])

# evaluate classification
table(predict(classifier3, abalone[,2:7]), abalone[,10], dnn=list('predicted','actual'))

# examine class means and standard deviations for rings
classifier3$apriori
classifier3$tables$rings

# plot normal distributions at the means of the classes
# one class
plot(function(x) dnorm(x, 14.488008, 2.7356786), 3, 25, ylim=c(0,1), col="purple", main="Rings distribution for old, young, and adult")
# another class
curve(dnorm(x, 6.884151, 1.2169139), add=TRUE, col="lavender")
# the final class
curve(dnorm(x, 9.888398, 0.7985117), add=TRUE, col = "magenta")

# EXERCISE 2: ABALONE DATASET
library(class)
abalone <- abalone[,-1]

# sample 2924 from 4177 (~70%) 

s_abalone <- sample(4176,4176*.7)

## create train & test sets based on sampled indexes 
abalone.train <- abalone[s_abalone,]
abalone.train <- abalone.train[complete.cases(abalone.train),]
abalone.test <- abalone[-s_abalone,]

sqrt(2924) 
k = 55
# k = 80

# train model & predict
KNNpred <- knn(train = abalone.train[,1:7], test = abalone.test[,1:7], cl = abalone.train$age.group, k = k)

# create contingency table/ confusion matrix 
contingency.table <- table(KNNpred,abalone.test$age.group)


contingency.matrix = as.matrix(contingency.table) 
sum(diag(contingency.matrix))/length(abalone.test$age.group)

accuracy <- c()

ks <- c(35,45,55,65,75,85,95,105)

for (k in ks) {
  
  KNNpred <- knn(train = abalone.train[1:7], test = abalone.test[1:7], cl = abalone.train$age.group, k = k) 
  cm = as.matrix(table(Actual=KNNpred, Predicted = abalone.test$age.group, dnn=list('predicted','actual'))) 
  accuracy <- c(accuracy,sum(diag(cm))/length(abalone.test$age.group))
  
}
plot(ks,accuracy,type = "b")

# EXERCISE 2: IRIS DATASET
s_iris <- sample(150, 105)

## create train & test sets based on sampled indexes 
iris.train <- iris[s_iris,]
iris.test <- iris[-s_iris,]

sqrt(105) 
k = 10

# train model & predict
KNNpred <- knn(train = iris.train[1:4], test = iris.test[1:4], cl = iris.train$Species, k = k)

# create contingency table/ confusion matrix 
contingency.table <- table(KNNpred,iris.test$Species)


contingency.matrix = as.matrix(contingency.table) 
sum(diag(contingency.matrix))/length(iris.test$Species)

accuracy <- c()

ks <- c(1, 3, 5, 7, 10, 12, 15, 20)

for (k in ks) {
  
  KNNpred <- knn(train = iris.train[1:4], test = iris.test[1:4], cl = iris.train$Species, k = k) 
  cm = as.matrix(table(Actual=KNNpred, Predicted = iris.test$Species, dnn=list('predicted','actual'))) 
  accuracy <- c(accuracy,sum(diag(cm))/length(iris.test$Species))
  
}

plot(ks,accuracy,type = "b", main="kNN for Iris (Species)")


# EXERCISE 3 : KMEANS IRIS
install.packages("ggplot2")
library(ggplot2)

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) + geom_point()
set.seed(123)

# RUN K MEANS
iris.km <- kmeans(iris[,-5], centers = 3)
assigned.clusters <- as.factor(iris.km$cluster)
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters)) + geom_point()

wss <- c()
ks <- c(2,3,4,5)
for (k in ks) {
  iris.km <- kmeans(iris[,-5], centers = k)
  wss <- c(wss,iris.km$tot.withinss)
}
plot(ks,wss,type = "b")

labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters==1] <- "Setosa"
labeled.clusters[labeled.clusters==2] <- "Versivolor"
labeled.clusters[labeled.clusters==3] <- "Virginica"
table(labeled.clusters, iris[,5])

# EXERCISE 3: KMEANS ABALONE
ggplot(new_abalone, aes(x = length, y = diameter, colour = age.group)) + geom_point()
set.seed(123)

# RUN K MEANS
abalone.km <- kmeans(abalone[,-8], centers = 3)

assigned.clusters <- as.factor(abalone.km$cluster)
ggplot(abalone_data.norm, aes(x = length, y = diameter, colour = assigned.clusters)) + geom_point()

wsx <- c()
ks <- c(1,3,5,7)

for (k in ks) {
  abalone.km <- kmeans(abalone_data.norm[,-8], centers = k)
  wsx <- c(wsx,abalone.km$tot.withinss)
}

length(ks)
length(wss)

plot(ks,wss,type = "b")

labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters==1] <- "Young"
labeled.clusters[labeled.clusters==2] <- "Adult"
labeled.clusters[labeled.clusters==3] <- "Old"
table(labeled.clusters, abalone_data.norm[,8])