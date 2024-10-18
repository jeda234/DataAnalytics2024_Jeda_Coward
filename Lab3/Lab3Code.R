####### Data Analytics Fall 2024 Lab 03 ######

epi <- read.csv("C:\\Users\\cowarj\\Documents\\DataAnalytics\\Lab3\\epi2024results_DA_F24_lab03.csv")
epi_la_caribbean <- epi[epi$region == "Latin America & Caribbean",]
epi_southern_asia <- epi[epi$region == "Southern Asia",]


# VARIABLE DISTRIBUTIONS

## HISTOGRAMS
fivenum(epi_la_caribbean$ECO,na.rm=TRUE)
stem(epi_la_caribbean$ECO)
hist(epi_la_caribbean$ECO)
hist(epi_la_caribbean$ECO, seq(20., 80., 1.0), prob=TRUE)
lines(density(epi_la_caribbean$ECO, na.rm=TRUE,bw="SJ"))
rug(epi_la_caribbean$ECO)

fivenum(epi_southern_asia$ECO,na.rm=TRUE)
stem(epi_southern_asia$ECO)
hist(epi_southern_asia$ECO)
hist(epi_southern_asia$ECO, seq(20., 65., 1.0), prob=TRUE)
lines(density(epi_southern_asia$ECO, na.rm=TRUE, bw="SJ"))
rug(epi_southern_asia$ECO)

## QQ PLOTS
plot(ecdf(epi_la_caribbean$ECO), do.points=FALSE, verticals=TRUE)
qqnorm(epi_la_caribbean$ECO); qqline(epi_la_caribbean$ECO)
qqplot(rnorm(ppoints(250)), epi_la_caribbean$ECO, main="Q-Q plot for Normal Distribution")
qqline(epi_la_caribbean$ECO)
qqplot(rt(ppoints(250), df=5), epi_la_caribbean$ECO, main="Q-Q plot for T Distribution")
qqline(epi_la_caribbean$ECO)

plot(ecdf(epi_southern_asia$ECO), do.points=FALSE, verticals=TRUE)
qqnorm(epi_southern_asia$ECO); qqline(epi_southern_asia$ECO)
qqplot(rnorm(ppoints(250)), epi_southern_asia$ECO, main="Q-Q plot for Normal Distribution")
qqline(epi_southern_asia$ECO)
qqplot(rt(ppoints(250), df=5), epi_southern_asia$ECO, main="Q-Q plot for T Distribution")
qqline(epi_southern_asia$ECO)

# LINEAR MODELS
# PART 2.1
linear_model <- lm(EPI ~ ECO + APO + AIR + PAR + FSS, data = epi)
summary(linear_model)

plot(epi$ECO, epi$APO, main = "EPI: ECO vs APO with Fitted Line",
     xlab = "ECO", ylab = "APO", pch = 15, col = "maroon")
abline(lm(BDH ~ ECO, data = epi), col = "red", lwd = 2)

# PART 2.2
linear_model_caribbean <- lm(EPI ~ ECO + APO + AIR + PAR + FSS, data = epi_la_caribbean)
summary(linear_model_caribbean)

plot(epi_la_caribbean$ECO, epi_la_caribbean$APO, main = "EPI Latin America & Caribbean: ECO vs APO with Fitted Line",
     xlab = "ECO", ylab = "APO", pch = 15, col = "aquamarine")
abline(lm(PAR ~ ECO, data = epi_la_caribbean), col = "lightblue", lwd = 2)

### The model in 2.1 is a better fit. 
### The line of best fit in 2.1 includes more data points than in 2.2, and the slope of the line is closer to the shape of the data versus 2.2

# CLASSIFICATION (kNN)
knn_variables <- c('EPI', 'ECO', 'BDH', 'SPI', 'AGR', 'region')
data <- epi[, knn_variables]
head(data)
x <- data[, knn_variables[1:5]]
y <- data$region 

# PART 3.1
n <- nrow(data)
train_indexes <- sample(n,n*.7)
data.train <- data[train_indexes, ]
data.test <- data[-train_indexes, ] 

x_train <- data.train[, 1:5]
y_train <- data.train$region 
x_test <- data.test[, 1:5]
y_test <- data.test$region

k = 5
KNNpred <- knn(train = x_train[1:3], test = x_test[1:3], cl = y_train, k = k)
contingency.table <- table(KNNpred, x_test$BDH)
contingency.table
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(x_test$BDH)

# PART 3.2
contingency.table <- table(KNNpred, x_test$ECO)
contingency.table
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(x_test$ECO)

contingency.table <- table(KNNpred, x_test$SPI)
contingency.table
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(x_test$SPI)

contingency.table <- table(KNNpred, x_test$AGR)
contingency.table
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(x_test$AGR)

### The kNN model regarding SPI had the highest sum/length value.
### I believe that this value and the table shows this model is most effective.

# CLUSTERING
regions1 <- subset(epi, region %in% c("Asia-Pacific", "Southern Asia", "Greater Middle East"))
regions2 <- subset(epi, region %in% c("Global West", "Sub-Sahara Africa", "Latin America & Caribbean"))

# PART 4.1 
ks <- c(10,20,30,40,50, 60)
regions1_data <- as.matrix(regions1[knn_variables])
regions2_data <- as.matrix(regions2[knn_variables])

wcss_matrix <- matrix(NA, nrow = length(ks), ncol = 2)
colnames(wcss_matrix) <- c("Region1_WCSS", "Region2_WCSS")
rownames(wcss_matrix) <- as.character(ks)

wcss_matrix <- matrix(NA, nrow = length(ks), ncol = 5)
wcss_df <- as.data.frame(wcss_matrix)
print(wcss_df)

# PART 4.2
for (k in ks) {
  regions1.km <- kmeans(epi[,1], centers = 1)
  wcss <- c(wcss_df,regions1.km$tot.withinss)
  
  regions2.km <- kmeans(epi[,1], centers = 1)
  wcss <- c(wcss_df,regions2.km$tot.withinss)
}

plot(ks,wcss,type = "b")

### I am having trouble running these plots :(
### The error I am getting states:
### Error in xy.coords(x, y, xlabel, ylabel, log) : 
###  'list' object cannot be coerced to type 'double'