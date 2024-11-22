## LAB 5 DATA ANALYTICS ##

library(caret)
library(e1071)
library(class)
library(tidyverse)

wine <- read_csv("C:\\Users\\cowarj\\Documents\\DataAnalytics\\Lab4\\wine.data", col_names = FALSE)
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
wine$Type <- as.factor(wine$Type)

## SETTING TRAIN AND TEST
set.seed(42)
train.indexes <- sample(nrow(wine), 0.7 * nrow(wine))
train <- wine[train.indexes, ]
test <- wine[-train.indexes, ]

x <- train[, -1]
y <- train[, 1] 

## SVM CLASSIFIER USING LINEAR KERNEL
# TRAIN #
svm_linear <- svm(Type ~ ., data = train, kernel = "linear")
train_pred_linear <- predict(svm_linear, train)
cm_linear <- table(Actual = train$Type, Predicted = train_pred_linear)

n <- sum(cm_linear)
nc <- nrow(cm_linear) 
diag <- diag(cm_linear)
rowsums <- apply(cm_linear, 1, sum)
colsums <- apply(cm_linear, 2, sum)

recall <- diag / rowsums
precision <- diag / colsums
f1 <- 2 * precision * recall / (precision + recall)

svm_linear_metrics <- data.frame(precision, recall, f1)
svm_linear_metrics

# TEST #
test_pred_linear <- predict(svm_linear, test)
cm_test_linear <- table(Actual = test$Type, Predicted = test_pred_linear)

n <- sum(cm_test_linear)
nc <- nrow(cm_test_linear) 
diag <- diag(cm_test_linear)
rowsums <- apply(cm_test_linear, 1, sum)
colsums <- apply(cm_test_linear, 2, sum)

recall <- diag / rowsums
precision <- diag / colsums
f1 <- 2 * precision * recall / (precision + recall)

svm_test_linear_metrics <- data.frame(precision, recall, f1)
svm_test_linear_metrics

## SVM CLASSIFIER USING POLYNOMIAL KERNEL
# TRAIN #
svm_polynomial <- svm(Type ~ ., data = train, kernel = "polynomial")
train_pred_polynomial <- predict(svm_polynomial, train)
cm_polynomial <- table(Actual = train$Type, Predicted = train_pred_polynomial)

n_polynomial <- sum(cm_polynomial)
nc_polynomial <- nrow(cm_polynomial)
diag_polynomial <- diag(cm_polynomial)
rowsums_polynomial <- apply(cm_polynomial, 1, sum)
colsums_polynomial <- apply(cm_polynomial, 2, sum)

recall_polynomial <- diag_polynomial / rowsums_polynomial
precision_polynomial <- diag_polynomial / colsums_polynomial
f1_polynomial <- 2 * precision_polynomial * recall_polynomial / (precision_polynomial + recall_polynomial)

svm_polynomial_metrics <- data.frame(precision_polynomial, recall_polynomial, f1_polynomial)
svm_polynomial_metrics

# TEST #
test_pred_polynomial <- predict(svm_polynomial, test)
cm_polynomial <- table(Actual = test$Type, Predicted = test_pred_polynomial)

n_polynomial <- sum(cm_polynomial)
nc_polynomial <- nrow(cm_polynomial)
diag_polynomial <- diag(cm_polynomial)
rowsums_polynomial <- apply(cm_polynomial, 1, sum)
colsums_polynomial <- apply(cm_polynomial, 2, sum)

recall_polynomial <- diag_polynomial / rowsums_polynomial
precision_polynomial <- diag_polynomial / colsums_polynomial
f1_polynomial <- 2 * precision_polynomial * recall_polynomial / (precision_polynomial + recall_polynomial)

svm_polynomial_metrics <- data.frame(precision_polynomial, recall_polynomial, f1_polynomial)
svm_polynomial_metrics

## KNN CLASSIFIER
# TRAIN #
knn <- train(Type ~ ., data = train, method = "knn", tuneLength = 10)
train_pred_knn <- predict(knn, train)
cm_knn <- table(Actual = train$Type, Predicted = train_pred_knn)

n_knn <- sum(cm_knn)
nc_knn <- nrow(cm_knn)
diag_knn <- diag(cm_knn)
rowsums_knn <- apply(cm_knn, 1, sum)
colsums_knn <- apply(cm_knn, 2, sum)

recall_knn <- diag_knn / rowsums_knn
precision_knn <- diag_knn / colsums_knn
f1_knn <- 2 * precision_knn * recall_knn / (precision_knn + recall_knn)

knn_metrics <- data.frame(precision_knn, recall_knn, f1_knn)
knn_metrics

# TEST #
test_pred_knn <- predict(knn, test)
cm_knn <- table(Actual = test$Type, Predicted = test_pred_knn)

n_knn <- sum(cm_knn)
nc_knn <- nrow(cm_knn)
diag_knn <- diag(cm_knn)
rowsums_knn <- apply(cm_knn, 1, sum)
colsums_knn <- apply(cm_knn, 2, sum)

recall_knn <- diag_knn / rowsums_knn
precision_knn <- diag_knn / colsums_knn
f1_knn <- 2 * precision_knn * recall_knn / (precision_knn + recall_knn)

knn_metrics <- data.frame(precision_knn, recall_knn, f1_knn)
knn_metrics

## NY HOUSING DATASET ##
housing <- read.csv("C:\\Users\\cowarj\\Documents\\DataAnalytics\\Lab5\\NY-House-Dataset.csv")
colnames(housing)

## SETTING TRAIN AND TEST
set.seed(123)
train.index <- createDataPartition(housing$PRICE, p = 0.7, list = FALSE)
train <- housing[train.index, ]
test <- housing[-train.index, ]

# SVM REGRESSION
svm_regression <- svm(PRICE ~ PROPERTYSQFT, data = train, kernel = "radial", cost = 1, gamma = 0.1)
svm_preds <- predict(svm_regression, test)
par(mfrow = c(1, 2))
plot(test$PRICE, svm_preds, main = "SVM: Predicted vs Actual", xlab = "Actual Price", ylab = "Predicted Price", col = "lavender", pch = 19)
abline(0, 1, col = "purple2")

# LINEAR REGRESSION
linear_regression <- lm(PRICE ~ PROPERTYSQFT, data = train)
linear_preds <- predict(linear_regression, test)
plot(test$PRICE, linear_preds, main = "Linear: Predicted vs Actual", xlab = "Actual Price", ylab = "Predicted Price", col = "pink2", pch = 19)
abline(0, 1, col = "magenta2")
