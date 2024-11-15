##########################################
### Principal Component Analysis (PCA) ###
##########################################
library(readr)
library(ggfortify)
library(e1071)
library(class)
library(psych)

# PCA with Wine Data Set
wine <- read_csv("C:\\Users\\cowarj\\Documents\\DataAnalytics\\Lab4\\wine.data", col_names = FALSE)
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
head(wine)

wine$Type <- as.factor(wine$Type)
wine <- wine[,-c(4,5,10)]
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

## Compute the PCs and plot the data set using the 1st and 2nd PC
wine.X <- wine[,-1]
principal_components <- princomp(wine.X, cor = TRUE, score = TRUE)
summary(principal_components)

autoplot(principal_components, data = wine, colour = 'Type',
         loadings = TRUE, loadings.colour = 'darkred',
         loadings.label = TRUE, loadings.label.size = 3)

wine_pca <- prcomp(wine.X, center = TRUE, scale. = TRUE)
attributes(wine_pca)
summary(wine_pca)
wine_pca$rotation

## Identify the variables that contribute the most to the 1st PC
### Nonflavanoid Phenols and Malic Acid have the most positive contribution to
### the first PC. Flavanoids and Total phenols have the most negative contribution.

## Train a classifier model to predict wine type using the 13 attributes
k=10
knn.pred <- knn(train = wine[,-1], test = wine[,-1], cl = wine$Type, k = k)

# Evaluate
cm <- table(Predicted=knn.pred, Actual = wine$Type, dnn=list('predicted','actual'))
cm

diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

data.frame(recall, precision, f1)

## Train a classifier model to predict wine type using the data projected into the first 3 PCs.
k=10
knn_pred_pca <- knn(train = wine_pca$x[,c(1,2)], test = wine_pca$x[,c(1,2)], cl = wine$Type, k = k)
cm_pca <- table(Predicted = knn_pred_pca, Actual = wine$Type, dnn = list('predicted','actual'))
cm_pca

diag = diag(cm_pca) # number of correctly classified instances per class 
rowsums = apply(cm_pca, 1, sum) # number of instances per class
colsums = apply(cm_pca, 2, sum) # number of predictions per class

precision_pca = diag / colsums 
recall_pca = diag / rowsums 
f1_pca = 2 * precision_pca * recall_pca / (precision_pca + recall_pca) 

data.frame(recall_pca, precision_pca, f1_pca)

## Identify least contributing variable to 1st PC and drop it
contributions <- abs(wine_pca$rotation[,1])
least <- names(sort(contributions, decreasing = FALSE))[1]
wine_reduced <- wine[, !(names(wine) == least)]
wine_reduced_scaled <- scale(wine_reduced[,-1])
wine_pca_reduced <- prcomp(wine_reduced_scaled, center = TRUE, scale. = TRUE)

## KNN using first 3 PCs after dropping least contributing variable
k <- 3
knn_pred_reduced <- knn(train = wine_pca_reduced$x[,1:3], test = wine_pca_reduced$x[,1:3], cl = wine$Type, k = k)
cm_reduced <- table(Predicted = knn_pred_reduced, Actual = wine$Type, dnn = list('predicted','actual'))
cm_reduced

diag = diag(cm_reduced) # number of correctly classified instances per class 
rowsums = apply(cm_reduced, 1, sum) # number of instances per class
colsums = apply(cm_reduced, 2, sum) # number of predictions per class

precision_reduced = diag / colsums 
recall_reduced = diag / rowsums 
f1_reduced = 2 * precision_reduced * recall_reduced / (precision_reduced + recall_reduced) 

data.frame(recall_reduced, precision_reduced, f1_reduced)