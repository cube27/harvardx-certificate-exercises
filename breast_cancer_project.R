options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

#analyze output and predictors
brca$y
brca$x

#dimensions
dim(brca$x)

#malignant proportion
mean(brca$y=="M")

#column with highest mean
which.max(colMeans(brca$x))

#column with lowest standard deviation
which.min(colSds(brca$x))

#scale matrix
x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")

sd(x_scaled[,1])
median(x_scaled[,1])

#distance between all samples
d <- dist(x_scaled)

#distance between 1st sample to other benign samples
d_1toB <- as.matrix(d)[1, brca$y == "B"]
mean(d_B2B[2:length(d_1toB)])

#distance between 1st sample to malignant samples
d_1toM <- as.matrix(d)[1, brca$y == "M"]
mean(d_1toM)

#features heatmap
d_predictors <- dist(t(x_scaled))
heatmap(as.matrix(d_predictors))

#hierarchical clustering
h <- hclust(d_predictors)
groups <- cutree(h, k = 5)
split(names(groups), groups)
test_split <- split(names(groups), groups)

#principal component analysis
pca<-prcomp(x_scaled)
summary(pca)

#plot first two principal components
data.frame(pca$x[,1:2], tumor_type = brca$y) %>%
  ggplot(aes(PC1, PC2, color = tumor_type)) +
  geom_point(alpha=0.5)

#boxplot of first 10 PCs grouped by tumor type
data.frame(tumor_type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -tumor_type) %>%
  ggplot(aes(PC, value, fill = tumor_type)) +
  geom_boxplot()

#create training and testing sets
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]

#benign proportion
mean(train_y=="B")
mean(test_y=="B")

#function to preduct kmeans
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

#perform k-means clustering
set.seed(3, sample.kind = "Rounding")
k <- kmeans(train_x, centers = 2)
kmeans_preds <- ifelse(predict_kmeans(test_x, k) == 1, "B", "M")
mean(kmeans_preds == test_y)

table(test_y,kmeans_preds)

#proportion of benign tumors correctly identified
71/(71+1)

#proportion of benign tumors correctly identified
35/(8+35)

#fit logistic regression model
train_glm <- train(train_x, train_y, method = "glm")
glm_preds <- predict(train_glm, test_x)
mean(glm_preds == test_y)

#fit lda model
train_lda <- train(train_x, train_y, method = "lda")
lda_preds <- predict(train_lda, test_x)
mean(lda_preds == test_y)

#fit qda model
train_qda <- train(train_x, train_y, method = "qda")
qda_preds <- predict(train_qda, test_x)
mean(qda_preds == test_y)

library(gam)
#fit loess model
set.seed(5, sample.kind = "Rounding")
train_loess <- train(train_x, train_y, method = "gamLoess")
loess_preds <- predict(train_loess, test_x)
mean(loess_preds == test_y)

#fit knn model
set.seed(7,sample.kind = "Rounding")
train_knn <- train(train_x, train_y, method = "knn",
                   tuneGrid = data.frame(k = seq(3, 21, 2)))
knn_preds <- predict(train_knn, test_x)
train_knn
mean(knn_preds == test_y)

#fit random forest model
set.seed(9,sample.kind = "Rounding")
train_rf <- train(train_x, train_y, method = "rf",
                  importance = TRUE,
                  tuneGrid = data.frame(mtry = c(3,5,7,9)))
rf_preds <- predict(train_rf, test_x)
mean(rf_preds == test_y)
train_rf$bestTune
varImp(train_rf)

#ensemble
model <- c("kmeans_preds","glm_preds","lda_preds","qda_preds","rf_preds","knn_preds","loess_preds")
pred <- sapply(1:7, function(x){
  as.factor(get(model[x]))})
dim(pred)

pred <- as.data.frame(pred)
names(pred) <-c("kmeans_preds","glm_preds","lda_preds","qda_preds","rf_preds","knn_preds","loess_preds")
acc <- colMeans(as.matrix(pred)==test_y)
mean(acc)

ensemble <- cbind(glm = glm_preds == "B", lda = lda_preds == "B", qda = qda_preds == "B", loess = loess_preds == "B", rf = rf_preds == "B", knn = knn_preds == "B", kmeans = kmeans_preds == "B")

ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "B", "M")
mean(ensemble_preds == test_y)

