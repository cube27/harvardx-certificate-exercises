# Load the COVID patient characteristics
library(readr)
covid <- read_csv("data/covid.csv")

# Add dependent variable/target variable
head(ifelse(train_set$date_died == "9999-99-99", 1, 0))
covid <- covid %>% mutate(died=ifelse(covid$date_died == "9999-99-99", 0, 1))
head(covid$date_died)
head(covid$died)

class(train_set$died)
covid$died <- as.factor(covid$died)
class(covid$died)
table(covid$died)

library(caret)
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = covid$died, times = 1,
                                  p = 0.1, list = FALSE)
train_set <- covid[-test_index,]
test_set <- covid[test_index,]

# Exploratory data analysis
names(train_set)
dim(train_set)

features <- c("sex", "patient_type", "intubed", "pneumonia", "age", "pregnancy", "diabetes", "copd", "asthma", "inmsupr", "hypertension", "other_disease", "cardiovascular", "obesity", "renal_chronic", "tobacco", "contact_other_covid", "covid_res", "icu")
set.seed(1, sample.kind = "Rounding")
index <- sample(nrow(train_set), 10000)
x <- train_set[index, features]
y <- train_set$died[index]

cor(x)

index <- sample(nrow(test_set), 1000)
#note that the line above is the corrected code - code in video at 0:52 is incorrect
x_test <- test_set[index, features]
y_test <- test_set$died[index]

#preprocessing
library(caret)
nzv <- nearZeroVar(x)

col_index <- setdiff(1:ncol(x), nzv)
length(col_index)  

#Model fitting
#colnames(x) <- 1:ncol(train_set)
#colnames(x_test) <- colnames(x)

# Decision Tree
set.seed(1, sample.kind = "Rounding")
train_rpart <- train(x[, col_index], y,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
plot_rpart <- plot(train_rpart)
plot_rpart

# compute accuracy
rpart_cm <- confusionMatrix(predict(train_rpart, x_test), y_test)
rpart_cm

# access the final model and plot it
library(rpart.plot)
rpart.plot(train_rpart$finalModel)


# KNN
set.seed(1, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y,
                   method = "knn", 
                   tuneGrid = data.frame(k = seq(1, 24, 3)),
                   trControl = control)
ggplot(train_knn)

# sample size
n <- 1000
# no. of folds
b <- 5
set.seed(1)
index <- sample(nrow(x), n)
set.seed(1, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = seq(1, 24, 3)),
                   trControl = control)
train_knn$bestTune[1]
fit_knn <- knn3(x[ ,col_index], y,  k = train_knn$bestTune[1])

y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type="class")
knn_cm <- confusionMatrix(y_hat_knn, factor(y_test))
knn_cm

knn_cm$byClass
mean(test_set$died == "0")
mean(train_set$died == "0")


# Random Forest
library(randomForest)
set.seed(1, sample.kind = "Rounding")
train_rf <- train(x[, col_index], y, method = "rf", 
                  ntree = 100,
                  tuneGrid = data.frame(mtry = seq(1:7)))
plot(train_rf)

rf_cm <- confusionMatrix(predict(train_rf, x_test), y_test)
rf_cm

imp <- varImp(train_rf)
tree_terms <- as.character(unique(train_rpart$finalModel$frame$var[!(train_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms

tibble(term = rownames(imp$importance), importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>%
  arrange(desc(importance)) %>%
  filter(term %in% tree_terms)


# Ensemble
pred_rpart <- predict(train_rpart, x_test)
pred_knn <- predict(train_knn, x_test)
pred_rf <- predict(train_rf, x_test)
pred <- cbind(rpart=as.numeric(pred_rpart)-1, knn=as.numeric(pred_knn)-1, rf=as.numeric(pred_rf)-1)
dim(pred)

accuracy <- colMeans(pred == y_test)
accuracy
mean(accuracy)

means_died <- rowMeans(pred == 1)
y_hat <- ifelse(means_died > 0.3, "1", "0")
mean(y_hat == y_test)

