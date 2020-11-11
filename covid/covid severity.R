# Required libraries
if (!require("readr")) install.packages("readr")
if (!require("rpart.plot")) install.packages("rpart.plot")
if (!require("caret")) install.packages("caret")
if (!require("randomForest")) install.packages("randomForest")
library(readr)
library(caret)
library(rpart.plot)
library(randomForest)

# Load the COVID patient characteristics
urlfile="https://raw.githubusercontent.com/cube27/harvardx-r-exercises/master/covid/data/covid.csv"
covid <- read_csv(url(urlfile))

# Add dependent variable/target variable
# Check date_died to see if death has occurred - create a new field based on this.
head(ifelse(train_set$date_died == "9999-99-99", 1, 0))
covid <- covid %>% mutate(died=ifelse(covid$date_died == "9999-99-99", 0, 1))
head(covid$date_died)
head(covid$died)

# The class needs to be a factor for classification models. Change type of the died field.
class(train_set$died)
covid$died <- as.factor(covid$died)
class(covid$died)

# Distribution of the COVID death population
table(covid$died)

# Split the data into 90% training data and 10% test data for validation.
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = covid$died, times = 1,
                                  p = 0.1, list = FALSE)
train_set <- covid[-test_index,]
test_set <- covid[test_index,]

# Exploratory data analysis
# Lets review the columns in detail
names(train_set)

# Dimensions of the training data
dim(train_set)

# Get a subset of features in the data set.
features <- c("sex", "patient_type", "intubed", "pneumonia", "age", "pregnancy", "diabetes", "copd", "asthma", "inmsupr", "hypertension", "other_disease", "cardiovascular", "obesity", "renal_chronic", "tobacco", "contact_other_covid", "covid_res", "icu")
set.seed(1, sample.kind = "Rounding")

# Sample 10000 observations from the training data.
index <- sample(nrow(train_set), 10000)
x <- train_set[index, features]
y <- train_set$died[index]

# Find correlation matrix of features
cor(x)

# Sample 1000 observations from the validation data
index <- sample(nrow(test_set), 1000)
x_test <- test_set[index, features]
y_test <- test_set$died[index]

# Lets ignore fields that are near zero to avoid noise
nzv <- nearZeroVar(x)

# Strip the near zero columns
col_index <- setdiff(1:ncol(x), nzv)
length(col_index)  

# Modeling/Analysis

# Decision Tree Model
set.seed(1, sample.kind = "Rounding")
train_rpart <- train(x[, col_index], y,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
# Plot the Accuracy vs. complexity paratemeter, cp
plot_rpart <- plot(train_rpart)
plot_rpart

# Get confusion matrix for the model and get the balanced accuracy
rpart_cm <- confusionMatrix(predict(train_rpart, x_test), y_test)
rpart_cm$byClass["Balanced Accuracy"]
accuracy_results <- tibble(Model = "Decision Tree Model", `Balanced Accuracy` = rpart_cm$byClass["Balanced Accuracy"])

# access the final model and plot it
rpart.plot(train_rpart$finalModel)


# KNN Model
set.seed(1, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y,
                   method = "knn", 
                   tuneGrid = data.frame(k = seq(1, 24, 3)),
                   trControl = control)
# Plot the KNN model Accuracy vs. No. of Neighbors
ggplot(train_knn)

# Repeat with 5-fold cross validation
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
# Plot the KNN model Accuracy vs. No. of Neighbors
ggplot(train_knn)
train_knn$bestTune[1]
fit_knn <- knn3(x[ ,col_index], y,  k = train_knn$bestTune[1])

y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type="class")
knn_cm <- confusionMatrix(y_hat_knn, factor(y_test))
knn_cm
accuracy_results <- bind_rows(accuracy_results,
                          data_frame(Model = "KNN Model",
                                     `Balanced Accuracy` = knn_cm$byClass["Balanced Accuracy"] ))

# Check Prevalence in data - much more survivors
mean(covid$died == "0")

# Random Forest
set.seed(1, sample.kind = "Rounding")
train_rf <- train(x[, col_index], y, method = "rf", 
                  ntree = 100,
                  tuneGrid = data.frame(mtry = seq(1:7)))
# Plot Random Forest Accuracy vs. No. of Randomly selected predictors
plot(train_rf)

rf_cm <- confusionMatrix(predict(train_rf, x_test), y_test)
rf_cm
accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Model = "Random Forest",
                                         `Balanced Accuracy` = rf_cm$byClass["Balanced Accuracy"] ))

# Find features importance
imp <- varImp(train_rf)

# Get the tree terms from the Decision Tree model
tree_terms <- as.character(unique(train_rpart$finalModel$frame$var[!(train_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms

# Combine the Decision Tree Model and Random Forest Model to find features with highest importance
tibble(term = rownames(imp$importance), importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>%
  arrange(desc(importance)) %>%
  filter(term %in% tree_terms)


# Ensemble with Decision Tree, KNN and Random Forest models
# Combine all the three model predictions
pred_rpart <- predict(train_rpart, x_test)
pred_knn <- predict(train_knn, x_test)
pred_rf <- predict(train_rf, x_test)
pred <- cbind(rpart=as.numeric(pred_rpart)-1, knn=as.numeric(pred_knn)-1, rf=as.numeric(pred_rf)-1)
dim(pred)

# Calculate average accuracy for each model. Note: this is not the balanced accuracy!
accuracy <- colMeans(pred == y_test)
accuracy
mean(accuracy)

# The Ensemble model accuracy is calculated below taking the best accuracy for each observation
means_died <- rowMeans(pred == 1)
y_hat <- ifelse(means_died > 0.3, "1", "0")
mean(y_hat == y_test)

