data(heights)
heights

str(heights)

heights$height[777]
mean(heights$height)
median(heights$height)

mean(heights$sex=="Male")

sum(heights$sex=="Female" & heights$height > 78)

#train/test

library(tidyverse)
library(caret)
library(dslabs)
data(heights)

# define the outcome and predictors
y <- heights$sex
x <- heights$height

# generate training and test sets
set.seed(2, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# guess the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))

# compute accuracy
mean(y_hat == test_set$sex)
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)

# examine the accuracy of 10 cutoffs
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)


read_mnist()
mnist_dl <- read_mnist()

#confusion matrix

# tabulate each combination of prediction and actual value
table(predicted = y_hat, actual = test_set$sex)
test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))
prev <- mean(y == "Male")

confusionMatrix(data = y_hat, reference = test_set$sex)

# F1-Score
# maximize F-score
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff, F_1) %>% 
  ggplot(aes(cutoff, F_1)) + 
  geom_point() + 
  geom_line()

max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)

confusionMatrix(data = y_hat, reference = test_set$sex)


#precision/recall
p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# ROC curve
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# plot both curves together
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

# plot precision against recall
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()


#assessment #1
library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

dat %>% filter(type=="inclass") %>% summarize(mean(sex=="Female"))
dat %>% filter(type=="online") %>% summarize(mean(sex=="Female"))

y_hat <- ifelse(x == 'inclass', "Female", "Male") %>% factor()
mean(y == y_hat)

table(y_hat, y)

confusionMatrix(y_hat, factor(dat$sex))

mean(y == "Female")


# assessment part 2
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# cutoffs for Sepal.Length
cutoff <- seq(min(train$Sepal.Length), max(train$Sepal.Length), 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

# cutoffs for Sepal.Width
cutoff <- seq(min(train$Sepal.Width), max(train$Sepal.Width), 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

# cutoffs for Petal.Width
cutoff <- seq(min(train$Petal.Width), max(train$Petal.Width), 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

# cutoffs for Petal.Length
cutoff <- seq(min(train$Petal.Length), max(train$Petal.Length), 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
#y_hat <- factor(y_hat)
mean(y_hat == test$Species)

temp_test <- test
test <- train
train <- temp_test
# cutoffs for Sepal.Length
cutoff <- seq(min(train$Sepal.Length), max(train$Sepal.Length), 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

# cutoffs for Sepal.Width
cutoff <- seq(min(train$Sepal.Width), max(train$Sepal.Width), 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

# cutoffs for Petal.Width
cutoff <- seq(min(train$Petal.Width), max(train$Petal.Width), 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)
best_cutoff_Width <- cutoff[which.max(accuracy)]
best_cutoff_Width

# cutoffs for Petal.Length
cutoff <- seq(min(train$Petal.Length), max(train$Petal.Length), 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)
best_cutoff_length <- cutoff[which.max(accuracy)]
best_cutoff_length

y_hat <- ifelse(test$Petal.Length > best_cutoff_length | test$Petal.Width > best_cutoff_Width, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
#y_hat <- factor(y_hat)
mean(y_hat == test$Species)

# From solution
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris,pch=21,bg=iris$Species)

# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)

length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)


#assessment for conditional probabilities
# P(disease|+) = P(+|disease)*P(disease)/P(+)
# P(+) = P(+|disease)*P(disease) + P(+|healthy)*P(healthy)
P_positive = 0.85*0.02 + 0.1*0.98
P_disease_given_positive = 0.85*0.02/P_positive

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

mean(test)

mean(test==0 & disease==1)

mean(disease[test==1])

mean(disease[test==1])/mean(disease)


# assessment #2

library(dslabs)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)


ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)


Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
plot(dat)

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)


# linear regression
library(tidyverse)
library(HistData)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

avg <- mean(train_set$son)
avg

mean((avg - test_set$son)^2)

# fit linear regression model
fit <- lm(son ~ father, data = train_set)
fit$coef

y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)

y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)


# assessment question 1
#set.seed(1) # if using R 3.5 or earlier
library(tidyverse)
library(caret)
set.seed(1, sample.kind="Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

rmse2 <- replicate(n, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata=test_set)
  RMSE(y_hat, test_set$y)
})

mean(rmse2)
sd(rmse2)


# question 2
library(tidyverse)
library(caret)
set.seed(1, sample.kind="Rounding")
n <- c(100, 500, 1000, 5000, 10000)
calc_rmse_stats <- sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse_calc <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata=test_set)
    sqrt(mean((y_hat-test_set$y)^2))
  })
  c(avg=mean(rmse_calc), sd=sd(rmse_calc))
})


# Qn 4
library(tidyverse)
library(caret)
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

rmse2 <- replicate(n, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata=test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})

mean(rmse2)
sd(rmse2)

# question 6
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))


# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata=test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata=test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata=test_set)
sqrt(mean((y_hat-test_set$y)^2))


# categorical prediction
library(dslabs)
data("heights")
y <- heights$height

set.seed(2) #if you are using R 3.5 or earlier
set.seed(2, sample.kind = "Rounding") #if you are using R 3.6 or later

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

train_set %>% 
  filter(round(height)==66) %>%
  summarize(y_hat = mean(sex=="Female"))

heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point()
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)
p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)$overall["Accuracy"]


#logistic regression
heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() + 
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])

range(p_hat)

# fit logistic regression model
glm_fit <- train_set %>% 
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data=., family = "binomial")

p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")

tmp <- heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) 
logistic_curve <- data.frame(x = seq(min(tmp$x), max(tmp$x))) %>%
  mutate(p_hat = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x))
tmp %>% 
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_line(data = logistic_curve, mapping = aes(x, p_hat), lty = 2)

y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]


# case study 2 or 7
mnist <- read_mnist()
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

data("mnist_27")
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_glm <- predict(fit_glm, mnist_27$test)
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7, 2))
confusionMatrix(data = y_hat_glm, reference = mnist_27$test$y)$overall["Accuracy"]

mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
  geom_raster()

mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black") 

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)


# assessment
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

dat$train %>% ggplot(aes(x, color = y)) + geom_density()

set.seed(1, sample.kind="Rounding") 
mu_1 <- seq(0, 3, len = 25)
accuracy <- sapply(mu_1, function(d){
  dat <- make_data(mu_1 = d)
  fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
  y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat_glm == dat$test$y)
})
qplot(mu_1, accuracy)

#smoothing

data("polls_2008")
qplot(day, margin, data = polls_2008)

# bin smoothers
span <- 7 
fit <- with(polls_2008,ksmooth(day, margin, x.points = day, kernel="box", bandwidth =span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

# kernel
span <- 7
fit <- with(polls_2008, ksmooth(day, margin,  x.points = day, kernel="normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")


# bin smoothers
span <- 7 
fit <- with(polls_2008,ksmooth(day, margin, x.points = day, kernel="box", bandwidth =span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

# kernel
span <- 7
fit <- with(polls_2008, ksmooth(day, margin,  x.points = day, kernel="normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")


#loess
polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth(color="red", span = 0.15, method = "loess", method.args = list(degree=1))


#assessment
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

span <- 60 / as.numeric(max(dat$date)-min(dat$date))
dat %>% ggplot(aes(day, deaths)) +
  geom_point() + 
  geom_smooth(color="red", span = span, method = "loess", method.args = list(degree=1))


dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

library(broom)
qplot(x_2, y, data = mnist_27$train)

mnist_27$train %>%
  mutate(y = ifelse(y=="2", 1, 2)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess", method.args = list(degree=2))

sum(mnist_27$train$y[which(mnist_27$train$x_2<=0.2)]==7)

mnist_27$test %>%
  mutate(y = ifelse(y=="2", 1, 2)) %>%
  ggplot(aes(x_2, y)) +
  geom_point()
  #geom_smooth(method = "loess", method.args = list(degree=2))


# matrices

library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

class(mnist$train$images)

x <- mnist$train$images[1:1000,] 
y <- mnist$train$labels[1:1000]

length(x[,1])
x_1 <- 1:5
x_2 <- 6:10
cbind(x_1, x_2)
dim(x)
dim(x_1)
dim(as.matrix(x_1))
dim(x)


my_vector <- 1:15

# fill the matrix by column
mat <- matrix(my_vector, 5, 3)
mat

# fill by row
mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
mat_t
identical(t(mat), mat_t)
matrix(my_vector, 5, 5)
grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid)

# flip the image back
image(1:28, 1:28, grid[, 28:1])

sums <- rowSums(x)
avg <- rowMeans(x)

data_frame(labels = as.factor(y), row_averages = avg) %>%
  qplot(labels, row_averages, data = ., geom = "boxplot")

avgs <- apply(x, 1, mean)
sds <- apply(x, 2, sd)

library(matrixStats)

sds <- colSds(x)
qplot(sds, bins = "30", color = I("black"))
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])

#extract columns and rows
x[ ,c(351,352)]
x[c(2,3),]
new_x <- x[ ,colSds(x) > 60]
dim(new_x)
class(x[,1])
dim(x[1,])

#preserve the matrix class
class(x[ , 1, drop=FALSE])
dim(x[, 1, drop=FALSE])

mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0

bin_x <- x
bin_x[bin_x < 255/2] <- 0 
bin_x[bin_x > 255/2] <- 1

#index with matrices
mat <- matrix(1:15, 5, 3)
as.vector(mat)
qplot(as.vector(x), bins = 30, color = I("black"))
new_x <- x
new_x[new_x < 50] <- 0

mat <- matrix(1:15, 5, 3)
mat[mat < 3] <- 0
mat

mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0
mat

#binarize the data
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1
bin_X <- (x > 255/2)*1

qplot(as.vector(bin_x), bins = 30, color = I("black"))

(x - rowMeans(x)) / rowSds(x)

t(t(x) - colMeans(x))

X_mean_0 <- sweep(x, 2, colMeans(x))

#scale each row of a matrix
(x - rowMeans(x)) / rowSds(x)

#scale each column
t(t(x) - colMeans(x))

#take each entry of a vector and subtracts it from the corresponding row or column
x_mean_0 <- sweep(x, 2, colMeans(x))

#divide by the standard deviation
x_mean_0 <- sweep(x, 2, colMeans(x))
x_standardized <- sweep(x_mean_0, 2, colSds(x), FUN = "/")

x <- matrix(rnorm(100*10), 100, 10)

dim(x)
rowSums(x)
nrow(x)
ncol(x)

x <- x + seq(nrow(x))

mean(rowMeans(mnist$train$images > 50 & mnist$train$images < 205))

x <- mnist$train$images
y <- mnist$train$labels

sums <- rowSums(x)
avg <- rowMeans(x)

x_avoid_grays <- ifelse(avg > 50 & avg < 205, avg, 0)

data_frame(labels = as.factor(y), row_averages = avg) %>%
  qplot(labels, row_averages, data = ., geom = "boxplot")

#dist
library(tidyverse)
library(dslabs)

if(!exists("mnist")) mnist <- read_mnist()
set.seed(0) # if using R 3.5 or earlier
set.seed(0, sample.kind = "Rounding") # if using R 3.6 or later
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

#the predictors are in x and the labels in y
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

y[1:3]

x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

#distance between two numbers
sqrt(sum((x_1 - x_2)^2))
sqrt(sum((x_1 - x_3)^2))
sqrt(sum((x_2 - x_3)^2))

#compute distance using matrix algebra
sqrt(crossprod(x_1 - x_2))
sqrt(crossprod(x_1 - x_3))
sqrt(crossprod(x_2 - x_3))

#compute distance between each row
d <- dist(x)
class(d)
as.matrix(d)[1:3,1:3]

#visualize these distances
image(as.matrix(d))

#order the distance by labels
image(as.matrix(d)[order(y), order(y)])

#compute distance between predictors
d <- dist(t(x))
dim(as.matrix(d))

d_492 <- as.matrix(d)[492,]

image(1:28, 1:28, matrix(d_492, 28, 28))

# assessment
library(dslabs)
data(tissue_gene_expression)

table(tissue_gene_expression$y)
head(tissue_gene_expression)

d <- dist(tissue_gene_expression$x)
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

image(as.matrix(d))


x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x,y)

library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27$test %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

#logistic regression
library(caret)
fit_glm <- glm(y~x_1+x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

#fit knn model
knn_fit <- knn3(y ~ ., data = mnist_27$train)

x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)

knn_fit <- knn3(y ~ ., data = mnist_27$train, k=5)

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

# overtraining and overfitting

y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class") 
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")  
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

#fit knn with k=1
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$train$y)$overall[["Accuracy"]]

y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall[["Accuracy"]]

#fit knn with k=401
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]

#pick the k in knn
ks <- seq(3, 251, 2)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
})

#pick the k that maximizes accuracy using the estimates built on the test data
ks[which.max(accuracy$test)]
max(accuracy$test)


#assessment

library(caret)
y <- heights$sex
x <- heights$height
set.seed(1,sample.kind = "Rounding")
partition_ind <- createDataPartition(y, times = 1,p = 0.5,list = FALSE)
test_set <-  heights %>% slice(partition_ind)
train_set <- heights %>% slice(-partition_ind)

k <- seq(1,101,3)
F_1 <- sapply(k,function(k){
  fit <- knn3(sex~height, dat = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class")
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(k, F_1)
max(F_1)
k[which.max(F_1)]


library(dslabs)
library(caret)
data("tissue_gene_expression")
set.seed(1,sample.kind = "Rounding")
library(caret)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
train_index <- -(createDataPartition(y, times = 1,p = 0.5,list = FALSE))
k = seq(1, 11, 2)
accuracy <- sapply(k, function(k){
  fit <- knn3(x[train_index,], y[train_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[-train_index,]),
                   type = "class")
  mean(y_hat == y[-train_index])
})

accuracy
plot(k, accuracy)


# cross validation
library(tidyverse)
library(caret)

# set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]
fit <- train(x_subset, y, method = "glm")
fit$results

install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)
pvals <- tt$p.value
ind <- which(pvals <= 0.01)

x_subset <- x[,ind]
fit <- train(x_subset, y, method = "glm")
fit$results

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

k = seq(1,7,2)
data("tissue_gene_expression")
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
fit <- train(x, y, method = "knn", tuneGrid = data.frame( k = k))
ggplot(fit)
fit


# Bootstrap
library(dslabs)
library(caret)
data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)

class(indexes)
sum(indexes[[1]] == 3)
sum(indexes[[1]] == 4)
sum(indexes[[1]] == 7)

n <- 1:10
get_count <- function(n){
  sum(indexes[[n]] == 3)
}

count_arr <- sapply(n, get_count)
sum(count_arr)

y <- rnorm(100, 0, 1)

set.seed(1, sample.kind="Rounding")
B <- 10000
q <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(q)
sd(q)

set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding")
indexes <- createResample(y, 10)
q <- sapply(indexes, function(ind){
  y_hat <- y[ind]
  quantile(y_hat, 0.75)
})
mean(q)
sd(q)


set.seed(1, sample.kind = "Rounding")
indexes <- createResample(y, 10000)
q <- sapply(indexes, function(ind){
  y_hat <- y[ind]
  quantile(y_hat, 0.75)
})
mean(q)
sd(q)


#naive bayes
# Generating train and test set
library("caret")
data("heights")
y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

# Estimating averages and standard deviations
params <- train_set %>%
  group_by(sex) %>%
  summarize(avg = mean(height), sd = sd(height))
params

# Estimating the prevalence
pi <- train_set %>% summarize(pi=mean(sex=="Female")) %>% pull(pi)
pi

# Getting an actual rule
x <- test_set$height
f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])
p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))

# balance sensitivity and specificity
# Computing sensitivity
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# Computing specificity
specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# Changing the cutoff of the decision rule
p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5))
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))

# Draw plot
qplot(x, p_hat_bayes_unbiased, geom = "line") +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_vline(xintercept = 67, lty = 2)


#qda
# Load data
data("mnist_27")

# Estimate parameters from the data
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))

# Contour plots
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm", lwd = 1.5)

# Fit model
library(caret)
train_qda <- train(y ~., method = "qda", data = mnist_27$train)
# Obtain predictors and accuracy
y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

# Draw separate plots for 2s and 7s
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm") +
  facet_wrap(~y)

#lda
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))
params <- params %>% mutate(sd_1 = mean(sd_1), sd_2 = mean(sd_2), r = mean(r))
train_lda <- train(y ~., method = "lda", data = mnist_27$train)
y_hat <- predict(train_lda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

#case study more than 3 classes
if(!exists("mnist"))mnist <- read_mnist()

set.seed(3456)    #use set.seed(3456, sample.kind="Rounding") in R 3.6 or later
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000)
y <- mnist$train$labels[index_127] 
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list = FALSE)

# get the quadrants
# temporary object to help figure out the quadrants
row_column <- expand.grid(row=1:28, col=1:28)
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14)

# binarize the values. Above 200 is ink, below is no ink
x <- x > 200 

# cbind proportion of pixels in upper right quadrant and proportion of pixels in lower right quadrant
x <- cbind(rowSums(x[ ,upper_left_ind])/rowSums(x),
           rowSums(x[ ,lower_right_ind])/rowSums(x)) 

train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train,1],
                        x_2 = x[index_train,2])

test_set <- data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train,1],
                       x_2 = x[-index_train,2])

train_set %>%  ggplot(aes(x_1, x_2, color=y)) + geom_point()

train_qda <- train(y ~ ., method = "qda", data = train_set)
predict(train_qda, test_set, type = "prob") %>% head()
predict(train_qda, test_set) %>% head()
confusionMatrix(predict(train_qda, test_set), test_set$y)$table
confusionMatrix(predict(train_qda, test_set), test_set$y)$overall["Accuracy"]
train_lda <- train(y ~ ., method = "lda", data = train_set)
confusionMatrix(predict(train_lda, test_set), test_set$y)$overall["Accuracy"]
train_knn <- train(y ~ ., method = "knn", tuneGrid = data.frame(k = seq(15, 51, 2)),
                   data = train_set)
confusionMatrix(predict(train_knn, test_set), test_set$y)$overall["Accuracy"]
train_set %>% mutate(y = factor(y)) %>% ggplot(aes(x_1, x_2, fill = y, color=y)) + geom_point(show.legend = FALSE) + stat_ellipse(type="norm")


# assessment
library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

# set.seed(1993) #if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

fit <- train(x, y, method = "lda")
fit$results["Accuracy"]

#solution
fit$finalModel$means
t(fit$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()


library(dslabs)      
library(caret)
data("tissue_gene_expression")

set.seed(1993) #set.seed(1993, sample.kind="Rounding") if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

fit <- train(x, y, method = "qda")
fit$results["Accuracy"]

t(fit$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()


# set.seed(1993) #if using R 3.5 or earlier
library(ggrepel)
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

fit <- train(x, y, method = "lda", preProcess = "center")
fit$results["Accuracy"]

fit$finalModel$means
t(fit$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline() +
  geom_text_repel()


library(dslabs)      
library(caret)
data("tissue_gene_expression")

# set.seed(1993) # if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]
fit <- train(x, y, method = "lda", preProcess = "center")
fit$results["Accuracy"]

fit$finalModel$means
t(fit$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline() 


# classification and regression trees
# Load data
library(tidyverse)
library(dslabs)
data("olive")
olive %>% as_tibble()
table(olive$region)
olive <- select(olive, -area)

# Predict region using KNN
library(caret)
fit <- train(region ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = olive)
ggplot(fit)

# Plot distribution of each predictor stratified by region
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free") +
  theme(axis.text.x = element_blank())

# plot values for eicosenoic and linoleic
p <- olive %>% 
  ggplot(aes(eicosenoic, linoleic, color = region)) + 
  geom_point()
p + geom_vline(xintercept = 0.065, lty = 2) + 
  geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)

# load data for regression tree
data("polls_2008")
qplot(day, margin, data = polls_2008)

library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)

# visualize the splits 
plot(fit, margin = 0.1)
text(fit, cex = 0.75)
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# change parameters
fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# use cross validation to choose cp
library(caret)
train_rpart <- train(margin ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data = polls_2008)
ggplot(train_rpart)

# access the final model and plot it
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)
polls_2008 %>% 
  mutate(y_hat = predict(train_rpart)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# prune the tree 
pruned_fit <- prune(fit, cp = 0.01)

# classification (decision) tree
# fit a classification tree and plot it
train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train)
plot(train_rpart)

# compute accuracy
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# random forest
library(randomForest)
fit <- randomForest(margin~., data = polls_2008) 
plot(fit)

polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="red")

library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# use cross validation to choose parameter
train_rf_2 <- train(y ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]


#assessment
library(rpart)
n <- 1000
sigma <- 0.25
# set.seed(1) # if using R 3.5 or ealier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
fit <- rpart(y ~ ., data = dat) 

plot(fit, margin = 0.1)
text(fit, cex = 0.75)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)


library(randomForest)
fit <- randomForest(y ~ x, data = dat)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

plot(fit)


library(randomForest)
fit <- #BLANK
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

# caret package
  library(tidyverse)
  library(dslabs)
  data("mnist_27")
  
  library(caret)
  train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
  train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
  
  y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
  y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")
  
  confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
  confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

#assessment
  library(caret)
  library(dslabs)
  set.seed(1991)
  data("tissue_gene_expression")
  
  fit <- with(tissue_gene_expression, 
              train(x, y, method = "rpart",
                    tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))
  
  ggplot(fit)      

  set.seed(1991)
  data("tissue_gene_expression")
  
  fit_rpart <- with(tissue_gene_expression, 
                    train(x, y, method = "rpart",
                          tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                          control = rpart.control(minsplit = 0)))
  ggplot(fit_rpart)
  confusionMatrix(fit_rpart)
  plot(fit_rpart$finalModel, margin = 0.1)
  text(fit_rpart$finalModel, cex = 0.75)

  set.seed(1991)
  library(randomForest)
  fit <- with(tissue_gene_expression, 
              train(x, y, method = "rf", 
                    nodesize = 1,
                    tuneGrid = data.frame(mtry = seq(50, 200, 25))))
  
  ggplot(fit)  
  
  imp <- varImp(fit)
  tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
  tree_terms
  
  tibble(term = rownames(imp$importance), importance = imp$importance$Overall) %>%
    mutate(rank = rank(-importance)) %>%
    arrange(desc(importance)) %>%
    filter(term %in% tree_terms)
  
  #exercise
  library(titanic)    # loads titanic_train data frame
  library(caret)
  library(tidyverse)
  library(rpart)
  
  # 3 significant digits
  options(digits = 3)
  
  # clean the data - `titanic_train` is loaded with the titanic package
  titanic_clean <- titanic_train %>%
    mutate(Survived = factor(Survived),
           Embarked = factor(Embarked),
           Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
           FamilySize = SibSp + Parch + 1) %>%    # count family members
    select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)
  
  set.seed(42, sample.kind = "Rounding")
  test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
  train_set <- titanic_clean %>% slice(-test_index)
  test_set <- titanic_clean %>% slice(test_index)
  
  set.seed(3, sample.kind = "Rounding")
  y_hat <- sample(c(0,1), length(test_set$Survived), replace = TRUE)
  mean(test_set$Survived == y_hat)

  x_fem <- train_set %>% filter(Sex == 'female')
  mean(x_fem$Survived==1)
  
  x_mal <- train_set %>% filter(Sex == 'male')
  mean(x_mal$Survived==1)

  model1 <- ifelse(test_set$Sex == "female", 1, 0)    
  mean(model1 == test_set$Survived)     

  test_set %>% group_by(Pclass) %>%
    summarize(Survival_Rate = mean(Survived==1))  
  model2 <- ifelse(test_set$Pclass == 1, 1, 0)
  mean(model2 == test_set$Survived)

  train_set %>% group_by(Sex, Pclass) %>%
    summarize(Survival_Rate = mean(Survived==1))
  
  fem_1_2_model <- ifelse( (test_set$Pclass==1 & test_set$Sex=='female') | (test_set$Pclass==2 & test_set$Sex =='female'), 1, 0)
  mean(fem_1_2_model == test_set$Survived)

  confusionMatrix(data=factor(model1), reference=factor(test_set$Survived))
  confusionMatrix(data=factor(model2), reference=factor(test_set$Survived))
  confusionMatrix(data=factor(fem_1_2_model), reference=factor(test_set$Survived))  

  F_meas(data=factor(model1), reference=factor(test_set$Survived))
  F_meas(data=factor(model2), reference=factor(test_set$Survived))
  F_meas(data=factor(fem_1_2_model), reference=factor(test_set$Survived))             

  set.seed(1, sample.kind = "Rounding")
  lda_model <- train(Survived ~ Fare, method = "lda", data = train_set)
  confusionMatrix(predict(lda_model, test_set), test_set$Survived)  
  
  set.seed(1, sample.kind = "Rounding")
  qda_model <- train(Survived ~ Fare, method = "qda", data = train_set)
  confusionMatrix(predict(qda_model, test_set), test_set$Survived)  

  set.seed(1, sample.kind = "Rounding")
  glm_model <- train(Survived ~ Age, method = "glm", data = train_set)
  confusionMatrix(predict(glm_model, test_set), test_set$Survived)
  
  set.seed(1, sample.kind = "Rounding")
  glm_model <- train(Survived ~ Sex + Pclass + Fare + Age, method = "glm", data = train_set)
  confusionMatrix(predict(glm_model, test_set), test_set$Survived)
  
  set.seed(1, sample.kind = "Rounding")
  glm_model <- train(Survived ~ ., method = "glm", data = train_set)
  confusionMatrix(predict(glm_model, test_set), test_set$Survived)
  
  set.seed(6, sample.kind = "Rounding")
  knn_model <- train(Survived ~ ., method = "knn", data = train_set,
                     tuneGrid = data.frame(k = seq(3, 51, 2)))
  knn_model
  plot(knn_model)
  
  set.seed(6, sample.kind = "Rounding")
  confusionMatrix(predict(knn_model, test_set), test_set$Survived)

  set.seed(8, sample.kind="Rounding")
  control <- trainControl(method="cv", number=10, p=0.9)
  knn_model <- train(Survived ~ ., method = "knn", data = train_set, trControl = control,
                     tuneGrid = data.frame(k = seq(3, 51, 2)))
  knn_model
  plot(knn_model)
  knn_model$bestTune  
  confusionMatrix(predict(knn_model, test_set), test_set$Survived)
  
  # decision tree
  set.seed(10, sample.kind = "Rounding")
  train_rpart <- train(Survived ~ .,
                       method = "rpart",
                       tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                       data = train_set)
  plot(train_rpart)
  
  # compute accuracy
  confusionMatrix(predict(train_rpart, test_set), test_set$Survived)$overall["Accuracy"]
  
  plot(train_rpart$finalModel, margin = 0.1)
  text(train_rpart$finalModel)
  
  #random forest
  library(caret)
  library(randomForest)
  set.seed(14)
  train_rf <- train(Survived ~ ., method = "rf", 
                    ntree = 100,
                    tuneGrid = data.frame(mtry = seq(1:7)),
                    data = train_set)
  
  plot(train_rf)
  
  confusionMatrix(predict(train_rf, test_set), test_set$Survived)$overall["Accuracy"]
  
  imp <- varImp(train_rf)
  tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
  tree_terms
  
  tibble(term = rownames(imp$importance), importance = imp$importance$Overall) %>%
    mutate(rank = rank(-importance)) %>%
    arrange(desc(importance)) %>%
    filter(term %in% tree_terms)
  
  #MNIST Case study
  library(dslabs)
  mnist <- read_mnist()
  
  names(mnist)
  dim(mnist$train$images)
  
  class(mnist$train$labels)
  table(mnist$train$labels)
  
  # sample 10k rows from training set, 1k rows from test set
  set.seed(123)
  index <- sample(nrow(mnist$train$images), 10000)
  x <- mnist$train$images[index,]
  y <- factor(mnist$train$labels[index])
  
  index <- sample(nrow(mnist$test$images), 1000)
  #note that the line above is the corrected code - code in video at 0:52 is incorrect
  x_test <- mnist$test$images[index,]
  y_test <- factor(mnist$test$labels[index])

  #preprocessing
  library(matrixStats)
  sds <- colSds(x)
  qplot(sds, bins = 256, color = I("black"))
  
  library(caret)
  nzv <- nearZeroVar(x)
  image(matrix(1:784 %in% nzv, 28, 28))
  
  col_index <- setdiff(1:ncol(x), nzv)
  length(col_index)  

  #Model fitting
  colnames(x) <- 1:ncol(mnist$train$images)
  colnames(x_test) <- colnames(x)
  
  control <- trainControl(method = "cv", number = 10, p = .9)
  train_knn <- train(x[,col_index], y,
                     method = "knn", 
                     tuneGrid = data.frame(k = c(1,3,5,7)),
                     trControl = control)
  ggplot(train_knn)
  
  n <- 1000
  b <- 2
  index <- sample(nrow(x), n)
  control <- trainControl(method = "cv", number = b, p = .9)
  train_knn <- train(x[index ,col_index], y[index],
                     method = "knn",
                     tuneGrid = data.frame(k = c(3,5,7)),
                     trControl = control)
  fit_knn <- knn3(x[ ,col_index], y,  k = 3)
  
  y_hat_knn <- predict(fit_knn,
                       x_test[, col_index],
                       type="class")
  cm <- confusionMatrix(y_hat_knn, factor(y_test))
  cm$overall["Accuracy"]
  
  cm$byClass[,1:2]
  
  library(Rborist)
  control <- trainControl(method="cv", number = 5, p = 0.8)
  grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))
  train_rf <-  train(x[, col_index], y,
                     method = "Rborist",
                     nTree = 50,
                     trControl = control,
                     tuneGrid = grid,
                     nSamp = 5000)
  ggplot(train_rf)
  train_rf$bestTune
  
  fit_rf <- Rborist(x[, col_index], y,
                    nTree = 1000,
                    minNode = train_rf$bestTune$minNode,
                    predFixed = train_rf$bestTune$predFixed)
  
  y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
  cm <- confusionMatrix(y_hat_rf, y_test)
  cm$overall["Accuracy"]
  
  rafalib::mypar(3,4)
  for(i in 1:12){
    image(matrix(x_test[i,], 28, 28)[, 28:1], 
          main = paste("Our prediction:", y_hat_rf[i]),
          xaxt="n", yaxt="n")
  }
  
  #Variable Importance
  library(randomForest)
  x <- mnist$train$images[index,]
  y <- factor(mnist$train$labels[index])
  rf <- randomForest(x, y,  ntree = 50)
  imp <- importance(rf)
  imp
  
  image(matrix(imp, 28, 28))
  
  p_max <- predict(fit_knn, x_test[,col_index])
  p_max <- apply(p_max, 1, max)
  ind  <- which(y_hat_knn != y_test)
  ind <- ind[order(p_max[ind], decreasing = TRUE)]
  rafalib::mypar(3,4)
  for(i in ind[1:12]){
    image(matrix(x_test[i,], 28, 28)[, 28:1],
          main = paste0("Pr(",y_hat_knn[i],")=",round(p_max[i], 2),
                        " but is a ",y_test[i]),
          xaxt="n", yaxt="n")
  }
  
  p_max <- predict(fit_rf, x_test[,col_index])$census  
  p_max <- p_max / rowSums(p_max)
  p_max <- apply(p_max, 1, max)
  ind  <- which(y_hat_rf != y_test)
  ind <- ind[order(p_max[ind], decreasing = TRUE)]
  rafalib::mypar(3,4)
  for(i in ind[1:12]){
    image(matrix(x_test[i,], 28, 28)[, 28:1], 
          main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2),
                        " but is a ",y_test[i]),
          xaxt="n", yaxt="n")
  }

  # Ensemble
  p_rf <- predict(fit_rf, x_test[,col_index])$census
  p_rf <- p_rf / rowSums(p_rf)
  p_knn <- predict(fit_knn, x_test[,col_index])
  p <- (p_rf + p_knn)/2
  y_pred <- factor(apply(p, 1, which.max)-1)
  confusionMatrix(y_pred, y_test)

  
  #assessment
  models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

  library(caret)
  library(dslabs)
  library(tidyverse)
  # set.seed(1) # if using R 3.5 or earlier
  set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
  data("mnist_27")
  
  fits <- lapply(models, function(model){ 
    print(model)
    train(y ~ ., method = model, data = mnist_27$train)
  }) 
  
  names(fits) <- models

  pred <- sapply(fits, function(object) {
    predict(object, newdata = mnist_27$test)
    })
  dim(pred)
  
  accuracy <- colMeans(pred == mnist_27$test$y)
  accuracy
  mean(accuracy)

  means_7 <- rowMeans(pred == "7")
  y_hat <- ifelse(means_7 > 0.5, "7", "2")
  mean(y_hat == mnist_27$test$y)

  acc <- sapply(fits, function(fit) { 
    min(fit$results$Accuracy)
    })
  mean(acc)

  means_7 <- rowMeans(pred[,which(acc > 0.8)] == "7")
  y_hat <- ifelse(means_7 > 0.5, "7", "2")
  mean(y_hat == mnist_27$test$y)
    
  # Recommendation Systems
  library(dslabs)
  library(tidyverse)
  data("movielens")
  
  head(movielens)
  
  movielens %>%
    summarize(n_users = n_distinct(userId),
              n_movies = n_distinct(movieId))
  
  keep <- movielens %>%
    dplyr::count(movieId) %>%
    top_n(5) %>%
    pull(movieId)
  tab <- movielens %>%
    filter(userId %in% c(13:20)) %>% 
    filter(movieId %in% keep) %>% 
    select(userId, title, rating) %>% 
    spread(title, rating)
  tab %>% knitr::kable()
  
  users <- sample(unique(movielens$userId), 100)
  rafalib::mypar()
  movielens %>% filter(userId %in% users) %>% 
    select(userId, movieId, rating) %>%
    mutate(rating = 1) %>%
    spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
    as.matrix() %>% t(.) %>%
    image(1:100, 1:100,. , xlab="Movies", ylab="Users")
  abline(h=0:100+0.5, v=0:100+0.5, col = "grey")
  
  movielens %>% 
    dplyr::count(movieId) %>% 
    ggplot(aes(n)) + 
    geom_histogram(bins = 30, color = "black") + 
    scale_x_log10() + 
    ggtitle("Movies")
  
  movielens %>%
    dplyr::count(userId) %>% 
    ggplot(aes(n)) + 
    geom_histogram(bins = 30, color = "black") + 
    scale_x_log10() +
    ggtitle("Users")
  
  library(caret)
  set.seed(755)
  test_index <- createDataPartition(y = movielens$rating, times = 1,
                                    p = 0.2, list = FALSE)
  train_set <- movielens[-test_index,]
  test_set <- movielens[test_index,]
  
  test_set <- test_set %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))
  }

  #
  mu_hat <- mean(train_set$rating)
  mu_hat
  
  naive_rmse <- RMSE(test_set$rating, mu_hat)
  naive_rmse
  
  predictions <- rep(2.5, nrow(test_set))
  RMSE(test_set$rating, predictions)
  
  rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
  
  # fit <- lm(rating ~ as.factor(userId), data = movielens)
  mu <- mean(train_set$rating) 
  movie_avgs <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = mean(rating - mu))
  
  movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))
  
  predicted_ratings <- mu + test_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    .$b_i
  
  model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
  rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Movie Effect Model",
                                       RMSE = model_1_rmse ))
  
  rmse_results %>% knitr::kable()
  
  train_set %>% 
    group_by(userId) %>% 
    summarize(b_u = mean(rating)) %>% 
    filter(n()>=100) %>%
    ggplot(aes(b_u)) + 
    geom_histogram(bins = 30, color = "black")
  
  # lm(rating ~ as.factor(movieId) + as.factor(userId))
  user_avgs <- test_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu - b_i))
  
  predicted_ratings <- test_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
  rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Movie + User Effects Model",  
                                       RMSE = model_2_rmse ))
  rmse_results %>% knitr::kable()  

  # assessment
  library(tidyverse)
  library(lubridate)
  library(dslabs)
  data("movielens")

  movielens %>% group_by(movieId) %>% summarize(n=n(), year = as.character(first(year))) %>%
    ggplot(aes(year, n)) +
    geom_boxplot() +
    coord_trans(y = "sqrt") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

  movielens %>% 
    filter(year >= 1993) %>%
    group_by(movieId) %>%
    summarize(n = n(), years = 2018 - first(year),
              title = title[1],
              rating = mean(rating)) %>%
    mutate(rate = n/years) %>%
    top_n(25, rate) %>%
    arrange(desc(rate)) 

  movielens %>% 
    filter(year >= 1993) %>%
    group_by(movieId) %>%
    summarize(n = n(), years = 2017 - first(year),
              title = title[1],
              rating = mean(rating)) %>%
    mutate(rate = n/years) %>%
    ggplot(aes(rate, rating)) +
    geom_point() +
    geom_smooth()  

  movielens <- mutate(movielens, date = as_datetime(timestamp))
  movielens %>% mutate(date = round_date(date, unit = "week")) %>%
    group_by(date) %>%
    summarize(rating = mean(rating)) %>%
    ggplot(aes(date, rating)) +
    geom_point() +
    geom_smooth()

  movielens %>% group_by(genres) %>%
    summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
    filter(n >= 1000) %>% 
    mutate(genres = reorder(genres, avg)) %>%
    ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
    geom_point() +
    geom_errorbar() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
    
  
  #regularization
  library(dslabs)
  library(tidyverse)
  library(caret)
  data("movielens")
  set.seed(755)
  test_index <- createDataPartition(y = movielens$rating, times = 1,
                                    p = 0.2, list = FALSE)
  train_set <- movielens[-test_index,]
  test_set <- movielens[test_index,]
  test_set <- test_set %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))
  }
  mu_hat <- mean(train_set$rating)
  naive_rmse <- RMSE(test_set$rating, mu_hat)
  rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
  mu <- mean(train_set$rating) 
  movie_avgs <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = mean(rating - mu))
  predicted_ratings <- mu + test_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    .$b_i
  model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
  rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Movie Effect Model",
                                       RMSE = model_1_rmse ))
  user_avgs <- test_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu - b_i))
  predicted_ratings <- test_set %>% 
    left_join(movie_avgs, by='movieId') %>%
         left_join(user_avgs, by='userId') %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
  rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Movie + User Effects Model",  
                                       RMSE = model_2_rmse ))
  
  test_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    mutate(residual = rating - (mu + b_i)) %>%
    arrange(desc(abs(residual))) %>% 
    select(title,  residual) %>% slice(1:10) %>% knitr::kable()
  
  movie_titles <- movielens %>% 
    select(movieId, title) %>%
    distinct()
  movie_avgs %>% left_join(movie_titles, by="movieId") %>%
    arrange(desc(b_i)) %>% 
    select(title, b_i) %>% 
    slice(1:10) %>%  
    knitr::kable()
  
  movie_avgs %>% left_join(movie_titles, by="movieId") %>%
    arrange(b_i) %>% 
    select(title, b_i) %>% 
    slice(1:10) %>%  
    knitr::kable()
  
  train_set %>% dplyr::count(movieId) %>% 
    left_join(movie_avgs) %>%
    left_join(movie_titles, by="movieId") %>%
    arrange(desc(b_i)) %>% 
    select(title, b_i, n) %>% 
    slice(1:10) %>% 
    knitr::kable()
  
  train_set %>% dplyr::count(movieId) %>% 
    left_join(movie_avgs) %>%
    left_join(movie_titles, by="movieId") %>%
    arrange(b_i) %>% 
    select(title, b_i, n) %>% 
    slice(1:10) %>% 
    knitr::kable()
  lambda <- 3
  mu <- mean(train_set$rating)
  movie_reg_avgs <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 
  
  data_frame(original = movie_avgs$b_i, 
             regularlized = movie_reg_avgs$b_i, 
             n = movie_reg_avgs$n_i) %>%
    ggplot(aes(original, regularlized, size=sqrt(n))) + 
    geom_point(shape=1, alpha=0.5)
  
  train_set %>%
    dplyr::count(movieId) %>% 
    left_join(movie_reg_avgs) %>%
    left_join(movie_titles, by="movieId") %>%
    arrange(desc(b_i)) %>% 
    select(title, b_i, n) %>% 
    slice(1:10) %>% 
    knitr::kable()
  
  train_set %>%
    dplyr::count(movieId) %>% 
    left_join(movie_reg_avgs) %>%
    left_join(movie_titles, by="movieId") %>%
    arrange(b_i) %>% 
    select(title, b_i, n) %>% 
    slice(1:10) %>% 
    knitr::kable()
  
  predicted_ratings <- test_set %>% 
    left_join(movie_reg_avgs, by='movieId') %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  
  model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
  rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Regularized Movie Effect Model",  
                                       RMSE = model_3_rmse ))
  rmse_results %>% knitr::kable()
  
  lambdas <- seq(0, 10, 0.25)
  mu <- mean(train_set$rating)
  just_the_sum <- train_set %>% 
    group_by(movieId) %>% 
    summarize(s = sum(rating - mu), n_i = n())
  rmses <- sapply(lambdas, function(l){
    predicted_ratings <- test_set %>% 
      left_join(just_the_sum, by='movieId') %>% 
      mutate(b_i = s/(n_i+l)) %>%
      mutate(pred = mu + b_i) %>%
      .$pred
    return(RMSE(predicted_ratings, test_set$rating))
  })
  qplot(lambdas, rmses)  
  lambdas[which.min(rmses)]
  
  lambdas <- seq(0, 10, 0.25)
  rmses <- sapply(lambdas, function(l){
    mu <- mean(train_set$rating)
    b_i <- train_set %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+l))
    b_u <- train_set %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    predicted_ratings <- 
      test_set %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      .$pred
    return(RMSE(predicted_ratings, test_set$rating))
  })
  
  qplot(lambdas, rmses)  
  
  lambda <- lambdas[which.min(rmses)]
  lambda
  
  rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Regularized Movie + User Effect Model",  
                                       RMSE = min(rmses)))
  rmse_results %>% knitr::kable()

  # assessment
  library(dslabs)
  library(tidyverse)
  library(caret)
  options(digits=7)
  
  # set.seed(1986) # if using R 3.5 or earlier
  set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
  n <- round(2^rnorm(1000, 8, 1))

  # set.seed(1) # if using R 3.5 or earlier
  set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
  mu <- round(80 + 2*rt(1000, 5))
  range(mu)
  schools <- data.frame(id = paste("PS",1:1000),
                        size = n,
                        quality = mu,
                        rank = rank(-mu))  

  schools %>% top_n(10, quality) %>% arrange(desc(quality))  

  # set.seed(1) # if using R 3.5 or earlier
  set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
  mu <- round(80 + 2*rt(1000, 5))
  
  scores <- sapply(1:nrow(schools), function(i){
    scores <- rnorm(schools$size[i], schools$quality[i], 30)
    scores
  })
  schools <- schools %>% mutate(score = sapply(scores, mean))
    
  top10 <- schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)
  median(top10$size)
  median(schools$size)  
  
  worse10 <- schools %>% arrange(score) %>% select(id, size, score) %>% head(10)
  median(worse10$size)
  
  schools %>% ggplot(aes(size, score, color=quality)) +
    geom_point(alpha=0.3) +
    geom_point(data = filter(schools, rank<=10), col = 2) 

  overall <- mean(sapply(scores, mean))      
  alpha <- 25
  score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
  schools <- schools %>% mutate(score_reg = score_reg)
  schools %>% mutate(score_reg = score_reg) %>%
    top_n(10, score_reg) %>% arrange(desc(score_reg))

  schools %>% ggplot(aes(size, score_reg, color=quality)) +
    geom_point(alpha=0.3) +
    geom_point(data = filter(schools, rank<=10), col = 2) 
  
  alphas <- seq(10,250)
  rmse <- sapply(alphas, function(alpha){
    score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
    mean((score_reg - schools$quality)^2)
  })
  plot(alphas, rmse)
  alphas[which.min(rmse)] 

  alpha <- 135
  score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
  schools %>% mutate(score_reg = score_reg) %>% top_n(10, score_reg) %>% arrange(desc(score_reg))
  schools %>% ggplot(aes(size, score_reg, color=quality)) +
    geom_point(alpha=0.3) +
    geom_point(data = filter(schools, rank<=10), col = 2) 
  
  alpha <- alphas[which.min(rmse)]  
  score_reg <- sapply(scores, function(x)
    overall+sum(x-overall)/(length(x)+alpha))
  mu <- mean(score_reg)
  schools <- data.frame(id = paste("PS",1:1000),
                        size = n,
                        quality = mu,
                        rank = rank(-mu)) 
  schools %>% mutate(score_reg = score_reg) %>%
    top_n(10, score_reg) %>% arrange(desc(score_reg))

  alphas <- seq(10,250)
  rmse <- sapply(alphas, function(alpha){
    score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
    mean((score_reg - schools$quality)^2)
  })
  plot(alphas, rmse)
  alphas[which.min(rmse)]   

  library(dslabs)
  library(tidyverse)
  library(caret)
  data("movielens")
  # matrix factorisation
  train_small <- movielens %>% 
    group_by(movieId) %>%
    filter(n() >= 50 | movieId == 3252) %>% ungroup() %>% #3252 is Scent of a Woman used in example
    group_by(userId) %>%
    filter(n() >= 50) %>% ungroup()
  
  y <- train_small %>% 
    select(userId, movieId, rating) %>%
    spread(movieId, rating) %>%
    as.matrix()
  
  rownames(y)<- y[,1]
  y <- y[,-1]
  colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])
  
  y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))
  y <- sweep(y, 2, colMeans(y, na.rm=TRUE))
  
  m_1 <- "Godfather, The"
  m_2 <- "Godfather: Part II, The"
  qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2)
  
  m_1 <- "Godfather, The"
  m_3 <- "Goodfellas"
  qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)
  
  m_4 <- "You've Got Mail" 
  m_5 <- "Sleepless in Seattle" 
  qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)
  
  cor(y[, c(m_1, m_2, m_3, m_4, m_5)], use="pairwise.complete") %>% 
    knitr::kable()
  
  set.seed(1)
  options(digits = 2)
  Q <- matrix(c(1 , 1, 1, -1, -1), ncol=1)
  rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5)
  P <- matrix(rep(c(2,0,-2), c(3,5,4)), ncol=1)
  rownames(P) <- 1:nrow(P)
  
  X <- jitter(P%*%t(Q))
  X %>% knitr::kable(align = "c")
  
  cor(X)
  
  t(Q) %>% knitr::kable(aling="c")
  
  P
  
  set.seed(1)
  options(digits = 2)
  m_6 <- "Scent of a Woman"
  Q <- cbind(c(1 , 1, 1, -1, -1, -1), 
             c(1 , 1, -1, -1, -1, 1))
  rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5, m_6)
  P <- cbind(rep(c(2,0,-2), c(3,5,4)), 
             c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2
  rownames(P) <- 1:nrow(X)
  
  X <- jitter(P%*%t(Q), factor=1)
  X %>% knitr::kable(align = "c")
  
  cor(X)
  
  t(Q) %>% knitr::kable(align="c")
  
  P
  
  six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6)
  tmp <- y[,six_movies]
  cor(tmp, use="pairwise.complete")

  
  # SVD and PCA
  y[is.na(y)] <- 0
  y <- sweep(y, 1, rowMeans(y))
  pca <- prcomp(y)
  
  dim(pca$rotation)
  
  dim(pca$x)
  
  plot(pca$sdev)
  
  var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
  plot(var_explained)
  
  library(ggrepel)
  pcs <- data.frame(pca$rotation, name = colnames(y))
  pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() + 
    geom_text_repel(aes(PC1, PC2, label=name),
                    data = filter(pcs, 
                                  PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1))
  
  pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)
  
  pcs %>% select(name, PC1) %>% arrange(desc(PC1)) %>% slice(1:10)
  
  pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)
  
  pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)
  
  
  # exercise - Student grades
  set.seed(1987, sample.kind="Rounding")
  #if using R 3.6 or later, use `set.seed(1987, sample.kind="Rounding")` instead
  n <- 100
  k <- 8
  Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
  m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
  m <- m[order(rowMeans(m), decreasing = TRUE),]
  y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
  colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                   paste(rep("Science",k), 1:k, sep="_"),
                   paste(rep("Arts",k), 1:k, sep="_"))
  
  my_image <- function(x, zlim = range(x), ...){
    colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
    cols <- 1:ncol(x)
    rows <- 1:nrow(x)
    image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
          xlab="", ylab="",  col = colors, zlim = zlim, ...)
    abline(h=rows + 0.5, v = cols + 0.5)
    axis(side = 1, cols, colnames(x), las = 2)
  }
  
  my_image(y)
  
  my_image(cor(y), zlim = c(-1,1))
  range(cor(y))
  axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

  s <- svd(y)
  names(s)  
  
  y_svd <- s$u %*% diag(s$d) %*% t(s$v)
  abs(y - y_svd)
  
  ss_y <- colSums(y^2)
  ss_yv <- apply((y%*%s$v)^2, 2, sum)
  ggplot() +
    geom_point(aes(x=1:24, y=ss_y), color="blue") +
    geom_point(aes(x=1:24, y=ss_yv), color="red")
  
  data.frame(x = sqrt(ss_yv), y = s$d) %>%
    ggplot(aes(x,y)) +
    geom_point()

  identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))  

  plot(-s$u[,1]*s$d[1], rowMeans(y))  

  my_image(s$v)  
  
  plot(s$u[,1], ylim = c(-0.25, 0.25))
  plot(s$v[,1], ylim = c(-0.25, 0.25))
  with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))
  my_image(y)  

  resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
  my_image(cor(resid), zlim = c(-1,1))
  axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2) 
  
  plot(s$u[,2], ylim = c(-0.25, 0.25))
  plot(s$v[,2], ylim = c(-0.25, 0.25))
  with(s, my_image((u[, 2, drop=FALSE]*d[1]) %*% t(v[, 2, drop=FALSE])))
  my_image(resid)  

  plot(s$v[,2])
  
  resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
  my_image(cor(resid), zlim = c(-1,1))
  axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)  

  y_hat <- with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
  my_image(y, zlim = range(y))
  my_image(y_hat, zlim = range(y))
  my_image(y - y_hat, zlim = range(y))  

  # comp. check
  data("tissue_gene_expression")
  dim(tissue_gene_expression$x)
  pc <- prcomp(tissue_gene_expression$x)
  data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
             tissue = tissue_gene_expression$y) %>%
    ggplot(aes(pc_1, pc_2, color = tissue)) +
    geom_point()
  
  data.frame(pc_1 = pc$x[,1], avg = rowMeans(tissue_gene_expression$x), 
             tissue = tissue_gene_expression$y) %>%
    ggplot(aes(pc_1, avg, color = tissue)) +
    geom_point()
  
  test <- data.frame(pc_1 = pc$x[,1], avg = rowMeans(tissue_gene_expression$x), 
             tissue = tissue_gene_expression$y)
  cor(test$avg, test$pc_1)
  
  x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
  pc <- prcomp(x)
  data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
             tissue = tissue_gene_expression$y) %>%
    ggplot(aes(pc_1, pc_2, color = tissue)) +
    geom_point()
   
  for(i in 1:10)
    boxplot(pc$x[,i] ~ tissue_gene_expression$y)

  boxplot(pc$x[,7] ~ tissue_gene_expression$y)
  
  plot(summary(pc)$importance[3,1:10])
  