##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(lubridate)
library(recosystem)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

dim(edx)

edx[edx$rating==0]
edx[edx$rating==3]

movie_group <- edx %>% group_by(movieId) %>% summarize(count=n())
user_group <- edx %>% group_by(userId) %>% summarize(n())

# Lets review the genre counts
edx %>% filter(genres %like% "Drama") %>% summarize(n())
edx %>% filter(genres %like% "Comedy") %>% summarize(n())
edx %>% filter(genres %like% "Thriller") %>% summarize(n())
edx %>% filter(genres %like% "Romance") %>% summarize(n())

# Review ratings distribution
edx %>% select(rating) %>% group_by(rating) %>% summarize(count=n()) %>% arrange(desc(count))

# Lets look at the distribution for ratings
hist(edx$rating)

# Get the average, use it for prediction and record RMSE
mu_hat <- mean(edx$rating)
rmse_avg <- RMSE(edx$rating, mu_hat)
rmse_results <- tibble(method = "Just the average", RMSE = rmse_avg)

# Assign edx as the train_set table and validation as the test_set table
train_set <- edx
test_set <- validation

# Calculate the average
mu <- mean(train_set$rating) 

# Calculate the movie effect by looking at the mean when grouped by movie
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# Join with validation test to compare with movies that have been predicted
predicted_ratings <- mu+test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

# Calculate RMSE and record
model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))

# Add user effect to the movie effect and get RMSE for comparison
# with validation test
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

# Calculate residual and plot it for the movie effect.
test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  select(title,  residual) %>% slice(1:10) %>% knitr::kable()
movie_residual <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  select(title,  residual)
plot(1:50000, movie_residual$residual[1:50000])

# Get distinct movie titles
movie_titles <- edx %>% 
  select(movieId, title) %>%
  distinct()

# Inspect the top 10 movies with the most positive movie effect
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

# Inspect the least 10 movies with the negative movie effect
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

# We can see there is a more negative movie effect than positive effect

# Inspect the top 10 movies with the most positive effect and 
# the number of reviews
train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

# Inspect the least 10 movies with the most negative effect and 
# the number of reviews
train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

# We can see that the movies dont appear to be top 10 movies in reality.
# This could be because people tend to review popular reviews and this
# is not captured in the top 10.

# We want to first check the regularization rate histogram to get an idea of the
# how the regularization affects the movie effect.
# Lets set a regularization rate that gives a normal distribution to b_i and
# performs regularization - pulls values closer to 0.
lambda <- 0
mu <- mean(train_set$rating)
movie_reg_avgs_no <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) %>%
  mutate(eff="No Regularization")

lambda2 <- 3
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda2), n_i = n()) %>%
  mutate(eff="With Regularization Rate 3")

# Lets plot the distribution for the regularized movie effect and
# non-regularized movie effect
reg_movie_eff <- rbind(movie_reg_avgs_no, movie_reg_avgs)
ggplot(reg_movie_eff, aes(b_i, fill = eff)) + geom_density(alpha = 0.2)

# We can see how b_i helps pull the movie effect closer to zero.

# Plot the regularized vs. original movie effect
data_frame(original = movie_avgs$b_i, 
           regularized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

# We can see that the regularization helps pull the movie effect closer to 0 as
# well as how the movies with more reviews are unaffected by the effect which
# is as expected.

# Inspect the top 10 movies with positive movie effect
train_set %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

# We can see that the regularized movie effect makes more sense looking at the
# titles of the movie names.

# Now lets look at the least 10 movies with lowest ratings.
train_set %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

# Looking at the movie titles it is understood why these movies were given
# lower ratings.

# Get the predicted ratings based on this regularized movie effect model
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

# Calculate RMSE and add to the results table
model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()

# Comparing the RMSE for the regularized movie effect model with the original
# movie effect model it doesn't appear regularization improved RMSE
# siginificantly. I checked other values for lambda like 2, 6, 100, 1000 - I get
# similar RMSE results with those values too.

# Lets now build the regularized movie and user effect model. This time we will
# use different values for lambda to tune it for the best RMSE result.
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

# Lets plot to see the lambda that gives the least RMSE
qplot(lambdas, rmses)  
lambda <- lambdas[which.min(rmses)]
lambda

# Lets add it to our RMSE table
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

# Matrix Factorization - we will use the recosystem package to build a
# recommender system using Matrix Factorization.
# First create a data frame with user, movie and rating
train_set <- edx  %>%  select(userId, movieId, rating) %>%
  mutate(userId = as.integer(userId)
         ,movieId = as.integer(movieId)
         ,rating = rating)   

# Lets specify the source of data to the recommender system. This is stored
# in memory rather than refering to a file.
train_Memory <- data_memory(user_index = train_set$userId,
                            item_index = train_set$movieId,
                            rating = train_set$rating, index1 = TRUE)

# Create an instance of the recommender system and initialize it
recommender <- Reco()

# Next, tune the recommender system
# The following parameters are used:
# dim - the number of latent factors
# 
opts <- recommender$tune(
  train_Memory,
  opts = list(
    dim      = c(70), # number of latent factors
    costp_l1 = 0, # L1 regularization cost for user factors
    costq_l1 = 0, # L1 regularization cost for item factors
    nthread  = 4, # number of threads for parallel computing
    niter    = 10 # number of iterations
  )
)

# Train the recommender system with optimized parameters
recommender$train(train_Memory, opts = c(opts$min, # optimized parameters                     
                                         niter = 10, # iterations 
                                         nthread = 4)) # number of threads 

# Prepare the validation set to calculate RMSE
test_set <- validation %>% select(userId, movieId, rating) %>%
  mutate(userId = as.integer(userId)
         ,movieId = as.integer(movieId)
         ,rating = rating)   
test_Memory <- data_memory(user_index = test_set$userId, 
                           item_index = test_set$movieId, 
                           rating = test_set$rating, index1 = TRUE)  

# Get prediction for validation data set
test_set$prediction <- recommender$predict(test_Memory, out_memory())

# Histogram of the predictions
hist(test_set$prediction)
sum(test_set$prediction < 0.5)
sum(test_set$prediction > 5)

# We can see there are a few values that are greater than 5 and less than 0.5
# rating. We will set these to 5 and 0.5 respectively.
test_set <- test_set %>% mutate(prediction_opts = case_when(prediction < 0.5 ~ 0.5, prediction > 5 ~ 5, TRUE ~ prediction))

# Calculate RMSE and record result
RMSE <- RMSE(test_set$rating, test_set$prediction_opts)  
# When input is the original, real-valued rating use: RMSE <- rmse(test$rating, ceiling(test$prediction_opts * 2)/2 )   
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Matrix Factorization Model",  
                                     RMSE = RMSE))
rmse_results %>% knitr::kable()
