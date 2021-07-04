### WRANGLE-DATA ###

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
library(stringr)

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
set.seed(1, sample.kind="Rounding")
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

# Extract year of release from title column for edx and validation
validation <- validation %>%
  mutate(year = str_extract(title, " \\(\\d*\\)$")) %>%
  mutate(year = str_remove(year, "\\("), year = str_remove(year, "\\)")) %>%
  mutate(year = as.integer(year)) %>%
  mutate(title = str_remove(title, " \\(\\d*\\)$"))
edx <- edx %>%
  mutate(year = str_extract(title, " \\(\\d*\\)$")) %>%
  mutate(year = str_remove(year, "\\("), year = str_remove(year, "\\)")) %>%
  mutate(year = as.integer(year)) %>%
  mutate(title = str_remove(title, " \\(\\d*\\)$"))

save(edx, file = "rda/edx.rda")
save(validation, file = "rda/validation.rda")



### DATA-ANALYSIS ###



load("rda/edx.rda")
library(lubridate)
### Collect infos

## Collect movie infos
movie_infos <- edx %>% group_by(movieId) %>%
  mutate(total_number_of_ratings = n(),
         avg_rating = mean(rating),
         se = sd(rating)/sqrt(n())) %>%
  slice(1) %>%
  select(-rating, -userId, -timestamp)

## Collect user infos
user_infos <- edx %>% group_by(userId) %>%
  mutate(total_number_of_ratings = n(),
         avg_rating = mean(rating),
         se = sd(rating)/sqrt(n())) %>%
  slice(1) %>%
  select(-rating, -movieId, -timestamp, - year, -genres, -title)

### Observing effects

## Movie effect
mu_hat <- mean(edx$rating)

movie_infos %>%
  mutate(ymin = avg_rating - 2*se,
         ymax = avg_rating + 2*se) %>%
  mutate(included = ifelse(mu_hat >= ymin & mu_hat <= ymax, TRUE, FALSE)) %>%
  ggplot(aes(x = reorder(movieId, avg_rating, FUN = mean),
             y = avg_rating, group = movieId,
             ymin = avg_rating - 2*se,
             ymax = avg_rating + 2*se,
             color = included)) +
  geom_point(show.legend = FALSE) +
  geom_errorbar(show.legend = FALSE) +
  scale_x_discrete(labels = NULL) +
  labs(x = "movieId", y = "avg_rating with error bars") +
  geom_hline(yintercept = mu_hat, color = "blue")

## User effect
user_infos %>%
  mutate(ymin = avg_rating - 2*se,
         ymax = avg_rating + 2*se) %>%
  mutate(included = ifelse(mu_hat >= ymin & mu_hat <= ymax, TRUE, FALSE)) %>%
  ggplot(aes(x = reorder(userId, avg_rating, FUN = mean),
             y = avg_rating,
             group = userId,
             ymin = avg_rating - 2*se,
             ymax = avg_rating + 2*se,
             color = included)) +
  geom_point(show.legend = FALSE) +
  geom_errorbar(show.legend = FALSE) +
  scale_x_discrete(labels = NULL) +
  labs(x = "userId", y = "avg_rating with error bars") +
  geom_hline(yintercept = mu_hat, color = "blue")

## Time effect
edx %>% mutate(date = as_datetime(timestamp)) %>%
  mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.2, method.args = list(degree=1), color = "red") +
  geom_hline(yintercept = mu_hat, color = "blue")

## Popularity effect
# Observe positive correlation between number of ratings and rating of movies
edx %>% group_by(movieId) %>%
  summarize(rating = mean(rating), nratings = round(n(), digits = -3)) %>%
  ungroup() %>%
  group_by(nratings) %>%
  summarize(se = sd(rating)/sqrt(n()), rating = mean(rating)) %>%
  ggplot(aes(x = nratings,
             y = rating,
             ymin = rating - 2*se,
             ymax = rating + 2*se)) +
  geom_point() +
  geom_errorbar() +
  geom_smooth(method = "lm")

# Observe that movies need time to accumulate more ratings
# Therefore consider rate of ratings per year instead of total number of ratings
edx %>%  group_by(movieId, year) %>%
  summarize(nratings = n()) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = nratings)) +
  geom_boxplot(aes(group = year)) +
  geom_smooth(method = "loess", span = 0.2, method.args = list(degree=1))+
  scale_y_continuous(trans = "sqrt") +
  scale_x_continuous(breaks = seq(min(edx$year), max(edx$year), 5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_vline(xintercept = year(min(as_datetime(edx$timestamp))), color = "red") +
  labs(y = "number of ratings per movie", x = "year of release of the movie")

# Observe correlation between popularity rate and rating
edx%>%
  group_by(movieId) %>%
  summarize(n = n(),
            years = max_year - year,
            rating = mean(rating)) %>%
  slice(1) %>%
  mutate(popularity_rate = n/years) %>%
  mutate(popularity_rate = round(popularity_rate)) %>%
  group_by(popularity_rate) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(popularity_rate, rating)) +
  geom_point(aes(popularity_rate, rating)) +
  geom_smooth(method="lm")

# The more popularity_rate, the more positive is our error
# Thus we over-modeled the Popularity effect already!
max_year <- max(year(as_datetime(edx$timestamp)))

#### WE NEED TO RUN THE MODEL WE HAVE SO FAR, CONTINUE BELOW ####
## Estimating Movie effect b_i_hat
movie_effect <- edx %>% group_by(movieId) %>%
  summarize(b_i_hat = mean(rating) - mu_hat)

## Estimating Movie effect b_i_hat and User effect b_u_hat using Regularization
lambdas <- seq(0, 10, 0.25)

# Tune parameter lambda via 1-fold cross-validation:
# Test set will be 30% of edx
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.3, list = FALSE)
train_set_cv <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set_cv <- temp %>% 
  semi_join(train_set_cv, by = "movieId") %>%
  semi_join(train_set_cv, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set_cv)
train_set_cv <- rbind(train_set_cv, removed)

# Calculate RMSEs for different lambdas
RMSEs <- sapply(lambdas, function(lambda){
  
  movie_effect <- train_set_cv %>% group_by(movieId) %>%
    summarize(b_i_hat = sum(rating - mu_hat)/(n()+lambda))
  
  user_effect <- train_set_cv %>%
    left_join(movie_effect, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u_hat = sum(rating - b_i_hat - mu_hat)/(n()+lambda))
  
  ratings_pred <- test_set_cv %>%
    left_join(movie_effect, by = "movieId") %>%
    left_join(user_effect, by = "userId") %>%
    mutate(pred = mu_hat + b_i_hat + b_u_hat) %>%
    pull(pred)
  
  return(RMSE(ratings_pred, test_set_cv$rating))
})

qplot(lambdas, RMSEs)

lambda <- lambdas[which.min(RMSEs)]

# Use tuned lamda to estimate Movie effect b_i_hat and User effect b_u_hat
movie_effect <- edx %>% group_by(movieId) %>%
  summarize(b_i_hat = sum(rating - mu_hat)/(n()+lambda))

user_effect <- edx %>%
  left_join(movie_effect, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u_hat = sum(rating - b_i_hat - mu_hat)/(n()+lambda))
#### CONTINUE HERE FOR POPULARITY EFFECT ####

edx_date <- edx %>% mutate(mu_hat = mu_hat) %>%
  left_join(movie_effect, by = "movieId") %>%
  left_join(user_effect, by = "userId") %>%
  mutate(date = as_datetime(timestamp)) %>%
  mutate(date = round_date(date, unit = "week")) %>%
  mutate(date = as.numeric(date))

edx_weeks <- edx_date %>%
  mutate(mu_u_i_hat = rating - mu_hat - b_u_hat - b_i_hat) %>%
  group_by(date) %>%
  summarize(mu_u_i_hat = mean(mu_u_i_hat)) %>%
  mutate(date = as.numeric(date))

# See how good the model until now fits the edx data
set.seed(1, sample.kind="Rounding")
grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)
time_effect_loess <- train(mu_u_i_hat ~ date, 
                           data = edx_weeks,
                           method = "gamLoess",
                           tuneGrid = grid)

edx_popularity_rate_diff <- edx %>% mutate(mu_hat = mu_hat) %>%
  left_join(movie_effect, by = "movieId") %>%
  left_join(user_effect, by = "userId") %>%
  mutate(mu_u_i_hat = predict(time_effect_loess, edx_date)) %>%
  mutate(y_hat = mu_hat + b_u_hat + b_i_hat + mu_u_i_hat) %>%
  mutate(y_hat = ifelse(y_hat < 0.5, 0.5, y_hat)) %>%
  mutate(y_hat = ifelse(y_hat > 5, 5, y_hat)) %>%
  group_by(movieId) %>%
  summarize(n = n(),
            years = max_year - year,
            y_hat = mean(y_hat),
            rating = mean(rating)) %>%
  slice(1) %>%
  mutate(popularity_rate = n/years) %>%
  mutate(popularity_rate = round(popularity_rate)) %>%
  group_by(popularity_rate) %>%
  summarize(rating_pred_diff = mean(y_hat - rating))

edx_popularity_rate_diff %>%
  ggplot(aes(popularity_rate, rating_pred_diff)) +
  geom_point(aes(popularity_rate, rating_pred_diff)) +
  geom_smooth(method = "lm")


## Genre effect
length(unique(edx$genres))

edx %>% group_by(genres) %>%
  summarize(n = n()) %>%
  filter(n >= 100000) %>%
  ungroup() %>%
  summarize(number_of_genres = n(), percent_covered = sum(n)/nrow(edx))

edx %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 100000) %>%
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) +
  geom_point() +
  geom_errorbar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



### FINAL-MODEL ###



load("rda/edx.rda")

# Overall rating average
mu_hat <- mean(edx$rating)

## Estimating Movie effect b_i_hat
movie_effect <- edx %>% group_by(movieId) %>%
  summarize(b_i_hat = mean(rating) - mu_hat)

## Estimating Movie effect b_i_hat and User effect b_u_hat using Regularization
lambdas <- seq(0, 10, 0.25)

# Tune parameter lambda via 1-fold cross-validation:
# Test set will be 30% of edx
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.3, list = FALSE)
train_set_cv <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set_cv <- temp %>% 
  semi_join(train_set_cv, by = "movieId") %>%
  semi_join(train_set_cv, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set_cv)
train_set_cv <- rbind(train_set_cv, removed)

# Calculate RMSEs for different lambdas
RMSEs <- sapply(lambdas, function(lambda){
  
  movie_effect <- train_set_cv %>% group_by(movieId) %>%
    summarize(b_i_hat = sum(rating - mu_hat)/(n()+lambda))
  
  user_effect <- train_set_cv %>%
    left_join(movie_effect, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u_hat = sum(rating - b_i_hat - mu_hat)/(n()+lambda))
  
  ratings_pred <- test_set_cv %>%
    left_join(movie_effect, by = "movieId") %>%
    left_join(user_effect, by = "userId") %>%
    mutate(pred = mu_hat + b_i_hat + b_u_hat) %>%
    pull(pred)
  
  return(RMSE(ratings_pred, test_set_cv$rating))
})

qplot(lambdas, RMSEs)

lambda <- lambdas[which.min(RMSEs)]

# Use tuned lamda to estimate Movie effect b_i_hat and User effect b_u_hat
movie_effect <- edx %>% group_by(movieId) %>%
  summarize(b_i_hat = sum(rating - mu_hat)/(n()+lambda))

user_effect <- edx %>%
  left_join(movie_effect, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u_hat = sum(rating - b_i_hat - mu_hat)/(n()+lambda))

## Estimating Time effect on overall average rating mu_u_i_hat
# Introduce variable "date" that is rounded to week of rating to apply loess
edx_weeks <- edx_date %>%
  mutate(mu_u_i_hat = rating - mu_hat - b_u_hat - b_i_hat) %>%
  group_by(date) %>%
  summarize(mu_u_i_hat = mean(mu_u_i_hat)) %>%
  mutate(date = as.numeric(date))

set.seed(1, sample.kind="Rounding")
grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)
time_effect_loess <- train(mu_u_i_hat ~ date, 
                           data = edx_weeks,
                           method = "gamLoess",
                           tuneGrid = grid)

## Estimate Popularity effect
# See how good the model until now fits the edx data
max_year <- max(year(as_datetime(edx$timestamp)))

edx_date <- edx %>% mutate(mu_hat = mu_hat) %>%
  left_join(movie_effect, by = "movieId") %>%
  left_join(user_effect, by = "userId") %>%
  mutate(date = as_datetime(timestamp)) %>%
  mutate(date = round_date(date, unit = "week")) %>%
  mutate(date = as.numeric(date))

edx_weeks <- edx_date %>%
  mutate(mu_u_i_hat = rating - mu_hat - b_u_hat - b_i_hat) %>%
  group_by(date) %>%
  summarize(mu_u_i_hat = mean(mu_u_i_hat)) %>%
  mutate(date = as.numeric(date))

set.seed(1, sample.kind="Rounding")
grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)
time_effect_loess <- train(mu_u_i_hat ~ date, 
                           data = edx_weeks,
                           method = "gamLoess",
                           tuneGrid = grid)

# Introduce variable "popularity rate"
edx_popularity_rate_diff <- edx %>% mutate(mu_hat = mu_hat) %>%
  left_join(movie_effect, by = "movieId") %>%
  left_join(user_effect, by = "userId") %>%
  mutate(mu_u_i_hat = predict(time_effect_loess, edx_date)) %>%
  mutate(y_hat = mu_hat + b_u_hat + b_i_hat + mu_u_i_hat) %>%
  mutate(y_hat = ifelse(y_hat < 0.5, 0.5, y_hat)) %>%
  mutate(y_hat = ifelse(y_hat > 5, 5, y_hat)) %>%
  group_by(movieId) %>%
  summarize(n = n(),
            years = max_year - year,
            y_hat = mean(y_hat),
            rating = mean(rating)) %>%
  slice(1) %>%
  mutate(popularity_rate = n/years) %>%
  mutate(popularity_rate = round(popularity_rate)) %>%
  group_by(popularity_rate) %>%
  summarize(rating_pred_diff = mean(y_hat - rating))

set.seed(1, sample.kind="Rounding")
popularity_effect_lm_correction <- train(rating_pred_diff ~ popularity_rate,
                                         data =edx_popularity_rate_diff,
                                         method ="lm")

## Genre effect using factorization

# ## define test set
# test_set <- edx
# ## preprocess test set to be able to apply the model
# test_set <- test_set %>% mutate(mu_hat = mu_hat) %>%
#   mutate(date = as_datetime(timestamp)) %>%
#   mutate(date = round_date(date, unit = "week")) %>%
#   mutate(date = as.numeric(date))
# test_pop_rate <- test_set %>% group_by(movieId) %>%
#   summarize(n = n(),
#             years = max_year - year,
#             rating = mean(rating)) %>%
#   slice(1) %>%
#   mutate(popularity_rate = round(n/years)) %>%
#   select(-n, -years, -rating)
# test_set <- test_set %>% left_join(test_pop_rate, by = "movieId")
# # Calculate residues
# edx_residue <- edx %>% mutate(mu_hat = mu_hat) %>%
#   left_join(movie_effect, by = "movieId") %>%
#   left_join(user_effect, by = "userId") %>%
#   mutate(mu_u_i_hat = predict(time_effect_loess, test_set)) %>%
#   mutate(b_p_i_hat = predict(popularity_effect_lm_correction, test_set)) %>%
#   mutate(y_hat = mu_hat + b_u_hat + b_i_hat + mu_u_i_hat - b_p_i_hat) %>%
#   mutate(y_hat = ifelse(y_hat < 0.5, 0.5, y_hat)) %>%
#   mutate(y_hat = ifelse(y_hat > 5, 5, y_hat)) %>%
#   mutate(residue = rating - y_hat) %>%
#   select(userId, movieId, residue)
# 
# # Save it in case it is needed later because of RAM crash
# save(edx_residue, file = "rda/edx_residue.rda")
# load("rda/edx_residue.rda")
# 
# # Increase upper memory limit to handle the following matrix
# memory.limit() # originally 15963
# memory.limit(size=15963*5)

#edx_residue <- edx_residue %>% spread(key = movieId, value = residue) %>%
#  as.matrix()

# Examine matrix to check if it looks correct
# edx_residue[1:10, 1:10][, -1]


# Replace NAs with zeros (not a good approximation, but a simple one)
# edx_residue[is.na(edx_residue)] <- 0

# We are not going to run a PCA since we do not need all principal components
# The computing time would be too long
# pca <- prcomp(edx_residue[, -1])

# We run a SVD instead to calculate
# residue = U*D*V^t = u_1*d_1*v*1 + u_2*d_2*v*2 + ...
# Do not run this. It takes too long:
#s <- svd(edx_residue[, -1], nu = 1, nv = 1)



### PREDICTIONS ###


load("rda/validation.rda")

## loss function to evaluate final product
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

## define test set
test_set <- validation

## preprocess test set to be able to apply the model
test_set <- test_set %>% mutate(mu_hat = mu_hat) %>%
  mutate(date = as_datetime(timestamp)) %>%
  mutate(date = round_date(date, unit = "week")) %>%
  mutate(date = as.numeric(date))
test_pop_rate <- test_set %>% group_by(movieId) %>%
  summarize(n = n(),
            years = max_year - year,
            rating = mean(rating)) %>%
  slice(1) %>%
  mutate(popularity_rate = round(n/years)) %>%
  select(-n, -years, -rating)
test_set <- test_set %>% left_join(test_pop_rate, by = "movieId")

## make predictions
predictions_all <- test_set %>% mutate(mu_hat = mu_hat) %>%
  left_join(movie_effect, by = "movieId") %>%
  left_join(user_effect, by = "userId") %>%
  mutate(mu_u_i_hat = predict(time_effect_loess, test_set)) %>%
  mutate(b_p_i_hat = predict(popularity_effect_lm_correction, test_set)) %>%
  mutate(y_hat = mu_hat + b_u_hat + b_i_hat + mu_u_i_hat - b_p_i_hat) %>%
  mutate(y_hat = ifelse(y_hat < 0.5, 0.5, y_hat)) %>%
  mutate(y_hat = ifelse(y_hat > 5, 5, y_hat))

predictions <- predictions_all %>% pull(y_hat)

## evaluate predictions
RMSE(predictions, test_set$rating)

save(predictions, file = "rda/predictions.rda")


### PERFORMANCE-EVALUATION


load("rda/predictions.rda")


# Define evaluation data frame
evaluation <- test_set %>%
  mutate(diff = predictions - rating) %>% # diff = predictions - rating
  select(movieId, userId, title, genres, year, diff, timestamp)

## Time effect
evaluation %>% mutate(date = as_datetime(timestamp)) %>%
  mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(diff = mean(diff)) %>%
  ggplot(aes(date, diff)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.2, method.args = list(degree=1), color = "red")


## Popularity effect
# Still present -> we corrected too much, we should have not corrected with b_p_i_hat
# RMSE without correction: 0.8647923 and with correction: 0.8649033
evaluation %>% mutate(popularity_rate = predictions_all$popularity_rate) %>%
  group_by(popularity_rate) %>%
  summarize(diff = mean(diff)) %>%
  ggplot(aes(popularity_rate, diff)) +
  geom_point(aes(popularity_rate, diff)) +
  geom_smooth(method = "lm")


# Examine diff and year correlation
# -> ratings for older movies are harder to predict
# -> older movies have less ratings and are therefore harder to predict
totals_year <- evaluation %>% group_by(year) %>%
  summarize(total_appearances = n())

evaluation %>% group_by(year) %>%
  summarize(diff = mean(diff)) %>%
  left_join(totals_year, by = "year") %>%
  ggplot(aes(year, diff, size = total_appearances)) +
  geom_point(alpha = 0.5) +
  scale_size(range = c(1, 4))

# Examine diff and genre correlation
evaluation %>% group_by(genres) %>%
  summarize(diff = mean(diff)) %>%
  arrange(desc(diff)) %>%
  ggplot(aes(genres, diff)) +
  geom_point() +
  scale_x_discrete(labels = NULL)

# -> genres with less ratings are harder to predict
totals_genres <- evaluation %>% group_by(genres) %>%
  summarize(total_appearances = n())
evaluation %>% group_by(genres) %>%
  summarize(diff = mean(diff)) %>%
  left_join(totals_genres, by = "genres") %>%
  ggplot(aes(total_appearances, diff)) +
  geom_point() +
  labs(x = "total appearances of genres")
evaluation %>% group_by(genres) %>%
  summarize(diff = mean(diff)) %>%
  left_join(totals_genres, by = "genres") %>%
  filter(total_appearances <= 100) %>%
  ggplot(aes(total_appearances, diff)) +
  geom_point() +
  labs(x = "total appearances of genres")
