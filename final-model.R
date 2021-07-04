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


