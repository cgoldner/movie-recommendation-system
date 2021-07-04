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
  
