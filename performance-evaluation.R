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
