# Create edx set, validation set (final hold-out test set). ----

# PLEASE NOTE: This Script follows RStudio's outline format do please use the menu on the right hand side of your window for easy navigation.

## Install and load required packagies and libraries ----
# Note: this process could take a couple of minutes

if (!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
if (!require(data.table))
  install.packages("data.table", repos = "http://cran.us.r-project.org")
if (!require(lubridate))
  install.packages("lubridate", repos = "http://cran.us.r-project.org")
if (!require(stringr))
  install.packages("stringr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(stringr)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip


## Download and convert dataset ----
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip",
              dl)

ratings <-
  fread(
    text = gsub("::", "\t", readLines(unzip(
      dl, "ml-10M100K/ratings.dat"
    ))),
    col.names = c("userId", "movieId", "rating", "timestamp")
  )

movies <-
  str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


## Data type transofrmations ----

# if using R 3.6 or earlier:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))

# if using R 4.0 or later:
movies <-
  as.data.frame(movies) %>% mutate(
    movieId = as.numeric(movieId),
    title = as.character(title),
    genres = as.character(genres)
  )

movielens <- left_join(ratings, movies, by = "movieId")


## Creating the validation dataset. It will only be used a the end of the script to test the model's performance. ----

### Validation set will be 10% of MovieLens data ----
set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <-
  createDataPartition(
    y = movielens$rating,
    times = 1,
    p = 0.1,
    list = FALSE
  )
edx <- movielens[-test_index, ]
temp <- movielens[test_index, ]

### Make sure userId and movieId in validation set are also in edx set ----
validation <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

### Add rows removed from validation set back into edx set and remove unnecessary variables ----
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# save and reload data files

## save(edx, file = "rdata/edx.rda")
## save(validation, file = "rdata/validation.rda")

load("rdata/edx.rda")
load("rdata/validation.rda")

### Set number of digits ----
options(digits = 7)

### Extract Year of Release and Year of Rating from timestamp ----
edx <-
  edx %>% mutate(
    release_year = as.numeric(str_sub(
      title, start = -5, end = -2
    )),
    year_rated = year(as_datetime(timestamp)),
    month_rated = month(as_datetime(timestamp), label = TRUE)
  )

validation <-
  validation %>% mutate(
    release_year = as.numeric(str_sub(
      title, start = -5, end = -2
    )),
    year_rated = year(as_datetime(timestamp)),
    month_rated = month(as_datetime(timestamp), label = TRUE)
  )



# Exploratory Analysis ----

## Structure review ----
glimpse(edx)


## Calculate edx average rating ----
edx_mu <- mean(edx$rating)


## Exploratory Plots ----

### Ratings Distribution Histogram in the edx dataset ----
edx %>% ggplot(aes(rating)) +
  geom_histogram(bins = 10, color = I("black")) +
  geom_vline(xintercept = edx_mu,
             linetype = "dashed",
             colour = "blue") +
  ggtitle("Ratings Distribution") +
  ylab("Count") +
  xlab("Rating") +
  ggtitle("Ratings Distribution")

### Average rating by user in the edx dataset ----
edx %>% group_by(userId) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(mean_rating)) +
  geom_histogram(bins = 50, color = I("black")) +
  labs(x = "Mean rating", y = "Number of users") +
  ggtitle("Mean rating by user")

### Release Year Average Rating Curve ----
edx %>%
  group_by(release_year) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(release_year, mean_rating)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess",
              level = 0.95,
              colour = "blue") +
  ggtitle("Mean Rating by Year of Release") +
  ylab("Mean Rating") +
  xlab("Release Year")

### Rating Year Average Rating Curve ----
edx %>%
  group_by(year_rated) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(year_rated, mean_rating)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess",
              level = 0.95,
              colour = "blue") +
  ggtitle("Mean Rating by Year of Release") +
  ylab("Mean Rating") +
  xlab("Year of Review")

### Movie genre effect on user rating ----

# My laptop couldn't handle this operation over the entire dataset. I've sampled 1M rows instead.
set.seed(1, sample.kind = "Rounding")
edx_genre <- edx %>%
  slice(sample(1:nrow(edx), 1000000)) %>%
  separate_rows(genres, sep = "\\|")


edx_genre %>% group_by(genres) %>%
  summarize(
    count = n(),
    mean_rating = mean(rating),
    se = sd(rating) / sqrt(n())
  ) %>%
  filter(count > 10000) %>%
  mutate(genres = reorder(genres, mean_rating)) %>%
  ggplot(aes(
    x = genres,
    y = mean_rating,
    ymin = mean_rating - 2 * se,
    ymax = mean_rating + 2 * se
  )) +
  geom_point(fill = "blue", colour = "blue") +
  geom_errorbar(colour = "darkgrey") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Mean Rating by Movie Genre")



# Methodology ----

## Set Netflix Target RMSE ----
netflix_objective <- 0.86490

## Partition edx into train and test sets ----

### Create train set and test sets from edx ----
set.seed(1, sample.kind = "Rounding")
edx_test_index <-
  createDataPartition(
    y = edx$rating,
    times = 1,
    p = 0.1,
    list = FALSE
  )
edx_train <- edx[-edx_test_index,]
temp <- edx[edx_test_index,]

### Make sure userId and movieId in test set are also in train set ----
edx_test <- temp %>%
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

### Add rows removed from test set back into train set ----
removed <- anti_join(temp, edx_test)
edx_train <- rbind(edx_train, removed)

### Remove temporary files to tidy environment ----
rm(edx_test_index, temp, removed)


## Define a loess function (RMSE) ----
RMSE <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings) ^ 2))
}


## Construct the most basic model - Just the average ----

### Calculate the edx_train set average ----
edx_train_mu <- mean(edx_train$rating)

### Measure the RMSE of the edx_train average vs the edx_test dataset
RMSE_avg <- RMSE(edx_test$rating, edx_train_mu)

### Add the training_results to a dataframe ----
training_results <-
  data.frame(Method = "Netflix Objective", RMSE = netflix_objective) %>%
  rbind(c("Just the Average", round(RMSE_avg, 5)))



## Calculate movie effect (b_m) ----

### movie effect ----
movie_b_m <- edx_train %>%
  group_by(movieId) %>%
  summarise(b_m = mean(rating - edx_train_mu))

### Predict ratings - adjust for movie effects ----
predicted_b_m <- edx_train_mu + edx_test %>%
  left_join(movie_b_m, by = "movieId") %>%
  pull(b_m)

### ensure the predictions don't exceed the rating limits ----
predicted_b_m[predicted_b_m < 0.5] <- 0.5
predicted_b_m[predicted_b_m > 5] <- 5

### Calculate RMSE based on movie effects ----
RMSE_b_m <- RMSE(predicted_b_m, edx_test$rating)

### Append training_results to dataframe ----
training_results <-
  rbind(training_results,
        data.frame(Method = "Movie Effect",
                   RMSE = RMSE_b_m))

training_results %>% knitr::kable()

### Plot movie effects distribution ----
movie_b_m %>%
  ggplot(aes(b_m)) +
  geom_histogram(bins = 10, color = I("black")) +
  labs(x = "Movie effects (b_m)") +
  ggtitle("Movie Effect Distribution")


## Calculate user effect (b_u) ----

### user effect ----
user_b_u <- edx_train %>%
  left_join(movie_b_m, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - edx_train_mu - b_m))

### Predict ratings - adjust for movie and user effects ----
predicted_b_u <- edx_test %>%
  left_join(movie_b_m, by = "movieId") %>%
  left_join(user_b_u, by = "userId") %>%
  mutate(pred = edx_train_mu + b_m + b_u) %>%
  pull(pred)

### ensure the predictions don't exceed the rating limits ----
predicted_b_u[predicted_b_u < 0.5] <- 0.5
predicted_b_u[predicted_b_u > 5] <- 5

### Calculate RMSE based on user effects ----
RMSE_b_u <- RMSE(predicted_b_u, edx_test$rating)

### Append training_results to dataframe ----
training_results <-
  rbind(training_results,
        data.frame(Method = "Users Effect",
                   RMSE = RMSE_b_u))

training_results %>% knitr::kable()

### User Effect Distribution ----
user_b_u %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 10, color = I("black")) +
  labs(x = "User effects (b_u)") +
  ggtitle("User Effect Distribution")



## Calculate genre effect (b_g) ----

### genre effect ----
genre_b_g <- edx_train %>%
  left_join(movie_b_m, by = "movieId") %>%
  left_join(user_b_u, by = "userId") %>%
  group_by(genres) %>%
  summarise(b_g = mean(rating - edx_train_mu - b_m - b_u))

### Predict ratings - adjust for movie, user and genre effects ----
predicted_b_g <- edx_test %>%
  left_join(movie_b_m, by = "movieId") %>%
  left_join(user_b_u, by = "userId") %>%
  left_join(genre_b_g, by = "genres") %>%
  mutate(pred = edx_train_mu + b_m + b_u + b_g) %>%
  pull(pred)

### ensure the predictions don't exceed the rating limits ----
predicted_b_g[predicted_b_g < 0.5] <- 0.5
predicted_b_g[predicted_b_g > 5] <- 5

### Calculate RMSE based on genre effects ----
RMSE_b_g <- RMSE(predicted_b_g, edx_test$rating)

### Append training_results to dataframe ----
training_results <-
  rbind(training_results,
        data.frame(Method = "Genre Effect",
                   RMSE = RMSE_b_g))

training_results %>% knitr::kable()

### Genre Effect Distribution ----
genre_b_g %>%
  ggplot(aes(b_g)) +
  geom_histogram(bins = 10, color = I("black")) +
  labs(x = "Genre effects (b_g)") +
  ggtitle("Genre Effect Distribution")


## Calculate year of release effect (b_y) ----

### release year effect ----
year_b_y <- edx_train %>%
  left_join(movie_b_m, by = "movieId") %>%
  left_join(user_b_u, by = "userId") %>%
  left_join(genre_b_g, by = "genres") %>%
  group_by(release_year) %>%
  summarise(b_y = mean(rating - edx_train_mu - b_m - b_u - b_g))

### Predict ratings - adjust for movie, user, genre and year effects ----
predicted_b_y <- edx_test %>%
  left_join(movie_b_m, by = "movieId") %>%
  left_join(user_b_u, by = "userId") %>%
  left_join(genre_b_g, by = "genres") %>%
  left_join(year_b_y, by = "release_year") %>%
  mutate(pred = edx_train_mu + b_m + b_u + b_g + b_y) %>%
  pull(pred)

### ensure the predictions don't exceed the rating limits ----
predicted_b_y[predicted_b_y < 0.5] <- 0.5
predicted_b_y[predicted_b_y > 5] <- 5

### Calculate RMSE based on year effects ----
RMSE_b_y <- RMSE(predicted_b_y, edx_test$rating)

### Append training_results to dataframe ----
training_results <-
  rbind(training_results,
        data.frame(Method = "Release Year Effect",
                   RMSE = RMSE_b_y))

training_results %>% knitr::kable()

### Release Year Effect Distribution ----
year_b_y %>%
  ggplot(aes(b_y)) +
  geom_histogram(bins = 10, color = I("black")) +
  labs(x = "Release Year effects (b_y)") +
  ggtitle("Release Year Effect Distribution")


## Calculate year rated effect (b_r) ----

### year rated effect ----
year_b_r <- edx_train %>%
  left_join(movie_b_m, by = "movieId") %>%
  left_join(user_b_u, by = "userId") %>%
  left_join(genre_b_g, by = "genres") %>%
  left_join(year_b_y, by = "release_year") %>%
  group_by(year_rated) %>%
  summarise(b_r = mean(rating - edx_train_mu - b_m - b_u - b_g - b_y))

### Predict ratings - adjust for movie, user, genre, year and review date effects ----
predicted_b_r <- edx_test %>%
  left_join(movie_b_m, by = "movieId") %>%
  left_join(user_b_u, by = "userId") %>%
  left_join(genre_b_g, by = "genres") %>%
  left_join(year_b_y, by = "release_year") %>%
  left_join(year_b_r, by = "year_rated") %>%
  mutate(pred = edx_train_mu + b_m + b_u + b_g + b_y + b_r) %>%
  pull(pred)

### ensure the predictions don't exceed the rating limits ----
predicted_b_r[predicted_b_r < 0.5] <- 0.5
predicted_b_r[predicted_b_r > 5] <- 5

### Calculate RMSE based on review date effects model ----
RMSE_b_r <- RMSE(predicted_b_r, edx_test$rating)

### Append training_results to dataframe ----
training_results <-
  rbind(training_results,
        data.frame(Method = "Year Rated Effect",
                   RMSE = RMSE_b_r))

training_results %>% knitr::kable()

### Year Rated Effect Distribution ----
year_b_r %>%
  ggplot(aes(b_r)) +
  geom_histogram(bins = 10, color = I("black")) +
  labs(x = "Review year effects (b_r)") +
  ggtitle("Review Year Effect Distribution")



# Regularisation ----

## Sequence of lambdas ranging from 1 to 3 with 0.1 increments ----
lambdas <- seq(4, 6, 0.1)


## Programmatically regularise model, predict ratings and calculate RMSE for each value of lambda ----

# This took a couple of minutes on a laptop with 8GB RAM.
rmses <- sapply(lambdas, function(l) {
  b_m <- edx_train %>%
    group_by(movieId) %>%
    summarise(b_m = sum(rating - edx_train_mu) / (n() + l))
  b_u <- edx_train %>%
    left_join(b_m, by = "movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_m - edx_train_mu) / (n() + l))
  b_g <- edx_train %>%
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarise(b_g = sum(rating - b_m - b_u - edx_train_mu) / (n() + l))
  b_y <- edx_train %>%
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    group_by(release_year) %>%
    summarise(b_y = sum(rating - b_m - b_u - b_g - edx_train_mu) / (n() +
                                                                      l))
  b_r <- edx_train %>%
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_y, by = "release_year") %>%
    group_by(year_rated) %>%
    summarise(b_r = sum(rating - b_m - b_u - b_g - edx_train_mu) / (n() +
                                                                      l))
  predicted_ratings <- edx_test %>%
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_y, by = "release_year") %>%
    left_join(b_r, by = "year_rated") %>%
    mutate(pred = edx_train_mu + b_m + b_u + b_g + b_y + b_r) %>%
    pull(pred)
  return(RMSE(predicted_ratings, edx_test$rating))
})

### Choose and visualise optimal lambda ----
lambda <- lambdas[which.min(rmses)]

data.frame(lambdas, rmses) %>%
  ggplot(aes(lambdas, rmses)) +
  geom_point() +
  geom_hline(yintercept = min(rmses),
             linetype = 'dotted',
             col = "red") +
  annotate(
    "text",
    x = lambda,
    y = min(rmses),
    label = lambda,
    vjust = -1,
    color = "red"
  ) +
  labs(x = "Lambda", y = "RMSE")

### Minimum RMSE achieved ----
RMSE_reg <- min(rmses)

### Append training_results to dataframe ----
training_results <-
  rbind(training_results,
        data.frame(Method = "Regularised RMSE",
                   RMSE = RMSE_reg))

training_results %>% knitr::kable()



# Final Modelling ----

## Model all effects with the full edx dataset, regularised with optimal lambda ----
b_m <- edx %>%
  group_by(movieId) %>%
  summarise(b_m = sum(rating - edx_mu) / (n() + lambda))

b_u <- edx %>%
  left_join(b_m, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - b_m - edx_mu) / (n() + lambda))

b_g <- edx %>%
  left_join(b_m, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  group_by(genres) %>%
  summarise(b_g = sum(rating - b_m - b_u - edx_mu) / (n() + lambda))

b_y <- edx %>%
  left_join(b_m, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  group_by(release_year) %>%
  summarise(b_y = sum(rating - b_m - b_u - b_g - edx_mu) / (n() + lambda))

b_r <- edx %>%
  left_join(b_m, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_y, by = "release_year") %>%
  group_by(year_rated) %>%
  summarise(b_r = sum(rating - b_m - b_u - b_g - b_y - edx_mu) / (n() +
                                                                    lambda))

## Predict ratings in validation set using final model ----
predicted_ratings <- validation %>%
  left_join(b_m, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_y, by = "release_year") %>%
  left_join(b_r, by = "year_rated") %>%
  mutate(pred = edx_mu + b_m + b_u + b_g + b_y + b_r) %>%
  pull(pred)



# Final validation of RMSE using the validation data set ----

## Calculate final RMSE against the validation set ----
RMSE_validated <- RMSE(validation$rating, predicted_ratings)


## Results Comparison ----

data.frame(Method = "Just the Average",
           RMSE = RMSE_avg,
           Difference = "-") %>% rbind(c(
             "RMSE Validated",
             round(RMSE_validated, 5),
             format(round(RMSE_validated - RMSE_avg, 5), scientific = F)
           ))

data.frame(Method = "Netflix Objective",
           RMSE = "0.86490",
           Difference = "-") %>% rbind(c(
             "RMSE Validated",
             round(RMSE_validated, 5),
             format(round(RMSE_validated - netflix_objective, 5), scientific = F)
           ))

# End of Script
