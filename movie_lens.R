# Create edx set, validation set (final hold-out test set). ----

# This Script follows RStudio's outline format do please use the menu on the right hand side of your window for easy navigation.

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
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

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

## Top 10 movies based on overall ratings ----

edx %>% group_by(title) %>%
  summarise(n = n()) %>%
  slice_max(n, n = 10) %>%
  knitr::kable()

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
edx_train <- edx[-edx_test_index, ]
temp <- edx[edx_test_index, ]

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
RMSE_1 <-RMSE(edx_test$rating, edx_train_mu)

### Add the results to a dataframe ----
results <- tibble(Method = "Just the Average", RMSE = RMSE_1)



