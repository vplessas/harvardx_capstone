

###Online Store Sales Forecasting - HarvardX Capstone Project###

#Prepare Environment ----

##Install and load necessary libraries ----

if (!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(fpp3))
  install.packages("fpp3", repos = "http://cran.us.r-project.org")
if (!require(knitr))
  install.packages("knitr", repos = "http://cran.us.r-project.org")
if (!require(readxl))
  install.packages("readxl", repos = "http://cran.us.r-project.org")
if (!require(skimr))
  install.packages("skimr", repos = "http://cran.us.r-project.org")
if (!require(distributional))
  install.packages("distributional", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(fpp3) # Loads the Tidyverts group of packages
library(knitr)
library(readxl)
library(skimr)
library(distributional)

options(dplyr.summarise.inform = FALSE)


## Pull data from UCI Machine Learning Repository ----

url<- "https://archive.ics.uci.edu/ml/machine-learning-databases/00352/Online%20Retail.xlsx"
tmp <- tempfile(fileext = ".xlsx")
download.file(url, destfile = tmp, mode="wb")

online_store<- read_xlsx(tmp)



# Exploratory Analysis ----

## First pass ----

skim(online_store)


# We have examined the bottom 10 prices in the dataset. As expected the large negative values observed earlier are present here and they are not related to products, rather financial adjustments. There's also a number of products with zero prices, meaning they have contributed to the company's revenue.

online_store %>%
slice_min(na.omit(UnitPrice),n=10, with_ties = F)


#Similarly, for the top 10 prices , we can see that they correspond to customer fees and instruction manuals which are not considered as products.


online_store %>%
slice_max(na.omit(UnitPrice),n=10, with_ties = F)


# Another important observation histogram that product codes (StockCode) appear to differentiate a lot, both in their formats and character length. Is there potentially a pattern that the majority of product codes follow? Figure 1, shows that the vast majority of codes follow a five character format, with a few codes being shorter and a few more longer than that.

online_store %>%
  mutate(length = str_length(StockCode)) %>%
  ggplot(aes(length))+
  geom_bar()


#We have first looked at the shorter length code and examine what products do they refer to. As expected they are not referring to actual products that can be traded.

online_store %>%
  mutate(length = str_length(StockCode)) %>%
  filter(length < 5) %>%
  distinct(StockCode, Description)


#However, product codes of length >5 are actual products whose code has a letter suffix. This could potentially indicated that they are part of an assortment or they are variations of a "parent" project.

online_store %>%
  mutate(length = str_length(StockCode)) %>%
  filter(length > 5) %>%
  distinct(StockCode, Description) %>% head(10)


#Similarly, if we filter on the top 10 products based on Quantity. We can identify the large value observed earlier. 

online_store %>%
  slice_max(na.omit(Quantity), n=10)


# At this point we can start cleaning up some of these inconsistencies and review again what our data look like. Specifically we have filtered out any entries with prices less than zero. Entries with negative quantities have also been removed as they in general refer to product returns, stock adjustments or damaged stock. We are interested in forecasting the volume of stock that the business will need to deliver to customers so negative quantities need to be factored out.
# Additionally, we have only kept entries whose product codes are of character length 5 or 6. That way our dataset will consists of actual products.

online_store<- online_store %>%
  filter(!InvoiceNo %in% c("581483", "541431"))


online_store<- online_store %>%
  filter(Quantity > 0 , UnitPrice > 0, str_length(StockCode) %in% c(5,6))


## Second Pass ----

#The new skimr output is a lot cleaner and something we can move into the next phase of our analysis.

skim(online_store)


# Final check for any remaining non numeric codes

online_store %>%
  mutate(fivedig = str_sub(StockCode, 1,5)) %>%
  filter(is.na(as.numeric(fivedig))) %>%
  distinct(StockCode, Description)


## Performance by Country ----

online_store %>%
  group_by(Country) %>%
  summarise(revenue = sum(Quantity * UnitPrice)) %>%
  slice_max(na.omit(revenue), n=10) %>%
  ggplot(aes(reorder(Country, -revenue), revenue, fill = Country)) +
  geom_col()+
  theme(
    axis.text.x = element_text(
      size =  8,
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    legend.position = "none")+
  labs(x = "Country") +
  ggtitle("Revenue for Top 10 Countries")


## Filter on the United Kingdom ---- 
online_store <- 
  online_store %>%
  filter(Country == "United Kingdom") %>%
  mutate(
    Quanity = as.integer(Quantity),
    Revenue = Quantity * UnitPrice,
    InvoiceDate = as_date(InvoiceDate),
    product_code = str_sub(StockCode, 1,5)
    ) %>%
  group_by(product_code, InvoiceDate, InvoiceNo) %>%
  summarise(Quantity = sum(Quantity), Revenue = sum(Revenue)) %>%
  ungroup()
  

## Overall trend ----

online_store %>%
  group_by(InvoiceDate) %>%
  summarise(Quantity = sum(Quantity)) %>%
  ungroup() %>%
  ggplot(aes(InvoiceDate, Quantity)) +
  geom_point() +
  geom_smooth(method = "lm", se= FALSE) +
  labs(y = "Units Sold") +
  scale_x_date(date_labels = "%b %y", date_breaks  = "1 month") +
  ggtitle("Sales Trend in Units") +
  theme(
    axis.text.x = element_text(
      size =  8,
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(colour = "#d4dddd"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )


## Product Grouping ----

### Assign_categories ----

ab_hl<- online_store %>%
  group_by(product_code) %>%
  summarise(Revenue = sum(Revenue),
            Orders = n()) %>%
  ungroup() %>%
  arrange(desc(Revenue)) %>%
  mutate(proportions_rev = Revenue/sum(Revenue), cumulative_rev = cumsum(proportions_rev), 
         ab = case_when( 
           cumulative_rev <= 0.8 ~ "A",
           TRUE ~"B")) %>%
  arrange(desc(Orders)) %>%
  mutate(proportions_ord = Orders/sum(Orders), cumulative_ord = cumsum(proportions_ord), 
         hl = case_when( 
           cumulative_ord <= 0.8 ~ "H",
           TRUE ~"L")) %>%
  select(-Revenue, -Orders, -proportions_rev, -cumulative_rev, -proportions_ord, -cumulative_ord)

### two by two table ----
table(ab_hl$ab, ab_hl$hl) %>% kable()


### Grouped Time Seies ----
online_store %>%
  left_join(ab_hl, by = "product_code") %>%
  mutate(ab_hl = paste0(ab,hl)) %>%
  group_by(ab_hl, InvoiceDate) %>%
  summarise(Quantity = sum(Quantity)) %>%
  ungroup() %>%
  ggplot(aes(InvoiceDate, Quantity, colour = ab_hl)) +
  geom_line() +
  facet_grid(ab_hl~., scales = "free_y")+
  labs(y = "Units Sold") +
  scale_x_date(date_labels = "%b %y", date_breaks  = "1 month") +
  ggtitle("Units Sold Online by Product Group") +
  theme(
    axis.text.x = element_text(
      size =  8,
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(colour = "#d4dddd"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )


### Final Dataset ----
online_store <- online_store %>%
  left_join(ab_hl, by = "product_code") %>%
  mutate(ab_hl = paste0(ab,hl)) %>%
  filter(ab_hl == "AH") %>%
  group_by(ab_hl, InvoiceDate) %>%
  summarise(Quantity = sum(Quantity)) %>%
  ungroup()

online_store %>% glimpse()


# Time Series Analysis ----

#For the following part of out this report we will be using the [tidyverts](https://tidyverts.org/) packages

## Convert into a tsibble object ----

online_store_tsbl<- online_store %>%
  as_tsibble(key = ab_hl, index = InvoiceDate) %>%
  filter_index("2011") %>%
  group_by_key() %>%
  summarise(Quantity = sum(Quantity)) %>%
  ungroup()

online_store_tsbl %>% head()


# The feasts package contains a very useful plotting function [autoplot()](https://fabletools.tidyverts.org/reference/autoplot.tbl_ts.html) which allows us to easily produce time-series plots from a tibble with minimum code. We have used this function across the rest of our analysis.

## Plot Time Series ----
online_store_tsbl %>%
  autoplot() +
  geom_smooth(method = "lm", se = FALSE)


## Missing Values ----

### identify_gaps ----

online_store_tsbl %>%
  has_gaps(.full = TRUE)


### Distribution of gaps in our tsibble. ----

online_store_tsbl %>%
count_gaps(.full = TRUE) %>% 
  ggplot(aes(.n)) +
  scale_x_log10()+
  geom_histogram(bins = 20)


### fill gaps ----
online_store_gaps<-
  online_store_tsbl %>%
  group_by_key() %>%
  fill_gaps() %>%
  tidyr::fill(Quantity, .direction = "down") %>%
  ungroup()

### Chacking for any remaining gaps ----
online_store_gaps %>% has_gaps(.full = TRUE) %>% kable()


## Outliers ----


### Fitting an STL model ----

decomposition<- online_store_gaps %>%
  model(
    stl = STL(Quantity ~ season(period =1), robust = TRUE)
  ) %>%
  components()

decomposition %>% autoplot()



# We have used a rule to define outliers as those that are greater than 3 interquartile ranges (IQRs) from the central 50% of the data, which would make only 1 in 500,000 normally distributed observations to be outliers. Below table show the output of this:

outliers <-
  decomposition %>%
  filter(
    remainder < quantile(remainder, 0.25) - 3* IQR(remainder) |
      remainder > quantile(remainder, 0.75) + 3* IQR(remainder)
  )

outliers


# As a final step, we have removed those outliers from the dataset and have used an Auto Regressive Integrated Moving Average (ARIMA) model from the fable package to replace them with interpolation.

### Remove Outliers ----
online_store_outliers <-
  online_store_gaps %>%
  anti_join(outliers) %>%
  fill_gaps()

### Fill with Interpolation ----

online_store_clean <-
  online_store_outliers %>%
  model(ARIMA(Quantity)) %>%
  interpolate(online_store_outliers)



# Time Series Features ----

## Plot Normalised Time Series ----

online_store_clean %>%
  autoplot() +
  geom_smooth(method = "lm", se = FALSE)


## Lag Plots ----

lagged_sales <-
  online_store_clean %>%
  filter(month(InvoiceDate) > 1)

lagged_sales %>%
  gg_lag(Quantity, geom = "point")+
  labs(x = "lag(Quantity, k)")



## Autocorrelation ----

# Correlogram short
online_store_clean %>%
  ACF(Quantity, lag_max = 28) %>%
  autoplot()


# For data with trend like ours it's useful to increase the number of estimated lags. When data have a trend, the autocorrelations for small lags tend to be large and positive because observations nearby in time are also nearby in value. So the ACF of a trended time series tends to have positive values that slowly decrease as the lags increase.

#Correlogram long
online_store_clean %>%
  ACF(Quantity, lag_max = 84) %>%
  autoplot()


## STL Decomposition ----

# We use the feat_stl() function from the feasts function to extract those features for one or multiple time series in our tsibble. Below we see the values for the trend and seasonal and trend strength of our time series as well as the peak and trough values in the season. We can see that within a week our series tends to peak on the 3rd day (Wednesday) and falls on the 6th (Saturday.)

# Extract features
online_store_clean %>% features(Quantity, feat_stl)


#Decomposition model and plot ----
online_store_clean %>%
  model(
    stl = STL(Quantity ~ trend(window = 7) + season(window = "periodic"))
  ) %>%
  components() %>% autoplot()
  

#Correctly identifying the components tha make our time series is an important step to produce good forecast by selecting the appropriate model which fits our data best.


# Forecasting Methodology ----

## Train and Test sets

# It is best practice to split our time series between train and test data sets. As we are testing out models to select the most appropriate one for our data, we train those models in the train data and we evaluate the performance on the test set which contains data that have not been used in the training process. That way we can avoid over-fitting and ensure our model generalises good enough to predict outcomes on new data.
# 
# For our methodology, we have split our data between the online_store_fit and online_store_validate (28 days) datasets. We have further split the online_store_fit set between the online_store_train and online_store_test (28 days) datasets. 
# 
# We have then fitted multiple models on the online_store_train set and have measured their accuracy on the online_store_test dataset by producing one-step forecasts using the [refit()](https://fable.tidyverts.org/reference/refit.ETS.html). It is worth noting that each model tested is not re-estimated in this case. Instead, the model obtained previously (and stored as method_fit) is applied to the test data. Because the model was not re-estimated, the “residuals” obtained here are actually one-step forecast errors.
# 
# Finally we refited all the trained models on the entire online_store_fit dataset and produced one-step forecast on teh online_store_validate dataset which until now had not been used. The model that yielded the smaller error was then selected to produce multiple step forecasts (or true forecasts) using the entire dataset and measure it's performance on the last 28 days of that set.
# 
# In order to ensure we have not over-fitted our model, we have used time series bootstrapping to generate 100 time series of bootstrap block size 28. We've then created simulated forecasts from those series using the winning model from our previous step and measured our accuracy again over the online_store_validate datasets and compared with our previous result.


online_store_fit <- online_store_clean %>%
  filter(InvoiceDate <= (max(InvoiceDate)-28))

online_store_validate <- online_store_clean %>%
  filter(InvoiceDate > (max(InvoiceDate)-28))

online_store_train <- online_store_fit %>%
  filter(InvoiceDate <= (max(InvoiceDate)-28))

online_store_test <- online_store_fit %>%
  filter(InvoiceDate > (max(InvoiceDate)-28))


## Forecasting Methods

## NAIVE ----

# One of the simplest forecasting techniques avaialable is the Naive method. For naïve forecasts, we simply set all forecasts to be the value of the last observation. This method is usually a good benchmark for other methods, or simply put if a more advanced method cannot beat the Naive's accuracy score with we might as well use the Naive one as our best estimation for demand.


### Fit Seasonal Naive ----
fit_naive<- online_store_train %>%
  model(snaive = SNAIVE(Quantity ~ lag("week")))

### Create Forecasts ----
fc_naive<- fit_naive %>%
  forecast(h = 28) 

### Plot Forecasts ----
fc_naive %>%
  autoplot(online_store_train %>% filter(InvoiceDate >= "2011-07-01"))

### Evaluate on the test dataset ----
fit_naive %>% refit(online_store_test) %>%
  accuracy() %>% 
  select(ab_hl, .model, MAPE)


## Linear Regression ----

# A Linear Regression model in its simplest case, allows for a linear relationship between the forecast variable y and a single predictor variable x
# We will use the trend and seasonality variables we've identified earlier as predictor variables in the linear regression function.

### Fit Linear Model ----
fit_lm <- online_store_train %>%
  model(
    lm = TSLM(Quantity ~ trend() + season())
  )

### Generate Forecasts ----
fc_lm <- fit_lm %>%
  forecast(h = 28)

### Plot Forecasts ----

fc_lm %>% autoplot(online_store_train %>% filter(InvoiceDate >= "2011-07-01"))

### Evaluate on the test dataset ----

fit_lm %>% refit(online_store_test) %>%
  accuracy() %>% 
  select(ab_hl, .model, MAPE)


## Exponential Smoothing ----

# Forecasts produced using exponential smoothing methods are weighted averages of past observations, with the weights decaying exponentially as the observations get older. This framework generates reliable forecasts quickly and for a wide range of time series, which is a great advantage and of major importance to applications in industry.

### Simple Exponential Smoothing ----

# We fit a Simple Exponential Smoothing to our train data using the ETL function from fable package and by passing the parameters error("A") + trend("N") + season("N"). "A" stands for additive, meaning the residual errors get added as the algorithm progresses over the time series, and "N" simply cancels out the algorithms fucntion to search for trend and seasonality in the data.

#### Fit SES ----
fit_ses <- online_store_train %>%
  model(
    ets = ETS(Quantity ~ error("A") + trend("N") + season("N"))
  )

#### Generate Forecasts ----
fc_ses <- fit_ses %>%
  forecast(h = 28)


#### Visualise Forecasts ----
fc_ses %>% autoplot(online_store_train %>% filter(InvoiceDate >= "2011-07-01"))


#Given both the trend and seasonal patterns in our data, it comes as no surprise that we don't have a good MAPE result in this case.
#### Evanualte Forecasts ----
fit_ses %>% refit(online_store_test) %>%
  accuracy() %>% 
  select(ab_hl, .model, MAPE)


### Holt's Exponential Smoothing ----

#### Fit HES ----
fit_hes <- online_store_train %>%
  model(
    ets = ETS(Quantity ~ error("A") + trend("A") + season("N"))
  )

#### Generate Forecasts ----
fc_hes <- fit_hes %>%
  forecast(h = 28)

#### Plot Forecasts ----
fc_hes %>% autoplot(online_store_train %>% filter(InvoiceDate >= "2011-07-01"))

#### Evaluate on the test dataset ----
fit_hes %>% refit(online_store_test) %>%
  accuracy() %>% 
  select(ab_hl, .model, MAPE)


### Holt Winter's Exponential Smoothing ----

# There are two variations to this method that differ in the nature of the seasonal component. The additive method is preferred when the seasonal variations are roughly constant through the series, while the multiplicative method is preferred when the seasonal variations are changing proportional to the level of the series. 
# A method that often provides accurate and robust forecasts for seasonal data is the Holt-Winters method with a damped trend and multiplicative seasonality.
  
#### Fit HWES ----
fit_hwes <- online_store_train %>%
  model(
    ets_additive = ETS(Quantity ~ error("A") + trend("A") + season("A")),
    ets_multiplicative = ETS(Quantity ~ error("M") + trend("A") + season("M")),
    ets_damped = ETS(Quantity ~ error("M") + trend("Ad") + season("M"))
  )

#### Generate Forecasts ----
fc_hwes <- fit_hwes %>%
  forecast(h = 28)

#### Plot Forecasts ----
fc_hwes %>% autoplot(online_store_train %>% filter(InvoiceDate >= "2011-07-01"), level = NULL)

#### Evaluate Forecasts ----
fit_hwes %>% refit(online_store_test) %>%
  accuracy() %>% 
  select(ab_hl, .model, MAPE)


# Validation ----

# At this point we feel that we have achieved some good enough scores and we have proceeded into fitting all above tuned methods against the entire online_store_fit data set. Note that we have yet to use the online_store_validate dataset in our testing , which we'll do now using multi step forecasts and compare all methods applied thus far.


## Fit all tuned models over the entire online_store_fit  dataset ----

fit <- online_store_clean %>%
  filter(InvoiceDate <= (max(InvoiceDate)-28)) %>%
  model(
     snaive = SNAIVE(Quantity ~ lag("week")),
    lm = TSLM(Quantity ~ trend() + season()),
    ets_additive = ETS(Quantity ~ error("A") + trend("A") + season("A")),
    ets_multiplicative = ETS(Quantity ~ error("M") + trend("A") + season("M")),
    ets_damped = ETS(Quantity ~ error("M") + trend("Ad") + season("M"))
  ) 

## Generate Forecasts ----
fc<- fit %>%
  forecast(h = 28)

## Evaluate multistep forecasts over the validation dataset ----
fc %>% accuracy(online_store_clean)%>% 
  select(ab_hl, .model, MAPE)


## Repeat with the best model only ----
fit <- online_store_clean %>%
  filter(InvoiceDate <= (max(InvoiceDate)-28)) %>%
  model(
    ets_damped = ETS(Quantity ~ error("M") + trend("Ad") + season("M"))
  ) 

fc<- fit %>%
  forecast(h = 28)


fc %>% autoplot(online_store_clean %>% filter(InvoiceDate >= "2011-08-01"))


# Bootstrapping & Forecast Bagging ----

## Bootstrapping ----

# A useful technique in data science which allow us to avoid over-fiting our model is bootstrapping. In practice, we bootstrap the residuals of a time series in order to simulate future values of a series using a model. The process is the following:

### Fit an STL model ----
online_store_stl<- online_store_fit %>%
  model(stl = STL(Quantity))

### Generate 100 time series ----
sim<- online_store_stl %>%
generate(new_data = online_store_fit, times = 100, bootstrap_block_size = 28, seed = 2022) %>%
  select(-.model, -Quantity)

### Plot time series ----
online_store_stl %>%
  generate(new_data = online_store_fit, times = 100, bootstrap_block_size = 28, seed = 2022) %>%
  autoplot(.sim) +
  autolayer(online_store_fit, Quantity) +
  guides(colour = "none") +
  labs(title = "AH Product sales: Bootstrapped series",
       y="Units Sold")


# For each of these series, we fit an ETS model and generate multiple 28 days multi step forecasts . This is a time-consuming process as there are a large number of series. It took a couple of mutes to complete on a normal windows laptop with 8GB of RAM.

### Generate Simulated Forecasts ----
ets_forecasts <- sim %>%
  model(ets = ETS(.sim ~ error("M") + trend("Ad") + season("M"))) %>%
  forecast(h= 28)

### Plot simulated Forecasts ----
ets_forecasts %>%
  update_tsibble(key = .rep) %>%
  autoplot(.mean) +
 autolayer(online_store_fit, Quantity) +
  guides(colour = "none") +
  labs(title = "AH Product sales: Bootstrapped series",
       y="Units Sold")


## Forecast Bagging ----

# One use for these bootstrapped time series is to improve forecast accuracy. If we produce forecasts from each of the additional time series, and average the resulting forecasts, we get better forecasts than if we simply forecast the original time series directly. This is called “bagging” which stands for “bootstrap aggregating”.

### Average all simulated forecasts ----
bagged <- ets_forecasts %>%
  summarise(bagged_mean = mean(.mean))

### COnvert into a fable object ----
bagged_forecasts<- bagged %>%
  mutate(dist = dist_degenerate(bagged_mean)) %>%
  as_fable(response = "Quantity", distribution = dist)

### Evaluate Bagged forecasts voer the validation dataset ----
bagged_forecasts %>% accuracy(online_store_clean) %>% mutate(method = "Bagged ETS") %>%
  select(method, MAPE) 

### Compare Bagged forecasts vs pre-boorstrapped ETS vs Actuals ----
online_store_fit %>%
  model(ets_auto = ETS(Quantity ~ error("M") + trend("Ad") + season("M"))) %>%
  forecast(h = 28) %>%
  autoplot(online_store_clean %>% filter(InvoiceDate >= "2011-08-01")) +
  autolayer(bagged, bagged_mean, col = "#D55E00") +
  labs(title = "AH Product sales: Bootstrapped Forecast",
       y="Units Sold")


