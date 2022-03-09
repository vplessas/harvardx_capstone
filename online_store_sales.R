

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


### Groupped Time Seies ----
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

```


```{r final dataset, include=FALSE}

online_store <- online_store %>%
  left_join(ab_hl, by = "product_code") %>%
  mutate(ab_hl = paste0(ab,hl)) %>%
  filter(ab_hl == "AH") %>%
  group_by(ab_hl, InvoiceDate) %>%
  summarise(Quantity = sum(Quantity)) %>%
  ungroup()

online_store %>% glimpse()

```


# Time Series Analysis

For the following part of out this report we will be using the [tidyverts](https://tidyverts.org/) packages which consist of :
  
  [tsibble](https://tsibble.tidyverts.org/) : a data infrastructure for tidy temporal data with wrangling tools.
[feasts](https://feasts.tidyverts.org/): a collection of tools for the analysis of time series data.
[fable](https://fable.tidyverts.org/) :  a collection of commonly used univariate and multivariate time series forecasting models including exponential smoothing via state space models.

A lot of the code has been inspired or adapted from Rob J Hyndman's the excellent online  textbook on forecasting [Forecasting Principles and Practice 3rd ed.](https://otexts.com/fpp3/). 

## Convert into a tsibble object

To coerce a data frame to tsibble, we need to declare key and index. In our case the ab_hl column containing the categorical variables can be considered as key and the index is the InvoiceDate column containing temporal values. The Quantity column or any other numerical fields can be considered as measured variables. Printing out the top 6 tsibble rows shows the new class "A tsibble 6x3[1d]" indicating the number of rows and columns visible as well as the detected temporal frequency (daily). The printout also indicates what the key of this tsibble object.

```{r convert to tsibble, echo=FALSE}

online_store_tsbl<- online_store %>%
  as_tsibble(key = ab_hl, index = InvoiceDate) %>%
  filter_index("2011") %>%
  group_by_key() %>%
  summarise(Quantity = sum(Quantity)) %>%
  ungroup()

online_store_tsbl %>% head()

  
```


The feasts package contains a very useful plotting function [autoplot()](https://fabletools.tidyverts.org/reference/autoplot.tbl_ts.html) which allows us to easily produce time-series plots from a tibble with minimum code. We have used this function across the rest of our analysis.

Figure 5 shows the time series for AH product sales in 2011. We have fitted a linear regression line to indicate the upward sales trend which we've detected earlier. At this point it's worth revisiting the  previously observed outliers which are visible in the second half of the year.

We have proceed in applying common techniques to identify and smooth out those outliers in the following steps.

```{r visulise_ts, echo=FALSE}
online_store_tsbl %>%
  autoplot() +
  geom_smooth(method = "lm", se = FALSE)
```


## Missing Values

Before proceeding any further we need to inspect our tsibble for any implicit gaps. This is an impotant step as a number of the models we'll try to fit in our dataset cannot process missing values.
The [has_gaps()](https://www.rdocumentation.org/packages/tsibble/versions/1.1.1/topics/has_gaps) function from the tsibble package has identified the existence of gaps in our set.

```{r identify_gaps, echo=FALSE}

online_store_tsbl %>%
  has_gaps(.full = TRUE)

```

Figure 6 shows the distribution of gaps in our tsibble. It appears that we don't have a lot of consecutive gaps in our timeseries, rather intermitent gaps of one and a few of two and 3 consecutive ones. The [count_gaps()](https://www.rdocumentation.org/packages/tsibble/versions/0.5.0/topics/count_gaps) function from the tsibble package has help in ths regard.

```{r visualise gaps, echo=FALSE}

online_store_tsbl %>%
count_gaps(.full = TRUE) %>% 
  ggplot(aes(.n)) +
  scale_x_log10()+
  geom_histogram(bins = 20)
```

It is common practice in time series analysis to replace missing values with an approximate value of mean or median , fill with the previous or next non-blank observation or use a more advanced method for interpolation (needs reference here).
Here we have filled any implicit gaps with the last non-blank observation in the time series. Running the has_gaps() function again, verifies that we don't have any further gaps in our data.

```{r fill gaps, echo=FALSE}
online_store_gaps<-
  online_store_tsbl %>%
  group_by_key() %>%
  fill_gaps() %>%
  tidyr::fill(Quantity, .direction = "down") %>%
  ungroup()

online_store_gaps %>% has_gaps(.full = TRUE) %>% kable()

```


## Outliers

Now that our dataset is gap free, we can try and smooth out any outliers still present in our time series before we proceed in the modelling and forecasting phase. We will fit an STL Decomposition model in our data.Then any outliers should show up in the remainder series which will indicate any values that could not be explained by the model. Figure 7 shows the output of our decomposed time series with the 3 previously suspected outliers clearly visible in the remainder part of the plot.

```{r fit_stl, echo=FALSE}

decomposition<- online_store_gaps %>%
  model(
    stl = STL(Quantity ~ season(period =1), robust = TRUE)
  ) %>%
  components()

decomposition %>% autoplot()

```

We have used a rule to define outliers as those that are greater than 3 interquartile ranges (IQRs) from the central 50% of the data, which would make only 1 in 500,000 normally distributed observations to be outliers. Below table show the output of this:
  
  ```{r set_outlier_limits, echo=FALSE}

outliers <-
  decomposition %>%
  filter(
    remainder < quantile(remainder, 0.25) - 3* IQR(remainder) |
      remainder > quantile(remainder, 0.75) + 3* IQR(remainder)
  )

outliers
```

As a final step, we have removed those outliers from the dataset and have used an Auto Regressive Integrated Moving Average (ARIMA) model from the fable package to replace them with interpolation.

```{r remove_outliers2, include=FALSE}
online_store_outliers <-
  online_store_gaps %>%
  anti_join(outliers) %>%
  fill_gaps()
```


```{r interpolate_fill, include=FALSE}
online_store_clean <-
  online_store_outliers %>%
  model(ARIMA(Quantity)) %>%
  interpolate(online_store_outliers)
```



# Time Series Features

Figure 8 shows the now normalised time series. Observe how the outliers are no longer skewing the distribution of the series.

```{r series_plot, echo=FALSE}
online_store_clean %>%
  autoplot() +
  geom_smooth(method = "lm", se = FALSE)

```

## Lag Plots

Our time series is univariate, meaning there's only one indipendent variance (time) interacting with the response variable (Units Sold). In such time series every time point can be thought as a feature by creating different time lags and examine the relationship to the response variable.
Figure 9 shows lagged values of the time-seris. Each graph shows $y_{t}$ plotted against $y_{t-k}$ for different values of $k$ .  The colours here indicate the days of the week. There seems to be a strong positive relationship for lag 7 which indicates the existence of a weekly seasonality. 

```{r lagged_plots, echo=FALSE}

lagged_sales <-
  online_store_clean %>%
  filter(month(InvoiceDate) > 1)

lagged_sales %>%
  gg_lag(Quantity, geom = "point")+
  labs(x = "lag(Quantity, k)")

```


## Autocorrelation

Autocorrelation measures the extent of a linear relationship between lagged values of a time series. Lagged autocorrelation coefficients, for example $r_{1}$ measures the relationship between $y_{t}$ and $y_{t-1}$ , $r_{2}$ measures the realtionship between $y_{t}$ and $y_{t-2}$ and so on.

The autocorrelation function can be written as $$r_{k} = \frac{\sum\limits_{t=k+1}^T (y_{t}-\bar{y})(y_{t-k}-\bar{y})}{\sum\limits_{t=1}^T (y_{t}-\bar{y})^2}$$
when $T$ is the length of the time series.

Figure 10 shows the autocorrelation plot (correlogram) for $k$ lag = 28. $r_{7}$ , $r_{14}$, $r_{21}$ and $r_{28}$ are higher than the others indicating a visible weekly seasonal pattern in the data. The dashed blue line indicates where the correlations are significantly different from zero.

```{r correlogram, echo=FALSE}

online_store_clean %>%
  ACF(Quantity, lag_max = 28) %>%
  autoplot()
```

For data with trend like ours it's useful to increase the number of estimated lags (see Figure 11). When data have a trend, the autocorrelations for small lags tend to be large and positive because observations nearby in time are also nearby in value. So the ACF of a trended time series tends to have positive values that slowly decrease as the lags increase.

```{r long_correlogram, echo=FALSE}

online_store_clean %>%
  ACF(Quantity, lag_max = 84) %>%
  autoplot()

```


## STL Decomposition

A time series decomposition can be used to measure the strength of trend and seasonality in a time series. Recall that the decomposition is written as 
$$y_t = T_t + S_{t} + R_t$$ where $T_{t}$ is the smoothed trend component, $S_{t}t$  is the  seasonal component and $R_{t}$  is a remainder component. For strongly trended data, the seasonally adjusted data should have much more variation than the remainder component. But for data with little or no trend, the two variances should be approximately the same. So we define the strength of trend as: 
  $$F_T = \max\left(0, 1 - \frac{\text{Var}(R_t)}{\text{Var}(T_t+R_t)}\right)$$
  This will give a measure of the strength of the trend between 0 and 1

The strength of the seasonality is defined simirarly but with respect to the detrended data tather than the seasonally adjusted data:
  $$F_S = \max\left(0, 1 - \frac{\text{Var}(R_t)}{\text{Var}(S_{t}+R_t)}\right)$$
  A series with seasonal strength FS close to 0 exhibits almost no seasonality, while a series with strong seasonality will have $F_{S}$ close to 1.

We can use the feat_stl() function from the feasts function to extract those features for one or multiple time series in our tsibble. Below we see the values for the trend and seasonal and trend strength of our time series as well as the peak and trough values in the season. We can see that within a week our series tends to peak on the 3rd day (Wednesday) and falls on the 6th (Saturday.)

```{r stl_features, echo=FALSE}

online_store_clean %>% features(Quantity, feat_stl)
```

We've previously fitted an STL Decomposition model, suppressing the seasonality in order to identify any potential outliers in our data. Figure 12 shows a full decomposition model with the smoothed upwards trend and the weekly seasonal pattern clearly visible. The remainder component shown in the bottom panel is what is left over when the seasonal and trend-cycle components have been subtracted from the data. These components can be added together to reconstruct the data shown in the top panel. 

```{r  components_plot, echo=FALSE}

online_store_clean %>%
  model(
    stl = STL(Quantity ~ trend(window = 7) + season(window = "periodic"))
  ) %>%
  components() %>% autoplot()
  
```
Correctly identifying the components tha make our time series is an important step to produce good forecast by selecting the appropriate model which fits our data best.


# Forecasting Methodology

## Train and Test sets

It is best practice to split our time series between train and test data sets. As we are testing out models to select the most appropriate one for our data, we train those models in the train data and we evaluate the performance on the test set which contains data that have not been used in the training process. That way we can avoid over-fitting and ensure our model generalises good enough to predict outcomes on new data.

For our methodology, we have split our data between the online_store_fit and online_store_validate (28 days) datasets. We have further split the online_store_fit set between the online_store_train and online_store_test (28 days) datasets. 

We have then fitted multiple models on the online_store_train set and have measured their accuracy on the online_store_test dataset by producing one-step forecasts using the [refit()](https://fable.tidyverts.org/reference/refit.ETS.html). It is worth noting that each model tested is not re-estimated in this case. Instead, the model obtained previously (and stored as method_fit) is applied to the test data. Because the model was not re-estimated, the “residuals” obtained here are actually one-step forecast errors.

Finally we refited all the trained models on the entire online_store_fit dataset and produced one-step forecast on teh online_store_validate dataset which until now had not been used. The model that yielded the smaller error was then selected to produce multiple step forecasts (or true forecasts) using the entire dataset and measure it's performance on the last 28 days of that set.

In order to ensure we have not over-fitted our model, we have used time series bootstrapping to generate 100 time series of bootstrap block size 28. We've then created simulated forecasts from those series using the winning model from our previous step and measured our accuracy again over the online_store_validate datasets and compared with our previous result.

```{r data_slits, include=FALSE}

online_store_fit <- online_store_clean %>%
  filter(InvoiceDate <= (max(InvoiceDate)-28))

online_store_validate <- online_store_clean %>%
  filter(InvoiceDate > (max(InvoiceDate)-28))

online_store_train <- online_store_fit %>%
  filter(InvoiceDate <= (max(InvoiceDate)-28))

online_store_test <- online_store_fit %>%
  filter(InvoiceDate > (max(InvoiceDate)-28))

```


## Forecasting Methods

### NAIVE

One of the simplest forecasting techniques avaialable is the Naive method. For naïve forecasts, we simply set all forecasts to be the value of the last observation. This method is usually a good benchmark for other methods, or simply put if a more advanced method cannot beat the Naive's accuracy score with we might as well use the Naive one as our best estimation for demand.

The simple Naive can be written as :
  
  $$\hat{y}_{T+h|T} = y_{T}$$
  
  In our case because our data have a seasonality, we will use a variation fo the Naive method called Seasonal Naive. In this case, we set each forecast to be equal to the last observed value from the same season. It can be written as :
  $$\hat{y}_{T+h|T} = y_{T+h-m(k+1)},$$
  
  where $m$ = the seasonal period and $k$ is the integer part of $(h-1)/m$ the number of completed points in the period prior to time $(h-1)/m$.

```{r SNAIVE, include=FALSE}

fit_naive<- online_store_train %>%
  model(snaive = SNAIVE(Quantity ~ lag("week")))

fc_naive<- fit_naive %>%
  forecast(h = 28) 

```

```{r plot snaive, echo=FALSE}

fc_naive %>%
  autoplot(online_store_train %>% filter(InvoiceDate >= "2011-07-01"))
```

We've fitted the SNAIVE algorithm on the training set and tested 28 one-step forecast on the test one. Figure 12 visualises the generated forecasts as well as their 80% and 95% confident intervals. The evaluation on the trained model yielded a MAPE of 28.9 which is will be our benchmark.
```{r naive accuracy, echo=FALSE}
fit_naive %>% refit(online_store_test) %>%
  accuracy() %>% 
  select(ab_hl, .model, MAPE)

```


### Linear Regression

A Linear Regression model in its simplest case, allows for a linear relationship between the forecast variable $y$ and a single predictor variable $x$ :
$$y_t = \beta_0 + \beta_1 x_t + \varepsilon_t$$

The coefficients β0 and β1 denote the intercept and the slope of the line respectively. The intercept β0 represents the predicted value of y when x=0. The slope β1 represents the average predicted change in y resulting from a one unit increase in x. We can think of each observation yt as consisting of the systematic or explained part of the model,$\beta_0+\beta_1x_t$ , and the random “error”, $ε_{t}$. The linear regression equation is estimated using the TSLM() function from the fable package.

We will use the trend and seasonality variables we've identified earlier as predictor variables in the linear regression function.

```{r TSLM, include=FALSE}
# Linear Model ----

fit_lm <- online_store_train %>%
  model(
    lm = TSLM(Quantity ~ trend() + season())
  )

fc_lm <- fit_lm %>%
  forecast(h = 28)

```

```{r plot tslm, echo=FALSE}

fc_lm %>% autoplot(online_store_train %>% filter(InvoiceDate >= "2011-07-01"))
```
Figure 13 shows the plotted one step forecasts derived from fitting the linear regression algorithm on the train dataset. A MAPE of 28.2 has been observed which is only a marginal improvement over the SNAIVE one. HOwever, unike SNAIVE, the seasonal pattern looks more consistent and teh prediction intervals are narrower.

```{r refit tslm, echo=FALSE}

fit_lm %>% refit(online_store_test) %>%
  accuracy() %>% 
  select(ab_hl, .model, MAPE)
```


### Exponential Smoothing

Exponential smoothing was proposed in the late 1950s (Brown, 1959; Holt, 1957; Winters, 1960), and has motivated some of the most successful forecasting methods. Forecasts produced using exponential smoothing methods are weighted averages of past observations, with the weights decaying exponentially as the observations get older. In other words, the more recent the observation the higher the associated weight. This framework generates reliable forecasts quickly and for a wide range of time series, which is a great advantage and of major importance to applications in industry.

#### Simple Exponential Smoothing

This method is suitable for forecasting data with no clear trend or seasonal pattern. Forecasts are calculated using weighted averages, where the weights decrease exponentially as observations come from further in the past — the smallest weights are associated with the oldest observations: 
  
  $$\hat{y}_{T+1|T} = \alpha y_T + \alpha(1-\alpha) y_{T-1} + \alpha(1-\alpha)^2 y_{T-2}+ \cdots$$
  
  where 0≤α≤1 is the smoothing parameter. The one-step-ahead forecast for time T+1 is a weighted average of all of the observations in the series y1,…,yT. The rate at which the weights decrease is controlled by the parameter α. For any α between 0 and 1, the weights attached to the observations decrease exponentially as we go back in time, hence the name “exponential smoothing”.

The weighted average form of the equation can be written as:
  
  $$\hat{y}_{T+1|T} = \alpha y_T + (1-\alpha) \hat{y}_{T|T-1},$$
  
  where 0≤α≤1 is the smoothing parameter.

We fit a Simple Exponential Smoothing to our train data using the ETL function from fable package and by passing the parameters error("A") + trend("N") + season("N"). "A" stands for additive, meaning the residual errors get added as the algorithm progresses over the time series, and "N" simply cancels out the algorithms fucntion to search for trend and seasonality in the data.
```{r Simple ES, include=FALSE}
# Exponential Smoothing ----

## Simple Exponential Smoothing ----

fit_ses <- online_store_train %>%
  model(
    ets = ETS(Quantity ~ error("A") + trend("N") + season("N"))
  )

fc_ses <- fit_ses %>%
  forecast(h = 28)

```

Single exponential smoothing produces flat forecasts as no trend or seasonality compponets are being accounted. Figure 14, visualises the produces forecasts for the next 28 days along with the 80% and 95% confidence intervals

```{r visualise SeS, echo=FALSE}

fc_ses %>% autoplot(online_store_train %>% filter(InvoiceDate >= "2011-07-01"))
```

Given both the trend and seasonal patterns in our data, it comes as no surprise that we don't have a good MAPE result in this case.
```{r refit ses, echo=FALSE}
fit_ses %>% refit(online_store_test) %>%
  accuracy() %>% 
  select(ab_hl, .model, MAPE, ACF1)
```

#### Holt's Exponential Smoothing

Holt (1957) extended simple exponential smoothing to allow the forecasting of data with a trend. This method involves a forecast equation and two smoothing equations (one for the level and one for the trend):
  
  $$\begin{align*}
\text{Forecast equation}&& \hat{y}_{t+h|t} &= \ell_{t} + hb_{t} \\
\text{Level equation}   && \ell_{t} &= \alpha y_{t} + (1 - \alpha)(\ell_{t-1} + b_{t-1})\\
\text{Trend equation}   && b_{t}    &= \beta^*(\ell_{t} - \ell_{t-1}) + (1 -\beta^*)b_{t-1},
\end{align*}$$
  
  where ℓt denotes an estimate of the level of the series at time t, bt denotes an estimate of the trend (slope) of the series at time t, α is the smoothing parameter for the level, 0≤α≤1, and β∗ is the smoothing parameter for the trend, 0≤β∗≤1.

```{r holts ES, include=FALSE}
# Exponential Smoothing ----

## Holt's Exponential Smoothing ----

fit_hes <- online_store_train %>%
  model(
    ets = ETS(Quantity ~ error("A") + trend("A") + season("N"))
  )

fc_hes <- fit_hes %>%
  forecast(h = 28)


```


```{r plot HES, echo=FALSE}

fc_hes %>% autoplot(online_store_train %>% filter(InvoiceDate >= "2011-07-01"))
```

The forecast function is no longer flat but trending (See Figure 15). The h-step-ahead forecast is equal to the last estimated level plus h times the last estimated trend value. Hence the forecasts are a linear function of h.

Also, there's been a noticeable improvement in the MAPE score which indicates that our algorithm has began to fit our training data better.
```{r refit hes, echo=FALSE}

fit_hes %>% refit(online_store_test) %>%
  accuracy() %>% 
  select(ab_hl, .model, MAPE, ACF1)
```


#### Holt Winter's Exponential Smoothing

Holt (1957) and Winters (1960) extended Holt’s method to capture seasonality. The Holt-Winters seasonal method comprises the forecast equation and three smoothing equations — one for the level ℓt, one for the trend bt, and one for the seasonal component st, with corresponding smoothing parameters α, β∗ and γ. We use m to denote the period of the seasonality, i.e., the number of seasons in a year.

There are two variations to this method that differ in the nature of the seasonal component. The additive method is preferred when the seasonal variations are roughly constant through the series, while the multiplicative method is preferred when the seasonal variations are changing proportional to the level of the series. 

The component form for the additive method is:
  
  $$\begin{align*}
\hat{y}_{t+h|t} &= \ell_{t} + hb_{t} + s_{t+h-m(k+1)} \\
\ell_{t} &= \alpha(y_{t} - s_{t-m}) + (1 - \alpha)(\ell_{t-1} + b_{t-1})\\
b_{t} &= \beta^*(\ell_{t} - \ell_{t-1}) + (1 - \beta^*)b_{t-1}\\
s_{t} &= \gamma (y_{t}-\ell_{t-1}-b_{t-1}) + (1-\gamma)s_{t-m},
\end{align*}$$
  
  where k is the integer part of (h−1)/m, which ensures that the estimates of the seasonal indices used for forecasting come from the final year of the sample. The level equation shows a weighted average between the seasonally adjusted observation (yt−st−m) and the non-seasonal forecast (ℓt−1+bt−1) for time t.


The component form for the multiplicative method is: 
  
  $$\begin{align*}
\hat{y}_{t+h|t} &= (\ell_{t} + hb_{t})s_{t+h-m(k+1)} \\
\ell_{t} &= \alpha \frac{y_{t}}{s_{t-m}} + (1 - \alpha)(\ell_{t-1} + b_{t-1})\\
b_{t} &= \beta^*(\ell_{t}-\ell_{t-1}) + (1 - \beta^*)b_{t-1}                \\
s_{t} &= \gamma \frac{y_{t}}{(\ell_{t-1} + b_{t-1})} + (1 - \gamma)s_{t-m}.
\end{align*}$$
  
  A method that often provides accurate and robust forecasts for seasonal data is the Holt-Winters method with a damped trend and multiplicative seasonality: 
  
  $$\begin{align*}
\hat{y}_{t+h|t} &= \left[\ell_{t} + (\phi+\phi^2 + \dots + \phi^{h})b_{t}\right]s_{t+h-m(k+1)} \\
\ell_{t} &= \alpha(y_{t} / s_{t-m}) + (1 - \alpha)(\ell_{t-1} + \phi b_{t-1})\\
b_{t} &= \beta^*(\ell_{t} - \ell_{t-1}) + (1 - \beta^*)\phi b_{t-1}             \\
s_{t} &= \gamma \frac{y_{t}}{(\ell_{t-1} + \phi b_{t-1})} + (1 - \gamma)s_{t-m}.
\end{align*}$$
  
  
  Using the model function in fable we can fit all three versions of Exponential Smoothing on our train data simultaneously. We differentiate between the method by passing different arguments within the corresponding functions. For example for the multiplicative method we can define error("M") + trend("A") + season("M").
```{r hwes, include=FALSE}
# Exponential Smoothing ----

## Holt - Winter's Exponential Smoothing ----

fit_hwes <- online_store_train %>%
  model(
    ets_additive = ETS(Quantity ~ error("A") + trend("A") + season("A")),
    ets_multiplicative = ETS(Quantity ~ error("M") + trend("A") + season("M")),
    ets_damped = ETS(Quantity ~ error("M") + trend("Ad") + season("M"))
  )

fc_hwes <- fit_hwes %>%
  forecast(h = 28)

```


Three sets of forecasts are plotted in figure 16 which correspond to each of the methods used to fit our train data. We can see that the algorithm has correctly picked up on both the trend and seasonal components of our time series, however the differences appear to be small between them.
```{r visualise hwes, echo=FALSE}

fc_hwes %>% autoplot(online_store_train %>% filter(InvoiceDate >= "2011-07-01"), level = NULL)
```

Perhaps the biggest observation is on how well the algorithm has performed when all 3 trained models get refitted as one-step forecasts and measured on the test data set. Their performance is almost identical with a slight edge on the multiplicative version of Exponential Smoothing. This is also ~10% improvement over the forecast error of Simple Exponential Smoothing and ~8% improvement over the SNAIVE algorithm. 
```{r refit hwes, echo=FALSE}
fit_hwes %>% refit(online_store_test) %>%
  accuracy() %>% 
  select(ab_hl, .model, MAPE)
```


## Validation

At this point we feel that we have achieved some good enough scores and we have proceeded into fitting all above tuned methods against the entire online_store_fit data set. Note that we have yet to use the online_store_validate dataset in our testing , which we'll do now using multi step forecasts and compare all methods applied thus far.

Again the fable package allows us to fit all of the algorithms simultaneously and evaluate the best method.

Looking at the MAPE results we can observe that the damped trend version of the algorithm had surpassed all other methods with a MAPE score of 25.1% which is  11% improvement over the SNAIVE method.

```{r fit all, include=FALSE}
## Fit Model ----

fit <- online_store_clean %>%
  filter(InvoiceDate <= (max(InvoiceDate)-28)) %>%
  model(
     snaive = SNAIVE(Quantity ~ lag("week")),
    lm = TSLM(Quantity ~ trend() + season()),
    ets_additive = ETS(Quantity ~ error("A") + trend("A") + season("A")),
    ets_multiplicative = ETS(Quantity ~ error("M") + trend("A") + season("M")),
    ets_damped = ETS(Quantity ~ error("M") + trend("Ad") + season("M"))
  ) 

fc<- fit %>%
  forecast(h = 28)

```

```{r measure all, echo=FALSE}
fc %>% accuracy(online_store_clean)%>% 
  select(ab_hl, .model, MAPE)

```

Because it is difficult to visualise all five forecasts produced by each method, we proceed with plotting only the 28 days ahead multi step process from the ETS Damped one which produced the best score (see figure 17). We can observe that that our trained algorithm is performing fairly well in keeping up wit the weekly trend and seasonality pattern fo the previously unused validate dataset. Something that also catches our eye is how large the confidence intervals are. Perhaps our model doesn't generalise enough and there's still variability in the data that cannot be explained by our Exponential SMoothing model.
```{r visualise best, echo=FALSE}

fit <- online_store_clean %>%
  filter(InvoiceDate <= (max(InvoiceDate)-28)) %>%
  model(
    ets_damped = ETS(Quantity ~ error("M") + trend("Ad") + season("M"))
  ) 

fc<- fit %>%
  forecast(h = 28)


fc %>% autoplot(online_store_clean %>% filter(InvoiceDate >= "2011-08-01"))
```


# Bootstrapping & Forecast Bagging

## Bootstrapping

A useful technique in data science which allow us to avoid over-fiting our model is bootstrapping. In practice, we bootstrap the residuals of a time series in order to simulate future values of a series using a model. The process is the following:

First, the time series is transformed if necessary, and then decomposed into trend, seasonal and remainder components using STL. 

Then we obtain shuffled versions of the remainder component to get bootstrapped remainder series. Because there may be autocorrelation present in an STL remainder series, we cannot simply use the re-draw procedure. Instead, we use a “blocked bootstrap”, where contiguous sections of the time series are selected at random and joined together. 

These bootstrapped remainder series are added to the trend and seasonal components, and the transformation is reversed to give variations on the original time series.

We implement the bootstrapping process bellow:

We generated 100 simulations of our time-series (online_store_fit) using the [generate()](https://fable.tidyverts.org/reference/generate.RW.html) fucntion from the fable model. We have used a block size of 28 to capture 4 weeks of data which is also teh forecasting horizon we are aiming for. Figure 18 shows shows a visualisation fo these simulated series over the original data.

```{r generate_bs, echo=FALSE}

online_store_stl<- online_store_fit %>%
  model(stl = STL(Quantity))

sim<- online_store_stl %>%
generate(new_data = online_store_fit, times = 100, bootstrap_block_size = 28, seed = 2022) %>%
  select(-.model, -Quantity)

online_store_stl %>%
  generate(new_data = online_store_fit, times = 100, bootstrap_block_size = 28, seed = 2022) %>%
  autoplot(.sim) +
  autolayer(online_store_fit, Quantity) +
  guides(colour = "none") +
  labs(title = "AH Product sales: Bootstrapped series",
       y="Units Sold")
  


```


For each of these series, we fit an ETS model and generate multiple 28 days multi step forecasts . This is a time-consuming process as there are a large number of series. Figure 19 shows these forecasts generated. It took a couple of mutes to complete on a normal windows laptop with 8GB of RAM.

```{r generate_bs_fcs, echo=FALSE}

ets_forecasts <- sim %>%
  model(ets = ETS(.sim ~ error("M") + trend("Ad") + season("M"))) %>%
  forecast(h= 28)


ets_forecasts %>%
  update_tsibble(key = .rep) %>%
  autoplot(.mean) +
 autolayer(online_store_fit, Quantity) +
  guides(colour = "none") +
  labs(title = "AH Product sales: Bootstrapped series",
       y="Units Sold")

```

## Forecast Bagging

One use for these bootstrapped time series is to improve forecast accuracy. If we produce forecasts from each of the additional time series, and average the resulting forecasts, we get better forecasts than if we simply forecast the original time series directly. This is called “bagging” which stands for “bootstrap aggregating”.

```{r bagging, echo=FALSE}

bagged <- ets_forecasts %>%
  summarise(bagged_mean = mean(.mean))

bagged_forecasts<- bagged %>%
  mutate(dist = dist_degenerate(bagged_mean)) %>%
  as_fable(response = "Quantity", distribution = dist)

bagged_forecasts %>% accuracy(online_store_clean) %>% mutate(method = "Bagged ETS") %>%
  select(method, MAPE) 

```

We've managed to improve our previous MAPE slightly which of course is a welcomed achievement but more importantly it provides us with the reassurance that we have not overfitted our model and would problably perform well on forecasting 28 days of sales for this product group on data that the algorithm has not yet seen. Figure 20 visually compares the bagged forecast (orange), with directly applied ETS damped (before bootstrapping) and with the actual values fo the last 28 days of the dataset.

As Bergmeir et al. (2016) has shown, on average, bagging gives better forecasts than just applying ETS() directly. However it comes with a performance cost as it more complex data structures it could take a significant ammount of time to calculate.

```{r plot bagged, echo=FALSE}
online_store_fit %>%
  model(ets_auto = ETS(Quantity ~ error("M") + trend("Ad") + season("M"))) %>%
  forecast(h = 28) %>%
  autoplot(online_store_clean %>% filter(InvoiceDate >= "2011-08-01")) +
  autolayer(bagged, bagged_mean, col = "#D55E00") +
  labs(title = "AH Product sales: Bootstrapped Forecast",
       y="Units Sold")
```
