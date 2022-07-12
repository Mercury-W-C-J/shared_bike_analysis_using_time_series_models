setwd("D:/school/SJTU/Prediction Analysis and Methods/project")
library(fpp3)
library(Rcpp)
library(fable.prophet)
library(rlang)
library(prophet)
library(ggplot2)
library(ggplotify)

## Daily Analysis ##
data_test <- read.csv("day_result1.csv",head=T)

# Group the data in the level of day
test <- aggregate(data_test$started_at, list(data_test$started_at_day), FUN=sum)
names(test)[names(test) =="Group.1"] <-"ds"
names(test)[names(test) =="x"] <-"y"
test=test %>%
  mutate(ds = as_date(ds))
ts_test = as_tsibble(test, index = ds)

# Fill the date missed
x <- as_date(ymd("2019-04-01"))
y <- as_date(ymd("2021-12-31"))
z <- data.frame(ds=as_date(x:y),y=0)
newdf <- merge(ts_test,z,by="ds",all=TRUE)
data_filled <- newdf[,1:2]
names(data_filled)[names(data_filled) =="y.x"] <-"y"
ts_data_filled = as_tsibble(data_filled,index=ds)


# Train set
ts_train = ts_data_filled[1:1037,]

# Some holidays are needed to be set first, but it would influence our data due to unmatched dates
playoffs <- tibble(
  holiday = 'playoff',
  ds = as.Date(
    c('2008-01-13', '2009-01-03', '2010-01-16',
      '2010-01-24', '2010-02-07', '2011-01-08',
      '2013-01-12', '2014-01-12', '2014-01-19',
      '2014-02-02', '2015-01-11', '2016-01-17',
      '2016-01-24', '2016-02-07')),
  lower_window = 0,
  upper_window = 1
)
holidays <- playoffs
m <- prophet(holidays=holidays)

# Add US federal holidays
m <- add_country_holidays(m, country_name = 'US')

# Fit the model
m <- fit.prophet(m,ts_train)

# Make predictions
future <- make_future_dataframe(m,28,freq = 'days')
forecast_m <- predict(m,future)

# Plot the components
prophet_plot_components(m, forecast_m)

# Plot the forecast
plot(m,forecast_m)

# Compare the forecast and real data in detailed graph
forecast_m <- forecast_m %>%
  mutate(ds = ymd(ds))
forecast_m_as <- as_tsibble(forecast_m,index = ds)
forecast_m_as[1037:1065,] %>%
  autoplot(yhat,col='red')+autolayer(ts_data_filled[1037:1065,],y,col='black')

# Test Performance for last 28 days
# 由于该prophet包下测试集误差无法直接给出，需要借助其中Cross Validation的包进行调取，在设定了cutoff(下面的initial参数)后并不会对测试集造成污染
m_cv <- prophet(holidays=holidays)
m_cv <- add_country_holidays(m_cv, country_name = 'US')
m_cv <- fit.prophet(m_cv,ts_data_filled)

m.cv <- cross_validation(m_cv,initial = 1036,period = 28, horizon = 28, units = 'days')
m.p <- performance_metrics(m.cv)
tail(m.p,n=1)

# ARIMA model for comparison
ts_train = ts_data_filled[1:1037,]
fit <- ts_train %>%
  model(
    ARIMA_search = ARIMA(y, stepwise = FALSE, approx = FALSE)    
  )

forecast_ARIMA <- fit %>%
  forecast(h=28)

forecast_ARIMA %>%
  filter(.model=='ARIMA_search')%>%
  autoplot(ts_test)

accuracy(forecast_ARIMA,data = ts_test)

glance(fit)
fit %>%
  select('ARIMA_search') %>%
  report()

fit %>%
  select('ARIMA_search') %>%
  gg_tsresiduals()





## Hourly Analysis ##
data <- read.csv('filled_hourly_agg.csv',head=T)

# Select August as our data set
data_4weeks <- data %>%
  filter(day.hour.time<='2019-08-31 23:00:00' & day.hour.time>='2019-08-01 00:00:00')

# Group the data in the level of hour
test <- aggregate(data_4weeks$started_at, list(data_4weeks$day.hour.time), FUN=sum)
names(test)[names(test) =="Group.1"] <-"ds"
names(test)[names(test) =="x"] <-"y"
test=test[,1:2]
test=test %>%
  mutate(ds = ymd_hms(ds))

# Train and test set split
ts_test = as_tsibble(test, index = ds)
ts_train = ts_test[1:576,]

# ARIMA and prophet model
fit <- ts_train %>%
  model(
    prophet = prophet(y~season(period='weeks',order=4,type='multiplicative')+
                        season(period='days',order=6,type='multiplicative')),
    ARIMA_search = ARIMA(y, stepwise = FALSE, approx = FALSE)    
  )

fit %>%
  select('ARIMA_search') %>%
  report()

# Make prediction
fcst <- fit %>%
  forecast(h=7*24)

# Test performance in the next week
accuracy(fcst,data=ts_test) %>% arrange(RMSE)

fcst %>%
  autoplot(ts_test)

# Detailed plot
ts_test_detail=ts_test[576:744,]
fcst %>%
  autoplot(ts_test_detail)

# Residual Analysis
fit %>%
  select('prophet') %>%
  gg_tsresiduals()

fit %>%
  select('ARIMA_search') %>%
  gg_tsresiduals()

fit_aug <- fit %>%
  augment()

fit_aug %>%
  ACF(.resid) %>%
  autoplot()

fit_aug %>%
  filter(.model=='prophet') %>%
  features(.innov,box_pierce,lag=10)

fit_aug %>%
  filter(.model=='ARIMA_search') %>%
  features(.innov,box_pierce,lag=10,dof = 6)

fit_aug %>% features(.innov,box_pierce)
fit_aug %>% features(.innov,ljung_box)


# We use another Prophet library for the component graph
library(rlang)
library(prophet)

m <- prophet(ts_train,weekly.seasonality = 4,daily.seasonality = 6)
m <- fit.prophet(m,ts_train)

future <- make_future_dataframe(m,24*7,freq = 'hours')

forecast_m <- predict(m,future)

prophet_plot_components(m, forecast_m)
