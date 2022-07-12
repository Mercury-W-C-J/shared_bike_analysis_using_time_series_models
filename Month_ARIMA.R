setwd("D:/学习/大三下/预测分析与方法/Group Project")
options(scipen = 100)
library(fpp3)
library(Rcpp)
library(fable.prophet)

# 导入数据并命名变量
data_test <- read.csv("month_result.csv",head=T)
test <- aggregate(data_test$started_at, list(data_test$started_at_day), FUN=sum)
names(test)[names(test) =="Group.1"] <-"date"
names(test)[names(test) =="x"] <-"number"

# 将数据格式转换为tsibble
test=test %>%
mutate(mth = yearmonth(as_date(date)))
ts_test = as_tsibble(test, index = mth)

# 画时序图
ts_test %>%
  autoplot()

# 画差分图和自相关系数图
ts_test %>%
  gg_tsdisplay(difference(number,12),
               plot_type = "partial") + labs(title = "Seasonally differenced" , y="")
ts_test %>%  
  gg_tsdisplay(difference(number,12) %>% difference(),
               plot_type = "partial") + labs(title = "Double differenced" , y="")

# 划分训练集
ts_test_2021 <- ts_test %>%
  filter(yearmonth(as_date(date)) <= yearmonth(as_date("2021-02-01")))

# 模型拟合  
fit <- ts_test_2021 %>%
  model(A_112010 = ARIMA(number~pdq(1,1,2) + PDQ(0,1,0)),
        A_stepwise = ARIMA(number),
        A_search = ARIMA(number, stepwise=FALSE),
        ETS_N_A = ETS(number ~trend("N") + season("A"), opt_crit = "mse"),
        ETS_N_M = ETS(number ~trend("N") + season("M"), opt_crit = "mse"),
        ETS_A_A = ETS(number ~trend("A") + season("A"), opt_crit = "mse"),
        ETS_A_M = ETS(number ~trend("A") + season("M"), opt_crit = "mse"),
        ETS_Ad_A = ETS(number ~trend("Ad") + season("A"), opt_crit = "mse"),
        ETS_Ad_M = ETS(number ~trend("Ad") + season("M"), opt_crit = "mse")
        )

# 展示拟合模型
fit %>%
  pivot_longer(everything(),names_to = "Model name", values_to = "Orders")
# 划分测试集
number_2022 <- ts_test %>%
  filter(yearmonth(as_date(date)) >= yearmonth(as_date("2021-03-01")))

# 模型预测
number_forecast <- fit %>%
  forecast(number_2022)

# 画预测图
number_forecast %>%
  filter(.model=='ETS_A_A') %>%
  autoplot(bind_rows(ts_test_2021,number_2022))+guides(colour = guide_legend(title = "Forecast"))

# 展示模型预测精度
accuracy(number_forecast,data=ts_test)

