# libraries needed
library(prophet)
library(lubridate)
library(tidyverse)

# reading data
data = read.csv(file.choose(), header = T)
data$dteday = ymd(data$dteday)

# quick plot (function of ggplot2)
qplot(dteday, cnt, data = data,
      main = "Bike Rentals in Washington")

# Data
ds = data$dteday
y = data$cnt
df = data.frame(ds, y)
df$temp = data$temp
df$hum = data$hum

# forecasting model with regressors
m = prophet()
m = add_country_holidays(m, country_name = "US")
m = add_regressor(m, "temp")
m = add_regressor(m, "hum")
m = fit.prophet(m, df)

# making data frame with regressors
future = make_future_dataframe(m, periods = 10)
x = data.frame(df$temp)
colnames(x) = "temp"
y = data.frame(runif(10, 0.1, 0.3))
colnames(y) = "temp"
future$temp = rbind(x,y)
x = data.frame(df$hum)
colnames(x) = "hum"
y = data.frame(runif(10, 0.4, 0.8))
colnames(y) = "hum"
future$hum = rbind(a,b)
future = as.matrix(future)
colnames(future) = NULL
colnames(future) = c("ds", "temp", "hum")
future = as.data.frame(future)
future$temp = as.numeric(future$temp)
future$hum = as.numeric(future$hum)
future$ds = ymd(future$ds)

# predition
forecast = predict(m, future)

# plot forecast
plot(m, forecast)

# plotting the forecast components
prophet_plot_components(m, forecast)

# evaluating the model performance
pred = forecast$yhat[1:731]
actual = df[,2]
plot(actual, pred)
abline(lm(pred ~ actual))
summary(lm(pred ~ actual))
