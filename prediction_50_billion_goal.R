# Forecasting RMG export growth of Bangladesh 
# visualizing the impact of RMG export earning to total export earning
# Author: Mohsin Uddin

# libraries needed
library(tidyverse)
library(forecast)

# read data
data = read.csv(file.choose(), header = T)

# prediction using ARIMA
export = ts(data$rmg, start = min(data$year), end = max(data$year))
arima = auto.arima(export)
predict(arima, n.ahead = 10, se.fit = T)
forecast = forecast(arima, h = 10)
plot(forecast)

# barplot of % OF RMG’S TO TOTAL EXPORT of BD
ggplot(data, aes(year, percent))+
  geom_bar(stat = "identity", fill = "#E69F00")
