library(forecast)
library(tseries)
library(lubridate)
library(rugarch)
data<-read.csv("C:/Users/glane/Downloads/hpimonthly.csv",stringsAsFactors = FALSE)
View(data)
dmy(data$Date)

#Remove £ and , to convert into time series
data$Price <- as.numeric(gsub("[\\£,]", "", data$Price))
data_new <- ts(data$Price, frequency = 12 ,start = 1991)

plot(data_new, main = "Time Series of Price")

decomposed_data_new <- decompose(data_new, type = "additive")
plot(decomposed_data_new )

ggAcf(data_new, main='ACF for Differenced Series') 
ggPacf(data_new, main='PACF for Differenced Series') 


#Ensuring the stationarity of the series:
trans_log<-log(data_new)

ar(trans_log)
adf.test(trans_log)

p<-diff(trans_log) 
ar(p)



ggAcf(p, main='ACF for Differenced Series') 
ggPacf(p, main='PACF for Differenced Series') 



model=ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(4, 3), include.mean = TRUE),
  distribution.model = "norm")

modelfit=ugarchfit(spec=model,data=p)
modelfit

modelfor=ugarchforecast(modelfit, data = p, n.ahead = 100,n.roll = 4)
modelfor

plot(modelfor)

