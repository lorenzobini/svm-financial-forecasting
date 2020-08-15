library(quantmod)
library(caret)
library(kernlab)
library(Metrics)
library(xts)
library(tseries)
library(forecast)


stock = get.hist.quote(instrument = "RDS-B", start="2007-10-01", quote="AdjClose",
                       origin="1970-01-01", provider="yahoo", compression = "m",
                       retclass = "zoo")
index(stock) = as.yearmon(index(stock))

stockTrain = stock[1:(0.7*length(stock))]
stockTest = stock[(0.7*length(stock)+1):(0.9*length(stock))]
stockData = stock[1:(0.9*length(stock))]
stockForecast = stock[(0.9*length(stock)+1):length(stock)]
fit = arima(stockTrain, order = c(5,2,20))

arma.forecast = forecast(fit, h=25)
plot(arma.forecast, main="RDS")

lines(stockTest)


fit = arima(stockData, order = c(5,2,20))
arma.forecast = forecast(fit, h=10)
plot(arma.forecast, main="RDS")

lines(stockForecast)





#------------------------------------



lagFunct = function(stock){
  input = merge(lag(stock$Adjusted,1),
                lag(stock$Adjusted,2),
                lag(stock$Adjusted,3),
                lag(stock$Adjusted,4),
                lag(stock$Adjusted,5),
                lag(stock$Adjusted,6),
                lag(stock$Adjusted,7),
                lag(stock$Adjusted,8),
                lag(stock$Adjusted,9),
                lag(stock$Adjusted,10),
                all=FALSE)
  
  
  data <<- merge(input, stock$Adjusted, all=FALSE)
  data <<- na.omit(data)
  colnames(data) = c("lag1","lag2","lag3","lag4","lag5","lag6",
                     "lag7","lag8","lag9","lag10","TARGET")
  return(data)
}

arimaForecasting = function(stock,desc,p,d,q,h1,h2){
  #Parte di test
  
  lnt = length(stock)
  stockTrain = stock[1:(0.75*length(stock))]
  stockTest = stock[(0.75*length(stock)):(0.90*length(stock))]
  fit = arima(stockTrain, order = c(p,d,q))
  
  arma.preds = predict(fit, n.ahead = ((0.90*length(TSLA))-(0.65*length(TSLA))))$pred
  arma.forecast = forecast(fit, h = h1)
  plot(arma.forecast, main = desc)
  
  accuracy(arma.preds, stockTest)[2]
  
  lines(stockTest)
  
  #Parte di forecasting
  
  stockData = stock[1:(0.90*length(stock))]
  stockForecast = stock[(0.90*length(stock)):length(stock)]
  
  fit = arima(stockData, order = c(p,d,q))
  arma.forecast = forecast(fit, h=h2)
  plot(arma.forecast, main = desc)
  
  lines(stockForecast)
  
  return(arma.forecast)
  
}