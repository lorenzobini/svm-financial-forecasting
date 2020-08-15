library(quantmod)
library(caret)
library(kernlab)
library(Metrics)
library(xts)
library(tseries)













mse = 0
mae = 0
data = NULL

svmComput = function(ind){
  indexHistQ = get.hist.quote(instrument = ind,
                              start = "2007-10-01",
                              end = "2017-10-31",
                              quote="AdjClose",
                              provider = "yahoo",
                              origin = "1970-01-01",
                              compression="d",
                              retclass = "zoo")
  
  colnames(indexHistQ) = "Price"
  input = merge(lag(indexHistQ$Price,1),
                lag(indexHistQ$Price,2),
                lag(indexHistQ$Price,3),
                lag(indexHistQ$Price,4),
                lag(indexHistQ$Price,5),
                lag(indexHistQ$Price,6),
                lag(indexHistQ$Price,7),
                lag(indexHistQ$Price,8),
                lag(indexHistQ$Price,9),
                lag(indexHistQ$Price,10),
                all=FALSE)
  
  
  data <<- merge(input, indexHistQ$Price, all=FALSE)
  data <<- na.omit(data)
  colnames(data) = c("lag1","lag2","lag3","lag4","lag5","lag6",
                     "lag7","lag8","lag9","lag10","TARGET")
  
  
  trainIndex = 1:(nrow(data)*0.65)
  testIndex = (nrow(data)*0.65+1):(nrow(data)*0.90)
  training = as.data.frame(data[trainIndex])
  row.names(training) = NULL
  test = as.data.frame(data[testIndex])
  row.names(test) = NULL
  
  bootControl = trainControl(number = 30)
  set.seed(2)
  indexTrn = ncol(training)
  
  svmFit = train(training[-indexTrn],
                 training[,indexTrn],
                 method = "svmLinear",
                 tuneLength = 7,
                 trControl = bootControl)
  
  svmFit
  svmBest = svmFit$finalModel
  svmBest
  
  predsvm = predict(svmBest, test[,-ncol(test)])
  actualTS = test[,ncol(test)]
  predictedTS = predsvm
  mse <<- mse(actual = actualTS, predicted = predictedTS)
  mae <<- mae(actual = actualTS, predicted = predictedTS)
  
  predictedTS <<- xts(predictedTS, order.by = index(data[testIndex]))
  actualTS <<- xts(actualTS, order.by = index(data[testIndex]))
  
  
  return(svmBest)
}


#computazione del modello predittivo per ogni titolo

#TESLA
TSLA.svm = svmComput("TSLA")
mse
mae
plot(actualTS, lwd=2)
lines(predictedTS, col = "royalblue", lwd=2)
legend(x="topleft", legend=c("TSLA", "Prediction"), 
       col=c("black", "royalblue"), lty = 1:1, lwd=2)

#TOYOTA
TM.svm = svmComput("TM")
mse
mae
plot(actualTS, lwd=2)
lines(predictedTS, col = "green", lwd=2)
legend(x="topleft", legend=c("TM", "Prediction"), 
       col=c("black", "green"), lty = 1:1, lwd=2)

#EASYJET
EZJ.svm = svmComput("EZJ.L")
mse
mae
plot(actualTS, lwd=2)
lines(predictedTS, col = "violet", lwd=2)
legend(x="topleft", legend=c("EZJ", "Prediction"), 
       col=c("black", "violet"), lty = 1:1, lwd=2)

#SHELL
RDS.svm = svmComput("RDS-B")
mse
mae
plot(actualTS, lwd=2)
lines(predictedTS, col = "orange", lwd=2)
legend(x="topleft", legend=c("RDS", "Prediction"), 
       col=c("black", "orange"), lty = 1:1, lwd=2)

#BP
BP.svm = svmComput("BP")
mse
mae
plot(actualTS, lwd=2)
lines(predictedTS, col = "gold", lwd=2)
legend(x="topleft", legend=c("BP", "Prediction"), 
       col=c("black", "gold"), lty = 1:1, lwd=2)

rm(mse)
rm(mae)
rm(actualTS)
rm(predictedTS)













#------------------------------------------------------------










#metodo di download 1
getSymbols("HPQ", src="yahoo")
input = merge(lag(HPQ$HPQ.Close,1),
              lag(HPQ$HPQ.Close,2),
              lag(HPQ$HPQ.Close,3),
              lag(HPQ$HPQ.Close,4),
              lag(HPQ$HPQ.Close,5),
              lag(HPQ$HPQ.Close,6),
              lag(HPQ$HPQ.Close,7),
              lag(HPQ$HPQ.Close,8),
              lag(HPQ$HPQ.Close,9),
              lag(HPQ$HPQ.Close,10),
              all=FALSE)

data = merge(input, HPQ$HPQ.Close, all=FALSE)
data = na.omit(data)
colnames(data) = c("lag1",
                   "lag2",
                   "lag3",
                   "lag4",
                   "lag5",
                   "lag6",
                   "lag7",
                   "lag8",
                   "lag9",
                   "lag10",
                   "TARGET")

#metodo di download 2
HPQ = get.hist.quote(instrument = "HPQ", start = "2000-01-01", quote="AdjClose",
                     provider = "yahoo", origin = "1970-01-01", compression="d",
                     retclass = "zoo")
colnames(HPQ) = "HPQ"
input = merge(lag(HPQ$HPQ,1),
              lag(HPQ$HPQ,2),
              lag(HPQ$HPQ,3),
              lag(HPQ$HPQ,4),
              lag(HPQ$HPQ,5),
              lag(HPQ$HPQ,6),
              lag(HPQ$HPQ,7),
              lag(HPQ$HPQ,8),
              lag(HPQ$HPQ,9),
              lag(HPQ$HPQ,10),
              all=FALSE)


data = merge(input, HPQ$HPQ, all=FALSE)
data = na.omit(data)
colnames(data) = c("lag1",
                   "lag2",
                   "lag3",
                   "lag4",
                   "lag5",
                   "lag6",
                   "lag7",
                   "lag8",
                   "lag9",
                   "lag10",
                   "TARGET")

#training

trainIndex = 1:(nrow(data)*0.75)
training = as.data.frame(data[trainIndex])
row.names(training) = NULL
test = as.data.frame(data[-trainIndex])
row.names(test) = NULL

bootControl = trainControl(number = 30)
set.seed(2)
indexTrn = ncol(training)

svmFit = train(training[-indexTrn],
               training[,indexTrn],
               method = "svmRadial",
               tuneLength = 7,
               trControl = bootControl)

svmFit
svmBest = svmFit$finalModel
svmBest
 
predsvm = predict(svmBest, test[,-ncol(test)])
actualTS =test[,ncol(test)]
predictedTS = predsvm
mse(actual = actualTS, predicted = predictedTS)
mae(actual = actualTS, predicted = predictedTS)

predictedTS = xts(predictedTS, order.by = index(data[-trainIndex]))
actualTS = xts(actualTS, order.by = index(data[-trainIndex]))
plot(actualTS)
lines(predictedTS, col = "blue")
