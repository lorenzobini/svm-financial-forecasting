#TODO
#Rivedere dyagraph
#Considerare performance analytics chart (ultima pag slides 6)


library(xts)
library(quantmod)
library(tseries)
library(dygraphs)
library(PerformanceAnalytics)
library(caret)
library(kernlab)
library(Metrics)

#downloading financial data------------------

symbols = c("TSLA", "TM", "RDS-B", "BP")

for(i in 1:length(symbols)){
  ddata = get.hist.quote(instrument = symbols[i], start = "2007-10-01", end = "2017-10-31", 
                        quote = "AdjClose" , provider = "yahoo", origin = "1970-01-01", 
                        compression = "d", reclass("zoo"))
  index(ddata) = as.yearmon(index(ddata))
  if(i==1) TSLA = ddata
  if(i==2) TM = ddata
  if(i==3) RDS = ddata
  if(i==4) BP = ddata
}


#Easyjet presenta valori mensili ripetuti per il marzo di ogni anno
#e non presenta il mese di ottobre di ogni anno
#Provvedo a scaricarlo settimanalmente e convertire i valori

EZJ = get.hist.quote(instrument = "EZJ.L", start = "2007-10-01", end = "2017-10-31", 
                     quote = "AdjClose" , provider = "yahoo", origin = "1970-01-01", 
                     compression = "w", reclass("zoo"))
EZJ = to.monthly(EZJ)[,4]


rm(i)
rm(symbols)
rm(ddata)


#---------------end downloading financial data


#simple and compounded returns computation-------------

TSLA.sr <- na.omit(CalculateReturns(TSLA[endpoints(TSLA, on = "months")], method = "simple"))
TSLA.cr <- na.omit(CalculateReturns(TSLA[endpoints(TSLA, on = "months")], method = "compound"))

TM.sr <- na.omit(CalculateReturns(TM[endpoints(TM, on = "months")], method = "simple"))
TM.cr <- na.omit(CalculateReturns(TM[endpoints(TM, on = "months")], method = "compound"))

EZJ.sr <- na.omit(CalculateReturns(EZJ[endpoints(EZJ, on = "months")], method = "simple"))
EZJ.cr <- na.omit(CalculateReturns(EZJ[endpoints(EZJ, on = "months")], method = "compound"))

RDS.sr <- na.omit(CalculateReturns(RDS[endpoints(RDS, on = "months")], method = "simple"))
RDS.cr <- na.omit(CalculateReturns(RDS[endpoints(RDS, on = "months")], method = "compound"))

BP.sr <- na.omit(CalculateReturns(BP[endpoints(BP, on = "months")], method = "simple"))
BP.cr <- na.omit(CalculateReturns(BP[endpoints(BP, on = "months")], method = "compound"))

#--------------------------------end return computation


#comparison plot on compounded monthly returns-----------

compoundedRet <- merge(TSLA.cr, TM.cr, EZJ.cr,RDS.cr, BP.cr)
colnames(compoundedRet) <- c("TSLA", "TM", "EZJ", "RDS", "BP")
dygraph(compoundedRet, ylab = "Ritorni Composti") %>% 
  dyRangeSelector() %>%
  dySeries("TSLA", label="TESLA", color = "royalblue") %>%
  dySeries("TM", label="TOYOTA", color = "limegreen") %>%
  dySeries("EZJ", label="EASYJET", color = "violet") %>%
  dySeries("RDS", label="SHELL", color = "gold") %>%
  dySeries("BP", color = "orange")

#--------------------------------------end comparison plot


#diagnostic plots-------------------------------
 
breakpoints = c(-0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3 ,0.4 ,0.5 ,0.6)


#TESLA
par(mfrow =c(2,2))
hist(TSLA.cr, main="Ritorni mensili di Tesla", xlab="TSLA",
     col="royalblue", breaks = breakpoints)
plot(density(TSLA.cr), main="Ritorni mensili di Tesla", xlab = "",
     col="royalblue", lwd=2)
qqnorm(TSLA.cr, main = "Ritorni mensili di Tesla", col = "royalblue")
qqline(TSLA.cr)
boxplot(coredata(TSLA.cr), main = "Ritorni mensili di Tesla",
        col = "royalblue", names = "TSLA")
par(mfrow=c(1,1))

#TOYOTA
par(mfrow=c(2,2))
hist(TM.cr, main="Ritorni mensili di Toyota", xlab="TM",
     col="limegreen", breaks = breakpoints)
plot(density(TM.cr), main="Ritorni mensili di Toyota", xlab = "",
     col="limegreen", lwd=2)
qqnorm(TM.cr, main = "Ritorni mensili di Toyota", col = "limegreen")
qqline(TM.cr)
boxplot(coredata(TM.cr), main = "Ritorni mensili di Toyota",
        col = "limegreen", names = "TM")
par(mfrow=c(1,1))

#BP
par(mfrow=c(2,2))
hist(BP.cr, main="Ritorni mensili di BP", xlab="BP",
     col="orange", breaks = breakpoints)
plot(density(BP.cr), main="Ritorni mensili di BP", xlab = "",
     col="orange", lwd=2)
qqnorm(BP.cr, main = "Ritorni mensili di BP", col = "orange")
qqline(BP.cr)
boxplot(coredata(BP.cr), main = "Ritorni mensili di BP",
        col = "orange", names = "BP")
par(mfrow=c(1,1))

#RDS
par(mfrow=c(2,2))
hist(RDS.cr, main="Ritorni mensili di Shell", xlab="RDS",
     col="gold", breaks = breakpoints)
plot(density(RDS.cr), main="Ritorni mensili di Shell", xlab = "",
     col="gold", lwd=2)
qqnorm(RDS.cr, main = "Ritorni mensili di Shell", col = "gold")
qqline(RDS.cr)
boxplot(coredata(RDS.cr), main = "Ritorni mensili di Shell",
        col = "gold", names = "RDS")
par(mfrow=c(1,1))

#EASYJET
par(mfrow=c(2,2))
hist(EZJ.cr, main="Ritorni mensili di Easyjet", xlab="EZJ",
     col="violet", breaks = breakpoints)
plot(density(EZJ.cr), main="Ritorni mensili di Easyjet", xlab = "",
     col="violet", lwd=2)
qqnorm(EZJ.cr, main = "Ritorni mensili di Easyjet", col = "violet")
qqline(EZJ.cr)
boxplot(coredata(EZJ.cr), main = "Ritorni mensili di Easyjet",
        col = "violet", names = "EZJ")
par(mfrow=c(1,1))

rm(breakpoints)

#------------------end diagnostic plots

#descriptive statistics----------------
descStat = function(x) {
  result = data.frame(mean = mean(x),
                      variance = var(x),
                      standard_dev = sd(x),
                      skewness = skewness(x),
                      kurtosis = kurtosis(x),
                      quantile1percent = qnorm(p=0.01, mean=mean(x), sd=sd(x)),
                      quantile5percent = qnorm(p=0.05, mean=mean(x), sd=sd(x)))
  return(result)
}
TSLAstat = descStat(TSLA.cr)
TMstat = descStat(TM.cr)
BPstat = descStat(BP.cr)
RDSstat = descStat(RDS.cr)
EZJstat = descStat(EZJ.cr)

summaryStat = rbind(TSLAstat,TMstat,BPstat,RDSstat,EZJstat)
row.names(summaryStat) <- c("TSLA","TM","BP","RDS","EZJ")
summaryStat

#-----------end descriptive statistics

#covariance matrix and correlation matrix-------------

dataMerged = cbind(tail(coredata(TSLA.cr), 88),
                   tail(coredata(TM.cr), 88),
                   tail(coredata(BP.cr), 88),
                   tail(coredata(RDS.cr), 88),
                   tail(coredata(EZJ.cr), 88))
colnames(dataMerged) = c("TSLA", "TM", "BP","RDS","EZJ")

covMat = cov(dataMerged)
covMat
corMat = cor(dataMerged)
corMat

#pairwise scatterplot
pairs(dataMerged, 
      col="royalblue",
      pch=18,
      cex=1.5,
      cex.axis=1.5)

#---------end covariance matrix and correlation matrix

#forecasting based on prices--------------------------

tsr <- function(x, desc){
  data = stl(x[,1], s.window = "period")
  plot(data, main = desc)
  return(data)
}
TSLA.tsr = tsr(TSLA, "TSLA")
TM.tsr = tsr(TM, "TM")
BP.tsr = tsr(BP, "BP")
RDS.tsr = tsr(RDS,"RDS")
EZJ.tsr = tsr(EZJ, "EZJ")

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

