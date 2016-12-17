library("PerformanceAnalytics")
library(forecast)
library(ggplot2)

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plot_model <- function(model_fitted,predictions){
  d1 <- data.frame(c(model_fitted,prediction$mean), temp$Books_issued,as.Date(temp$X))
  names(d1) <- c("Fitted", "Actual", "Date")
  
  posterior.interval <- cbind.data.frame(
    prediction$lower[,1],
    prediction$upper[,1],
    prediction$lower[,2],
    prediction$upper[,2],
    d1[d1$Date >= "2016-01-01",3])
  names(posterior.interval) <- c("LL_80", "UL_80","LL_95","UL_95", "Date")
  
  d2 <- merge(x=d1,y=posterior.interval,on="Date",all.x = TRUE)
  
  ggplot(data=d2, aes(x=Date)) +
    geom_ribbon(aes(ymin=LL_80, ymax=UL_80), fill="grey", alpha=0.7) +
    geom_ribbon(aes(ymin=LL_95, ymax=UL_95), fill="grey", alpha=0.4) +
    geom_line(aes(y=Actual, colour = "Actual"), size=1.2) +
    geom_line(aes(y=Fitted, colour = "Fitted"), size=1.2, linetype=2) +
    theme_bw() + theme(legend.title = element_blank()) + 
    ylab("") + xlab("") +
    geom_vline(xintercept=as.numeric(as.Date("2016-01-01")), linetype=2) +
    ggtitle(paste0("ARIMA - Language and Literature -- Holdout MAPE = ",round(100*MAPE(test_data,prediction$mean),2), "% -- RMSE = ",round(RMSE(test_data,prediction$mean),2))) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0))
}


MAPE <- function(y,yhat){
  mean(abs(y-yhat)/y)
}

RMSE <- function(y,yhat)
{
  sqrt(mean((y-yhat)**2))
}

#---------------------------------loading data----------------------------------
data <- read.csv("EECS-731 Intro to DataScience/Final Project/cleaned_data.csv")

#-----------------------------data pre-processing-------------------------------
temp <- data[data$genre == 'P',c(1,2)]
temp$X <- as.Date(temp$X,format = "%Y-%m-%d")
train_data <- temp[(temp$X >= "2012-01-01") & (temp$X < "2016-01-01"),]
train_data <- ts(train_data$Books_issued,start = c(2012,26),frequency=52)
test_data <- temp[(temp$X >= "2016-01-01"),]
test_data <- ts(test_data$Books_issued,start = c(2016,1),frequency=52)

#-----------------Looking for trend,seasonality and levels----------------------
plot(decompose(train_data))
plot(train_data,type='l')

#----------------------------calculating p,d,q values---------------------------
plot(diff(train_data, differences=1))
acf(diff(train_data, differences=1),lag.max = 20)
pacf(diff(train_data, differences=1),lag.max = 20)
Acf(diff(train_data, differences=1),plot = FALSE)
Pacf(diff(train_data, differences=1))

#------------------------------Building ARIMA model-----------------------------
#auto.arima(train_data)
auto.arima.model <- auto.arima(train_data)
arima.model <- arima(train_data,order = c(0,1,1),seasonal = list(order = c(0,1,1),period = 52))

#----------------------------Calculating predictions----------------------------
prediction <- forecast(arima.model,h=27)
auto.prediction <- forecast(auto.arima.model,h=27)
plot_model( arima.model$residuals + train_data ,predictions)
d1 <- data.frame(c(fitted(arima.model),prediction$mean), temp$Books_issued,as.Date(temp$X), c(c(1:52)*0,model.holt.winter$fitted[,1],holt.prediction$mean))
names(d1) <- c("Arima_fitted_pred","Actual","Date","HW_fitted_pred")
write.csv(d1,"/Users/Pranav/Documents/EECS-731 Intro to DataScience/Final Project/Weekly_results.csv")

length(prediction$mean)

length(model.holt.winter$fitted)

#-------------------------------Model Validation--------------------------------
Box.test(prediction$residuals,type = "Ljung-Box")
plot.ts(prediction$residuals)
plotForecastErrors(prediction$residuals)

#-------------------------------Building HW model-------------------------------
model.holt.winter <- HoltWinters(train_data)

#----------------------------Calculating predictions----------------------------
holt.prediction <- forecast(model.holt.winter,h=27)
plot(holt.prediction)
lines(test_data,col='red')
acf(holt.prediction$residuals,lag.max = 20,na.action = na.contiguous)

RMSE(test_data,holt.prediction$mean)
MAPE(test_data,holt.prediction$mean)

#-------------------------------Model Validation--------------------------------
Box.test(holt.prediction$residuals,type = "Ljung-Box")
plot.ts(holt.prediction$residuals)
plotForecastErrors(holt.prediction$residuals)





#---------------Resources----------------------
#https://people.duke.edu/~rnau/seasarim.htm
#https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html



#--------------------------------References-----------------------------

#ts.plot(train_data,ts(holt.prediction$mean,start = c(2016,1),frequency = 52),col=c("black",'blue'))
#lines(test_data,col='red')
#polygon(c(time(test_data),rev(time(test_data))), c(holt.prediction$upper[,1],rev(prediction$lower[,1])), 
#        col=rgb(0,0,0.6,0.2), border=FALSE)
#polygon(c(time(test_data),rev(time(test_data))), c(holt.prediction$upper[,2],rev(prediction$lower[,2])), 
#        col=rgb(0,0,1,0.2), border=FALSE)
