########################### Loading Required Libraries ###########################################################################################

library(forecast)
library(fpp)
library(smooth)
library(readxl)
library(imputeTS)
library(padr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(moments)

########################### Exploratory Data Analysis (EDA #######################################################################################
#reading the input data file. Reading col 1 as datae, col 2 as numeric and removing first 2 rows
delhi<-read_excel(file.choose(),sheet = "Delhi",col_types = c("date","numeric"),skip = 2)
View(delhi) #data in reverse oeder
delhi <- delhi[rev(1:nrow(delhi)),] #reversing the data entries
str((delhi))
summary(delhi) # Y column has 80 NA values

#Plotting graph of date vs pm25
plot(x= delhi$date, y= delhi$pm25,type="l") 
ggplot(data = delhi) + geom_smooth(mapping = aes(x = date, y = pm25)) #overall graph shows negative trend

# there are missing observations in the given data set. Findling out the total NA values
date_time <-pad(as.data.frame(delhi$date))
colnames(date_time) <- 'date'
updated_data<- full_join(date_time,delhi)
str(updated_data)
sum(is.na(updated_data$pm25)) # total 323 NA values

#Structure of NA values
plotNA.distribution(updated_data$pm25)
plotNA.distributionBar(updated_data$pm25)	
statsNA(updated_data$pm25)


########################### Feature Engineering - Creating Time, Date, Day and Month columns for Analysis ########################################

updated_data$Time<- format(as.POSIXct(updated_data$date,format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")
updated_data$Date<- format(as.POSIXct(updated_data$date,format="%Y:%m:%d %H:%M:%S"),"%Y:%m:%d")
updated_data$Day <- weekdays(as.Date(updated_data$dat,format ="%Y:%m:%d"))
updated_data$Month <- months(as.Date(updated_data$dat,format ="%Y:%m:%d"))

str(updated_data)

updated_data$Time <- as.factor(updated_data$Time)
updated_data$Date <- as.Date(updated_data$Date,format ="%Y:%m:%d")
updated_data$Day <- factor(updated_data$Day,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),ordered=TRUE)
updated_data$Month <- factor(updated_data$Month,levels=c("January","February","March","April"),ordered=TRUE)


str(updated_data)

ggplot(updated_data,aes(x=Time,y=pm25,fill=Time))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_boxplot()
ggplot(updated_data,aes(x=Day,y=pm25,fill=Day))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_boxplot()
ggplot(updated_data,aes(x=Month,y=pm25,fill=Month))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_boxplot()

#checking outliers in April month
April <- updated_data[2161:21047,]
boxplot(April$pm25)$out
plot(x=April$date,y=April$pm25, type = "l")  

# It is clear from the graph we have consistant lower PM2.5 values in the month of April that is the reason higher PM2.5 values are shown as outliers in the graph. 
# However these higher values are with the ramge of overall data so we can consider these value as not outliers.

########################### Missing Data Interpolation ###########################################################################################

# Deriving NA values by different algorithms and storing it in seperate columns and comparing each algorithm with 3 graphs 
# 1) entire time series data
# 2) close look at 94 NA in a row at the start of the series
# 3) close look at 62 NA in a row at the end of the series

# We have used Seasonally Splitted Missing Value Imputation (na_seasplit) from imputeTS library with interpolation, kalman, locf and ma algorithm

updated_data$na_seasplit_interpolation <- na_seasplit(updated_data$pm25,algorithm="interpolation",find_frequency=TRUE )
updated_data$na_seasplit_kalman<- na_seasplit(updated_data$pm25,algorithm = "kalman",find_frequency=TRUE )
updated_data$na_seasplit_locf<- na_seasplit(updated_data$pm25,algorithm = "locf",find_frequency=TRUE )
updated_data$na_seasplit_ma<- na_seasplit(updated_data$pm25,algorithm = "ma",find_frequency=TRUE )

########################### Interpolation Algotithm ##############################################################################################
#entire time series
ggplot()+
  geom_line(data = updated_data,aes(x=date,y=na_seasplit_interpolation,color="interpolation")) +
  geom_line(data = updated_data,aes(x=date,y=pm25,color="pm25")) 

# close look at 94 NA in a row at the start of the series
ggplot()+
  geom_line(data = updated_data[400:800,],aes(x=updated_data$date[400:800],y=updated_data$na_seasplit_interpolation[400:800],color="interpolation")) +
  geom_line(data = updated_data[400:800,],aes(x=updated_data$date[400:800],y=updated_data$pm25[400:800],color="pm25")) 

# close look at 62 NA in a row at the end of the series
ggplot()+
  geom_line(data = updated_data[2400:21047,],aes(x=updated_data$date[2400:21047],y=updated_data$na_seasplit_interpolation[2400:21047],color="interpolation")) +
  geom_line(data = updated_data[2400:21047,],aes(x=updated_data$date[2400:21047],y=updated_data$pm25[2400:21047],color="pm25")) 

########################### Kalman Algotithm #####################################################################################################
#entire time series
ggplot()+
  geom_line(data = updated_data,aes(x=date,y=na_seasplit_kalman,color="na_seasplit_kalman")) +
  geom_line(data = updated_data,aes(x=date,y=pm25,color="pm25")) 

# close look at 94 NA in a row at the start of the series
ggplot()+
  geom_line(data = updated_data[400:800,],aes(x=updated_data$date[400:800],y=updated_data$na_seasplit_kalman[400:800],color="kalman")) +
  geom_line(data = updated_data[400:800,],aes(x=updated_data$date[400:800],y=updated_data$pm25[400:800],color="pm25")) 

# close look at 62 NA in a row at the end of the series
ggplot()+
  geom_line(data = updated_data[2400:21047,],aes(x=updated_data$date[2400:21047],y=updated_data$na_seasplit_kalman[2400:21047],color="kalman")) +
  geom_line(data = updated_data[2400:21047,],aes(x=updated_data$date[2400:21047],y=updated_data$pm25[2400:21047],color="pm25")) 


########################### LOCF Algotithm #######################################################################################################
#entire time series
ggplot()+
  geom_line(data = updated_data,aes(x=date,y=na_seasplit_locf,color="na_seasplit_locf")) +
  geom_line(data = updated_data,aes(x=date,y=pm25,color="pm25")) 

# close look at 94 NA in a row at the start of the series
ggplot()+
  geom_line(data = updated_data[400:800,],aes(x=updated_data$date[400:800],y=updated_data$na_seasplit_locf[400:800],color="locf")) +
  geom_line(data = updated_data[400:800,],aes(x=updated_data$date[400:800],y=updated_data$pm25[400:800],color="pm25")) 

# close look at 62 NA in a row at the end of the series
ggplot()+
  geom_line(data = updated_data[2400:21047,],aes(x=updated_data$date[2400:21047],y=updated_data$na_seasplit_locf[2400:21047],color="locf")) +
  geom_line(data = updated_data[2400:21047,],aes(x=updated_data$date[2400:21047],y=updated_data$pm25[2400:21047],color="pm25")) 


########################### MA Algotithm #########################################################################################################
#entire time series
ggplot()+
  geom_line(data = updated_data,aes(x=date,y=na_seasplit_ma,color="na_seasplit_ma")) +
  geom_line(data = updated_data,aes(x=date,y=pm25,color="pm25"))

# close look at 94 NA in a row at the start of the series
ggplot()+
  geom_line(data = updated_data[400:800,],aes(x=updated_data$date[400:800],y=updated_data$na_seasplit_ma[400:800],color="ma")) +
  geom_line(data = updated_data[400:800,],aes(x=updated_data$date[400:800],y=updated_data$pm25[400:800],color="pm25")) 

# close look at 62 NA in a row at the end of the series
ggplot()+
  geom_line(data = updated_data[2400:21047,],aes(x=updated_data$date[2400:21047],y=updated_data$na_seasplit_ma[2400:21047],color="ma")) +
  geom_line(data = updated_data[2400:21047,],aes(x=updated_data$date[2400:21047],y=updated_data$pm25[2400:21047],color="pm25")) 

# by looking at the set of above two graphs ma algorithms is more closely predicting the missing values correctly so we are considering 
# Splitted Missing Value Imputation (na_seasplit) with moving average (ma) as a final imputation technique

########################### Model Building Required Model Data  #################################################################################

model_data <- updated_data[,c("date","na_seasplit_ma")]  # selecting final columns date and na_seasplit_ma for model building
colnames(model_data) <- c("Date","PM25") # changing the column names for better understanding

# running the Augmented Dickey-Fuller Test to check stationarity
adf.test(model_data$PM25) # data is stationary as P value is 0.01

########################### Spliting into train and test data set ###############################################################################

train <- head(model_data, 1570) #60%
test <- tail(model_data,  1047) #40%

########################### converting into msts:  Time Series ####################################################################

# converting the y variable (i.e. PM25) column into Time Series (ts)

train$PM25 <- ts( train$PM25,frequency = 24)
test$PM25 <- ts( test$PM25,frequency = 24)


########################### Seasonal Naive ######################################################################################################
fit_snaive <- snaive(train$PM25,h = 1047)

#Seasonal Naive Test Actual vs Predicted Graph
plot(test$Date,test$PM25, type="l", pch=21, col="red", xlab="Time", ylab="PM25",main = "Seasonal Naive Test Actual vs Predicted Graph")
lines(test$Date,fit_snaive$mean,pch=21, col="blue", type="l", lty=2)
legend("topleft", legend = c("Actual Graph", "Predicted Graph"), col = c("red", "blue"),pch = 21)

#Seasonal Naive Train Actual vs Fitted Graph
plot(train$Date,train$PM25, type="l", pch=21, col="red", xlab="Time", ylab="PM25",main = "Seasonal Naive Train Actual vs Predicted Graph")
lines(train$Date,fit_snaive$fitted,pch=21, col="blue", type="l", lty=2)
legend("topleft", legend = c("Actual Graph", "Predicted Graph"), col = c("red", "blue"),pch = 21)

# test RMSE
fit_snaive_test<-accuracy(as.vector(fit_snaive$mean), test$PM25)
fit_snaive_test

# tain RMSE
fit_snaive_train <- accuracy(fit_snaive)
fit_snaive_train

# model_data RMSE
fit_snaive_model_data <- accuracy(fit_snaive <- snaive(model_data$PM25))
fit_snaive_model_data

########################### Exponential smoothing ###############################################################################################
fit_es <- es(train$PM25,loss="aTMSE",  h = 1047)

#Exponential Smoothing Test Actual vs Predicted Graph
plot(test$Date,test$PM25, type="l", pch=21, col="red", xlab="Time", ylab="PM25",main = "Exponential Smoothing Test Actual vs Predicted Graph")
lines(test$Date,fit_es$forecast,pch=21, col="blue", type="l", lty=2)
legend("topleft", legend = c("Actual Graph", "Predicted Graph"), col = c("red", "blue"),pch = 21)

#Exponential Smoothing Train Actual vs Fitted Graph
plot(train$Date,train$PM25, type="l", pch=21, col="red", xlab="Time", ylab="PM25",main = "Exponential Smoothing Train Actual vs Predicted Graph")
lines(train$Date,fit_es$fitted,pch=21, col="blue", type="l", lty=2)
legend("topleft", legend = c("Actual Graph", "Predicted Graph"), col = c("red", "blue"),pch = 21)

# test RMSE
fit_es_test<-accuracy(as.vector(fit_es$forecast), test$PM25)
fit_es_test

# tain RMSE
fit_es_train <- accuracy(as.vector(fit_es$fitted), train$PM25)
fit_es_train

# model_data RMSE
fit_es_model_data <- es(model_data$PM25, interval = T,silent = F)
fit_es_data <- accuracy(as.vector(fit_es_model_data$fitted), model_data$PM25)
fit_es_data


########################### HoltWinters #########################################################################################################
fit_HoltWinters <- HoltWinters(train$PM25)
pred_HoltWinters <- forecast(fit_HoltWinters,h=1047)
fitted <- as.data.frame(fit_HoltWinters$fitted)

autoplot(forecast(fit_HoltWinters,h=1047))
#HoltWinters Test Actual vs Predicted Graph
plot(test$Date,test$PM25, type="l", pch=21, col="red", xlab="Time", ylab="PM25",main = "HoltWinters Test Actual vs Predicted Graph")
lines(test$Date,pred_HoltWinters$mean,pch=21, col="blue", type="l", lty=2)
legend("topleft", legend = c("Actual Graph", "Predicted Graph"), col = c("red", "blue"),pch = 21)

#HoltWinters Train Actual vs Predicted Graph
plot(train$Date,train$PM25, type="l", pch=21, col="red", xlab="Time", ylab="PM25",main = "HoltWinters Train Actual vs Predicted Graph")
lines(train$Date[25:1570],fitted$xhat,pch=21, col="blue", type="l", lty=2)
legend("topleft", legend = c("Actual Graph", "Predicted Graph"), col = c("red", "blue"),pch = 21)

# test RMSE
fit_HoltWinters_test<-accuracy(as.vector(pred_HoltWinters$mean), test$PM25)
fit_HoltWinters_test

# tain RMSE
fit_HoltWinters_train <- accuracy(as.vector(fit_HoltWinters$fitted), train$PM25)
fit_HoltWinters_train

# model_data RMSE
fit_HoltWinters_model_data <- HoltWinters(model_data$PM25)
fit_HoltWinters_data <- accuracy(as.vector(fit_HoltWinters_model_data$fitted), model_data$PM25)
fit_HoltWinters_data

########################### Auto Arima #########################################################################################################
fit_auto_arima <- auto.arima(train$PM25,trace = TRUE,lambda = "auto")
pred_auto_arima <- forecast(fit_auto_arima,h=1047)

autoplot(forecast(fit_auto_arima,h=1047))
#Auto Arima Test Actual vs Predicted Graph
plot(test$Date,test$PM25, type="l", pch=21, col="red", xlab="Time", ylab="PM25",main = "Auto Arima Test Actual vs Predicted Graph")
lines(test$Date,pred_auto_arima$mean,pch=21, col="blue", type="l", lty=2)
legend("topleft", legend = c("Actual Graph", "Predicted Graph"), col = c("red", "blue"),pch = 21)

#Auto Arima Train Actual vs Fitted Graph
plot(train$Date,train$PM25, type="l", pch=21, col="red", xlab="Time", ylab="PM25",main = "Auto Arima Train Actual vs Predicted Graph")
lines(train$Date,pred_auto_arima$fitted,pch=21, col="blue", type="l", lty=2)
legend("topleft", legend = c("Actual Graph", "Predicted Graph"), col = c("red", "blue"),pch = 21)

# test RMSE
fit_auto_arima_test<-accuracy(as.vector(pred_auto_arima$mean), test$PM25)
fit_auto_arima_test

# tain RMSE
fit_auto_arima_train <- accuracy(as.vector(fit_auto_arima$fitted), train$PM25)
fit_auto_arima_train

# model_data RMSE
fit_auto_arima_model_data <- auto.arima(model_data$PM25)
fit_auto_arima_data <- accuracy(as.vector(fit_auto_arima_model_data$fitted), model_data$PM25)
fit_auto_arima_data

########################### arima #############################################################################################################

model_data$PM25 %>% ggtsdisplay()    #p=1, d=0 (as peradf.test data is stationary), q=1
model_data$PM25 %>% diff(lag=24) %>% ggtsdisplay()  # P=0, D=1, Q=1 or 2

fit_arima <- arima(train$PM25, c(1,0,0),seasonal = list(order = c(0,1,1), period = 24))
pred_arima <- forecast(fit_arima,h=1047)

autoplot(forecast(fit_arima,h=1047))
#Arima Test Actual vs Predicted Graph
plot(test$Date,test$PM25, type="l", pch=21, col="red", xlab="Time", ylab="PM25",main = "Arima Test Actual vs Predicted Graph")
lines(test$Date,pred_arima$mean,pch=21, col="blue", type="l", lty=2)
legend("topleft", legend = c("Actual Graph", "Predicted Graph"), col = c("red", "blue"),pch = 21)

#Arima Train Actual vs Fitted Graph
plot(train$Date,train$PM25, type="l", pch=21, col="red", xlab="Time", ylab="PM25",main = " Arima Train Actual vs Predicted Graph")
lines(train$Date,pred_arima$fitted,pch=21, col="blue", type="l", lty=2)
legend("topleft", legend = c("Actual Graph", "Predicted Graph"), col = c("red", "blue"),pch = 21)

# test RMSE
fit_arima_test<-accuracy(as.vector(pred_arima$mean), test$PM25)
fit_arima_test

# tain RMSE
summary(fit_arima)

# model_data RMSE
fit_arima_model_data <- arima(model_data$PM25, c(1,0,1),seasonal = list(order = c(0,1,2), period = 24))
summary(fit_arima_model_data)

########################### tbats ##############################################################################################################
fit_tbats <- tbats(train$PM25)
pred_tbats <- forecast(fit_tbats,h=1047)

autoplot(forecast(fit_tbats,h=1047))
#TBATS Test Actual vs Predicted Graph
plot(test$Date,test$PM25, type="l", pch=21, col="red", xlab="Time", ylab="PM25",main = "TBATS Test Actual vs Predicted Graph")
lines(test$Date,pred_tbats$mean,pch=21, col="blue", type="l", lty=2)
legend("topleft", legend = c("Actual Graph", "Predicted Graph"), col = c("red", "blue"),pch = 21)

#TBATS Train Actual vs Fitted Graph
plot(train$Date,train$PM25, type="l", pch=21, col="red", xlab="Time", ylab="PM25",main = " TBATS Train Actual vs Predicted Graph")
lines(train$Date,pred_tbats$fitted,pch=21, col="blue", type="l", lty=2)
legend("topleft", legend = c("Actual Graph", "Predicted Graph"), col = c("red", "blue"),pch = 21)

# test RMSE
fit_tbats_test<-accuracy(as.vector(pred_tbats$mean), test$PM25)
fit_tbats_test

# tain RMSE
fit_tbats_train <- accuracy(as.vector(fit_tbats$fitted), train$PM25)
fit_tbats_train

# model_data RMSE
fit_tbats_model_data <- tbats(model_data$PM25)
fit_tbats_data <- accuracy(as.vector(fit_tbats_model_data$fitted), model_data$PM25)
fit_tbats_data

########################### nnetar #############################################################################################################
fit_nnetar <- nnetar(train$PM25)
pred_nnetar <- forecast(fit_nnetar,h=1047)

autoplot(pred_nnetar)+theme(plot.title=element_text(hjust = 0.5))
#nnetar Test Actual vs Predicted Graph
plot(test$Date,test$PM25, type="l", pch=21, col="red", xlab="Time", ylab="PM25",main = "nnetar Test Actual vs Predicted Graph")
lines(test$Date,pred_nnetar$mean,pch=21, col="blue", type="l", lty=2)
legend("topleft", legend = c("Actual Graph", "Predicted Graph"), col = c("red", "blue"),pch = 21)

# test RMSE
fit_nnetar_test<-accuracy(as.vector(pred_nnetar$mean), test$PM25)
fit_nnetar_test

# tain RMSE
fit_nnetar_train <- accuracy(as.vector(fit_nnetar$fitted), train$PM25)
fit_nnetar_train

# model_data RMSE
fit_nnetar_model_data <- nnetar(model_data$PM2)
fit_nnetar_data <- accuracy(as.vector(fit_nnetar_model_data$fitted), model_data$PM25)
fit_nnetar_data

########################### stlf ################################################################################################################
fit_stlf <- stlf(train$PM25,method = "arima", h = 1047)

autoplot(fit_stlf,main = "Forecasting by STLF Model",xlab = "Days",ylab = "PM2.5 Value")
#STLF Test Actual vs Predicted Graph
plot(test$Date,test$PM25, type="l", pch=21, col="red", xlab="Time", ylab="PM25",main = "STLF Test Actual vs Predicted Graph")
lines(test$Date,fit_stlf$mean,pch=21, col="blue", type="l", lty=2)
legend("topleft", legend = c("Actual Graph", "Predicted Graph"), col = c("red", "blue"),pch = 21)

#STLF Train Actual vs Fitted Graph
plot(train$Date,train$PM25, type="l", pch=21, col="red", xlab="Time", ylab="PM25",main = " STLF Train Actual vs Predicted Graph")
lines(train$Date,fit_stlf$fitted,pch=21, col="blue", type="l", lty=2)
legend("topleft", legend = c("Actual Graph", "Predicted Graph"), col = c("red", "blue"),pch = 21)

# test RMSE
fit_stlf_test<-accuracy(as.vector(fit_stlf$mean), test$PM25)
fit_stlf_test

# tain RMSE
fit_stlf_train <- accuracy(as.vector(fit_stlf$fitted), train$PM25)
fit_stlf_train

# model_data RMSE
fit_stlf_model_data <- stlf(model_data$PM25)
fit_stlf_data <- accuracy(as.vector(fit_stlf_model_data$fitted), model_data$PM25)
fit_stlf_data

########################### Test RMSE Values ####################################################################################################
fit_snaive_test
fit_es_test
fit_HoltWinters_test
fit_auto_arima_test
fit_arima_test
fit_tbats_test
fit_nnetar_test
fit_stlf_test


# by looking at the test, train and full data RMSE results STLF model is giving lower rmse values compared to other models

# Checking the Residual Plot of STLF Model
plot(fit_stlf$residuals)
hist(fit_stlf$residuals)
skewness(fit_stlf$residuals)

# model residuals follows normal distribution


########################### Final Model ####################################################################################################

Final_Model <- stlf(model_data$PM2,method = "arima", h = 336)
                
pred <- as.data.frame (Final_Model$mean)
pred["Date"] <- data.frame(seq(from = as.POSIXct("2018-04-20 01:00",tz="UTC"),length.out = 14*24, by = "hour"))
autoplot(forecast(Final_Model,h=168))

ggplot()+
  geom_line(data = tail(model_data,336L),aes(x = tail(Date,336L), y = tail(PM25,336L),color="Last Two Weeks Data")) +
  geom_line(data = pred,aes(x=Date, y=x,color="Predictions")) +
  labs(title=paste("PM2.5 Predictions for Next",336,"Days",sep = " ") ,x ="Day", y = "PM2.5") +
  labs(color = "LINE")+
  scale_x_datetime(date_breaks = "1 days",date_labels = "%b %d")+
  theme_light()+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme(axis.title.x = element_text(size = 20,color = "grey20")) +
  theme(axis.title.y = element_text(size = 20, color = "grey20")) +
  theme(axis.text = element_text(size = 13, color = "blue"))+
  theme(legend.title = element_text(color="black", size=20))+
  theme(plot.title = element_text(color="grey20", size=20))+
  theme(legend.text = element_text(size=15))
