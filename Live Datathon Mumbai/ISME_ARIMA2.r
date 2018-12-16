
library('ggplot2')
library('forecast')
library('tseries')

daily_data = read.csv('Training_data.csv', header=TRUE, stringsAsFactors=FALSE)

head(daily_data)

daily_data$Date = as.Date(daily_data$time_stamp)

ggplot(daily_data, aes(Date, daily_data$volume_sell)) + geom_line() + scale_x_date('month')  + ylab("Volume Sell") +
            xlab("")

daily_data<-daily_data[1:310,]
daily_data$Date = as.Date(daily_data$time_stamp)

ggplot(daily_data, aes(Date, daily_data$volume_sell)) + geom_line() + scale_x_date('month')  + ylab("Volume Sell") +
            xlab("")

adf_test<-adf.test(daily_data$volume_sell)
adf_test
cat("With a p-value of", as.numeric(adf_test[4]), "can accept data as", as.character(adf_test[3]))

#Time series and auto.arima
daily_data<- daily_data[seq(dim(daily_data)[1],1),]
pricearima <- ts(daily_data$volume_sell, start = c(2016,08,01), frequency = 1)
fitlnstock<-auto.arima(pricearima)
fitlnstock
plot(pricearima,type='l')
title('Price')
#exp(lnstock)

#Forecasted Values From ARIMA

# Forcast next 300 days (Test data of 298 and void gap of two days inbetween train and test data)
finalforecastvalues=forecast(fitlnstock,h=300)

#finalforecastvalues
plot(finalforecastvalues)

write.csv(finalforecastvalues, file = "MyData.csv") # Save prediction dataframe to file for later processing
