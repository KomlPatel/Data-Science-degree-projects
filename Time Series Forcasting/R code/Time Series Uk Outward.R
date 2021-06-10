# Required library
#install.packages("readxl")
require("readxl")
#install.packages("forecast")
library(forecast)
#install.packages("fpp")
library(fpp)

# set Working directry 

#setwd("C:\Users\DIPESH\Documents\Final Projects\Case Studies\UK outward passengers")

# Import xls file for forcasting

UK1 <- readxl::read_xls("~/Final Projects/Case Studies/UK Outward Passengers Movement.xls",skip = 6,col_names = F)
View(UK1)
# drop 1st 6 rows 
UK <- UK[-c(1:6),]

# Change Col Name 
names(UK)[1]<-paste("Year")
names(UK)[2]<-paste("Quater")
names(UK)[3]<-paste("Ireland")
names(UK)[4]<-paste("other EU not Ireland")
names(UK)[5]<-paste("Rest of Europe and Med")
names(UK)[6]<-paste("Rest of World")
names(UK)[7]<-paste("Total")

UK <- as.data.frame(UK)
View(UK)

# Convert data into time series data

UK_ireland <- ts(UK$Ireland, start=c(1996,1), end=c(2005,4), frequency=4)
UK_other_eu <- ts(UK$`other EU not Ireland`, start=c(1996,1), end=c(2005,4), frequency=4)
UK_rest_eu_med <- ts(UK$`Rest of Europe and Med`, start=c(1996,1), end=c(2005,4), frequency=4)
UK_rest_world <- ts(UK$`Rest of World`, start=c(1996,1), end=c(2005,4), frequency=4)
UK_total <- ts(UK$Total, start=c(1996,1), end=c(2005,4), frequency=4)

# Printing the variables of data
print(UK_ireland)
print(UK_other_eu)
print(UK_rest_eu_med)
print(UK_rest_world)
print(UK_total)

# plot Graphes of each variable
plot(UK_ireland)
plot(UK_other_eu)
plot(UK_rest_eu_med)
plot(UK_rest_world)
plot(UK_total)

# Forcasting data Using stl function 

stl_ireland <-  stl(UK_ireland,s.window="periodic")
stl_other_eu <-  stl(UK_other_eu,s.window="periodic")
stl_rest_eu_med <-  stl(UK_rest_eu_med,s.window="periodic")
stl_rest_world <-  stl(UK_rest_world,s.window="periodic")
stl_total <-  stl(UK_total,s.window="periodic")

# Graphes for decomposed variables
plot(stl_ireland)
plot(stl_other_eu)
plot(stl_rest_eu_med)
plot(stl_rest_world)
plot(stl_total)

# Checking accuracy of the model:

accuracy(forecast(stl_ireland,h=2))
accuracy(forecast(stl_other_eu,h=2))
accuracy(forecast(stl_rest_eu_med,h=2))
accuracy(forecast(stl_rest_world,h=2))
accuracy(forecast(stl_total,h=2))

# Forecasting variables for next 3 years

forecast(stl_ireland, h=12)
forecast(stl_other_eu,h=12)
forecast(stl_rest_eu_med,h=12)
forecast(stl_rest_world,h=12)
forecast(stl_total,h=12)

# Graphes of forecasted variables by stl(decomposition)
plot(forecast(stl_ireland))
plot(forecast(stl_other_eu))
plot(forecast(stl_rest_eu_med))
plot(forecast(stl_rest_world))
plot(forecast(stl_total))

# Using ets function (smoothening method) to forecast
ets_ireland <- ets(UK_ireland)
ets_other_eu <- ets(UK_other_eu)
ets_rest_eu_med <- ets(UK_rest_eu_med)
ets_rest_world <- ets(UK_rest_world)
ets_total <- ets(UK_total)

# Checking accuracy of the model (ETS-smoothening)
accuracy(ets_ireland$fitted,UK_ireland)
accuracy(ets_other_eu$fitted,UK_other_eu)
accuracy(ets_rest_eu_med$fitted,UK_rest_eu_med)
accuracy(ets_rest_world$fitted,UK_rest_world)
accuracy(ets_total$fitted,UK_total)

# Forecasting variables for next 3 years
forecast(ets_ireland,h=12)
forecast(ets_other_eu,h=12)
forecast(ets_rest_eu_med,h=12)
forecast(ets_rest_world,h=12)
forecast(ets_total,h=12)

# Graphes of forecasted values by ets function 
plot(forecast(ets_ireland))
plot(forecast(ets_other_eu))
plot(forecast(ets_rest_eu_med))
plot(forecast(ets_rest_world))
plot(forecast(ets_total))

# Forcasting data using auto.arima function

arima_ireland <- auto.arima(UK_ireland)
arima_other_eu <- auto.arima(UK_other_eu)
arima_rest_eu_med <- auto.arima(UK_rest_eu_med)
arima_rest_world <- auto.arima(UK_rest_world)
arima_total <- auto.arima(UK_total)

# Checking accuracy of the model:
accuracy(arima_ireland)
accuracy(arima_other_eu)
accuracy(arima_rest_eu_med)
accuracy(arima_rest_world)
accuracy(arima_total)

# Forecasting variables for next 3 years
forecast(arima_ireland,h=12)
forecast(arima_other_eu,h=12)
forecast(arima_rest_eu_med,h=12)
forecast(arima_rest_world,h=12)
forecast(arima_total,h=12)

# Graph of forecasted values by arima: 
plot(forecast(arima_ireland))
plot(forecast(arima_other_eu))
plot(forecast(arima_rest_eu_med))
plot(forecast(arima_rest_world))
plot(forecast(arima_total))



