# Get the data points in form of a R vector.

rainfall <- c(799,1174.8,865.1,1334.6,635.4,918.5,685.5,998.6,784.2,985,882.8,1071)

time_Data<-ts(rainfall,start = c(2012,1),frequency = 12)

View(time_Data)

# Print the timeseries data.

print(time_Data)

summary(time_Data)

time(time_Data)

cycle(time_Data)

plot(time_Data,
     type = "b", 
     lty = 1,
     lwd=2,
     col='red')

abline(reg=lm(time_Data~time(time_Data)))

boxplot(time_Data~cycle(time_Data))

acf(time_Data)

pacf(time_Data)

lag(rainfall,3)


myts <- ts(rainfall, start=c(2009, 1), end=c(2014, 12), frequency=12)
myts
# subset the time series (June 2014 to December 2014)
myts2 <- window(myts, start=c(2014, 6), end=c(2014, 12))

trends_1<-stl(myts,s.window="period")

plot(trends_1)

install.packages('TTR')
library(TTR)

trends_2<-SMA(myts,n=3)
trends_2
plot(trends_2)
trends_2_1<-decompose(myts)

plot(trends_2_1)

install.packages('')

rainseriesforecasts <- HoltWinters(myts, beta=FALSE, gamma=FALSE)
rainseriesforecasts
