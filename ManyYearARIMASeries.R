library(data.table)
DT <- read.csv(file = "D:\\MyStuff\\8638610_meantrend.csv", header = TRUE, sep = ",")
names(DT)
DT$yr <- DT$ï..Year
DT$mth <- DT$Month
DT$WL_mth <- DT$Monthly_MSL

NewDT <- DT[337:1428,8:10]

library('ggplot2')
library('forecast')
library('tseries')


ggplot() +
  geom_line(data = NewDT, (x = NewDT$yr,y = NewDT$WL_mth) + scale_x_date(NewDT$mth)) 
DTset <- setDT(NewDT)
DTsetTrain <- DTset[yr < 2008]
table(DTsetTrain$yr)
DTsetPredict <- DTset[yr > 2007]

#Looking for seasonality, trend and error
#using seasonal adjustment to get rid of seasonality
NewDT <- DTsetTrain

count_ts = ts(NewDT[, c('WL_mth')])

NewDT$WL_mth = tsclean(count_ts)

count_ma = ts(na.omit(NewDT$WL_mth), frequency=12)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)



#Augmented Dickey-Fuller Test to check for stationarity
adf.test(count_ma, alternative = "stationary")
#autocorrelation test for data already stationary
Acf(count_ma, main='')
Pacf(count_ma, main='')

#Augmented Dickey-Fuller Test on data once differenced 
count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

#autocorrelation test for differenced
Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

auto.arima(deseasonal_cnt, seasonal = FALSE)

auto.arima(count_ma, ic="aic", trace = TRUE)

fit <- auto.arima(deseasonal_cnt, seasonal = FALSE)
tsdisplay(residuals(fit), lag.max = 45, main ='(3,1,1) Model Residuals')

fcast <- forecast(fit, h = 120)
plot(fcast)

fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal = TRUE)

fit_w_seasonality

tsdisplay(residuals(fit_w_seasonality), lag.max = 45, main = '(2,1,2)(1,0,0)[12]')


fit3 <- auto.arima(ts(deseasonal_cnt, frequency = 12), D = 1)
plot(forecast(auto.arima(ts(deseasonal_cnt,frequency=12), D=1),h=120))

plot(forecast(fit3, D=1, h=120))
fcast <- forecast:::forecast.Arima(fit3, h = 120)
fcast$mean

temp <- as.data.frame (cbind(fit4$mean, DTsetPredict$WL_mth))

temp <- as.data.frame (cbind(fcast$mean, DTsetPredict$WL_mth[1:120]))

setDT(temp)
names(temp) <- c("fitted","original")
temp <- temp[,diff:=fitted-original]

sse <- sum(temp$diff ^2)
sse

mean(temp$diff^2)
plot(temp)
