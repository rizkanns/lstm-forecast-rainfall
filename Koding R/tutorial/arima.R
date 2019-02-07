library('ggplot2')
library('forecast')
library('tseries')

# prepare data
daily_data = read.csv("csv-harian-perak1.csv", header=TRUE, stringsAsFactors=FALSE)
head(daily_data)

# make a date for daily
daily_data$Tanggal <- strptime(as.character(daily_data$Tanggal), "%d/%m/%Y")
format(daily_data$Tanggal, "%Y-%m-%d")
daily_data$Tanggal = as.Date(daily_data$Tanggal)

# make daily rainfall plot
ggplot(daily_data, aes(Tanggal, Curah.Hujan)) + geom_line() + scale_x_date('day')  + ylab("Daily Rainfall") +
  xlab("")

# make a clean plot
count_ts = ts(daily_data[, c('Curah.Hujan')])

daily_data$Clean_CH = tsclean(count_ts)

ggplot() +
  geom_line(data = daily_data, aes(x = Tanggal, y = Clean_CH)) + ylab('Cleaned Daily Rainfall')

# plot with different color
daily_data$ch = ma(daily_data$Clean_CH, order=7) # using the clean count with no outliers
daily_data$ch_30 = ma(daily_data$Clean_CH, order=30)

ggplot() +
  geom_line(data = daily_data, aes(x = Tanggal, y = Clean_CH, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = Tanggal, y = ch,   colour = "Data Mingguan"))  +
  geom_line(data = daily_data, aes(x = Tanggal, y = ch_30, colour = "Data Bulanan"))  +
  ylab('Daily Rainfall')

# decomposisi
count_ma = ts(na.omit(daily_data$ch), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

# stationary
adf.test(count_ma, alternative = "stationary")

# auto corelation
Acf(count_ma, main='')

Pacf(count_ma, main='')

count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

auto.arima(deseasonal_cnt, seasonal=FALSE)

fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')

fit2 = arima(deseasonal_cnt, order=c(1,1,7))

fit2

tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

arima(x = deseasonal_cnt, order = c(1, 1, 7))

fcast <- forecast(fit2, h=30)
plot(fcast)

hold <- window(ts(deseasonal_cnt), start=700)

fit_no_holdout = arima(ts(deseasonal_cnt[-c(700:725)]), order=c(1,1,7))

fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))

fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality

seas_fcast <- forecast(fit_w_seasonality, h=30)
plot(seas_fcast)
