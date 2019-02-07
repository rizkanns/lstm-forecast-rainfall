library(keras)
library(tensorflow)
#source : http://rwanjohi.rbind.io/2018/04/05/time-series-forecasting-using-lstm-in-r/


#setwd("D:/Kuliah/TA/workspace")
#load("D:/Kuliah/TA/workspace/a_lstm_prediction.RData")

# colnames(data)<-c("smin","smaks","sr","kr","ch","lp","kar","kat",
#                   "nino12","nino3","nino34","nino4","ch1","ch2")
# colnames(ya)<-c("Asli","Prediksi")

# rm(data)
data <- read.csv("csv-harian-perak1.csv", header = TRUE, sep = ",")
data[is.na(data)] <- 0
# data <- data.frame(data[,3:8])
# data <- data.frame(data[,-2])
# data <- data.frame(data[,5])
data <- data.matrix(data[,-1])
data <- data.matrix(data[,-1])
data <- data.matrix(data[,-1])
# data <- data.matrix(data[,-5])
data <- (data - min(data)) / (max(data) - min(data))

# data$Tanggal <- strptime(as.character(data$Tanggal), "%d/%m/%Y")
# format(data$Tanggal, "%Y-%m-%d")
# data$Tanggal = as.Date(data$Tanggal)


# library(ggplot2)
# ggplot(data.frame(data), aes(x = 1:nrow(data), y = `ch`)) + geom_line()
# ggplot(data.frame(data[1:n,]), aes(x = Tanggal, y = `ch`)) + scale_x_date('day') +  geom_line()
# ggplot(data.frame(data[n+1:n+366,]), aes(x = Tanggal, y = `ch`)) + scale_x_date('day') +  geom_line()
# 
# ya <- data.frame(ya)
# data <- data.frame(data)
# 
# ggplot() +
#   geom_line(data = ya, aes(x = 1:nrow(ya), y = Asli, colour = "Asli")) +
#   geom_line(data = ya, aes(x = 1:nrow(ya), y = Prediksi,   colour = "Prediksi"))  +
#   ylab('Daily Rainfall')
# 
# ggplot() +
#   geom_line(data = ya, aes(x = 1:nrow(ya), y = Asli, colour = "Asli")) +
#   geom_line(data = ya, aes(x = 1:nrow(ya), y = Prediksi,   colour = "Prediksi"))  +
#   ylab('Daily Rainfall')

# library(data.table)
# data = data.table(data)
# lag<-data[, shift(.SD, 1, 0, "lag", TRUE), .SDcols=1:12]
# lag[is.na(lag)] <- 0
# datalag<-cbind(data,lag)


# transform data to stationarity
# diffed = diff(data, differences = 1)
# head(diffed)


# k=1
# # create a lagged dataset, i.e to be supervised learning
# lags <- function(x, k)
# {
#   lagged =  c(rep(NA, k), x[1:(length(x)-k)])
#   DF = as.data.frame(cbind(lagged, x))
#   colnames(DF) <- c( paste0('x-', k), 'x')
#   DF[is.na(DF)] <- 0
#   return(DF)
# }


## split into train and test sets
# supervised = lags(diffed, 1)
# head(supervised)
supervised <- data

N = nrow(supervised)
#n = round(N *0.8, digits = 0)
n = 1825

train = supervised[1:n, ]
test  = supervised[(n+1):(n+366),  ]


## scale data
normalize <- function(train, test, feature_range = c(0, 1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  
  std_train = ((x - min(x) ) / (max(x) - min(x)  ))
  std_test  = ((test - min(x) ) / (max(x) - min(x)  ))
  
  scaled_train = std_train *(fr_max -fr_min) + fr_min
  scaled_test = std_test *(fr_max -fr_min) + fr_min
  
  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
  
}


## inverse-transform
# inverter = function(scaled, scaler, feature_range = c(0, 1)){
#   min = scaler[1]
#   max = scaler[2]
#   n = length(scaled)
#   mins = feature_range[1]
#   maxs = feature_range[2]
#   inverted_dfs = numeric(n)
#   
#   for( i in 1:n){
#     X = (scaled[i]- mins)/(maxs - mins)
#     rawValues = X *(max - min) + min
#     inverted_dfs[i] <- rawValues
#   }
#   return(inverted_dfs)
# }


# Scaled = normalize(train, test, c(-1, 1))

#y_train = Scaled$scaled_train[, 2]
#x_train = Scaled$scaled_train[, 1]

y_train = data.matrix(train[, 5])
x_train = data.matrix(train[, -5])

#y_test = Scaled$scaled_test[, 2]
#x_test = Scaled$scaled_test[, 1]

y_test = data.matrix(test[, 5])
x_test = data.matrix(test[, -5])

## fit the model
# dim(x_train) <- c(length(x_train), 1, 1)
# dim(x_train)
# X_shape2 = dim(x_train)[2]
# X_shape3 = dim(x_train)[3]
batch_size = 1

# x_train <- array_reshape(x = x_train, dim = c(length(x_train), 1, 1))
# x_test <- array_reshape(x = x_test, dim = c(length(x_test), 1, 1))
x_train <- array_reshape(x = x_train, dim = list(nrow(x_train), 1, 11))
x_test <- array_reshape(x = x_test, dim = list(nrow(x_test), 1, 11))
#y_test <- array_reshape(x = y_test, dim = list(nrow(y_test), 1, 11))
#x_train <- array_reshape(x = x_train, dim = list(n, 1, 1))
#y_test <- array_reshape(x = x_test, dim = list(N-n, 1, p))

p <- 11
units = 32
timesteps = 1

rm(model)

# stacked layer lstm
model <- keras_model_sequential() 
model %>% 
  layer_lstm(units, return_sequences = TRUE, input_shape = c(timesteps, p)) %>% 
  layer_lstm(units, return_sequences = TRUE) %>% 
  layer_lstm(units) %>%
  layer_dense(units=1,activation = 'sigmoid')

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.0005 , decay = 1e-6 ),
  metrics = c('mae')
)

history <- model %>% fit( 
  x_train, 
  y_train,
  batch_size = 32,
  epochs = 10,
  verbose=1,
  shuffle=FALSE,
  validation_data = list(x_test, y_test),
  validation_split = 0.1
)

# lstm biasa
# model <- keras_model_sequential() 
# model%>%
#   layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE)%>%
#   layer_dense(units = 1)



# model %>% compile(
#   optimizer = optimizer_rmsprop(lr = 0.002),
#   loss = 'mse'
# )

summary(model)

# nb_epoch = 5   
# for(i in 1:nb_epoch ){
#   model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
#   model %>% reset_states()
# }

None = 1
rm(None)
L = length(y_test)
# scaler = Scaled$scaler
predictions = numeric(L)

scores <- predict_on_batch(model, x_test)
scores
score <- model %>% evaluate(x_test, y_test, batch_size=batch_size)
score
# for(i in 1:L){
#   X = y_test[i]
#   dim(Y) = c(1,1,1)
#   X = array_reshape(x = x_test, dim = list(nrow(x_test), 1, 11))
#   #yhat = model %>% evaluate(x_test, y_test, batch_size=batch_size)
#   yhat = model %>% predict(X, verbose=None, steps=None)
#   # invert scaling
#   #yhat = inverter(yhat, scaler,  c(-1, 1))
#   # invert differencing
#   yhat  = yhat + data[(n+i)]
#   # store
#   predictions[i] <- yhat
# }
# 
# plot(predictions)
# 
# ya <- cbind(x_test,predictions)
# 
# head(ya)
# ya <- data.frame(ya)
# 
# colnames(ya)<-c("Asli","Prediksi")
# 
# ggplot() +
#    geom_line(data = ya, aes(x = 1:nrow(ya), y = Asli, colour = "Asli")) +
#    geom_line(data = ya, aes(x = 1:nrow(ya), y = Prediksi,   colour = "Prediksi"))  +
#    ylab('Daily Rainfall')

rmse <- function(a, p){
  sqrt(mean((a - p)^2))
}

rmse(y_test, scores)
