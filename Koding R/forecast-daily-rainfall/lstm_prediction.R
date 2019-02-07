library(keras)
library(tensorflow)
library(ggplot2)
library(data.table)
library(mlbench)
library(caret)
#source : http://rwanjohi.rbind.io/2018/04/05/time-series-forecasting-using-lstm-in-r/


setwd("D:/Kuliah/TA/workspace")
load("D:/Kuliah/TA/workspace/a_lstm_prediction.RData")

# colnames(test)<-c("smin","smaks","sr","kr","ch","lp","kar","kat", "nino12","nino3","nino34","nino4")
# colnames(ya)<-c("Asli","Prediksi")

# rm(data)
# target <- read.csv("csv-harian-perak1.csv", header = TRUE, sep = ",")
# target[is.na(target)] <- 0
# target <- data.matrix(target[,5])
# 
# data <- read.csv("opt.3perak1-linier-2.csv", header = TRUE, sep = ",")
# data[is.na(data)] <- 0
# data <- data.matrix(data[,-1])
# # data <- data.matrix(data[,-5])
# data <- (data - min(data)) / (max(data) - min(data))

data <- read.csv("csv-harian-perak1.csv", header = TRUE, sep = ",")
data[is.na(data)] <- 0
data <- data.matrix(data[,-1])
data <- data.matrix(data[,-1])
data <- data.matrix(data[,-1])


# calculate correlation matrix
correlationMatrix <- cor(data[,])
# summarize the correlation matrix
print(correlationMatrix)
plot(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

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
# lag<-data[, shift(.SD, 1, 0, "lag", TRUE), .SDcols=5]
# lag[is.na(lag)] <- 0
# datalag<-cbind(data,lag)
# datalag <- (datalag - min(datalag)) / (max(datalag) - min(datalag))

# # transform data to stationarity
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


# split into train and test sets
# supervised = lags(diffed, 12)
# head(supervised)
supervised <- data

N = nrow(supervised)
#n = round(N *0.8, digits = 0)
n = 1825

train = supervised[c(1:1095,1461:1825), ]
test  = supervised[1096:1460,  ]

# n = 1825
# 
# train = supervised[(1:n), ]
# test  = supervised[n+1:n+365,  ]

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


# inverse-transform
inverter = function(scaled, scaler, feature_range = c(0, 1)){
  min = scaler[1]
  max = scaler[2]
  n = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(n)

  for( i in 1:n){
    X = (scaled[i]- mins)/(maxs - mins)
    rawValues = X *(max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}


#Scaled = normalize(train, test, c(-1, 1))

#y_train = Scaled$scaled_train[, 5]
#x_train = Scaled$scaled_train[, c(3,4,6,7)]

y_train = data.matrix(train[,5])
x_train = data.matrix(train[,c(3,4,6,7)])

#y_test = Scaled$scaled_test[, 5]
#x_test = Scaled$scaled_test[, c(3,4,6,7)]

y_test = data.matrix(test[,5])
x_test = data.matrix(test[,c(3,4,6,7)])

## fit the model
# dim(x_train) <- c(length(x_train), 1, 1)
# dim(x_train)
# X_shape2 = dim(x_train)[2]
# X_shape3 = dim(x_train)[3]
batch_size = 1

x_train <- array_reshape(x = x_train, dim = list(nrow(x_train), 1, 4))
x_test <- array_reshape(x = x_test, dim = list(nrow(x_test), 1, 4))

p <- 4
units = 32
timesteps = 1

rm(model)
# stacked layer lstm
model <- keras_model_sequential() 
model %>% 
  layer_lstm(units, return_sequences = TRUE, input_shape = c(timesteps, p)) %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_lstm(units, return_sequences = TRUE) %>%
  layer_lstm(units) %>%
  layer_dense(units=1,activation = 'sigmoid')

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.001 , decay = 1e-6 ),
  metrics = c('mae')
)

history <- model %>% fit( 
  x_train, 
  y_train,
  batch_size = 16,
  epochs = 50,
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
#   loss = 'mse',
#   metrics = c('mae')
# )

# nb_epoch = 5   
# for(i in 1:nb_epoch ){
#   model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
#   model %>% reset_states()
# }

scores = predict_on_batch(model, x_test)
scores

#scores = inverter(scores, scaler,  c(-1, 1))
#y_test = inverter(y_test, scaler,  c(-1, 1))

score <- model %>% evaluate(x_test, y_test, batch_size=batch_size)
score

rmse <- function(a, p){
  sqrt(mean((a - p)^2))
}

rmse(y_test, scores)

plotbaru <- cbind(y_test, scores)
colnames(plotbaru)<-c("Asli","Prediksi")
plotbaru <- data.frame(plotbaru)

ggplot() +
   geom_line(data = plotbaru, aes(x = 1:nrow(plotbaru), y = Asli, colour = "Asli")) +
   geom_line(data = plotbaru, aes(x = 1:nrow(plotbaru), y = Prediksi,   colour = "Prediksi"))  +
   ylab('Daily Rainfall')

#Load Train and Test datasets
#Identify feature and response variable(s) and values must be numeric and numpy arrays
# x_train <- input_variables_values_training_datasets
# y_train <- target_variables_values_training_datasets
# x_test <- input_variables_values_test_datasets
# x <- data.frame(x)
# x_test <- data.frame(x_test)
# x_train <- data.frame(x_train)
# y_train <- data.frame(y_train)
# x <- cbind(x_train,y_train)
# # Train the model using the training sets and check score
# linear <- lm(y_train ~ ., data = x)
# summary(linear)
# #Predict Output
# predicted = predict(linear,x_test)
# 
# plot( y_train, y1, type="l", col="red" )
# par(new=TRUE)
# plot( x, y2, type="l", col="green" )

# plotbaru <- rbind(y_train, y_test)
