library(tibble)
library(keras)
library(tensorflow)
library(ggplot2)

# prepare data
setwd("D:/Kuliah/TA/workspace")
load("D:/Kuliah/TA/workspace/a_lstm_barubaru.RData")
data <- read.csv("opt.3perak1-linier-2.csv", header = TRUE, sep = ",")
data <- read.csv("opt.3perak2-linier-2.csv", header = TRUE, sep = ",")
data <- read.csv("opt.3juanda-linier-2.csv", header = TRUE, sep = ",")
data[is.na(data)] <- 0
data <- data.matrix(data[,-1])
data <- as.data.frame(scale(data[]))

target <- read.csv("csv-harian-perak1.csv", header = TRUE, sep = ",")
target <- read.csv("csv-harian-perak2.csv", header = TRUE, sep = ",")
target <- read.csv("csv-harian-juanda.csv", header = TRUE, sep = ",")
target <- data.matrix(target[,8])
target[is.na(target)] <- 0

data <- cbind(data,target)

head(data)
N = nrow(data)
p = ncol(data)

# bagi banyak training testing
n = round(N *0.8, digits = 0)
m = N - n

#data <- (data - min(data)) / (max(data) - min(data))

x_train = data.matrix(data[1:n, 1:p])
y_train <- array_reshape(x = x_train, dim = list(n, 1, p))

x_test  = data.matrix(data[(n+1):N, ])
y_test <- array_reshape(x = x_test, dim = list(m, 1, p))


#y_test <- matrix(rnorm(p*m), ncol = p, nrow = m)
#x_test <- array(data = y_test, dim = c(nrow(y_test),1, p))
#y_train <- matrix(rnorm(p*n), ncol = p, nrow = n)
#x_train <- array(data = y_train, dim = c(nrow(y_train), 1, p))

# constants
units = 32
timesteps = 1

# define and compile model
# expected input data shape: (batch_size, timesteps, feature)
# 1. stacked lstm sequence
model <- keras_model_sequential() 
model %>% 
  layer_lstm(units, return_sequences = TRUE, input_shape = c(timesteps, p)) %>% 
  layer_lstm(units, return_sequences = TRUE) %>% 
  layer_lstm(units) %>%
  layer_dense(units=p, activation = 'softmax')

# 2. lstm biasa dense 2 dimensi -> dimension -1 
# model <- keras_model_sequential() 
# model%>%
#  layer_lstm(units, batch_input_shape = c(batch_size, x_train, y_train), stateful= TRUE)%>%
#  layer_dense(units = 1)

# compile
rm(model)

# 1. rmsprop - categorical
# batch: 128
# $loss
# [1] 54.64908
# $acc
# [1] 0.6291227

# batch: 64
# $loss
# [1] 54.32065
# $acc
# [1] 0.6737062

# 128
# $loss
# [1] 54.64908
# $acc
# [1] 0.6291227

# batch: 32
# epoch: 100
# $loss
# [1] 54.30645
# $acc
# [1] 0.7233737

# batch: 32
# epoch: 500
# $loss
# [1] 54.27905
# $acc
# [1] 0.8861948

# batch: 32
# epoch: 1000
# $loss
# [1] 54.27939
# $acc
# [1] 0.8989701
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'rmsprop',
  metrics = c('accuracy')
)

# 2. adam - mse
# batch: 128
# $loss
# [1] 0.2188381
# $acc
# [1] 0.7038196

# batch: 64
# $loss
# [1] 0.2190786
# $acc
# [1] 0.649459
model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),  
  metrics = c('accuracy')
)

# 3. rmsprop - mae
# batch: 128
# $loss
# [1] 0.4687406
# $acc
# [1] 0.03037414

# batch: 64
# $loss
# [1] 0.461569
# $acc
# [1] 0.0305045
model %>% compile(
  loss = "mae",
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# summary
summary(model)

# train
history <- model %>% fit( 
  y_train, 
  x_train,
  batch_size = units, 
  epochs = 1000, 
  validation_data = list(y_test, x_test)
  #validation_split = 0.2
)

# evaluate
model %>% evaluate(x_train, y_train)
model %>% evaluate(x_test, y_test)

# predict
model %>% predict_classes(x_test)

# evaluate the model
evals <- model %>% evaluate(x_test, y_test, batch_size = 10)
accuracy = evals[2][[1]]* 100

# check accuracy
accuracy
