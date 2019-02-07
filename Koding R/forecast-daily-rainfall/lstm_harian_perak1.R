library(keras)
library(tensorflow)
#install_keras()

library(tibble)
data <- read.csv("csv-harian-perak1.csv", header = TRUE, sep = ",")
data <- data.matrix(data[,-1])
data <- data.matrix(data[,-1])
data <- data.matrix(data[,-1])

data[is.na(data)] <- 0
head(data)
plot(data)

library(ggplot2)
ggplot(data.frame(data), aes(x = 1:nrow(data), y = `Suhu.Minimum`)) + geom_line()
ggplot(data.frame(data[1:100,]), aes(x = 1:100, y = `NINO12`)) + geom_line()

## normalisasi data training
#data<-data.matrix(data)
#data<-data.frame(data)

## data train biasa, split dataset into training (80%) and validation (20%)
#split_data <- sample(2, nrow(data), replace=TRUE, prob=c(0.8,0.2))
#train_data <- data[split_data==1,]
#validation_data <- data[split_data==2,]

## multiple linear regression model
#results <- lm(Suhu.Minimum~NINO4,validation_data)
#summary(results)


sequence_generator <- function(start) {
  value <- start - 1
  function() {
    value <<- value + 1
    value
  }
}

gen <- sequence_generator(10)
gen()

## data train dari lstm
train_data <- data[1:8000,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data <- scale(data, center = mean, scale = std)

## data test dari lstm
test_data <- data[9001:nrow(data),]
mean <- apply(test_data, 2, mean)
std <- apply(test_data, 2, sd)
data <- scale(data, center = mean, scale = std)

## fungsi generator
generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows), 
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]]-1, 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,2]
    }            
    
    list(samples, targets)
  }
}

lookback = 1440
step = 6
delay = 144
batch_size = 128

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 8000,
  shuffle = TRUE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 8001,
  max_index = 9000,
  step = step,
  batch_size = batch_size
)

test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 9001,
  max_index = NULL,
  step = step,
  batch_size = batch_size
)

# How many steps to draw from val_gen in order to see the entire validation set
val_steps <- (9000 - 8001 - lookback) / batch_size

# How many steps to draw from test_gen in order to see the entire test set
test_steps <- (nrow(data) - 9001 - lookback) / batch_size

evaluate_naive_method <- function() {
  batch_maes <- c()
  for (step in 1:val_steps) {
    c(samples, targets) %<-% val_gen()
    preds <- samples[,dim(samples)[[2]],2]
    mae <- mean(abs(preds - targets))
    batch_maes <- c(batch_maes, mae)
  }
  print(mean(batch_maes))
}

evaluate_naive_method()

library(keras)
model <- keras_model_sequential() %>% 
  layer_flatten(input_shape = c(lookback / step, dim(data)[-1])) %>% 
  layer_dense(units = 32, activation = "relu") %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
)

plot(history)

model <- keras_model_sequential() %>% 
  layer_flatten(input_shape = c(lookback / step, dim(data)[-1])) %>% 
  layer_dense(units = 200, activation = "relu") %>% 
  layer_dense(units = 100, activation = "relu") %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 1,
  validation_data = val_gen,
  validation_steps = val_steps
)
  
plot(history)

model <- keras_model_sequential() %>% 
  layer_gru(units = 145, input_shape = list(NULL, dim(data)[[-1]])) %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)


history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 1,
  validation_data = val_gen,
  validation_steps = val_steps
)

plot(history)

## A FIRST RNN
model <- keras_model_sequential() %>% 
  layer_gru(units = 32, dropout = 0.2, recurrent_dropout = 0.2,
            input_shape = list(NULL, dim(data)[[-1]])) %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 1,
  validation_data = val_gen,
  validation_steps = val_steps
)

##  DROPOUT TO FIGHT OVERFITTING
model <- keras_model_sequential() %>% 
  layer_lstm(units = 200, dropout = 0.2, recurrent_dropout = 0.2,
             input_shape = list(NULL, dim(data)[[-1]])) %>% 
  layer_dense(units = 100, activation = "relu") %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mse"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 1,
  validation_data = val_gen,
  validation_steps = val_steps
)

## STACKING RECURRENT LAYER
model <- keras_model_sequential() %>% 
  layer_gru(units = 32, 
            dropout = 0.1, 
            recurrent_dropout = 0.5,
            return_sequences = TRUE,
            input_shape = list(NULL, dim(data)[[-1]])) %>% 
  layer_gru(units = 64, activation = "relu",
            dropout = 0.1,
            recurrent_dropout = 0.5) %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 1,
  validation_data = val_gen,
  validation_steps = val_steps
)

## the rainfall prediction task.
model <- keras_model_sequential() %>% 
  bidirectional(
    layer_gru(units = 32), input_shape = list(NULL, dim(data)[[-1]])) %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 1,
  validation_data = val_gen,
  validation_steps = val_steps
)

## Long short term memory
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 60, activation = 'relu', input_shape = k) %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = 'sigmoid')

track = model %>% fit(X_train, Y_train, epochs = 64, batch_size = 10,
                      callbacks = callback_early_stopping(patience = 1, monitor = 'mse'),
                      validation_split = 0.05
)
plot(track)

## Long short term memory
model <- keras_model_sequential() %>% 
  bidirectional(
    layer_gru(units = 64,activation = "relu"), input_shape = list(NULL, dim(data)[[-1]])) %>% 
  layer_dense(units = 1, activation = "relu")

model %>% compile(
  optimizer = optimizer_sgd(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 200,
  epochs = 40,
  validation_data = val_gen,
  validation_steps = val_steps
)

plot(history)


## prediction
prediction.set <- test_gen()[[1]]
prediction <- predict(model, prediction.set)

plot(prediction)


pred_generator <- function(gen) {
  function() { # wrap it in a function to make it callable
    gen()[1]  # call the given generator and get the first element (i.e. samples)
  }
}

preds <- model %>% 
  predict_generator(
    generator = pred_generator(test_gen), # pass test_gen directly to pred_generator without calling it
    steps = test_steps
  )

evaluate_generator(model, test_gen, test_steps)

history <- model %>% fit_generator(
  test_gen,
  steps_per_epoch = 500,
  epochs = 1,
  validation_data = val_gen,
  validation_steps = val_steps
)

plot(history)
