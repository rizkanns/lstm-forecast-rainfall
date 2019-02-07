library(keras)
install_keras()

data <- read.csv("perak1.csv", header = FALSE)
data <- read.csv("opt.perak2.csv")
data2 <- read.csv("perak2.csv", header = FALSE)
data <- read.csv("perak1.csv")
data <- data.matrix(data[,-1])
data2 <- data.matrix(data2[,-1])
data[is.na(data)] <- 0
head(data)
plot(data)

library(ggplot2)
ggplot(data.frame(data), aes(x = 1:nrow(data), y = `V3`)) + geom_line()
ggplot(data.frame(data[1:100,]), aes(x = 1:100, y = `V3`)) + geom_line()

## normalisasi data
data<-data.matrix(data)
data<-data.frame(data)
train_data <- data[1:200,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data <- scale(data, center = mean, scale = std)
test_data <- data[201:nrow(data),]
mean <- apply(test_data, 2, mean)
std <- apply(test_data, 2, sd)
data <- scale(data, center = mean, scale = std)

## fungsi generator
generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 50, step = 6) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows), 
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]], 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,2]
    }            
    
    list(samples, targets)
  }
}

lookback <- 12
step <- 6
delay <- 3
batch_size <- 10

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 200,
  shuffle = TRUE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 201,
  max_index = 300,
  step = step,
  batch_size = batch_size
)

test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 301,
  max_index = NULL,
  step = step,
  batch_size = batch_size
)

# How many steps to draw from val_gen in order to see the entire validation set
val_steps <- (300 - 201 - lookback) / batch_size

# How many steps to draw from test_gen in order to see the entire test set
test_steps <- (nrow(data) - 301 - lookback) / batch_size

library(keras)

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
  epochs = 40,
  validation_data = val_gen,
  validation_steps = val_steps
)

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
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
)

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
  epochs = 50,
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
  epochs = 50,
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
  epochs = 50,
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
  epochs = 40,
  validation_data = val_gen,
  validation_steps = val_steps
)

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
  steps_per_epoch = 2000,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
)

plot(history)

prediction.set <- test_gen()[[1]]
prediction <- predict(model, prediction.set)

plot(prediction)

data <- scale(data, center = mean, scale = std)
is.data.frame(test_data)

pred = predict.lm(test_data)

denorm_pred = pred * std + mean

def pred_generator(gen):
  for data, labels in gen:
  yield data  # discards labels

preds = model.predict_generator(pred_generator(test_generator), number_of_steps)

denorm_loss = loss * std

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
  epochs = 50,
  validation_data = val_gen,
  validation_steps = val_steps
)

plot(history)
