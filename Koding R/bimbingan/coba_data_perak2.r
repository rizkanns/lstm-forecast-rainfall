## Load and prep data
data = read.csv('opt.perak2.csv')
data <- data.matrix(data[,-1])
data[is.na(data)] <- 0
N = nrow(data)
p = ncol(data)

mean <- apply(data, 2, mean)
std <- apply(data, 2, sd)
data <- scale(data, center = mean, scale = std)


## split data, training & testing, 80:20, AND convert dataframe to an array
Ind = sample(N, N*0.8, replace = FALSE) 
p = ncol(data)
Y_train = data.matrix(data[Ind, 1])
X_train  = data.matrix(data[Ind,2:p])

Y_test = data.matrix(data[-Ind, 1])
X_test  = data.matrix(data[-Ind, 2:p])

k = ncol(X_train)

library(tensorflow)
library(keras)
------------------------------------------------
## create your model,and add layers 
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 60, activation = 'relu', input_shape = k) %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = 'sigmoid')

## see your model structure
summary(model)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae",
  metrics = c('accuracy')
)

track = model %>% fit(X_train, Y_train, epochs = 2000, batch_size = 20,
                      callbacks = callback_early_stopping(patience = 2, monitor = 'mae'),
                      validation_split = 0.3
)
plot(track)

##prediction 
pred <- model %>% predict(X_test, batch_size = 128)
Y_pred = round(pred)
# Confusion matrix
CM = table(Y_pred, Y_test)

# evaluate the model
evals <- model %>% evaluate(X_test, Y_test, batch_size = 10)

accuracy = evals[2][[1]]* 100