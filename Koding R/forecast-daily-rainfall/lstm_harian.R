## Load and prep data
data <- read.csv("opt.3perak1-linier-2.csv", header = TRUE, sep = ",")
data[is.na(data)] <- 0
N = nrow(data)
p = ncol(data)

mean <- apply(data, 2, mean)
std <- apply(data, 2, sd)
data <- scale(data, center = mean, scale = std)

## split data, training & testing, 80:20, AND convert dataframe to an array
Ind = sample(N, round(N*0.8), replace = FALSE) 
p = ncol(data)
y_train = data.matrix(data[Ind, 1])
x_train  = data.matrix(data[Ind,2:p])

y_test = data.matrix(data[-Ind, 1])
x_test  = data.matrix(data[-Ind, 2:p])

k = ncol(x_train)

library(tensorflow)
library(keras)
------------------------------------------------
 
## see your model structure
summary(model)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae",
  metrics = c('accuracy')
)
## create your model,and add layers
max_features = 10000
batch_size = 20

model <- keras_model_sequential() 
model%>%
  layer_lstm(units, batch_input_shape = c(batch_size, x_train, y_train), stateful= TRUE)%>%
  layer_dense(units = 1)

model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = max_features, output_dim = 128) %>% 
  layer_lstm(units = 32) %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.02 , decay = 1e-6 ),  
  metrics = c('accuracy')
)

summary(model)
.libPaths("D:/Kuliah/TA/workspace")
nb_epoch = 10  
for(i in 1:nb_epoch ){
  model %>% fit(x_train, y_train, epochs=nb_epoch, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model %>% reset_states()
}

track = model %>% fit(x_train, y_train, epochs = 2000, batch_size = 20,
                      callbacks = callback_early_stopping(patience = 2, monitor = 'mae'),
                      validation_split = 0.3
)

history <- model %>% fit(
  x_train, y_train,
  epochs = 10,
  batch_size = 128,
  validation_split = 0.2
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
