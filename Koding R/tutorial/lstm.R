# Core Tidyverse
library(tidyverse)
library(glue)
library(forcats)

# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)

# Visualization
library(cowplot)

# Preprocessing
library(recipes)

# Sampling / Accuracy
library(rsample)
library(yardstick) 

# Modeling
library(keras)
library(tfruns)

# Split
require(stats)
require(graphics)

periods_train <- 12 * 100
periods_test  <- 12 * 50
skip_span     <- 12 * 22 - 1

rolling_origin_resamples <- rolling_origin(
  data,
  initial    = periods_train,
  assess     = periods_test,
  cumulative = FALSE,
  skip       = skip_span
)

rolling_origin_resamples

index <- matrix(data=1:10959,nrow=10959, ncol=1, dimnames=NULL)
data <- cbind(index,data)

library(data.table)
colnames(data)<-c("index","smin","smaks","sr","kr","ch","lp","kar","kat",
                  "nino12","nino3","nino34","nino4")
data <- data.frame(data)

# Plotting function for a single split
plot_split <- function(splits, expand_y_axis = TRUE, 
                       alpha = 1, size = 1, base_size = 14) {
  
  # Manipulate data
  train_tbl <- training(split) %>%
    add_column(key = "training") 
  
  test_tbl  <- testing(split) %>%
    add_column(key = "testing") 
  
  data_manipulated <- bind_rows(train_tbl, test_tbl) %>%
    as_tbl_time(index = index) %>%
    mutate(key = fct_relevel(key, "training", "testing"))
  
  # Collect attributes
  train_time_summary <- train_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  test_time_summary <- test_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()

}

## test plot
plot_split()

rolling_origin_resamples$splits[[1]] %>%
  plot_split(expand_y_axis = TRUE) +
  theme(legend.position = "bottom")


## Load and prep data
data <- read.csv("csv-harian-perak1.csv", header = TRUE, sep = ",")
data <- data.matrix(data[,-1])
data <- data.matrix(data[,-1])
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
