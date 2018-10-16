### FUNGSI RNN SPATIO-TEMPORAL

RNN_spatio_temporal <- function()
{
  library(rnn)
  library(Metrics)
  ## load data
  setwd("E:/KULIAH/MATKUL/bismillah ta/Koding/rnn")
  
  data1 <- read.csv("karangkates.csv",header=TRUE,sep=";", col.names=c("tgl","smin","smaks","sr","kr","ch","lp","kar","kat"))
  data2 <- read.csv("karangploso.csv",header=TRUE,sep=";", col.names=c("tgl","smin","smaks","sr","kr","ch","lp","kar","kat"))
  head(data1)
  head(data2)
  data1 <- data.matrix(data1)
  data2 <- data.matrix(data2)
  
  
  DF <- data.frame(a = 1:3, b = letters[10:12],
                   c = seq(as.Date("2004-01-01"), by = "week", len = 3),
                   stringsAsFactors = TRUE)
  data.matrix(DF[1:2])
  data.matrix(DF)
  
  # time series plot
  library(ggplot2)
  ggplot(data.frame(data1), aes(x = 1:nrow(data1), y = `ch`)) + geom_line()
  ggplot(data.frame(data2), aes(x = 1:nrow(data2), y = `ch`)) + geom_line()
  ggplot(data.frame(data1[1:100,]), aes(x = 1:100, y = `ch`)) + geom_line()
  ggplot(data.frame(data2[1:100,]), aes(x = 1:100, y = `ch`)) + geom_line()
  
  # penggabungan menjadi list
  #data <- array(c(data1, data2, data3), dim=c(nrow(data1), ncol(data1), 3))
  
  data[is.na(data)] <- 0
  data <- list(data1, data2)
  ## normalisasi data
  library(matrixStats)
  
  range_data<-function(z){(z-colMins(z))/(colMaxs(z)-colMins(z))}
  min_data<-colMins(data,na.rm = TRUE)
  max_data<-colMaxs(data,na.rm = TRUE)
  
  ## mendefinisikan matriks kinerja
  korelasi <- matrix(data=NA, 3, 2)
  ##R-sq <- matrix(data=NA, 3, 2)
  ##MSE <- matrix(data=NA, 3, 2)
  
  ## mendefinisikan matriks data output
  result_train_all <- array(data=NA, dim=c(n_train, 3, 3), dimnames=NULL)
  result_test_all <- array(data=NA, dim=c(nrow(data)-n_train, 3, 3), dimnames=NULL)
  
  ##nyoba dari web
  data <- data1
  data <- data.matrix(data[,-1])
  train_data <- data[1:10000,]
  mean <- apply(train_data, 2, mean)
  std <- apply(train_data, 2, sd)
  data <- scale(data, center = mean, scale = std)
  
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
  
  lookback <- 1440
  step <- 6
  delay <- 144
  batch_size <- 128
  
  train_gen <- generator(
    data,
    lookback = lookback,
    delay = delay,
    min_index = 1,
    max_index = 10000,
    shuffle = TRUE,
    step = step, 
    batch_size = batch_size
  )
  
  val_gen = generator(
    data,
    lookback = lookback,
    delay = delay,
    min_index = 10001,
    max_index = 15000,
    step = step,
    batch_size = batch_size
  )
  
  test_gen <- generator(
    data,
    lookback = lookback,
    delay = delay,
    min_index = 15001,
    max_index = NULL,
    step = step,
    batch_size = batch_size
  )
  
  # How many steps to draw from val_gen in order to see the entire validation set
  val_steps <- (300000 - 200001 - lookback) / batch_size
  
  # How many steps to draw from test_gen in order to see the entire test set
  test_steps <- (nrow(data) - 300001 - lookback) / batch_size
  
  mean(abs(preds - targets))
  library(keras)
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
  
  
  
  
  
  
  
  
  
  
  for (lok in 1:2) {
    lok<-1
    data[[lok]] <- range_data(data[[lok]])
    data <- range_data(data[[1]])
    data[]
    
    ## mendefinisikan prediktor & respon
    n_train <- 200
    y_train1 <- array(data=NA, dim=c(n_train, 1, 3), dimnames=NULL)
    ##x_train1 <- array(data=NA, dim=c(1, n_train, ncol(data[lok])-1, 3), dimnames=NULL)
    ##x_train1[1, , , ] <- data[1:n_train, 2:ncol(data),1:3 ]
    
    x_train1[[lok]] <- as.list(data[[lok[1:n_train, 2:ncol(data[[lok]])]]])
    y_train1[ ,1, ] <- data[1:n_train, 1,1:3]
    
    ## Mendefinisikan model
    set.seed(2000)
    
    # menurunkan dimensi array
    kol_matriks <- ncol(data[lok])
    x_train <- array(data=NA, dim=c(1, n_train, kol_matriks), dimnames=NULL)
    ##x_train <- array(data=NA, dim=c(1, n_train, ncol(data[lok])-1), dimnames=NULL)
    y_train<- matrix(data=NA,n_train, 1)
    y_train[ ,1]<- y_train1[ ,1,lok]
    y_train <- t(y_train)
    x_train[1, , ] <- as.array(x_train1[1,data[lok]])
    
    model<- trainr(Y=y_train,
                   X=x_train,
                   learningrate=0.01,
                   hidden_dim = 6,
                   numepochs = 10000,
                   network_type="rnn",
                   sigmoid="logistic")
    
    ## plot error by epoch
    assign(paste("error lokasi ke-", lok, sep = ""), lok) 
    error_1<-t(model$error)
    rownames(error_1) <-1:nrow(error_1)
    colnames(error_1 ) <- "error "
    plot(error_1)
    
    ##evaluasi kinerja model
    pred_train <- t(predictr(model,x_train))
    r_train_model <- round(cor(t(y_train), pred_train),5)
    plot(t(y_train),pred_train,ylab="pred_train")
    
    ## Mempersiapkan data testing model
    x_test<-array(data=NA, dim=c(1, nrow(data)-(n_train), ncol(data)-1), dimnames=NULL)
    x_test[1, , ] <- data[(n_train+1):nrow(data), 2:(ncol(data)), lok]
    y_test<-as.matrix(data[(n_train+1):nrow(data),1, lok])
    pred_test <-t(predictr(model, x_test))
    r_test_model <- round(cor(y_test, pred_test),5)
    
    ## Unscaling data and prediksi
    unscale_data<-function(x,max_x, min_x)
    {x*(max_x-min_x)+min_x}
    
    y_train_actual <- t(unscale_data(y_train, max_data, min_data))
    y_test_actual <- unscale_data(y_test, max_data, min_data)
    train_actual<-unscale_data( pred_train, max_data,min_data)
    test_actual <-unscale_data( pred_test, max_data,min_data)
    err_train <- y_train_actual - train_actual
    err_test <- y_test_actual - test_actual
    
    result_train<-cbind(y_train_actual,round(train_actual,2), round(err_train,2)) 
    result_test<-cbind(y_test_actual,  round(test_actual,2), round(err_test, 2))
    
    result_train_all[ , ,lok] <- result_train
    result_test_all[ , ,lok] <- result_test
    korelasi[lok,1] <- r_train_model
    korelasi[lok,2] <- r_test_model
    
    #Comparing Plots of Predicted Curve vs Actual Curve: Test Data
    plot(y_test_actual, col="blue", type="l",main ="Testing M1 : Actual vs Predicted Curve", lwd = 2) 
    lines(test_actual, type = "l", col = "red", lwd = 1)
  }
  print(result_train_all) 
  print(result_train_all) 
  print(result_test_all)
  print(korelasi) 
}
