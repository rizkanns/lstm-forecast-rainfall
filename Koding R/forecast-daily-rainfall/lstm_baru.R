## load dataset
perak <- read.csv("perak.csv", header = TRUE, col.names= c("NA", "ID", "DY", "TN","TM","TA","HA","RF","LG","VA","VX"))
perak2 <- read.csv("perak.csv", header = TRUE, col.names= c("NA", "ID", "DY", "TN","TM","TA","HA","RF","LG","VA","VX"))
perak2 <- data.frame(perak2[,-1])

str(perak)
pairs(perak[,-c(1,3)])

row.has.na <- apply(perak, 1, function(x){any(is.na(x))})
sum(row.has.na)

## read dataset
head(perak)
tail(perak)
str(perak)
dim(perak)
summary(perak)
typeof(perak)

## banyak colom yang digunakan
banyak_row <- nrow(perak)
banyak_col <- ncol(perak)
banyak_data <- nrow(perak)*ncol(perak)
data_digunakan <- round(1/sqrt(banyak_row))
data_digunakan <- 1

## col yang dipilih
banyak_col
for(i in banyak_col)
  {perak <- data.frame(perak[,-1])}

## drawing plot
barplot(table(perak2), ylab="Count",col="darkblue")

## make divide training and testing
seq_1 <- perak2[1:100,]
seq_2 <- perak2[101:200,]
seq_3 <- perak2[201:300,]
data <- cbind(seq_1, seq_2, seq_3)
data_set = aperm((array(c(data), dim = c(100,3))))
dim(data_set)

lab_1 <- perak2[2:101,]
lab_2 <- perak2[102:201,]
lab_3 <- perak2[202:301,]
lab <- cbind(lab_1, lab_2, lab_3)
lab_set = aperm((array(c(lab), dim = c(100,3))))
dim(lab_set)

trainData <- list(data=data_set, lab=lab_set)
summary(trainData)

## installing package mxnet for lstm
cran <- getOption('repos')
cran['dmlc'] <- "https://s3-us-west-2.amazonaws.com/apache-mxnet/R/CRAN/"
options(repos = cran)
install.packages("mxnet")

## specify lstm
require(mxnet)
mx.set.seed(0)

lstm <- function(num.hidden, indata, prev.state, param, seqidx, layeridx, dropout=0) {
  if (dropout > 0)
    indata <- mx.symbol.Dropout(data=indata, p=dropout)
  i2h <- mx.symbol.FullyConnected(data=indata,
                                  weight=param$i2h.weight,
                                  bias=param$i2h.bias,
                                  num.hidden=num.hidden * 4,
                                  name=paste0("t", seqidx, ".l", layeridx, ".i2h"))
  h2h <- mx.symbol.FullyConnected(data=prev.state$h,
                                  weight=param$h2h.weight,
                                  bias=param$h2h.bias,
                                  num.hidden=num.hidden * 4,
                                  name=paste0("t", seqidx, ".l", layeridx, ".h2h"))
  gates <- i2h + h2h
  slice.gates <- mx.symbol.SliceChannel(gates, num.outputs=4,
                                        name=paste0("t", seqidx, ".l", layeridx, ".slice"))
  
  in.gate <- mx.symbol.Activation(slice.gates[[1]], act.type="sigmoid")
  in.transform <- mx.symbol.Activation(slice.gates[[2]], act.type="tanh")
  forget.gate <- mx.symbol.Activation(slice.gates[[3]], act.type="sigmoid")
  out.gate <- mx.symbol.Activation(slice.gates[[4]], act.type="sigmoid")
  next.c <- (forget.gate * prev.state$c) + (in.gate * in.transform)
  next.h <- out.gate * mx.symbol.Activation(next.c, act.type="tanh")
  
  return (list(c=next.c, h=next.h))
}


lstm.unroll <- function(num.lstm.layer, seq.len, input.size,
                        num.hidden, num.embed, num.label, dropout=0.) {
  
  embed.weight <- mx.symbol.Variable("embed.weight")
  cls.weight <- mx.symbol.Variable("cls.weight")
  cls.bias <- mx.symbol.Variable("cls.bias")
  
  param.cells <- lapply(1:num.lstm.layer, function(i) {
    cell <- list(i2h.weight = mx.symbol.Variable(paste0("l", i, ".i2h.weight")),
                 i2h.bias = mx.symbol.Variable(paste0("l", i, ".i2h.bias")),
                 h2h.weight = mx.symbol.Variable(paste0("l", i, ".h2h.weight")),
                 h2h.bias = mx.symbol.Variable(paste0("l", i, ".h2h.bias")))
    return (cell)
  })
  last.states <- lapply(1:num.lstm.layer, function(i) {
    state <- list(c=mx.symbol.Variable(paste0("l", i, ".init.c")),
                  h=mx.symbol.Variable(paste0("l", i, ".init.h")))
    return (state)
  })

  # embeding layer
  label <- mx.symbol.Variable("label")
  data <- mx.symbol.Variable("data")
  embed <- mx.symbol.Embedding(data=data, input_dim=input.size,
                               weight=embed.weight, output_dim=num.embed, name="embed")
  wordvec <- mx.symbol.SliceChannel(data=embed, num_outputs=seq.len, squeeze_axis=1)
  
  last.hidden <- list()
  for (seqidx in 1:seq.len) {
    hidden <- wordvec[[seqidx]]
    # stack lstm
    for (i in 1:num.lstm.layer) {
      dp <- ifelse(i==1, 0, dropout)
      next.state <- lstm(num.hidden, indata=hidden,
                         prev.state=last.states[[i]],
                         param=param.cells[[i]],
                         seqidx=seqidx, layeridx=i,
                         dropout=dp)
      hidden <- next.state$h
      last.states[[i]] <- next.state
    }
    # decoder
    if (dropout > 0)
      hidden <- mx.symbol.Dropout(data=hidden, p=dropout)
    last.hidden <- c(last.hidden, hidden)
  }
  
  last.hidden$dim <- 0
  last.hidden$num.args <- seq.len
  concat <-mxnet:::mx.varg.symbol.Concat(last.hidden)
  fc <- mx.symbol.FullyConnected(data=concat,
                                 weight=cls.weight,
                                 bias=cls.bias,
                                 num.hidden=num.label)
  
  label <- mx.symbol.transpose(data=label)
  label <- mx.symbol.Reshape(data=label, target.shape=c(0))
  
  loss.all <- mx.symbol.SoftmaxOutput(data=fc, label=label, name="sm")
  return (loss.all)
}

mx.lstm <- function(train.data, eval.data=NULL,
                    num.lstm.layer, seq.len,
                    num.hidden, num.embed, num.label,
                    batch.size, input.size,
                    ctx=mx.ctx.default(),
                    num.round=10, update.period=1,
                    initializer=mx.init.uniform(0.01),
                    dropout=0, optimizer='sgd',
                    ...) {
  # check data and change data into iterator
  train.data <- check.data(train.data, batch.size, TRUE)
  eval.data <- check.data(eval.data, batch.size, FALSE)
  
  # get unrolled lstm symbol
  rnn.sym <- lstm.unroll(num.lstm.layer=num.lstm.layer,
                         num.hidden=num.hidden,
                         seq.len=seq.len,
                         input.size=input.size,
                         num.embed=num.embed,
                         num.label=num.label,
                         dropout=dropout)
  init.states.c <- lapply(1:num.lstm.layer, function(i) {
    state.c <- paste0("l", i, ".init.c")
    return (state.c)
  })
  init.states.h <- lapply(1:num.lstm.layer, function(i) {
    state.h <- paste0("l", i, ".init.h")
    return (state.h)
  })
  init.states.name <- c(init.states.c, init.states.h)
  
  # set up lstm model
  model <- setup.rnn.model(rnn.sym=rnn.sym,
                           ctx=ctx,
                           num.rnn.layer=num.lstm.layer,
                           seq.len=seq.len,
                           num.hidden=num.hidden,
                           num.embed=num.embed,
                           num.label=num.label,
                           batch.size=batch.size,
                           input.size=input.size,
                           init.states.name=init.states.name,
                           initializer=initializer,
                           dropout=dropout)
  
  # train lstm model
  model <- train.rnn( model, train.data, eval.data,
                      num.round=num.round,
                      update.period=update.period,
                      ctx=ctx,
                      init.states.name=init.states.name,
                      ...)
  # change model into MXFeedForwardModel
  model <- list(symbol=model$symbol, arg.params=model$rnn.exec$ref.arg.arrays, aux.params=model$rnn.exec$ref.aux.arrays)
  return(structure(model, class="MXFeedForwardModel"))
}

mx.lstm.inference <- function(num.lstm.layer,
                              input.size,
                              num.hidden,
                              num.embed,
                              num.label,
                              batch.size=1,
                              arg.params,
                              ctx=mx.cpu(),
                              dropout=0.) {
  sym <- lstm.inference.symbol(num.lstm.layer=num.lstm.layer,
                               input.size=input.size,
                               num.hidden=num.hidden,
                               num.embed=num.embed,
                               num.label=num.label,
                               dropout=dropout)
  
  init.states.c <- lapply(1:num.lstm.layer, function(i) {
    state.c <- paste0("l", i, ".init.c")
    return (state.c)
  })
  init.states.h <- lapply(1:num.lstm.layer, function(i) {
    state.h <- paste0("l", i, ".init.h")
    return (state.h)
  })
  init.states.name <- c(init.states.c, init.states.h)
  
  seq.len <- 1
  # set up lstm model
  model <- setup.rnn.model(rnn.sym=sym,
                           ctx=ctx,
                           num.rnn.layer=num.lstm.layer,
                           seq.len=seq.len,
                           num.hidden=num.hidden,
                           num.embed=num.embed,
                           num.label=num.label,
                           batch.size=batch.size,
                           input.size=input.size,
                           init.states.name=init.states.name,
                           initializer=mx.init.uniform(0.01),
                           dropout=dropout)
  arg.names <- names(model$rnn.exec$ref.arg.arrays)
  for (k in names(arg.params)) {
    if ((k %in% arg.names) && is.param.name(k) ) {
      rnn.input <- list()
      rnn.input[[k]] <- arg.params[[k]]
      mx.exec.update.arg.arrays(model$rnn.exec, rnn.input, match.name=TRUE)
    }
  }
  init.states <- list()
  for (i in 1:num.lstm.layer) {
    init.states[[paste0("l", i, ".init.c")]] <- model$rnn.exec$ref.arg.arrays[[paste0("l", i, ".init.c")]]*0
    init.states[[paste0("l", i, ".init.h")]] <- model$rnn.exec$ref.arg.arrays[[paste0("l", i, ".init.h")]]*0
  }
  mx.exec.update.arg.arrays(model$rnn.exec, init.states, match.name=TRUE)
  
  return (model)
}


mx.lstm.forward <- function(model, input.data, new.seq=FALSE) {
  if (new.seq == TRUE) {
    init.states <- list()
    for (i in 1:model$num.rnn.layer) {
      init.states[[paste0("l", i, ".init.c")]] <- model$rnn.exec$ref.arg.arrays[[paste0("l", i, ".init.c")]]*0
      init.states[[paste0("l", i, ".init.h")]] <- model$rnn.exec$ref.arg.arrays[[paste0("l", i, ".init.h")]]*0
    }
    mx.exec.update.arg.arrays(model$rnn.exec, init.states, match.name=TRUE)
  }
  dim(input.data) <- c(model$batch.size)
  data <- list(data=mx.nd.array(input.data))
  mx.exec.update.arg.arrays(model$rnn.exec, data, match.name=TRUE)
  mx.exec.forward(model$rnn.exec, is.train=FALSE)
  init.states <- list()
  for (i in 1:model$num.rnn.layer) {
    init.states[[paste0("l", i, ".init.c")]] <- model$rnn.exec$ref.outputs[[paste0("l", i, ".last.c_output")]]
    init.states[[paste0("l", i, ".init.h")]] <- model$rnn.exec$ref.outputs[[paste0("l", i, ".last.h_output")]]
  }
  mx.exec.update.arg.arrays(model$rnn.exec, init.states, match.name=TRUE)
  prob <- model$rnn.exec$ref.outputs[["sm_output"]]
  return (list(prob=prob, model=model))
}

X.train <- list(data=data_set, label=lab_set)
X.val <- list(data=data_set, label=lab_set)
print(data)


mx.lstm <- function(mx.lstm){
            batch_size=1
            seq_len=3
            num.hidden=25
            num.embed=2
            num_layer=1
            num_round=2
            update.period=1
            learning.rate=0.3
            num_labels=7
            input_size=7
            moment=0.9
}

mx.lstm <- function(mx.lstm){
  batch.size = 32
  seq.len = 32
  num.hidden = 256
  num.embed = 256
  num.lstm.layer = 2
  num.round = 5
  learning.rate= 0.1
  wd=0.00001
  clip_gradient=1
  update.period = 1
}

model <- mx.lstm(trainData,
                 eval.data=NULL,
                 ctx=mx.cpu,
                 num.round=num_round,
                 update.period=update.period,
                 num.lstm.layer=num_layer,
                 seq.len=seq.len,
                 num.hidden=num.hidden,
                 num.embed=num.embed,
                 num.label=num_labels,
                 batch.size=batch_size,
                 input.size=input_size,
                 initializer=mx.init.uniform(0.1),
                 learning.rate=learning.rate,
                 momentum=moment)

model <- mx.lstm(X.train)

make.output <- function(prob, sample=FALSE) {
  if (!sample) {
    idx <- which.max(as.array(prob))
  }
  else {
    idx <- choice(prob)
  }
  return (idx)
}

infer.model <- mx.lstm.inference(num.lstm.layer=num.lstm.layer,
                                 input.size=vocab,
                                 num.hidden=num.hidden,
                                 num.embed=num.embed,
                                 num.label=vocab,
                                 arg.params=model$arg.params,
                                 ctx=mx.cpu())


start <- 'a'
seq.len <- 75
random.sample <- TRUE

last.id <- dic[[start]]
out <- "a"
for (i in (1:(seq.len-1))) {
  input <- c(last.id-1)
  ret <- mx.lstm.forward(infer.model, input, FALSE)
  infer.model <- ret$model
  prob <- ret$prob
  last.id <- make.output(prob, random.sample)
  out <- paste0(out, lookup.table[[last.id]])
}
cat (paste0(out, "\n"))

##evaluating model performance
mx.set.seed(2018)
