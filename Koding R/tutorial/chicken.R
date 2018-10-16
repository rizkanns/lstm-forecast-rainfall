## collecting data ##
data("chicken",package="astsa")
plot(chicken,xlab="Date",ylab="Price(cents)",col="darkblue")

## preparing data ##
data<-chicken
require(quantmod)
data<-as.zoo(data)

x1<-Lag(data,k=1)
x2<-Lag(data,k=2)
x3<-Lag(data,k=3)
x4<-Lag(data,k=4)

x<-cbind(x1,x2,x3,x4,data)

x<-log(x)
head(round(x,2))

## remove missing value ##
## dicari dari x1 sampe x4 yang kosong 1:4, ##
x<-x[-(1:4),] 
head(round(x,2))

## preparing the data for input into the neural network ##
x<-data.matrix(x)
range_data<-function(x){(x-min(x))/(max(x)-min(x))}
min_data<-min(x)
max_data<-max(x)
x<-range_data(x)

x1<-as.matrix(x[,1])
x2<-as.matrix(x[,2])
x3<-as.matrix(x[,3])
x4<-as.matrix(x[,4])
y<-as.matrix(x[,5])

## diplot antara data forecast dan real ##
n_train<-170
y_train<-as.matrix(y[1:n_train])
x1_train<-as.matrix(t(x1[1:n_train,]))
x2_train<-as.matrix(t(x2[1:n_train,]))
x3_train<-as.matrix(t(x3[1:n_train,]))
x4_train<-as.matrix(t(x4[1:n_train,]))

x_train<-array(c(x1_train,x2_train,x3_train,x4_train),dim=c(dim(x1_train),4))

require(rnn)
set.seed(2018)
model1<-trainr(Y=t(y_train),X=x_train,learningrate=0.05,hidden_dim=3,numepochs=500,network_type="rnn",sigmoid="logistic")

error_1<-t(model1$error)
rownames(error_1)<-1:nrow(error_1)
colnames(error_1)<-"error"
plot(error_1)

pred1_train<-t(predictr(model1,x_train))
round(cor(y_train,pred1_train),5)
plot(y_train,pred1_train,ylab="pred1_train")

set.seed(2018)
model2<-trainr(Y=t(y_train),X=x_train,learningrate=0.05,hidden_dim=c(3,2),numepochs=500,network_type="rnn",sigmoid="logistic")
pred2_train<-t(predictr(model2,x_train))
round(cor(y_train,pred2_train),5)
plot(t(y_train),pred1_train,ylab="pred1_train")

x1_test<-as.matrix(t(x1[(n_train+1):nrow(x1),]))
x2_test<-as.matrix(t(x2[(n_train+1):nrow(x2),]))
x3_test<-as.matrix(t(x3[(n_train+1):nrow(x3),]))
x4_test<-as.matrix(t(x4[(n_train+1):nrow(x4),]))
y_test<-as.matrix(y[(n_train+1):nrow(x4)])
x_test<-array(c(x1_test,x2_test,x3_test,x4_test),dim=c(dim(x1_test),4))

##UNTUKMELIHATGRAFIKFORECASTDANPREDIKSI
unscale_data<-function(x,max_x,min_x){x*(max_x-min_x)+min_x}
y_train_actual<-unscale_data(y_train,max_data,min_data)
pred1_train_actual<-unscale_data(pred1_train,max_data,min_data)
plot(y_train_actual,col="blue",type="l",main="TestingM1:ActualvsPredictedCurve",lwd=2)
lines(pred1_train_actual,type="l",col="red",lwd=1)

pred1_test<-t(predictr(model1,x_test))
pred2_test<-t(predictr(model2,x_test))

unscale_data<-function(x,max_x,min_x){x*(max_x-min_x)+min_x}
pred1_actual<-unscale_data(pred1_test,max_data,min_data)
pred1_actual<-exp(pred1_actual)
pred1_actual<-ts(matrix(pred1_actual),end=c(2016,7),frequency=12)
pred2_actual<-unscale_data(pred2_test,max_data,min_data)
pred2_actual<-exp(pred2_actual)
pred2_actual<-ts(matrix(pred2_actual),end=c(2016,7),frequency=12)

y_actual<-unscale_data(y_test,max_data,min_data)
y_actual<-exp(y_actual)
y_actual<-ts(matrix(y_actual),end=c(2016,7),frequency=12)

result_all<-cbind(y_actual,round(pred1_actual,2),round(pred2_actual,2))
colnames(result_all)<-c("actual","Model1","Model2")
result_all

##KASI OPSI RANGE DARI Y-NYA
unscale_data<-function(x,max_x,min_x){x*(max_x-min_x)+min_x}
y_actual<-unscale_data(y_test,max_data,min_data)
pred1_actual<-unscale_data(pred1_actual,max_data,min_data)
pred2_actual<-unscale_data(pred2_actual,max_data,min_data)
plot(y_actual,col="blue",type="l",main="TestingM2:ActualvsPredictedCurve",lwd=2)
lines(pred1_actual,type="l",col="red",lwd=1)
lines(pred2_actual,type="l",col="green",lwd=1)

start=n_train
end=6
k=-1
for(i in start:((start+end)-1)){k=k+1}
cat("Workingforecastformonth",n_train+k,"\n")

y_train_step<-as.matrix(y[1:(n_train+k)])
x1_train_step<-as.matrix(t(x1[1:(n_train+k),]))
x2_train_step<-as.matrix(t(x2[1:(n_train+k),]))
x3_train_step<-as.matrix(t(x3[1:(n_train+k),]))
x4_train_step<-as.matrix(t(x4[1:(n_train+k),]))
x_train_step<-array(c(x1_train_step,x2_train_step,x3_train_step,x4_train_step),dim=c(dim(x1_train_step),4))

set.seed(2018)
model_step<-trainr(Y=t(y_train_step),X=x_train_step,learningrate=0.05,hidden_dim=2,numepochs=500,network_type="rnn",sigmoid="logistic")

x1_test_step<???as.matrix(t(x1[(n_train+k+1),]))
x2_test_step<???as.matrix(t(x2[(n_train+k+1),]))
x3_test_step<???as.matrix(t(x3[(n_train+k+1),]))
x4_test_step<???as.matrix(t(x4[(n_train+k+1),]))
y_test_step<???as.matrix(y[(n_train+k+1)])
x_test_step<???array(c(x1_test_step,
x2_test_step,
x3_test_step,
x4_test_step),
dim=c(dim(x1_test_step),4))

pred_test<???t(predictr(model_step,
x_test_step))
if(i==start)forecast<???as.numeric(pred_test)
if(i>start)forecast<???cbind(forecast,
as.numeric(pred_test))