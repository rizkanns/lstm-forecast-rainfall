library(tibble)
data1 <- read.csv("csv-harian-perak1.csv", header = TRUE, sep = ",")
data1 <- data.matrix(data1[,-1])
data1 <- data.matrix(data1[,-1])
data1 <- data.matrix(data1[,-1])
data1 <- data.matrix(data1[,-5])
data1[is.na(data1)] <- 0

data2 <- read.csv("csv-harian-perak2.csv", header = TRUE, sep = ",")
data2 <- data.matrix(data2[,-1])
data2 <- data.matrix(data2[,-1])
data2 <- data.matrix(data2[,-1])
data2[is.na(data2)] <- 0

data3 <- read.csv("csv-harian-juanda.csv", header = TRUE, sep = ",")
data3 <- data.matrix(data3[,-1])
data3 <- data.matrix(data3[,-1])
data3 <- data.matrix(data3[,-1])
data3[is.na(data3)] <- 0

ch1 <- read.csv("csv-harian-perak1.csv", header = TRUE, sep = ",")
ch1 <- data.matrix(ch1[,8])
ch1[is.na(ch1)] <- 0
ch1<-as.factor(ch1)

ch2 <- read.csv("csv-harian-perak2.csv", header = TRUE, sep = ",")
ch2 <- data.matrix(ch2[,8])
ch2[is.na(ch2)] <- 0

ch3 <- read.csv("csv-harian-juanda.csv", header = TRUE, sep = ",")
ch3 <- data.matrix(ch3[,8])
ch3[is.na(ch3)] <- 0

head(data1)
head(data2)
head(data3)

head(ch1)
head(ch2)
head(ch3)

## timelag
library(data.table)
data1 = data.table(data1)
lag1<-data1[, shift(.SD, 1:3, NA, "lead", TRUE), .SDcols=1:11]
datalag1<-cbind(data1,lag1,ch1)

data2 = data.table(data2)
lag2<-data2[, shift(.SD, 1:3, NA, "lead", TRUE), .SDcols=1:11]
datalag2<-cbind(data2,lag2,ch2)

data3 = data.table(data3)
lag3<-data3[, shift(.SD, 1:3, NA, "lead", TRUE), .SDcols=1:11]
datalag3<-cbind(data3,lag3,ch3)


#library
library(zoo)
library(e1071)
library(caret) #createdatapartition
#require(dplyr)
#require(stats)
library(data.table)
library(readxl)

#Read Data
setwd("E:/5115100095/TA/Koding/timelag")
load("E:/5115100095/TA/Koding/timelag/timelag_environtment.RData")
#data <- read.csv("laporan_iklim_harian_karangkates_87_12_nino_ch.csv",header=TRUE,sep=";", 
#                  col.names=c("tgl","smin","smaks","sr","kr","lp","kar","kat",
#                              "nino12","nino3","nino34","nino4","ch1","ch2"),
#                 stringsAsFactors=FALSE)
data <- read_excel("laporan_iklim_harian_karangkates_87_12_nino_ch.xlsx",
                   col_types = c("text", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric"))
colnames(data)<-c("smin","smaks","sr","kr","ch","lp","kar","kat",
                  "nino12","nino3","nino34","nino4","ch1","ch2")

#Mengubah tipe data char ke numeric
#data$smin<-as.numeric(data$smin)
#data[,1:14] <- sapply(data[,1:14], as.numeric)

#Menghilangkan variabel tanggal
data<-data[,c(-1,-6)]
data1$ch1<-as.factor(data1$ch1) #factor dijalankan setelah is.na
data$ch2<-as.factor(data$ch2)
ch1<-data$ch1 
ch2<-data$ch2
head(data)
str(data)

#Data curah hujan dengan kategori:
#1. tidak hujan (0-5 mm/hari)
#2. hujan (>5 mm/hari)
data1<-data[,c(-12,-13)]
head(data1)
#View(data1)

#Data curah hujan dengan kategori:
#1. tidak hujan (0-20 mm/hari)
#2. hujan (>20 mm/hari)
data2<-data[,-12]
head(data2)

#fitur timelag
data1 = data.table(data1)
lag1<-data1[, shift(.SD, 1:3, NA, "lead", TRUE), .SDcols=1:11]
datalag1<-cbind(data1,lag1,ch1)
head(datalag1)
#View(datalag1)
anyNA(datalag1)
str(datalag1)
datalag1[is.na(datalag1)] <- 0

#https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
# calculate correlation matrix
correlationMatrix <- cor(datalag1[,1:44])
# summarize the correlation matrix
print(correlationMatrix)
plot(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)

#Rank features by importance using the caret r packageR
# ensure results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(lattice)
library(ggplot2)
library(caret)
options(error=utils::recover)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
datalag1$ch1<-as.factor(datalag1$ch1)
model <- train(ch1~., data=datalag1, method="lvq",preProcess="scale", trControl=control)
plot(model)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

library(randomForest)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(datalag1[,1:44], datalag1$ch1, sizes=c(1,5,10,15,20,25,30,35,40,41,42,43,44), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

#create data training dan testing
set.seed(3033)
intrain <- createDataPartition(y = datalag1$ch1, p= 0.7, list = FALSE)
training <- datalag1[intrain,]
testing <- datalag1[-intrain,]
dim(training)
dim(testing)



trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

svm_Linear <- train(ch1 ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
# prepare sample data in the form of data frame with cols of timesteps (x) and values (y)  
data(AirPassengers) 
head(DF)
monthly_data <- unclass(AirPassengers)
months <- 1:144
DF <- data.frame(months,monthly_data)
colnames(DF)<-c("x","y")

# train an svm model, consider further tuning parameters for lower MSE
svmodel <- svm(y ~ x,data=DF, type="eps-regression",kernel="radial",cost=10000, gamma=10)
#specify timesteps for forecast, eg for all series + 12 months ahead
nd <- 1:156
#compute forecast for all the 156 months 
prognoza <- predict(svmodel, newdata=data.frame(x=nd))
head(prognoza)
#plot the results
ylim <- c(min(DF$y), max(DF$y))
xlim <- c(min(nd),max(nd))
plot(DF$y, col="blue", ylim=ylim, xlim=xlim, type="l")
par(new=TRUE)
plot(prognoza, col="red", ylim=ylim, xlim=xlim)
