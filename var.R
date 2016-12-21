setwd("C:/Users/Aashna/Documents/R/win-library/3.3")
#install.packages("mlbench")
#install.packages("caret")
#install.packages("e1071")
#install.packages("randomForest")
library(mlbench)
library(caret)
library(dplyr)
library(e1071)
library(randomForest)
t1.data<- read.table(file="Problem2_expressionMatrix.txt",header=TRUE)
t1.patient<-read.table(file="Problem2_patientData.txt", header=TRUE)
t1.data.t<- t(t1.data)

relapse<- grepl("TRUE", t1.patient[,2])
healthy<- grepl("FALSE", t1.patient[,2])

#create vector of 0s and 1s for each response
relapse.status<- vector(mode="numeric", length= nrow(t1.patient))
relapse.status[relapse]=1
relapse.status[healthy]=0


t1data.patient<- as.data.frame(rbind(cbind(as.data.frame(t1.data.t),relapse.status )))
#Create partitions
train_set=sample(2,nrow(t1data.patient),replace=TRUE,prob=c(0.75,0.25))
train_data<-t1data.patient[train_set==1,]
test_data<-t1data.patient[train_set==2,]
modelfit<-train(relapse.status~.,data=train_data,method="rf",importance=TRUE)#Use of training data set for modelfitting
predictors=varImp(modelfit)#Finding the important variables 

#Finding important variables from the entire dataset
t1data.patient_fit<-as.data.frame(t1data.patient)

model_fit<-train(relapse.status~.,data=t1data.patient_fit,method="rf",importance=TRUE)
predictors_fit<-varImp(model_fit)


