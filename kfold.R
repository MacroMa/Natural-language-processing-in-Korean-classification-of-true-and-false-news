#Packages used in the project

library(Matrix)
library(maps)
library(reshape)
library(tidyr)
library(dplyr)
library(reshape2)
library(grid)
library(tree)
library(party)
library(rpart)
library(e1071)
library(MASS)
library(ggplot2)
library(klaR)
library(pROC)

set.seed(2222)

news=read.csv(file = "newsdata.csv")
news=news[,which(colSums(news) > 0)]

#Assign training sets and test sets
k <- 10
fitdata=news
fitdata$kfold <- rep_len(1:k,nrow(fitdata))
data.Trained <- dplyr::filter(fitdata,kfold!= 2)
data.Test <- dplyr::filter(fitdata,kfold == 2)
data.Trained=data.Trained[,-792]
data.Test=data.Test[,-792]


#logit

##Create data
logitdata=data.Trained


##Training model
mylogit <- glm(logitdata$TorF ~., data = logitdata)

#Forecast results
logitdata=data.Test
logitdata$predict=(abs(predict(mylogit,logitdata,tpredictpe = "response"))>6)
logitdata$predict[which(logitdata$predict==FALSE)]=0
logitdata$predict[which(logitdata$predict==TRUE)]=1

c=data.frame(logitdata$TorF,logitdata$predict)
##Confusion matrix and accuracy
logit_confusion=table(actual=logitdata$TorF,predictedclass=logitdata$predict)
logit_error_rate=(sum(logit_confusion)-sum(diag(logit_confusion)))/sum(logit_confusion)
"accuracy";1-logit_error_rate


#Bayes
##Create data
Bayesdata=data.Trained
Bayesdata$TorF=as.factor(Bayesdata$TorF)
##Training model
myBayes <- naiveBayes(Bayesdata$TorF~.,data =Bayesdata,laplace = 0)
##Forecast results
Bayesdata=data.Test
Bayesdata$predict=Predict(myBayes,Bayesdata)
Bayesdata$predict=as.numeric(Bayesdata$predict)
Bayesdata$predict[which(Bayesdata$predict<1.5)]=0
Bayesdata$predict[which(Bayesdata$predict>1.5)]=1
##Confusion matrix and accuracy
Bayes_confusion=table(actual=Bayesdata$TorF,predictedclass=Bayesdata$predict)
Bayes_error_rate=(sum(Bayes_confusion)-sum(diag(Bayes_confusion)))/sum(Bayes_confusion)
"accuracy";1-Bayes_error_rate


#svm
##Create data
svmdata=data.Trained
##Training model
mysvm <- svm(TorF ~ ., data = svmdata)
##Forecast results
svmdata=data.Test
svmdata$predict=predict(mysvm,svmdata)
svmdata$predict[which(svmdata$predict<=1e-1)]=0
svmdata$predict[which(svmdata$predict>1e-1)]=1
##Confusion matrix and accuracy
svm_confusion=table(actual=svmdata$TorF,predictedclass=svmdata$predict)
svm_error_rate=(sum(svm_confusion)-sum(diag(svm_confusion)))/sum(svm_confusion)
"accuracy";1-svm_error_rate

