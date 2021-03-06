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



#main
rate=2
news=read.csv(file = "newsdata.csv")
news=news[,which(colSums(news) > 0)]

ratedata=news

#Calculation of T & F frequency statistics
Tcol=colSums(news[1:107,])
Fcol=colSums(news[108:200,])


#Select variables with enough differences
ratedata=data.frame(matrix(ncol = 0,nrow = 200))
for (i in 2:791) {
  #Column with more than rate difference, selected
  if ((Tcol[i]>Fcol[i]*rate)|(Fcol[i]>Tcol[i]*rate)) {
    ratedata=cbind(data.frame(ratedata),data.frame(news[,i]))
  }
}

ratedata=cbind(news[,1],ratedata)
colnames(ratedata)=colnames(news[,1:ncol(ratedata)])



#logit
##Create data
logitdata=ratedata
##Training model
mylogit <- glm(logitdata$TorF ~., data = logitdata)
##Forecast results
predict1=predict(mylogit,logitdata,type = "response")

logitdata$predict=(predict(mylogit,logitdata,tpredictpe = "response")>0.4)
logitdata$predict[which(logitdata$predict==FALSE)]=0
logitdata$predict[which(logitdata$predict==TRUE)]=1

##Confusion matrix and accuracy
logit_confusion=table(actual=logitdata$TorF,predictedclass=logitdata$predict)
logit_error_rate=(sum(logit_confusion)-sum(diag(logit_confusion)))/sum(logit_confusion)
"accuracy";1-logit_error_rate


#Bayes
##Create data
Bayesdata=ratedata
Bayesdata$TorF=as.factor(Bayesdata$TorF)
##Training model
myBayes <- naiveBayes(Bayesdata$TorF~.,data =Bayesdata)
##Forecast results
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
svmdata=ratedata
##Training model
mysvm <- svm(TorF ~ ., data = svmdata)
##Forecast results

svmdata$predict=predict(mysvm,svmdata)
svmdata$predict[which(svmdata$predict<=1e-1)]=0
svmdata$predict[which(svmdata$predict>1e-1)]=1

##Confusion matrix and accuracy
svm_confusion=table(actual=svmdata$TorF,predictedclass=svmdata$predict)
svm_error_rate=(sum(svm_confusion)-sum(diag(svm_confusion)))/sum(svm_confusion)
"accuracy";1-svm_error_rate

#############

# Build a ROC object and compute the AUC, draw ROC, print AUC and the best THRESHOLDS

roc(svmdata$predict,svmdata$TorF, plot=TRUE, print.thres=TRUE, print.auc=TRUE)
roc(as.numeric(Bayesdata$predict),as.numeric(Bayesdata$TorF), plot=TRUE, print.thres=TRUE, print.auc=TRUE)
roc(logitdata$predict,logitdata$TorF, plot=TRUE, print.thres=TRUE, print.auc=TRUE)
