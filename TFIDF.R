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

Tidata=data.frame(matrix(ncol = ncol(news)-1,nrow = nrow(news)))
temp=data.frame(matrix(ncol = 1,nrow = nrow(news)))
colnames(temp)=c("TorF")
Tidata=cbind(temp,Tidata)

#Result assignment
Tidata$TorF=news$TorF
#TF related parameters
news_row_sum=rowSums(news)
#IDF related parameters
idfdata=news

f<-function(x) sum(x==0)
zero_number_of_eachcol=apply(idfdata,2,f) 


for (i in 1:200) {
  for (j in 2:791) {
    tf=news[i,j]/news_row_sum[i]
    idf=log(nrow(news)/(nrow(news)-zero_number_of_eachcol[j]))
    Tidata[i,j]=tf*idf
  }
}

d=Tidata
bd=d
d=scale(d[,2:791])
temp=data.frame(Tidata[,1])
colnames(temp)=c("TorF")
d=data.frame(cbind(temp,d))
d[is.na(d)] <- 0
Tidata=d

#logit

##Create data
logitdata=Tidata
##Training model
mylogit <- glm(logitdata$TorF ~., data = logitdata)
##Forecast results
predict1=predict(mylogit,logitdata,type = "response")
logitdata$predict=(predict(mylogit,logitdata,tpredictpe = "response")>1e-14)
logitdata$predict[which(logitdata$predict==FALSE)]=0
logitdata$predict[which(logitdata$predict==TRUE)]=1
##Confusion matrix and accuracy
logit_confusion=table(actual=logitdata$TorF,predictedclass=logitdata$predict)
logit_error_rate=(sum(logit_confusion)-sum(diag(logit_confusion)))/sum(logit_confusion)
"accuracy";1-logit_error_rate


#Bayes
##Create data
Bayesdata=bd
Bayesdata$TorF=as.factor(Bayesdata$TorF)
##Training model
myBayes <- naiveBayes(Bayesdata$TorF~.,data =Bayesdata)
##Forecast results
a=data.frame(Predict(myBayes,Bayesdata))

Bayesdata$predict=Predict(myBayes,Bayesdata)
Bayesdata$predict[which(Bayesdata$predict==0)]=0
Bayesdata$predict[which(Bayesdata$predict==1)]=1
##Confusion matrix and accuracy
Bayes_confusion=table(actual=Bayesdata$TorF,predictedclass=Bayesdata$predict)
Bayes_error_rate=(sum(Bayes_confusion)-sum(diag(Bayes_confusion)))/sum(Bayes_confusion)
"accuracy";1-Bayes_error_rate


#svm
##Create data
svmdata=Tidata
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

