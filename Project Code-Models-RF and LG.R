
## Project DAAN 881
rm(list = ls())
gc()

library(readxl)
library(caret)
library(ggplot2)
library(caret)
library(dplyr)
library(lubridate)
library(e1071)
library(randomForest)
library(caTools)
library(ROCR)


SBA_Dataset <- read.csv("C:/Data File/SBA_Dataset.csv")



summary(SBA_Dataset)
str(SBA_Dataset)

SBA_Dataset$MIS_Status<-ifelse(SBA_Dataset$MIS_Status==0,"Deny","Approve")

SBA_Dataset$MIS_Status<-as.factor(SBA_Dataset$MIS_Status) 

SBA_Dataset$MIS_Status<-as.factor(SBA_Dataset$MIS_Status)
SBA_Dataset$DisbursementDate<-as.Date(SBA_Dataset$DisbursementDate,format="%d-%b-%y")
SBA_Dataset$ApprovalDate<-as.Date(SBA_Dataset$ApprovalDate,format="%d-%b-%y")

#######################################################
#### Random Forest
#######################################################

#Data Partitioning  70%  and 90% Training


set.seed(5867)

SBA_Dataset_boruta<-SBA_Dataset
SBA_Dataset_boruta$MIS_Status<-as.factor(SBA_Dataset_boruta$MIS_Status)
SBA_Dataset_rf_vector = sample.split(SBA_Dataset_boruta,SplitRatio = 0.70) 
SBA_Dataset_rf_train =subset(SBA_Dataset_boruta,SBA_Dataset_rf_vector ==TRUE) # Train
SBA_Dataset_rf_test=subset(SBA_Dataset_boruta, SBA_Dataset_rf_vector==FALSE)


######

##Please uncooment for For 10 folds cross validtaion, 3 times###########

#set.seed(5666)

#rfcontrol <- trainControl(method='repeatedcv', number=10, repeats=3, verboseIter=TRUE)
##rftunegrid<-expand.grid(.mtry=(10:25))

#SBA_Dataset_rfmodel <- train(MIS_Status~.,  data=SBA_Dataset_rf_train, method='rf', 
#                             metric='Accuracy',tunegrid=rftunegrid,ntree=50,
#                            trControl=rfcontrol,tunegrid=rftunegrid)
#


rfmatrix<-data.frame(index=1:10, Mtry=1:10, Ntree=1:10,
                  precision=1:10, Recall=1:10,AUC=1:10)


for (i in 1:1)
{
rfmatrix$index[i]<-i
rfmatrix$X.Ntree[i]<-sample(299:300,1)
rfmatrix$X.Mtry[i]<-sample(14:14,1)

#SBA_Dataset_rfmodel<- randomForest(MIS_Status~., SBA_Dataset_rf_train,
#                                   ntree=300,mtry=20)

set.seed(5867)
SBA_Dataset_rfmodel<- randomForest(MIS_Status~., SBA_Dataset_rf_train,
                              ntree=rfmatrix$X.Ntree[i],mtry=rfmatrix$X.Mtry[i])
#set.seed(5867)
#SBA_Dataset_rfmodel<- randomForest(MIS_Status~., SBA_Dataset_rf_train,
 #                               ntree=300,mtry=14)

gc()
SBA_Dataset_rfmodel_train_pred <- predict(SBA_Dataset_rfmodel,  SBA_Dataset_rf_train)                   
mean(SBA_Dataset_rfmodel_train_pred==SBA_Dataset_rf_train$MIS_Status)
confusionMatrix(SBA_Dataset_rfmodel_train_pred,SBA_Dataset_rf_train$MIS_Status)

SBA_Dataset_rfmodel_test_pred <- predict(SBA_Dataset_rfmodel, SBA_Dataset_rf_test)                   
mean(SBA_Dataset_rfmodel_test_pred== SBA_Dataset_rf_test$MIS_Status)
cmtest<-confusionMatrix(SBA_Dataset_rfmodel_test_pred, SBA_Dataset_rf_test$MIS_Status)

rfmatrix$X.Precsion[i]<-cmtest$byClass['Pos Pred Value']
rfmatrix$X.Recall[i]<-cmtest$byClass['Sensitivity']

rocpred<-prediction(as.numeric(SBA_Dataset_rfmodel_test_pred),as.numeric(SBA_Dataset_rf_test$MIS_Status))

rfauc<-performance(rocpred,"auc")
rfmatrix$X.AUC[i]<-as.numeric(rfauc@y.values)


print (paste("Iteration-> ",i))
print (paste("Ntree-> ",rfmatrix$X.Ntree[i]))
print (paste("Ntree Actual-> ",SBA_Dataset_rfmodel$ntree))
print (paste("Mtry-> ",rfmatrix$X.Mtry[i]))
print (paste("Mtry Actual-> ",SBA_Dataset_rfmodel$mtry))
print (paste("Precision-> ",rfmatrix$X.Precsion[i]))
print (paste("Recall-> ",rfmatrix$X.Recall[i]))
print (paste("Auc-> ",rfmatrix$X.AUC[i]))
print ("##########################")

}# END for (i in 1:10)




#######################################################
#### Logistic regression
#######################################################

gc()
set.seed(5867)

SBA_Dataset_boruta<-SBA_Dataset
str(SBA_Dataset_boruta)
SBA_Dataset_boruta$State<-as.factor(SBA_Dataset_boruta$State)
SBA_Dataset_boruta$NACIS_Classification<-as.factor(SBA_Dataset_boruta$NACIS_Classification)
#SBA_Dataset_boruta$ApprovalDate<-as.factor(SBA_Dataset_boruta$ApprovalDate)
#SBA_Dataset_boruta$DisbursementDate <-as.factor(SBA_Dataset_boruta$DisbursementDate)

library(caTools)   
SBA_Dataset_ro_vector = sample.split(SBA_Dataset_boruta,SplitRatio = 0.70) 
my_ro_train =subset(SBA_Dataset_boruta,SBA_Dataset_ro_vector ==TRUE) # Train
my_ro_test=subset(SBA_Dataset_boruta, SBA_Dataset_ro_vector==FALSE)


my_ro_model <- glm(MIS_Status ~., data = my_ro_train , family =  binomial)

summary(my_ro_model)

# Train predictions
my_ro_train_pred<-predict(my_ro_model,newdata = my_ro_train,type = "response")
my_ro_train_class_pred<- as.numeric(my_ro_train_pred > 0.5)
mean(my_ro_train_class_pred == my_ro_train$MIS_Status)
# Test predictions
my_ro_test_pred<-predict(my_ro_model,newdata = my_ro_test,type = "response")
my_ro_test_class_pred<- as.numeric(my_ro_test_pred > 0.5)
mean(my_ro_test_class_pred == my_ro_test$MIS_Status)

