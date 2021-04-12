f=function(myfit)
{
  #------------------------------
  #Finding probabilities
  #------------------------------
  
  prob=predict(fit,type="prob")[,2]
  
  #--------------------------------------------------------
  #Creating space for sensititvity and specificity
  #--------------------------------------------------------
  
  observed=train_data$y
  cutpoints = seq(0,1,0.01)
  sensitivity = seq(1,101,1)
  specificity = seq(1,101,1)
  cutpoint_perf = cbind(cutpoints,sensitivity,specificity)
  x=data.frame(table(observed))
  n1=x[2,2]
  n2=x[1,2]
  
  for (i in 1:101)
  {
    pred = ifelse(prob < cutpoint_perf[i,1],0,1)
    sumed = pred + observed
    pred1_1 = ifelse(sumed==2,1,0)
    correct1_1 = sum(pred1_1)
    pred0_0 = ifelse(sumed==0,1,0)
    correct0_0 = sum(pred0_0)
    cutpoint_perf[i,2] = correct1_1/n1
    cutpoint_perf[i,3] = correct0_0/n2
  }
  
  cutvalue_table = data.frame(cutpoint_perf)
  cutvalue_table$diff = abs(cutvalue_table$sensitivity-cutvalue_table$specificity)
  cut_point=subset(cutvalue_table,cutvalue_table$diff==min(cutvalue_table$diff))
  optimum_cut=mean((cut_point$cutpoints))
  
  #Prediction for train data
  #---------------------------------
  train_predicted=ifelse(prob<optimum_cut,0,1)
  
  #Classification Result for train data
  #---------------------------------------
  train_sensitivity=table(observed,train_predicted)[2,2]/rowSums(table(observed,train_predicted))[2]
  train_specificity=table(observed,train_predicted)[1,1]/rowSums(table(observed,train_predicted))[1]
  train_misclass_error=mean(observed!=train_predicted)
  
  #---------------------------------------------
  #ROC curve & AUC value for train data
  #---------------------------------------------
  library(ROCR)
  par(mfrow=c(1,2))
  pr1 =prediction(prob, observed)
  roc1=performance(pr1, measure = "tpr", x.measure = "fpr")
  plot(roc1,col="red",xlab="1-Specificity",ylab="Sensitivity",main="ROC of train data")
  abline(a=0,b=1,lwd=1,lty=4,col="black")
  
  auc1 = performance(pr1, measure = "auc")
  train_auc = auc1@y.values[[1]]
  
  #############################################################
  
  #predicting prob & response for test data
  #--------------------------------------------
  
  testprob=predict(fit,newdata=test_data,type="prob")[,2]
  test_predicted=ifelse(testprob<optimum_cut,0,1)
  length(test_data$y)
  length(testprob)
  #Classification Result for test data
  #---------------------------------------
  test_sensitivity=table(test_data$y,test_predicted)[2,2]/rowSums(table(test_data$y,test_predicted))[2]
  test_specificity=table(test_data$y,test_predicted)[1,1]/rowSums(table(test_data$y,test_predicted))[1]
  test_misclass_error=mean(test_data$y!=test_predicted)
  
  #---------------------------------------------
  #ROC curve & AUC value for test data
  #---------------------------------------------
  
  pr2 =prediction(testprob, test_data$y)
  roc2=performance(pr2, measure = "tpr", x.measure = "fpr")
  plot(roc2,col="red",xlab="1-Specificity",ylab="Sensitivity",main="ROC of test data")
  abline(a=0,b=1,lwd=1,lty=4,col="black")
  
  auc2 = performance(pr2, measure = "auc")
  test_auc = auc2@y.values[[1]]
  
  g=data.frame(train=c(train_auc,train_misclass_error,train_sensitivity,train_specificity),
               test=c(test_auc,test_misclass_error,test_sensitivity,test_specificity))
  g=round(g,4)
  row.names(g)=c("AUC","Class.Error","Sensitivity","specificty")
  h=list(optimum_cutvalue=optimum_cut,model_result=g)
  print(h)
}

result=f(fit)


#===================================
#random forest model
#===================================

set.seed(344)

# Create a Random Forest model with default parameters
library(rpart)
library(rpart.plot)
fit <- rpart(y~ ., data = train_data,method="class")
rpart.plot(fit)
summary(fit)

result=f(fit)




















str(test)
train_pred=predict(model1,test,type="class")
conf_matrix=table(test$Y,train_pred)
sum(is.na(train_pred))
mean(test$Y==train_pred)
colSums(is.na(train_pred))
#=======================================
#AUC AND ROC
#=======================================
library(pROC)
predwithprob=predict(model1,test,type="prob")
roc=roc(test$Y,predwithprob[,2])
auc=auc(test$Y,predwithprob[,2])
plot(roc)


a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(Y~ ., data = train, ntree = 100, mtry = i, importance = TRUE)
  predValid <- predict(model3, type = "class")
  a[i-2] = mean(predValid == train$Y)
}

a

plot(3:8,a)



testorigina=data.frame(fread("C:\\Users\\System2\\Desktop\\suneel\\project1\\citibank\\test_citibank.csv"))
head(testorigina)


