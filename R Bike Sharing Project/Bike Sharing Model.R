rm(list=ls())
setwd("C:/Users/Victoria Zhang/Documents/HKBU DABE Courses/second semester/ECON7035 Artificial Intelligence for Business/group project/bike sharing")
bike= read.csv("hour.csv")
bike2= read.csv("day.csv")
head(bike)
#View(bike)
typeof(bike$time)
is.na(bike)
na.omit(bike)
bike$season <-as.factor(bike$season)
bike$holiday <-as.factor(bike$holiday)
bike$weekday<-as.factor(bike$weekday)
bike$weathersit<-as.factor(bike$weathersit)

bike2$season <-as.factor(bike2$season)
bike2$holiday <-as.factor(bike2$holiday)
bike2$weekday<-as.factor(bike2$weekday)
bike2$weathersit<-as.factor(bike2$weathersit)
summary(bike)


set.seed(1)
a = seq(1,nrow(bike),by=1)
a
i = sample(a,nrow(bike)*0.7, replace=FALSE) 
train_bike=bike[i,]
test_bike=bike[-i,]
head(train_bike)
head(test_bike)

#cnt
p=c(1,2,4,5,9,15,16)
train_bike1=train_bike[,-p]
test_bike1=test_bike[,-p]

train_x1 = train_bike1[, -10]
train_y1 = train_bike1[,10]

test_x1 = test_bike1[, -10]
test_y1 = test_bike1[, 10]

#casual
p=c(1,2,4,5,9,16,17)
train_bike2=train_bike[,-p]
test_bike2=test_bike[,-p]

train_x2 = train_bike2[, -10]
train_y2 = train_bike2[,10]

test_x2 = test_bike2[, -10]
test_y2 = test_bike2[, 10]


#registered
p=c(1,2,4,5,9,15,17)
train_bike3=train_bike[,-p]
test_bike3=test_bike[,-p]

train_x3 = train_bike3[, -10]
train_y3 = train_bike3[,10]

test_x3 = test_bike3[, -10]
test_y3 = test_bike3[, 10]


#lm model
#for cnt
model1<- lm(train_bike1$cnt~train_bike1$season+train_bike1$hr+train_bike1$holiday+train_bike1$weekday+ train_bike1$weathersit+train_bike1$temp+train_bike1$atemp+train_bike1$hum+train_bike1$windspeed)
summary(model1)
data1 <- data.frame(pred = predict(model1,train_x1), actual = test_y1)
head(data1)
predicted1 = predict(model1,test_bike1)
MSE_test1 = mean((predicted- test_y1)^2)
MSE_test1


sqrt(mean(model1$residuals^2))

#Casual
model2<- lm(train_bike2$casual~train_bike2$season+train_bike2$hr+train_bike2$holiday+train_bike2$weekday+ train_bike2$weathersit+train_bike2$temp+train_bike2$atemp+train_bike2$hum+train_bike2$windspeed)
summary(model2)
predicted2 = predict(model2,test_bike2)
#data <- data.frame(pred = predict(model2,train_x), actual = test_y)
#head(data)
MSE_test2 = mean((predicted2 - test_y2)^2)
MSE_test2


#registered
model3<- lm(train_bike3$registered
            ~train_bike3$season+train_bike3$hr+train_bike3$holiday+train_bike3$weekday+ train_bike3$weathersit+train_bike3$temp+train_bike3$atemp+train_bike3$hum+train_bike3$windspeed)
summary(model3)
predicted3 = predict(model3,test_bike3)
data3 <- data.frame(pred = predict(model3,train_x3), actual = test_y3)
#head(data)
MSE_test3 = mean((predicted3 - test_y3)^2)
MSE_test3

sqrt(mean(model3$residuals^2))

AIC(model3)
BIC(model3)

#for the MSE Graph
#test.mse1<-rep(NA,18)
#test.mse2<-rep(NA,18)
#test.mse3<-rep(NA,18)
#for(i in 1:18){
  #lm_1<-lm(cnt~season+hr+holiday+weekday+weathersit+temp+atemp+hum+windspeed,data=train_bike1)
  #pred_1 <- predict(lm_1,test_bike1)
  #test.mse1[i] <- mean((pred_1 - test_bike1$cnt)^2)
  #lm_2<-lm(casual~season+hr+holiday+weekday+weathersit+temp+atemp+hum+windspeed,data=train_bike2)
  #pred_2 <- predict(lm_2,test_bike2)
  #test.mse2[i] <- mean((pred_2 - test_bike2$casual)^2)
  #lm_3<-lm(registered~season+hr+holiday+weekday+weathersit+temp+atemp+hum+windspeed,data=train_bike3)
  #pred_3 <- predict(lm_3,test_bike3)
  #test.mse3[i] <- mean((pred_3 - test_bike3$registered)^2)
#}

#dataframe3<-data.frame(1:18,test.mse1)
#ggplot(aes(x=1:18,y=test.mse1),data=dataframe3)+geom_point(col = "red")+geom_line(col="blue")+labs(title="Test set MSE under best subset method",x="size",y="Test mse")+theme_bw()



#best subset
#cnt
library(leaps)
regfit1 <- regsubsets(cnt~season+hr+holiday+weekday+weathersit+temp+atemp+hum+windspeed,data=train_bike1, nvmax = 18)
reg.summary1 <- summary(regfit1)
reg.summary1

test.matrix1<-model.matrix(cnt~season+hr+holiday+weekday+weathersit+temp+atemp+hum+windspeed,data=test_bike1, nvmax = 18)

#casual
regfit2 <- regsubsets(casual~season+hr+holiday+weekday+weathersit+temp+atemp+hum+windspeed,data=train_bike2, nvmax = 18)
reg.summary2 <- summary(regfit2)
reg.summary2

test.matrix2<-model.matrix(casual~season+hr+holiday+weekday+weathersit+temp+atemp+hum+windspeed,data=test_bike2, nvmax = 18)

#registered
regfit3 <- regsubsets(registered~season+hr+holiday+weekday+weathersit+temp+atemp+hum+windspeed,data=train_bike3, nvmax = 18)
reg.summary3 <- summary(regfit3)
reg.summary3

test.matrix3<-model.matrix(registered~season+hr+holiday+weekday+weathersit+temp+atemp+hum+windspeed,data=test_bike3, nvmax = 18)
#mse
test.mse1<-rep(NA,18)
test.mse2<-rep(NA,18)
test.mse3<-rep(NA,18)

for(i in 1:18){
  coef_i1 <- coef(regfit1,i)
  reg_pred_i1 <- test.matrix1[, names(coef_i1)] %*% coef_i1
  test.mse1[i] <- mean((reg_pred_i1 - test_bike1$cnt)^2)
  coef_i2 <- coef(regfit2,i)
  reg_pred_i2 <- test.matrix2[, names(coef_i2)] %*% coef_i2
  test.mse2[i] <- mean((reg_pred_i2 - test_bike2$casual)^2)
  coef_i3 <- coef(regfit3,i)
  reg_pred_i3 <- test.matrix3[, names(coef_i3)] %*% coef_i2
  test.mse3[i] <- mean((reg_pred_i3 - test_bike3$registered)^2)
}

#dataframe3<-data.frame(1:18,test.mse1,test.mse2,test.mse3)
#ggplot(aes(x=1:18,y=test.mse1),data=dataframe3)+geom_point(col = "red")+geom_line(col="blue")+labs(title="Test set MSE under best subset method",x="size",y="Test mse")+theme_bw()
plot(1:18,test.mse3,col="green",type="l",xlab = "Number of Variables",ylab = "Test MSE",xaxt="n",yaxt="n",main="Best Subset Test MSE",lwd=2)
par(new=TRUE)
plot(1:18,test.mse1,col="red",type="l",xlab = "Number of Variables",ylab = "Test MSE",lwd=2)
par(new=TRUE)
plot(1:18,test.mse2,col="orange",type="l",xlab = "Number of Variables",ylab = "Test MSE",xaxt="n",yaxt="n",lwd=2)


legend("topright",legend=c("CNT","Casual","Registered"), col=c("red","orange","green"),lty=1,lwd=2)


#bic graph
par(mfrow=c(2,2))
library(ggplot2)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",
     type="l",main="CNT BIC Performance")

points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)],
       col = "red", pch = 25)

#cp
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",
     type="l",main="CNT Cp Performance")

points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)],
       col = "red", pch = 25)


#rsq
plot(reg.summary$rsq,xlab="Number of Variables",ylab="R Square",
     type="l",main="CNT R Square Performance")

points(which.min(reg.summary$rsq), reg.summary$rsq[which.min(reg.summary$rsq)],
       col = "red", pch = 25)

#adjusted R Square
plot(reg.summary$adjr2,xlab="Number of Variables",ylab=" Adjusted R Square",
     type="l",main="CNT Adjusted R Square Performance")

points(which.min(reg.summary$adjr2), reg.summary$adjr2[which.min(reg.summary$adjr2)],
       col = "red", pch = 25)

# decision tree models
library(tree)
library(rpart)
tree<-tree(cnt~season+hr+holiday+weekday+weathersit+temp+atemp+hum+windspeed,data=train_bike1)
summary(tree)
plot(tree)
text(tree,pretty=0)
tree_prediction1<-predict(tree,newdata=test_bike1)
tree_mse1<-mean((test_bike1$cnt-tree_prediction1)^2)
tree_mse1

tree1<-rpart(cnt~season+hr+holiday+weekday+weathersit+temp+atemp+hum+windspeed,data=train_bike1)
summary(tree1)
rsq.rpart(tree1)
#perform cv.tree

tree_cv<-cv.tree(tree)
tree_cv
plot(tree_cv$size,tree_cv$dev,xlab="Terminal Nodes",ylab="cross validation errors",type="b",main="Cross Validation Graph")

#prune the tree
tree_prune<-prune.tree(tree,best=6)
plot(tree_prune)
text(tree_prune)

#mse after pruning the tree
tree_prediction2<-predict(tree_prune,newdata=test_bike1)
tree_mse2<-mean((test_bike1$cnt-tree_prediction2)^2)
tree_mse2

# for the mse graphs
tree.mse1<-rep(NA,18)
tree.mse2<-rep(NA,18)

for(i in 1:18){
tree_1<-tree(cnt~season+hr+holiday+weekday+weathersit+temp+atemp+hum+windspeed,data=train_bike1,split = c("deviance","gini"))
pred_1 <- predict(tree_1,test_bike1)
summary(tree_1)
tree.mse1[i] <- mean((pred_1 - test_bike1$cnt)^2)
tree_prune<-prune.tree(tree_1,best=i)
tree_prediction2<-predict(tree_prune,newdata=test_bike1)
tree.mse2[i]<-mean((test_bike1$cnt-tree_prediction2)^2)
}


plot(1:18,tree.mse1,col="green",type="l",xlab = "Number of Variables",ylab = "Test MSE",xaxt="n",yaxt="n",main="Best Subset Test MSE",lwd=2)
par(new=TRUE)
plot(1:18,tree.mse2,col="red",type="l",xlab = "Number of Variables",ylab = "Test MSE",lwd=2)

legend("topright",legend=c("CNT","Casual","Registered"), col=c("red","orange","green"),lty=1,lwd=2)


R2 <- 1 - (sum((test_bike1$cnt-tree_prediction2)^2)/sum((test_bike1$cnt-mean(test_bike1$cnt))^2))
R2
#tree nodes 8 tree nodes is best
which.min(tree_cv$size)
#xgboost
#install.packages("xgboost")
library(xgboost)
library("Matrix")
set.seed(1)
a = seq(1,nrow(bike),by=1)
a
i = sample(a,nrow(bike)*0.7, replace=FALSE) 
train_bike=bike[i,-1]
test_bike=bike[-i,-1]
head(train_bike)
head(test_bike)

train_x = data.matrix(train_bike[, -10])
train_y = train_bike[,10]

test_x = data.matrix(test_bike[, -10])
test_y = test_bike[, 10]

xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

xgbc = xgboost(data = xgb_train,max_depth=6, eta=0.1,objective="reg:squarederror", nround=100)
pred_y = predict(xgbc, xgb_test)
mse = mean((test_y - pred_y)^2)
mse

summary(xgbc)
importance <- xgb.importance(model = xgbc)  
xgb.plot.importance(importance,main="Feature Importance Gain Graph",col="purple",measure="Gain")
head(importance)


#random forest
library(randomForest)
library(devtools)
install_github("h2oai/h2o-3/h2o-r/ensemble/h2oEnsemble-package")
p_load("h2o")
library(h2o)
h2o.init(nthreads = -1)
h2o.removeAll() 

bike4=bike[,-p]
set.seed(1)
a = seq(1,nrow(bike4),by=1)
a
i = sample(a,nrow(bike4)*0.7, replace=FALSE) 
train_bike4=bike4[i,-10]
test_bike4=bike4[-i,-10]
y.train4<-bike4[i,10]
y.test4<-bike4[-i,10]
head(train_bike4)
head(test_bike4)

#cnt training set
train_h2o1<- as.data.frame(train_bike4)

train_h2o1$cnt <- as.factor(y.train4)

train_h2o1 <- as.h2o(train_h2o1)

#cnt test set
test_h2o1<- as.data.frame(test_bike4)

test_h2o1$cnt <- as.factor(y.test4)

test_h2o1 <- as.h2o(test_h2o1)

dev.off()
p=6
rf_cnt1<-h2o.randomForest(1:9,10,ntree=100,max_depth=p,training_frame=train_h2o1)
perf_cnt1 <- h2o.performance(rf_cnt1)
#predict_cnt1 <-  h2o.predict(rf_cnt1,test_h2o1)
summary(rf_cnt1)
varimp_rf_cnt1<- h2o.varimp(rf_cnt1)
varimp_rf_cnt1
varimp_rf_cnt1_plot <- h2o.varimp_plot(rf_cnt1)
varimp_rf_cnt1_plot
varimp_rf_cnt1_confusion_matrix<-h2o.confusionMatrix(rf_cnt1)
varimp_rf_cnt1_confusion_matrix

#rf_cnt2<-randomForest(train_bike4,y.train4,test_bike4,y.test4,importance=TRUE,ntree=200,mtry=p)
#importance(rf_cnt2)

#rf_cnt3<-randomForest(train_bike4,y.train4,test_bike4,y.test4,importance=TRUE,ntree=300,mtry=p)
#importance(rf_cnt3)

rf_cnt4<-h2o.randomForest(1:9,10,ntree=400,max_depth=p,training_frame=train_h2o1)
perf_cnt4 <- h2o.performance(rf_cnt4)
#predict_cnt4 <-  h2o.predict(rf_cnt4,test_h2o1)
summary(rf_cnt4)
varimp_rf_cnt4<- h2o.varimp(rf_cnt4)
varimp_rf_cnt4
varimp_rf_cnt4_plot <- h2o.varimp_plot(rf_cnt4)
varimp_rf_cnt4_plot
varimp_rf_cnt4_confusion_matrix<-h2o.confusionMatrix(rf_cnt4)
varimp_rf_cnt4_confusion_matrix

plot(perf_cnt1,xlab = "Number of Trees",ylab="Test MSE",type="l",lwd=2,col="black")
lines(perf_cnt4,col="red",type="l",xlab = "Number of Trees",ylab = "Test MSE")
#lines(rf_cnt3$test$mse,col="blue",type="l",xlab = "Number of Trees",ylab = "Test MSE")
#lines(rf_cnt4$test$mse,col="red",type="l",xlab = "Number of Trees",ylab = "Test MSE")
legend("topright",legend=c("ntree=100","ntree=400"),
       col=c("black", "red"))



#adjusted R Square
pred=predict(rf3,y.test)
summary(rf3$test$rsq)


