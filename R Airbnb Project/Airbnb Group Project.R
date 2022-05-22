
rm(list=ls())

#Install the packages needed
library(readr)

#using readr packages to read the csv file
AB<-read.csv("AB_NYC_2019.csv")
summary(AB)
head(AB)

#check about the columns
dim(AB) #48896 data 16 columns
colnames(AB)
str(AB)

#check for the head AB Again
head(AB)

#drop for the unnecessary columns
MY_DF <- AB[,-c(1,2,3,4,6,7,8,13,15)]
head(MY_DF)
summary(MY_DF)

#convert the list into numerical data
MY_DF[3] <- list(as.numeric(unlist(MY_DF[3])))
MY_DF[4] <- list(as.numeric(unlist(MY_DF[4])))
MY_DF[5] <- list(as.numeric(unlist(MY_DF[5])))
MY_DF[6] <- list(as.numeric(unlist(MY_DF[6])))
MY_DF[7] <- list(as.numeric(unlist(MY_DF[7])))

MY_DF[1] <- as.factor(ifelse(MY_DF$neighbourhood_group == "Manhattan", 1, 0))
MY_DF[2] <- as.factor(ifelse(MY_DF$room_type == "Entire home/apt", 1, 0))


# remove na data
MY_DF <- na.omit(MY_DF)
head(MY_DF)
typeof(MY_DF$availability_365)
# data machine learning preprocessing using for 70% and 30% to train the data
set.seed(1)
ind <- sample(1:nrow(MY_DF),size=0.7*nrow(MY_DF),replace = FALSE)
train <- MY_DF[ind,]
test  <- MY_DF[-ind,]
head(train)

#First Multiple Regression
rg1 <- lm(price~.,data = train)
result <- summary(rg1)
result
rg1

#check for the confidence interval
confint(rg1)

#plot about rg1
plot(rg1)
plot(rg1, col='lightblue', pch=16)                        
abline(rg1, col="red")  

#Minimize Residuals and calculate MSE  Model Evaluation
#RSS RSE MSE MEAS
RSS_train <- sum(result$residuals^2)
result$sigma   ### Check it with system generated RSE(Residual standard error)
MSE_train <- mean(result$residuals^2)
y_cap <- predict(rg1, newdata = test)

data <- data.frame(pred = predict(rg1,test), actual = test$price)
head(data)
summary(data)
RSS_test = sum((data$actual-data$pred)^2)
MSE_test = mean((data$actual - data$pred)^2)
RSE_test = sqrt(RSS_test/(48895*0.7-3-1))                               
RSE_test

#RSS_r and TSS
RSS_r_test=sum(result$residuals^2)
RSS_r_test

#TSS
TSS_test=sum((test$price-mean(test$price))^2)
TSS_test


