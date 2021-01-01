library(randomForest)
mydata<-read.csv('C:/Users/JAIKUMAR/Desktop/karthikeyan sir/Assignment/Random Forest(19th June_20)/Assignment/Fraud_check (1).csv')
TIRisky <- NULL
TIRisky <- ifelse(mydata$Taxable.Income<=30000,1,0)
mydata[,"TIRisky"] <- TIRisky

mydata$Undergrad <- as.factor(mydata$Undergrad)
mydata$Marital.Status <- as.factor(mydata$Marital.Status)
mydata$Urban <- as.factor(mydata$Urban)
mydata$TIRisky <- as.factor(mydata$TIRisky)

fraud_risky <- mydata[mydata$TIRisky == "1",] 
fraud_not_risky <- mydata[mydata$TIRisky == "0",]

data_train <- rbind(fraud_risky[1:93,], fraud_not_risky[1:357,])
data_test <- rbind(fraud_risky[94:124,], fraud_not_risky[357:476,])

# Building a random forest model on training data 
fit.forest <- randomForest(TIRisky~.,data=data_train, na.action=na.roughfix,importance=TRUE)

# Training accuracy 
mean(data_train$TIRisky == predict(fit.forest,data_train)) # 100% accuracy 

summary(fit.forest)


pred_train <- predict(fit.forest,data_train)
library(caret)

# Confusion Matrix
confusionMatrix(data_train$TIRisky, pred_train)
