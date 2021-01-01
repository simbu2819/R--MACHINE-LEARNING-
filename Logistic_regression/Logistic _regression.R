
creditcard<-read.csv("C:/Users/JAIKUMAR/Desktop/karthikeyan sir/Assignment/Logistic_regression(11th June_20)/Assignments/creditcard.csv")
#Finding null values
sum(is.na(creditcard))
#Removing null values- na.omit(dataset)
creditcard <- na.omit(creditcard[,-1])


creditcard$card[creditcard$card=='yes']=1
creditcard$card[creditcard$card=='no']=0

creditcard$owner[creditcard$owner=='yes']=1
creditcard$owner[creditcard$owner=='no']=0

creditcard$selfemp[creditcard$selfemp=='yes']=1
creditcard$selfemp[creditcard$selfemp=='no']=0

str(creditcard)
creditcard$card<-as.factor(creditcard$card)
creditcard$owner<-as.numeric(creditcard$owner)
creditcard$selfemp<-as.numeric(creditcard$selfemp)
str(creditcard)
# Logistic Regression
#glm(y~x,family="bin....)
logit<-glm(card~.,data=creditcard,family= "binomial")
summary(logit)


# Confusion Matrix Table
#predict(modelobject,testdataset)
prob=predict(logit,type=c("response"),creditcard)
prob

#table(dataframe1,dataframe2) ..to create 2X2 matrix
confusion<-table(prob>0.5,creditcard$card)
confusion

# Model Accuracy
#adding diagonal elements in the confusion matrix
Accuracy<-sum(diag(confusion))/sum(confusion)
Accuracy

##############

######################







## ROC Curve

#Extract from the fitted model object the vector of fitted probabilities:
install.packages("ROCR")
install.packages("pROC")
library(ROCR)
library(pROC)
#prediction(probability values from model,y variable)
rocrpred<-prediction(prob,creditcard$card)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
#plot(rocrperf)
auc <- auc(creditcard$card ~ prob)
auc

#################

