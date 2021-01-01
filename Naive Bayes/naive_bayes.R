train<-read.csv('C:/Users/JAIKUMAR/Desktop/karthikeyan sir/Latest Assignment/Naive Bayes(24th June_20)/Assignment/SalaryData_Train (2).csv')

test<-read.csv('C:/Users/JAIKUMAR/Desktop/karthikeyan sir/Latest Assignment/Naive Bayes(24th June_20)/Assignment/SalaryData_Test (1).csv')

str(train)

str(test)

summary(train)

data<-na.omit(train)

train$workclass<-as.factor(train$workclass)

str(train)

train$education<-as.factor(train$education)


train$maritalstatus<-as.factor(train$maritalstatus)


train$occupation<-as.factor(train$occupation)



train$relationship<-as.factor(train$relationship)

train$race<-as.factor(train$race)


train$sex<-as.factor(train$sex)


train$native<-as.factor(train$native)

train$Salary<-as.factor(train$Salary)
str(train)

#test data

test$workclass<-as.factor(test$workclass)

str(test)

test$education<-as.factor(test$education)


test$maritalstatus<-as.factor(test$maritalstatus)


train$occupation<-as.factor(train$occupation)



test$relationship<-as.factor(test$relationship)

test$race<-as.factor(test$race)


train$sex<-as.factor(train$sex)


test$native<-as.factor(test$native)

test$Salary<-as.factor(test$Salary)

str(test)


#install.packages('naivebayes')

library(naivebayes)
library(ggplot2)
#library(caret)

ggplot(data=train,aes(x=Salary, y =age, fill =Salary)) +geom_boxplot() +ggtitle("Box Plot")


ggplot(data=train,aes(x =sex, fill = Salary)) +geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=train,aes(x =relationship, fill =Salary)) +geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=train,aes(x =race, fill = Salary)) +geom_density(alpha = 0.9, color = 'Violet')




library(psych)

Model <- naiveBayes(train$Salary ~ ., data = train)
Model


Model_pred <- predict(Model,test)
mean(Model_pred==test$Salary)

confusionMatrix(Model_pred,test$Salary)
