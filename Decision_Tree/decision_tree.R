#Data Load
#Install the required packages
#install.packages("caret")
#install.packages("C50")
#Library invoke
library(caret)
library(C50)
#To make the results consistent across the runs
CompanyData <- read.csv('C:/Users/JAIKUMAR/Desktop/karthikeyan sir/Assignment/Decision_Tree(17th June_20)/Assignments/Company_Data (1).csv')


dim(CompanyData)

summary(CompanyData)

#View(CompanyData)

str(CompanyData)

sale <- NULL
sale <- ifelse(CompanyData$Sales<=9.9,0,1)
CompanyData[,"sale"] <- sale
CompanyData$sale<-as.factor(CompanyData$sale)

str(CompanyData)

CompanyData$ShelveLoc[CompanyData$ShelveLoc=='Bad']=0
CompanyData$ShelveLoc[CompanyData$ShelveLoc=='Medium']=1
CompanyData$ShelveLoc[CompanyData$ShelveLoc=='Good']=2

CompanyData$ShelveLoc<-as.numeric(CompanyData$ShelveLoc)

CompanyData$Urban[CompanyData$Urban=='No']=0
CompanyData$Urban[CompanyData$Urban=='Yes']=1
CompanyData$Urban<-as.numeric(CompanyData$Urban)

CompanyData$US[CompanyData$US=='No']=0
CompanyData$US[CompanyData$US=='Yes']=1
CompanyData$US<-as.numeric(CompanyData$US)


str(CompanyData)


set.seed(10000)
#Data Partition
inTraininglocal<-createDataPartition(CompanyData$sale,p=.70,list = F)
training<-CompanyData[inTraininglocal,]
testing<-CompanyData[-inTraininglocal,]
testing$sale<-as.numeric(testing$sale)

library(rpart)
#Model Building
model<-rpart(sale~.,data = training) 
#Generate the model summary
summary(model)

#Predict for test data set
pred<-predict(model,testing[-12])


###########






