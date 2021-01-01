
library(caret)

require(caTools)  # loading caTools library

data<-read.csv('C:/Users/JAIKUMAR/Desktop/karthikeyan sir/Latest Assignment/KNN/Assignment/glass (2).csv')
     
str(data)

data$Type

summary(data)

data<-na.omit(data)

data$type<-as.factor(data$Type)

str(data)


library(caret)
#install.packages('pROC')
library(pROC)
library(mlbench)
library(class)
library(lattice)
#library(gmodels) 


summary(data[c("RI","Na","Mg")])

#Create a function to normalize the data
norm <- function(k){ 
  return((k-min(k))/(max(k)-min(k)))
}
#test normalization
norm(c(1,2,3,4,5))


data_n<- as.data.frame(lapply(data[1:9], norm))
#View(glass_n)
summary(data_n[c("RI","Na","Mg")])



#create training and test datasets
set.seed(123)
ind <- sample(2, nrow(glass_n), replace = TRUE, prob = c(0.7,0.3))
glass_train <- data_n[ind==1,]
glass_test <-  data_n[ind==2,]


#Get labels for training and test datasets
set.seed(123)
ind1 <- sample(2, nrow(data), replace = TRUE, prob = c(0.7,0.3))
glass_train_labels <- data[ind1==1,10]
glass_test_labels <-  data[ind1==2,10]


glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=3)
table(glass_test_pred,glass_test_labels)


mean(glass_test_pred==glass_test_labels) 

install.packages('gmodels')
library(gmodels)

CrossTable(x=glass_test_labels,y=glass_test_pred,prop.chisq = FALSE) 
