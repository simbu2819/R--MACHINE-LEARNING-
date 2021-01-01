data<-read.csv('C:/Users/JAIKUMAR/Desktop/karthikeyan sir/Assignment/Multi-linear Regression(10th June_20)/Assignments/50_Startups (3).csv')

head(data)

summary(data)

dim(data)

str(data)

data$State[data$State=='New York']=0

data$State[data$State=='Florida']=1

data$State[data$State=='California']=2

data$State<-as.numeric(data$State)

str(data)

cor(data)

data<-na.omit(data)

karthikeyan<-lm(Profit~R.D.Spend+Administration+Marketing.Spend+State,data=data)

summary(karthikeyan)

confint(karthikeyan)

#Model accuracy assessment
#Residual Standard Error (RSE), or sigma

sigma(karthikeyan)/mean(data$Profit)
