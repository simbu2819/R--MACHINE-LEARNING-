data<-read.csv('C:/Users/JAIKUMAR/Desktop/karthikeyan sir/Assignment/Simple_linear(8th june_20)/Assignments/Salary_Data.csv')

dim(data)
str(data)
cor(data)
data<-na.omit(data)

karthikeyan_sir<-lm(Salary~YearsExperience,data=data)
summary(karthikeyan_sir)
#Model accuracy assessment
#Residual Standard Error (RSE), or sigma

sigma(karthikeyan_sir)/mean(data$Salary)
