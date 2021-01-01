#install.packages('dummies')
library("dummies")
library("dendextend")
library("cluster")
library(NbClust)

data<-read.csv('C:/Users/JAIKUMAR/Desktop/karthikeyan sir/Assignment/Clustering(11th june_20)/Assignments/EastWestAirlines.csv')
str(data)
mydata  <- data[,2:11]

#Creating Dummy variables for categorical data
mydata1<- dummy.data.frame(data, names = "cc1_miles", omit.constants=FALSE )
mydata1<- dummy.data.frame(data, names = "cc2_miles", omit.constants=FALSE )
mydata1<- dummy.data.frame(data, names = "cc3_miles", omit.constants=FALSE )


my_data   <- scale(mydata1)


dendogram<- dist(my_data, method = "euclidean")


#plot(dendogram)
# Hierarchical clustering using Ward's method
Hierarchical<- hclust(dendogram, method = "ward.D2" )
plot(Hierarchical)

hierarchy   <- as.dendrogram(Hierarchical)

plot(hierarchy)






#kmeans


library(factoextra)
#K-Means clustering

k_means = eclust(mydata, "kmeans", k = 3, nstart = 1, graph = FALSE)
fviz_cluster(k_means, geom = "point", frame.type = "norm")

x_train<-mydata
y_train<-data[,12]

fit<-kmeans(x_train,3)
summary(fit)
fit$cluster

