library(factoextra)
library(cluster)
library(fpc)
install.packages('NbClust')
library(NbClust)
universities<-read.csv("C:/Users/JAIKUMAR/Desktop/karthikeyan sir/Assignment/PCA(15th june_20)/Assignment/wine (1).csv")


str(universities)
#PCA Summary
karthikeyan_sir_pca <- princomp(universities[,-1], cor = TRUE, scores = TRUE, covmat = NULL)
summary(karthikeyan_sir_pca)

plot(karthikeyan_sir_pca)

biplot(karthikeyan_sir_pca)

#Clustering for data
number_of_Clusters = NbClust(universities, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all")

fviz_nbclust(number_of_Clusters) + theme_minimal()


#Hierarchical clustering

hierarchical_clust = eclust(universities, "hclust", k = 7, method = "complete", graph = FALSE) 
fviz_dend(hierarchical_clust, rect = TRUE, show_labels = FALSE) 


#K-Means clustering

k_means = eclust(universities, "kmeans", k = 5, nstart = 1, graph = FALSE)
fviz_cluster(k_means, geom = "point", frame.type = "norm")


