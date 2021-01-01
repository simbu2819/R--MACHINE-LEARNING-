library(arules)
install.packages('arulesViz')
library(arulesViz)

data<-read.csv('C:/Users/JAIKUMAR/Desktop/karthikeyan sir/Assignment/Association rules(12th june_20)/Assignments/my_movies (2).csv')

final <- apriori(as.matrix(data[,6:15],parameter=list(support=0.2, confidence = 0.5,minlen=5)))

final
inspect(head(sort(final, by = "lift")))  
inspect(head(sort(final, by = "lift")))  


plot(final,method = "scatterplot")
plot(final, method = "grouped")
plot(final,method = "graph")
