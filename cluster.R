pa<-c("ggplot2","dplyr","car","gvlma","tidyverse","magrittr")
lapply(pa,require,character.only=TRUE)
#Cluster is unsupervised learning method which means that there is no outcome to be predicted and it just tries to find patterns in the data. 
#To practice clustering, I use iris data which shows the sepal length, sepal width, petal length, and petal width of different species flowers. 
#Let's take a look at the data first. 
head(iris)
iris%>%ggplot(aes(x=Sepal.Length,y=Sepal.Width,color=Species))+geom_point()
iris%>%ggplot(aes(x=Petal.Length,y=Petal.Width,color=Species))+geom_point()
#From the scatter plot, you may find out the petal length and petal width differ due to the species but you don't see the pattern from the scatter plot between the sepal length and sepal width. 
#To cluster the observation into groups, we have to consider all the varibles, not just the petal length and width. As a result, we will use cluster here. 
#As I mention before, there is no outcome to be predicted and the species is just for validation, so I will eliminate the last colume, species.  
df<-iris[-5]
#Clustering is a task of grouping a set of objects based on their similarity, and that's what it looks like when we put the data into groups. 
distE <- dist(df, method="euclidean") 
Ecluster <- hclust(distE)
plot(Ecluster, xlab="euclidean distance")
#In here, I use the Hierarchical Clustering with euclidean distance and you also try the manhattan distance if you want. 
#I will draw a line here to show how it looks like when we divide the group into three. 
abline(h=30, col="red")

#I am going you how to cluster it with one of the popular method, kmeans.
#kmeans will ramdomly put the points as a mean of the cluster and reassign data points to the cluster whose centroid is closest. It will adjust the centroid when a new point join the cluter.  
set.seed(124)
kmeans.cluster2<- kmeans(df, centers=2,nstart=25)
kmeans.cluster2
# I want a high intra-cluster similarity and low inter-cluster similarity. Distance is the difference between the point, so we are looking for shorter distance for the points in the same group(within) and longer distance for points in different groups. 
# Since the starting assignments are random, by setting n=25, R will run 25 different random starting assignments and select the one with the lowest within. 
# You can see from the "Within cluster sum of squares by cluster", the total_ss is the sum of squared distances of each data point to the global sample mean, which is the average of all sample mean. Between_ss is the sum of squared distances of these three(group) means to the global mean. We want the high between_ss and high percentage.
# Let's try to divide it into 3 and 4 groups.
set.seed(124)
kmeans.cluster3 <- kmeans(df, centers=3,nstart=25)
kmeans.cluster3
kmeans.cluster4 <- kmeans(df, centers=4,nstart=25)
kmeans.cluster4
# As you can see, the percentage will get higher when I divide it into more groups because each group will be identical. However, the change of the percentage will getting smaller and samller if I increase the number of group. Here comes the quesiton, how many group should I choose? We can use the folling method to find the optimum number of cluster.
# To solve this problem, I will use a plot to show you.  
ratio <- rep(NA, times = 10)
for (k in 2:length(ratio)) {
  kmeans_fit <- kmeans(df, centers = k, nstart = 20)
  ratio[k] <- kmeans_fit$tot.withinss / kmeans_fit$betweenss
}
plot(ratio, type="b", xlab="k")
#As you can see, the ratio, which is the within cluster sum of squares by cluster will increase when I choose more k value, however,the slope of the line is geeting flatter and flatter when the k value increase. 
#As I mention before, we are looking for a well fit model with less complexity. 
#From the plot, I will choose to cluster the data into 3 groups becasue after 3, the line is getting flat so the change of the raio won't be as much as before. 
#Now, we can compare our cluster with the species to check the accuracy. 
table(kmeans.cluster3$cluster,iris$Species)
#As you can see setosa is grouped into cluster3, versicolor is grouped into cluster 1 with wrongly classified 2 data points into cluster 2, and lastly,virginica is grouped into cluster 2 and wrongly placed 14 point into cluster 1.
#For virginica, we can calculate the accuracny of the cluter by following function.
accurancy_virginica<-36/(14+36)*100
#Even though some points is not place in the cluster we predict, a 72% accurancy is a well prediction. 