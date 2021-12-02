#- Hierarchichal Cluster Analysis :
#Libraries Required - 
library(tidyverse) #data manipulation
library(cluster) #clustering algo
library(factoextra) #clustering vizuals
library(dendextend) #for comparing two dendrograms

#Retrieving the data - utilities dataset-
utilities <- read.csv(file.choose(), header =T)
view(utilities)
str(utilities)

#Data Preparation - 
#1 . Skipping cetegorical variable - 
utilities1 <- utilities[-1] #skipping the categorical variable for better analysis.
str(utilities1)

#2. Data standardisation (scaled) to make variables comparable.
utilities_scaled <- scale(utilities1)
view(utilities_scaled)
head(utilities_scaled)

#Agglomerative Hierarchical Clustering -

#1 . Calculating distance : Euclidean -
distance <- dist(utilities_scaled, method = "euclidean") 
print(distance,digits = 3) #Distance matrix
#gives euclidean distance - how close and how far the variables are.


#2. HC Using Complete Linkage - 
hca <- hclust(distance, method = "complete", members = NULL) # complete linkage method.
hca
#Dendrogram -  
plot(hca, cex =0.6, labels = utilities$Company)
plot(hca, cex = 0.6)
#can see 10 and 13 close
#brining them on same line - 
plot(hca, cex =0.6, labels = utilities$Company, hang = -1)
plot(hca, cex=0.6,hang=-1)

#Running HC in R 
#creating groups -
groups <- cutree(hca, k = 4)
table(groups)
utilities$Cluster <- groups #inserting new col to the original dataset
head(utilities)
View(utilities)
#taking avg of all variables to see how cluster differ -
aggregate(utilities_scaled, list(utilities$Cluster), mean)
#list the mean within clusters

#checking how good the clusters are using silhoutte plot -
plot(silhouette(cutree(hca, k =4),distance))
#we have 4 clusters so 4 silhouttes. #have an outline 

#how we decide k ? 
plot(hca, cex = 0.6, hang = -1)
rect.hclust(hca, k=4, border=2:5)
#cluster visual -
fviz_cluster(list(data = utilities_scaled, cluster = groups))
#tell whether there is overlap or not.
#there is overlap

#Elbow method - to figure out how many k are there -
fviz_nbclust( utilities_scaled, FUN = hcut, method = "wss")
#wss = within sum of squares
#4th onwards the distance is constant and hence we take 3 .
#So  k =3 is optimal. 

#redoing with k = 3 -
#creating groups -

groups1 <- cutree(hca, k = 3)
table(groups1)
utilities$Cluster <- groups1 #inserting new col to the original dataset
head(utilities)
View(utilities)
plot(silhouette(cutree(hca, k =3),distance))
plot(hca, cex = 0.6, hang = -1)
rect.hclust(hca, k=3, border=2:5)
fviz_cluster(list(data = utilities_scaled, cluster = groups1))
#no more overlap present. 




