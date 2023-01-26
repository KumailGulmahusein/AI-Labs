#Source Data
mydata = read.csv('C:\\Users\\Kumail\\Desktop\\AI Labs\\Lab 1_Assessed\\spaeth_01.csv', sep=",")

#Plot Data
plot(mydata)

#Delete missing data and standardize variables
mydata = na.omit(mydata) # deletion of missing data
mydata = scale(mydata) # standardize variables 

#Matrix of Euclidean distances between each datapoint
d <- dist(mydata, method = "euclidean")

#Average linkage Hierarchical clustering
fit <- hclust(d, method="average")

#Display Dendrogram
plot(fit)

#Create clusters by cutting Dendrogram
Hgroups <- cutree(fit, k=5)

#Draw dendrogram with red borders around these 5 clusters
rect.hclust(fit, k=5, border="red") 

#Draw scatterplot with the assigned clusters as colours
plot(mydata, col=Hgroups)

#k-means clustering with k-means command k=5
fit <- kmeans(mydata, 5) # 5 cluster solution

#Get different statistics on the clusters. e.g mean
aggregate(mydata,by=list(fit$cluster),FUN=mean)

#Store assignments
Kgroups = fit$cluster 

#Draw scatterplot on k-means
plot(mydata, col=Kgroups)

#Source weighted kappa function
source("C:\\Users\\Kumail\\Desktop\\AI Labs\\Lab 1_Assessed\\WK_R.r")

#Calculate Wk between hierarchical and k-means
wk = WK_R(Kgroups, Hgroups)
