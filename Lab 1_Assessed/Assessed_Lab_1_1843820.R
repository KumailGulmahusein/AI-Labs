#Source Dataset
mydata = read.csv('C:\\Users\\Kumail\\Desktop\\AI Labs\\Lab 1_Assessed\\seeds_dataset.csv', sep=",")
realdata = read.csv('C:\\Users\\Kumail\\Desktop\\AI Labs\\Lab 1_Assessed\\seeds_real.csv', sep=",")
source("C:\\Users\\Kumail\\Desktop\\AI Labs\\Lab 1_Assessed\\WK_R.r")

#Plot data
plot(mydata)

mydata = na.omit(mydata) # deletion of missing data
mydata = scale(mydata) # standardize variables

#Variables to store values
WKValuesKmeans <- vector(mode ="numeric")
NumberOfCluster <- vector(mode ="numeric")

for (i in 2:10){
  #k-means clustering with k-means command k=i
  fit <- kmeans(mydata, i)
  
  #Store assignments
  Kgroups = fit$cluster
  
  #Store k-means values with number of cluster
  NumberOfCluster <- append(NumberOfCluster, i)
  
  #Weighted Kappa Comparison
  wk = WK_R(Kgroups, realdata$Real)
  
  #Store WK values on WKValues vector
  WKValuesKmeans <- append(WKValuesKmeans, wk)
}

#Print weighted kappa values
print(WKValuesKmeans)

#Plot weighted Kappa Graph
plot(WKValuesKmeans)

#Matrix of Euclidean distance between each data point
d <- dist(mydata, method = "euclidean")

#AVERAGE
#Average linkage Hierarchical clustering
averagefit <- hclust(d, method="average")
#Plot Average Dendrogram
plot(averagefit)
#Create clusters by cutting Dendrogram (Average)
HgroupsAverage <- cutree(averagefit, k=3)
#Draw scatterplot with the assigned clusters (Average)
plot(mydata, col=HgroupsAverage)
#Find weighted kappa values Average method
wkaverage = WK_R(realdata$Real, HgroupsAverage)

#SINGLE
#Single linkage Hierarchical clustering
singlefit <- hclust(d, method="single")
#Plot Single Dendrogram
plot(singlefit)
#Create clusters by cutting Dendrogram (Single)
HgroupsSingle <- cutree(singlefit, k=3)
#Draw scatterplot with the assigned clusters (Single)
plot(mydata, col=HgroupsSingle)
#Find weighted kappa values Single method
wksingle = WK_R(realdata$Real, HgroupsSingle)

#COMPLETE
#Complete linkage Hierarchical clustering
completefit <- hclust(d, method="complete")
#Plot Complete Dendrogram
plot(completefit)
#Create clusters by cutting Dendrogram (Complete)
HgroupsComplete <- cutree(completefit, k=3)
#Draw scatterplot with the assigned clusters (Complete)
plot(mydata, col=HgroupsComplete)
#Find weighted kappa values Complete method
wkcomplete = WK_R(realdata$Real, HgroupsComplete)

#Draw scatterplot of real data
plot(mydata, col=realdata$Real)

#Concatenate Hc weighted kappa
HierarchicalWK=c(wkaverage, wksingle, wkcomplete)
#Plot Hierarchical WK values
plot(HierarchicalWK)
