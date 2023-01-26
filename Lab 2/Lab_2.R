#Pull data into Rstudio
winedata = read.csv('C:\\Users\\Kumail\\Desktop\\AI Labs\\Lab 2\\winedata.csv', sep=",")

#Separate class and values into separate variables
wineclass = winedata[,1]
winevalues = winedata[,-1]

#Build a training set of the first 100 values and test set the remaining values (101-178)
#set up a training set
wineclassTrain = wineclass[1:100]
winevaluesTrain = winevalues[1:100,]
#and testset
wineclassTest = wineclass[100:178]
winevaluesTest = winevalues[100:178,]

#Build decision tree with command rpart
install.packages("rpart")
library(rpart)
fit <- rpart(wineclassTrain~., method="class", data=winevaluesTrain)

#Plot decision tree
plot(fit, uniform=TRUE, main="Decision Tree for WineData3")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#Test classifier on test set by calculating the predictions for each test case in our test set
treepred <-predict(fit, winevaluesTest, type = 'class')

#Compare to actual test class values to get accuracy
n = length(wineclassTest) #the number of test cases
ncorrect = sum(treepred==wineclassTest) #the number of correctly predicted
accuracy=ncorrect/n
print(accuracy)

#View results as confusion matrix
table_mat = table(wineclassTest, treepred)
print(table_mat)

#Effect of pruning
pfit<- prune(fit, cp=0.1)
plot(pfit, uniform=TRUE, main="Pruned Decision Tree for WineData3")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

#Install class library
library(class)

#Generate our predicted classes
knn3pred = knn(winevaluesTrain, winevaluesTest, wineclassTrain, k=3)

#Calculate accuracy as before
n = length(wineclassTest) #the number of test cases
ncorrect = sum(knn3pred==wineclassTest) #the number of correctly predicted
accuracy=ncorrect/n
print(accuracy)
