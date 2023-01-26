#Pull data into Rstudio
seeddata = read.csv('C:\\Users\\Kumail\\Desktop\\AI Labs\\Lab 2\\seeds_dataset_class.csv', sep=",")

#Randomize data
seeds_rand=seeddata[sample(209,209),]

seeddata = na.omit(seeddata) # deletion of missing data
#seeddata = scale(seeddata) # standardize variables

#Separate class and values into separate variables
seedclass = seeds_rand[,1]
seedvalues = seeds_rand[,-1]

#Build a training set of the first 125 values and test set the remaining values (125-209)
#set up a training set
seedclassTrain = seedclass[1:125]
seedvaluesTrain = seedvalues[1:125,]
#and test set
seedclassTest = seedclass[125:209]
seedvaluesTest = seedvalues[125:209,]

#Build decision tree with command r part
install.packages("rpart")
library(rpart)
fit <- rpart(seedclassTrain~., method="class", data=seedvaluesTrain)

#Plot decision tree
plot(fit, uniform=TRUE, main="Decision Tree for Seed Data")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#Test classifier on test set by calculating the predictions for each test case in our test set
treepred <-predict(fit, seedvaluesTest, type = 'class')

#Compare to actual test class values to get accuracy
n = length(seedclassTest) #the number of test cases
ncorrect = sum(treepred==seedclassTest) #the number of correctly predicted
accuracy=ncorrect/n
print(accuracy)

pAccuracy <- vector(mode ="numeric")
pFit <- vector(mode ="numeric")

for(i in 1:3){
  #Prune tree to make data simpler
  pfit<- prune(fit, cp=i/10)
  plot(pfit, uniform=TRUE, main="Pruned Decision Tree for seed Data")
  text(pfit, use.n=TRUE, all=TRUE, cex=.8)
  
  #Test classifier on test set by calculating the predictions for each test case in our test set
  ptreepred <-predict(pfit, seedvaluesTest, type = 'class')
  
  #Compare to actual test class values to get accuracy
  n = length(seedclassTest) #the number of test cases
  ncorrect = sum(treepred==seedclassTest) #the number of correctly predicted
  accuracy=ncorrect/n
  
  #Add prune accuracy and prune values to list
  pAccuracy <- append(pAccuracy, accuracy)
  pFit <- append(pFit, pfit)
}

#Draw scatter plot of Groove length and area with color
plot(seedvaluesTest$GrooveLength, seedvaluesTest$Area, pch=16, col = ptreepred)

#Install class library
library(class)

#Assign desired k values to 'a'
a <- c(3,5,7)

KaccuracyValues <- vector(mode ="numeric")

for (i in a){
  #Generate our predicted classes
  knnpred = knn(seedvaluesTrain, seedvaluesTest, seedclassTrain, k=i)

  #Calculate accuracy as before
  n = length(seedclassTest) #the number of test cases
  ncorrect = sum(knnpred==seedclassTest) #the number of correctly predicted
  Kaccuracy=ncorrect/n

  #Store Kaccuracy Values
  KaccuracyValues <- append(KaccuracyValues, Kaccuracy)
  
}
#Compare KNN accuracy and prune accuracy
print(KaccuracyValues)
print(pAccuracy)
