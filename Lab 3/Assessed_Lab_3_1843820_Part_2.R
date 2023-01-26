#Install required packages
install.packages ("neuralnet")

#Import required libraries
library(neuralnet)

#Read data
mydata = read.csv('C:\\Users\\Kumail\\Desktop\\AI Labs\\Lab 3\\winedata2.csv',sep = ",")

#Build a training set of the first half values and test set the remaining half values
#set up a training set
wineclassTrain = mydata[1:65,1]
winevaluesTrain = mydata[1:65,4:5]
#and test set
wineclassTest = mydata[66:130,1]
winevaluesTest = mydata[66:130,4:5]

#Turn class values from (1,2) to (0,1)
wineclassTrain = replace(wineclassTrain,wineclassTrain == 1,0)
wineclassTrain = replace(wineclassTrain,wineclassTrain >= 2,1)

wineclassTest = replace(wineclassTest,wineclassTest == 1,0)
wineclassTest = replace(wineclassTest,wineclassTest >= 2,1)

#Normalize outputs
scaledTrain <- as.data.frame(scale(winevaluesTrain))
scaledTest <- as.data.frame(scale(winevaluesTest))

#Set train input
trainin = scaledtrain
#Set train output
trainout = wineclassTrain
#Combine train data
winedat = cbind(trainout,trainin)

#train a neural network on the wine data
set.seed(3)
NN = neuralnet(winedat[,1]~., winedat[,-1], hidden = c(3,3) , threshold = 0.001, stepmax = 1e+05, linear.output = FALSE)

#Plot data
plot(NN)

#Predict with test set
predict_testNN = compute(NN, scaledtest)
predict_testNN$neurons
predict_testNN$net.result
predict_out = as.numeric(predict_testNN$net.result>0.5)
predict_out

#Compare to actual test class values to get accuracy
n = length(wineclassTest) #the number of test cases
ncorrect = sum(predict_out==wineclassTest) #the number of correctly predicted
accuracy=ncorrect/n
print(accuracy)

