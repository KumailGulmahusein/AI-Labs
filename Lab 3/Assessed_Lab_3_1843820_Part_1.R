#Install Packages
install.packages ("neuralnet")
library(neuralnet)

#OR gate input data
trainin = rbind(c(1,1),c(1,-1),c(-1,1),c(-1,-1));

#OR gate output data
trainout = rbind(0,1,1,0);

#Combined OR gate data
ORdat = cbind (trainout, trainin);

#Fit neural network with no hidden layers
set.seed(2)
NN = neuralnet(ORdat[,1]~., ORdat[,-1], hidden = c(3,3) , threshold = 0.001,stepmax = 1e+05, linear.output = FALSE)

#Visualise the NN
plot(NN)

#Gives random weights and biases which we can check with
#NN$weights

#Now let us use compute to see if the network responds to an input signal (1,1)
#testin= rbind(c(1,1))
#predict_testNN = compute(NN, testin)

#The activation of the output neuron is here
#predict_testNN$net.result

#To calculate the discrete class we threshold it at 0.5
#predict_out = as.numeric(predict_testNN$net.result>0.5)
#print(predict_out)

#Set up the input sequence
testin=rbind(c(1,1),c(1,-1),c(-1,1), c(-1,-1))

#Predict with various inputs
predict_testNN = compute(NN, testin)
predict_testNN$neurons
predict_testNN$net.result
predict_out = as.numeric(predict_testNN$net.result>0.5)
predict_out

