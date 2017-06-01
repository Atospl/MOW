library(mxnet)
library(mlbench)

mx.set.seed(0)

## training one-layer neural network, please remember to multiply its predictions by range of trainset
trainNN = function(trainset, neurons){
  model <- nnet(count/range(count)[2] ~ ., data=trainset, size=neurons, maxit=300, MaxNWts=2000) 
  return(model)
}

