library(mlbench)
library(nnet)
set.seed(0)
## training one-layer neural network, please remember to multiply its predictions by range of trainset
trainNN = function(trainset, neurons){
  model <- nnet(count/range(count)[2] ~ ., data=trainset, size=neurons, maxit=100, MaxNWts=3000) 
  return(model)
}

