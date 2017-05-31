crossValidate = function(trainset, k){
  set.seed(1)
  folds=sample(rep(1:k, length=nrow(trainProcessed)))
  numberOfVariables=ncol(myDataFrame)-1 
  errorsMatrix=matrix(NA,K,numberOfVariables)
  for(k in 1:k){
    # train models
    cat("Fold",k,"  \n")
    trainIndex = (folds!=k)
    models = getModels(trainset[trainingIndex,])
    # get predictions
    testIndex = -trainIndex
    predictions = getPredictions(trainset[testIndex,])
    errors = getErrors(predictions)
  }
}

getModels = function(trainset){
  
}

getPredictions = function(testset){
  
}

getErrors = function(predictions){
  
}