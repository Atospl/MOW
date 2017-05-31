source('Regression.R')

## Function for performing cross-validation
crossValidate = function(trainset, k){
  set.seed(1)
  folds=sample(rep(1:k, length=nrow(trainProcessed)))

  # build error frame
  error.iterations = c(1:k)
  error.models = c("lin", "log", "rob", "tree")
  error.types = c("rmse", "rmsle")
  error <- data.frame(error.models, error.types, error.iterations)
  
  # train models
  cat("Fold",k,"  \n")
  trainIndex <- (folds!=k)
  models <- getModels(trainset[trainIndex,])
  
  # get predictions
  testIndex <- -trainIndex
  predictions <- getPredictions(trainset[testIndex,], models)
  errors <- getErrors(trainset[testIndex,],predictions)
  
  return(errors)
}

## Combines all kinds of regression models for given trainset
getModels = function(trainset){
  
  linearModel <- linearReg(trainset)
  localModel <- localReg(count ~ humidity + hours + atemp, trainset)
  robustModel <- robustReg(trainset)
  treeModel <- regTree(trainset)
  
  modelsList <- list("lin" = linearModel, "loc" = localModel, "rob" = robustModel, "tree" = treeModel)
  return(modelsList)
}

getPredictions = function(testset, models){
  linPredict <- predict(models$lin, newdata=testset)
  locPredict <- predict(models$loc, newdata=testset)
  robPredict <- predict(models$rob, newdata=testset)
  treePredict <- predict(models$tree, newdata=testset)
  
  predictions.linear <- ifelse(linPredict < 0, 0, linPredict)
  predictions.local <- ifelse(locPredict < 0, 0, locPredict)
  predictions.robust <- ifelse(robPredict < 0, 0, robPredict)
  predictions.tree <- ifelse(treePredict < 0, 0, treePredict)
  
  predictionsList <- list("lin"=predictions.linear, "loc"=predictions.local, "rob"=predictions.robust,
                          "tree"=predictions.tree)
  return(predictionsList)
}

getErrors = function(testset, predictions){
  errors.linRMSLE <- rmsle(testset$count, predictions$lin)
  errors.locRMSLE <- rmsle(testset$count, predictions$loc)
  errors.robRMSLE <- rmsle(testset$count, predictions$rob)
  errors.treeRMSLE <- rmsle(testset$count, predictions$tree)
  
  errors.linRMSE <- rmse(testset$count, predictions$lin) 
  errors.locRMSE <- rmse(testset$count, predictions$loc)
  errors.robRMSE <- rmse(testset$count, predictions$rob)
  errors.treeRMSE <- rmse(testset$count, predictions$tree)    

  errorsList <- list("lin"=list("rmsle"=errors.linRMSLE,
                                "rmse"=errors.linRMSE),
                     "loc"=list("rmsle"=errors.locRMSLE,
                                "rmse"=errors.linRMSE),
                     "rob"=list("rmsle"=errors.robRMSLE,
                                "rmse"=errors.robRMSE),
                     "tree"=list("rmsle"=errors.treeRMSLE,
                                 "rmse"=errors.treeRMSE))
  return(errorsList)
}