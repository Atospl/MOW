source('Regression.R')
library('Metrics')

## Function for performing cross-validation
crossValidate = function(trainset, k){
  set.seed(1)
  folds=sample(rep(1:k, length=nrow(trainset)))

  # build error frame
  error.iterations = c(1:k)
  error.models = c("lin", "log", "rob", "tree")
  error.types = c("rmse", "rmsle")
  #error <- array(0, dim = c(length(error.iterations), length(error.models), length(error.types)))
  #error <- array(error.models, error.types, error.iterations)
  error <- list('linRMSLE', 'locRMSLE', 'robRMSLE', 'treeRMSLE', 'linRMSE', 'locRMSE', 'robRMSE', 'treeRMSE')
  
  for (i in 1:k){
  # train models
  cat("Fold",i,"  \n")
  trainIndex <- (folds!=i)
  models <- getModels(trainset[trainIndex,])
  
  # get predictions
  testIndex <- -trainIndex
  predictions <- getPredictions(trainset[testIndex,], models)
  errors <- getErrors(trainset[testIndex,],predictions)

  if(length(error$linRMSLE))
  {
    error$linRMSLE <- c(error$linRMSLE, errors$lin$rmsle)
    error$locRMSLE <- c(error$locRMSLE, errors$loc$rmsle)
    error$robRMSLE <- c(error$robRMSLE, errors$rob$rmsle)
    error$treeRMSLE <- c(error$treeRMSLE, errors$tree$rmsle)
    error$linRMSE <- c(error$linRMSE, errors$lin$rmse)
    error$locRMSE <- c(error$locRMSE, errors$loc$rmse)
    error$robRMSE <- c(error$robRMSE, errors$rob$rmse)
    error$treeRMSE <- c(error$treeRMSE, errors$tree$rmse)
  }
  else
  {
    error$linRMSLE <- errors$lin$rmsle
    error$locRMSLE <- errors$loc$rmsle
    error$robRMSLE <- errors$rob$rmsle
    error$treeRMSLE <- errors$tree$rmsle
    error$linRMSE <- errors$lin$rmse
    error$locRMSE <- errors$loc$rmse
    error$robRMSE <- errors$rob$rmse
    error$treeRMSE <- errors$tree$rmse
  }  
  }
  return(error)
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

meanPredictions = function(predicions)
{
  meanPredictions <- c()
  
  for(i in 1:length(predictions[[1]]))
  {
    pred <- c()
    for(j in 1:length(predictions))
    {
      pred <- c(pred, predictions[[j]][i])
    }
    meanPredictions <- c(meanPredictions, mean(pred))
  }
  return(meanPredictions)
}

predictTestset = function (trainset, testset)
{
  preprocessedTrainset <- prepareDataset(trainset)
  models <- getModels(preprocessedTrainset)
  preprocessedTestset <- prepareDataset(testset, FALSE)
  predictions <- getPredictions(preprocessedTestset, models)
  
  meanPred <- meanPredictions(predictions)
  
  data <- matrix(c(testset$datetime, meanPred), nrow = length(meanPred), ncol = 2)
  
  return (data)
}