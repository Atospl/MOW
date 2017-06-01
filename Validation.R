source('Regression.R')
source('Deep.R')
library('Metrics')

## Function for performing cross-validation
crossValidate = function(trainset, k){
  set.seed(1)
  folds=sample(rep(1:k, length=nrow(trainset)))

  # build error frame
  error.iterations = c(1:k)
  error.models = c("lin", "log", "rob", "tree", "mean")
  error.types = c("rmse", "rmsle")
  #error <- array(0, dim = c(length(error.iterations), length(error.models), length(error.types)))
  #error <- array(error.models, error.types, error.iterations)
  error <- list('linRMSLE', 'locRMSLE', 'robRMSLE', 'treeRMSLE', 'meanRMSLE', 'linRMSE', 'locRMSE', 'robRMSE', 'treeRMSE', 'meanRMSE')
  
  for (i in 1:k){
    # train models
    cat("Fold",i,"  \n")
    trainIndex <- (folds!=i)
    models <- getModels(trainset[trainIndex,])
    
    # get predictions
    testIndex <- -trainIndex
    # add neural network prediction
    neuralP <- predict(models$neural, newdata=trainset[testIndex,])*range(trainset$count)[2]
    predictions <- combinePredictions(trainset[testIndex,], models, neuralP)
    
    errors <- getErrors(trainset[testIndex,],predictions)
  
    if(length(error$linRMSLE))
    {
      error$linRMSLE <- c(error$linRMSLE, errors$lin$rmsle)
      error$locRMSLE <- c(error$locRMSLE, errors$loc$rmsle)
      error$robRMSLE <- c(error$robRMSLE, errors$rob$rmsle)
      error$treeRMSLE <- c(error$treeRMSLE, errors$tree$rmsle)
      error$neuralRMSLE <- c(error$neuralRMSLE, errors$neural$rmsle)
      
      error$meanRMSLE <- c(error$meanRMSLE, errors$mean$rmsle)
      error$linRMSE <- c(error$linRMSE, errors$lin$rmse)
      error$locRMSE <- c(error$locRMSE, errors$loc$rmse)
      error$robRMSE <- c(error$robRMSE, errors$rob$rmse)
      error$treeRMSE <- c(error$treeRMSE, errors$tree$rmse)
      error$neuralRMSE <- c(error$neuralRMSE, errors$neural$rmse)
      error$meanRMSE <- c(error$meanRMSE, errors$mean$rmse)
    }
    else
    {
      error$linRMSLE <- errors$lin$rmsle
      error$locRMSLE <- errors$loc$rmsle
      error$robRMSLE <- errors$rob$rmsle
      error$neuralRMSLE <- errors$neural$rmsle
      error$treeRMSLE <- errors$tree$rmsle
      error$meanRMSLE <- errors$mean$rmsle
      error$linRMSE <- errors$lin$rmse
      error$locRMSE <- errors$loc$rmse
      error$robRMSE <- errors$rob$rmse
      error$treeRMSE <- errors$tree$rmse
      error$neuralRMSE <- errors$neural$rmse
      error$meanRMSE <- errors$mean$rmse
    }  
  }
  return(error)
}

## Combines all kinds of regression models for given trainset
getModels = function(trainset){
  
  linearModel <- linearReg(trainset)
  localModel <- localReg(trainset)
  robustModel <- robustReg(trainset)
  treeModel <- regTree(trainset)
  neuralModel <- trainNN(trainset, 40)
    
  modelsList <- list("lin" = linearModel, "loc" = localModel, 
                     "rob" = robustModel, "tree" = treeModel,
                     "neural" = neuralModel)
  return(modelsList)
}

combinePredictions = function(testset, models, neural){
  linPredict <- predict(models$lin, newdata=testset)
  locPredict <- predict(models$loc, newdata=testset)
  robPredict <- predict(models$rob, newdata=testset)
  treePredict <- predict(models$tree, newdata=testset)

  predictions.linear <- ifelse(linPredict < 0, 0, linPredict)
  predictions.local <- ifelse(locPredict < 0, 0, locPredict)
  predictions.robust <- ifelse(robPredict < 0, 0, robPredict)
  predictions.tree <- ifelse(treePredict < 0, 0, treePredict)

  predictionsList <- list("lin"=predictions.linear, "loc"=predictions.local, "rob"=predictions.robust,
                          "tree"=predictions.tree, "neural"=neural)
  return(predictionsList)
}

getErrors = function(testset, predictions){
  meanPred <- meanPredictions(predictions)
  
  errors.linRMSLE <- rmsle(testset$count, predictions$lin)
  errors.locRMSLE <- rmsle(testset$count, predictions$loc)
  errors.robRMSLE <- rmsle(testset$count, predictions$rob)
  errors.treeRMSLE <- rmsle(testset$count, predictions$tree)
  error.neuralRMSLE <- rmsle(testset$count, predictions$neural)
  errors.meanRMSLE <- rmsle(testset$count, meanPred)
  
  errors.linRMSE <- rmse(testset$count, predictions$lin) 
  errors.locRMSE <- rmse(testset$count, predictions$loc)
  errors.robRMSE <- rmse(testset$count, predictions$rob)
  errors.treeRMSE <- rmse(testset$count, predictions$tree)  
  error.neuralRMSE <- rmse(testset$count, predictions$neural)
  errors.meanRMSE <- rmse(testset$count, meanPred)
  

  errorsList <- list("lin"=list("rmsle"=errors.linRMSLE,
                                "rmse"=errors.linRMSE),
                     "loc"=list("rmsle"=errors.locRMSLE,
                                "rmse"=errors.linRMSE),
                     "rob"=list("rmsle"=errors.robRMSLE,
                                "rmse"=errors.robRMSE),
                     "tree"=list("rmsle"=errors.treeRMSLE,
                                 "rmse"=errors.treeRMSE),
                     "neural"=list("rmsle"=error.neuralRMSLE,
                                 "rmse"=error.neuralRMSE),
                     "mean"=list("rmsle"=errors.meanRMSLE,
                                 "rmse"=errors.meanRMSE)
                     )

  return(errorsList)
}

meanPredictions = function(predictions)
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

savePredictionsCSV = function (data, filename)
{
  write.table(data, filename, quote = FALSE, row.names = FALSE, col.names = c("datetime", "count"), sep = ',')
}