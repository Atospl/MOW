#library(leaps)
library(caret)
library(mlbench)
library(doMC)
library(randomForest)

# register cores for multithreading
registerDoMC(cores = 6)
## to ensure results are repeatable
set.seed(7)

## Correlation matrix analysis
correlationAnalysis = function(train){
  corMatrix <- cor(train[,c("season", "holiday", "workingday", "weather", "temp", "atemp", "humidity",
                            "windspeed", "weekdays", "hours", "onwaytowork")])
  print(corMatrix)
  highlyCorrelated <- findCorrelation(corMatrix, cutoff=0.9, names=TRUE)
  print(highlyCorrelated)
  return(corMatrix)
  ## turns out only ATemp and Temp are highly correlated
}


## Regsubset
regsubsetAnalysis = function(train){
  out <- regsubsets(count ~ season + holiday + workingday, data=train)
  summary(out)
  plot(out)
}

## LVQ analysis
LVQImportanceAnalysis = function(train){
  train$count <- as.factor(train$count)
  control <- trainControl(method="repeatedcv")#, number=14, repeats=30)
  model <- train(count~., data=train, method="lvq")
  importance <- varImp(model, scale=FALSE)
  print(importance)
  plot(importance)
}

## Random Forest analysis
## TODO
randomForestAnalysis = function(train){
  control <- rfeControl(functions=rfFuncs, method="cv", number=10)
  results <- rfe(train[,c("season", "holiday", "workingday", "weather", "temp", "atemp", "humidity",
                          "windspeed", "weekdays", "hours", "onwaytowork")], 
                 train$count, 
                 c(1:11), 
                 rfeControl=control)
  return(results)
}

