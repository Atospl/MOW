library(MASS)
library(Metrics)

linearRegressionModel = function(trainset){
  model <- lm(count ~ ., family = binomial, data = trainset)
  predictions <- ifelse(predict(model) < 0, 0, predict(model))
  rmsle(trainProcessed$count, predictions)
  return(rmsle)
}