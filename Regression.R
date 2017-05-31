library(MASS)
library(Metrics)
library(rpart)

linearReg = function(trainset, fam = binomial){
  model <- lm(count ~ ., family = fam, data = trainset)
  return(model)
}


#local regression - wywala sie dla niektorych parametrow - np. holiday: NA/NaN/Inf in foreign function call (arg 2) 
#localRegression(count ~ humidity + hours + atemp, pTrain) - działa
localReg = function(formula, trainset, sp=0.75, deg=2)
{
  model <- loess(formula, trainset, degree = deg, span = sp)
  return(model)
}
#plot(train[, 'temp'], train[, 'count'])
#lines(predict(model), col = 'red', lwd = 2)


#robust regression - metoda rlm
robustReg = function(trainset, fam = binomial)
{
  model <- rlm(count ~ ., family = fam, data = trainset)
  return(model)
}

#drzewo regresji
regTree = function (trainset, cp=0.04, meth = "anova")
{
  model <- rpart(count ~ ., method = meth, data = trainset)
  model <- prune(model, cp)
  return (model)
}