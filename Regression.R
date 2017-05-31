library(MASS)
library(Metrics)
library(rpart)

linearReg = function(trainset, fam = binomial){
  model <- lm(count ~ ., family = fam, data = trainset)
  return(model)
}


#local regression - wywala sie dla niektorych parametrow - np. holiday: NA/NaN/Inf in foreign function call (arg 2) 
#localRegression(count ~ humidity + hours + atemp, pTrain) - działa
localReg = function(formula, trainset, sp, deg)
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
regTree = function (trainset, meth = "anova")
{
  model <- rpart(count ~ ., method = meth, data = trainset)
  
  return (model)
}

fit <- rpart(count ~ season + temp + holiday + workingday, method = "anova", data = train)
#przycinanie - parametr cp
cp <- 0.05
pfit <- prune(fit, cp)
plot(pfit, uniform = TRUE)
printcp(pfit)