train = read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)


#dodanie nowych atrybutow
for(i in 1:dim(train)[1])
{
  train[i, 'wday'] = as.POSIXlt(train[i, 1])$wday
  
  if(train[i, 'workingday'] && (as.POSIXlt(train[i, 1])$h == 9 || as.POSIXlt(train[i, 1])$h == 10))#jesli dzien roboczy i 9 lub 10 - dojazd do pracy
  {
    train[i, 'onwaytowork'] <- 1
  }
  else
    train[i, 'onwaytowork'] <- 0
}

#regsubset
install.packages('leaps')
library(leaps)
out = NULL
out <- regsubsets(count ~ season + holiday + workingday, data = train)
summary(out)
plot(out)


#stepwise regression i przy okazji regresja liniowa (metoda lm)
library(MASS)
head(train)
model <- lm(count~ season + holiday + workingday, family = binomial, data = train)
summary(model)
step <- stepAIC(model, trace = FALSE)
step$anova

forward <- stepAIC(model, direction = 'forward', trace = FALSE)
forward$anova
backward <- stepAIC(model, direction = 'backward', trace = FALSE)
backward$anova

#local regression - wywala sie dla niektorych parametrow - np. holiday: NA/NaN/Inf in foreign function call (arg 2) 

lmodel <- loess(count ~ temp + workingday, train)

plot(train[, 'temp'], train[, 'count'])
lines(predict(lmodel), col = 'red', lwd = 2)

#robust regression - metoda rlm
model <- rlm(count ~ season + holiday + workingday, family = binomial, data = train)
summary(model)

#drzewo regresji
library(rpart)
fit <- rpart(count ~ season + temp + holiday + workingday, method = "anova", data = train)
#przycinanie - parametr cp
cp <- 0.05
pfit <- prune(fit, cp)
plot(pfit, uniform = TRUE)
printcp(pfit)
