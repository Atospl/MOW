library(lubridate)

## Adds attribute of day (MON/TUE/WED/THU/FRI/SAT/SUN)
## to the dataset basing on the date
addDayAttr=function(trainset) {
    weekdays <- weekdays(as.Date(trainset$datetime))
    days <- factor(weekdays, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                      "Saturday", "Sunday"), ordered=TRUE)
    trainset$weekdays <- as.integer(days)
    
    return(trainset)
}

## Adds attribute containing hour of the day
addHourAttr = function(trainset) {
    trainset$hours <- hour(train$datetime)
    return(trainset)
}

## Adds attribute 'onwaytowork' - 1 if people may travel from or to work, 0 else
addWorkWayAttribute = function(trainset) {
    for(i in 1:dim(trainset)[1]) {
        if(trainset[i, 'workingday'] && 
            (
              (as.POSIXlt(trainset[i, 1])$h <= 9 && as.POSIXlt(trainset[i, 1])$h >= 7) || 
              (as.POSIXlt(trainset[i, 1])$h <= 9 && as.POSIXlt(trainset[i, 1])$h >= 7)
            )
          )
        {
            trainset[i, 'onwaytowork'] <- 1
        }
        else
        {
            trainset[i, 'onwaytowork'] <- 0
        }
    }
    return(trainset)
}

## Removes datapoints which lay 3 stddevs from mean of count attr
## approx 1% of the trainset
removeOutliers = function(trainset) {
    cleanedSet <- trainset[which(abs(trainset$count - mean(trainset$count)) < 3*sd(train$count)),]
    return(cleanedSet)
}

## Invokes all functions connected with attributes preparation
prepareDataset = function(trainset) {
  trainset <- addDayAttr(trainset)
  trainset <- addHourAttr(trainset)
  trainset <- addWorkWayAttribute(trainset)
  trainset <- removeOutliers(trainset)
  return(trainset)
}
