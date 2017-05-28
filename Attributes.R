library(lubridate)

## Adds attribute of day (MON/TUE/WED/THU/FRI/SAT/SUN)
## to the dataset basing on the date
addDayAttr=function(trainset) {
    weekdays <- weekdays(as.Date(trainset$datetime)
    days <- factor(weekdays, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                      "Saturday", "Sunday"), ordered=TRUE)
    trainset$weekdays <- as.integer(days)
    
    return trainset
}

addHourAttr = function(trainset) {
    trainset$hours <- hour(train$datetime)
    return trainset
}

addWorkWayAttribute = function(trainset) {
    for(i in 1:dim(trainset)[1]) {
        if(trainset[i, 'workingday'] && 
            ((as.POSIXlt(trainset[i, 1])$h <= 9 && as.POSIXlt(trainset[i, 1])$h >= 7) || (as.POSIXlt(trainset[i, 1])$h <= 9 && as.POSIXlt(trainset[i, 1])$h >= 7))
        {
            trainset[i, 'onwaytowork'] <- 1
        }
        else
            trainset[i, 'onwaytowork'] <- 0
        }

    return trainset
}
