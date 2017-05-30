## File contains commands useful in the process of data mining of trainset

## load the dataset
train = read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)   

## get working days
workingTrain <- train[which(train$workingday == 1)]

## get off work days
workingTrain <- train[which(train$workingday == 1)]

## plot for count vs hour of day
plot2 <- ggplot(workingTrain, aes(x = hour(datetime), y = count), xaxp=c(0:23, 1)) + geom_point() + labs(x = "Hour of day", y="Count") + geom_smooth() + scale_x_continuous(breaks=c(0:23))

