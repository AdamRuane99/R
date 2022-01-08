## Neural Networks ##

library(MASS)
head(Boston)
library(neuralnet)

str(Boston)


data <- Boston

##Normalise the data ##

maxs <- apply(data, 2, max) ##MARGIN 2 over columns ##
mins <- apply(data,2,min)

scaled.data <- scale(data, center = mins, scale = maxs-mins) ## Centre min value , divide by max - min ## Normalise values ## Between 0 and 1 ##

scaled <- as.data.frame(scaled.data)
head(scaled)


library(caTools)

sample <- sample.split(scaled$medv, SplitRatio = 0.7)
train <- subset(scaled, sample == T)
test <- subset(scaled, sample == F)

## Neural package requires manual selection ##
##Trick ##

n <- names(train)

f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))



nn <- neuralnet(f, data = train, hidden = c(5,3), linear.output = T)

plot(nn) ## NN Model ##

## --- Predictions ---- ##

predicted.nn.vals <- compute(nn, test[1:13]) ## without predict label col ##
str(predicted.nn.vals)

true.predictions <- predicted.nn.vals$net.result*(max(data$medv)-min(data$medv))+min(data$medv) ## Initial subtraction step ##

## Unscale it ##


# Convert the test data
test.r <- (test$medv)*(max(data$medv)-min(data$medv))+min(data$medv)


MSE.nn <- sum((test.r - true.predictions)^2)/nrow(test)
MSE.nn


error.df <- data.frame(test.r, true.predictions)



library(ggplot2)


ggplot(error.df, aes(x = test.r, y = true.predictions)) + geom_point() + stat_smooth()
