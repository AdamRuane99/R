#--- K Nearest Neighbours --- ##

rm(list=ls()) ## Empty env. ##

library(ISLR)
str(Caravan)

any(is.na(Caravan))

##Big Difference in variables/Cols



var(Caravan[,1])
var(Caravan[,2])

purchase <- Caravan[,86] ##Last Col ##


standardised.Caravan <- scale(Caravan[,-86])

print(var(standardised.Caravan[,1]))
print(var(standardised.Caravan[,2]))

## Standardized Columns now ##

library(caTools)
 
##sample <- sample.split(Caravan$Purchase, SplitRatio = 0.75 )

##train <- subset(Caravan, sample == TRUE)
##test <- subset(Caravan, sample == F)



# First 100 rows for test set
test.index <- 1:1000
test.data <- standardised.Caravan[test.index,]
test.purchase <- purchase[test.index]

# Rest of data for training
train.data <- standardised.Caravan[-test.index,]
train.purchase <- purchase[-test.index]


## -- KNN MODEL ---- ##


library(class)



set.seed(101)

predicted.purchase <- knn(train.data,test.data, train.purchase, k = 1)

print(head(predicted.purchase))

missclass.error <- mean(test.purchase != predicted.purchase)

## --  0.116--- ##

print(missclass.error)


## choosing a K vlaue ##

predicted.purchase_new.k <- knn(train.data,test.data, train.purchase, k = 3)
missclass.error <- mean(test.purchase != predicted.purchase_new.k)
print(missclass.error)

## 0.073 ##

predicted.purchase_new.k5 <- knn(train.data,test.data, train.purchase, k = 5)
missclass.error <- mean(test.purchase != predicted.purchase_new.k5)
print(missclass.error)

## 0.066 ##

## get for loop to do manual work ##

predicted.purchase <- NULL 
error.rate <- NULL 

for(i in 1:20){
  set.seed(101)
  predicted.purchase <- knn(train.data, test.data, train.purchase, k = i)
  error.rate[i] <- mean(test.purchase != predicted.purchase)

}
## Visulaise K Elbow method ##

library(ggplot2)
k.values <- 1:20
error.df <- data.frame(error.rate, k.values)

ggp <- ggplot(error.df, aes(k.values, error.rate)) + geom_point() + geom_line(lty = 'dotted', color = 'red')

Text <- "Elbow"
annotation <- data.frame(
  x = 9,
  y = 0.083,
  label = Text
)

graph_arrow <- ggp +                               # Draw ggplot2 plot with arrow , point out arrow ##
  geom_segment(aes(x = 9,
                   y = 0.08,
                   xend = 9,
                   yend = 0.06),
               arrow = arrow(length = unit(0.7, "cm")))

graph <- graph_arrow + geom_text(data=annotation, aes( x=x, y=y, label=label),                 , 
                        color="red", 
                        size=4 , angle=0, fontface="bold" )

graph + ggtitle("KNN Elbow Method")
