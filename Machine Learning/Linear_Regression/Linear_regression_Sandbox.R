any(is.na(df)) ## Check for NA Values ##
str(df)

library(corrgram)
install.packages("corrplot")
library(corrplot)

# Numbers only ##
##Useful line ##

num.cols <- sapply(df, is.numeric)
# filter 
## print correlation matrix ##
cor.data <- cor(df[,num.cols])
print(cor.data)



##install.packages('corrgram') ##

## start witgh corrplot ##
## Quick visual correlation ##
print(corrplot(cor.data))

corrgram(df)


corrgram(df,
         type = NULL,
         order = FALSE,
         diag.panel = NULL,
         label.pos = c(0.5, 0.5),
         label.srt = 0,
         cex.labels = NULL,
         font.labels = 1,
         row1attop = TRUE,
         dir = "",
         gap = 0,
         abs = FALSE,
         cor.method = "pearson",
         outer.labels = NULL
)


ggplot(df, aes(x=G3)) + geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')
print()




##---------------------------------------------
## ----------- TRAIN AND TEST MODEL ------##########

install.packages("caTools")
library(caTools)


## Setting a seed ##

set.seed(101)
##

## split sample ##


sample <- sample.split(df$G3,SplitRatio = 0.7) ## 70% TRAIN DATA 30% TEST DATA ##

## 70% Training ##
train <- subset(df, sample == TRUE)

## 30% Test ## 

test <- subset(df, sample = FALSE)


##model <- lm(y ~ x1 + x2, data ) # Feature column 


##model <- lm(y ~., data ) # Feature column  ## use all features ##


##Train and build model ## 

model <- lm(G3 ~., train)

## quick summary and interpretation of model ##


summary(model)

## coeefcients section ##

##estimates = value of slope calculated by regression ##

## pvalue small == better ## 
## stars/asterix ##

## significance levels ##
## the more stars the higher the significance ##
## 3 stars unlikely that the value isn't a relevnat featre. (likely a strong feature to use ##)##

## r squared value to value the goodness of fit, higher the better ##


res <- residuals(model)
class(res)


res <- as.data.frame(res)
head(res)


ggplot(res, aes(res)) + geom_histogram(fill = "blue" )



## easy  way to display model visually ##
plot(model)



## good to have it evenly distributed ##


## negative residuals predicting poor test results ##


##PREDICTIONS ##

##predict off test dataset 

G3.predictions <- predict(model, test)

results <- cbind(G3.predictions, test$G3)
colnames(results) <- c('predicted', 'actual')
results <- as.data.frame(results)
print(head(results))




Stored_ML <- "C:/Users/Adam/Documents/Past Projects/Data Analyst Portfolio ETL SQL PBI/ML Folder/"
stored_results <- paste(Stored_ML,"predicted.vs.actual_before_function.csv")

write.csv(results,stored_results)


## However I noticed the model was considering 0 scores ##

## I want to remove that and create a function to do so and remove negative values##


to_zero <- function(x) {
  if(x <0){
    return(0)
  }else{
    return(x)
  }
}

min(results) ##doesn't make sense to have negatives ##



results$predicted <- sapply(results$predicted,to_zero)

min(results) ## to zero 

stored_results_ammended <- paste(Stored_ML,"predicted.vs.actual_after_function.csv")

write.csv(results,stored_results_ammended)

## Mean-squared 


MSE <- mean((results$actual - results$predicted)^2)
print('MSE')
print(MSE)

print("Squared Root of MSE")
RMSE <- print(MSE ^ 0.5)


#############



SSE <- sum((results$predicted - results$actual)^2)
SST <- sum( ( mean(df$G3)- results$actual)^2)


R2 <- 1 - SSE/SST
print(R2)


## explaining about 83.5 % ## 
