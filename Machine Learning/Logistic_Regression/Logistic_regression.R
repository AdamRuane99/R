

## ------   y = b0 + b1x <---- p = 1 
##                              ------
##                                1 + e -z 

#########-- Logistic Regression ######################
##install.packages("Amelia") ## Gets missing data ##
library(Amelia)
library(ggplot2)

dir <- "C:/Users/Adam/Documents/Past Projects/Data Analyst Portfolio ETL SQL PBI/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Machine Learning with R/"
df.train <- paste0(dir,"titanic_train.csv")
df.train <- read.csv(df.train)


str(df.train)
missmap(df.train, main = "Missing Map", col = c("white", "black"), legend = TRUE) ## This tells us where we have Nulls ##


ggplot(df.train, aes(Survived)) + geom_bar()

ggplot(df.train, aes(Sex)) + geom_bar(aes(fill=factor(Sex)))

ggplot(df.train, aes(Age)) + geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')

ggplot(df.train, aes(SibSp)) + geom_bar()

ggplot(df.train, aes(Fare)) + geom_histogram(fill= 'green', color = 'black', alpha =0.5)

pl <- ggplot(df.train, aes(Pclass, Age)) 
pl2 <- pl + geom_boxplot(aes(group= Pclass, fill= factor(Pclass), alpha = 0.4))
pl3 <- pl2 + scale_y_continuous(breaks = seq(min(0), max(80), by = 2)) 
print(pl3)


### ---- IMPUTATION ------- ####


## -- We can see the wealthier passengers in the higher classes tend to be older, which makes sense. 
## I will use these average age values to impute based on Pclass for Age. ##


impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    
    if (is.na(age[i])){
      
      if (class[i] == 1){
        out[i] <- 37
        
      }else if (class[i] == 2){
        out[i] <- 29
        
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}


## Use the Functio ##

fixed.ages <- impute_age(df.train$Age, df.train$Pclass)


df.train$Age <- fixed.ages

missmap(df.train, main= "Imputation Check", col = c("White", 'black'), legend = TRUE)

library(dplyr)

df.train <- select(df.train, -PassengerId, -Name, -Ticket, -Cabin)
head(df.train)

### --- Change to Factor ----#

df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$Parch <- factor(df.train$Parch)
df.train$SibSp <- factor(df.train$SibSp)


str(df.train)


log.model <- glm(Survived ~ . , family = binomial(link = 'logit'), data = df.train)
summary(log.model)

### ----- *** Means how important it was --------##

library(caTools)
set.seed(101)
split <- sample.split(df.train$Survived, SplitRatio = 0.7)
final.train <- subset(df.train, split == TRUE)
final.test <- subset(df.train, split = FALSE)



final.log.model <- glm(Survived ~., family = binomial(link = 'logit'), data = final.train)
summary(final.log.model)


fitted.probabilities <- predict(final.log.model, final.test, type = 'response')
fitted.results <- ifelse(fitted.probabilities > 0.5, 1,0)
missClassError <- mean(fitted.results != final.test$Survived)
print(1-missClassError)

##confusion matrix ##

table(final.test$Survived, fitted.probabilities > 0.5)



