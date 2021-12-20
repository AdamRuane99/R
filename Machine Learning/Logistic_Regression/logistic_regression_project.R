##--- Logistic Regression Project --
##In this project I will be working with the UCI adult dataset. I will be attempting to predict if people in the data set 
## belong in a certain class by salary, either making <=50k or >50k per year.

dir <- "C:/Users/Adam/Documents/"

file <- "adult_sal.csv"


adult <- read.csv(paste0(dir,file))

head(adult)

## drop index ##

library(dplyr)
adult <- select(adult,-X)

summary(adult)
str(adult)

table(adult$type_employer)

## 1836 Null Values / (worked without pay)

unemp <- function(job) {
  job <- as.character(job)
  if (job == 'Never-worked'| job=='Without-pay') {
    return('Unemployed')
  } else {
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer, unemp)
table(adult$type_employer)

group_emp <- function(job) {
  if(job == 'Local-gov' | job == 'State-gov' ) {
    return('SL-gov') 
  } else if (job == 'Self-emp-inc' | job == 'Self-emp-not-inc' ) {
    return('Self-emp')
  } else{
    return(job)
  }
}


adult$type_employer <- sapply(adult$type_employer, group_emp)
table(adult$type_employer)



table(adult$marital)

## -- Reduce this to three groups:

## -- Married
## -- Not-Married
## -- Never-Married

group_martial <- function(mar) {
  if(mar == 'Divorced' | mar == 'Widowed'| mar =='Seperated'){
    return('Not-Married')
  } else if(mar == 'Never-married') {
    return('Never-Married')
  } else {
    return('Married')
  } 
}

adult$marital <- sapply(adult$marital, group_martial)
table(adult$marital)

table(adult$country)


levels(adult$country)



Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')




group_country <- function(country){
  if(country %in% Asia){
    return('Asia') 
  } else if(country %in% North.America) {
  return('North America')
  } else if(country %in% Europe){
  return('Europe')
  } else if(country %in% Latin.and.South.America){
  return('Latin and South America')
  } else {
  return('Other')
  }
}

adult$country <- sapply(adult$country, group_country)

str(adult)

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
str(adult)

library(Amelia)

## -- Covert any ? to NA --- ## 

adult[adult == '?'] <- NA
table(adult$type_employer)
adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$occupation <- sapply(adult$occupation,factor)



missmap(adult, main = "Map of missing data" ,col = c("white", "grey"), legend = T )

head(adult$occupation)

## remove y values ##
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))
## remove NA ##
adult <- na.omit(adult)


str(adult)
## looks good ##

library(ggplot2)
library(dplyr)

ggplot(adult, aes(age)) + geom_histogram(aes(fill= income), color = 'black', binwidth = 1 + theme_bw())


ggplot(adult, aes(hr_per_week)) + geom_histogram() + theme_bw()

adult <- adult %>%
  rename(region = 'country')


chart <- ggplot(adult, aes(region)) + geom_bar(aes(fill = income) ,color = 'black')
chart + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## --- Train - Test - Split --##

class(adult$income)
adult$income <- sapply(adult$income,factor) 
class(adult$income)


set.seed(101) 
sample = sample.split(adult$income, SplitRatio = .75)
train = subset(adult, sample == TRUE)
test  = subset(adult, sample == FALSE)



model <- glm(income ~ ., family = binomial(logit), data = train)
summary(model)


## Lots of features ##
## - Using STep -- ##
## AIC ##

new.step.model <- step(model)

summary(new.step.model)


## First Model Predictions ##
test$predicted.income <- predict(model,newdata = test, type = 'response') ## Doing Classification, so Response ##

table(test$income, test$predicted.income > 0.5)

accuracy <- (6372 + 1423)/(5346 + 421 + 747 + 1165)
accuracy

## Recall ## 
(5346/(5346 + 421))

###############
test$predicted.income_2 <- predict(new.step.model,newdata = test, type = 'response') 
table(test$income, test$predicted.income_2 > 0.5)
