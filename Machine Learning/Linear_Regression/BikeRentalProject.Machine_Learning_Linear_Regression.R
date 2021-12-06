dir <- "C:/Users/Adam/Documents/Past Projects/Data Analyst Portfolio ETL SQL PBI/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/bikeshare"

bikedata <- read.csv(paste0(dir,".csv"), header = T)


plot(x=bikedata$temp, y = bikedata$count, alpha = 0.5)


#####--------------------------------######################
library(ggplot2)

bikedata$datetime <- as.POSIXct(bikedata$datetime)

pl <- ggplot(bikedata,aes(temp,count)) 
pl1 <- pl + geom_point(alpha=0.2, aes(color=temp)) + theme_bw()
print(pl1)


######################################################


pl2<- pl1  + scale_color_continuous(low='#55D8CE',high='#FF6E2E') +theme_bw()
print(pl2)


cor(bikedata$temp,bikedata$count) ## temp-count

cor(bikedata[,c('temp','count')]) ## Table 





ggplot(bikedata, aes(factor(season), count)) + geom_boxplot(aes(color=factor(season)))+ theme_bw()


### ----------- Feature Engineering -----------#### 


bikedata$hour <- sapply(bikedata$datetime,function(x){format(x,"%H")}) ## Get hour ##

library(dplyr) 

##-- WOrking days ##


pl <- ggplot(filter(bikedata,workingday==1),aes(hour,count)) 
pl <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.5)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_bw()

##--- Non - WOrking Days --#


pl <- ggplot(filter(bikedata,workingday==0),aes(hour,count)) 
pl <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.8)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_bw()


## Building a model ##
temp.model <- lm(count~temp,bikedata)
summary(temp.model)

##If the weather was 25 degrees ##

## two lines of code ###
temp.test <- data.frame(temp=c(25))
predict(temp.model,temp.test)


bikedata$hour <- sapply(bikedata$hour,as.numeric)

## model (COUNT IS PREDICTION) ##

## Pass DF at end ##

model <- lm(count ~ . -casual - registered -datetime -atemp,bikedata ) ## WITHOUT THOSE PARTS ##
summary(model)














