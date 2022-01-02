##-- Lending Club data -- ##

dir <- "C:/Users/Adam/Documents/"

file <- "loan_data.csv"

loans <- read.csv(paste0(dir,file))


loans$inq.last.6mths <- as.factor(loans$inq.last.6mths)
loans$credit.policy <- factor(loans$credit.policy)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)

library(ggplot2)

pl <- ggplot(loans, aes(x = fico)) 
pl <- pl + geom_histogram(aes(fill = not.fully.paid), color = 'black')
pl + scale_fill_manual(values = c('green','red')) + theme_bw()


pl <- ggplot(loans, aes(x = factor(purpose))) 
pl <- pl + geom_bar(aes(fill = not.fully.paid), color = 'black', position = "dodge")
pl + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


 ggplot(loans, aes(x = int.rate, y = fico)) + geom_point()
 
 
 ggplot(loans, aes(x = int.rate, y = fico)) + geom_point(aes(fill = not.fully.paid), alpha = 0.3) + theme_bw()
 
 
 ## -- Train the model -- ##
 
 library(caTools)

 sample <- sample.split(loans$not.fully.paid, SplitRatio = 0.7) 
train <- subset(loans, sample == T ) 
test <- subset(loans, sample == F ) 
 

library(e1071)

model <- svm(not.fully.paid ~. , data = train )

summary(model)


pred.values <- predict(model, test[1:13]) ## without not fully paud ##

table(pred.values, test$not.fully.paid)


## -- Tune results ##

tune.results <- tune(svm,train.x=not.fully.paid~., data=train,kernel='radial',
                     ranges=list(cost= c(0.1, 0.5, 1, 10), gamma= c(0.1, 0.5, 0.75, 1)))


model <- svm(not.fully.paid ~ .,data=train,cost=10,gamma = 0.1)
predicted.values <- predict(model,test[1:13])
table(predicted.values,test$not.fully.paid)
