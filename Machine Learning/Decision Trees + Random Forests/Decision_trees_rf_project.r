## Tree Methods Project ##

library(ISLR) 

df <- College


library(ggplot2)

pl <- ggplot(df, aes(Room.Board, Grad.Rate))
pl2 <- pl + geom_point(aes(color = Private))
print(pl2)


plh <- ggplot(df, aes(F.Undergrad)) + geom_histogram(aes(fill = Private), color = 'Black')
print(plh)

## Chnaging outluer with grad rate > 100 to 100 

subset(df, Grad.Rate > 100)
df['Cazenovia College','Grad.Rate'] <- 100

library(caTools)

sample = sample.split(df$Private, SplitRatio = .70)
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)


library(rpart)

#-- Train Model ==

tree <- rpart(Private ~., method = 'class', data = train)

tree.preds <- predict(tree,test)

###

tree.preds <- as.data.frame(tree.preds)
joiner <- function(x) {
  if (x>=0.5) {
 return('Yes') }
else{
  return('No')
}
}


tree.preds$Private <- sapply(tree.preds$Yes, joiner)
head(tree.preds)


table(tree.preds$Private, test$Private)



library(rpart.plot)
## Plot tree 

prp(tree)

rf.model <- randomForest(Private ~., data = train, importance = T)

rf.model$confusion


rf.model$importance

rf.preds <- predict(rf.model, test)

table(rf.preds, test$Private)


