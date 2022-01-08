## Neural Networks Project ##

## UCI Repo - Bank Auth Data set ##


## -- To check if the computer will understand whther a bank note is real or fake ##

## -- USe of wavlet Transformations to help computer understand what it is looking at ##

## -- Hopefullt the Neural network will process it and be able to detect whether it is real or fake ##

dir <- "C:/Users/Adam/Documents/Adam's File/"
file <- "bank_note_data.csv"

df <- read.csv(paste0(dir,file))

any(is.na(df))

library(corrplot)


cor <- cor(df)
corrplot(cor, tl.col = "brown", tl.srt = 30, bg = "White",
         title = "\n\n Correlation Plot Of Data",
         type = "full")



library(caTools)

sample <- sample.split(df$Class, SplitRatio = 0.7)

train <- subset(df, sample == T)
test <- subset(df, sample == F)


str(train)
train$Class <- as.factor(train$Class)

library(neuralnet)


## Get column names
n <- names(train)
# Paste together
f <- as.formula(paste("Class ~", paste(n[!n %in% "Class"], collapse = " + ")))


nn <- neuralnet(f, data = train, hidden = 10, linear.output = F)

plot(nn)


predicted.nn.values <- compute(nn,test[1:4])
str(predicted.nn.values)

head(predicted.nn.values$net.result)

predictions <- sapply(predicted.nn.values$net.result,round)


table(predictions,test$Class)

## Perfectly well ##

## -- Compare model to Random Forest -- ##

library(randomForest)
df$Class <- factor(df$Class)
library(caTools)
set.seed(101)
split = sample.split(df$Class, SplitRatio = 0.70)

train = subset(df, split == TRUE)
test = subset(df, split == FALSE)

model <- randomForest(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,data=train)

rf.pred <- predict(model,test)

table(rf.pred,test$Class)


## -- It alos perfromed really well, which is a good test for the neural net -- ##

## -- However, the Nueral Net perfromed perfectly well in comparison ## 