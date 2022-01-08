## -- Kmeans Project --##

dir <- "C:/Users/Adam/Documents/"

file1 <- "winequality-red.csv"
file2 <- "winequality-white.csv"

df1 <- read.csv(paste0(dir,file1), sep = ';')
df2 <- read.csv(paste0(dir,file2), sep = ';')


df1$label <- "red"
df2$label <- "white"


wine <- rbind(df1, df2)
str(wine)


library(ggplot2)


pl <- ggplot(wine,aes(x=residual.sugar)) + geom_histogram(aes(fill=label),color='black',bins=50)
# Optional adding of fill colors
pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()

pl <- ggplot(wine,aes(x=residual.sugar)) + geom_histogram(aes(fill=label),color='black',bins=50)
# Optional adding of fill colors
pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()


pl <- ggplot(aes(x = citric.acid, y = residual.sugar)) + geom_point(aes(fill = label))

pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()


## Check for other potential features ##
pl <- ggplot(wine,aes(x=citric.acid,y=residual.sugar)) + geom_point(aes(color=label),alpha=0.2)
# Optional adding of fill colors
pl + scale_color_manual(values = c('#ae4554','#faf7ea')) +theme_dark()

##

pl <- ggplot(wine,aes(x=volatile.acidity,y=residual.sugar)) + geom_point(aes(color=label),alpha=0.2)
# Optional adding of fill colors
pl + scale_color_manual(values = c('#ae4554','#faf7ea')) +theme_dark()

## Looks like generally white wine doesn't surpass 0.6/0.7volatile acidity. BUt there is some noise.. ##


## K Means ##


clus.data <- wine[,1:12]


wine.cluster <- kmeans(clus.data, 2)

print(wine.cluster$centers)











