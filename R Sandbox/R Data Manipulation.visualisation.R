 

filepath <- "C:/Users/Adam/Documents/Past Projects/Data Analyst Portfolio ETL SQL PBI/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Capstone and Data Viz Projects/Data Visualization Project/Economist_Assignment_Data.csv"

library(readxl)
install.packages("readxl")


data <- read.csv(filepath, header =  TRUE)


library(ggplot2)
library(ggthemes)
library(data.table)
df <- fread(filepath, drop =1) ##fread will remove the first column ##
head(df)


p <- ggplot(df, aes(x = CPI, y = HDI, col = Region)) 
p2 <- p + geom_point(size = 4, shape = 1)

p3 <- p2 + geom_smooth(aes(group = 1), method = 'lm', formula = y~log(x), se = FALSE, color = 'red') ##Formual makes look logged ##
## se = FALSE removes the grey area under line ##


##p4 <-  p3 + geom_text(aes(label=Country))



pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

p4 <- p3 + geom_text(aes(label = Country), color = "gray20", 
                       data = subset(df, Country %in% pointsToLabel),check_overlap = TRUE, ggthemes) ## to avoid any overlap ## ##Use of %in%


  
print(p4)

