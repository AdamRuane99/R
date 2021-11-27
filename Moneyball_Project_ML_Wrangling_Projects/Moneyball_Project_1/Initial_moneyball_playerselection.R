location <-  "C:/Users/Adam/Documents/GitHub/moneyball/Moneyball-Data_Project1/Batting.csv"
location_salaries <-  "C:/Users/Adam/Documents/GitHub/moneyball/Moneyball-Data_Project1/Salaries.csv"


batting <- read.csv(location)
sal <- read.csv(location_salaries)


print(head(batting$AB))
print(head(batting$X2B))


###BattingAverage new column##

batting$BA <- batting$H / batting$AB


tail(batting$BA,5)


###Now the On Base Percentage (OBP) and Slugging Percentage (SLG). ##
## 1B = H-2B-3B-HR##

batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR


# On Base Percentage
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)

# Creating Slugging Average (SLG)
batting$SLG <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) ) / batting$AB


summary(batting)
summary(sal)

## Min YEAR = 1871 ##
##For salary it is 1985## 
## I need to ensure the years are alligned ## 

batting <- subset(batting,yearID >= 1985)

summary(batting)

combo <- merge(batting,sal,by=c('playerID','yearID'))

summary(combo)

## If the Oakland A's lost 3 Players during the off season. I want to see their statistics so that I can try replicate that with another player. ##

##The players lost were: first baseman 2000 AL MVP Jason Giambi (giambja01) to the New York Yankees, ##
##outfielder Johnny Damon (damonjo01) to the Boston Red Sox and infielder Rainer Gustavo "Ray" Olmedo ('saenzol01').##

lost_players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01') )
print(head(lost_players))


## Now I have all the information we need! 
##I will Find Replacement Players for the key three players I lost! However, 

##The total combined salary of the three players can not exceed 15 million dollars.
##Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
##Their mean OBP had to equal to or greater than the mean OBP of the lost players

##Getting playesr after 2001 ##

library(dplyr)
avail.players <- filter(combo,yearID==2001)

library(ggplot2)
ggplot(avail.players,aes(x=OBP,y=salary)) + geom_point()

## The graph helps. ##
##Looks like there is no point in paying above 7 million or so ((I'm just eyeballing this number. (Suggested Number) ##
##I'll choose that as a cutt off point. There are also a lot of players with OBP==0. Let's get rid of them too.##


avail.players <- filter(avail.players,salary<7000000,OBP>0)

##The total AB of the lost players is 1469. This is about 1500, 
##meaning I should probably cut off my avail.players at 1500/3= 500 AB.##

avail.players <- filter(avail.players,AB >= 500)

##Now I will sort by OBP and see what I have got! ##

possible_suggestions <- head(arrange(avail.players,desc(OBP)),10)

## Grapb my columns ##
## One way ##
possible_suggestions <- possible_suggestions[,c('playerID','OBP','AB','salary')]

## Another way ## 

possible2 <- possible_suggestions %>% select('playerID','OBP','AB','salary')

##Can't choose giambja again, but the other ones look good (2-4). I might choose them!##

possible[2:4,]


## Salary talk! berkmla01 looks just as good for a far cheaper price???#

## Lets discuss thsi internally... ## 

## Lots of suggestions to choose from! ##
##Great, looks like I just saved the 2001 Oakland A's a lot of money! 
##If only I had a time machine and R, I could have made a lot of money in 2001 picking players!##
