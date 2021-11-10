########################################################################################
#Name: Adam Ruane                                                                      #
#ID No: 17386193                                                                       #
#Project Name: AdamRuane_Project- R Studio                                             # 
#Description: Relationship between Death and Birth rates in North America,             #
#Date: 01/12/20                                                                        #
########################################################################################

#############################
#Read CSV File into R studio#
#############################

DataUSA = read.csv("DataUSA.csv")

#########################
#Delete Unwanted Columns#
#########################

DataUSA = DataUSA[,c(1,2,3,5,6,8,9)]

###################
#Delete Extra Rows#
###################

DataUSA = subset(DataUSA, Continent == "North America")

###########################################
#Some Statistical Calculations on the Data#
###########################################

################
#Number of Rows#
################

nrow(DataUSA)

###################
#Number of columns#
###################

ncol(DataUSA)

##########################################
# Dim finds dimension (both col and row) #
##########################################

dim(DataUSA)

##############
# Find Names #
##############

names(DataUSA)

################
# Type of Data #
################

class(DataUSA)

#############################
# Find Type for all columns #
#############################

sapply(DataUSA[1,], class)

########################
# Find Summary of Data #
########################

summary(DataUSA)

###################################
# Find Summary of columns 2 and 5 #
###################################

summary(DataUSA[,2])
summary(DataUSA[,5])

#########################################
# Find min, max, mean, sd of population #
#########################################

min(DataUSA$Population)
max(DataUSA$Population)
mean(DataUSA$Population)
sd(DataUSA$Population)

#################################################
# Find min, max, mean, sd of Birth & Death Rate #
#################################################
# Figure 8, Noted in Word Analysis  #
#####################################
min(DataUSA$Birth.Rate)
max(DataUSA$Birth.Rate)
mean(DataUSA$Birth.Rate)
sd(DataUSA$Birth.Rate)

min(DataUSA$Death.Rate)
max(DataUSA$Death.Rate)
mean(DataUSA$Death.Rate)
sd(DataUSA$Death.Rate)

##################################################

###########################################
# Now to order it by Death Rates          #
###########################################

Death.Rate= order(DataUSA$Death.Rate, decreasing= TRUE)
orderedDataUSA = DataUSA[Death.Rate,]

#######################
#Order by Birth rates #
#######################

Birth.Rate= order(DataUSA$Birth.Rate, decreasing= TRUE)
orderedDataUSA2 = DataUSA[Birth.Rate,]



######################################################
# Noted: Texas #1 Highest Death Rate, #2 California. #
# Noted: Haweii #1 highest Birth Rate, #2 California #
######################################################

DataUSA
orderedDataUSA
orderedDataUSA2

####################
# Show only 4 Rows #
####################

DataUSA[1:4,]
#Top 4 Deaths
orderedDataUSA[1:4,]
#Top 4 Births
orderedDataUSA2[1:4,]

####################

rm(orderedDataUSA)
rm(orderedDataUSA2)

########################################
# Create a new calculated column       #
#  Percentage of Deaths(as a Whole)    #
# Percentage of Births (as a Whole)     #
# People in Pop. With Health Insurance #
# Percent of Ppl without Insurance     #
########################################

DataUSA$PercentageOfDeaths= DataUSA$Death.Rate/DataUSA$Population*100 
DataUSA$PercentageOfBirths= DataUSA$Birth.Rate/DataUSA$Population*100
DataUSA$NumberOfPeopleWithINS= DataUSA$Percent.People.with.Health.Ins*DataUSA$Population/100
DataUSA$Percentage.Ppl.Without.HealthIns= (100 - DataUSA$Percent.People.with.Health.Ins)

###################################################################
# Therefore now we know exactly our percentages and figures with #
# Four extra columns. We can check with dim.                     #
###################################################################

dim(DataUSA)
names(DataUSA)

############################
# Now we can create Graphs #
############################

plot(DataUSA[,8:11])


################################################################
# So I have plotted the scatter plot given it headings,        #
# and also divided it into thousands because the numbers were, #
# too large to comprehend,                                     #
# Figure 1 in Word                                             #
################################################################

plot(DataUSA$Death.Rate/1000, DataUSA$Birth.Rate/1000, 
     main= "CORRELATION BETWEEN DEATH & BIRTH RATES",
     pch=15, las=0, col.main= "orange", cex.main=1, xlab = "Number of Deaths(thousands)",
     ylab = "Number of Births (thousands)")
###############################################################################
# Percentages #
###############
plot(DataUSA$PercentageOfDeaths, DataUSA$PercentageOfBirths, 
     main= "CORRELATION BETWEEN DEATH & BIRTH RATES",
     pch=15, las=0, col.main= "orange", cex.main=1, xlab = "Number of Deaths(%)",
     ylab = "Number of Births (%)")

##################################################################################
# Type l + H + S will give a line plot and a high intensity chart and Step chart #
##################################################################################
plot(DataUSA$PercentageOfDeaths,
     main= "Line graph Of Death Percentage",
     pch=15, las=0, col.main= "orange", cex.main=1, ylab = 
             "Number of Deaths(%)", type = "l")

plot(DataUSA$PercentageOfDeaths,
     main= "High Intensity graph Of Death Percentage",
     pch=15, las=0, col.main= "orange", cex.main=1, ylab = 
             "Number of Deaths(%)", type = "h")

plot(DataUSA$PercentageOfDeaths,
     main= "Step chart Of Death Percentage",
     pch=15, las=0, col.main= "orange", cex.main=1, ylab = 
             "Number of Deaths(%)", type = "s")

################################################################################
# Correlation between the Death Rate and those who don't have health Insurance #
################################################################################

plot(DataUSA$Percentage.Ppl.Without.HealthIns, DataUSA$PercentageOfDeaths,
     main= "CORRELATION BETWEEN DEATH Rates and Those without Health INS",
     pch=15, las=0, col.main= "orange", cex.main=0.8, xlab = "Perecentage of Ppl without Insurance",
     ylab = "Percentage of Deaths")

#################################################################################
#                             HISTOGRAMS                                        #   
#################################################################################
# On average the death rate is about 2-4%. Birth is 0-5% but there are outliers #
#################################################################################
# Figure 2 #
############
hist(DataUSA$PercentageOfDeaths, main= "Histogram of Death Rates",
     col.main= "orange", cex.main=1.1, xlab= "Perecentage of Date Rate",
     sub= "Frequency of Deaths", cex.sub=0.9, col.sub= "blue")

############
# Figure 3 #
############
hist(DataUSA$PercentageOfBirths, main= "Histogram of Birth Rates",
     col.main= "orange", cex.main=1.1, xlab= "Perecentage of Birth Rate",
     sub= "Frequency of Births", cex.sub=0.9, col.sub= "blue")

#################################################################################

####################################################################################
#set two graphs beside each other, NOTE: NOtch gives you the option to draw a notch#
####################################################################################
par(mfrow = c(1,2)) 

boxplot(DataUSA$PercentageOfDeaths, notch = TRUE, xlab= "Death Rate")
boxplot(DataUSA$PercentageOfBirths, notch = TRUE, xlab= "Birth Rate")
#################################
# Set Parameters in same graph  #
#################################
par(mfrow = c(1,1))

boxplot(DataUSA[,c(4,5,7)], notch = TRUE, xlab= "Death Rate vs Birth in Pop." )
#######################################################################################################
# Here, we saw that death and birth rates are a lot lower, but below we see death is slightly higher. #
#######################################################################################################
# Figure 4 #
############
par(mfrow = c(1,1))
boxplot(DataUSA[,c(8,9)], notch = TRUE, xlab= "Death rate vs Birth Rate")    

########################
# Death rate per State #
# Figure 5 in Word Doc #
########################


barplot(DataUSA$PercentageOfDeaths, names.arg=DataUSA$State.Name, horiz= TRUE, las = 1,
        cex.names= 0.45, border= NA, col = "green", col.axis= "red")
legend("topright", c("Death Rate", "Per State"), fill=c("green","red") );
       
#########################################################################################
# We can see that North Dakota leads the way in terms of percentage of deaths per state #
# Vermont has the lowest.                                                               #
#########################################################################################

#################################################################
# Correlation                                                   #
# Between Birth and Death Rates + Death and people w/ Insurance #
#################################################################
# Figure 6 #
############

cor(DataUSA$PercentageOfDeaths, DataUSA$PercentageOfBirths)

DeathBirthRateFit= lm(PercentageOfBirths~PercentageOfDeaths, data= DataUSA)
DeathBirthRateFit




plot(DataUSA$PercentageOfDeaths, DataUSA$PercentageOfBirths, 
     main= "CORRELATION BETWEEN DEATH & BIRTH RATES",
     pch=13,col= "green", las=0, col.main= "orange", cex.main=1, xlab = "Number of Deaths(%)",
     ylab = "Number of Births (%)")

##################################
# Adds Regression Line           #
# Dashed makes the line (dashed) #
##################################

abline(DeathBirthRateFit, col="red", lty= "dashed")

############
# Add Text #
############

text(1,25 , adj=c(0,0), labels= "Birth Rate at 5.124 + Death Rate at 0.23% ", col= "orange")


##################################################################################################
# We can see the correlation between the percentage of people without Health Ins and death rates #
# Figure 7                                                                                       #
##################################################################################################
cor(DataUSA$Percentage.Ppl.Without.HealthIns, DataUSA$PercentageOfDeaths)

DeathInsuranceFit = lm(PercentageOfDeaths~Percentage.Ppl.Without.HealthIns, data= DataUSA)
DeathInsuranceFit

plot(DataUSA$Percentage.Ppl.Without.HealthIns, DataUSA$PercentageOfDeaths,
     main= "CORRELATION BETWEEN DEATH Rates and Those without Health INS",
     pch=15, las=0, col.main= "orange", cex.main=0.8, xlab = "Perecentage of Ppl without Insurance",
     ylab = "Percentage of Deaths")

abline(DeathInsuranceFit, col= "green")


text(40,5 , adj=c(0,0), labels= "Death % at 3.213 + The % ofPeople without Ins at -0.006 ", col= "orange")

#############################
# Low Positive correlation  #
#############################
# Pch = TYpe of point to use #
##############################



#################################################
# We can then get multiple charts in one window # 
#################################################
par(mfrow=c(3,3), mar=c(2,0.5,2,1), las=1, bty="n")
#########################################################



boxplot(DataUSA$Death.Rate, notch= TRUE)
boxplot(DataUSA$Birth.Rate, notch= TRUE)

boxplot(DataUSA[,c(4,5,7)], notch= TRUE)

barplot(DataUSA$PercentageOfDeaths, names.arg=DataUSA$State.Name, horiz= TRUE, las = 1,
        cex.names= 0.45, border= NA, col = "green", col.axis= "red")
legend("topright", c("Death Rate", "Per State"), fill=c("green","red") );
#################################################################################

cor(DataUSA$PercentageOfDeaths, DataUSA$PercentageOfBirths)

DeathBirthRateFit= lm(PercentageOfBirths~PercentageOfDeaths, data= DataUSA)
DeathBirthRateFit




plot(DataUSA$PercentageOfDeaths, DataUSA$PercentageOfBirths, 
     main= "CORRELATION BETWEEN DEATH & BIRTH RATES",
     pch=13, col= "green", las=0, col.main= "orange", cex.main=1, xlab = "Number of Deaths(%)",
     ylab = "Number of Births (%)")


abline(DeathBirthRateFit, col="red", lty= "dashed")

text(1,25 , adj=c(0,0), labels= "Birth Rate at 5.124 + Death Rate at 0.23% ", col= "orange")

###########################################################################################
cor(DataUSA$Percent.People.with.Health.Ins, DataUSA$PercentageOfDeaths)

DeathInsuranceFit = lm(PercentageOfDeaths~Percent.People.with.Health.Ins, data= DataUSA)
DeathInsuranceFit

plot(DataUSA$Percent.People.with.Health.Ins, DataUSA$PercentageOfDeaths,
     main= "CORRELATION BETWEEN DEATH Rates and Those without Health INS",
     pch=14, col= "red", las=0, col.main= "orange", cex.main=0.8, xlab = "Perecentage of Ppl with Insurance",
     ylab = "Percentage of Deaths")

abline(DeathInsuranceFit, col= "green")


text(40,5 , adj=c(0,0), labels= "Death % at 3.213 + The % ofPeople with Ins at -0.006 ", col= "orange")

######################################################################################################


hist(DataUSA$PercentageOfDeaths, main= "Histogram of Death Rates",
     col.main= "orange", cex.main=1.1, xlab= "Perecentage of Date Rate",
     sub= "Frequency of Deaths", cex.sub=0.9, col.sub= "blue")

hist(DataUSA$PercentageOfBirths, main= "Histogram of Birth Rates",
     col.main= "orange", cex.main=1.1, xlab= "Perecentage of Birth Rate",
     sub= "Frequency of Births", cex.sub=0.9, col.sub= "blue")
###############################################################################

plot(DataUSA$PercentageOfDeaths,
     main= "Line graph Of Death %",
     pch=15, las=0, col.main= "orange", cex.main=1, ylab = 
             "Number of Deaths(%)", type = "s")

############################################################
# This gives us 9 Graphs together Figure (8), But now I can reset it. #
############################################################
# Reset the charting to one big chart                      #
par(mfrow=c(1,1), las=1, bty="n")
############################################################

#####################
# Create Graph File #
#####################

pdf("AdamRuane_Graphs.pdf")

plot(DataUSA[,8:11])

plot(DataUSA$Death.Rate/1000, DataUSA$Birth.Rate/1000, 
     main= "CORRELATION BETWEEN DEATH & BIRTH RATES",
     pch=15, las=0, col.main= "orange", cex.main=1, xlab = "Number of Deaths(thousands)",
     ylab = "Number of Births (thousands)")


plot(DataUSA$PercentageOfDeaths, DataUSA$PercentageOfBirths, 
     main= "CORRELATION BETWEEN DEATH & BIRTH RATES",
     pch=15, las=0, col.main= "orange", cex.main=1, xlab = "Number of Deaths(%)",
     ylab = "Number of Births (%)")


plot(DataUSA$PercentageOfDeaths,
     main= "Line graph Of Death Percentage",
     pch=15, las=0, col.main= "orange", cex.main=1, ylab = 
             "Number of Deaths(%)", type = "l")

plot(DataUSA$PercentageOfDeaths,
     main= "High Intensity graph Of Death Percentage",
     pch=15, las=0, col.main= "orange", cex.main=1, ylab = 
             "Number of Deaths(%)", type = "h")

plot(DataUSA$PercentageOfDeaths,
     main= "Step chart Of Death Percentage",
     pch=15, las=0, col.main= "orange", cex.main=1, ylab = 
             "Number of Deaths(%)", type = "s")


hist(DataUSA$PercentageOfDeaths, main= "Histogram of Death Rates",
     col.main= "orange", cex.main=1.1, xlab= "Perecentage of Date Rate",
     sub= "Frequency of Deaths", cex.sub=0.9, col.sub= "blue")


hist(DataUSA$PercentageOfBirths, main= "Histogram of Birth Rates",
     col.main= "orange", cex.main=1.1, xlab= "Perecentage of Birth Rate",
     sub= "Frequency of Births", cex.sub=0.9, col.sub= "blue")

par(mfrow = c(1,2)) 

boxplot(DataUSA$PercentageOfDeaths, notch = TRUE, xlab= "Death Rate")
boxplot(DataUSA$PercentageOfBirths, notch = TRUE, xlab= "Birth Rate")

par(mfrow=c(1,1))

barplot(DataUSA$PercentageOfDeaths, names.arg=DataUSA$State.Name, horiz= TRUE, las = 1,
        cex.names= 0.45, border= NA, col = "green", col.axis= "red")
legend("topright", c("Death Rate", "Per State"), fill=c("green","red") );



cor(DataUSA$PercentageOfDeaths, DataUSA$PercentageOfBirths)

DeathBirthRateFit= lm(PercentageOfBirths~PercentageOfDeaths, data= DataUSA)
DeathBirthRateFit




plot(DataUSA$PercentageOfDeaths, DataUSA$PercentageOfBirths, 
     main= "CORRELATION BETWEEN DEATH & BIRTH RATES",
     pch=13,col= "green", las=0, col.main= "orange", cex.main=1, xlab = "Number of Deaths(%)",
     ylab = "Number of Births (%)")


abline(DeathBirthRateFit, col="red", lty= "dashed")

text(1,25 , adj=c(0,0), labels= "Birth Rate at 5.124 + Death Rate at 0.23% ", col= "orange")



cor(DataUSA$Percentage.Ppl.Without.HealthIns, DataUSA$PercentageOfDeaths)

DeathInsuranceFit = lm(PercentageOfDeaths~Percentage.Ppl.Without.HealthIns, data= DataUSA)
DeathInsuranceFit

plot(DataUSA$Percentage.Ppl.Without.HealthIns, DataUSA$PercentageOfDeaths,
     main= "CORRELATION BETWEEN DEATH Rates and Those without Health INS",
     pch=15, las=0, col.main= "orange", cex.main=0.8, xlab = "Perecentage of Ppl without Insurance",
     ylab = "Percentage of Deaths")

abline(DeathInsuranceFit, col= "green")


text(40,5 , adj=c(0,0), labels= "Death % at 3.213 + The % ofPeople without Ins at -0.006 ", col= "orange")


dev.off()

###################
# Create New File #
###################

write.csv(DataUSA, "DataUSAFinal.csv")













