empty <- data.frame() # empty data frame

c1 <- 1:10 # vector of integers

c2 <- letters[1:10] # vector of strings

df <- data.frame(col.name.1=c1,col.name.2=c2)

##IMPORTING_FILES ##
d2 <- read.csv('some.file.name.csv')

# For Excel Files
# Load the readxl package
library(readxl)
# Call info from the sheets using read.excel
df <- read_excel('Sample-Sales-Data.xlsx',sheet='Sheet1')

# Output to csv
write.csv(df, file='some.file.csv')

##FOR SQL ##
install.packages("RODBC") 
# RODBC Example of syntax
library(RODBC)

myconn <-odbcConnect("Database_Name", uid="User_ID", pwd="password")
dat <- sqlFetch(myconn, "Table_Name")
querydat <- sqlQuery(myconn, "SELECT * FROM table")
close(myconn)


##Getting Info on Data ##
# Row and columns counts
nrow(df)
ncol(df)


# Column Names
colnames(df)

# Row names (may just return index)
rownames(df)


##Referencing Cells


vec <- df[[5, 2]] # get cell by [[row,col]] num

newdf <- df[1:5, 1:2] # get multiplt cells in new df

df[[2, 'col.name.1']] <- 99999 # reassign a single cell

# returns a data frame (and not a vector!)
rowdf <- df[1, ]

# to get a row as a vector, use following
vrow <- as.numeric(as.vector(df[1,]))



cars <- mtcars
head(cars)



colv1 <- cars$mpg # returns a vector
colv1

colv2 <- cars[, 'mpg'] # returns vector
colv2

colv3<- cars[, 1] # a is int or string
colv3

colv4 <- cars[['mpg']] # returns a vector
colv4


# Ways of Returning Data Frames
mpgdf <- cars['mpg'] # returns 1 col df
head(mpgdf)

mpgdf2 <- cars[1] # returns 1 col df
head(mpgdf2)





# Both arguments are DFs)
df2 <- data.frame(col.name.1=2000,col.name.2='new' )
df2

# use rbind to bind a new row!
dfnew <- rbind(df,df2)

df$newcol <- rep(NA, nrow(df)) # NA column
df



# Rename second column
colnames(df)[2] <- 'SECOND COLUMN NEW NAME'
df

# Rename all at once with a vector
colnames(df) <- c('col.name.1', 'col.name.2', 'newcol', 'copy.of.col2' ,'col1.times.2')
df


##SELECTING MULTIPLE ROWS ##
first.ten.rows <- df[1:10, ] # Same as head(df, 10)
first.ten.rows


#SELECTING MULTIPLE COLS 
df[, c(1, 2, 3)] #Grab cols 1 2 3



df[, -1] # keep all but first column

df[, -c(1, 3)] # drop cols 1 and 3


##DEALING WITH MISSING DATA ##

any(is.na(df)) # detect anywhere in df ##Returns False##

any(is.na(df$col.name.1)) # anywhere in col

# delete selected missing data rows
df <- df[!is.na(df$col), ]

# replace NAs with something else
df[is.na(df)] <- 0 # works on whole df


df$col[is.na(df$col)] <- 999 # For a selected column