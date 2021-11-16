## Dates ##

my.date <- as.Date("Nov-03-1990", format = "%b-%d-%y")
my.date


##%d = Day of month Decimal Number 
##%m = 
##%b = Month (Abreviated)
## % B Month (Full Name) 
## %y  Year (2 Digits)
## %Y = YEAR (4 Digits)

as.POSIXct("11:02:03",format = "%H:%M:%S") ##Hours, minutes, seconds -- Timetsamp info ##


help("strptime")
