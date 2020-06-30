## Working with dates



# Setting a date as character

date<-'2020-06-11'

# Conversion to date (without time zone)

date_2<-as.Date(date)

is.POSIXct(date_2)

# Conversion to POSIXct (date + timezone)

date.posit<-as.POSIXct(date, 
           format="%Y-%m-%d",tz="GMT")


is.POSIXct(date.posit)


# Checking the timezone

attr(date.posit,"tzone")


# Subtracting POSIXct to obtain time difference in days

date.posit2<-as.POSIXct('2019-06-01', 
                       format="%Y-%m-%d",tz="GMT")


difftime(date.posit,date.posit2)


# Using lubridate to obtain information from date class

require(lubridate)

# Obtaining the day,month and year

day(date)

month(date)

year(date)

## WIP
