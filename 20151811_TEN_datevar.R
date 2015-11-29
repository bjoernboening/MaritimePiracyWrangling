# Daniel's date var
setwd("C:/Users/Dani/Google Drive/05_Master Thesis/00_Piracy_2015-16/03_Data/Tennessee/CSV")
TEN <- read.csv("20151118_TEN.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")

str(TEN)
str(TEN$day) #check for character
str(TEN$month) #check for character
str(TEN$year) #check for character

#install.packages("stringr")
library(stringr)
TEN$day <- str_pad(TEN$day, 2, pad = "0") #add zero before one digit number
summary(TEN$day)

TEN$month <- str_pad(TEN$month, 2, pad = "0") #add zero before one digit number
summary(TEN$month)

library(zoo)
TEN$date3 <- as.character(paste(TEN$year, TEN$month, sep = "-")) #paste year month behind each other
summary(TEN$date3)
#zoo <- read.zoo(TEN$date2, FUN = as.yearmon)
#plot(zoo)
TEN$date3 <- as.yearmon(TEN$date3)

#TEN$day <- as.numeric(TEN$day)
#TEN$month <- as.numeric(TEN$month)
#TEN$year <- as.numeric(TEN$year)
#TEN$year <- TEN$year-1900

#substring(TEN$year, 1, 1)

TEN$year <- substring(TEN$year, 3) #delete first two digits
summary(TEN$year)

library(base)

#create date: year/month/day
TEN$date <- as.character(paste(TEN$year, TEN$month, TEN$day, sep = "/")) #paste time variables behind each other
summary(TEN$date)

TEN$date <- as.Date(TEN$date, "%y/%m/%d") #set as Date variable

#create a date2: year/month/01
TEN$date2 <- as.character(paste(TEN$year, TEN$month, "01", sep = "/")) #paste year month var behind each other, add 01 for day
summary(TEN$date2)

TEN$date2 <- as.Date(TEN$date2, "%y/%m/%d") #set as Date variable, year month only
