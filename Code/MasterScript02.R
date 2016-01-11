#########################################################
#### Maritime Piracy Data Analysis ######################
#########################################################

# Import the dataset about piracy attacks into your wd 
# Call libraries we need for the project, make sure you have them installed
library(base)
library(rio) # swiss army knife for imports
library(plyr) # count occurences
library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(ggplot2) # nice plots
library(stargazer) # nicer regression output which looks like a real publication
library(car) # scatterplots 
library(httr) # scraping from http sites
library(XML) # Tool for generating XML file
library(WDI) # Scraping Data from the World Bank 
library(countrycode) # provides world bank country codes 
library(Amelia) #eyeballing missing values
library(tidyr) # reshaping

#set working directories if necessary (if data lies in git repo it is not necessary though)
try(setwd("E:/bjoer/Documents/GitHub/MaritimePiracyWrangling/Data"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub/MaritimePiracyWrangling/Data"),silent=TRUE)
getwd()

#import data
sea <- read.csv("sealine.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = c("", "NA"))

######################################
# Scraping Data from World Bank -BB
######################################

#get rid of NA for WDI parsing with factor vectors
sea <- read.csv("sealine.csv", header = TRUE, sep = ";", stringsAsFactors = TRUE, na.strings = c("", "NA"))
#country names in ISO
cc <- unique(na.omit(sea)$Country)
sea$iso2c <- countrycode(cc, "country.name", "iso2c")
iso <- countrycode(cc, "country.name", "iso2c")

#parsing desired data from World Bank
allWDI <- WDI(iso, indicator = c("SL.UEM.TOTL.ZS", 
                                 "SL.UEM.1524.MA.ZS", 
                                 "SL.UEM.1524.ZS",
                                 "DT.TDS.DECT.GD.ZS",
                                 "GC.DOD.TOTL.GD.ZS",
                                 "NY.GDP.PCAP.PP.KD.ZG",
                                 "NY.GDP.PCAP.KD.ZG",
                                 "IC.BUS.EASE.XQ",
                                 "BN.KLT.DINV.CD.ZS",
                                 "SP.POP.GROW",
                                 "SP.RUR.TOTL.ZG",
                                 "SP.URB.GROW",
                                 "SI.POV.DDAY",
                                 "FP.CPI.TOTL.ZG",
                                 "SL.TLF.ACTI.1524.ZS",
                                 "SL.EMP.VULN.MA.ZS",
                                 "SL.EMP.VULN.ZS"), 
              start=1993, end=2014)

# renaming
names(allWDI)[2] <- 'country'
names(allWDI)[3] <- 'year'
names(allWDI)[4] <- 'unem.total'
names(allWDI)[5] <- 'unem.youth.m'
names(allWDI)[6] <- 'unem.youth'
names(allWDI)[7] <- 'debtservice'
names(allWDI)[8] <- 'debt.gov'
names(allWDI)[9] <- 'GDP.PPP'
names(allWDI)[10] <- 'GDP'
names(allWDI)[11] <- 'easybusiness'
names(allWDI)[12] <- 'FDI.ofGDP'
names(allWDI)[13] <- 'pop.grow'
names(allWDI)[14] <- 'pop.rur'
names(allWDI)[15] <- 'pop.urb.grow'
names(allWDI)[16] <- 'pov.125'
names(allWDI)[17] <- 'infl'
names(allWDI)[18] <- 'labor.part'
names(allWDI)[19] <- 'vul.emp.m'
names(allWDI)[20] <- 'vul.emp'


######
#Merge the data sets
######
sea2 <- merge(allWDI,sea,by=c("iso2c"), all.x = TRUE) #merges WDI and countries with sealine

######
#Armed Conflict data
######
military <- read.csv("ArmedConflict.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA")) 
milcc <- military$Location #155 unique values
military$iso2c <- countrycode(milcc, "country.name", "iso2c") #only 84 iso countries
military$year <- military$Year
military$Year <- NULL

######
#Piracy data from Tenneessee
######
shipping <- read.csv("MaritimePiracyTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = c("", "NA"))
missmap(shipping) # eyeballing missing data
#country names
#cc <- unique(na.omit(shipping)$closest_coastal_state) #108 unique values
#iso <- countrycode(cc, "country.name", "iso2c") #only 84 iso countries
shipcc <- shipping$closest_coastal_state
shipping$iso2c <- countrycode(shipcc, "country.name", "iso2c")

#rename
names(shipping)[6] <- 'time'
shipping$time <- factor(shipping$time,
                        levels = c(1,2,3,4),
                        labels = c("beforenoon", "afternoon", "night", "morning"))
shipping$time <- factor(shipping$time)

names(shipping)[24] <- 'attack'
shipping$attack[shipping$attack==-99] <- NA
shipping$attack <- factor(shipping$attack,
                          levels = c(0,1),
                          labels = c("unsucc", "succ"))
shipping$attack <- factor(shipping$attack)

names(shipping)[18] <- 'vessel'
shipping$vessel[shipping$vessel==2] <- 111
shipping$vessel[shipping$vessel==4] <- 111
shipping$vessel[shipping$vessel==6] <- 111
shipping$vessel[shipping$vessel==3] <- 222
shipping$vessel[shipping$vessel==7] <- 222
shipping$vessel[shipping$vessel==8] <- 222
shipping$vessel[shipping$vessel==1] <- 333
shipping$vessel[shipping$vessel==5] <- 333
shipping$vessel[shipping$vessel==9] <- 333
shipping$vessel[shipping$vessel==10] <- 333
shipping$vessel[shipping$vessel==111] <- 1
shipping$vessel[shipping$vessel==222] <- 2
shipping$vessel[shipping$vessel==-99] <- NA
shipping$vessel[shipping$vessel==22] <- NA
shipping$vessel[shipping$vessel==696] <- NA

shipping$vessel[shipping$vessel == 111] <- 1
shipping$vessel[shipping$vessel == 222] <- 2
shipping$vessel[shipping$vessel == 333] <- 3
shipping$vessel <- factor(shipping$vessel,
                          levels = c(1,2,3),
                          labels = c("merchant", "oil", "other"))

#Aggregate to Country Level
library(reshape2)
aggregat <- dcast(shipping, iso2c + year ~ attack_vessel_recode, sum) # p317 R for Dummies
