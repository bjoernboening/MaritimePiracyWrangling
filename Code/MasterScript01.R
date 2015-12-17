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
#empty cells are now coded with NA and can manually be excluded from any function with na.omit command
shipping <- read.csv("MaritimePiracyTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = c("", "NA"))

######################################
# Scraping Data from World Bank -BB
######################################
missmap(shipping) # eyeballing missing data

#get rid of NA for WDI parsing with factor vectors
shipping <- read.csv("MaritimePiracyTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = TRUE, na.strings = c("", "NA"))
#country names
cc <- unique(na.omit(shipping)$closest_coastal_state) #108 unique values
iso <- countrycode(cc, "country.name", "iso2c") #only 84 iso countries

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
              start=1994, end=2014)
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
#Armed Conflict
######
military <- read.csv("Data/armed-conflict.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA")) 
milcc <- military$Location #155 unique values
military$iso2c <- countrycode(milcc, "country.name", "iso2c") #only 84 iso countries
military$year <- military$Year
military$Year <- NULL

######
#Merge
######
total <- merge(allWDI,military,by=c("iso2c","year"))
#write.csv(total, file="conflictsWDImerge.csv", na = "NA") #produces csv file if desired

###########################
#single parsing if desired#
###########################
#unem <- WDI(iso, indicator = "SL.UEM.TOTL.ZS", start=1994, end=2014)
#unem.y.m <- WDI(iso, indicator = "SL.UEM.1524.MA.ZS", start=1994, end=2014)
#unem.y <- WDI(iso, indicator = "SL.UEM.1524.ZS", start=1994, end=2014)
#debt.service <- WDI(iso, indicator = "DT.TDS.DECT.GD.ZS", start=1994, end=2014)
#debt.ratio <- WDI(iso, indicator = "GC.DOD.TOTL.GD.ZS", start=1994, end=2014)
#trade.balance <- WDI(iso, indicator = "BNGSRMRCHKD", start=1994, end=2014)
#GDP.ppp <- WDI(iso, indicator = "NY.GDP.PCAP.PP.KD.ZG", start=1994, end=2014)
#GDP <- WDI(iso, indicator = "NY.GDP.PCAP.KD.ZG", start=1994, end=2014)
#easybusiness <- WDI(iso, indicator = "IC.BUS.EASE.XQ", start=1994, end=2014)
#FDI <- WDI(iso, indicator = "BN.KLT.DINV.CD.ZS", start=1994, end=2014)
#pop.growth <- WDI(iso, indicator = "SP.POP.GROW", start=1994, end=2014)
#pop.rural <- WDI(iso, indicator = "SP.RUR.TOTL.ZG", start=1994, end=2014)
#pop.urban <- WDI(iso, indicator = "SP.URB.GROW", start=1994, end=2014)
#pov.125 <- WDI(iso, indicator = "SI.POV.DDAY", start=1994, end=2014)
#pov.250 <- WDI(iso, indicator = "SI.POV.25DAY", start=1994, end=2014)
#inflation <- WDI(iso, indicator = "FP.CPI.TOTL.ZG", start=1994, end=2014)
#labor.part <- WDI(iso, indicator = "SL.TLF.ACTI.1524.ZS", start=1994, end=2014)
#vulnerable.emp.m <- WDI(iso, indicator = "SL.EMP.VULN.MA.ZS", start=1994, end=2014)
#vulnerable.emp <- WDI(iso, indicator = "SL.EMP.VULN.ZS", start=1994, end=2014)

#######################################################
#how many attacks (succ & unsucc) per country per year#
#######################################################

library(zoo)
shipping$cy <- as.character(paste(shipping$closest_coastal_state, shipping$year, sep = "-")) #paste countrylevel and year behind each other
summary(shipping$cy)



###########################
#aggregate to countrylevel#
###########################

aggdata <-aggregate(shipping, by=list(shipping$closest_coastal_state,shipping$year), 
                    FUN=mean, na.rm=TRUE)
