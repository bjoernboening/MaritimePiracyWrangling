#########################################################
#### Maritime Piracy Data Analysis ######################
#########################################################

######
#IMPORT
######

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
library(Amelia) # eyeballing missing values

# set working directories if necessary (if data lies in git repo it is not necessary though)
try(setwd("E:/bjoer/Documents/GitHub/MaritimePiracyWrangling/Data"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub/MaritimePiracyWrangling/Data"), silent=TRUE)
getwd()

#import data
# empty cells are now coded with NA and can manually be excluded from any function with na.omit command
piracy <- read.csv("MaritimePiracyTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = c("", "NA"))
# have a look at how the variables are created
#str(shipping)

# create sub sample for tests
#sample <- shipping[shipping$closest_coastal_state %in% c("Indonesia", "Malaysia", "Singapore") & year[2004:2014] ]
#sub <- subset(sample, year > 2002, select = c(1:25))
# to get rid of NAs
#sample$incident_type[is.na(sample$incident_type)] <- NULL
#sample[!is.na(sample$closest_coastal_state), ]

#subsetting (keep) variables
sub <- piracy[c(4, 6, 12, 18, 24, 23, 25)]

######
#DATA WRANGLING
######

#renaming and recoding
names(sub)[1] <- 'year'

names(sub)[2] <- 'time'
sub$time <- factor(sub$time,
                   levels = c(1,2,3,4),
                   labels = c("early", "day", "evening", "night"))
sub$time <- factor(sub$time)

names(sub)[3] <- 'state'

names(sub)[4] <- 'type'
sub$type[sub$type==1] <- 111
sub$type[sub$type==5] <- 111
sub$type[sub$type==9] <- 111
sub$type[sub$type==2] <- 222
sub$type[sub$type==3] <- 222
sub$type[sub$type==4] <- 222
sub$type[sub$type==6] <- 222
sub$type[sub$type==7] <- 222
sub$type[sub$type==8] <- 222
sub$type[sub$type==111] <- 1
sub$type[sub$type==222] <- 2
sub$type[sub$type==-99] <- NA
sub$type[sub$type==10] <- NA
sub$type[sub$type==22] <- NA
sub$type[sub$type==696] <- NA
sub$type <- factor(sub$type,
                   levels = c(1,2),
                   labels = c("small", "big"))
sub$type <- factor(sub$type)

names(sub)[5] <- 'incident'
sub$incident[sub$incident==-99] <- NA
sub$incident <- factor(sub$incident,
                       levels = c(0,1),
                       labels = c("attempted", "acutal"))
sub$incident <- factor(sub$incident)

names(sub)[6] <- 'stat'
sub$stat[sub$stat==-99] <- NA
sub$status <- recode(sub$stat, "c(1)='2'; c(2,3,4)='1'") # what a bastard this line was arrgg
sub$status <- factor(sub$status,
                     levels = c(1,2),
                     labels = c("stationary", "moving"))
sub$status <- factor(sub$status)
sub$stat = NULL

names(sub)[6] <- 'violence'
sub$violence[sub$violence==-99] <- NA
sub$violence[sub$violence==10] <- NA
sub$violence[sub$violence==5] <- NA
sub$violence[sub$violence==6] <- 4
sub$violence <- factor(sub$violence,
                     levels = c(1,2,3,4),
                     labels = c("hijacked", "boarding", "fired upon", "detained"))
sub$violence <- factor(sub$violence)

# Delete missing values through listwise deletion
# you can omit NAs in the analysis
#sub <- na.omit(sub)

######
#Aggregate to MACRO
######

#library(reshape2)
#agg1 <- dcast(sub, state + year ~ incident, length) # p317
#agg2 <- dcast(sub, state + year ~ time, length) # p317
#agg3 <- dcast(sub, state + year ~ status, length) # p317
#agg4 <- dcast(sub, state + year ~ violence, length) # p317
#agg5 <- dcast(sub, state + year ~ type, length) # p317
#aggregated1 <- merge(agg1,agg2, by=c("state","year"))
#aggregated2 <- merge(agg3,agg4, by=c("state","year"))
#aggregated3 <- merge(aggregated2, agg5, by=c("state", "year"))
#finalagg <- merge(aggregated3, aggregated1, by=c("state", "year"))
#write.csv(finalagg, "TimeSeries.csv")

# create iso2c for merging with remaining datasets
subcc <- finalagg$state #155 unique values
finalagg$iso2c <- countrycode(subcc, "country.name", "iso2c") #only 84 iso countries


######
#Armed Conflict
######
military <- read.csv("armed-conflict.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA")) 
milcc <- military$Location #155 unique values
military$iso2c <- countrycode(milcc, "country.name", "iso2c") #only 84 iso countries
military$year <- military$Year
military$Year <- NULL

######
#Scraping Data from World Bank -BB
######
missmap(shipping)
#get rid of NA for WDI parsing
shipping <- read.csv("MaritimePiracyTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = TRUE, na.strings = c("", "NA"))
#shipping$closest_coastal_state <- na.omit(shipping)$closest_coastal_state
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
              start=1993, end=2014)

# renaming
names(allWDI)[1] <- 'iso2c'
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
names(allWDI)[13] <- 'pop'
names(allWDI)[14] <- 'pop.rur'
names(allWDI)[15] <- 'pop.urb'
names(allWDI)[16] <- 'pov.125'
names(allWDI)[17] <- 'infl'
names(allWDI)[18] <- 'labor.part'
names(allWDI)[19] <- 'vul.emp.m'
names(allWDI)[20] <- 'vul.emp'

######
#Merge
######
total <- merge(allWDI,military,by=c("iso2c","year"), all.x = TRUE) #merges conflicts and WDI
total2 <- merge(total,finalagg,by=c("iso2c","year"), all.y =TRUE) #adds WDI and conflict data only in years with incidents
#needs revision if we plan to add null incident years for each country as well
#write.csv(total2, "AllTimeSeries.csv")

#single parsing if desired
unem <- WDI(iso, indicator = "SL.UEM.TOTL.ZS", start=1994, end=2014)
unem.y.m <- WDI(iso, indicator = "SL.UEM.1524.MA.ZS", start=1994, end=2014)
unem.y <- WDI(iso, indicator = "SL.UEM.1524.ZS", start=1994, end=2014)
debt.service <- WDI(iso, indicator = "DT.TDS.DECT.GD.ZS", start=1994, end=2014)
debt.ratio <- WDI(iso, indicator = "GC.DOD.TOTL.GD.ZS", start=1994, end=2014)
#trade.balance <- WDI(iso, indicator = "BNGSRMRCHKD", start=1994, end=2014)
GDP.ppp <- WDI(iso, indicator = "NY.GDP.PCAP.PP.KD.ZG", start=1994, end=2014)
GDP <- WDI(iso, indicator = "NY.GDP.PCAP.KD.ZG", start=1994, end=2014)
easybusiness <- WDI(iso, indicator = "IC.BUS.EASE.XQ", start=1994, end=2014)
FDI <- WDI(iso, indicator = "BN.KLT.DINV.CD.ZS", start=1994, end=2014)
pop.growth <- WDI(iso, indicator = "SP.POP.GROW", start=1994, end=2014)
pop.rural <- WDI(iso, indicator = "SP.RUR.TOTL.ZG", start=1994, end=2014)
pop.urban <- WDI(iso, indicator = "SP.URB.GROW", start=1994, end=2014)
pov.125 <- WDI(iso, indicator = "SI.POV.DDAY", start=1994, end=2014)
#pov.250 <- WDI(iso, indicator = "SI.POV.25DAY", start=1994, end=2014)
inflation <- WDI(iso, indicator = "FP.CPI.TOTL.ZG", start=1994, end=2014)
labor.part <- WDI(iso, indicator = "SL.TLF.ACTI.1524.ZS", start=1994, end=2014)
vulnerable.emp.m <- WDI(iso, indicator = "SL.EMP.VULN.MA.ZS", start=1994, end=2014)
vulnerable.emp <- WDI(iso, indicator = "SL.EMP.VULN.ZS", start=1994, end=2014)

###Grab GDP per capita data for our 10 key countries
countries <- c("Indonesia", "Yemen", "Malaysia", "Bangladesh", "Nigeria", "India", "Somalia", "Philippines", "Vietnam", "Brazil")

# Convert the country names to iso2c format used in the World Bank data
iso2cNames <- countrycode(countries, "country.name", "iso2c")

#actual scraping and creating a new dataset 
wdiData2 <- WDI(iso2cNames, indicator='NY.GDP.PCAP.PP.CD', start=1994, end=2014)
#Getting rid of the first columm.  
wdiData2$iso2c = NULL

######
#Merging Data
######
names(wdiData2)[1] <- 'closest_coastal_state'
total2 <- merge(allmerge,wdiData2,by=c("closest_coastal_state","year"))


########################################
#Creating a new variable for frequency count of attacks per country per year -CK
#########################################
#data.frame ( table ( data$Group, data$Size ) )
CountYrCtry <- table (shipping$year, shipping$closest_coastal_state)
CountYrCtry
#Checking our newly created table.   It is indeed a table.  We need a variable.
class(CountYrCtry)
#coverts our table into a variable 
CountYrCtryVar3 = as.data.frame(CountYrCtry)
class(CountYrCtryVar3)
#preparing to merge, renaming new variable's columns  
names(CountYrCtryVar3)[1] <- 'year'
names(CountYrCtryVar3)[2] <- 'closest_coastal_state'
#merging our new variable into the dataset.  
total3 <- merge(total2,CountYrCtryVar3,by=c("closest_coastal_state","year"))


##########################################
#222Creating a new variable for the success ratio of attacks in a given year per country
##########################################
#data.frame ( table ( data$Group, data$Size ) )
SuccRatCtryYr <- table (shipping$year, shipping$closest_coastal_state, shipping$Incident_type_recode)
SuccRatCtryYr
class(SuccRatCtryYr)

#coverts our table into a variable 
SuccRatCtryYr2 = as.data.frame(SuccRatCtryYr)
class(SuccRatCtryYr2)

#preparing to merge, renaming new variable's columns  
names(SuccRatCtryYr2)[1] <- 'year'
names(SuccRatCtryYr2)[2] <- 'closest_coastal_state'
names(SuccRatCtryYr2)[3] <- 'Incident_type_recode'
names(SuccRatCtryYr2)[4] <- 'Atk_suc_count'

#merging our new variable into the dataset.  
total4 <- merge(total3,SuccRatCtryYr2,by=c("closest_coastal_state","year", "Incident_type_recode"))

#Creating a new variable 
total4$Suc_Rat2 <- total4$Atk_suc_count/total4$Freq


#######################################
#Cleaning the Master -LH
#######################################
total4$incident_type = NULL
total4$day = NULL
total4$month = NULL
total4$timeofday = NULL
total4$timeofdayrecode = NULL
total4$incident_action = NULL
total4$latitude = NULL
total4$longitude = NULL
total4$location_description = NULL
total4$territorial_water_status = NULL
total4$closest_state_cow_code = NULL
total4$location_precision = NULL
total4$geolocation_source = NULL
total4$location_desription = NULL
total4$vessel_name = NULL
total4$vessel_country = NULL
total4$Vessel_country_cow_code = NULL
total4$vessel_status = NULL
total4$Violence.Dummy = NULL
total4$Steaming.Recode = NULL
total4$Incident_action_recode = NULL
total4$vessel_type = NULL

#######################################
## Descriptive Statistics
#######################################
# count countries = 9 total 
count(total4$closest_coastal_state)
sort(table(total4$closest_coastal_state), decreasing = TRUE)

# Fix or Drop! Ploting the GDP per capita change overtime in our 10 countries.
ggplot(wdiData2, aes(year, NY.GDP.PCAP.PP.CD, color=country)) + geom_line() + 
  xlab('Year') + ylab('GDP per capita')

####Creating a new variable for frequency count of attacks per country per year
#data.frame ( table ( data$Group, data$Size ) )
CountYrCtry <- table (shipping$year, shipping$closest_coastal_state)
CountYrCtry
shipping$CountYrCtryVar2  <- table (shipping$year, shipping$closest_coastal_state)

#attempting a probit regression 
shipping$timeofdayrecode <- factor(shipping$timeofdayrecode)
shipping$vessel_type <- factor(shipping$vessel_type)
shipping$vessel_status <- factor(shipping$vessel_status)
shipping$Incident_type_recode <- factor(shipping$Incident_type_recode)

xtabs(~ timeofdayrecode + Incident_type_recode, data=shipping)

myprobit <- glm(Incident_type_recode ~ timeofdayrecode + vessel_type + vessel_status, family=binomial(link="probit"), data=shipping)
summary (myprobit)

#estimate model
logit1 <- glm(notyemen ~ as.factor(shiptype) + as.factor(shipcategory), data = shipping, familiy = "binominal")
lm(logit1)

source()

###############################
### End of script
