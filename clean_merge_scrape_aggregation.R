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
#attach(shipping)

# set working directories 
try(setwd("/Users/codykoebnick/Downloads/Data Set"),silent=TRUE)
try(setwd("E:/bjoer/Documents/GitHub/MaritimePiracyWrangling"),silent=TRUE)
try(setwd("/Users/laurencehendry/GitHub/MaritimePiracy"),silent=TRUE) 
getwd()

#import data
  # empty cells are now coded with NA and can manually be excluded from any function with na.omit command
shipping <- read.csv("MaritimePiracyTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = c("", "NA"))
  # have a look at how the variables are created
str(shipping)
  # create sub sample for wrangling
sample <- shipping[shipping$closest_coastal_state %in% c("Indonesia", "Malaysia", "Singapore") & year[2004:2014] ]
sub <- subset(sample, year > 2004, select = c(1:25))

sample$incident_type[is.na(sample$incident_type)] <- NULL
sample[!is.na(sample$closest_coastal_state), ]

# Fix or drop! combine date into one column - 
unite(shipping, "date", c("year", "month", "day"), sep = "-")


######################################
# Add a variable on country coastline length web-scraped from wikipedia -LH
#####################################

# Scraping from Wikipedia
URL <- 'https://en.wikipedia.org/wiki/List_of_countries_by_length_of_coastline'

tables <- URL %>% GET() %>%
  content(as = 'parsed') %>% 
  readHTMLTable()

names(tables)

CoastlineTable <- tables[[1]]

head(CoastlineTable)[, 1:3]

#cleaning the scrape for merge
CoastlineTable$V2 = NULL
CoastlineTable$V3 = NULL
CoastlineTable$V4 = NULL
CoastlineTable$V5 = NULL
CoastlineTable$V6 = NULL
CoastlineTable$V8 = NULL

#renaming, part of scrape
colnames(CoastlineTable)
names(CoastlineTable)
names(CoastlineTable)[1] <- 'closest_coastal_state'
names(CoastlineTable)[2] <- 'Coast/Area ratio (m/km2)'

#merging
#p297 from R for Dummies
allmerge <- merge(shipping, CoastlineTable, all.x=TRUE)


######################################
# Scraping Data from World Bank -CK
######################################

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
