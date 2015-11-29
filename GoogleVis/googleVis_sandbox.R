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
library(googleVis)
#attach(shipping)

# set working directories 
try(setwd("/Users/codykoebnick/Downloads/Data Set"))
try(setwd("E:/bjoer/Documents/GitHub/MaritimePiracyWrangling"))
getwd()
try(setwd("/Users/laurencehendry/GitHub/MaritimePiracy")) 
getwd()

#import data
  # empty cells are now coded with NA and can manually be excluded from any function with na.omit command
shipping <- read.csv("MaritimePiracyTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = c("", "NA"))
  # have a look at how the variables are created
str(shipping)
  # creae unique identifier country year
  # GET external source data
library(dplyr)

shipping$LatLong <- paste(shipping$latitude, shipping$longitude, sep = ":") # combine lat long into new variable

G1 <- gvisGeoMap(shipping, locationvar = "LatLong", numvar = "closest_state_cow_code", hovervar = "Violence.Dummy", options=list(dataMode="markers"))
plot(G1)
