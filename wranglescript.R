require(dplyr)
require (tidyr)
require(lubridate)
library(httr)
library(jsonlite)

library(rgeos)
library(sp)
library(rgdal)


options(stringsAsFactors = FALSE)

setwd("C:/Users/creil/Desktop/Info370/Final")

data <- read.csv("data/kc_house_data.csv")

income <- read.csv("data/ACS_16_5YR_S1901_with_ann.csv")

#take relevant columns from income data
income_clean <- select(income, GEO.id2, HC01_EST_VC13, HC01_EST_VC15)

#clean column names
colnames(income_clean) <- c("zipcode", "median_income", "mean_income")

#remove row 1
income_clean <- income_clean[-c(1),]


factorToNum <- function (f){
  return(as.numeric(levels(f))[f])
}

#change the columns from factors to numeric
income_clean$zipcode <- factorToNum(income_clean$zipcode)
income_clean$mean_income <- factorToNum(income_clean$mean_income)
income_clean$median_income <- factorToNum(income_clean$median_income)

#join data by  zipcode
joined <- left_join(data, income_clean, by="zipcode")

#add more specific column names to new columns
colnames(joined)[22:23] <- paste(colnames(joined)[22:23], "_by_zip", sep="")

#zillow api id
ZWSID <- "X1-ZWz18a8v9t2x3f_1acr8"

url <- "http://www.zillow.com/webservice/GetRegionChildren.htm?zws-id=X1-ZWz18a8v9t2x3f_1acr8&state=wa&city=seattle&childtype=neighborhood"

KC_neighborhoods <- readOGR(dsn = "data/neighborhood", layer="neighborhood")

sp::coordinates(joined) <- ~ long + lat
proj4string(joined) <- CRS("+proj=longlat")

joined <- spTransform(joined, proj4string(KC_neighborhoods))
sp::proj4string(joined) <- proj4string(KC_neighborhoods)

sp::over(joined, KC_neighborhoods)

data$neighborhood <- new$NEIGHBORHOOD
