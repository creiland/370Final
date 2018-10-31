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
income_clean$zipcode <- as.numeric(income_clean$zipcode)
income_clean$mean_income <- as.numeric(income_clean$mean_income)
income_clean$median_income <- as.numeric(income_clean$median_income)

#join data by  zipcode
joined <- left_join(data, income_clean, by="zipcode")

#add more specific column names to new columns
colnames(joined)[22:23] <- paste(colnames(joined)[22:23], "_by_zip", sep="")

#zillow api id
##ZWSID <- "X1-ZWz18a8v9t2x3f_1acr8"

##url <- "http://www.zillow.com/webservice/GetRegionChildren.htm?zws-id=X1-ZWz18a8v9t2x3f_1acr8&state=wa&city=seattle&childtype=neighborhood"

#read in shp file 
KC_neighborhoods <- readOGR(dsn = "data/neighborhood", layer="neighborhood")


GisToDf <- function(shpData, columnToJoin, newColName){
  #copy joined and update coordinates for joined dataset
  joined_copy <- data.frame(joined)
  sp::coordinates(joined_copy) <- ~ long + lat
  proj4string(joined_copy) <- CRS("+proj=longlat")
  
  #honestly not sure what this does but it works
  #I think it combines the coordinate data for the joined_copy and the 
  joined_copy <- spTransform(joined_copy, proj4string(shpData))
  sp::proj4string(joined_copy) <- proj4string(shpData)
  
  #joins the shpdata to the joined dataset
  joined_copy <- sp::over(joined_copy, shpData)
  return(joined_copy)
}


#dataframe for neighborhood data
nh <- GisToDf(KC_neighborhoods)

#join neighborhood to joined
joined$neighborhood <- nh$NEIGHBORHO

#set NAs for neighborhood to "No Neighborhood"
joined$neighborhood <- ifelse(is.na(joined$neighborhood), 
                             'No Neighborhood', joined$neighborhood)

#data for topography
KC_topo <- readOGR(dsn = "data/topo", layer="mtpeaks")
topog <- GisToDf(KC_topo)
