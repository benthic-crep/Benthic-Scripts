#This script will take raw CoralNet point data annotated by the robot and calculate % cover to the functional level (Tier 1).
#You will need to modify the script to include the column headers in your specific dataset. 
#Script developed by Courtney Couch 2/25/19

rm(list=ls())

library(tidyr)
library(dplyr)

setwd("C:/Users/Jonathan.Charendoff/Documents/BIA/PWF Code/")


#read in point level data 
#read in code look up table
lookup<-read.csv("CoralNet_Labelset.csv") #ESD uses a look up table with 3 classification tiers. Tier 1 being coarse functional groups (e.g. CORAL, TURF, CCA). Tier 3 is at the genus level with some genus-morphology combinations.
c.data<-read.csv("Mala_12.2023.csv")

#only keep data we want
cols <- c("Name", "Date", "Island", "Site", "Transect", "Point", "Label") #choose columns you want to keep. Your columns may be different.
c.data <- c.data[, cols]
colnames(c.data)[c(1,7)] <- c("Image_Name", "SHORT_CODE") #rename columns to merge with the look up table. May not be columns 1 and 7
colnames(lookup)[c(2,3)] <- c("SHORT_CODE", "Full_Name") #rename lookup table columns to match. Full_name is the full description for the SHORT_CODE

#Merge together data and look up table and add 00 to site numbers so you avoid MAR-22 being converted to March 22nd
ab<-merge(c.data,lookup,by="SHORT_CODE");nrow(ab)
table(ab$OBS_YEAR)


#Add this column so we can calculate percent later on
ab$POINTS<-1

#Modify and add columns
####These lines are ESD specific for relabeling some columns that we use in our survey design. 
  #ab$TIER_1<-ab$CATEGORY_CODE
  #ab$TIER_2<-ab$SUBCATEGORY_CODE
  #ab$TIER_3<-ab$GENERA_CODE
  #ab$REP<-ab$REPLICATE
  #ab$MAX_DEPTH<-ab$SITE_MAX_DEPTH_FT
  #ab$MIN_DEPTH<-ab$SITE_MIN_DEPTH_FT
####

#### REMOVE SEVERAL UNCLASSIFIED CATEGORIES PRIOR TO CALCULATING % COVER
UNIDENTIFIED<-c("MOBF", "TAPE", "UNK", "WAND", "SHAD") #change to reflect the "other" categories you don't want to include in the perecnt cover calculation

length(unique(ab$SITE))


#Generate a SITE table
SITE_FIELDS<-c("METHOD", "REGION", "OBS_YEAR", "ISLAND", "PERM_SITE", "CLIMATE_STATION_YN", "SITEVISITID","SITE", "LATITUDE", "LONGITUDE", "REEF_ZONE", "DEPTH_BIN") #Change these values to reflect your metadata
sites<-aggregate(ab[,"POINTS"],by=ab[,SITE_FIELDS], sum)
sites$x<-NULL
dim(sites)

### GENERATE DATA AT SITE LEVEL FOR TIER 1 CATEGORIES
#Sum up all tier 1 points by site - pool transects together. You need to use dcast to insert zero values where there was no coral at a site (for example)
photo <- ab %>%
  select(-SHORT_CODE) %>%
  tidyr::pivot_wider(names_from = FULL_name, values_from = POINTS, values_fill = 0, values_fn = sum)
head(photo)


#now convert to proportions
r_levels<-c(unique(as.character(ab$TIER_1)))

photo$N<-rowSums(photo[,r_levels])
data.cols<-c(r_levels)

#Substract mobile inverts and tape wand shallow and unclassified
photo$new.N<-photo$N-(photo$MF+photo$UC+photo$TW) #Change these to match yourlist of other/unclassified categories

#Calculate % cover
photo[,data.cols]<-photo[,data.cols]/photo$new.N*100
head(photo)


wsd<-merge(sites, photo, by=c("METHOD", "OBS_YEAR", "SITEVISITID","SITE"), all.y=T) #change the "by = c()" argument to reflect your metadata if needed
View(wsd) #view data in separate window

#this data is in a wide format -- 1 row is one site. You may need to use pivot_longer to better integrate into plotting or further analysis.

write.csv(wsd, "Percent_Cover_Wide_Format.csv")