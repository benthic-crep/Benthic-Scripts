#This script will take raw CoralNet point data annotated by the robot and calculate % cover to the functional level (Tier 1).
#You will need to modify the script to include the column headers in your specific dataset. 
#Script developed by Courtney Couch 2/25/19

rm(list=ls())

setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/BIA")

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/fish_team_functions.R")

#read in point level data 
#read in code look up table
c.data<-read.csv("ASRAMP2018_image_ab_data.csv")
lookup<-read.csv("CNET_categories_lookup.csv")

#Merge together data and look up table and add 00 to site numbers so you avoid MAR-22 being converted to March 22nd
ab<-merge(c.data,lookup,by="SHORT_CODE");nrow(ab)
table(ab$OBS_YEAR)


#Modify and add columns
ab$POINTS<-1
ab$METHOD<-"CNET"
ab$TIER_1<-ab$CATEGORY_CODE
ab$TIER_2<-ab$SUBCATEGORY_CODE
ab$TIER_3<-ab$GENERA_CODE
ab$REP<-ab$REPLICATE
ab$IMAGE_NAME<-ab$ORIGINAL_FILE_NAME
ab$PHOTOID<-ab$IMAGE_NUMBER
ab$MAX_DEPTH<-ab$SITE_MAX_DEPTH_FT
ab$MIN_DEPTH<-ab$SITE_MIN_DEPTH_FT


#Generate list of sites- your columns may be different
SURVEY_INFO<-c("MISSIONID","REGION","OBS_YEAR", "ISLAND", "SITEVISITID","SITE","LATITUDE","LONGITUDE","REEF_ZONE", "DEPTH_BIN", "PERM_SITE", "CLIMATE_STATION_YN", "MIN_DEPTH", "MAX_DEPTH", "HABITAT_CODE")
survey_site<-Aggregate_InputTable(ab, SURVEY_INFO)


#Save list of sites
write.csv(survey_site,"All Photoquad Sites.csv")


### SOME CLEAN UP
#CREATING CLASS EMA "Encrusting Macroalgae
levels(ab$TIER_1)<-c(levels(ab$TIER_1), "EMA")
levels(ab$CATEGORY_NAME)<-c(levels(ab$CATEGORY_NAME), "Encrusting macroalga")
ab[ab$GENERA_NAME %in% c("Lobophora sp","Peyssonnelia sp", "Encrusting macroalga"),]$TIER_1<-"EMA"
ab[ab$GENERA_NAME %in% c("Lobophora sp","Peyssonnelia sp", "Encrusting macroalga"),]$TIER_2<-"EMA"
ab[ab$GENERA_NAME %in% c("Lobophora sp","Peyssonnelia sp", "Encrusting macroalga"),]$SUBCATEGORY_NAME<-"Encrusting macroalga"
ab[ab$GENERA_NAME %in% c("Lobophora sp","Peyssonnelia sp", "Encrusting macroalga"),]$CATEGORY_NAME<-"Encrusting macroalga"

###Create a Halimeda class
ab$GENERA_NAME<-as.character(ab$GENERA_NAME)
ab$TIER_1<-as.character(ab$TIER_1)

ab$TIER_3<-ifelse(ab$TIER_3=="HALI","HAL",as.character(ab$TIER_3))
ab$TIER_1<-ifelse(ab$TIER_3=="HAL","HAL",as.character(ab$TIER_1))
ab$CATEGORY_NAME<-ifelse(ab$TIER_3=="HAL","Halimeda sp",ab$CATEGORY_NAME)

hal<-subset(ab,TIER_1=="HAL")
head(hal)


ab<-droplevels(ab)
table(ab$ISLAND, ab$OBS_YEAR)
summary(ab)


#### REMOVE SEVERAL UNCLASSIFIED CATEGORIES PRIOR TO CALCULATING % COVER
UNIDENTIFIED_T1<-c("TW", "MF", "UC")
UNIDENTIFIED_T2<-c("MOBF", "TAPE", "UNK", "WAND", "SHAD")

length(unique(ab$SITE))

#Generate a SITE table
SITE_FIELDS<-c("METHOD", "REGION", "OBS_YEAR", "ISLAND", "PERM_SITE", "CLIMATE_STATION_YN", "SITEVISITID","SITE", "LATITUDE", "LONGITUDE", "REEF_ZONE", "DEPTH_BIN")
sites<-aggregate(ab[,"POINTS"],by=ab[,SITE_FIELDS], sum)
sites$x<-NULL
dim(sites)

### GENERATE DATA AT SITE LEVEL FOR TIER 1 CATEGORIES
#Sum up all tier 1 points by site - pool transects together. You need to use dcast to insert zero values where there was no coral at a site (for example)
photo<-dcast(ab, METHOD + OBS_YEAR + SITEVISITID + SITE  ~ TIER_1, value="POINTS", fun.aggregate=sum, fill=0)
head(photo)


#now convert to proportions
r_levels<-c(unique(as.character(ab$TIER_1)))
r_levels<-c(unique(as.character(ab$TIER_1)))

photo$N<-rowSums(photo[,r_levels])
data.cols<-c(r_levels)

#Substract mobile inverts and tape wand shallow and uclassified
photo$new.N<-photo$N-(photo$MF+photo$UC+photo$TW)

#Calculate % cover
photo[,data.cols]<-photo[,data.cols]/photo$new.N*100
head(photo)

r_levels<-c(unique(as.character(ab$TIER_1)))
data.cols<-c(r_levels)


wsd<-merge(sites, photo, by=c("METHOD", "OBS_YEAR", "SITEVISITID","SITE"), all.y=T)
View(wsd) #view data in separate window