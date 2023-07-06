#This script will take raw CoralNet point data annotated by the robot and calculate % cover to the functional level (Tier 1).
#You will need to modify the script to include the column headers in your specific dataset. 
#Script developed by Courtney Couch 6/13/2023

rm(list=ls())

library(dplyr)
library(tidyr)

#read in point level data 
#read in code look up table
c.data<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Raw Data from CoralNet/2017_NWHI_CnetAnnotations.csv")
lookup<-read.csv("T:/Benthic/Data/Lookup Tables/All_Photoquad_codes.csv")
lookup<- lookup %>% 
  rename("SHORT_CODE" = "Cnet_SHORT_CODE")

#Merge together data and look up table and add 00 to site numbers so you avoid MAR-22 being converted to March 22nd
ab<-left_join(c.data,lookup,by="SHORT_CODE");nrow(ab)
table(ab$OBS_YEAR)


#Modify and add columns
ab$POINTS<-1
ab$REP<-ab$REPLICATE
ab$IMAGE_NAME<-ab$ORIGINAL_FILE_NAME
ab$PHOTOID<-ab$IMAGE_NUMBER

#Generate list of sites- your metadata columns may be different
survey_site<-unique(ab[,c("REGION_NAME","OBS_YEAR", "ISLANDCODE","SITE")])#create a list of Genera and Species by region and year

#Save list of sites
write.csv(survey_site,"All Photoquad Sites.csv")


### SOME CLEAN UP
#CREATING CLASS EMA "Encrusting Macroalgae
levels(ab$TIER_1)<-c(levels(ab$TIER_1), "EMA")
levels(ab$T1_DESC)<-c(levels(ab$T1_DESC), "Encrusting macroalga")
ab[ab$T3_DESC %in% c("Lobophora sp","Peyssonnelia sp", "Encrusting macroalga"),]$TIER_1<-"EMA"
ab[ab$T3_DESC %in% c("Lobophora sp","Peyssonnelia sp", "Encrusting macroalga"),]$TIER_2<-"EMA"
ab[ab$T3_DESC %in% c("Lobophora sp","Peyssonnelia sp", "Encrusting macroalga"),]$T2_DESC<-"Encrusting macroalga"
ab[ab$T3_DESC %in% c("Lobophora sp","Peyssonnelia sp", "Encrusting macroalga"),]$T1_DESC<-"Encrusting macroalga"

###Create a Halimeda class
ab$T3_DESC<-as.character(ab$T3_DESC)
ab$TIER_1<-as.character(ab$TIER_1)

ab$TIER_3<-ifelse(ab$TIER_3=="HALI","HAL",as.character(ab$TIER_3))
ab$TIER_1<-ifelse(ab$TIER_3=="HAL","HAL",as.character(ab$TIER_1))
ab$T1_DESC<-ifelse(ab$TIER_3=="HAL","Halimeda sp",ab$T1_DESC)

hal<-subset(ab,TIER_1=="HAL")
head(hal)


ab<-droplevels(ab)
table(ab$ISLAND, ab$OBS_YEAR)
summary(ab)


#### REMOVE SEVERAL UNCLASSIFIED CATEGORIES PRIOR TO CALCULATING % COVER
UNIDENTIFIED_T1<-c("TW", "MF", "UC")
UNIDENTIFIED_T2<-c("MOBF", "TAPE", "UNK", "WAND", "SHAD")

length(unique(ab$SITE))


### GENERATE DATA AT SITE LEVEL FOR TIER 1 CATEGORIES
#Sum up all tier 1 points by site - pool all photos together. 
photo<-ab %>% pivot_wider(
  id_cols = c(REGION_NAME,OBS_YEAR,ISLANDCODE,SITE),
  names_from = "TIER_1", 
  values_from = "POINTS",
  values_fn = sum,
  values_fill=0)      

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

View(photo)

#Generate island/year summary
df.summary<- photo %>% 
  group_by(OBS_YEAR,ISLANDCODE) %>% 
 summarize_at(vars(CORAL,CCA,TURF, MA), funs(mean, sd))

