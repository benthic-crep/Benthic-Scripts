#This script will take raw CoralNet point data annotated by the robot and calculate % cover to the functional level (Tier 1).
#You will need to modify the script to include the column headers in your specific dataset. 
#Script developed by Courtney Couch 2/25/19

rm(list=ls())

library(tidyr)
library(dplyr)

setwd()#change to your working directory


#read in point level data 
#read in code look up table
lookup<-read.csv("LOOKUP.csv") #ESD uses a look up table with 3 classification tiers. Tier 1 being coarse functional groups (e.g. CORAL, TURF, CCA). Tier 3 is at the genus level with some genus-morphology combinations.
c.data<-read.csv("RAW_DATA.csv")

#only keep data we want
cols <- c("Name", "Date", "Island", "Site", "Transect", "Point", "Label") #choose columns you want to keep. Your columns may be different.
c.data <- c.data[, cols]
colnames(c.data)[c(1,7)] <- c("Image_Name", "SHORT_CODE") #rename columns to merge with the look up table. May not be columns 1 and 7
colnames(lookup)[c(2,3)] <- c("SHORT_CODE", "Full_Name") #rename lookup table columns to match. Full_name is the full description for the SHORT_CODE

#Merge together data and look up table
ab<-merge(c.data,lookup[,2:3],by="SHORT_CODE");nrow(ab)



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


### GENERATE DATA AT SITE LEVEL
#Sum up all points
photo <- ab %>%
  select(-c(SHORT_CODE, Image_Name)) %>% #need to remove the SHORT_CODE column because it is redundant to Full_Name. Add in more columns into select() to change the level you want to summarize at. If you were to summarize at the transect level, you would add in any column that contains information about sub-transect levels.
  tidyr::pivot_wider(names_from = Full_Name, values_from = POINTS, values_fill = 0, values_fn = sum)
head(photo)


#now convert to proportions
r_levels<-c(unique(as.character(ab$Full_Name)))

photo$N<-rowSums(photo[,r_levels])
data.cols<-c(r_levels)

#Substract mobile inverts and tape wand shallow and unclassified
photo$new.N<-photo$N-(photo$`Diseased Coral`+ photo$`No Disease`) #Change these to match yourlist of other/unclassified categories

#Calculate % cover
photo[,data.cols]<-photo[,data.cols]/photo$new.N*100
head(photo)

#this data is in a wide format -- 1 row is one site. You may need to use pivot_longer to better integrate into plotting or further analysis.

write.csv(photo, "Percent_Cover_Wide_Format.csv")