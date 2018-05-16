# benthic LPI data -- pre-processing to make analysis ready data # 
# written by: morgan winston
# last updated: 5/15/18 [mw]

#### initialization ####
rm(list=ls())

# load required packages
library(dplyr)
library(plyr)
library(reshape)

# set working directory
setwd("T:/Benthic/Data")

# source functions
source("C:/Users/Morgan.Winston/Documents/GitHub/Benthic-Scripts/Functions/core_functions.R")
source("C:/Users/Morgan.Winston/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions.R")
stderr <- function(x) sd(x)/sqrt(length(x))


#### data pre-processing ####

# read in data
lpi <- read.csv("ESD_BENTHIC_LPI.csv")

# remove calibration transects (which have -1 in CALIBRATION column)
lpi$CALIBRATION[is.na(lpi$CALIBRATION)] = 0
lpi <- lpi[ which(lpi$CALIBRATION == 0),]

# add leading zeroes to site names to prevent from changing to a date
lpi$SITE <- SiteNumLeadingZeros(lpi$SITE)

# add benthic categories: EMA & HALI
lpi$BENTHICCATEGORY <- as.character(lpi$BENTHICCATEGORY) # have to change from a factor to character before adding new values

for(i in c(1:nrow(lpi))){
  if(lpi$CATEGORY_GENUS[i] %in% c("Lobophora", "Peyssonelia")){
    lpi$BENTHICCATEGORY[i] = "EMA"
  }
  
  if(lpi$CATEGORY_GENUS[i] == "Halimeda"){
    lpi$BENTHICCATEGORY[i] = "HALI"
  }
}

lpi$METHOD <- "LPI"
lpi$POINTS <- 1

# generate a SITE table
SITE_FIELDS<-c("METHOD", "REGION", "ISLAND", "SITE", "LATITUDE", "LONGITUDE", "REEF_ZONE", "DEPTH_BIN", "OBS_YEAR")
sites<-aggregate(lpi[,"POINTS"],by=lpi[,SITE_FIELDS], sum)
sites$x<-NULL

# drop unnecessary columns for moving forward with processing
lpi <- lpi[, c("REGION", "ISLAND", "SITE", "REEF_ZONE", "DEPTH_BIN", "OBS_YEAR",
               "TRANNUM", "LPI_SEG", "BENTHICCATEGORY", "COUNT")]
  # note: cover will be estimated only to the benthic category level


#### begin generating benthic cover estimates ####

# calculate total points per benthic category per transect
tran<-aggregate(lpi[,"COUNT"],by=lpi[,c("REGION", "OBS_YEAR", "ISLAND", "REEF_ZONE", "DEPTH_BIN", "SITE", "TRANNUM", "BENTHICCATEGORY")], sum)
names(tran)[length(names(tran))]<-"COUNT"

# now format this more or less as a crosstab, with field of interest as column variable
tran2<-cast(tran, REGION + OBS_YEAR + ISLAND + REEF_ZONE + DEPTH_BIN + SITE + TRANNUM ~ BENTHICCATEGORY, value="COUNT", fun.aggregate=sum, fill=0)

# now convert to percentages
r_levels<-unique(as.character(lpi$BENTHICCATEGORY))
tran2$N<-rowSums(tran2[,r_levels])
tran2[,r_levels]<-tran2[,r_levels]/tran2$N

#### site level estimates ####
# now take average of transects within a site
site<-aggregate(tran2[,r_levels], by = tran2[,c("REGION", "OBS_YEAR", "ISLAND", "REEF_ZONE", "DEPTH_BIN", "SITE")],  mean)
head(site)

# save data:
setwd("") # set working directory to where you wish to save data
write.csv(site, "xxxx.csv") # save data, specifying a name for the file

#### island level estimates ####
isl_m<-aggregate(tran2[,r_levels], by = tran2[,c("REGION", "OBS_YEAR", "ISLAND")],  mean)
names(isl_m) <- c("REGION", "OBS_YEAR","ISLAND", "HALI_mean","CORAL_mean","CALG_mean","TALG_mean","MALG_mean","SAND_mean",
                  "ZOAN_mean","EMA_mean","INVT_mean","CYAN_mean","DEAD_mean","OCTO_mean","ANTH_mean","MANM_mean")
isl_se<-aggregate(tran2[,r_levels], by = tran2[,c("REGION", "OBS_YEAR", "ISLAND")],  stderr)
names(isl_se) <- c("REGION", "OBS_YEAR","ISLAND", "HALI_se","CORAL_se","CALG_se","TALG_se","MALG_se","SAND_se",
                  "ZOAN_se","EMA_se","INVT_se","CYAN_se","DEAD_se","OCTO_se","ANTH_se","MANM_se")
isl<-merge(isl_m, isl_se)

# save data:
setwd("") # set working directory to where you wish to save data
write.csv(isl, "xxxx.csv") # save data, specifying a name for the file

