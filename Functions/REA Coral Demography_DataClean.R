# Summarizing dataset E- 2013 and 2016 ----------------------------------------

################
#####
rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic Functions.R")
library(plyr) ##for ddply function below

## LOAD benthic data
x<-read.csv("Data/HistoricalREA_V0_CORAL_OBS_E.csv")
x$SITE<-SiteNumLeadingZeros(x$SITE) # Change site number such as MAR-22 to MAR-0022

# get strata and sectors data NOTE: we need these files
#sectors<-read.csv("Data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

# load site master to merge with sector names
#site_master<-read.csv("Data/SITE MASTER.csv")
#site_master$SITE<-SiteNumLeadingZeros(site_master$SITE)




# HOUSEKEEPING ------------------------------------------------------------

#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","ISLANDCODE","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","SITE_MIN_DEPTH","SITE_MAX_DEPTH","HABITAT_CODE","DIVER","TRANSECT","SEGMENT","SEGWIDTH",
             "SEGLENGTH","NO_SURVEY_YN","TAXONCODE","MORPH_CODE","COLONYLENGTH","OLDDEAD",
            "RECENTDEAD","RECENT_GENERAL_CAUSE_CODE","RECENT_SPECIFIC_CAUSE_CODE",
            "RECENTDEAD_2",	"RECENT_GENERAL_CAUSE_CODE_2","RECENT_SPECIFIC_CAUSE_CODE_2","COND",
            "EXTENT",	"SEVERITY","GENUS_CODE","S_ORDER")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

#Double check level and class of variables to make sure there aren't any errors
sapply(x,levels)
sapply(x,class)

#Removing specfic colonies and segments
x$NO_SURVEY_YN<-is.na(x$NO_SURVEY_YN)<-0 #Change NAs (blank cells) to 0
x<-subset(x,NO_SURVEY_YN>-1) #rows with -1
x<-subset(x,SEGLENGTH!="NA") #Remove segments that were not surveyed for coral demography

#Create new colummns that combine species, genus and morphology
colnames(x)[colnames(x)=="TAXONCODE"]<-"SPCODE" #Change column name
x$SPMORPH<-paste(x$SPCODE,x$MORPH_CODE,sep="")
x$GENMORPH<-paste(x$GENUS_CODE,x$MORPH_CODE,sep="")


##DOUBLE CHECK THAT WE ARE DEALING WITH NO COLONY SITES AND NAs CORRECTLY
#Remove colony fragments, note include sites with no colonies (AAAA)
x<-subset(x, COLONYLENGTH>5|SPCODE=="AAAA")
tail(x)#make sure that AAAA's are included

#Change old dead,recent dead, extent and severity from -9 or . to NA
x[x==-9]<-NA
x[x=="."]<-NA


##Calcuating segment and transect area and add column for transect area
x<-Transectarea(x)
sapply(x,levels)
head(x)
nrow(x)



#Remove transects with less than 5m surveyed and check how many rows were removed
x<-subset(x,TRANAREA>=5) 
nrow(x)
head(x)
levels(x$OBS_YEAR)

head(x)
tail(x)
sapply(x,levels)
summary(x)



#add SITE MASTER information to x -NOTE this code was copied from the fish team- do we have an analysis year and scheme?
x<-merge(x, site_master[,c("SITE", "SEC_NAME", "ANALYSIS_YEAR", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE)  #..  should actually pick up ANALYSIS_SEC from the sectors file.


#CHECK THAT all ANALYSIS_SCHEMES are present in the site_master file)
idw<-x[is.na(x$ANALYSIS_SCHEME), c("REGION", "SITE","OBS_YEAR", "METHOD"),]
if(dim(idw)[1]>0) {cat("sites with MISSING ANALYSIS_SCHEME")}   # should be 0


wd<-droplevels(x)
save(wd, file="Benthicwd.Rdata")  #Save clean working data

