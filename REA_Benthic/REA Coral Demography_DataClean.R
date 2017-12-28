# This script will clean the raw benthic REA data using method E (2013-present) and prepare it for analysis

################
#####
rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic Functions.R")
library(plyr) ##for ddply function below

## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/ReportCard")
x<-read.csv("Data/HistoricalREA_V0_CORAL_OBS_E.csv")
x$SITE<-SiteNumLeadingZeros(x$SITE) # Change site number such as MAR-22 to MAR-0022

# get strata and sectors data NOTE: we need these files
#sectors<-read.csv("Data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

# load site master to merge with sector names
#site_master<-read.csv("Data/SITE MASTER.csv")
#site_master$SITE<-SiteNumLeadingZeros(site_master$SITE)




# HOUSEKEEPING ------------------------------------------------------------

#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","REGION_NAME","ISLAND","ISLANDCODE","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","SITE_MIN_DEPTH","SITE_MAX_DEPTH","SITEVISITID","HABITAT_CODE","DIVER","TRANSECT","SEGMENT","SEGWIDTH",
             "SEGLENGTH","NO_SURVEY_YN","TAXONCODE","MORPH_CODE","COLONYLENGTH","OLDDEAD",
            "RECENTDEAD","RECENT_GENERAL_CAUSE_CODE","RECENT_SPECIFIC_CAUSE_CODE",
            "RECENTDEAD_2",	"RECENT_GENERAL_CAUSE_CODE_2","RECENT_SPECIFIC_CAUSE_CODE_2","COND",
            "EXTENT",	"SEVERITY","GENUS_CODE","S_ORDER")

head(x[,DATA_COLS])
x<-x[,DATA_COLS]

#Double check level and class of variables to make sure there aren't any errors
sapply(x,levels)
sapply(x,class)

#Remove specfic colonies and segments
x$NO_SURVEY_YN<-is.na(x$NO_SURVEY_YN)<-0 #Change NAs (blank cells) to 0
x<-subset(x,NO_SURVEY_YN>-1) #Exclude rows -1
x<-subset(x,SEGLENGTH!="NA") #Remove segments that were not surveyed for coral demography

##Change column names to make code easier to write
colnames(x)[colnames(x)=="TAXONCODE"]<-"SPCODE" #Change column name
colnames(x)[colnames(x)=="RECENTDEAD"]<-"RDEXTENT1" #Change column name
colnames(x)[colnames(x)=="RECENT_GENERAL_CAUSE_CODE"]<-"GENRD1" #Change column name
colnames(x)[colnames(x)=="RECENT_SPECIFIC_CAUSE_CODE"]<-"RD1" #Change column name
colnames(x)[colnames(x)=="RECENTDEAD_2"]<-"RDEXTENT2" #Change column name
colnames(x)[colnames(x)=="RECENT_GENERAL_CAUSE_CODE_2"]<-"GENRD2" #Change column name
colnames(x)[colnames(x)=="RECENT_SPECIFIC_CAUSE_CODE_2"]<-"RD2" #Change column name
colnames(x)[colnames(x)=="COND"]<-"COND3" #Change column name
colnames(x)[colnames(x)=="EXTENT"]<-"EXTENT3" #Change column name
colnames(x)[colnames(x)=="SEVERITY"]<-"SEVERITY3" #Change column name
colnames(x)[colnames(x)=="COND_DESCRIPTION"]<-"COND_DESCRIPTION3" #Change column name

#Create new colummns that combine species, genus and morphology
x$SPMORPH<-paste(x$SPCODE,x$MORPH_CODE,sep="")
x$GENMORPH<-paste(x$GENUS_CODE,x$MORPH_CODE,sep="")


##DOUBLE CHECK THAT WE ARE DEALING WITH NO COLONY SITES AND NAs CORRECTLY

#Remove scleractinan colony fragments, but include include sites with no colonies (AAAA) and other cnidarians
#if AAAA or other cnidarians are excluded here then the transect area may not calculated properly.
x<-subset(x, COLONYLENGTH>5&S_ORDER=="Scleractinia"|COLONYLENGTH<0|SPCODE=="AAAA")
tail(x)#make sure that AAAA's are included


##Calcuating segment and transect area and add column for transect area
x<-Transectarea(x)
sapply(x,levels)
head(x)
nrow(x)



#Remove transects with less than 5m surveyed and check how many rows were removed
nrow(x)
wd<-subset(x,TRANSECTAREA>=5) 
nrow(wd)
head(wd)

#######################
## CLEAN UP NAs #######
#######################
x[x=="-9"]<-NA
x[x=="."]<-NA
tmp.lev<-levels(x$GENRD1); head(tmp.lev)
levels(x$GENRD1)<-c(tmp.lev, "UNKNOWN")
x[is.na(x$GENRD1),"GENRD1"]<-"UNKNOWN"

tmp.lev<-levels(x$RD1); head(tmp.lev)
levels(x$RD1)<-c(tmp.lev, "UNKNOWN")
x[is.na(x$RD1),"RD1"]<-"UNKNOWN"

tmp.lev<-levels(x$GENRD2); head(tmp.lev)
levels(x$GENRD2)<-c(tmp.lev, "UNKNOWN")
x[is.na(x$GENRD2),"GENRD2"]<-"UNKNOWN"

tmp.lev<-levels(x$RD2); head(tmp.lev)
levels(x$RD2)<-c(tmp.lev, "UNKNOWN")
x[is.na(x$RD2),"RD2"]<-"UNKNOWN"

head(x)

####Fixing Errors in data where causes of recent mortality were entered into both the recent cause and the "COND3" columns.
#This code converts the 3 condition columns to characters first (won't work as factors) then changes the "COND3" column to "UNKNOWN"
#if it is already listed in either of the recent cause columns. It also changes the extent to -9.
x$COND3<-as.character(x$COND3)
x$RD1<-as.character(x$RD1)
x$RD2<-as.character(x$RD2)

dim(x[(x$RD1==x$COND3)|(x$RD2==x$COND3),]) #check dimension of df
x[(x$RD1==x$COND3)|(x$RD2==x$COND3),] #show rows that meet criteria

#change cells
x$EXTENT3<-ifelse(((x$RD1==x$COND3)|(x$RD2==x$COND3)),-9, x$EXTENT3)
x$COND3<-ifelse(((x$RD1==x$COND3)|(x$RD2==x$COND3)),"UNKNOWN", x$COND3)







#add SITE MASTER information to x -NOTE this code was copied from the fish team- I'm working on an analogous file for these data
#x<-merge(x, site_master[,c("SITE", "SEC_NAME", "ANALYSIS_YEAR", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE)  #..  should pick up ANALYSIS_SEC from the sectors file.


#CHECK THAT all ANALYSIS_SCHEMES are present in the site_master file)
#idw<-x[is.na(x$ANALYSIS_SCHEME), c("REGION", "SITE","OBS_YEAR", "METHOD"),]
#if(dim(idw)[1]>0) {cat("sites with MISSING ANALYSIS_SCHEME")}   # should be 0

wd<-droplevels(wd)


save(wd, file="Benthicwd.Rdata")  #Save clean working data

