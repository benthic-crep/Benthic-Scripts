#### Benthic DIVERVSDIVER function - to compare benthic REA estimates (density, colony length, old dead, recent dead, condition/disease prevalence) of one diver versus their buddy
## Version: 05/18/2018
## Written by: Marie Ferguson & Courtney Couch
## Note: benthic divervsdiver functions based off of fish REA divervsdiver functions

# required files: ALL_REA_ADULTCORAL_RAW.rdata, ALL_REA_JUVCORAL_RAW.rdata
# required function files: Benthic_functions.R, core_functions.R
# currently this is set up to specify the following data: data, date 1, date 2, date 3; e.g. divervsdiver(working.data, date1="2015-03-27", date2="2015-03-28", date3="2015-03-29")
# divervsdiver function saves one png containing 1 graph for each benthic REA summary metric in the working directory


#CREATE ADULT CLEAN ANALYSIS READY DATA----------------------------------------
# This script will clean the raw benthic REA data using method E (2013-present) and prepare it for analysis

rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")

library(tidyr)
library(vegan)
library(ggplot2)
library(splitstackshape)
library(plyr)
library(dplyr)


## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
load("ALL_REA_ADULTCORAL_RAW.rdata") #from oracle
x<-df #leave this as df

x$SITE<-SiteNumLeadingZeros(x$SITE) # Change site number such as MAR-22 to MAR-0022


### Use these functions to look at data
head(x)
tail(x)


# load site master to merge with sector names later in the script
# See REA Generate Benthic Site Master and Sectors script for more details on how site master file was created.
site_master<-read.csv("Benthic2013-17_SiteMaster_v5.csv",stringsAsFactors = F);nrow(site_master)
site_master$SITE<-SiteNumLeadingZeros(site_master$SITE)


#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","REGION_NAME","ISLAND","ISLANDCODE","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "SITE_MIN_DEPTH","SITE_MAX_DEPTH","SITEVISITID","HABITAT_CODE","DIVER","TRANSECT","SEGMENT","TRANWIDTH",
             "TRANLENGTH","EXCLUDE_FLAG","NO_SURVEY_YN","COLONYID","SPECIES","MORPH_CODE","COLONYLENGTH","OLDDEAD",
             "RECENTDEAD","RECENT_GENERAL_CAUSE_CODE","RECENT_SPECIFIC_CAUSE_CODE",
             "RECENTDEAD_2",	"RECENT_GENERAL_CAUSE_CODE_2","RECENT_SPECIFIC_CAUSE_CODE_2","DZCODE",
             "EXTENT",	"SEVERITY","GENUS_CODE","S_ORDER","TAXONNAME")

#remove extraneous columns
head(x[,DATA_COLS])
x<-x[,DATA_COLS]


SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE",  "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE")
survey_site<-Aggregate_InputTable(x, SURVEY_INFO)


#Double check level and class of variables to make sure there aren't any errors
#sapply(x,levels)
#sapply(x,class)##Change column names to make code easier to code

colnames(x)[colnames(x)=="TRANWIDTH"]<-"SEGWIDTH" #Change column name
colnames(x)[colnames(x)=="TRANLENGTH"]<-"SEGLENGTH" #Change column name
colnames(x)[colnames(x)=="SPECIES"]<-"SPCODE" #Change column name

head(x)



#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
taxa<-read.csv("2013-17_Taxa_MASTER.csv")

nrow(x)

#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
x.<-Convert_to_Taxoncode(x)

#Check to see whether there are hard corals that have a SPCODE and GENUSCODE but no S_ORDER
test<-x.[is.na(x.$S_ORDER),]
levels(test$SPCODE) #there should be not hard corals in this list

#Change columns to character
x.$GENUS_CODE<-as.character(x.$GENUS_CODE)
x.$SPCODE<-as.character(x.$SPCODE)
x.$TAXONCODE<-as.character(x.$TAXONCODE)
x.$S_ORDER<-as.character(x.$S_ORDER)

#This will fix errors where there is missing s_order for hard corals
x.$S_ORDER<-ifelse(x.$GENUS_CODE %in% taxa$SPCODE,"Scleractinia",x.$S_ORDER)

#There are some SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genus or taxoncodes to the spcode and the remaining NAs in the Taxon and genus code to AAAA

x.$GENUS_CODE<-ifelse(is.na(x.$GENUS_CODE)&x.$S_ORDER=="Scleractinia",x.$SPCODE,x.$GENUS_CODE)
x.$TAXONCODE<-ifelse(is.na(x.$TAXONCODE)&x.$S_ORDER=="Scleractinia",x.$SPCODE,x.$TAXONCODE)

x.$TAXONCODE[is.na(x.$TAXONCODE)]<-"AAAA" #change nas to AAAA
x.$GENUS_CODE[is.na(x.$GENUS_CODE)]<-"AAAA"#change nas to AAAA
#utils::View(x) #view data in separate window

#Check that Unknown scl were changed correctly
test<-subset(x.,TAXONCODE=="UNKN"&S_ORDER=="Scleractinia");head(test)
test<-subset(x.,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia");head(test)
test<-subset(x.,GENUS_CODE=="AAAA");head(test)

#Confirm that no rows were dropped during merge
nrow(x)
nrow(x.)
x<-x.

#Test whether there are missing values in the NO_SURVEY_YN column. The value should be 0 or -1
x.na<-x[is.na(x$NO_SURVEY_YN),]
test<-ddply(x.na,.(SITE),
            summarize,
            SEG=length(unique(SEGMENT)))
test

#Convert NAs to 0 and remove trasects with no surveys
x$EXCLUDE_FLAG<-is.na(x$EXCLUDE_FLAG)<-0 #Change NAs (blank cells) to 0
x$NO_SURVEY_YN<-is.na(x$NO_SURVEY_YN)<-0 #Change NAs (blank cells) to 0
x<-subset(x,NO_SURVEY_YN>-1) #Exclude rows -1
x<-subset(x,SEGLENGTH!="NA") #Remove segments that were not surveyed for coral demography



##Calcuating segment and transect area and add column for transect area
x<-Transectarea(x)
sapply(x,levels)
head(x)
nrow(x)

#add SITE MASTER information to x 
nrow(x)
x<-merge(x, site_master[,c("OBS_YEAR","SITEVISITID","SITE","ANALYSIS_YEAR")], by=c("OBS_YEAR","SITEVISITID","SITE"), all.y=TRUE)  
length(unique(x$SITEVISITID)) #double check that sites weren't dropped
head(x);nrow(x)


## CLEAN UP NAs ##

x[,9:ncol(x)][x[,9:ncol(x)]=="."]<-NA #fix this
x[,9:ncol(x)][x[,9:ncol(x)]=="-9"]<-NA
head(x)

DATA_COLS<-c("MISSIONID","REGION_NAME","ISLAND","SITE","LATITUDE","LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "SITEVISITID","HABITAT_CODE","DIVER","TRANSECT","SEGMENT","SEGWIDTH",
             "SEGLENGTH","COLONYID","SPCODE","MORPH_CODE","COLONYLENGTH","GENUS_CODE","S_ORDER","TRANSECTAREA","ANALYSIS_YEAR")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

awd13_17<-droplevels(x)


## CREATE JUVENILE CLEAN ANALYSIS READY DATA ----

load("ALL_REA_JUVCORAL_RAW.rdata")
x<-df
x$SITE<-SiteNumLeadingZeros(x$SITE)

#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","REGION_NAME","ISLAND","ISLANDCODE","SITE","LATITUDE","LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "SITEVISITID","HABITAT_CODE","DIVER","TRANSECT","SEGMENT","TRANWIDTH",
             "TRANLENGTH","COLONYID","SPECIES","MORPH_CODE","COLONYLENGTH","GENUS_CODE","S_ORDER","EXCLUDE_FLAG")

head(x[,DATA_COLS])
x<-x[,DATA_COLS]


#Double check level and class of variables to make sure there aren't any errors
sapply(x,levels)
sapply(x,class)##Change column names to make code easier to code
colnames(x)[colnames(x)=="SPECIES"]<-"SPCODE" #Change column name
colnames(x)[colnames(x)=="TRANWIDTH"]<-"SEGWIDTH" #Change column name
colnames(x)[colnames(x)=="TRANLENGTH"]<-"SEGLENGTH" #Change column name


#Remove specfic colonies and segments
x$EXCLUDE_FLAG<-is.na(x$EXCLUDE_FLAG)<-0 #Change NAs (blank cells) to 0
x<-subset(x,EXCLUDE_FLAG>-1);summary(x$EXCLUDE_FLAG) #Exclude rows -1
x<-subset(x,SEGLENGTH!="NA") #Remove segments that were not surveyed for juveniles
nrow(x)


##Calcuating segment and transect area and add column for transect area
x<-Transectarea(x)
head(x)

#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
taxa<-read.csv("2013-17_Taxa_MASTER.csv")

#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
x.<-Convert_to_Taxoncode(x)

#There are some SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genus or taxoncodes to the spcode and the remaining NAs in the Taxon and genus code to AAAA
x.$GENUS_CODE<-as.character(x.$GENUS_CODE)
x.$SPCODE<-as.character(x.$SPCODE)
x.$TAXONCODE<-as.character(x.$TAXONCODE)

x.$GENUS_CODE<-ifelse(is.na(x.$GENUS_CODE)&x.$S_ORDER=="Scleractinia",x.$SPCODE,x.$GENUS_CODE)
x.$TAXONCODE<-ifelse(is.na(x.$TAXONCODE)&x.$S_ORDER=="Scleractinia",x.$SPCODE,x.$TAXONCODE)

x.$TAXONCODE[is.na(x.$TAXONCODE)]<-"AAAA" #change nas to AAAA
x.$GENUS_CODE[is.na(x.$GENUS_CODE)]<-"AAAA"#change nas to AAAA
#utils::View(x.) #view data in separate window

#Check that Unknown scl were changed correctly
test<-subset(x.,TAXONCODE=="UNKN"&S_ORDER=="Scleractinia");head(test)
test<-subset(x.,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia");head(test)
test<-subset(x.,GENUS_CODE=="AAAA");head(test)

#Confirm that no rows were dropped during merge
nrow(x)
nrow(x.)
x<-x.

#add SITE MASTER information to x 
nrow(x)
x<-merge(x, site_master[,c("OBS_YEAR","SITEVISITID","SITE","ANALYSIS_YEAR")], by=c("OBS_YEAR","SITEVISITID","SITE"), all.y=TRUE)  
length(unique(x$SITEVISITID)) #double check that sites weren't dropped
head(x);nrow(x)


## CLEAN UP NAs 

x[,9:ncol(x)][x[,9:ncol(x)]=="."]<-NA #fix this
x[,9:ncol(x)][x[,9:ncol(x)]=="-9"]<-NA
head(x)

DATA_COLS<-c("MISSIONID","REGION_NAME","ISLAND","SITE","LATITUDE","LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "SITEVISITID","HABITAT_CODE","DIVER","TRANSECT","SEGMENT","SEGWIDTH",
             "SEGLENGTH","COLONYID","SPCODE","MORPH_CODE","COLONYLENGTH","GENUS_CODE","S_ORDER","TRANSECTAREA","ANALYSIS_YEAR")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

jwd13_17<-droplevels(x)

######################################################################
## CREATE 2018 ADULT CLEAN ANALYSIS READY DATA ----------------------------------------
# This script will clean the raw benthic REA from ASRAMP 2018 (prior to being incorporated into oracle) and prepare it for analysis
######################################################################

## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/ASRAMP 2018")
ad<-read.csv("ASRAMP2018_adults.csv") #read in adult data

#read in a temporary file of all of the segments surveyed. The adult demographic data spit out by the application doesn't have the segments that no colonies were found.
seg<-read.csv("ASRAMP2018_segreport.csv");seg<-subset(seg,select = -c(6,9,11))

#split the segment data table into adults and juvs
segad<-subset(seg,Method=="ADULT")
segjuv<-subset(seg,Method=="JUVENILE")

#Add a colony id (temporary)
ad$COLONYID<-seq(1:length(ad$COLONYID))

#merge in the no colony segments
x<-merge(ad,segad,by=c("MISSIONID","OBS_YEAR","SITE","SITEVISITID","LATITUDE",	"LONGITUDE","DIVER","ISLAND","TRANSECT","SEGMENT","SEGLENGTH","SEGWIDTH","REEF_ZONE","DEPTH_BIN","SITE_MIN_DEPTH","SITE_MAX_DEPTH"),all=TRUE)

x$SITE<-SiteNumLeadingZeros(x$SITE) # Change site number such as MAR-22 to MAR-0022


### Use these functions to look at data
head(x)
tail(x)
colnames(x)

#Create vector of column names to include then exclude unwanted columns from dataframe
#NOTE- I REMOVED RECENTDEAD_3 FOR NOW BECAUSE IT'S NOT IN THE APP REPORT
DATA_COLS<-c("MISSIONID","REGION_NAME","ISLAND","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "SITEVISITID","HABITAT_CODE","DIVER","TRANSECT","SEGMENT","No.colony.observed","SEGWIDTH","SEGLENGTH",
             "COLONYID","SPECIES","MORPH_CODE","COLONYLENGTH","OLDDEAD",
             "RECENTDEAD","RECENT_GENERAL_CAUSE_CODE","RECENT_SPECIFIC_CAUSE_CODE",
             "RECENTDEAD_2",	"RECENT_GENERAL_CAUSE_CODE_2","RECENT_SPECIFIC_CAUSE_CODE_2",	
             "RECENT_GENERAL_CAUSE_CODE_3","RECENT_SPECIFIC_CAUSE_CODE_3","CONDITION_1",
             "CONDITION_2","CONDITION_3","GENUS_CODE","S_ORDER","TAXONNAME","SITE_MIN_DEPTH","SITE_MAX_DEPTH")

#remove extraneous columns
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

#Double check level and class of variables to make sure there aren't any errors
sapply(x,levels)
sapply(x,class)##Change column names to make code easier to code

colnames(x)[colnames(x)=="SPECIES"]<-"SPCODE" #Change column name
colnames(x)[colnames(x)=="RECENTDEAD"]<-"RDEXTENT1" #Change column name
colnames(x)[colnames(x)=="RECENT_GENERAL_CAUSE_CODE"]<-"GENRD1" #Change column name
colnames(x)[colnames(x)=="RECENT_SPECIFIC_CAUSE_CODE"]<-"RD1" #Change column name
colnames(x)[colnames(x)=="RECENTDEAD_2"]<-"RDEXTENT2" #Change column name
colnames(x)[colnames(x)=="RECENT_GENERAL_CAUSE_CODE_2"]<-"GENRD2" #Change column name
colnames(x)[colnames(x)=="RECENT_SPECIFIC_CAUSE_CODE_2"]<-"RD2" #Change column name
colnames(x)[colnames(x)=="RECENT_GENERAL_CAUSE_CODE_3"]<-"GENRD3" #Change column name
colnames(x)[colnames(x)=="RECENT_SPECIFIC_CAUSE_CODE_3"]<-"RD3" #Change column name
colnames(x)[colnames(x)=="REGION_NAME"]<-"REGION" #Change column name


#add temporary missionid and region names
x$REGION_NAME<-ifelse(x$ISLAND %in% c("Baker","Howland","Jarvis","Kingman","Palmyra"),"Pacific Remote Island Areas","American Samoa")


head(x)

# #Create a list of sites only surveyed for photoquads
# SURVEY_INFO<-c("NO_SURVEY_YN","SITEVISITID","OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT")
# pq_only<-Aggregate_InputTable(x, SURVEY_INFO)
# pq<-subset(pq_only,NO_SURVEY_YN==-1)
# 


#There are some SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genus to the spcode and the remaining NAs in the Taxon and genus code to AAAA
x$GENUS_CODE<-as.character(x$GENUS_CODE)
x$SPCODE<-as.character(x$SPCODE)

#x$GENUS_CODE<-ifelse(is.na(x$GENUS_CODE)&x$S_ORDER=="Scleractinia",x$SPCODE,x$GENUS_CODE)

x$GENUS_CODE[is.na(x$GENUS_CODE)]<-"AAAA"#change nas to AAAA
x$SPCODE[is.na(x$SPCODE)]<-"AAAA"#change nas to AAAA

utils::View(x) #view data in separate window



##Calcuating segment and transect area and add column for transect area
x<-Transectarea(x)
sapply(x,levels)
head(x)
nrow(x)

#There is an error with one of the sites (OFU-1000) this is a temporary fix
x$TRANSECT<-1

## CLEAN UP NAs ##

x[x=="-"]<-NA #fix this
x[x=="-9"]<-NA
tmp.lev<-levels(x$GENRD1); head(tmp.lev)
levels(x$GENRD1)<-c(tmp.lev, "NONE") # change to NONE
x[is.na(x$GENRD1),"GENRD1"]<-"NONE"

tmp.lev<-levels(x$RD1); head(tmp.lev)
levels(x$RD1)<-c(tmp.lev, "NONE")
x[is.na(x$RD1),"RD1"]<-"NONE"

tmp.lev<-levels(x$GENRD2); head(tmp.lev)
levels(x$GENRD2)<-c(tmp.lev, "NONE")
x[is.na(x$GENRD2),"GENRD2"]<-"NONE"

tmp.lev<-levels(x$RD2); head(tmp.lev)
levels(x$RD2)<-c(tmp.lev, "NONE")
x[is.na(x$RD2),"RD2"]<-"NONE"

tmp.lev<-levels(x$CONDITION_1); head(tmp.lev)
levels(x$CONDITION_1)<-c(tmp.lev, "NONE")
x[is.na(x$CONDITION_1),"CONDITION_1"]<-"NONE"

tmp.lev<-levels(x$CONDITION_2); head(tmp.lev)
levels(x$CONDITION_2)<-c(tmp.lev, "NONE")
x[is.na(x$CONDITION_2),"CONDITION_2"]<-"NONE"

tmp.lev<-levels(x$CONDITION_3); head(tmp.lev)
levels(x$CONDITION_3)<-c(tmp.lev, "NONE")
x[is.na(x$CONDITION_3),"CONDITION_3"]<-"NONE"


DATA_COLS<-c("MISSIONID","REGION","ISLAND","SITE","LATITUDE","LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "SITEVISITID","HABITAT_CODE","DIVER","TRANSECT","SEGMENT","SEGWIDTH",
             "SEGLENGTH","COLONYID","SPCODE","MORPH_CODE","COLONYLENGTH","GENUS_CODE","S_ORDER","TRANSECTAREA")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

head(x)

awd18<-droplevels(x)

# CREATE JUVENILE CLEAN ANALYSIS READY DATA ----

## CREATE JUVENILE CLEAN ANALYSIS READY DATA ----
juv<-read.csv("ASRAMP2018_juveniles.csv");colnames(juv)[9]<-"TRANSECT"
segjuv<-subset(seg,Method=="JUVENILE")

#Create colony id that doesn't overlap with adults
juv$temp<-seq(1:length(juv$COLONYID))
juv$COLONYID<-juv$temp+21291


#merge juv and segments together
x<-merge(juv,segjuv,by=c("OBS_YEAR","MISSIONID","SITE","SITEVISITID","LATITUDE","DIVER",	"LONGITUDE","ISLAND","TRANSECT","SEGMENT","SEGLENGTH","SEGWIDTH","REEF_ZONE","DEPTH_BIN"),all=TRUE)

x$SITE<-SiteNumLeadingZeros(x$SITE)


#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION_NAME","ISLAND","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "SITEVISITID","HABITAT_CODE","DIVER","TRANSECT","SEGMENT","SEGWIDTH",
             "SEGLENGTH","COLONYID","MORPH_CODE","SPECIES","COLONYLENGTH","GENUS","S_ORDER","SITE_MIN_DEPTH","SITE_MAX_DEPTH")

head(x[,DATA_COLS])
x<-x[,DATA_COLS]


x$REGION_NAME<-ifelse(x$ISLAND %in% c("Baker","Howland","Jarvis","Kingman","Palmyra"),"Pacific Remote Island Areas","American Samoa")
genlist<-read.csv("T:/Benthic/Data/SpGen_Reference/AllGenList.csv");genlist<-subset(genlist,select=-c(Genus))

x<-merge(x,genlist,by="GENUS",all.x=TRUE);x

#Double check level and class of variables to make sure there aren't any errors
#sapply(x,levels)
#sapply(x,class)##Change column names to make code easier to code
colnames(x)[colnames(x)=="SPECIES"]<-"SPCODE" #Change column name
colnames(x)[colnames(x)=="REGION_NAME"]<-"REGION" #Change column name

#There are some SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genus to the spcode and the remaining NAs in the Taxon and genus code to AAAA
x$GENUS_CODE[x$GENUS_CODE=="-"]<-NA 
x$GENUS_CODE<-as.character(x$GENUS_CODE)
x$SPCODE<-as.character(x$SPCODE)

x$GENUS_CODE<-ifelse(x$SPCODE=="UNKN","UNKN",x$GENUS_CODE)

#Remove Tubastrea from dataframe
x$S_ORDER<-ifelse(x$SPCODE=="TUSP",NA,as.character(x$S_ORDER))
x$GENUS_CODE<-ifelse(x$SPCODE=="TUSP",NA,as.character(x$GENUS_CODE))
x$SPCODE<-ifelse(x$SPCODE=="TUSP",NA,as.character(x$SPCODE))
x$COLONYLENGTH<-ifelse(x$GENUS=="Tubastraea","-",x$COLONYLENGTH)
x$GENUS<-ifelse(x$GENUS=="Tubastraea",NA,as.character(x$GENUS))
head(subset(x,SITE=="JAR-01916"))
head(subset(x,ISLAND=="Kingman"))

#Change NA to AAAA to ensure you keep the 0s in
x$GENUS_CODE[is.na(x$GENUS_CODE)]<-"AAAA"#change nas to AAAA
x$SPCODE[is.na(x$SPCODE)]<-"AAAA"#change nas to AAAA

utils::View(x) #view data in separate window



##Calcuating segment and transect area and add column for transect area
x<-Transectarea(x)
sapply(x,levels)
head(x)
nrow(x)

#There is an error with one of the sites (OFU-1000) this is a temporary fix
x$TRANSECT<-1

## CLEAN UP NAs ##

x[x=="-"]<-NA #fix this
x[x=="-9"]<-NA

DATA_COLS<-c("MISSIONID","REGION","ISLAND","SITE","LATITUDE","LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "SITEVISITID","HABITAT_CODE","DIVER","TRANSECT","SEGMENT","SEGWIDTH",
             "SEGLENGTH","COLONYID","SPCODE","MORPH_CODE","COLONYLENGTH","GENUS_CODE","S_ORDER","TRANSECTAREA")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

jwd18<-droplevels(x)

##COMBINE old and new data
awd13_17<-subset(awd13_17,select= -c(OBS_YEAR))
colnames(awd13_17)[colnames(awd13_17)=="REGION_NAME"]<-"REGION" #Change column name
colnames(awd13_17)[23]<-"OBS_YEAR"
awd13_17<-as.data.frame(awd13_17,stringsAsFactors=F)

jwd13_17<-subset(jwd13_17,select= -c(OBS_YEAR))
colnames(jwd13_17)[colnames(jwd13_17)=="REGION_NAME"]<-"REGION" #Change column name
colnames(jwd13_17)[23]<-"OBS_YEAR"
jwd13_17<-as.data.frame(jwd13_17,stringsAsFactors=F)


awd18<-as.data.frame(awd18,stringsAsFactors=F)
jwd18<-as.data.frame(jwd18,stringsAsFactors=F)


head(awd13_17);head(awd18)
head(jwd13_17);head(jwd18)

awd<-rbind(awd13_17,awd18)
jwd<-rbind(jwd13_17,jwd18)

colnames(awd13_17);colnames(awd18)
sort(colnames(jwd13_17));sort(colnames(jwd18))

#######################################################

### Final Tweaks -------------------------------------------------

## Colony fragments and scleractinans are subseted in the functions 
# Double check that there are no NAs in GENUS_CODE- change
new_DF <- awd[is.na(awd$GENUS_CODE),] 
new_DF
# new_DF2 <- jwd[is.na(jwd$GENUS_CODE),] 
# new_DF2

# Add a column for adult fragments so we can remove them from the dataset later (-1 indicates fragment)
awd$Fragment<-ifelse(awd$COLONYLENGTH < 5 & awd$S_ORDER == "Scleractinia", -1, 0)
#jwd$Fragment<-0 # you need to add this column so that you can use the site level functions correctly

# Remove transects with less than 5m surveyed and check how many rows were removed
nrow(awd)
nrow(jwd)
awd<-subset(awd,TRANSECTAREA >= 5) 
jwd<-subset(jwd,TRANSECTAREA >= 1)
nrow(awd)
nrow(jwd)

## Change Transects 3 and 4 in the juvenile data to 1 and 2 so we can merge with adult data
# BE CAREFUL- because we survey different areas for adults and juveniles you can not merge together ad and juvs until AFTER you calculate density
jwd$TRANSECT[jwd$TRANSECT == "3"] <- "1"
jwd$TRANSECT[jwd$TRANSECT == "4"] <- "2"

############################################################


#Create a look a table of all of the colony attributes- you will need this for the Calc_RDden and Calc_Condden functions
SURVEY_INFO<-c("MISSIONID","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SITE",  "REEF_ZONE",
               "DEPTH_BIN", "TRANSECT","SEGMENT","DIVER","COLONYID","GENUS_CODE","SPCODE","MORPH_CODE","COLONYLENGTH")
survey_colony<-Aggregate_InputTable(awd, SURVEY_INFO)

SURVEY_INFO<-c("MISSIONID","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SITE",  "REEF_ZONE",
               "DEPTH_BIN", "TRANSECT","DIVER")
survey_transect<-Aggregate_InputTable(awd, SURVEY_INFO)

SURVEY_INFO<-c("MISSIONID","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SITE",  "REEF_ZONE",
               "DEPTH_BIN","DIVER")
survey_site<-Aggregate_InputTable(awd, SURVEY_INFO)




#Calculate Proportion of colonies surveyed by species for each diver
awd<-subset(awd,S_ORDER=="Scleractinia")
sp<-ddply(awd, .(REGION,OBS_YEAR,ISLAND,SITE,SITEVISITID,DIVER,S_ORDER,SPCODE,GENUS_CODE),
          summarise,
          ColCount=length(COLONYID)) #change to count
sp$counted<-1

sp.<-ddply(sp, .(REGION,OBS_YEAR,ISLAND,SITE,SITEVISITID,S_ORDER,SPCODE,GENUS_CODE),
              summarise,
              divercount=length(counted)) #change to count

ndiver<-ddply(awd, .(REGION,OBS_YEAR,ISLAND,SITE,SITEVISITID),
          summarise,
          ndiver=length(unique(DIVER))) 

adf<-merge(sp.,ndiver,by=c("REGION","OBS_YEAR","ISLAND","SITE","SITEVISITID"),all=TRUE)
adf$prop.observed<-adf$divercount/adf$ndiver
adf$Adult_juv<-"Adult"


#Calculate Proportion of colonies surveyed by species for each diver JUVENILE
jwd<-subset(jwd,S_ORDER=="Scleractinia")
sp<-ddply(jwd, .(REGION,OBS_YEAR,ISLAND,SITE,SITEVISITID,DIVER,S_ORDER,SPCODE,GENUS_CODE),
          summarise,
          ColCount=length(COLONYID)) #change to count
sp$counted<-1

sp.<-ddply(sp, .(REGION,OBS_YEAR,ISLAND,SITE,SITEVISITID,S_ORDER,SPCODE,GENUS_CODE),
           summarise,
           divercount=length(counted)) #change to count

ndiver<-ddply(jwd, .(REGION,OBS_YEAR,ISLAND,SITE,SITEVISITID),
              summarise,
              ndiver=length(unique(DIVER))) 

jdf<-merge(sp.,ndiver,by=c("REGION","OBS_YEAR","ISLAND","SITE","SITEVISITID"),all=TRUE)
jdf$prop.observed<-jdf$divercount/jdf$ndiver
jdf$Adult_juv<-"Juv"

diver<-rbind(adf,jdf)

#create a look up table for species genus codes
SURVEY_INFO<-c("SPCODE","GENUS_CODE")
sp.genlist<-Aggregate_InputTable(diver, SURVEY_INFO)

#Read in colony density from other scripts
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
a1<-read.csv("Benthic13_17sitedata.csv",stringsAsFactors = FALSE)
a1<-a1[,c("REGION_NAME","ANALYSIS_YEAR","ISLAND","SITE","SITEVISITID",
          "SPCODE","AdColDen","JuvColDen")]

#remove rows that have NAs
a1<-a1[complete.cases(a1),]


a2<-read.csv("Benthic REABenthic18sitedata.csv",stringsAsFactors = FALSE);a2<-subset(a2,select= -c(X,TRANSECT,AdColCount,JuvColCount))
colnames(a1)[1]<-"REGION"
colnames(a1)[2]<-"OBS_YEAR"
colnames(a2)[7]<-"REGION"
a1 <- gather(a1, Adult_juv, ColDen, AdColDen:JuvColDen, factor_key=TRUE) #convert wide to long format
a1$Adult_juv<-gsub("AdColDen","Adult",a1$Adult_juv);a1$Adult_juv<-gsub("JuvColDen","Juv",a1$Adult_juv)

a2 <- gather(a2, Adult_juv, ColDen, AdColDen:JuvColDen, factor_key=TRUE) #convert wide to long format
a2$Adult_juv<-gsub("AdColDen","Adult",a2$Adult_juv);a2$Adult_juv<-gsub("JuvColDen","Juv",a2$Adult_juv)


cd.all<-rbind(a1,a2)
cd.all<-subset(cd.all,SPCODE!="SSSS")
cd.all<-merge(cd.all,sp.genlist,by="SPCODE",all.x=TRUE)

cd.site<-ddply(cd.all,.(REGION,OBS_YEAR,ISLAND,SITE,SITEVISITID,Adult_juv,SPCODE,GENUS_CODE),
            summarise,
          ColDen=mean(ColDen))

#You will have NAs for the proportion observed. Leave NAs in
all.data<-merge(cd.site,diver,by=c("REGION","OBS_YEAR","ISLAND","SITE","SITEVISITID","SPCODE","GENUS_CODE","Adult_juv"),all=TRUE) # change this to all.x=TRUE

#Combining years
all.data$OBS_YEAR<-ifelse(all.data$REGION=="American Samoa" & all.data$OBS_YEAR=="2015","2015_2016",all.data$OBS_YEAR)
all.data$OBS_YEAR<-ifelse(all.data$REGION=="Pacific Remote Island Areas" & all.data$OBS_YEAR %in% c("2017","2018"),"2017_2018",all.data$OBS_YEAR)

subreg<-read.csv("Island_Region_Reference.csv")
all.data<-merge(all.data,subreg, by=c("REGION","ISLAND"),all.x=TRUE)

all.<-ddply(all.data,.(REGION,SUBREGION_NAME,OBS_YEAR,SPCODE,GENUS_CODE,Adult_juv),
            summarise,
            prop.observed=mean(prop.observed,na.rm=TRUE),
            ColDen=mean(ColDen,na.rm=TRUE))

all.final<-subset(all.,ColDen>0)
all.final<-all.final[complete.cases(all.final),]

write.csv(all.final,file="T:/Benthic/Data/Benthic_taxa comparison_sitev2.csv")


#Subset Adult data
ad.cd<-subset(cd.all,Adult_juv=="Adult")
ad.cd0<-subset(ad.cd,ColDen>0&REGION=="American Samoa")
head(ad.cd0)
as.wide<-dcast(ad.cd0, formula=OBS_YEAR +ISLAND+SITEVISITID +SITE ~ SPCODE, value.var="ColDen",fill=0)
head(as.wide)



#read in data
new_sp<-as.wide[5:ncol(as.wide)] #extract community matrix
head(as.wide)
env <-as.wide[c(1,4)]

#PERMANOVA with zero-adjusted Bray Curtis dissimilarity metric
#adonis(new_sp ~ Year, data=env, method="bray",permutations=9999)


# calculate distance for NMDS
sol <- metaMDS(new_sp)

# Create meta data for grouping
MyMeta <-as.wide[c(1,2)]
MyMeta$OBS_YEAR<-as.factor(MyMeta$OBS_YEAR)


#Combine metadata and nmds points
NMDS <- data.frame(MDS1 = sol$points[,1], MDS2 = sol$points[,2],group=MyMeta$OBS_YEAR)


NMDS.mean=aggregate(NMDS[,1:2],list(group=NMDS$group),mean)
plot(sol$points, col = MyMeta$OBS_YEAR)

ord<-ordiellipse(sol, MyMeta$OBS_YEAR, display = "sites", 
                 kind = "se", conf = 0.95, label = T)


veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}




##Separate linetypes for Region
df_ell <- data.frame()
if(is.null(ord[[g]]))
  for(g in levels(NMDS$group))  {
    df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$group==g,],
                                                     veganCovEllipse(ord[[g]]$cov,ord[[g]]$center,ord[[g]]$scale)))
                                  ,group=g))
  }
df_ell2<-cSplit(df_ell, splitCols = "group", sep = " ", direction = "wide", drop = FALSE)
df_ell<-cbind(df_ell,df_ell2$group_1)
colnames(df_ell)<-c("MDS1","MDS2","group","ISLAND")


ggplot(data = NMDS, aes(x=MDS1, y=MDS2)) + geom_point(aes(color = group,pch=ISLAND)) +
  geom_path(data=df_ell, aes(x=MDS1, y=MDS2,colour=group,linetype=ISLAND), size=0.75)+
  annotate("text",x=NMDS.mean$MDS1,y=NMDS.mean$MDS2,label=NMDS.mean$group,family="Times",fontface = "bold",size=3)+
  theme_bw()+theme(legend.key.height=unit(0.5,"line"),legend.margin=margin(t = 0, unit='cm'),legend.key=element_blank(),legend.title=element_blank(),panel.grid=element_blank())+
  theme(legend.spacing.y= unit(0.1, "cm"),legend.position=c(0.1,0.85),legend.text = element_text(family="Times",size = 6, face = "bold"))+
  theme(axis.title.x = element_text(family="Times",face = "bold", color = "black", size =10), 
        axis.title.y = element_text(family="Times",face = "bold", color = "black", size = 10),
        axis.text.y = element_text(family="Times",color = "black", size = 10),
        axis.text.x = element_text(family="Times",color = "black", size = 10))+
  theme(axis.ticks.length=unit(-0.25, "cm"), axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")))+
  theme(axis.title.y=element_text(margin=margin(0,-5,0,0)))+
  theme(axis.title.x=element_text(margin=margin(-5,0,0,0)))+
  scale_color_manual(breaks=c("MID 2002","MID 2004","MID 2014"),
                     labels=c("2002", "2004","2014"),
                     values=c('darkorange','cyan3','mediumpurple3','darkorange','cyan3','mediumpurple3'))
ggsave(file="CommunityNMDS_names.pdf", width=3.14, height=2.89)


