# This script will clean the raw benthic REA data using method E (2013-present) and prepare it for analysis

#Need to eventually develop a script that will pull directly from oracle. For now I've downloaded the flat files from the t drive and aggreated them here.
################
#####
rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions.R")
source("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/core_functions.R")

## LOAD benthic data
# setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Coral Demography")
# file_names <- dir() #where you have your files
# x <- do.call(rbind,lapply(file_names,read.csv))

setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
load("ALL_REA_ADULTCORAL_RAW.rdata") #from oracle
x<-df

x$SITE<-SiteNumLeadingZeros(x$SITE) # Change site number such as MAR-22 to MAR-0022


# get strata and sectors data  
# Eventually use the combined fish and benthic sector area file. The file still has issues so using the benthic only for now
sectors<-read.csv("Benthic_SectorArea_v5.csv", stringsAsFactors=FALSE)


# load site master to merge with sector names later in the script
# See REA Generate Benthic Site Master and Sectors script for more details on how site master file was created.
site_master<-read.csv("Benthic2013-17_SiteMaster_v5.csv");nrow(site_master)
site_master$SITE<-SiteNumLeadingZeros(site_master$SITE)


# HOUSEKEEPING ------------------------------------------------------------

#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","REGION_NAME","ISLAND","ISLANDCODE","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","SITE_MIN_DEPTH","SITE_MAX_DEPTH","SITEVISITID","HABITAT_CODE","DIVER","TRANSECT","SEGMENT","TRANWIDTH",
             "TRANLENGTH","EXCLUDE_FLAG","NO_SURVEY_YN","COLONYID","SPECIES","MORPH_CODE","COLONYLENGTH","OLDDEAD",
            "RECENTDEAD","RECENT_GENERAL_CAUSE_CODE","RECENT_SPECIFIC_CAUSE_CODE",
            "RECENTDEAD_2",	"RECENT_GENERAL_CAUSE_CODE_2","RECENT_SPECIFIC_CAUSE_CODE_2","DZCODE",
            "EXTENT",	"SEVERITY","GENUS_CODE","S_ORDER","TAXONNAME")

head(x[,DATA_COLS])
x<-x[,DATA_COLS]

#Double check level and class of variables to make sure there aren't any errors
sapply(x,levels)
sapply(x,class)##Change column names to make code easier to code
colnames(x)[colnames(x)=="TRANWIDTH"]<-"SEGWIDTH" #Change column name
colnames(x)[colnames(x)=="TRANLENGTH"]<-"SEGLENGTH" #Change column name
colnames(x)[colnames(x)=="SPECIES"]<-"SPCODE" #Change column name
colnames(x)[colnames(x)=="RECENTDEAD"]<-"RDEXTENT1" #Change column name
colnames(x)[colnames(x)=="RECENT_GENERAL_CAUSE_CODE"]<-"GENRD1" #Change column name
colnames(x)[colnames(x)=="RECENT_SPECIFIC_CAUSE_CODE"]<-"RD1" #Change column name
colnames(x)[colnames(x)=="RECENTDEAD_2"]<-"RDEXTENT2" #Change column name
colnames(x)[colnames(x)=="RECENT_GENERAL_CAUSE_CODE_2"]<-"GENRD2" #Change column name
colnames(x)[colnames(x)=="RECENT_SPECIFIC_CAUSE_CODE_2"]<-"RD2" #Change column name
colnames(x)[colnames(x)=="SITE_MIN_DEPTH"]<-"SITE_MIN_DEPTH_FT" #Change column name
colnames(x)[colnames(x)=="SITE_MAX_DEPTH"]<-"SITE_MAX_DEPTH_FT" #Change column name

head(x)
#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
taxa<-read.csv("2013-17_Taxa_MASTER.csv")

nrow(x)

#Convert SPCODE in raw colony data to taxoncode -generates a look up table
x.<-Convert_to_Taxoncode(x)
utils::View(x.) #view data in separate window
#Confirm that no rows were dropped during merge
nrow(x)
nrow(x.)
x<-x.

#Create new colummns that combine species, genus and morphology
x$TAXMORPH<-paste(x$TAXONCODE,x$MORPH_CODE,sep="")
x$GENMORPH<-paste(x$GENUS_CODE,x$MORPH_CODE,sep="")


#add SITE MASTER information to x 
#x<-merge(x, site_master[,c("SITE", "SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE) #Fish team's original code, we may want to create analysis scheme later in the 
nrow(site_master)
x<-merge(x, site_master[,c("OBS_YEAR","SITEVISITID","SITE","SEC_NAME","BENTHIC_SEC_CODE", "PROTECTION_DS","ANALYSIS_YEAR")], by=c("OBS_YEAR","SITEVISITID","SITE"))  
length(unique(x$SITEVISITID)) #double check that sites weren't dropped
head(x)


#Remove specfic colonies and segments
x$NO_SURVEY_YN<-is.na(x$NO_SURVEY_YN)<-0 #Change NAs (blank cells) to 0 CHECK THIS 
x<-subset(x,NO_SURVEY_YN>-1) #Exclude rows -1
x<-subset(x,SEGLENGTH!="NA") #Remove segments that were not surveyed for coral demography


##Calcuating segment and transect area and add column for transect area
x<-Transectarea(x)
sapply(x,levels)
head(x)
nrow(x)


#CHECK THAT all SEC_CODES are present in the site_master file
test<-x[is.na(x$BENTHIC_SEC_CODE), c("REGION", "SITE","OBS_YEAR"),]
if(dim(test)[1]>0) {cat("sites with MISSING SECTORS present")}   # should be 0

#If there are missing sectors, generate a table of missing sites, lat, long, reef zone and depth bins. Manually correct Site Master file
a<-subset(x,is.na(x$BENTHIC_SEC_CODE))
a<-a[c("OBS_YEAR","SITE","LATITUDE", "LONGITUDE","REEF_ZONE","DEPTH_BIN")]
test<-unique(a)
write.csv(test,"missingsectors.csv")




#######################
## CLEAN UP NAs #######
#######################
x[x=="."]<-NA
x[x=="-9"]<-NA
tmp.lev<-levels(x$GENRD1); head(tmp.lev)
levels(x$GENRD1)<-c(tmp.lev, "NODATA")
x[is.na(x$GENRD1),"GENRD1"]<-"NODATA"

tmp.lev<-levels(x$RD1); head(tmp.lev)
levels(x$RD1)<-c(tmp.lev, "NODATA")
x[is.na(x$RD1),"RD1"]<-"NODATA"

tmp.lev<-levels(x$GENRD2); head(tmp.lev)
levels(x$GENRD2)<-c(tmp.lev, "NODATA")
x[is.na(x$GENRD2),"GENRD2"]<-"NODATA"

tmp.lev<-levels(x$RD2); head(tmp.lev)
levels(x$RD2)<-c(tmp.lev, "NODATA")
x[is.na(x$RD2),"RD2"]<-"NODATA"

tmp.lev<-levels(x$SITE_MIN_DEPTH_FT); head(tmp.lev)
levels(x$SITE_MIN_DEPTH_FT)<-c(tmp.lev, "NODATA")
x[is.na(x$SITE_MIN_DEPTH_FT),"SITE_MIN_DEPTH_FT"]<-"NODATA"

tmp.lev<-levels(x$SITE_MAX_DEPTH_FT); head(tmp.lev)
levels(x$SITE_MAX_DEPTH_FT)<-c(tmp.lev, "NODATA")
x[is.na(x$SITE_MAX_DEPTH_FT),"SITE_MAX_DEPTH_FT"]<-"NODATA"

tmp.lev<-levels(x$Estab_Yr); head(tmp.lev)
levels(x$Estab_Yr)<-c(tmp.lev, "NODATA")
x[is.na(x$Estab_Yr),"Estab_Yr"]<-"NODATA"

head(x)

####Fixing Errors in data where causes of recent mortality were entered into both the recent cause and the "COND" columns.
#This code converts the 3 condition columns to characters first (won't work as factors) then changes the "COND" column to "NODATA"
#if it is already listed in either of the recent cause columns. It also changes the extent to -9.
x$COND<-as.character(x$COND)
x$RD1<-as.character(x$RD1)
x$RD2<-as.character(x$RD2)

dim(x[(x$RD1==x$COND)|(x$RD2==x$COND),]) #check dimension of df
x[(x$RD1==x$COND)|(x$RD2==x$COND),] #show rows that meet criteria


wd<-droplevels(x)


save(wd, file="TMPBenthicREA_Adultwd_V2.Rdata")  #Save clean working data


# This script will clean the raw juvenile coral REA data using method E (2013-present) and prepare it for analysis

################
#####
rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic_Functions.R")
source("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/core_functions.R")
## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
load("ALL_REA_JUVCORAL_RAW.rdata")
x<-df
x$SITE<-SiteNumLeadingZeros(x$SITE)

# load site master to merge with sector names later in the script
# See REA Generate Benthic Site Master and Sectors script for more details on how site master file was created.
site_master<-read.csv("Benthic2013-17_SiteMaster_v5.csv");nrow(site_master)
site_master$SITE<-SiteNumLeadingZeros(site_master$SITE)


# HOUSEKEEPING ------------------------------------------------------------

#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","REGION_NAME","ISLAND","ISLANDCODE","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","SITE_MIN_DEPTH","SITE_MAX_DEPTH","SITEVISITID","HABITAT_CODE","DIVER","EXCLUDE_FLAG","TRANSECT","SEGMENT","TRANWIDTH",
             "TRANLENGTH","COLONYID","SPECIES","MORPH_CODE","COLONYLENGTH","COLONYWIDTH","GENUS_CODE","S_ORDER")

head(x[,DATA_COLS])
x<-x[,DATA_COLS]

#Double check level and class of variables to make sure there aren't any errors
sapply(x,levels)
sapply(x,class)##Change column names to make code easier to code
colnames(x)[colnames(x)=="SPECIES"]<-"SPCODE" #Change column name
colnames(x)[colnames(x)=="TRANWIDTH"]<-"SEGWIDTH" #Change column name
colnames(x)[colnames(x)=="TRANLENGTH"]<-"SEGLENGTH" #Change column name
colnames(x)[colnames(x)=="SITE_MIN_DEPTH"]<-"SITE_MIN_DEPTH_FT" #Change column name
colnames(x)[colnames(x)=="SITE_MAX_DEPTH"]<-"SITE_MAX_DEPTH_FT" #Change column name


#add SITE MASTER information to x 
#x<-merge(x, site_master[,c("SITE", "SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE) #Fish team's original code, we may want to create analysis scheme later in the 
nrow(site_master)
x<-merge(x, site_master[,c("OBS_YEAR","SITEVISITID","SITE","SEC_NAME","BENTHIC_SEC_CODE", "PROTECTION_DS","ANALYSIS_YEAR")], by=c("OBS_YEAR","SITEVISITID","SITE"), all.y=TRUE)  
length(unique(x$SITEVISITID)) #double check that sites weren't dropped
head(x)


#Remove specfic colonies and segments
x$EXCLUDE_FLAG<-is.na(x$EXCLUDE_FLAG)<-0 #Change NAs (blank cells) to 0
x<-subset(x,EXCLUDE_FLAG>-1);summary(x$EXCLUDE_FLAG) #Exclude rows -1
x<-subset(x,SEGLENGTH!="NA") #Remove segments that were not surveyed for juveniles
nrow(x)


##Calcuating segment and transect area and add column for transect area
x<-Transectarea(x)
sapply(x,levels)
head(x)
nrow(x)

#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
taxa<-read.csv("2013-17_Taxa_MASTER.csv")

#Convert SPCODE in raw colony data to taxoncode -generates a look up table
x.<-Convert_to_Taxoncode(x)
utils::View(x.) #view data in separate window
#Confirm that no rows were dropped during merge
nrow(x)
nrow(x.)
x<-x.
head(x)

#CHECK THAT all SEC_CODES are present in the site_master file
test<-x[is.na(x$BENTHIC_SEC_CODE), c("REGION", "SITE","OBS_YEAR"),]
if(dim(test)[1]>0) {cat("sites with MISSING SECTORS present")}   # should be 0

#If there are missing sectors, generate a table of missing sites, lat, long, reef zone and depth bins. Manually correct Site Master file
a<-subset(x,is.na(x$BENTHIC_SEC_CODE))
a<-a[c("OBS_YEAR","SITE","LATITUDE", "LONGITUDE","REEF_ZONE","DEPTH_BIN")]
test<-unique(a)
write.csv(test,"missingsectors.csv")


#######################
## CLEAN UP NAs #######
#######################

tmp.lev<-levels(x$SITE_MIN_DEPTH_FT); head(tmp.lev)
levels(x$SITE_MIN_DEPTH_FT)<-c(tmp.lev, "NODATA")
x[is.na(x$SITE_MIN_DEPTH_FT),"SITE_MIN_DEPTH_FT"]<-"NODATA"

tmp.lev<-levels(x$SITE_MAX_DEPTH_FT); head(tmp.lev)
levels(x$SITE_MAX_DEPTH_FT)<-c(tmp.lev, "NODATA")
x[is.na(x$SITE_MAX_DEPTH_FT),"SITE_MAX_DEPTH_FT"]<-"NODATA"

head(x)

wd<-droplevels(x)


save(wd, file="TMPBenthicREA_Juvwd.Rdata")  #Save clean working data



