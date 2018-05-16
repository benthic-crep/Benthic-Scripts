#Summarizing Method A 2002-2007, 2010 and 2011 Dataset-------

################
#####
rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")

## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
load("ALL_REA_METHODA_RAW.Rdata")
df$SITE<-SiteNumLeadingZeros(df$SITE) # Change site number such as MAR-22 to MAR-0022
a<-df

# HOUSEKEEPING ------------------------------------------------------------
DATA_COLS<-c("REGION","ISLANDCODE","DEPTH_BIN","OBS_YEAR","SITE","TRANSECT","TRANWIDTH","TRANLENGTH","TAXONCODE","SIZECLASS","COUNT","S_ORDER")
head(a[,DATA_COLS])
a<-a[,DATA_COLS]

head(a)

a$TRANAREA<-a$TRANLENGTH*a$TRANWIDTH
nrow(a)
a<-subset(a,TRANAREA>=5)
nrow(a)
head(a)

###Add new column for adult vs. juv
###Create new column for island group
a$SIZEGROUP<-NA
a$SIZEGROUP<-as.character(a$SIZEGROUP)
for (i in 1:length(a$SIZECLASS)){ #opening brace
  
  if(a$SIZECLASS[i] =="0-5 cm"){ #c&p
    a[i,40] = "JUVENILE" #c&p
  } #c&p
  if(a$SIZECLASS[i] !="0-5 cm"){ #c&p
    a[i,40] = "ADULT" #c&p
  } #c&p
} #closing curly brace for entire forloop

head(a)
levels(as.factor(a$SIZEGROUP))

methoda<-a

#Summarizing Method B  Dataset-------

################

## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
load("ALL_REA_METHODB_RAW.Rdata")
df$SITE<-SiteNumLeadingZeros(df$SITE) # Change site number such as MAR-22 to MAR-0022
a<-df

# HOUSEKEEPING ------------------------------------------------------------
DATA_COLS<-c("REGION","ISLANDCODE","DEPTH_BIN","OBS_YEAR","SITE","MINDEPTH","MAXDEPTH","TRANSECT","TRANWIDTH","TRANLENGTH","SPECIES","SIZECLASS","COUNT","S_ORDER")
head(a[,DATA_COLS])
a<-a[,DATA_COLS]

a$TRANAREA<-a$TRANLENGTH*a$TRANWIDTH
nrow(a)
a<-subset(a,TRANAREA>=5)
nrow(a)
head(a)

###Add new column for adult vs. juv
###Create new column for island group
a$SIZEGROUP<-NA
a$SIZEGROUP<-as.character(a$SIZEGROUP)
for (i in 1:length(a$SIZECLASS)){ #opening brace
  
  if(a$SIZECLASS[i] =="0-5 cm"){ #c&p
    a[i,16] = "JUVENILE" #c&p
  } #c&p
  if(a$SIZECLASS[i] !="0-5 cm"){ #c&p
    a[i,16] = "ADULT" #c&p
  } #c&p
} #closing curly brace for entire forloop

head(a)
levels(as.factor(a$SIZEGROUP))

methodb<-a


# Summarizing dataset C- 2007-2013 ----------------------------------------

## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
load("ALL_REA_METHODC_RAW.Rdata");x<-df
x$SITE<-SiteNumLeadingZeros(x$SITE) # Change site number such as MAR-22 to MAR-0022


# HOUSEKEEPING ------------------------------------------------------------

#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","ISLAND","ISLANDCODE","SITEVISITID","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","METHODNAME","MINDEPTH","MAXDEPTH","DIVER","TRANSECT","SEGMENT","SEGWIDTH",
             "SEGLENGTH","TAXONCODE","TAXONNAME","COLONYLENGTH","COLONYWIDTH","OLDDEAD",
             "RECENTDEAD","COND","COND_DESCRIPTION",
             "EXTENT",	"SEVERITY","GENUS","S_ORDER")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

#Double check level and class of variables to make sure there aren't any errors
sapply(x,levels)
sapply(x,class)


colnames(x)[colnames(x)=="TRANWIDTH"]<-"SEGWIDTH" #Change column name
colnames(x)[colnames(x)=="TRANLENGTH"]<-"SEGLENGTH" #Change column name
colnames(x)[colnames(x)=="SPECIES"]<-"SPCODE" #Change column name
olnames(x)[colnames(x)=="MIN_DEPTH"]<-"SITE_MIN_DEPTH_FT" #Change column name
colnames(x)[colnames(x)=="MAX_DEPTH"]<-"SITE_MAX_DEPTH_FT" #Change column name
colnames(x)[colnames(x)=="MAP_CODE"]<-"COND" #Change column name
colnames(x)[colnames(x)=="DZCODE"]<-"RD1" #Change column name


head(x)

#Removing specfic colonies and segments
x<-subset(x,SEGLENGTH!="NA") #Remove segments that were not surveyed for coral demography


##Calcuating segment and transect area and add column for transect area
x<-Transectarea(x)
sapply(x,levels)
head(x)
nrow(x)

#Changing NAs to unknown
x[is.na(x)]<-"UNKONWN"

#Remove transects with less than 5m surveyed and check how many rows were removed
x<-subset(x,TRANAREA>=5) 
nrow(x)
head(x)
levels(x$OBS_YEAR)


methodc<-x