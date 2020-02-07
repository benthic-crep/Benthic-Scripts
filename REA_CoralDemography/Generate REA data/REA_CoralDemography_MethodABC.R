#Summarizing Method A 2002-2007, 2010 and 2011 Dataset-------

################
#####
rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Morgan.Winston/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions.R")
source("C:/Users/Morgan.Winston/Documents/GitHub/fish-paste/lib/core_functions.R")

## LOAD benthic data
setwd("T:/Benthic/Data/REA Coral Demography/Raw from Oracle")
load("ALL_REA_METHODA_RAW.Rdata")
df$SITE<-SiteNumLeadingZeros(df$SITE) # Change site number such as MAR-22 to MAR-0022
a<-df

# HOUSEKEEPING ------------------------------------------------------------
DATA_COLS<-c("REGION","ISLANDCODE","DEPTH_BIN","OBS_YEAR","SITE","TRANSECT","TRANWIDTH","TRANLENGTH","SPECIES","SIZECLASS","COUNT","S_ORDER")
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
    a[i,14] = "JUVENILE" #c&p
  } #c&p
  if(a$SIZECLASS[i] !="0-5 cm"){ #c&p
    a[i,14] = "ADULT" #c&p
  } #c&p
} #closing curly brace for entire forloop

head(a)
levels(as.factor(a$SIZEGROUP))

methoda<-a

#Summarizing Method B  Dataset-------

################

## LOAD benthic data
setwd("T:/Benthic/Data/REA Coral Demography/Raw from Oracle")
load("ALL_REA_METHODB_RAW.Rdata")
df$SITE<-SiteNumLeadingZeros(df$SITE) # Change site number such as MAR-22 to MAR-0022
b <-df

# HOUSEKEEPING ------------------------------------------------------------
DATA_COLS<-c("REGION","ISLANDCODE","DEPTH_BIN","OBS_YEAR","SITE","MINDEPTH","MAXDEPTH","TRANSECT","TRANWIDTH","TRANLENGTH","SPECIES","SIZECLASS","COUNT","S_ORDER")
head(b[,DATA_COLS])
b<-b[,DATA_COLS]

b$TRANAREA<-b$TRANLENGTH*b$TRANWIDTH
nrow(b)
b<-subset(b,TRANAREA>=5)
nrow(b)
head(b)

###Add new column for adult vs. juv
###Create new column for island group
b$SIZEGROUP<-NA
b$SIZEGROUP<-as.character(b$SIZEGROUP)
for (i in 1:length(b$SIZECLASS)){ #opening brace
  
  if(b$SIZECLASS[i] =="0-5 cm"){ #c&p
    b[i,16] = "JUVENILE" #c&p
  } #c&p
  if(b$SIZECLASS[i] !="0-5 cm"){ #c&p
    b[i,16] = "ADULT" #c&p
  } #c&p
} #closing curly brace for entire forloop

head(b)
levels(as.factor(b$SIZEGROUP))

methodb<-b


# Summarizing dataset C- 2007-2013 ----------------------------------------

## LOAD benthic data
setwd("T:/Benthic/Data/REA Coral Demography/Raw from Oracle")
load("ALL_REA_METHODC_RAW.Rdata");c<-df
c$SITE<-SiteNumLeadingZeros(c$SITE) # Change site number such as MAR-22 to MAR-0022


# HOUSEKEEPING ------------------------------------------------------------

#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","ISLAND","ISLANDCODE","SITEVISITID","SITE","LATITUDE","LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","METHODNAME","MINDEPTH","MAXDEPTH","DIVER","TRANSECT","SEGMENT","TRANWIDTH",
             "TRANLENGTH","SPECIES","TAXONNAME","COLONYLENGTH","COLONYWIDTH","OLDDEAD",
             "RECENTDEAD","MAP_CODE","MAP_DESCRIPTION",
             "EXTENT",	"SEVERITY","GENUS","S_ORDER")
head(c[,DATA_COLS])
c<-c[,DATA_COLS]

#Double check level and class of variables to make sure there aren't any errors
sapply(c,levels)
sapply(c,class)

colnames(c)[colnames(c)=="TRANWIDTH"]<-"SEGWIDTH" #Change column name
colnames(c)[colnames(c)=="TRANLENGTH"]<-"SEGLENGTH" #Change column name
colnames(c)[colnames(c)=="MIN_DEPTH"]<-"SITE_MIN_DEPTH_FT" #Change column name
colnames(c)[colnames(c)=="MAX_DEPTH"]<-"SITE_MAX_DEPTH_FT" #Change column name
colnames(c)[colnames(c)=="MAP_DESCRIPTION"]<-"COND_DESCRIPTION" #Change column name
colnames(c)[colnames(c)=="MAP_CODE"]<-"COND" #Change column name
colnames(c)[colnames(c)=="DZCODE"]<-"RD1" #Change column name

head(c)

#Removing specfic colonies and segments
c<-subset(c,SEGLENGTH!="NA") #Remove segments that were not surveyed for coral demography

##Calcuating segment and transect area and add column for transect area
c<-Transectarea(c)
sapply(c,levels)
head(c)
nrow(c)

#Changing NAs to unknown
c[is.na(c)]<-"UNKNOWN"

#Remove transects with less than 5m surveyed and check how many rows were removed
c<-subset(c,TRANSECTAREA>=5) 
nrow(c)
head(c)
levels(c$OBS_YEAR)

methodc<-c
