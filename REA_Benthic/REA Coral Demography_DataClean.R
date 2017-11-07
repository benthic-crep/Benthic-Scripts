# Summarizing dataset E- 2013 and 2016 ----------------------------------------

################
#####
rm(list=ls())
#Load raw data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/ReportCard")#set directory
a <- read.csv("Data/HistoricalREA_V0_CORAL_OBS_E.csv")


library(ggplot2)
require(gridExtra)
library(reshape2)
library(plyr)
library(grid)
library(data.table) 
library(tidyr)


###FUNCTIONS

# HOUSEKEEPING ------------------------------------------------------------
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

x<-subset(x,NO_SURVEY_YN!=-1) #What is the difference between 0 and NA, if no diff then change NA's to 0 and subset -1
x<-subset(x,SEGLENGTH!="NA") #Remove segments that were not surveyed for coral dem

colnames(x)[colnames(x)=="TAXONCODE"]<-"SPCODE" #Change column name

#Create new colummns that combine species, genus and morphology
x$SPMORPH<-paste(x$SPCODE,x$MORPH_CODE,sep="")
x$GENMORPH<-paste(x$GENUS_CODE,x$MORPH_CODE,sep="")

#Remove colony fragments
x<-subset(x, COLONYLENGTH>5)


head(x)
tail(x)
sapply(x,levels)
summary(x)

###Create new column for island group
x$ISLANDGROUP<-NA
x$ISLANDGROUP<-as.character(x$ISLANDGROUP)
for (i in 1:length(x$ISLANDCODE)){ #opening brace
  
  if(x$ISLANDCODE[i] =="HAW"){ #c&p
    x[i,c("ISLANDGROUP")] = "HAW" #c&p
  } #c&p
  if(x$ISLANDCODE[i] =="OAH"){ #c&p
    x[i,c("ISLANDGROUP")] = "OAH" #c&p
  } #c&p
  if(x$ISLANDCODE[i] =="MOL"|x$ISLANDCODE[i] =="MAI"|x$ISLANDCODE[i] =="LAN"){ #c&p
    x[i,c("ISLANDGROUP")] = "MAUINUI" #c&p
  } #c&p
  if(x$ISLANDCODE[i] =="KAU"|x$ISLANDCODE[i] =="NII"){ #c&p
    x[i,c("ISLANDGROUP")] = "KAUNII" #c&p
  } #c&p
  
} #closing curly brace for entire forloop

levels(as.factor(x$ISLANDGROUP))

x<-subset(x,ISLANDGROUP!="NA")



#add SITE MASTER information to x 
x<-merge(x, site_master[,c("SITE", "SEC_NAME", "ANALYSIS_YEAR", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE)  #..  should actually pick up ANALYSIS_SEC from the sectors file.







###Add new column COUNT (This will allow us to include sites that didn't have any colonies)
ncol(a)
a$COUNT<-NA
a$COUNT<-as.character(a$COUNT)
for (i in 1:length(a$S_ORDER)){ #opening brace
  
  if(a$S_ORDER[i] =="Scleractinia"){ #c&p
    a[i,c("COUNT")] = "1" #c&p
  } #c&p
  if(a$S_ORDER[i] !="Scleractinia"){ #c&p
    a[i,c("COUNT")] = "0" #c&p
  } #c&p
} #closing curly brace for entire forloop

head(a)
levels(as.numeric(a$COUNT))


a$SEGAREA<-a$SEGLENGTH*a$SEGWIDTH # Calculate segment area

#Calculate total transect area then merge back to a dataframe
S.df<-ddply(a, .(REGION,ISLANDCODE,OBS_YEAR,SITE,TRANSECT,SEGMENT),
            summarise,
            SEGAREA=unique(SEGAREA))
TR.DF<-ddply(S.df, .(REGION,ISLANDCODE,OBS_YEAR,SITE,TRANSECT),
             summarise,
             TRANAREA=sum(SEGAREA))

new.df<-merge(a,TR.DF, by=c("REGION","ISLANDCODE","OBS_YEAR","SITE","TRANSECT"),all=TRUE)
sapply(new.df,levels)
head(new.df)
nrow(new.df)

new.df<-subset(new.df,TRANAREA>=5) #Remove transects with less than 5m surveyed and check how many rows were removed
nrow(new.df)
head(new.df)
levels(new.df$OBS_YEAR)
