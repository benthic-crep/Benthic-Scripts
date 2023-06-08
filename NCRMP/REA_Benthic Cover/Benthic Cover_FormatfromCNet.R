#Typically CoralNet data is downloaded from CoralNet then sent to the Data services team to upload to Oracle then merge with various look up tables
#However, there may be times when you want to download data from CoralNet and immediately summarize the data before sending it the data team
#This script will help you format data so it can be integrated into the larger dataset
#Note- the sites will need to included in the SURVEY MASTER file

rm(list=ls())

setwd("T:/Benthic/Data/REA Coral Demography & Cover/Raw Data from CoralNet")

library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below
library(splitstackshape)
library(plyr)
library(dplyr)
library(tidyr)

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/fish_team_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/Islandwide Mean&Variance Functions.R")

#read in Cnet codes/names look up table and change column names to match larger dataset
lu<-read.csv("T:/Benthic/Data/Lookup Tables/All_Photoquad_codes.csv")
lu$CATEGORY_CODE<-lu$TIER_1
lu$SUBCATEGORY_CODE<-lu$TIER_2
lu$GENERA_CODE<-lu$TIER_3
lu$CODE<-lu$TIER_3

lu$CATEGORY_NAME<-lu$T1_DESC
lu$SUBCATEGORY_NAME<-lu$T2_DESC
lu$GENERA_NAME<-lu$T3_DESC
colnames(lu)[colnames(lu)=="Cnet_SHORT_CODE"]<-"SHORT_CODE"

#read in data directly from Cnet
# f15<-read.csv("2015_NWHI_CnetAnnotations_fish.csv")
# b17<-read.csv("2017_NWHI_CnetAnnotations_benthic.csv")
#laysan<-read.csv("2015_2017_NWHI_CnetAnnotations_Laysan.csv")
# nwhi<-read.csv("2014_2017_NWHI_CnetAnnotations.csv")
swa<-read.csv("2023_Swains_CnetAnnotations.csv")

#tmp<-rbind(f15,b17)
tmp<-swa

new.cov<-tmp
head(new.cov)

#Format columns
names(new.cov)<-toupper(names(new.cov));head(new.cov)

colnames(new.cov)[colnames(new.cov)=="LABEL"]<-"SHORT_CODE"
colnames(new.cov)[colnames(new.cov)=="DATE"]<-"DATE_TAKEN"
colnames(new.cov)[colnames(new.cov)=="DATE.ANNOTATED"]<-"DATE_ANNOTATED"
colnames(new.cov)[colnames(new.cov)=="ROW"]<-"ROW_"
colnames(new.cov)[colnames(new.cov)=="COLUMN"]<-"COL"
colnames(new.cov)[colnames(new.cov)=="NAME"]<-"ORIGINAL_FILE_NAME"

View(new.cov)


#Convert dates to date format
new.cov$DATE_TAKEN<-lubridate::mdy(new.cov$DATE_TAKEN);head(new.cov$DATE_TAKEN)
new.cov$DATE_ANNOTATED<-lubridate::ymd_hms(new.cov$DATE_ANNOTATED);head(new.cov$DATE_ANNOTATED)
head(new.cov)


#Convert date column into OBS_DAY, Month and Year
new.cov<-new.cov %>%
  dplyr::mutate(OBS_YEAR = lubridate::year(DATE_TAKEN), 
                OBS_MONTH = lubridate::month(DATE_TAKEN), 
                OBS_DAY = lubridate::day(DATE_TAKEN))


#Extract replicate and image number from file name
ri<-new.cov %>%
    separate(ORIGINAL_FILE_NAME,c("ISL","S_NUM","YEAR","REPLICATE","IMAGE_NUMBER","IMG_TYPE")) %>%
    dplyr::select(REPLICATE,IMAGE_NUMBER)
head(ri)

new.cov<-cbind(new.cov,ri)

#Add zeros to SITE 
new.cov$SITE<- as.factor(new.cov$SITE)
new.cov$SITE<-SiteNumLeadingZeros(new.cov$SITE) # Change site number such as MAR-22 to MAR-0022

#Join with Cnet code look up table
new.cov<-left_join(new.cov,lu)
head(new.cov)


#Join with survey master
#survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")

#Use SM coordinates-some coordinates are wrong in data and need to be updated
colnames(survey_master)[colnames(survey_master)=="LATITUDE_LOV"]<-"LATITUDE" #Change column name- we will eventually change this column back to "taxoncode" after we modify the spcode names to match the taxalist we all feel comfortable identifying
colnames(survey_master)[colnames(survey_master)=="LONGITUDE_LOV"]<-"LONGITUDE" #Change column name- we will eventually change this column back to "taxoncode" after we modify the spcode names to match the taxalist we all feel comfortable identifying
colnames(survey_master)[colnames(survey_master)=="new_MIN_DEPTH_M"]<-"MIN_DEPTH" #Change column name- we will eventually change this column back to "taxoncode" after we modify the spcode names to match the taxalist we all feel comfortable identifying
colnames(survey_master)[colnames(survey_master)=="new_MAX_DEPTH_M"]<-"MAX_DEPTH" #Change column name- we will eventually change this column back to "taxoncode" after we modify the spcode names to match the taxalist we all feel comfortable identifying


#Identify column names you will need to merge with larger CNET dataframe
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_BIA_STR_CNET.rdata") #load data
c.name<-colnames(cnet)
code.lu<-unique(cnet[,c("REGION_NAME","ISLANDCODE","REEF_ZONE_CODE","DEPTH_CODE","REGION","ISLAND","REEF_ZONE","DEPTH_BIN")]) #These columns don't exisit in the survey master or data downloaded from Cnet and will need to be added to the dataframe before merging with the larger dataset
cn.lu<-unique(cnet[,c("NAME","SHORT_CODE")]) #These columns don't exisit in the survey master or data downloaded from Cnet and will need to be added to the dataframe before merging with the larger dataset


df<-new.cov %>%
  left_join(survey_master) %>%
  left_join(code.lu) %>%
  left_join(cn.lu)
df$FUNCTIONAL_GROUP<-NA #Functional group is wierd- it can have multiple categories for a single code (e.g. CCAH can be both Algae and CCA)- ignoring

df<-df %>% dplyr::select(c.name)#only include columns in the larger cnet dataset

head(df)
nrow(new.cov);nrow(df)

#Review data
View(df)
lapply(df, summary)
table(df$OBS_YEAR,df$ISLAND)

df$REGION_NAME<-"Northwestern Hawaiian Islands"

df<-df %>% dplyr::filter(OBS_YEAR!=2016)#already in the CoralNet data from Oracle

write.csv(df, file="T:/Benthic/Data/REA Coral Demography & Cover/Raw Data from CoralNet/2014-2017_NWHI_CnetAnnotations_formatted.csv",row.names = F)

#write.csv(df, file="T:/Benthic/Data/REA Coral Demography & Cover/Raw Data from CoralNet/2015_2017_NWHI_CnetAnnotations_formatted.csv",row.names = F)




