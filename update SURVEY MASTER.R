
#CREATE ADULT CLEAN ANALYSIS READY DATA----------------------------------------
# This script will clean the raw benthic REA data using method E that comes directly from the new data base application.
#Note- these data represent the revised data structure insituted in November 2018. Several recent dead and condition columns were added
rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data")
site_data<-read.csv("SURVEY MASTER.csv") #from oracle
site22<-read.csv("SURVEY MASTER_2022only.csv")
View(site22)

# site_data$bANALYSIS_SCHEME<-ifelse(site_data$OBS_YEAR=="2013" & site_data$Oceanography!="1"& site_data$MISSIONID %in% c("HA1304","HA1305","SB1320"),"BEN_SEC2013",as.character(site_data$bANALYSIS_SCHEME))
# site_data$bANALYSIS_SCHEME<-ifelse(site_data$OBS_YEAR=="2014"& site_data$Oceanography!="1"&site_data$MISSIONID%in% c("HA1401","HA1404"),"BEN_SEC2014",as.character(site_data$bANALYSIS_SCHEME))
# site_data$bANALYSIS_SCHEME<-ifelse(site_data$OBS_YEAR=="2015"& site_data$Oceanography!="1"&site_data$MISSIONID%in% c("HA1501","HA1503","HA1505"),"BEN_SEC2015",as.character(site_data$bANALYSIS_SCHEME))
# site_data$bANALYSIS_SCHEME<-ifelse(site_data$OBS_YEAR=="2016"& site_data$Oceanography!="1"&site_data$MISSIONID %in%c("SE1602","HA1616"),"BEN_SEC2016",as.character(site_data$bANALYSIS_SCHEME))
# site_data$bANALYSIS_SCHEME<-ifelse(site_data$OBS_YEAR=="2017"& site_data$Oceanography!="1"&site_data$MISSIONID%in% c("HA1701","HA1704"),"BEN_SEC2017",as.character(site_data$bANALYSIS_SCHEME))
# site_data$bANALYSIS_SCHEME<-ifelse(site_data$OBS_YEAR=="2018"& site_data$Oceanography!="1"&site_data$MISSIONID =="HA1801","BEN_SEC2018",as.character(site_data$bANALYSIS_SCHEME))
# site_data$bANALYSIS_SCHEME<-ifelse(is.na(site_data$bANALYSIS_SCHEME),"NO SCHEME",as.character(site_data$bANALYSIS_SCHEME))

nrow(site_data)

site22<-site22 %>% mutate(TYPE= dplyr::recode(TYPE,
                                                   `Oceanography`="FIXED",     
                                                   `Benthic`="RANDOM",
                                                   `Fish`="RANDOM"))
View(site22)

#Combine historical and 2022 data
df<-rbind(site_data,site22)

write.csv(df,"SURVEY MASTER.csv")
