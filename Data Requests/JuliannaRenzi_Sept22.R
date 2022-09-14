#This script tweaks the site-level data from our normal benthic summary scripts for a data request
#Data requested by: Julianna Renzi jrenzi@umail.ucsb.edu
#9/13/22


rm(list=ls())

#Set Run Flags
DEBUG=TRUE

#LOAD LIBRARY FUNCTIONS ...
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

## LOAD benthic data
demo<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS.csv")
cover1<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2020_Tier1_SITE.csv")
cover3<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2020_Tier3_SITE.csv")


demo2<-subset(demo, select = c(REGION_NAME,ISLAND,SITE,DATE_,REEF_ZONE,DEPTH_BIN, LATITUDE, LONGITUDE,MIN_DEPTH_M,MAX_DEPTH_M,GENUS_CODE,
                               AdColDen,TotDZ,AcuteDZ,ChronicDZ,ALGA,TotDZ_prev,AcuteDZ_prev,ChronicDZ_prev,AlgalOG_prev))
              
colnames(demo2)[colnames(demo2)=="TotDZ"]<-"TotDZ_Denity" #Change column name
colnames(demo2)[colnames(demo2)=="AcuteDZ"]<-"AcuteDZ_Density" #Change column name
colnames(demo2)[colnames(demo2)=="ChronicDZ"]<-"ChronicDZ_Density" #Change column name
colnames(demo2)[colnames(demo2)=="ALGA"]<-"AlgalOG_Density" #Change column name


colnames(cover1)[colnames(cover1)=="new_MIN_DEPTH_M"]<-"MIN_DEPTH_M" #Change column name
colnames(cover3)[colnames(cover3)=="new_MIN_DEPTH_M"]<-"MIN_DEPTH_M" #Change column name
colnames(cover1)[colnames(cover1)=="new_MAX_DEPTH_M"]<-"MAX_DEPTH_M" #Change column name
colnames(cover3)[colnames(cover3)=="new_MAX_DEPTH_M"]<-"MAX_DEPTH_M" #Change column name


#remove permanent sites, climate sites and special projects from COVER data
cover1$PERM_SITE[is.na(cover1$PERM_SITE)]<-"0"
cover1$TRANSECT_PHOTOS[is.na(cover1$TRANSECT_PHOTOS)]<-"0"
cover1<-cover1[!cover1$MISSIONID%in% c("MP1410","MP1512","MP1602","MP2006","FAGAALU1","FAGAALU2"),] #I left SE1602 in (2016 Jarvis and Rose)
cover1<-subset(cover1,is.na(OCC_SITEID)& EXCLUDE_FLAG!="-1"& PERM_SITE!=-1 & Oceanography !=1)#Remove occ sites,sites with exclude flag =-1 and permanent sites

cover3$PERM_SITE[is.na(cover3$PERM_SITE)]<-"0"
cover3$TRANSECT_PHOTOS[is.na(cover3$TRANSECT_PHOTOS)]<-"0"
cover3<-cover3[!cover3$MISSIONID%in% c("MP1410","MP1512","MP1602","MP2006","FAGAALU1","FAGAALU2"),] #I left SE1602 in (2016 Jarvis and Rose)
cover3<-subset(cover3,is.na(OCC_SITEID)& EXCLUDE_FLAG!="-1"& PERM_SITE!=-1 & Oceanography !=1)


#Subset cover columns
cover1<-subset(cover1, select = c(REGION,ISLAND,SITE,DATE_,REEF_ZONE,DEPTH_BIN, LATITUDE, LONGITUDE,MIN_DEPTH_M,MAX_DEPTH_M,CCA:TURF))
cover3<-subset(cover3, select = c(REGION,ISLAND,SITE,DATE_,REEF_ZONE,DEPTH_BIN, LATITUDE, LONGITUDE,MIN_DEPTH_M,MAX_DEPTH_M,ACAS:ZO))

write.csv(demo2, file="T:/Benthic/Data/Data Requests/JuliannaRenzi/PacificNCRMP_CoralDemographicHealth_2013-2019_SITE.csv",row.names = F)
write.csv(cover1, file="T:/Benthic/Data/Data Requests/JuliannaRenzi/PacificNCRMP_Tier1Cover_2013-2019_SITE.csv",row.names = F)
write.csv(cover3, file="T:/Benthic/Data/Data Requests/JuliannaRenzi/PacificNCRMP_Tier3Cover_2013-2019_SITE.csv",row.names = F)


