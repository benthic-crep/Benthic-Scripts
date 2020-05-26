#This script reads in the diver and SfM-generated demographic data that has been QC'd and cleaned up
#Then generates segment-level summarized that for methods comparision

rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

#Read in files
sfm_site<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMGENUS_SITE.csv")
sfm_str<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMGENUS_STRATA.csv")
sfm_sec<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMGENUS_SECTOR.csv")

diver_site<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_DiverGENUS_SITE.csv")
diver_str<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_DiverGENUS_STRATA.csv")
diver_sec<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_DiverGENUS_SECTOR.csv")

#Select columns to keep in site data
sfm_site<-dplyr::select(sfm_site, c(SITE,SITEVISITID,GENUS_CODE,AdColCount,AdColDen,JuvColDen,Ave.size,Ave.od,Ave.rd,
                                    BLE_prev,AcuteDZ_prev,ChronicDZ_prev,METHOD,ISLAND,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,
                                    MIN_DEPTH_M,MAX_DEPTH_M))

diver_site<-dplyr::select(diver_site, c(SITE,SITEVISITID,GENUS_CODE,AdColCount,AdColDen,JuvColDen,Ave.size,Ave.od,Ave.rd,
                                    BLE_prev,AcuteDZ_prev,ChronicDZ_prev,METHOD,ISLAND,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,
                                    MIN_DEPTH_M,MAX_DEPTH_M))

#Combine diver and sfm data in long format
all.site<-rbind(sfm_site,diver_site)
all.str<-rbind(sfm_str,diver_str)
all.sec<-rbind(sfm_sec,diver_sec)

colnames(sfm_site)[4:12] <- paste("SfM_", colnames(sfm_site[,c(4:12)]), sep = "");sfm_site<-dplyr::select(sfm_site,-(METHOD))
colnames(sfm_str)[10:25] <- paste("SfM_", colnames(sfm_str[,c(10:25)]), sep = "")
colnames(sfm_sec)[8:23] <- paste("SfM_", colnames(sfm_sec[,c(8:23)]), sep = "")

colnames(diver_site)[4:12] <- paste("Diver_", colnames(diver_site[,c(4:12)]), sep = "");diver_site<-dplyr::select(diver_site,-(METHOD))
colnames(diver_str)[10:25] <- paste("Diver_", colnames(diver_str[,c(10:25)]), sep = "")
colnames(diver_sec)[8:23] <- paste("Diver_", colnames(diver_sec[,c(8:23)]), sep = "")

site.wide<-full_join(sfm_site,diver_site);head(site.wide)
head(site.wide)
#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
site.wide$Diver_JuvColDen[is.na(site.wide$Diver_JuvColDen)]<-0
site.wide$Diver_AdColDen[is.na(site.wide$Diver_AdColDen)]<-0
site.wide$SfM_JuvColDen[is.na(site.wide$SfM_JuvColDen)]<-0
site.wide$SfM_AdColDen[is.na(site.wide$SfM_AdColDen)]<-0

head(site.wide)

head(subset(site.wide,GENUS_CODE=="SSSS"))

