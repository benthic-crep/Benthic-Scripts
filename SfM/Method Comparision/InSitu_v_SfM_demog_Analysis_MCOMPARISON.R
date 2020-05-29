#This script reads in the diver and SfM-generated demographic data that has been QC'd and cleaned up
#Then generates segment-level summarized that for methods comparision

rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
library(gridExtra)
library(reshape2)
library(plyr)
#install.packages("ggpmisc")
library(hydroGOF)
library(tidyverse)
library(ggpmisc)
library(lme4)
source("T:/Benthic/Data/SfM/ScriptFiles/SfMvDiver Plotting Functions.R") 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")

setwd("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision")

#Read in files
sfm_site<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMGENUS_SITE.csv")
sfm_str<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMGENUS_STRATA.csv")
sfm_sec<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMGENUS_SECTOR.csv")

diver_site<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_DiverGENUS_SITE.csv")
diver_str<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_DiverGENUS_STRATA.csv")
diver_sec<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_DiverGENUS_SECTOR.csv")

#Select columns to keep in site data
sfm_site<-dplyr::select(sfm_site, c(SITE,SITEVISITID,GENUS_CODE,TRANSECTAREA_ad,TRANSECTAREA_j,AdColCount,AdColDen,JuvColDen,Ave.size,Ave.od,Ave.rd,
                                    BLE_prev,AcuteDZ_prev,ChronicDZ_prev,METHOD,ISLAND,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,
                                    MIN_DEPTH_M,MAX_DEPTH_M))

diver_site<-dplyr::select(diver_site, c(SITE,SITEVISITID,GENUS_CODE,TRANSECTAREA_ad,TRANSECTAREA_j,AdColCount,AdColDen,JuvColDen,Ave.size,Ave.od,Ave.rd,
                                    BLE_prev,AcuteDZ_prev,ChronicDZ_prev,METHOD,ISLAND,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,
                                    MIN_DEPTH_M,MAX_DEPTH_M)) 

#Combine diver and sfm data in long format
all.site<-rbind(sfm_site,diver_site)
all.str<-rbind(sfm_str,diver_str)
all.sec<-rbind(sfm_sec,diver_sec)

#Set up in wide format
colnames(sfm_site)[6:14] <- paste("SfM_", colnames(sfm_site[,c(6:14)]), sep = "");sfm_site<-dplyr::select(sfm_site,-(METHOD))
                                                                                                                     
colnames(sfm_str)[10:25] <- paste("SfM_", colnames(sfm_str[,c(10:25)]), sep = "")
colnames(sfm_sec)[8:23] <- paste("SfM_", colnames(sfm_sec[,c(8:23)]), sep = "")

colnames(diver_site)[6:14] <- paste("Diver_", colnames(diver_site[,c(6:14)]), sep = "");diver_site<-dplyr::select(diver_site,-c(METHOD,ISLAND,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,MIN_DEPTH_M,MAX_DEPTH_M))
colnames(diver_str)[10:25] <- paste("Diver_", colnames(diver_str[,c(10:25)]), sep = "")
colnames(diver_sec)[8:23] <- paste("Diver_", colnames(diver_sec[,c(8:23)]), sep = "")

site.wide<-merge(sfm_site,diver_site,by=c("SITE","SITEVISITID","GENUS_CODE","TRANSECTAREA_ad","TRANSECTAREA_j"),all=T)

View(subset(site.wide,SITE=="NII-02594")) #Make sure columns merge properly
View(subset(diver_site,SITE=="NII-02594")) #Make sure columns merge properly

#View(site.wide)

#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
site.wide$Diver_JuvColDen[is.na(site.wide$Diver_JuvColDen)]<-0
site.wide$Diver_AdColDen[is.na(site.wide$Diver_AdColDen)]<-0
site.wide$SfM_JuvColDen[is.na(site.wide$SfM_JuvColDen)]<-0
site.wide$SfM_AdColDen[is.na(site.wide$SfM_AdColDen)]<-0

head(site.wide)

head(subset(site.wide,GENUS_CODE=="SSSS"))


# Plotting Regressions and Bland-Altman by Taxon --------------------------
#PlotAll(dataframe, variable 1, variable 2, y-axis name 1, y-axis name 2, x-axis name 1, x-axis name 2)

outpath<- "T:/Benthic/Data/SfM/Method Comparision/Figures"
if(!dir.exists(outpath)){dir.create(outpath)}
p1<-PlotAll(site.wide,"Diver_AdColDen","SfM_AdColDen","SfM Adult Density","Difference SfM Analyst and Diver", "Diver Adult Density","Mean Adult Density"); p1
p1

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Juvenile Density"
if(!dir.exists(outpath)){dir.create(outpath)}
p4<-PlotAll(site.wide,"Diver_JuvColDen","SfM_JuvColDen","SfM Juvenile Density","Difference SfM Analyst and Diver", "Diver Juvenile Density","Mean Juvenile Density"); p4

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Colony Size"
if(!dir.exists(outpath)){dir.create(outpath)}
p7<-PlotAll(site.wide,"Diver_Ave.size","SfM_Ave.size","SfM Colony Length","Difference SfM Analyst and Diver", "Diver Colony Length","Mean Colony Length")

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Old Dead"
if(!dir.exists(outpath)){dir.create(outpath)}
p10<-PlotAll(site.wide,"Diver_Ave.od","SfM_Ave.od","SfM Average Old Dead Pct","Difference SfM Analyst and Diver", "Diver Average Old Dead Pct","Average Old Dead Pct")

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Recent Dead"
if(!dir.exists(outpath)){dir.create(outpath)}
p13<-PlotAll(site.wide,"Diver_Ave.rd","SfM_Ave.rd","SfM Average Recent Dead Pct","Difference SfM Analyst and Diver", "Diver Average Recent Dead Pct","Average Recent Dead Pct")

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Bleaching"
if(!dir.exists(outpath)){dir.create(outpath)}
p16<-PlotAll(site.wide,"Diver_BLE_prev","SfM_BLE_prev","SfM Bleaching Prevalence","Difference SfM Analyst and Diver", "Diver Bleaching Prevalence","Mean Bleaching Prevalence")

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/ChronicDZ"
if(!dir.exists(outpath)){dir.create(outpath)}
p19<-PlotAll(site.wide,"Diver_ChronicDZ_prev","SfM_ChronicDZ_prev","SfM Chronic Disease Prevalence","Difference SfM Analyst and Diver", "Diver Chronic Disease Prevalence","Mean Chronic Disease Prevalence")

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/AcuteDZ"
if(!dir.exists(outpath)){dir.create(outpath)}
p22<-PlotAll(site.wide,"Diver_AcuteDZ_prev","SfM_AcuteDZ_prev","SfM General Disease Prevalence","Difference SfM Analyst and Diver", "Diver General Disease Prevalence","Mean General Disease Prevalence")


#Mixed Models
s<-subset(all.site,GENUS_CODE=="SSSS")
hist(s$AdColDen)


