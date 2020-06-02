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
seg<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_GENUS_SEGMENT.csv")

#site<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMGENUS_SITE.csv")


#Select columns to keep in segment data
seg<-dplyr::select(seg, c(METHOD,SITE,SITEVISITID,SEGMENT,GENUS_CODE,ANALYST,SEGAREA_ad,SEGAREA_j,AdColCount,AdColDen,JuvColDen,Ave.size,Ave.od,Ave.rd,
                                    BLE_prev,AcuteDZ_prev,ChronicDZ_prev,ISLAND,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,HABITAT_CODE,
                                    MIN_DEPTH_M,MAX_DEPTH_M))

# #Select columns to keep in site data
# site<-dplyr::select(site, c(METHOD,SITE,SITEVISITID,GENUS_CODE,TRANSECTAREA_ad,TRANSECTAREA_j,AdColCount,AdColDen,JuvColDen,Ave.size,Ave.od,Ave.rd,
#                                     BLE_prev,AcuteDZ_prev,ChronicDZ_prev,ISLAND,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,
#                                     MIN_DEPTH_M,MAX_DEPTH_M))

sfm_seg<-subset(seg,METHOD=="SfM")
diver_seg<-subset(seg,METHOD=="Diver")


#Set up in wide format
colnames(sfm_seg)[9:17] <- paste("SfM_", colnames(sfm_seg[,c(9:17)]), sep = "");sfm_seg<-dplyr::select(sfm_seg,-c(METHOD,ANALYST,SEGAREA_ad,SEGAREA_j))
                                                                                                                     
colnames(diver_seg)[9:17] <- paste("Diver_", colnames(diver_seg[,c(9:17)]), sep = "");diver_seg<-dplyr::select(diver_seg,-c(METHOD,ANALYST,ISLAND,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,MIN_DEPTH_M,MAX_DEPTH_M,SEGAREA_ad,SEGAREA_j))

seg.wide<-merge(sfm_seg,diver_seg,by=c("SITE","SITEVISITID","SEGMENT","GENUS_CODE"),all=T)

View(subset(seg.wide,SITE=="NII-02594")) #Make sure columns merge properly
View(subset(diver_site,SITE=="NII-02594")) #Make sure columns merge properly

#View(seg.wide)

# #Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
seg.wide$Diver_JuvColDen[is.na(seg.wide$Diver_JuvColDen)]<-0
seg.wide$Diver_AdColDen[is.na(seg.wide$Diver_AdColDen)]<-0
seg.wide$SfM_JuvColDen[is.na(seg.wide$SfM_JuvColDen)]<-0
seg.wide$SfM_AdColDen[is.na(seg.wide$SfM_AdColDen)]<-0

head(seg.wide)

head(subset(seg.wide,GENUS_CODE=="SSSS"))


# Plotting Regressions and Bland-Altman by Taxon --------------------------
#PlotAll(dataframe, variable 1, variable 2, y-axis name 1, y-axis name 2, x-axis name 1, x-axis name 2)

outpath<- "T:/Benthic/Data/SfM/Method Comparision/Figures"
if(!dir.exists(outpath)){dir.create(outpath)}
p1<-PlotAll(seg.wide,"Diver_AdColDen","SfM_AdColDen","SfM Adult Density","Difference SfM Analyst and Diver", "Diver Adult Density","Mean Adult Density"); p1
p1

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Juvenile Density"
if(!dir.exists(outpath)){dir.create(outpath)}
p4<-PlotAll(seg.wide,"Diver_JuvColDen","SfM_JuvColDen","SfM Juvenile Density","Difference SfM Analyst and Diver", "Diver Juvenile Density","Mean Juvenile Density"); p4

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Colony Size"
if(!dir.exists(outpath)){dir.create(outpath)}
p7<-PlotAll(seg.wide,"Diver_Ave.size","SfM_Ave.size","SfM Colony Length","Difference SfM Analyst and Diver", "Diver Colony Length","Mean Colony Length")

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Old Dead"
if(!dir.exists(outpath)){dir.create(outpath)}
p10<-PlotAll(seg.wide,"Diver_Ave.od","SfM_Ave.od","SfM Average Old Dead Pct","Difference SfM Analyst and Diver", "Diver Average Old Dead Pct","Average Old Dead Pct")

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Recent Dead"
if(!dir.exists(outpath)){dir.create(outpath)}
p13<-PlotAll(seg.wide,"Diver_Ave.rd","SfM_Ave.rd","SfM Average Recent Dead Pct","Difference SfM Analyst and Diver", "Diver Average Recent Dead Pct","Average Recent Dead Pct")

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Bleaching"
if(!dir.exists(outpath)){dir.create(outpath)}
p16<-PlotAll(seg.wide,"Diver_BLE_prev","SfM_BLE_prev","SfM Bleaching Prevalence","Difference SfM Analyst and Diver", "Diver Bleaching Prevalence","Mean Bleaching Prevalence")

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/ChronicDZ"
if(!dir.exists(outpath)){dir.create(outpath)}
p19<-PlotAll(seg.wide,"Diver_ChronicDZ_prev","SfM_ChronicDZ_prev","SfM Chronic Disease Prevalence","Difference SfM Analyst and Diver", "Diver Chronic Disease Prevalence","Mean Chronic Disease Prevalence")

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/AcuteDZ"
if(!dir.exists(outpath)){dir.create(outpath)}
p22<-PlotAll(seg.wide,"Diver_AcuteDZ_prev","SfM_AcuteDZ_prev","SfM General Disease Prevalence","Difference SfM Analyst and Diver", "Diver General Disease Prevalence","Mean General Disease Prevalence")


#Mixed Models
s<-subset(seg,GENUS_CODE=="SSSS")
hist(s$AdColDen)
nrow(s)
nrow(subset(s,AdColCount==0))

table(s$HABITAT_CODE)

#Simplify Habitat codes
s<-s %>% mutate(HAB_R1=recode(HABITAT_CODE, 
                                                  `AGR`="Carbonate Reef",
                                                  `APR`="Carbonate Reef",
                                                  `PAV`="Pavement",
                                                  `PPR`="Pavement",
                                                  `RRB`="Rubble",
                                                  `ROB`="Rock & Boulder",
                                                  `SCR`="Reef with Sand",
                                                  `PSC`="Reef with Sand"))

nullmod<-glmer(AdColCount~1 + (1|SEC_NAME/SITE)+ offset(SEGAREA_ad),family=poisson,data=s)
mod1<-glmer(AdColCount~METHOD + (1|SEC_NAME/SITE)+ offset(SEGAREA_ad),family=poisson,data=s)
mod2<-glmer(AdColCount~METHOD + ANALYST+ (1|SEC_NAME/SITE)+ offset(SEGAREA_ad),family=poisson,data=s)
mod3<-glmer(AdColCount~ANALYST+ (1|SEC_NAME/SITE)+ offset(SEGAREA_ad),family=poisson,data=s)

fullmod<-glmer(AdColCount~METHOD*MAX_DEPTH_M + (1|SEC_NAME/SITE)+ offset=SEGAREA_ad,family=poisson,data=s)
fullmod<-glmer(AdColCount~METHOD*HABITAT_CODE + (1|SEC_NAME/SITE)+ offset=SEGAREA_ad,family=poisson,data=s)
fullmod<-glmer(AdColCount~METHOD*RugosityBin + (1|SEC_NAME/SITE)+ offset=SEGAREA_ad,family=poisson,data=s)

anova(mod1,nullmod,test="chisq")
anova(mod2,nullmod,test="chisq")
anova(mod2,mod1,test="chisq")
anova(mod2,mod3,test="chisq")
