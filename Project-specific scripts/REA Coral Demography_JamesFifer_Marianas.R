#Prepare our site level taxoncode demographic data from the Marianas for James Fifer james.e.fifer@gmail.com 
#James is a PhD student from Boston University

rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
df<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_TAXONCODE.csv")

#Add TAXONCODE NAME
taxa<-read.csv("T:/Benthic/Data/SpGen_Reference/CoralTaxa_lookup.csv")
nrow(df)
df<-merge(df,taxa,by="TAXONCODE",all.x=TRUE)
nrow(df)
DATA_COLS<-c("REGION","OBS_YEAR","ISLAND","SITEVISITID","SITE","REEF_ZONE",
             "DEPTH_BIN",	"DATE_","LATITUDE",	"LONGITUDE",	"SITE_MIN_DEPTH",	"SITE_MAX_DEPTH",
             "TAXONCODE","TAXONNAME","AdColDen","BLE_prev","Adpres.abs")
head(df[,DATA_COLS])
df<-df[,DATA_COLS]

colnames(df)[colnames(df)=="AdColDen"]<-"Adult_Colony_Density_m2" 
colnames(df)[colnames(df)=="BLE_prev"]<-"Adult_Bleaching_Prevalence" 
colnames(df)[colnames(df)=="Adpres.abs"]<-"Taxon_Presence" 

df<-subset(df,REGION=="MARIAN")

write.csv(df,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Project-specific scripts/NOAA_Marianas_JamesFifer.csv")


