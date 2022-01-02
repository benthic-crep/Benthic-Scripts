#read in GENUSCODE and TAXONCODE strata summary tables from REA Coral Demographic_CalcPooled.R script
# This script combines the adult taxoncode and juvenile genus level data & adds names for regions, islands, sectors, taxon



rm(list=ls())

#LOAD LIBRARY FUNCTIONS ...
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")


#Read in data
tax<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicREA_stratadata_TAXONCODE.csv")
gen<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicREA_stratadata_GENUS.csv")

#Read in look up tables
seclu<-read.csv("T:/Benthic/Data/Lookup Tables/SectorNamelookup.csv")
taxlu<-read.csv("T:/Benthic/Data/Lookup Tables/2013-20_Taxa_MASTER.csv")


data.cols<-c("REGION","ISLAND","SECTOR","ANALYSIS_YEAR","DB_RZ","TAXONCODE","n","AdColDen","SE_AdColDen",
             "AcuteDZ", "SE_AcuteDZ","ChronicDZ", "SE_ChronicDZ","BLE","SE_BLE")

tax.<-tax %>% dplyr::select(data.cols)

#Remove special sectors- need to fix this in the original code
levels(as.factor(tax.$SECTOR))
  
