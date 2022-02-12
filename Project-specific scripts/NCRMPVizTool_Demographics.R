#read in GENUSCODE and TAXONCODE strata summary tables from REA Coral Demographic_CalcPooled.R script
# This script combines the adult taxoncode and juvenile genus level data & adds names for regions, islands, sectors, taxon



rm(list=ls())

#LOAD LIBRARY FUNCTIONS ...
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")


#Read in data
gen<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicREA_stratadata_GENUS.csv")

#Read in look up tables
seclu<-read.csv("T:/Benthic/Data/Lookup Tables/SectorNamelookup.csv")
taxlu<-read.csv("T:/Benthic/Data/Lookup Tables/2013-20_Taxa_MASTER.csv")

seclu<-unique(seclu[,c("REGIONNAME","REGION","ISLAND","ISLANDCODE","SECTORNAME","SECTOR")])


#Prep genus-level data
data.cols<-c("REGION","ISLAND","SECTOR","ANALYSIS_YEAR","DB_RZ","GENUS_CODE","n","AdColDen","SE_AdColDen","JuvColDen","SE_JuvColDen",
             "AcuteDZ", "SE_AcuteDZ","ChronicDZ", "SE_ChronicDZ","BLE","SE_BLE")

gen<-gen %>% dplyr::select(data.cols)
gen$SPCODE<-gen$GENUS_CODE


#merge with taxa look up table and remove taxa that aren't found in a given region
taxlu$sry<-paste(taxlu$SPCODE,taxlu$REGION,taxlu$OBS_YEAR,sep="_")
gen$sry<-paste(gen$SPCODE,gen$REGION,gen$ANALYSIS_YEAR,sep="_")

s<-filter(gen,GENUS_CODE=="SSSS")

df<-gen %>%
  dplyr::filter(sry %in% taxlu$sry)

df<-rbind(df,s)

head(gen)

df<-left_join(df,seclu)

df<-left_join(df,taxlu)
head(df)

df<-df[,c("REGIONNAME","REGION","ISLAND","ISLANDCODE", "SECTORNAME","SECTOR","DB_RZ","ANALYSIS_YEAR","n","GENUS_CODE","TAXON_NAME","AdColDen","SE_AdColDen","JuvColDen","SE_JuvColDen","AcuteDZ", "SE_AcuteDZ","ChronicDZ", "SE_ChronicDZ","BLE","SE_BLE")]

df$REGIONNAME<-ifelse(df$SECTOR=="AGS","Mariana Archipelago",df$REGIONNAME)
df$ISLAND<-ifelse(df$SECTOR=="AGS","Alamagan, Guguan, Sarigan",df$ISLAND)
df$ISLANDCODE<-ifelse(df$SECTOR=="AGS","AGS",df$ISLANDCODE)
df$SECTORNAME<-ifelse(df$SECTOR=="AGS","Alamagan, Guguan, Sarigan",df$SECTORNAME)

#Change Stata names to match AOI Naming Convention
df<-df %>% mutate(STRATAcode=recode(DB_RZ,
                                           `FD`="FRF_D",
                                           `FM`="FRF_M",
                                           `FS`="FRF_S",
                                           `BD`="BRF_D",
                                           `BM`="BRF_M",
                                           `BS`="BRF_S",
                                           `LD`="LAG_D",
                                           `LM`="LAG_M",
                                           `LS`="LAG_S",
                                           `PD`="PRS_D",
                                           `PM`="PRS_M",
                                           `PS`="PRS_S"))
                                    



#Change Column names to match AOI Naming convention 
colnames(df)[colnames(df)=="REGIONNAME"]<-"JURISDICTIONname" #subset just acute diseased colonies
colnames(df)[colnames(df)=="REGION"]<-"JURISDICTIONcode" #subset just acute diseased colonies
colnames(df)[colnames(df)=="ISLAND"]<-"SUBREGIONname" #subset just acute diseased colonies
colnames(df)[colnames(df)=="ISLANDCODE"]<-"SUBREGIONcode" #subset just acute diseased colonies
colnames(df)[colnames(df)=="SECTOR"]<-"SECTORcode" #subset just acute diseased colonies
colnames(df)[colnames(df)=="SECTORNAME"]<-"SECTORname" #subset just acute diseased colonies
colnames(df)[colnames(df)=="STRATANAME"]<-"STRATAname" #subset just acute diseased colonies
colnames(df)[colnames(df)=="STRATACODE"]<-"STRATAcode" #subset just acute diseased colonies


write.csv(df,file="T:/Benthic/Data/Data Requests/NCRMPviztool_genus_demographics.csv",row.names = F)

