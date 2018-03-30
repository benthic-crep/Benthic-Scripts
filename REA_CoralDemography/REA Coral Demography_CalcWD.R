rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")


#LOAD THE CLEAN wd 
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
load("TMPBenthicREA_Adultwd_v2.Rdata")

load("TMPBenthicREA_Adultwd_V2.Rdata")
awd<-wd
load("TMPBenthicREA_Juvwd.Rdata")
jwd<-wd


#base information about the survey - field names should match those in input file (obviously!)
UNIQUE_SURVEY<-c("SITEVISITID","SITE")
UNIQUE_TRANSECT<-c(UNIQUE_SURVEY, "TRANSECT")
UNIQUE_COLONY<-c(UNIQUE_TRANSECT, "COLONYID")


#Generate a table of metadata at the transect and colony level for ADULTS- ADD SECTOR NAME, ANALYSIS YEAR EVENTUALLY
SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT","TRANSECT")
survey_transect<-Aggregate_InputTable(awd, SURVEY_INFO)

SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT","TRANSECT","COLONYID","GENUS_CODE","TAXONCODE","S_ORDER")
survey_colony<-Aggregate_InputTable(awd, SURVEY_INFO)

save(survey_transect, file="TMPsurveys_Transect.Rdata")
save(survey_colony, file="TMPsurveys_Colony.Rdata")


#Generate a table of metadata at the transect and colony level for JUVENILES- ADD SECTOR NAME, ANALYSIS YEAR EVENTUALLY
SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT","TRANSECT")
jsurvey_transect<-Aggregate_InputTable(jwd, SURVEY_INFO)

SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT","TRANSECT","COLONYID","GENUS_CODE","TAXONCODE","S_ORDER")
jsurvey_colony<-Aggregate_InputTable(jwd, SURVEY_INFO)

save(survey_transect, file="TMPsurveys_Transect.Rdata")
save(survey_colony, file="TMPsurveys_Colony.Rdata")

#Pull all species information into a separate df, for possible later use ..
TAXON_MORPH_FIELDS<-c("TAXONCODE","GENUS_CODE", "S_ORDER", "TAXMORPH", "GENMORPH","MORPH_CODE")
taxon_morph_table<-Aggregate_InputTable(awd, TAXON_MORPH_FIELDS)
save(taxon_morph_table, file="TMPtaxonmorph.Rdata")


#Remove scleractinan colony fragments, but include include sites with no colonies (AAAA) and other cnidarians
#if AAAA or other cnidarians are excluded here then the transect area may not calculated properly.
awd<-subset(awd, COLONYLENGTH>5&S_ORDER=="Scleractinia"|COLONYLENGTH<0|SPCODE=="AAAA")
tail(wd)#make sure that AAAA's are included

#Remove transects with less than 5m surveyed and check how many rows were removed
nrow(awd)
awd<-subset(awd,TRANSECTAREA>=5) 
nrow(awd)
head(awd)


##Change Transects 3 and 4 in the juvenile data to 1 and 2 so we can merge with adult data
jwd$TRANSECT[jwd$TRANSECT == "3"] <- "1"
jwd$TRANSECT[jwd$TRANSECT == "4"] <- "2"


#test functions on MHI data to make code run faster
awd2<-subset(awd,ISLAND=="Hawaii")
jwd2<-subset(jwd,ISLAND=="Hawaii")

# # GENERATE SUMMARY METRICS at the transect-level -this is just a start, more metrics to come --------------------------------------------------
# acd.gen<-Calc_ColDen_Transect(awd2,"GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="Colabun"]<-"AdColabun";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen"# calculate density at genus level as well as total
# od.gen<-Calc_Dead_Sev_Ext_Transect(awd2,"GENUS_CODE","OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od"
# rd.gen<-Calc_Dead_Sev_Ext_Transect(awd2,"GENUS_CODE",c("RDEXTENT1", "RDEXTENT2")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd"
# rdabun.gen<-Calc_RDabun_Transect(awd2,"GENUS_CODE")
# condabun.gen<-Calc_Condabun_Transect(awd2,"GENUS_CODE")
# jcd.gen<-Calc_ColDen_Transect(jwd2,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="Colabun"]<-"JuvColabun";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen"
# 
# 
# acd.tax<-Calc_ColDen_Transect(awd2,"TAXONCODE");colnames(acd.tax)[colnames(acd.tax)=="Colabun"]<-"AdColabun";colnames(acd.tax)[colnames(acd.tax)=="ColDen"]<-"AdColDen"# calculate density at genus level as well as total
# od.tax<-Calc_Dead_Sev_Ext_Transect(awd2,"TAXONCODE","OLDDEAD"); colnames(od.tax)[colnames(od.tax)=="Ave.y"]<-"Ave.od"
# rd.tax<-Calc_Dead_Sev_Ext_Transect(awd2,"TAXONCODE",c("RDEXTENT1", "RDEXTENT2")); colnames(rd.tax)[colnames(rd.tax)=="Ave.y"]<-"Ave.rd"
# rdabun.tax<-Calc_RDabun_Transect(awd2,"TAXONCODE")
# condabun.tax<-Calc_Condabun_Transect(awd2,"TAXONCODE")
# jcd.tax<-Calc_ColDen_Transect(jwd2,"TAXONCODE"); colnames(jcd.tax)[colnames(jcd.tax)=="Colabun"]<-"JuvColabun";colnames(jcd.tax)[colnames(jcd.tax)=="ColDen"]<-"JuvColDen"
# 
# #Merge density and partial moratlity data together. R
# MyMerge <- function(x, y){
#   df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","GENUS_CODE"), all.x= TRUE, all.y= TRUE)
#   return(df)
# }
# data.gen<-Reduce(MyMerge, list(acd.gen,od.gen,rd.gen,jcd.gen));
# 
# #Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
# data.gen$JuvColabun[is.na(data.gen$JuvColabun)]<-0;data.gen$JuvColDen[is.na(data.gen$JuvColDen)]<-0
# data.gen$AdColabun[is.na(data.gen$AdColabun)]<-0;data.gen$AdColDen[is.na(data.gen$AdColDen)]<-0
# 
# MyMerge <- function(x, y){
#   df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","TAXONCODE"), all.x= TRUE, all.y= TRUE)
#   return(df)
# }
# data.tax<-Reduce(MyMerge, list(acd.tax,od.tax,rd.tax,jcd.tax))
# data.tax$JuvColabun[is.na(data.tax$JuvColabun)]<-0;data.tax$JuvColDen[is.na(data.tax$JuvColDen)]<-0
# data.tax$AdColabun[is.na(data.tax$AdColabun)]<-0;data.tax$AdColDen[is.na(data.tax$AdColDen)]<-0



# GENERATE SUMMARY METRICS at the SITE-level -this is just a start, more metrics to come --------------------------------------------------
acd.gen<-Calc_ColDen_Site(awd2,"GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="Colabun"]<-"AdColabun";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen"# calculate density at genus level as well as total
od.gen<-Calc_Dead_Sev_Ext_Site(awd2,"GENUS_CODE","OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od"
rd.gen<-Calc_Dead_Sev_Ext_Site(awd2,"GENUS_CODE",c("RDEXTENT1", "RDEXTENT2")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd"
#rdabun.gen<-Calc_RDabun_Site(awd2,"GENUS_CODE")
#condabun.gen<-Calc_Condabun_Site(awd2,"GENUS_CODE")
jcd.gen<-Calc_ColDen_Site(jwd2,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="Colabun"]<-"JuvColabun";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen"


acd.tax<-Calc_ColDen_Site(awd2,"TAXONCODE");colnames(acd.tax)[colnames(acd.tax)=="Colabun"]<-"AdColabun";colnames(acd.tax)[colnames(acd.tax)=="ColDen"]<-"AdColDen"# calculate density at genus level as well as total
od.tax<-Calc_Dead_Sev_Ext_Site(awd2,"TAXONCODE","OLDDEAD"); colnames(od.tax)[colnames(od.tax)=="Ave.y"]<-"Ave.od"
rd.tax<-Calc_Dead_Sev_Ext_Site(awd2,"TAXONCODE",c("RDEXTENT1", "RDEXTENT2")); colnames(rd.tax)[colnames(rd.tax)=="Ave.y"]<-"Ave.rd"
#rdabun.tax<-Calc_RDabun_Site(awd2,"TAXONCODE")
#condabun.tax<-Calc_Condabun_Site(awd2,"TAXONCODE")
jcd.tax<-Calc_ColDen_Site(jwd2,"TAXONCODE"); colnames(jcd.tax)[colnames(jcd.tax)=="Colabun"]<-"JuvColabun";colnames(jcd.tax)[colnames(jcd.tax)=="ColDen"]<-"JuvColDen"

#Merge density and partial moratlity data together. R
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","GENUS_CODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.gen<-Reduce(MyMerge, list(acd.gen,od.gen,rd.gen,jcd.gen));

#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.gen$JuvColabun[is.na(data.gen$JuvColabun)]<-0;data.gen$JuvColDen[is.na(data.gen$JuvColDen)]<-0
data.gen$AdColabun[is.na(data.gen$AdColabun)]<-0;data.gen$AdColDen[is.na(data.gen$AdColDen)]<-0

MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TAXONCODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.tax<-Reduce(MyMerge, list(acd.tax,od.tax,rd.tax,jcd.tax))
data.tax$JuvColabun[is.na(data.tax$JuvColabun)]<-0;data.tax$JuvColDen[is.na(data.tax$JuvColDen)]<-0
data.tax$AdColabun[is.na(data.tax$AdColabun)]<-0;data.tax$AdColDen[is.na(data.tax$AdColDen)]<-0








##Unweighted Summaries for Benthic Summary Reports
# GENERATE SUMMARY METRICS at the transect-level -this is just a start, more metrics to come --------------------------------------------------
rd<-merge(m1,m4, by=c("SITE","SITEVISITID","TRANSECT","GENUS_CODE"),all.x=TRUE)
con<-merge(m1,m5, by=c("SITE","SITEVISITID","TRANSECT","GENUS_CODE"),all.x=TRUE) # THIS CODE GENERATES NAS IN THE RDCOND COLUMN FOR TAXA THAT AREN'T PRESENT ON TRANSECT. SPEAK WITH IVOR
rd.<-ifelse(rd$Colabun==0& rd$RDCond=="NA",rd$RDabun=="NA",ifelse(rd$Colabun>0 & rd$RDabun=="NA",rd$RDabun==0,rd$RDabun==rd$RDabun))####this isn't working
con$Condabun[is.na(con$Condabun)]<-0
rd$RDabun[is.na(rd$RDabun)]<-0

head(con);head(rd)
rd$rdprev<-rd$RDabun/rd$Colabun*100 #calculate prevalence
con$condprev<-con$Condabun/con$Colabun*100 #calculate prevalence

AcuteDZ<-subset(rd,RDCond=="All_DZGN");colnames(AcuteDZ)[colnames(AcuteDZ)=="RDabun"]<-"AcuteDZabun";colnames(AcuteDZ)[colnames(AcuteDZ)=="rdprev"]<-"AcuteDZprev" #Change column name
ChronicDZ<-subset(con,COND=="ChronicDZ");colnames(ChronicDZ)[colnames(ChronicDZ)=="Condabun"]<-"ChronicDZabun";colnames(ChronicDZ)[colnames(ChronicDZ)=="condprev"]<-"ChronicDZprev" #Change column name
COTS<-subset(rd,RDCond=="COTS");colnames(COTS)[colnames(COTS)=="RDabun"]<-"COTSabun";colnames(COTS)[colnames(COTS)=="rdprev"]<-"COTSprev" #Change column name
BLE<-subset(con,COND=="BLE");colnames(BLE)[colnames(BLE)=="Condabun"]<-"BLEabun";colnames(BLE)[colnames(BLE)=="condprev"]<-"BLEprev" #Change column name
head(ChronicDZ)


a1<-merge(AcuteDZ,ChronicDZ, by=c("DEPTH_BIN","REEF_ZONE","SITE","SITEVISITID","TRANSECT","GENUS_CODE","Colabun","ColDen"),all=TRUE) # THIS CODE GENERATES NAS IN THE RDCOND COLUMN FOR TAXA THAT AREN'T PRESENT ON TRANSECT. SPEAK WITH IVOR
a2<-merge(a1,BLE, by=c("DEPTH_BIN","REEF_ZONE","SITE","SITEVISITID","TRANSECT","GENUS_CODE","Colabun","ColDen"),all=TRUE) # THIS CODE GENERATES NAS IN THE RDCOND COLUMN FOR TAXA THAT AREN'T PRESENT ON TRANSECT. SPEAK WITH IVOR
a3<-merge(a2,COTS, by=c("DEPTH_BIN","REEF_ZONE","SITE","SITEVISITID","TRANSECT","GENUS_CODE","Colabun","ColDen"),all=TRUE) # THIS CODE GENERATES NAS IN THE RDCOND COLUMN FOR TAXA THAT AREN'T PRESENT ON TRANSECT. SPEAK WITH IVOR
head(a3)

a3$TotDZabun<-a3$AcuteDZabun+a3$ChronicDZabun
a3$TotDZprev<-a3$TotDZabun/a3$Colabun
colnames(a3)[colnames(a3)=="Colabun"]<-"AdultColabun"
colnames(a3)[colnames(a3)=="ColDen"]<-"AdultColDen"


a3<- subset(a3, select=-c(RDCond.x,RDCond.y,COND.x,COND.y)) #remove extra columns
a4<-merge(a3,m11,by =c("DEPTH_BIN","REEF_ZONE","SITE","SITEVISITID","TRANSECT","GENUS_CODE"),all=TRUE) #merge adult data with juv data
a4[is.na(a4)]<-0 
head(a4)

