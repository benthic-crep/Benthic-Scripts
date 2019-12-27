#This script reads in the diver and SfM-generated demographic data that has been QC'd and cleaned up
#Then generates segment-level summarized that for methods comparision

rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")

#Read in files
ad_diver<-read.csv("T:/Benthic/Data/SfM/Analysis Ready/HARAMP19_DIVERAdult_CLEANED.csv")
j_diver<-read.csv("T:/Benthic/Data/SfM/Analysis Ready/HARAMP19_DIVERJuv_CLEANED.csv")
ad_sfm<-read.csv("T:/Benthic/Data/SfM/Analysis Ready/HARAMP19_SfMAdult_CLEANED.csv")
j_sfm<-read.csv("T:/Benthic/Data/SfM/Analysis Ready/HARAMP19_SfMJuv_CLEANED.csv")

#Final Tweaks and merge adult and juv datasets-------------------------------------------------
#Add a column for adult fragments so we can remove them from the dataset later (-1 indicates fragment)
ad_diver$Fragment[is.na(ad_diver$Fragment)] <- 0
j_diver$Fragment <- 0 # you need to add this column so that you can use the site level functions correctly
ad_diver$EX_BOUND<-0 #add column so we can merge with sfm data

ad_diver[,c("METHOD","ANALYST", "REGION","OBS_YEAR","MISSIONID","ISLAND","SEC_NAME","SITEVISITID","SITE","REEF_ZONE","DEPTH_BIN",
            "HABITAT_CODE","LATITUDE","LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","SEGLENGTH","SEGWIDTH",
            "SEGAREA","TRANSECTAREA","NO_COLONY_","COLONYID","Fragment","S_ORDER","GENUS_CODE","SPCODE","TAXONCODE","TAXONNAME",
            "MORPH_CODE","EX_BOUND","COLONYLENGTH","OLDDEAD","GENRD1","GENRD2","GENRD3","RD1","RDEXTENT1","RD2","RDEXTENT2","RD3",
            "RDEXTENT3","CONDITION_1","EXTENT_1","SEVERITY_1","CONDITION_2","EXTENT_2","SEVERITY_2","CONDITION_3","EXTENT_3","SEVERITY_3")]


#Create a look a table of all of the colony attributes- you will need this the functions below
SURVEY_COL<-c("DATE_","SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH","TRANSECT","SEGMENT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony<-unique(awd[,SURVEY_COL])

SURVEY_SITE<-c("DATE_","SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH")
survey_site<-unique(awd[,SURVEY_SITE])

SURVEY_Seg<-c("DATE_","SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN","HABITAT_CODE", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH","METHOD","TRANSECT","SEGMENT")
survey_segment<-unique(awd[,SURVEY_Seg])

#Site/segement table for SfM QC
survey_segment1<-subset(survey_segment,TRANSECT=="1")
write.csv(survey_segment1,file="T:/Benthic/SfM/Materials for SfM benthic QC/HARAMPbenthic_site_seglist.csv")


# GENERATE SUMMARY METRICS at the Segment-leveL BY GENUS--------------------------------------------------
#Calc_ColDen_Transect
acd.gen<-Calc_ColDen_Seg(data = awd,grouping_field = "GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen";colnames(acd.gen)[colnames(acd.gen)=="SEGAREA"]<-"SEGAREA_ad"# calculate density at genus level as well as total
jcd.gen<-Calc_ColDen_Seg(jwd,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen"
jcd.gen<-subset(jcd.gen,select=-c(SEGAREA))

#Calc_ColMetric_Transect
cl.gen<-Calc_ColMetric_Seg(data = awd,grouping_field = "GENUS_CODE",pool_fields = "COLONYLENGTH"); colnames(cl.gen)[colnames(cl.gen)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.gen<-Calc_ColMetric_Seg(data = awd,grouping_field = "GENUS_CODE",pool_fields = "OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.gen<-Calc_ColMetric_Seg(data = awd,grouping_field = "GENUS_CODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead

#Calc_RDden_Transect
rdden.gen<-Calc_RDden_Seg(data=awd,grouping_field ="GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.gen<-subset(rdden.gen,select = c(SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,DZGN_G));colnames(acutedz.gen)[colnames(acutedz.gen)=="DZGN_G"]<-"DZGN_G_den" #subset just acute diseased colonies

#Calc_CONDden_Transect
condden.gen<-Calc_CONDden_Seg(data=awd,grouping_field ="GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.gen<-subset(condden.gen,select = c(SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,BLE));colnames(ble.gen)[colnames(ble.gen)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.gen<-subset(condden.gen,select = c(SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,CHRO));colnames(chronicdz.gen)[colnames(chronicdz.gen)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies

#Calc_Richness_Transect
#rich.gen<-Calc_Richness_Transect(awd,"GENUS_CODE")

#ADD CODE TO CHANGE TRANSECT NUMBERS FOR JUVENILES
jcd.gen$TRANSECT[jcd.gen$TRANSECT==3]<-1
jcd.gen$TRANSECT[jcd.gen$TRANSECT==4]<-2


#Merge density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","SEGMENT","GENUS_CODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.gen<-Reduce(MyMerge, list(acd.gen,jcd.gen,cl.gen,od.gen,rd.gen,acutedz.gen,chronicdz.gen,ble.gen));
head(data.gen)

#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.gen$JuvColCount[is.na(data.gen$JuvColCount)]<-0;data.gen$JuvColDen[is.na(data.gen$JuvColDen)]<-0
data.gen$AdColCount[is.na(data.gen$AdColCount)]<-0;data.gen$AdColDen[is.na(data.gen$AdColDen)]<-0

#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.gen$DZGN_G_prev<-(data.gen$DZGN_G_den*data.gen$SEGAREA_ad)/data.gen$AdColCount*100
data.gen$BLE_prev<-(data.gen$BLE_den*data.gen$SEGAREA_ad)/data.gen$AdColCount*100
data.gen$CHRO_prev<-(data.gen$CHRO_den*data.gen$SEGAREA_ad)/data.gen$AdColCount*100

View(data.gen)

# #Remove data from transects with less than 5m surveyed for adults and 1m for juvs.
# data.gen$TRANSECTAREA_ad<-ifelse(data.gen$TRANSECTAREA_ad<5,NA,data.gen$TRANSECTAREA_ad);data.gen[data.gen$TRANSECTAREA_ad<5,]
# data.gen$TRANSECTAREA_j<-ifelse(data.gen$TRANSECTAREA_j<1,NA,data.gen$TRANSECTAREA_j);data.gen[data.gen$TRANSECTAREA_j<1,]

#GENERATE SITE-LEVEL DATA BY AVERAGING TRANSECTS-----------------------------------
#Since we are moving to a 1 stage design, we need to summarize the transects before rolling up to site. Dione suggested that we calculate mean of 2 transects rather than pooling or dropping a transect

# site.data.gen<-ddply(data.gen, .(SITE,SITEVISITID,GENUS_CODE), #calc total colonies by condition
#                      summarise,
#                      AdColCount=sum(AdColCount,na.rm=T),AdColDen=mean(AdColDen,na.rm = T),Ave.od=mean(Ave.od,na.rm = T),
#                      Ave.rd=mean(Ave.rd,na.rm = T),JuvColDen=mean(JuvColDen,na.rm=T),
#                      BLE=mean(BLE_den,na.rm=T),AcuteDZ=mean(DZGN_G_den,na.rm=T),ChronicDZ=mean(CHRO_den,na.rm=T),
#                      BLE_prev=mean(BLE_prev,na.rm=T),AcuteDZ_prev=mean(DZGN_G_prev,na.rm=T),ChronicDZ_prev=mean(CHRO_prev,na.rm=T))
# 
# #Duplicate dataframe because the ddply step above takes a while to create. Allows you to tweak code below without having to rerun the ddply step above
# site.data.gen2<-site.data.gen



# GENERATE SUMMARY METRICS at the Segment-leveL BY TAXONCODE--------------------------------------------------
#Calc_ColDen_Transect
acd.tax<-Calc_ColDen_Seg(data = awd,grouping_field = "TAXONCODE");colnames(acd.tax)[colnames(acd.tax)=="ColCount"]<-"AdColCount";colnames(acd.tax)[colnames(acd.tax)=="ColDen"]<-"AdColDen";colnames(acd.tax)[colnames(acd.tax)=="SEGAREA"]<-"SEGAREA_ad"# calculate density at genus level as well as total
jcd.tax<-Calc_ColDen_Seg(jwd,"TAXONCODE"); colnames(jcd.tax)[colnames(jcd.tax)=="ColCount"]<-"JuvColCount";colnames(jcd.tax)[colnames(jcd.tax)=="ColDen"]<-"JuvColDen"
jcd.tax<-subset(jcd.tax,select=-c(SEGAREA))

#Calc_ColMetric_Transect
cl.tax<-Calc_ColMetric_Seg(data = awd,grouping_field = "TAXONCODE",pool_fields = "COLONYLENGTH"); colnames(cl.tax)[colnames(cl.tax)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.tax<-Calc_ColMetric_Seg(data = awd,grouping_field = "TAXONCODE",pool_fields = "OLDDEAD"); colnames(od.tax)[colnames(od.tax)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.tax<-Calc_ColMetric_Seg(data = awd,grouping_field = "TAXONCODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.tax)[colnames(rd.tax)=="Ave.y"]<-"Ave.rd" #Average % recent dead

#Calc_RDden_Transect
rdden.tax<-Calc_RDden_Seg(data=awd,grouping_field ="TAXONCODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.tax<-subset(rdden.tax,select = c(SITEVISITID,SITE,TRANSECT,SEGMENT,TAXONCODE,DZGN_G));colnames(acutedz.tax)[colnames(acutedz.tax)=="DZGN_G"]<-"DZGN_G_den" #subset just acute diseased colonies

#Calc_CONDden_Transect
condden.tax<-Calc_CONDden_Seg(data=awd,grouping_field ="TAXONCODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.tax<-subset(condden.tax,select = c(SITEVISITID,SITE,TRANSECT,SEGMENT,TAXONCODE,BLE));colnames(ble.tax)[colnames(ble.tax)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.tax<-subset(condden.tax,select = c(SITEVISITID,SITE,TRANSECT,SEGMENT,TAXONCODE,CHRO));colnames(chronicdz.tax)[colnames(chronicdz.tax)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies

#Calc_Richness_Transect
#rich.tax<-Calc_Richness_Transect(awd,"TAXONCODE")

#ADD CODE TO CHANGE TRANSECT NUMBERS FOR JUVENILES
jcd.tax$TRANSECT[jcd.tax$TRANSECT==3]<-1
jcd.tax$TRANSECT[jcd.tax$TRANSECT==4]<-2


#Merge density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","SEGMENT","TAXONCODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.tax<-Reduce(MyMerge, list(acd.tax,jcd.tax,cl.tax,od.tax,rd.tax,acutedz.tax,chronicdz.tax,ble.tax));
head(data.tax)

#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.tax$JuvColCount[is.na(data.tax$JuvColCount)]<-0;data.tax$JuvColDen[is.na(data.tax$JuvColDen)]<-0
data.tax$AdColCount[is.na(data.tax$AdColCount)]<-0;data.tax$AdColDen[is.na(data.tax$AdColDen)]<-0

#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.tax$DZGN_G_prev<-(data.tax$DZGN_G_den*data.tax$SEGAREA_ad)/data.tax$AdColCount*100
data.tax$BLE_prev<-(data.tax$BLE_den*data.tax$SEGAREA_ad)/data.tax$AdColCount*100
data.tax$CHRO_prev<-(data.tax$CHRO_den*data.tax$SEGAREA_ad)/data.tax$AdColCount*100


head(data.tax)
head(subset(data.tax,TAXONCODE==0))



# Prepare Diver data to merge with SfM data -------------------------------
#Make sure you have all transects and segments
levels(as.factor(data.tax$TRANSECT))
levels(as.factor(data.tax$SEGMENT))

#Merge survey segment with segment-level demographic data
diver.tax<-merge(survey_segment,data.tax,by=c("SITE","SITEVISITID","TRANSECT","SEGMENT"),all.y=T)
if(nrow(data.tax)!=nrow(diver.tax)) {cat("Warning: dataframes did not merge properly")}   
head(diver.tax)

