#This script reads in the diver and SfM-generated demographic data that has been QC'd and cleaned up
#Then generates segment-level summarized that for methods comparision

rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

#Read in files
ad_sfm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMAdult_MCLEANED.csv")
j_sfm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMJuv_MCLEANED.csv") 
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

#TEMPORARILY REMOVE ARI'S DATA BECAUSE ISN'T COMPLETE
ad_sfm<-subset(ad_sfm,ANALYST!="AH")
j_sfm<-subset(ad_sfm,ANALYST!="AH")

#Double check that the number of segments in the geodatabase matches what annoators said they completed 
#metadata file manually assembled from the tracking sheet pulled from google drive 
meta<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP2019_SfM_Meta.csv")
meta<-meta[,c("REGION","ISLAND","SITE","Mosaic_secsues","Segments_Annotated","Rugosity")]
head(meta)

#Missing segments from geodatabase
seg_tally<-ddply(ad_sfm,.(REGION,ISLAND,SITE),
                 summarize,
                 Segments_Annotated=length(unique(SEGMENT)))

miss.seg<-anti_join(seg_tally, meta, by="Segments_Annotated") #identify number of segments that don't match in geodatabase vs. tracking sheet


gd.list<-unique(ad_sfm$SITE)
meta.list<-unique(meta$SITE)

miss.site<-anti_join(meta.list,gd.list, by="SITE") #identify number of segments that don't match in geodatabase vs. tracking sheet





ad_sfm$TRANSECTAREA<-Transectarea(ad_sfm)
j_sfm$TRANSECTAREA<-Transectarea(j_sfm)


#Check if any site-segments have been dropped 
t1<-ddply(ad_sfm,.(SITE,SEGMENT),summarize,n=length(unique(ANALYST)));nrow(t1[t1$n>1,])
t1<-ddply(j_sfm,.(SITE,SEGMENT),summarize,n=length(unique(ANALYST)));nrow(t1[t1$n>1,]) 


#Create a look up table of all of the colony attributes- you will need this for the functions below
SURVEY_COL<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony<-unique(ad_sfm[,SURVEY_COL])

SURVEY_SITE<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")
survey_site<-unique(ad_sfm[,SURVEY_SITE])

SURVEY<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN","HABITAT_CODE", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","METHOD","TRANSECT","SEGMENT")
surveyment<-unique(ad_sfm[,SURVEY])


#Combine juvenile and adult data
aj_sfm<-full_join(ad_sfm,j_sfm) #fixes NA problem


# GENERATE SUMMARY METRICS at the Segment-leveL BY GENUS--------------------------------------------------

#Calc_ColDen_Transect
acd.gen<-Calc_ColDen_Transect(data = ad_sfm,grouping_field = "GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen";colnames(acd.gen)[colnames(acd.gen)=="SEGAREA"]<-"SEGAREA_ad"# calculate density at genus level as well as total
jcd.gen<-Calc_ColDen_Transect(j_sfm,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen"


## This function calculates mean colony length, % recent dead, % old dead, condition severity or condition extent to the segment level
## NOTE: can run both adult & juvenile data with this function for COLONYLENGTH
#c("COLONYLENGTH","RDEXTENT1", "RDEXTENT2", "RDEXTENT3", "OLDDEAD","SEVERITY_1","SEVERITY_2", "SEVERITY_3", "EXTENT_1", "EXTENT_2", "EXTENT_3")
ex_b<-subset(ad_sfm,EX_BOUND==0)
cl.gen<-Calc_ColMetric_Transect(data = ex_b,grouping_field = "GENUS_CODE",pool_fields = "COLONYLENGTH"); colnames(cl.gen)[colnames(cl.gen)=="Ave.y"]<-"Ave.size" #Average % old dead
od.gen<-Calc_ColMetric_Transect(data = ad_sfm,grouping_field = "GENUS_CODE",pool_fields = "OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.gen<-Calc_ColMetric_Transect(data = ad_sfm,grouping_field = "GENUS_CODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead


#Calc_RDden_Transect
rdden.gen<-Calc_RDden_Transect(data=ad_sfm,grouping_field ="GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.gen<-subset(rdden.gen,select = c(SITEVISITID,SITE,TRANSECT,GENUS_CODE,DZGN_G));colnames(acutedz.gen)[colnames(acutedz.gen)=="DZGN_G"]<-"AcuteDZ" #subset just acute diseased colonies


#Calc_CONDden_Transect
condden.gen<-Calc_CONDden_Transect(data=ad_sfm,grouping_field ="GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.gen<-subset(condden.gen,select = c(SITEVISITID,SITE,TRANSECT,GENUS_CODE,BLE));colnames(ble.gen)[colnames(ble.gen)=="BLE"]<-"BLE" #subset just bleached colonies
chronicdz.gen<-subset(condden.gen,select = c(SITEVISITID,SITE,TRANSECT,GENUS_CODE,CHRO));colnames(chronicdz.gen)[colnames(chronicdz.gen)=="CHRO"]<-"ChronicDZ"

#Calc_Richness_Transect
#rich.gen<-Calc_Richness_Transect(ad_sfm,"GENUS_CODE")


#Join density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
data.gen <- join_all(list(acd.gen,jcd.gen,cl.gen,od.gen,rd.gen,acutedz.gen,chronicdz.gen,ble.gen), 
                by=c("SITE","SITEVISITID","TRANSECT","GENUS_CODE"), type='full')
head(data.gen)


#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.gen$JuvColCount[is.na(data.gen$JuvColCount)]<-0;data.gen$JuvColDen[is.na(data.gen$JuvColDen)]<-0
data.gen$AdColCount[is.na(data.gen$AdColCount)]<-0;data.gen$AdColDen[is.na(data.gen$AdColDen)]<-0


#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.gen$AcuteDZ_prev<-(data.gen$AcuteDZ*data.gen$TRANSECTAREA)/data.gen$AdColCount*100
data.gen$BLE_prev<-(data.gen$BLE*data.gen$TRANSECTAREA)/data.gen$AdColCount*100
data.gen$ChronicDZ_prev<-(data.gen$ChronicDZ*data.gen$TRANSECTAREA)/data.gen$AdColCount*100

View(data.gen)

#Add adult and juvenile pres/ab columns
data.gen$Adpres.abs<-ifelse(data.gen$AdColDen>0,1,0)
data.gen$Juvpres.abs<-ifelse(data.gen$JuvColDen>0,1,0)

#We only surveyed 1 transect/site so data.gen is site-level data
site.data.gen2<-dplyr::select(data.gen,-c(TRANSECT,TRANSECTAREA))


#Merge site data with metadata

# Merge Site level data with sectors file and export site data ------------
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

#Merge together survey meta data and sector area files and check for missmatches 
meta<-left_join(survey_site,sectors)
meta[which(is.na(meta$AREA_HA)),]
nrow(survey_site)
nrow(meta)

#TEMPORARILY remove KAU-02016- it's incomplete in current version
site.data.gen2<-dplyr::filter(site.data.gen2, SITE != "KAU-02016")

#Merge site level data and meta data
site.data.gen2<-left_join(site.data.gen2,meta);head(site.data.gen2)


#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
site.data.gen2$DB_RZ<-paste(site.data.gen2$DEPTH_BIN,site.data.gen2$REEF_ZONE,sep="_")
site.data.gen2$STRATANAME<-paste(site.data.gen2$SEC_NAME,site.data.gen2$DB_RZ,sep="_")
site.data.gen2$ANALYSIS_SCHEMA<-site.data.gen2$STRATANAME
site.data.gen2$DOMAIN_SCHEMA<-site.data.gen2$SEC_NAME
site.data.gen2$ANALYSIS_YEAR<-site.data.gen2$OBS_YEAR


# Calculate STATA-level Estimates for SFM --------------------------------

#Create a vector of columns to subset for strata estimates
# c.keep<-c("REGION","DOMAIN_SCHEMA","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
#           "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep2<-c("REGION","DOMAIN_SCHEMA","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
           "n_h","N_h","D._h","SE_D._h")
# c.keep3<-c("REGION","DOMAIN_SCHEMA","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
#            "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep4<-c("REGION","DOMAIN_SCHEMA","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
           "n_h","N_h","prev","SEprev")


acdG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","AdColDen","Adpres.abs");acdG_st=acdG_st[,c.keep2]
colnames(acdG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","AdColDen","SE_AdColDen")

jcdG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","JuvColDen","Juvpres.abs");jcdG_st=jcdG_st[,c.keep2]
colnames(jcdG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","JuvColDen","SE_JuvColDen")

odG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.od");odG_st=odG_st[,c.keep2]
colnames(odG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.od","SE_Ave.od")

rdG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.rd");rdG_st=rdG_st[,c.keep2]
colnames(rdG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.rd","SE_Ave.rd")

clG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.size");clG_st=clG_st[,c.keep2]
colnames(clG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.size","SE_Ave.size")

BLEG_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","BLE");BLEG_st=BLEG_st[,c.keep4]
colnames(BLEG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","BLE","SE_BLE")

AcuteDZG_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","AcuteDZ");AcuteDZG_st=AcuteDZG_st[,c.keep4]
colnames(AcuteDZG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","AcuteDZ_Prev","SE_AcuteDZ_Prev")

ChronicDZG_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","ChronicDZ");ChronicDZG_st=ChronicDZG_st[,c.keep4]
colnames(ChronicDZG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","ChronicDZ_Prev","SE_ChronicDZ_Prev")


#Double Check that revised pooling is adding up NH (total sites) correctly
View(acdG_st)
View(sectors)


# Calculate SECTOR-level Estimates for SFM --------------------------------
acdG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","AdColDen","Adpres.abs")
acdG_sec<-acdG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AdColDen","SE_AdColDen")]
jcdG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","JuvColDen","Juvpres.abs")
jcdG_sec<-jcdG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen")]
odG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.od")
odG_sec<-odG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.od","SE_Ave.od")]
rdG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.rd")
rdG_sec<-rdG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.rd","SE_Ave.rd")]
clG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.size")
clG_sec<-clG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.size","SE_Ave.size")]
bleG_sec<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","BLE")
bleG_sec<-bleG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_BLE_Prev","SE_BLE_Prev")]
AcuteDZG_sec<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","AcuteDZ")
AcuteDZG_sec<-AcuteDZG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AcuteDZ_Prev","SE_AcuteDZ_Prev")]
ChronicDZG_sec<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","ChronicDZ")
ChronicDZG_sec<-ChronicDZG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_ChronicDZ_Prev","SE_ChronicDZ_Prev")]


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
sfm.strat<-Reduce(MyMerge, list(acdG_st,jcdG_st,odG_st,rdG_st,clG_st,BLEG_st,AcuteDZG_st,ChronicDZG_st))


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
sfm.sector<-Reduce(MyMerge, list(acdG_sec,jcdG_sec,odG_sec,rdG_sec,clG_sec,bleG_sec,AcuteDZG_sec,ChronicDZG_sec))
colnames(sfm.sector)[colnames(sfm.sector)=="DOMAIN_SCHEMA"]<-"Sector"

site.data.gen2$METHOD<-"SfM"
sfm.strat$METHOD<-"SfM"
sfm.sector$METHOD<-"SfM"


# #Save file for method comparsion
write.csv(site.data.gen2,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMGENUS_SITE.csv",row.names = F)
write.csv(sfm.strat,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMGENUS_STRATA.csv",row.names = F)
write.csv(sfm.sector,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMGENUS_SECTOR.csv",row.names = F)

# SUMMARIZE VISUAL DIVER DATA ---------------------------------------------

diver_site<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS.csv")

diver_site$METHOD<-"Diver"

#Only include the sites that were annotated by SfM
site.data.gen2<-dplyr::filter(diver_site, SITEVISITID %in% c(survey_site$SITEVISITID));head(site.data.gen2) 

#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
site.data.gen2$DB_RZ<-paste(site.data.gen2$DEPTH_BIN,site.data.gen2$REEF_ZONE,sep="_")
site.data.gen2$STRATANAME<-paste(site.data.gen2$SEC_NAME,site.data.gen2$DB_RZ,sep="_")
site.data.gen2$ANALYSIS_SCHEMA<-site.data.gen2$STRATANAME
site.data.gen2$DOMAIN_SCHEMA<-site.data.gen2$SEC_NAME
site.data.gen2$ANALYSIS_YEAR<-site.data.gen2$OBS_YEAR


# Calculate STATA-level Estimates for SFM --------------------------------

#Create a vector of columns to subset for strata estimates
# c.keep<-c("REGION","DOMAIN_SCHEMA","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
#           "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep2<-c("REGION","DOMAIN_SCHEMA","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
           "n_h","N_h","D._h","SE_D._h")
# c.keep3<-c("REGION","DOMAIN_SCHEMA","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
#            "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep4<-c("REGION","DOMAIN_SCHEMA","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
           "n_h","N_h","prev","SEprev")


acdG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","AdColDen","Adpres.abs");acdG_st=acdG_st[,c.keep2]
colnames(acdG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","AdColDen","SE_AdColDen")

jcdG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","JuvColDen","Juvpres.abs");jcdG_st=jcdG_st[,c.keep2]
colnames(jcdG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","JuvColDen","SE_JuvColDen")

odG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.od");odG_st=odG_st[,c.keep2]
colnames(odG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.od","SE_Ave.od")

rdG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.rd");rdG_st=rdG_st[,c.keep2]
colnames(rdG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.rd","SE_Ave.rd")

clG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.size");clG_st=clG_st[,c.keep2]
colnames(clG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.size","SE_Ave.size")

BLEG_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","BLE");BLEG_st=BLEG_st[,c.keep4]
colnames(BLEG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","BLE","SE_BLE")

AcuteDZG_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","AcuteDZ");AcuteDZG_st=AcuteDZG_st[,c.keep4]
colnames(AcuteDZG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","AcuteDZ_Prev","SE_AcuteDZ_Prev")

ChronicDZG_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","ChronicDZ");ChronicDZG_st=ChronicDZG_st[,c.keep4]
colnames(ChronicDZG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","ChronicDZ_Prev","SE_ChronicDZ_Prev")


#Double Check that revised pooling is adding up NH (total sites) correctly
View(acdG_st)
View(sectors)


# Calculate SECTOR-level Estimates for SFM --------------------------------
acdG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","AdColDen","Adpres.abs")
acdG_sec<-acdG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AdColDen","SE_AdColDen")]
jcdG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","JuvColDen","Juvpres.abs")
jcdG_sec<-jcdG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen")]
odG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.od")
odG_sec<-odG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.od","SE_Ave.od")]
rdG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.rd")
rdG_sec<-rdG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.rd","SE_Ave.rd")]
clG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.size")
clG_sec<-clG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.size","SE_Ave.size")]
bleG_sec<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","BLE")
bleG_sec<-bleG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_BLE_Prev","SE_BLE_Prev")]
AcuteDZG_sec<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","AcuteDZ")
AcuteDZG_sec<-AcuteDZG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AcuteDZ_Prev","SE_AcuteDZ_Prev")]
ChronicDZG_sec<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","ChronicDZ")
ChronicDZG_sec<-ChronicDZG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_ChronicDZ_Prev","SE_ChronicDZ_Prev")]


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
diver.strat<-Reduce(MyMerge, list(acdG_st,jcdG_st,odG_st,rdG_st,clG_st,BLEG_st,AcuteDZG_st,ChronicDZG_st))


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
diver.sector<-Reduce(MyMerge, list(acdG_sec,jcdG_sec,odG_sec,rdG_sec,clG_sec,bleG_sec,AcuteDZG_sec,ChronicDZG_sec))
colnames(diver.sector)[colnames(diver.sector)=="DOMAIN_SCHEMA"]<-"Sector"


diver.strat$METHOD<-"Diver"
diver.sector$METHOD<-"Diver"


# #Save file for method comparsion
write.csv(diver_site,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_DiverGENUS_SITE.csv",row.names = F)
write.csv(diver.strat,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_DiverGENUS_STRATA.csv",row.names = F)
write.csv(diver.sector,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_DiverGENUS_SECTOR.csv",row.names = F)

