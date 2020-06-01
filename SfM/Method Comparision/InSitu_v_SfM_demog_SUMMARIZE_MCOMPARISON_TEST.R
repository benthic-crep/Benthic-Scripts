#This script reads in the diver and SfM-generated demographic data that has been QC'd and cleaned up
#Then generates segment-level summarized that for methods comparision


#Start by integrating sfm and diver colony level data before running calctransect script



rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

#Read in files
ad_sfm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMAdult_MCLEANED.csv")
j_sfm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMJuv_MCLEANED.csv") 
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)


#Double check that the number of segments in the geodatabase matches what annoators said they completed 
#metadata file manually assembled from the tracking sheet pulled from google drive 
meta<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP2019_SfM_Meta.csv")
meta<-meta[,c("ISLAND","SITE","Mosaic_Issues","Segments_Annotated","Rugosity")]
head(meta)

#Missing segments from geodatabase FOR ADULTS
seg_tally<-ddply(ad_sfm,.(ISLAND,SITE),
                 summarize,
                 Segments_inGD=length(unique(SEGMENT)))

tmp.seg<-full_join(meta,seg_tally)
View(tmp.seg)

miss.seg<-dplyr::filter(tmp.seg, Segments_Annotated !=Segments_inGD);miss.seg #identify sites that have missing segments
miss.site<-dplyr::filter(tmp.seg, is.na(Segments_inGD));miss.site #identify sites that have missing segments


#Missing segments from geodatabase FOR JUV
seg_tally<-ddply(j_sfm,.(ISLAND,SITE),
                 summarize,
                 Segments_inGD=length(unique(SEGMENT)))


tmp.seg<-full_join(meta,seg_tally)
View(tmp.seg)

miss.seg<-dplyr::filter(tmp.seg, Segments_Annotated !=Segments_inGD);miss.seg #identify sites that have missing segments
miss.site<-dplyr::filter(tmp.seg, is.na(Segments_inGD));miss.site #identify sites that have missing segments


ad_sfm$TRANSECTAREA<-Transectarea(ad_sfm)
j_sfm$TRANSECTAREA<-Transectarea(j_sfm)

#Double check transect areas
summary(j_sfm$TRANSECTAREA)
summary(ad_sfm$TRANSECTAREA)

# head(subset(j_sfm,TRANSECTAREA==4.5)) #look at site LAN-01813
# View(subset(j_sfm,SITE=="LAN-01813"))

#Check if any site-segments have been dropped 
t1<-ddply(ad_sfm,.(SITE,SEGMENT),summarize,n=length(unique(ANALYST)));nrow(t1[t1$n>1,])
t1<-ddply(j_sfm,.(SITE,SEGMENT),summarize,n=length(unique(ANALYST)));nrow(t1[t1$n>1,]) 

ad_sfm$SITE_SEG<-paste(ad_sfm$SITE,ad_sfm$SEGMENT,sep ="_")
j_sfm$SITE_SEG<-paste(j_sfm$SITE,j_sfm$SEGMENT,sep ="_")


# PREP VISUAL DIVER DATA ---------------------------------------------
## LOAD benthic data
awd_<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED.csv")
jwd_<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Juveniles_raw_CLEANED.csv")

awd<-subset(awd_,OBS_YEAR=="2019")
jwd<-subset(jwd_,OBS_YEAR=="2019")

#Colony fragments and scleractinans are subseted in the functions 
#Add a column for adult fragments so we can remove them from the dataset later (-1 indicates fragment)
awd$Fragment<-ifelse(awd$OBS_YEAR <2018 & awd$COLONYLENGTH <5 & awd$S_ORDER=="Scleractinia",-1,awd$Fragment)
head(subset(awd,Fragment==-1& OBS_YEAR<2018)) #double check that pre 2018 fragments create
awd$Fragment[is.na(awd$Fragment)] <- 0
jwd$Fragment <- 0 # you need to add this column so that you can use the site level functions correctly


#Simplify Bleaching Severity categories: in 2019 the team decided to simplify the bleaching severity from 1-5 to 1-3 to improve consistency in severity values
#This code converts the severity data collected prior to 2019 to a 1-3 scale
awd$DATE_ <- as.Date(awd$DATE_, format = "%Y-%m-%d")
jwd$DATE_ <- as.Date(jwd$DATE_, format = "%Y-%m-%d")

awd_pre <- awd %>% filter(DATE_ < as.Date('2019-07-11'))
awd_post<-awd %>% filter(DATE_ >= as.Date('2019-07-11'))

awd_pre<-Convert_Severity(awd_pre,"SEVERITY_1","SEVERITY_1n")
awd_pre<-Convert_Severity(awd_pre,"SEVERITY_2","SEVERITY_2n")
#awd_pre<-Convert_Severity(awd_pre,"SEVERITY_3","SEVERITY_3n") #There were no severity measurements prior to 2020

head(awd_pre)

#After checking that severity numbers were changed correctly, convert back to original column names & drop original columns
awd_pre<-subset(awd_pre,select=-c(SEVERITY_1));colnames(awd_pre)[which(colnames(awd_pre) == 'SEVERITY_1n')] <- "SEVERITY_1" #change group to whatever your grouping field is.
awd_pre<-subset(awd_pre,select=-c(SEVERITY_2));colnames(awd_pre)[which(colnames(awd_pre) == 'SEVERITY_2n')] <- "SEVERITY_2" #change group to whatever your grouping field is.
#awd_pre<-subset(awd_pre,select=-c(SEVERITY_3));colnames(awd_pre)[which(colnames(awd_pre) == 'SEVERITY_3n')] <- "SEVERITY_3" #change group to whatever your grouping field is.
awd_pre$SEVERITY_3<-NA

head(awd_pre)

#Combine dataframes before and after 2019 & check that rows weren't dropped
awd.<-rbind(awd_pre,awd_post);write.csv(awd.,"test.csv")

#Change bleaching severity = 1 to NA
x<-x %>% mutate_at(.vars = c("CONDITION_1", "EXTENT_1", "SEVERITY_1"), 
                   list(~replace(.,CONDITION_1 =='BLE' & SEVERITY_1=='1', 'NA')));View(x)
x<-x %>% mutate_at(.vars = c("CONDITION_2", "EXTENT_2", "SEVERITY_2"), 
                   list(~replace(.,CONDITION_2 =='BLE' & SEVERITY_2=='1', 'NA')));View(x)
x<-x %>% mutate_at(.vars = c("CONDITION_3", "EXTENT_3", "SEVERITY_3"), 
                   list(~replace(.,CONDITION_3 =='BLE' & SEVERITY_3=='1', 'NA')));View(x)

nrow(awd)
nrow(awd.);head(awd.)
awd<-awd.; rm("awd.") #remove temporary dataframe if all good. 


#Only include sites and segments surveyed by divers during HARAMP 2019
awd$SITE_SEG<-paste(awd$SITE,awd$SEGMENT,sep="_")
jwd$SITE_SEG<-paste(jwd$SITE,jwd$SEGMENT,sep="_")

#Change columns to merge with sfm data
colnames(awd)[which(colnames(awd) == 'DIVER')] <- "ANALYST"
colnames(jwd)[which(colnames(jwd) == 'DIVER')] <- "ANALYST"
awd$EX_BOUND<-0;awd$EX_BOUND<-as.numeric(awd$EX_BOUND)
awd$SEGAREA<-awd$SEGLENGTH*awd$SEGWIDTH
jwd$SEGAREA<-jwd$SEGLENGTH*jwd$SEGWIDTH

awd<-dplyr::select(awd,-c(bANALYSIS_SCHEME,ANALYSIS_YEAR,EXCLUDE_FLAG,REGION_NAME,NO_SURVEY_YN,DATE_,ISLANDCODE))
jwd<-dplyr::select(jwd,-c(bANALYSIS_SCHEME,ANALYSIS_YEAR,EXCLUDE_FLAG,REGION_NAME,NO_SURVEY_YN,DATE_,ISLANDCODE))
awd<-dplyr::filter(awd, SITE_SEG %in% c(ad_sfm$SITE_SEG));head(awd) 
jwd<-dplyr::filter(jwd, SITE_SEG %in% c(j_sfm$SITE_SEG));head(jwd) 

length(unique(awd$SITE))
length(unique(jwd$SITE))

awd$METHOD<-"Diver";awd$METHOD<-as.factor(awd$METHOD)
jwd$METHOD<-"Diver";jwd$METHOD<-as.factor(jwd$METHOD)

sort(colnames(awd))
sort(colnames(ad_sfm))

sort(colnames(jwd))
sort(colnames(j_sfm))

sapply(awd,class)
sapply(ad_sfm,class)

awd.all<-rbind(ad_sfm,awd)
jwd.all<-rbind(j_sfm,jwd)

#Create a look up table of all of the colony attributes- you will need this for the functions below
SURVEY_COL<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony<-unique(ad_sfm[,SURVEY_COL])

SURVEY_SITE<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")
survey_site<-unique(ad_sfm[,SURVEY_SITE])

SURVEY<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
          "DEPTH_BIN","HABITAT_CODE", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","METHOD","TRANSECT","SEGMENT")
survey_segment<-unique(ad_sfm[,SURVEY])

nrow(survey_site)

#TEMPORARY WORK AROUND-ASK MICHAEL TO FIX
survey_site$REEF_ZONE<-ifelse(survey_site$SITE=="HAW-04285","Forereef",as.character(survey_site$REEF_ZONE))








# GENERATE SUMMARY METRICS at the Segment-leveL BY GENUS--------------------------------------------------

#Calc_ColDen_Transect
acd.gen<-Calc_ColDen_Transect(data = ad_sfm,grouping_field = "GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen";colnames(acd.gen)[colnames(acd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_ad"# calculate density at genus level as well as total
jcd.gen<-Calc_ColDen_Transect(j_sfm,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen";colnames(jcd.gen)[colnames(jcd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_j"


## This function calculates mean colony length, % recent dead, % old dead, condition severity or condition extent to the segment level
## NOTE: can run both adult & juvenile data with this function for COLONYLENGTH
#c("COLONYLENGTH","RDEXTENT1", "RDEXTENT2", "RDEXTENT3", "OLDDEAD","SEVERITY_1","SEVERITY_2", "SEVERITY_3", "EXTENT_1", "EXTENT_2", "EXTENT_3")
ex_b<-subset(ad_sfm,EX_BOUND==0)
cl.gen<-Calc_ColMetric_Transect(data = ex_b,grouping_field = "GENUS_CODE",pool_fields = "COLONYLENGTH"); colnames(cl.gen)[colnames(cl.gen)=="Ave.y"]<-"Ave.size" #Average % old dead
od.gen<-Calc_ColMetric_Transect(data = ad_sfm,grouping_field = "GENUS_CODE",pool_fields = "OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.gen<-Calc_ColMetric_Transect(data = ad_sfm,grouping_field = "GENUS_CODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead


#Calc_RDden_Transect
rdden.gen<-Calc_RDden_Transect(data=ad_sfm,grouping_field ="GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.gen<-subset(rdden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,GENUS_CODE,DZGN_G));colnames(acutedz.gen)[colnames(acutedz.gen)=="DZGN_G"]<-"AcuteDZ" #subset just acute diseased colonies


#Calc_CONDden_Transect
condden.gen<-Calc_CONDden_Transect(data=ad_sfm,grouping_field ="GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,GENUS_CODE,BLE));colnames(ble.gen)[colnames(ble.gen)=="BLE"]<-"BLE" #subset just bleached colonies
chronicdz.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,GENUS_CODE,CHRO));colnames(chronicdz.gen)[colnames(chronicdz.gen)=="CHRO"]<-"ChronicDZ"

#Calc_Richness_Transect
#rich.gen<-Calc_Richness_Transect(ad_sfm,"GENUS_CODE")


#Join density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
data.gen <- join_all(list(acd.gen,jcd.gen,cl.gen,od.gen,rd.gen,acutedz.gen,chronicdz.gen,ble.gen), 
                     by=c("METHOD","SITE","SITEVISITID","TRANSECT","GENUS_CODE"), type='full')
head(data.gen)
length(unique(data.gen$SITE))

#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.gen$JuvColCount[is.na(data.gen$JuvColCount)]<-0;data.gen$JuvColDen[is.na(data.gen$JuvColDen)]<-0
data.gen$AdColCount[is.na(data.gen$AdColCount)]<-0;data.gen$AdColDen[is.na(data.gen$AdColDen)]<-0


#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.gen$AcuteDZ_prev<-(data.gen$AcuteDZ*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100
data.gen$BLE_prev<-(data.gen$BLE*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100
data.gen$ChronicDZ_prev<-(data.gen$ChronicDZ*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100

View(data.gen)

#Add adult and juvenile pres/ab columns
data.gen$Adpres.abs<-ifelse(data.gen$AdColDen>0,1,0)
data.gen$Juvpres.abs<-ifelse(data.gen$JuvColDen>0,1,0)

#We only surveyed 1 transect/site so data.gen is site-level data
if(length(unique(data.gen$TRANSECT))>1) {cat("WARNING:MORE THAN 1 TRANSECT/SITE IN DF")} #Check that adult data weren't dropped  
site.data.gen2<-dplyr::select(data.gen,-(TRANSECT))

#Merge site data with metadata

# Merge Site level data with sectors file and export site data ------------
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

#Merge together survey meta data and sector area files and check for missmatches 
meta<-left_join(survey_site,sectors)
meta[which(is.na(meta$AREA_HA)),]
nrow(survey_site)
nrow(meta)


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







# GENERATE SUMMARY METRICS at the transect-leveL BY GENUS--------------------------------------------------
#Calc_ColDen_Transect
acd.gen<-Calc_ColDen_Transect(data = awd,grouping_field = "GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen";colnames(acd.gen)[colnames(acd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_ad"# calculate density at genus level as well as total
jcd.gen<-Calc_ColDen_Transect(jwd,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen";colnames(jcd.gen)[colnames(jcd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_j"

#Calc_ColMetric_Transect
cl.gen<-Calc_ColMetric_Transect(data = awd,grouping_field = "GENUS_CODE",pool_fields = "COLONYLENGTH"); colnames(cl.gen)[colnames(cl.gen)=="Ave.y"]<-"Ave.size" #Average % old dead
od.gen<-Calc_ColMetric_Transect(data = awd,grouping_field = "GENUS_CODE",pool_fields = "OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.gen<-Calc_ColMetric_Transect(data = awd,grouping_field = "GENUS_CODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead

#Calc_RDden_Transect
rdden.gen<-Calc_RDden_Transect(awd,survey_colony,"GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.gen<-subset(rdden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,GENUS_CODE,DZGN_G));colnames(acutedz.gen)[colnames(acutedz.gen)=="DZGN_G"]<-"AcuteDZ" #subset just acute diseased colonies

#Calc_CONDden_Transect
condden.gen<-Calc_CONDden_Transect(awd,survey_colony,"GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,GENUS_CODE,BLE));colnames(ble.gen)[colnames(ble.gen)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,GENUS_CODE,CHRO));colnames(chronicdz.gen)[colnames(chronicdz.gen)=="CHRO"]<-"ChronicDZ" #subset just chronic diseased colonies

#Calc_Richness_Transect
rich.gen<-Calc_Richness_Transect(awd,"GENUS_CODE")

#ADD CODE TO CHANGE TRANSECT NUMBERS FOR JUVENILES
jcd.gen$TRANSECT[jcd.gen$TRANSECT==3]<-1
jcd.gen$TRANSECT[jcd.gen$TRANSECT==4]<-2


#Merge density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("METHOD","SITE","SITEVISITID","TRANSECT","GENUS_CODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.gen<-Reduce(MyMerge, list(acd.gen,jcd.gen,cl.gen,od.gen,rd.gen,acutedz.gen,chronicdz.gen,ble.gen));
head(data.gen)


#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.gen$JuvColCount[is.na(data.gen$JuvColCount)]<-0;data.gen$JuvColDen[is.na(data.gen$JuvColDen)]<-0
data.gen$AdColCount[is.na(data.gen$AdColCount)]<-0;data.gen$AdColDen[is.na(data.gen$AdColDen)]<-0

#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.gen$AcuteDZ_prev<-(data.gen$AcuteDZ*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100
data.gen$BLE_prev<-(data.gen$BLE_den*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100
data.gen$ChronicDZ_prev<-(data.gen$ChronicDZ*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100

#GENERATE SITE-LEVEL DATA BY AVERAGING TRANSECTS-----------------------------------
site.data.gen2<-ddply(data.gen, .(METHOD,SITE,SITEVISITID,GENUS_CODE,TRANSECTAREA_ad,TRANSECTAREA_j), #calc total colonies by condition
                      summarise,
                      AdColCount=sum(AdColCount,na.rm=T),AdColDen=mean(AdColDen,na.rm = T),Ave.od=mean(Ave.od,na.rm = T),
                      Ave.rd=mean(Ave.rd,na.rm = T),Ave.size=mean(Ave.size,na.rm=T),JuvColDen=mean(JuvColDen,na.rm=T),
                      BLE=mean(BLE_den,na.rm=T),AcuteDZ=mean(AcuteDZ,na.rm=T),ChronicDZ=mean(ChronicDZ,na.rm=T),
                      BLE_prev=mean(BLE_prev,na.rm=T),AcuteDZ_prev=mean(AcuteDZ_prev,na.rm=T),ChronicDZ_prev=mean(ChronicDZ_prev,na.rm=T))



#Add adult and juvenile pres/ab columns
site.data.gen2$Adpres.abs<-ifelse(site.data.gen2$AdColDen>0,1,0)
site.data.gen2$Juvpres.abs<-ifelse(site.data.gen2$JuvColDen>0,1,0)


#Merge site data with metadata
#Merge together survey meta data and sector area files and check for missmatches 
meta<-left_join(survey_site,sectors)
meta[which(is.na(meta$AREA_HA)),]
nrow(survey_site)
nrow(meta)

#Merge site level data and meta data
site.data.gen2<-left_join(site.data.gen2,meta);head(site.data.gen2)

#######################
############################

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

diver_site<-site.data.gen2

# #Save file for method comparsion
write.csv(diver_site,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_DiverGENUS_SITE.csv",row.names = F)
write.csv(diver.strat,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_DiverGENUS_STRATA.csv",row.names = F)
write.csv(diver.sector,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_DiverGENUS_SECTOR.csv",row.names = F)


###The diver data has duplicate entries that is causing merging issues. Fix this next

