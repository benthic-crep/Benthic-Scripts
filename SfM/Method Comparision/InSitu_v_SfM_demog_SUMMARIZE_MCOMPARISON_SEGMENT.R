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


#Double check that the number of segments in the geodatabase matches what annoators said they completed 
#metadata file manually assembled from the tracking sheet pulled from google drive 
meta<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP2019_SfM_Meta.csv")
meta<-meta[,c("ISLAND","SITE","Mosaic_Issues","Segments_Annotated","Rugosity")]
head(meta)

# #Missing segments from geodatabase FOR ADULTS
# seg_tally<-ddply(ad_sfm,.(ISLAND,SITE),
#                  summarize,
#                  Segments_inGD=length(unique(SEGMENT)))
# 
# tmp.seg<-full_join(meta,seg_tally)
# View(tmp.seg)
# 
# miss.seg<-dplyr::filter(tmp.seg, Segments_Annotated !=Segments_inGD);miss.seg #identify sites that have missing segments
# miss.site<-dplyr::filter(tmp.seg, is.na(Segments_inGD));miss.site #identify sites that have missing segments
# 
# 
# #Missing segments from geodatabase FOR JUV
# seg_tally<-ddply(j_sfm,.(ISLAND,SITE),
#                  summarize,
#                  Segments_inGD=length(unique(SEGMENT)))
# 
# 
# tmp.seg<-full_join(meta,seg_tally)
# View(tmp.seg)
# 
# miss.seg<-dplyr::filter(tmp.seg, Segments_Annotated !=Segments_inGD);miss.seg #identify sites that have missing segments
# miss.site<-dplyr::filter(tmp.seg, is.na(Segments_inGD));miss.site #identify sites that have missing segments
# 

ad_sfm$TRANSECTAREA<-Transectarea(ad_sfm)
j_sfm$TRANSECTAREA<-Transectarea(j_sfm)

#Double check transect areas
summary(j_sfm$TRANSECTAREA)
summary(ad_sfm$TRANSECTAREA)

# head(subset(j_sfm,TRANSECTAREA==4.5)) #look at site LAN-01813
# View(subset(j_sfm,SITE=="LAN-01813"))

#Check if any site-segments have been dropped 
ad_sfm$SITE_SEG<-paste(ad_sfm$SITE,ad_sfm$SEGMENT,sep ="_")
j_sfm$SITE_SEG<-paste(j_sfm$SITE,j_sfm$SEGMENT,sep ="_")
length(unique(ad_sfm$SITE_SEG));length(unique(ad_sfm$SITE)) #should be 389 ss and 104 sites
length(unique(j_sfm$SITE_SEG));length(unique(j_sfm$SITE)) #should be 312 ss and 104 sites


#Need to identify 1 analyst per segment
#Because we had different people go back and make some corrections on the belts,we may have more than 1 person on a segment.
#But the annoations are not evenly distributed across the 2 annoators (e.g. annoator 1 may have annoated 15 colonies and #2 only did 2)
#This won't work for the mixed models, so I've identified the person with the max number of annoatations and labeled them as the annoator of the full segment
#It's not perfect, but the best we can do right now.

#Adults
tmp<-ddply(ad_sfm,.(METHOD,SITE_SEG,ANALYST),summarize,n=length(COLONYID)) #identify # of colonies surveyed for each method, ss and annotoator
tmp2<-ddply(tmp,.(METHOD,SITE_SEG),summarize,n=max(n)) #identify the max number of colonies/seg and merge back with tmp to identify primary annoator
atmp<-left_join(tmp2,tmp)
head(tmp,10);head(atmp,10)
colnames(atmp)[colnames(atmp)=="ANALYST"]<-"N.ANALYST" #Change column name
atmp<-subset(atmp,select=-n)


#Juveniles
tmp<-ddply(j_sfm,.(METHOD,SITE_SEG,ANALYST),summarize,n=length(COLONYID)) #identify # of colonies surveyed for each method, ss and annotoator
tmp2<-ddply(tmp,.(METHOD,SITE_SEG),summarize,n=max(n)) #identify the max number of colonies/seg and merge back with tmp to identify primary annoator
jtmp<-left_join(tmp2,tmp)
head(tmp,10);head(jtmp,10)
colnames(jtmp)[colnames(jtmp)=="ANALYST"]<-"N.ANALYST" #Change column name
jtmp<-subset(jtmp,select=-n)

#Merge back to original dataframe and change column names
ad_sfmN<-left_join(ad_sfm,atmp)
length(unique(ad_sfmN$SITE));length(unique(ad_sfmN$SITE_SEG))# double check that numbers match Site should be 104 and 389
ad_sfmN<-subset(ad_sfmN,select=-ANALYST);colnames(ad_sfmN)[colnames(ad_sfmN)=="N.ANALYST"]<-"ANALYST" #Change back to ANALYST
ad_sfmN$ANALYST<-ifelse(ad_sfmN$SITE=="HAW-03465","MSL",as.character(ad_sfmN$ANALYST))#change annoators because equally split
ad_sfmN$ANALYST<-ifelse(ad_sfmN$SITE=="MAI-02530","FL",as.character(ad_sfmN$ANALYST))

View(ad_sfmN)

j_sfmN<-left_join(j_sfm,jtmp)
length(unique(j_sfmN$SITE));length(unique(j_sfmN$SITE_SEG))# double check that numbers match Site should be 104 and 312
j_sfmN<-subset(j_sfmN,select=-ANALYST);colnames(j_sfmN)[colnames(j_sfmN)=="N.ANALYST"]<-"ANALYST" #Change back to ANALYST
ad_sfmN$ANALYST<-ifelse(ad_sfmN$SITE=="HAW-03465","MSL",as.character(ad_sfmN$ANALYST))#change annoators because equally split
ad_sfmN$ANALYST<-ifelse(ad_sfmN$SITE=="MAI-02530","FL",as.character(ad_sfmN$ANALYST))

View(j_sfmN)


# PREP VISUAL DIVER DATA ---------------------------------------------
## LOAD benthic data
# awd_<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED.csv")
# jwd_<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Juveniles_raw_CLEANED.csv")

awd_<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_E_raw_CLEANED.csv")
jwd_<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_F_raw_CLEANED.csv")

awd<-subset(awd_,OBS_YEAR=="2019"&TRANSECT==1)
jwd<-subset(jwd_,OBS_YEAR=="2019"&TRANSECT==1)


#Changing Analyst names to match SfM
awd$DIVER<-ifelse(awd$DIVER=="AH_","AH",as.character(awd$DIVER))
awd$DIVER<-ifelse(awd$DIVER=="M_A","MA",as.character(awd$DIVER))
jwd$DIVER<-ifelse(jwd$DIVER=="AH_","AH",as.character(jwd$DIVER))
jwd$DIVER<-ifelse(jwd$DIVER=="M_A","MA",as.character(jwd$DIVER))


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
awd.<-awd. %>% mutate_at(.vars = c("CONDITION_1", "EXTENT_1", "SEVERITY_1"), 
                   list(~replace(.,CONDITION_1 =='BLE' & SEVERITY_1=='1', NA)));View(awd.)
awd.<-awd. %>% mutate_at(.vars = c("CONDITION_2", "EXTENT_2", "SEVERITY_2"), 
                   list(~replace(.,CONDITION_2 =='BLE' & SEVERITY_2=='1', NA)))
awd.<-awd. %>% mutate_at(.vars = c("CONDITION_3", "EXTENT_3", "SEVERITY_3"), 
                   list(~replace(.,CONDITION_3 =='BLE' & SEVERITY_3=='1', NA)))

nrow(awd)
nrow(awd.);head(awd.)
awd<-awd.; rm("awd.") #remove temporary dataframe if all good. 

#Change columns to merge with sfm data
colnames(awd)[which(colnames(awd) == 'DIVER')] <- "ANALYST"
colnames(jwd)[which(colnames(jwd) == 'DIVER')] <- "ANALYST"
awd$EX_BOUND<-0;awd$EX_BOUND<-as.numeric(awd$EX_BOUND)
jwd$EX_BOUND<-0;jwd$EX_BOUND<-as.numeric(jwd$EX_BOUND)

#Only include sites and segments surveyed by divers during HARAMP 2019
awd$SITE_SEG<-paste(awd$SITE,awd$SEGMENT,sep="_")
jwd$SITE_SEG<-paste(jwd$SITE,jwd$SEGMENT,sep="_")

awd$SEGAREA<-awd$SEGLENGTH*awd$SEGWIDTH
jwd$SEGAREA<-jwd$SEGLENGTH*jwd$SEGWIDTH

awd<-dplyr::select(awd,-c(bANALYSIS_SCHEME,ANALYSIS_YEAR,EXCLUDE_FLAG,REGION_NAME,NO_SURVEY_YN,DATE_,ISLANDCODE))
jwd<-dplyr::select(jwd,-c(bANALYSIS_SCHEME,ANALYSIS_YEAR,EXCLUDE_FLAG,REGION_NAME,NO_SURVEY_YN,DATE_,ISLANDCODE))
awd<-dplyr::filter(awd, SITE_SEG %in% c(ad_sfmN$SITE_SEG));head(awd) 
jwd<-dplyr::filter(jwd, SITE_SEG %in% c(j_sfmN$SITE_SEG));head(jwd) 

#Drop segments that have <2.5 segarea for adults
awd<-dplyr::filter(awd, SEGAREA==2.5);View(awd) 
jwd<-dplyr::filter(jwd, SEGAREA==1);View(jwd) 

length(unique(awd$SITE))
length(unique(jwd$SITE))


#Remove segments that were annoated in SfM, but not surveyd by divers
ad_sfm_sub<-dplyr::filter(ad_sfmN, SITE_SEG %in% c(awd$SITE_SEG));nrow(ad_sfm);nrow(ad_sfm_sub)
j_sfm_sub<-dplyr::filter(j_sfmN, SITE_SEG %in% c(jwd$SITE_SEG));nrow(j_sfm);nrow(j_sfm_sub);View(j_sfm_sub) 


length(unique(ad_sfm_sub$SITE));length(unique(ad_sfm_sub$SITE_SEG))
length(unique(j_sfm_sub$SITE));length(unique(j_sfm_sub$SITE_SEG))

awd$METHOD<-"Diver";awd$METHOD<-as.factor(awd$METHOD)
jwd$METHOD<-"Diver";jwd$METHOD<-as.factor(jwd$METHOD)

sort(colnames(awd))
sort(colnames(ad_sfm_sub))

sort(colnames(jwd))
sort(colnames(j_sfm_sub))

sapply(awd,class)
sapply(ad_sfm,class)

awd.all<-rbind(ad_sfm_sub,awd)
jwd.all<-rbind(j_sfm_sub,jwd)

#Remove juvenile colonies <0.7cm
tmp<-jwd.all %>% mutate_at(.vars = c("GENUS_CODE", "SPCODE","TAXONCODE"), 
                         list(~replace(.,COLONYLENGTH <0.7, "AAAA")));View(tmp)
tmp<-tmp %>% mutate_at(.vars = c("S_ORDER", "COLONYID","TAXONNAME"), 
                            list(~replace(.,TAXONCODE=="AAAA", NA)));View(tmp)
jwd.all<-tmp %>% mutate_at(.vars = c("COLONYLENGTH"), 
                         list(~replace(.,TAXONCODE=="AAAA", 0)));View(jwd.all)

#Create a look up table of all of the colony attributes- you will need this for the functions below
SURVEY_COL<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony<-unique(awd.all[,SURVEY_COL])

SURVEY_SITE<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")
survey_site<-unique(awd.all[,SURVEY_SITE])

SURVEY<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
          "DEPTH_BIN","HABITAT_CODE", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","SITE_SEG","ANALYST")
survey_segment<-unique(awd.all[,SURVEY])

nrow(survey_segment)

#TEMPORARY WORK AROUND-ASK MICHAEL TO FIX
survey_site$REEF_ZONE<-ifelse(survey_site$SITE=="HAW-04285","Forereef",as.character(survey_site$REEF_ZONE))
survey_segment$REEF_ZONE<-ifelse(survey_segment$SITE=="HAW-04285","Forereef",as.character(survey_segment$REEF_ZONE))


# GENERATE SUMMARY METRICS at the Segment-leveL BY GENUS--------------------------------------------------

#Calc_ColDen_SEG
acd.gen<-Calc_ColDen_Seg(data = awd.all,grouping_field = "GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen";colnames(acd.gen)[colnames(acd.gen)=="SEGAREA"]<-"SEGAREA_ad"# calculate density at genus level as well as total
jcd.gen<-Calc_ColDen_Seg(jwd.all,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen";colnames(jcd.gen)[colnames(jcd.gen)=="SEGAREA"]<-"SEGAREA_j"


## This function calculates mean colony length, % recent dead, % old dead, condition severity or condition extent to the segment level
## NOTE: can run both adult & juvenile data with this function for COLONYLENGTH
#c("COLONYLENGTH","RDEXTENT1", "RDEXTENT2", "RDEXTENT3", "OLDDEAD","SEVERITY_1","SEVERITY_2", "SEVERITY_3", "EXTENT_1", "EXTENT_2", "EXTENT_3")
ex_b<-subset(awd.all,EX_BOUND==0)
cl.gen<-Calc_ColMetric_Seg(data = ex_b,grouping_field = "GENUS_CODE",pool_fields = "COLONYLENGTH"); colnames(cl.gen)[colnames(cl.gen)=="Ave.y"]<-"Ave.size" #Average % old dead
od.gen<-Calc_ColMetric_Seg(data = awd.all,grouping_field = "GENUS_CODE",pool_fields = "OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.gen<-Calc_ColMetric_Seg(data = awd.all,grouping_field = "GENUS_CODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead


#Calc_RDden_Transect
rdden.gen<-Calc_RDden_Seg(data=awd.all,grouping_field ="GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.gen<-subset(rdden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,DZGN_G));colnames(acutedz.gen)[colnames(acutedz.gen)=="DZGN_G"]<-"AcuteDZ" #subset just acute diseased colonies


#Calc_CONDden_Transect
condden.gen<-Calc_CONDden_Seg(data=awd.all,grouping_field ="GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,BLE));colnames(ble.gen)[colnames(ble.gen)=="BLE"]<-"BLE" #subset just bleached colonies
chronicdz.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,CHRO));colnames(chronicdz.gen)[colnames(chronicdz.gen)=="CHRO"]<-"ChronicDZ"

#Calc_Richness_Transect
#rich.gen<-Calc_Richness_Transect(ad_sfm,"GENUS_CODE")


#Join density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
data.gen <- join_all(list(acd.gen,jcd.gen,cl.gen,od.gen,rd.gen,acutedz.gen,chronicdz.gen,ble.gen), 
                     by=c("METHOD","SITE","SITEVISITID","TRANSECT","SEGMENT","GENUS_CODE"), type='full')
head(data.gen)
length(unique(data.gen$SITE))

#LOOK AT NAS IN SEGMENTS AND SEG AREA
View(data.gen)


#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.gen$JuvColCount[is.na(data.gen$JuvColCount)]<-0;data.gen$JuvColDen[is.na(data.gen$JuvColDen)]<-0
data.gen$AdColCount[is.na(data.gen$AdColCount)]<-0;data.gen$AdColDen[is.na(data.gen$AdColDen)]<-0

data.gen$JuvColCount<-ifelse(data.gen$SEGMENT==15,NA,data.gen$JuvColCount) #Segment 15 wasn't surveyed for juvs
data.gen$JuvColDen<-ifelse(data.gen$SEGMENT==15,NA,data.gen$JuvColDen) #Segment 15 wasn't surveyed for juvs


#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.gen$AcuteDZ_prev<-(data.gen$AcuteDZ*data.gen$SEGAREA_ad)/data.gen$AdColCount*100
data.gen$BLE_prev<-(data.gen$BLE*data.gen$SEGAREA_ad)/data.gen$AdColCount*100
data.gen$ChronicDZ_prev<-(data.gen$ChronicDZ*data.gen$SEGAREA_ad)/data.gen$AdColCount*100

View(data.gen)

#Add adult and juvenile pres/ab columns
data.gen$Adpres.abs<-ifelse(data.gen$AdColDen>0,1,0)
data.gen$Juvpres.abs<-ifelse(data.gen$JuvColDen>0,1,0)

#We only surveyed 1 transect/site so data.gen is site-level data
if(length(unique(data.gen$TRANSECT))>1) {cat("WARNING:MORE THAN 1 TRANSECT/SITE IN DF")} #Check that adult data weren't dropped  
seg.data.gen2<-dplyr::select(data.gen,-(TRANSECT))

#Merge site data with metadata

# Merge Site level data with sectors file and export site data ------------
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

#Merge together survey meta data and sector area files and check for missmatches 
meta<-left_join(survey_segment,sectors)
meta[which(is.na(meta$AREA_HA)),]
nrow(survey_segment)
nrow(meta)


#Merge site level data and meta data
seg.data.gen2<-left_join(seg.data.gen2,meta);head(seg.data.gen2)



# # GENERATE SUMMARY METRICS at the SITE-leveL BY GENUS--------------------------------------------------
# 
# #Calc_ColDen_Transect
# acd.gen<-Calc_ColDen_Transect(data = ad_sfm,grouping_field = "GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen";colnames(acd.gen)[colnames(acd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_ad"# calculate density at genus level as well as total
# jcd.gen<-Calc_ColDen_Transect(j_sfm,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen";colnames(jcd.gen)[colnames(jcd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_j"
# 
# 
# ## This function calculates mean colony length, % recent dead, % old dead, condition severity or condition extent to the segment level
# ## NOTE: can run both adult & juvenile data with this function for COLONYLENGTH
# #c("COLONYLENGTH","RDEXTENT1", "RDEXTENT2", "RDEXTENT3", "OLDDEAD","SEVERITY_1","SEVERITY_2", "SEVERITY_3", "EXTENT_1", "EXTENT_2", "EXTENT_3")
# ex_b<-subset(ad_sfm,EX_BOUND==0)
# cl.gen<-Calc_ColMetric_Transect(data = ex_b,grouping_field = "GENUS_CODE",pool_fields = "COLONYLENGTH"); colnames(cl.gen)[colnames(cl.gen)=="Ave.y"]<-"Ave.size" #Average % old dead
# od.gen<-Calc_ColMetric_Transect(data = ad_sfm,grouping_field = "GENUS_CODE",pool_fields = "OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
# rd.gen<-Calc_ColMetric_Transect(data = ad_sfm,grouping_field = "GENUS_CODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead
# 
# 
# #Calc_RDden_Transect
# rdden.gen<-Calc_RDden_Transect(data=ad_sfm,grouping_field ="GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
# acutedz.gen<-subset(rdden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,GENUS_CODE,DZGN_G));colnames(acutedz.gen)[colnames(acutedz.gen)=="DZGN_G"]<-"AcuteDZ" #subset just acute diseased colonies
# 
# 
# #Calc_CONDden_Transect
# condden.gen<-Calc_CONDden_Transect(data=ad_sfm,grouping_field ="GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
# ble.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,GENUS_CODE,BLE));colnames(ble.gen)[colnames(ble.gen)=="BLE"]<-"BLE" #subset just bleached colonies
# chronicdz.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,GENUS_CODE,CHRO));colnames(chronicdz.gen)[colnames(chronicdz.gen)=="CHRO"]<-"ChronicDZ"
# 
# #Calc_Richness_Transect
# #rich.gen<-Calc_Richness_Transect(ad_sfm,"GENUS_CODE")
# 
# 
# #Join density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
# data.gen <- join_all(list(acd.gen,jcd.gen,cl.gen,od.gen,rd.gen,acutedz.gen,chronicdz.gen,ble.gen), 
#                      by=c("METHOD","SITE","SITEVISITID","TRANSECT","GENUS_CODE"), type='full')
# head(data.gen)
# length(unique(data.gen$SITE))
# 
# #Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
# data.gen$JuvColCount[is.na(data.gen$JuvColCount)]<-0;data.gen$JuvColDen[is.na(data.gen$JuvColDen)]<-0
# data.gen$AdColCount[is.na(data.gen$AdColCount)]<-0;data.gen$AdColDen[is.na(data.gen$AdColDen)]<-0
# 
# 
# #Calculate transect level prevalence for acute dz, chronic dz and bleaching
# data.gen$AcuteDZ_prev<-(data.gen$AcuteDZ*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100
# data.gen$BLE_prev<-(data.gen$BLE*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100
# data.gen$ChronicDZ_prev<-(data.gen$ChronicDZ*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100
# 
# View(data.gen)
# 
# #Add adult and juvenile pres/ab columns
# data.gen$Adpres.abs<-ifelse(data.gen$AdColDen>0,1,0)
# data.gen$Juvpres.abs<-ifelse(data.gen$JuvColDen>0,1,0)
# 
# #We only surveyed 1 transect/site so data.gen is site-level data
# if(length(unique(data.gen$TRANSECT))>1) {cat("WARNING:MORE THAN 1 TRANSECT/SITE IN DF")} #Check that adult data weren't dropped  
# site.data.gen2<-dplyr::select(data.gen,-(TRANSECT))
# 
# #Merge site data with metadata
# 
# # Merge Site level data with sectors file and export site data ------------
# sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)
# 
# #Merge together survey meta data and sector area files and check for missmatches 
# meta<-left_join(survey_site,sectors)
# meta[which(is.na(meta$AREA_HA)),]
# nrow(survey_site)
# nrow(meta)
# 
# 
# #Merge site level data and meta data
# site.data.gen2<-left_join(site.data.gen2,meta);head(site.data.gen2)

# 
# #Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
# site.data.gen2$DB_RZ<-paste(site.data.gen2$DEPTH_BIN,site.data.gen2$REEF_ZONE,sep="_")
# site.data.gen2$STRATANAME<-paste(site.data.gen2$SEC_NAME,site.data.gen2$DB_RZ,sep="_")
# site.data.gen2$ANALYSIS_SCHEMA<-site.data.gen2$STRATANAME
# site.data.gen2$DOMAIN_SCHEMA<-site.data.gen2$SEC_NAME
# site.data.gen2$ANALYSIS_YEAR<-site.data.gen2$OBS_YEAR
# 
# 
# # Calculate STATA-level Estimates for SFM --------------------------------
# 
# #Create a vector of columns to subset for strata estimates
# # c.keep<-c("REGION","DOMAIN_SCHEMA","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
# #           "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
# c.keep2<-c("REGION","DOMAIN_SCHEMA","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
#            "n_h","N_h","D._h","SE_D._h")
# # c.keep3<-c("REGION","DOMAIN_SCHEMA","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
# #            "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
# c.keep4<-c("REGION","DOMAIN_SCHEMA","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
#            "n_h","N_h","prev","SEprev")
# 
# 
# acdG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","AdColDen","Adpres.abs");acdG_st=acdG_st[,c.keep2]
# colnames(acdG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","AdColDen","SE_AdColDen")
# 
# jcdG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","JuvColDen","Juvpres.abs");jcdG_st=jcdG_st[,c.keep2]
# colnames(jcdG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","JuvColDen","SE_JuvColDen")
# 
# odG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.od");odG_st=odG_st[,c.keep2]
# colnames(odG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.od","SE_Ave.od")
# 
# rdG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.rd");rdG_st=rdG_st[,c.keep2]
# colnames(rdG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.rd","SE_Ave.rd")
# 
# clG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.size");clG_st=clG_st[,c.keep2]
# colnames(clG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.size","SE_Ave.size")
# 
# BLEG_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","BLE");BLEG_st=BLEG_st[,c.keep4]
# colnames(BLEG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","BLE","SE_BLE")
# 
# AcuteDZG_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","AcuteDZ");AcuteDZG_st=AcuteDZG_st[,c.keep4]
# colnames(AcuteDZG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","AcuteDZ_Prev","SE_AcuteDZ_Prev")
# 
# ChronicDZG_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","ChronicDZ");ChronicDZG_st=ChronicDZG_st[,c.keep4]
# colnames(ChronicDZG_st)<-c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","ChronicDZ_Prev","SE_ChronicDZ_Prev")
# 
# 
# #Double Check that revised pooling is adding up NH (total sites) correctly
# View(acdG_st)
# View(sectors)
# 
# 
# # Calculate SECTOR-level Estimates for SFM --------------------------------
# acdG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","AdColDen","Adpres.abs")
# acdG_sec<-acdG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AdColDen","SE_AdColDen")]
# jcdG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","JuvColDen","Juvpres.abs")
# jcdG_sec<-jcdG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen")]
# odG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.od")
# odG_sec<-odG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.od","SE_Ave.od")]
# rdG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.rd")
# rdG_sec<-rdG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.rd","SE_Ave.rd")]
# clG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.size")
# clG_sec<-clG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.size","SE_Ave.size")]
# bleG_sec<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","BLE")
# bleG_sec<-bleG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_BLE_Prev","SE_BLE_Prev")]
# AcuteDZG_sec<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","AcuteDZ")
# AcuteDZG_sec<-AcuteDZG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AcuteDZ_Prev","SE_AcuteDZ_Prev")]
# ChronicDZG_sec<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","ChronicDZ")
# ChronicDZG_sec<-ChronicDZG_sec[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_ChronicDZ_Prev","SE_ChronicDZ_Prev")]
# 
# 
# MyMerge <- function(x, y){
#   df <- merge(x, y, by= c("REGION","Sector","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
#   return(df)
# }
# sfm.strat<-Reduce(MyMerge, list(acdG_st,jcdG_st,odG_st,rdG_st,clG_st,BLEG_st,AcuteDZG_st,ChronicDZG_st))
# 
# 
# MyMerge <- function(x, y){
#   df <- merge(x, y, by= c("REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
#   return(df)
# }
# sfm.sector<-Reduce(MyMerge, list(acdG_sec,jcdG_sec,odG_sec,rdG_sec,clG_sec,bleG_sec,AcuteDZG_sec,ChronicDZG_sec))
# colnames(sfm.sector)[colnames(sfm.sector)=="DOMAIN_SCHEMA"]<-"Sector"


# #Save file for method comparsion
write.csv(seg.data.gen2,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_GENUS_SEGMENT.csv",row.names = F)
# write.csv(site.data.gen2,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMGENUS_SITE.csv",row.names = F)
# write.csv(sfm.strat,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMGENUS_STRATA.csv",row.names = F)
# write.csv(sfm.sector,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMGENUS_SECTOR.csv",row.names = F)


