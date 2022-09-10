#This script reads in the diver and SfM-generated demographic data that has been QC'd and cleaned up
#Then generates segment-level summarized that for methods comparision

rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Jonathan.Charendoff/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Jonathan.Charendoff/Documents/GitHub/fish-paste/lib/GIS_functions.R")
# source("C:/Users/Corinne.Amir/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
# source("C:/Users/Corinne.Amir/Documents/GitHub/fish-paste/lib/core_functions.R")
# source("C:/Users/Corinne.Amir/Documents/GitHub/fish-paste/lib/GIS_functions.R")
source("C:/Users/jonathan.charendoff/Downloads/core_functions.R")

#Read in files
ad_sfm<-read.csv("T:/Benthic/Data/SfM/Calibration QC/MARAMP22_SfMAdult_CLEANED.csv")
j_sfm<-read.csv("T:/Benthic/Data/SfM/Calibration QC/MARAMP22_SfMJuv_CLEANED.csv") 


#Check number of unique site-segments
t1<-ddply(ad_sfm,.(SITE,SEGMENT),summarize,n=length(unique(ANALYST)))
t2<-ddply(j_sfm,.(SITE,SEGMENT),summarize,n=length(unique(ANALYST)))
t1
t2

#We are missing some annotator data from seg HAW-4294 5 and 10, remove these for now.
#ad_sfm<-subset(ad_sfm,SITE != c("HAW-04294"))
#j_sfm<-subset(j_sfm,SITE != "HAW-04294")


###FOR CALIBRATION: Use this script to assign transect
#Create Transect column and use this to code duplicate segments
ad_sfm<-ad_sfm %>% mutate(TRANSECT=recode(ANALYST,
                                        `JC`="1",
                                        `MSL`="2",
                                        `NBO`="3",
                                        `NA`="NA"))
#Check that segments were changed correctly
ad_sfm<-droplevels(ad_sfm)
table(ad_sfm$SITE,ad_sfm$TRANSECT)

#Create Transect column and use this to code duplicate segments
j_sfm<-j_sfm %>% mutate(TRANSECT=recode(ANALYST,
                                        `JC`="1",
                                        `MSL`="2",
                                        `NBO`="3",
                                        `NA`="NA"))
#Check that segments were changed correctly
j_sfm<-droplevels(j_sfm)
table(j_sfm$SITE,j_sfm$TRANSECT)




##Calcuate segment and transect area and add column for transect area
ad_sfm$MISSIONID<-"RA2201"
j_sfm$MISSIONID<-"RA2201"

ad_sfm$TRANSECTAREA<-Transectarea(ad_sfm) #check if needed
j_sfm$TRANSECTAREA<-Transectarea(j_sfm) #check if needed

colnames(ad_sfm)[colnames(ad_sfm)=="FRAGMENT"]<-"Fragment"
colnames(j_sfm)[colnames(j_sfm)=="FRAGMENT"]<-"Fragment"

#Check if any site-segments have been dropped 
t1<-ddply(ad_sfm,.(SITE,SEGMENT),summarize,n=length(unique(ANALYST)));nrow(t1[t1$n>1,])
t2<-ddply(j_sfm,.(SITE,SEGMENT),summarize,n=length(unique(ANALYST)));nrow(t1[t1$n>1,]) 

########################### 8/25/22
#Create a look up table of all of the colony attributes- you will need this for the functions below
SURVEY_COL<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony<-unique(ad_sfm[,SURVEY_COL])

SURVEY_SITE<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")
survey_site<-unique(ad_sfm[,SURVEY_SITE])

SURVEY_Seg<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN","HABITAT_CODE", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","METHOD","TRANSECT","SEGMENT")
survey_segment<-unique(ad_sfm[,SURVEY_Seg])

#Combine juvenile and adult data
aj_sfm<-full_join(ad_sfm,j_sfm) #fixes NA problem


#Create a site list containing list of sites double surveyed by sfm and divers
aj_sfm$MethodRep <- paste0(aj_sfm$METHOD,"_",aj_sfm$TRANSECT)
aj_sfm$SS <- paste0(aj_sfm$SITE,"_",aj_sfm$SEGMENT)

seglist <- as.data.frame.matrix(table(aj_sfm$SS, aj_sfm$MethodRep));dim(seglist)
seglist$SS <-rownames(seglist)
seglist<-seglist %>% filter(seglist$DIVER_1!=0);dim(seglist)
seglist<-seglist %>% filter(seglist$DIVER_2!=0);dim(seglist)
seglist<-seglist %>% filter(seglist$SfM_1!=0);dim(seglist)
seglist<-seglist %>% filter(seglist$SfM_2!=0);dim(seglist)
#Once filtering is completed, nrow should = 43 (FOR COMPARISON)

write.csv(seglist,file="T:/Benthic/Data/SfM/Calibration QC/Comparison_seglist.csv",row.names = F)

# 
# #Remove site-segments that are not present among all methods
# dim(aj_sfm)
# aj_sfm<-subset(aj_sfm,SS%in%seglist$SS)
# dim(aj_sfm)
# length(unique(aj_sfm$SS))
# table(aj_sfm$SS,aj_sfm$METHOD)




# GENERATE SUMMARY METRICS at the Segment-leveL BY GENUS--------------------------------------------------
#REMOVE COLONIES THAT COULD'T BE FULLY ANNOTATED IN SFM
ad_sfm<-subset(ad_sfm,EX_BOUND==0) #don't worry about for now


#Calc_ColDen_Transect
acd.gen<-Calc_ColDen_Seg(data = ad_sfm,grouping_field = "GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen";colnames(acd.gen)[colnames(acd.gen)=="SEGAREA"]<-"SEGAREA_ad"# calculate density at genus level as well as total
jcd.gen<-Calc_ColDen_Seg(j_sfm,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen"
jcd.gen<-subset(jcd.gen,select=-c(SEGAREA))


## This function calculates mean colony length, % recent dead, % old dead, condition severity or condition extent to the segment level
## NOTE: can run both adult & juvenile data with this function for COLONYLENGTH
#c("COLONYLENGTH","RDEXTENT1", "RDEXTENT2", "RDEXTENT3", "OLDDEAD","SEVERITY_1","SEVERITY_2", "SEVERITY_3", "EXTENT_1", "EXTENT_2", "EXTENT_3")
cl.gen<-Calc_ColMetric_Seg(data = ad_sfm,grouping_field = "GENUS_CODE",pool_fields = "COLONYLENGTH"); colnames(cl.gen)[colnames(cl.gen)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.gen<-Calc_ColMetric_Seg(data = ad_sfm,grouping_field = "GENUS_CODE",pool_fields = "OLD_DEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.gen<-Calc_ColMetric_Seg(data = ad_sfm,grouping_field = "GENUS_CODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead


#Calc_RDden_Transect

##Create prevalence for all of the different conditions/DZs look at density for each dZ then create prevalence values for each disease and condition

rdden.gen<-Calc_RDden_Seg(data=ad_sfm,grouping_field ="GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.gen<-subset(rdden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,DZGN_G));colnames(acutedz.gen)[colnames(acutedz.gen)=="DZGN_G"]<-"DZGN_G_den" #subset just acute diseased colonies


#Calc_CONDden_Transect
condden.gen<-Calc_CONDden_Seg(data=ad_sfm,grouping_field ="GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,BLE));colnames(ble.gen)[colnames(ble.gen)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,CHRO));colnames(chronicdz.gen)[colnames(chronicdz.gen)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies

#Calc_Richness_Transect
#rich.gen<-Calc_Richness_Transect(ad_sfm,"GENUS_CODE")


#Join density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
data.gen <- join_all(list(acd.gen,jcd.gen,cl.gen,od.gen,rd.gen,chronicdz.gen,ble.gen), 
                by=c("METHOD","SITE","SITEVISITID","TRANSECT","SEGMENT","GENUS_CODE"), type='full')
head(data.gen)


#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.gen$JuvColCount[is.na(data.gen$JuvColCount)]<-0;data.gen$JuvColDen[is.na(data.gen$JuvColDen)]<-0
data.gen$AdColCount[is.na(data.gen$AdColCount)]<-0;data.gen$AdColDen[is.na(data.gen$AdColDen)]<-0


#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.gen$DZGN_G_prev<-(data.gen$DZGN_G_den*data.gen$SEGAREA_ad)/data.gen$AdColCount*100
data.gen$BLE_prev<-(data.gen$BLE_den*data.gen$SEGAREA_ad)/data.gen$AdColCount*100
data.gen$CHRO_prev<-(data.gen$CHRO_den*data.gen$SEGAREA_ad)/data.gen$AdColCount*100

View(data.gen)


#Concatenate method, Site,Transect and segment
data.gen$MethodRep<-paste(data.gen$METHOD,data.gen$TRANSECT,sep="_")
data.gen$SS<-paste(data.gen$SITE,data.gen$SEGMENT,sep="_")


#Check that each site-segment remaining has 2 divers and 2 annotators
t1<-ddply(data.gen,.(SITE,SEGMENT),summarize,n=length(unique(MethodRep)));nrow(t1[t1$n==6,]) 

t1<-as.data.frame.matrix(table(data.gen$SS,data.gen$MethodRep));dim(t1)
t1<-t1%>%filter(t1$SfM_1!=0);dim(t1) 
t1<-t1%>%filter(t1$SfM_2!=0);dim(t1) 


#Make final dataframe to save
data.gen2<-left_join(data.gen,survey_segment)
if(nrow(data.gen)!=nrow(data.gen2)) {cat("WARNING: Dfs didn't merge properly")}

#Save file for segment calibration
write.csv(data.gen,file="T:/Benthic/Data/SfM/Calibration QC/MARAMP_repeats_GENUS_Summarized Data-CALIBRATION.csv",row.names = F)


