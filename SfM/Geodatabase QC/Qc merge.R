rm(list=ls())
setwd("T:/Benthic/Data/SfM/QC")

sfm<-read.csv("HARAMP2019_FINAL_QCd_geodatabase.csv")




#SfM/ADULT: Merge Adult data and  SURVEY MASTER  -------------------------------------
survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")


colnames(survey_master)[colnames(survey_master)=="new_MIN_DEPTH_M"]<-"MIN_DEPTH_M" #Change column name
colnames(survey_master)[colnames(survey_master)=="new_MAX_DEPTH_M"]<-"MAX_DEPTH_M" #Change column name
colnames(survey_master)[colnames(survey_master)=="LATITUDE_SV"]<-"LATITUDE" #Change column name
colnames(survey_master)[colnames(survey_master)=="LONGITUDE_SV"]<-"LONGITUDE" #Change column name


sfm<-left_join(sfm,survey_master[,c("MISSIONID","REGION","OBS_YEAR","ISLAND","SITEVISITID","SITE","SEC_NAME",
                                "REEF_ZONE","DEPTH_BIN","HABITAT_CODE","LATITUDE","LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")])

head(x)
if(nrow(x)!=nrow(x)) {cat("WARNING:Data were dropped")} #Check that adult data weren't dropped  


sfm$COLONYID<-c(1:length(sfm$FID))

#Separate by adults and juveniles
ad<-subset(sfm,JUVENILE==0|REMNANT==-1)
ad<-subset(ad,select=-c(FID,totaldead,SEGAREA))
j<-subset(sfm,JUVENILE=="-1") # includes segments where NO_COLONY = -1
j<-subset(j,select=c(COLONYID,ANALYST,OBS_YEAR,MISSION_ID,SITE,TRANSECT,SEGMENT,SEGLENGTH,SEGWIDTH,NO_COLONY,SPCODE,FRAGMENT,MORPH_CODE,
                     EX_BOUND,JUVENILE,FRAGMENT,REMNANT,Shape_Leng,SEGAREA))






#Export QC'd data
#Data ends up in "T:/Benthic/Data/SfM/QC" NOT within Benthic-Scripts Github folder
# setwd('T:/Benthic/Data/SfM/QC/')
write.csv(ad,"HARAMP2019_QCdsfm_ADULT_v3.csv",row.names = F)
write.csv(j,"HARAMP2019_QCdsfm_JUV_v3.csv",row.names = F)

