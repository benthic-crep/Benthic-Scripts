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



t1<-ddply(ad_diver,.(SITE,TRANSECT,SEGMENT),summarize,n=length(unique(DIVER)));t1[t1$n>1,]
t1<-ddply(j_diver,.(SITE,TRANSECT,SEGMENT),summarize,n=length(unique(DIVER)));t1[t1$n>1,]

#Temporary fixes - WORK WITH MICHAEL TO FIX IN ORACLE
ad_diver<-ad_diver[!(ad_diver$SITE=="HAW-04239" & ad_diver$SEGMENT=="10" & ad_diver$DIVER=="RS"),] #these were double entered under 2 different site names
j_diver<-j_diver[!(j_diver$SITE=="HAW-04239" & j_diver$SEGMENT=="10" & j_diver$DIVER=="RS"),] #these were double entered under 2 different site names
j_diver<-j_diver[!(j_diver$SITE=="NII-02580" & j_diver$SEGMENT=="10" & j_diver$DIVER=="MA"),] #Entered by mistake

###MOL-2255 SEGMENT 10 ADULT DATA WASN'T ENTERED-have Michael add it.
###HAW-04239 SEGMENT 0 ADULT DATA WASN'T ENTERED-have Michael add it.

j_diver$DIVER<-ifelse(j_diver$SITE=="KAU-02164" & j_diver$SEGMENT=="0" & j_diver$DIVER=="M_A","MSW",as.character(j_diver$DIVER))

ad_sfm<-ad_sfm[!(ad_sfm$SITE=="HAW-04259" & ad_sfm$SEGMENT=="10"),]  # These were annotated by mistake, we didn't do in water repeats
j_sfm<-j_sfm[!(j_sfm$SITE=="HAW-04259" & j_sfm$SEGMENT=="10"),]  # These were annotated by mistake, we didn't do in water repeats

ad_sfm<-ad_sfm[!(ad_sfm$SITE=="HAW-04294" & ad_sfm$SEGMENT=="10"),] # These were annotated by mistake, we didn't do in water repeats
j_sfm<-j_sfm[!(j_sfm$SITE=="HAW-04294" & j_sfm$SEGMENT=="10"),] # These were annotated by mistake, we didn't do in water repeats
j_sfm<-j_sfm[!(j_sfm$SITE=="HAW-04299" & j_sfm$SEGMENT=="15"),] # These were annotated by mistake, we didn't do in water repeats

ad_sfm<-ad_sfm[!(ad_sfm$SITE=="HAW-04278" & ad_sfm$SEGMENT %in% c("10","15")),] # These were annotated by mistake, we didn't do in water repeats
j_sfm<-j_sfm[!(j_sfm$SITE=="HAW-04278" & j_sfm$SEGMENT %in% c("10","15")),] # These were annotated by mistake, we didn't do in water repeats


###FOR CALIBRATION: Use this script to assign transect
#Create Transect column and use this to code duplicate segments
# ad_sfm<-ad_sfm %>% mutate(TRANSECT=recode(ANALYST, 
#                                 `MA`="1",
#                                 `RS`="2",
#                                 `MW`="2", 
#                                 `CA`="3",
#                                 `NA`="NA"))
# #Check that segments were changed correctly
# ad_sfm<-droplevels(ad_sfm)
# table(ad_sfm$SITE,ad_sfm$TRANSECT)
# 
# #Create Transect column and use this to code duplicate segments
# j_sfm<-j_sfm %>% mutate(TRANSECT=recode(ANALYST, 
#                                           `MA`="1",
#                                           `RS`="2",
#                                           `MW`="2", 
#                                           `CA`="3",
#                                           `NA`="NA"))
# #Check that segments were changed correctly
# j_sfm<-droplevels(j_sfm)
# table(j_sfm$SITE,j_sfm$TRANSECT)


##FOR COMPARATIVE ANALYSIS: Use this script to assign transect
#Adults
ad_sfm<-ad_sfm[!(ad_sfm$ANALYST=="CA"),] # These were annotated by mistake, we didn't do in water repeats
table(ad_sfm$SITE,ad_sfm$ANALYST)

SURVEY_Seg<-c("SITEVISITID", "SITE","SEGMENT","ANALYST")
sfm_seg<-unique(ad_sfm[,SURVEY_Seg])
sfm_seg$SS<- paste(sfm_seg$SITE,sfm_seg$SEGMENT,sep="_")  

###THIS ISN'T WORKING FOR
#Randomly assign annotators(Transects)
tr<-c("1","2")
sfm_seg <- sfm_seg %>%
  group_by(SS) %>% # note the group_by()
  mutate(TRANSECT=sample(tr, size=n(),  replace=F))
sfm_seg<-as.data.frame(sfm_seg)
table(sfm_seg$SS,sfm_seg$TRANSECT) #Check

nrow(ad_sfm)
ad_sfm<-left_join(ad_sfm,sfm_seg[,!(colnames(sfm_seg)=="SS")])
nrow(ad_sfm)

#Juveniles
j_sfm<-j_sfm[!(j_sfm$ANALYST=="CA"),] # These were annotated by mistake, we didn't do in water repeats
table(j_sfm$SITE,j_sfm$ANALYST)

SURVEY_Seg<-c("SITEVISITID", "SITE","SEGMENT","ANALYST")
sfm_seg<-unique(j_sfm[,SURVEY_Seg])
sfm_seg$SS<- paste(sfm_seg$SITE,sfm_seg$SEGMENT,sep="_")  

#Randomly assign Transect numbers
tr<-c("1","2")
sfm_seg <- sfm_seg %>%
  group_by(SS) %>% # note the group_by()
  mutate(TRANSECT=sample(tr, size=n(),  replace=F))
sfm_seg<-as.data.frame(sfm_seg)
table(sfm_seg$SS,sfm_seg$TRANSECT)


nrow(j_sfm)
j_sfm<-left_join(j_sfm,sfm_seg[,!(colnames(sfm_seg)=="SS")])
nrow(j_sfm)
head(j_sfm)

##Calcuating segment and transect area and add column for transect area
ad_sfm$TRANSECTAREA<-Transectarea(ad_sfm)
j_sfm$TRANSECTAREA<-Transectarea(j_sfm)


#Final Tweaks and merge adult and juv datasets-------------------------------------------------
#Add a column for adult fragments so we can remove them from the dataset later (-1 indicates fragment)
ad_diver$Fragment[is.na(ad_diver$Fragment)] <- 0
j_diver$Fragment <- 0 # you need to add this column so that you can use the site level functions correctly
ad_diver$EX_BOUND<-0 #add column so we can merge with sfm data
j_diver$EX_BOUND<-0 #add column so we can merge with sfm data

#Remove Porites bernardi, it's causing issues with duplicate segments.
ad_diver<-subset(ad_diver,SPCODE!="PBER")
j_diver<-subset(j_diver,SPCODE!="PBER")

colnames(ad_diver)[colnames(ad_diver)=="DIVER"]<-"ANALYST" #Change column so we can merge with the sfm data
colnames(j_diver)[colnames(j_diver)=="DIVER"]<-"ANALYST" 

ad_DATACOLS<-c("METHOD","ANALYST", "REGION","OBS_YEAR","MISSIONID","ISLAND","SEC_NAME","SITEVISITID","SITE","REEF_ZONE","DEPTH_BIN",
            "HABITAT_CODE","LATITUDE","LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","SEGLENGTH","SEGWIDTH",
            "SEGAREA","TRANSECTAREA","COLONYID","EX_BOUND","Fragment","S_ORDER","GENUS_CODE","SPCODE","TAXONCODE","TAXONNAME",
            "COLONYLENGTH","OLDDEAD","GENRD1","GENRD2","GENRD3","RD1","RDEXTENT1","RD2","RDEXTENT2","RD3",
            "RDEXTENT3","CONDITION_1","EXTENT_1","SEVERITY_1","CONDITION_2","EXTENT_2","SEVERITY_2","CONDITION_3","EXTENT_3","SEVERITY_3")

head(ad_diver[,ad_DATACOLS])
ad_diver<-ad_diver[,ad_DATACOLS]


j_DATACOLS<-c("METHOD","ANALYST", "REGION","OBS_YEAR","MISSIONID","ISLAND","SEC_NAME","SITEVISITID","SITE","REEF_ZONE","DEPTH_BIN",
        "HABITAT_CODE","LATITUDE","LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","SEGLENGTH","SEGWIDTH",
        "SEGAREA","TRANSECTAREA","COLONYID","EX_BOUND","Fragment","S_ORDER","GENUS_CODE","SPCODE","TAXONCODE","TAXONNAME",
        "COLONYLENGTH")         

head(j_diver[,j_DATACOLS])
j_diver<-j_diver[,j_DATACOLS]

#Combine diver and sfm data
awd<-rbind(ad_diver,ad_sfm)
jwd<-rbind(j_diver,j_sfm)

#Create a look a table of all of the colony attributes- you will need this the functions below
SURVEY_COL<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony<-unique(awd[,SURVEY_COL])

SURVEY_SITE<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")
survey_site<-unique(awd[,SURVEY_SITE])

SURVEY_Seg<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN","HABITAT_CODE", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","METHOD","TRANSECT","SEGMENT")
survey_segment<-unique(awd[,SURVEY_Seg])


# GENERATE SUMMARY METRICS at the Segment-leveL BY GENUS--------------------------------------------------
#Calc_ColDen_Transect
acd.gen<-Calc_ColDen_Seg(data = awd,grouping_field = "GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen";colnames(acd.gen)[colnames(acd.gen)=="SEGAREA"]<-"SEGAREA_ad"# calculate density at genus level as well as total
jcd.gen<-Calc_ColDen_Seg(jwd,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen"
jcd.gen<-subset(jcd.gen,select=-c(SEGAREA))

#REMOVE COLONIES THAT COULD'T BE FULLY ANNOTATED IN SFM
awd<-subset(awd,EX_BOUND==0)

#Calc_ColMetric_Transect
cl.gen<-Calc_ColMetric_Seg(data = awd,grouping_field = "GENUS_CODE",pool_fields = "COLONYLENGTH"); colnames(cl.gen)[colnames(cl.gen)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.gen<-Calc_ColMetric_Seg(data = awd,grouping_field = "GENUS_CODE",pool_fields = "OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.gen<-Calc_ColMetric_Seg(data = awd,grouping_field = "GENUS_CODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead

#Calc_RDden_Transect
rdden.gen<-Calc_RDden_Seg(data=awd,grouping_field ="GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.gen<-subset(rdden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,DZGN_G));colnames(acutedz.gen)[colnames(acutedz.gen)=="DZGN_G"]<-"DZGN_G_den" #subset just acute diseased colonies

#Calc_CONDden_Transect
condden.gen<-Calc_CONDden_Seg(data=awd,grouping_field ="GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,BLE));colnames(ble.gen)[colnames(ble.gen)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,CHRO));colnames(chronicdz.gen)[colnames(chronicdz.gen)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies

#Calc_Richness_Transect
#rich.gen<-Calc_Richness_Transect(awd,"GENUS_CODE")


#Merge density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("METHOD","SITE","SITEVISITID","TRANSECT","SEGMENT","GENUS_CODE"), all.x= TRUE, all.y= TRUE)
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

#Concatenate method, Site,Transect and segment
data.gen$MethodRep<-paste(data.gen$METHOD,data.gen$TRANSECT,sep="_")
data.gen$SS<-paste(data.gen$SITE,data.gen$SEGMENT,sep="_")

data.gen2<-left_join(data.gen,survey_segment)
if(nrow(data.gen)!=nrow(data.gen2)) {cat("WARNING: Dfs didn't merge properly")}

write.csv(data.gen2,file="T:/Benthic/Data/SfM/Summarized Data/HARAMP_repeats_GENUS_Summarized Data.csv",row.names = F)

