#This script reads in the diver and SfM-generated demographic data that has been QC'd and cleaned up
#Then generates segment-level summarized that for methods comparision

rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("T:/Benthic/Data/SfM/ScriptFiles/SfMvDiver Plotting Functions.R") 
source("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-scripts/Functions/core_functions.R")

## LOAD benthic data
# load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_ADULTCORAL_RAW_2013-2020.rdata") #from oracle

#Read in files

ad_sfm<-read.csv("T:/Benthic/Data/SfM/Analysis Ready/ASRAMP23_SfM_Adult_CLEANED.csv")
j_sfm<-read.csv("T:/Benthic/Data/SfM/Analysis Ready/ASRAMP23_SfM_Juv_CLEANED.csv") 
sectors<-read.csv("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)


#Double check that the number of segments in the geodatabase matches what annoators said they completed 
#metadata file manually assembled from the tracking sheet pulled from google drive 
meta<-read.csv("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/ASRAMP2023_SfM_Meta.csv")
meta<-meta[,c("ISLAND","SITE","Mosaic_Issues","Segments_Annotated","Rugosity")]
head(meta)

ad_sfm$SITE_SEG<-paste(ad_sfm$SITE,ad_sfm$SEGMENT,sep ="_")
j_sfm$SITE_SEG<-paste(j_sfm$SITE,j_sfm$SEGMENT,sep ="_")
length(unique(ad_sfm$SITE_SEG));length(unique(ad_sfm$SITE)) #should be 389 ss and 104 sites
length(unique(j_sfm$SITE_SEG));length(unique(j_sfm$SITE)) #should be 312 ss and 104 sites

ad_repeat.site.segs.sfm <- unique(ad_sfm$SITE_SEG[which(ad_sfm$TRANSECT == "A")])
j_repeat.site.segs.sfm <- unique(j_sfm$SITE_SEG[which(j_sfm$TRANSECT == "A")])

ad_sfm <- subset(ad_sfm, SITE_SEG %in% ad_repeat.site.segs.sfm)
j_sfm <- subset(j_sfm, SITE_SEG %in% j_repeat.site.segs.sfm)
ad_sfm$TRANSECT <- as.factor(ad_sfm$TRANSECT)
levels(ad_sfm$TRANSECT) <- c(1, 2)
j_sfm$TRANSECT <- as.factor(j_sfm$TRANSECT)
levels(j_sfm$TRANSECT) <- c(1, 2)

# PREP VISUAL DIVER DATA ---------------------------------------------
## LOAD benthic data/
awd_<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED.csv")
jwd_<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Juveniles_raw_CLEANED.csv")

awd<-subset(awd_,OBS_YEAR=="2023")
jwd<-subset(jwd_,OBS_YEAR=="2023")


#Changing Analyst names to match SfM
awd$DIVER<-ifelse(awd$DIVER=="J_E","JE",as.character(awd$DIVER))
awd$DIVER<-ifelse(awd$DIVER=="J_C","JC",as.character(awd$DIVER))
jwd$DIVER<-ifelse(jwd$DIVER=="J_E","JE",as.character(jwd$DIVER))
jwd$DIVER<-ifelse(jwd$DIVER=="J_C","JC",as.character(jwd$DIVER))


#Colony fragments and scleractinans are subseted in the functions 
#Add a column for adult fragments so we can remove them from the dataset later (-1 indicates fragment)
colnames(awd)[colnames(awd)=="Fragment"]<-"FRAGMENT"
awd$FRAGMENT[is.na(awd$FRAGMENT)] <- 0
jwd$FRAGMENT <- 0 # you need to add this column so that you can use the site level functions correctly

#Simplify Bleaching Severity categories: in 2019 the team decided to simplify the bleaching severity from 1-5 to 1-3 to improve consistency in severity values
#This code converts the severity data collected prior to 2019 to a 1-3 scale
awd$DATE_ <- as.Date(awd$DATE_, format = "%Y-%m-%d")
jwd$DATE_ <- as.Date(jwd$DATE_, format = "%Y-%m-%d")

colnames(awd)[which(colnames(awd) == 'DIVER')] <- "ANALYST"
colnames(jwd)[which(colnames(jwd) == 'DIVER')] <- "ANALYST"
awd$EX_BOUND<-0;awd$EX_BOUND<-as.numeric(awd$EX_BOUND) #add ex_bound columns to match SfM
jwd$EX_BOUND<-0;jwd$EX_BOUND<-as.numeric(jwd$EX_BOUND)



#Calculate segment area
awd$SEGAREA<-awd$SEGLENGTH*awd$SEGWIDTH
jwd$SEGAREA<-jwd$SEGLENGTH*jwd$SEGWIDTH

#Only include sites and segments surveyed by divers during HARAMP 2019
awd$SITE_SEG<-paste(awd$SITE,awd$SEGMENT,sep="_")
jwd$SITE_SEG<-paste(jwd$SITE,jwd$SEGMENT,sep="_")

awd <- subset(awd, SITE_SEG %in% ad_repeat.site.segs.sfm)
jwd <- subset(jwd, SITE_SEG %in% j_repeat.site.segs.sfm)

#remove columns that aren't needed & 
awd<-dplyr::select(awd,-c(TRANSECTAREA,bANALYSIS_SCHEME,ANALYSIS_YEAR,REGION_NAME,NO_SURVEY_YN,DATE_,ISLANDCODE))
jwd<-dplyr::select(jwd,-c(TRANSECTAREA,bANALYSIS_SCHEME,ANALYSIS_YEAR,REGION_NAME,NO_SURVEY_YN,DATE_,ISLANDCODE))
awd<-dplyr::filter(awd, SITE_SEG %in% c(ad_sfm$SITE_SEG));head(awd) 
jwd<-dplyr::filter(jwd, SITE_SEG %in% c(j_sfm$SITE_SEG));head(jwd) 


##
length(unique(awd$SITE))
length(unique(jwd$SITE))


#Remove segments that were annoated in SfM, but not surveyd by divers
ad_sfm_sub<-dplyr::filter(ad_sfm, SITE_SEG %in% c(awd$SITE_SEG));nrow(ad_sfm);nrow(ad_sfm_sub)
j_sfm_sub<-dplyr::filter(j_sfm, SITE_SEG %in% c(jwd$SITE_SEG));nrow(j_sfm);nrow(j_sfm_sub)

length(unique(ad_sfm_sub$SITE));length(unique(ad_sfm_sub$SITE_SEG))
length(unique(j_sfm_sub$SITE));length(unique(j_sfm_sub$SITE_SEG))

# Add Method column to diver data
awd$METHOD<-"Diver";awd$METHOD<-as.factor(awd$METHOD)
jwd$METHOD<-"Diver";jwd$METHOD<-as.factor(jwd$METHOD)


#Merge diver and SfM data
sort(colnames(awd))
sort(colnames(ad_sfm_sub))
awd$HABITAT_CODE <- "AGR"
awd <- awd[,-17]

sort(colnames(jwd))
sort(colnames(j_sfm_sub))
jwd$HABITAT_CODE <- "AGR"
j_sfm_sub$MISSIONID <- "RA2301"

sapply(awd,class)
sapply(ad_sfm,class)

awd.all<-rbind(ad_sfm_sub,awd)
jwd.all<-rbind(j_sfm_sub,jwd)

#Drop segments that have <2.5 segarea for adults and <1 for juveniles
#awd.all<-dplyr::filter(awd.all, SEGAREA==2.5)
#jwd.all<-dplyr::filter(jwd.all, SEGAREA==1)

#Calculate transect area
awd.all$TRANSECTAREA<-TransectareaMETHOD(awd.all)
jwd.all$TRANSECTAREA<-TransectareaMETHOD(jwd.all)

#Double check transect areas
summary(jwd.all$TRANSECTAREA)
summary(awd.all$TRANSECTAREA)

#Remove juvenile colonies <0.7cm
tmp<-jwd.all %>% mutate_at(.vars = c("GENUS_CODE", "SPCODE","TAXONCODE"), 
                           list(~replace(.,COLONYLENGTH <0.7, "AAAA")));View(tmp)
tmp<-tmp %>% mutate_at(.vars = c("S_ORDER", "COLONYID","TAXONNAME"), 
                       list(~replace(.,TAXONCODE=="AAAA", NA)));View(tmp)
jwd.all<-tmp %>% mutate_at(.vars = c("COLONYLENGTH"), 
                           list(~replace(.,TAXONCODE=="AAAA", 0)));View(jwd.all)



colnames(awd.all)[colnames(awd.all)=="FRAGMENT"]<-"Fragment"
colnames(jwd.all)[colnames(jwd.all)=="FRAGMENT"]<-"Fragment"

awd.all$RD1[which(awd.all$RD1=="")]<- "NONE"
awd.all$RDEXTENT1[which(awd.all$RD1=="NONE")]<-0
awd.all$RD2[which(awd.all$RD2=="")]<- "NONE"
awd.all$RDEXTENT2[which(awd.all$RD2=="NONE")]<-0
awd.all$RD3[which(awd.all$RD3=="")]<- "NONE"
awd.all$RDEXTENT3[which(awd.all$RD3=="NONE")]<-0
awd.all$RD3[which(is.na(awd.all$RD3))]<- "NONE"
awd.all$SEGWIDTH[which(is.na(awd.all$SEGWIDTH))] <- 1
awd.all$CONDITION_1[which(awd.all$CONDITION_1=="")]<- "NONE"
awd.all$EXTENT_1[which(awd.all$CONDITION_1=="NONE")]<-0
awd.all$CONDITION_2[which(awd.all$CONDITION_2=="")]<- "NONE"
awd.all$EXTENT_2[which(awd.all$CONDITION_2=="NONE")]<-0
awd.all$CONDITION_3[which(awd.all$CONDITION_3=="")]<- "NONE"
awd.all$EXTENT_3[which(awd.all$CONDITION_3=="NONE")]<-0
awd.all$CONDITION_3[which(is.na(awd.all$CONDITION_3))]<- "NONE"
awd.all$EXTENT_3[which(is.na(awd.all$CONDITION_3))]<-0

#awd.all <- awd.all[-which(awd.all$ANALYST == "JDG" & awd.all$SITE_SEG == "ASC-00594_5"),]

#Create a look up table of all of the colony attributes- you will need this for the functions below
SURVEY_COL<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony<-unique(awd.all[,SURVEY_COL])

SURVEY_COL<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE", "ANALYST",
              "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony_DIVER<-unique(awd.all[,SURVEY_COL])

SURVEY_SITE<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")
survey_site<-unique(awd.all[,SURVEY_SITE])

SURVEY_Seg<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN","HABITAT_CODE", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","METHOD","TRANSECT","SEGMENT")
survey_segment<-unique(awd.all[,SURVEY_Seg])
SURVEY_Seg<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN","HABITAT_CODE", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","METHOD","TRANSECT","SEGMENT")
survey_segment_j<-unique(jwd.all[,SURVEY_Seg])

#Combine juvenile and adult data
ajwd<-full_join(awd.all,jwd.all) #fixes NA problem


#Create a site list containing list of sites double surveyed by sfm and divers
ajwd$MethodRep <- paste0(ajwd$METHOD,"_",ajwd$TRANSECT)
ajwd$SS <- paste0(ajwd$SITE,"_",ajwd$SEGMENT)

seglist <- as.data.frame.matrix(table(ajwd$SITE_SEG, ajwd$MethodRep));dim(seglist)
seglist$SS <-rownames(seglist)
seglist<-seglist %>% filter(seglist$DIVER_1!=0);dim(seglist)
seglist<-seglist %>% filter(seglist$DIVER_2!=0);dim(seglist)
seglist<-seglist %>% filter(seglist$SfM_1!=0);dim(seglist)
seglist<-seglist %>% filter(seglist$SfM_2!=0);dim(seglist)
#Once filtering is completed, nrow should = 43 (FOR COMPARISON)

write.csv(seglist,file="T:/Benthic/Data/SfM/Summarized Data/Comparison_seglist.csv",row.names = F)


#Remove site-segments that are not present among all methods
dim(ajwd)
ajwd<-subset(ajwd,SS%in%seglist$SS)
dim(ajwd)
length(unique(ajwd$SS))
table(ajwd$SS,ajwd$METHOD)
length(unique(awd$SS))

awd.all$GENRD1[awd.all$RD1 == "NONE"] <- "NONE"
awd.all$GENRD2[awd.all$RD2 == "NONE"] <- "NONE"
awd.all$GENRD3[awd.all$RD3 == "NONE"] <- "NONE"
awd.all$GENRD1[awd.all$RD1 == "FISH" | awd.all$RD1 == "GAST"] <- "PRED"
awd.all$GENRD1[awd.all$RD1 == "UNKN"] <- "UNKN"

# GENERATE SUMMARY METRICS at the Segment-leveL BY GENUS--------------------------------------------------
#REMOVE COLONIES THAT COULD'T BE FULLY ANNOTATED IN SFM
awd.all<-subset(awd.all,EX_BOUND==0)

#Calc_ColDen_Transect
acd.gen<-Calc_ColDen_Seg_DIVER(data = awd.all,grouping_field = "GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen";colnames(acd.gen)[colnames(acd.gen)=="SEGAREA"]<-"SEGAREA_ad"# calculate density at genus level as well as total
jcd.gen<-Calc_ColDen_Seg_DIVER(jwd.all,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen"
colnames(jcd.gen)[colnames(jcd.gen)=="SEGAREA"]<-"SEGAREA_j"


## This function calculates mean colony length, % recent dead, % old dead, condition severity or condition extent to the segment level
## NOTE: can run both adult & juvenile data with this function for COLONYLENGTH
#c("COLONYLENGTH","RDEXTENT1", "RDEXTENT2", "RDEXTENT3", "OLDDEAD","SEVERITY_1","SEVERITY_2", "SEVERITY_3", "EXTENT_1", "EXTENT_2", "EXTENT_3")
cl.gen<-Calc_ColMetric_Seg_DIVER(data = awd.all,grouping_field = "GENUS_CODE",pool_fields = "COLONYLENGTH"); colnames(cl.gen)[colnames(cl.gen)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.gen<-Calc_ColMetric_Seg_DIVER(data = awd.all,grouping_field = "GENUS_CODE",pool_fields = "OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.gen<-Calc_ColMetric_Seg_DIVER(data = awd.all,grouping_field = "GENUS_CODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead


#Calc_RDden_Transect
rdden.gen<-Calc_RDden_Seg_DIVER(data=awd.all,survey_colony_f=survey_colony_DIVER,grouping_field ="GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.gen<-subset(rdden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,DZGN_G));colnames(acutedz.gen)[colnames(acutedz.gen)=="DZGN_G"]<-"DZGN_G_den" #subset just acute diseased colonies


#Calc_CONDden_Transect
condden.gen<-Calc_CONDden_Seg_DIVER(data=awd.all,grouping_field ="GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,BLE));colnames(ble.gen)[colnames(ble.gen)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,CHRO));colnames(chronicdz.gen)[colnames(chronicdz.gen)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies

#Calc_Diversity_Seg_DIVER
div.data<-Calc_Diversity_Seg_DIVER(awd.all,"TAXONCODE")


#Join density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
data.gen <- join_all(list(acd.gen,cl.gen,od.gen,rd.gen,acutedz.gen,chronicdz.gen,ble.gen), 
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
jcd.gen$MethodRep<-paste(jcd.gen$METHOD,jcd.gen$TRANSECT,sep="_")
jcd.gen$SS<-paste(jcd.gen$SITE,jcd.gen$SEGMENT,sep="_")
div.data$MethodRep<-paste(div.data$METHOD,div.data$TRANSECT,sep="_")
div.data$SS<-paste(div.data$SITE,div.data$SEGMENT,sep="_")

#Check that each site-segment remaining has 2 divers and 2 annotators
t1<-ddply(data.gen,.(SITE,SEGMENT),summarize,n=length(unique(MethodRep)));nrow(t1[t1$n==4,]) 

t1<-as.data.frame.matrix(table(data.gen$SS,data.gen$MethodRep));dim(t1)
t1<-t1%>%filter(t1$DIVER_2!=0);dim(t1)
t1<-t1%>%filter(t1$DIVER_1!=0);dim(t1)
t1<-t1%>%filter(t1$SfM_1!=0);dim(t1) 
t1<-t1%>%filter(t1$SfM_2!=0);dim(t1) 

t1<-ddply(div.data,.(SITE,SEGMENT),summarize,n=length(unique(MethodRep)));nrow(t1[t1$n==4,]) 

t1<-as.data.frame.matrix(table(div.data$SS,div.data$MethodRep));dim(t1)
t1<-t1%>%filter(t1$DIVER_2!=0);dim(t1)
t1<-t1%>%filter(t1$DIVER_1!=0);dim(t1)
t1<-t1%>%filter(t1$SfM_1!=0);dim(t1) 
t1<-t1%>%filter(t1$SfM_2!=0);dim(t1) 


#Make final dataframe to save
data.gen2<-left_join(data.gen,survey_segment)
if(nrow(data.gen)!=nrow(data.gen2)) {cat("WARNING: Dfs didn't merge properly")}

div.data2<-left_join(div.data,survey_segment)

jcd.gen2<-left_join(jcd.gen,survey_segment_j)
if(nrow(div.data)!=nrow(div.data2)) {cat("WARNING: Dfs didn't merge properly")}

#Save file for larger comparative analysis
write.csv(data.gen2,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/MARAMP_repeats_GENUS_Summarized Data.csv",row.names = F)
write.csv(jcd.gen2,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/MARAMP_repeats_GENUS_JUVS_Summarized Data.csv",row.names = F)
write.csv(div.data2,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/MARAMP_repeats_DIVERSITY_Summarized Data.csv",row.names = F)



