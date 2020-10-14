#This script reads in the diver and SfM-generated demographic data that has been QC'd and cleaned up
#Then generates segment-level summarized that for methods comparision

rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")
# source("C:/Users/Corinne.Amir/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
# source("C:/Users/Corinne.Amir/Documents/GitHub/fish-paste/lib/core_functions.R")
# source("C:/Users/Corinne.Amir/Documents/GitHub/fish-paste/lib/GIS_functions.R")

## LOAD benthic data
# load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_ADULTCORAL_RAW_2013-2020.rdata") #from oracle

#Read in files
ad_diver<-read.csv("T:/Benthic/Data/SfM/Analysis Ready/HARAMP19_DIVERAdult_CLEANED.csv")
j_diver<-read.csv("T:/Benthic/Data/SfM/Analysis Ready/HARAMP19_DIVERJuv_CLEANED.csv")
ad_sfm<-read.csv("T:/Benthic/Data/SfM/Analysis Ready/HARAMP19_SfMAdult_CLEANED.csv")
j_sfm<-read.csv("T:/Benthic/Data/SfM/Analysis Ready/HARAMP19_SfMJuv_CLEANED.csv") 


#Check number of unique site-segments....should all be (or at least end up being 43)
t1<-ddply(ad_sfm,.(SITE,SEGMENT),summarize,n=length(unique(ANALYST)));nrow(t1[t1$n>1,]) #44
t1<-ddply(j_sfm,.(SITE,SEGMENT),summarize,n=length(unique(ANALYST)));nrow(t1[t1$n>1,]) #44
t1<-ddply(ad_diver,.(SITE,SEGMENT),summarize,n=length(unique(DIVER)));nrow(t1[t1$n>1,]) #44 with at least 2 divers
t1<-ddply(j_diver,.(SITE,SEGMENT),summarize,n=length(unique(DIVER)));nrow(t1[t1$n>1,]) #35 with at least 2 divers


#Temporary fixes - WORK WITH MICHAEL TO FIX IN ORACLE
# ad_diver<-ad_diver[!(ad_diver$SITE=="HAW-04239" & ad_diver$SEGMENT=="10" & ad_diver$DIVER=="RS"),] #these were double entered under 2 different site names
# j_diver<-j_diver[!(j_diver$SITE=="HAW-04239" & j_diver$SEGMENT=="10" & j_diver$DIVER=="RS"),] #these were double entered under 2 different site names
# j_diver<-j_diver[!(j_diver$SITE=="NII-02580" & j_diver$SEGMENT=="10" & j_diver$DIVER=="M_A"),] #Entered by mistake
# 
# ###MOL-2255 SEGMENT 10 ADULT DATA WASN'T ENTERED-have Michael add it.
# ###HAW-04239 SEGMENT 0 ADULT DATA WASN'T ENTERED-have Michael add it.
# 
# j_diver$DIVER<-ifelse(j_diver$SITE=="KAU-02164" & j_diver$SEGMENT=="0" & j_diver$DIVER=="M_A","MSW",as.character(j_diver$DIVER)) 
# 
# j_diver$DIVER<-ifelse(j_diver$SITE=="HAW-04221" & j_diver$SEGMENT=="10" & j_diver$DIVER=="M_A","MSW",as.character(j_diver$DIVER)) #correct...? 
# j_diver$DIVER<-ifelse(j_diver$SITE=="HAW-03433" & j_diver$SEGMENT=="10" & j_diver$DIVER=="JDG","BVA",as.character(j_diver$DIVER))

ad_sfm<-ad_sfm[!(ad_sfm$SITE=="HAW-04259" & ad_sfm$SEGMENT=="10"),]  # These were annotated by mistake, we didn't do in water repeats
j_sfm<-j_sfm[!(j_sfm$SITE=="HAW-04259" & j_sfm$SEGMENT=="10"),]  # These were annotated by mistake, we didn't do in water repeats

#Make changes based on above QC.....FIX THESE PROBLEMS WITHIN SfMdemog_QC_correct.R
ad_sfm$SEGMENT<-ifelse(ad_sfm$SITE=="HAW-04294" & ad_sfm$ANALYST=="RS", 5,as.numeric(ad_sfm$SEGMENT)) #RS accidentally turned into segment = 10 (caused by 1357 problem)
j_sfm$SEGMENT<-ifelse(j_sfm$SITE=="HAW-04294" & j_sfm$ANALYST=="RS", 5,as.numeric(j_sfm$SEGMENT)) #RS accidentally turned into segment = 10 (caused by 1357 problem)
#j_sfm<-j_sfm[!(j_sfm$SITE=="HAW-04294" & j_sfm$SEGMENT=="10"),] # These were annotated by mistake, we didn't do in water repeats  ..............
#j_sfm<-j_sfm[!(j_sfm$SITE=="HAW-04299" & j_sfm$SEGMENT=="15"),] # These were annotated by mistake, we didn't do in water repeats  ..............
#ad_sfm<-ad_sfm[!(ad_sfm$SITE=="HAW-04299" & ad_sfm$SEGMENT=="15"),] # just double checking  ..............

ad_sfm<-ad_sfm[!(ad_sfm$SITE=="HAW-04278" & ad_sfm$SEGMENT %in% c("10","15")),] # These were annotated by mistake, we didn't do in water repeats
j_sfm<-j_sfm[!(j_sfm$SITE=="HAW-04278" & j_sfm$SEGMENT %in% c("10","15")),] # These were annotated by mistake, we didn't do in water repeats  ..............
ad_diver<-ad_diver[!(ad_diver$SITE=="HAW-04278" & ad_diver$SEGMENT %in% c("10","15")),]
j_diver<-j_diver[!(j_diver$SITE=="HAW-04278" & j_diver$SEGMENT %in% c("10","15")),]
ad_diver<-ad_diver[!(ad_diver$SITE=="OAH-03233" & ad_diver$SEGMENT %in% c("15")),] #only in diver surveys...maybe remove?
j_diver<-j_diver[!(j_diver$SITE=="OAH-03233" & j_diver$SEGMENT %in% c("15")),] #only in diver surveys...maybe remove?


# ###FOR CALIBRATION: Use this script to assign transect
# #Create Transect column and use this to code duplicate segments
# ad_sfm<-ad_sfm %>% mutate(TRANSECT=recode(ANALYST,
#                                         `MA`="1",
#                                         `RS`="2",
#                                         `MW`="2",
#                                         `CA`="3",
#                                         `ML`="4",
#                                         `FL`="5",
#                                         `AH`="6",
#                                         `NA`="NA"))
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
#                                           `ML`="4",
#                                           `FL`="5",
#                                           `AH`="6",
#                                           `NA`="NA"))
# #Check that segments were changed correctly
# j_sfm<-droplevels(j_sfm)
# table(j_sfm$SITE,j_sfm$TRANSECT)


#FOR COMPARATIVE ANALYSIS: Use this script to assign transect
#Adults
ad_sfm$ANALYST<-ifelse(ad_sfm$ANALYST=="MW","RS",as.character(ad_sfm$ANALYST))
ad_sfm<-ad_sfm[(ad_sfm$ANALYST%in% c("MA","RS")),] # Gets rid of all anotators except for MA and RS
ad_sfm <- droplevels(ad_sfm)


#Make sure that all site-segments have both analysts represented
analyst1<-filter(ad_sfm,ANALYST=="RS");nrow(analyst1)
analyst2<-filter(ad_sfm,ANALYST=="MA");nrow(analyst2)

as.data.frame.matrix(table(analyst1$SITE, analyst1$SEGMENT)) #identical==good
as.data.frame.matrix(table(analyst2$SITE, analyst2$SEGMENT)) 


#Randomly assign annotators to transects
# hashtagged code is attempted versions of randomly assigning transect...not working but could lead to a better version than the one currently used
SURVEY_Seg<-c("SITEVISITID", "SITE","SEGMENT","ANALYST")
sfm_seg<-unique(ad_sfm[,SURVEY_Seg])
sfm_seg$SS<- paste(sfm_seg$SITE,sfm_seg$SEGMENT, sep="_")

# a<-sfm_seg[sfm_seg$ANALYST=="RS",];nrow(a) #split annotators into spearate dataframes
# transect1<-sample_n(a, size=nrow(a)/2, replace=F) %>% 
#   mutate(TRANSECT = 1)#assign transect = 1 to a randomly chosen half of the dataframe
# 
# b<-sfm_seg %>% filter(SS %in% transect1$SS)
# b<-full_join(b,transect1) 
# b<-b %>% mutate(TRANSECT=if_else(is.na(TRANSECT),2,1))
# 
# sfm_seg<-full_join(b,sfm_seg)
# c<-sfm_seg %>% filter(is.na(TRANSECT)==T)
# d<-c[c$ANALYST=="RS",]
# transects<- sample(1:2,size=nrow(d),replace=T)
# d$TRANSECT<-transects

sfm_seg<-sfm_seg[order(sfm_seg$SS),]
sfm_seg$TRANSECT<-sample(1:2,replace=F) # assign site-segments to transect 1 or transect 2

nrow(ad_sfm)
ad_sfm<-left_join(ad_sfm,sfm_seg[,!(colnames(sfm_seg)=="SS")])
nrow(ad_sfm) # make sure rows do not get dropped following the join



#Juveniles
j_sfm$ANALYST<-ifelse(j_sfm$ANALYST=="MW","RS",as.character(j_sfm$ANALYST)) #need 2 analysts only
j_sfm<-j_sfm[(j_sfm$ANALYST%in% c("MA","RS")),] # Gets rid of all anotators except for MA and RS
j_sfm <- droplevels(j_sfm)
table(j_sfm$SITE,j_sfm$ANALYST)


#Make sure that all sites have both analysts represented
analyst1<-filter(j_sfm,ANALYST=="RS");nrow(analyst1)
analyst2<-filter(j_sfm,ANALYST=="MA");nrow(analyst2)

as.data.frame.matrix(table(analyst1$SITE, analyst1$SEGMENT)) #identical==good
as.data.frame.matrix(table(analyst2$SITE, analyst2$SEGMENT)) 


#Randomly assign annotators to transects
SURVEY_Seg<-c("SITEVISITID", "SITE","SEGMENT","ANALYST")
sfm_seg<-unique(j_sfm[,SURVEY_Seg])
sfm_seg$SS<- paste(sfm_seg$SITE,sfm_seg$SEGMENT, sep="_") #make list of unique site-segments per analyst

sfm_seg<-sfm_seg[order(sfm_seg$SS),]
sfm_seg$TRANSECT<-sample(1:2,replace=F) # assign site-segments to transect 1 or transect 2

nrow(j_sfm)
j_sfm<-left_join(j_sfm,sfm_seg[,!(colnames(sfm_seg)=="SS")])
nrow(j_sfm) # make sure rows do not get dropped following the join


##Calcuate segment and transect area and add column for transect area
ad_sfm$TRANSECTAREA<-Transectarea(ad_sfm)
j_sfm$TRANSECTAREA<-Transectarea(j_sfm)


#Check if any site-segments have been dropped 
t1<-ddply(ad_sfm,.(SITE,SEGMENT),summarize,n=length(unique(ANALYST)));nrow(t1[t1$n>1,]) #44
t1<-ddply(j_sfm,.(SITE,SEGMENT),summarize,n=length(unique(ANALYST)));nrow(t1[t1$n>1,]) #44
t1<-ddply(ad_diver,.(SITE,SEGMENT),summarize,n=length(unique(DIVER)));nrow(t1[t1$n>1,]) #43
t1<-ddply(j_diver,.(SITE,SEGMENT),summarize,n=length(unique(DIVER)));nrow(t1[t1$n>1,]) #35


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


#Choose needed columns for adult and juvenile data
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


#Create a look up table of all of the colony attributes- you will need this for the functions below
SURVEY_COL<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony<-unique(awd[,SURVEY_COL])

SURVEY_COL<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE", "ANALYST",
              "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony_DIVER<-unique(awd[,SURVEY_COL])

SURVEY_SITE<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")
survey_site<-unique(awd[,SURVEY_SITE])

SURVEY_Seg<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN","HABITAT_CODE", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","METHOD","TRANSECT","SEGMENT")
survey_segment<-unique(awd[,SURVEY_Seg])

#Combine juvenile and adult data
ajwd<-full_join(awd,jwd) #fixes NA problem


#Create a site list containing list of sites double surveyed by sfm and divers
ajwd$MethodRep <- paste0(ajwd$METHOD,"_",ajwd$TRANSECT)
ajwd$SS <- paste0(ajwd$SITE,"_",ajwd$SEGMENT)

seglist <- as.data.frame.matrix(table(ajwd$SS, ajwd$MethodRep));dim(seglist)
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




# GENERATE SUMMARY METRICS at the Segment-leveL BY GENUS--------------------------------------------------
#REMOVE COLONIES THAT COULD'T BE FULLY ANNOTATED IN SFM
awd<-subset(awd,EX_BOUND==0)

#Calc_ColDen_Transect
acd.gen<-Calc_ColDen_Seg_DIVER(data = awd,grouping_field = "GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen";colnames(acd.gen)[colnames(acd.gen)=="SEGAREA"]<-"SEGAREA_ad"# calculate density at genus level as well as total
jcd.gen<-Calc_ColDen_Seg_DIVER(jwd,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen"
jcd.gen<-subset(jcd.gen,select=-c(SEGAREA))


## This function calculates mean colony length, % recent dead, % old dead, condition severity or condition extent to the segment level
## NOTE: can run both adult & juvenile data with this function for COLONYLENGTH
#c("COLONYLENGTH","RDEXTENT1", "RDEXTENT2", "RDEXTENT3", "OLDDEAD","SEVERITY_1","SEVERITY_2", "SEVERITY_3", "EXTENT_1", "EXTENT_2", "EXTENT_3")
cl.gen<-Calc_ColMetric_Seg_DIVER(data = awd,grouping_field = "GENUS_CODE",pool_fields = "COLONYLENGTH"); colnames(cl.gen)[colnames(cl.gen)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.gen<-Calc_ColMetric_Seg_DIVER(data = awd,grouping_field = "GENUS_CODE",pool_fields = "OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.gen<-Calc_ColMetric_Seg_DIVER(data = awd,grouping_field = "GENUS_CODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead


#Calc_RDden_Transect
rdden.gen<-Calc_RDden_Seg_DIVER(data=awd,survey_colony_f=survey_colony_DIVER,grouping_field ="GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.gen<-subset(rdden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,DZGN_G));colnames(acutedz.gen)[colnames(acutedz.gen)=="DZGN_G"]<-"DZGN_G_den" #subset just acute diseased colonies


#Calc_CONDden_Transect
condden.gen<-Calc_CONDden_Seg_DIVER(data=awd,grouping_field ="GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,BLE));colnames(ble.gen)[colnames(ble.gen)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,CHRO));colnames(chronicdz.gen)[colnames(chronicdz.gen)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies

#Calc_Diversity_Seg_DIVER
div.data<-Calc_Diversity_Seg_DIVER(awd,"TAXONCODE")


#Join density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
data.gen <- join_all(list(acd.gen,jcd.gen,cl.gen,od.gen,rd.gen,acutedz.gen,chronicdz.gen,ble.gen), 
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
if(nrow(div.data)!=nrow(div.data2)) {cat("WARNING: Dfs didn't merge properly")}

#Save file for larger comparative analysis
write.csv(data.gen2,file="T:/Benthic/Data/SfM/Summarized Data/HARAMP_repeats_GENUS_Summarized Data.csv",row.names = F)
write.csv(data.gen2,file="T:/Benthic/Data/SfM/Summarized Data/HARAMP_repeats_GENUS_ANALYST_Summarized Data.csv",row.names = F)
write.csv(div.data2,file="T:/Benthic/Data/SfM/Summarized Data/HARAMP_repeats_DIVERSITY_Summarized Data.csv",row.names = F)

#Save file for segment calibration
write.csv(data.gen2,file="T:/Benthic/Data/SfM/Summarized Data/HARAMP_repeats_GENUS_Summarized Data-CALIBRATION.csv",row.names = F)


