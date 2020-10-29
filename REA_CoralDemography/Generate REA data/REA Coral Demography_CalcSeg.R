#This script reads in the diver and SfM-generated demographic data that has been QC'd and cleaned up
#Then generates segment-level from 2013-present NCRMP missions

rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")


## LOAD benthic data
awd<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED.csv")
jwd<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Juveniles_raw_CLEANED.csv")

#Final Tweaks before calculating segment-level data-------------------------------------------------
#Colony fragments will be removed when you generate the segment level data 
#We have denoted fragments differently over the course of our dataset. This code makes sure Fragment is 0 or -1 (-1 indicates it's a fragment)
awd$Fragment<-ifelse(awd$OBS_YEAR <2018 & awd$COLONYLENGTH <5 & awd$S_ORDER=="Scleractinia",-1,awd$Fragment)
head(subset(awd,Fragment==-1& OBS_YEAR<2018)) #double check that pre 2018 fragments create
awd$Fragment[is.na(awd$Fragment)] <- 0
jwd$Fragment <- 0 # you need to add this column so that you can use the site level functions correctly
awd$METHOD<-"DIVER"
jwd$METHOD<-"DIVER"

#Change DIVER to ANALYST so you can use Calc_Seg functions
colnames(awd)[colnames(awd)=="DIVER"]<-"ANALYST" 
colnames(jwd)[colnames(jwd)=="DIVER"]<-"ANALYST" 


#Calculate Segment Area
awd$SEGAREA<-awd$SEGLENGTH*awd$SEGWIDTH
jwd$SEGAREA<-jwd$SEGLENGTH*jwd$SEGWIDTH

#Simplify Bleaching Severity categories: in 2019 the team decided to simplify the bleaching severity from 1-5 to 1-3 to improve consistency in severity values
#This code converts the severity data collected prior to 2019 to a 1-3 scale
awd$DATE_ <- as.Date(awd$DATE_, format = "%m/%d/%Y")
jwd$DATE_ <- as.Date(jwd$DATE_, format = "%m/%d/%Y")

#We simplified bleaching severity ranking from 1-5 to 1-3 on 7/11/2019. We decided to drop severity 1 because there is too much inconsistency between divers
awd_pre <- awd %>% filter(DATE_ < as.Date('2019-07-11'))
awd_post<-awd %>% filter(DATE_ >= as.Date('2019-07-11'))

awd_pre<-Convert_Severity(awd_pre,"SEVERITY_1","SEVERITY_1n")
awd_pre<-Convert_Severity(awd_pre,"SEVERITY_2","SEVERITY_2n")
#awd_pre<-Convert_Severity(awd_pre,"SEVERITY_3","SEVERITY_3n") #There were no severity measurements prior to 2020

head(awd_pre)
#View(awd_pre)

#After checking that severity numbers were changed correctly, convert back to original column names & drop original columns
awd_pre<-subset(awd_pre,select=-c(SEVERITY_1));colnames(awd_pre)[which(colnames(awd_pre) == 'SEVERITY_1n')] <- "SEVERITY_1" #change group to whatever your grouping field is.
awd_pre<-subset(awd_pre,select=-c(SEVERITY_2));colnames(awd_pre)[which(colnames(awd_pre) == 'SEVERITY_2n')] <- "SEVERITY_2" #change group to whatever your grouping field is.
#awd_pre<-subset(awd_pre,select=-c(SEVERITY_3));colnames(awd_pre)[which(colnames(awd_pre) == 'SEVERITY_3n')] <- "SEVERITY_3" #change group to whatever your grouping field is.
awd_pre$SEVERITY_3<-NA

head(awd_pre)



#Combine dataframes before and after 2019 & check that rows weren't dropped
awd.<-rbind(awd_pre,awd_post);write.csv(awd.,"test.csv")
nrow(awd)
nrow(awd.);head(awd.)
awd<-awd.; rm("awd.") #remove temporary dataframe if all good. 

#ADD CODE TO CHANGE TRANSECT NUMBERS FOR JUVENILES
jwd$TRANSECT[jwd$TRANSECT==3]<-1
jwd$TRANSECT[jwd$TRANSECT==4]<-2

awd$SEGMENT<-as.factor(awd$SEGMENT)
jwd$SEGMENT<-as.factor(jwd$SEGMENT)

awd$SS<-paste(awd$SITE,awd$TRANSECT,awd$SEGMENT,sep = "_")
jwd$SS<-paste(jwd$SITE,jwd$TRANSECT,jwd$SEGMENT,sep = "_")

#Create a look up table of all of the colony attributes- you will need this for the functions below
SURVEY_COL<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony<-unique(awd[,SURVEY_COL])

SURVEY_COL<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE", "ANALYST",
              "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony_DIVER<-unique(awd[,SURVEY_COL])

SURVEY_SITE<-c("METHOD","MISSIONID","DATE_","SITEVISITID", "ANALYSIS_YEAR","OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","HABITAT_CODE")
survey_siteAd<-unique(awd[,SURVEY_SITE])

SURVEY_SITE<-c("METHOD","MISSIONID","DATE_","SITEVISITID", "ANALYSIS_YEAR","OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","HABITAT_CODE")
survey_siteJ<-unique(jwd[,SURVEY_SITE])


#We did juvenile only surveys in 2017 in PRIA, this will make sure the SV table has both adult and juv sites.
survey_site<-left_join(survey_siteJ,survey_siteAd);nrow(survey_site) 


# GENERATE SUMMARY METRICS at the Segment-leveL BY GENUS--------------------------------------------------

#Calc_ColDen_Seg
acd.gen<-Calc_ColDen_Seg_DIVER(data = awd,grouping_field = "GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen";colnames(acd.gen)[colnames(acd.gen)=="SEGAREA"]<-"SEGAREA_ad"# calculate density at genus level as well as total
jcd.gen<-Calc_ColDen_Seg_DIVER(jwd,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen"
jcd.gen<-subset(jcd.gen,select=-c(SEGAREA))


## This function calculates mean colony length, % recent dead, % old dead, condition severity or condition extent to the segment level
## NOTE: can run both adult & juvenile data with this function for COLONYLENGTH
cl.gen<-Calc_ColMetric_Seg_DIVER(data = awd,grouping_field = "GENUS_CODE",pool_fields = "COLONYLENGTH"); colnames(cl.gen)[colnames(cl.gen)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.gen<-Calc_ColMetric_Seg_DIVER(data = awd,grouping_field = "GENUS_CODE",pool_fields = "OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.gen<-Calc_ColMetric_Seg_DIVER(data = awd,grouping_field = "GENUS_CODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead


#Calc_RDden_Transect
rdden.gen<-Calc_RDden_Seg_DIVER(data=awd,survey_colony_f=survey_colony_DIVER,grouping_field ="GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
rdden.gen<-subset(rdden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,SEGMENT,ANALYST,GENUS_CODE,DZGN_G,BBD,
                                       BFI,COTS,DAMG_G,FISH,GAST,OTHR_G,OVRG_G,PRED_G,PUS,SEDI_G,TLS,UNKN_G,WSY)) #only include RD causes we are interested in

#Calc_CONDden_Transect
condden.gen<-Calc_CONDden_Seg_DIVER(data=awd,grouping_field ="GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
condden.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,SEGMENT,ANALYST,GENUS_CODE,ALG,BIN,BLE,BLP,CHRO,DAMG,DIS,FUG,PRS,PTR,SGA,TIN))




#Calc_Diversity_Seg_DIVER
#div.data<-Calc_Diversity_Seg_DIVER(awd,"TAXONCODE")


#Join all metrics together
data.gen <- join_all(list(acd.gen,jcd.gen,cl.gen,od.gen,rd.gen,rdden.gen,condden.gen), 
                     by=c("METHOD","SITE","SITEVISITID","TRANSECT","ANALYST","SEGMENT","GENUS_CODE"), type='full')
head(data.gen)

#Calculate transect level prevalence for each recent dead cause or condition
#health states that end in _G indicate general categories (e.g. DZGN_G = all acute diseases that cause mortality)
data.gen2<-data.gen #duplicate datafram
prev<-(data.gen2[,16:42]*data.gen2[,7])/data.gen2[,9]*100 #calculate prevalence
colnames(prev) <- paste(colnames(prev), "prev", sep = "_") #add prevalence suffix to health states
data.gen2<-cbind(data.gen2,prev) #merge prev back into data.gen2
colnames(data.gen2)[16:42] <- paste(colnames(data.gen2)[16:42], "den", sep = "_") #add density suffix to health states
head(data.gen2)


View(data.gen)


#Make final dataframe to save
data.gen3<-left_join(data.gen2,survey_site)
if(nrow(data.gen2)!=nrow(data.gen3)) {cat("WARNING: Dfs didn't merge properly")}


# div.data2<-left_join(div.data,survey_site)
# if(nrow(div.data)!=nrow(div.data2)) {cat("WARNING: Dfs didn't merge properly")}

#Save files
write.csv(data.gen3,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Segment/BenthicREA_segment_GENUS.csv",row.names = F)
# write.csv(div.data2,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Segment/BenthicREA_segment_Diversity.csv",row.names = F)



