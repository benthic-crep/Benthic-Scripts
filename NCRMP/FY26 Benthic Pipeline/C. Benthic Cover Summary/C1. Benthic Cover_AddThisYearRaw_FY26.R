#This Script:
# (1) Cleans the most recent year's benthic cover data (e.g. 2024)
# (2) Pulls Old "Clean" Benthic Cover Data and adds new data to it, saves it to t: drive

#It's a modification from Benthic_Cover_RawtoEstimates_v2/3

#Clear Data/Packages
rm(list=ls())
pkgs <- names(sessionInfo()$otherPkgs)
for (package in pkgs) {
  detach(paste0("package:", package), unload = TRUE, character.only = TRUE)
}

#Load Libraries
library(lubridate)
source("./Functions/Benthic_Functions_newApp_vTAOfork.R")
source("../fish-paste/lib/core_functions.R")
library(dplyr)#to avert plyr/dplyr issues

### (1) Cleans the most recent year's benthic cover data (e.g. 2024)

#Load Random Benthic Cover Data from "Access Raw Cover and REA Data From Oracle.r" 
CRC=load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/FY26/Raw_COV_RANDOM_CNET_2024.rdata")
CFC=load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/FY26/Raw_COV_FIXED_CNET_2024.rdata")
COV_FC=COV_FIX_2024
COV_RC=COV_RAN_2024

#Random Site Cover Data
COV_RC$SITE<-SiteNumLeadingZeros(as.factor(COV_RC$SITE))
COV_RC$METHOD="CNET";COV_RC$FIXED_OR_RANDOM="RANDOM";COV_RC$POINTS=1
COV_RC$OBS_YEAR=as.numeric(COV_RC$OBS_YEAR);COV_RC$DATE_=ymd_hms(COV_RC$DATE_);

#Fixed Site Cover Data
COV_FC$SITE<-SiteNumLeadingZeros(as.factor(COV_FC$SITE))
COV_FC$METHOD="CNET";COV_FC$FIXED_OR_RANDOM="FIXED";COV_FC$POINTS=1
COV_FC$OBS_YEAR=as.numeric(COV_FC$OBS_YEAR);COV_FC$DATE_=ymd_hms(COV_FC$DATE_);

#Making Assumption Here that TIER_1 = CATEGORY_CODE,TIER_2 = SUBCATEGORY_CODE,TIER_3 = GENERA_CODE,
COV_RC=COV_RC %>% dplyr::rename(TIER_1 = CATEGORY_CODE,TIER_2 = SUBCATEGORY_CODE,TIER_3 = GENERA_CODE,
                                IMAGE_NAME=ORIGINAL_FILE_NAME,REP=REPLICATE,PHOTOID=IMAGE_NUMBER)
COV_FC=COV_FC %>% dplyr::rename(TIER_1 = CATEGORY_CODE,TIER_2 = SUBCATEGORY_CODE,TIER_3 = GENERA_CODE,
                                IMAGE_NAME=ORIGINAL_FILE_NAME,REP=REPLICATE,PHOTOID=IMAGE_NUMBER)

#Load Survey Master and Assign SiteVisitID to the CPCE data, clean up dates
sm<-read.csv("./NCRMP/FY26 Benthic Pipeline/A. Survey Master Prep/SURVEY_MASTER_2024_benthic.csv")
sm$SITE<-SiteNumLeadingZeros(as.factor(sm$SITE))
sm$DATE_RAW=sm$DATE_
sm$DATE_=mdy(sm$DATE_)
sm$DATE_[which(is.na(sm$DATE_))]=mdy_hms(sm$DATE_RAW[which(is.na(sm$DATE_))])
sm$DATE_[which(is.na(sm$DATE_))]=ymd_hms(sm$DATE_RAW[which(is.na(sm$DATE_))])
length(which(is.na(sm$DATE_)))

#Combine cpce and coralnet, fixed and random
FIELDS_TO_RETAIN<-c("MISSIONID","METHOD","REGION", "OBS_YEAR","ISLAND",
                    "SITEVISITID","SITE", "LATITUDE", "LONGITUDE", "REEF_ZONE", "DEPTH_BIN",
                    "PERM_SITE", "CLIMATE_STATION_YN", "MIN_DEPTH", "MAX_DEPTH",
                    "HABITAT_CODE","REP", "IMAGE_NAME", "PHOTOID", "ANALYST",
                    "TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME", "POINTS")
setdiff(FIELDS_TO_RETAIN,names(COV_FC))
y<-COV_FC[,FIELDS_TO_RETAIN]; head(y)
z<-COV_RC[,FIELDS_TO_RETAIN]; head(z)
bia24=rbind(y,z)

#################################### Add Tier 2b
#Add Tier 2b (genus for corals, order for macroalgae)
codes_lu<-read.csv("T:/Benthic/Data/Lookup Tables/All_Photoquad_codes.csv")
codes_lu<-codes_lu[,c("T2b_DESC","TIER_2b","CODE")];colnames(codes_lu)[which(names(codes_lu) =="CODE")]<-"TIER_3"
bia24<-left_join(bia24,codes_lu,by="TIER_3")

#################################### Add Tier 2b
#Flag sites that have more than 33 and less than 15 images
#With the exception of OCC 2012 sites, there should be 30 images/site/10 points/image
#Ignore 2012 OCC sites. They analyzed 50 points per images 
PointCount=bia24 %>% group_by(OBS_YEAR,SITEVISITID,SITE) %>% summarize(count=sum(POINTS))
PointCountDrop=PointCount %>% filter(count<150) %>% filter(OBS_YEAR!=2012) #|count>330 keep abnormally high sites

#Remove "Drop" sites from bia24
bia24=bia24 %>% filter(!(SITE %in% PointCountDrop$SITE))

#Check this against site master list
table(sm$REGION,sm$OBS_YEAR)
sm %>% group_by(REGION,OBS_YEAR) %>% filter(OBS_YEAR==2024) %>%  summarize(nSite=length(unique(SITE))) %>%
  pivot_wider(names_from=OBS_YEAR,values_from = nSite)
#bia24.site<-ddply(subset(cnet,OBS_YEAR=="2019"),.(REGION,OBS_YEAR),summarize,nSite=length(unique(SITE)));bia24.site
bia24.site=bia24 %>% filter(OBS_YEAR=="2024") %>% group_by(REGION,OBS_YEAR) %>% summarize(nSite=length(unique(SITE)))
bia24.site

#missing "KAH-00728","KAH-00677" from image dataset
setdiff(sm$SITE[which(sm$OBS_YEAR==2024)],bia24$SITE)

#identify which new sites are in the CoralNet data, but still need to be integrated into the SURVEY MASTER file
miss.from.sm<-bia24[!(bia24$SITEVISITID %in% sm$SITEVISITID),]

#There are some missing Tier3 information for pre 2013 data. If these data are missing then fill it with tier2 code
bia24$TIER_2<-ifelse(bia24$TIER_2=="HAL","HALI",as.character(bia24$TIER_2)) #change to match the Tier 3 halimeda code
bia24<-bia24 %>% dplyr::mutate(TIER_3=coalesce(TIER_3,TIER_2))
bia24<-bia24 %>% dplyr::mutate(GENERA_NAME=coalesce(GENERA_NAME,SUBCATEGORY_NAME))
bia24<-bia24 %>% dplyr::mutate(TIER_2b=coalesce(TIER_2b,TIER_2))
bia24<-bia24 %>% dplyr::mutate(T2b_DESC=coalesce(T2b_DESC,SUBCATEGORY_NAME))

head(bia24)

# Reclassify EMA and Halimeda --------------------------------------------

#CREATING CLASS EMA "Encrusting Macroalgae
bia24 = bia24 %>%  mutate(across(c(TIER_1, CATEGORY_NAME, TIER_2,SUBCATEGORY_NAME,TIER_2b,T2b_DESC,TIER_3,GENERA_NAME),as.factor))
levels(bia24$TIER_1)<-c(levels(bia24$TIER_1), "EMA")
levels(bia24$CATEGORY_NAME)<-c(levels(bia24$CATEGORY_NAME), "Encrusting macroalga")
levels(bia24$TIER_2)<-c(levels(bia24$TIER_2), "EMA")
levels(bia24$SUBCATEGORY_NAME)<-c(levels(bia24$SUBCATEGORY_NAME), "Encrusting macroalga")
bia24[bia24$GENERA_NAME %in% c("Lobophora sp","Peyssonnelia sp", "Encrusting macroalga"),]$TIER_1<-"EMA"
bia24[bia24$GENERA_NAME %in% c("Lobophora sp","Peyssonnelia sp", "Encrusting macroalga"),]$CATEGORY_NAME<-"Encrusting macroalga"
bia24[bia24$GENERA_NAME %in% c("Lobophora sp","Peyssonnelia sp", "Encrusting macroalga"),]$TIER_2<-"EMA"
bia24[bia24$GENERA_NAME %in% c("Lobophora sp","Peyssonnelia sp", "Encrusting macroalga"),]$SUBCATEGORY_NAME<-"Encrusting macroalga"

###Create a Halimeda class
bia24$TIER_3<-ifelse(bia24$TIER_3=="HALI","HALI",as.character(bia24$TIER_3))
bia24$TIER_1<-ifelse(bia24$TIER_3=="HALI","HALI",as.character(bia24$TIER_1))
bia24$CATEGORY_NAME<-ifelse(bia24$TIER_3=="HALI","Halimeda sp",as.character(bia24$CATEGORY_NAME))
hal<-subset(bia24,TIER_1=="HALI")
head(hal)

#LEPT and LPHY ==> LEPT
#ALSP,GOAL and GOSP ==> GOSP
#ASTS and MONS ==> ASTS
bia24$TIER_3[which(bia24$TIER_3=="LPHY")]="LEPS"
bia24$TIER_3[which(bia24$TIER_3=="ALSP")]="GOSP"
bia24$TIER_3[which(bia24$TIER_3=="GOAL")]="GOSP"
bia24$TIER_3[which(bia24$TIER_3=="MONS")]="ASTS"
bia24$TIER_2b[which(bia24$TIER_2b=="LPHY")]="LEPS"
bia24$TIER_2b[which(bia24$TIER_2b=="ALSP")]="GOSP"
bia24$TIER_2b[which(bia24$TIER_2b=="GOAL")]="GOSP"
bia24$TIER_2b[which(bia24$TIER_2b=="MONS")]="ASTS"

########################################################################################################################################
# Annotated Point (ab) Data are now clean...
# Generate Site-level Data at TIER 1 level--------------
########################################################################################################################################

# (2) Pulls Old "Clean" Benthic Cover Data and adds new data to it
BIA10_23_name=load(file="T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/BIA_2010-2023_CLEANED.RData")
bia23=eval(parse(text=BIA10_23_name))
BIA10_24<-rbind(bia23,bia24)

#Write it out
#### WORKING WITH CLEAN DATA FILE AT THIS POINT  ab=site-image-category in rows
BIA10_24<-droplevels(BIA10_24)
save(BIA10_24, file="T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/BIA_2010-2024_CLEANED.RData")


##########################################################################################################
##########################################################################################################
#Test 24 data against 23 data
rm(list=ls())
ab23_name=load("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/BIA_2010-2023_CLEANED.RData")
ab23=eval(parse(text=ab23_name));rm(list=ab23_name)
ab24_name=load("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/BIA_2010-2024_CLEANED.RData")
ab24=eval(parse(text=ab24_name));rm(list=ab24_name)

head(ab23)
head(ab24)

SiteCount=ab23 %>% filter(REGION!="CT") %>% group_by(REGION,OBS_YEAR) %>% summarize(NSV23=length(unique(SITEVISITID)))
SiteCount24=ab24 %>% filter(REGION!="CT") %>% group_by(REGION,OBS_YEAR) %>% summarize(NSV24=length(unique(SITEVISITID)))
SiteCount=SiteCount %>% full_join(SiteCount24) %>% arrange(OBS_YEAR,REGION)
View(SiteCount)
