rm(list = ls()) # clear workspace
library(tidyverse)
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")

## LOAD: Site visit table from Oracle database that has sectors defined to each site (by Kisei)
raw<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/RA2301_Leg3_SITE_VISIT_DATA.csv") 

# Only OCC sites with photoquads / SfM  (May want to eventually include all OCC sites into survey master- this will be a bigger lift)
sv <- raw %>% filter(!(TYPE %in% c("Oceanography") & PHOTOMOSAIC_YN == "NO" & TRANSECT_PHOTOS == "NO"))

## QC
sv %>% distinct(TYPE, PHOTOMOSAIC_YN, TRANSECT_PHOTOS, CB_ACTIVITY_YN, FISH_REA_YN) %>% arrange(TYPE)


## CLEANUP
colnames(sv)[colnames(sv)=="MAX_DEPTH_M"]<-"new_MAX_DEPTH_M"
colnames(sv)[colnames(sv)=="MIN_DEPTH_M"]<-"new_MIN_DEPTH_M"
colnames(sv)[colnames(sv)=="LONGITUDE_LOS"]<-"LONGITUDE_LOV"
colnames(sv)[colnames(sv)=="LATITUDE_LOS"]<-"LATITUDE_LOV"


## INDICATORS OF SITE TYPE
sv.final <- sv %>% mutate(Benthic = ifelse(TYPE == "Benthic", 1, 0)) %>% 
  mutate(Fish = ifelse(TYPE == "Fish" & FISH_REA_YN == "YES", 1, 0)) %>%
  mutate(Oceanography = ifelse(TYPE == "Oceanography", 1, 0)) %>%
  mutate(CAU = ifelse(CAUS_DEPLOY_YN == "YES", 1, 0))


## SECTOR ASSIGNMENTS SHOULD BE QCed BY BENTHIC & FISH 
# ArcGIS: (1) load sitevisit file and plot lat/longs per site onto map (color code by depth bin); filter so only plotting fish sites!
#         (2) check sector assignments w/ sector & reef zone shapefiles: https://docs.google.com/spreadsheets/d/1ubvirPEe6gf9Ub-KEIw9bThGtvlkvzY5_09mdaUtlQo/edit?pli=1#gid=0
#         (3) compare numbers per sector-reef zone-depth bin with targeted allocation & determine sector assignments to sites on the edge of 2 sectors, etc.

sv.final$PERM_SITE<-0
sv.final$OLD_SITE<-sv.final$SITE
sv.final$benth<-1
sv.final$fish<-0
sv.final$ARMS<-0
sv.final$microb<-0
sv.final$arms_r<-0
sv.final$arms_d<-0
sv.final$cau_r<-0
sv.final$cau_d<-0
sv.final$HUMANS20<-NA 
sv.final$HUMANS200<-NA 
sv.final$TYPE<- "RANDOM"
sv.final$ANALYSIS_SCHEME<-"RAMP_BASIC"
sv.final$bANALYSIS_SCHEME<-"RAMP_BASIC"
sv.final$ANALYSIS_YEAR<-"2023"
sv.final$TUT2012<-NA
sv.final$OTHER_AREA_GROUPING<-NA
sv.final$SPECIAL_PROJ_YN<-NA
sv.final$SPECIAL_PROJ_DESCRIPTION<-NA
sv.final$OBS_YEAR<-2023
sv.final$EXCLUDE_FLAG<-0
sv.final$SEC_NAME<-sv.final$SECTOR

sv.final <- sv.final %>% mutate(TRANSECT_PHOTOS = ifelse(TRANSECT_PHOTOS == "YES", -1, 0)) %>% 
  mutate(PHOTOMOSAIC_YN = ifelse(PHOTOMOSAIC_YN == "YES", -1, 0)) %>%
  mutate(CLIMATE_STATION_YN = ifelse(CLIMATE_STATION_YN == "YES", -1, 0)) %>%
  mutate(Oceanography = ifelse(TYPE == "Oceanography", 1, 0)) %>%
  mutate(CAU = ifelse(CAUS_DEPLOY_YN == "YES", 1, 0))



# COMPARE SEC_NAME with Sectors-Strata-Areas file & SurveyMaster
sectors<-read.csv("C:/Users/courtney.s.couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)
SM <- read.csv("C:/Users/courtney.s.couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv") # load most recent SURVEY MASTER

#Add zeros to beginning of site number so we avoid MAR-22 changing to March 22
sv.final$SITE<- as.factor(sv.final$SITE)
sv.final$SITE<-SiteNumLeadingZeros(sv.final$SITE) 

sm.cols<-colnames(SM)

#Only include columns that are in the original SM file
sv.final<-sv.final[names(sv.final) %in% sm.cols]

## ADD LATEST SITES TO SURVEY MASTER
SM.full <- SM %>% bind_rows(sv.final %>% mutate_at(vars(SPECIAL_PROJ_YN), ~as.character(.)))



write.csv(SM.full, "C:/Users/courtney.s.couch/Documents/GitHub/Benthic-Scripts/Survey Master Prep/SURVEY_MASTER_w2013benthic.csv")


cols.keep<-c("SITEVISITID",	"REGION",	"ISLAND",	"SITE",	"OLD_SITE",	"OCC_SITEID",	"REEF_ZONE",	"DEPTH_BIN",
             "ROUNDID",	"MISSIONID",	"OBS_YEAR",	"DATE_",	"HABITAT_CODE",	"LATITUDE_SV",	"LONGITUDE_SV",	"LATITUDE_LOV",	"LONGITUDE_LOV",
             "EXCLUDE_FLAG",	"CLIMATE_STATION_YN",	"PERM_SITE",	"TRANSECT_PHOTOS",	"PHOTOMOSAIC_YN",	"new_MIN_DEPTH_M",	"new_MAX_DEPTH_M",	"benth",
             "fish",	"arms_d",	"arms_r",	"cau_d",	"cau_r",	"microb",	"ARMS",	"Benthic",	"CAU",	"Fish",	"Oceanography",	"SEC_NAME",	"HUMANS20",	
             "HUMANS200",	"TYPE",	"ANALYSIS_SCHEME",	"bANALYSIS_SCHEME",	"ANALYSIS_YEAR",
             "TUT2012",	"OTHER_AREA_GROUPING",	"SPECIAL_PROJ_YN",	"SPECIAL_PROJ_DESCRIPTION")

sv<-sv[,cols.keep]

write.csv(sv,file="T:/Benthic/Data/REA Coral Demography & Cover/Cleaned_sitevisit_112322.csv")
