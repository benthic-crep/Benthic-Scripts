rm(list = ls()) # clear workspace
library(tidyverse)
source("../fish-paste/lib/core_functions.R")
source("./Useful Scripts/Oracle_Fetch.R")


#This Code assumes that FISH team has already added FISH sites, and we're mopping up missed benthic data only sites

## LOAD Mission Info
MI_24=fetch_data(base_url = "https://picapex.nmfs.local/gis/esd/esd_data/mission_info",YEAR==2024)
targetMI=MI_24 %>% filter(REGION%in%c("NWHI","MHI")) %>% pull(MISSIONID) %>% unique()

## LOAD: Site visit table from Oracle database that has sectors defined to each site (by Kisei)
SV_24=NULL
for (mi_i in targetMI) {
  SV_24i=fetch_data(base_url = "https://picapex.nmfs.local/gis/esd/esd_data/site_visit_info",
                       MISSIONID==mi_i)
  SV_24=rbind(SV_24,SV_24i)
}
dim(SV_24)

## LOAD existing SURVEY MASTER
SM_OLD=read.csv("../fish-paste/data/SURVEY MASTER.csv")
SM_OLD24=SM_OLD %>% filter(OBS_YEAR==2024)

#Check sites not included
SV_24_EX=SV_24[which(!(SV_24$SITEVISITID%in%SM_OLD$SITEVISITID)),]

#compare sets and check for consistency
dim(SM_OLD24)
dim(SV_24)
dim(SV_24_EX)

# #For simplicity drop any column that is all NA
NA_COLS=SV_24_EX %>% keep(~all(is.na(.x))) %>% names
SV_24_EX=SV_24_EX %>% select(!any_of(NA_COLS))

# Drop any site that does not have either PHOTOMOASIC or TRANSECT_PHOTOS
SV_24_PH <- SV_24_EX %>% filter(TRANSECT_PHOTOS==-1|PHOTOMOSAIC_YN==-1)

## QC
SM_OLD24 %>% dplyr::count(TYPE, PHOTOMOSAIC_YN, TRANSECT_PHOTOS, Fish) %>% arrange(TYPE)
SV_24_PH %>% dplyr::count(TYPE, PHOTOMOSAIC_YN, TRANSECT_PHOTOS, FISH_REA_YN) %>% arrange(TYPE)


## CLEANUP
SV_24_PH=SV_24_PH %>%
  dplyr::rename(new_MAX_DEPTH_M=MAX_Z_M,
         new_MIN_DEPTH_M=MIN_Z_M,
         LONGITUDE_LOV=LONGITUDE_LOS,
         LATITUDE_LOV=LATITUDE_LOS)
# colnames(SV_24_PH)[colnames(SV_24_PH)=="MAX_DEPTH_M"]<-"new_MAX_DEPTH_M"
# colnames(SV_24_PH)[colnames(SV_24_PH)=="MIN_DEPTH_M"]<-"new_MIN_DEPTH_M"
# colnames(SV_24_PH)[colnames(SV_24_PH)=="LONGITUDE_LOS"]<-"LONGITUDE_LOV"
# colnames(SV_24_PH)[colnames(SV_24_PH)=="LATITUDE_LOS"]<-"LATITUDE_LOV"


## INDICATORS OF SITE TYPE
sv.final <- SV_24_PH %>% dplyr::mutate(Benthic = ifelse(TYPE == "Benthic", 1, 0)) %>% 
  dplyr::mutate(Fish = ifelse(TYPE == "Fish" & FISH_REA_YN == "YES", 1, 0)) %>%
  dplyr::mutate(Oceanography = ifelse(TYPE == "Oceanography", 1, 0)) %>%
  dplyr::mutate(CAU = ifelse(CAUS_DEPLOY_YN == "YES", 1, 0))
dim(sv.final)


## SECTOR ASSIGNMENTS SHOULD BE QCed BY BENTHIC & FISH 
# In R:   (1) Load sector-level .shp file ("T:/Common/Maps/Pacific/Sector/ALLPacific_Sectors_Islands_5km_buffer.shp")
#         (2) Convert SV.final into point file
#         (3) Over the lat/lon into sectors
library(sf)

#(1)
SEC5KM=st_read(dsn = "T:/Common/Maps/Pacific/Sector/",layer = "ALLPacific_Sectors_Islands_5km_buffer")
#(2)
LL_LOS=st_as_sf(sv.final,coords = c("LONGITUDE_LOV","LATITUDE_LOV"))

#Run some shapefile clean up...
#Drop duplicated sectors (10,68)
SEC5KM=SEC5KM[-c(10,68),]

#Assume Planar Geometry (for in/out of polygon, shouldn't be a problem)
sf_use_s2(FALSE)
#Make shapefile geometry valid
SEC5KM=st_make_valid(SEC5KM)
INVAL_i=which(!st_is_valid(SEC5KM))

#set crs
st_crs(LL_LOS)=st_crs(SEC5KM)

#(3) Calculate Sector overlap -#this returns a matrix, that should have a single answer
SECTORS=st_intersects(x = LL_LOS,y=SEC5KM,sparse = F)

#pull that answer from the matrix (i.e. for each point, which poly are you in?)
SEC_i=apply(SECTORS,1,which)
#some don't fall in any poly - I checked, they're all in Kure...
nona_i=which(lapply(SEC_i,length)==1)
#Assign those that do fall in poly to assigned poly
sv.final$SEC_NAME[nona_i]=SEC5KM$SEC_NAME[unlist(SEC_i)]
#force the rest of them to Kure
sv.final[which(is.na(sv.final$SEC_NAME)),"SEC_NAME"]="Kure"

#Fill out the other columns
sort(setdiff(names(SM_OLD24),names(sv.final)))
sv.final[,c("ANALYSIS_SCHEME",
            "bANALYSIS_SCHEME")]<-"RAMP_BASIC"
sv.final[,c("ANALYSIS_YEAR",
            "OBS_YEAR")]<-"2024"
sv.final[,c("ARMS",
            "arms_r",
            "arms_d",
            "cau_r",
            "cau_d",
            "fish",
            "microb")]<-0
sv.final[,c("benth")]<-1
sv.final[,c("HUMANS20",
            "HUMANS200",
            "OCC_SITEID",
            "OTHER_AREA_GROUPING",
            "SPECIAL_PROJ_DESCRIPTION",
            "SPECIAL_PROJ_YN",
            "TUT2012")]<-NA
sv.final[,c("OLD_SITE")]<-sv.final$SITE
sv.final=sv.final[,names(SM_OLD24)]
all(names(SM_OLD24)==names(sv.final))

#Already in correct format
# sv.final <- sv.final %>% mutate(TRANSECT_PHOTOS = ifelse(TRANSECT_PHOTOS == "YES", -1, 0)) %>% 
#   mutate(PHOTOMOSAIC_YN = ifelse(PHOTOMOSAIC_YN == "YES", -1, 0)) %>%
#   mutate(CLIMATE_STATION_YN = ifelse(CLIMATE_STATION_YN == "YES", -1, 0)) %>%
#   mutate(Oceanography = ifelse(TYPE == "Oceanography", 1, 0)) %>%
#   mutate(CAU = ifelse(CAUS_DEPLOY_YN == "YES", 1, 0))

# COMPARE SEC_NAME with Sectors-Strata-Areas file & SurveyMaster
sectors<-read.csv("../fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)
#SM <- read.csv("C:/Users/courtney.s.couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv") # load most recent SURVEY MASTER

#Add zeros to beginning of site number so we avoid MAR-22 changing to March 22
sv.final$SITE<- as.factor(sv.final$SITE)
sv.final$SITE<-SiteNumLeadingZeros(sv.final$SITE) 

## ADD LATEST SITES TO SURVEY MASTER
SM_NEW =rbind(SM_OLD,sv.final) #SM_OLD %>% bind_rows(sv.final %>% mutate_at(vars(SPECIAL_PROJ_YN), ~as.character(.)))

#NEED TO LOOKUP OCC SITE ID!
#Also duplicate PHOTOs from OAH-4326

write.csv(x = SM_NEW,file = "./NCRMP/Survey Master Prep/SURVEY_MASTER_2024_benthic.csv",row.names = F)


cols.keep<-c("SITEVISITID",	"REGION",	"ISLAND",	"SITE",	"OLD_SITE",	"OCC_SITEID",	"REEF_ZONE",	"DEPTH_BIN",
             "ROUNDID",	"MISSIONID",	"OBS_YEAR",	"DATE_",	"HABITAT_CODE",	"LATITUDE_SV",	"LONGITUDE_SV",	"LATITUDE_LOV",	"LONGITUDE_LOV",
             "EXCLUDE_FLAG",	"CLIMATE_STATION_YN",	"PERM_SITE",	"TRANSECT_PHOTOS",	"PHOTOMOSAIC_YN",	"new_MIN_DEPTH_M",	"new_MAX_DEPTH_M",	"benth",
             "fish",	"arms_d",	"arms_r",	"cau_d",	"cau_r",	"microb",	"ARMS",	"Benthic",	"CAU",	"Fish",	"Oceanography",	"SEC_NAME",	"HUMANS20",	
             "HUMANS200",	"TYPE",	"ANALYSIS_SCHEME",	"bANALYSIS_SCHEME",	"ANALYSIS_YEAR",
             "TUT2012",	"OTHER_AREA_GROUPING",	"SPECIAL_PROJ_YN",	"SPECIAL_PROJ_DESCRIPTION")

sv.final<-sv.final[,cols.keep]

write.csv(sv.final,file="T:/Benthic/Data/REA Coral Demography & Cover/Cleaned_SiteVisit_2024.csv")
