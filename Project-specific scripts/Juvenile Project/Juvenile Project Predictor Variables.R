
# This script will clean the raw benthic REA data using method E that comes directly from the new data base application.
#Note- these data represent the revised data structure insituted in November 2018. Several recent dead and condition columns were added
#These data only include surveys conducted between 2013-2019
rm(list=ls())

#Set Run Flags
DEBUG=TRUE

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/fish_team_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/Islandwide Mean&Variance Functions.R")

library(VCA)
library(forcats)

setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project")

#LOAD DATA
jwd_strat<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/JuvProject_pb_STRATA.csv")#Post bleaching strata-level juvenile data
jwd_site<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/JuvProject_pb_SITE.csv")#Post bleaching strata-level juvenile data
sh<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Predictor Variables/ESD_Fish_Complexity.csv")#Substrate height from fish sites
cover1<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2019_Tier1_SITE.csv")#Cover from all sites
cover3<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2019_Tier3_SITE.csv")#Cover from all sites
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

#Only use Total Scl juvenile data 
jwd_siteS<-subset(jwd_site,GENUS_CODE=="SSSS")


#Prepare Substrate Height data
SURVEY_SITE<-c("DATE_","SITEVISITID", "ANALYSIS_YEAR","OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE","HABITAT_CODE","REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MEAN_SH","SD_SH_DIFF")
sh.site<-unique(sh[,SURVEY_SITE]);head(sh.site)
sh.site$STRATANAME<-paste(sh.site$SEC_NAME,sh.site$REEF_ZONE,sh.site$DEPTH_BIN,sep="_") #Create stratum


# Prepare Cover data --------
cover<-left_join(cover3,cover1[,c("SITEVISITID","CORAL","MA","TURF","SED")])  
nrow(cover);View(cover)

#Create summarized benthic columns
cover$RUBBLE<-cover$CCAR+cover$RUB+cover$TURFR
cover$TURF_BARE<-cover$TURFH+cover$HARD
cover$CCA<-cover$CCAH
cover$SAND<-cover$SED
cover$TURF<-cover$TURFH

#Consolidate columns and combine rubble and sand
cols<-c("SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE","REEF_ZONE",
        "DEPTH_BIN", "LATITUDE", "LONGITUDE","CORAL","CCA","RUBBLE","SAND","TURF","MA")
cover<-cover[,cols]
cover$SAND_RUB<-cover$RUBBLE+cover$SAND
head(cover)

cover$STRATANAME<-paste(cover$SEC_NAME,cover$REEF_ZONE,cover$DEPTH_BIN,sep="_") #Create stratum


#Calculate Strata-Level Cover data
nrow(jwd_siteS)
wsd<-left_join(jwd_siteS,cover[,c("SITEVISITID","CORAL","CCA","RUBBLE","SAND","TURF","MA","SAND_RUB")]); head(wsd);nrow(wsd)
head(wsd[which(is.na(wsd)),])

#Merge together wsd and sectors
wsd<-left_join(wsd,sectors[,c("SEC_NAME","REEF_ZONE","DEPTH_BIN","AREA_HA")]);nrow(wsd);head(wsd)

#Subset just Forereef sites
wsd$REEF_ZONE<-ifelse(wsd$REEF_ZONE=="Protected Slope","Forereef",as.character(wsd$REEF_ZONE))
wsd<-subset(wsd,REEF_ZONE=="Forereef")

wsd$STRATANAME<-paste(wsd$SEC_NAME,wsd$REEF_ZONE,wsd$DEPTH_BIN,sep="_") #Create stratum
head(wsd)
wsd$ANALYSIS_SEC<-wsd$SEC_NAME
wsd$ANALYSIS_YEAR<-wsd$OBS_YEAR

data.cols<-c("CORAL","CCA","RUBBLE","SAND","TURF","MA","SAND_RUB")

### CALCULATE MEAN AND VARIANCE WITHIN STRATA ###
SPATIAL_POOLING_BASE<-c("REGION","ISLAND", "ANALYSIS_SEC", "REEF_ZONE", "STRATANAME")    
ADDITIONAL_POOLING_BY<-c("ANALYSIS_YEAR")                                    # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)

#generate within strata means and vars
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
dps<-Calc_PerStrata(wsd, data.cols, c(POOLING_LEVEL, "AREA_HA"))
head(dps$Mean)

###### REMOVE STRATA with N=1 (cannot pool those up)
dps$Mean<-dps$Mean[dps$Mean$N>1,]
dps$SampleVar<-dps$SampleVar[dps$SampleVar$N>1,]
dps$SampleSE<-dps$SampleSE[dps$SampleSE$N>1,]

# e.g. SAVE BY ISLAND AND REEF_ZONE PER YEAR
OUTPUT_LEVEL<-c("REGION","ISLAND","ANALYSIS_SEC","STRATANAME","ANALYSIS_YEAR") 
dpst<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA");dpst<-as.data.frame(dpst)

#Clean up- remove SE columns and remove "Mean" from column names
dpst<-dpst %>% dplyr::select(Mean.REGION:Mean.SAND_RUB)

dpst<-dpst %>%
  dplyr::rename_all(funs(stringr::str_replace_all(., "Mean.", "")))
head(dpst)

write.csv(dpst, file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Predictor Variables/JuvProject_Cover_STRATA.csv")



#Calculate Strata-Level Substrate Height data

#Merge together wsd and sectors
wsd<-left_join(sh.site,sectors[,c("SEC_NAME","REEF_ZONE","DEPTH_BIN","AREA_HA")]);nrow(wsd);head(wsd)

#Remove NAs from dataframe
wsd<-wsd[!is.na(wsd$MEAN_SH), ]
View(wsd)

#Subset just Forereef sites
wsd$REEF_ZONE<-ifelse(wsd$REEF_ZONE=="Protected Slope","Forereef",as.character(wsd$REEF_ZONE))
wsd<-subset(wsd,REEF_ZONE=="Forereef")

wsd$STRATANAME<-paste(wsd$SEC_NAME,wsd$REEF_ZONE,wsd$DEPTH_BIN,sep="_") #Create stratum
head(wsd)


wsd$ANALYSIS_SEC<-wsd$SEC_NAME
wsd$ANALYSIS_YEAR<-wsd$OBS_YEAR

data.cols<-c("MEAN_SH","SD_SH_DIFF")

### CALCULATE MEAN AND VARIANCE WITHIN STRATA ###
SPATIAL_POOLING_BASE<-c("REGION","ISLAND", "ANALYSIS_SEC", "REEF_ZONE", "STRATANAME","DEPTH_BIN")    

#generate within strata means and vars
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE)
dps<-Calc_PerStrata(wsd, data.cols, c(POOLING_LEVEL, "AREA_HA"))
head(dps$Mean)

###### REMOVE STRATA with N=1 (cannot pool those up)
dps$Mean<-dps$Mean[dps$Mean$N>1,]
dps$SampleVar<-dps$SampleVar[dps$SampleVar$N>1,]
dps$SampleSE<-dps$SampleSE[dps$SampleSE$N>1,]

# e.g. SAVE BY ISLAND AND REEF_ZONE PER YEAR
OUTPUT_LEVEL<-c("REGION","ISLAND","ANALYSIS_SEC","STRATANAME","DEPTH_BIN") 
dpst<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA");dpst<-as.data.frame(dpst)

#Clean up- remove SE columns and remove "Mean" from column names
dpst<-dpst %>% dplyr::select(Mean.REGION:Mean.MEAN_SH,PooledSE.MEAN_SH)

dpst<-dpst %>%
  dplyr::rename_all(funs(stringr::str_replace_all(., "Mean.", "")))

dpst<-dpst %>%
  dplyr::rename_all(funs(stringr::str_replace_all(., "Pooled", "")))

head(dpst)

colnames(dpst)[which(colnames(dpst) == 'ANALYSIS_SEC')]<-"SEC_NAME" 
colnames(dpst)[which(colnames(dpst) == 'SE.MEAN_SH')]<-"SE_MEAN_SH" 


write.csv(dpst, file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Predictor Variables/JuvProject_SubstrateHeight_STRATA.csv",row.names = F)

#Change specific sector names to match the sector shape file polygons
dpst$SEC_NAME[dpst$SEC_NAME == "HAW_HAMAKUA"] <- "HAW_HAMAK"
dpst$SEC_NAME[dpst$SEC_NAME == "GUA_PATI_POINT"] <- "GUA_PATI_PT"

#Separate strata to create sector maps in Arc
sh<-subset(dpst,DEPTH_BIN=="Shallow")
m<-subset(dpst,DEPTH_BIN=="Mid")
d<-subset(dpst,DEPTH_BIN=="Deep")

write.csv(sh, file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Predictor Variables/JuvProject_SubstrateHeight_Shallow.csv",row.names = F)
write.csv(m, file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Predictor Variables/JuvProject_SubstrateHeight_Mid.csv",row.names = F)
write.csv(d, file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Predictor Variables/JuvProject_SubstrateHeight_Deep.csv",row.names = F)

# Combine juvenile, cover and substrate height data -----------------------


