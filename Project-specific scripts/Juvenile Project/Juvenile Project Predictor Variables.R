
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
library(geosphere)
library(rgdal)
library(stringr)
library(mgcv)

setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project")

#LOAD DATA
jwd_strat<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvProject_pb_STRATA.csv")#Post bleaching strata-level juvenile data
jwd_site<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvProject_pb_SITE.csv"); jwd_site<-subset(jwd_site,select= -c(X))#Post bleaching strata-level juvenile data
d_strat<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvProject_deltadensity_STRATA.csv")

SM<-read.csv("M:/Environmental Data Summary/Outputs/Survey_Master_Timeseries_2021-02-27.csv")
sh<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Predictor Variables/ESD_Fish_Complexity.csv")#Substrate height from fish sites
cover1<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2020_Tier1_SITE.csv")#Cover from all sites
cover3<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2020_Tier3_SITE.csv")#Cover from all sites
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)
load("C:/Users/Courtney.S.Couch/Documents/GitHub/env_data_summary/outputs/Survey_Master_Timeseries_2021-02-27.Rdata") #Survey master file with env data

#Subset survey master and env columns of interest
cols<-c("MISSIONID","DATE_","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE","HABITAT_CODE","REEF_ZONE",
               "DEPTH_BIN", "LATITUDE_LOV", "LONGITUDE_LOV","new_MIN_DEPTH_M","new_MAX_DEPTH_M","DHW.MeanMax_Degree_Heating_Weeks_YR01",
        "DHW.MeanMax_Degree_Heating_Weeks_YR05", "DHW.MeanMax_Degree_Heating_Weeks_YR10","DHW.MaxMax_Degree_Heating_Weeks_YR10",
        "DHW.MeanDur_Major_Degree_Heating_Weeks_YR10","mean_annual_range_Chlorophyll_A_ESAOCCCI_8Day_YR05",
        "mean_annual_range_Kd490_ESAOCCCI_8Day_YR05","mean_kdPAR_VIIRS_Weekly_YR05")
sm_env<-SM[,cols]


#Only use Total Scl juvenile data 
jwd_siteS<-subset(jwd_site,GENUS_CODE=="SSSS")
jwd_siteS$ANALYSIS_SEC<-jwd_siteS$SEC_NAME
sm_env$ANALYSIS_SEC<-sm_env$SEC_NAME
sm_env$STRATANAME<-paste(sm_env$ANALYSIS_SEC,sm_env$REEF_ZONE,sm_env$DEPTH_BIN,sep="_")

#List of Predictor Variables, where the data can be found and whether they are ready to use at the Stratum level
predlist<-data.frame(Variable=c("Habitat Code","Frequency of DHW events","Mean Max DHW over previous 10yr","Mean Max DHW over previous 5yr",
                                "Time Since last > 8 DHW","Latitude","kd490","Chlorophylla", "Mean Depth","Mean Proximity to Humans","Wave Power","Substrate Height",
                                "Benthic Cover"),
                     Data_Location= c("SURVEY_MASTER","EDS","EDS","EDS","EDS","SURVEY_MASTER","EDS","EDS","SURVEY_MASTER","SURVEY_MASTER","M Drive","Predictor Variables Folder",
                                      "T drive"),
                     Summarized_to_Stratum= c("Y","N","N","N","N","Y","N","N","Y","N","N","Y","Y"))



# DHW ---------------------------------------------------------------------
dhw_mean<-sm_env %>%
  group_by(REGION,OBS_YEAR,ISLAND,ANALYSIS_SEC,STRATANAME) %>%
  summarize(MeanMaxDHW10=mean(DHW.MeanMax_Degree_Heating_Weeks_YR10),MaxMaxDHW10=mean(DHW.MaxMax_Degree_Heating_Weeks_YR10))



# SUBSTRATE HEIGHT --------------------------------------------------------

SURVEY_SITE<-c("DATE_","SITEVISITID", "ANALYSIS_YEAR","OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE","HABITAT_CODE","REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MEAN_SH","SD_SH_DIFF")
sh.site<-unique(sh[,SURVEY_SITE]);head(sh.site)
sh.site$STRATANAME<-paste(sh.site$SEC_NAME,sh.site$REEF_ZONE,sh.site$DEPTH_BIN,sep="_") #Create stratum

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


# Habitat code ------------------------------------------------------------
# in order to convert habitat type to continuous factor- calculate % of sites in a stratum that are carbonate reef, basalt and sand/rubble

#Simplify habitat codes into 3 groups (carbonate, basalt, sand/rubble)
jwd_siteS$ANALYSIS_SEC<-jwd_siteS$SEC_NAME
jwd_siteS<-jwd_siteS %>% mutate(Hab_simple=recode(HABITAT_CODE, 
                                                  `AGR`="Carbonate",
                                                  `APR`="Carbonate",
                                                  `APS`="Carbonate",
                                                  `WAL`="Carbonate",
                                                  `PAV`="Basalt",
                                                  `PPR`="Basalt",
                                                  `RRB`="Sand_Rubble",
                                                  `ROB`="Basalt",
                                                  `SAG`="Carbonate",
                                                  `SCR`="Sand_Rubble",
                                                  `PSC`="Sand_Rubble"))

hab_sum<-jwd_siteS %>%
    group_by(REGION,OBS_YEAR,ISLAND,ANALYSIS_SEC,STRATANAME,Hab_simple) %>%
    summarize(n=length(unique(SITEVISITID)))

site_sum<-jwd_siteS %>%
  group_by(REGION,OBS_YEAR,ISLAND,ANALYSIS_SEC,STRATANAME) %>%
  summarize(ntot=length(unique(SITEVISITID)))

hab_sum<-left_join(hab_sum,site_sum)
hab_sum$prop<-hab_sum$n/hab_sum$ntot*100
hab_sum<-subset(hab_sum,select= -c(n,ntot))
hab_sum$Hab_simple = paste(hab_sum$Hab_simple,"HC",sep = "_")

head(hab_sum)


hab_sum_w<-hab_sum %>%
  spread(Hab_simple,prop)
head(hab_sum_w)

write.csv(hab_sum_w, file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Predictor Variables/JuvProject_Habitat_STRATA.csv")



# MEAN DEPTH --------------------------------------------------------------

jwd_siteS$MidDepth<-(jwd_siteS$MAX_DEPTH_M+jwd_siteS$MIN_DEPTH_M)/2
depth_mean<-jwd_siteS %>%
  group_by(REGION,OBS_YEAR,ISLAND,ANALYSIS_SEC,STRATANAME) %>%
  summarize(MeanDepth=mean(MidDepth),MeanMaxDepth=mean(MAX_DEPTH_M))

write.csv(depth_mean, file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Predictor Variables/JuvProject_MeanDepth_STRATA.csv")


# LATITUDE ----------------------------------------------------------------
# Calculate mean latitutde/stratum
#Use projected coordinates (utm) instead of unprojected coordinates (lat/lon). 
#Use"weighted mean" (i.e, latitudinal gravity center) instead of the arithmetic mean.

#Convert unprojected to projected coordinates
islands = unique(jwd_siteS$ISLAND)#Create vector of island names from your dataframe

df_utm = NULL #create empty dataframe

for (isl in 1:length(islands)) {
  
  df_i = jwd_siteS %>% subset(ISLAND %in% islands[[isl]])
  
  zone <- (floor((df_i$LONGITUDE[1] + 180)/6) %% 60) + 1 #identify UTM zone for each island
  xy_utm = as.data.frame(cbind(utm = project(as.matrix(df_i[, c("LONGITUDE", "LATITUDE")]), paste0("+proj=utm +units=km +zone=", zone)))) #convert to projected coordinates
  colnames(xy_utm) = c("X", "Y")
  df_i = cbind(df_i, xy_utm) #add the new projected coordinates back into original dataframe
  df_utm = rbind(df_utm, df_i) #do this for each island
  
}


#df=jwd_siteS
df=df_utm

# df_sub = as.data.frame(str_match(df$STRATANAME, "^(.*)_(.*)$")[,-1])[2]
# colnames(df_sub) = "depth_strata"
# df = cbind(df, df_sub)

# lat_mean = df %>%
#   group_by(REGION,OBS_YEAR,ISLAND,ANALYSIS_SEC,STRATANAME)%>%
#   mutate(depth_strata_sum = n())%>%
#   summarize(lat_strata_weighted_mean = weighted.mean(LATITUDE, depth_strata_sum))
# View(df.)

lat_mean = df %>%
  group_by(REGION,OBS_YEAR,ISLAND,ANALYSIS_SEC,STRATANAME)%>%
  mutate(depth_strata_sum = n())%>%
  summarize(utmlat_strata_weighted_mean = weighted.mean(X, depth_strata_sum),
            lat_strata_weighted_mean = weighted.mean(LATITUDE, depth_strata_sum))
View(lat_mean)


model = gam(JuvColDen ~ + s(lat_strata_weighted_mean, k = 3),
            family = "tw(theta = NULL, link = 'log',a = 1.01, b = 1.99)",
            data = df,
            weights = df$weight,
            gamma = 1.4)

plot(model)


model = gam(JuvColDen ~ + s(lat_depth_weighted_mean, k = 3),
            family = "tw(theta = NULL, link = 'log',a = 1.01, b = 1.99)",
            data = df,
            #weights = df$weight,
            gamma = 1.4)

plot(model)

write.csv(lat_mean, file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Predictor Variables/JuvProject_MeanLatitude_STRATA.csv")




# BENTHIC COVER
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








~~~~~~~~
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


