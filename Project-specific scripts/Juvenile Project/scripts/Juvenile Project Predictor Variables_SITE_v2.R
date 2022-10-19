#start by extracting habitat info

# This script will clean the raw benthic REA data using method E that comes directly from the new data base application.
#Note- these data represent the revised data structure insituted in November 2018. Several recent dead and condition columns were added
#These data only include surveys conducted between 2013-2019

#Uses all forereef site-level data between 2013-2019 

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
detach(package:dplyr) #dplyr has issues if plyr is loaded first
library(dplyr)
library(tidyr)

setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project")

#LOAD DATA
jwd_site<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvProject_SITE_weights_AllYears_wHD.csv") #This dataframe has both juvenile and human density data

tsdhw<-read.csv("T:/Benthic/Projects/Juvenile Project/Juvenile_TimeSinceDHW4_8_v2.csv"); tsdhw<-subset(tsdhw,select= -c(X,ISLAND))
wave<-read.csv("T:/Benthic/Projects/Juvenile Project/Pacific_WaveActionData_v4.csv")
load("C:/Users/Courtney.S.Couch/Documents/GitHub/env_data_summary/outputs/Survey_Master_Timeseries_2022-04-04.Rdata")
cover1<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2020_Tier1_SITE.csv")#Cover from all sites
cover3<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2020_Tier3_SITE.csv")#Cover from all sites
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)
human<-read.csv("T:/Benthic/Projects/Juvenile Project/Data/JuvCoralSitse_w_pop_den.csv", stringsAsFactors=FALSE)

#Change Regions to correspond to juvenile data
Convert_Region<-function(data){
  data$REGION<-ifelse(data$ISLAND %in% c("Farallon de Pajaros", "Maug", "Asuncion", "Alamagan", "Pagan", "Agrihan", "Guguan", "Sarigan","Farallon_de_Pajaros")
                      ,"NMI", as.character(data$REGION))
  data$REGION<-ifelse(data$ISLAND %in% c("Saipan", "Tinian", "Aguijan", "Rota", "Guam")
                    ,"SMI", as.character(data$REGION))
  data$REGION<-ifelse(data$ISLAND %in% c("Howland","Baker")
                    ,"PHOENIX", as.character(data$REGION))
  data$REGION<-ifelse(data$ISLAND =="Wake"
                    ,"WAKE", as.character(data$REGION))
  data$REGION<-ifelse(data$ISLAND %in% c("Kingman","Palmyra","Jarvis")
                    ,"LINE", as.character(data$REGION))
  return(data$REGION)
}

SM$REGION<-Convert_Region(SM)
cover1$REGION<-Convert_Region(cover1)
cover3$REGION<-Convert_Region(cover3)
sectors$REGION<-Convert_Region(sectors)
jwd_site$REGION<-Convert_Region(jwd_site)
SM$STRATANAME<-paste(SM$SEC_NAME,SM$REEF_ZONE,SM$DEPTH_BIN,sep="_")


#Remove spaces in island and sec names
jwd_site <- mutate_if(jwd_site, 
                is.character, 
                str_replace_all, pattern = " ", replacement = "_")

wave<-mutate_if(wave, 
                is.character, 
                str_replace_all, pattern = " ", replacement = "_")
cover1<-mutate_if(cover1, 
              is.character, 
              str_replace_all, pattern = " ", replacement = "_")

cover3<-mutate_if(cover3, 
                  is.character, 
                  str_replace_all, pattern = " ", replacement = "_")

sectors<-mutate_if(sectors, 
                  is.character, 
                  str_replace_all, pattern = " ", replacement = "_")

SM<-mutate_if(SM, 
                is.character, 
              str_replace_all, pattern = " ", replacement = "_")

#Clean up juv site data 
#removing islands and years where special missions were conducted on StRS surveys were only conducted at a small subset of islands within a region
levels(jwd_site$MISSIONID)
jwd_site<-jwd_site[!jwd_site$MISSIONID %in% c("MP1410","MP1512","MP1602","SE1602","MP2006"),] #SE1602 = 2016 Rose and Jarvis
jwd_site$Year_Island<-paste(jwd_site$OBS_YEAR,jwd_site$ISLAND,sep="_")
jwd_site$Year_Region<-paste(jwd_site$OBS_YEAR,jwd_site$REGION,sep="_")
jwd_site<-jwd_site[!jwd_site$Year_Island %in% c("2017_Baker","2017_Jarvis","2017_Howland"),] 
#jwd_site<-subset(jwd_site,Year_Region !="2013_MHI")
jwd_site<-jwd_site[!jwd_site$ISLAND %in% c("Laysan","Midway","Maro"),]#remove these islands since they weren't surveyed in all years

jwd_site<-droplevels(jwd_site);levels(jwd_site$MISSIONID)
jwd_site$STRATANAME<-paste(jwd_site$SEC_NAME,jwd_site$REEF_ZONE,jwd_site$DEPTH_BIN,sep="_")

#View(jwd_site)
nrow(jwd_site)

#Merge depth data in
jwd_site<-left_join(jwd_site,SM[,c("SITEVISITID","new_MAX_DEPTH_M","new_MIN_DEPTH_M","LATITUDE_LOV", "LONGITUDE_LOV")])
jwd_site$LATITUDE<-jwd_site$LATITUDE_LOV
jwd_site$LONGITUDE<-jwd_site$LONGITUDE_LOV
nrow(jwd_site)


#Columns to keep
jcols<-c("DATE_","SITEVISITID", "ANALYSIS_YEAR","OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE","HABITAT_CODE","REEF_ZONE","new_MIN_DEPTH_M","new_MAX_DEPTH_M",
         "DEPTH_BIN","TRANSECTAREA_j","GENUS_CODE","JuvColCount","JuvColDen","Year_Island","STRATANAME","LATITUDE", "LONGITUDE",
         "n","NH","sw","HumanDen")
jwd_site<-jwd_site[,jcols]
head(jwd_site)


#Calculate median depth
jwd_site<-jwd_site %>%
  rowwise() %>%
  mutate(Depth_Median=median(c(new_MIN_DEPTH_M,new_MAX_DEPTH_M)))


#Subset survey master and env columns of interest -not prior to 10/14/22, I was using the 750m VIIRS Chla data, but the data only goes back to 2012. Swaping in 4km data

cols<-c("MISSIONID","DATE_","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE","REEF_ZONE",
               "DEPTH_BIN","STRATANAME", "DHW.MeanMax_Degree_Heating_Weeks_YR01","DHW.MeanMax_Degree_Heating_Weeks_YR03", 
        "DHW.MeanMax_Degree_Heating_Weeks_YR05","DHW.MeanMax_Degree_Heating_Weeks_YR10","DHW.MeanMax_Degree_Heating_Weeks_YR10YR01",
        "DHW.MaxMax_Degree_Heating_Weeks_YR03","DHW.MaxMax_Degree_Heating_Weeks_YR05","DHW.MaxMax_Degree_Heating_Weeks_YR10","DHW.Np10y_Major_Degree_Heating_Weeks_YR10",
        "mean_SST_CRW_Daily_YR10","sd_SST_CRW_Daily_YR10","mean_Chlorophyll_A_ESAOCCCI_8Day_YR05","sd_Chlorophyll_A_ESAOCCCI_8Day_YR05","mean_biweekly_range_SST_CRW_Daily_YR10")
sm_env<-SM[,cols]

#Not enough sampling in each sector- pool them together
sm_env$SEC_NAME<-ifelse(sm_env$SEC_NAME %in% c("TAU_OPEN","TAU_SANCTUARY"),"TAU",as.character(sm_env$SEC_NAME))
sm_env$SEC_NAME<-ifelse(sm_env$SEC_NAME %in% c("SWA_OPEN","SWA_SANCTUARY"),"SWA",as.character(sm_env$SEC_NAME))
View(sm_env)

#Calculate CVchla and CVsst
sm_env$CVsst<-sm_env$sd_SST_CRW_Daily_YR10/sm_env$mean_SST_CRW_Daily_YR10
sm_env$CVchla<-sm_env$sd_Chlorophyll_A_ESAOCCCI_8Day_YR05/sm_env$mean_Chlorophyll_A_ESAOCCCI_8Day_YR05

# #Change NaN to NA
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

sm_env[is.nan(sm_env)] <- NA

#Combine Tier 1 and 3 cover
cover<-left_join(cover3,cover1[,c("SITEVISITID","CORAL","MA","TURF","SED")])  
nrow(cover);View(cover)

#Create summarized benthic columns
cover$RUBBLE<-cover$CCAR+cover$RUB+cover$TURFR
cover$TURF_BARE<-cover$TURFH+cover$HARD
cover$CCA<-cover$CCAH+cover$CCAR
cover$SAND<-cover$SED
cover$TURF<-cover$TURFH
cover$EMA_MA<-cover$EMA+cover$PESP+cover$MA

#Consolidate columns and combine rubble and sand
cols<-c("SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE","REEF_ZONE",
        "DEPTH_BIN", "LATITUDE", "LONGITUDE","CORAL","CCA","RUBBLE","SAND","TURF","MA","EMA_MA")
cover<-cover[,cols]
cover$SAND_RUB<-cover$RUBBLE+cover$SAND
head(cover)

#Make tweaks to pooling sector pooling
#Not enough sampling in each sector- pool them together
cover$SEC_NAME<-ifelse(cover$SEC_NAME %in% c("TAU_OPEN","TAU_SANCTUARY"),"TAU",as.character(cover$SEC_NAME))
cover$SEC_NAME<-ifelse(cover$SEC_NAME %in% c("SWA_OPEN","SWA_SANCTUARY"),"SWA",as.character(cover$SEC_NAME))
View(cover)

cover$STRATANAME<-paste(cover$SEC_NAME,cover$REEF_ZONE,cover$DEPTH_BIN,sep="_") #Create stratum

#Calculate strata mean cover for- 

cover_st<-cover %>%
  group_by(OBS_YEAR,STRATANAME) %>%
  summarize(CORALst=mean(CORAL,na.rm=T),
            CCAst=mean(CCA,na.rm=T),
            SAND_RUBst=mean(SAND_RUB,na.rm=T),
            TURFst=mean(TURF,na.rm=T),
            EMA_MAst=mean(EMA_MA,na.rm=T),)


#remove NAs from tsdhw
tsdhw<-tsdhw %>% filter(!is.na(YearSinceDHW4))

# Combine juvenile and predictor data at site-level -----------------------

cover_forsite<-cover %>% select(SITE,CORAL,CCA,SAND_RUB,TURF,EMA_MA)
sm_env<-sm_env %>% select(SITE,SITEVISITID,DHW.MeanMax_Degree_Heating_Weeks_YR01,DHW.MaxMax_Degree_Heating_Weeks_YR03,DHW.MeanMax_Degree_Heating_Weeks_YR03,
                          DHW.MeanMax_Degree_Heating_Weeks_YR05,DHW.MaxMax_Degree_Heating_Weeks_YR05,DHW.MeanMax_Degree_Heating_Weeks_YR10,DHW.MaxMax_Degree_Heating_Weeks_YR10,
                          DHW.MeanMax_Degree_Heating_Weeks_YR10YR01,DHW.Np10y_Major_Degree_Heating_Weeks_YR10,mean_biweekly_range_SST_CRW_Daily_YR10,
                          CVchla,CVsst,mean_SST_CRW_Daily_YR10,mean_Chlorophyll_A_ESAOCCCI_8Day_YR05,sd_SST_CRW_Daily_YR10,
                          sd_Chlorophyll_A_ESAOCCCI_8Day_YR05)
wave<-wave %>% select(SITE,WavePower)

all_pred_site<- jwd_site   %>% 
  left_join(cover_forsite) %>%
  left_join(tsdhw) %>%
  left_join(sm_env) %>%
  left_join(wave)


nrow(jwd_site)
nrow(all_pred_site)

all_pred_site$REGION_YEAR<-paste(all_pred_site$REGION,all_pred_site$OBS_YEAR,sep="_")
all_pred_site<-dplyr::filter(all_pred_site,REGION_YEAR !="NWHI_2014")

all_pred_site<-left_join(all_pred_site,cover_st)
nrow(all_pred_site)

#More manual tweaks
#there are some sites that don't have cover- sub with strata means for a given year
all_pred_site$CORAL<-ifelse(is.na(all_pred_site$CORAL),all_pred_site$CORALst,all_pred_site$CORAL)
all_pred_site$CCA<-ifelse(is.na(all_pred_site$CCA),all_pred_site$CCAst,all_pred_site$CCA)
all_pred_site$SAND_RUB<-ifelse(is.na(all_pred_site$SAND_RUB),all_pred_site$SAND_RUBst,all_pred_site$SAND_RUB)
all_pred_site$TURF<-ifelse(is.na(all_pred_site$TURF),all_pred_site$TURFst,all_pred_site$TURF)
all_pred_site$EMA_MA<-ifelse(is.na(all_pred_site$EMA_MA),all_pred_site$EMA_MAst,all_pred_site$EMA_MA)


#Missing depths- went back to datasheets or use max depth
all_pred_site$Depth_Median<-ifelse(all_pred_site$SITE=="TIN-00646",21.3,all_pred_site$Depth_Median)
all_pred_site$Depth_Median<-ifelse(all_pred_site$SITE=="TIN-00581",13.7,all_pred_site$Depth_Median)
all_pred_site$Depth_Median<-ifelse(all_pred_site$SITE=="WAK-00430",9.44,all_pred_site$Depth_Median)
all_pred_site$Depth_Median<-ifelse(all_pred_site$SITE=="TUT-01987",22.1,all_pred_site$Depth_Median)
all_pred_site$Depth_Median<-ifelse(is.na(all_pred_site$Depth_Median),all_pred_site$new_MAX_DEPTH_M,all_pred_site$Depth_Median)#missing min depth from several 2013 MHI sites


View(all_pred_site)

#Check for duplicate sites
all_pred_site %>% 
  group_by(SITE) %>% 
  filter(n()>1)


write.csv(all_pred_site, file="T:/Benthic/Projects/Juvenile Project/JuvDen_Pred_SITE_AllYears.csv",row.names = F)



# # Time Since DHW ----------------------------------------------------------
# tsdhw<-tsdhw %>% drop_na(ISLAND) #drop rows that have NA values in ISLAND
# tsdhw_meta<-left_join(SM,tsdhw) # We don't have 2017 cover data from NWHI so we have to merge this with SM not sm_env
# ts_sum<-tsdhw_meta %>%
#   group_by(REGION,ISLAND,SEC_NAME,STRATANAME) %>%
#   summarize(YearSinceDHW4=mean(YearSinceDHW4,na.rm=T),
#             YearSinceDHW8=mean(YearSinceDHW8,na.rm=T))
# 
# #Temporary fix for Palmyra and Kingman- raw DHW data are incorrect for these 2 reefs
# ts_sum$YearSinceDHW4<-ifelse(ts_sum$ISLAND=="Kingman",2.1428571,ts_sum$YearSinceDHW4)
# ts_sum$YearSinceDHW4<-ifelse(ts_sum$ISLAND=="Palmyra",2.1428571,ts_sum$YearSinceDHW4)
# ts_sum$YearSinceDHW4<-ifelse(ts_sum$ISLAND=="Tinian",NA,ts_sum$YearSinceDHW4)
# 
# ts_sum$YearSinceDHW8<-ifelse(ts_sum$ISLAND=="Kingman",NA,ts_sum$YearSinceDHW8)
# ts_sum$YearSinceDHW8<-ifelse(ts_sum$ISLAND=="Palmyra",2.221429,ts_sum$YearSinceDHW8)
# ts_sum$YearSinceDHW4<-ifelse(ts_sum$ISLAND=="Tinian",NA,ts_sum$YearSinceDHW4)
# 


# BENTHIC COVER -----------------------------------------------------------


#We are missing some cover data from some benthic sites so use all benthic and fish sites
#Merge together wsd and sectors

sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

strataKEEP<-unique(all_pred_site$STRATANAME)
cover<-dplyr::filter(cover,STRATANAME %in% strataKEEP)

#Add NH for forereef and protected reef sites
#Not enough sampling in each sector- pool them together
sectors$SEC_NAME<-ifelse(sectors$SEC_NAME %in% c("TAU_OPEN","TAU_SANCTUARY"),"TAU",as.character(sectors$SEC_NAME))
sectors$SEC_NAME<-ifelse(sectors$SEC_NAME %in% c("SWA_OPEN","SWA_SANCTUARY"),"SWA",as.character(sectors$SEC_NAME))

tmp<-ddply(sectors,.(SEC_NAME,REEF_ZONE,DEPTH_BIN,AREA_HA),
           summarize, temp=length(ISLAND))
new.NH<-ddply(tmp,.(SEC_NAME,REEF_ZONE,DEPTH_BIN),
              summarize, new.Area=sum(AREA_HA))
nrow(sectors)
sectors<-left_join(sectors,new.NH)
sectors<-subset(sectors,select=-c(AREA_HA))
sectors<-sectors %>% rename(AREA_HA=new.Area)
nrow(sectors)

sectors<-mutate_if(sectors, 
                  is.character, 
                  str_replace_all, pattern = " ", replacement = "_")



wsd<-left_join(cover,sectors[,c("SEC_NAME","REEF_ZONE","DEPTH_BIN","AREA_HA")]);nrow(wsd);head(wsd)

#Subset just Forereef sites
# wsd$REEF_ZONE<-ifelse(wsd$REEF_ZONE=="Protected Slope","Forereef",as.character(wsd$REEF_ZONE))
# wsd<-subset(wsd,REEF_ZONE=="Forereef")

wsd$SEC_NAME<-wsd$SEC_NAME
wsd$ANALYSIS_YEAR<-wsd$OBS_YEAR

data.cols<-c("CORAL","CCA","RUBBLE","SAND","TURF","MA","SAND_RUB")

### CALCULATE MEAN AND VARIANCE WITHIN STRATA ###
SPATIAL_POOLING_BASE<-c("REGION","ISLAND", "SEC_NAME", "REEF_ZONE", "STRATANAME","DEPTH_BIN")    
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
OUTPUT_LEVEL<-c("REGION","ISLAND","SEC_NAME","STRATANAME","DEPTH_BIN","ANALYSIS_YEAR") 
dpst<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA");dpst<-as.data.frame(dpst)

#Clean up- remove SE columns and remove "Mean" from column names
dpst<-dpst %>% dplyr::select(Mean.REGION:Mean.SAND_RUB,-c(Mean.N))

dpst<-dpst %>%
  dplyr::rename_all(funs(stringr::str_replace_all(., "Mean.", "")))
head(dpst)

cover_sum<-dpst
head(cover_sum)

cover_sum3<-cover_sum

cover_sum2<-cover_sum%>% dplyr::filter(ANALYSIS_YEAR>2013)

#SAVE BY SECTOR PER YEAR
OUTPUT_LEVEL<-c("REGION","ISLAND","SEC_NAME","ANALYSIS_YEAR") 
dpsec<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")

dpsec<-as.data.frame(dpsec)
dpsec<-dpsec %>% dplyr::select(Mean.REGION:Mean.SAND_RUB,-c(Mean.N))

dpsec<-dpsec %>%
  dplyr::rename_all(funs(stringr::str_replace_all(., "Mean.", "")))
head(dpsec)

colnames(dpsec)[c(5:11)]<-paste(colnames(dpsec)[c(5:11)],"sec",sep="_")

write.csv(dpsec, file="T:/Benthic/Projects/Juvenile Project/BenthicCover_JuvenileProject_Tier1_SECTOR.csv",row.names = F)



# #Generate a list of Regions and years to include in final summary
REGION<-c("NWHI","MHI","PHOENIX","LINE","SMI","NMI","SAMOA","WAKE")
ANALYSIS_YEAR<-c("2016","2019","2018","2018","2017","2017","2018","2017")
keep<-as.data.frame(cbind(REGION,ANALYSIS_YEAR))

keep$r_y<-paste(keep$REGION,keep$ANALYSIS_YEAR,sep = "_")

#ONly include years where sites were most recently surveyed for each region
cover_sum$r_y<-paste(cover_sum$REGION,cover_sum$ANALYSIS_YEAR,sep = "_")
cover_sum<-cover_sum[cover_sum$r_y %in% c(keep$r_y),]


#CONVERT TO WIDE THEN CALCULATE DELTA CHANGE in cover
#Calculate absolute % cover change between years
CalcDeltaCover<-function(data,data.col,metric_name="DeltaCCA"){
  data$D_COL<-data[,data.col]
  #Convert long to wide
  wide<-data %>%
    dplyr::select(REGION,ISLAND,ANALYSIS_YEAR,SEC_NAME,STRATANAME,DEPTH_BIN,D_COL) %>%
    pivot_wider(names_from = ANALYSIS_YEAR,values_from = D_COL)
  
  colnames(wide)[6:ncol(wide)] <- paste("t", colnames(wide[,c(6:ncol(wide))]), sep = "") #add "t" to year column
  
  wide$DeltaCover<-NULL
  for (i in c(1:nrow(wide))){ #opening brace
    if(wide$REGION[i] %in% c("SMI","NMI","WAKE")){ #c&p
      wide$DeltaCover[i] = wide$t2017[i]-wide$t2014[i] #c&p
    } #c&P
    if(wide$REGION[i] %in% c("SAMOA","LINE","PHOENIX")){ #c&p
      wide$DeltaCover[i] = wide$t2018[i]-wide$t2015[i] #c&p
    } #c&P
    if(wide$REGION[i] == "MHI"){ #c&p
      wide$DeltaCover[i] = wide$t2019[i]-wide$t2016[i] #c&p
    } #c&P
    if(wide$REGION[i] == "NWHI"){ #c&p
      wide$DeltaCover[i] = wide$t2016[i]-wide$t2014[i] #c&p
    } #c&P
  } #closing curly brace for entire forloop
  colnames(wide)[which(colnames(wide) == 'DeltaCover')] <- metric_name #change group to whatever your grouping field is.
  
  wide<- dplyr::select(wide,-c(t2010:t2019))
  return(wide)
}
#Calc Delta CCA and Coral cover
CCA_sumW<-CalcDeltaCover(cover_sum3,"CCA","Delta_CCA");head(CCA_sumW)
Coral_sumW<-CalcDeltaCover(cover_sum3,"CORAL","Delta_CORAL");head(Coral_sumW)
Turf_sumW<-CalcDeltaCover(cover_sum3,"TURF","Delta_TURF");head(Turf_sumW)
SandRub_sumW<-CalcDeltaCover(cover_sum3,"SAND_RUB","Delta_SAND_RUB");head(SandRub_sumW)

delcover<- CCA_sumW   %>%    
  left_join(Coral_sumW) %>%
  left_join(Turf_sumW) %>%
  left_join(SandRub_sumW)

head(delcover)
nrow(delcover)
nrow(CCA_sumW)

cover_sum2$ANALYSIS_YEAR<-as.factor(cover_sum2$ANALYSIS_YEAR)

p<-ggplot(subset(cover_sum3,REGION=="SAMOA"),aes(x=ANALYSIS_YEAR,y=CCA,fill=DEPTH_BIN))+
    geom_bar(stat='identity')+
    facet_wrap(~SEC_NAME,scale='free_x')+
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90)
      ,plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      ,axis.ticks.x = element_blank() # no x axis ticks
      ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
      ,legend.position="bottom")
p

p<-ggplot(subset(cover_sum2,REGION=="MHI"),aes(x=ANALYSIS_YEAR,y=CCA,fill=DEPTH_BIN))+
  geom_bar(stat='identity')+
  facet_wrap(~SEC_NAME,scale='free_x')+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="bottom")
p

p<-ggplot(subset(cover_sum2,REGION=="LINE"),aes(x=ANALYSIS_YEAR,y=CCA,fill=DEPTH_BIN))+
  geom_bar(stat='identity')+
  facet_wrap(~SEC_NAME,scale='free_x')+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="bottom")
p


#Plot delta cover
#convert wide to long
library(stringr)
delcoverN<-delcover %>% 
  rename_all(~stringr::str_replace(.,"Delta_",""))

delcoverL<-pivot_longer(delcoverN,
                        cols = CCA:SAND_RUB,
                        names_to = "Category",
                        values_to = "DeltaCover")

#Calculate Island-level means- These are unweighted and therefore incorrect- just for curiosity sake
# delcover_sum<-delcoverL %>%
#   group_by(REGION,ISLAND,Category) %>%
#   summarize(MeanDelta=mean(DeltaCover),na.rm=T),SEDelta=mean(MAX_DEPTH_M,na.rm=T))

p<-ggplot(delcoverL,aes(x=Category,y=DeltaCover,fill=Category))+
  geom_boxplot()+
  geom_hline(yintercept=1)+
  facet_wrap(~REGION)+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="bottom")
p



# Combine all summaries into 1 dataframe ----------------------------------
all_pred<- d_strat   %>%    
      left_join(cover_sum) %>%
      left_join(delcover) %>%
      left_join(depth_sum) %>%
      left_join(hab_sum_w) %>%
      left_join(lat_sum) %>%
      left_join(eds_sum) %>%
      left_join(sh_sum) %>%
      left_join(humans_sum) %>%
      left_join(wave_sum)%>%
      left_join(ts_sum)

head(all_pred)
View(all_pred)

write.csv(all_pred, file="T:/Benthic/Projects/Juvenile Project/JuvDeltaDen_Pred.csv",row.names = F)


# #Change specific sector names to match the sector shape file polygons
# dpst$SEC_NAME[dpst$SEC_NAME == "HAW_HAMAKUA"] <- "HAW_HAMAK"
# dpst$SEC_NAME[dpst$SEC_NAME == "GUA_PATI_POINT"] <- "GUA_PATI_PT"
# 
# #Separate strata to create sector maps in Arc
# sh<-subset(dpst,DEPTH_BIN=="Shallow")
# m<-subset(dpst,DEPTH_BIN=="Mid")
# d<-subset(dpst,DEPTH_BIN=="Deep")
# 
# write.csv(sh, file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Predictor Variables/JuvProject_SubstrateHeight_Shallow.csv",row.names = F)
# write.csv(m, file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Predictor Variables/JuvProject_SubstrateHeight_Mid.csv",row.names = F)
# write.csv(d, file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Predictor Variables/JuvProject_SubstrateHeight_Deep.csv",row.names = F)
# 

#Plot mean values for predictors by sector and/or stratum to make sure values make sense

all_pred<-subset(all_pred,select=-c(SEC_NAME,ANALYSIS_YEAR,years,DeltaDen))
all_pred_l<- all_pred %>% pivot_longer(
  cols = DeltaDen_mo:MeanWavePower,
  names_to = "METRIC",
  values_to = "Value")


Plot_Preds<-function(data,metric_name="MeanWavePower"){
  data$METRIC=data[,metric_name]
  # data$METRIC<-data$metric_name
  p<-ggplot(data,aes(x=SEC_NAME,y=METRIC,fill=DEPTH_BIN))+
    geom_bar(position="stack", stat="identity")+
    facet_wrap(~REGION,scale='free_x')+
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90)
      ,plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      ,axis.ticks.x = element_blank() # no x axis ticks
      ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
      ,legend.position="bottom")
 return(p) 
}


WavePlot<-Plot_Preds(all_pred,"MeanWavePower");WavePlot
CORALPlot<-Plot_Preds(all_pred,"CORAL");CORALPlot
CCAPlot<-Plot_Preds(all_pred,"CCA");CCAPlot
BasaltPlot<-Plot_Preds(all_pred,"Basalt_HC");BasaltPlot
MeanMaxDHWPlot<-Plot_Preds(all_pred,"MeanMaxDHW10");MeanMaxDHWPlot
WavePlot<-Plot_Preds(all_pred,"MeanWavePower");WavePlot








