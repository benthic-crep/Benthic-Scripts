#This script is a modified version of the Benthic Cover_RawtoEstimates script that generates strata, sector and island summaries from site level data
#After discussion amongst the benthic and fish teams regarding pooling strategies we decided that if individuals are interested in comparing strata, sectors and islands across time
#it is not appropriate to include all strata surveyed in a given year because the same strata may not have been sampled across years due to logistical constraints.
#In this script, we identify which strata were sampled in multiple years then subset those strata. It may not be appropriate to use island-level estimates because the island data generated in this
#script may only consist of a few strata.



rm(list=ls())

setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/BIA")

library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below
library(RODBC)
# to connect to oracle

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/fish_team_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/Islandwide Mean&Variance Functions.R")

#Read in Tier 1 and 3 site level data from Benthic Cover_RawtoEstimates_v2 script (includes all sites and all years)
wsd_t1<-read.csv("T:/Benthic/Data/Data Requests/2021 IEA/BenthicCover_2010-2020_Tier1_SITE.csv")
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv")


# CHECK THAT DATA IS READY FOR POOLING AND DO SOME FINAL CLEAN UPS --------

#Identify which taxonomic level you would like to summarize

wsd<-wsd_t1

#Define data columns
T1data.cols<-colnames(subset(wsd,select=c(CCA:TURF,CCA_CORAL,BSR)))

data.cols<-T1data.cols

#remove permanent sites, climate sites and special projects
wsd$PERM_SITE[is.na(wsd$PERM_SITE)]<-"0"
wsd$TRANSECT_PHOTOS[is.na(wsd$TRANSECT_PHOTOS)]<-"0"
wsd<-wsd[!wsd$MISSIONID%in% c("MP1410","MP1512","MP1602","MP2006","FAGAALU1","FAGAALU2"),] #I left SE1602 in (2016 Jarvis and Rose)

#Remove occ sites,sites with exclude flag =-1 and permanent sites
wsd<-subset(wsd,is.na(OCC_SITEID)& EXCLUDE_FLAG!="-1"& PERM_SITE!=-1 & Oceanography !=1)

View(wsd)
#Check analysis sector names & make sure number of sites match SURVEY master file
wsd<-droplevels(wsd)
levels(wsd$MISSIONID)
table(wsd$SEC_NAME, wsd$OBS_YEAR)


## check whether we have ISLANDS that arent in the sectors file
setdiff(unique(wsd$ISLAND),unique(sectors$ISLAND))

#set all Backreef to a single DEPTH_ZONE ("All") 
levels(wsd$DEPTH_BIN)<-c(levels(wsd$DEPTH_BIN), "All")
levels(sectors$DEPTH_BIN)<-c(levels(sectors$DEPTH_BIN), "All")
wsd[wsd$REEF_ZONE=="Backreef",]$DEPTH_BIN<-"All"
sectors[sectors$REEF_ZONE=="Backreef",]$DEPTH_BIN<-"All"

wsd$DEPTH_BIN<-as.character(wsd$DEPTH_BIN)# won't change value to "All" if it is a factor
wsd[wsd$ISLAND=="Rose" & wsd$REEF_ZONE=="Lagoon",]$DEPTH_BIN<-"All"
sectors[sectors$ISLAND=="Rose" & sectors$REEF_ZONE=="Lagoon",]$DEPTH_BIN<-"All"
wsd$DEPTH_BIN<-as.factor(wsd$DEPTH_BIN)# change back to factor

wsd$STRATA<-paste(substring(wsd$REEF_ZONE,1,1), substring(wsd$DEPTH_BIN,1,1), sep="")
sectors$STRATA<-paste(substring(sectors$REEF_ZONE,1,1), substring(sectors$DEPTH_BIN,1,1), sep="")


#Change Analysis year according to desired pooling
wsd[is.na(wsd$ANALYSIS_YEAR),]
levels(wsd$ANALYSIS_YEAR)<-c(levels(wsd$ANALYSIS_YEAR), "2015-16")
wsd[wsd$REGION %in% c("MHI", "NWHI") & wsd$OBS_YEAR %in% seq(2010,2012),]$ANALYSIS_YEAR<-"2010-12" #We had poor sampling in both years for all islands so we are pooling them together
wsd[wsd$REGION %in% c("MHI", "NWHI") & wsd$OBS_YEAR %in% seq(2013,2015),]$ANALYSIS_YEAR<-"2013-15"#We had poor sampling in both years for some islands so we are pooling them together- we could probably separate years for some of the islands/sectors- I don't love this
wsd[wsd$SEC_NAME %in% c("TAU_SANCTUARY") & wsd$OBS_YEAR %in% seq(2010,2012),]$ANALYSIS_YEAR<-"2010-12" #Poor sampling in earlier years-pooling for these sectors
wsd[wsd$SEC_NAME %in% c("TUT_AUNUU_A") & wsd$OBS_YEAR %in% seq(2010,2012),]$ANALYSIS_YEAR<-"2010-12"
wsd[wsd$SEC_NAME %in% c("TUT_AUNUU_B") & wsd$OBS_YEAR %in% seq(2010,2012),]$ANALYSIS_YEAR<-"2010-12"
wsd[wsd$SEC_NAME %in% c("TUT_FAGATELE") & wsd$OBS_YEAR %in% seq(2010,2012),]$ANALYSIS_YEAR<-"2010-12"
wsd[wsd$SEC_NAME %in% c("TUT_FAGALUA") & wsd$OBS_YEAR %in% seq(2010,2012),]$ANALYSIS_YEAR<-"2010-12"

#set all Backreef to a single DEPTH_ZONE ("All") 
wsd$STRATA<-paste(wsd$REEF_ZONE, wsd$DEPTH_BIN, sep="_")
sectors$STRATA<-paste(sectors$REEF_ZONE, sectors$DEPTH_BIN, sep="_")

wsd$ANALYSIS_SEC<-wsd$SEC_NAME


#Merge AREA_HA into dataframe
sectors$STRATANAME<-paste(sectors$SEC_NAME,sectors$STRATA,sep = "_")
wsd$STRATANAME<-paste(wsd$SEC_NAME,wsd$STRATA,sep = "_")

wsd<-left_join(wsd,sectors[,c("STRATANAME","AREA_HA")])

#Double check that years are pooled correctly
table(wsd$SEC_NAME,wsd$ANALYSIS_YEAR)


#NOW CHECK HOW MANY REPS WE HAVE PER STRATA
a<-dcast(wsd, ANALYSIS_SCHEME + ISLAND + ANALYSIS_SEC + OBS_YEAR ~ STRATA, value.var="AREA_HA", length); a

# GENERATE DATA FOR TEMPORAL ANALYSIS - Sectors are only included if they: the strata have at least 2 sites and all strata are surveyed in all years
#in the previous version of the data (MHICover2010-_Tier1_Sector_TimeSeries_Gove_v2) I did not ensure that sectors had all 3 strata. 
#8/18/22- I'm correcting this issue


#Subset Main Hawaiian islands
wsd<-subset(wsd,REGION=="MHI")

# Subset Strata and Sectors that were surveyed in more than 1 year -------

#Drop stata with less than 2 sites
st.list<-ddply(wsd,.(ANALYSIS_YEAR,REGION,ISLAND,ANALYSIS_SEC ,STRATANAME),summarize,n=length(unique(SITEVISITID)))
st.list2<-st.list %>% dplyr::filter(n>=2);head(st.list2) #remove strata that have less than 2 sites/stratum

#Drop sectors with less than 3 strata- important for temporal analysis to compare sectors with all strata
st.list2<-ddply(st.list2,.(ANALYSIS_YEAR,REGION,ISLAND,ANALYSIS_SEC),summarize,n=length(unique(STRATANAME)))
st.list3<-st.list2 %>% dplyr::filter(n==3);head(st.list3) #remove strata that have less than 2 sites/stratum
st.list3$SEC_YEAR<-paste(st.list3$ANALYSIS_SEC,st.list3$ANALYSIS_YEAR)
wsd$SEC_YEAR<-paste(wsd$ANALYSIS_SEC,wsd$ANALYSIS_YEAR)


wsd2<-wsd[wsd$SEC_YEAR %in% c(st.list3$SEC_YEAR),] #Subset data to only include sectors_years with strata with 2 sites and all 3 strata
wsd2<-separate(wsd2,STRATA,c("REEFZONE","DEPTH_BIN"),sep="_");wsd2<-dplyr::select(wsd2,-REEFZONE)

# POOL WSD (WORKING SITE DATA TO STRATA THEN TO HIGHER LEVELS -------------

### CALCULATE MEAN AND VARIANCE WITHIN STRATA ###
SPATIAL_POOLING_BASE<-c("REGION","ISLAND", "ANALYSIS_SEC", "REEF_ZONE","DEPTH_BIN", "STRATANAME")    
ADDITIONAL_POOLING_BY<-c("ANALYSIS_YEAR")                                    # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)

#generate within strata means and vars
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
dps<-Calc_PerStrata(wsd2, data.cols, c(POOLING_LEVEL, "AREA_HA"))

#SAVE BY STRATUM PER YEAR
###### REMOVE STRATA with N=1 (cannot pool those up)
dps$Mean<-dps$Mean[dps$Mean$N>1,]
dps$SampleVar<-dps$SampleVar[dps$SampleVar$N>1,]
dps$SampleSE<-dps$SampleSE[dps$SampleSE$N>1,]

dpst<-dps
colnames(dpst$Mean)[10:ncol(dpst$Mean)] <- paste("Mean.", colnames(dpst$Mean[,10:ncol(dpst$Mean)]), sep = "")
colnames(dpst$SampleSE)[10:ncol(dpst$SampleSE)] <- paste("SE.", colnames(dpst$SampleSE[,10:ncol(dpst$SampleSE)]), sep = "")
dpst<-left_join(dpst$Mean,dpst$SampleSE)


# e.g. SAVE BY SECTOR PER YEAR
OUTPUT_LEVEL<-c("REGION","ISLAND","ANALYSIS_SEC","ANALYSIS_YEAR") 
dpsec<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
dpsec<-as.data.frame(dpsec)
dpsec<-subset(dpsec,select=-c(PooledSE.REGION:PooledSE.N))
colnames(dpsec)[1:5]<-sub("Mean.","",colnames(dpsec)[1:5]) #remove mean from metadata columns

nstrat<-ddply(wsd,.(ANALYSIS_SEC,ANALYSIS_YEAR),
              summarize,
              nstrat=length(unique(STRATANAME)))

dpsec<-left_join(dpsec,nstrat)

dpsec<-subset(dpsec,select= -c(Mean.EMA,Mean.HAL,Mean.I,Mean.SC, Mean.SED,Mean.TOT_AREA_WT,
                               PooledSE.EMA,PooledSE.HAL,PooledSE.I,PooledSE.SC, PooledSE.SED))


write.csv(dpsec, file="T:/Benthic/Data/Data Requests/2021 IEA/MHICoverAllYears_Sector_IEA.csv",row.names=F)

table(dpsec$ANALYSIS_SEC,dpsec$ANALYSIS_YEAR)

old<-read.csv("T:/Benthic/Data/Data Requests/2021 IEA/MHICover2010-_Tier1_Sector_TimeSeries_Gove_v2.csv")

table(old$ANALYSIS_SEC,old$ANALYSIS_YEAR)

# Generate estimates of mean benthic cover at shal, mid and deep sites for each island for --------
#EXCLUDE strata that have <2 sites and sectors that don't have all 3 strata


### CALCULATE MEAN AND VARIANCE WITHIN STRATA ###
SPATIAL_POOLING_BASE<-c("REGION","ISLAND", "ANALYSIS_SEC", "REEF_ZONE","DEPTH_BIN", "STRATANAME")    
ADDITIONAL_POOLING_BY<-c("ANALYSIS_YEAR")                                    # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)

#generate within strata means and vars - ALL YEARS
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
dps<-Calc_PerStrata(wsd2, data.cols, c(POOLING_LEVEL, "AREA_HA"))

#SAVE BY STRATUM PER YEAR
###### REMOVE STRATA with N=1 (cannot pool those up)
dps$Mean<-dps$Mean[dps$Mean$N>1,]
dps$SampleVar<-dps$SampleVar[dps$SampleVar$N>1,]
dps$SampleSE<-dps$SampleSE[dps$SampleSE$N>1,]


dpst<-dps
colnames(dpst$Mean)[10:ncol(dpst$Mean)] <- paste("Mean.", colnames(dpst$Mean[,10:ncol(dpst$Mean)]), sep = "")
colnames(dpst$SampleSE)[10:ncol(dpst$SampleSE)] <- paste("SE.", colnames(dpst$SampleSE[,10:ncol(dpst$SampleSE)]), sep = "")

dpst<-left_join(dpst$Mean,dpst$SampleSE)

kona<-subset(dpst,ANALYSIS_SEC=="HAW_KONA")
head(kona)

write.csv(kona, file="T:/Benthic/Data/Data Requests/2021 IEA/MHICover__HAWAII_KONA_STRATA_IEA.csv",row.names=F)


wsd_19<-filter(wsd2,ANALYSIS_YEAR =="")

#generate within strata means and vars
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
dps<-Calc_PerStrata(wsd_19, data.cols, c(POOLING_LEVEL, "AREA_HA"))

#SAVE BY STRATUM PER YEAR- JUST 
###### REMOVE STRATA with N=1 (cannot pool those up)
dps$Mean<-dps$Mean[dps$Mean$N>1,]
dps$SampleVar<-dps$SampleVar[dps$SampleVar$N>1,]
dps$SampleSE<-dps$SampleSE[dps$SampleSE$N>1,]


dpst<-dps
colnames(dpst$Mean)[10:ncol(dpst$Mean)] <- paste("Mean.", colnames(dpst$Mean[,10:ncol(dpst$Mean)]), sep = "")
colnames(dpst$SampleSE)[10:ncol(dpst$SampleSE)] <- paste("SE.", colnames(dpst$SampleSE[,10:ncol(dpst$SampleSE)]), sep = "")

dpst<-left_join(dpst$Mean,dpst$SampleSE)

kona<-subset(dpst,ANALYSIS_SEC=="HAW_KONA")
#Island-level estimates summarized for the 3 depth strata
OUTPUT_LEVEL<-c("REGION","ISLAND","ANALYSIS_YEAR","DEPTH_BIN") 
dpisldb<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
dpisldb<-as.data.frame(dpisldb)
dpisldb<-subset(dpisldb,select=-c(PooledSE.REGION:PooledSE.N))
colnames(dpisldb)[1:5]<-sub("Mean.","",colnames(dpisldb)[1:5]) #remove mean from metadata columns


dpisldb<-subset(dpisldb,select= -c(Mean.EMA,Mean.HAL,Mean.I,Mean.SC, Mean.SED,Mean.TOT_AREA_WT,
                               PooledSE.EMA,PooledSE.HAL,PooledSE.I,PooledSE.SC, PooledSE.SED))


write.csv(dpisldb, file="T:/Benthic/Data/Data Requests/2021 IEA/MHICover__ISLAND_DEPTH_IEA.csv",row.names=F)


# Generate estimates of mean benthic cover at shal, mid and deep sites for each island for 2010/2012 & --------
#EXCLUDE strata that have <2 sites and sectors that don't have all 3 strata & only sectors that were surveyed in both 2010 and 

#Drop stata with less than 2 sites
st.list<-ddply(wsd,.(ANALYSIS_YEAR,REGION,ISLAND,ANALYSIS_SEC ,STRATANAME),summarize,n=length(unique(SITEVISITID)))
st.list2<-st.list %>% dplyr::filter(n>=2);head(st.list2) #remove strata that have less than 2 sites/stratum

#Drop sectors with less than 3 strata- important for temporal analysis to compare sectors with all strata
st.list2<-ddply(st.list2,.(ANALYSIS_YEAR,REGION,ISLAND,ANALYSIS_SEC),summarize,n=length(unique(STRATANAME)))
st.list3<-st.list2 %>% dplyr::filter(n==3);head(st.list3) #remove strata that have less than 2 sites/stratum
st.list3$SEC_YEAR<-paste(st.list3$ANALYSIS_SEC,st.list3$ANALYSIS_YEAR)
wsd$SEC_YEAR<-paste(wsd$ANALYSIS_SEC,wsd$ANALYSIS_YEAR)

wsd2<-wsd[wsd$SEC_YEAR %in% c(st.list3$SEC_YEAR),] #Subset data to only include sectors_years with strata with 2 sites and all 3 strata
wsd2<-separate(wsd2,STRATA,c("REEFZONE","DEPTH_BIN"),sep="_");wsd2<-dplyr::select(wsd2,-REEFZONE)

#Subset the 2010/2012 &  sites
wsd.10_19<-filter(wsd2,ANALYSIS_YEAR %in% c("2010-12",""))

#Only include sectors that were surveyed in both years
sec.list<-ddply(wsd.10_19,.(REGION,ISLAND,ANALYSIS_SEC),summarize,n=length(unique(ANALYSIS_YEAR)))
sec.list2<-sec.list %>% dplyr::filter(n==2);head(sec.list2) #remove strata that have less than 2 sites/stratum

wsd.10_19<-wsd.10_19[wsd.10_19$ANALYSIS_SEC %in% c(sec.list2$ANALYSIS_SEC),] #Subset data to only include sectors_years with strata with 2 sites and all 3 strata


# POOL WSD (WORKING SITE DATA TO STRATA THEN TO HIGHER LEVELS -------------

### CALCULATE MEAN AND VARIANCE WITHIN STRATA ###
SPATIAL_POOLING_BASE<-c("REGION","ISLAND", "ANALYSIS_SEC", "REEF_ZONE","DEPTH_BIN", "STRATANAME")    
ADDITIONAL_POOLING_BY<-c("ANALYSIS_YEAR")                                    # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)

#generate within strata means and vars
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
dps<-Calc_PerStrata(wsd.10_19, data.cols, c(POOLING_LEVEL, "AREA_HA"))

#SAVE BY STRATUM PER YEAR
###### REMOVE STRATA with N=1 (cannot pool those up)
dps$Mean<-dps$Mean[dps$Mean$N>1,]
dps$SampleVar<-dps$SampleVar[dps$SampleVar$N>1,]
dps$SampleSE<-dps$SampleSE[dps$SampleSE$N>1,]

dpst<-dps
colnames(dpst$Mean)[10:ncol(dpst$Mean)] <- paste("Mean.", colnames(dpst$Mean[,10:ncol(dpst$Mean)]), sep = "")
colnames(dpst$SampleSE)[10:ncol(dpst$SampleSE)] <- paste("SE.", colnames(dpst$SampleSE[,10:ncol(dpst$SampleSE)]), sep = "")
dpst<-left_join(dpst$Mean,dpst$SampleSE)



#Island-level estimates summarized for the 3 depth strata
OUTPUT_LEVEL<-c("REGION","ISLAND","ANALYSIS_YEAR","DEPTH_BIN") 
dpisldb<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
dpisldb<-as.data.frame(dpisldb)
dpisldb<-subset(dpisldb,select=-c(PooledSE.REGION:PooledSE.N))
colnames(dpisldb)[1:5]<-sub("Mean.","",colnames(dpisldb)[1:5]) #remove mean from metadata columns


dpisldb<-subset(dpisldb,select= -c(Mean.EMA,Mean.HAL,Mean.I,Mean.SC, Mean.SED,Mean.TOT_AREA_WT,
                                   PooledSE.EMA,PooledSE.HAL,PooledSE.I,PooledSE.SC, PooledSE.SED))


write.csv(dpisldb, file="T:/Benthic/Data/Data Requests/2021 IEA/MHICover_2010__ISLAND_DEPTH_IEA.csv",row.names=F)



