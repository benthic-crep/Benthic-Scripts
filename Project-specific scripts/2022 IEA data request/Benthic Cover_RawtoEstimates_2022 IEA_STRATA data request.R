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
wsd_t3<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2020_Tier3_SITE_v2.csv")
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv")


# CHECK THAT DATA IS READY FOR POOLING AND DO SOME FINAL CLEAN UPS --------

#Identify which taxonomic level you would like to summarize

wsd<-wsd_t1
#wsd<-wsd_t3

#Define data columns
T1data.cols<-colnames(subset(wsd,select=c(CCA:TURF,CCA_CORAL,BSR)))
#T3data.cols<-colnames(subset(wsd,select=c(ACAS:TAB)))

data.cols<-T1data.cols
#data.cols<-T3data.cols

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

## TREAT GUGUAN, ALAMAGAN, SARIGAN AS ONE ISLAND  (REALLY ONE BASE REPORTING UNIT .. BUT SIMPLER TO STICK TO 'ISLAND')
# SGA<-c("Guguan", "Alamagan", "Sarigan")
# levels(wsd$ISLAND)<-c(levels(wsd$ISLAND), "AGS")
# levels(sectors$ISLAND)<-c(levels(sectors$ISLAND), "AGS")
# wsd[wsd$ISLAND %in% SGA,]$ISLAND<-"AGS"
# sectors[sectors$ISLAND %in% SGA,]$ISLAND<-"AGS"
# 
# SGA<-c("Guguan", "Alamagan", "Sarigan")
# levels(wsd$SEC_NAME)<-c(levels(wsd$SEC_NAME), "AGS")
# levels(sectors$SEC_NAME)<-c(levels(sectors$SEC_NAME), "AGS")
# wsd[wsd$SEC_NAME %in% SGA,]$SEC_NAME<-"AGS"
# sectors[sectors$SEC_NAME %in% SGA,]$SEC_NAME<-"AGS"


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


# Generate STRATA level data for each island for ALL 2019 data ----------------------------
wsd2<-wsd[wsd$STRATANAME %in% c(st.list_w3$STRATANAME),] #Subset data to only include strata of interest

# POOL WSD (WORKING SITE DATA TO STRATA THEN TO HIGHER LEVELS -------------

### CALCULATE MEAN AND VARIANCE WITHIN STRATA ###
SPATIAL_POOLING_BASE<-c("REGION","ISLAND", "ANALYSIS_SEC", "REEF_ZONE", "STRATA")    
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
colnames(dpst$Mean)[9:ncol(dpst$Mean)] <- paste("Mean.", colnames(dpst$Mean[,9:ncol(dpst$Mean)]), sep = "")
colnames(dpst$SampleSE)[9:ncol(dpst$SampleSE)] <- paste("SE.", colnames(dpst$SampleSE[,9:ncol(dpst$SampleSE)]), sep = "")

dpst<-left_join(dpst$Mean,dpst$SampleSE)


# e.g. SAVE BY SECTOR PER YEAR
OUTPUT_LEVEL<-c("REGION","ISLAND","ANALYSIS_SEC","ANALYSIS_YEAR") 
dpisl<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
dpisl<-as.data.frame(dpisl)
dpisl<-subset(dpisl,select=-c(PooledSE.REGION:PooledSE.N))
colnames(dpisl)[1:5]<-sub("Mean.","",colnames(dpisl)[1:5]) #remove mean from metadata columns

nstrat<-ddply(wsd,.(ANALYSIS_SEC,ANALYSIS_YEAR),
              summarize,
              nstrat=length(unique(STRATANAME)))

dpisl<-left_join(dpisl,nstrat)

dpisl<-subset(dpisl,select= -c(Mean.EMA,Mean.HAL,Mean.I,Mean.SC, Mean.SED,Mean.TOT_AREA_WT,
                               PooledSE.EMA,PooledSE.HAL,PooledSE.I,PooledSE.SC, PooledSE.SED))




write.csv(dpst, file="T:/Benthic/Data/Data Requests/2021 IEA/BenthicCover_2010and2019_Tier1_STRATA_2022IEA.csv",row.names=F)
#write.csv(dpst, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicCover_2010-2019_Tier3_STRATA_forTimesSeries_v2.csv",row.names=F)





# Generate STRATA level data for strata and sectors that were surveyed in BOTH 2010/12 & 2019 

# Subset Strata and Sectors that were surveyed in more than 1 year -------

st.list<-ddply(wsd,.(ANALYSIS_YEAR,REGION,ISLAND,ANALYSIS_SEC ,STRATANAME),summarize,n=length(unique(SITEVISITID)))
st.list2<-st.list %>% dplyr::filter(n>=2);head(st.list2) #remove strata that have less than 2 sites/stratum

#Generate list of strata that were surveyed in all years for a given region and had at least 2 sites/stratum
st.list_w<-dcast(st.list2, formula=REGION+ISLAND+ANALYSIS_SEC+STRATANAME~ ANALYSIS_YEAR, value.var="n",fill=0)#count # of years of data
st.list_w$year_n<-rowSums(st.list_w[,5:ncol(st.list_w)] > 0, na.rm=T) 

st.list_w<-st.list_w %>% dplyr::filter(year_n>=2);head(st.list2) #remove strata that have less than 2 sites/stratum


#Identify the max number of years a stratum was surveyed within a sector
yMax<-ddply(st.list_w,.(REGION,ISLAND,ANALYSIS_SEC),
            summarize,
            year_n=max(year_n))
st.list_w3<-left_join(yMax,st.list_w) #only include strata that were surveyed the maxiumum number of years for a given sector
head(st.list_w3);st.list_w3<-droplevels(st.list_w3) #generate the list

wsd2<-wsd[wsd$STRATANAME %in% c(st.list_w3$STRATANAME),] #Subset data to only include strata of interest

# POOL WSD (WORKING SITE DATA TO STRATA THEN TO HIGHER LEVELS -------------

### CALCULATE MEAN AND VARIANCE WITHIN STRATA ###
SPATIAL_POOLING_BASE<-c("REGION","ISLAND", "ANALYSIS_SEC", "REEF_ZONE", "STRATA")    
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
colnames(dpst$Mean)[9:ncol(dpst$Mean)] <- paste("Mean.", colnames(dpst$Mean[,9:ncol(dpst$Mean)]), sep = "")
colnames(dpst$SampleSE)[9:ncol(dpst$SampleSE)] <- paste("SE.", colnames(dpst$SampleSE[,9:ncol(dpst$SampleSE)]), sep = "")

dpst<-left_join(dpst$Mean,dpst$SampleSE)

write.csv(dpst, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicCover_2010-2019_Tier1_STRATA_forTimesSeries_v2.csv",row.names=F)
#write.csv(dpst, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicCover_2010-2019_Tier3_STRATA_forTimesSeries_v2.csv",row.names=F)



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



#Only include sectors and years that were included in the first version of the data
mhi<-subset(dpsec,REGION=="MHI")
mhi$S_Y<-paste(mhi$ANALYSIS_SEC,mhi$ANALYSIS_YEAR,sep="_")

#read in original version of data
old<-read.csv("T:/Benthic/Data/Data Requests/2021 IEA/MHICover2010-2019_Tier1_Sector_TimeSeries_Gove.csv")
old$S_Y<-paste(old$ANALYSIS_SEC,old$ANALYSIS_YEAR,sep="_")

mhi<-subset(mhi,S_Y %in% old$S_Y)

nrow(mhi)
nrow(old)

#Make sure all other categories match - they do
mhi$Mean.CORAL != old$Mean.CORAL
mhi$Mean.CCA != old$Mean.CCA
mhi$Mean.MA != old$Mean.MA
mhi$Mean.TURF != old$Mean.TURF

write.csv(mhi, file="T:/Benthic/Data/Data Requests/2021 IEA/MHICover2010-2019_Tier1_Sector_TimeSeries_Gove_v2.csv",row.names=F)


plot(mhi$Mean.BSR,old$Mean.BSR)

#####NOTE: We are are not summarizing to ISLAND level because we've dropped quite a few strata and sectors. 
#Talk to Courtney if you want island-level time series data.



