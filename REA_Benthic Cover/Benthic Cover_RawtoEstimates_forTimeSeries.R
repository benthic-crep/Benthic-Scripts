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
wsd_t1<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2020_Tier1_SITE.csv")
wsd_t3<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2020_Tier3_SITE.csv")


# CHECK THAT DATA IS READY FOR POOLING AND DO SOME FINAL CLEAN UPS --------

#Identify which taxonomic level you would like to summarize
#wsd<-wsd_t1
wsd<-wsd_t3

#Define data columns
#data.cols<-T1data.cols
data.cols<-T3data.cols

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

write.csv(wsd,"test.csv")

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
wsd[wsd$REGION %in% c("MHI", "NWHI") & wsd$OBS_YEAR %in% seq(2010,2012),]$ANALYSIS_YEAR<-"2010-12"
wsd[wsd$REGION %in% c("MHI", "NWHI") & wsd$OBS_YEAR %in% seq(2013,2015),]$ANALYSIS_YEAR<-"2013-15"
wsd[wsd$REGION %in% c("SAMOA") & wsd$OBS_YEAR %in% seq(2015,2016,2015-16),]$ANALYSIS_YEAR<-"2015-16"

#set all Backreef to a single DEPTH_ZONE ("All") 
wsd$STRATA<-paste(wsd$REEF_ZONE, wsd$DEPTH_BIN, sep="_")
sectors$STRATA<-paste(sectors$REEF_ZONE, sectors$DEPTH_BIN, sep="_")


## generate a complete list of all ANALYSIS STRATA and their size
sectors$AREA_HA<-as.numeric(sectors$AREA_HA)
SCHEMES<-c("RAMP_BASIC","MARI2011","MARI2014","TUT10_12","AS_SANCTUARY")
for(i in 1:length(SCHEMES)){
  tmp2<-sectors[,c("SEC_NAME", SCHEMES[i])]
  tmp2$SCHEME<-SCHEMES[i]
  names(tmp2)<- c("SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_SCHEME")
  
  tmp<-aggregate(sectors$AREA_HA, sectors[,c(SCHEMES[i], "STRATA")], sum)
  tmp$SCHEME<-SCHEMES[i]
  names(tmp)<-c("ANALYSIS_SEC", "STRATA", "AREA_HA", "ANALYSIS_SCHEME")
  if(i==1){
    st<-tmp
    as<-tmp2
  } else {
    st<-rbind(st, tmp)
    as<-rbind(as, tmp2)
  }	
}

as$TMP<-1
as<-aggregate(as$TMP, by=as[,c("SEC_NAME", "ANALYSIS_SCHEME", "ANALYSIS_SEC")], length) 
as$x<-NULL

wsd<-merge(wsd, as, by=c("SEC_NAME", "ANALYSIS_SCHEME"), all.x=T)  # add ANALYSIS_SCHEME for this sector and sceheme combination
unique(wsd[is.na(wsd$ANALYSIS_SCHEME), c("ISLAND", "ANALYSIS_SEC", "SEC_NAME", "OBS_YEAR", "ANALYSIS_SCHEME", "STRATA")])

dcast(st, ANALYSIS_SEC ~ ANALYSIS_SCHEME, value.var="AREA_HA", sum)
wsd<-merge(wsd, st, by=c("ANALYSIS_SCHEME", "ANALYSIS_SEC", "STRATA"), all.x=T)
#check if some are missing an AREA_HA .. which means that they didnt get into the stratification scheme properly
unique(wsd[is.na(wsd$AREA_HA), c("ISLAND", "ANALYSIS_SEC", "SEC_NAME", "OBS_YEAR", "ANALYSIS_SCHEME", "STRATA")])

#change swains back to SEC_NAME for Swains
wsd$ANALYSIS_SEC<-ifelse(wsd$ISLAND=="Swains",as.character(wsd$SEC_NAME),as.character(wsd$ANALYSIS_SEC))

#NOW CHECK HOW MANY REPS WE HAVE PER STRATA
a<-dcast(wsd, ANALYSIS_SCHEME + ISLAND + ANALYSIS_SEC + OBS_YEAR ~ STRATA, value.var="AREA_HA", length); a


# Subset Strata and Sectors that were surveyed in more than 1 year --------
tmp<-ddply(wsd,.(ANALYSIS_YEAR,ANALYSIS_SEC,STRATA),
                  summarize,
                  n=length(unique(SITEVISITID)))
tmp1<-tmp[tmp$n>1,]
wsd<-left_join(tmp1,wsd) #merge the 2 data frames to remove sectors only surveyed once
table(wsd$REGION,wsd$ANALYSIS_YEAR)


# GENERATE DATA FOR TEMPORAL ANALYSIS
wsd$STRATANAME<- paste(wsd$ANALYSIS_SEC ,wsd$REEF_ZONE,wsd$DEPTH_BIN,sep="_") #create full stratum name
st.list<-ddply(wsd,.(ANALYSIS_YEAR,REGION,ISLAND,ANALYSIS_SEC ,STRATANAME),summarize,n=length(unique(SITEVISITID)))
st.list2<-subset(st.list,n>=2);head(st.list) #remove strata that have less than 2 sites/stratum

#Generate list of strata that were surveyed in all years for a given region and had at least 2 sites/stratum
st.list_w<-dcast(st.list2, formula=REGION+ISLAND+ANALYSIS_SEC+STRATANAME~ ANALYSIS_YEAR, value.var="n",fill=0)
st.list_w$year_n<-rowSums(st.list_w[,5:ncol(st.list_w)] > 0, na.rm=T) #count # of years of data
#Identify the max number of years a stratum was surveyed within a sector
yMax<-ddply(st.list_w,.(REGION,ISLAND,ANALYSIS_SEC),
            summarize,
            year_n=max(year_n))
st.list_w3<-left_join(yMax,st.list_w) #only include strata that were surveyed the maxiumum number of years for a given sector
head(st.list_w3);st.list_w3<-droplevels(st.list_w3) #generate the list

wsd<-wsd[wsd$STRATANAME %in% c(st.list_w3$STRATANAME),] #Subset data to only include strata of interest



# POOL WSD (WORKING SITE DATA TO STRATA THEN TO HIGHER LEVELS -------------

### CALCULATE MEAN AND VARIANCE WITHIN STRATA ###
SPATIAL_POOLING_BASE<-c("REGION","ISLAND", "ANALYSIS_SEC", "REEF_ZONE", "STRATA")
ADDITIONAL_POOLING_BY<-c("ANALYSIS_YEAR")                                    # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)
 
#generate within strata means and vars
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
dps<-Calc_PerStrata(wsd, data.cols, c(POOLING_LEVEL, "AREA_HA"))
head(dps$Mean)
#save(dps, file="tmp PRIMM BIA per strata2.RData")

# e.g. SAVE BY ISLAND AND REEF_ZONE PER YEAR
OUTPUT_LEVEL<-c("REGION","ISLAND","ANALYSIS_SEC","STRATA","ANALYSIS_YEAR") 
dpst<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
#write.csv(dpst, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicCover_2010-2019_Tier1_STRATA_forTimesSeries.csv")
write.csv(dpst, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicCover_2010-2019_Tier3_STRATA_forTimesSeries.csv")

# e.g. SAVE BY SECTOR PER YEAR
OUTPUT_LEVEL<-c("REGION","ISLAND","ANALYSIS_SEC","ANALYSIS_YEAR") 
dpsec<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
#write.csv(dpsec, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicCover_2010-2019_Tier1_SECTOR_forTimesSeries.csv")
write.csv(dpsec, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicCover_2010-2019_Tier3_SECTOR_forTimeseries.csv")


# e.g. SAVE BY ISLAND PER YEAR
OUTPUT_LEVEL<-c("REGION","ISLAND","ANALYSIS_YEAR") 
dpis<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
#write.csv(dpis, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicCover_2010-2019_Tier1_ISLAND_forTimesSeries.csv")
write.csv(dpis, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicCover_2010-2019_Tier3_ISLAND_forTimeseries.csv")



#Create a summary table of sites from SURVEY MASTER to make sure that sites weren't dropped
sm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)

#Are there NAs in TRANSECT PHOTOS
sm$TRANSECT_PHOTOS[is.na(sm$TRANSECT_PHOTOS)]<-0
sm$Oceanography[is.na(sm$Oceanography)]<-0
sm$EXCLUDE_FLAG[is.na(sm$EXCLUDE_FLAG)]<-0
sm$CLIMATE_STATION_YN[is.na(sm$CLIMATE_STATION_YN)]<-0
sm$PERM_SITE[is.na(sm$PERM_SITE)]<-0

sm2<-subset(sm,Oceanography!=1 & TRANSECT_PHOTOS!=0 & EXCLUDE_FLAG!="-1"& PERM_SITE!=-1 &CLIMATE_STATION_YN!=-1)
sm2$Stratum<-paste(sm2$SEC_NAME,sm2$REEF_ZONE,sm2$DEPTH_BIN)
table(sm2$Stratum,sm2$OBS_YEAR)



#QC Checks- Doing this mannually for now. build in some scripts
#1. Do the number of sites for each island/year match the survey master?- both fish and benthic sites should be included
#2. Does AREA_HA in the summarized data match what's in the SectorStrata Area file?
#3. Does the range of values make sense? Are there any values that are obviously incorrect (e.g. 200% cover)


