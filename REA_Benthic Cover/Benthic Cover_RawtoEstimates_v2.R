##########Summarize 2013-2017 data
rm(list=ls())

setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/BIA")

library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below
library(RODBC)            # to connect to oracle

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/fish_team_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/Islandwide Mean&Variance Functions.R")

#Climate data - this is from CPCE
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_BIA_CLIMATE_PERM.rdata")   #bia
cli$SITE<-SiteNumLeadingZeros(cli$SITE)

#BIA data - this is from CPCE
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_BIA_STR_RAW_NEW.rdata")   #bia
bia$SITE<-SiteNumLeadingZeros(bia$SITE)

#CNET data - from CoralNet
#These data contain human annotated data. There may be a small subset of robot annotated data. 
#The robot annoations are included because the confidence threshold in CoralNet was set to 90% allowing the robot to annotate points when it was 90% certain.
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_BIA_STR_CNET.rdata") #load data
cnet$SITE<-SiteNumLeadingZeros(cnet$SITE)

##Generate Table of all the bia categories to review
head(bia)
bia_tab<-ddply(bia,.(TIER_1, CATEGORY_NAME, TIER_2, SUBCATEGORY_NAME, TIER_3, GENERA_NAME),summarize,count=sum(POINTS))
#write.csv(bia_tab, file="BIA categories.csv")
table(bia$TIER_1)
table(bia$TIER_2)

##Generate Table of all the bia categories to review
head(cnet)
cnet_tab<-ddply(cnet,.(CATEGORY_CODE,CATEGORY_NAME,SUBCATEGORY_CODE,SUBCATEGORY_NAME,GENERA_CODE,GENERA_NAME,FUNCTIONAL_GROUP),summarize,count=length(ROUNDID))
#write.csv(cnet_tab, file="CNET categories.csv")

sm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SITE MASTER.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)


##### MERGE THEM TOGETHER - LOOKING LIKE BIA) #############
bia$METHOD<-"CPCE"
cli$METHOD<-"CPCE"
#bia$FUNCTIONAL_GROUP<-"BIA"    #ACTUALLY FUNCTIONAL_GROUP SEEMS A BIT MIXED UP ... FROM QUICK LOOK AT CNET FILE, IT CAN TAKE DIFFERENT VALUES FOR SAME CODES (eg ALGAE or Hare Substrate) - SO GOING TO IGNORE IT!

cnet$POINTS<-1
cnet$METHOD<-"CNET"
cnet$REP<-cnet$REPLICATE
cnet$IMAGE_NAME<-cnet$ORIGINAL_FILE_NAME
cnet$PHOTOID<-cnet$IMAGE_NUMBER
cnet$TIER_1<-cnet$CATEGORY_CODE
cnet$TIER_2<-cnet$SUBCATEGORY_CODE
cnet$TIER_3<-cnet$GENERA_CODE


#Combine cpc and coralnet
FIELDS_TO_RETAIN<-c("MISSIONID","METHOD", "REGION", "OBS_YEAR","ISLAND", "SITEVISITID","SITE", "LATITUDE", "LONGITUDE", "REEF_ZONE", "DEPTH_BIN", "PERM_SITE", "CLIMATE_STATION_YN", "MIN_DEPTH", "MAX_DEPTH", "HABITAT_CODE", "REP", "IMAGE_NAME", "PHOTOID", "ANALYST", "TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME", "POINTS")
x<-bia[,FIELDS_TO_RETAIN]; head(x)
y<-cnet[,FIELDS_TO_RETAIN]; head(y)
z<-cli[,FIELDS_TO_RETAIN]; head(z)

ab<-rbind(x,y,z)

#write.csv(ab, file="tmp All BIA BOTH METHODS.csv")

SURVEY_INFO<-c("OBS_YEAR", "REGION",  "ISLAND")
survey_island<-Aggregate_InputTable(cnet, SURVEY_INFO)

#There are some missing Tier3 information. If these data are missing then fill it with tier2 code
CATEGORY_FIELDS<-c("METHOD", "TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME")
summary(ab[,CATEGORY_FIELDS])
levels(ab$TIER_3)<-c(levels(ab$TIER_3), levels(ab$TIER_2))
levels(ab$GENERA_NAME)<-c(levels(ab$GENERA_NAME), levels(ab$SUBCATEGORY_NAME))
ab[is.na(ab$TIER_3), ]$TIER_3<-ab[is.na(ab$TIER_3), ]$TIER_2
ab[is.na(ab$GENERA_NAME), ]$GENERA_NAME<-ab[is.na(ab$GENERA_NAME), ]$SUBCATEGORY_NAME
ab<-droplevels(ab)

#all.tab<-aggregate(ab$POINTS, by=ab[,c("METHOD", "TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME")], FUN=length)
#write.csv(all.tab, file="tmp All CATEGORIES BOTH METHODS.csv")

### SOME CLEAN UP

#CREATING CLASS EMA "Encrusting Macroalgae
levels(ab$TIER_1)<-c(levels(ab$TIER_1), "EMA")
levels(ab$CATEGORY_NAME)<-c(levels(ab$CATEGORY_NAME), "Encrusting macroalga")
ab[ab$GENERA_NAME %in% c("Lobophora sp","Peyssonnelia sp", "Encrusting macroalga"),]$TIER_1<-"EMA"
ab[ab$GENERA_NAME %in% c("Lobophora sp","Peyssonnelia sp", "Encrusting macroalga"),]$TIER_2<-"EMA"
ab[ab$GENERA_NAME %in% c("Lobophora sp","Peyssonnelia sp", "Encrusting macroalga"),]$SUBCATEGORY_NAME<-"Encrusting macroalga"
ab[ab$GENERA_NAME %in% c("Lobophora sp","Peyssonnelia sp", "Encrusting macroalga"),]$CATEGORY_NAME<-"Encrusting macroalga"

###Create a Halimeda class
ab$GENERA_NAME<-as.character(ab$GENERA_NAME)
ab$TIER_1<-as.character(ab$TIER_1)

for (i in 1:nrow(ab)){ #opening brace
  if(ab$GENERA_NAME[i] =="Halimeda sp"){ 
    ab$TIER_1[i]="HAL" 
  }
}

hal<-subset(ab,TIER_2=="HAL")
head(hal)

all.tab<-aggregate(ab$POINTS, by=ab[,c("METHOD", "TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME")], FUN=length)
write.csv(all.tab, file="All CATEGORIES BOTH METHODS CLEANED.csv")

save(ab, file="T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/BIA_2010-2018_CLEANED.RData")

test<-ddply(ab,.(REGION,OBS_YEAR),summarize,nSite=length(unique(SITE)))
test

#### WORKING WITH CLEAN DATA FILE AT THIS POINT  
ab<-droplevels(ab)
table(ab$ISLAND, ab$OBS_YEAR)

summary(ab)

#We are missing depth bin, reef zone and habitat_code information from some sites.
#This information is also missing from the SURVEY MASTER file

levels(ab$DEPTH_BIN)<-c(levels(ab$DEPTH_BIN), "UNKNOWN")
levels(ab$REEF_ZONE)<-c(levels(ab$REEF_ZONE), "UNKNOWN")
levels(ab$HABITAT_CODE)<-c(levels(ab$HABITAT_CODE), "UNKNOWN")

ab[is.na(ab$DEPTH_BIN),]$DEPTH_BIN<-"UNKNOWN"
ab[is.na(ab$REEF_ZONE),]$REEF_ZONE<-"UNKNOWN"
ab[is.na(ab$HABITAT_CODE),]$HABITAT_CODE<-"UNKNOWN"


#### NB THERE ARE SEVERAL UNCLASSIFIED CATEGORIES  - HIGHLIGHTING THOSE HERE
UNIDENTIFIED_T1<-c("TW", "MF", "UC")
UNIDENTIFIED_T2<-c("MOBF", "TAPE", "UNK", "WAND", "SHAD")

length(unique(ab$SITE))

test<-ddply(ab,.(REGION,OBS_YEAR),summarize,nSite=length(unique(SITE)))
test

#Generate a SITE table
sites<-ddply(ab,.(METHOD,REGION,OBS_YEAR,ISLAND,PERM_SITE,CLIMATE_STATION_YN,SITE,LATITUDE,LONGITUDE,REEF_ZONE,DEPTH_BIN),
             summarize,x=sum(POINTS))
sites$x<-NULL
dim(sites)

### GENERATE DATA AT SITE LEVEL AND FOR ALL HARD_CORAL
## FIRST T1

photo<-dcast(ab, formula=METHOD + OBS_YEAR + SITEVISITID + SITE  ~ TIER_1, value.var="POINTS", sum, fill=0)
head(photo)

r_levels<-c(unique(as.character(ab$TIER_1)))
photo$N<-rowSums(photo[,r_levels])
data.cols<-c(r_levels)

#Substract mobile inverts and tape wand shallow and uclassified
photo$new.N<-photo$N-(photo$MF+photo$UC+photo$TW)

#Calculate proportion
photo[,data.cols]<-(photo[,data.cols]/photo$new.N)*100
head(photo)

r_levels<-c(unique(as.character(ab$TIER_1)))
data.cols<-c(r_levels)

wsd<-merge(sites, photo, by=c("METHOD", "OBS_YEAR", "SITE"), all.y=T)

test1<-ddply(wsd,.(OBS_YEAR),summarize,nSite=length(unique(SITE)))
test1

####################################################################################################################################################################
#
#     CHECK THAT DATA IS READY FOR POOLING AND DO SOME FINAL CLEAN UPS, EG SET BACKREEF DEPTH_ZONE TO ALL, CREATE THE "SGA" LOCATION
#
####################################################################################################################################################################
## NOW POOL UP TO Stratum AND YEAR
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv");tail(sectors)
sm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)

#Merge Site master with wsd
sm<-sm[,c("SITEVISITID","SITE","ANALYSIS_YEAR","ANALYSIS_SCHEME","OBS_YEAR","SEC_NAME","EXCLUDE_FLAG")]
wsd_t1<-merge(wsd,sm,by=c("SITEVISITID","SITE","OBS_YEAR"),all.x=TRUE)
head(wsd_t1)

test2<-ddply(wsd_t1,.(OBS_YEAR),summarize,nSite=length(unique(SITE)))

#make sure the number of sites by region and year match before and after merging with survey master
test1
test2

#Save Tier 1 site data to t drive. This file has all sites (fish, benthic and OCC) that were annoated between 2010 and 2018
write.csv(wsd_t1, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/BIA_2010-2018_Tier1_SITE.csv")


#Tier3
photo<-dcast(ab, formula=METHOD + OBS_YEAR + SITEVISITID + SITE  ~ TIER_3, value.var="POINTS", sum, fill=0)
head(photo)

r_levels<-c(unique(as.character(ab$TIER_3)))
photo$N<-rowSums(photo[,r_levels])
data.cols<-c(r_levels)

#Substract mobile inverts and tape wand shallow and uclassified
photo$new.N<-photo$N-(photo$WAND+photo$UNK+photo$TAPE+photo$MOBF+photo$SHAD)

#Calculate proportion
photo[,data.cols]<-(photo[,data.cols]/photo$new.N)*100
head(photo)

r_levels<-c(unique(as.character(ab$TIER_3)))
data.cols<-c(r_levels)

wsd<-merge(sites, photo, by=c("METHOD", "OBS_YEAR", "SITE"), all.y=T)

test1<-ddply(wsd,.(OBS_YEAR),summarize,nSite=length(unique(SITE)))
test1

####################################################################################################################################################################
#
#     CHECK THAT DATA IS READY FOR POOLING AND DO SOME FINAL CLEAN UPS, EG SET BACKREEF DEPTH_ZONE TO ALL, CREATE THE "SGA" LOCATION
#
####################################################################################################################################################################
## NOW POOL UP TO Stratum AND YEAR
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv");tail(sectors)
sm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)

#Merge Site master with wsd
sm<-sm[,c("SITEVISITID","SITE","ANALYSIS_YEAR","ANALYSIS_SCHEME","OBS_YEAR","SEC_NAME","EXCLUDE_FLAG")]
wsd_t3<-merge(wsd,sm,by=c("SITEVISITID","SITE","OBS_YEAR"),all.x=TRUE)
head(wsd_t3)

test2<-ddply(wsd_t3,.(REGION,OBS_YEAR),summarize,nSite=length(unique(SITE)))

#make sure the number of sites by region and year match before and after merging with survey master
test1
test2

#Save Tier 1 site data to t drive. This file has all sites (fish, benthic and OCC) that were annoated between 2010 and 2018
write.csv(wsd_t3, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/BIA_2010-2018_Tier3_SITE.csv")


####################################################################################################################################################################
#
#     CHECK THAT DATA IS READY FOR POOLING AND DO SOME FINAL CLEAN UPS, EG SET BACKREEF DEPTH_ZONE TO ALL, CREATE THE "SGA" LOCATION
#
####################################################################################################################################################################

wsd<-wsd_t1
#wsd<-wsd_t3

#remove permanent sites, climate sites and special projects
wsd<-subset(wsd,PERM_SITE!="-1"&CLIMATE_STATION_YN!="-1" & EXCLUDE_FLAG!="-1")

#Check analysis sector names
wsd<-droplevels(wsd)
levels(wsd$an)

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
SGA<-c("Guguan", "Alamagan", "Sarigan")
levels(wsd$ISLAND)<-c(levels(wsd$ISLAND), "AGS")
levels(sectors$ISLAND)<-c(levels(sectors$ISLAND), "AGS")
wsd[wsd$ISLAND %in% SGA,]$ISLAND<-"AGS"
sectors[sectors$ISLAND %in% SGA,]$ISLAND<-"AGS"

#Change Analysis year according to desired pooling
wsd[is.na(wsd$ANALYSIS_YEAR),]

levels(wsd$ANALYSIS_YEAR)<-c(levels(wsd$ANALYSIS_YEAR), "2016on","2015-16")
wsd[wsd$REGION %in% c("MHI", "NWHI") & wsd$OBS_YEAR==2016,]$ANALYSIS_YEAR<-"2016on"
wsd[wsd$REGION %in% c("MHI", "NWHI") & wsd$OBS_YEAR %in% seq(2010,2012),]$ANALYSIS_YEAR<-"2010-12"
wsd[wsd$REGION %in% c("MHI", "NWHI") & wsd$OBS_YEAR %in% seq(2013,2015),]$ANALYSIS_YEAR<-"2013-15"
wsd[wsd$ISLAND %in% c("Baker", "Howland") & wsd$OBS_YEAR =="2015",]$ANALYSIS_YEAR<-"2015"
wsd[wsd$ISLAND %in% c("Kingman", "Palmyra","Jarvis") & wsd$OBS_YEAR =="2015",]$ANALYSIS_YEAR<-"2015"
wsd[wsd$ISLAND =="Wake" & wsd$OBS_YEAR =="2014",]$ANALYSIS_YEAR<-"2014"
wsd[wsd$ISLAND =="Wake" & wsd$OBS_YEAR =="2017",]$ANALYSIS_YEAR<-"2017"
wsd[wsd$ISLAND =="Johnston" & wsd$OBS_YEAR =="2015",]$ANALYSIS_YEAR<-"2015"
wsd[wsd$REGION =="SAMOA" & wsd$OBS_YEAR =="2015",]$ANALYSIS_YEAR<-"2015"
wsd[wsd$REGION =="MHI" & wsd$OBS_YEAR ==2013,]$ANALYSIS_YEAR<-"2013"
wsd[wsd$REGION =="MHI" & wsd$OBS_YEAR ==2015,]$ANALYSIS_YEAR<-"2015"

###FIX THIS IN THE LONGTERM
#Remove Kauai special project and Samoa special project and Timor Leste
wsd<-wsd[!is.na(wsd$ANALYSIS_SCHEME),]
wsd<-subset(wsd,ANALYSIS_SCHEME!="SAMOA_CLIMATE")
wsd<-subset(wsd,REGION!="CT")


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

#NOW CHECK HOW MANY REPS WE HAVE PER STRATA
a<-dcast(wsd, ANALYSIS_SCHEME + ISLAND + ANALYSIS_SEC + OBS_YEAR ~ STRATA, value.var="AREA_HA", length); a

####################################################################################################################################################################
#
#     POOL WSD (WORKING SITE DATA TO STRATA THEN TO HIGHER LEVELS
##
###################################################################################################################################################################


### CALCULATE MEAN AND VARIANCE WITHIN STRATA ###
SPATIAL_POOLING_BASE<-c("ISLAND", "ANALYSIS_SEC", "REEF_ZONE", "STRATA")    
ADDITIONAL_POOLING_BY<-c("ANALYSIS_YEAR")                                    # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)

#generate within strata means and vars
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
dps<-Calc_PerStrata(wsd, data.cols, c(POOLING_LEVEL, "AREA_HA"))
head(dps$Mean)
save(dps, file="tmp PRIMM BIA per strata2.RData")

###### REMOVE STRATA with N=1 (cannot pool those up)
dps$Mean<-dps$Mean[dps$Mean$N>1,]
dps$SampleVar<-dps$SampleVar[dps$SampleVar$N>1,]
dps$SampleSE<-dps$SampleSE[dps$SampleSE$N>1,]

# e.g. SAVE BY ISLAND AND REEF_ZONE PER YEAR
OUTPUT_LEVEL<-c("ISLAND","STRATA","ANALYSIS_YEAR") 
dpst<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
write.csv(dpst, file="BIA_2010-2018_Tier1_STRATA.csv")

# e.g. SAVE BY SECTOR PER YEAR
OUTPUT_LEVEL<-c("ANALYSIS_SEC","ANALYSIS_YEAR") 
dpsec<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
write.csv(dpsec, file="BIA_2010-2018_Tier1_SECTOR.csv")


# e.g. SAVE BY ISLAND PER YEAR
OUTPUT_LEVEL<-c("ISLAND","ANALYSIS_YEAR") 
dpis<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
write.csv(dpis, file="BIA_2010-2018_Tier1_ISLAND.csv")


#QC Checks- Doing this mannually for now. build in some scripts
#1. Do the number of sites for each island/year match the site master?- both fish and benthic sites should be included
#2. Does AREA_HA in the summarized data match what's in the SectorStrata Area file?
#3. Does the range of values make sense? Are there any values that are obviously incorrect (e.g. 200% cover)


