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


# #BIA data - this is from CPCE
load("ALL_BIA_STR_RAW_NEW.rdata")   #bia
bia$SITE<-SiteNumLeadingZeros(bia$SITE)

#CNET data - from CoralNet
load("ALL_BIA_STR_CNET.rdata") #load data
cnet$SITE<-SiteNumLeadingZeros(cnet$SITE)

##Generate Table of all the bia categories
head(bia)
bia_tab<-aggregate(bia$POINTS, by=bia[,c("TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME")], FUN=length)
#write.csv(bia_tab, file="BIA categories.csv")
table(bia$TIER_1)
table(bia$TIER_2)

##Generate Table of all the bia categories
head(cnet)
cnet_tab<-aggregate(cnet$ROUNDID, by=cnet[,c("CATEGORY_CODE", "CATEGORY_NAME", "SUBCATEGORY_CODE", "SUBCATEGORY_NAME", "GENERA_CODE", "GENERA_NAME", "SHORT_CODE", "FUNCTIONAL_GROUP")], FUN=length)
#write.csv(cnet_tab, file="CNET categories.csv")

#read in CNET robot generate files and look up and merge together
c.meta<-read.csv("ASRAMP2018_image_meta.csv")
c.data<-read.csv("ASRAMP2018_image_robot_data.csv")
lookup<-read.csv("CNET_categories_lookup.csv")
df1<-merge(c.meta,c.data,by=c("ORIGINAL_FILE_NAME"))
head(df1);nrow(df1)
df2<-merge(df1,lookup,by="SHORT_CODE");nrow(df2)
table(df2$OBS_YEAR)
df2$SITE<-SiteNumLeadingZeros(df2$SITE)

sm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SITE MASTER.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)

sm<-sm[,c("SITEVISITID","SITE","PERM_SITE","CLIMATE_STATION_YN","METHOD","DEPTH_BIN","HABITAT_CODE","REEF_ZONE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT","ANALYSIS_YEAR","OBS_YEAR","SEC_NAME","EXCLUDE_FLAG","DATE_")]
robot<-merge(df2,sm,by=c("SITE","OBS_YEAR"),all.x=TRUE)
head(robot)
robot$CLIMATE_STATION_YN[is.na(robot$CLIMATE_STATION_YN)]<-"-1"
robot$PERM_SITE[is.na(robot$PERM_SITE)]<-"-1"

write.csv(robot,"ASRAMP2018_BIA_robot_raw.csv")

#CHECK for missing sites in the site_master file
test<-robot[is.na(robot$SITEVISITID),]
if(dim(test)[1]>0) {cat("sites missing from SITE MASTER")}   # should be 0

#Export a list of missing sites - manually generate list of OCC site read back in
SURVEY_INFO<-c("SITEVISITID", "SITE","OBS_YEAR")
miss.sites<-new_Aggregate_InputTable(test, SURVEY_INFO)
write.csv(miss.sites,"Missing sites from SITEMASTER.csv")

SURVEY_INFO<-c("SITEVISITID", "SITE","OBS_YEAR")
miss.sites<-new_Aggregate_InputTable(test, SURVEY_INFO)

#Temporary manual fix for labeling OCC sites- need to get the OCC SITEMASTER list
occ<-read.csv("ASRAMP2018_OCCsitelist.csv")

head(robot)
robot$PERM_SITE<-ifelse(robot$SITE %in% occ$SITE,-1,0)
robot$CLIMATE_STATION_YN<-ifelse(robot$SITE %in% occ$SITE,-1,0)
head(robot)

#Save Raw CNET data
write.csv(robot,"ASRAMP2018_Cnet_ROBOT_RAW.csv")

#Modify and add columns
robot$POINTS<-1
robot$METHOD<-"CNET"
robot$TIER_1<-robot$CATEGORY_CODE
robot$TIER_2<-robot$SUBCATEGORY_CODE
robot$TIER_3<-robot$GENERA_CODE
robot$REP<-robot$REPLICATE
robot$IMAGE_NAME<-robot$ORIGINAL_FILE_NAME
robot$PHOTOID<-robot$IMAGE_NUMBER
robot$MISSIONID<-"HA1801"
robot$MAX_DEPTH<-robot$SITE_MAX_DEPTH_FT
robot$MIN_DEPTH<-robot$SITE_MIN_DEPTH_FT


##### MERGE THEM TOGETHER - LOOKING LIKE BIA) #############
bia$METHOD<-"CPCE"
bia$SHORT_CODE<-"BIA"
#bia$FUNCTIONAL_GROUP<-"BIA"    #ACTUALLY FUNCTIONAL_GROUP SEEMS A BIT MIXED UP ... FROM QUICK LOOK AT CNET FILE, IT CAN TAKE DIFFERENT VALUES FOR SAME CODES (eg ALGAE or Hare Substrate) - SO GOING TO IGNORE IT!

cnet$POINTS<-1
cnet$METHOD<-"CNET"
cnet$REP<-cnet$REPLICATE
cnet$IMAGE_NAME<-cnet$ORIGINAL_FILE_NAME
cnet$PHOTOID<-cnet$IMAGE_NUMBER
cnet$TIER_1<-cnet$CATEGORY_CODE
cnet$TIER_2<-cnet$SUBCATEGORY_CODE
cnet$TIER_3<-cnet$GENERA_CODE


#Combine cpc, old coralnet and robot data
FIELDS_TO_RETAIN<-c("MISSIONID","METHOD", "REGION", "OBS_YEAR","ISLAND", "SITEVISITID","SITE", "LATITUDE", "LONGITUDE", "REEF_ZONE", "DEPTH_BIN", "PERM_SITE", "CLIMATE_STATION_YN", "MIN_DEPTH", "MAX_DEPTH", "HABITAT_CODE", "REP", "IMAGE_NAME", "PHOTOID", "ANALYST", "TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME", "SHORT_CODE", "POINTS")
x<-bia[,FIELDS_TO_RETAIN]; head(x)
y<-cnet[,FIELDS_TO_RETAIN]; head(y)
z<-robot[,FIELDS_TO_RETAIN]; head(z)

ab<-rbind(x,y,z)
#write.csv(ab, file="tmp All BIA BOTH METHODS.csv")

SURVEY_INFO<-c("OBS_YEAR", "REGION",  "ISLAND")
survey_island<-Aggregate_InputTable(cnet, SURVEY_INFO)



CATEGORY_FIELDS<-c("METHOD", "TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME", "SHORT_CODE")
summary(ab[,CATEGORY_FIELDS])
levels(ab$TIER_3)<-c(levels(ab$TIER_3), levels(ab$TIER_2))
levels(ab$GENERA_NAME)<-c(levels(ab$GENERA_NAME), levels(ab$SUBCATEGORY_NAME))
ab[is.na(ab$TIER_3), ]$TIER_3<-ab[is.na(ab$TIER_3), ]$TIER_2
ab[is.na(ab$GENERA_NAME), ]$GENERA_NAME<-ab[is.na(ab$GENERA_NAME), ]$SUBCATEGORY_NAME
ab<-droplevels(ab)

#all.tab<-aggregate(ab$POINTS, by=ab[,c("METHOD", "TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME", "SHORT_CODE")], FUN=length)
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
  if(ab$GENERA_NAME[i] =="Halimeda sp"){ #c&p
    ab$TIER_1[i]="HAL" #c&p
  } #c&p
} #closing curly brace for entire for loop

hal<-subset(ab,TIER_2=="HAL")
head(hal)

all.tab<-aggregate(ab$POINTS, by=ab[,c("METHOD", "TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME", "SHORT_CODE")], FUN=length)
write.csv(all.tab, file="All CATEGORIES BOTH METHODS CLEANED.csv")

save(ab, file="All BIA BOTH METHODS clean.RData")

#### WORKING WITH CLEAN DATA FILE AT THIS POINT  
#### (MUCH OF THE ABOVE CODE IS A BIT UNNECESSARY FOR THIS AS YOU ONLY WANT RECENT(ie CNET) DATA, 
#### SO DIDNT NEED TO POOL IT ALL - HOWEVER IT HINK ITS GOOD TO USE THE ABOVE AS A STANDARD

#### 2017 Howland, Baker, Wake, Jarvis, 2016 Jarvis)
#### Strata scale BIA  ... coral 
#load("All BIA BOTH METHODS clean.RData"); head(ab)

ab<-droplevels(ab)
table(ab$ISLAND, ab$OBS_YEAR)
summary(ab)

# levels(ab$DEPTH_BIN)<-c(levels(ab$DEPTH_BIN), "UNKNOWN")
# levels(ab$REEF_ZONE)<-c(levels(ab$REEF_ZONE), "UNKNOWN")
# ab[is.na(ab$DEPTH_BIN),]$DEPTH_BIN<-"UNKNOWN"
# ab[is.na(ab$REEF_ZONE),]$REEF_ZONE<-"UNKNOWN"

#### NB THERE ARE SEVERAL UNCLASSIFIED CATEGORIES  - HIGHLIGHTING THOSE HERE
UNIDENTIFIED_T1<-c("TW", "MF", "UC")
UNIDENTIFIED_T2<-c("MOBF", "TAPE", "UNK", "WAND", "SHAD")

length(unique(ab$SITE))

#Generate a SITE table
SITE_FIELDS<-c("METHOD", "REGION", "OBS_YEAR", "ISLAND", "PERM_SITE", "CLIMATE_STATION_YN", "SITE", "LATITUDE", "LONGITUDE", "REEF_ZONE", "DEPTH_BIN")
sites<-aggregate(ab[,"POINTS"],by=ab[,SITE_FIELDS], sum)
sites$x<-NULL
dim(sites)

### GENERATE DATA AT SITE LEVEL AND FOR ALL HARD_CORAL
## FIRST T1
#ab2<-aggregate(ab[,"POINTS"],by=ab[,c("METHOD", "OBS_YEAR", "SITE", "REP", "PHOTOID", "TIER_1")], sum)
#names(ab2)[length(names(ab2))]<-"POINTS"
photo<-cast(ab, METHOD + OBS_YEAR + SITEVISITID + SITE  ~ TIER_1, value="POINTS", fun.aggregate=sum, fill=0)
head(photo)


#now convert to proportions
r_levels<-c(unique(as.character(ab$TIER_1)))
r_levels<-c(unique(as.character(ab$TIER_1)))

photo$N<-rowSums(photo[,r_levels])
data.cols<-c(r_levels)

#Substract mobile inverts and tape wand shallow and uclassified
photo$new.N<-photo$N-(photo$MF+photo$UC+photo$TW)

#Calculate proportion
photo[,data.cols]<-photo[,data.cols]/photo$new.N
head(photo)

r_levels<-c(unique(as.character(ab$TIER_1)))
data.cols<-c(r_levels)


wsd<-merge(sites, photo, by=c("METHOD", "OBS_YEAR", "SITE"), all.y=T)
#utils::View(wsd) #view data in separate window

#There are a number of sites where CCR and Fish survey completed. The SITEVISITID was not assigned correctly. This script corrects that. 
sv<-read.csv("CCR Sites_SITEVISITIDissue.csv")
wsd$SITEVISITID<-as.character(wsd$SITEVISITID)
swaps=which(wsd$SITE %in% sv$SITE)
wsd$SITEVISITID[swaps]<-sv$correct.SITEVISITID[match(wsd$SITE[swaps],sv$SITE)]

head(subset(wsd,SITE=="HAW-01802"))

####################################################################################################################################################################
#
#     CHECK THAT DATA IS READY FOR POOLING AND DO SOME FINAL CLEAN UPS, EG SET BACKREEF DEPTH_ZONE TO ALL, CREATE THE "SGA" LOCATION
#
####################################################################################################################################################################
## NOW POOL UP TO Stratum AND YEAR
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv");tail(sectors)
sm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SITE MASTER.csv")
#new.rz<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/BIA/PRIA KiJoPa Benthic RZ.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)
#new.rz$SITE<-SiteNumLeadingZeros(new.rz$SITE)

#The fish team uses "protected reef slope" as a reef zone, but the benthic team has historically lumped this into forereef
#the new.rz dataframe is a list of sites KIN, JOH and PAL sites with the apppropriate forereef sites converted to protected reef slope
#this is merged with SM. Use the "REEF_ZONE_NEW" column for analysis
# new.rz<-new.rz[,c("SITE","OBS_YEAR","ANALYSIS_YEAR","REEF_ZONE_NEW")]
# sm<-merge(sm,new.rz,by=c("SITE","OBS_YEAR","ANALYSIS_YEAR"),all=TRUE)
# sm$REEF_ZONE_NEW<-ifelse(is.na(sm$REEF_ZONE_NEW),as.character(sm$REEF_ZONE),as.character(sm$REEF_ZONE_NEW)) #Fill in NA values with original reef zone


#Merge Site master with wsd
sm<-sm[,c("SITEVISITID","SITE","ANALYSIS_YEAR","ANALYSIS_SCHEME","OBS_YEAR","SEC_NAME","EXCLUDE_FLAG")]
wsd<-merge(wsd,sm,by=c("SITEVISITID","SITE","OBS_YEAR"),all.x=TRUE)
head(wsd)
wsd<-subset(wsd,PERM_SITE==0)


####################################################################################################################################################################
#
#     CHECK THAT DATA IS READY FOR POOLING AND DO SOME FINAL CLEAN UPS, EG SET BACKREEF DEPTH_ZONE TO ALL, CREATE THE "SGA" LOCATION
#
####################################################################################################################################################################

## check wwhether we have ISLANDS that arent in the sectors file
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

#The protected reef slope area and nh for Johnston aren't correct in sectors file. Just use the FRF area at Johnston for now until we fix this.
sectors$AREA_HA<-ifelse(sectors$REEF_ZONE=="Protected Slope"&sectors$ISLAND=="Johnston","0",as.numeric(as.character(sectors$AREA_HA)))
sectors$NH<-ifelse(sectors$REEF_ZONE=="Protected Slope"&sectors$ISLAND=="Johnston","0",as.numeric(as.character(sectors$NH)))
class(sectors$AREA_HA)
#Pooling FRF and PRS together
wsd$REEF_ZONE<-ifelse(wsd$REEF_ZONE=="Forereef"|wsd$REEF_ZONE=="Protected Slope","Forereef2",as.character(wsd$REEF_ZONE))
sectors$REEF_ZONE<-ifelse(sectors$REEF_ZONE=="Forereef"|sectors$REEF_ZONE=="Protected Slope","Forereef2",as.character(sectors$REEF_ZONE))

#Double check that Protected Slope was changed to Forereef
head(subset(wsd,REEF_ZONE=="Protected Slope"))# this should be zero

wsd$STRATA<-paste(substring(wsd$REEF_ZONE,1,1), substring(wsd$DEPTH_BIN,1,1), sep="")
sectors$STRATA<-paste(substring(sectors$REEF_ZONE,1,1), substring(sectors$DEPTH_BIN,1,1), sep="")

## TREAT GUGUAN, ALAMAGAN, SARIGAN AS ONE ISLAND  (REALLY ONE BASE REPORTING UNIT .. BUT SIMPLER TO STICK TO 'ISLAND')
SGA<-c("Guguan", "Alamagan", "Sarigan")
levels(wsd$ISLAND)<-c(levels(wsd$ISLAND), "AGS")
levels(sectors$ISLAND)<-c(levels(sectors$ISLAND), "AGS")
wsd[wsd$ISLAND %in% SGA,]$ISLAND<-"AGS"
sectors[sectors$ISLAND %in% SGA,]$ISLAND<-"AGS"

#Change Analysis year according to desired pooling
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

cast(st, ANALYSIS_SEC ~ ANALYSIS_SCHEME, value="AREA_HA", sum)
wsd<-merge(wsd, st, by=c("ANALYSIS_SCHEME", "ANALYSIS_SEC", "STRATA"), all.x=T)
#check if some are missing an AREA_HA .. which means that they didnt get into the stratification scheme properly
unique(wsd[is.na(wsd$AREA_HA), c("ISLAND", "ANALYSIS_SEC", "SEC_NAME", "OBS_YEAR", "ANALYSIS_SCHEME", "STRATA")])

#NOW CHECK HOW MANY REPS WE HAVE PER STRATA
a<-cast(wsd, ANALYSIS_SCHEME + ISLAND + ANALYSIS_SEC + OBS_YEAR ~ STRATA, value="AREA_HA", length); a

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
write.csv(dpst, file="PRIMM STRATA_ISLAND_YEAR.csv")


# e.g. SAVE BY ISLAND PER YEAR
OUTPUT_LEVEL<-c("ISLAND","ANALYSIS_YEAR") 
dpis<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
write.csv(dpis, file="PRIMM ISLAND & YEAR.csv")


#QC Checks- Doing this mannually for now. build in some scripts
#1. Do the number of sites for each island/year match the site master?- both fish and benthic sites should be included
#2. Does AREA_HA in the summarized data match what's in the SectorStrata Area file?
#3. Does the range of values make sense? Are there any values that are obviously incorrect (e.g. 200% cover)


