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

write.csv(robot,"ASRAMP2018_BIA_robot_raw.Rdata")

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

SURVEY_INFO<-c("MISSIONID","REGION","OBS_YEAR", "ISLAND", "SITEVISITID","SITE","LATITUDE","LONGITUDE","REEF_ZONE", "DEPTH_BIN", "PERM_SITE", "CLIMATE_STATION_YN", "MIN_DEPTH", "MAX_DEPTH", "HABITAT_CODE")
survey_site<-Aggregate_InputTable(ab, SURVEY_INFO)

colnames(survey_site)[colnames(survey_site)=="MIN_DEPTH"]<-"SITE_MIN_DEPTH_FT" #Change column name
colnames(survey_site)[colnames(survey_site)=="MAX_DEPTH"]<-"SITE_MAX_DEPTH_FT" #Change column name


#Save list of sites
write.csv(survey_site,"All Photoquad Sites.csv")

#Remove Lat,Long and date before merging with SITE MASTER. The Lat and Long are 1 digit longer in the SM than the BIA data and will not merge properly with SM
survey_site.<- subset(survey_site, select=-c(LATITUDE,LONGITUDE))

#Read in original SITE MASTER
sm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SITE MASTER.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)


#Merge Site master with list of BIA sites
allsite<-merge(survey_site.,sm,by=c("REGION","ISLAND", "OBS_YEAR","SITEVISITID","SITE","REEF_ZONE", "DEPTH_BIN","SITE_MAX_DEPTH_FT", "SITE_MIN_DEPTH_FT", "HABITAT_CODE"),all=TRUE)
head(allsite)

#Indentify sites that did not merge properly
test<-ddply(allsite,.(SITE),summarize,sites=length(SITE));subset(test,sites>1) #this should return 0 rows

#Generate a list of sites found in the BIA data, but not SM and remove all columns except OBS_YEAR and SITEVISITID
miss.sites<-allsite[is.na(allsite$METHOD), ]
miss.sites.<- subset(miss.sites, select=c(OBS_YEAR,SITEVISITID))
nrow(miss.sites.)

#The older CPCE data doesn't have tier 3 classification, so we assign it tier 2 if the value is missing
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

# ###Create a Peyssonnelia class
# ab$GENERA_NAME<-as.character(ab$GENERA_NAME)
# ab$TIER_1<-as.character(ab$TIER_1)
# 
# for (i in 1:nrow(ab)){ #opening brace
#   if(ab$GENERA_NAME[i] =="Peyssonnelia sp"){ #c&p
#     ab$TIER_1[i]="PESP" #c&p
#   } #c&p
# } #closing curly brace for entire for loop
# 
# pes<-subset(ab,TIER_2=="PESP")
# head(pes)

all.tab<-aggregate(ab$POINTS, by=ab[,c("METHOD", "TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME", "SHORT_CODE")], FUN=length)

ab<-droplevels(ab)
table(ab$ISLAND, ab$OBS_YEAR)
summary(ab)


#### NB THERE ARE SEVERAL UNCLASSIFIED CATEGORIES  THAT WILL NEED TO BE REMOVED PRIOR TO CALCULATING % COVER
UNIDENTIFIED_T1<-c("TW", "MF", "UC")
UNIDENTIFIED_T2<-c("MOBF", "TAPE", "UNK", "WAND", "SHAD")

length(unique(ab$SITE))

#Generate a SITE table
SITE_FIELDS<-c("METHOD", "REGION", "OBS_YEAR", "ISLAND", "PERM_SITE", "CLIMATE_STATION_YN", "SITEVISITID","SITE", "LATITUDE", "LONGITUDE", "REEF_ZONE", "DEPTH_BIN")
sites<-aggregate(ab[,"POINTS"],by=ab[,SITE_FIELDS], sum)
sites$x<-NULL
dim(sites)



####################################
### GENERATE DATA AT SITE LEVEL FOR TIER 1 CATEGORIES
#Sum up all tier 1 points by site - pool transects together. You need to use dcast to insert zero values where there was no coral at a site (for example)
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


wsd<-merge(sites, photo, by=c("METHOD", "OBS_YEAR", "SITEVISITID","SITE"), all.y=T)
utils::View(wsd) #view data in separate window

sv<-read.csv("CCR Sites_SITEVISITIDissue.csv")

wsd$SITEVISITID<-as.character(wsd$SITEVISITID)
swaps=which(wsd$SITE %in% sv$SITE)
wsd$SITEVISITID[swaps]<-sv$correct.SITEVISITID[match(wsd$SITE[swaps],sv$SITE)]

## NOW POOL UP TO Stratum AND YEAR
#sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas_v2.csv")
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv")
sm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SITE MASTER.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)


#Merge Site master with wsd
sm<-sm[,c("SITEVISITID","SITE","ANALYSIS_YEAR","ANALYSIS_SCHEME","OBS_YEAR","SEC_NAME","EXCLUDE_FLAG")]
wsd1<-merge(wsd,sm,by=c("SITEVISITID","SITE","OBS_YEAR"),all.x=TRUE)
head(wsd1)
wsd<-subset(wsd1,PERM_SITE==0)


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
wsd[wsd$REGION =="SAMOA" & wsd$OBS_YEAR =="2015",]$ANALYSIS_YEAR<-"2015-16"


#Generate a 
sectors<-subset(sectors,select=c("SEC_NAME","REEF_ZONE","DEPTH_BIN","AREA_HA","NH","RAMP_BASIC","MARI2011","MARI2014","TUT10_12","AS_SANCTUARY"))
a <- melt(sectors,id.vars=c("SEC_NAME","REEF_ZONE","DEPTH_BIN","AREA_HA","NH"))
colnames(a)[colnames(a)=="variable"]<-"ANALYSIS_SCHEME"
colnames(a)[colnames(a)=="value"]<-"ANALYSIS_SEC"
head(a)

test<-merge(wsd, a, by=c("SEC_NAME", "ANALYSIS_SCHEME","REEF_ZONE","DEPTH_BIN"), all.x=T)  # add ANALYSIS_SCHEME for this sector and sceheme combination
bad<-test[is.na(test$ANALYSIS_SCHEME),]
#Kauai 2013 disease sites missing from SM
write.csv(bad,"sitevisit_issues.csv")

#Remove Kauai special project and Samoa special project and Timor Leste
wsd<-merge(wsd, a, by=c("SEC_NAME", "ANALYSIS_SCHEME","REEF_ZONE","DEPTH_BIN"))  # add ANALYSIS_SCHEME for this sector and sceheme combination
wsd<-subset(wsd,ANALYSIS_SCHEME!="SAMOA_CLIMATE")
wsd<-subset(wsd,REGION!="CT")

#check if some are missing an AREA_HA .. which means that they didnt get into the stratification scheme properly
unique(wsd[is.na(wsd$AREA_HA), c("ISLAND", "ANALYSIS_SEC", "SEC_NAME", "OBS_YEAR", "ANALYSIS_YEAR", "ANALYSIS_SCHEME", "STRATA")])

#NOW CHECK HOW MANY REPS WE HAVE PER STRATA
a<-cast(wsd, REGION + ANALYSIS_SCHEME + ISLAND + ANALYSIS_SEC + ANALYSIS_YEAR ~ STRATA, value="AREA_HA", length); a

# OUTPUT sites per years (appendix 3) -------------------------------------
save(a, file="sites_year_reef_zone_depth_bin.rdata") ## use this for table in appendix 3 - see appendices R file


####################################################################################################################################################################
#
#     POOL WSD (WORKING SITE DATA TO STRATA THEN TO HIGHER LEVELS
##
###################################################################################################################################################################

#Change PRIA region according to PIRO request.
wsd$REGION<-ifelse(wsd$ISLAND %in% c("Baker","Howland"),"PRIA_Phoenix",as.character(wsd$REGION))
wsd$REGION<-ifelse(wsd$ISLAND %in% c("Kingman","Palmyra","Jarvis"),"PRIA_Line",as.character(wsd$REGION))
wsd$REGION<-ifelse(wsd$ISLAND =="Wake","Wake",as.character(wsd$REGION))
wsd$REGION<-ifelse(wsd$ISLAND =="Johnston","Johnston",as.character(wsd$REGION))

wsd<-droplevels(wsd)
table(wsd$REGION,wsd$ANALYSIS_YEAR)

### CALCULATE MEAN AND VARIANCE WITHIN STRATA ###
SPATIAL_POOLING_BASE<-c("REGION", "ISLAND", "ANALYSIS_SEC", "REEF_ZONE", "STRATA")    
ADDITIONAL_POOLING_BY<-c("ANALYSIS_YEAR")                                    # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)

#generate within strata means and vars
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
dps<-Calc_PerStrata(wsd, data.cols, c(POOLING_LEVEL, "AREA_HA"))
#save(dps,file="tmp REA per strata.RData")
head(dps$Mean)

###### REMOVE STRATA with N=1 (cannot pool those up)
dps$Mean<-dps$Mean[dps$Mean$N>1,]
dps$SampleVar<-dps$SampleVar[dps$SampleVar$N>1,]
dps$SampleSE<-dps$SampleSE[dps$SampleSE$N>1,]


# SAVE BY REGION PER YEAR
OUTPUT_LEVEL<-c("REGION", "ANALYSIS_YEAR") 
dp<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
save(dp, file="ESD_CoralCover_2010_2018_region.csv")
dp<-as.data.frame(dp)

datacols<-c("Mean.REGION","Mean.ANALYSIS_YEAR","Mean.N","Mean.CORAL","PooledSE.CORAL")
dp<-dp<-dp[,datacols]
dp<-cbind(dp[c(1:3)],dp[c(4:ncol(dp))]*100);head(dp)
write.csv(dp,"ESD_CoralCover_PIRO2.csv")



####################################
### GENERATE DATA AT SITE LEVEL FOR TIER 3 CATEGORIES
#Sum up all tier 1 points by site - pool transects together. You need to use dcast to insert zero values where there was no coral at a site (for example)
photo<-cast(ab, METHOD + OBS_YEAR + SITEVISITID + SITE  ~ TIER_3, value="POINTS", fun.aggregate=sum, fill=0)
head(photo)


#now convert to proportions
r_levels<-c(unique(as.character(ab$TIER_3)))
r_levels<-c(unique(as.character(ab$TIER_3)))

photo$N<-rowSums(photo[,r_levels])
data.cols<-c(r_levels)

#Substract mobile inverts and tape wand shallow and uclassified
photo$new.N<-photo$N-(photo$TAPE+photo$UNK+photo$SHAD+photo$WAND+photo$MOBF)

#Calculate proportion
photo[,data.cols]<-photo[,data.cols]/photo$new.N
head(photo)

r_levels<-c(unique(as.character(ab$TIER_3)))
data.cols<-c(r_levels)


wsd<-merge(sites, photo, by=c("METHOD", "OBS_YEAR", "SITEVISITID","SITE"), all.y=T)
utils::View(wsd) #view data in separate window

sv<-read.csv("CCR Sites_SITEVISITIDissue.csv")

wsd$SITEVISITID<-as.character(wsd$SITEVISITID)
swaps=which(wsd$SITE %in% sv$SITE)
wsd$SITEVISITID[swaps]<-sv$correct.SITEVISITID[match(wsd$SITE[swaps],sv$SITE)]

## NOW POOL UP TO Stratum AND YEAR
#sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas_v2.csv")
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv")
sm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SITE MASTER.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)


#Merge Site master with wsd
sm<-sm[,c("SITEVISITID","SITE","ANALYSIS_YEAR","ANALYSIS_SCHEME","OBS_YEAR","SEC_NAME","EXCLUDE_FLAG")]
wsd1<-merge(wsd,sm,by=c("SITEVISITID","SITE","OBS_YEAR"),all.x=TRUE)
head(wsd1)
wsd<-subset(wsd1,PERM_SITE==0)


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
wsd[wsd$REGION =="SAMOA" & wsd$OBS_YEAR =="2015",]$ANALYSIS_YEAR<-"2015-16"


sectors<-subset(sectors,select=c("SEC_NAME","REEF_ZONE","DEPTH_BIN","AREA_HA","NH","RAMP_BASIC","MARI2011","MARI2014","TUT10_12","AS_SANCTUARY"))
a <- melt(sectors,id.vars=c("SEC_NAME","REEF_ZONE","DEPTH_BIN","AREA_HA","NH"))
colnames(a)[colnames(a)=="variable"]<-"ANALYSIS_SCHEME"
colnames(a)[colnames(a)=="value"]<-"ANALYSIS_SEC"
head(a)

test<-merge(wsd, a, by=c("SEC_NAME", "ANALYSIS_SCHEME","REEF_ZONE","DEPTH_BIN"), all.x=T)  # add ANALYSIS_SCHEME for this sector and sceheme combination
bad<-test[is.na(test$ANALYSIS_SCHEME),]
#Kauai 2013 disease sites missing from SM
write.csv(bad,"sitevisit_issues.csv")

#Remove Kauai special project and Samoa special project and Timor Leste
wsd<-merge(wsd, a, by=c("SEC_NAME", "ANALYSIS_SCHEME","REEF_ZONE","DEPTH_BIN"))  # add ANALYSIS_SCHEME for this sector and sceheme combination
wsd<-subset(wsd,ANALYSIS_SCHEME!="SAMOA_CLIMATE")
wsd<-subset(wsd,REGION!="CT")
wsd<-subset(wsd,OBS_YEAR>2012)

#check if some are missing an AREA_HA .. which means that they didnt get into the stratification scheme properly
unique(wsd[is.na(wsd$AREA_HA), c("ISLAND", "ANALYSIS_SEC", "SEC_NAME", "OBS_YEAR", "ANALYSIS_YEAR", "ANALYSIS_SCHEME", "STRATA")])

#NOW CHECK HOW MANY REPS WE HAVE PER STRATA
a<-cast(wsd, REGION + ANALYSIS_SCHEME + ISLAND + ANALYSIS_SEC + ANALYSIS_YEAR ~ STRATA, value="AREA_HA", length); a

# OUTPUT sites per years (appendix 3) -------------------------------------
save(a, file="sites_year_reef_zone_depth_bin.rdata") ## use this for table in appendix 3 - see appendices R file


####################################################################################################################################################################
#
#     POOL WSD (WORKING SITE DATA TO STRATA THEN TO HIGHER LEVELS
##
###################################################################################################################################################################
#Change PRIA region according to PIRO request.
wsd$REGION<-ifelse(wsd$ISLAND %in% c("Baker","Howland"),"PRIA_Phoenix",as.character(wsd$REGION))
wsd$REGION<-ifelse(wsd$ISLAND %in% c("Kingman","Palmyra","Jarvis"),"PRIA_Line",as.character(wsd$REGION))
wsd$REGION<-ifelse(wsd$ISLAND =="Wake","Wake",as.character(wsd$REGION))
wsd$REGION<-ifelse(wsd$ISLAND =="Johnston","Johnston",as.character(wsd$REGION))

table(wsd$ISLAND,wsd$ANALYSIS_YEAR)

### CALCULATE MEAN AND VARIANCE WITHIN STRATA ###
SPATIAL_POOLING_BASE<-c("REGION", "ISLAND", "ANALYSIS_SEC", "REEF_ZONE", "STRATA")    
ADDITIONAL_POOLING_BY<-c("ANALYSIS_YEAR")                                    # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)

#generate within strata means and vars
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
dps<-Calc_PerStrata(wsd, data.cols, c(POOLING_LEVEL, "AREA_HA"))
#save(dps,file="tmp REA per strata.RData")
head(dps$Mean)

###### REMOVE STRATA with N=1 (cannot pool those up)
dps$Mean<-dps$Mean[dps$Mean$N>1,]
dps$SampleVar<-dps$SampleVar[dps$SampleVar$N>1,]
dps$SampleSE<-dps$SampleSE[dps$SampleSE$N>1,]


# SAVE BY REGION PER YEAR
OUTPUT_LEVEL<-c("REGION", "ANALYSIS_YEAR") 
dp<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
dp<-as.data.frame(dp)

datacols<-c("Mean.REGION","Mean.ANALYSIS_YEAR","Mean.N","Mean.POCS","PooledSE.POCS")
dp<-dp<-dp[,datacols]
dp<-cbind(dp[c(1:3)],dp[c(4:ncol(dp))]*100);head(dp)
dp<-write.csv(dp,"ESD_POCSCover_region.csv")
