#This script generates the cover data for the 2019 American Samoa National Marine Sanctuary report completed in March 2019.
#In November 2019, Mareike Sudek requested to see data for Fagatelle and Fagalua Bays seperately. The code for this is at the bottom of this script
#At the time these data were generated in early 2019 we did not have access to the human-annotated 2018 data, so the robot data were used. The 2016 data were included in November, but not for the report.
rm(list=ls())

setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/BIA")

library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below
library(RODBC)            # to connect to oracle

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/fish_team_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_old.R")

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

#read in CNET robot from 2018 and SAMOA 2016 data (workaround til I can access data from oracle) generate files and look up and merge together
c.meta<-read.csv("ASRAMP2018_image_meta.csv")
c.data<-read.csv("ASRAMP2018_image_robot_data.csv")
b.meta<-read.csv("SAMOA2016_Cnetmeta.csv")
b.data<-read.csv(file="V:/ANALYSIS/CORALNET/BIA Annotations/annotations_SAMOA2016_BIA_raw.csv")
lookup<-read.csv("CNET_categories_lookup.csv")
df1<-merge(c.meta,c.data,by=c("ORIGINAL_FILE_NAME"))
df2<-merge(b.meta,b.data,by=c("ORIGINAL_FILE_NAME"))
head(df2);nrow(df2)
df3<-rbind(df1,df2)
df4<-merge(df3,lookup,by="SHORT_CODE");nrow(df4)
table(df4$OBS_YEAR)
df4$SITE<-SiteNumLeadingZeros(df4$SITE)


#Temporary workaround. Merge in 2016 data- can't access data in Oracle right now. Stupid password issues.
sm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)

#We don't have SITEVISITID in the robot metadata so merging on SITE- this is a problem for Howland and Baker where benthic and fish surveyed the same sites
sm<-sm[,c("SITEVISITID","SITE","PERM_SITE","CLIMATE_STATION_YN","TRANSECT_PHOTOS","DEPTH_BIN","REEF_ZONE","ANALYSIS_YEAR","OBS_YEAR","SEC_NAME","DATE_")]
robot<-merge(df4,sm,by=c("SITE","OBS_YEAR"),all.x=TRUE)
head(robot)
robot$CLIMATE_STATION_YN[is.na(robot$CLIMATE_STATION_YN)]<-"0"
robot$PERM_SITE[is.na(robot$PERM_SITE)]<-"0"

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
#occ<-read.csv("ASRAMP2018_OCCsitelist.csv")


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
FIELDS_TO_RETAIN<-c("MISSIONID","METHOD", "REGION", "OBS_YEAR","ISLAND", "SITEVISITID","SITE", "LATITUDE", "LONGITUDE", "REEF_ZONE", "DEPTH_BIN", "PERM_SITE", "CLIMATE_STATION_YN", "REP", "IMAGE_NAME", "PHOTOID", "ANALYST", "TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME", "SHORT_CODE", "POINTS")
x<-bia[,FIELDS_TO_RETAIN]; head(x)
y<-cnet[,FIELDS_TO_RETAIN]; head(y)
z<-robot[,FIELDS_TO_RETAIN]; head(z)

ab<-rbind(x,y,z)

#write.csv(ab, file="tmp All BIA BOTH METHODS.csv")
SURVEY_INFO<-c("OBS_YEAR", "REGION",  "ISLAND")
survey_island<-Aggregate_InputTable(cnet, SURVEY_INFO)

SURVEY_INFO<-c("MISSIONID","REGION","OBS_YEAR", "ISLAND", "SITEVISITID","SITE","LATITUDE","LONGITUDE","REEF_ZONE", "DEPTH_BIN", "PERM_SITE", "CLIMATE_STATION_YN")
survey_site<-Aggregate_InputTable(ab, SURVEY_INFO)

#Save list of sites
write.csv(survey_site,"All Photoquad Sites.csv")

#Remove Lat,Long and date before merging with SITE MASTER. The Lat and Long are 1 digit longer in the SM than the BIA data and will not merge properly with SM
survey_site.<- subset(survey_site, select=-c(LATITUDE,LONGITUDE))

#Read in original SITE MASTER
sm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SITE MASTER.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)


#Merge Site master with list of BIA sites
allsite<-merge(survey_site.,sm,by=c("REGION","ISLAND", "OBS_YEAR","SITEVISITID","SITE","REEF_ZONE", "DEPTH_BIN"),all=TRUE)
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

### GENERATE DATA AT SITE LEVEL FOR TIER 1 CATEGORIES
#Sum up all tier 1 points by site - pool transects together. You need to use dcast to insert zero values where there was no coral at a site (for example)
photo<-dcast(ab, formula=METHOD + OBS_YEAR + SITEVISITID + SITE  ~ TIER_1, value.var="POINTS", fun.aggregate=sum, fill=0)
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


####################################################################################################################################################################
#
#     CHECK THAT DATA IS READY FOR POOLING AND DO SOME FINAL CLEAN UPS, EG SET BACKREEF DEPTH_ZONE TO ALL
#     This is the original script using data in the wide format
####################################################################################################################################################################

## NOW POOL UP TO Stratum AND YEAR
#sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas_v2.csv")
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv")
sm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)

#Merge Site master with wsd
sm<-sm[,c("SITEVISITID","ANALYSIS_YEAR","OBS_YEAR","SEC_NAME","EXCLUDE_FLAG","TRANSECT_PHOTOS","Oceanography")]
wsd1<-merge(wsd,sm,by=c("SITEVISITID","OBS_YEAR"),all.x=TRUE)
head(wsd1)
wsd1$ANALYSIS_YEAR<-wsd1$OBS_YEAR
wsd1$ANALYSIS_YEAR<-as.character(wsd1$ANALYSIS_YEAR)

wsd1<-subset(wsd1,REGION=="SAMOA")
View(wsd1) #view data in separate window

#remove permanent sites, climate sites and special projects
wsd1$TRANSECT_PHOTOS[is.na(wsd1$TRANSECT_PHOTOS)]<-"0"
wsd1<-subset(wsd1,Oceanography!=1 & TRANSECT_PHOTOS==-1 & EXCLUDE_FLAG!=-1& PERM_SITE!=-1 &CLIMATE_STATION_YN!=-1)

#Check that # of sites/sector match survey master
wsd1<-droplevels(wsd1)
table(wsd1$SEC_NAME, wsd1$OBS_YEAR)


####Calculate Cover using demography script template
#Generate a table of metadata at the transect and site level for ADULTS
SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE","DEPTH_BIN", "LATITUDE", "LONGITUDE")
survey_site<-Aggregate_InputTable(wsd1, SURVEY_INFO)

#Merge together survey meta data and sector area files and check for missmatches 
meta<-merge(survey_site,sectors,by=c("REGION","SEC_NAME","ISLAND","REEF_ZONE","DEPTH_BIN"),all.x=TRUE)
test<-meta[is.na(meta$AREA_HA),]
test 
meta<-merge(survey_site,sectors,by=c("REGION","SEC_NAME","ISLAND","REEF_ZONE","DEPTH_BIN"))

#Merge site level data and meta data & test for missmatches
sitedata<-merge(wsd1,meta[,c("SITE","SITEVISITID","SEC_NAME","AREA_HA","NH","BENTHIC_2018")],by=c("SITEVISITID","SITE","SEC_NAME"),all.x=TRUE)
test<-sitedata[is.na(sitedata$AREA_HA),]
test
sitedata<-merge(wsd1,meta[,c("SITE","SITEVISITID","AREA_HA","NH","BENTHIC_2018")],by=c("SITEVISITID","SITE"))

#Check that # of sites/sector match survey master
table(sitedata$SEC_NAME, sitedata$OBS_YEAR)


#Create STRATANAME by idenityfing which ANALAYSIS SCHEME you want to use then concatinating with depth and reef zone that will be used to pool data
sitedata$STRATANAME=paste0(sitedata$BENTHIC_2018,"_",sitedata$DEPTH_BIN,"_",sitedata$REEF_ZONE)
sitedata$DB_RZ<-paste(substring(sitedata$REEF_ZONE,1,1), substring(sitedata$DEPTH_BIN,1,1), sep="")

#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
sitedata$ANALYSIS_SCHEMA<-sitedata$STRATANAME
sitedata$DOMAIN_SCHEMA<-sitedata$BENTHIC_2018

#Calculate metrics at Sector-level
coral_d<-Calc_Domain_Cover(sitedata,"CORAL");colnames(coral_d)[colnames(coral_d)=="D._st"]<-"MeanCoral";colnames(coral_d)[colnames(coral_d)=="SE_D._st"]<-"SECoral";colnames(coral_d)[colnames(coral_d)=="DOMAIN_SCHEMA"]<-"Sector";coral_d<-coral_d[,c("OBS_YEAR","Sector","n","Ntot","MeanCoral","SECoral")]
cca_d<-Calc_Domain_Cover(sitedata,"CCA");colnames(cca_d)[colnames(cca_d)=="D._st"]<-"MeanCCA";colnames(cca_d)[colnames(cca_d)=="SE_D._st"]<-"SECCA";colnames(cca_d)[colnames(cca_d)=="DOMAIN_SCHEMA"]<-"Sector";cca_d<-cca_d[,c("OBS_YEAR","Sector","n","Ntot","MeanCCA","SECCA")]
ma_d<-Calc_Domain_Cover(sitedata,"MA");colnames(ma_d)[colnames(ma_d)=="D._st"]<-"MeanMA";colnames(ma_d)[colnames(ma_d)=="SE_D._st"]<-"SEMA";colnames(ma_d)[colnames(ma_d)=="DOMAIN_SCHEMA"]<-"Sector";ma_d<-ma_d[,c("OBS_YEAR","Sector","n","Ntot","MeanMA","SEMA")]
turf_d<-Calc_Domain_Cover(sitedata,"TURF");colnames(turf_d)[colnames(turf_d)=="D._st"]<-"MeanTURF";colnames(turf_d)[colnames(turf_d)=="SE_D._st"]<-"SETURF";colnames(turf_d)[colnames(turf_d)=="DOMAIN_SCHEMA"]<-"Sector";turf_d<-turf_d[,c("OBS_YEAR","Sector","n","Ntot","MeanTURF","SETURF")]


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("OBS_YEAR","Sector","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
is<-Reduce(MyMerge, list(coral_d,cca_d,ma_d,turf_d))

#multiply values by 100 to get percentage
is<-cbind(is[c(1:4)],is[c(5:ncol(is))]*100)
head(is)

#Calculate metrics at Strata-level
coral_st<-Calc_Strata_Cover(sitedata,"CORAL");coral_st<-coral_st[c(1:3,5,6,8,11)];colnames(coral_st)<- c("OBS_YEAR","Sector","Stratum","n","Ntot","MeanCoral","SECoral") #change group to whatever your grouping field is.
cca_st<-Calc_Strata_Cover(sitedata,"CCA");cca_st<-cca_st[c(1:3,5,6,8,11)];colnames(cca_st)<- c("OBS_YEAR","Sector","Stratum","n","Ntot","MeanCCA","SECCA")
ma_st<-Calc_Strata_Cover(sitedata,"MA");ma_st<-ma_st[c(1:3,5,6,8,11)];colnames(ma_st)<- c("OBS_YEAR","Sector","Stratum","n","Ntot","MeanMA","SEMA")
turf_st<-Calc_Strata_Cover(sitedata,"TURF");turf_st<-turf_st[c(1:3,5,6,8,11)];colnames(turf_st)<- c("OBS_YEAR","Sector","Stratum","n","Ntot","MeanTURF","SETURF")

MyMerge <- function(x, y){
  df <- merge(x, y, by= c("OBS_YEAR","Sector","Stratum","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
st<-Reduce(MyMerge, list(coral_st,cca_st,ma_st,turf_st))

#multiply values by 100 to get percentage
samoa_st<-cbind(st[c(1:5)],st[c(6:ncol(st))]*100)
head(samoa_st,20)

write.csv(samoa_st,"C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/ASRAMP 2018/ASNMS_Report_Cover_Strata2.csv")
write.csv(is,"C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/ASRAMP 2018/ASNMS_Report_Cover_Sector2.csv")


#Merge site level data and meta data & test for missmatches
sitedata<-merge(wsd1,meta[,c("SITE","SITEVISITID","SEC_NAME","AREA_HA","NH","BENTHIC_2018")],by=c("SITEVISITID","SITE","SEC_NAME"),all.x=TRUE)
test<-sitedata[is.na(sitedata$AREA_HA),]
test
sitedata<-merge(wsd1,meta[,c("SITE","SITEVISITID","AREA_HA","NH","BENTHIC_2018")],by=c("SITEVISITID","SITE"))


##########################################
# Generate cover data for Fagalu and fagatelle separately  ----------------
sitedata<-subset(sitedata,ISLAND=="Tutuila")
#Check that # of sites/sector match survey master
table(sitedata$SEC_NAME, sitedata$OBS_YEAR)


#Create STRATANAME by idenityfing which ANALAYSIS SCHEME you want to use then concatinating with depth and reef zone that will be used to pool data
sitedata$STRATANAME=paste0(sitedata$SEC_NAME,"_",sitedata$DEPTH_BIN,"_",sitedata$REEF_ZONE)
sitedata$DB_RZ<-paste(substring(sitedata$REEF_ZONE,1,1), substring(sitedata$DEPTH_BIN,1,1), sep="")


#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
sitedata$ANALYSIS_SCHEMA<-sitedata$STRATANAME
sitedata$DOMAIN_SCHEMA<-sitedata$SEC_NAME

#Calculate metrics at Strata-level for 2015, 2015 and 2018 for all available strata that have at least 2 sites/stratum
coral_st<-Calc_Strata_Cover(sitedata,"CORAL");coral_st<-coral_st[c(1:3,5,6,8,11)];colnames(coral_st)<- c("OBS_YEAR","Sector","Stratum","n","Ntot","MeanCoral","SECoral") #change group to whatever your grouping field is.
cca_st<-Calc_Strata_Cover(sitedata,"CCA");cca_st<-cca_st[c(1:3,5,6,8,11)];colnames(cca_st)<- c("OBS_YEAR","Sector","Stratum","n","Ntot","MeanCCA","SECCA")
ma_st<-Calc_Strata_Cover(sitedata,"MA");ma_st<-ma_st[c(1:3,5,6,8,11)];colnames(ma_st)<- c("OBS_YEAR","Sector","Stratum","n","Ntot","MeanMA","SEMA")
turf_st<-Calc_Strata_Cover(sitedata,"TURF");turf_st<-turf_st[c(1:3,5,6,8,11)];colnames(turf_st)<- c("OBS_YEAR","Sector","Stratum","n","Ntot","MeanTURF","SETURF")

MyMerge <- function(x, y){
  df <- merge(x, y, by= c("OBS_YEAR","Sector","Stratum","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
st<-Reduce(MyMerge, list(coral_st,cca_st,ma_st,turf_st))

#multiply values by 100 to get percentage
samoa_st<-cbind(st[c(1:5)],st[c(6:ncol(st))]*100)
head(samoa_st,20)


#Calculate metrics at Sector-level

#remove 2016 data- very low repliation for 2016 and Fagalua Shallow stratum because it was not sampled in 2018
sitedata<-subset(sitedata,OBS_YEAR!="2016",STRATANAME!="TUT_FAGALUA_Shallow_Forereef")

coral_d<-Calc_Domain_Cover(sitedata,"CORAL");colnames(coral_d)[colnames(coral_d)=="D._st"]<-"MeanCoral";colnames(coral_d)[colnames(coral_d)=="SE_D._st"]<-"SECoral";colnames(coral_d)[colnames(coral_d)=="DOMAIN_SCHEMA"]<-"Sector";coral_d<-coral_d[,c("OBS_YEAR","Sector","n","Ntot","MeanCoral","SECoral")]
cca_d<-Calc_Domain_Cover(sitedata,"CCA");colnames(cca_d)[colnames(cca_d)=="D._st"]<-"MeanCCA";colnames(cca_d)[colnames(cca_d)=="SE_D._st"]<-"SECCA";colnames(cca_d)[colnames(cca_d)=="DOMAIN_SCHEMA"]<-"Sector";cca_d<-cca_d[,c("OBS_YEAR","Sector","n","Ntot","MeanCCA","SECCA")]
ma_d<-Calc_Domain_Cover(sitedata,"MA");colnames(ma_d)[colnames(ma_d)=="D._st"]<-"MeanMA";colnames(ma_d)[colnames(ma_d)=="SE_D._st"]<-"SEMA";colnames(ma_d)[colnames(ma_d)=="DOMAIN_SCHEMA"]<-"Sector";ma_d<-ma_d[,c("OBS_YEAR","Sector","n","Ntot","MeanMA","SEMA")]
turf_d<-Calc_Domain_Cover(sitedata,"TURF");colnames(turf_d)[colnames(turf_d)=="D._st"]<-"MeanTURF";colnames(turf_d)[colnames(turf_d)=="SE_D._st"]<-"SETURF";colnames(turf_d)[colnames(turf_d)=="DOMAIN_SCHEMA"]<-"Sector";turf_d<-turf_d[,c("OBS_YEAR","Sector","n","Ntot","MeanTURF","SETURF")]


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("OBS_YEAR","Sector","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
is<-Reduce(MyMerge, list(coral_d,cca_d,ma_d,turf_d))

#multiply values by 100 to get percentage
is<-cbind(is[c(1:4)],is[c(5:ncol(is))]*100)
head(is)


samoa_st<-subset(samoa_st,Sector %in% c("TUT_FAGALUA","TUT_FAGATELE"))
is<-subset(is,Sector %in% c("TUT_FAGALUA","TUT_FAGATELE"))


write.csv(samoa_st,"C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/ASRAMP 2018/ASNMS_Cover_MareikeSTRATA.csv")
write.csv(is,"C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/ASRAMP 2018/ASNMS_Cover_MareikeSECTOR.csv")
