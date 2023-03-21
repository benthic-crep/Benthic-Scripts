rm(list=ls())


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
#The robot annotations are included because the confidence threshold in CoralNet was set to 70-90% allowing the robot to annotate points when it was 70-90% certain.
#2019 NWHI data not in these view because it was analyzed as part of a bleaching dataset
#load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_BIA_STR_CNET.rdata") #load data

cnet<-read.csv("T:/DataManagement/DataRequests/Courtney Couch/2023/MV_BIA_CNET_ANALYSIS_DATA.csv");cnet<-select(cnet,-c(SITE,TYPE))

sm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
sm<-select(sm,c("SITEVISITID","SITE"))

#This function isn't working- work with tye on this. This is hack for now
#cnet$SITE<-SiteNumLeadingZeros(cnet$SITE)

cnet<-left_join(cnet,sm)

#Temporary work around for merging in 2014-2017 NWHI data that hasn't been uploaded to Oracle yet- remove this once Michael has incorporated data
new.nw<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Raw Data from CoralNet/2014-2017_NWHI_CnetAnnotations_formatted.csv")
new.nw<-new.nw %>% drop_na(ROUNDID) #remove blank rows

new.cnet<-new.nw

class(new.cnet$DATE_)
class(new.cnet$DATE_TAKEN)

#Date conversations still not working
new.cnet$DATE_<-lubridate::mdy(new.cnet$DATE_)
new.cnet$DATE_TAKEN<-lubridate::ymd(new.cnet$DATE_TAKEN);head(new.cnet$DATE_TAKEN)
new.cnet$DATE_ANNOTATED<-lubridate::ymd_hms(new.cnet$DATE_ANNOTATED);head(new.cnet$DATE_ANNOTATED)

#combine old cnet and 2015 & 2017 nwhi cnet data
cnet<-rbind(cnet,new.cnet) 
table(cnet$REGION,cnet$OBS_YEAR)
table(new.cnet$ISLAND,new.cnet$OBS_YEAR)


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

sm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv")

test<-subset(sm,OBS_YEAR=="2019",TRANSECT_PHOTOS=="-1");nrow(test)

# Merge together all Photoquad Datasets & make sure columns match ---------------------------------------
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
FIELDS_TO_RETAIN<-c("MISSIONID","METHOD", "REGION", "OBS_YEAR","ISLAND", "SITEVISITID","SITE", "LATITUDE", "LONGITUDE", "REEF_ZONE", "DEPTH_BIN", "PERM_SITE", 
                    "CLIMATE_STATION_YN", "MIN_DEPTH", "MAX_DEPTH", "HABITAT_CODE", "REP", "IMAGE_NAME", "PHOTOID", "ANALYST", "TIER_1", "CATEGORY_NAME", 
                    "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME", "POINTS")
x<-bia[,FIELDS_TO_RETAIN]; head(x)
y<-cnet[,FIELDS_TO_RETAIN]; head(y)
z<-cli[,FIELDS_TO_RETAIN]; head(z)

ab<-rbind(x,y,z)


#Flag sites that have more than 33 and less than 15 images
#With the exception of OCC 2012 sites, there should be 30 images/site/10 points/image
test<-ddply(ab,.(OBS_YEAR,SITEVISITID,SITE),summarize,count=sum(POINTS))
test2<-test[test$count<150 |test$count>330,]
View(test2)
#Ignore 2012 OCC sites. They analyzed 50 points per images 

#Remove sites with less than 150 points -These really should be removed from Oracle eventually
test3<-test[test$count<150,];test3
ab<-ab[!(ab$SITE %in% test3$SITE),];head(ab)
subset(ab,SITE %in% c("TUT-00210","TUT-00275","OAH-00558")) #double check that sites were dropped properly

#Generate a table of # of sites/region and year from original datasets before data cleaning takes place
#use this later in the script to make sure sites haven't been dropped after data clean up.
oracle.site<-ddply(ab,.(REGION,OBS_YEAR),summarize,nSite=length(unique(SITE)))
oracle.site

#Check this against site master list
table(sm$REGION,sm$OBS)
ab.site<-ddply(subset(cnet,OBS_YEAR=="2019"),.(REGION,OBS_YEAR),summarize,nSite=length(unique(SITE)));ab.site

#identify which new sites are in the CoralNet data, but still need to be integrated into the SURVEY MASTER file
miss.from.sm<-cnet[!(cnet$SITEVISITID %in% sm$SITEVISITID),]
miss.from.smSITE<-ddply(miss.from.sm,.(SITEVISITID,REGION,ISLAND,SITE,REEF_ZONE,DEPTH_BIN,ROUNDID,MISSIONID,OBS_YEAR, DATE_,HABITAT_CODE,
                                       LATITUDE,LONGITUDE,MIN_DEPTH,MAX_DEPTH),summarize,tmp=length(REPLICATE));miss.from.smSITE

write.csv(miss.from.smSITE,file="C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/121120_SitesmissingfromSM.csv") #export list and manually add to SM
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


# Reclassify EMA and Halimeda --------------------------------------------


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

ab$TIER_3<-ifelse(ab$TIER_3=="HALI","HAL",as.character(ab$TIER_3))
ab$TIER_1<-ifelse(ab$TIER_3=="HAL","HAL",as.character(ab$TIER_1))
ab$CATEGORY_NAME<-ifelse(ab$TIER_3=="HAL","Halimeda sp",ab$CATEGORY_NAME)

hal<-subset(ab,TIER_1=="HAL")
head(hal)

all.tab<-aggregate(ab$POINTS, by=ab[,c("METHOD", "TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME")], FUN=length)
write.csv(all.tab, file="All CATEGORIES BOTH METHODS CLEANED.csv")

save(ab, file="T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/BIA_2010-2020_CLEANED.RData")

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


#Generate a SITE table
sites<-unique(ab[,c("METHOD","REGION","OBS_YEAR","ISLAND","PERM_SITE","CLIMATE_STATION_YN","SITEVISITID","LATITUDE","LONGITUDE","REEF_ZONE","DEPTH_BIN")])

dim(sites)


# Generate Site-level Data at TIER 1 level--------------

photo<-dcast(ab, formula=METHOD + OBS_YEAR + SITEVISITID + SITE  ~ TIER_1, value.var="POINTS", sum, fill=0)
head(photo)

r_levels<-c(unique(as.character(ab$TIER_1)))
photo$N<-rowSums(photo[,r_levels])

#Subtract mobile inverts and tape wand shallow and unclassified
photo$new.N<-photo$N-(photo$MF+photo$UC+photo$TW)

#Add CCA + Coral
photo$CCA_CORAL<-photo$CCA + photo$CORAL

r_levels<-c(unique(as.character(ab$TIER_1)),"CCA_CORAL")
data.cols<-c(r_levels)
data.cols

#Calculate proportion
photo[,data.cols]<-(photo[,data.cols]/photo$new.N)*100
head(photo)

#Add Reef Builder Ratio: BSR (Benthic Substrate Ratio)
photo$BSR<-(photo$CCA + photo$CORAL)/(photo$TURF+ photo$MA)

# plot(photo$BSR,
#      (photo$CORAL+photo$CCA)/(photo$TURF+photo$MA))
# abline(0,1)


r_levels<-c(unique(as.character(ab$TIER_1)),"BSR")
T1data.cols<-c(r_levels)
T1data.cols<-T1data.cols[!T1data.cols %in% c("TW","UC","MF")]

wsd<-merge(sites, photo, by=c("METHOD", "OBS_YEAR", "SITEVISITID"), all.y=T)

#Make sure that you have the correct # of sites/region and year
test1<-ddply(wsd,.(REGION,OBS_YEAR),summarize,nSite_wsd=length(unique(SITE)))

#check against original number of sites pulled from oracle
full_join(test1,oracle.site)

#Merge Tier 1 data with SURVEY MASTER FILE
#Remember this will have all the sites ever surveyed not just StRS sites
#You will need TRANSECT_PHOTOS, EXCLUDE FLAG and Oceanography from the SM file to be able to filter out OCC and Special Project sites

sm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")

#Convert date formats
sm$DATE_<-mdy(sm$DATE_)
class(sm$DATE_)

head(sm)

sm<-sm[,c("DATE_","MISSIONID","SITEVISITID","SITE","OCC_SITEID","ANALYSIS_YEAR","ANALYSIS_SCHEME","OBS_YEAR","SEC_NAME","EXCLUDE_FLAG","TRANSECT_PHOTOS","Oceanography","new_MIN_DEPTH_M","new_MAX_DEPTH_M")]
wsd_t1<-merge(sm,wsd,by=c("SITEVISITID","SITE","OBS_YEAR"),all.y=TRUE)
head(wsd_t1)


#Are there NAs in TRANSECT PHOTOS
test<-wsd_t1[is.na(wsd_t1$TRANSECT_PHOTOS),]
View(test) # none of the 2010 imagery has TRANSECT_PHOTOS assigned - ASK MICHAEL TO FIX
wsd_t1$TRANSECT_PHOTOS<-"-1" #make sure that all rows = -1


#Remove the unknowns and TWS columns
wsd_t1<-subset(wsd_t1,select= -c(MF,UC,TW))

#Remove sites that have less than 150 points after removing MF,UC, TW (a lot of our early imagery was poor quality and will be removed)
wsd_t1<-subset(wsd_t1,new.N >=150)

#Save Tier 1 site data to t drive. This file has all sites (fish, benthic and OCC) that were annoated between 2010 and 2018
write.csv(wsd_t1, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2020_Tier1_SITE.csv",row.names=F)
# write.csv(wsd_t1, file="T:/Benthic/Data/Data Requests/2021 IEA/BenthicCover_2010-2020_Tier1_SITE.csv",row.names=F)


# Generate Site-level Data at TIER 3 level--------------
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
T3data.cols<-c(r_levels)
T3data.cols<-T3data.cols[!T3data.cols %in% c("WAND","UNK","TAPE","MOBF","SHAD")]

wsd<-merge(sites, photo, by=c("METHOD", "OBS_YEAR", "SITEVISITID"), all.y=T)

#Merge Tier 3 data with SURVEY MASTER data
wsd_t3<-merge(sm,wsd,by=c("SITEVISITID","SITE","OBS_YEAR"),all.y=TRUE)
head(wsd_t3)

test<-wsd_t3[is.na(wsd_t3$TRANSECT_PHOTOS),]
View(test) # none of the 2010 imagery has TRANSECT_PHOTOS assigned - ASK MICHAEL TO FIX
wsd_t3$TRANSECT_PHOTOS<-"-1" #make sure that all rows = -1


#Remove the unknowns and TWS columns
wsd_t3<-subset(wsd_t3,select= -c(WAND,UNK,TAPE,MOBF,SHAD))

#Remove sites that have less than 150 points after removing MF,UC, TW (a lot of our early imagery was poor quality and will be removed)
wsd_t3<-subset(wsd_t3,new.N >=150)

#Save Tier 1 site data to t drive. This file has all sites (fish, benthic and OCC) that were annoated between 2010 and 2018
write.csv(wsd_t3, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2020_Tier3_SITE.csv",row.names = F)


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

#Check analysis sector names & make sure number of sites match SURVEY master file
wsd<-droplevels(wsd)
levels(wsd$MISSIONID)
table(wsd$SEC_NAME, wsd$OBS_YEAR)

View(wsd)

## check whether we have ISLANDS that arent in the sectors file- should be 0
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
levels(wsd$ANALYSIS_YEAR)<-c(levels(wsd$ANALYSIS_YEAR),"2015-16")
wsd[wsd$REGION %in% c("MHI", "NWHI") & wsd$OBS_YEAR %in% seq(2010,2012),]$ANALYSIS_YEAR<-"2010-12"
wsd[wsd$REGION %in% c("MHI", "NWHI") & wsd$OBS_YEAR %in% seq(2013,2015),]$ANALYSIS_YEAR<-"2013-15"
wsd[wsd$REGION %in% c("SAMOA") & wsd$OBS_YEAR %in% seq(2015,2016),]$ANALYSIS_YEAR<-"2015-16"

#Change Analysis scheme in WSD from RAMP_BASIC to RAMP_CURRENT
wsd$ANALYSIS_SCHEME<-ifelse(wsd$ANALYSIS_SCHEME %in% c("RAMP_BASIC","AS_SANCTUARY"),"RAMP_CURRENT",wsd$ANALYSIS_SCHEME)


## generate a complete list of all ANALYSIS STRATA and their size
sectors$AREA_HA<-as.numeric(sectors$AREA_HA)
SCHEMES<-c("RAMP_CURRENT","MARI2011","MARI2014","TUT10_12")
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
SPATIAL_POOLING_BASE<-c("REGION","ISLAND", "ANALYSIS_SEC", "REEF_ZONE", "STRATA")    
ADDITIONAL_POOLING_BY<-c("ANALYSIS_YEAR")                                    # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)

#generate within strata means and vars
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
dps<-Calc_PerStrata(wsd, data.cols, c(POOLING_LEVEL, "AREA_HA"))

#SAVE BY STRATUM PER YEAR
###### REMOVE STRATA with N=1 (cannot pool those up)
dps$Mean<-dps$Mean[dps$Mean$N>1,]
dps$SampleVar<-dps$SampleVar[dps$SampleVar$N>1,]
dps$SampleSE<-dps$SampleSE[dps$SampleSE$N>1,]

dpst<-dps
colnames(dpst$Mean)[9:ncol(dpst$Mean)] <- paste("Mean.", colnames(dpst$Mean[,9:ncol(dpst$Mean)]), sep = "")
colnames(dpst$SampleSE)[9:ncol(dpst$SampleSE)] <- paste("SE.", colnames(dpst$SampleSE[,9:ncol(dpst$SampleSE)]), sep = "")

dpst<-left_join(dpst$Mean,dpst$SampleSE)
ri<-read.csv("T:/Benthic/Data/Lookup Tables/NCRMP_Regions_Islands.csv"); ri<-subset(ri,select = -c(X))
dpst<-left_join(dpst,ri)
#write.csv(dpst, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicCover_2010-2019_Tier1_STRATA.csv",row.names = F)
write.csv(dpst, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicCover_2010-2019_Tier3_STRATA.csv",row.names = F)

#SAVE BY SECTOR PER YEAR
OUTPUT_LEVEL<-c("REGION","ISLAND","ANALYSIS_SEC","ANALYSIS_YEAR") 
dpsec<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
#write.csv(dpsec, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicCover_2010-2019_Tier1_SECTOR.csv",row.names = F)
write.csv(dpsec, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicCover_2010-2019_Tier3_SECTOR.csv",row.names = F)


# e.g. SAVE BY ISLAND PER YEAR
OUTPUT_LEVEL<-c("REGION","ISLAND","ANALYSIS_YEAR") 
dpis<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
#write.csv(dpis, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicCover_2010-2019_Tier1_ISLAND.csv",row.names = F)
write.csv(dpis, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicCover_2010-2019_Tier3_ISLAND.csv",row.names = F)



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


