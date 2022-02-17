#This script generates data for the NCRMP Viztool
#It's a modification from Benthic_Cover_RawtoEstimates_v2
#Updates: 
#1. No longer combining all backreef depths and Lagoon depths into "Backreef_All" and "Lagoon_All"
#2. Adding script to calcualte regional estimates of cover



rm(list=ls())

setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/BIA")
# 
# library(gdata)             # needed for drop_levels()
# library(reshape)           # reshape library inclues the cast() function used below
# library(RODBC)            # to connect to oracle

#LOAD LIBRARY FUNCTIONS ... 
source("M:/Benthic Scripts_LEA/Benthic_Functions_newApp_vTAOfork.R")
source("M:/Benthic Scripts_LEA/core_functions.R")
source("M:/Benthic Scripts_LEA/fish_team_functions.R")
source("M:/Benthic Scripts_LEA/Islandwide Mean&Variance Functions.R")

#Climate data - this is from CPCE
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_BIA_CLIMATE_PERM.rdata")   #bia

cli$SITE<-SiteNumLeadingZeros(cli$SITE)

#BIA data - this is from CPCE
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_BIA_STR_RAW_NEW.rdata")   #bia

bia$SITE<-SiteNumLeadingZeros(bia$SITE)

#CNET data - from CoralNet
#These data contain human annotated data. There may be a small subset of robot annotated data. 
#The robot annoations are included because the confidence threshold in CoralNet was set to 90% allowing the robot to annotate points when it was 90% certain.
#2019 NWHI data not in these view because it was analyzed as part of a bleaching dataset
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_BIA_STR_CNET.rdata") #load data

cnet$SITE<-SiteNumLeadingZeros(cnet$SITE)
table(cnet$ISLAND,cnet$OBS_YEAR)


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

sm<-read.csv("M:/Benthic Scripts_LEA/SURVEY MASTER.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)

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
FIELDS_TO_RETAIN<-c("MISSIONID","METHOD", "REGION", "OBS_YEAR","ISLAND", "SITEVISITID","SITE", "LATITUDE", "LONGITUDE", "REEF_ZONE", "DEPTH_BIN", "PERM_SITE", "CLIMATE_STATION_YN", "MIN_DEPTH", "MAX_DEPTH", "HABITAT_CODE", "REP", "IMAGE_NAME", "PHOTOID", "ANALYST", "TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME", "POINTS")
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

ab$TIER_3<-ifelse(ab$TIER_3=="HALI","HAL",ab$TIER_3)
ab$TIER_1<-ifelse(ab$TIER_3=="HAL","HAL",ab$TIER_1)
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
sites<-ddply(ab,.(METHOD,REGION,OBS_YEAR,ISLAND,PERM_SITE,CLIMATE_STATION_YN,SITE,LATITUDE,LONGITUDE,REEF_ZONE,DEPTH_BIN),
             summarize,x=sum(POINTS))
sites$x<-NULL
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

#Add Reef Builder Ratio: BSR (Benthic Substrate Ratio)
photo$BSR<-(photo$CCA + photo$CORAL)/(photo$TURF+ photo$MA)

#Change Inf to NA
photo<-photo%>% mutate_if(is.numeric, ~ifelse(abs(.) == Inf,NA,.))

r_levels<-c(unique(as.character(ab$TIER_1)),"CCA_CORAL","BSR")
data.cols<-c(r_levels)
data.cols

#Calculate proportion
photo[,data.cols]<-(photo[,data.cols]/photo$new.N)*100
head(photo)

r_levels<-c(unique(as.character(ab$TIER_1)))
T1data.cols<-c(r_levels)
T1data.cols<-T1data.cols[!T1data.cols %in% c("TW","UC","MF")]


wsd<-merge(sites, photo, by=c("METHOD", "OBS_YEAR", "SITE"), all.y=T)

#Make sure that you have the correct # of sites/region and year
test1<-ddply(wsd,.(REGION,OBS_YEAR),summarize,nSite_wsd=length(unique(SITE)))

#check against original number of sites pulled from oracle
full_join(test1,oracle.site)

#Merge Tier 1 data with SURVEY MASTER FILE
#Remember this will have all the sites ever surveyed not just StRS sites
#You will need TRANSECT_PHOTOS, EXCLUDE FLAG and Oceanography from the SM file to be able to filter out OCC and Special Project sites

#sm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")

#Convert date formats
sm$DATE_<-mdy(sm$DATE_)
class(sm$DATE_)

head(sm)

sm<-sm[,c("DATE_","MISSIONID","SITEVISITID","SITE","OCC_SITEID","ANALYSIS_YEAR","OBS_YEAR","SEC_NAME","EXCLUDE_FLAG","TRANSECT_PHOTOS","Oceanography")]
wsd_t1<-merge(sm,wsd,by=c("SITEVISITID","SITE","OBS_YEAR"),all.y=TRUE)
head(wsd_t1)


#Are there NAs in TRANSECT PHOTOS
test<-wsd_t1[is.na(wsd_t1$TRANSECT_PHOTOS),]
View(test) # none of the 2010 imagery has TRANSECT_PHOTOS assigned - ASK MICHAEL TO FIX
wsd_t1$TRANSECT_PHOTOS<-"-1" #make sure that all rows = -1


#Remove the unknowns and TWS columns
wsd_t1<-subset(wsd_t1,select= -c(MF,UC,TW))

#Save Tier 1 site data to t drive. This file has all sites (fish, benthic and OCC) that were annoated between 2010 and 2018
write.csv(wsd_t1, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2020_Tier1_SITE.csv",row.names=F)


# CHECK THAT DATA IS READY FOR POOLING AND DO SOME FINAL CLEAN UPS --------

#Identify which taxonomic level you would like to summarize
wsd<-wsd_t1

#Define data columns
data.cols<-T1data.cols

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


# Final clean up before pooling -------------------------------------------
sectors<-read.csv("M:/Benthic Scripts_LEA/Sectors-Strata-Areas.csv")
seclu<-read.csv("T:/Benthic/Data/Lookup Tables/PacificNCRMP_Benthic_Sectors_Lookup_v3.csv") #list of SEC_NAME (smallest sector) and corresponding pooled sector scheme

#Merge site data with Sector look up table. This table indicates how sectors should be pooled or not
#For NCRMP viztool data- Keep pooling scheme the same across years
wsd<-left_join(wsd,seclu)
wsd<-left_join(wsd,sectors)


#Create Stata column
wsd$STRATA<-paste(substring(wsd$REEF_ZONE,1,1), substring(wsd$DEPTH_BIN,1,1), sep="")
sectors$STRATA<-paste(substring(sectors$REEF_ZONE,1,1), substring(sectors$DEPTH_BIN,1,1), sep="")



## TREAT GUGUAN, ALAMAGAN, SARIGAN AS ONE ISLAND  (REALLY ONE BASE REPORTING UNIT .. BUT SIMPLER TO STICK TO 'ISLAND')
SGA<-c("Guguan", "Alamagan", "Sarigan")
levels(wsd$ISLAND)<-c(levels(wsd$ISLAND), "Sarigan, Alamagan, Guguan")
levels(sectors$ISLAND)<-c(levels(sectors$ISLAND), "Sarigan, Alamagan, Guguan")
wsd[wsd$ISLAND %in% SGA,]$ISLAND<-"Sarigan, Alamagan, Guguan"
sectors[sectors$ISLAND %in% SGA,]$ISLAND<-"Sarigan, Alamagan, Guguan"
# 
# SGA<-c("Guguan", "Alamagan", "Sarigan")
# levels(wsd$SEC_NAME)<-c(levels(wsd$SEC_NAME), "SGA")
# levels(sectors$SEC_NAME)<-c(levels(sectors$SEC_NAME), "SGA")
# wsd[wsd$SEC_NAME %in% SGA,]$SEC_NAME<-"SGA"
# sectors[sectors$SEC_NAME %in% SGA,]$SEC_NAME<-"SGA"


#Remove NWHI islands only surveyed by PMNM
remove<-c("Laysan","Maro","Midway")
wsd<-dplyr::filter(wsd, !PooledSector_Cover_Viztool %in% remove)

#Remove PRIA 2016 and 2017 surveys- done off cycle for the bleaching response, and do not have all metrics
wsd$REGION_YEAR<-paste(wsd$REGION,wsd$ANALYSIS_YEAR,sep = "_")
wsd$REGION_YEAR<-ifelse((wsd$ISLAND=="Wake" & wsd$ANALYSIS_YEAR=="2017"),"PRIAs_2017w",wsd$REGION_YEAR) #This will help you keep wake 2017 data

remove<-c("PRIAs_2016","PRIAs_2017")
wsd<-dplyr::filter(wsd, !REGION_YEAR %in% remove)


#Change Analysis year according to desired pooling
wsd[is.na(wsd$ANALYSIS_YEAR),]
levels(wsd$ANALYSIS_YEAR)<-c(levels(wsd$ANALYSIS_YEAR),"2015-16","2014-15","2017-18")
wsd[wsd$REGION %in% c("MHI", "NWHI") & wsd$OBS_YEAR %in% seq(2010,2012),]$ANALYSIS_YEAR<-"2010-12"
wsd[wsd$REGION %in% c("MHI", "NWHI") & wsd$OBS_YEAR %in% seq(2013,2015),]$ANALYSIS_YEAR<-"2013-15"
wsd[wsd$REGION %in% c("SAMOA") & wsd$OBS_YEAR %in% seq(2015,2016),]$ANALYSIS_YEAR<-"2015-16"
wsd[wsd$REGION %in% c("PRIAs") & wsd$OBS_YEAR %in% seq(2014,2015),]$ANALYSIS_YEAR<-"2014-15"
wsd[wsd$REGION %in% c("PRIAs") & wsd$OBS_YEAR %in% seq(2017,2018),]$ANALYSIS_YEAR<-"2017-18"

#Define Analysis Sector-some sectors are pooled together
wsd$ANALYSIS_SEC<-wsd$PooledSector_Cover_Viztool

sectors$AREA_HA<-as.numeric(sectors$AREA_HA)

#NOW CHECK HOW MANY REPS WE HAVE PER STRATA
a<-dcast(wsd, ISLAND + ANALYSIS_SEC + OBS_YEAR ~ STRATA, value.var="AREA_HA", length); a

####################################################################################################################################################################
#
#     POOL WSD (WORKING SITE DATA TO STRATA THEN TO HIGHER LEVELS
##
###################################################################################################################################################################
#Save site level data - need to send site, lat and long to Viztool
write.csv(wsd,"T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/2022ViztoolSites_Cover.csv")


#Some sectors are pooled together, this will ensure that strata area is pooled correctly
area.tmp<-ddply(wsd,.(REGION,ISLAND,ANALYSIS_YEAR,ANALYSIS_SEC,STRATA,AREA_HA),summarize,temp=sum(AREA_HA,na.rm=TRUE)) #calculate # of possible sites in a given stratum
new.area<-ddply(area.tmp,.(REGION,ISLAND,ANALYSIS_YEAR,ANALYSIS_SEC,STRATA),summarize,AREA_HA_correct=sum(AREA_HA,na.rm=TRUE)) #calculate # of possible sites in a given stratum

wsd<-left_join(wsd,new.area)

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
ri<-read.csv("T:/Benthic/Data/Lookup Tables/NCRMP_Regions_Islands.csv")
dpst<-left_join(dpst,ri)
write.csv(dpst, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicCover_2010-2019_Tier1_STRATA_v2.csv",row.names = F)

#SAVE BY SECTOR PER YEAR
OUTPUT_LEVEL<-c("REGION","ISLAND","ANALYSIS_SEC","ANALYSIS_YEAR") 
dpsec<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
colnames(dpsec$Mean)[6:ncol(dpsec$Mean)] <- paste("Mean.", colnames(dpsec$Mean[,6:ncol(dpsec$Mean)]), sep = "")
colnames(dpsec$PooledSE)[6:ncol(dpsec$PooledSE)] <- paste("SE.", colnames(dpsec$PooledSE[,6:ncol(dpsec$PooledSE)]), sep = "")


dpsec<-left_join(dpsec$Mean,dpsec$PooledSE)
dpsec<-left_join(dpsec,ri)

write.csv(dpsec, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicCover_2010-2019_Tier1_SECTOR_v2.csv",row.names = F)


# e.g. SAVE BY ISLAND PER YEAR
OUTPUT_LEVEL<-c("REGION","ISLAND","ANALYSIS_YEAR") 
dpis<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
colnames(dpis$Mean)[5:ncol(dpis$Mean)] <- paste("Mean.", colnames(dpis$Mean[,5:ncol(dpis$Mean)]), sep = "")
colnames(dpis$PooledSE)[5:ncol(dpis$PooledSE)] <- paste("SE.", colnames(dpis$PooledSE[,5:ncol(dpis$PooledSE)]), sep = "")


dpis<-left_join(dpis$Mean,dpis$PooledSE)
dpis<-left_join(dpis,ri)

write.csv(dpis, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicCover_2010-2019_Tier1_ISLAND_v2.csv",row.names = F)


# e.g. SAVE BY REGION PER YEAR
OUTPUT_LEVEL<-c("REGION","ANALYSIS_YEAR") 
dpr<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
colnames(dpr$Mean)[4:ncol(dpr$Mean)] <- paste("Mean.", colnames(dpr$Mean[,4:ncol(dpr$Mean)]), sep = "")
colnames(dpr$PooledSE)[4:ncol(dpr$PooledSE)] <- paste("SE.", colnames(dpr$PooledSE[,4:ncol(dpr$PooledSE)]), sep = "")

dpr<-left_join(dpr$Mean,dpr$PooledSE)
write.csv(dpr, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Region/BenthicCover_2010-2019_Tier1_Region_v2.csv",row.names = F)



