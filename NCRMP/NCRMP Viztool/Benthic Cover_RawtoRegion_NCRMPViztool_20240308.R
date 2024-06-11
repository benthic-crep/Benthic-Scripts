#This script generates data for the NCRMP Viztool
#It's a modification from Benthic_Cover_RawtoEstimates_v2
#This script reads in the raw point level data and generates site, strata, sector, island and regional roll ups at the Tier 1 and Tier 2b (genus) level
#Updates: 
#1. No longer combining all backreef depths and Lagoon depths into "Backreef_All" and "Lagoon_All"
#2. Adding script to calculate regional estimates of cover

#Clear out memory and packages
rm(list=ls())
if(!is.null(names(sessionInfo()$otherPkgs))){
  invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
}
#Set WRITE Booleans
WRITE=TRUE
WRITE_EVEN_RIDICULOUS_STUFF=FALSE

#LOAD LIBRARY FUNCTIONS ... 
library(lubridate)
source("./Functions/Benthic_Functions_newApp_vTAOfork.R")
source("../fish-paste/lib/core_functions.R")
source("../fish-paste/lib/fish_team_functions.R")
source("../fish-paste/lib/Islandwide Mean&Variance Functions.R")
library(dplyr)#to avert plyr/dplyr issues
##############  Add generalized function to move from point-level classification and label set, outputting wide table of proportions.

#Climate data - this is from CPCE
#Will load "cli"
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_BIA_CLIMATE_PERM.rdata")   #bia
cli$SITE<-SiteNumLeadingZeros(cli$SITE)

#BIA data - this is from CPCE
#Will load "bia"
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_BIA_STR_RAW_NEW.rdata")   #bia
bia$SITE<-SiteNumLeadingZeros(bia$SITE)

#CNET data - from CoralNet
#These data contain human annotated data. There may be a small subset of robot annotated data. 
#The robot annoations are included because the confidence threshold in CoralNet was set to 75-90% allowing the robot to annotate points when it was 90% certain.
#2019 NWHI data not in these view because it was analyzed as part of a bleaching dataset
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_CNET_Annotations.rdata") #load data
#Will load "cnet", need to make sure SITE is a factor
cnet$SITE=factor(cnet$SITE)
cnet$SITE<-SiteNumLeadingZeros(cnet$SITE)


##Generate Table of all the bia categories to review
bia_tab<-ddply(bia,.(TIER_1, CATEGORY_NAME, TIER_2, SUBCATEGORY_NAME, TIER_3, GENERA_NAME),summarize,count=sum(POINTS))
if(WRITE){write.csv(bia_tab, file="BIA categories.csv")}

##Generate Table of all the bia categories to review
cnet_tab<-ddply(cnet,.(CATEGORY_CODE,CATEGORY_NAME,SUBCATEGORY_CODE,SUBCATEGORY_NAME,GENERA_CODE,GENERA_NAME,FUNCTIONAL_GROUP),summarize,count=length(ROUNDID))
if(WRITE){write.csv(cnet_tab, file="CNET categories.csv")}

sm_all<-read.csv("../fish-paste/data/SURVEY MASTER.csv")
sm=sm_all
sm$SITE=factor(sm$SITE)#make it a factor
sm$SITE<-SiteNumLeadingZeros(sm$SITE)

#####SOME DATE FORMAT FAIL REWRITE - Re-written TAO 3/8/2024#####
#Convert date formats
sm$DATE_.<-lubridate::mdy(sm$DATE_)
sm$DATE_.[is.na(sm$DATE_.)]=mdy_hms(sm$DATE_[is.na(sm$DATE_.)])
#which(is.na(sm$DATE_.))
sm$DATE_=sm$DATE_.
sm<-sm[,c("DATE_","MISSIONID","SITEVISITID","SITE","OCC_SITEID","ANALYSIS_YEAR","OBS_YEAR","SEC_NAME","EXCLUDE_FLAG","TRANSECT_PHOTOS","Oceanography","LATITUDE_LOV","LONGITUDE_LOV")]

test<-subset(sm,OBS_YEAR=="2019"&TRANSECT_PHOTOS=="-1");nrow(test)

# Merge together all Photoquad Datasets & make sure columns match ---------------------------------------
bia$METHOD<-"CPCE"
cli$METHOD<-"CPCE"
#bia$FUNCTIONAL_GROUP<-"BIA"    #ACTUALLY FUNCTIONAL_GROUP SEEMS A BIT MIXED UP ... FROM QUICK LOOK AT CNET FILE, IT CAN TAKE DIFFERENT VALUES FOR SAME CODES (eg ALGAE or Hare Substrate) - SO GOING TO IGNORE IT!

#Make cnet play nice
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

#add NWHI Special Cruise Data 2014,2015,2019
nw=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Raw Data from CoralNet/2014-2017_NWHI_CnetAnnotations_formatted.csv")
nw$SITE=factor(nw$SITE)
nw$SITE<-SiteNumLeadingZeros(nw$SITE)
nw$POINTS<-1
nw$METHOD<-"CNET"
nw$REP<-nw$REPLICATE
nw$IMAGE_NAME<-nw$ORIGINAL_FILE_NAME
nw$PHOTOID<-nw$IMAGE_NUMBER
nw$TIER_1<-nw$CATEGORY_CODE
nw$TIER_2<-nw$SUBCATEGORY_CODE
nw$TIER_3<-nw$GENERA_CODE

n457=nw[,FIELDS_TO_RETAIN]

ab<-rbind(x,y,z,n457)

#Add Tier 2b (genus for corals, order for macroalgae)
codes_lu<-read.csv("T:/Benthic/Data/Lookup Tables/All_Photoquad_codes.csv")
codes_lu<-codes_lu[,c("T2b_DESC","TIER_2b","CODE")];colnames(codes_lu)[which(names(codes_lu) =="CODE")]<-"TIER_3"
ab<-left_join(ab,codes_lu)

#Flag sites that have more than 33 and less than 15 images
#With the exception of OCC 2012 sites, there should be 30 images/site/10 points/image
test<-ddply(ab,.(OBS_YEAR,SITEVISITID,SITE),summarize,count=sum(POINTS))
test2<-test[test$count<150 |test$count>330,]
#View(test2)
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
table(sm_all$REGION,sm_all$OBS)
ab.site<-ddply(subset(cnet,OBS_YEAR=="2019"),.(REGION,OBS_YEAR),summarize,nSite=length(unique(SITE)));ab.site

#identify which new sites are in the CoralNet data, but still need to be integrated into the SURVEY MASTER file
miss.from.sm<-cnet[!(cnet$SITEVISITID %in% sm$SITEVISITID),]
miss.from.smSITE<-ddply(miss.from.sm,.(SITEVISITID,REGION,ISLAND,SITE,REEF_ZONE,DEPTH_BIN,ROUNDID,MISSIONID,OBS_YEAR, DATE_,HABITAT_CODE,
                                       LATITUDE,LONGITUDE,MIN_DEPTH,MAX_DEPTH),summarize,tmp=length(REPLICATE));miss.from.smSITE

if(WRITE){write.csv(miss.from.smSITE,file="../fish-paste/data/03082024_SitesmissingfromSM.csv")} #export list and manually add to SM

#Don't commit this file, too big!!!
if(WRITE_EVEN_RIDICULOUS_STUFF){write.csv(ab, file="tmp All BIA BOTH METHODS.csv")}

SURVEY_INFO<-c("OBS_YEAR", "REGION",  "ISLAND")
survey_island<-Aggregate_InputTable(cnet, SURVEY_INFO)

#There are some missing Tier3 information for pre 2013 data. If these data are missing then fill it with tier2 code
ab$TIER_2<-ifelse(ab$TIER_2=="HAL","HALI",as.character(ab$TIER_2)) #change to match the Tier 3 halimeda code

ab<-ab %>% dplyr::mutate(TIER_3=coalesce(TIER_3,TIER_2))
ab<-ab %>% dplyr::mutate(GENERA_NAME=coalesce(GENERA_NAME,SUBCATEGORY_NAME))
ab<-ab %>% dplyr::mutate(TIER_2b=coalesce(TIER_2b,TIER_2))
ab<-ab %>% dplyr::mutate(T2b_DESC=coalesce(T2b_DESC,SUBCATEGORY_NAME))

head(ab)

# Drop Swains CPCE 2010 and 2012 (we reanalyzed these using CNET in 2023) #TAO 3/8/24--------------------------------------------
sw10.12i=which(ab$ISLAND=="Swains"&ab$OBS_YEAR%in%c(2010,2012)&ab$METHOD=="CPCE")
ab=ab[-sw10.12i,]

# Reclassify EMA and Halimeda --------------------------------------------

#CREATING CLASS EMA "Encrusting Macroalgae
levels(ab$TIER_1)<-c(levels(ab$TIER_1), "EMA")
levels(ab$CATEGORY_NAME)<-c(levels(ab$CATEGORY_NAME), "Encrusting macroalga")
ab[ab$GENERA_NAME %in% c("Lobophora sp","Peyssonnelia sp", "Encrusting macroalga"),]$TIER_1<-"EMA"
ab[ab$GENERA_NAME %in% c("Lobophora sp","Peyssonnelia sp", "Encrusting macroalga"),]$TIER_2<-"EMA"
ab[ab$GENERA_NAME %in% c("Lobophora sp","Peyssonnelia sp", "Encrusting macroalga"),]$SUBCATEGORY_NAME<-"Encrusting macroalga"
ab[ab$GENERA_NAME %in% c("Lobophora sp","Peyssonnelia sp", "Encrusting macroalga"),]$CATEGORY_NAME<-"Encrusting macroalga"

###Create a Halimeda class
ab$TIER_3<-ifelse(ab$TIER_3=="HALI","HALI",as.character(ab$TIER_3))
ab$TIER_1<-ifelse(ab$TIER_3=="HALI","HALI",as.character(ab$TIER_1))
ab$CATEGORY_NAME<-ifelse(ab$TIER_3=="HALI","Halimeda sp",as.character(ab$CATEGORY_NAME))
hal<-subset(ab,TIER_1=="HALI")
head(hal)

#Sampling Effort Check
test<-ddply(ab,.(REGION,OBS_YEAR),summarize,nSite=length(unique(SITE)))
test %>% ggplot(aes(OBS_YEAR,nSite))+geom_point()+geom_line()+stat_smooth(method="loess",span=3,se = FALSE)+facet_grid(REGION~.)

#We are missing depth bin, reef zone and habitat_code information from some sites.
#This information is also missing from the SURVEY MASTER file

levels(ab$DEPTH_BIN)<-c(levels(ab$DEPTH_BIN), "UNKNOWN")
levels(ab$REEF_ZONE)<-c(levels(ab$REEF_ZONE), "UNKNOWN")
levels(ab$HABITAT_CODE)<-c(levels(ab$HABITAT_CODE), "UNKNOWN")

ab[is.na(ab$DEPTH_BIN),]$DEPTH_BIN<-"UNKNOWN"
ab[is.na(ab$REEF_ZONE),]$REEF_ZONE<-"UNKNOWN"
ab[is.na(ab$HABITAT_CODE),]$HABITAT_CODE<-"UNKNOWN"

##### UPDATE IN ORACLE?

#We have some likely misidentified points in the Hawaiian Island (e.g. Acanthastrea in the MHI).
#Read in list of hawaii codes and change everything that isn't in the list to UNKN
hawaiicodes<-read.csv("T:/Benthic/Data/Lookup Tables/Hawaii_Photoquad_codes.csv")

ab$TIER_1<-ifelse(ab$REGION %in% c("MHI","NWHI") & !(ab$TIER_3 %in% hawaiicodes$TIER_3),"UC",ab$TIER_1)
ab$CATEGORY_NAME<-ifelse(ab$REGION %in% c("MHI","NWHI") & !(ab$TIER_3 %in% hawaiicodes$TIER_3),"Unclassified",ab$CATEGORY_NAME)
ab$TIER_2<-ifelse(ab$REGION %in% c("MHI","NWHI") & !(ab$TIER_3 %in% hawaiicodes$TIER_3),"UNK",ab$TIER_2)
ab$SUBCATEGORY_NAME<-ifelse(ab$REGION %in% c("MHI","NWHI") & !(ab$TIER_3 %in% hawaiicodes$TIER_3),"Unclassified/Unknown",ab$SUBCATEGORY_NAME)
ab$TIER_2b<-ifelse(ab$REGION %in% c("MHI","NWHI") & !(ab$TIER_3 %in% hawaiicodes$TIER_3),"Unclassified/Unknown",ab$TIER_2b)
ab$T2b_DESC<-ifelse(ab$REGION %in% c("MHI","NWHI") & !(ab$TIER_3 %in% hawaiicodes$TIER_3),"UC",ab$T2b_DESC)
ab$TIER_3<-ifelse(ab$REGION %in% c("MHI","NWHI") & !(ab$TIER_3 %in% hawaiicodes$TIER_3),"UNK",ab$TIER_3)
ab$GENERA_NAME<-ifelse(ab$REGION %in% c("MHI","NWHI") & !(ab$TIER_3 %in% hawaiicodes$TIER_3),"Unclassified/Unknown",ab$GENERA_NAME)

#LEPT and LPHY ==> LEPT
#ALSP and GOSP ==> GOSP
#ASTS and MONS ==> ASTS
ab$TIER_3[which(ab$TIER_3=="LPHY")]="LEPS"
ab$TIER_3[which(ab$TIER_3=="ALSP")]="GOSP"
ab$TIER_3[which(ab$TIER_3=="MONS")]="ASTS"
ab$TIER_2b[which(ab$TIER_2b=="LPHY")]="LEPS"
ab$TIER_2b[which(ab$TIER_2b=="ALSP")]="GOSP"
ab$TIER_2b[which(ab$TIER_2b=="MONS")]="ASTS"

#####


#### WORKING WITH CLEAN DATA FILE AT THIS POINT  
ab<-droplevels(ab)
if(WRITE){save(ab, file="T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/BIA_2010-2023_CLEANED.RData")}
#ablist=load("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/BIA_2010-2023_CLEANED.RData")

#Generate a SITE table- excluding lat and long for now because there are issues that we need to address with Michael- use lat and long from SM below
sites<-ddply(ab,.(METHOD,REGION,OBS_YEAR,ISLAND,PERM_SITE,CLIMATE_STATION_YN,SITE,REEF_ZONE,DEPTH_BIN),
             summarize,x=sum(POINTS))
sites$x<-NULL
dim(sites)

########################################################################################################################################
# Annotated Point (ab) Data are now clean...
# Generate Site-level Data at TIER 1 level--------------
########################################################################################################################################

#####DCAST REWRITE - Re-written TAO 3/7/2024#####
levels(ab$TIER_1)
#photo.<-dcast(ab, formula=METHOD + OBS_YEAR + SITEVISITID + SITE  ~ TIER_1, value.var="POINTS", sum, fill=0)
photo=ab %>% group_by(METHOD,OBS_YEAR,SITEVISITID,SITE,TIER_1) %>%
  dplyr::summarize(POINTS=sum(POINTS)) %>% 
  dplyr::arrange(TIER_1) %>% 
  pivot_wider(names_from = TIER_1,values_from = POINTS,values_fill = 0) %>% 
  dplyr::arrange(METHOD,OBS_YEAR,SITEVISITID,SITE) 
#all(photo==photo.)

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

#####MERGE REWRITE - Re-written TAO 3/8/2024#####
#wsd.<-merge(sites, photo, by=c("METHOD", "OBS_YEAR", "SITE"), all.y=T)
wsd<-left_join(photo, sites, by=c("METHOD", "OBS_YEAR", "SITE"))
# wsd.=wsd.[,names(wsd)]
# wsd=arrange(wsd,SITEVISITID,METHOD,OBS_YEAR,SITE)
# wsd.=arrange(wsd.,SITEVISITID,METHOD,OBS_YEAR,SITE)
# dim(wsd);dim(wsd.)
# head(wsd);head(wsd.)
# which(!apply(wsd==wsd.,1,all))
# (apply(wsd==wsd.,2,all))
# wsd==wsd.
# plot(wsd$SITEVISITID,wsd.$SITEVISITID)

#Make sure that you have the correct # of sites/region and year
test1<-ddply(wsd,.(REGION,OBS_YEAR),summarize,nSite_wsd=length(unique(SITE)))

#check against original number of sites pulled from oracle
full_join(test1,oracle.site)

#Merge Tier 1 data with SURVEY MASTER FILE
#Remember this will have all the sites ever surveyed not just StRS sites
#You will need TRANSECT_PHOTOS, EXCLUDE FLAG and Oceanography from the SM file to be able to filter out OCC and Special Project sites

#####MERGE REWRITE  - Re-written TAO 3/8/2024#####
#wsd_t1.<-merge(sm,wsd,by=c("SITEVISITID","SITE","OBS_YEAR"),all.y=TRUE)
wsd_t1<-left_join(wsd,sm,by=c("SITEVISITID","SITE","OBS_YEAR"))
# wsd_t1.=wsd_t1.[,names(wsd_t1)]
# wsd_t1=arrange(wsd_t1,SITEVISITID,METHOD,OBS_YEAR,SITE)
# wsd_t1.=arrange(wsd_t1.,SITEVISITID,METHOD,OBS_YEAR,SITE)
# dim(wsd_t1.);dim(wsd_t1)
# dim(wsd);dim(sm)
# head(wsd_t1.);head(wsd_t1)
# apply(wsd_t1.==wsd_t1,2,all)

##############  Add generalized function to move from point-level classification and label set, outputting wide table of proportions.

#Are there NAs in TRANSECT PHOTOS
test<-wsd_t1[is.na(wsd_t1$TRANSECT_PHOTOS),]
#View(test) # none of the 2010 imagery has TRANSECT_PHOTOS assigned - ASK MICHAEL TO FIX
wsd_t1$TRANSECT_PHOTOS<-"-1" #make sure that all rows = -1


#Remove the unknowns and TWS columns
wsd_t1<-subset(wsd_t1,select= -c(MF,UC,TW))

wsd_t1[which(is.na(wsd_t1$SITEVISITID)),]
#Save Tier 1 site data to t drive. This file has all sites (fish, benthic and OCC) that were annotated between 2010 and 2023
if(WRITE){write.csv(wsd_t1, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2023_Tier1_SITE.csv",row.names=F)}


########################################################################################################################################
# Annotated Point (ab) Data are now clean...
# Generate Site-level Data at TIER 2b (Genus/Class) level - Re-written TAO 3/8/2024#####--------------
########################################################################################################################################
#photo.<-dcast(ab, formula=METHOD + OBS_YEAR + SITEVISITID + SITE  ~ TIER_2b, value.var="POINTS", sum, fill=0)
photo=ab %>% group_by(METHOD,OBS_YEAR,SITEVISITID,SITE,TIER_2b) %>%
  dplyr::summarize(POINTS=sum(POINTS)) %>% 
  dplyr::arrange(TIER_2b) %>% 
  pivot_wider(names_from = TIER_2b,values_from = POINTS,values_fill = 0) %>% 
  dplyr::arrange(METHOD,OBS_YEAR,SITEVISITID,SITE) 
# photo=photo[,names(photo.)]
# head(photo.);head(as.data.frame(photo));
# wii=which(photo!=photo.,arr.ind = T)
# table(wii[,2])
#all(photo==photo.)

head(photo)

r_levels<-c(unique(as.character(ab$TIER_2b)))
photo$N<-rowSums(photo[,r_levels])

#Subtract mobile inverts and tape wand shallow and unclassified
photo$new.N<-photo$N-(photo$WAND+photo$UNK+photo$TAPE+photo$MOBF+photo$SHAD)

#Change Inf to NA
photo<-photo%>% mutate_if(is.numeric, ~ifelse(abs(.) == Inf,NA,.))

r_levels<-c(unique(as.character(ab$TIER_2b)))
data.cols<-c(r_levels)
data.cols

#Calculate proportion
photo[,data.cols]<-(photo[,data.cols]/photo$new.N)*100
head(photo)

r_levels<-c(unique(as.character(ab$TIER_2b)))
T2data.cols<-c(r_levels)
T2data.cols<-T2data.cols[!T2data.cols %in% c("WAND","UNK","TAPE","MOBF","SHAD")]


#####MERGE REWRITE - Re-written TAO 3/8/2024#####
#wsd.<-merge(sites, photo, by=c("METHOD", "OBS_YEAR", "SITE"), all.y=T)
wsd<-inner_join(sites, photo, by=c("METHOD", "OBS_YEAR", "SITE"))
#dim(wsd);dim(wsd.)
#Make sure that you have the correct # of sites/region and year
test1<-ddply(wsd,.(REGION,OBS_YEAR),summarize,nSite_wsd=length(unique(SITE)))

#check against original number of sites pulled from oracle
full_join(test1,oracle.site)

#Merge Tier 2b data with SURVEY MASTER FILE
#Remember this will have all the sites ever surveyed not just StRS sites
#You will need TRANSECT_PHOTOS, EXCLUDE FLAG and Oceanography from the SM file to be able to filter out OCC and Special Project sites
#wsd_t2.<-merge(sm,wsd,by=c("SITEVISITID","SITE","OBS_YEAR"),all.y=TRUE)
wsd_t2<-left_join(wsd,sm,by=c("SITEVISITID","SITE","OBS_YEAR"))
# wsd_t2.=wsd_t2.[,names(wsd_t2)]
# wsd_t2=arrange(wsd_t2,SITEVISITID,METHOD,OBS_YEAR,SITE)
# wsd_t2.=arrange(wsd_t2.,SITEVISITID,METHOD,OBS_YEAR,SITE)
# dim(wsd_t2.);dim(wsd_t2)
# dim(wsd);dim(sm)
# head(wsd_t2.);head(wsd_t2)
# apply(wsd_t2.==wsd_t2,2,all)
head(wsd_t2)


#Are there NAs in TRANSECT PHOTOS
test<-wsd_t2[is.na(wsd_t2$TRANSECT_PHOTOS),]
#View(test) # none of the 2010 imagery has TRANSECT_PHOTOS assigned - ASK MICHAEL TO FIX
wsd_t2$TRANSECT_PHOTOS<-"-1" #make sure that all rows = -1


#Remove the unknowns and TWS columns
wsd_t2<-subset(wsd_t2,select= -c(WAND,UNK,TAPE,MOBF,SHAD))

#Save Tier 1 site data to t drive. This file has all sites (fish, benthic and OCC) that were annoated between 2010 and 2018
if(WRITE){write.csv(wsd_t2, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2023_Tier2b_SITE.csv",row.names=F)}


########################################################################################################################################
#Keep just the coral genera columns and metadata
taxa.cols<-wsd_t2 %>% dplyr::select(ACAS:ZO)
coral.genera<-read.csv("T:/Benthic/Data/Lookup Tables/Genus_lookup.csv")
coral.genera<-coral.genera %>% dplyr::filter(!SPCODE %in%c("AAAA","SSSS","WIRE","CALG","ZOSP","ZOPA","UNKN","TUBA"))
gen<-coral.genera$GENUS_CODE
gen<-unique(gen)
#gen<-c(gen,"COL","MASS","BR","ENC","FOL","FREE","TAB") #not including general morphology groups 
taxa.cols<-taxa.cols[names(taxa.cols) %in% gen] #only include the list of genera we want
wsd_t2<-dplyr::select(wsd_t2,-c(ACAS:ZO)) #remove original data columns
wsd_t2<-cbind(wsd_t2,taxa.cols) #combine metadata and subsetted taxa
wsd_t2<-filter(wsd_t2,OBS_YEAR>=2013) # drop sites before 2013- corals only identified to morphological group and algae only to CCA, UPMA, EMA and BGMA

########################################################################################################################################
# 
# Generate Site-level Data at TIER 3 level - Re-written TAO 3/8/2024#####--------------
########################################################################################################################################
#photo.<-dcast(ab, formula=METHOD + OBS_YEAR + SITEVISITID + SITE  ~ TIER_3, value.var="POINTS", sum, fill=0)
photo=ab %>% group_by(METHOD,OBS_YEAR,SITEVISITID,SITE,TIER_3) %>%
  dplyr::summarize(POINTS=sum(POINTS)) %>% 
  dplyr::arrange(TIER_3) %>% 
  pivot_wider(names_from = TIER_3,values_from = POINTS,values_fill = 0) %>% 
  dplyr::arrange(METHOD,OBS_YEAR,SITEVISITID,SITE) 
# photo=photo[,names(photo.)]
# head(photo.);head(as.data.frame(photo));
# wii=which(photo!=photo.,arr.ind = T)
# table(wii[,2])
#all(photo==photo.)
head(photo)

r_levels<-c(unique(as.character(ab$TIER_3)))
photo$N<-rowSums(photo[,r_levels])

#Subtract mobile inverts and tape wand shallow and unclassified
photo$new.N<-photo$N-(photo$WAND+photo$UNK+photo$TAPE+photo$MOBF+photo$SHAD)

#Change Inf to NA
photo<-photo%>% mutate_if(is.numeric, ~ifelse(abs(.) == Inf,NA,.))

r_levels<-c(unique(as.character(ab$TIER_3)))
data.cols<-c(r_levels)
data.cols

#Calculate proportion
photo[,data.cols]<-(photo[,data.cols]/photo$new.N)*100
head(photo)

r_levels<-c(unique(as.character(ab$TIER_3)))
T3data.cols<-c(r_levels)
T3data.cols<-T3data.cols[!T3data.cols %in% c("WAND","UNK","TAPE","MOBF","SHAD")]


#####MERGE REWRITE - Re-written TAO 3/8/2024#####
#wsd.<-merge(sites, photo, by=c("METHOD", "OBS_YEAR", "SITE"), all.y=T)
wsd<-inner_join(sites, photo, by=c("METHOD", "OBS_YEAR", "SITE"))
#dim(wsd);dim(wsd.)
#Make sure that you have the correct # of sites/region and year
test1<-ddply(wsd,.(REGION,OBS_YEAR),summarize,nSite_wsd=length(unique(SITE)))

#check against original number of sites pulled from oracle
full_join(test1,oracle.site)

#Merge Tier 2b data with SURVEY MASTER FILE
#Remember this will have all the sites ever surveyed not just StRS sites
#You will need TRANSECT_PHOTOS, EXCLUDE FLAG and Oceanography from the SM file to be able to filter out OCC and Special Project sites
#wsd_t3.<-merge(sm,wsd,by=c("SITEVISITID","SITE","OBS_YEAR"),all.y=TRUE)
wsd_t3<-left_join(wsd,sm,by=c("SITEVISITID","SITE","OBS_YEAR"))
# wsd_t3.=wsd_t3.[,names(wsd_t3)]
# wsd_t3=arrange(wsd_t3,SITEVISITID,METHOD,OBS_YEAR,SITE)
# wsd_t3.=arrange(wsd_t3.,SITEVISITID,METHOD,OBS_YEAR,SITE)
# dim(wsd_t3.);dim(wsd_t3)
# dim(wsd);dim(sm)
# head(wsd_t3.);head(wsd_t3)
# apply(wsd_t3.==wsd_t3,2,all)
head(wsd_t3)


#Are there NAs in TRANSECT PHOTOS
test<-wsd_t3[is.na(wsd_t3$TRANSECT_PHOTOS),]
#View(test) # none of the 2010 imagery has TRANSECT_PHOTOS assigned - ASK MICHAEL TO FIX
wsd_t3$TRANSECT_PHOTOS<-"-1" #make sure that all rows = -1


#Remove the unknowns and TWS columns
wsd_t3<-subset(wsd_t3,select= -c(WAND,UNK,TAPE,MOBF,SHAD))

#Save Tier 1 site data to t drive. This file has all sites (fish, benthic and OCC) that were annoated between 2010 and 2018
if(WRITE){write.csv(wsd_t3, file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2023_Tier3_SITE.csv",row.names=F)}

########################################################################################################################################
#wsd_t1 %>% group_by(REGION,ISLAND,ANALYSIS_YEAR) %>% dplyr::summarize(N=length(N)) %>% filter(REGION %in% c("NWHI")) %>% print(n=9999)


# CHECK THAT DATA IS READY FOR POOLING AND DO SOME FINAL CLEAN UPS --------

#Identify which taxonomic level you would like to summarize
#WRITE=TRUE
for (TIER in 1:2){#TIER=1#2#2#
#TIER=2  
  if(TIER==1){
    wsd<-wsd_t1
  }else if(TIER==2){
    wsd<-wsd_t2
  }else{
    wsd<-wsd_t3
  }
  
  #Define data columns
  if(TIER==1){
    data.cols<-T1data.cols
  }else if(TIER==2){
    data.cols<-T2data.cols#colnames(taxa.cols)
  }else{
    data.cols<-T3data.cols
  }
  
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
  
  #View(wsd)
  
  
  # Final clean up before pooling -------------------------------------------
  sectors<-read.csv("../fish-paste/data/Sectors-Strata-Areas.csv")
  seclu<-read.csv("T:/Benthic/Data/Lookup Tables/PacificNCRMP_Benthic_Sectors_Lookup_v4.csv") #list of SEC_NAME (smallest sector) and corresponding pooled sector scheme
  
  ## check whether we have ISLANDS that arent in the sectors file- should be 0
  setdiff(unique(wsd$ISLAND),unique(sectors$ISLAND))
  
  
  #Merge site data with Sector look up table. This table indicates how sectors should be pooled or not
  #For NCRMP viztool data- Keep pooling scheme the same across years
  wsd<-left_join(wsd,seclu)
  wsd<-left_join(wsd,sectors)
  
  
  #Create Strata column
  wsd$STRATA<-paste(substring(wsd$REEF_ZONE,1,1), substring(wsd$DEPTH_BIN,1,1), sep="")
  wsd$STRATANAME_TRENDS<-paste(wsd$PooledSector_Viztool,wsd$REEF_ZONE,wsd$DEPTH_BIN,sep="_")
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
  
  
  #Remove NWHI islands only surveyed by PMNM -  Don't remove these anymore (March 15 2024, TAO CSC); We will still remove these.
 remove<-c("Laysan","Maro","Midway")
 wsd<-dplyr::filter(wsd, !PooledSector_Viztool %in% remove)
  
  #Separating Guam and CNMI in MARIAN for Viztool
  wsd <- wsd %>% mutate(wsd,
                        REGION= case_when(
                          ISLAND =="Guam" ~ "GUA",
                          REGION == "MARIAN" & ISLAND !="Guam" ~ "CNMI",
                          TRUE ~ REGION))
  
  wsd <- wsd %>% mutate(wsd,
                        REGION_NAME= case_when(
                          ISLAND =="Guam" ~ "Guam",
                          REGION == "CNMI"~ "Commonwealth of the Northern Mariana Islands",
                          TRUE ~ REGION_NAME))
  
  #Remove PRIA 2016 and 2017 surveys- done off cycle for the bleaching response, and do not have all metrics
  wsd$REGION_YEAR<-paste(wsd$REGION,wsd$ANALYSIS_YEAR,sep = "_")
  wsd$REGION_YEAR<-ifelse((wsd$ISLAND=="Wake" & wsd$ANALYSIS_YEAR=="2017"),"PRIAs_2017w",wsd$REGION_YEAR) #This will help you keep wake 2017 data
  table(wsd$REGION_YEAR)
  
  ####Do remove PRIAs data####
  remove<-c("PRIAs_2016","PRIAs_2017")
  wsd<-dplyr::filter(wsd, !REGION_YEAR %in% remove)
  
  #Change Analysis year according to desired pooling
  wsd[is.na(wsd$ANALYSIS_YEAR),]
  levels(wsd$ANALYSIS_YEAR)<-sort(unique(c(unique(wsd$ANALYSIS_YEAR),"2015-16","2014-15","2017-18")))
  i_1012_hi=which(wsd$REGION %in% c("MHI", "NWHI") & wsd$OBS_YEAR %in% seq(2010,2012))
  if(length(i_1012_hi)>0) {wsd[i_1012_hi,]$ANALYSIS_YEAR<-"2010-12"}
  wsd[wsd$REGION %in% c("MHI", "NWHI") & wsd$OBS_YEAR %in% seq(2013,2015),]$ANALYSIS_YEAR<-"2013-15"
  wsd[wsd$REGION %in% c("SAMOA") & wsd$OBS_YEAR %in% seq(2015,2016),]$ANALYSIS_YEAR<-"2015-16"
  
  #nNce we get to region: Pooled Wake -> rest of pria by changing analysis year...
  wsd[wsd$REGION %in% c("PRIAs") & wsd$OBS_YEAR %in% seq(2014,2015),]$ANALYSIS_YEAR<-"2014-15"
  wsd[wsd$REGION %in% c("PRIAs") & wsd$OBS_YEAR %in% seq(2017,2018),]$ANALYSIS_YEAR<-"2017-18"
  
  #Define Analysis Sector-some sectors are pooled together - we are using the demographic pooled sectors because the cover and demographic data will need to be merged into 1 file
  wsd$ANALYSIS_SEC<-wsd$PooledSector_Viztool
  sectors$AREA_HA<-as.numeric(sectors$AREA_HA)
  
  #NOW CHECK HOW MANY REPS WE HAVE PER STRATA
  #a.<-dcast(wsd, ISLAND + ANALYSIS_SEC + OBS_YEAR ~ STRATA, value.var="AREA_HA", length); a.
  a=wsd %>% group_by(ISLAND,ANALYSIS_SEC,OBS_YEAR,STRATA) %>%
    dplyr::summarize(AREA_HA=length(AREA_HA)) %>% 
    dplyr::arrange(STRATA) %>% 
    pivot_wider(names_from = STRATA,values_from = AREA_HA,values_fill = 0) %>% 
    dplyr::arrange(ISLAND,ANALYSIS_SEC,OBS_YEAR) 
  # a==a.
  View(a)
  
  RepTab=wsd %>% group_by(REGION,ANALYSIS_SEC) %>% filter(OBS_YEAR>=2013)%>% dplyr::count(ANALYSIS_YEAR) %>% pivot_wider(names_from=ANALYSIS_YEAR,values_from = n)
  RepTab=RepTab[,c(names(RepTab)[1:2],sort(names(RepTab)[3:10]))]
  RepTab %>% print(n=999)
  ####################################################################################################################################################################
  #
  #     POOL WSD (WORKING SITE DATA TO STRATA THEN TO HIGHER LEVELS
  ##
  ###################################################################################################################################################################
  #Save site level data - need to send site, lat and long to Viztool
  if(WRITE){write.csv(wsd,"T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/2023ViztoolSites_Cover.csv")}
  
  
  #Some sectors are pooled together, this will ensure that strata area is pooled correctly
  area.tmp<-ddply(wsd,.(REGION,ISLAND,ANALYSIS_YEAR,ANALYSIS_SEC,STRATA,AREA_HA),summarize,temp=sum(AREA_HA,na.rm=TRUE)) #calculate # of possible sites in a given stratum
  new.area<-ddply(area.tmp,.(REGION,ISLAND,ANALYSIS_YEAR,ANALYSIS_SEC,STRATA),summarize,AREA_HA_correct=sum(AREA_HA,na.rm=TRUE)) #calculate # of possible sites in a given stratum
  
  wsd<-left_join(wsd,new.area)
  
  
  #Generate complete and trend data at wsd level
  
  
  #SAVE BY STRATUM PER YEAR
  
  ###### Build out Complete and Trends Dataset
  wsd$SEC_STR=paste0(wsd$ANALYSIS_SEC,"_",wsd$STRATA)
  #
  wsd=wsd %>%
    mutate(SAMPLING_REGION= case_when(
      ISLAND =="Wake" ~ "MARIAN",
      REGION =="CNMI" ~ "MARIAN",
      REGION =="GUA" ~ "MARIAN",
      TRUE ~ as.character(REGION)))
  
  #Let's Not TAO MArch 14 2024
  ###Drop Data before 2013 #Do we really want to, even for Tier 1 data?
  ###wsd=wsd %>% filter(OBS_YEAR>=2013)
  
  ###### REMOVE STRATA that are undersampled in specific years (N<2) (cannot pool those up)
  #2023 MARCH - SUBSET FOR TEMPORALLY COHERENT STRATA LEVEL SAMPLING - NO LESS THAN 2 SAMPLES PER STRATA THE WHOLE TIME
  ###############################
  strat2drop=wsd
  
  #This is the data frame of regional-scale "should have been sampled"
  
  #Nested region and strata as single value
  REG_STRATA=unique(strat2drop[,c("SAMPLING_REGION","STRATANAME_TRENDS")])
  REG_STRATA$SR_STR=paste0(REG_STRATA$SAMPLING_REGION,"_",REG_STRATA$STRATANAME_TRENDS)
  
  #Years in which a region should have been sampled
  Canonical_N=200
  REGIONAL_SAMPLING_EFFORT_YEARS=table(strat2drop[,c("REGION","OBS_YEAR")],useNA = "ifany") %>%
    as.data.frame() %>%   mutate(SR_YR=paste0(REGION,"_",OBS_YEAR))  %>% arrange(REGION) %>% filter(Freq>0)
  
  REGIONAL_SAMPLING_EFFORT_YEARS=table(strat2drop[,c("SAMPLING_REGION","ANALYSIS_YEAR")],useNA = "ifany") %>%
    as.data.frame() %>%   mutate(SR_YR=paste0(SAMPLING_REGION,"_",ANALYSIS_YEAR))  %>% arrange(SAMPLING_REGION) %>% filter(Freq>0)
  REGIONAL_SAMPLING_EFFORT_YEARS_CANON=REGIONAL_SAMPLING_EFFORT_YEARS %>% filter(Freq>Canonical_N) %>% arrange(SAMPLING_REGION)
  
  #Highlight special island level sampling
  REGIONAL_SAMPLING_EFFORT_YEARS_SP=REGIONAL_SAMPLING_EFFORT_YEARS %>% filter(Freq<=Canonical_N&Freq>0) 
  ISLAND_SAMPLING_EFFORT_YEARS=table(strat2drop[,c("SAMPLING_REGION","ISLAND","ANALYSIS_YEAR")],useNA = "ifany") %>%
    as.data.frame() %>%   
    mutate(SR_YR=paste0(SAMPLING_REGION,"_",ANALYSIS_YEAR)) %>% 
    filter(SR_YR %in% REGIONAL_SAMPLING_EFFORT_YEARS_SP$SR_YR) %>% 
    filter(Freq>0) %>% arrange(SR_YR)
  
  #Actual sampling Described by Stratum
  Sample_Table=table(strat2drop[,c("SAMPLING_REGION","STRATANAME_TRENDS","ANALYSIS_YEAR")],useNA = "ifany")
  Actual_Sample_Table=Sample_Table %>% 
    as.data.frame() %>% 
    mutate(SR_YR=paste0(SAMPLING_REGION,"_",ANALYSIS_YEAR),
           SR_STR=paste0(SAMPLING_REGION,"_",STRATANAME_TRENDS))%>%
    filter(SR_STR%in%REG_STRATA$SR_STR) # Drop Strata not present in a region
  
  Low_Freq_STR_Canon=Actual_Sample_Table %>% 
    filter(SR_YR %in% REGIONAL_SAMPLING_EFFORT_YEARS_CANON$SR_YR) %>% 
    filter(Freq<=1)
  
  DropSTR_Canon=unique(Low_Freq_STR_Canon$STRATANAME_TRENDS)
  
  #dropwsd=wsd %>% filter(STRATANAME_TRENDS%in%DropSTR_Canon)
  #table(dropwsd[,c("STRATANAME_TRENDS","ANALYSIS_YEAR","REGION")])
  
  Low_Freq_STR_SP=Actual_Sample_Table %>%
    filter(SR_YR %in% REGIONAL_SAMPLING_EFFORT_YEARS_SP$SR_YR) 
  Low_Freq_STR_SP=Low_Freq_STR_SP %>% 
    mutate(ISLAND=substr(Low_Freq_STR_SP$STRATANAME_TRENDS,1,
                         regexpr("_", Low_Freq_STR_SP$STRATANAME_TRENDS, fixed = TRUE) - 1),
           ISL_YR=paste0(ISLAND,"_",ANALYSIS_YEAR))
  SPEC_TARGETS=Low_Freq_STR_SP %>% group_by(ISL_YR) %>% summarize(ISL_N=sum(Freq)) %>% filter(ISL_N>0)
  Low_Freq_STR_SP=Low_Freq_STR_SP %>% filter(ISL_YR %in% SPEC_TARGETS$ISL_YR) %>% filter(Freq<=1)
  
  DropSTR_SP=Low_Freq_STR_SP$STRATANAME_TRENDS
  
  DropSTR=union(DropSTR_Canon,DropSTR_SP)
  
  
  ###### GENERATE "TRENDS" DATA OF STRATA, by REMOVING ANY STRATUM with N<=1 in any sample year
  #Continue with wsdComp and wsdTrend
  
  wsdComp=wsd #Leave all sites in there until we dump stratum-year N=1
  wsdTrend=wsd %>% filter(!STRATANAME_TRENDS%in%DropSTR) #Filter out any undersampled stratum from DropSTR
  dim(wsdComp)
  dim(wsdTrend)
  
  
  ### CALCULATE MEAN AND VARIANCE WITHIN STRATA ###
  SPATIAL_POOLING_BASE<-c("REGION","ISLAND", "ANALYSIS_SEC", "REEF_ZONE", "STRATA")    
  ADDITIONAL_POOLING_BY<-c("ANALYSIS_YEAR")                                    # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)
  
  #generate within strata means and vars
  POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
  ################################################################################
  ################################################################################
  ################################################################################
  dpsComp<-Calc_PerStrata(wsdComp, data.cols, c(POOLING_LEVEL, "AREA_HA_correct"))
  
  #Clear out any Stratum-Year combo at N=1
  dpsComp$Mean=dpsComp$Mean %>% filter(N>1)
  dpsComp$SampleSE=dpsComp$SampleSE %>% filter(N>1)
  dpsComp$SampleVar=dpsComp$SampleVar %>% filter(N>1)
  min(dpsComp$Mean$N)
  
  dpsTrend<-Calc_PerStrata(wsdTrend, data.cols, c(POOLING_LEVEL, "AREA_HA_correct"))
  min(dpsTrend$Mean$N)
  
  #Complete
  dpstC=dpsComp
  colnames(dpstC$Mean)[9:ncol(dpstC$Mean)] <- paste("Mean.", colnames(dpstC$Mean[,9:ncol(dpstC$Mean)]), sep = "")
  colnames(dpstC$SampleSE)[9:ncol(dpstC$SampleSE)] <- paste("SE.", colnames(dpstC$SampleSE[,9:ncol(dpstC$SampleSE)]), sep = "")
  dpstC<-left_join(dpstC$Mean,dpstC$SampleSE)
  
  #Trends
  dpstT=dpsTrend
  colnames(dpstT$Mean)[9:ncol(dpstT$Mean)] <- paste("Mean.", colnames(dpstT$Mean[,9:ncol(dpstT$Mean)]), sep = "")
  colnames(dpstT$SampleSE)[9:ncol(dpstT$SampleSE)] <- paste("SE.", colnames(dpstT$SampleSE[,9:ncol(dpstT$SampleSE)]), sep = "")
  dpstT<-left_join(dpstT$Mean,dpstT$SampleSE)
  
  #Drop GUAM MPA from Trends Dataset
  dpstT=dpstT %>% filter(ANALYSIS_SEC!="GUA_MP")
  
  ri<-read.csv("T:/Benthic/Data/Lookup Tables/NCRMP_Regions_Islands.csv")
  ri <- ri %>% mutate(ri,
                      REGION_NAME= case_when(
                        ISLAND =="Guam" ~ "Guam",
                        REGION_NAME == "Mariana Archipelago"~ "Commonwealth of the Northern Mariana Islands",
                        TRUE ~ REGION_NAME))
  
  dpstC<-left_join(dpstC,ri)
  dpstT<-left_join(dpstT,ri)
  if(TIER==1){
    if(WRITE){write.csv(dpstC, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier1_STRATA_Complete_Viztool.csv",row.names = F)}
    if(WRITE){write.csv(dpstT, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier1_STRATA_Trends_Viztool.csv",row.names = F)}
  }else if(TIER==2){
    if(WRITE){write.csv(dpstC, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier2b_STRATA_Complete_Viztool.csv",row.names = F)}
    if(WRITE){write.csv(dpstT, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier2b_STRATA_Trends_Viztool.csv",row.names = F)}
  }else{
    if(WRITE){write.csv(dpstC, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier3_STRATA_Complete_Viztool.csv",row.names = F)}
    if(WRITE){write.csv(dpstT, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier3_STRATA_Trends_Viztool.csv",row.names = F)}
  }
  
  #SAVE BY SECTOR PER YEAR
  OUTPUT_LEVEL<-c("REGION","ISLAND","ANALYSIS_SEC","ANALYSIS_YEAR") 
  dpsecC<-Calc_Pooled_Simple(dpsComp$Mean, dpsComp$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA_correct")
  colnames(dpsecC$Mean)[6:ncol(dpsecC$Mean)] <- paste("Mean.", colnames(dpsecC$Mean[,6:ncol(dpsecC$Mean)]), sep = "")
  colnames(dpsecC$PooledSE)[6:ncol(dpsecC$PooledSE)] <- paste("SE.", colnames(dpsecC$PooledSE[,6:ncol(dpsecC$PooledSE)]), sep = "")
  dpsecC<-left_join(dpsecC$Mean,dpsecC$PooledSE)
  dpsecC<-left_join(dpsecC,ri)
  
  dpsecT<-Calc_Pooled_Simple(dpsTrend$Mean, dpsTrend$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA_correct")
  colnames(dpsecT$Mean)[6:ncol(dpsecT$Mean)] <- paste("Mean.", colnames(dpsecT$Mean[,6:ncol(dpsecT$Mean)]), sep = "")
  colnames(dpsecT$PooledSE)[6:ncol(dpsecT$PooledSE)] <- paste("SE.", colnames(dpsecT$PooledSE[,6:ncol(dpsecT$PooledSE)]), sep = "")
  dpsecT<-left_join(dpsecT$Mean,dpsecT$PooledSE)
  dpsecT<-left_join(dpsecT,ri)
  
  if(TIER==1){
    if(WRITE){write.csv(dpsecC, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier1_SECTOR_Complete_Viztool.csv",row.names = F)}
    if(WRITE){write.csv(dpsecT, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier1_SECTOR_Trends_Viztool.csv",row.names = F)}
  }else if(TIER==2){
    if(WRITE){write.csv(dpsecC, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier2b_SECTOR_Complete_Viztool.csv",row.names = F)}
    if(WRITE){write.csv(dpsecT, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier2b_SECTOR_Trends_Viztool.csv",row.names = F)}
  }else{
    if(WRITE){write.csv(dpsecC, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier3_SECTOR_Complete_Viztool.csv",row.names = F)}
    if(WRITE){write.csv(dpsecT, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier3_SECTOR_Trends_Viztool.csv",row.names = F)}
  }
  
  # e.g. SAVE BY ISLAND PER YEAR
  OUTPUT_LEVEL<-c("REGION","ISLAND","ANALYSIS_YEAR") 
  dpisC<-Calc_Pooled_Simple(dpsComp$Mean, dpsComp$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA_correct")
  colnames(dpisC$Mean)[5:ncol(dpisC$Mean)] <- paste("Mean.", colnames(dpisC$Mean[,5:ncol(dpisC$Mean)]), sep = "")
  colnames(dpisC$PooledSE)[5:ncol(dpisC$PooledSE)] <- paste("SE.", colnames(dpisC$PooledSE[,5:ncol(dpisC$PooledSE)]), sep = "")
  dpisC<-left_join(dpisC$Mean,dpisC$PooledSE)
  dpisC<-left_join(dpisC,ri)
  
  dpisT<-Calc_Pooled_Simple(dpsTrend$Mean, dpsTrend$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA_correct")
  colnames(dpisT$Mean)[5:ncol(dpisT$Mean)] <- paste("Mean.", colnames(dpisT$Mean[,5:ncol(dpisT$Mean)]), sep = "")
  colnames(dpisT$PooledSE)[5:ncol(dpisT$PooledSE)] <- paste("SE.", colnames(dpisT$PooledSE[,5:ncol(dpisT$PooledSE)]), sep = "")
  dpisT<-left_join(dpisT$Mean,dpisT$PooledSE)
  dpisT<-left_join(dpisT,ri)
  
  if(TIER==1){
    if(WRITE){write.csv(dpisC, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier1_ISLAND_Complete_Viztool.csv",row.names = F)}
    if(WRITE){write.csv(dpisT, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier1_ISLAND_Trends_Viztool.csv",row.names = F)}
  }else if(TIER==2){
    if(WRITE){write.csv(dpisC, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier2b_ISLAND_Complete_Viztool.csv",row.names = F)}
    if(WRITE){write.csv(dpisT, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier2b_ISLAND_Trends_Viztool.csv",row.names = F)}
  }else{
    if(WRITE){write.csv(dpisC, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier3_ISLAND_Complete_Viztool.csv",row.names = F)}
    if(WRITE){write.csv(dpisT, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier3_ISLAND_Trends_Viztool.csv",row.names = F)}
  }
  
  # e.g. SAVE BY REGION PER YEAR
  OUTPUT_LEVEL<-c("REGION","ANALYSIS_YEAR") 
  dprC<-Calc_Pooled_Simple(dpsComp$Mean, dpsComp$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA_correct")
  colnames(dprC$Mean)[4:ncol(dprC$Mean)] <- paste("Mean.", colnames(dprC$Mean[,4:ncol(dprC$Mean)]), sep = "")
  colnames(dprC$PooledSE)[4:ncol(dprC$PooledSE)] <- paste("SE.", colnames(dprC$PooledSE[,4:ncol(dprC$PooledSE)]), sep = "")
  dprC<-left_join(dprC$Mean,dprC$PooledSE)
  
  #Drop "Special" Mission (i.e. Howland, Baker, Jarvis in 2016 and 2017)
  dpsTrendREG=dpsTrend
  dpsTrendREG$Mean=dpsTrendREG$Mean %>% filter(!(ISLAND %in% c("Howland", "Baker", "Jarvis")&ANALYSIS_YEAR%in%c("2016","2017")))
  dpsTrendREG$SampleVar=dpsTrendREG$SampleVar %>% filter(!(ISLAND %in% c("Howland", "Baker", "Jarvis")&ANALYSIS_YEAR%in%c("2016","2017")))
  
  #Pool Wake with rest of Pria
  dpsTrendREG$Mean=dpsTrend$Mean %>% filter() %>% mutate(ANALYSIS_YEAR= case_when(
    ANALYSIS_YEAR %in% c("2014","2015") & REGION=="PRIAs" ~ "2014_2015",
    ANALYSIS_YEAR %in% c("2017","2018") & REGION=="PRIAs" ~ "2017_2018",
    TRUE ~ ANALYSIS_YEAR))
  
  dpsTrendREG$SampleVar=dpsTrend$SampleVar %>% mutate(ANALYSIS_YEAR= case_when(
    ANALYSIS_YEAR %in% c("2014","2015") & REGION=="PRIAs" ~ "2014_2015",
    ANALYSIS_YEAR %in% c("2017","2018") & REGION=="PRIAs" ~ "2017_2018",
    TRUE ~ ANALYSIS_YEAR))
  
  dprT<-Calc_Pooled_Simple(dpsTrendREG$Mean, dpsTrendREG$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA_correct")
  colnames(dprT$Mean)[4:ncol(dprT$Mean)] <- paste("Mean.", colnames(dprT$Mean[,4:ncol(dprT$Mean)]), sep = "")
  colnames(dprT$PooledSE)[4:ncol(dprT$PooledSE)] <- paste("SE.", colnames(dprT$PooledSE[,4:ncol(dprT$PooledSE)]), sep = "")
  dprT<-left_join(dprT$Mean,dprT$PooledSE)
  
  if(TIER==1){
    if(WRITE){write.csv(dprC, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier1_REGION_Complete_Viztool.csv",row.names = F)}
    if(WRITE){write.csv(dprT, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier1_REGION_Trends_Viztool.csv",row.names = F)}
  }else if(TIER==2){
    if(WRITE){write.csv(dprC, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier2b_REGION_Complete_Viztool.csv",row.names = F)}
    if(WRITE){write.csv(dprT, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier2b_REGION_Trends_Viztool.csv",row.names = F)}
  }else{
    if(WRITE){write.csv(dprC, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier3_REGION_Complete_Viztool.csv",row.names = F)}
    if(WRITE){write.csv(dprT, file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/BenthicCover_2010-2023_Tier3_REGION_Trends_Viztool.csv",row.names = F)}
  }
}
