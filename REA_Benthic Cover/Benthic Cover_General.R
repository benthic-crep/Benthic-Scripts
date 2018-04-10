rm(list=ls())

setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/BIA")

library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below
library(RODBC)            # to connect to oracle

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/fish_team_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Islandwide Mean&Variance Functions.R")


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


##### MERGE THEM TOGETHER - LOOKING LIKE BIA) #############
bia$METHOD<-"CPCE"
bia$SHORT_CODE<-"BIA"
#bia$FUNCTIONAL_GROUP<-"BIA"    # IDW - ACTUALLY FUNCTIONAL_GROUP SEEMS A BIT MIXED UP ... FROM QUICK LOOK AT CNET FILE, IT CAN TAKE DIFFERENT VALUES FOR SAME CODES (eg ALGAE or Hare Substrate) - SO GOING TO IGNORE IT!

cnet$POINTS<-1
cnet$METHOD<-"CNET"
cnet$REP<-cnet$REPLICATE
cnet$IMAGE_NAME<-cnet$ORIGINAL_FILE_NAME
cnet$PHOTOID<-cnet$IMAGE_NUMBER
cnet$TIER_1<-cnet$CATEGORY_CODE
cnet$TIER_2<-cnet$SUBCATEGORY_CODE
cnet$TIER_3<-cnet$GENERA_CODE


FIELDS_TO_RETAIN<-c("MISSIONID","METHOD", "REGION", "REGION_NAME","OBS_YEAR","ISLAND", "SITEVISITID","SITE", "LATITUDE", "LONGITUDE", "REEF_ZONE", "DEPTH_BIN", "PERM_SITE", "CLIMATE_STATION_YN", "MIN_DEPTH", "MAX_DEPTH", "HABITAT_CODE", "DATE_", "REP", "IMAGE_NAME", "PHOTOID", "ANALYST", "TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME", "SHORT_CODE", "POINTS")
x<-bia[,FIELDS_TO_RETAIN]; head(x)
y<-cnet[,FIELDS_TO_RETAIN]; head(y)

ab<-rbind(x, y)
#ab$DATE_ <- as.Date(ab$DATE_, "%m/%d/%Y")
ab$DATE_ <- as.factor(ab$DATE_)


#write.csv(ab, file="tmp All BIA BOTH METHODS.csv")
SURVEY_INFO<-c("OBS_YEAR", "REGION",  "ISLAND")
survey_island<-Aggregate_InputTable(cnet, SURVEY_INFO)

SURVEY_INFO<-c("MISSIONID","REGION","OBS_YEAR", "ISLAND", "SITEVISITID","SITE","LATITUDE","LONGITUDE","REEF_ZONE", "DEPTH_BIN", "PERM_SITE", "CLIMATE_STATION_YN", "MIN_DEPTH", "MAX_DEPTH", "HABITAT_CODE","DATE_")
survey_site<-Aggregate_InputTable(ab, SURVEY_INFO)

colnames(survey_site)[colnames(survey_site)=="MIN_DEPTH"]<-"SITE_MIN_DEPTH_FT" #Change column name
colnames(survey_site)[colnames(survey_site)=="MAX_DEPTH"]<-"SITE_MAX_DEPTH_FT" #Change column name


#Save list of sites
write.csv(survey_site,"All Photoquad Sites.csv")

#Remove Lat,Long and date before merging with SITE MASTER. The Lat and Long are 1 digit longer in the SM than the BIA data and will not merge properly with SM
survey_site.<- subset(survey_site, select=-c(LATITUDE,LONGITUDE,DATE_))

#Read in original SITE MASTER
sm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SITE MASTER.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)
sm$DATE_ <- as.factor(sm$DATE_)


#Merge Site master with list of BIA sites
allsite<-merge(survey_site.,sm,by=c("REGION","ISLAND", "OBS_YEAR","SITEVISITID","SITE","REEF_ZONE", "DEPTH_BIN","SITE_MAX_DEPTH_FT", "SITE_MIN_DEPTH_FT", "HABITAT_CODE"),all=TRUE)
head(allsite)

#Indentify sites that did not merge properly
test<-ddply(allsite,.(SITE),summarize,sites=length(SITE));subset(test,sites>1) #this should return 0 rows

#Generate a list of sites found in the BIA data, but not SM and remove all columns except OBS_YEAR and SITEVISITID
miss.sites<-allsite[is.na(allsite$METHOD), ]
miss.sites.<- subset(miss.sites, select=c(OBS_YEAR,SITEVISITID))
nrow(miss.sites.)


#Extract all of the metadata for the sites that need to be incorporated into the SITE MASTER file. 
#The missing are a combination of permanent sites, climate stations, special projects and BAK and HOW 2017 benthic sites that were only surveyed for juveniles and photoquads
sitestomerge<-merge(survey_site,miss.sites.,by=c("OBS_YEAR","SITEVISITID"));nrow(sitestomerge)
sitestomerge<- subset(sitestomerge, select=-c(MISSIONID))
sitestomerge$METHOD<-NA
sitestomerge$EXCLUDE_FLAG<-NA
sitestomerge$SEC_NAME<-NA
sitestomerge$HUMANS20<-NA
sitestomerge$HUMANS200<-NA
sitestomerge$TYPE<-NA
sitestomerge$ANALYSIS_SCHEME<-NA
sitestomerge$ANALYSIS_YEAR<-NA
sitestomerge$TUT2012<-NA
sitestomerge$OTHER_AREA_GROUPING<-NA
sitestomerge$BENTHIC_SEC_CODE<-NA

#add columns to sm to rbind correctly with sitestomerge
sm$CLIMATE_STATION_YN<-NA
sm$PERM_SITE<-NA

#Rbind the original SITE MASTER and missing sites. I tried to use merge(), but the lat, long and dates aren't matching. this is the cleanest way to combine dfs
sm.new<-rbind(sm,sitestomerge)
write.csv(sm.new,"tmpSITE MASTER.csv")

sm.new<-read.csv("SITE MASTER_v2.csv")


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

ab<-subset(ab, ab$REGION =="PRIAs")
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
SITE_FIELDS<-c("METHOD", "REGION", "OBS_YEAR", "DATE_", "ISLAND", "PERM_SITE", "CLIMATE_STATION_YN", "SITE", "LATITUDE", "LONGITUDE", "REEF_ZONE", "DEPTH_BIN")
sites<-aggregate(ab[,"POINTS"],by=ab[,SITE_FIELDS], sum)
sites$x<-NULL
write.csv(sites, file="All BIA PRIA Sites.csv")
dim(sites)

### GENERATE DATA AT SITE LEVEL AND FOR ALL HARD_CORAL
## FIRST T1
#ab2<-aggregate(ab[,"POINTS"],by=ab[,c("METHOD", "OBS_YEAR", "SITE", "REP", "PHOTOID", "TIER_1")], sum)
#names(ab2)[length(names(ab2))]<-"POINTS"
photo<-cast(ab, METHOD + OBS_YEAR + SITE + REP + PHOTOID ~ TIER_1, value="POINTS", fun.aggregate=sum, fill=0)
head(photo)
## NOW CORAL ONLY - ASSUMING YOU DONT WANT THIS LEVEL
# hc<-ab
# hc$GENUS<-as.character(hc$GENERA_NAME)
# hc[hc$TIER_1 != "CORAL",]$GENUS<-"zOTHER"
# unique(hc$GENUS)
# CORAL_GEN<-unique(hc[hc$TIER_1=="CORAL",]$GENUS)
# photo2<-cast(hc, METHOD + OBS_YEAR + SITE + REP + PHOTOID ~ GENUS, value="POINTS", fun.aggregate=sum, fill=0)
# photo2$zOTHER<-NULL
# head(photo2)

# p_all<-merge(photo, photo2, by=c("METHOD", "OBS_YEAR", "SITE", "REP", "PHOTOID"), all.x=T)
# photo<-p_all

#now convert to percentages
r_levels<-unique(as.character(ab$TIER_1))
photo$N<-rowSums(photo[,r_levels])
#data.cols<-c(r_levels, CORAL_GEN)
data.cols<-c(r_levels)

photo[,data.cols]<-photo[,data.cols]/photo$N
head(photo)

# now take average of photos within a rep, then average of reps within a site
rep<-aggregate(photo[,data.cols], by = photo[,c("METHOD", "OBS_YEAR", "SITE", "REP")],  mean)
site<-aggregate(rep[,data.cols], by = rep[,c("METHOD", "OBS_YEAR", "SITE")],  mean)
head(site)

wsd<-merge(sites, site, by=c("METHOD", "OBS_YEAR", "SITE"), all.y=T)

save(wsd, file="tmp SiteData.RData")

####################################################################################################################################################################
#
#     CHECK THAT DATA IS READY FOR POOLING AND DO SOME FINAL CLEAN UPS, EG SET BACKREEF DEPTH_ZONE TO ALL, CREATE THE "SGA" LOCATION
#
####################################################################################################################################################################
## NOW POOL UP TO ISLAND AND YEAR
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA/Sectors-Strata-Areas.csv")
sm<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA/SITE MASTER.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)

#FIDDLE IT TO MAKE IT WORK WITH THE STANDARD POOLING DATA
wsd$SITEVISITID<-paste(wsd$SITE, wsd$OBS_YEAR, sep="")
wsd$SEC_NAME<-wsd$ISLAND
wsd$ANALYSIS_SCHEME<-"RAMP_BASIC"
head(wsd)

write.csv(wsd,"PRIA_Cover_Site.csv")

## check wwhether we have ISLANDS that arent in the sectors file
setdiff(unique(wsd$ISLAND),unique(sectors$ISLAND))

#set all Backreef to a single DEPTH_ZONE ("All") 
wsd$STRATA<-paste(substring(wsd$REEF_ZONE,1,1), substring(wsd$DEPTH_BIN,1,1), sep="")
sectors$STRATA<-paste(substring(sectors$REEF_ZONE,1,1), substring(sectors$DEPTH_BIN,1,1), sep="")

## generate a complete list of all ANALYSIS STRATA and their size
SCHEMES<-c("RAMP_BASIC")
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

wsd<-merge(wsd, as, by=c("SEC_NAME", "ANALYSIS_SCHEME"), all.x=T)  # add ANALYSISS_SCHEME for tthis sector and sceheme combination
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
SPATIAL_POOLING_BASE<-c("ISLAND", "SEC_NAME", "REEF_ZONE", "STRATA")    
ADDITIONAL_POOLING_BY<-c("METHOD", "OBS_YEAR")                                    # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)

#generate within strata means and vars
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
dps<-Calc_PerStrata(wsd, data.cols, c(POOLING_LEVEL, "AREA_HA"))
head(dps$Mean)
save(dps, file="tmp PRIMM BIA per strata.RData")

###### REMOVE STRATA with N=1 (cannot pool those up)
dps$Mean<-dps$Mean[dps$Mean$N>1,]
dps$SampleVar<-dps$SampleVar[dps$SampleVar$N>1,]
dps$SampleSE<-dps$SampleSE[dps$SampleSE$N>1,]


# e.g. SAVE BY ISLAND AND REEF_ZONE PER YEAR
OUTPUT_LEVEL<-c("ISLAND","STRATA","OBS_YEAR") 
dp<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
write.csv(dp, file="PRIMM STRATA_ISLAND_YEAR.csv")


# e.g. SAVE BY ISLAND PER YEAR
OUTPUT_LEVEL<-c("ISLAND","OBS_YEAR") 
dp<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
write.csv(dp, file="PRIMM ISLAND & YEAR.csv")




