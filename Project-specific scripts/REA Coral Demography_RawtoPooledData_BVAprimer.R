
#CREATE ADULT CLEAN ANALYSIS READY DATA----------------------------------------
# This script will clean the raw benthic REA data using method E that comes directly from the new data base application.
#Note- these data represent the revised data structure insituted in November 2018. Several recent dead and condition columns were added
rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
load("ALL_REA_ADULTCORAL_RAW_new.rdata") #from oracle
x<-df #leave this as df
#x<-read.csv("V0_CORAL_OBS_E_final.csv")

x$SITE<-SiteNumLeadingZeros(x$SITE) # Change site number such as MAR-22 to MAR-0022

### Use these functions to look at data
head(x)
tail(x)
table(x$REGION, x$OBS_YEAR) #review years and regions in dataframe

# load site master to merge with sector names later in the script
# See REA Generate Benthic Site Master and Sectors script for more details on how site master file was created.
site_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SITE MASTER.csv");nrow(site_master)
site_master$SITE<-SiteNumLeadingZeros(site_master$SITE)

#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","REGION_NAME","ISLAND","ISLANDCODE","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","NO_SURVEY_YN","EXCLUDE_FLAG","SITEVISITID","HABITAT_CODE","DIVER","TRANSECTNUM","SEGMENT","SEGWIDTH","SEGLENGTH","FRAGMENT_YN",
             "COLONYID","TAXONCODE","COLONYLENGTH","OLDDEAD",
             "RECENTDEAD_1","RECENT_GENERAL_CAUSE_CODE_1","RECENT_SPECIFIC_CAUSE_CODE_1",
             "RECENTDEAD_2",	"RECENT_GENERAL_CAUSE_CODE_2","RECENT_SPECIFIC_CAUSE_CODE_2",	
             "RECENT_GENERAL_CAUSE_CODE_3","RECENT_SPECIFIC_CAUSE_CODE_3","RECENTDEAD_3","COND",
             "CONDITION_2","CONDITION_3","GENUS_CODE","S_ORDER","TAXONNAME","SITE_MIN_DEPTH","SITE_MAX_DEPTH")



#remove extraneous columns
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

#Double check level and class of variables to make sure there aren't any errors
sapply(x,levels)
sapply(x,class)##Change column names to make code easier to code

colnames(x)[colnames(x)=="TAXONCODE"]<-"SPCODE" #Change column name
colnames(x)[colnames(x)=="TRANSECTNUM"]<-"TRANSECT" #Change column name
colnames(x)[colnames(x)=="RECENTDEAD_1"]<-"RDEXTENT1" #Change column name
colnames(x)[colnames(x)=="RECENT_GENERAL_CAUSE_CODE_1"]<-"GENRD1" #Change column name
colnames(x)[colnames(x)=="RECENT_SPECIFIC_CAUSE_CODE_1"]<-"RD1" #Change column name
colnames(x)[colnames(x)=="RECENTDEAD_2"]<-"RDEXTENT2" #Change column name
colnames(x)[colnames(x)=="RECENTDEAD_3"]<-"RDEXTENT3" #Change column name
colnames(x)[colnames(x)=="RECENT_GENERAL_CAUSE_CODE_2"]<-"GENRD2" #Change column name
colnames(x)[colnames(x)=="RECENT_SPECIFIC_CAUSE_CODE_2"]<-"RD2" #Change column name
colnames(x)[colnames(x)=="RECENT_GENERAL_CAUSE_CODE_3"]<-"GENRD3" #Change column name
colnames(x)[colnames(x)=="RECENT_SPECIFIC_CAUSE_CODE_3"]<-"RD3" #Change column name
colnames(x)[colnames(x)=="FRAGMENT_YN"]<-"Fragment" #Change column name
colnames(x)[colnames(x)=="COND"]<-"CONDITION_1" #Change column name

head(x)

# CLEAN UP ----------------------------------------------------------------

##Remove sites that were only surveyed for photoquads but not demographics
#Note-photoquad only sites are not included in data prior to 2018
#Test whether there are missing values in the NO_SURVEY_YN column. The value should be 0 or -1
x.na<-x[is.na(x$NO_SURVEY_YN),]
test<-ddply(x.na,.(SITE),
            summarize,
            SEG=length(unique(SEGMENT)))
test
x$NO_SURVEY_YN<-is.na(x$NO_SURVEY_YN)<-0 #Change NAs (blank cells) to 0
x<-subset(x,NO_SURVEY_YN==0)


#TEMPORARY FIX-SPEAK WITH DM TO CORRECT IN ORACLE
#Change all special missions to exclude flag =-1, right now they are 0 Then exclude these sites
levels(x$MISSIONID)
x$EXCLUDE_FLAG<-ifelse(x$MISSIONID %in% c("MP1410","MP1512","MP1602","SE1602"),-1,0)
head(subset(x,EXCLUDE_FLAG==-1))

x<-subset(x,SEGLENGTH!="NA") #Remove segments that were not surveyed for coral demography
x<-subset(x,EXCLUDE_FLAG==0);head(subset(x,EXCLUDE_FLAG==-1))# this dataframe should be empty

head(x)

#Change NAs in RD extent to 0
head(subset(x,S_ORDER=="Scleractinia" & is.na(x$RDEXTENT1))) #identify columns that have NAs
x$RDEXTENT1<-ifelse(x$S_ORDER=="Scleractinia"& is.na(x$RDEXTENT1),0,x$RDEXTENT1)

head(subset(x,S_ORDER=="Scleractinia" & is.na(x$RDEXTENT2))) #identify columns that have NAs
x$RDEXTENT2<-ifelse(x$S_ORDER=="Scleractinia"& is.na(x$RDEXTENT2),0,x$RDEXTENT2)

head(subset(x,S_ORDER=="Scleractinia" & is.na(x$RDEXTENT3))) #identify columns that have NAs
x$RDEXTENT3<-ifelse(x$S_ORDER=="Scleractinia"& is.na(x$RDEXTENT3),0,x$RDEXTENT3)


# Assign TAXONCODE --------------------------------------------------------
#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
taxa<-read.csv("2013-18_Taxa_MASTER.csv")

#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
x.<-Convert_to_Taxoncode(x)

#Check to see whether there are hard corals that have a SPCODE and GENUSCODE but no S_ORDER
test<-x.[is.na(x.$S_ORDER),];test<-droplevels(test)
head(test) #this dataframe should be empty

#Create a list Species codes and associated genera
SURVEY_INFO<-c("OBS_YEAR","SPCODE","TAXONCODE","GENUS_CODE","TAXONNAME")
test<-new_Aggregate_InputTable(x., SURVEY_INFO)
head(test)

#Change columns to character
x.$GENUS_CODE<-as.character(x.$GENUS_CODE)
x.$SPCODE<-as.character(x.$SPCODE)
x.$TAXONCODE<-as.character(x.$TAXONCODE)
x.$S_ORDER<-as.character(x.$S_ORDER)


#There are some SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genera or taxoncodes to the spcode and the remaining NAs in the Taxon and genus code to AAAA
x.$GENUS_CODE<-ifelse(is.na(x.$GENUS_CODE)&x.$S_ORDER=="Scleractinia",x.$SPCODE,x.$GENUS_CODE)
x.$TAXONCODE<-ifelse(is.na(x.$TAXONCODE)&x.$S_ORDER=="Scleractinia",x.$SPCODE,x.$TAXONCODE)
x.$GENUS_CODE<-ifelse(x.$TAXONCODE=="UNKN","UNKN",x.$GENUS_CODE)
x.$GENUS_CODE<-ifelse(x.$TAXONCODE=="AAAA","AAAA",x.$GENUS_CODE)

#utils::View(x) #view data in separate window

#Check that Unknown scl were changed correctly
test<-subset(x.,TAXONCODE=="UNKN"&S_ORDER=="Scleractinia");head(test)
test<-subset(x.,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia");head(test)
test<-subset(x.,GENUS_CODE=="AAAA");head(test)
test<-subset(x.,SPCODE=="AAAA");head(test)

#Confirm that no rows were dropped during merge
nrow(x)
nrow(x.)
x<-x.

# #Create new colummns that combine species, genus and morphology
# x$TAXMORPH<-paste(x$TAXONCODE,x$MORPH_CODE,sep="")
# x$GENMORPH<-paste(x$GENUS_CODE,x$MORPH_CODE,sep="")


#add SITE MASTER information to x 
#x<-merge(x, site_master[,c("SITE", "SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE) #Fish team's original code, we may want to create analysis scheme later in the 
length(unique(x$SITEVISITID)) #double check that sites weren't dropped
nrow(subset(site_master,METHOD %in% c("CORALBELT_METHOD_E_F","CORALBELT_METHOD_F_PHOTOQUADS")))
x<-merge(x, site_master[,c("OBS_YEAR","SITEVISITID","SITE","SEC_NAME","ANALYSIS_YEAR")], by=c("OBS_YEAR","SITEVISITID","SITE"),all.x=T)  
length(unique(x$SITEVISITID)) #double check that sites weren't dropped
head(x)


#CHECK THAT all SEC_NAME are present in the site_master file
test<-x[is.na(x$SEC_NAME), c("MISSIONID","REGION", "SITE","OBS_YEAR"),]
test<-droplevels(test);table(test$SITE,test$MISSIONID) #create a table of missing sites by missionid
if(dim(test)[1]>0) {cat("sites with MISSING SECTORS present")}   # should be 0

###NWHI 2014 and 2015, and a few Hawaii island 2013 sites are missing from SITE MASTER

#If there are missing sectors, generate a table of missing sites, lat, long, reef zone and depth bins. Manually correct Site Master file
a<-subset(x,is.na(x$SEC_NAME))
a<-a[c("OBS_YEAR","SITE","LATITUDE", "LONGITUDE","REEF_ZONE","DEPTH_BIN")]
test<-unique(a)
write.csv(test,"missingsectors.csv")


##Calcuating segment and transect area and add column for transect area
x<-Transectarea(x)
# sapply(x,levels)
head(x)
nrow(x)


## CLEAN UP NAs ##
DATE_<-x$DATE_
head(x$DATE_)
x<-subset(x,select =-c(DATE_)) #the date column is causing problems. delete for now
x[x=="-9"]<-NA
x[x==""]<-NA

tmp.lev<-levels(x$GENRD1); head(tmp.lev)
levels(x$GENRD1)<-c(tmp.lev, "NONE") # change to NONE
x[is.na(x$GENRD1),"GENRD1"]<-"NONE"

tmp.lev<-levels(x$RD1); head(tmp.lev)
levels(x$RD1)<-c(tmp.lev, "NONE")
x[is.na(x$RD1),"RD1"]<-"NONE"

tmp.lev<-levels(x$GENRD2); head(tmp.lev)
levels(x$GENRD2)<-c(tmp.lev, "NONE")
x[is.na(x$GENRD2),"GENRD2"]<-"NONE"

tmp.lev<-levels(x$RD2); head(tmp.lev)
levels(x$RD2)<-c(tmp.lev, "NONE")
x[is.na(x$RD2),"RD2"]<-"NONE"

tmp.lev<-levels(x$GENRD3); head(tmp.lev)
levels(x$GENRD3)<-c(tmp.lev, "NONE")
x[is.na(x$GENRD3),"GENRD3"]<-"NONE"

tmp.lev<-levels(x$RD3); head(tmp.lev)
levels(x$RD3)<-c(tmp.lev, "NONE")
x[is.na(x$RD3),"RD3"]<-"NONE"

tmp.lev<-levels(x$CONDITION_1); head(tmp.lev)
levels(x$CONDITION_1)<-c(tmp.lev, "NONE")
x[is.na(x$CONDITION_1),"CONDITION_1"]<-"NONE"

tmp.lev<-levels(x$CONDITION_2); head(tmp.lev)
levels(x$CONDITION_2)<-c(tmp.lev, "NONE")
x[is.na(x$CONDITION_2),"CONDITION_2"]<-"NONE"

tmp.lev<-levels(x$CONDITION_3); head(tmp.lev)
levels(x$CONDITION_3)<-c(tmp.lev, "NONE")
x[is.na(x$CONDITION_3),"CONDITION_3"]<-"NONE"

head(x)

awd<-droplevels(x)

SURVEY_INFO<-c("S_ORDER","GENUS_CODE","TAXONCODE","TAXONNAME")
test<-new_Aggregate_InputTable(awd, SURVEY_INFO)


save(awd, file="TMPBenthicREA_Adultwd_022619.Rdata")  #Save clean working data


## CREATE JUVENILE CLEAN ANALYSIS READY DATA ----
## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
load("ALL_REA_JUVCORAL_RAW_new.rdata") #from oracle
x<-df #leave this as df

#x<-read.csv("V0_CORAL_OBS_F_final.csv")

x$SITE<-SiteNumLeadingZeros(x$SITE) # Change site number such as MAR-22 to MAR-0022


### Use these functions to look at data
head(x)
tail(x)
table(x$REGION, x$OBS_YEAR) #review years and regions in dataframe

# load site master to merge with sector names later in the script
# See REA Generate Benthic Site Master and Sectors script for more details on how site master file was created.
site_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SITE MASTER.csv");nrow(site_master)
site_master$SITE<-SiteNumLeadingZeros(site_master$SITE)


#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","REGION_NAME","ISLAND","ISLANDCODE","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","NO_SURVEY_YN","EXCLUDE_FLAG","SITEVISITID","HABITAT_CODE","DIVER","TRANSECTNUM","SEGMENT","SEGWIDTH","SEGLENGTH",
             "COLONYID","TAXONCODE","COLONYLENGTH","GENUS_CODE","S_ORDER","TAXONNAME","MINDEPTH","MAXDEPTH")


#remove extraneous columns
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

#Double check level and class of variables to make sure there aren't any errors
sapply(x,levels)
sapply(x,class)##Change column names to make code easier to code

colnames(x)[colnames(x)=="TAXONCODE"]<-"SPCODE" #Change column name
colnames(x)[colnames(x)=="TRANSECTNUM"]<-"TRANSECT" #Change column name
colnames(x)[colnames(x)=="MINDEPTH"]<-"SITE_MIN_DEPTH" #Change column name
colnames(x)[colnames(x)=="MAXDEPTH"]<-"SITE_MAX_DEPTH" #Change column name

head(x)

# CLEAN UP ----------------------------------------------------------------

##Remove sites that were only surveyed for photoquads but not demographics
#Note-photoquad only sites are not included in data prior to 2018
#Test whether there are missing values in the NO_SURVEY_YN column. The value should be 0 or -1
x.na<-x[is.na(x$NO_SURVEY_YN),]
test<-ddply(x.na,.(SITE),
            summarize,
            SEG=length(unique(SEGMENT)))
test
x$NO_SURVEY_YN<-is.na(x$NO_SURVEY_YN)<-0 #Change NAs (blank cells) to 0
x<-subset(x,NO_SURVEY_YN==0)

#TEMPORARY FIX-SPEAK WITH DM TO CORRECT IN ORACLE
#Change all special missions to exclude flag =-1, right now they are 0 Then exclude these sites
levels(x$MISSIONID)
x$EXCLUDE_FLAG<-ifelse(x$MISSIONID %in% c("MP1410","MP1512","MP1602","SE1602"),-1,0)
head(subset(x,EXCLUDE_FLAG==-1))

x<-subset(x,SEGLENGTH!="NA") #Remove segments that were not surveyed for coral demography
x<-subset(x,EXCLUDE_FLAG==0);head(subset(x,EXCLUDE_FLAG==-1))# this dataframe should be empty


# Assign TAXONCODE --------------------------------------------------------
#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
taxa<-read.csv("2013-18_Taxa_MASTER.csv")

nrow(x)

#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
x.<-Convert_to_Taxoncode(x)

#Check to see whether there are hard corals that have a SPCODE and GENUSCODE but no S_ORDER
test<-x.[is.na(x.$S_ORDER),];test<-droplevels(test)
levels(test$SPCODE) #there should be not hard corals in this list

#Create a list Species codes and associated genera
SURVEY_INFO<-c("OBS_YEAR","SPCODE","TAXONCODE","GENUS_CODE","TAXONNAME")
test<-new_Aggregate_InputTable(x., SURVEY_INFO)
test

#Change columns to character
x.$GENUS_CODE<-as.character(x.$GENUS_CODE)
x.$SPCODE<-as.character(x.$SPCODE)
x.$TAXONCODE<-as.character(x.$TAXONCODE)
x.$S_ORDER<-as.character(x.$S_ORDER)


#There are some SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genus or taxoncodes to the spcode and the remaining NAs in the Taxon and genus code to AAAA

x.$GENUS_CODE<-ifelse(is.na(x.$GENUS_CODE)&x.$S_ORDER=="Scleractinia",x.$SPCODE,x.$GENUS_CODE)
x.$TAXONCODE<-ifelse(is.na(x.$TAXONCODE)&x.$S_ORDER=="Scleractinia",x.$SPCODE,x.$TAXONCODE)
x.$GENUS_CODE<-ifelse(x.$TAXONCODE=="UNKN","UNKN",x.$GENUS_CODE)
x.$GENUS_CODE<-ifelse(x.$TAXONCODE=="AAAA","AAAA",x.$GENUS_CODE)

#utils::View(x) #view data in separate window

#Check that Unknown scl were changed correctly
test<-subset(x.,TAXONCODE=="UNKN"&S_ORDER=="Scleractinia");head(test)
test<-subset(x.,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia");head(test)
test<-subset(x.,GENUS_CODE=="AAAA");head(test)
test<-subset(x.,SPCODE=="AAAA");head(test)

#Confirm that no rows were dropped during merge
nrow(x)
nrow(x.)
x<-x.

# #Create new colummns that combine species, genus and morphology
# x$TAXMORPH<-paste(x$TAXONCODE,x$MORPH_CODE,sep="")
# x$GENMORPH<-paste(x$GENUS_CODE,x$MORPH_CODE,sep="")


#add SITE MASTER information to x 
#x<-merge(x, site_master[,c("SITE", "SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE) #Fish team's original code, we may want to create analysis scheme later in the 
length(unique(x$SITEVISITID)) #double check that sites weren't dropped
nrow(subset(site_master,METHOD %in% c("CORALBELT_METHOD_E_F","CORALBELT_METHOD_F_PHOTOQUADS")))
colnames(site_master)
x<-merge(x, site_master[,c("OBS_YEAR","SITEVISITID","SITE","SEC_NAME","ANALYSIS_YEAR")], by=c("OBS_YEAR","SITEVISITID","SITE"),all.x=T)  
length(unique(x$SITEVISITID)) #double check that sites weren't dropped
head(x)
write.csv(x,"test.csv")

#CHECK THAT all SEC_CODES are present in the site_master file
test<-x[is.na(x$SEC_NAME), c("MISSIONID","REGION", "SITE","OBS_YEAR"),]
if(dim(test)[1]>0) {cat("sites with MISSING SECTORS present")}   # should be 0

###NWHI 2014 and 2015 sites missing for now

#If there are missing sectors, generate a table of missing sites, lat, long, reef zone and depth bins. Manually correct Site Master file
a<-subset(x,is.na(x$SEC_NAME))
a<-a[c("OBS_YEAR","SITE","LATITUDE", "LONGITUDE","REEF_ZONE","DEPTH_BIN")]
test<-unique(a)
write.csv(test,"missingsectors.csv")

##Calcuating segment and transect area and add column for transect area
x<-Transectarea(x)
# sapply(x,levels)
head(x)
nrow(x)


## CLEAN UP NAs 
DATE_<-x$DATE_
head(x$DATE_) 
x<-subset(x,select =-c(DATE_))
x[x=="-9"]<-NA
x<-cbind(x,DATE_);head(x)

head(x)

jwd<-droplevels(x)


#Final Tweaks before calculating Site-level data-------------------------------------------------
#Colony fragments and scleractinans are subseted in the functions 

#Add a column for adult fragments so we can remove them from the dataset later (-1 indicates fragment)
# awd<-CreateFragment(awd)
awd$Fragment<-ifelse(awd$OBS_YEAR <2018 & awd$COLONYLENGTH <5 & awd$S_ORDER=="Scleractinia",-1,awd$Fragment)
head(subset(awd,Fragment==-1& OBS_YEAR<2018)) #double check that pre 2018 fragments create
awd$Fragment[is.na(awd$Fragment)] <- 0
#head(subset(awd,Fragment==-1 & OBS_YEAR==2018))#double check that post 2018 fragments are created
jwd$Fragment<-0 # you need to add this column so that you can use the site level functions correctly

#Remove transects with less than 5m surveyed and check how many rows were removed
nrow(awd)
awd<-subset(awd,TRANSECTAREA>=5) 
jwd<-subset(jwd,TRANSECTAREA>=1)
nrow(awd)


#Create a look a table of all of the colony attributes- you will need this for the Calc_RDden and Calc_Condden functions
SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH","TRANSECT","COLONYID","GENUS_CODE","TAXONCODE","COLONYLENGTH")
survey_colony<-new_Aggregate_InputTable(awd, SURVEY_INFO)
SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH")
survey_site<-new_Aggregate_InputTable(awd, SURVEY_INFO)

write.csv(survey_site,"test.csv")

# GENERATE SUMMARY METRICS at the transect-level  --------------------------------------------------

acd.gen<-Calc_ColDen_Transect(awd,"TAXONCODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen"# calculate density at genus level as well as total
od.gen<-Calc_ColMetric_Transect(awd,"TAXONCODE","OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.gen<-Calc_ColMetric_Transect(awd,"TAXONCODE",c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead
rdden.gen<-Calc_RDden_Transect(awd,"TAXONCODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
condden.gen<-Calc_CONDden_Transect(awd,"TAXONCODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
acutedz.gen<-subset(rdden.gen,select = c(SITEVISITID,SITE,TRANSECT,TAXONCODE,DZGNS)) #subset just bleached colonies
ble.gen<-subset(condden.gen,select = c(SITEVISITID,SITE,TRANSECT,TAXONCODE,BLE)) #subset just bleached colonies
chronicdz.gen<-subset(condden.gen,select = c(SITEVISITID,SITE,TRANSECT,TAXONCODE,CHRO)) #subset just bleached colonies
jcd.gen<-Calc_ColDen_Transect(jwd,"TAXONCODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen"
rich.gen<-Calc_Richness_Transect(awd,"TAXONCODE")

#ADD CODE TO CHANGE TRANSECT NUMBERS FOR JUVENILES
jcd.gen$TRANSECT[jcd.gen$TRANSECT==3]<-1
jcd.gen$TRANSECT[jcd.gen$TRANSECT==4]<-2


#Merge density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","TAXONCODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.gen<-Reduce(MyMerge, list(acd.gen,od.gen,rd.gen,jcd.gen,acutedz.gen,chronicdz.gen,ble.gen));

head(data.gen)

#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.gen$JuvColCount[is.na(data.gen$JuvColCount)]<-0;data.gen$JuvColDen[is.na(data.gen$JuvColDen)]<-0
data.gen$AdColCount[is.na(data.gen$AdColCount)]<-0;data.gen$AdColDen[is.na(data.gen$AdColDen)]<-0


#GENERATE SITE-LEVEL DATA BY AVERAGING TRANSECTS-----------------------------------
#Since we are moving to a 1 stage design, we need to summarize the transects before rolling up to site.
#Dione suggested that we calculate mean of 2 transects rather than pooling or dropping a transect

#define columns you would like to average
data.cols<-c("AdColDen","Ave.size","Ave.od","Ave.rd","JuvColDen","BLE","ActueDZ","ChronicDZ")


#now average metrics to site level
#Can't get the aggregate to work properly because we have NAs in the data and we need to retain the 0 values in the density column
#This is still clunky, but works for now.
site.data.gen<-ddply(data.gen, .(SITE,SITEVISITID,TAXONCODE), #calc total colonies by condition
                     summarise,
                     AdColCount=sum(AdColCount,na.rm=T),AdColDen=mean(AdColDen,na.rm = T),Ave.od=mean(Ave.od,na.rm = T),
                     Ave.rd=mean(Ave.rd,na.rm = T),JuvColDen=mean(JuvColDen,na.rm=T),BLE=mean(BLE,na.rm=T),AcuteDZ=mean(DZGNS,na.rm=T),ChronicDZ=mean(CHRO,na.rm=T))


# get strata and sectors data. Note, this is the benthic sector/area file. we are still working on properly merging fish and benthic files.
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

#Generate a table of metadata at the transect and site level for ADULTS
# SURVEY_INFO<-c("SITEVISITID", "ANALYSIS_YEAR","OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME","SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT","TRANSECT")
# survey_transect<-Aggregate_InputTable(awd, SURVEY_INFO)

SURVEY_INFO<-c("SITEVISITID","ANALYSIS_YEAR", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH")
survey_site<-new_Aggregate_InputTable(awd, SURVEY_INFO)
write.csv(survey_site,"test2.csv")

#Merge together survey meta data and sector area files and check for missmatches 
meta<-merge(survey_site,sectors,by=c("REGION","SEC_NAME","ISLAND","REEF_ZONE","DEPTH_BIN"),all.x=TRUE)
meta[which(is.na(meta$AREA_HA)),]

#Merge site level data and meta data
site.data.gen<-merge(site.data.gen,meta,by=c("SITEVISITID","SITE"),all.x=TRUE)

write.csv(site.data.gen,"T:/Benthic/Data/BenthicREA_TAXONsitedata_2013-2018.csv")
