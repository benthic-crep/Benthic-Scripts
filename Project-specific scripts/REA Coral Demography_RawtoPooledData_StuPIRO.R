
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


#Change columns to character
x$GENUS_CODE<-as.character(x$GENUS_CODE)
x$SPCODE<-as.character(x$SPCODE)
x$S_ORDER<-as.character(x$S_ORDER)

# Assign TAXONCODE --------------------------------------------------------
#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
taxa<-read.csv("2013-18_Taxa_MASTER.csv")

#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
x<-Convert_to_Taxoncode(x)

#There are some SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genera or taxoncodes to the spcode and the remaining NAs in the Taxon and genus code to AAAA
x$GENUS_CODE<-ifelse(is.na(x$GENUS_CODE)&x$S_ORDER=="Scleractinia",x$SPCODE,x$GENUS_CODE)
x$TAXONCODE<-ifelse(is.na(x$TAXONCODE)&x$S_ORDER=="Scleractinia",x$SPCODE,x$TAXONCODE)
x$GENUS_CODE<-ifelse(x$TAXONCODE=="UNKN","UNKN",x$GENUS_CODE)
x$GENUS_CODE<-ifelse(x$SPCODE=="AAAA","AAAA",x$GENUS_CODE)
x$TAXONCODE<-ifelse(x$SPCODE=="AAAA","AAAA",x$TAXONCODE)

#utils::View(x) #view data in separate window

#Check that Unknown scl were changed correctly
test<-subset(x,TAXONCODE=="UNKN"&S_ORDER=="Scleractinia");head(test)
test<-subset(x,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia");head(test)
test<-subset(x,TAXONCODE=="AAAA");head(test)
test<-subset(x,SPCODE=="AGLO"& OBS_YEAR=="2018");head(test)



#add SITE MASTER information to x 
#x<-merge(x, site_master[,c("SITE", "SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE) #Fish team's original code, we may want to create analysis scheme later in the 
length(unique(x$SITEVISITID)) #double check that sites weren't dropped
nrow(subset(site_master,METHOD %in% c("CORALBELT_METHOD_E_F","CORALBELT_METHOD_F_PHOTOQUADS")))

x<-merge(x, site_master[,c("METHOD","OBS_YEAR","SITEVISITID","SITE","SEC_NAME","ANALYSIS_YEAR")], by=c("OBS_YEAR","SITEVISITID","SITE"),all.y=T)  
length(unique(x$SITEVISITID)) #double check that sites weren't dropped

write.csv(x,"test.csv")
#CHECK THAT all SEC_NAME are present in the site_master file
test<-x[is.na(x$SEC_NAME), c("MISSIONID","REGION", "SITE","OBS_YEAR"),]
test<-droplevels(test);table(test$SITE,test$MISSIONID) #create a table of missing sites by missionid
if(dim(test)[1]>0) {cat("sites with MISSING SECTORS present")}   # should be 0

#Create a list of missing sites that can be inported into the SITE MASTER file if needed
test<-x[is.na(x$SEC_NAME),]
miss.sites<-ddply(test,.(OBS_YEAR,SITEVISITID,SITE,MISSIONID,REGION,REGION_NAME,ISLAND,LATITUDE,LONGITUDE,
                         REEF_ZONE,DEPTH_BIN,DATE_,EXCLUDE_FLAG,HABITAT_CODE),
                  summarize,temp=median(SITEVISITID),SITE_MAX_DEPTH=median(SITE_MAX_DEPTH),SITE_MIN_DEPTH=median(SITE_MIN_DEPTH))
head(miss.sites,20)


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
write.csv(awd,"CoralBelt_E_raw.csv")


SURVEY_INFO<-c("S_ORDER","GENUS_CODE","TAXONCODE","TAXONNAME")
test<-new_Aggregate_InputTable(awd, SURVEY_INFO)


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



#Change columns to character
x$GENUS_CODE<-as.character(x$GENUS_CODE)
x$SPCODE<-as.character(x$SPCODE)
x$S_ORDER<-as.character(x$S_ORDER)

taxa<-read.csv("2013-18_Taxa_MASTER.csv")

#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
x<-Convert_to_Taxoncode(x)

#There are some SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genera or taxoncodes to the spcode and the remaining NAs in the Taxon and genus code to AAAA
x$GENUS_CODE<-ifelse(is.na(x$GENUS_CODE)&x$S_ORDER=="Scleractinia",x$SPCODE,x$GENUS_CODE)
x$TAXONCODE<-ifelse(is.na(x$TAXONCODE)&x$S_ORDER=="Scleractinia",x$SPCODE,x$TAXONCODE)
x$GENUS_CODE<-ifelse(x$TAXONCODE=="UNKN","UNKN",x$GENUS_CODE)
x$GENUS_CODE<-ifelse(x$SPCODE=="AAAA","AAAA",x$GENUS_CODE)
x$TAXONCODE<-ifelse(x$SPCODE=="AAAA","AAAA",x$TAXONCODE)

#utils::View(x) #view data in separate window

#Check that Unknown scl were changed correctly
test<-subset(x,TAXONCODE=="UNKN"&S_ORDER=="Scleractinia");head(test)
test<-subset(x,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia");head(test)
test<-subset(x,TAXONCODE=="AAAA");head(test)
test<-subset(x,SPCODE=="AGLO"& OBS_YEAR=="2018");head(test)

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
write.csv(test,"missingsites.csv")
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
write.csv(jwd,"CoralBelt_F_raw.csv")


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

#Duplicate dataframe because the ddply step above takes a while to create. Allows you to tweak code below without having to rerun the ddply step above
site.data.gen2<-site.data.gen

# get strata and sectors data. Note, this is the benthic sector/area file. we are still working on properly merging fish and benthic files.
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

#Generate a table of metadata at the transect and site level for ADULTS
# SURVEY_INFO<-c("SITEVISITID", "ANALYSIS_YEAR","OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME","SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT","TRANSECT")
# survey_transect<-Aggregate_InputTable(awd, SURVEY_INFO)

SURVEY_INFO<-c("SITEVISITID","ANALYSIS_YEAR", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH")
survey_site<-new_Aggregate_InputTable(awd, SURVEY_INFO)
#write.csv(survey_site,"test2.csv")

#Merge together survey meta data and sector area files and check for missmatches 
meta<-merge(survey_site,sectors,by=c("REGION","SEC_NAME","ISLAND","REEF_ZONE","DEPTH_BIN"),all.x=TRUE)
meta[which(is.na(meta$AREA_HA)),]


#Merge site level data and meta data
site.data.gen2<-merge(site.data.gen2,meta,by=c("SITEVISITID","SITE"),all.x=TRUE)
rich.data<-merge(rich.gen,meta,by=c("SITEVISITID","SITE"),all.x=TRUE)

#write.csv(site.data.gen,"T:/Benthic/Data/BenthicREA_TAXONsitedata_2013-2018.csv")

site.data.gen2$Adpres.abs<-ifelse(site.data.gen2$AdColDen>0,1,0)
site.data.gen2$Juvpres.abs<-ifelse(site.data.gen2$JuvColDen>0,1,0)

write.csv(site.data.gen2,"BenthicREA_sitedata.csv")

site.data.gen2<-read.csv("BenthicREA_sitedata.csv")

# POOLING DATA from Site to Strata and Domain---------------------------------------------------
site.data.gen2<-PoolSecStrat(site.data.gen2)
rich.data<-PoolSecStrat(rich.data)

#QC CHECK to make sure the sectors and strata pooled correctly
rich.test<-ddply(rich.data,.(REGION,BEN_SEC,OBS_YEAR,STRATANAME),summarize,n=length(SITE))
data.test<-ddply(subset(site.data.gen2,TAXONCODE=="SSSS"),.(REGION,BEN_SEC,OBS_YEAR,STRATANAME),summarize,n=length(SITE))
sm.test<-ddply(subset(site_master,METHOD=="CORALBELT_METHOD_E_F"),.(REGION,ISLAND,SEC_NAME,OBS_YEAR,REEF_ZONE,DEPTH_BIN),summarize,n=length(SITE))

write.csv(data.test,"tmp_sitedataQC.csv")
write.csv(sm.test,"tmp_sitemasterQC.csv")

#Subset just Forereef Sites & just target taxa
site.data.gen2<-subset(site.data.gen2,REEF_ZONE=="Forereef")
#site.data.gen2<-subset(site.data.gen2,TAXONCODE %in% c("ACSP", "MOSP", "PAVS", "POCS","POSP","SSSS"))
rich.data<-subset(rich.data,REEF_ZONE=="Forereef")

#Make sure you everything but forereef are dropped
table(site.data.gen2$REEF_ZONE,site.data.gen2$TAXONCODE)
table(rich.data$REEF_ZONE)


#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
site.data.gen2$ANALYSIS_SCHEMA<-site.data.gen2$STRATANAME
site.data.gen2$DOMAIN_SCHEMA<-site.data.gen2$BEN_SEC
rich.data$ANALYSIS_SCHEMA<-rich.data$STRATANAME
rich.data$DOMAIN_SCHEMA<-rich.data$BEN_SEC


#Calculate metrics at Strata-level-We need to work on combining metrics into 1 function

#Create a vector of columns to subset for strata estimates
c.keep<-c("REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","TAXONCODE",
          "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep2<-c("REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","TAXONCODE",
           "n_h","N_h","D._h","SE_D._h")
acdG_st<-Calc_Strata(site.data.gen2,"TAXONCODE","AdColDen","Adpres.abs");acdG_st=acdG_st[,c.keep]
colnames(acdG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","TAXONCODE","n","Ntot","AdColDen","SE_AdColDen","Adult_avp","Adult_seprop","Adult_Abun","Adult_SE_Abun","Adult_CV")

jcdG_st<-Calc_Strata(site.data.gen2,"TAXONCODE","JuvColDen","Juvpres.abs");jcdG_st=jcdG_st[,c.keep]
colnames(jcdG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","TAXONCODE","n","Ntot","JuvColDen","SE_JuvColDen")

odG_st<-Calc_Strata(site.data.gen2,"TAXONCODE","Ave.od");odG_st=odG_st[,c.keep2]
colnames(odG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","TAXONCODE","n","Ntot","Ave.od","SE_Ave.od")

c.keep<-c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","n_h","N_h","D._h","SE_D._h")
rich_st<-Calc_Strata_Cover_Rich(rich.data,"Richness");rich_st=rich_st[,c.keep]
colnames(rich_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Sector","Stratum","n","Ntot","Richness","SE_Richess") 


#Double Check that revised pooling is adding up NH (total sites) correctly
head(acdG_st,20)
subset(sectors,ISLAND=="Kingman") #Insert whichever island/strata you want to check.

#Add in CalcAnalayis_strata & island



#Calculate Regional Estimates
acdG_is<-Calc_Domain(site.data.gen2,"TAXONCODE","AdColDen","Adpres.abs")
acdG_is<-acdG_is[,c("ANALYSIS_YEAR","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_AdColDen","SE_AdColDen")]
jcdG_is<-Calc_Domain(site.data.gen2,"TAXONCODE","JuvColDen","Juvpres.abs")
jcdG_is<-jcdG_is[,c("ANALYSIS_YEAR","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen")]
odG_is<-Calc_Domain(site.data.gen2,"TAXONCODE","Ave.od");odG_is<-odG_is[,c("ANALYSIS_YEAR","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_Ave.od","SE_Ave.od")]
rich_is<-Calc_Domain_Cover_Rich(rich.data,"Richness");colnames(rich_is)[colnames(rich_is)=="DOMAIN_SCHEMA"]<-"Sector"


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","TAXONCODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
st.data.gen<-Reduce(MyMerge, list(acdG_st,jcdG_st))
colnames(st.data.gen)[colnames(st.data.gen)=="ANALYSIS_SCHEMA"]<-"Stratum"


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("ANALYSIS_YEAR","DOMAIN_SCHEMA","TAXONCODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
is.data.gen<-Reduce(MyMerge, list(acdG_is,jcdG_is))
colnames(is.data.gen)[colnames(is.data.gen)=="DOMAIN_SCHEMA"]<-"Sector"

#Merge Taxon Name
load("ALL_REA_ADULTCORAL_RAW_new.rdata") #from oracle
x<-df #leave this as df
tax.name<-subset(x,S_ORDER=="Scleractinia")
tax.name<-ddply(tax.name,.(TAXONCODE,TAXONNAME), #create a list of TAXONNAMES
                summarise,
                count=length(REGION))

levels(tax.name)<-c(levels(tax.name$TAXONCODE), "SSSS")
levels(tax.name)<-c(levels(tax.name$TAXONNAME), "Total Hard Coral")

a <- data.frame(TAXONCODE='SSSS',TAXONNAME='Total Hard Coral',count='1') 
tax.name<-rbind(tax.name,a)
tail(tax.name)

st.data.gen<-merge(st.data.gen,tax.name,by="TAXONCODE")
is.data.gen<-merge(is.data.gen,tax.name,by="TAXONCODE")

write.csv(st.data.gen,"Pacificwide_demography_frf_str4_TAXON.csv")
write.csv(is.data.gen,"Pacificwide_demography_frf_sec4_TAXON.csv")





#Things to work on
#1. put pooling changes into csv file rather than write them out in text, too clunky and easy to get confused with different years
#2. Separate rz/db from sector name so have a column for sector and stratum- this will allow us to subset just certain depths and zone more easily later on.
#3. Make sure that ntot added up correctly across years and domains


