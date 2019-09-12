
#CREATE ADULT CLEAN ANALYSIS READY DATA----------------------------------------
# This script will clean the raw benthic REA data using method E that comes directly from the new data base application.
#Note- these data represent the revised data structure insituted in November 2018. Several recent dead and condition columns were added
#These data only include surveys conducted between 2013-2018
rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_ADULTCORAL_RAW_2013-2018.rdata") #from oracle
x<-df #leave this as df

x$SITE<-SiteNumLeadingZeros(x$SITE) # Change site number such as MAR-22 to MAR-0022

#Convert date formats
class(x$DATE_)
x$DATE_ <- as.Date(x$DATE_, format = "%Y-%m-%d")


### Use these functions to look at data
head(x)
tail(x)
table(x$REGION, x$OBS_YEAR) #review years and regions in dataframe


#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","REGION_NAME","ISLAND","ISLANDCODE","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","NO_SURVEY_YN","EXCLUDE_FLAG","SITEVISITID","HABITAT_CODE","DIVER","TRANSECTNUM","SEGMENT","SEGWIDTH","SEGLENGTH","FRAGMENT_YN",
             "COLONYID","TAXONCODE","COLONYLENGTH","OLDDEAD",
             "RECENTDEAD_1","RECENT_GENERAL_CAUSE_CODE_1","RECENT_SPECIFIC_CAUSE_CODE_1",
             "RECENTDEAD_2",	"RECENT_GENERAL_CAUSE_CODE_2","RECENT_SPECIFIC_CAUSE_CODE_2",	
             "RECENT_GENERAL_CAUSE_CODE_3","RECENT_SPECIFIC_CAUSE_CODE_3","RECENTDEAD_3","COND",
             "CONDITION_2","CONDITION_3","EXTENT_1","EXTENT_2","EXTENT_3","SEVERITY_1","SEVERITY_2","SEVERITY_3",
             "GENUS_CODE","S_ORDER","TAXONNAME","SITE_MIN_DEPTH","SITE_MAX_DEPTH")


#remove extraneous columns
head(x[,DATA_COLS])
x<-x[,DATA_COLS]
sort(colnames(x))

#Double check level and class of variables to make sure there aren't any errors
sapply(x,levels)
sapply(x,class)##Change column names to make code easier to code

colnames(x)[colnames(x)=="TAXONCODE"]<-"SPCODE" #Change column name- we will eventually change this column back to "taxoncode" after we modify the spcode names to match the taxalist we all feel comfortable identifying
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


# Merge Adult data and  SURVEY MASTER -------------------------------------
#SURVEY MASTER was created by Ivor and Courtney by extracting sites directly from the Site Visit table from Oracle. It should be the complete list of sites surveyed since 2000
survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")


#add SITE MASTER information to x 
length(unique(x$SITEVISITID)) #check the number of sites in demographic data

x<-merge(x, survey_master[,c("OBS_YEAR","SITEVISITID","SITE","SEC_NAME","ANALYSIS_YEAR","bANALYSIS_SCHEME")], by=c("OBS_YEAR","SITEVISITID","SITE"),all.x=T)  
length(unique(x$SITEVISITID)) #double check that sites weren't dropped

write.csv(x,"test.csv")

#CHECK THAT all SEC_NAME are present in the survey_master file
test<-x[is.na(x$SEC_NAME), c("MISSIONID","REGION", "SITE","OBS_YEAR"),]
test<-droplevels(test);table(test$SITE,test$MISSIONID) #create a table of missing sites by missionid
if(dim(test)[1]>0) {cat("sites with MISSING SECTORS present")}   # should be 0

#Missing sites from MP1410 and MP1512 (2014 Maui special projects)

#Create a list of missing sites that can be inported into the SITE MASTER file if needed
test<-x[is.na(x$SEC_NAME),]
miss.sites<-ddply(test,.(OBS_YEAR,SITEVISITID,SITE,MISSIONID,REGION,REGION_NAME,ISLAND,LATITUDE,LONGITUDE,
                         REEF_ZONE,DEPTH_BIN,DATE_,EXCLUDE_FLAG,HABITAT_CODE),
                  summarize,temp=median(SITEVISITID),SITE_MAX_DEPTH=median(SITE_MAX_DEPTH),SITE_MIN_DEPTH=median(SITE_MIN_DEPTH))
head(miss.sites,20)

# CLEAN UP ----------------------------------------------------------------

##Remove sites that were only surveyed for photoquads but not demographics
#Note-photoquad only sites are not included in data prior to 2018
#Test whether there are missing values in the NO_SURVEY_YN column. The value should be 0 or -1

x.na<-x[is.na(x$NO_SURVEY_YN)&x$OBS_YEAR>2013,]
x.na
# test<-ddply(x.na,.(SITE),
#             summarize,
#             SEG=length(unique(SEGMENT)))
# test
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
taxa<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/REA_CoralDemography/2013-18_Taxa_MASTER.csv")

#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
x.<-Convert_to_Taxoncode(x)

#Check to make sure SPCODE was converted correctly
head(x.[x.$SPCODE!=x.$TAXONCODE,])

#If there are issues use this code to create a list SPCODE (lowest taxonomic resolution we have), TAXONCODE (the taxonomic level we all feel comfortable with) and associated genera
#This is used for spot checking that TAXONCODE was converted properly & can be compared against TAXA MASTER 
SURVEY_INFO<-c("OBS_YEAR","REGION","SPCODE","TAXONCODE","GENUS_CODE","TAXONNAME")
test<-new_Aggregate_InputTable(x., SURVEY_INFO)
head(test)

#Check to see whether S_ORDER is NA and not AAAA (the code for no colonies observed on the segment)
x.[x.$SPCODE!="AAAA"& is.na(x.$S_ORDER),] #this dataframe should be empty


#Change columns to character
x.$GENUS_CODE<-as.character(x.$GENUS_CODE)
x.$SPCODE<-as.character(x.$SPCODE)
x.$TAXONCODE<-as.character(x.$TAXONCODE)
x.$S_ORDER<-as.character(x.$S_ORDER)

#Make sure there are no NA values in genus code or taxoncode if it's supposed to be a scleractinian
subset(x.,S_ORDER=="Scleractinia" & GENUS_CODE=="NA") #this dataframe should be empty
subset(x.,S_ORDER=="Scleractinia" & TAXONCODE=="NA") #this dataframe should be empty

#Fix missing NAs if need be
# x.$GENUS_CODE<-ifelse(is.na(x.$GENUS_CODE)&x.$S_ORDER=="Scleractinia",x.$SPCODE,x.$GENUS_CODE)
# x.$TAXONCODE<-ifelse(is.na(x.$TAXONCODE)&x.$S_ORDER=="Scleractinia",x.$SPCODE,x.$TAXONCODE)

#There are some old SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genera or taxoncodes to the spcode and the remaining NAs in the Taxon and genus code to AAAA
x.$GENUS_CODE<-ifelse(x.$TAXONCODE=="UNKN","UNKN",x.$GENUS_CODE)
x.$TAXONCODE<-ifelse(x.$SPCODE=="AAAA","AAAA",x.$TAXONCODE)
x.$GENUS_CODE<-ifelse(x.$TAXONCODE=="AAAA","AAAA",x.$GENUS_CODE)
x.$TAXONCODE<-ifelse(x.$SPCODE %in% c("MOAS","LEPA"),"UNKN",x.$TAXONCODE)
x.$GENUS_CODE<-ifelse(x.$SPCODE %in% c("MOAS","LEPA"),"UNKN",x.$GENUS_CODE)


View(x) #view data in separate window

#Check that Unknown scl were changed correctly
head(subset(x.,TAXONCODE=="UNKN"&S_ORDER=="Scleractinia"),40)
head(subset(x.,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia"))
head(subset(x.,GENUS_CODE=="AAAA"))
head(subset(x.,SPCODE=="AAAA"))

#Confirm that no rows were dropped 
nrow(x)
nrow(x.)


##Calcuating segment and transect area and add column for transect area
x<-Transectarea(x.)
# sapply(x,levels)
head(x)
nrow(x)


## CLEAN UP NAs ##
x[,27:46][x[,27:46] ==-9] <- NA #Convert missing numeric values to NA (they are entered as -9 in Oracle)

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
#write.csv(awd,file="T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_E_raw_CLEANED.csv")


## CREATE JUVENILE CLEAN ANALYSIS READY DATA ----
## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_JUVCORAL_RAW_2013-2018.rdata") #from oracle
x<-df #leave this as df

x$SITE<-SiteNumLeadingZeros(x$SITE) # Change site number such as MAR-22 to MAR-0022

#Convert date formats
class(x$DATE_)
x$DATE_ <- as.Date(x$DATE_, format = "%Y-%m-%d")

### Use these functions to look at data
head(x)
tail(x)
table(x$REGION, x$OBS_YEAR) #review years and regions in dataframe

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


# Merge Juvenile data and SITE MASTER -------------------------------------
# load site master to merge with demographic data
survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")

#add SITE MASTER information to x 
#x<-merge(x, survey_master[,c("SITE", "SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE) #Fish team's original code, we may want to create analysis scheme later in the 
length(unique(x$SITEVISITID)) #double check that sites weren't dropped

x<-merge(x, survey_master[,c("OBS_YEAR","SITEVISITID","SITE","SEC_NAME","ANALYSIS_YEAR","bANALYSIS_SCHEME")], by=c("OBS_YEAR","SITEVISITID","SITE"),all.x=T)  
length(unique(x$SITEVISITID)) #double check that sites weren't dropped

write.csv(x,"test.csv")

#CHECK THAT all SEC_NAME are present in the survey_master file
test<-x[is.na(x$SEC_NAME), c("MISSIONID","REGION", "SITE","OBS_YEAR"),]
test<-droplevels(test);table(test$SITE,test$MISSIONID) #create a table of missing sites by missionid
if(dim(test)[1]>0) {cat("sites with MISSING SECTORS present")}   # should be 0

#Create a list of missing sites that can be inported into the SITE MASTER file if needed
test<-x[is.na(x$SEC_NAME),]
miss.sites<-ddply(test,.(OBS_YEAR,SITEVISITID,SITE,MISSIONID,REGION,REGION_NAME,ISLAND,LATITUDE,LONGITUDE,
                         REEF_ZONE,DEPTH_BIN,DATE_,EXCLUDE_FLAG,HABITAT_CODE),
                  summarize,temp=median(SITEVISITID),SITE_MAX_DEPTH=median(SITE_MAX_DEPTH),SITE_MIN_DEPTH=median(SITE_MIN_DEPTH))
head(miss.sites,20)


# CLEAN UP ----------------------------------------------------------------

##Remove sites that were only surveyed for photoquads but not demographics
#Note-photoquad only sites are not included in data prior to 2018
#Test whether there are missing values in the NO_SURVEY_YN column. The value should be 0 or -1
x.na<-x[is.na(x$NO_SURVEY_YN)&x$OBS_YEAR>2013,]
x.na
# test<-ddply(x.na,.(SITE),
#             summarize,
#             SEG=length(unique(SEGMENT)))
# test
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
taxa<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/REA_CoralDemography/2013-18_Taxa_MASTER.csv")

#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
x.<-Convert_to_Taxoncode(x)

#Check to make sure SPCODE was converted correctly
head(x.[x.$SPCODE!=x.$TAXONCODE,])

#If there are issues use this code to create a list SPCODE (lowest taxonomic resolution we have), TAXONCODE (the taxonomic level we all feel comfortable with) and associated genera
#This is used for spot checking that TAXONCODE was converted properly & can be compared against TAXA MASTER 
SURVEY_INFO<-c("OBS_YEAR","REGION","SPCODE","TAXONCODE","GENUS_CODE","TAXONNAME")
test<-new_Aggregate_InputTable(x., SURVEY_INFO)
head(test)

#Check to see whether S_ORDER is NA and not AAAA (the code for no colonies observed on the segment)
x.[x.$SPCODE!="AAAA"& is.na(x.$S_ORDER),] #this dataframe should be empty


#Change columns to character
x.$GENUS_CODE<-as.character(x.$GENUS_CODE)
x.$SPCODE<-as.character(x.$SPCODE)
x.$TAXONCODE<-as.character(x.$TAXONCODE)
x.$S_ORDER<-as.character(x.$S_ORDER)

#Make sure there are no NA values in genus code or taxoncode if it's supposed to be a scleractinian
subset(x.,S_ORDER=="Scleractinia" & GENUS_CODE=="NA") #this dataframe should be empty
subset(x.,S_ORDER=="Scleractinia" & TAXONCODE=="NA") #this dataframe should be empty

#Fix missing NAs if need be
# x.$GENUS_CODE<-ifelse(is.na(x.$GENUS_CODE)&x.$S_ORDER=="Scleractinia",x.$SPCODE,x.$GENUS_CODE)
# x.$TAXONCODE<-ifelse(is.na(x.$TAXONCODE)&x.$S_ORDER=="Scleractinia",x.$SPCODE,x.$TAXONCODE)

#There are some old SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genera or taxoncodes to the spcode and the remaining NAs in the Taxon and genus code to AAAA
x.$GENUS_CODE<-ifelse(x.$TAXONCODE=="UNKN","UNKN",x.$GENUS_CODE)
x.$TAXONCODE<-ifelse(x.$SPCODE=="AAAA","AAAA",x.$TAXONCODE)
x.$GENUS_CODE<-ifelse(x.$TAXONCODE=="AAAA","AAAA",x.$GENUS_CODE)
x.$TAXONCODE<-ifelse(x.$SPCODE %in% c("MOAS","LEPA"),"UNKN",x.$TAXONCODE)
x.$GENUS_CODE<-ifelse(x.$SPCODE %in% c("MOAS","LEPA"),"UNKN",x.$GENUS_CODE)

View(x) #view data in separate window

#Check that Unknown scl were changed correctly
head(subset(x.,TAXONCODE=="UNKN"&S_ORDER=="Scleractinia"),40)
head(subset(x.,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia"))
head(subset(x.,GENUS_CODE=="AAAA"))
head(subset(x.,SPCODE=="AAAA"))

#Confirm that no rows were dropped 
nrow(x)
nrow(x.)



##Calcuating segment and transect area and add column for transect area
x<-Transectarea(x.)
# sapply(x,levels)
head(x)
nrow(x)


## CLEAN UP NAs ##
x[,26:29][x[,26:29] ==-9] <- NA #Convert missing numeric values to NA (they are entered as -9 in Oracle)


jwd<-droplevels(x)
#write.csv(Jwd,file="T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_F_raw_CLEANED.csv")


#Final Tweaks before calculating Site-level data-------------------------------------------------
#Colony fragments and scleractinans are subseted in the functions 

#Add a column for adult fragments so we can remove them from the dataset later (-1 indicates fragment)
# awd<-CreateFragment(awd)
awd$Fragment<-ifelse(awd$OBS_YEAR <2018 & awd$COLONYLENGTH <5 & awd$S_ORDER=="Scleractinia",-1,awd$Fragment)
head(subset(awd,Fragment==-1& OBS_YEAR<2018)) #double check that pre 2018 fragments create
awd$Fragment[is.na(awd$Fragment)] <- 0
jwd$Fragment<-0 # you need to add this column so that you can use the site level functions correctly


#Modify bleaching severity measure based on recent changes
x$SEVERITY_1n<-NULL
x$SEVERITY_2n<-NULL
x$SEVERITY_3n<-NULL

for (i in c(1:nrow(x))){ #opening brace
  if(x$SEVERITY_1[i] =="1"){ #c&p
    x$SEVERITY_1n[i] = "1" #c&p
  } #c&p
  if(x$SEVERITY_1[i]==2|x$SEVERITY_1[i]==3){ #c&p
    x$SEVERITY_1n[i]= "2" #c&p
  } #c&p
  if(x$SEVERITY_1[i]==4|x$SEVERITY_1[i]==5){ #c&p
    x$SEVERITY_1n[i]= "3" #c&p
  } #c&p
  if(x$SEVERITY_2[i] =="1"){ #c&p
    x$SEVERITY_2n[i] = "1" #c&p
  } #c&p
  if(x$SEVERITY_2[i]==2|x$SEVERITY_2[i]==3){ #c&p
    x$SEVERITY_2n[i]= "2" #c&p
  } #c&p
  if(x$SEVERITY_2[i]==4|x$SEVERITY_2[i]==5){ #c&p
    x$SEVERITY_2n[i]= "3" #c&p
  } #c&p
  if(x$SEVERITY_3[i] =="1"){ #c&p
    x$SEVERITY_3n[i] = "1" #c&p
  } #c&p
  if(x$SEVERITY_3[i]==2|x$SEVERITY_3[i]==3){ #c&p
    x$SEVERITY_3n[i]= "2" #c&p
  } #c&p
  if(x$SEVERITY_2[i]==4|x$SEVERITY_3[i]==5){ #c&p
    x$SEVERITY_3n[i]= "3" #c&p
  } #c&p
} #c&p



#Create a look a table of all of the colony attributes- you will need this the functions below
SURVEY_INFO<-c("DATE_","SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH","TRANSECT","COLONYID","GENUS_CODE","TAXONCODE","COLONYLENGTH")
survey_colony<-new_Aggregate_InputTable(awd, SURVEY_INFO)
SURVEY_INFO<-c("DATE_","SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH")
survey_site<-new_Aggregate_InputTable(awd, SURVEY_INFO)

write.csv(survey_site,"test.csv")

# GENERATE SUMMARY METRICS at the transect-leveL BY GENUS--------------------------------------------------
acd.gen<-Calc_ColDen_Transect(awd,"GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen";colnames(acd.gen)[colnames(acd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_ad"# calculate density at genus level as well as total
od.gen<-Calc_ColMetric_Transect(awd,"GENUS_CODE","OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.gen<-Calc_ColMetric_Transect(awd,"GENUS_CODE",c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead
rdden.gen<-Calc_RDden_Transect(awd,"GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
condden.gen<-Calc_CONDden_Transect(awd,"GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
acutedz.gen<-subset(rdden.gen,select = c(SITEVISITID,SITE,TRANSECT,GENUS_CODE,DZGNS));colnames(acutedz.gen)[colnames(acutedz.gen)=="DZGNS"]<-"DZGNS_den" #subset just acute diseased colonies
ble.gen<-subset(condden.gen,select = c(SITEVISITID,SITE,TRANSECT,GENUS_CODE,BLE));colnames(ble.gen)[colnames(ble.gen)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.gen<-subset(condden.gen,select = c(SITEVISITID,SITE,TRANSECT,GENUS_CODE,CHRO));colnames(chronicdz.gen)[colnames(chronicdz.gen)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies
rich.gen<-Calc_Richness_Transect(awd,"GENUS_CODE")
jcd.gen<-Calc_ColDen_Transect(jwd,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen";colnames(jcd.gen)[colnames(jcd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_j"

#ADD CODE TO CHANGE TRANSECT NUMBERS FOR JUVENILES
jcd.gen$TRANSECT[jcd.gen$TRANSECT==3]<-1
jcd.gen$TRANSECT[jcd.gen$TRANSECT==4]<-2


#Merge density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","GENUS_CODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.gen<-Reduce(MyMerge, list(acd.gen,jcd.gen,od.gen,rd.gen,acutedz.gen,chronicdz.gen,ble.gen));

head(data.gen)

#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.gen$JuvColCount[is.na(data.gen$JuvColCount)]<-0;data.gen$JuvColDen[is.na(data.gen$JuvColDen)]<-0
data.gen$AdColCount[is.na(data.gen$AdColCount)]<-0;data.gen$AdColDen[is.na(data.gen$AdColDen)]<-0

#Calculate site level prevalence for acute dz, chronic dz and bleaching
data.gen$DZGNS_prev<-(data.gen$DZGNS_den*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100
data.gen$BLE_prev<-(data.gen$BLE_den*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100
data.gen$CHRO_prev<-(data.gen$CHRO_den*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100

#Remove data from transects with less than 5m surveyed for adults and 1m for juvs.
data.gen<-data.gen %>% mutate_at(.vars = c("AdColCount","AdColDen", "Ave.od", "Ave.rd","BLE_den","CHRO_den","DZGNS_den","BLE_prev","CHRO_prev","DZGNS_prev"), funs(ifelse(TRANSECTAREA_ad <5, NA, .)))
data.gen<-data.gen %>% mutate_at(.vars = c("JuvColDen"), funs(ifelse(TRANSECTAREA_j <1, NA, .)))

#GENERATE SITE-LEVEL DATA BY AVERAGING TRANSECTS-----------------------------------
#Since we are moving to a 1 stage design, we need to summarize the transects before rolling up to site.
#Dione suggested that we calculate mean of 2 transects rather than pooling or dropping a transect

#now average metrics to site level
site.data.gen<-ddply(data.gen, .(SITE,SITEVISITID,GENUS_CODE), #calc total colonies by condition
                     summarise,
                     AdColCount=sum(AdColCount,na.rm=T),AdColDen=mean(AdColDen,na.rm = T),Ave.od=mean(Ave.od,na.rm = T),
                     Ave.rd=mean(Ave.rd,na.rm = T),JuvColDen=mean(JuvColDen,na.rm=T),
                     BLE=mean(BLE_den,na.rm=T),AcuteDZ=mean(DZGNS_den,na.rm=T),ChronicDZ=mean(CHRO_den,na.rm=T),
                     BLE_prev=mean(BLE_prev,na.rm=T),AcuteDZ_prev=mean(DZGNS_prev,na.rm=T),ChronicDZ_prev=mean(CHRO_prev,na.rm=T))

#Duplicate dataframe because the ddply step above takes a while to create. Allows you to tweak code below without having to rerun the ddply step above
site.data.gen2<-site.data.gen

# get strata and sectors data. Note, this is the benthic sector/area file. we are still working on properly merging fish and benthic files.
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

SURVEY_INFO<-c("DATE_","SITEVISITID","ANALYSIS_YEAR", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH")
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

write.csv(site.data.gen2,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/BenthicREA_sitedata.csv")

# POOLING DATA from Site to Strata and Domain---------------------------------------------------
site.data.gen2<-PoolSecStrat(site.data.gen2)
rich.data<-PoolSecStrat(rich.data)

#QC CHECK to make sure the sectors and strata pooled correctly
rich.test<-ddply(rich.data,.(REGION,BEN_SEC,OBS_YEAR,STRATANAME),summarize,n=length(SITE))
data.test<-ddply(subset(site.data.gen2,GENUS_CODE=="SSSS"),.(REGION,BEN_SEC,OBS_YEAR,STRATANAME),summarize,n=length(SITE))
sm.test<-ddply(subset(survey_master,METHOD=="CORALBELT_METHOD_E_F"),.(REGION,ISLAND,SEC_NAME,OBS_YEAR,REEF_ZONE,DEPTH_BIN),summarize,n=length(SITE))

write.csv(data.test,"tmp_sitedataQC.csv")
write.csv(sm.test,"tmp_sitemasterQC.csv")

#Subset just Forereef Sites & just target taxa
site.data.gen2<-subset(site.data.gen2,REEF_ZONE=="Forereef")
site.data.gen2<-subset(site.data.gen2,GENUS_CODE %in% c("ACSP", "MOSP", "PAVS", "POCS","POSP","SSSS"))
rich.data<-subset(rich.data,REEF_ZONE=="Forereef")

#Make sure you everything but forereef are dropped
table(site.data.gen2$REEF_ZONE,site.data.gen2$GENUS_CODE)
table(rich.data$REEF_ZONE)


#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
site.data.gen2$ANALYSIS_SCHEMA<-site.data.gen2$STRATANAME
site.data.gen2$DOMAIN_SCHEMA<-site.data.gen2$ISLAND
rich.data$ANALYSIS_SCHEMA<-rich.data$STRATANAME
rich.data$DOMAIN_SCHEMA<-rich.data$ISLAND


#Calculate metrics at Strata-level-We need to work on combining metrics into 1 function

#Create a vector of columns to subset for strata estimates
c.keep<-c("REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","GENUS_CODE",
          "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep2<-c("REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","GENUS_CODE",
           "n_h","N_h","D._h","SE_D._h")
acdG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","AdColDen","Adpres.abs");acdG_st=acdG_st[,c.keep]
colnames(acdG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","GENUS_CODE","n","Ntot","AdColDen","SE_AdColDen","Adult_avp","Adult_seprop","Adult_Abun","Adult_SE_Abun","Adult_CV")

odG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.od");odG_st=odG_st[,c.keep2]
colnames(odG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","GENUS_CODE","n","Ntot","Ave.od","SE_Ave.od")

c.keep<-c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","n_h","N_h","D._h","SE_D._h")
rich_st<-Calc_Strata_Cover_Rich(rich.data,"Richness");rich_st=rich_st[,c.keep]
colnames(rich_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Sector","Stratum","n","Ntot","Richness","SE_Richess") 


#Double Check that revised pooling is adding up NH (total sites) correctly
head(acdG_st,20)
subset(sectors,ISLAND=="Kingman") #Insert whichever island/strata you want to check.

#Add in CalcAnalayis_strata & island


#Calculate Regional Estimates
acdG_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","AdColDen","Adpres.abs")
acdG_is<-acdG_is[,c("ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AdColDen","SE_AdColDen")]
odG_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.od");odG_is<-odG_is[,c("ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.od","SE_Ave.od")]
rich_is<-Calc_Domain_Cover_Rich(rich.data,"Richness");colnames(rich_is)[colnames(rich_is)=="DOMAIN_SCHEMA"]<-"Sector"


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","GENUS_CODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
st.data.gen<-Reduce(MyMerge, list(acdG_st,odG_st))
colnames(st.data.gen)[colnames(st.data.gen)=="ANALYSIS_SCHEMA"]<-"Stratum"

write.csv(st.data.gen,"Demog_surveymaster_strata.csv")
write.csv(rich_st,"Pacificwide_richness_frf_str3.csv")


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
is.data.gen<-Reduce(MyMerge, list(acdG_is,odG_is))
colnames(is.data.gen)[colnames(is.data.gen)=="DOMAIN_SCHEMA"]<-"Island"

write.csv(is.data.gen,"Demog_surveymaster_island.csv")
write.csv(rich_is,"Pacificwide_richness_frf_isl3.csv")


#Things to work on
#1. put pooling changes into csv file rather than write them out in text, too clunky and easy to get confused with different years
#2. Separate rz/db from sector name so have a column for sector and stratum- this will allow us to subset just certain depths and zone more easily later on.
#3. Make sure that ntot added up correctly across years and domains


