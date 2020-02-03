
#CREATE ADULT CLEAN ANALYSIS READY DATA----------------------------------------
# This script will clean the raw benthic REA data using method E that comes directly from the new data base application.
#Note- these data represent the revised data structure insituted in November 2018. Several recent dead and condition columns were added
#These data only include surveys conducted between 2013-2019
rm(list=ls())

#Set Run Flags
DEBUG=TRUE

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_ADULTCORAL_RAW_2013-2019.rdata") #from oracle
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

# Column Names Changes... -------------------------------------------------
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

if(DEBUG){head(x)}


# Merge Adult data and  SURVEY MASTER -------------------------------------
#SURVEY MASTER was created by Ivor and Courtney by extracting sites directly from the Site Visit table from Oracle. It should be the complete list of sites surveyed since 2000
survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")



#Check that OBS_YEAR, SITEVISITID, and SITE are all the same in both x and survey master
OYerror=which(x$OBS_YEAR!=survey_master$OBS_YEAR[match(x$SITEVISITID,survey_master$SITEVISITID)])
SIerror=which(as.vector(x$SITE)!=survey_master$SITE[match(x$SITEVISITID,survey_master$SITEVISITID)])
SIOYerrors=unique(c(OYerror,SIerror))
if(length(SIOYerrors)>0){print(paste0("Warning: Raw Data disagree with Survey Master for sitevisitids: ",x$SITEVISITID[SIOYerrors]))}

#add SITE MASTER information to x 
length(unique(x$SITEVISITID)) #check the number of sites in demographic data
#merge 'em
x<-left_join(x, survey_master[,c("OBS_YEAR","SITEVISITID","SITE","SEC_NAME","ANALYSIS_YEAR","bANALYSIS_SCHEME")])  
length(unique(x$SITEVISITID)) #double check that sites weren't dropped
if(length()>0){print(paste0("Warning: Raw Data disagree with Survey Master for sitevisitids: ",x$SITEVISITID[SIOYerrors]))}


if(DEBUG){write.csv(x,"test.csv")}

#Ensure that all rows in X have properly assigned SEC_NAME...
####CHECK THAT all SEC_NAME are present in the survey_master file
test<-x[is.na(x$SEC_NAME), c("MISSIONID","REGION", "SITE","OBS_YEAR"),]
test<-droplevels(test);table(test$SITE,test$MISSIONID) #create a table of missing sites by missionid
if(dim(test)[1]>0) {cat("Warning: sites with MISSING SECTORS present")}   # should be 0

#Create a list of missing sites that can be inported into the SITE MASTER file if needed
test<-x[is.na(x$SEC_NAME),]
miss.sites<-ddply(test,.(OBS_YEAR,SITEVISITID,SITE,MISSIONID,REGION,REGION_NAME,ISLAND,LATITUDE,LONGITUDE,
                         REEF_ZONE,DEPTH_BIN,DATE_,EXCLUDE_FLAG,HABITAT_CODE),
                  summarize,temp=median(SITEVISITID),SITE_MAX_DEPTH=median(SITE_MAX_DEPTH),SITE_MIN_DEPTH=median(SITE_MIN_DEPTH))
#Should be a 0 row data.frame
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
x$NO_SURVEY_YN[is.na(x$NO_SURVEY_YN)]<-0 #Change NAs (blank cells) to 0
##Acutally do the removal of transects that were only surveyed for photoquads but not demographics
x<-subset(x,NO_SURVEY_YN==0)


#TEMPORARY FIX-SPEAK WITH DM TO CORRECT IN ORACLE
#Change all special missions to exclude flag =-1, right now they are 0. Then exclude these sites
levels(x$MISSIONID)
x$EXCLUDE_FLAG<-ifelse(x$MISSIONID %in% c("MP1410","MP1512","MP1602","SE1602"),-1,0)
head(subset(x,EXCLUDE_FLAG==-1))

x<-subset(x,SEGLENGTH!="NA") #Remove segments that were not surveyed for coral demography (no SEGLENGTH=0 in db)
#Actually remove special missions.
x<-subset(x,EXCLUDE_FLAG==0);
# this dataframe should be empty
head(subset(x,EXCLUDE_FLAG==-1))

head(x)

#Change NAs in RecentDead extent to 0
head(subset(x,S_ORDER=="Scleractinia" & is.na(x$RDEXTENT1))) #identify columns that have NAs
x$RDEXTENT1<-ifelse(x$S_ORDER=="Scleractinia"& is.na(x$RDEXTENT1),0,x$RDEXTENT1)

head(subset(x,S_ORDER=="Scleractinia" & is.na(x$RDEXTENT2))) #identify columns that have NAs
x$RDEXTENT2<-ifelse(x$S_ORDER=="Scleractinia"& is.na(x$RDEXTENT2),0,x$RDEXTENT2)

head(subset(x,S_ORDER=="Scleractinia" & is.na(x$RDEXTENT3))) #identify columns that have NAs
x$RDEXTENT3<-ifelse(x$S_ORDER=="Scleractinia"& is.na(x$RDEXTENT3),0,x$RDEXTENT3)


# Assign TAXONCODE --------------------------------------------------------
#This section of code has been disecetively tricky to code and ensure that all the different taxaonomic levels are generated correctly
#MODIFY WITH CAUTION
#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
taxa<-read.csv("T:/Benthic/Data/SpGen_Reference/2013-19_Taxa_MASTER.csv")

taxa$OBS_YEAR<-as.integer(taxa$OBS_YEAR) #need to convert to factor in order to join with taxa df
nrow(x)
#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
#This function will give you warnings that you are joining factors with different levels. THis is correct, but doesn't matter. Ignore
x$TAXONCODE<-Convert_to_Taxoncode(data = x,taxamaster = taxa)
nrow(x)

#Check to make sure SPCODE was converted correctly
View(subset(x,SPCODE!=TAXONCODE))

#If there are issues use this code to create a list SPCODE (lowest taxonomic resolution we have), TAXONCODE (the taxonomic level we all feel comfortable with) and associated genera
#This is used for spot checking that TAXONCODE was converted properly & can be compared against TAXA MASTER 
SURVEY_INFO<-c("OBS_YEAR","REGION","SPCODE","TAXONCODE","GENUS_CODE","TAXONNAME")
test<-new_Aggregate_InputTable(x, SURVEY_INFO)
head(test)

#Check to see whether S_ORDER is NA and not AAAA (the code for no colonies observed on the segment)
x[x$SPCODE!="AAAA"& is.na(x$S_ORDER),] #this dataframe should be empty


#Change columns to character
x$GENUS_CODE<-as.character(x$GENUS_CODE)
x$SPCODE<-as.character(x$SPCODE)
x$TAXONCODE<-as.character(x$TAXONCODE)
x$S_ORDER<-as.character(x$S_ORDER)

#Make sure there are no NA values in genus code or taxoncode if it's supposed to be a scleractinian
subset(x,S_ORDER=="Scleractinia" & GENUS_CODE=="NA") #this dataframe should be empty
subset(x,S_ORDER=="Scleractinia" & TAXONCODE=="NA") #this dataframe should be empty

#Fix missing NAs if need be
# x$GENUS_CODE<-ifelse(is.na(x$GENUS_CODE)&x$S_ORDER=="Scleractinia",x$SPCODE,x$GENUS_CODE)
# x$TAXONCODE<-ifelse(is.na(x$TAXONCODE)&x$S_ORDER=="Scleractinia",x$SPCODE,x$TAXONCODE)

#There are some old SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genera or taxoncodes to the spcode and the remaining NAs in the Taxon and genus code to AAAA
x$GENUS_CODE<-ifelse(x$TAXONCODE=="UNKN","UNKN",x$GENUS_CODE)
x$TAXONCODE<-ifelse(x$SPCODE=="AAAA","AAAA",x$TAXONCODE)
x$GENUS_CODE<-ifelse(x$TAXONCODE=="AAAA","AAAA",x$GENUS_CODE)
x$TAXONCODE<-ifelse(x$SPCODE %in% c("MOAS","LEPA"),"UNKN",x$TAXONCODE)
x$GENUS_CODE<-ifelse(x$SPCODE %in% c("MOAS","LEPA"),"UNKN",x$GENUS_CODE)


View(x) #view data in separate window

#Check that Unknown scl were changed correctly
head(subset(x,TAXONCODE=="UNKN"&S_ORDER=="Scleractinia"),40)
head(subset(x,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia"))
head(subset(x,GENUS_CODE=="AAAA"))
head(subset(x,SPCODE=="AAAA"))


##Calcuating segment and transect area and add column for transect area
x$TRANSECTAREA<-Transectarea(x)
# sapply(x,levels)
head(x)
nrow(x)


## CLEAN UP NAs ##
NegNineCheckCols=c("RDEXTENT1","GENRD1","RD1","RDEXTENT2","GENRD2","RD2","GENRD3","RD3",
                   "RDEXTENT3","CONDITION_1","CONDITION_2","CONDITION_3","EXTENT_1","EXTENT_2","EXTENT_3","SEVERITY_1",
                  "SEVERITY_2","SEVERITY_3","GENUS_CODE","S_ORDER")
x[,NegNineCheckCols][x[,NegNineCheckCols]==-9] <- NA #Convert missing numeric values to NA (they are entered as -9 in Oracle)

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
write.csv(awd,file="T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_E_raw_CLEANED.csv")


## CREATE JUVENILE CLEAN ANALYSIS READY DATA ----
## LOAD benthic data
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_JUVCORAL_RAW_2013-2019.rdata") #from oracle
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

x<-left_join(x, survey_master[,c("OBS_YEAR","SITEVISITID","SITE","SEC_NAME","ANALYSIS_YEAR","bANALYSIS_SCHEME")])  
length(unique(x$SITEVISITID)) #double check that sites weren't dropped

if (DEBUG) {write.csv(x,"test.csv")}

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
taxa<-read.csv("T:/Benthic/Data/SpGen_Reference/2013-19_Taxa_MASTER.csv")

taxa$OBS_YEAR<-as.integer(taxa$OBS_YEAR) #need to convert to factor in order to join with taxa df
nrow(x)
#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
x$TAXONCODE<-Convert_to_Taxoncode(data = x,taxamaster = taxa)
nrow(x)

#If there are issues use this code to create a list SPCODE (lowest taxonomic resolution we have), TAXONCODE (the taxonomic level we all feel comfortable with) and associated genera
#This is used for spot checking that TAXONCODE was converted properly & can be compared against TAXA MASTER 
SURVEY_INFO<-c("OBS_YEAR","REGION","SPCODE","TAXONCODE","GENUS_CODE","TAXONNAME")
test<-new_Aggregate_InputTable(x, SURVEY_INFO)
head(test)

#Check to see whether S_ORDER is NA and not AAAA (the code for no colonies observed on the segment)
x[x$SPCODE!="AAAA"& is.na(x$S_ORDER),] #this dataframe should be empty


#Change columns to character
x$GENUS_CODE<-as.character(x$GENUS_CODE)
x$SPCODE<-as.character(x$SPCODE)
x$TAXONCODE<-as.character(x$TAXONCODE)
x$S_ORDER<-as.character(x$S_ORDER)

#Make sure there are no NA values in genus code or taxoncode if it's supposed to be a scleractinian
subset(x,S_ORDER=="Scleractinia" & GENUS_CODE=="NA") #this dataframe should be empty
subset(x,S_ORDER=="Scleractinia" & TAXONCODE=="NA") #this dataframe should be empty

#Fix missing NAs if need be
# x$GENUS_CODE<-ifelse(is.na(x$GENUS_CODE)&x$S_ORDER=="Scleractinia",x$SPCODE,x$GENUS_CODE)
# x$TAXONCODE<-ifelse(is.na(x$TAXONCODE)&x$S_ORDER=="Scleractinia",x$SPCODE,x$TAXONCODE)

#There are some old SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genera or taxoncodes to the spcode and the remaining NAs in the Taxon and genus code to AAAA
x$GENUS_CODE<-ifelse(x$TAXONCODE=="UNKN","UNKN",x$GENUS_CODE)
x$TAXONCODE<-ifelse(x$SPCODE=="AAAA","AAAA",x$TAXONCODE)
x$GENUS_CODE<-ifelse(x$TAXONCODE=="AAAA","AAAA",x$GENUS_CODE)
x$TAXONCODE<-ifelse(x$SPCODE %in% c("MOAS","LEPA"),"UNKN",x$TAXONCODE)
x$GENUS_CODE<-ifelse(x$SPCODE %in% c("MOAS","LEPA"),"UNKN",x$GENUS_CODE)

View(x) #view data in separate window

#Check that Unknown scl were changed correctly
head(subset(x,TAXONCODE=="UNKN"&S_ORDER=="Scleractinia"),40)
head(subset(x,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia"))
head(subset(x,GENUS_CODE=="AAAA"))
head(subset(x,SPCODE=="AAAA"))


##Calcuating segment and transect area and add column for transect area
x$TRANSECTAREA<-Transectarea(x)
# sapply(x,levels)
head(x)
nrow(x)


## CLEAN UP NAs ##
NegNineCheckCols=c("S_ORDER","TAXONNAME","SITE_MIN_DEPTH","SITE_MAX_DEPTH","COLONYLENGTH")
x[,NegNineCheckCols][x[,NegNineCheckCols] ==-9] <- NA #Convert missing numeric values to NA (they are entered as -9 in Oracle)


jwd<-droplevels(x)
write.csv(jwd,file="T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_F_raw_CLEANED.csv")


#Final Tweaks before calculating Site-level data-------------------------------------------------
#Colony fragments and scleractinans are subseted in the functions 
awd<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_E_raw_CLEANED.csv")
jwd<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_F_raw_CLEANED.csv")


#Add a column for adult fragments so we can remove them from the dataset later (-1 indicates fragment)
# awd<-CreateFragment(awd)
awd$Fragment<-ifelse(awd$OBS_YEAR <2018 & awd$COLONYLENGTH <5 & awd$S_ORDER=="Scleractinia",-1,awd$Fragment)
head(subset(awd,Fragment==-1& OBS_YEAR<2018)) #double check that pre 2018 fragments create
awd$Fragment[is.na(awd$Fragment)] <- 0
jwd$Fragment <- 0 # you need to add this column so that you can use the site level functions correctly

#Simplify Bleaching Severity categories: in 2019 the team decided to simplify the bleaching severity from 1-5 to 1-3 to improve consistency in severity values
#This code converts the severity data collected prior to 2019 to a 1-3 scale
awd_pre<-awd[awd$OBS_YEAR<2019,]
awd_post<-awd[awd$OBS_YEAR>=2019,]
Convert_Severity<-function(data,severity_field,severity_new){
  data$SEV<-data[,severity_field]
  data<-data %>% mutate(sev_new=recode(SEV, 
                                       `1`="1",
                                       `2`="1",
                                       `3`="3",
                                       `4`="3",
                                       `5`="3"))
  colnames(data)[which(colnames(data) == 'sev_new')] <- severity_new #change group to whatever your grouping field is.
  data<-subset(data,select=-c(SEV))
return(data)
}

awd_pre<-Convert_Severity(awd_pre,"SEVERITY_1","SEVERITY_1n")
awd_pre<-Convert_Severity(awd_pre,"SEVERITY_2","SEVERITY_2n")
#awd_pre<-Convert_Severity(awd_pre,"SEVERITY_3","SEVERITY_3n") #There were no severity measurements prior to 2020

head(awd_pre)
#View(awd_pre)

#After checking that severity numbers were changed correctly, convert back to original column names & drop original columns
awd_pre<-subset(awd_pre,select=-c(SEVERITY_1));colnames(awd_pre)[which(colnames(awd_pre) == 'SEVERITY_1n')] <- "SEVERITY_1" #change group to whatever your grouping field is.
awd_pre<-subset(awd_pre,select=-c(SEVERITY_2));colnames(awd_pre)[which(colnames(awd_pre) == 'SEVERITY_2n')] <- "SEVERITY_2" #change group to whatever your grouping field is.
#awd_pre<-subset(awd_pre,select=-c(SEVERITY_3));colnames(awd_pre)[which(colnames(awd_pre) == 'SEVERITY_3n')] <- "SEVERITY_3" #change group to whatever your grouping field is.
awd_pre$SEVERITY_3<-NA

head(awd_pre)



#Combine dataframes before and after 2019 & check that rows weren't dropped
awd.<-rbind(awd_pre,awd_post)
nrow(awd)
nrow(awd.);head(awd.)
awd<-awd.; rm("awd.") #remove temporary dataframe if all good. 


#Create a look a table of all of the colony attributes- you will need this the functions below
SURVEY_COL<-c("DATE_","SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH","TRANSECT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony<-unique(awd[,SURVEY_COL])#new_Aggregate_InputTable(awd, SURVEY_INFO)#TAO 2019/10/07

SURVEY_SITE<-c("DATE_","SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE")
survey_siteAd<-unique(awd[,SURVEY_SITE])#new_Aggregate_InputTable(awd, SURVEY_INFO)#TAO 2019/10/07

SURVEY_SITE<-c("DATE_","SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE")
survey_siteJ<-unique(jwd[,SURVEY_SITE])#new_Aggregate_InputTable(awd, SURVEY_INFO)#TAO 2019/10/07

#There can be multiple depths for a given site since more than one transect has been surveyed
#This code identifies the max and min depth for a site then merges into site visit table for ad and j dataframes
tmp<-ddply(awd,.(SITEVISITID),summarize,
           SITE_MAX_DEPTH=max(SITE_MAX_DEPTH),
           SITE_MIN_DEPTH=max(SITE_MIN_DEPTH))
nrow(survey_siteAd)
survey_siteAd<-left_join(survey_siteAd,tmp);nrow(survey_siteAd)

tmp<-ddply(jwd,.(SITEVISITID),summarize,
           SITE_MAX_DEPTH=max(SITE_MAX_DEPTH),
           SITE_MIN_DEPTH=max(SITE_MIN_DEPTH))
nrow(survey_siteJ)
survey_siteJ<-left_join(survey_siteJ,tmp);nrow(survey_siteJ)

#We did juvenile only surveys in 2017 in PRIA, this will make sure the SV table has both adult and juv sites.
survey_site<-left_join(survey_siteJ,survey_siteAd);nrow(survey_site) 

#TEMPORARY WORK AROUND-ASK MICHAEL TO FIX
survey_site$REEF_ZONE<-ifelse(survey_site$SITE=="HAW-04285","Forereef",as.character(survey_site$REEF_ZONE))

######################  ######################  ######################  ######################  ###################### 
###################### COURTNEY AND TOM REVIEWED TO THIS POINT 2019/10/07 ###################### 
######################  ######################  ######################  ######################  ###################### 

# GENERATE SUMMARY METRICS at the transect-leveL BY GENUS--------------------------------------------------
#Calc_ColDen_Transect
acd.gen<-Calc_ColDen_Transect(data = awd,grouping_field = "GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen";colnames(acd.gen)[colnames(acd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_ad"# calculate density at genus level as well as total
jcd.gen<-Calc_ColDen_Transect(jwd,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen";colnames(jcd.gen)[colnames(jcd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_j"

#Calc_ColMetric_Transect
cl.gen<-Calc_ColMetric_Transect(data = awd,grouping_field = "GENUS_CODE",pool_fields = "COLONYLENGTH"); colnames(cl.gen)[colnames(cl.gen)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.gen<-Calc_ColMetric_Transect(data = awd,grouping_field = "GENUS_CODE",pool_fields = "OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.gen<-Calc_ColMetric_Transect(data = awd,grouping_field = "GENUS_CODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead

#Calc_RDden_Transect
rdden.gen<-Calc_RDden_Transect(awd,survey_colony,"GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.gen<-subset(rdden.gen,select = c(SITEVISITID,SITE,TRANSECT,GENUS_CODE,DZGN_G));colnames(acutedz.gen)[colnames(acutedz.gen)=="DZGN_G"]<-"DZGN_den" #subset just acute diseased colonies

#Calc_CONDden_Transect
condden.gen<-Calc_CONDden_Transect(awd,survey_colony,"GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.gen<-subset(condden.gen,select = c(SITEVISITID,SITE,TRANSECT,GENUS_CODE,BLE));colnames(ble.gen)[colnames(ble.gen)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.gen<-subset(condden.gen,select = c(SITEVISITID,SITE,TRANSECT,GENUS_CODE,CHRO));colnames(chronicdz.gen)[colnames(chronicdz.gen)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies

#Calc_Richness_Transect
rich.gen<-Calc_Richness_Transect(awd,"GENUS_CODE")

#ADD CODE TO CHANGE TRANSECT NUMBERS FOR JUVENILES
jcd.gen$TRANSECT[jcd.gen$TRANSECT==3]<-1
jcd.gen$TRANSECT[jcd.gen$TRANSECT==4]<-2


#Merge density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","GENUS_CODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.gen<-Reduce(MyMerge, list(acd.gen,jcd.gen,cl.gen,od.gen,rd.gen,acutedz.gen,chronicdz.gen,ble.gen));
head(data.gen)

#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.gen$JuvColCount[is.na(data.gen$JuvColCount)]<-0;data.gen$JuvColDen[is.na(data.gen$JuvColDen)]<-0
data.gen$AdColCount[is.na(data.gen$AdColCount)]<-0;data.gen$AdColDen[is.na(data.gen$AdColDen)]<-0

#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.gen$DZGN_prev<-(data.gen$DZGN_den*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100
data.gen$BLE_prev<-(data.gen$BLE_den*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100
data.gen$CHRO_prev<-(data.gen$CHRO_den*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100

#Remove data from transects with less than 5m surveyed for adults and 1m for juvs.
data.gen$TRANSECTAREA_ad<-ifelse(data.gen$TRANSECTAREA_ad<5,NA,data.gen$TRANSECTAREA_ad);data.gen[data.gen$TRANSECTAREA_ad<5,]
data.gen$TRANSECTAREA_j<-ifelse(data.gen$TRANSECTAREA_j<1,NA,data.gen$TRANSECTAREA_j);data.gen[data.gen$TRANSECTAREA_j<1,]

#GENERATE SITE-LEVEL DATA BY AVERAGING TRANSECTS-----------------------------------
#Since we are moving to a 1 stage design, we need to summarize the transects before rolling up to site. Dione suggested that we calculate mean of 2 transects rather than pooling or dropping a transect

site.data.gen<-ddply(data.gen, .(SITE,SITEVISITID,GENUS_CODE), #calc total colonies by condition
                     summarise,
                     AdColCount=sum(AdColCount,na.rm=T),AdColDen=mean(AdColDen,na.rm = T),Ave.od=mean(Ave.od,na.rm = T),
                     Ave.rd=mean(Ave.rd,na.rm = T),Ave.size=mean(Ave.cl,na.rm=T),JuvColDen=mean(JuvColDen,na.rm=T),
                     BLE=mean(BLE_den,na.rm=T),AcuteDZ=mean(DZGN_den,na.rm=T),ChronicDZ=mean(CHRO_den,na.rm=T),
                     BLE_prev=mean(BLE_prev,na.rm=T),AcuteDZ_prev=mean(DZGN_prev,na.rm=T),ChronicDZ_prev=mean(CHRO_prev,na.rm=T))

#Duplicate dataframe because the ddply step above takes a while to create. Allows you to tweak code below without having to rerun the ddply step above
site.data.gen2<-site.data.gen


# GENERATE SUMMARY METRICS at the transect-leveL BY SPCODE (finest resolution)--------------------------------------------------
#Calc_ColDen_Transect
acd.sp<-Calc_ColDen_Transect(data = awd,grouping_field = "SPCODE");colnames(acd.sp)[colnames(acd.sp)=="ColCount"]<-"AdColCount";colnames(acd.sp)[colnames(acd.sp)=="ColDen"]<-"AdColDen";colnames(acd.sp)[colnames(acd.sp)=="TRANSECTAREA"]<-"TRANSECTAREA_ad"# calculate density at genus level as well as total
jcd.sp<-Calc_ColDen_Transect(jwd,"SPCODE"); colnames(jcd.sp)[colnames(jcd.sp)=="ColCount"]<-"JuvColCount";colnames(jcd.sp)[colnames(jcd.sp)=="ColDen"]<-"JuvColDen";colnames(jcd.sp)[colnames(jcd.sp)=="TRANSECTAREA"]<-"TRANSECTAREA_j"

#Calc_ColMetric_Transect
cl.sp<-Calc_ColMetric_Transect(data = awd,grouping_field = "SPCODE",pool_fields = "COLONYLENGTH"); colnames(cl.sp)[colnames(cl.sp)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.sp<-Calc_ColMetric_Transect(data = awd,grouping_field = "SPCODE",pool_fields = "OLDDEAD"); colnames(od.sp)[colnames(od.sp)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.sp<-Calc_ColMetric_Transect(data = awd,grouping_field = "SPCODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.sp)[colnames(rd.sp)=="Ave.y"]<-"Ave.rd" #Average % recent dead

#Calc_RDden_Transect
rdden.sp<-Calc_RDden_Transect(awd,survey_colony,"SPCODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.sp<-subset(rdden.sp,select = c(SITEVISITID,SITE,TRANSECT,SPCODE,DZGN_G));colnames(acutedz.sp)[colnames(acutedz.sp)=="DZGN_G"]<-"DZGN_den" #subset just acute diseased colonies

#Calc_CONDden_Transect
condden.sp<-Calc_CONDden_Transect(awd,survey_colony,"SPCODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.sp<-subset(condden.sp,select = c(SITEVISITID,SITE,TRANSECT,SPCODE,BLE));colnames(ble.sp)[colnames(ble.sp)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.sp<-subset(condden.sp,select = c(SITEVISITID,SITE,TRANSECT,SPCODE,CHRO));colnames(chronicdz.sp)[colnames(chronicdz.sp)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies

#Calc_Richness_Transect
rich.sp<-Calc_Richness_Transect(awd,"SPCODE")

#ADD CODE TO CHANGE TRANSECT NUMBERS FOR JUVENILES
jcd.sp$TRANSECT[jcd.sp$TRANSECT==3]<-1
jcd.sp$TRANSECT[jcd.sp$TRANSECT==4]<-2


#Merge density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","SPCODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.sp<-Reduce(MyMerge, list(acd.sp,jcd.sp,cl.sp,od.sp,rd.sp,acutedz.sp,chronicdz.sp,ble.sp));
head(data.sp)

#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.sp$JuvColCount[is.na(data.sp$JuvColCount)]<-0;data.sp$JuvColDen[is.na(data.sp$JuvColDen)]<-0
data.sp$AdColCount[is.na(data.sp$AdColCount)]<-0;data.sp$AdColDen[is.na(data.sp$AdColDen)]<-0

#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.sp$DZGN_prev<-(data.sp$DZGN_den*data.sp$TRANSECTAREA_ad)/data.sp$AdColCount*100
data.sp$BLE_prev<-(data.sp$BLE_den*data.sp$TRANSECTAREA_ad)/data.sp$AdColCount*100
data.sp$CHRO_prev<-(data.sp$CHRO_den*data.sp$TRANSECTAREA_ad)/data.sp$AdColCount*100

#Remove data from transects with less than 5m surveyed for adults and 1m for juvs.
data.sp$TRANSECTAREA_ad<-ifelse(data.sp$TRANSECTAREA_ad<5,NA,data.sp$TRANSECTAREA_ad);data.sp[data.sp$TRANSECTAREA_ad<5,]
data.sp$TRANSECTAREA_j<-ifelse(data.sp$TRANSECTAREA_j<1,NA,data.sp$TRANSECTAREA_j);data.sp[data.sp$TRANSECTAREA_j<1,]

#GENERATE SITE-LEVEL DATA BY AVERAGING TRANSECTS-----------------------------------
#Since we are moving to a 1 stage design, we need to summarize the transects before rolling up to site. Dione suggested that we calculate mean of 2 transects rather than pooling or dropping a transect

site.data.sp<-ddply(data.sp, .(SITE,SITEVISITID,SPCODE), #calc total colonies by condition
                    summarise,
                    AdColCount=sum(AdColCount,na.rm=T),AdColDen=mean(AdColDen,na.rm = T),Ave.od=mean(Ave.od,na.rm = T),
                    Ave.rd=mean(Ave.rd,na.rm = T),Ave.size=mean(Ave.cl,na.rm=T),JuvColDen=mean(JuvColDen,na.rm=T),
                    BLE=mean(BLE_den,na.rm=T),AcuteDZ=mean(DZGN_den,na.rm=T),ChronicDZ=mean(CHRO_den,na.rm=T),
                    BLE_prev=mean(BLE_prev,na.rm=T),AcuteDZ_prev=mean(DZGN_prev,na.rm=T),ChronicDZ_prev=mean(CHRO_prev,na.rm=T))

#Duplicate dataframe because the ddply step above takes a while to create. Allows you to tweak code below without having to rerun the ddply step above
site.data.sp2<-site.data.sp



# GENERATE SUMMARY METRICS at the transect-leveL BY TAXONCODE (finest resolution)--------------------------------------------------
#Calc_ColDen_Transect
acd.tax<-Calc_ColDen_Transect(data = awd,grouping_field = "TAXONCODE");colnames(acd.tax)[colnames(acd.tax)=="ColCount"]<-"AdColCount";colnames(acd.tax)[colnames(acd.tax)=="ColDen"]<-"AdColDen";colnames(acd.tax)[colnames(acd.tax)=="TRANSECTAREA"]<-"TRANSECTAREA_ad"# calculate density at genus level as well as total
jcd.tax<-Calc_ColDen_Transect(jwd,"TAXONCODE"); colnames(jcd.tax)[colnames(jcd.tax)=="ColCount"]<-"JuvColCount";colnames(jcd.tax)[colnames(jcd.tax)=="ColDen"]<-"JuvColDen";colnames(jcd.tax)[colnames(jcd.tax)=="TRANSECTAREA"]<-"TRANSECTAREA_j"

#Calc_ColMetric_Transect
cl.tax<-Calc_ColMetric_Transect(data = awd,grouping_field = "TAXONCODE",pool_fields = "COLONYLENGTH"); colnames(cl.tax)[colnames(cl.tax)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.tax<-Calc_ColMetric_Transect(data = awd,grouping_field = "TAXONCODE",pool_fields = "OLDDEAD"); colnames(od.tax)[colnames(od.tax)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.tax<-Calc_ColMetric_Transect(data = awd,grouping_field = "TAXONCODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.tax)[colnames(rd.tax)=="Ave.y"]<-"Ave.rd" #Average % recent dead

#Calc_RDden_Transect
rdden.tax<-Calc_RDden_Transect(awd,survey_colony,"TAXONCODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.tax<-subset(rdden.tax,select = c(SITEVISITID,SITE,TRANSECT,TAXONCODE,DZGN_G));colnames(acutedz.tax)[colnames(acutedz.tax)=="DZGN_G"]<-"DZGN_den" #subset just acute diseased colonies

#Calc_CONDden_Transect
condden.tax<-Calc_CONDden_Transect(awd,survey_colony,"TAXONCODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.tax<-subset(condden.tax,select = c(SITEVISITID,SITE,TRANSECT,TAXONCODE,BLE));colnames(ble.tax)[colnames(ble.tax)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.tax<-subset(condden.tax,select = c(SITEVISITID,SITE,TRANSECT,TAXONCODE,CHRO));colnames(chronicdz.tax)[colnames(chronicdz.tax)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies

#Calc_Richness_Transect
rich.tax<-Calc_Richness_Transect(awd,"TAXONCODE")

#ADD CODE TO CHANGE TRANSECT NUMBERS FOR JUVENILES
jcd.tax$TRANSECT[jcd.tax$TRANSECT==3]<-1
jcd.tax$TRANSECT[jcd.tax$TRANSECT==4]<-2


#Merge density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","TAXONCODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.tax<-Reduce(MyMerge, list(acd.tax,jcd.tax,cl.tax,od.tax,rd.tax,acutedz.tax,chronicdz.tax,ble.tax));
head(data.tax)

#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.tax$JuvColCount[is.na(data.tax$JuvColCount)]<-0;data.tax$JuvColDen[is.na(data.tax$JuvColDen)]<-0
data.tax$AdColCount[is.na(data.tax$AdColCount)]<-0;data.tax$AdColDen[is.na(data.tax$AdColDen)]<-0

#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.tax$DZGN_prev<-(data.tax$DZGN_den*data.tax$TRANSECTAREA_ad)/data.tax$AdColCount*100
data.tax$BLE_prev<-(data.tax$BLE_den*data.tax$TRANSECTAREA_ad)/data.tax$AdColCount*100
data.tax$CHRO_prev<-(data.tax$CHRO_den*data.tax$TRANSECTAREA_ad)/data.tax$AdColCount*100

#Remove data from transects with less than 5m surveyed for adults and 1m for juvs.
data.tax$TRANSECTAREA_ad<-ifelse(data.tax$TRANSECTAREA_ad<5,NA,data.tax$TRANSECTAREA_ad);data.tax[data.tax$TRANSECTAREA_ad<5,]
data.tax$TRANSECTAREA_j<-ifelse(data.tax$TRANSECTAREA_j<1,NA,data.tax$TRANSECTAREA_j);data.tax[data.tax$TRANSECTAREA_j<1,]

#GENERATE SITE-LEVEL DATA BY AVERAGING TRANSECTS-----------------------------------
#Since we are moving to a 1 stage design, we need to summarize the transects before rolling up to site. Dione suggested that we calculate mean of 2 transects rather than pooling or dropping a transect

site.data.tax<-ddply(data.tax, .(SITE,SITEVISITID,TAXONCODE), #calc total colonies by condition
                     summarise,
                     AdColCount=sum(AdColCount,na.rm=T),AdColDen=mean(AdColDen,na.rm = T),Ave.od=mean(Ave.od,na.rm = T),
                     Ave.rd=mean(Ave.rd,na.rm = T),Ave.size=mean(Ave.cl,na.rm=T),JuvColDen=mean(JuvColDen,na.rm=T),
                     BLE=mean(BLE_den,na.rm=T),AcuteDZ=mean(DZGN_den,na.rm=T),ChronicDZ=mean(CHRO_den,na.rm=T),
                     BLE_prev=mean(BLE_prev,na.rm=T),AcuteDZ_prev=mean(DZGN_prev,na.rm=T),ChronicDZ_prev=mean(CHRO_prev,na.rm=T))

#Duplicate dataframe because the ddply step above takes a while to create. Allows you to tweak code below without having to rerun the ddply step above
site.data.tax2<-site.data.tax



# Merge Site level data with sectors file and export site data ------------
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

#Merge together survey meta data and sector area files and check for missmatches 
meta<-left_join(survey_site,sectors)
meta[which(is.na(meta$AREA_HA)),]
nrow(survey_site)
nrow(meta)


#Merge site level data and meta data
site.data.gen2<-left_join(site.data.gen2,meta)
rich.data.gen<-left_join(rich.gen,meta)
site.data.sp2<-left_join(site.data.sp2)
rich.data.sp<-left_join(rich.sp,meta)
site.data.tax2<-left_join(site.data.tax2)

rich.data.tax<-left_join(rich.tax,meta)

#Add adult and juvenile pres/ab columns
site.data.gen2$Adpres.abs<-ifelse(site.data.gen2$AdColDen>0,1,0)
site.data.gen2$Juvpres.abs<-ifelse(site.data.gen2$JuvColDen>0,1,0)
site.data.sp2$Adpres.abs<-ifelse(site.data.sp2$AdColDen>0,1,0)
site.data.sp2$Juvpres.abs<-ifelse(site.data.sp2$JuvColDen>0,1,0)
site.data.tax2$Adpres.abs<-ifelse(site.data.tax2$AdColDen>0,1,0)
site.data.tax2$Juvpres.abs<-ifelse(site.data.tax2$JuvColDen>0,1,0)


write.csv(site.data.gen2,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS.csv",row.names = F)
write.csv(site.data.sp2,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_SPCODE.csv",row.names = F)
write.csv(site.data.tax2,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_TAXONCODE.csv",row.names = F)




rm(list=ls())

#Set Run Flags
DEBUG=TRUE

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")



site.data.gen2<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS.csv")
site.data.sp2<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_SPCODE.csv")
site.data.tax2<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_TAXONCODE.csv")



# POOLING DATA from Site to Strata and Domain---------------------------------------------------
survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)
site.data.gen2<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS.csv")

#This function will pool data according to predetermined groups. Protected reef slope is converted to Forereef here
site.data.gen2<-PoolSecStrat(site.data.gen2)
rich.data<-PoolSecStrat(rich.data.gen)

#QC CHECK to make sure the sectors and strata pooled correctly
rich.test<-ddply(rich.data,.(REGION,BEN_SEC,OBS_YEAR,STRATANAME),summarize,n=length(SITE))
data.test<-ddply(subset(site.data.gen2,GENUS_CODE=="SSSS"),.(REGION,BEN_SEC,OBS_YEAR,STRATANAME),summarize,n=length(SITE))
sm.test<-ddply(subset(survey_master,Benthic=="1"&EXCLUDE_FLAG=="0"&OBS_YEAR>=2013),.(REGION,ISLAND,SEC_NAME,OBS_YEAR,REEF_ZONE,DEPTH_BIN),summarize,n=length(SITE))

write.csv(data.test,"tmp_sitedataQC.csv")
write.csv(sm.test,"tmp_sitemasterQC.csv")

# #Subset just Forereef Sites & just target taxa
# site.data.gen2<-subset(site.data.gen2,REEF_ZONE=="Forereef")
# site.data.gen2<-subset(site.data.gen2,GENUS_CODE %in% c("ACSP", "MOSP", "PAVS", "POCS","POSP","SSSS"))
# rich.data<-subset(rich.data,REEF_ZONE=="Forereef")

# #Make sure you everything but forereef are dropped
# table(site.data.gen2$REEF_ZONE,site.data.gen2$GENUS_CODE)
# table(rich.data$REEF_ZONE)


#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
site.data.gen2$ANALYSIS_SCHEMA<-site.data.gen2$STRATANAME
site.data.gen2$DOMAIN_SCHEMA<-site.data.gen2$ISLAND
rich.data$ANALYSIS_SCHEMA<-rich.data$STRATANAME
rich.data$DOMAIN_SCHEMA<-rich.data$ISLAND


#Calculate metrics at Strata-level-We need to work on combining metrics into 1 function

#Create a vector of columns to subset for strata estimates
c.keep<-c("REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
          "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep2<-c("REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
           "n_h","N_h","D._h","SE_D._h")
c.keep3<-c("REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
          "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
acdG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","AdColDen","Adpres.abs");acdG_st=acdG_st[,c.keep]
colnames(acdG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","AdColDen","SE_AdColDen","Adult_avp","Adult_seprop","Adult_Abun","Adult_SE_Abun","Adult_CV")

jcdG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","JuvColDen","Juvpres.abs");jcdG_st=jcdG_st[,c.keep]
colnames(jcdG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","JuvColDen","SE_JuvColDen","Juv_avp","Juv_seprop","Juv_Abun","Juv_SE_Abun","Juv_CV")

odG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.od");odG_st=odG_st[,c.keep2]
colnames(odG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.od","SE_Ave.od")

rdG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.rd");rdG_st=rdG_st[,c.keep2]
colnames(rdG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.rd","SE_Ave.rd")

clG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.size");clG_st=clG_st[,c.keep2]
colnames(clG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.Ave.size","SE_Ave.size")

BLEG_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","BLE");BLEG_st=BLEG_st[,c.keep2]
colnames(BLEG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","BLE","SE_BLE")

AcuteDZG_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","Ave.cl");AcuteDZG_st=AcuteDZG_st[,c.keep2]
colnames(AcuteDZG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","AcuteDZ","SE_AcuteDZ")

ChronicDZG_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","Ave.cl");ChronicDZG_st=ChronicDZG_st[,c.keep2]
colnames(ChronicDZG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","ChronicDZ","SE_ChronicDZ")


# c.keep<-c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","n_h","N_h","D._h","SE_D._h")
# rich_st<-Calc_Strata_Cover_Rich(rich.data,"Richness");rich_st=rich_st[,c.keep]
# colnames(rich_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Sector","REEF_ZONE","DB_RZ","Stratum","n","Ntot","Richness","SE_Richess") 


#Double Check that revised pooling is adding up NH (total sites) correctly
View(acdG_st)
View(sectors)


#Calculate Island Estimates
acdG_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","AdColDen","Adpres.abs")
acdG_is<-acdG_is[,c("ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AdColDen","SE_AdColDen")]
jcdG_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","JuvColDen","Juvpres.abs")
jcdG_is<-jcdG_is[,c("ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen")]
odG_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.od")
odG_is<-odG_is[,c("ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.od","SE_Ave.od")]
rdG_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.rd")
rdG_is<-rdG_is[,c("ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.rd","SE_Ave.rd")]
clG_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.size")
clG_is<-clG_is[,c("ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.size","SE_Ave.size")]
bleG_is<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","Ave.size")
bleG_is<-bleG_is[,c("ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_BLE_Prevalence","SE_Ave.size")]


#Calculate Sector Estimates
site.data.gen2$ANALYSIS_SCHEMA<-site.data.gen2$STRATANAME
site.data.gen2$DOMAIN_SCHEMA<-site.data.gen2$BEN_SEC
rich.data$ANALYSIS_SCHEMA<-rich.data$STRATANAME
rich.data$DOMAIN_SCHEMA<-rich.data$BEN_SEC

acdG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","AdColDen","Adpres.abs")
acdG_sec<-acdG_sec[,c("ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AdColDen","SE_AdColDen")]
jcdG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","JuvColDen","Juvpres.abs")
jcdG_sec<-jcdG_sec[,c("ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen")]
odG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.od")
odG_sec<-odG_sec[,c("ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.od","SE_Ave.od")]
rdG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.rd")
rdG_sec<-rdG_sec[,c("ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.rd","SE_Ave.rd")]
clG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.size")
clG_sec<-clG_sec[,c("ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.size","SE_Ave.size")]


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
st.data.gen<-Reduce(MyMerge, list(acdG_st,jcdG_st,odG_st,rdG_st,clG_st))
colnames(st.data.gen)[colnames(st.data.gen)=="ANALYSIS_SCHEMA"]<-"Stratum"

write.csv(st.data.gen,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicREA_stratadata_GENUS.csv")
# write.csv(rich_st,"Pacificwide_richness_frf_str3.csv")


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
is.data.gen<-Reduce(MyMerge, list(acdG_is,jcdG_is,odG_is,rdG_is,clG_is))
colnames(is.data.gen)[colnames(is.data.gen)=="DOMAIN_SCHEMA"]<-"Island"

write.csv(is.data.gen,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicREA_islanddata_GENUS.csv")


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
sec.data.gen<-Reduce(MyMerge, list(acdG_sec,jcdG_sec,odG_sec,rdG_sec,clG_sec))
colnames(sec.data.gen)[colnames(sec.data.gen)=="DOMAIN_SCHEMA"]<-"Sector"

write.csv(is.data.gen,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicREA_sectordata_GENUS.csv")




#Things to work on
#1. put pooling changes into csv file rather than write them out in text, too clunky and easy to get confused with different years
#2. Separate rz/db from sector name so have a column for sector and stratum- this will allow us to subset just certain depths and zone more easily later on.
#3. Make sure that ntot added up correctly across years and domains


