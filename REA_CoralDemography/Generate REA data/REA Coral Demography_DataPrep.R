#CREATE ADULT CLEAN ANALYSIS READY DATA----------------------------------------
# This script will clean the raw benthic REA data using method E that comes directly from the new data base application.
#Note- these data represent the revised data structure instituted in November 2018 and 2019. Several recent dead and condition columns were added
#These data only include surveys conducted between 2013-2020
#NOTE: Depth should not be used the in the raw data because the column was depricated in Oracale and is inconsistent.
#Use depth data from SURVEY MASTER
rm(list=ls())

#Set Run Flags
DEBUG=TRUE

#LOAD LIBRARY FUNCTIONS ...
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_ADULTCORAL_RAW_2013-2020.rdata") #from oracle
#load("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_ADULTCORAL_RAW_2013-2020.rdata") #from oracle

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
             "COLONYID","TAXONCODE","MORPH_CODE","MORPHOLOGY","COLONYLENGTH","OLDDEAD",
             "RECENTDEAD_1","RECENT_GENERAL_CAUSE_CODE_1","RECENT_SPECIFIC_CAUSE_CODE_1",
             "RECENTDEAD_2",	"RECENT_GENERAL_CAUSE_CODE_2","RECENT_SPECIFIC_CAUSE_CODE_2",
             "RECENT_GENERAL_CAUSE_CODE_3","RECENT_SPECIFIC_CAUSE_CODE_3","RECENTDEAD_3","COND",
             "CONDITION_2","CONDITION_3","EXTENT_1","EXTENT_2","EXTENT_3","SEVERITY_1","SEVERITY_2","SEVERITY_3",
             "GENUS_CODE","S_ORDER","TAXONNAME")


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
OYerror<-which(x$OBS_YEAR!=survey_master$OBS_YEAR[match(x$SITEVISITID,survey_master$SITEVISITID)])
SIerror<-which(as.vector(x$SITE)!=survey_master$SITE[match(x$SITEVISITID,survey_master$SITEVISITID)])
SIOYerrors<-unique(c(OYerror,SIerror))
if(length(SIOYerrors)>0){print(paste0("Warning: Raw Data disagree with Survey Master for sitevisitids: ",x$SITEVISITID[SIOYerrors]))}

#merge 'em NOTE: left-join will spit out a Warning message that you are joining on factors that have different levels. Basically you have more sites in survey master than x. This is correct and can be ignored here.
x<-left_join(x, survey_master[,c("OBS_YEAR","SITEVISITID","SITE","SEC_NAME","ANALYSIS_YEAR","bANALYSIS_SCHEME","new_MIN_DEPTH_M","new_MAX_DEPTH_M")])

if(DEBUG){write.csv(x,"test.csv")}

colnames(x)[colnames(x)=="new_MIN_DEPTH_M"]<-"MIN_DEPTH_M" #Change column name
colnames(x)[colnames(x)=="new_MAX_DEPTH_M"]<-"MAX_DEPTH_M" #Change column name


#Ensure that all rows in X have properly assigned SEC_NAME...
####CHECK THAT all SEC_NAME are present in the survey_master file
test<-x[is.na(x$SEC_NAME), c("MISSIONID","REGION", "SITE","OBS_YEAR"),]
test<-droplevels(test);table(test$SITE,test$MISSIONID) #create a table of missing sites by missionid
if(dim(test)[1]>0) {cat("Warning: sites with MISSING SECTORS present")}   # should be 0

#Create a list of missing sites that can be inported into the SITE MASTER file if needed
test<-x[is.na(x$SEC_NAME),]
miss.sites<-ddply(test,.(OBS_YEAR,SITEVISITID,SITE,MISSIONID,REGION,REGION_NAME,ISLAND,LATITUDE,LONGITUDE,
                         REEF_ZONE,DEPTH_BIN,DATE_,EXCLUDE_FLAG,HABITAT_CODE),
                  summarize,temp=median(SITEVISITID))
#Should be a 0 row data.frame
head(miss.sites,20)

# CLEAN UP ----------------------------------------------------------------

##Remove sites that were only surveyed for photoquads but not demographics
#Note-photoquad only sites were not included in data prior to 2018
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
x<-subset(x,SEGLENGTH!="NA") #Remove segments that were not surveyed for coral demography (no SEGLENGTH=0 in db)


#Change NAs in RecentDead extent to 0
head(subset(x,S_ORDER=="Scleractinia" & is.na(x$RDEXTENT1))) #identify columns that have NAs
x$RDEXTENT1<-ifelse(x$S_ORDER=="Scleractinia"& is.na(x$RDEXTENT1),0,x$RDEXTENT1)

head(subset(x,S_ORDER=="Scleractinia" & is.na(x$RDEXTENT2))) #identify columns that have NAs
x$RDEXTENT2<-ifelse(x$S_ORDER=="Scleractinia"& is.na(x$RDEXTENT2),0,x$RDEXTENT2)

head(subset(x,S_ORDER=="Scleractinia" & is.na(x$RDEXTENT3))) #identify columns that have NAs
x$RDEXTENT3<-ifelse(x$S_ORDER=="Scleractinia"& is.na(x$RDEXTENT3),0,x$RDEXTENT3)


# Assign TAXONCODE --------------------------------------------------------
#This section of code has been disecpetively tricky to code to ensure that all the different taxaonomic levels are generated correctly
#MODIFY WITH CAUTION
#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
#taxa<-read.csv("T:/Benthic/Data/Lookup Tables/2013-20_Taxa_MASTER.csv")
taxa<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Data/Lookup Tables/2013-20_Taxa_MASTER.csv")

x$OBS_YEAR<-as.factor(x$OBS_YEAR)#convert to factor to merge with taxa master

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
write.csv(awd,file="T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED.csv",row.names = FALSE)
#write.csv(awd,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED.csv",row.names = FALSE)


## CREATE JUVENILE CLEAN ANALYSIS READY DATA ----
## LOAD benthic data
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_JUVCORAL_RAW_2013-2020.rdata") #from oracle

x<-df #leave this as df

#Convert date formats
class(x$DATE_)
x$DATE_ <- as.Date(x$DATE_, format = "%Y-%m-%d")

#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","REGION_NAME","ISLAND","ISLANDCODE","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","NO_SURVEY_YN","EXCLUDE_FLAG","SITEVISITID","HABITAT_CODE","DIVER","TRANSECTNUM","SEGMENT","SEGWIDTH","SEGLENGTH",
             "COLONYID","TAXONCODE","MORPH_CODE","MORPHOLOGY","COLONYLENGTH","GENUS_CODE","S_ORDER","TAXONNAME")


#remove extraneous columns
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

#Cleanup 2017 NWHI data to merge with the rest of the juvenile data
nw<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/PMNM2017_JUVENILECOLONY_QCd.csv")

head(nw[,DATA_COLS])
nw<-nw[,DATA_COLS]




#Convert date formats
class(x$DATE_)
nw$DATE_ <- as.Date(nw$DATE_, format = "%Y-%m-%d")

x<-rbind(x,nw)

x$SITE<-SiteNumLeadingZeros(x$SITE) # Change site number such as MAR-22 to MAR-0022


### Use these functions to look at data
head(x)
tail(x)
table(x$REGION, x$OBS_YEAR) #review years and regions in dataframe


#Double check level and class of variables to make sure there aren't any errors
sapply(x,levels)
sapply(x,class)##Change column names to make code easier to code

colnames(x)[colnames(x)=="TAXONCODE"]<-"SPCODE" #Change column name
colnames(x)[colnames(x)=="TRANSECTNUM"]<-"TRANSECT" #Change column name


head(x)


# Merge Juvenile data and SITE MASTER -------------------------------------
# load site master to merge with demographic data
survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")

#add SITE MASTER information to x
#x<-merge(x, survey_master[,c("SITE", "SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE) #Fish team's original code, we may want to create analysis scheme later in the
length(unique(x$SITEVISITID)) #double check that sites weren't dropped

#merge 'em NOTE: left-join will spit out a Warning message that you are joining on factors that have different levels. Basically you have more sites in survey master than x. This is correct and can be ignored here.
x<-left_join(x, survey_master[,c("OBS_YEAR","SITEVISITID","SITE","SEC_NAME","ANALYSIS_YEAR","bANALYSIS_SCHEME","new_MIN_DEPTH_M","new_MAX_DEPTH_M")])

if(DEBUG){write.csv(x,"test.csv")}

colnames(x)[colnames(x)=="new_MIN_DEPTH_M"]<-"MIN_DEPTH_M" #Change column name
colnames(x)[colnames(x)=="new_MAX_DEPTH_M"]<-"MAX_DEPTH_M" #Change column name

#CHECK THAT all SEC_NAME are present in the survey_master file
test<-x[is.na(x$SEC_NAME), c("MISSIONID","REGION", "SITE","OBS_YEAR"),]
test<-droplevels(test);table(test$SITE,test$MISSIONID) #create a table of missing sites by missionid
if(dim(test)[1]>0) {cat("sites with MISSING SECTORS present")}   # should be 0

#Create a list of missing sites that can be inported into the SITE MASTER file if needed
test<-x[is.na(x$SEC_NAME),]
miss.sites<-ddply(test,.(OBS_YEAR,SITEVISITID,SITE,MISSIONID,REGION,REGION_NAME,ISLAND,LATITUDE,LONGITUDE,
                         REEF_ZONE,DEPTH_BIN,DATE_,EXCLUDE_FLAG,HABITAT_CODE),
                  summarize,temp=median(SITEVISITID))
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
x<-subset(x,SEGLENGTH!="NA") #Remove segments that were not surveyed for coral demography


# Assign TAXONCODE --------------------------------------------------------
#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
#taxa<-read.csv("T:/Benthic/Data/Lookup Tables/2013-20_Taxa_MASTER.csv")
taxa<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Data/Lookup Tables/2013-20_Taxa_MASTER.csv")

x$OBS_YEAR<-as.factor(x$OBS_YEAR) #need to convert to factor in order to join with taxa df
nrow(x)
#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
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
NegNineCheckCols=c("S_ORDER","TAXONNAME","COLONYLENGTH")
x[,NegNineCheckCols][x[,NegNineCheckCols] ==-9] <- NA #Convert missing numeric values to NA (they are entered as -9 in Oracle)


jwd<-droplevels(x)
write.csv(jwd,file="T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Juveniles_raw_CLEANED.csv",row.names = FALSE)
#write.csv(jwd,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Juveniles_raw_CLEANED.csv",row.names = FALSE)

