
#DIVER:ADULT CLEAN ANALYSIS READY DATA----------------------------------------
# This script will clean the raw benthic REA data using method E that comes directly from the new data base application.
#These data only include surveys conducted between 2013-2018
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

x<-df
x$SITE<-SiteNumLeadingZeros(x$SITE) # Change site number such as MAR-22 to MAR-0022

#Convert date formats
class(x$DATE_)
x$DATE_ <- as.Date(x$DATE_, format = "%Y-%m-%d")


### Use these functions to look at data
head(x)
tail(x)
table(x$REGION, x$OBS_YEAR) #review years and regions in dataframe

#Convert Segment number from old to new numbering system
x$SEGMENT<-ConvertSegNumber(x)

#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","REGION_NAME","ISLAND","ISLANDCODE","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","NO_SURVEY_YN","EXCLUDE_FLAG","SITEVISITID","HABITAT_CODE","DIVER","TRANSECTNUM","SEGMENT","SEGWIDTH","SEGLENGTH","FRAGMENT_YN",
             "COLONYID","TAXONCODE","COLONYLENGTH","OLDDEAD",
             "RECENTDEAD_1","RECENT_GENERAL_CAUSE_CODE_1","RECENT_SPECIFIC_CAUSE_CODE_1",
             "RECENTDEAD_2",	"RECENT_GENERAL_CAUSE_CODE_2","RECENT_SPECIFIC_CAUSE_CODE_2",	
             "RECENT_GENERAL_CAUSE_CODE_3","RECENT_SPECIFIC_CAUSE_CODE_3","RECENTDEAD_3","COND",
             "CONDITION_2","CONDITION_3","EXTENT_1","EXTENT_2","EXTENT_3","SEVERITY_1","SEVERITY_2","SEVERITY_3",
             "GENUS_CODE","S_ORDER","TAXONNAME","SITE_MIN_DEPTH","SITE_MAX_DEPTH")

x<-subset(x,REGION=="MHI" & OBS_YEAR =="2019") #subset just MHI 2019 data

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

#Add column for method type
x$METHOD<-"DIVER"

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
x<-merge(x, survey_master[,c("OBS_YEAR","SITEVISITID","SITE","SEC_NAME","ANALYSIS_YEAR","bANALYSIS_SCHEME")], by=c("OBS_YEAR","SITEVISITID","SITE"),all.x=T)  
length(unique(x$SITEVISITID)) #double check that sites weren't dropped

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
#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
taxa<-read.csv("T:/Benthic/Data/SpGen_Reference/2013-19_Taxa_MASTER.csv")

#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
#x$TAXONCODE<-Convert_to_Taxoncode_tom(data = x,taxamaster = taxa)#not working need to ask tom
x<-Convert_to_Taxoncode(x)

#Check to make sure SPCODE was converted correctly
head(x[x$SPCODE!=x$TAXONCODE,])

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
x$SEGAREA<-x$SEGWIDTH*x$SEGLENGTH
x$TRANSECTAREA<-Transectarea(x)

# sapply(x,levels)
head(x)
nrow(x)


## CLEAN UP NAs ##
# NegNineCheckCols=c("RDEXTENT1","GENRD1","RD1","RDEXTENT2","GENRD2","RD2","GENRD3","RD3",
#                    "RDEXTENT3","CONDITION_1","CONDITION_2","CONDITION_3","EXTENT_1","EXTENT_2","EXTENT_3","SEVERITY_1",
#                    "SEVERITY_2","SEVERITY_3","GENUS_CODE","S_ORDER")
# x[,NegNineCheckCols][x[,NegNineCheckCols]==-9] <- NA #Convert missing numeric values to NA (they are entered as -9 in Oracle)

tmp.lev<-levels(x$GENRD1); tmp.lev
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


## DIVER:JUVENILE CLEAN ANALYSIS READY DATA ----
## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_JUVCORAL_RAW_2013-2019.rdata") #from oracle

x<-df
x$SITE<-SiteNumLeadingZeros(x$SITE) # Change site number such as MAR-22 to MAR-0022

#Convert date formats
class(x$DATE_)
x$DATE_ <- as.Date(x$DATE_, format = "%Y-%m-%d")

### Use these functions to look at data
head(x)
tail(x)
table(x$REGION, x$OBS_YEAR) #review years and regions in dataframe

#Convert Segment number from old to new numbering system
x$SEGMENT<-ConvertSegNumber(x)


#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","REGION_NAME","ISLAND","ISLANDCODE","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","NO_SURVEY_YN","EXCLUDE_FLAG","SITEVISITID","HABITAT_CODE","DIVER","TRANSECTNUM","SEGMENT","SEGWIDTH","SEGLENGTH",
             "COLONYID","TAXONCODE","COLONYLENGTH","GENUS_CODE","S_ORDER","TAXONNAME","MINDEPTH","MAXDEPTH")

x<-subset(x,REGION=="MHI" & OBS_YEAR =="2019") #subset just MHI 2019 data

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

#Add column for method type
x$METHOD<-"DIVER"

head(x)



# Merge Juvenile data and SITE MASTER -------------------------------------
# load site master to merge with demographic data
survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")

#add SITE MASTER information to x 
#x<-merge(x, survey_master[,c("SITE", "SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE) #Fish team's original code, we may want to create analysis scheme later in the 
length(unique(x$SITEVISITID)) #double check that sites weren't dropped

x<-join(x, survey_master[,c("OBS_YEAR","SITEVISITID","SITE","SEC_NAME","ANALYSIS_YEAR","bANALYSIS_SCHEME")], by=c("OBS_YEAR","SITEVISITID","SITE"))  
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

#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
#x$TAXONCODE<-Convert_to_Taxoncode_tom(data = x,taxamaster = taxa)#not working need to ask tom
x<-Convert_to_Taxoncode(x)

#Check to make sure SPCODE was converted correctly
head(x[x$SPCODE!=x$TAXONCODE,])

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
x$SEGAREA<-x$SEGWIDTH*x$SEGLENGTH
x$TRANSECTAREA<-Transectarea(x)
# sapply(x,levels)
head(x)
nrow(x)


## CLEAN UP NAs ##
NegNineCheckCols=c("S_ORDER","TAXONNAME","SITE_MIN_DEPTH","SITE_MAX_DEPTH","COLONYLENGTH")
x[,NegNineCheckCols][x[,NegNineCheckCols] ==-9] <- NA #Convert missing numeric values to NA (they are entered as -9 in Oracle)


jwd<-droplevels(x)
#write.csv(Jwd,file="T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_F_raw_CLEANED.csv")


#Final Tweaks before calculating Site-level data-------------------------------------------------
#Colony fragments and scleractinans are subseted in the functions 

#Add a column for adult fragments so we can remove them from the dataset later (-1 indicates fragment)
# awd<-CreateFragment(awd)
awd$Fragment<-ifelse(awd$OBS_YEAR <2018 & awd$COLONYLENGTH <5 & awd$S_ORDER=="Scleractinia",-1,awd$Fragment)
head(subset(awd,Fragment==-1& OBS_YEAR<2018)) #double check that pre 2018 fragments create
awd$Fragment[is.na(awd$Fragment)] <- 0
jwd$Fragment <- 0 # you need to add this column so that you can use the site level functions correctly



#Create a look a table of all of the colony attributes- you will need this the functions below
SURVEY_COL<-c("DATE_","SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH","TRANSECT","SEGMENT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony<-unique(awd[,SURVEY_COL])

SURVEY_SITE<-c("DATE_","SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH")
survey_site<-unique(awd[,SURVEY_SITE])

SURVEY_Seg<-c("DATE_","SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN","HABITAT_CODE", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH","METHOD","TRANSECT","SEGMENT")
survey_segment<-unique(awd[,SURVEY_Seg])

#Site/segement table for SfM QC
survey_segment1<-subset(survey_segment,TRANSECT=="1")
write.csv(survey_segment1,file="T:/Benthic/SfM/Materials for SfM benthic QC/HARAMPbenthic_site_seglist.csv")


# GENERATE SUMMARY METRICS at the Segment-leveL BY GENUS--------------------------------------------------
#Calc_ColDen_Transect
acd.gen<-Calc_ColDen_Seg(data = awd,grouping_field = "GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen";colnames(acd.gen)[colnames(acd.gen)=="SEGAREA"]<-"SEGAREA_ad"# calculate density at genus level as well as total
jcd.gen<-Calc_ColDen_Seg(jwd,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen"
jcd.gen<-subset(jcd.gen,select=-c(SEGAREA))

#Calc_ColMetric_Transect
cl.gen<-Calc_ColMetric_Seg(data = awd,grouping_field = "GENUS_CODE",pool_fields = "COLONYLENGTH"); colnames(cl.gen)[colnames(cl.gen)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.gen<-Calc_ColMetric_Seg(data = awd,grouping_field = "GENUS_CODE",pool_fields = "OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.gen<-Calc_ColMetric_Seg(data = awd,grouping_field = "GENUS_CODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead

#Calc_RDden_Transect
rdden.gen<-Calc_RDden_Seg(data=awd,grouping_field ="GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.gen<-subset(rdden.gen,select = c(SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,DZGN_G));colnames(acutedz.gen)[colnames(acutedz.gen)=="DZGN_G"]<-"DZGN_G_den" #subset just acute diseased colonies

#Calc_CONDden_Transect
condden.gen<-Calc_CONDden_Seg(data=awd,grouping_field ="GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.gen<-subset(condden.gen,select = c(SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,BLE));colnames(ble.gen)[colnames(ble.gen)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.gen<-subset(condden.gen,select = c(SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,CHRO));colnames(chronicdz.gen)[colnames(chronicdz.gen)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies

#Calc_Richness_Transect
#rich.gen<-Calc_Richness_Transect(awd,"GENUS_CODE")

#ADD CODE TO CHANGE TRANSECT NUMBERS FOR JUVENILES
jcd.gen$TRANSECT[jcd.gen$TRANSECT==3]<-1
jcd.gen$TRANSECT[jcd.gen$TRANSECT==4]<-2


#Merge density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","SEGMENT","GENUS_CODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.gen<-Reduce(MyMerge, list(acd.gen,jcd.gen,cl.gen,od.gen,rd.gen,acutedz.gen,chronicdz.gen,ble.gen));
head(data.gen)

#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.gen$JuvColCount[is.na(data.gen$JuvColCount)]<-0;data.gen$JuvColDen[is.na(data.gen$JuvColDen)]<-0
data.gen$AdColCount[is.na(data.gen$AdColCount)]<-0;data.gen$AdColDen[is.na(data.gen$AdColDen)]<-0

#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.gen$DZGN_G_prev<-(data.gen$DZGN_G_den*data.gen$SEGAREA_ad)/data.gen$AdColCount*100
data.gen$BLE_prev<-(data.gen$BLE_den*data.gen$SEGAREA_ad)/data.gen$AdColCount*100
data.gen$CHRO_prev<-(data.gen$CHRO_den*data.gen$SEGAREA_ad)/data.gen$AdColCount*100

View(data.gen)

# #Remove data from transects with less than 5m surveyed for adults and 1m for juvs.
# data.gen$TRANSECTAREA_ad<-ifelse(data.gen$TRANSECTAREA_ad<5,NA,data.gen$TRANSECTAREA_ad);data.gen[data.gen$TRANSECTAREA_ad<5,]
# data.gen$TRANSECTAREA_j<-ifelse(data.gen$TRANSECTAREA_j<1,NA,data.gen$TRANSECTAREA_j);data.gen[data.gen$TRANSECTAREA_j<1,]

#GENERATE SITE-LEVEL DATA BY AVERAGING TRANSECTS-----------------------------------
#Since we are moving to a 1 stage design, we need to summarize the transects before rolling up to site. Dione suggested that we calculate mean of 2 transects rather than pooling or dropping a transect

# site.data.gen<-ddply(data.gen, .(SITE,SITEVISITID,GENUS_CODE), #calc total colonies by condition
#                      summarise,
#                      AdColCount=sum(AdColCount,na.rm=T),AdColDen=mean(AdColDen,na.rm = T),Ave.od=mean(Ave.od,na.rm = T),
#                      Ave.rd=mean(Ave.rd,na.rm = T),JuvColDen=mean(JuvColDen,na.rm=T),
#                      BLE=mean(BLE_den,na.rm=T),AcuteDZ=mean(DZGN_G_den,na.rm=T),ChronicDZ=mean(CHRO_den,na.rm=T),
#                      BLE_prev=mean(BLE_prev,na.rm=T),AcuteDZ_prev=mean(DZGN_G_prev,na.rm=T),ChronicDZ_prev=mean(CHRO_prev,na.rm=T))
# 
# #Duplicate dataframe because the ddply step above takes a while to create. Allows you to tweak code below without having to rerun the ddply step above
# site.data.gen2<-site.data.gen



# GENERATE SUMMARY METRICS at the Segment-leveL BY TAXONCODE--------------------------------------------------
#Calc_ColDen_Transect
acd.tax<-Calc_ColDen_Seg(data = awd,grouping_field = "TAXONCODE");colnames(acd.tax)[colnames(acd.tax)=="ColCount"]<-"AdColCount";colnames(acd.tax)[colnames(acd.tax)=="ColDen"]<-"AdColDen";colnames(acd.tax)[colnames(acd.tax)=="SEGAREA"]<-"SEGAREA_ad"# calculate density at genus level as well as total
jcd.tax<-Calc_ColDen_Seg(jwd,"TAXONCODE"); colnames(jcd.tax)[colnames(jcd.tax)=="ColCount"]<-"JuvColCount";colnames(jcd.tax)[colnames(jcd.tax)=="ColDen"]<-"JuvColDen"
jcd.tax<-subset(jcd.tax,select=-c(SEGAREA))

#Calc_ColMetric_Transect
cl.tax<-Calc_ColMetric_Seg(data = awd,grouping_field = "TAXONCODE",pool_fields = "COLONYLENGTH"); colnames(cl.tax)[colnames(cl.tax)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.tax<-Calc_ColMetric_Seg(data = awd,grouping_field = "TAXONCODE",pool_fields = "OLDDEAD"); colnames(od.tax)[colnames(od.tax)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.tax<-Calc_ColMetric_Seg(data = awd,grouping_field = "TAXONCODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.tax)[colnames(rd.tax)=="Ave.y"]<-"Ave.rd" #Average % recent dead

#Calc_RDden_Transect
rdden.tax<-Calc_RDden_Seg(data=awd,grouping_field ="TAXONCODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.tax<-subset(rdden.tax,select = c(SITEVISITID,SITE,TRANSECT,SEGMENT,TAXONCODE,DZGN_G));colnames(acutedz.tax)[colnames(acutedz.tax)=="DZGN_G"]<-"DZGN_G_den" #subset just acute diseased colonies

#Calc_CONDden_Transect
condden.tax<-Calc_CONDden_Seg(data=awd,grouping_field ="TAXONCODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.tax<-subset(condden.tax,select = c(SITEVISITID,SITE,TRANSECT,SEGMENT,TAXONCODE,BLE));colnames(ble.tax)[colnames(ble.tax)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.tax<-subset(condden.tax,select = c(SITEVISITID,SITE,TRANSECT,SEGMENT,TAXONCODE,CHRO));colnames(chronicdz.tax)[colnames(chronicdz.tax)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies

#Calc_Richness_Transect
#rich.tax<-Calc_Richness_Transect(awd,"TAXONCODE")

#ADD CODE TO CHANGE TRANSECT NUMBERS FOR JUVENILES
jcd.tax$TRANSECT[jcd.tax$TRANSECT==3]<-1
jcd.tax$TRANSECT[jcd.tax$TRANSECT==4]<-2


#Merge density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","SEGMENT","TAXONCODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.tax<-Reduce(MyMerge, list(acd.tax,jcd.tax,cl.tax,od.tax,rd.tax,acutedz.tax,chronicdz.tax,ble.tax));
head(data.tax)

#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.tax$JuvColCount[is.na(data.tax$JuvColCount)]<-0;data.tax$JuvColDen[is.na(data.tax$JuvColDen)]<-0
data.tax$AdColCount[is.na(data.tax$AdColCount)]<-0;data.tax$AdColDen[is.na(data.tax$AdColDen)]<-0

#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.tax$DZGN_G_prev<-(data.tax$DZGN_G_den*data.tax$SEGAREA_ad)/data.tax$AdColCount*100
data.tax$BLE_prev<-(data.tax$BLE_den*data.tax$SEGAREA_ad)/data.tax$AdColCount*100
data.tax$CHRO_prev<-(data.tax$CHRO_den*data.tax$SEGAREA_ad)/data.tax$AdColCount*100


head(data.tax)
head(subset(data.tax,TAXONCODE==0))



# Prepare Diver data to merge with SfM data -------------------------------
#Make sure you have all transects and segments
levels(as.factor(data.tax$TRANSECT))
levels(as.factor(data.tax$SEGMENT))

#Merge survey segment with segment-level demographic data
diver.tax<-merge(survey_segment,data.tax,by=c("SITE","SITEVISITID","TRANSECT","SEGMENT"),all.y=T)
if(nrow(data.tax)!=nrow(diver.tax)) {cat("Warning: dataframes did not merge properly")}   
head(diver.tax)



# SFM:ADULT CLEAN ANALYSIS READY DATA --------------------
x<-read.csv("T:/Benthic/Data/SfM/QC/HARAMP2019_QCdsfm_ADULT.csv")


head(x)
View(x)
colnames(x)

# Column Names Changes... -------------------------------------------------
colnames(x)[colnames(x)=="MISSION_ID"]<-"MISSIONID" #Change column name
colnames(x)[colnames(x)=="RD_1"]<-"RDEXTENT1" #Change column name
colnames(x)[colnames(x)=="RDCAUSE1"]<-"RD1" #Change column name
colnames(x)[colnames(x)=="RD_2"]<-"RDEXTENT2" #Change column name
colnames(x)[colnames(x)=="RD_3"]<-"RDEXTENT3" #Change column name
colnames(x)[colnames(x)=="RDCAUSE2"]<-"RD2" #Change column name
colnames(x)[colnames(x)=="RDCAUSE3"]<-"RD3" #Change column name
colnames(x)[colnames(x)=="FRAGMENT_Y"]<-"Fragment" #Change column name
colnames(x)[colnames(x)=="CON_1"]<-"CONDITION_1" #Change column name
colnames(x)[colnames(x)=="CON_2"]<-"CONDITION_2" #Change column name
colnames(x)[colnames(x)=="CON_3"]<-"CONDITION_3" #Change column name
colnames(x)[colnames(x)=="SEV_1"]<-"SEVERITY_1" #Change column name
colnames(x)[colnames(x)=="SEV_2"]<-"SEVERITY_2" #Change column name
colnames(x)[colnames(x)=="SEV_3"]<-"SEVERITY_3" #Change column name
colnames(x)[colnames(x)=="SHAPE_Leng"]<-"COLONYLENGTH" #Change column name

#Add column for method type
x$METHOD<-"SfM"

if(DEBUG){head(x)}

table(x$SITE,x$ANALYST)

#Adding and Modifying columns

#Fill in columns with values that we know should not be different across any of the rows
x$MISSION_ID <- as.factor(rep("SE1902", times = nrow(sfm.raw)))
x$OBS_YEAR <- as.vector(rep(2019, times = nrow(sfm.raw)))

x$COLONYLENGTH<-x$COLONYLENGTH*100 #convert from m to cm

x$S_ORDER<-ifelse(x$NO_COLONY_==0 & x$SPCODE!="NONE","Scleractinia","NONE") #add S_order column

#Generate General RD cause code
gencodes<-read.csv("T:/Benthic/Data/SpGen_Reference/GeneralRDcode_lookup.csv")
nrow(x)
x<-x %>% 
  inner_join(gencodes, by = c("RD1" = "RDCODE")) %>% mutate(GENRD1 = GENCODE)%>% select(-GENCODE)
x<-x %>% 
  inner_join(gencodes, by = c("RD2" = "RDCODE")) %>% mutate(GENRD2 = GENCODE)%>% select(-GENCODE)

head(x,10)

#Create Transect column and use this to code duplicate segments
x$TRANSECT<-ifelse(x$ANALYST=="MA","1","2")

#Merge data with Survey
segvisit<-read.csv("T:/Benthic/SfM/Materials for SfM benthic QC/HARAMPbenthic_site_seglist.csv");head(segvisit)

df<-merge(x,segvisit,by=c("OBS_YEAR","MISSIONID","SITE","SEGMENT"))
# CLEAN UP ----------------------------------------------------------------

#Change NAs in RecentDead extent to 0
head(subset(x,S_ORDER=="Scleractinia" & is.na(x$RDEXTENT1))) #identify columns that have NAs
x$RDEXTENT1<-ifelse(x$S_ORDER=="Scleractinia"& is.na(x$RDEXTENT1),0,x$RDEXTENT1)

head(subset(x,S_ORDER=="Scleractinia" & is.na(x$RDEXTENT2))) #identify columns that have NAs
x$RDEXTENT2<-ifelse(x$S_ORDER=="Scleractinia"& is.na(x$RDEXTENT2),0,x$RDEXTENT2)

head(subset(x,S_ORDER=="Scleractinia" & is.na(x$RDEXTENT3))) #identify columns that have NAs
x$RDEXTENT3<-ifelse(x$S_ORDER=="Scleractinia"& is.na(x$RDEXTENT3),0,x$RDEXTENT3)


# Assign TAXONCODE --------------------------------------------------------
#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
taxa<-read.csv("T:/Benthic/Data/SpGen_Reference/2013-19_Taxa_MASTER.csv")

#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
#x$TAXONCODE<-Convert_to_Taxoncode_tom(data = x,taxamaster = taxa)#not working need to ask tom
x<-Convert_to_Taxoncode(x)

#Check to make sure SPCODE was converted correctly
head(x[x$SPCODE!=x$TAXONCODE,])

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
x$SEGAREA<-x$SEGWIDTH*x$SEGLENGTH
x$TRANSECTAREA<-Transectarea(x)

# sapply(x,levels)
head(x)
nrow(x)


## CLEAN UP NAs ##
# NegNineCheckCols=c("RDEXTENT1","GENRD1","RD1","RDEXTENT2","GENRD2","RD2","GENRD3","RD3",
#                    "RDEXTENT3","CONDITION_1","CONDITION_2","CONDITION_3","EXTENT_1","EXTENT_2","EXTENT_3","SEVERITY_1",
#                    "SEVERITY_2","SEVERITY_3","GENUS_CODE","S_ORDER")
# x[,NegNineCheckCols][x[,NegNineCheckCols]==-9] <- NA #Convert missing numeric values to NA (they are entered as -9 in Oracle)


# Clean up NAs ------------------------------------------------------------
tmp.lev<-levels(x$GENRD1); tmp.lev
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
x<-read.csv("T:/Benthic/Data/SfM/QC/HARAMP2019_QCdsfm_JUV.csv") #from oracle

#Convert date formats
class(x$DATE_)
x$DATE_ <- as.Date(x$DATE_, format = "%Y-%m-%d")

### Use these functions to look at data
head(x)
tail(x)
table(x$REGION, x$OBS_YEAR) #review years and regions in dataframe

#Convert Segment number from old to new numbering system
x$SEGMENT<-as.factor(x$SEGMENT)
x<-x %>% mutate(SEGMENT=recode(SEGMENT, 
                               `1`="0",
                               `3`="5",
                               `5`="10",
                               `7`="15",
                               `NA`="NA"))

#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","REGION_NAME","ISLAND","ISLANDCODE","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","NO_SURVEY_YN","EXCLUDE_FLAG","SITEVISITID","HABITAT_CODE","DIVER","TRANSECTNUM","SEGMENT","SEGWIDTH","SEGLENGTH",
             "COLONYID","TAXONCODE","COLONYLENGTH","GENUS_CODE","S_ORDER","TAXONNAME","MINDEPTH","MAXDEPTH")

x<-subset(x,REGION=="MHI" & OBS_YEAR =="2019") #subset just MHI 2019 data

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

#Add column for method type
x$METHOD<-"DIVER"

head(x)


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

#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
#x$TAXONCODE<-Convert_to_Taxoncode_tom(data = x,taxamaster = taxa)#not working need to ask tom
x<-Convert_to_Taxoncode(x)

#Check to make sure SPCODE was converted correctly
head(x[x$SPCODE!=x$TAXONCODE,])

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
x$SEGAREA<-x$SEGWIDTH*x$SEGLENGTH
x$TRANSECTAREA<-Transectarea(x)
# sapply(x,levels)
head(x)
nrow(x)


## CLEAN UP NAs ##
NegNineCheckCols=c("S_ORDER","TAXONNAME","SITE_MIN_DEPTH","SITE_MAX_DEPTH","COLONYLENGTH")
x[,NegNineCheckCols][x[,NegNineCheckCols] ==-9] <- NA #Convert missing numeric values to NA (they are entered as -9 in Oracle)


jwd<-droplevels(x)
#write.csv(Jwd,file="T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_F_raw_CLEANED.csv")


#Final Tweaks before calculating Site-level data-------------------------------------------------
#Colony fragments and scleractinans are subseted in the functions 

#Add a column for adult fragments so we can remove them from the dataset later (-1 indicates fragment)
# awd<-CreateFragment(awd)
awd$Fragment<-ifelse(awd$OBS_YEAR <2018 & awd$COLONYLENGTH <5 & awd$S_ORDER=="Scleractinia",-1,awd$Fragment)
head(subset(awd,Fragment==-1& OBS_YEAR<2018)) #double check that pre 2018 fragments create
awd$Fragment[is.na(awd$Fragment)] <- 0
jwd$Fragment <- 0 # you need to add this column so that you can use the site level functions correctly



#Create a look a table of all of the colony attributes- you will need this the functions below
SURVEY_COL<-c("DATE_","SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH","TRANSECT","SEGMENT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony<-unique(awd[,SURVEY_COL])

SURVEY_SITE<-c("DATE_","SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH")
survey_site<-unique(awd[,SURVEY_SITE])

SURVEY_Seg<-c("DATE_","SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN","HABITAT_CODE", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH","METHOD","TRANSECT","SEGMENT")
survey_segment<-unique(awd[,SURVEY_Seg])

#Site/segement table for SfM QC
survey_segment1<-subset(survey_segment,TRANSECT=="1")
write.csv(survey_segment1,file="T:/Benthic/SfM/Materials for SfM benthic QC/HARAMPbenthic_site_seglist.csv")


# GENERATE SUMMARY METRICS at the Segment-leveL BY GENUS--------------------------------------------------
#Calc_ColDen_Transect
acd.gen<-Calc_ColDen_Seg(data = awd,grouping_field = "GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen";colnames(acd.gen)[colnames(acd.gen)=="SEGAREA"]<-"SEGAREA_ad"# calculate density at genus level as well as total
jcd.gen<-Calc_ColDen_Seg(jwd,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen"
jcd.gen<-subset(jcd.gen,select=-c(SEGAREA))

#Calc_ColMetric_Transect
cl.gen<-Calc_ColMetric_Seg(data = awd,grouping_field = "GENUS_CODE",pool_fields = "COLONYLENGTH"); colnames(cl.gen)[colnames(cl.gen)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.gen<-Calc_ColMetric_Seg(data = awd,grouping_field = "GENUS_CODE",pool_fields = "OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.gen<-Calc_ColMetric_Seg(data = awd,grouping_field = "GENUS_CODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead

#Calc_RDden_Transect
rdden.gen<-Calc_RDden_Seg(data=awd,grouping_field ="GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.gen<-subset(rdden.gen,select = c(SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,DZGN_G));colnames(acutedz.gen)[colnames(acutedz.gen)=="DZGN_G"]<-"DZGN_G_den" #subset just acute diseased colonies

#Calc_CONDden_Transect
condden.gen<-Calc_CONDden_Seg(data=awd,grouping_field ="GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.gen<-subset(condden.gen,select = c(SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,BLE));colnames(ble.gen)[colnames(ble.gen)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.gen<-subset(condden.gen,select = c(SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,CHRO));colnames(chronicdz.gen)[colnames(chronicdz.gen)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies

#Calc_Richness_Transect
#rich.gen<-Calc_Richness_Transect(awd,"GENUS_CODE")

#ADD CODE TO CHANGE TRANSECT NUMBERS FOR JUVENILES
jcd.gen$TRANSECT[jcd.gen$TRANSECT==3]<-1
jcd.gen$TRANSECT[jcd.gen$TRANSECT==4]<-2


#Merge density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","SEGMENT","GENUS_CODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.gen<-Reduce(MyMerge, list(acd.gen,jcd.gen,cl.gen,od.gen,rd.gen,acutedz.gen,chronicdz.gen,ble.gen));
head(data.gen)

#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.gen$JuvColCount[is.na(data.gen$JuvColCount)]<-0;data.gen$JuvColDen[is.na(data.gen$JuvColDen)]<-0
data.gen$AdColCount[is.na(data.gen$AdColCount)]<-0;data.gen$AdColDen[is.na(data.gen$AdColDen)]<-0

#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.gen$DZGN_G_prev<-(data.gen$DZGN_G_den*data.gen$SEGAREA_ad)/data.gen$AdColCount*100
data.gen$BLE_prev<-(data.gen$BLE_den*data.gen$SEGAREA_ad)/data.gen$AdColCount*100
data.gen$CHRO_prev<-(data.gen$CHRO_den*data.gen$SEGAREA_ad)/data.gen$AdColCount*100

View(data.gen)

# #Remove data from transects with less than 5m surveyed for adults and 1m for juvs.
# data.gen$TRANSECTAREA_ad<-ifelse(data.gen$TRANSECTAREA_ad<5,NA,data.gen$TRANSECTAREA_ad);data.gen[data.gen$TRANSECTAREA_ad<5,]
# data.gen$TRANSECTAREA_j<-ifelse(data.gen$TRANSECTAREA_j<1,NA,data.gen$TRANSECTAREA_j);data.gen[data.gen$TRANSECTAREA_j<1,]

#GENERATE SITE-LEVEL DATA BY AVERAGING TRANSECTS-----------------------------------
#Since we are moving to a 1 stage design, we need to summarize the transects before rolling up to site. Dione suggested that we calculate mean of 2 transects rather than pooling or dropping a transect

# site.data.gen<-ddply(data.gen, .(SITE,SITEVISITID,GENUS_CODE), #calc total colonies by condition
#                      summarise,
#                      AdColCount=sum(AdColCount,na.rm=T),AdColDen=mean(AdColDen,na.rm = T),Ave.od=mean(Ave.od,na.rm = T),
#                      Ave.rd=mean(Ave.rd,na.rm = T),JuvColDen=mean(JuvColDen,na.rm=T),
#                      BLE=mean(BLE_den,na.rm=T),AcuteDZ=mean(DZGN_G_den,na.rm=T),ChronicDZ=mean(CHRO_den,na.rm=T),
#                      BLE_prev=mean(BLE_prev,na.rm=T),AcuteDZ_prev=mean(DZGN_G_prev,na.rm=T),ChronicDZ_prev=mean(CHRO_prev,na.rm=T))
# 
# #Duplicate dataframe because the ddply step above takes a while to create. Allows you to tweak code below without having to rerun the ddply step above
# site.data.gen2<-site.data.gen



# GENERATE SUMMARY METRICS at the Segment-leveL BY TAXONCODE--------------------------------------------------
#Calc_ColDen_Transect
acd.tax<-Calc_ColDen_Seg(data = awd,grouping_field = "TAXONCODE");colnames(acd.tax)[colnames(acd.tax)=="ColCount"]<-"AdColCount";colnames(acd.tax)[colnames(acd.tax)=="ColDen"]<-"AdColDen";colnames(acd.tax)[colnames(acd.tax)=="SEGAREA"]<-"SEGAREA_ad"# calculate density at genus level as well as total
jcd.tax<-Calc_ColDen_Seg(jwd,"TAXONCODE"); colnames(jcd.tax)[colnames(jcd.tax)=="ColCount"]<-"JuvColCount";colnames(jcd.tax)[colnames(jcd.tax)=="ColDen"]<-"JuvColDen"
jcd.tax<-subset(jcd.tax,select=-c(SEGAREA))

#Calc_ColMetric_Transect
cl.tax<-Calc_ColMetric_Seg(data = awd,grouping_field = "TAXONCODE",pool_fields = "COLONYLENGTH"); colnames(cl.tax)[colnames(cl.tax)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.tax<-Calc_ColMetric_Seg(data = awd,grouping_field = "TAXONCODE",pool_fields = "OLDDEAD"); colnames(od.tax)[colnames(od.tax)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.tax<-Calc_ColMetric_Seg(data = awd,grouping_field = "TAXONCODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.tax)[colnames(rd.tax)=="Ave.y"]<-"Ave.rd" #Average % recent dead

#Calc_RDden_Transect
rdden.tax<-Calc_RDden_Seg(data=awd,grouping_field ="TAXONCODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.tax<-subset(rdden.tax,select = c(SITEVISITID,SITE,TRANSECT,SEGMENT,TAXONCODE,DZGN_G));colnames(acutedz.tax)[colnames(acutedz.tax)=="DZGN_G"]<-"DZGN_G_den" #subset just acute diseased colonies

#Calc_CONDden_Transect
condden.tax<-Calc_CONDden_Seg(data=awd,grouping_field ="TAXONCODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.tax<-subset(condden.tax,select = c(SITEVISITID,SITE,TRANSECT,SEGMENT,TAXONCODE,BLE));colnames(ble.tax)[colnames(ble.tax)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.tax<-subset(condden.tax,select = c(SITEVISITID,SITE,TRANSECT,SEGMENT,TAXONCODE,CHRO));colnames(chronicdz.tax)[colnames(chronicdz.tax)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies

#Calc_Richness_Transect
#rich.tax<-Calc_Richness_Transect(awd,"TAXONCODE")

#ADD CODE TO CHANGE TRANSECT NUMBERS FOR JUVENILES
jcd.tax$TRANSECT[jcd.tax$TRANSECT==3]<-1
jcd.tax$TRANSECT[jcd.tax$TRANSECT==4]<-2


#Merge density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","SEGMENT","TAXONCODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.tax<-Reduce(MyMerge, list(acd.tax,jcd.tax,cl.tax,od.tax,rd.tax,acutedz.tax,chronicdz.tax,ble.tax));
head(data.tax)

#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.tax$JuvColCount[is.na(data.tax$JuvColCount)]<-0;data.tax$JuvColDen[is.na(data.tax$JuvColDen)]<-0
data.tax$AdColCount[is.na(data.tax$AdColCount)]<-0;data.tax$AdColDen[is.na(data.tax$AdColDen)]<-0

#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.tax$DZGN_G_prev<-(data.tax$DZGN_G_den*data.tax$SEGAREA_ad)/data.tax$AdColCount*100
data.tax$BLE_prev<-(data.tax$BLE_den*data.tax$SEGAREA_ad)/data.tax$AdColCount*100
data.tax$CHRO_prev<-(data.tax$CHRO_den*data.tax$SEGAREA_ad)/data.tax$AdColCount*100


head(data.tax)
head(subset(data.tax,TAXONCODE==0))



# Prepare Diver data to merge with SfM data -------------------------------
#Make sure you have all transects and segments
levels(as.factor(data.tax$TRANSECT))
levels(as.factor(data.tax$SEGMENT))

#Merge survey segment with segment-level demographic data
diver.tax<-merge(survey_segment,data.tax,by=c("SITE","SITEVISITID","TRANSECT","SEGMENT"),all.y=T)
if(nrow(data.tax)!=nrow(diver.tax)) {cat("Warning: dataframes did not merge properly")}   
head(diver.tax)

