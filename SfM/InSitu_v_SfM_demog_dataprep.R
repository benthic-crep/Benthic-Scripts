
#DIVER/ADULT: CLEAN ANALYSIS READY DATA----------------------------------------
# This script will clean the raw benthic REA data using method E that comes directly from the new data base application.
rm(list=ls())

#Set Run Flags
DEBUG=TRUE

#LOAD LIBRARY FUNCTIONS ... 
# source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
# source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
# source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

source("C:/Users/Corinne.Amir/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Corinne.Amir/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Corinne.Amir/Documents/GitHub/fish-paste/lib/GIS_functions.R")


## DIVER-ADULT: Load benthic data 
# setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_ADULTCORAL_RAW_2013-2019.rdata") #from oracle

x<-df
x$SITE<-SiteNumLeadingZeros(x$SITE) # Change site number such as MAR-22 to MAR-0022

#Convert date formats
class(x$DATE_)
x$DATE_ <- as.Date(x$DATE_, format = "%Y-%m-%d")


### Use these functions to look at data
head(x)
tail(x)
sapply(x, unique)
table(x$REGION, x$OBS_YEAR) #review years and regions in dataframe

#Convert Segment number from old to new numbering system
x$SEGMENT<-ConvertSegNumber(x)

head(table(x$SITE,x$SEGMENT)) #Double check that segment numbers are correct


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
#Logical NAs within RECENT_GENERAL/SPECIFIC_CAUSE_CODEs, conditions, and extents, but only SEVERITY_4 = logical class


#DIVER/ADULT: Column Names Changes... -------------------------------------------------
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


#DIVER/ADULT: Merge Diver/Adult data and SURVEY MASTER -------------------------------------
#SURVEY MASTER was created by Ivor and Courtney by extracting sites directly from the Site Visit table from Oracle. It should be the complete list of sites surveyed since 2000
#survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
setwd("C:/Users/Corinne.Amir/Documents/GitHub/Benthic-Scripts/SfM")
survey_master <- read.csv("SURVEY MASTER.csv")

#Check that OBS_YEAR, SITEVISITID, and SITE are all the same in both x and survey master
OYerror=which(x$OBS_YEAR!=survey_master$OBS_YEAR[match(x$SITEVISITID,survey_master$SITEVISITID)])
SIerror=which(as.vector(x$SITE)!=survey_master$SITE[match(x$SITEVISITID,survey_master$SITEVISITID)])
SIOYerrors=unique(c(OYerror,SIerror))
if(length(SIOYerrors)>0){print(paste0("Warning: Raw Data disagree with Survey Master for sitevisitids: ",x$SITEVISITID[SIOYerrors]))}

#add SITE MASTER information to x 
length(unique(x$SITEVISITID)) #check the number of sites in demographic data
#join 'em
x<- inner_join(x, survey_master[,c("OBS_YEAR","SITEVISITID","SITE","SEC_NAME","ANALYSIS_YEAR","bANALYSIS_SCHEME","MIN_DEPTH_M","MAX_DEPTH_M")], by=c("OBS_YEAR","SITEVISITID","SITE"))

#Ensure that all rows in X have properly assigned SEC_NAME...
####CHECK THAT all SEC_NAME are present in the survey_master file
test<-x[is.na(x$SEC_NAME), c("MISSIONID","REGION", "SITE","OBS_YEAR"),]
test<-droplevels(test);table(test$SITE,test$MISSIONID) #create a table of missing sites by missionid
if(dim(test)[1]>0) {cat("Warning: sites with MISSING SECTORS present")}   # should be 0

#Create a list of missing sites that can be imported into the SITE MASTER file if needed
test<-x[is.na(x$SEC_NAME),]
miss.sites<-ddply(test,.(OBS_YEAR,SITEVISITID,SITE,MISSIONID,REGION,REGION_NAME,ISLAND,LATITUDE,LONGITUDE,
                         REEF_ZONE,DEPTH_BIN,DATE_,EXCLUDE_FLAG,HABITAT_CODE),
                  summarize,temp=median(SITEVISITID),SITE_MAX_DEPTH=median(SITE_MAX_DEPTH),SITE_MIN_DEPTH=median(SITE_MIN_DEPTH))
#Should be a 0 row data.frame
head(miss.sites,20)

#DIVER/ADULT: CLEAN UP ----------------------------------------------------------------

#Generate General RD cause code
gencodes<-read.csv("T:/Benthic/Data/SpGen_Reference/GeneralRDcode_lookup.csv")
head(x)
levels(x$RD1)

#Remove exisiting GENRD columns until Michael can fix database
x<-subset(x,select=-c(GENRD1,GENRD2,GENRD3))
x<- droplevels(x)

x<-CreateGenRDCode(x,"RD1","GENRD1",gencodes) #Creates an additional column in x (called GENRD#) that has the generalized code associated with the given RD#--ok
x<-CreateGenRDCode(x,"RD2","GENRD2",gencodes)
x<-CreateGenRDCode(x,"RD3","GENRD3",gencodes)

x <- droplevels(x)
head(x)


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


#DIVER/ADULT: Assign TAXONCODE --------------------------------------------------------
#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
taxa<-read.csv("T:/Benthic/Data/SpGen_Reference/2013-19_Taxa_MASTER.csv")

#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
#x$TAXONCODE<-Convert_to_Taxoncode_tom(data = x,taxamaster = taxa)#not working need to ask tom
x<-Convert_to_Taxoncode(x)

#Check to make sure SPCODE was converted correctly
b <- x[x$SPCODE!=x$TAXONCODE,]
head(b[rowSums(is.na(b)) != ncol(b), ])

#If there are issues, use this code to create a list SPCODE (lowest taxonomic resolution we have), TAXONCODE (the taxonomic level we all feel comfortable with) and associated genera
#This is used for spot checking that TAXONCODE was converted properly & can be compared against TAXA MASTER 
SURVEY_INFO<-c("OBS_YEAR","REGION","SPCODE","TAXONCODE","GENUS_CODE","TAXONNAME")
test<-new_Aggregate_InputTable(x, SURVEY_INFO)
head(test) 

taxa_subset <- subset(taxa, REGION=="MHI"|REGION== "NWHI")

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


# CLEAN UP NAs ##
NegNineCheckCols=c("RDEXTENT1","GENRD1","RD1","RDEXTENT2","GENRD2","RD2","GENRD3","RD3",
                   "RDEXTENT3","CONDITION_1","CONDITION_2","CONDITION_3","EXTENT_1","EXTENT_2","EXTENT_3","SEVERITY_1",
                   "SEVERITY_2","SEVERITY_3","GENUS_CODE","S_ORDER")
x[,NegNineCheckCols][x[,NegNineCheckCols]==-9] <- NA #Convert missing numeric values to NA (they are entered as -9 in Oracle)


#DIVER/ADULT: Convert NAs --------------------------------------------------------------

#leave NAs for severity and extent? STILL MANY COLUMNS WITH LOGICAL NA = OK?
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
write.csv(awd,file="T:/Benthic/Data/SfM/Analysis Ready/HARAMP19_DIVERAdult_CLEANED.csv",row.names = F)


#Check number of Site-Segments that contain at least 2 divers ----------------------------------------------------
analyst.per.ss <- ddply(x,.(SITE, SEGMENT), summarize, num.analyst = n_distinct(DIVER))
analyst.per.ss <- filter(analyst.per.ss, num.analyst>1) # 153 SS with 2 divers


## DIVER/JUVENILE: CLEAN ANALYSIS READY DATA -------------------------------------------------------------------------------
## LOAD benthic data
#load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_JUVCORAL_RAW_2013-2019.rdata") #from oracle
#load("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Temp Sfm Files/ALL_REA_JUVCORAL_RAW_2013-2019.rdata") #from oracle
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_JUVCORAL_RAW_2013-2019.rdata") 

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
sapply(x,class)

##Change column names to make code easier to code
colnames(x)[colnames(x)=="TAXONCODE"]<-"SPCODE" #Change column name
colnames(x)[colnames(x)=="TRANSECTNUM"]<-"TRANSECT" #Change column name
colnames(x)[colnames(x)=="MINDEPTH"]<-"SITE_MIN_DEPTH" #Change column name
colnames(x)[colnames(x)=="MAXDEPTH"]<-"SITE_MAX_DEPTH" #Change column name

#Add column for method type
x$METHOD<-"DIVER"

head(x)


# Merge Juvenile data and SITE MASTER -------------------------------------
# load site master to merge with demographic data
#survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
survey_master <- read.csv("SURVEY MASTER.csv")

#add SITE MASTER information to x 
#x<-merge(x, survey_master[,c("SITE", "SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE) #Fish team's original code, we may want to create analysis scheme later in the 
length(unique(x$SITEVISITID)) #double check that sites weren't dropped

x<-join(x, survey_master[,c("OBS_YEAR","SITEVISITID","SITE","SEC_NAME","ANALYSIS_YEAR","bANALYSIS_SCHEME","MIN_DEPTH_M","MAX_DEPTH_M")], by=c("OBS_YEAR","SITEVISITID","SITE"))  
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


# DIVER/JUVENILES: CLEAN UP ----------------------------------------------------------------

##Remove sites that were only surveyed for photoquads but not demographics
#Note-photoquad only sites are not included in data prior to 2018
#Test whether there are missing values in the NO_SURVEY_YN column. The value should be 0 or -1
x.na<-x[is.na(x$NO_SURVEY_YN) & x$OBS_YEAR>2013,]
x.na
# test<-ddply(x.na,.(SITE),
#             summarize,
#             SEG=length(unique(SEGMENT)))
# test
x$NO_SURVEY_YN<-is.na(x$NO_SURVEY_YN)<-0 #Change NAs (blank cells) to 0
x<-subset(x,NO_SURVEY_YN==0)

#TEMPORARY FIX-SPEAK WITH DM TO CORRECT IN ORACLE
x<-subset(x,SEGLENGTH!="NA") #Remove segments that were not surveyed for coral demography
x<-subset(x,EXCLUDE_FLAG==0);head(subset(x,EXCLUDE_FLAG==-1))# this dataframe should be empty


#DIVER/JUVENILES: Assign TAXONCODE --------------------------------------------------------
#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
taxa<-read.csv("T:/Benthic/Data/SpGen_Reference/2013-19_Taxa_MASTER.csv")

#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
#x$TAXONCODE<-Convert_to_Taxoncode_tom(data = x,taxamaster = taxa)#not working need to ask tom
x<-Convert_to_Taxoncode(x)

#Check to make sure SPCODE was converted correctly
head(x[x$SPCODE!=x$TAXONCODE,])
x[x$SPCODE!=x$TAXONCODE, SURVEY_INFO] #Shows the specific instances where SPCODE is not identical to TAXONCODE--1 POCS present

#If there are issues use this code to create a list SPCODE (lowest taxonomic resolution we have), TAXONCODE (the taxonomic level we all feel comfortable with) and associated genera
#This is used for spot checking that TAXONCODE was converted properly & can be compared against TAXA MASTER 
SURVEY_INFO<-c("OBS_YEAR","REGION","SPCODE","TAXONCODE","GENUS_CODE","TAXONNAME")
test<-new_Aggregate_InputTable(x, SURVEY_INFO) #displays all SPCODES recorded within "x" and their corresponding TAXONCODE, GENUS_CODE,  and TAXONNAME
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
write.csv(jwd,file="T:/Benthic/Data/SfM/Analysis Ready/HARAMP19_DIVERJuv_CLEANED.csv",row.names = F)

#Check number of Site-Segments that contain at least 2 divers ----------------------------------------------------
analyst.per.ss <- ddply(x,.(SITE, SEGMENT), summarize, num.analyst = n_distinct(DIVER))
analyst.per.ss <- filter(analyst.per.ss, num.analyst>1) # 135 SS with 2 divers


# SFM/ADULT: CLEAN ANALYSIS READY DATA ----------------------------------------------------
df<-read.csv("T:/Benthic/Data/SfM/QC/HARAMP2019_QCdsfm_ADULT.csv")

x<-df
head(x)
View(x)
nrow(x)


#SfM/ADULT: Column Names Changes -------------------------------------------------
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
colnames(x)[colnames(x)=="FID"]<-"COLONYID" #Change column name

x<-subset(x,select=-c(MISSIONID)) #MISSION_ID not present within origional dataframe

#Add column for method type
x$METHOD<-"SfM"

if(DEBUG){head(x)}

table(x$SITE,x$ANALYST)


#SfM/ADULT: Adding and Modifying columns --------------------------------------------

#Fill in columns with values that we know should not be different across any of the rows
x$OBS_YEAR <- as.vector(rep(2019, times = nrow(x)))
x$COLONYLENGTH<-x$COLONYLENGTH*100 #convert from m to cm
x$S_ORDER<-ifelse(x$NO_COLONY_==0 & x$SPCODE!="NONE","Scleractinia","NONE") #add S_order column

#I manually removed the bleaching severity 1 colonies - ifelse wasn't working

#Create Genuscode and taxonname column from spcode
genlookup<-read.csv("T:/Benthic/Data/SpGen_Reference/Genus_lookup.csv")
x<-CreateGenusCode(x,genlookup) 
head(x)

#Generate General RD cause code
gencodes<-read.csv("T:/Benthic/Data/SpGen_Reference/GeneralRDcode_lookup.csv")
head(x)
levels(x$RD1)

x<-CreateGenRDCode(x,"RD1","GENRD1",gencodes)
x<-CreateGenRDCode(x,"RD2","GENRD2",gencodes)
x<-CreateGenRDCode(x,"RD3","GENRD3",gencodes)

head(x)
nrow(df);nrow(x) #make sure rows weren't dropped


#SfM/ADULT: Merge Adult data and  SURVEY MASTER  -------------------------------------
#survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
setwd("C:/Users/Corinne.Amir/Documents/GitHub/Benthic-Scripts/SfM")
survey_master <- read.csv("SURVEY MASTER.csv")

colnames(survey_master)[colnames(survey_master)=="LATITUDE_SV"]<-"LATITUDE" #Change column name
colnames(survey_master)[colnames(survey_master)=="LONGITUDE_SV"]<-"LONGITUDE" #Change column name


x<-left_join(x,survey_master[,c("REGION","MISSIONID","OBS_YEAR","ISLAND","SITEVISITID","SITE","SEC_NAME",
                            "REEF_ZONE","DEPTH_BIN","HABITAT_CODE","LATITUDE","LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")],by=c("OBS_YEAR","SITE"))

head(x)
nrow(x)


#SfM/ADULT: Assign TAXONCODE --------------------------------------------------------
#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
taxa<-read.csv("T:/Benthic/Data/SpGen_Reference/2013-19_Taxa_MASTER.csv")

x$SPCODE<-ifelse(x$NO_COLONY_==-1,"AAAA",as.character(x$SPCODE)) #currently no sites have -1 for adults (1/22/2020)


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

#There are some old SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genera or taxoncodes to the spcode and the remaining NAs in the Taxon and genus code to AAAA
x$GENUS_CODE<-ifelse(x$TAXONCODE=="UNKN","UNKN",x$GENUS_CODE)
x$TAXONCODE<-ifelse(x$SPCODE=="AAAA","AAAA",x$TAXONCODE)
x$GENUS_CODE<-ifelse(x$TAXONCODE=="AAAA","AAAA",x$GENUS_CODE)


View(x) #view data in separate window

#Check that Unknown scl were changed correctly
head(subset(x,TAXONCODE=="UNKN"&S_ORDER=="Scleractinia"),40)
head(subset(x,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia"))
head(subset(x,GENUS_CODE=="AAAA"))
head(subset(x,SPCODE=="AAAA"))


#In order to record no colonies observed in a segment, we need to create a small colony on the image.This code removes that size measure
x$COLONYLENGTH<-ifelse(x$SPCODE=="AAAA",0,as.character(x$COLONYLENGTH))


# sapply(x,levels)
head(x)
nrow(x)

#Reorder columns
x<-x[,c("METHOD","ANALYST", "REGION","OBS_YEAR","MISSIONID","ISLAND","SEC_NAME","SITEVISITID","SITE","REEF_ZONE","DEPTH_BIN",
  "HABITAT_CODE","LATITUDE","LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","SEGMENT","SEGLENGTH","SEGWIDTH",
  "SEGAREA","COLONYID","Fragment","S_ORDER","GENUS_CODE","SPCODE","TAXONCODE","TAXONNAME",
  "EX_BOUND","COLONYLENGTH","OLDDEAD","GENRD1","GENRD2","GENRD3","RD1","RDEXTENT1","RD2","RDEXTENT2","RD3",
  "RDEXTENT3","CONDITION_1","EXTENT_1","SEVERITY_1","CONDITION_2","EXTENT_2","SEVERITY_2","CONDITION_3","EXTENT_3","SEVERITY_3")]         
               

## CLEAN UP NAs ##
NegNineCheckCols=c("RDEXTENT1","GENRD1","RD1","RDEXTENT2","GENRD2","RD2","GENRD3","RD3",
                   "RDEXTENT3","CONDITION_1","CONDITION_2","CONDITION_3","EXTENT_1","EXTENT_2","EXTENT_3","SEVERITY_1",
                   "SEVERITY_2","SEVERITY_3","GENUS_CODE","S_ORDER")
x[,NegNineCheckCols][x[,NegNineCheckCols]==-9] <- NA #Convert missing numeric values to NA (they are entered as -9 in Oracle)


#SfM/ADULT: Clean up NAs ------------------------------------------------------------
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
write.csv(awd,file="T:/Benthic/Data/SfM/Analysis Ready/HARAMP19_SfMAdult_CLEANED.csv",row.names = F)

#Check number of Site-Segments that contain at least 2 annotators ----------------------------------------------------
analyst.per.ss<-x %>% filter(ANALYST=="RS" | ANALYST=="MW" | ANALYST=="MA")
analyst.per.ss$ANALYST<-droplevels(analyst.per.ss$ANALYST)
analyst.per.ss <- ddply(x,.(SITE, SEGMENT), summarize, num.analyst = n_distinct(ANALYST))
analyst.per.ss <- filter(analyst.per.ss, num.analyst>1) # 44 SS with 2 divers = 1 more than FINAL RESULT IN QC_CORRECT


# SFM/JUVENILE: CLEAN ANALYSIS READY DATA -------------------------------------
df<-read.csv("T:/Benthic/Data/SfM/QC/HARAMP2019_QCdsfm_JUV.csv") #851 rows

x<-df
head(x)
View(x)
nrow(x)

#SFM/JUVENILE: Column Names Changes... -------------------------------------------------
colnames(x)[colnames(x)=="FRAGMENT_Y"]<-"Fragment" #Change column name
colnames(x)[colnames(x)=="SHAPE_Leng"]<-"COLONYLENGTH" #Change column name
colnames(x)[colnames(x)=="FID"]<-"COLONYID" #Change column name

#Add column for method type
x$METHOD<-"SfM"

if(DEBUG){head(x)}

table(x$SITE,x$ANALYST)


#SFM/JUVENILE: Adding and Modifying columns --------------------------------------------

#Fill in columns with values that we know should not be different across any of the rows
x$OBS_YEAR <- as.vector(rep(2019, times = nrow(x)))
x$COLONYLENGTH<-x$COLONYLENGTH*100 #convert from m to cm
x$S_ORDER<-ifelse(x$NO_COLONY_==0 & x$SPCODE!="NONE","Scleractinia","NONE") #add S_order column

#Create Genuscode and taxonname column from spcode
genlookup<-read.csv("T:/Benthic/Data/SpGen_Reference/Genus_lookup.csv")
x<-CreateGenusCode(x,genlookup) 
head(x)


#SFM/JUVENILE: Merge Juvenile data and SURVEY MASTER (SfM) -------------------------------------
#survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
survey_master <- read.csv("SURVEY MASTER.csv")

colnames(survey_master)[colnames(survey_master)=="LATITUDE_SV"]<-"LATITUDE" #Change column name
colnames(survey_master)[colnames(survey_master)=="LONGITUDE_SV"]<-"LONGITUDE" #Change column name


x<-left_join(x,survey_master[,c("REGION","MISSIONID","OBS_YEAR","ISLAND","SITEVISITID","SITE","SEC_NAME",
                                "REEF_ZONE","DEPTH_BIN","HABITAT_CODE","LATITUDE","LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")],by=c("OBS_YEAR","SITE"))

head(x)
nrow(x)



#SFM/JUVENILE: Assign TAXONCODE --------------------------------------------------------
#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
taxa<-read.csv("T:/Benthic/Data/SpGen_Reference/2013-19_Taxa_MASTER.csv")

x$SPCODE<-ifelse(x$NO_COLONY_==-1,"AAAA",as.character(x$SPCODE)) #add S_order column


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

#There are some old SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genera or taxoncodes to the spcode and the remaining NAs in the Taxon and genus code to AAAA
x$GENUS_CODE<-ifelse(x$TAXONCODE=="UNKN","UNKN",x$GENUS_CODE)
x$TAXONCODE<-ifelse(x$SPCODE=="AAAA","AAAA",x$TAXONCODE)
x$GENUS_CODE<-ifelse(x$TAXONCODE=="AAAA","AAAA",x$GENUS_CODE)

View(x) #view data in separate window

#Check that Unknown scl were changed correctly
head(subset(x,TAXONCODE=="UNKN"&S_ORDER=="Scleractinia"),40)
head(subset(x,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia"))
head(subset(x,GENUS_CODE=="AAAA"))
head(subset(x,SPCODE=="AAAA"))

#head() function in this case (1/22/2020) have unequal nrow, so double check that UNKNs are in the same rows for all columns containing UNKN
a1<- subset(x,TAXONCODE=="UNKN"&S_ORDER=="Scleractinia")
a2<- subset(x,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia")


#In order to record no colonies observed in a segment, we need to create a small colony on the image.This code removes that size measure
x$COLONYLENGTH<-ifelse(x$SPCODE=="AAAA",0,as.character(x$COLONYLENGTH))

#Remove colonies with EX_BOUND = -1
x <- subset(x, EX_BOUND==0)

# sapply(x,levels)
head(x)
nrow(x)

#Reorder columns
x<-x[,c("METHOD","ANALYST", "REGION","OBS_YEAR","MISSIONID","ISLAND","SEC_NAME","SITEVISITID","SITE","REEF_ZONE","DEPTH_BIN",
        "HABITAT_CODE","LATITUDE","LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","SEGMENT","SEGLENGTH","SEGWIDTH",
        "SEGAREA","COLONYID","Fragment","S_ORDER","GENUS_CODE","SPCODE","TAXONCODE","TAXONNAME",
        "EX_BOUND","COLONYLENGTH")]         


## CLEAN UP NAs ##
NegNineCheckCols=c("S_ORDER","TAXONNAME","MIN_DEPTH_M","MAX_DEPTH_M","COLONYLENGTH")
x[,NegNineCheckCols][x[,NegNineCheckCols] ==-9] <- NA #Convert missing numeric values to NA (they are entered as -9 in Oracle)


jwd<-droplevels(x)
write.csv(jwd,file="T:/Benthic/Data/SfM/Analysis Ready/HARAMP19_SfMJuv_CLEANED.csv",row.names = F)

#Check number of Site-Segments that contain at least 2 divers ----------------------------------------------------
analyst.per.ss <- ddply(x,.(SITE, SEGMENT), summarize, num.analyst = n_distinct(ANALYST))
analyst.per.ss <- filter(analyst.per.ss, num.analyst>1) # 44 SS with 2 divers = SAME AS FINAL SS COUNT IN QC_CORRECT

