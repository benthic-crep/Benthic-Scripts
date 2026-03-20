#This Script: for both Adults (AD) and juveniles (JV)
# (1) Cleans the most recent year's benthic REA Demography data (e.g. 2024), for adults and juveniles
# (2) Pulls Old "Clean" REA Data and adds new data to it, saves it to t: drive

#It's a modification from REA Coral Demography_1_DataPrep_2025TAO

#CREATE ADULT CLEAN ANALYSIS READY DATA FROM NEWEST YEAR - 2024 -------------------------------------
# This script will clean the raw benthic REA data using method E that comes directly from the new data base application.
#Note- these data represent the revised data structure instituted in November 2018 and 2019. Several recent dead and condition columns were added
#These data only include surveys conducted between 2013-2020
#NOTE: Depth should not be used the in the raw data because the column was deprecated in Oracale and is inconsistent.
#Use depth data from SURVEY MASTER

# ---- Section 0: Prep ----

# --- Section 0.1 Clear Data and Packages ---
rm(list=ls())
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

# --- Section 0.2: Set Run Flags ---
DEBUG=TRUE

# --- Section 0.3: LOAD LIBRARY FUNCTIONS ...
source("../fish-paste/lib/core_functions.R")
source("./Functions/Benthic_Functions_newApp_v2025TAOfork.R")
source("./Functions/Core_Benthic_Aggregation_Functions_2025.R")
#source("../fish-paste/lib/GIS_functions.R") #This code contains functions to find ReefZone and SEC_NAME from a shapefile,
#but uses the now defunct rgdal to do so.... BAD BAD

# ---- Section 1: Load, Clean, Write New Adult Data ----

## LOAD benthic data from most recent year
loadname=load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/FY26/Raw_REA_ADULT_2024.rdata") #from oracle
AD<-eval(parse(text=loadname)); rm(list=loadname)

#Add zeros to beginning of site number so we avoid MAR-22 changing to March 22
AD$SITE<-SiteNumLeadingZeros(as.factor(AD$SITE))  #function in "fish-paste core_functions"

#Convert date formats
AD$DATE_og=AD$DATE_
AD$DATE_ <- ymd_hms(AD$DATE_og)
which(is.na(AD$DATE_))

#review years and regions in dataframe
table(AD$REGION, AD$OBS_YEAR) 
table(AD$REGION, AD$MISSIONID) 
AD %>% filter(MISSIONID=="MP2502") %>% group_by(ISLANDCODE,OBS_MONTH) %>% summarize(NSites=length(unique(SITE)))

#Drop Special Missions (ie don't include tutuila LBSP data)
AD=AD %>% filter(REGION%in%c("MHI","NWHI"))

#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","REGION_NAME","ISLAND","ISLANDCODE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","NO_SURVEY_YN","SITEVISITID", "SITE","DIVER","TRANSECTNUM","SEGMENT","SEGWIDTH","SEGLENGTH","FRAGMENT_YN",
             "COLONYID","TAXONCODE","MORPHOLOGY","COLONYLENGTH","OLDDEAD",
             "RECENTDEAD_1","RECENT_GENERAL_CAUSE_CODE_1","RECENT_SPECIFIC_CAUSE_CODE_1",
             "RECENTDEAD_2",	"RECENT_GENERAL_CAUSE_CODE_2","RECENT_SPECIFIC_CAUSE_CODE_2",
             "RECENT_GENERAL_CAUSE_CODE_3","RECENT_SPECIFIC_CAUSE_CODE_3","RECENTDEAD_3","CONDITION_1",
             "CONDITION_2","CONDITION_3","EXTENT_1","EXTENT_2","EXTENT_3","SEVERITY_1","SEVERITY_2","SEVERITY_3",
             "GENUS_CODE","S_ORDER","TAXONNAME","HABITAT_CODE","MORPH_CODE")

#Added HABITAT_CODE and MORPH_CODE to the list now that Lori has added them to the NCEI views

#remove extraneous columns
head(AD[,DATA_COLS])
AD<-AD[,DATA_COLS]

#Double check level and class of variables to make sure there aren't any errors
sapply(AD,levels)
sapply(AD,class)##Change column names to make code easier to code

# Column Names Changes...
AD=AD %>% rename(SPCODE=TAXONCODE,#Change column name- we will eventually change this column back to "taxoncode" after we modify the spcode names to match the taxalist we all feel comfortable identifying
              TRANSECT=TRANSECTNUM, #Change column name
              RDEXTENT1=RECENTDEAD_1, #Change column name
              GENRD1=RECENT_GENERAL_CAUSE_CODE_1, #Change column name
              RD1=RECENT_SPECIFIC_CAUSE_CODE_1, #Change column name
              RDEXTENT2=RECENTDEAD_2, #Change column name
              RDEXTENT3=RECENTDEAD_3, #Change column name
              GENRD2=RECENT_GENERAL_CAUSE_CODE_2, #Change column name
              RD2=RECENT_SPECIFIC_CAUSE_CODE_2, #Change column name
              GENRD3=RECENT_GENERAL_CAUSE_CODE_3, #Change column name
              RD3=RECENT_SPECIFIC_CAUSE_CODE_3, #Change column name
              Fragment=FRAGMENT_YN) #Change column name

# Merge Adult data and  SURVEY MASTER
#SURVEY MASTER was created by Ivor and Courtney by extracting sites directly from the Site Visit table from Oracle. It should be the complete list of sites surveyed since 2000
survey_master<-read.csv("./NCRMP/FY26 Benthic Pipeline/A. Survey Master Prep/SURVEY_MASTER_2024_benthic.csv")

#Convert date formats ###2025TAO Update - use lubridate instead of as.Date
survey_master$DATE_RAW=survey_master$DATE_
survey_master$DATE_=mdy(survey_master$DATE_)
survey_master$DATE_[which(is.na(survey_master$DATE_))]=mdy_hms(survey_master$DATE_RAW[which(is.na(survey_master$DATE_))])
survey_master$DATE_[which(is.na(survey_master$DATE_))]=ymd_hms(survey_master$DATE_RAW[which(is.na(survey_master$DATE_))])
length(which(is.na(survey_master$DATE_)))

#Use SM coordinates-some coordinates are wrong in data and need to be updated
survey_master=survey_master %>% rename(LATITUDE=LATITUDE_LOV,
                                       LONGITUDE=LONGITUDE_SV)

#Check that OBS_YEAR, SITEVISITID, and SITE are all the same in both AD and survey master
OYerror<-which(AD$OBS_YEAR!=survey_master$OBS_YEAR[match(AD$SITEVISITID,survey_master$SITEVISITID)])
SIerror<-which(as.vector(AD$SITE)!=survey_master$SITE[match(AD$SITEVISITID,survey_master$SITEVISITID)])
SIOYerrors<-unique(c(OYerror,SIerror))
if(length(SIOYerrors)>0){print(paste0("Warning: Raw Data disagree with Survey Master for sitevisitids: ",AD$SITEVISITID[SIOYerrors]))}

#merge 'em NOTE: left-join will spit out a Warning message that you are joining on factors that have different levels. Basically you have more sites in survey master than AD. This is correct and can be ignored here.
AD<-left_join(AD,
             survey_master[,c("OBS_YEAR","SITEVISITID","SITE","LATITUDE","LONGITUDE",
                              "SEC_NAME","ANALYSIS_YEAR","bANALYSIS_SCHEME","new_MIN_DEPTH_M","new_MAX_DEPTH_M")],
             by = join_by(OBS_YEAR, SITEVISITID, SITE))
AD=AD %>% rename(MIN_DEPTH_M=new_MIN_DEPTH_M,
                 MAX_DEPTH_M=new_MAX_DEPTH_M)

#Ensure that all rows in X have properly assigned SEC_NAME...
####CHECK THAT all SEC_NAME are present in the survey_master file
test<-AD[is.na(AD$SEC_NAME), c("MISSIONID","REGION", "SITE","OBS_YEAR"),]
test<-droplevels(test);table(test$SITE,test$MISSIONID) #create a table of missing sites by missionid
if(dim(test)[1]>0) {cat("Warning: sites with MISSING SECTORS present")}   # should be 0

#Create a list of missing sites that can be imported into the SITE MASTER file if needed
test<-AD[is.na(AD$SEC_NAME),]
miss.sites<-ddply(test,.(OBS_YEAR,SITEVISITID,SITE,MISSIONID,REGION,REGION_NAME,ISLAND,LATITUDE,LONGITUDE,
                         REEF_ZONE,DEPTH_BIN,DATE_),
                  summarize,temp=median(SITEVISITID))
#Should be a 0 row data.frame
head(miss.sites,20)


# CLEAN UP
##Remove sites that were only surveyed for photoquads but not demographics
#Note-photoquad only sites were not included in data prior to 2018
#Test whether there are missing values in the NO_SURVEY_YN column. The value should be 0 or -1
AD.na<-AD[is.na(AD$NO_SURVEY_YN)&AD$OBS_YEAR>2013,]
AD.na
AD$NO_SURVEY_YN[is.na(AD$NO_SURVEY_YN)]<-0 #Change NAs (blank cells) to 0 - fix in the database

##Acutally do the removal of transects that were only surveyed for photoquads but not demographics
nrow(AD)
AD=AD %>% filter(NO_SURVEY_YN==0)
nrow(AD)

#Change NAs in RecentDead extent to 0  - note, NWHI 2014,2015 and 2017 only one recent dead and condition category were recorded - fix in the database
#AD=AD %>% mutate(across(c(RDEXTENT1,RDEXTENT2,RDEXTENT3),~replace_na(., 0)))
head(subset(AD,S_ORDER=="Scleractinia" & is.na(AD$RDEXTENT1))) #identify columns that have NAs
AD$RDEXTENT1<-ifelse(AD$S_ORDER=="Scleractinia"& is.na(AD$RDEXTENT1),0,AD$RDEXTENT1)
head(subset(AD,S_ORDER=="Scleractinia" & is.na(AD$RDEXTENT2))) #identify columns that have NAs
AD$RDEXTENT2<-ifelse(AD$S_ORDER=="Scleractinia"& is.na(AD$RDEXTENT2),0,AD$RDEXTENT2)
head(subset(AD,S_ORDER=="Scleractinia" & is.na(AD$RDEXTENT3))) #identify columns that have NAs
AD$RDEXTENT3<-ifelse(AD$S_ORDER=="Scleractinia"& is.na(AD$RDEXTENT3),0,AD$RDEXTENT3)

# Assign TAXONCODE
### On Friday 11/21/2025 CC and TAO discussed the taxonomic approach in our REA data and decided
#(1) That existing "analysis ready" data from 13-23 shall remain unchanged
#(2) That the "Taxa_MASTER" file will shift from a "if species code is listed, keep it, else genus" to a more explicit
# pairing of SPCODE and TAXONCODE that varys from OBS_YEAR and REGION. i.e. currently we only have SPCODE in the Taxa_MASTER,
#and if you're code is not present, you roll to genus (i.e. genus by omission). To better track changes, especially with an increasing 
#number of species complex codes in use, we will move to a complete lookup for all raw SPCODEs that explicitly match in the TAXONCODE column 
#to: 1) the appropriate SPCODE, 2) the GENUS_CODE, 3) or an explicit NA (for taxa not present in the region)
#to map this we will write a new "Convert_to_Taxoncode_2025" function.
#Setup Code in "A2. Taxonomic Master Generation REA.R "

#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
taxa<-read.csv("T:/Benthic/Data/Lookup Tables/2013-24_Taxa_MASTER_2025VERSION.csv")

#OBS_YEAR to factor
AD$OBS_YEAR<-as.factor(AD$OBS_YEAR)#convert to factor to merge with taxa master

#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
#This function will give you warnings that you are joining factors with different levels. THis is correct, but doesn't matter. Ignore
#function in Benthic_Functions_newApp_v2025TAOfork.R
AD$TAXONCODE<-Convert_to_Taxoncode_2025(data = AD,taxamaster = taxa)
nrow(AD)

#Check to make sure SPCODE was converted correctly
AD %>%
  dplyr::select(MISSIONID,REGION,ISLANDCODE,SEC_NAME,REEF_ZONE,DEPTH_BIN,SITE,OBS_YEAR,SPCODE,TAXONCODE) %>%
  filter(SPCODE!=TAXONCODE) %>% distinct() %>% View()
#View(subset(AD,SPCODE!=TAXONCODE))
#2025 TAO: Apparently in 2024, we're sticking to the codes going in...

#If there are issues use this code to create a list SPCODE (lowest taxonomic resolution we have), TAXONCODE (the taxonomic level we all feel comfortable with) and associated genera
#This is used for spot checking that TAXONCODE was converted properly & can be compared against TAXA MASTER
SURVEY_INFO<-c("OBS_YEAR","REGION","SPCODE","TAXONCODE","GENUS_CODE","TAXONNAME")
#test<-new_Aggregate_InputTable(AD, SURVEY_INFO) # A very silly function indeed. 
test<-AD %>% dplyr::select(all_of(SURVEY_INFO)) %>% distinct()

#Check to see whether S_ORDER is NA and not AAAA (the code for no colonies observed on the segment)
AD[AD$SPCODE!="AAAA" & is.na(AD$S_ORDER),] #this dataframe should be empty

#Change columns to character
AD$GENUS_CODE<-as.character(AD$GENUS_CODE)
AD$SPCODE<-as.character(AD$SPCODE)
AD$TAXONCODE<-as.character(AD$TAXONCODE)
AD$S_ORDER<-as.character(AD$S_ORDER)

#Make sure there are no NA values in genus code or taxoncode if it's supposed to be a scleractinian
subset(AD,S_ORDER=="Scleractinia" & GENUS_CODE=="NA") #this dataframe should be empty
subset(AD,S_ORDER=="Scleractinia" & TAXONCODE=="NA") #this dataframe should be empty


#In 2023 we created several species complexes for taxa that are very difficult to tell apart
#2025 TAO: but for most years we ignore them!
AD$TAXONCODE<-ifelse(AD$TAXONCODE %in% c("PMEA","PVER"),"PMVC",AD$TAXONCODE)
AD$TAXONCODE<-ifelse(AD$TAXONCODE %in% c("PGRA","PWOO","PEYD"),"PGWC",AD$TAXONCODE)
AD$TAXONCODE<-ifelse(AD$TAXONCODE %in% c("PMON","PRUS"),"PMRC",AD$TAXONCODE)
AD$TAXONCODE<-ifelse(AD$TAXONCODE == "MONS","ASTS",AD$TAXONCODE)
AD$TAXONCODE<-ifelse(AD$TAXONCODE == "MCUR","ACUR",AD$TAXONCODE)
AD$GENUS_CODE<-ifelse(AD$GENUS_CODE == "MONS","ASTS",AD$GENUS_CODE)


AD$TAXONNAME<-ifelse(AD$TAXONCODE== "PMVC","Pocillopora meandrina/verrucosa complex",AD$TAXONNAME)
AD$TAXONNAME<-ifelse(AD$TAXONCODE == "PGWC","Pocillopora grandis/woodjonesi complex",AD$TAXONNAME)
AD$TAXONNAME<-ifelse(AD$TAXONCODE == "PMRC", "Porites monticulosa/rus complex", AD$TAXONNAME)
AD$TAXONNAME<-ifelse(AD$TAXONCODE == "ASTS","Astrea sp",AD$TAXONNAME)
AD$TAXONNAME<-ifelse(AD$TAXONCODE == "ACUR","Astrea curta",AD$TAXONNAME)


#Fix missing NAs if need be
# AD$GENUS_CODE<-ifelse(is.na(AD$GENUS_CODE)&AD$S_ORDER=="Scleractinia",AD$SPCODE,AD$GENUS_CODE)
# AD$TAXONCODE<-ifelse(is.na(AD$TAXONCODE)&AD$S_ORDER=="Scleractinia",AD$SPCODE,AD$TAXONCODE)

#There are some old SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genera or taxoncodes to the spcode and the remaining NAs in the Taxon and genus code to AAAA
AD$GENUS_CODE<-ifelse(AD$TAXONCODE=="UNKN","UNKN",AD$GENUS_CODE)
AD$TAXONCODE<-ifelse(AD$SPCODE=="AAAA","AAAA",AD$TAXONCODE)
AD$GENUS_CODE<-ifelse(AD$TAXONCODE=="AAAA","AAAA",AD$GENUS_CODE)
AD$TAXONCODE<-ifelse(AD$SPCODE %in% c("MOAS","LEPA"),"UNKN",AD$TAXONCODE)
AD$GENUS_CODE<-ifelse(AD$SPCODE %in% c("MOAS","LEPA"),"UNKN",AD$GENUS_CODE)

View(AD) #view data in separate window

#Check that Unknown scl were changed correctly
head(subset(AD,TAXONCODE=="UNKN"&S_ORDER=="Scleractinia"))
head(subset(AD,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia"))
head(subset(AD,GENUS_CODE=="AAAA"))
head(subset(AD,SPCODE=="AAAA"))


##Calcuating segment and transect area and add column for transect area
AD$TRANSECTAREA<-Transectarea(AD)
# sapply(AD,levels)
head(AD)
nrow(AD)


## CLEAN UP NAs ##
NegNineCheckCols=c("COLONYLENGTH","OLDDEAD","RDEXTENT1","GENRD1","RD1","RDEXTENT2","GENRD2","RD2","GENRD3","RD3",
                   "RDEXTENT3","CONDITION_1","CONDITION_2","CONDITION_3","EXTENT_1","EXTENT_2","EXTENT_3","SEVERITY_1",
                   "SEVERITY_2","SEVERITY_3","GENUS_CODE","S_ORDER")

#TAO2025 A crazy way to index! But if it ain't broke...
AD[,NegNineCheckCols][AD[,NegNineCheckCols]==-9] <- NA #Convert missing numeric values to NA (they are entered as -9 in Oracle)- make sure these aren't converted to 0 later on
#View(AD)


#TAO2025 - none of these are factors...
tmp.lev<-levels(AD$GENRD1); head(tmp.lev)
levels(AD$GENRD1)<-c(tmp.lev, "NONE") # add to NONE
AD[is.na(AD$GENRD1),"GENRD1"]<-"NONE"

tmp.lev<-levels(AD$RD1); head(tmp.lev)
levels(AD$RD1)<-c(tmp.lev, "NONE")
AD[is.na(AD$RD1),"RD1"]<-"NONE"

tmp.lev<-levels(AD$GENRD2); head(tmp.lev)
levels(AD$GENRD2)<-c(tmp.lev, "NONE")
AD[is.na(AD$GENRD2),"GENRD2"]<-"NONE"

tmp.lev<-levels(AD$RD2); head(tmp.lev)
levels(AD$RD2)<-c(tmp.lev, "NONE")
AD[is.na(AD$RD2),"RD2"]<-"NONE"

tmp.lev<-levels(AD$GENRD3); head(tmp.lev)
levels(AD$GENRD3)<-c(tmp.lev, "NONE")
AD[is.na(AD$GENRD3),"GENRD3"]<-"NONE"

tmp.lev<-levels(AD$RD3); head(tmp.lev)
levels(AD$RD3)<-c(tmp.lev, "NONE")
AD[is.na(AD$RD3),"RD3"]<-"NONE"

tmp.lev<-levels(AD$CONDITION_1); head(tmp.lev)
levels(AD$CONDITION_1)<-c(tmp.lev, "NONE")
AD[is.na(AD$CONDITION_1),"CONDITION_1"]<-"NONE"

tmp.lev<-levels(AD$CONDITION_2); head(tmp.lev)
levels(AD$CONDITION_2)<-c(tmp.lev, "NONE")
AD[is.na(AD$CONDITION_2),"CONDITION_2"]<-"NONE"

tmp.lev<-levels(AD$CONDITION_3); head(tmp.lev)
levels(AD$CONDITION_3)<-c(tmp.lev, "NONE")
AD[is.na(AD$CONDITION_3),"CONDITION_3"]<-"NONE"

head(AD)
dim(AD)

awd<-droplevels(AD)

#(2-AD)

write.csv(awd,file="T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED_2024.csv",row.names = FALSE)
table(awd$OBS_YEAR)

# ---- Section 2: Load, Clean, Write New Juvenile Data ----

## LOAD juvenile benthic data from most recent year
loadname=load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/FY26/Raw_REA_JUVENILE_2024.rdata") #from oracle
JV<-eval(parse(text=loadname)) #leave this as df

#Add zeros to beginning of site number so we avoid MAR-22 changing to March 22
JV$SITE<-SiteNumLeadingZeros(as.factor(JV$SITE))  #function in "fish-paste core_functions"

#Convert date formats
JV$DATE_og=JV$DATE_
JV$DATE_ <- ymd_hms(JV$DATE_og)
which(is.na(JV$DATE_))

#Drop Special Missions (ie don't include tutuila LBSP data)
JV=JV %>% filter(REGION%in%c("MHI","NWHI"))

#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","REGION_NAME","ISLAND","ISLANDCODE","SITE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","NO_SURVEY_YN","SITEVISITID","DIVER","TRANSECTNUM","SEGMENT","SEGWIDTH","SEGLENGTH",
             "COLONYID","TAXONCODE","COLONYLENGTH","GENUS_CODE","S_ORDER","TAXONNAME","HABITAT_CODE","MORPH_CODE")

#remove extraneous columns
head(JV[,DATA_COLS])
JV<-JV[,DATA_COLS]

### Use these functions to look at data
table(JV$REGION, JV$OBS_YEAR) #review years and regions in dataframe

#Double check level and class of variables to make sure there aren't any errors
sapply(JV,levels)
sapply(JV,class)##Change column names to make code easier to code

# Column Names Changes...
JV=JV %>% rename(SPCODE=TAXONCODE,#Change column name- we will eventually change this column back to "taxoncode" after we modify the spcode names to match the taxalist we all feel comfortable identifying
                 TRANSECT=TRANSECTNUM) #Change column name

# Merge Juvenile data and SITE MASTER
# load site master to merge with demographic data
survey_master<-read.csv("./NCRMP/FY26 Benthic Pipeline/A. Survey Master Prep/SURVEY_MASTER_2024_benthic.csv")

#Use SM coordinates-some coordinates are wrong in data and need to be updated
colnames(survey_master)[colnames(survey_master)=="LATITUDE_LOV"]<-"LATITUDE" #Change column name- we will eventually change this column back to "taxoncode" after we modify the spcode names to match the taxalist we all feel comfortable identifying
colnames(survey_master)[colnames(survey_master)=="LONGITUDE_LOV"]<-"LONGITUDE" #Change column name- we will eventually change this column back to "taxoncode" after we modify the spcode names to match the taxalist we all feel comfortable identifying

#Check that OBS_YEAR, SITEVISITID, and SITE are all the same in both JV and survey master
OYerror<-which(JV$OBS_YEAR!=survey_master$OBS_YEAR[match(JV$SITEVISITID,survey_master$SITEVISITID)])
SIerror<-which(as.vector(JV$SITE)!=survey_master$SITE[match(JV$SITEVISITID,survey_master$SITEVISITID)])
SIOYerrors<-unique(c(OYerror,SIerror))
if(length(SIOYerrors)>0){print(paste0("Warning: Raw Data disagree with Survey Master for sitevisitids: ",JV$SITEVISITID[SIOYerrors]))}

#merge 'em NOTE: left-join will spit out a Warning message that you are joining on factors that have different levels. Basically you have more sites in survey master than JV. This is correct and can be ignored here.
JV<-left_join(JV, survey_master[,c("OBS_YEAR","SITEVISITID","SITE","LATITUDE","LONGITUDE","SEC_NAME","ANALYSIS_YEAR","bANALYSIS_SCHEME","new_MIN_DEPTH_M","new_MAX_DEPTH_M")])

colnames(JV)[colnames(JV)=="new_MIN_DEPTH_M"]<-"MIN_DEPTH_M" #Change column name
colnames(JV)[colnames(JV)=="new_MAX_DEPTH_M"]<-"MAX_DEPTH_M" #Change column name

#CHECK THAT all SEC_NAME are present in the survey_master file
test<-JV[is.na(JV$SEC_NAME), c("MISSIONID","REGION", "SITE","OBS_YEAR"),]
test<-droplevels(test);table(test$SITE,test$MISSIONID) #create a table of missing sites by missionid
if(dim(test)[1]>0) {cat("sites with MISSING SECTORS present")}   # should be 0

#Create a list of missing sites that can be imported into the SITE MASTER file if needed
test<-JV[is.na(JV$SEC_NAME),]
miss.sites<-ddply(test,.(OBS_YEAR,SITEVISITID,SITE,MISSIONID,REGION,REGION_NAME,ISLAND,LATITUDE,LONGITUDE,
                         REEF_ZONE,DEPTH_BIN,DATE_),
                  summarize,temp=median(SITEVISITID))
head(miss.sites,20) #should be empty



# CLEAN UP

##Remove sites that were only surveyed for photoquads but not demographics
#Note-photoquad only sites are not included in data prior to 2018
#Test whether there are missing values in the NO_SURVEY_YN column. The value should be 0 or -1
JV.na<-JV[is.na(JV$NO_SURVEY_YN)&JV$OBS_YEAR>2013,]
JV.na


is.na(JV$NO_SURVEY_YN)<-0 #Change NAs (blank cells) to 0 - corrected this line of code on 7/25/23- it was changing all NO_SURVEY_YN to 0
JV<-subset(JV,NO_SURVEY_YN==0)


# Assign TAXONCODE
#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
taxa<-read.csv("T:/Benthic/Data/Lookup Tables/2013-24_Taxa_MASTER_2025VERSION.csv")

JV$OBS_YEAR<-as.factor(JV$OBS_YEAR) #need to convert to factor in order to join with taxa df
nrow(JV)
#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
JVaaaa=JV %>% filter(SPCODE=="AAAA")
Convert_to_Taxoncode_2025(data = JVaaaa,taxamaster = taxa)

JV=JV %>% mutate(TAXONCODE=Convert_to_Taxoncode_2025(data = JV,taxamaster = taxa))
nrow(JV)

#Check to make sure SPCODE was converted correctly
View(subset(JV,SPCODE!=TAXONCODE))
JV[which(is.na(JV$TAXONCODE)),]

#If there are issues use this code to create a list SPCODE (lowest taxonomic resolution we have), TAXONCODE (the taxonomic level we all feel comfortable with) and associated genera
#This is used for spot checking that TAXONCODE was converted properly & can be compared against TAXA MASTER
SURVEY_INFO<-c("OBS_YEAR","REGION","SPCODE","TAXONCODE","GENUS_CODE","TAXONNAME")
JV %>% group_by(across(all_of(SURVEY_INFO))) %>% summarize(count=length(SITEVISITID))

#Check to see whether S_ORDER is NA and not AAAA (the code for no colonies observed on the segment)
JV[JV$SPCODE!="AAAA"& is.na(JV$S_ORDER),] #this dataframe should be empty


#Change columns to character
JV$GENUS_CODE<-as.character(JV$GENUS_CODE)
JV$SPCODE<-as.character(JV$SPCODE)
JV$TAXONCODE<-as.character(JV$TAXONCODE)
JV$S_ORDER<-as.character(JV$S_ORDER)

#Make sure there are no NA values in genus code or taxoncode if it's supposed to be a scleractinian
subset(JV,S_ORDER=="Scleractinia" & GENUS_CODE=="NA") #this dataframe should be empty
subset(JV,S_ORDER=="Scleractinia" & TAXONCODE=="NA") #this dataframe should be empty

#Fix missing NAs if need be
# JV$GENUS_CODE<-ifelse(is.na(JV$GENUS_CODE)&JV$S_ORDER=="Scleractinia",JV$SPCODE,JV$GENUS_CODE)
# JV$TAXONCODE<-ifelse(is.na(JV$TAXONCODE)&JV$S_ORDER=="Scleractinia",JV$SPCODE,JV$TAXONCODE)

#There are some old SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genera or taxoncodes to the spcode and the remaining NAs in the Taxon and genus code to AAAA
JV$GENUS_CODE<-ifelse(JV$TAXONCODE=="UNKN","UNKN",JV$GENUS_CODE)
JV$TAXONCODE<-ifelse(JV$SPCODE=="AAAA","AAAA",JV$TAXONCODE)
JV$GENUS_CODE<-ifelse(JV$TAXONCODE=="AAAA","AAAA",JV$GENUS_CODE)
JV$TAXONCODE<-ifelse(JV$SPCODE %in% c("MOAS","LEPA"),"UNKN",JV$TAXONCODE)
JV$GENUS_CODE<-ifelse(JV$SPCODE %in% c("MOAS","LEPA"),"UNKN",JV$GENUS_CODE)

View(JV) #view data in separate window

#Montastrea changed to Astrea in 2018
JV$GENUS_CODE<-ifelse(JV$GENUS_CODE == "MONS","ASTS",JV$GENUS_CODE)
JV$TAXONCODE<-ifelse(JV$SPCODE == "MONS","ASTS",JV$TAXONCODE)
JV$TAXONCODE<-ifelse(JV$SPCODE == "MCUR","ASTS",JV$TAXONCODE)
JV$TAXONCODE<-ifelse(JV$TAXONCODE == "ASTS","Astrea sp",JV$TAXONNAME)


#We only analyze juveniles at the genus level- change taxoncode to genus
JV$TAXONCODE<-JV$GENUS_CODE


#Check that Unknown scl were changed correctly
head(subset(JV,TAXONCODE=="UNKN"&S_ORDER=="Scleractinia"),40)
head(subset(JV,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia"))
head(subset(JV,GENUS_CODE=="AAAA"))
head(subset(JV,SPCODE=="AAAA"))


##Calcuating segment and transect area and add column for transect area
JV$TRANSECTAREA<-Transectarea(JV)
# sapply(JV,levels)
head(JV)
nrow(JV)


## CLEAN UP NAs ##
NegNineCheckCols=c("S_ORDER","TAXONNAME","COLONYLENGTH")
JV[,NegNineCheckCols][JV[,NegNineCheckCols] ==-9] <- NA #Convert missing numeric values to NA (they are entered as -9 in Oracle)


jwd<-droplevels(JV)
write.csv(jwd,file="T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Juveniles_raw_CLEANED_2024.csv",row.names = FALSE)



# ---- Section 3: Compile New Data to Existing Data ---------------------------------------
ad13.23=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED_2013.2023.csv")
jv13.23=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Juveniles_raw_CLEANED_2013.2023.csv")
ad24=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED_2024.csv")
jv24=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Juveniles_raw_CLEANED_2024.csv")

ad24.=ad24[,names(ad13.23)]
jv24.=jv24[,names(jv13.23)]

names(ad13.23)==names(ad24.)
names(jv13.23)==names(jv24.)

ad13.24=rbind(ad13.23,ad24.)
jv13.24=rbind(jv13.23,jv24.)

write.csv(ad13.24,file="T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED_2013.2024.csv",row.names = FALSE)
write.csv(jv13.24,file="T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Juveniles_raw_CLEANED_2013.2024.csv",row.names = FALSE)


