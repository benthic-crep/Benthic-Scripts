#This script cleans and prepared the SfM-generated data from the full set of method comparision sites from RAMP
#The data you read into this script is the exported Arc geodatabase that has already been QC'd. 
#A number columns need to be added or modified to follow the format of the REA deomographic data from Oracle

rm(list=ls())

#Set Run Flags
DEBUG=TRUE

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")
# source("C:/Users/Corinne.Amir/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
# source("C:/Users/Corinne.Amir/Documents/GitHub/fish-paste/lib/core_functions.R")
# source("C:/Users/Corinne.Amir/Documents/GitHub/fish-paste/lib/GIS_functions.R")

setwd("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision")

# SFM/ADULT: CLEAN ANALYSIS READY DATA ----------------------------------------------------
df<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP2019_v2_output_may132020.csv")

#Subset just the Adult data
x<-subset(df,SEGLENGTH=="2.5")
head(x)
View(x)
nrow(x)


#SfM/ADULT: Column Names Changes -------------------------------------------------
colnames(x)[colnames(x)=="RD_1"]<-"RDEXTENT1" #Change column name
colnames(x)[colnames(x)=="RDCAUSE1"]<-"RD1" #Change column name
colnames(x)[colnames(x)=="RD_2"]<-"RDEXTENT2" #Change column name
colnames(x)[colnames(x)=="RD_3"]<-"RDEXTENT3" #Change column name
colnames(x)[colnames(x)=="RDCAUSE2"]<-"RD2" #Change column name
colnames(x)[colnames(x)=="RDCAUSE3"]<-"RD3" #Change column name
colnames(x)[colnames(x)=="FRAGMENT"]<-"Fragment" #Change column name
colnames(x)[colnames(x)=="CON_1"]<-"CONDITION_1" #Change column name
colnames(x)[colnames(x)=="CON_2"]<-"CONDITION_2" #Change column name
colnames(x)[colnames(x)=="CON_3"]<-"CONDITION_3" #Change column name
colnames(x)[colnames(x)=="SEV_1"]<-"SEVERITY_1" #Change column name
colnames(x)[colnames(x)=="SEV_2"]<-"SEVERITY_2" #Change column name
colnames(x)[colnames(x)=="SEV_3"]<-"SEVERITY_3" #Change column name
colnames(x)[colnames(x)=="Shape_Leng"]<-"COLONYLENGTH" #Change column name


#Add column for method type
x$METHOD<-"SfM"

if(DEBUG){head(x)}


#Take a look at who annotated sites and double check that repeat segments aren't included
#Note:NII-02584 was split by Mollie and Rhonda- make sure to include both annotator's data
table(x$SITE,x$ANALYST)
tmp<-ddply(x,.(ANALYST),
           summarize,
           n=length(unique(SITE)))
tmp$prop<-tmp$n/90*100 #% of sites that each annotator annoated


#SfM/ADULT: Adding and Modifying columns --------------------------------------------

#Fill in columns with values that we know should not be different across any of the rows
x$OBS_YEAR <- as.vector(rep(2019, times = nrow(x)))
x$COLONYLENGTH<-x$COLONYLENGTH*100 #convert from m to cm
x$S_ORDER<-ifelse(x$NO_COLONY==0 & x$SPCODE!="NONE","Scleractinia","NONE") #add S_order column
x$TRANSECT<-1
x$COLONYID<-paste(x$SITE,x$SEGMENT,x$SPCODE,x$COLONYLENGTH,sep="_")

#Check for duplicate COLONYIDs
x$COLONYID[duplicated(x$COLONYID)]
x<-x[!duplicated(x$COLONYID), ] #remove duplicate rows (temporary workaround for now until we can figure out what is going on in the export)


#Change bleaching severity = 1 to NA
x[x$CONDITION_1 =="BLE" & x$SEVERITY_1=="1", 24:26] <- NA
subset(x,CONDITION_1=="BLE")
x[x$CONDITION_2 =="BLE" & x$SEVERITY_2=="1", 27:29] <- NA
subset(x,CONDITION_2=="BLE")
x[x$CONDITION_3 =="BLE" & x$SEVERITY_3=="1", 30:32] <- NA
subset(x,CONDITION_3=="BLE")

#Create Genuscode and taxonname column from spcode
genlookup<-read.csv("T:/Benthic/Data/Lookup Tables/Genus_lookup.csv")
x<-CreateGenusCode(x,genlookup) 
head(x)

#Generate General RD cause code
gencodes<-read.csv("T:/Benthic/Data/Lookup Tables/GeneralRDcode_lookup.csv")
head(x)
levels(x$RD1)

x<-CreateGenRDCode(x,"RD1","GENRD1",gencodes)
x<-CreateGenRDCode(x,"RD2","GENRD2",gencodes)
x<-CreateGenRDCode(x,"RD3","GENRD3",gencodes)

head(x)
nrow(df);nrow(x) #make sure rows weren't dropped


#SfM/ADULT: Merge Adult data and  SURVEY MASTER  -------------------------------------
survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
# survey_master <- read.csv("C:/Users/Corinne.Amir/Documents/GitHub/Benthic-Scripts/SfM/SURVEY MASTER.csv")

colnames(survey_master)[colnames(survey_master)=="LATITUDE_SV"]<-"LATITUDE" #Change column name
colnames(survey_master)[colnames(survey_master)=="LONGITUDE_SV"]<-"LONGITUDE" #Change column name


x<-left_join(x,survey_master[,c("MISSIONID","REGION","OBS_YEAR","ISLAND","SITEVISITID","SITE","SEC_NAME",
                            "REEF_ZONE","DEPTH_BIN","HABITAT_CODE","LATITUDE","LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")])

head(x)
nrow(x)


#SfM/ADULT: Assign TAXONCODE --------------------------------------------------------
#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
taxa<-read.csv("T:/Benthic/Data/Lookup Tables/2013-20_Taxa_MASTER.csv")
taxa$OBS_YEAR<-as.numeric(as.character(taxa$OBS_YEAR))

x$SPCODE<-ifelse(x$NO_COLONY==-1,"AAAA",as.character(x$SPCODE)) 


#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
x$TAXONCODE<-Convert_to_Taxoncode(x,taxa)

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


test<-subset(x,SPCODE=="UNKN")
table(test$ANALYST) # look at who has unknowns- go back to the imagery to double check ID
write.csv(test,"unknowns_to_check.csv")

#In order to record no colonies observed in a segment, we need to create a small colony on the image.This code removes that size measure
x$COLONYLENGTH<-ifelse(x$SPCODE=="AAAA",0,as.character(x$COLONYLENGTH))


# sapply(x,levels)
head(x)
nrow(x)

#Reorder columns
x<-x[,c("METHOD","ANALYST", "MISSIONID","REGION","OBS_YEAR","ISLAND","SEC_NAME","SITEVISITID","SITE","TRANSECT","REEF_ZONE","DEPTH_BIN",
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
write.csv(awd,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMAdult_MCLEANED.csv",row.names = F)


# SFM/JUVENILE: CLEAN ANALYSIS READY DATA -------------------------------------
#Subset just the Adult data
x<-subset(df,SEGLENGTH=="1")
head(x)
View(x)
nrow(x)

#SFM/JUVENILE: Column Names Changes... -------------------------------------------------
colnames(x)[colnames(x)=="FRAGMENT"]<-"Fragment" #Change column name
colnames(x)[colnames(x)=="Shape_Leng"]<-"COLONYLENGTH" #Change column name
colnames(x)[colnames(x)=="FID"]<-"COLONYID" #Change column name

x$COLONYID<-c(1:length(x$COLONYID)) #temporary workaround

#Add column for method type
x$METHOD<-"SfM"

if(DEBUG){head(x)}

table(x$SITE,x$ANALYST)


#SFM/JUVENILE: Adding and Modifying columns --------------------------------------------

#Fill in columns with values that we know should not be different across any of the rows
x$OBS_YEAR <- as.vector(rep(2019, times = nrow(x)))
x$COLONYLENGTH<-x$COLONYLENGTH*100 #convert from m to cm
x$S_ORDER<-ifelse(x$NO_COLONY==0 & x$SPCODE!="NONE","Scleractinia","NONE") #add S_order column
x$TRANSECT<-1
x$COLONYID<-paste(x$SITE,x$SEGMENT,x$SPCODE,x$COLONYLENGTH,sep="_")

#Check for duplicate COLONYIDs
x$COLONYID[duplicated(x$COLONYID)]
x<-x[!duplicated(x$COLONYID), ] #remove duplicate rows (temporary workaround for now until we can figure out what is going on in the export)


#Create Genuscode and taxonname column from spcode
genlookup<-read.csv("T:/Benthic/Data/Lookup Tables/Genus_lookup.csv")
x<-CreateGenusCode(x,genlookup) 
head(x)


#SFM/JUVENILE: Merge Juvenile data and SURVEY MASTER (SfM) -------------------------------------
survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
#survey_master <- read.csv("SURVEY MASTER.csv")

colnames(survey_master)[colnames(survey_master)=="LATITUDE_SV"]<-"LATITUDE" #Change column name
colnames(survey_master)[colnames(survey_master)=="LONGITUDE_SV"]<-"LONGITUDE" #Change column name


x<-left_join(x,survey_master[,c("MISSIONID","REGION","OBS_YEAR","ISLAND","SITEVISITID","SITE","SEC_NAME",
                                "REEF_ZONE","DEPTH_BIN","HABITAT_CODE","LATITUDE","LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")],by=c("OBS_YEAR","SITE"))

head(x)
nrow(x)



#SFM/JUVENILE: Assign TAXONCODE --------------------------------------------------------
#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
taxa<-read.csv("T:/Benthic/Data/Lookup Tables/2013-20_Taxa_MASTER.csv")
taxa$OBS_YEAR<-as.numeric(as.character(taxa$OBS_YEAR))
x$SPCODE<-ifelse(x$NO_COLONY==-1,"AAAA",as.character(x$SPCODE)) 


#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
x$TAXONCODE<-Convert_to_Taxoncode(x,taxa)

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
x<-x[,c("METHOD","ANALYST", "MISSIONID","REGION","OBS_YEAR","ISLAND","SEC_NAME","SITEVISITID","SITE","TRANSECT","REEF_ZONE","DEPTH_BIN",
        "HABITAT_CODE","LATITUDE","LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","SEGMENT","SEGLENGTH","SEGWIDTH",
        "SEGAREA","COLONYID","Fragment","S_ORDER","GENUS_CODE","SPCODE","TAXONCODE","TAXONNAME",
        "EX_BOUND","COLONYLENGTH")]         


## CLEAN UP NAs ##
NegNineCheckCols=c("S_ORDER","TAXONNAME","MIN_DEPTH_M","MAX_DEPTH_M","COLONYLENGTH")
x[,NegNineCheckCols][x[,NegNineCheckCols] ==-9] <- NA #Convert missing numeric values to NA (they are entered as -9 in Oracle)


jwd<-droplevels(x)
write.csv(jwd,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMJuv_MCLEANED.csv",row.names = F)


