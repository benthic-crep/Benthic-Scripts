
#CREATE ADULT CLEAN ANALYSIS READY DATA----------------------------------------
# This script will clean the raw benthic REA data using method E (2013-present) and prepare it for analysis

rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")

## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
load("ALL_REA_ADULTCORAL_RAW.rdata") #from oracle
x<-df #leave this as df

x$SITE<-SiteNumLeadingZeros(x$SITE) # Change site number such as MAR-22 to MAR-0022


### Use these functions to look at data
head(x)
tail(x)


#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","REGION_NAME","ISLAND","ISLANDCODE","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","SITE_MIN_DEPTH","SITE_MAX_DEPTH","SITEVISITID","HABITAT_CODE","DIVER","TRANSECT","SEGMENT","TRANWIDTH",
             "TRANLENGTH","EXCLUDE_FLAG","NO_SURVEY_YN","COLONYID","SPECIES","MORPH_CODE","COLONYLENGTH","OLDDEAD",
             "RECENTDEAD","RECENT_GENERAL_CAUSE_CODE","RECENT_SPECIFIC_CAUSE_CODE",
             "RECENTDEAD_2",	"RECENT_GENERAL_CAUSE_CODE_2","RECENT_SPECIFIC_CAUSE_CODE_2","DZCODE",
             "EXTENT",	"SEVERITY","GENUS_CODE","S_ORDER","TAXONNAME")

#remove extraneous columns
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

#Double check level and class of variables to make sure there aren't any errors
sapply(x,levels)
sapply(x,class)##Change column names to make code easier to code

colnames(x)[colnames(x)=="TRANWIDTH"]<-"SEGWIDTH" #Change column name
colnames(x)[colnames(x)=="TRANLENGTH"]<-"SEGLENGTH" #Change column name
colnames(x)[colnames(x)=="SPECIES"]<-"SPCODE" #Change column name
colnames(x)[colnames(x)=="RECENTDEAD"]<-"RDEXTENT1" #Change column name
colnames(x)[colnames(x)=="RECENT_GENERAL_CAUSE_CODE"]<-"GENRD1" #Change column name
colnames(x)[colnames(x)=="RECENT_SPECIFIC_CAUSE_CODE"]<-"RD1" #Change column name
colnames(x)[colnames(x)=="RECENTDEAD_2"]<-"RDEXTENT2" #Change column name
colnames(x)[colnames(x)=="RECENT_GENERAL_CAUSE_CODE_2"]<-"GENRD2" #Change column name
colnames(x)[colnames(x)=="RECENT_SPECIFIC_CAUSE_CODE_2"]<-"RD2" #Change column name
colnames(x)[colnames(x)=="SITE_MIN_DEPTH"]<-"SITE_MIN_DEPTH_FT" #Change column name
colnames(x)[colnames(x)=="SITE_MAX_DEPTH"]<-"SITE_MAX_DEPTH_FT" #Change column name
colnames(x)[colnames(x)=="DZCODE"]<-"COND" #Change column name

head(x)

# #Create a list of sites only surveyed for photoquads
# SURVEY_INFO<-c("NO_SURVEY_YN","SITEVISITID","OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT")
# pq_only<-Aggregate_InputTable(x, SURVEY_INFO)
# pq<-subset(pq_only,NO_SURVEY_YN==-1)
# 


#There are some SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genus to the spcode and the remaining NAs in the Taxon and genus code to AAAA
x$GENUS_CODE<-as.character(x$GENUS_CODE)
x$SPCODE<-as.character(x$SPCODE)

x$GENUS_CODE<-ifelse(is.na(x$GENUS_CODE)&x$S_ORDER=="Scleractinia",x$SPCODE,x$GENUS_CODE)

x$GENUS_CODE[is.na(x$GENUS_CODE)]<-"AAAA"#change nas to AAAA
#utils::View(x.) #view data in separate window

#Check that Unknown scl were changed correctly
test<-subset(x,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia");head(test)
test<-subset(x,GENUS_CODE=="AAAA");head(test)



#Test whether there are missing values in the NO_SURVEY_YN column. The value should be 0 or -1
x.na<-x[is.na(x$NO_SURVEY_YN),]
test<-ddply(x.na,.(SITE),
            summarize,
            SEG=length(unique(SEGMENT)))
test

#Convert NAs to 0 and remove trasects with no surveys
x$EXCLUDE_FLAG<-is.na(x$EXCLUDE_FLAG)<-0 #Change NAs (blank cells) to 0
x$NO_SURVEY_YN<-is.na(x$NO_SURVEY_YN)<-0 #Change NAs (blank cells) to 0
x<-subset(x,NO_SURVEY_YN>-1) #Exclude rows -1
x<-subset(x,SEGLENGTH!="NA") #Remove segments that were not surveyed for coral demography



##Calcuating segment and transect area and add column for transect area
x<-Transectarea(x)
sapply(x,levels)
head(x)
nrow(x)


## CLEAN UP NAs ##

x[x=="."]<-NA #fix this
x[x=="-9"]<-NA
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

tmp.lev<-levels(x$SITE_MIN_DEPTH_FT); head(tmp.lev)
levels(x$SITE_MIN_DEPTH_FT)<-c(tmp.lev, "NONE")
x[is.na(x$SITE_MIN_DEPTH_FT),"SITE_MIN_DEPTH_FT"]<-"NONE"

tmp.lev<-levels(x$SITE_MAX_DEPTH_FT); head(tmp.lev)
levels(x$SITE_MAX_DEPTH_FT)<-c(tmp.lev, "NONE")
x[is.na(x$SITE_MAX_DEPTH_FT),"SITE_MAX_DEPTH_FT"]<-"NONE"

tmp.lev<-levels(x$Estab_Yr); head(tmp.lev)
levels(x$Estab_Yr)<-c(tmp.lev, "NONE")
x[is.na(x$Estab_Yr),"Estab_Yr"]<-"NONE"

head(x)

awd<-droplevels(x)

#save(awd, file="TMPBenthicREA_Adultwd_V2.Rdata")  #Save clean working data



## CREATE JUVENILE CLEAN ANALYSIS READY DATA ----

load("ALL_REA_JUVCORAL_RAW.rdata")
x<-df
x$SITE<-SiteNumLeadingZeros(x$SITE)


#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","REGION_NAME","ISLAND","ISLANDCODE","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","SITE_MIN_DEPTH","SITE_MAX_DEPTH","SITEVISITID","HABITAT_CODE","DIVER","EXCLUDE_FLAG","TRANSECT","SEGMENT","TRANWIDTH",
             "TRANLENGTH","COLONYID","SPECIES","MORPH_CODE","COLONYLENGTH","COLONYWIDTH","GENUS_CODE","S_ORDER")

head(x[,DATA_COLS])
x<-x[,DATA_COLS]


#Double check level and class of variables to make sure there aren't any errors
sapply(x,levels)
sapply(x,class)##Change column names to make code easier to code
colnames(x)[colnames(x)=="SPECIES"]<-"SPCODE" #Change column name
colnames(x)[colnames(x)=="TRANWIDTH"]<-"SEGWIDTH" #Change column name
colnames(x)[colnames(x)=="TRANLENGTH"]<-"SEGLENGTH" #Change column name
colnames(x)[colnames(x)=="SITE_MIN_DEPTH"]<-"SITE_MIN_DEPTH_FT" #Change column name
colnames(x)[colnames(x)=="SITE_MAX_DEPTH"]<-"SITE_MAX_DEPTH_FT" #Change column name


#There are some SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genus to the spcode and the remaining NAs in the Taxon and genus code to AAAA
x$GENUS_CODE<-as.character(x$GENUS_CODE)
x$SPCODE<-as.character(x$SPCODE)

x$GENUS_CODE<-ifelse(is.na(x$GENUS_CODE)&x$S_ORDER=="Scleractinia",x$SPCODE,x$GENUS_CODE)

x$GENUS_CODE[is.na(x$GENUS_CODE)]<-"AAAA"#change nas to AAAA
#utils::View(x.) #view data in separate window

#Check that Unknown scl were changed correctly
test<-subset(x,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia");head(test)
test<-subset(x,GENUS_CODE=="AAAA");head(test)



#Remove specfic colonies and segments
x$EXCLUDE_FLAG<-is.na(x$EXCLUDE_FLAG)<-0 #Change NAs (blank cells) to 0
x<-subset(x,EXCLUDE_FLAG>-1);summary(x$EXCLUDE_FLAG) #Exclude rows -1
x<-subset(x,SEGLENGTH!="NA") #Remove segments that were not surveyed for juveniles
nrow(x)


##Calcuating segment and transect area and add column for transect area
x<-Transectarea(x)
sapply(x,levels)
head(x)
nrow(x)


## CLEAN UP NAs 

tmp.lev<-levels(x$SITE_MIN_DEPTH_FT); head(tmp.lev)
levels(x$SITE_MIN_DEPTH_FT)<-c(tmp.lev, "NONE")
x[is.na(x$SITE_MIN_DEPTH_FT),"SITE_MIN_DEPTH_FT"]<-"NONE"

tmp.lev<-levels(x$SITE_MAX_DEPTH_FT); head(tmp.lev)
levels(x$SITE_MAX_DEPTH_FT)<-c(tmp.lev, "NONE")
x[is.na(x$SITE_MAX_DEPTH_FT),"SITE_MAX_DEPTH_FT"]<-"NONE"

head(x)

jwd<-droplevels(x)



#Final Tweaks before calculating Site-level data-------------------------------------------------
#Colony fragments and scleractinans are subseted in the functions 

#Double check that there are no NAs in GENUS_CODE- change
new_DF <- awd[is.na(awd$GENUS_CODE),] 

#Add a column for adult fragments we we can remove them from the dataset later (-1 indicates fragment)
awd$Fragment<-ifelse(awd$COLONYLENGTH<5&awd$S_ORDER=="Scleractinia",-1,0)
jwd$Fragment<-0 # you need to add this column so that you can use the site level functions correctly

#Remove transects with less than 5m surveyed and check how many rows were removed
nrow(awd)
awd<-subset(awd,TRANSECTAREA>=5) 
jwd<-subset(jwd,TRANSECTAREA>=1)
nrow(awd)
head(awd)

##Change Transects 3 and 4 in the juvenile data to 1 and 2 so we can merge with adult data
#BE CAREFUL- because we survey different areas for adults and juveniles you can not merge together ad and juvs until AFTER you calculate density
jwd$TRANSECT[jwd$TRANSECT == "3"] <- "1"
jwd$TRANSECT[jwd$TRANSECT == "4"] <- "2"


#test functions on HAWAII ISLAND data to test code
awd2<-subset(awd,REGION=="SAMOA")
jwd2<-subset(jwd,REGION=="SAMOA")

#Create a look a table of all of the colony attributes- you will need this for the Calc_RDden and Calc_Condden functions
SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SITE", "DATE_", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT","TRANSECT","COLONYID","GENUS_CODE","SPCODE","MORPH_CODE","COLONYLENGTH")
survey_colony<-Aggregate_InputTable(awd2, SURVEY_INFO)

SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE", "SITE", "DATE_", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT","TRANSECT")
survey_transect<-Aggregate_InputTable(awd2, SURVEY_INFO)

SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE", "SITE", "DATE_", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT")
survey_site<-Aggregate_InputTable(awd2, SURVEY_INFO)


# GENERATE SUMMARY METRICS at the transect-level for genus and spcode --------
acd.gen<-Calc_ColDen_Transect(awd2,"GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen"# calculate density at genus level as well as total
rdabun.gen<-Calc_RDabun_Transect(awd2,"GENUS_CODE") # abundance of recent dead colonies by condition, you will need to subset which ever condition you want
condabun.gen<-Calc_Condabun_Transect(awd2,"GENUS_CODE")# abundance of condition colonies by condition, you will need to subset which ever condition you want
jcd.gen<-Calc_ColDen_Transect(jwd2,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen"


#at SPCODE level
acd.tax<-Calc_ColDen_Transect(awd2,"SPCODE");colnames(acd.tax)[colnames(acd.tax)=="ColCount"]<-"AdColCount";colnames(acd.tax)[colnames(acd.tax)=="ColDen"]<-"AdColDen"# calculate density at genus level as well as total
rdabun.tax<-Calc_RDabun_Transect(awd2,"SPCODE") # abundance of recent dead colonies by condition, you will need to subset which ever condition you want
condabun.tax<-Calc_Condabun_Transect(awd2,"SPCODE")# abundance of condition colonies by condition, you will need to subset which ever condition you want
jcd.tax<-Calc_ColDen_Transect(jwd2,"SPCODE"); colnames(jcd.tax)[colnames(jcd.tax)=="ColCount"]<-"JuvColCount";colnames(jcd.tax)[colnames(jcd.tax)=="ColDen"]<-"JuvColDen"

#Calculate Chronic disease abudance
condabun.gen$ChronicDZ<-condabun.gen$FUG+condabun.gen$SGA+condabun.gen$PDS+condabun.gen$PTR
condabun.tax$ChronicDZ<-condabun.tax$FUG+condabun.tax$SGA+condabun.tax$PDS+condabun.tax$PTR

##Calculate Prevalence of whatever conditions you want
rdabun.gen$AcuteDZprev<-rdabun.gen$AcuteDZ/rdabun.gen$ColCount*100 #calculate prevalence
rdabun.gen$COTSprev<-rdabun.gen$COTS/rdabun.gen$ColCount*100 #calculate prevalence
condabun.gen$ChronicDZprev<-condabun.gen$ChronicDZ/condabun.gen$ColCount*100 #calculate prevalence
condabun.gen$BLEprev<-condabun.gen$BLE/condabun.gen$ColCount*100 #calculate prevalence

##Calculate Prevalence of whatever conditions you want
rdabun.tax$AcuteDZprev<-rdabun.tax$AcuteDZ/rdabun.tax$ColCount*100 #calculate prevalence
rdabun.tax$COTSprev<-rdabun.tax$COTS/rdabun.tax$ColCount*100 #calculate prevalence
condabun.tax$ChronicDZprev<-condabun.tax$ChronicDZ/condabun.tax$ColCount*100 #calculate prevalence
condabun.tax$BLEprev<-condabun.tax$BLE/condabun.tax$ColCount*100 #calculate prevalence


#Merging together genus-level metrics
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","GENUS_CODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.gen<-Reduce(MyMerge, list(acd.gen,rdabun.gen,condabun.gen,jcd.gen))
data.gen$TotDZprev<-(data.gen$AcuteDZ+data.gen$ChronicDZ)/data.gen$ColCount.x #Calculate total disease prevalence

#Subset columns needed for analysis
data.gen<-subset(data.gen,select=c("SITE","SITEVISITID","TRANSECT","GENUS_CODE","AdColDen",
                                   "JuvColDen","BLEprev","TotDZprev","AcuteDZprev","ChronicDZprev","COTSprev"))

#Merging together genus-level metrics
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","SPCODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.tax<-Reduce(MyMerge, list(acd.tax,rdabun.tax,condabun.tax,jcd.tax))
data.tax$TotDZprev<-(data.tax$AcuteDZ+data.tax$ChronicDZ)/data.tax$ColCount.x #Calculate total disease prevalence

#Subset columns needed for analysis
data.tax<-subset(data.tax,select=c("SITE","SITEVISITID","TRANSECT","SPCODE","AdColDen",
                                   "JuvColDen","BLEprev","TotDZprev","AcuteDZprev","ChronicDZprev","COTSprev"))



##Unweighted Summaries for Benthic Summary Reports
bsrSITEg<-Calc_Sitemetrics_BSR(data.gen,"GENUS_CODE")
bsrISg<-Calc_Islmetrics_BSR(bsrSITEg,"GENUS_CODE")
bsrDEPTHg<-Calc_IslDepthmetrics_BSR(bsrSITEg,"GENUS_CODE")
bsrREEFZONEg<-Calc_IslReefZonemetrics_BSR(bsrSITEg,"GENUS_CODE")
bsrSITEt<-Calc_Sitemetrics_BSR(data.tax,"SPCODE")
bsrISt<-Calc_Islmetrics_BSR(bsrSITEt,"SPCODE")
bsrDEPTHt<-Calc_IslDepthmetrics_BSR(bsrSITEt,"SPCODE")
bsrREEFZONEt<-Calc_IslReefZonemetrics_BSR(bsrSITEt,"SPCODE")

##Create table for island level summaries for total sclerarctinans
TotalScl_SITE<-subset(bsrSITEg,GENUS_CODE=="SSSS")
TotalScl_IS<-subset(bsrISg,GENUS_CODE=="SSSS")
TotalScl_ISDEPTH<-subset(bsrDEPTHg,GENUS_CODE=="SSSS")

##Create table for island level summaries for target genera, change this to target genera
TargGenera_IS<-subset(bsrIS,GENUS_CODE %in% c("MOSP","POSP","LEPT")) 
TargTaxon_IS<-subset(bsrIS,SPCODE %in% c("MOSP","POSP","LEPT")) 


# get strata and sectors data and subset it for the regions you need
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
sectors<-read.csv("Benthic_SectorArea_v5.csv", stringsAsFactors=FALSE)
samoa<-subset(sectors,REGION=="SAMOA")
pria<-subset(sectors,REGION=="PRIAs")


###write out tables to csv files
write.csv(samoa,"Samoa_Stratareas.csv")
write.csv(pria,"PRIA_Stratareas.csv")
write.csv(TotalScl_SITE,"Sitemetrics_totalscl.csv")
write.csv(TotalScl_IS,"Islandmetrics_totalscl.csv")
write.csv(TotalScl_ISDEPTH,"IslandDepth_metrics_totalscl.csv")
write.csv(TargGenera_IS,"Islandmetrics_targetgenera.csv")
write.csv(TargTaxon_IS,"Islandmetrics_targettaxon.csv")
