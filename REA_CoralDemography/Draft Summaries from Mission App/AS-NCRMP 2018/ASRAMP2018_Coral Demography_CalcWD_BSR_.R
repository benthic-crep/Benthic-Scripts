
#CREATE ADULT CLEAN ANALYSIS READY DATA----------------------------------------
# This script will clean the raw benthic REA data using method E that comes directly from the new data base application.
# The data has gone with the benthic QA/QC, but hasn't gone through data managment's QC steps.
rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_BSR.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")

## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/ASRAMP 2018")
ad<-read.csv("ASRAMP2018_adults.csv") #read in adult data

#read in a temporary file of all of the segments surveyed. The adult demographic data spit out by the application doesn't have the segments that no colonies were found.
seg<-read.csv("ASRAMP2018_segreport.csv");seg<-subset(seg,select = -c(6,9,11))

#split the segment data table into adults and juvs
segad<-subset(seg,Method=="ADULT")
segjuv<-subset(seg,Method=="JUVENILE")

#Add a colony id (temporary)
ad$COLONYID<-seq(1:length(ad$COLONYID))

#merge in the no colony segments
x<-merge(ad,segad,by=c("MISSIONID","OBS_YEAR","SITE","SITEVISITID","LATITUDE",	"LONGITUDE","DIVER","ISLAND","TRANSECT","SEGMENT","SEGLENGTH","SEGWIDTH","REEF_ZONE","DEPTH_BIN","SITE_MIN_DEPTH","SITE_MAX_DEPTH"),all=TRUE)

x$SITE<-SiteNumLeadingZeros(x$SITE) # Change site number such as MAR-22 to MAR-0022


### Use these functions to look at data
head(x)
tail(x)
colnames(x)

#Create vector of column names to include then exclude unwanted columns from dataframe
#NOTE- I REMOVED RECENTDEAD_3 FOR NOW BECAUSE IT'S NOT IN THE APP REPORT
DATA_COLS<-c("MISSIONID","REGION_NAME","ISLAND","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","SITEVISITID","HABITAT_CODE","DIVER","TRANSECT","SEGMENT","No.colony.observed","SEGWIDTH","SEGLENGTH",
             "COLONYID","SPECIES","MORPH_CODE","COLONYLENGTH","OLDDEAD",
             "RECENTDEAD","RECENT_GENERAL_CAUSE_CODE","RECENT_SPECIFIC_CAUSE_CODE",
             "RECENTDEAD_2",	"RECENT_GENERAL_CAUSE_CODE_2","RECENT_SPECIFIC_CAUSE_CODE_2",	
             "RECENT_GENERAL_CAUSE_CODE_3","RECENT_SPECIFIC_CAUSE_CODE_3","CONDITION_1",
             "CONDITION_2","CONDITION_3","GENUS_CODE","S_ORDER","TAXONNAME","SITE_MIN_DEPTH","SITE_MAX_DEPTH")

#remove extraneous columns
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

#Double check level and class of variables to make sure there aren't any errors
sapply(x,levels)
sapply(x,class)##Change column names to make code easier to code

colnames(x)[colnames(x)=="SPECIES"]<-"SPCODE" #Change column name
colnames(x)[colnames(x)=="RECENTDEAD"]<-"RDEXTENT1" #Change column name
colnames(x)[colnames(x)=="RECENT_GENERAL_CAUSE_CODE"]<-"GENRD1" #Change column name
colnames(x)[colnames(x)=="RECENT_SPECIFIC_CAUSE_CODE"]<-"RD1" #Change column name
colnames(x)[colnames(x)=="RECENTDEAD_2"]<-"RDEXTENT2" #Change column name
colnames(x)[colnames(x)=="RECENT_GENERAL_CAUSE_CODE_2"]<-"GENRD2" #Change column name
colnames(x)[colnames(x)=="RECENT_SPECIFIC_CAUSE_CODE_2"]<-"RD2" #Change column name
colnames(x)[colnames(x)=="RECENT_GENERAL_CAUSE_CODE_3"]<-"GENRD3" #Change column name
colnames(x)[colnames(x)=="RECENT_SPECIFIC_CAUSE_CODE_3"]<-"RD3" #Change column name

#add temporary missionid and region names
x$REGION_NAME<-ifelse(x$ISLAND %in% c("Baker","Howland","Jarvis","Kingman","Palmyra"),"Pacific Remote Island Areas","American Samoa")


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

#x$GENUS_CODE<-ifelse(is.na(x$GENUS_CODE)&x$S_ORDER=="Scleractinia",x$SPCODE,x$GENUS_CODE)

x$GENUS_CODE[is.na(x$GENUS_CODE)]<-"AAAA"#change nas to AAAA
x$SPCODE[is.na(x$SPCODE)]<-"AAAA"#change nas to AAAA

utils::View(x) #view data in separate window



##Calcuating segment and transect area and add column for transect area
x<-Transectarea(x)
sapply(x,levels)
head(x)
nrow(x)

#There is an error with one of the sites (OFU-1000) this is a temporary fix
x$TRANSECT<-1

## CLEAN UP NAs ##

x[x=="-"]<-NA #fix this
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

#Identify ESA coral sightings
esa<-read.csv("esa_corals.csv");colnames(esa)[2]<-"SPCODE"
esa_sightings<-merge(awd,esa,by="SPCODE");head(esa_sightings)
write.csv(esa_sightings,"ASRAMP2018_ESAsightings.csv")



## CREATE JUVENILE CLEAN ANALYSIS READY DATA ----
juv<-read.csv("ASRAMP2018_juveniles.csv");colnames(juv)[9]<-"TRANSECT"
segjuv<-subset(seg,Method=="JUVENILE")

#Create colony id that doesn't overlap with adults
juv$temp<-seq(1:length(juv$COLONYID))
juv$COLONYID<-juv$temp+21291

read.csv()

#merge juv and segments together
x<-merge(juv,segjuv,by=c("OBS_YEAR","MISSIONID","SITE","SITEVISITID","LATITUDE","DIVER",	"LONGITUDE","ISLAND","TRANSECT","SEGMENT","SEGLENGTH","SEGWIDTH","REEF_ZONE","DEPTH_BIN"),all=TRUE)

x$SITE<-SiteNumLeadingZeros(x$SITE)


#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION_NAME","ISLAND","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","SITEVISITID","HABITAT_CODE","DIVER","TRANSECT","SEGMENT","SEGWIDTH",
             "SEGLENGTH","COLONYID","SPECIES","COLONYLENGTH","GENUS","S_ORDER","SITE_MIN_DEPTH","SITE_MAX_DEPTH")

head(x[,DATA_COLS])
x<-x[,DATA_COLS]


x$REGION_NAME<-ifelse(x$ISLAND %in% c("Baker","Howland","Jarvis","Kingman","Palmyra"),"Pacific Remote Island Areas","American Samoa")
genlist<-read.csv("T:/Benthic/Data/SpGen_Reference/AllGenList.csv");genlist<-subset(genlist,select=-c(Genus))

x<-merge(x,genlist,by="GENUS",all.x=TRUE);x

#Double check level and class of variables to make sure there aren't any errors
#sapply(x,levels)
#sapply(x,class)##Change column names to make code easier to code
colnames(x)[colnames(x)=="SPECIES"]<-"SPCODE" #Change column name

#There are some SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genus to the spcode and the remaining NAs in the Taxon and genus code to AAAA
x$GENUS_CODE[x$GENUS_CODE=="-"]<-NA 
x$GENUS_CODE<-as.character(x$GENUS_CODE)
x$SPCODE<-as.character(x$SPCODE)

x$GENUS_CODE<-ifelse(x$SPCODE=="UNKN","UNKN",x$GENUS_CODE)

#Remove Tubastrea from dataframe
x$S_ORDER<-ifelse(x$SPCODE=="TUSP",NA,as.character(x$S_ORDER))
x$GENUS_CODE<-ifelse(x$SPCODE=="TUSP",NA,as.character(x$GENUS_CODE))
x$SPCODE<-ifelse(x$SPCODE=="TUSP",NA,as.character(x$SPCODE))
x$COLONYLENGTH<-ifelse(x$GENUS=="Tubastraea","-",x$COLONYLENGTH)
x$GENUS<-ifelse(x$GENUS=="Tubastraea",NA,as.character(x$GENUS))
head(subset(x,SITE=="JAR-01916"))
head(subset(x,ISLAND=="Kingman"))

#Change NA to AAAA to ensure you keep the 0s in
x$GENUS_CODE[is.na(x$GENUS_CODE)]<-"AAAA"#change nas to AAAA
x$SPCODE[is.na(x$SPCODE)]<-"AAAA"#change nas to AAAA

utils::View(x) #view data in separate window



##Calcuating segment and transect area and add column for transect area
x<-Transectarea(x)
sapply(x,levels)
head(x)
nrow(x)

#There is an error with one of the sites (OFU-1000) this is a temporary fix
x$TRANSECT<-1

## CLEAN UP NAs ##

x[x=="-"]<-NA #fix this
x[x=="-9"]<-NA
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

# ##Remove Tubastrea from data
# awd<-subset(awd,GENUS_CODE!="TUSP")
# jwd<-subset(jwd,GENUS_CODE!="TUSP")


#Create a look a table of all of the colony attributes- you will need this for the Calc_RDden and Calc_Condden functions
SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR","REGION_NAME", "ISLAND","SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH","TRANSECT","COLONYID","GENUS_CODE","SPCODE","MORPH_CODE","COLONYLENGTH")
survey_colony<-Aggregate_InputTable(awd, SURVEY_INFO)

SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR","REGION_NAME", "ISLAND","SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH","TRANSECT")
survey_transect<-Aggregate_InputTable(awd, SURVEY_INFO)

SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR","REGION_NAME", "ISLAND","SITE", "REEF_ZONE","DATE_",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH")
survey_site<-Aggregate_InputTable(awd, SURVEY_INFO)

as<-subset(survey_site,REGION_NAME=="American Samoa")
pria<-subset(survey_site,REGION_NAME=="Pacific Remote Island Areas")
write.csv(as,file="T:/Benthic/Data/REA Coral Demography/WorkingData_forBSRs/SAMOA_2018_surveySite.csv")
write.csv(pria,file="T:/Benthic/Data/REA Coral Demography/WorkingData_forBSRs/PRIAs_2018_surveySite.csv")


# QC Checks- these were done manually by exporting from database a --------
#1. Identify colonies <5cm in the adult database but not fragments (Fragments indicated as 0% recent dead and DAMB)
#2. Check range of juveniles (should be 0-5cm)
#2. Identify colonies with 0% recent dead, but PRED or DZGEN as general cause code- need to double check these on datasheet
#3. Identify colonies with NO % recent dead, but a recent dead cause
#4. look for taxa errors (e.g. MOAS should be MOSP)- we need to remove codes that are no longer used from database
#5. check Plat
#6. Make sure that all sites have all segments. If segments are missing double check datasheets and keep a record of segments that weren't surveyed
#7. Make sure that "No.colony.observed" is either 0 or -1, there were cases where there was -, which should have been 0. This is a glitch in the app. Michael fixed it?
#8. Make sure that the list of codes in recent dead and condition columns is appropriate. For example, there shouldn't be a TLS in the condition columns


# GENERATE SUMMARY METRICS at the transect-level for genus and spcode --------
acd.gen<-Calc_ColDen_Transect(awd,"GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen"# calculate density at genus level as well as total
rdabun.gen<-Calc_RDabun_Transect(awd,"GENUS_CODE")
awd$alldead<-awd$OLDDEAD+as.numeric(awd$RDEXTENT1)+as.numeric(awd$RDEXTENT2)
dead.gen<-Calc_ColMetric_Transect(awd,"GENUS_CODE","alldead");colnames(dead.gen)[colnames(dead.gen)=="Ave.y"]<-"AllDead"


#Change the abundance of recent dead conditions by taxa to 0 if taxon was present, but no condition found (NA), if no colonies were counted for a given taxon leave as NA
nrow(rdabun.gen)
names(rdabun.gen)
correct_col=which(names(rdabun.gen)=="CORA"):which(names(rdabun.gen)=="ZOAN")
for(i in 1:nrow(rdabun.gen)){
  this_ColCount=rdabun.gen[i,"ColCount"]
  if(this_ColCount>0){
    this=rdabun.gen[i,correct_col]
    rdabun.gen[i,correct_col[which(is.na(this))]]=0
  }
  print(paste0("Row ",i," of ",nrow(rdabun.gen)))
}
### Test that it worked
which(is.na(subset(rdabun.gen,ColCount>0)),arr.ind = T)
###


# abundance of recent dead colonies by condition, you will need to subset which ever condition you want
condabun.gen<-Calc_CONDabunNEW_Transect(awd,"GENUS_CODE")# abundance of condition colonies by condition, you will need to subset which ever condition you want

nrow(condabun.gen)
names(condabun.gen)
correct_col=which(names(condabun.gen)=="ALG"):which(names(condabun.gen)=="TIN")
for(i in 1:nrow(condabun.gen)){
  this_ColCount=condabun.gen[i,"ColCount"]
  if(this_ColCount>0){
    this=condabun.gen[i,correct_col]
    condabun.gen[i,correct_col[which(is.na(this))]]=0
  }
  print(paste0("Row ",i," of ",nrow(condabun.gen)))
}

### Test that it worked
which(is.na(subset(condabun.gen,ColCount>0)),arr.ind = T)
###

#caculate juvenile density
jcd.gen<-Calc_ColDen_Transect(jwd,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen"


# #at SPCODE level
acd.tax<-Calc_ColDen_Transect(awd,"SPCODE");colnames(acd.tax)[colnames(acd.tax)=="ColCount"]<-"AdColCount";colnames(acd.tax)[colnames(acd.tax)=="ColDen"]<-"AdColDen"# calculate density at genus level as well as total
rdabun.tax<-Calc_RDabun_Transect(awd,"SPCODE") # abundance of recent dead colonies by condition, you will need to subset which ever condition you want
condabun.tax<-Calc_Condabun_Transect(awd,"SPCODE")# abundance of condition colonies by condition, you will need to subset which ever condition you want
jcd.tax<-Calc_ColDen_Transect(jwd,"SPCODE"); colnames(jcd.tax)[colnames(jcd.tax)=="ColCount"]<-"JuvColCount";colnames(jcd.tax)[colnames(jcd.tax)=="ColDen"]<-"JuvColDen"

#Calculate Chronic disease abudance
condabun.gen$ChronicDZ<-condabun.gen$FUG+condabun.gen$SGA
# condabun.tax$ChronicDZ<-condabun.tax$FUG+condabun.tax$SGA+condabun.tax$PDS+condabun.tax$PTR

##Calculate Prevalence of whatever conditions you want
health<-merge(rdabun.gen, condabun.gen, by= c("SITE","SITEVISITID","TRANSECT","GENUS_CODE","ColCount"), all.x= TRUE, all.y= TRUE)
health$TotDZprev<-(health$ChronicDZ+health$AcuteDZ)/health$ColCount*100 #calculate prevalence
health$AcuteDZprev<-health$AcuteDZ/health$ColCount*100 #calculate prevalence
health$COTSprev<-health$COTS/health$ColCount*100 #calculate prevalence
health$ChronicDZprev<-health$ChronicDZ/health$ColCount*100 #calculate prevalence
health$BLEprev<-health$BLE/health$ColCount*100 #calculate prevalence

#subset columns of interest
health<-health[,c("SITE","SITEVISITID","TRANSECT","GENUS_CODE","AcuteDZprev","COTSprev","BLEprev","ChronicDZprev","TotDZprev")]


# ##Calculate Prevalence of whatever conditions you want
# rdabun.tax$AcuteDZprev<-rdabun.tax$AcuteDZ/rdabun.tax$ColCount*100 #calculate prevalence
# rdabun.tax$COTSprev<-rdabun.tax$COTS/rdabun.tax$ColCount*100 #calculate prevalence
# condabun.tax$ChronicDZprev<-condabun.tax$ChronicDZ/condabun.tax$ColCount*100 #calculate prevalence
# condabun.tax$BLEprev<-condabun.tax$BLE/condabun.tax$ColCount*100 #calculate prevalence


#Merging together genus-level metrics
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","GENUS_CODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.gen<-Reduce(MyMerge, list(acd.gen,health,jcd.gen,dead.gen))
data.gen$JuvColCount[is.na(data.gen$JuvColCount)]<-0
data.gen$JuvColDen[is.na(data.gen$JuvColDen)]<-0

data.tax<-merge(acd.tax,jcd.tax, by=c("SITE","SITEVISITID","TRANSECT","SPCODE"),all=TRUE)
data.tax$JuvColCount[is.na(data.tax$JuvColCount)]<-0
data.tax$JuvColDen[is.na(data.tax$JuvColDen)]<-0



SURVEY_INFO<-c("SITEVISITID","OBS_YEAR", "REGION_NAME", "ISLAND", "SITE")
survey_site<-Aggregate_InputTable(awd, SURVEY_INFO)

#Merge site level data and meta data
data.tax.<-merge(data.tax,survey_site,by=c("SITEVISITID","SITE"),all.x=TRUE)

write.csv(data.tax.,"C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA/Benthic REABenthic18sitedata.csv")
# #Merging together taxa-level metrics
# MyMerge <- function(x, y){
#   df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","SPCODE"), all.x= TRUE, all.y= TRUE)
#   return(df)
# }
# data.tax<-Reduce(MyMerge, list(acd.tax,rdabun.tax,condabun.tax,jcd.tax))
# data.tax$TotDZprev<-(data.tax$AcuteDZ+data.tax$ChronicDZ)/data.tax$ColCount.x #Calculate total disease prevalence
# 
# #Subset columns needed for analysis
# data.tax<-subset(data.tax,select=c("SITE","SITEVISITID","TRANSECT","SPCODE","AdColDen",
#                                    "JuvColDen","BLEprev","TotDZprev","AcuteDZprev","ChronicDZprev","COTSprev"))
# 



##Unweighted Summaries for Benthic Summary Reports
bsrSITEg<-Calc_Sitemetrics_BSR(data.gen,"GENUS_CODE")
bsrSITEgf<-subset(bsrSITEg,REEF_ZONE=="Forereef")
bsrISg<-Calc_Islmetrics_BSR(bsrSITEg,"GENUS_CODE")
bsrISgf<-Calc_Islmetrics_BSR(bsrSITEgf,"GENUS_CODE")

bsrDEPTHg<-Calc_IslDepthmetrics_BSR(bsrSITEg,"GENUS_CODE")
bsrREEFZONEg<-Calc_IslReefZonemetrics_BSR(bsrSITEg,"GENUS_CODE")
# bsrSITEt<-Calc_Sitemetrics_BSR(data.tax,"SPCODE")
# bsrISt<-Calc_Islmetrics_BSR(bsrSITEt,"SPCODE")
# bsrDEPTHt<-Calc_IslDepthmetrics_BSR(bsrSITEt,"SPCODE")
# bsrREEFZONEt<-Calc_IslReefZonemetrics_BSR(bsrSITEt,"SPCODE")

##Create table for island level summaries for total sclerarctinans
TotalScl_SITE<-subset(bsrSITEg,GENUS_CODE=="SSSS")
TotalScl_SITEf<-subset(bsrSITEgf,GENUS_CODE=="SSSS")

TotalScl_IS<-subset(bsrISg,GENUS_CODE=="SSSS")
TotalScl_ISf<-subset(bsrISgf,GENUS_CODE=="SSSS")

TotalScl_ISDEPTH<-subset(bsrDEPTHg,GENUS_CODE=="SSSS")
TotalScl_ISf<-subset(TotalScl_IS,REEF_ZONE=="Forereef")


# ##Create table for island level summaries for target genera, change this to target genera
# TargGenera_IS<-subset(bsrIS,GENUS_CODE %in% c("MOSP","POSP","LEPT")) 
# TargTaxon_IS<-subset(bsrIS,SPCODE %in% c("MOSP","POSP","LEPT")) 



###write out tables to csv files
TotalScl_ISas<-subset(TotalScl_IS,REGION_NAME=="American Samoa")
TotalScl_ISpria<-subset(TotalScl_IS,REGION_NAME=="Pacific Remote Island Areas")

write.csv(TotalScl_ISas,file="T:/Benthic/Data/REA Coral Demography/WorkingData_forBSRs/SAMOA_2018_Islandmetrics_totalscl.csv")
write.csv(TotalScl_ISpria,file="T:/Benthic/Data/REA Coral Demography/WorkingData_forBSRs/PRIAs_2018_Islandmetrics_totalscl.csv")

targetgenISas<-subset(bsrISg,REGION_NAME=="American Samoa" & GENUS_CODE %in% c("ACSP","MOSP","POCS","POSP","GONS"))
targetgenISpria<-subset(bsrISg,REGION_NAME=="Pacific Remote Island Areas" & GENUS_CODE %in% c("PAVS","ACSP","MOSP","POCS","POSP"))

write.csv(targetgenISas,file="T:/Benthic/Data/REA Coral Demography/WorkingData_forBSRs/SAMOA_2018_Islandmetrics_targetgenera.csv")
write.csv(targetgenISpria,file="T:/Benthic/Data/REA Coral Demography/WorkingData_forBSRs/PRIAs_2018_Islandmetrics_targetgenera.csv")



ggplot(targetgenISas, aes(x=ISLAND, y=meanAdultColDen)) + 
  geom_bar(stat="identity", fill="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=meanAdultColDen-seAdultColDen, ymax=meanAdultColDen+seAdultColDen), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~GENUS_CODE)+
  labs( x="Island", y = "Mean Adult Density")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Adult Density: American Samoa")

ggplot(targetgenISpria, aes(x=ISLAND, y=meanAdultColDen)) + 
  geom_bar(stat="identity", fill="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=meanAdultColDen-seAdultColDen, ymax=meanAdultColDen+seAdultColDen), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~GENUS_CODE)+
  labs( x="Island", y = "Mean Adult Density")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Adult Density:Pacific Remote Island Areas")

ggplot(targetgenISas, aes(x=ISLAND, y=meanJuvColDen)) + 
  geom_bar(stat="identity", fill="red", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=meanJuvColDen-seJuvColDen, ymax=meanJuvColDen+seJuvColDen), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~GENUS_CODE)+
  labs( x="Island", y = "Mean Juv Density")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Juv Density: American Samoa")

ggplot(targetgenISpria, aes(x=ISLAND, y=meanJuvColDen)) + 
  geom_bar(stat="identity", fill="red", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=meanJuvColDen-seJuvColDen, ymax=meanJuvColDen+seJuvColDen), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~GENUS_CODE)+
  labs( x="Island", y = "Mean Juv Density")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Juv Density:Pacific Remote Island Areas")


###Depth Regressions
as<-subset(TotalScl_SITE,REGION_NAME=="American Samoa");as$SITE_MAX_DEPTH<-as.numeric(as$SITE_MAX_DEPTH)
pria<-subset(TotalScl_SITE,REGION_NAME=="Pacific Remote Island Areas");pria$SITE_MAX_DEPTH<-as.numeric(pria$SITE_MAX_DEPTH)

write.csv(as,file="T:/Benthic/Data/REA Coral Demography/WorkingData_forBSRs/SAMOA_2018_Sitemetrics_totalscl.csv")
write.csv(pria,file="T:/Benthic/Data/REA Coral Demography/WorkingData_forBSRs/PRIAs_2018_Sitemetrics_totalscl.csv")


ggplot(as, aes(x=SITE_MAX_DEPTH, y=ACD,group=ISLAND)) + 
  geom_point(aes(color=ISLAND), size=2) + 
  geom_smooth(aes(color=ISLAND), method = "lm", se=FALSE) +
  scale_x_continuous(breaks=seq(10,80,by=5))+
  labs( x="Maximum Depth", y = "Adult Density")+
  ggtitle("Adult Density:American Samoa")

ggplot(pria, aes(x=SITE_MAX_DEPTH, y=ACD,group=ISLAND)) + 
  geom_point(aes(color=ISLAND), size=2) + 
  geom_smooth(aes(color=ISLAND), method = "lm", se=FALSE) +
  labs( x="Maximum Depth", y = "Adult Density")+
  ggtitle("Adult Density:Pacific Remote Island Areas")

ggplot(as, aes(x=SITE_MAX_DEPTH, y=JCD,group=ISLAND)) + 
  geom_point(aes(color=ISLAND), size=2) + 
  geom_smooth(aes(color=ISLAND), method = "lm", se=FALSE) +
  labs( x="Maximum Depth", y = "Juvenile Density")+
  ggtitle("Juvenile Density:American Samoa")

ggplot(pria, aes(x=SITE_MAX_DEPTH, y=JCD,group=ISLAND)) + 
  geom_point(aes(color=ISLAND), size=2) + 
  geom_smooth(aes(color=ISLAND), method = "lm", se=FALSE) +
  labs( x="Maximum Depth", y = "Juvenile Density")+
  ggtitle("Juvenile Density:Pacific Remote Island Areas")

