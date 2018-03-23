rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic_Functions.R")
source("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/core_functions.R")


#LOAD THE CLEAN wd 
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
load("TMPBenthicREA_Adultwd_v2.Rdata")
awd<-wd
load("TMPBenthicREA_Juvwd.Rdata")
jwd<-wd


#base information about the survey - field names should match those in input file (obviously!)
UNIQUE_SURVEY<-c("SITEVISITID","SITE")
UNIQUE_TRANSECT<-c(UNIQUE_SURVEY, "TRANSECT")
UNIQUE_COLONY<-c(UNIQUE_TRANSECT, "COLONYID")


#Generate a table of metadata at the transect and colony level for ADULTS- ADD SECTOR NAME, ANALYSIS YEAR EVENTUALLY
SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT","TRANSECT")
survey_transect<-Aggregate_InputTable(awd, SURVEY_INFO)

SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT","TRANSECT","COLONYID","GENUS_CODE","TAXONCODE","S_ORDER")
survey_colony<-Aggregate_InputTable(awd, SURVEY_INFO)

save(survey_transect, file="TMPsurveys_Transect.Rdata")
save(survey_colony, file="TMPsurveys_Colony.Rdata")


#Generate a table of metadata at the transect and colony level for JUVENILES- ADD SECTOR NAME, ANALYSIS YEAR EVENTUALLY
SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT","TRANSECT")
jsurvey_transect<-Aggregate_InputTable(jwd, SURVEY_INFO)

SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT","TRANSECT","COLONYID","GENUS_CODE","TAXONCODE","S_ORDER")
jsurvey_colony<-Aggregate_InputTable(jwd, SURVEY_INFO)

save(survey_transect, file="TMPsurveys_Transect.Rdata")
save(survey_colony, file="TMPsurveys_Colony.Rdata")

#Pull all species information into a separate df, for possible later use ..
TAXON_MORPH_FIELDS<-c("TAXONCODE","GENUS_CODE", "S_ORDER", "TAXMORPH", "GENMORPH","MORPH_CODE")
taxon_morph_table<-Aggregate_InputTable(awd, TAXON_MORPH_FIELDS)
save(taxon_morph_table, file="TMPtaxonmorph.Rdata")


#Remove scleractinan colony fragments, but include include sites with no colonies (AAAA) and other cnidarians
#if AAAA or other cnidarians are excluded here then the transect area may not calculated properly.
awd<-subset(awd, COLONYLENGTH>5&S_ORDER=="Scleractinia"|COLONYLENGTH<0|SPCODE=="AAAA")
tail(wd)#make sure that AAAA's are included

#Remove transects with less than 5m surveyed and check how many rows were removed
nrow(awd)
awd<-subset(awd,TRANSECTAREA>=5) 
nrow(awd)
head(awd)

##Change Transects 3 and 4 in the juvenile data to 1 and 2 so we can merge with adult data
jwd$TRANSECT[jwd$TRANSECT == "3"] <- "1"
jwd$TRANSECT[jwd$TRANSECT == "4"] <- "2"


#test functions on MHI data to make code run faster
awd2<-subset(awd,REGION=="MHI")
jwd2<-subset(jwd,REGION=="MHI")

# GENERATE SUMMARY METRICS at the transect-level -this is just a start, more metrics to come --------------------------------------------------
m1<-Calc_ColDen_Transect(awd2,"GENUS_CODE")# calculate density at genus level as well as total
#m2<-Calc_Rich_By_Transect(wd2);m2<-subset(m2,select=-Var.4);richorder.cols<-names(m2[4:dim(m2)[2]]) # calculate richness at order level, remove column V1 (summary of AAAA) and create a separate df for just the data named gen.cols
m2<-Calc_olddead_Transect(awd2,"GENUS_CODE")
m3<-Calc_recentdead_Transect(awd2,"GENUS_CODE")
m4<-Calc_RDabun_Transect(awd2,"GENUS_CODE")
m5<-Calc_Condabun_Transect(awd2,"GENUS_CODE")


m6<-Calc_ColDen_Transect(awd2,"TAXONCODE")# calculate density at genus level as well as total
#m2<-Calc_Rich_By_Transect(wd2);m2<-subset(m2,select=-Var.4);richorder.cols<-names(m2[4:dim(m2)[2]]) # calculate richness at order level, remove column V1 (summary of AAAA) and create a separate df for just the data named gen.cols
m7<-Calc_olddead_Transect(awd2,"TAXONCODE")
m8<-Calc_recentdead_Transect(awd2,"TAXONCODE")
m9<-Calc_RDabun_Transect(awd2,"TAXONCODE")
m10<-Calc_Condabun_Transect(awd2,"TAXONCODE")

m11<-Calc_ColDen_Transect(jwd2,"GENUS_CODE"); colnames(m11)[colnames(m11)=="Colabun"]<-"JuvColabun";colnames(m11)[colnames(m11)=="ColDen"]<-"JuvColDen"
# calculate density at genus level as well as total
m12<-Calc_ColDen_Transect(jwd2,"TAXONCODE"); 

test<-Calc_ColDen_Transect_DEPTH(awd2)
##Unweighted Summaries for Benthic Summary Reports
# GENERATE SUMMARY METRICS at the transect-level -this is just a start, more metrics to come --------------------------------------------------
rd<-merge(m1,m4, by=c("SITE","SITEVISITID","TRANSECT","GENUS_CODE"),all.x=TRUE)
con<-merge(m1,m5, by=c("SITE","SITEVISITID","TRANSECT","GENUS_CODE"),all.x=TRUE) # THIS CODE GENERATES NAS IN THE RDCOND COLUMN FOR TAXA THAT AREN'T PRESENT ON TRANSECT. SPEAK WITH IVOR
rd.<-ifelse(rd$Colabun==0& rd$RDCond=="NA",rd$RDabun=="NA",ifelse(rd$Colabun>0 & rd$RDabun=="NA",rd$RDabun==0,rd$RDabun==rd$RDabun))####this isn't working
con$Condabun[is.na(con$Condabun)]<-0
rd$RDabun[is.na(rd$RDabun)]<-0

head(con);head(rd)
rd$rdprev<-rd$RDabun/rd$Colabun*100 #calculate prevalence
con$condprev<-con$Condabun/con$Colabun*100 #calculate prevalence

AcuteDZ<-subset(rd,RDCond=="All_DZGN");colnames(AcuteDZ)[colnames(AcuteDZ)=="RDabun"]<-"AcuteDZabun";colnames(AcuteDZ)[colnames(AcuteDZ)=="rdprev"]<-"AcuteDZprev" #Change column name
ChronicDZ<-subset(con,COND=="ChronicDZ");colnames(ChronicDZ)[colnames(ChronicDZ)=="Condabun"]<-"ChronicDZabun";colnames(ChronicDZ)[colnames(ChronicDZ)=="condprev"]<-"ChronicDZprev" #Change column name
COTS<-subset(rd,RDCond=="COTS");colnames(COTS)[colnames(COTS)=="RDabun"]<-"COTSabun";colnames(COTS)[colnames(COTS)=="rdprev"]<-"COTSprev" #Change column name
BLE<-subset(con,COND=="BLE");colnames(BLE)[colnames(BLE)=="Condabun"]<-"BLEabun";colnames(BLE)[colnames(BLE)=="condprev"]<-"BLEprev" #Change column name
head(ChronicDZ)


a1<-merge(AcuteDZ,ChronicDZ, by=c("DEPTH_BIN","REEF_ZONE","SITE","SITEVISITID","TRANSECT","GENUS_CODE","Colabun","ColDen"),all=TRUE) # THIS CODE GENERATES NAS IN THE RDCOND COLUMN FOR TAXA THAT AREN'T PRESENT ON TRANSECT. SPEAK WITH IVOR
a2<-merge(a1,BLE, by=c("DEPTH_BIN","REEF_ZONE","SITE","SITEVISITID","TRANSECT","GENUS_CODE","Colabun","ColDen"),all=TRUE) # THIS CODE GENERATES NAS IN THE RDCOND COLUMN FOR TAXA THAT AREN'T PRESENT ON TRANSECT. SPEAK WITH IVOR
a3<-merge(a2,COTS, by=c("DEPTH_BIN","REEF_ZONE","SITE","SITEVISITID","TRANSECT","GENUS_CODE","Colabun","ColDen"),all=TRUE) # THIS CODE GENERATES NAS IN THE RDCOND COLUMN FOR TAXA THAT AREN'T PRESENT ON TRANSECT. SPEAK WITH IVOR
head(a3)

a3$TotDZabun<-a3$AcuteDZabun+a3$ChronicDZabun
a3$TotDZprev<-a3$TotDZabun/a3$Colabun
colnames(a3)[colnames(a3)=="Colabun"]<-"AdultColabun"
colnames(a3)[colnames(a3)=="ColDen"]<-"AdultColDen"


a3<- subset(a3, select=-c(RDCond,COND.x,COND.y)) #remove extra columns
a4<-merge(a4,m11,by =c("DEPTH_BIN","REEF_ZONE","SITE","SITEVISITID","TRANSECT","GENUS_CODE"),all=TRUE) #merge adult data with juv data
a4[is.na(a4)]<-0 #THIS IS TEMPORARY- FIX NAS IN THE IFELSE STATEMENT ABOVE
head(a4)

##Unweighted Summaries for Benthic Summary Reports
# GENERATE SUMMARY METRICS at the transect-level -this is just a start, more metrics to come --------------------------------------------------
bsrSITE<-Calc_Sitemetrics_BSR(a4,"GENUS_CODE")
bsrIS<-Calc_Islmetrics_BSR(bsrSITE,"GENUS_CODE")
bsrDEPTH<-Calc_IslDepthmetrics_BSR(bsrSITE,"GENUS_CODE")

##Create table for island level summaries for total sclerarctinans
TotalScl_SITE<-subset(bsrSITE,GENUS_CODE=="SSSS")
TotalScl_IS<-subset(bsrIS,GENUS_CODE=="SSSS")
TotalScl_ISDEPTH<-subset(bsrDEPTH,GENUS_CODE=="SSSS")

##Create table for island level summaries for target genera, change this to target genera
TargGenera_IS<-subset(bsrIS,GENUS_CODE %in% c("MOSP","POSP","LEPT")) 


# get strata and sectors data and subset it for the regions you need
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
sectors<-read.csv("Benthic_SectorArea_v4.csv", stringsAsFactors=FALSE)
samoa<-subset(sectors,REGION=="SAMOA")
pria<-subset(sectors,REGION=="PRIAs")


###write out tables to csv files
write.csv(samoa,"Samoa_Stratareas.csv")
write.csv(pria,"PRIA_Stratareas.csv")
write.csv(TotalScl_SITE,"Sitemetrics_totalscl.csv")
write.csv(TotalScl_IS,"Islandmetrics_totalscl.csv")
write.csv(TotalScl_ISDEPTH,"IslandDepth_metrics_totalscl.csv")
write.csv(TargGenera_IS,"Islandmetrics_targetgenera.csv")




#Merge Site Data and Count Data Per Site Per Grouping Variable (e.g. Species, Tropic_MonRep, Family) 
wsd<-merge(survey_transect,m1,by=UNIQUE_TRANSECT)
wsd$TotColden<-rowSums(wsd[,gen.cols]) #calculate total colony density
subset(wsd,TotColden==0) #double check that there are transects with 0 colonies weren't dropped
data.cols<-c(gen.cols, "TotColden")

# OUTPUT working_site_data  -----------------------------------
save(wsd_site, file="TMPwsd_site.Rdata")
