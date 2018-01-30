rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic Functions.R")
source("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA/Benthic_Islandwide Mean&Variance Functions.R")

#LOAD THE CLEAN wd 
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
load("TMPBenthicREAwd.Rdata")

#get base survey info, calculate average depth+complexity+so on

#base information about the survey - field names should match those in input file (obviously!)
UNIQUE_SURVEY<-c("SITEVISITID","SITE")
UNIQUE_TRANSECT<-c(UNIQUE_SURVEY, "TRANSECT")
UNIQUE_COLONY<-c(UNIQUE_TRANSECT, "COLONYID")


#Generate a table of metadata at the transect and colony level - ADD SECTOR NAME, ANALYSIS YEAR EVENTUALLY
SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH","TRANSECT")
survey_transect<-Aggregate_InputTable(wd, SURVEY_INFO)

SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH","TRANSECT","COLONYID")
survey_colony<-Aggregate_InputTable(wd, SURVEY_INFO)

save(survey_transect, file="TMPsurveys_Transect.Rdata")
save(survey_colony, file="TMPsurveys_Colony.Rdata")

#Pull all species information into a separate df, for possible later use ..
TAXON_MORPH_FIELDS<-c("SPCODE","GENUS_CODE", "S_ORDER", "SPMORPH", "GENMORPH","MORPH_CODE")
taxon_morph_table<-Aggregate_InputTable(wd, TAXON_MORPH_FIELDS)
save(taxon_morph_table, file="TMPtaxonmorph.Rdata")

#test functions on MHI data to make code run faster
wd2<-subset(wd,REGION=="MHI")

# GENERATE SUMMARY METRICS at the transect-level -this is just a start, more metrics to come --------------------------------------------------
m1<-Calc_ColDen_By_Transect(wd2,"GENUS_CODE");m1<-subset(m1,select=-Var.5);gen.cols<-names(m1[4:dim(m1)[2]]) # calculate density at genus level, remove column V1 (summary of AAAA) and create a separate df for just the data named gen.cols

m2<-Calc_olddead_By_Transect(wd)

#Transform RD1 and RD2 from long to wide format, remove other site AND TRANSECT level info
#This function calculates colony density at the transect scale by first calculating the total survey area (using Calc_SurveyArea_By_SIte) then calcuating colony density
#Calc_ColDen_By_Transect<-function(data){
wdscl<-subset(wd2,S_ORDER=="Scleractinia")
rd1<-dcast(wdscl, formula=SITEVISITID + SITE+TRANSECT+COLONYID ~ RD1, value.var="RD1",length,fill=0);names(rd1)<-gsub("DZGN","DZGNS",names(rd1),fixed = TRUE)#; rd1<-rd1[,-c(1:3)]
rd2<-dcast(wdscl, formula=SITEVISITID + SITE+TRANSECT+COLONYID ~ RD2, value.var="RD2",length,fill=0) ;names(rd2)<-gsub("DZGN","DZGNS",names(rd2),fixed = TRUE)#; rd2<-rd2[,-c(1:3)]
rd3<-dcast(wdscl, formula=SITEVISITID + SITE+TRANSECT+COLONYID ~ GENRD1, value.var="GENRD1",length,fill=0)#; rd3<-rd3[,-c(1:3)]
rd4<-dcast(wdscl, formula=SITEVISITID + SITE+TRANSECT+COLONYID ~ GENRD2, value.var="GENRD2",length,fill=0)# ; rd4<-rd4[,-c(1:3)]

#merge all dataframes together
a<-merge(rd1,rd2,by=c("SITEVISITID","SITE","TRANSECT","COLONYID"),all=TRUE)
b<-merge(a,rd3,by=c("SITEVISITID","SITE","TRANSECT","COLONYID"),all=TRUE)
allrd<-merge(b,rd4,by=c("SITEVISITID","SITE","TRANSECT","COLONYID"),all=TRUE)

#remove .x and .y so that we can sum identifically named columns
names(allrd)<-gsub(".x","",names(allrd),fixed = TRUE)
names(allrd)<-gsub(".y","",names(allrd),fixed = TRUE)
head(allrd)

allrd.n<-allrd[,-c(1:4)]


#Sum identically named columns and remove the no data column
allrd2<-as.data.frame(sapply(unique(colnames(allrd.n)), 
                             function(x) rowSums(allrd.n[, colnames(allrd.n) == x, drop = FALSE])));allrd2<-allrd2[,!(colnames(allrd2) =="NODATA")]

#merge data with colony level metada, remove colonyid, then add up number of colonies with with condition to the transect level
allrd3<-merge(survey_colony,allrd2, by="COLONYID")
allrd3<-allrd3[,!(colnames(allrd3) =="COLONYID")]
metadata<-colnames(allrd3[1:15])
allrd3<-data.table(allrd3)
rdsum<-allrd3[, lapply(.SD, sum), by = metadata]
rdsum<-as.data.frame(rdsum)

##Calculate total transect area surveyed including transects with no colonies
trarea<-Calc_SurveyArea_By_Transect(wd)

#Sum total number of hard corals/transect
scl<-subset(wd,S_ORDER=="Scleractinia") 
b<-ddply(scl, .(SITE,SITEVISITID,TRANSECT),
         summarise,
         Colabun=length(COLONYLENGTH))

a<-merge(trarea,b, by=c("SITE","SITEVISITID","TRANSECT"),all.x=TRUE)

a[is.na(a)]<-0

rdsum2<-merge(a,rdsum,by=c("SITE","SITEVISITID","TRANSECT"),all.x=TRUE)




#cond$CronDZ<-allrd2$FUG+allrd2$PDS+allrd2$SGA+allrd2$PTR
#allrd2$SGA_PDS<-allrd2$PDS+allrd2$SGA

##Merging Colony abundance df and recent dead df




#Merge Site Data and Count Data Per Site Per Grouping Variable (e.g. Species, Tropic_MonRep, Family) 
wsd<-merge(survey_transect,m1,by=UNIQUE_TRANSECT)
wsd$TotColden<-rowSums(wsd[,gen.cols]) #calculate total colony density
subset(wsd,TotColden==0) #double check that there are transects with 0 colonies weren't dropped
data.cols<-c(gen.cols, "TotColden")

# OUTPUT working_site_data  -----------------------------------
save(wsd_site, file="TMPwsd_site.Rdata")
