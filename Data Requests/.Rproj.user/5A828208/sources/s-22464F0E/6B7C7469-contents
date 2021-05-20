#This script is a modified version of the REA Coral Demography_CalcPooled script that generates strata, sector and island summaries from site level data
#After discussion amongst the benthic and fish teams regarding pooling strategies we decided that if individuals are interested in comparing strata, sectors and islands across time
#it is not appropirate to include all strata surveyed in a given year because the same strata may not have been sampled across years due to logistical constraints.
#In this script, we identify which strata were sampled in multiple years then subset those strata. It may not be appropriate to use island-level estimates because the island data generated in this
#script may only consist of a few strata.

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

#Function that subsets strata that were surveyed in more than 1 year
Timeseries_Subset<-function(data){

  levels(data$MISSIONID)
  data$EXCLUDE_FLAG<-ifelse(data$MISSIONID %in% c("MP1410","MP1512","MP1602","MP2006"),-1,0) #I left SE1602 in (2016 Jarvis and Rose)
  head(subset(data,EXCLUDE_FLAG==-1))

  #Actually remove special missions.
  data<-subset(data,EXCLUDE_FLAG==0);
  # this dataframe should be empty
  head(subset(data,EXCLUDE_FLAG==-1))

  # GENERATE DATA FOR TEMPORAL ANALYSIS
  data$STRATANAME<- paste(data$SEC_NAME,data$REEF_ZONE,data$DEPTH_BIN,sep="_")
  st.list<-ddply(data,.(METHOD,OBS_YEAR,REGION,ISLAND,SEC_NAME,STRATANAME),summarize,n=length(unique(SITE)))
  st.list2<-subset(st.list,n>=2);head(st.list)

  #Generate list of strata that were surveyed in all years for a given region and had at least 2 sites/stratum
  st.list_w<-dcast(st.list2, formula=METHOD+REGION+ISLAND+SEC_NAME+STRATANAME~ OBS_YEAR, value.var="n",fill=0)
  dCOLS<-c("2013","2014","2015","2016","2017","2018","2019")
  st.list_w$year_n<-rowSums(st.list_w[,dCOLS] > 0, na.rm=T) #count # of years of data
  st.list_w2<-subset(st.list_w,REGION %in% c("MARIAN","PRIAs","SAMOA") & year_n>=2) #only include strata that have at least 2 years
  st.list_w3<-subset(st.list_w,REGION %in% c("MHI","NWHI") & year_n>=3)#only include strata that have at least 3 years
  st.list_w4<-rbind(st.list_w2,st.list_w3)

  head(st.list_w4);st.list_w4<-droplevels(st.list_w4) #generate the list

  data<-data[data$STRATANAME %in% c(st.list_w4$STRATANAME),] #Subset data to only include strata of interest

}

# POOLING DATA from Site to Strata and Domain at GENUS_CODE-level---------------------------------------------------
#Removing special missions and subsetting strata that were surveyed in more than 1 year
site.data.gen2<-Timeseries_Subset(site.data.gen2)

#Pooling
survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

#This function will pool data at Sector scale according to predetermined groups. Protected reef slope is converted to Forereef here
site.data.gen2<-PoolSecStrat(site.data.gen2)
# rich.data<-PoolSecStrat(rich.data.gen)

#QC CHECK to make sure the sectors and strata pooled correctly
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


#Calculate metrics at Strata-level-We need to work on combining metrics into 1 function

#Create a vector of columns to subset for strata estimates
c.keep<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
          "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep2<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
           "n_h","N_h","D._h","SE_D._h")
c.keep3<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
           "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep4<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
           "n_h","N_h","prev","SEprev")

acdGEN_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","AdColDen","Adpres.abs");acdGEN_st=acdGEN_st[,c.keep]
colnames(acdGEN_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","AdColDen","SE_AdColDen","Adult_avp","Adult_seprop","Adult_Abun","Adult_SE_Abun","Adult_CV")

jcdGEN_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","JuvColDen","Juvpres.abs");jcdGEN_st=jcdGEN_st[,c.keep]
colnames(jcdGEN_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","JuvColDen","SE_JuvColDen","Juv_avp","Juv_seprop","Juv_Abun","Juv_SE_Abun","Juv_CV")

odGEN_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.od");odGEN_st=odGEN_st[,c.keep2]
colnames(odGEN_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.od","SE_Ave.od")

rdGEN_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.rd");rdGEN_st=rdGEN_st[,c.keep2]
colnames(rdGEN_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.rd","SE_Ave.rd")

clGEN_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.size");clGEN_st=clGEN_st[,c.keep2]
colnames(clGEN_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.size","SE_Ave.size")

BLEGEN_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","BLE");BLEGEN_st=BLEGEN_st[,c.keep4]
colnames(BLEGEN_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","BLE","SE_BLE")

AcuteDZGEN_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","AcuteDZ");AcuteDZGEN_st=AcuteDZGEN_st[,c.keep4]
colnames(AcuteDZGEN_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","AcuteDZ","SE_AcuteDZ")

ChronicDZGEN_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","ChronicDZ");ChronicDZGEN_st=ChronicDZGEN_st[,c.keep4]
colnames(ChronicDZGEN_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","ChronicDZ","SE_ChronicDZ")


#Double Check that revised pooling is adding up NH (total sites) correctly
View(acdGEN_st)
View(sectors)


#Calculate Island Estimates
acdGEN_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","AdColDen","Adpres.abs")
acdGEN_is<-acdGEN_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AdColDen","SE_AdColDen","CV_D._st","Y._st","SE_Y._st")]
colnames(acdGEN_is)[colnames(acdGEN_is)=="CV_D._st"]<-"Adult_CV"
colnames(acdGEN_is)[colnames(acdGEN_is)=="Y._st"]<-"Adult_Abun"
colnames(acdGEN_is)[colnames(acdGEN_is)=="SE_Y._st"]<-"Adult_SE_Abun"

jcdGEN_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","JuvColDen","Juvpres.abs")
jcdGEN_is<-jcdGEN_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen","CV_D._st")]
colnames(jcdGEN_is)[colnames(jcdGEN_is)=="CV_D._st"]<-"Juv_CV"
colnames(jcdGEN_is)[colnames(jcdGEN_is)=="Y._st"]<-"Juv_Abun"
colnames(jcdGEN_is)[colnames(jcdGEN_is)=="SE_Y._st"]<-"Juv_SE_Abun"

odGEN_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.od")
odGEN_is<-odGEN_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.od","SE_Ave.od")]
rdGEN_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.rd")
rdGEN_is<-rdGEN_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.rd","SE_Ave.rd")]
clGEN_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.size")
clGEN_is<-clGEN_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.size","SE_Ave.size")]
bleGEN_is<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","BLE")
bleGEN_is<-bleGEN_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_BLE_Prev","SE_BLE_Prev")]
AcuteDZGEN_is<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","AcuteDZ")
AcuteDZGEN_is<-AcuteDZGEN_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AcuteDZ_Prev","SE_AcuteDZ_Prev")]
ChronicDZGEN_is<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","ChronicDZ")
ChronicDZGEN_is<-ChronicDZGEN_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_ChronicDZ_Prev","SE_ChronicDZ_Prev")]


#Calculate Sector Estimates
site.data.gen2$ANALYSIS_SCHEMA<-site.data.gen2$STRATANAME
site.data.gen2$DOMAIN_SCHEMA<-site.data.gen2$BEN_SEC


acdGEN_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","AdColDen","Adpres.abs")
acdGEN_sec<-acdGEN_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AdColDen","SE_AdColDen","CV_D._st","Y._st","SE_Y._st")]
colnames(acdGEN_sec)[colnames(acdGEN_sec)=="CV_D._st"]<-"Adult_CV"
colnames(acdGEN_sec)[colnames(acdGEN_sec)=="Y._st"]<-"Adult_Abun"
colnames(acdGEN_sec)[colnames(acdGEN_sec)=="SE_Y._st"]<-"Adult_SE_Abun"
jcdGEN_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","JuvColDen","Juvpres.abs")
jcdGEN_sec<-jcdGEN_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen","CV_D._st","Y._st","SE_Y._st")]
colnames(jcdGEN_sec)[colnames(jcdGEN_sec)=="CV_D._st"]<-"Juv_CV"
colnames(jcdGEN_sec)[colnames(jcdGEN_sec)=="Y._st"]<-"Juv_Abun"
colnames(jcdGEN_sec)[colnames(jcdGEN_sec)=="SE_Y._st"]<-"Juv_SE_Abun"

odGEN_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.od")
odGEN_sec<-odGEN_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.od","SE_Ave.od")]
rdGEN_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.rd")
rdGEN_sec<-rdGEN_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.rd","SE_Ave.rd")]
clGEN_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.size")
clGEN_sec<-clGEN_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.size","SE_Ave.size")]
bleGEN_sec<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","BLE")
bleGEN_sec<-bleGEN_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_BLE_Prev","SE_BLE_Prev")]
AcuteDZGEN_sec<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","AcuteDZ")
AcuteDZGEN_sec<-AcuteDZGEN_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AcuteDZ_Prev","SE_AcuteDZ_Prev")]
ChronicDZGEN_sec<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","ChronicDZ")
ChronicDZGEN_sec<-ChronicDZGEN_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_ChronicDZ_Prev","SE_ChronicDZ_Prev")]


#Merge dataframes together
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
st.data.gen<-Reduce(MyMerge, list(acdGEN_st,jcdGEN_st,odGEN_st,rdGEN_st,clGEN_st,BLEGEN_st,AcuteDZGEN_st,ChronicDZGEN_st))
colnames(st.data.gen)[colnames(st.data.gen)=="ANALYSIS_SCHEMA"]<-"Stratum"


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
is.data.gen<-Reduce(MyMerge, list(acdGEN_is,jcdGEN_is,odGEN_is,rdGEN_is,clGEN_is,bleGEN_is,AcuteDZGEN_is,ChronicDZGEN_is))
colnames(is.data.gen)[colnames(is.data.gen)=="DOMAIN_SCHEMA"]<-"ISLAND"


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
sec.data.gen<-Reduce(MyMerge, list(acdGEN_sec,jcdGEN_sec,odGEN_sec,rdGEN_sec,clGEN_sec,bleGEN_sec,AcuteDZGEN_sec,ChronicDZGEN_sec))
colnames(sec.data.gen)[colnames(sec.data.gen)=="DOMAIN_SCHEMA"]<-"Sector"

#REMOVE HOWLAND AND BAKER 2017 data- we only surveyed juveniles and coral cover at those islands in 2017
#Chose not to remove it until here in case you would like juvenile data from those locations and year.
# st.data.gen<-filter(st.data.gen,!(ISLAND %in% c("Howland","Baker") & st.data.gen$ANALYSIS_YEAR=="2017"))
# is.data.gen<-filter(is.data.gen,!(ISLAND %in% c("Howland","Baker") & is.data.gen$ANALYSIS_YEAR=="2017"))
# sec.data.gen<-filter(sec.data.gen,!(ISLAND %in% c("Howland","Baker") & sec.data.gen$ANALYSIS_YEAR=="2017"))


write.csv(st.data.gen,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicREA_STRATA_GENUS_CODE_Timeseries.csv",row.names=F)
write.csv(is.data.gen,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicREA_ISLAND_GENUS_CODE_Timeseries.csv",row.names=F)
write.csv(sec.data.gen,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicREA_SECTOR_GENUS_CODE_Timeseries.csv",row.names=F)



# POOLING DATA from Site to Strata and Domain at TAXONCODE-level---------------------------------------------------
#Removing special missions and subsetting strata that were surveyed in more than 1 year
site.data.tax2<-Timeseries_Subset(site.data.tax2)

##Pooling
survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

#This function will pool data at Sector scale according to predetermined groups. Protected reef slope is converted to Forereef here
site.data.tax2<-PoolSecStrat(site.data.tax2)
# rich.data<-PoolSecStrat(rich.data.tax)

#QC CHECK to make sure the sectors and strata pooled correctly
data.test<-ddply(subset(site.data.tax2,TAXONCODE=="SSSS"),.(REGION,BEN_SEC,OBS_YEAR,STRATANAME),summarize,n=length(SITE))
sm.test<-ddply(subset(survey_master,Benthic=="1"&EXCLUDE_FLAG=="0"&OBS_YEAR>=2013),.(REGION,ISLAND,SEC_NAME,OBS_YEAR,REEF_ZONE,DEPTH_BIN),summarize,n=length(SITE))

write.csv(data.test,"tmp_sitedataQC.csv")
write.csv(sm.test,"tmp_sitemasterQC.csv")

# #Subset just Forereef Sites & just target taxa
# site.data.tax2<-subset(site.data.tax2,REEF_ZONE=="Forereef")
# site.data.tax2<-subset(site.data.tax2,TAXONCODE %in% c("ACSP", "MOSP", "PAVS", "POCS","POSP","SSSS"))
# rich.data<-subset(rich.data,REEF_ZONE=="Forereef")

# #Make sure you everything but forereef are dropped
# table(site.data.tax2$REEF_ZONE,site.data.tax2$TAXONCODE)
# table(rich.data$REEF_ZONE)


#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
site.data.tax2$ANALYSIS_SCHEMA<-site.data.tax2$STRATANAME
site.data.tax2$DOMAIN_SCHEMA<-site.data.tax2$ISLAND


#Calculate metrics at Strata-level-We need to work on combining metrics into 1 function

#Create a vector of columns to subset for strata estimates
c.keep<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","TAXONCODE",
          "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep2<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","TAXONCODE",
           "n_h","N_h","D._h","SE_D._h")
c.keep3<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","TAXONCODE",
           "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep4<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","TAXONCODE",
           "n_h","N_h","prev","SEprev")

acdTAX_st<-Calc_Strata(site.data.tax2,"TAXONCODE","AdColDen","Adpres.abs");acdTAX_st=acdTAX_st[,c.keep]
colnames(acdTAX_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","TAXONCODE","n","Ntot","AdColDen","SE_AdColDen","Adult_avp","Adult_seprop","Adult_Abun","Adult_SE_Abun","Adult_CV")

jcdTAX_st<-Calc_Strata(site.data.tax2,"TAXONCODE","JuvColDen","Juvpres.abs");jcdTAX_st=jcdTAX_st[,c.keep]
colnames(jcdTAX_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","TAXONCODE","n","Ntot","JuvColDen","SE_JuvColDen","Juv_avp","Juv_seprop","Juv_Abun","Juv_SE_Abun","Juv_CV")

odTAX_st<-Calc_Strata(site.data.tax2,"TAXONCODE","Ave.od");odTAX_st=odTAX_st[,c.keep2]
colnames(odTAX_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","TAXONCODE","n","Ntot","Ave.od","SE_Ave.od")

rdTAX_st<-Calc_Strata(site.data.tax2,"TAXONCODE","Ave.rd");rdTAX_st=rdTAX_st[,c.keep2]
colnames(rdTAX_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","TAXONCODE","n","Ntot","Ave.rd","SE_Ave.rd")

clTAX_st<-Calc_Strata(site.data.tax2,"TAXONCODE","Ave.size");clTAX_st=clTAX_st[,c.keep2]
colnames(clTAX_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","TAXONCODE","n","Ntot","Ave.size","SE_Ave.size")

BLETAX_st<-Calc_Strata_Prevalence(site.data.tax2,"TAXONCODE","BLE");BLETAX_st=BLETAX_st[,c.keep4]
colnames(BLETAX_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","TAXONCODE","n","Ntot","BLE","SE_BLE")

AcuteDZTAX_st<-Calc_Strata_Prevalence(site.data.tax2,"TAXONCODE","AcuteDZ");AcuteDZTAX_st=AcuteDZTAX_st[,c.keep4]
colnames(AcuteDZTAX_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","TAXONCODE","n","Ntot","AcuteDZ","SE_AcuteDZ")

ChronicDZTAX_st<-Calc_Strata_Prevalence(site.data.tax2,"TAXONCODE","ChronicDZ");ChronicDZTAX_st=ChronicDZTAX_st[,c.keep4]
colnames(ChronicDZTAX_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","TAXONCODE","n","Ntot","ChronicDZ","SE_ChronicDZ")


#Double Check that revised pooling is adding up NH (total sites) correctly
View(acdTAX_st)
View(sectors)


#Calculate Island Estimates
acdTAX_is<-Calc_Domain(site.data.tax2,"TAXONCODE","AdColDen","Adpres.abs")
acdTAX_is<-acdTAX_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_AdColDen","SE_AdColDen","CV_D._st","Y._st","SE_Y._st")]
colnames(acdTAX_is)[colnames(acdTAX_is)=="CV_D._st"]<-"Adult_CV"
colnames(acdTAX_is)[colnames(acdTAX_is)=="Y._st"]<-"Adult_Abun"
colnames(acdTAX_is)[colnames(acdTAX_is)=="SE_Y._st"]<-"Adult_SE_Abun"

jcdTAX_is<-Calc_Domain(site.data.tax2,"TAXONCODE","JuvColDen","Juvpres.abs")
jcdTAX_is<-jcdTAX_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen","CV_D._st")]
colnames(jcdTAX_is)[colnames(jcdTAX_is)=="CV_D._st"]<-"Juv_CV"
colnames(jcdTAX_is)[colnames(jcdTAX_is)=="Y._st"]<-"Juv_Abun"
colnames(jcdTAX_is)[colnames(jcdTAX_is)=="SE_Y._st"]<-"Juv_SE_Abun"

odTAX_is<-Calc_Domain(site.data.tax2,"TAXONCODE","Ave.od")
odTAX_is<-odTAX_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_Ave.od","SE_Ave.od")]
rdTAX_is<-Calc_Domain(site.data.tax2,"TAXONCODE","Ave.rd")
rdTAX_is<-rdTAX_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_Ave.rd","SE_Ave.rd")]
clTAX_is<-Calc_Domain(site.data.tax2,"TAXONCODE","Ave.size")
clTAX_is<-clTAX_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_Ave.size","SE_Ave.size")]
bleTAX_is<-Calc_Domain_Prevalence(site.data.tax2,"TAXONCODE","BLE")
bleTAX_is<-bleTAX_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_BLE_Prev","SE_BLE_Prev")]
AcuteDZTAX_is<-Calc_Domain_Prevalence(site.data.tax2,"TAXONCODE","AcuteDZ")
AcuteDZTAX_is<-AcuteDZTAX_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_AcuteDZ_Prev","SE_AcuteDZ_Prev")]
ChronicDZTAX_is<-Calc_Domain_Prevalence(site.data.tax2,"TAXONCODE","ChronicDZ")
ChronicDZTAX_is<-ChronicDZTAX_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_ChronicDZ_Prev","SE_ChronicDZ_Prev")]


#Calculate Sector Estimates
site.data.tax2$ANALYSIS_SCHEMA<-site.data.tax2$STRATANAME
site.data.tax2$DOMAIN_SCHEMA<-site.data.tax2$BEN_SEC


acdTAX_sec<-Calc_Domain(site.data.tax2,"TAXONCODE","AdColDen","Adpres.abs")
acdTAX_sec<-acdTAX_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_AdColDen","SE_AdColDen","CV_D._st","Y._st","SE_Y._st")]
colnames(acdTAX_sec)[colnames(acdTAX_sec)=="CV_D._st"]<-"Adult_CV"
colnames(acdTAX_sec)[colnames(acdTAX_sec)=="Y._st"]<-"Adult_Abun"
colnames(acdTAX_sec)[colnames(acdTAX_sec)=="SE_Y._st"]<-"Adult_SE_Abun"
jcdTAX_sec<-Calc_Domain(site.data.tax2,"TAXONCODE","JuvColDen","Juvpres.abs")
jcdTAX_sec<-jcdTAX_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen","CV_D._st","Y._st","SE_Y._st")]
colnames(jcdTAX_sec)[colnames(jcdTAX_sec)=="CV_D._st"]<-"Juv_CV"
colnames(jcdTAX_sec)[colnames(jcdTAX_sec)=="Y._st"]<-"Juv_Abun"
colnames(jcdTAX_sec)[colnames(jcdTAX_sec)=="SE_Y._st"]<-"Juv_SE_Abun"

odTAX_sec<-Calc_Domain(site.data.tax2,"TAXONCODE","Ave.od")
odTAX_sec<-odTAX_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_Ave.od","SE_Ave.od")]
rdTAX_sec<-Calc_Domain(site.data.tax2,"TAXONCODE","Ave.rd")
rdTAX_sec<-rdTAX_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_Ave.rd","SE_Ave.rd")]
clTAX_sec<-Calc_Domain(site.data.tax2,"TAXONCODE","Ave.size")
clTAX_sec<-clTAX_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_Ave.size","SE_Ave.size")]
bleTAX_sec<-Calc_Domain_Prevalence(site.data.tax2,"TAXONCODE","BLE")
bleTAX_sec<-bleTAX_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_BLE_Prev","SE_BLE_Prev")]
AcuteDZTAX_sec<-Calc_Domain_Prevalence(site.data.tax2,"TAXONCODE","AcuteDZ")
AcuteDZTAX_sec<-AcuteDZTAX_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_AcuteDZ_Prev","SE_AcuteDZ_Prev")]
ChronicDZTAX_sec<-Calc_Domain_Prevalence(site.data.tax2,"TAXONCODE","ChronicDZ")
ChronicDZTAX_sec<-ChronicDZTAX_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_ChronicDZ_Prev","SE_ChronicDZ_Prev")]


#Merge dataframes together
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","TAXONCODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
st.data.tax<-Reduce(MyMerge, list(acdTAX_st,jcdTAX_st,odTAX_st,rdTAX_st,clTAX_st,BLETAX_st,AcuteDZTAX_st,ChronicDZTAX_st))
colnames(st.data.tax)[colnames(st.data.tax)=="ANALYSIS_SCHEMA"]<-"Stratum"


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","TAXONCODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
is.data.tax<-Reduce(MyMerge, list(acdTAX_is,jcdTAX_is,odTAX_is,rdTAX_is,clTAX_is,bleTAX_is,AcuteDZTAX_is,ChronicDZTAX_is))
colnames(is.data.tax)[colnames(is.data.tax)=="DOMAIN_SCHEMA"]<-"ISLAND"


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","TAXONCODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
sec.data.tax<-Reduce(MyMerge, list(acdTAX_sec,jcdTAX_sec,odTAX_sec,rdTAX_sec,clTAX_sec,bleTAX_sec,AcuteDZTAX_sec,ChronicDZTAX_sec))
colnames(sec.data.tax)[colnames(sec.data.tax)=="DOMAIN_SCHEMA"]<-"Sector"

#REMOVE HOWLAND AND BAKER 2017 data- we only surveyed juveniles and coral cover at those islands in 2017
#Chose not to remove it until here in case you would like juvenile data from those locations and year.
# st.data.tax<-filter(st.data.tax,!(ISLAND %in% c("Howland","Baker") & st.data.tax$ANALYSIS_YEAR=="2017"))
# is.data.tax<-filter(is.data.tax,!(ISLAND %in% c("Howland","Baker") & is.data.tax$ANALYSIS_YEAR=="2017"))
# sec.data.tax<-filter(sec.data.tax,!(ISLAND %in% c("Howland","Baker") & sec.data.tax$ANALYSIS_YEAR=="2017"))


write.csv(st.data.tax,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicREA_STRATA_TAXONCODE_Timeseries.csv",row.names=F)
write.csv(is.data.tax,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicREA_ISLAND_TAXONCODE_Timeseries.csv",row.names=F)
write.csv(sec.data.tax,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicREA_SECTOR_TAXONCODE_Timeseries.csv",row.names=F)

# POOLING DATA from Site to Strata and Domain at SPCODE-level---------------------------------------------------

#Removing special missions and subsetting strata that were surveyed in more than 1 year
site.data.sp2<-Timeseries_Subset(site.data.sp2)

#Pooling
survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

#This function will pool data at Sector scale according to predetermined groups. Protected reef slope is converted to Forereef here
site.data.sp2<-PoolSecStrat(site.data.sp2)
# rich.data<-PoolSecStrat(rich.data.gen)

#QC CHECK to make sure the sectors and strata pooled correctly
data.test<-ddply(subset(site.data.sp2,SPCODE=="SSSS"),.(REGION,BEN_SEC,OBS_YEAR,STRATANAME),summarize,n=length(SITE))
sm.test<-ddply(subset(survey_master,Benthic=="1"&EXCLUDE_FLAG=="0"&OBS_YEAR>=2013),.(REGION,ISLAND,SEC_NAME,OBS_YEAR,REEF_ZONE,DEPTH_BIN),summarize,n=length(SITE))

write.csv(data.test,"tmp_sitedataQC.csv")
write.csv(sm.test,"tmp_sitemasterQC.csv")

# #Subset just Forereef Sites & just target taxa
# site.data.sp2<-subset(site.data.sp2,REEF_ZONE=="Forereef")
# rich.data<-subset(rich.data,REEF_ZONE=="Forereef")

# #Make sure you everything but forereef are dropped
# table(site.data.sp2$REEF_ZONE,site.data.sp2$SPCODE)
# table(rich.data$REEF_ZONE)


#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
site.data.sp2$ANALYSIS_SCHEMA<-site.data.sp2$STRATANAME
site.data.sp2$DOMAIN_SCHEMA<-site.data.sp2$ISLAND


#Calculate metrics at Strata-level-We need to work on combining metrics into 1 function

#Create a vector of columns to subset for strata estimates
c.keep<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","SPCODE",
          "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep2<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","SPCODE",
           "n_h","N_h","D._h","SE_D._h")
c.keep3<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","SPCODE",
           "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep4<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","SPCODE",
           "n_h","N_h","prev","SEprev")

acdSP_st<-Calc_Strata(site.data.sp2,"SPCODE","AdColDen","Adpres.abs");acdSP_st=acdSP_st[,c.keep]
colnames(acdSP_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","SPCODE","n","Ntot","AdColDen","SE_AdColDen","Adult_avp","Adult_seprop","Adult_Abun","Adult_SE_Abun","Adult_CV")

jcdSP_st<-Calc_Strata(site.data.sp2,"SPCODE","JuvColDen","Juvpres.abs");jcdSP_st=jcdSP_st[,c.keep]
colnames(jcdSP_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","SPCODE","n","Ntot","JuvColDen","SE_JuvColDen","Juv_avp","Juv_seprop","Juv_Abun","Juv_SE_Abun","Juv_CV")

odSP_st<-Calc_Strata(site.data.sp2,"SPCODE","Ave.od");odSP_st=odSP_st[,c.keep2]
colnames(odSP_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","SPCODE","n","Ntot","Ave.od","SE_Ave.od")

rdSP_st<-Calc_Strata(site.data.sp2,"SPCODE","Ave.rd");rdSP_st=rdSP_st[,c.keep2]
colnames(rdSP_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","SPCODE","n","Ntot","Ave.rd","SE_Ave.rd")

clSP_st<-Calc_Strata(site.data.sp2,"SPCODE","Ave.size");clSP_st=clSP_st[,c.keep2]
colnames(clSP_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","SPCODE","n","Ntot","Ave.size","SE_Ave.size")

BLESP_st<-Calc_Strata_Prevalence(site.data.sp2,"SPCODE","BLE");BLESP_st=BLESP_st[,c.keep4]
colnames(BLESP_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","SPCODE","n","Ntot","BLE","SE_BLE")

AcuteDZSP_st<-Calc_Strata_Prevalence(site.data.sp2,"SPCODE","AcuteDZ");AcuteDZSP_st=AcuteDZSP_st[,c.keep4]
colnames(AcuteDZSP_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","SPCODE","n","Ntot","AcuteDZ","SE_AcuteDZ")

ChronicDZSP_st<-Calc_Strata_Prevalence(site.data.sp2,"SPCODE","ChronicDZ");ChronicDZSP_st=ChronicDZSP_st[,c.keep4]
colnames(ChronicDZSP_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","SPCODE","n","Ntot","ChronicDZ","SE_ChronicDZ")


#Double Check that revised pooling is adding up NH (total sites) correctly
View(acdSP_st)
View(sectors)


#Calculate Island Estimates
acdSP_is<-Calc_Domain(site.data.sp2,"SPCODE","AdColDen","Adpres.abs")
acdSP_is<-acdSP_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_AdColDen","SE_AdColDen","CV_D._st","Y._st","SE_Y._st")]
  colnames(acdSP_is)[colnames(acdSP_is)=="CV_D._st"]<-"Adult_CV"
  colnames(acdSP_is)[colnames(acdSP_is)=="Y._st"]<-"Adult_Abun"
  colnames(acdSP_is)[colnames(acdSP_is)=="SE_Y._st"]<-"Adult_SE_Abun"

jcdSP_is<-Calc_Domain(site.data.sp2,"SPCODE","JuvColDen","Juvpres.abs")
jcdSP_is<-jcdSP_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen","CV_D._st")]
  colnames(jcdSP_is)[colnames(jcdSP_is)=="CV_D._st"]<-"Juv_CV"
  colnames(jcdSP_is)[colnames(jcdSP_is)=="Y._st"]<-"Juv_Abun"
  colnames(jcdSP_is)[colnames(jcdSP_is)=="SE_Y._st"]<-"Juv_SE_Abun"

odSP_is<-Calc_Domain(site.data.sp2,"SPCODE","Ave.od")
odSP_is<-odSP_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_Ave.od","SE_Ave.od")]
rdSP_is<-Calc_Domain(site.data.sp2,"SPCODE","Ave.rd")
rdSP_is<-rdSP_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_Ave.rd","SE_Ave.rd")]
clSP_is<-Calc_Domain(site.data.sp2,"SPCODE","Ave.size")
clSP_is<-clSP_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_Ave.size","SE_Ave.size")]
bleSP_is<-Calc_Domain_Prevalence(site.data.sp2,"SPCODE","BLE")
bleSP_is<-bleSP_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_BLE_Prev","SE_BLE_Prev")]
AcuteDZSP_is<-Calc_Domain_Prevalence(site.data.sp2,"SPCODE","AcuteDZ")
AcuteDZSP_is<-AcuteDZSP_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_AcuteDZ_Prev","SE_AcuteDZ_Prev")]
ChronicDZSP_is<-Calc_Domain_Prevalence(site.data.sp2,"SPCODE","ChronicDZ")
ChronicDZSP_is<-ChronicDZSP_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_ChronicDZ_Prev","SE_ChronicDZ_Prev")]


#Calculate Sector Estimates
site.data.sp2$ANALYSIS_SCHEMA<-site.data.sp2$STRATANAME
site.data.sp2$DOMAIN_SCHEMA<-site.data.sp2$BEN_SEC


acdSP_sec<-Calc_Domain(site.data.sp2,"SPCODE","AdColDen","Adpres.abs")
acdSP_sec<-acdSP_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_AdColDen","SE_AdColDen","CV_D._st","Y._st","SE_Y._st")]
  colnames(acdSP_sec)[colnames(acdSP_sec)=="CV_D._st"]<-"Adult_CV"
  colnames(acdSP_sec)[colnames(acdSP_sec)=="Y._st"]<-"Adult_Abun"
  colnames(acdSP_sec)[colnames(acdSP_sec)=="SE_Y._st"]<-"Adult_SE_Abun"
jcdSP_sec<-Calc_Domain(site.data.sp2,"SPCODE","JuvColDen","Juvpres.abs")
jcdSP_sec<-jcdSP_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen","CV_D._st","Y._st","SE_Y._st")]
  colnames(jcdSP_sec)[colnames(jcdSP_sec)=="CV_D._st"]<-"Juv_CV"
  colnames(jcdSP_sec)[colnames(jcdSP_sec)=="Y._st"]<-"Juv_Abun"
  colnames(jcdSP_sec)[colnames(jcdSP_sec)=="SE_Y._st"]<-"Juv_SE_Abun"

odSP_sec<-Calc_Domain(site.data.sp2,"SPCODE","Ave.od")
odSP_sec<-odSP_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_Ave.od","SE_Ave.od")]
rdSP_sec<-Calc_Domain(site.data.sp2,"SPCODE","Ave.rd")
rdSP_sec<-rdSP_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_Ave.rd","SE_Ave.rd")]
clSP_sec<-Calc_Domain(site.data.sp2,"SPCODE","Ave.size")
clSP_sec<-clSP_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_Ave.size","SE_Ave.size")]
bleSP_sec<-Calc_Domain_Prevalence(site.data.sp2,"SPCODE","BLE")
bleSP_sec<-bleSP_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_BLE_Prev","SE_BLE_Prev")]
AcuteDZSP_sec<-Calc_Domain_Prevalence(site.data.sp2,"SPCODE","AcuteDZ")
AcuteDZSP_sec<-AcuteDZSP_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_AcuteDZ_Prev","SE_AcuteDZ_Prev")]
ChronicDZSP_sec<-Calc_Domain_Prevalence(site.data.sp2,"SPCODE","ChronicDZ")
ChronicDZSP_sec<-ChronicDZSP_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_ChronicDZ_Prev","SE_ChronicDZ_Prev")]


#Merge dataframes together
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","SPCODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
st.data.sp<-Reduce(MyMerge, list(acdSP_st,jcdSP_st,odSP_st,rdSP_st,clSP_st,BLESP_st,AcuteDZSP_st,ChronicDZSP_st))
colnames(st.data.sp)[colnames(st.data.sp)=="ANALYSIS_SCHEMA"]<-"Stratum"


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","SPCODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
is.data.sp<-Reduce(MyMerge, list(acdSP_is,jcdSP_is,odSP_is,rdSP_is,clSP_is,bleSP_is,AcuteDZSP_is,ChronicDZSP_is))
colnames(is.data.sp)[colnames(is.data.sp)=="DOMAIN_SCHEMA"]<-"ISLAND"


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","SPCODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
sec.data.sp<-Reduce(MyMerge, list(acdSP_sec,jcdSP_sec,odSP_sec,rdSP_sec,clSP_sec,bleSP_sec,AcuteDZSP_sec,ChronicDZSP_sec))
colnames(sec.data.sp)[colnames(sec.data.sp)=="DOMAIN_SCHEMA"]<-"Sector"

#REMOVE HOWLAND AND BAKER 2017 data- we only surveyed juveniles and coral cover at those islands in 2017
#Chose not to remove it until here in case you would like juvenile data from those locations and year.
# st.data.sp<-filter(st.data.sp,!(ISLAND %in% c("Howland","Baker") & st.data.sp$ANALYSIS_YEAR=="2017"))
# is.data.sp<-filter(is.data.sp,!(ISLAND %in% c("Howland","Baker") & is.data.sp$ANALYSIS_YEAR=="2017"))
# sec.data.sp<-filter(sec.data.sp,!(ISLAND %in% c("Howland","Baker") & sec.data.sp$ANALYSIS_YEAR=="2017"))


write.csv(st.data.sp,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicREA_STRATA_SPCODE_Timeseries.csv",row.names=F)
write.csv(is.data.sp,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicREA_ISLAND_SPCODE_Timeseries.csv",row.names=F)
write.csv(sec.data.sp,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicREA_SECTOR_SPCODE_Timeseries.csv",row.names=F)

