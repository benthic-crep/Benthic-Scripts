#This script was updated from the original REA Coral Demography_CalcPooled.R. 
#Updates:
#1. no longer using PoolSecStrat() to define pooling structure, now using look up table PacificNCRMP_Benthic_Sectors_Lookup_v4.csv
#This means that strata and sectors are pooled the same across all years. This is a divergence from previous years where sectors may have been pooled differently depending on sampling
#Pooling differently across years creates a lot of confusion and caveats that are difficult to convey to end users. Happy to discuss this more. 

#2. General clean up/updates to dplyr

rm(list=ls())

#LOAD LIBRARY FUNCTIONS ...
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

## LOAD benthic data
site.data.gen2<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS_updated.csv")
site.data.sp2<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_SPCODE_updated.csv")
site.data.tax2<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_TAXONCODE_updated.csv")


# site.data.gen2<-subset(site.data.gen2,ISLAND=="Rota")
# site.data.sp2<-subset(site.data.sp2,ISLAND=="Rota")
# site.data.tax2<-subset(site.data.tax2,ISLAND=="Rota")

# Remove special missions -------------------------------------------------

#Change all special missions to exclude flag =-1, right now they are 0. Then exclude these sites
levels(as.factor(site.data.gen2$MISSIONID))
site.data.gen2$EXCLUDE_FLAG<-ifelse(site.data.gen2$MISSIONID %in% c("MP1410","MP1512","MP1602","MP2006","MP2206"),-1,0) #I left SE1602 in (2016 Jarvis and Rose)
head(subset(site.data.gen2,EXCLUDE_FLAG==-1))

#Actually remove special missions.
site.data.gen2<-subset(site.data.gen2,EXCLUDE_FLAG==0);
# this dataframe should be empty
head(subset(site.data.gen2,EXCLUDE_FLAG==-1))

#Taxoncode
levels(site.data.tax2$MISSIONID)
site.data.tax2$EXCLUDE_FLAG<-ifelse(site.data.tax2$MISSIONID %in% c("MP1410","MP1512","MP1602","MP2006","MP2206"),-1,0) #I left SE1602 in (2016 Jarvis and Rose)
head(subset(site.data.tax2,EXCLUDE_FLAG==-1))

#Actually remove special missions.
site.data.tax2<-subset(site.data.tax2,EXCLUDE_FLAG==0);
# this dataframe should be empty
head(subset(site.data.tax2,EXCLUDE_FLAG==-1))

#Spcode
levels(site.data.sp2$MISSIONID)
site.data.sp2$EXCLUDE_FLAG<-ifelse(site.data.sp2$MISSIONID %in% c("MP1410","MP1512","MP1602","MP2006","MP2206"),-1,0) #I left SE1602 in (2016 Jarvis and Rose)
head(subset(site.data.sp2,EXCLUDE_FLAG==-1))

#Actually remove special missions.
site.data.sp2<-subset(site.data.sp2,EXCLUDE_FLAG==0);
# this dataframe should be empty
head(subset(site.data.sp2,EXCLUDE_FLAG==-1))


# POOLING DATA from Site to Strata and Domain at GENUS-level---------------------------------------------------
survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)
seclu<-read.csv("T:/Benthic/Data/Lookup Tables/PacificNCRMP_Benthic_Sectors_Lookup_v4.csv")
#seclu<-subset(seclu,select= -c(AREA_HA,NH))
#Old Pooling function- This function will pool data at Sector scale according to predetermined groups. Protected reef slope is converted to Forereef here
#site.data.gen2<-PoolSecStrat(site.data.gen2)

#Merge site data with Sector look up table. This table indicates how sectors should be pooled or not
site.data.gen2<-left_join(site.data.gen2,seclu)
head(site.data.gen2)


#Specific Changes to depth bin and reef zone
site.data.gen2<-site.data.gen2 %>% mutate(REEF_ZONE = if_else(REEF_ZONE %in% c("Protected Slope","Forereef"), "Forereef", as.character(REEF_ZONE))) #Convert PRS to FRF
site.data.gen2<-site.data.gen2 %>% mutate(DEPTH_BIN = if_else(ISLAND =="Kingman" & REEF_ZONE=="Lagoon", "ALL", as.character(DEPTH_BIN)))
site.data.gen2<-site.data.gen2 %>% mutate(DEPTH_BIN = if_else(ISLAND =="Rose" & REEF_ZONE=="Backreef", "ALL", as.character(DEPTH_BIN)))
site.data.gen2<-site.data.gen2 %>% mutate(REEF_ZONE = if_else(ISLAND =="Maug" & REEF_ZONE=="Lagoon", "Forereef", as.character(REEF_ZONE))) #Three sites were miss coded- fix in database

#Remove some sectors or strata that were poorly sampled
site.data.gen2 <- filter(site.data.gen2,!((SEC_NAME %in% c("GUA_ACHANG","GUA_PATI_POINT","GUA_PITI_BOMB","GUA_TUMON")& OBS_YEAR == "2017")))
site.data.gen2 <- filter(site.data.gen2,!((ISLAND =="Johnston" & REEF_ZONE=="Lagoon")))
site.data.gen2 <- filter(site.data.gen2,!((OBS_YEAR == "2018" & ISLAND=="Kingman" & REEF_ZONE=="Backreef")))

#Create Strataname
site.data.gen2$DB_RZ<-paste(substring(site.data.gen2$REEF_ZONE,1,1), substring(site.data.gen2$DEPTH_BIN,1,1), sep="")
site.data.gen2$STRATANAME=paste0(site.data.gen2$PooledSector_Demo_1,"_",site.data.gen2$DB_RZ)

#QC CHECK to make sure the sectors and strata pooled correctly
data.test<-ddply(subset(site.data.gen2,GENUS_CODE=="SSSS"),.(REGION,PooledSector_Demo_1,OBS_YEAR,STRATANAME),summarize,n=length(SITE))
sm.test<-ddply(subset(survey_master,Benthic=="1"&EXCLUDE_FLAG=="0"&OBS_YEAR>=2013),.(REGION,ISLAND,SEC_NAME,OBS_YEAR,REEF_ZONE,DEPTH_BIN),summarize,n=length(SITE))

write.csv(data.test,"tmp_sitedataQC.csv")
write.csv(sm.test,"tmp_sitemasterQC.csv")



# Calculate metrics at Strata-level ---------------------------------------------------

#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
site.data.gen2$ANALYSIS_SCHEMA<-site.data.gen2$STRATANAME
site.data.gen2$DOMAIN_SCHEMA<-site.data.gen2$PooledSector_Demo_1

#Create a vector of columns to subset for strata estimates
c.keep<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
          "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep2<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
           "n_h","N_h","D._h","SE_D._h")
c.keep3<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
           "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep4<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
           "n_h","N_h","prev","SEprev")

acdG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","AdColDen","Adpres.abs");acdG_st=acdG_st[,c.keep]
colnames(acdG_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","AdColDen","SE_AdColDen","Adult_avp","Adult_seprop","Adult_Abun","Adult_SE_Abun","Adult_CV")

jcdG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","JuvColDen","Juvpres.abs");jcdG_st=jcdG_st[,c.keep]
colnames(jcdG_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","JuvColDen","SE_JuvColDen","Juv_avp","Juv_seprop","Juv_Abun","Juv_SE_Abun","Juv_CV")

odG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.od");odG_st=odG_st[,c.keep2]
colnames(odG_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.od","SE_Ave.od")

rdG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.rd");rdG_st=rdG_st[,c.keep2]
colnames(rdG_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.rd","SE_Ave.rd")

clG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.size");clG_st=clG_st[,c.keep2]
colnames(clG_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.size","SE_Ave.size")

BLEG_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","BLE");BLEG_st=BLEG_st[,c.keep4]
colnames(BLEG_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Mean_BLE_Prev","SE_BLE_Prev")

TotDZG_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","TotDZ");TotDZG_st=TotDZG_st[,c.keep4]
colnames(TotDZG_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Mean_TotDZ_Prev","SE_TotDZ_Prev")

AcuteDZG_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","AcuteDZ");AcuteDZG_st=AcuteDZG_st[,c.keep4]
colnames(AcuteDZG_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Mean_AcuteDZ_Prev","SE_AcuteDZ_Prev")

ChronicDZG_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","ChronicDZ");ChronicDZG_st=ChronicDZG_st[,c.keep4]
colnames(ChronicDZG_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Mean_ChronicDZ_Prev","SE_ChronicDZ_Prev")


#Double Check that revised pooling is adding up NH (total sites) correctly
View(acdG_st)
View(sectors)


# Calculate metrics at Island-level ---------------------------------------------------


#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
site.data.gen2$ANALYSIS_SCHEMA<-site.data.gen2$STRATANAME
site.data.gen2$DOMAIN_SCHEMA<-site.data.gen2$ISLAND

acdG_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","AdColDen","Adpres.abs")
acdG_is<-acdG_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AdColDen","SE_AdColDen","CV_D._st","Y._st","SE_Y._st")]
colnames(acdG_is)[colnames(acdG_is)=="CV_D._st"]<-"Adult_CV"
colnames(acdG_is)[colnames(acdG_is)=="Y._st"]<-"Adult_Abun"
colnames(acdG_is)[colnames(acdG_is)=="SE_Y._st"]<-"Adult_SE_Abun"

jcdG_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","JuvColDen","Juvpres.abs")
jcdG_is<-jcdG_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen","CV_D._st")]
colnames(jcdG_is)[colnames(jcdG_is)=="CV_D._st"]<-"Juv_CV"
colnames(jcdG_is)[colnames(jcdG_is)=="Y._st"]<-"Juv_Abun"
colnames(jcdG_is)[colnames(jcdG_is)=="SE_Y._st"]<-"Juv_SE_Abun"

odG_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.od")
odG_is<-odG_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.od","SE_Ave.od")]
rdG_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.rd")
rdG_is<-rdG_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.rd","SE_Ave.rd")]
clG_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.size")
clG_is<-clG_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.size","SE_Ave.size")]
bleG_is<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","BLE")
bleG_is<-bleG_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_BLE_Prev","SE_BLE_Prev")]

TotDZG_is<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","TotDZ")
TotDZG_is<-TotDZG_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_TotDZ_Prev","SE_TotDZ_Prev")]
AcuteDZG_is<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","AcuteDZ")
AcuteDZG_is<-AcuteDZG_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AcuteDZ_Prev","SE_AcuteDZ_Prev")]
ChronicDZG_is<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","ChronicDZ")
ChronicDZG_is<-ChronicDZG_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_ChronicDZ_Prev","SE_ChronicDZ_Prev")]


# Calculate metrics at Sector-level ---------------------------------------------------
site.data.gen2$ANALYSIS_SCHEMA<-site.data.gen2$STRATANAME
site.data.gen2$DOMAIN_SCHEMA<-site.data.gen2$PooledSector_Demo_1


acdG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","AdColDen","Adpres.abs")
acdG_sec<-acdG_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AdColDen","SE_AdColDen","CV_D._st","Y._st","SE_Y._st")]
  colnames(acdG_sec)[colnames(acdG_sec)=="CV_D._st"]<-"Adult_CV"
  colnames(acdG_sec)[colnames(acdG_sec)=="Y._st"]<-"Adult_Abun"
  colnames(acdG_sec)[colnames(acdG_sec)=="SE_Y._st"]<-"Adult_SE_Abun"
jcdG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","JuvColDen","Juvpres.abs")
jcdG_sec<-jcdG_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen","CV_D._st","Y._st","SE_Y._st")]
  colnames(jcdG_sec)[colnames(jcdG_sec)=="CV_D._st"]<-"Juv_CV"
  colnames(jcdG_sec)[colnames(jcdG_sec)=="Y._st"]<-"Juv_Abun"
  colnames(jcdG_sec)[colnames(jcdG_sec)=="SE_Y._st"]<-"Juv_SE_Abun"
odG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.od")
odG_sec<-odG_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.od","SE_Ave.od")]
rdG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.rd")
rdG_sec<-rdG_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.rd","SE_Ave.rd")]
clG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.size")
clG_sec<-clG_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.size","SE_Ave.size")]
bleG_sec<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","BLE")
bleG_sec<-bleG_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_BLE_Prev","SE_BLE_Prev")]

TotDZG_sec<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","TotDZ")
TotDZG_sec<-TotDZG_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_TotDZ_Prev","SE_TotDZ_Prev")]
AcuteDZG_sec<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","AcuteDZ")
AcuteDZG_sec<-AcuteDZG_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AcuteDZ_Prev","SE_AcuteDZ_Prev")]
ChronicDZG_sec<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","ChronicDZ")
ChronicDZG_sec<-ChronicDZG_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_ChronicDZ_Prev","SE_ChronicDZ_Prev")]


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
st.data.gen<-Reduce(MyMerge, list(acdG_st,jcdG_st,odG_st,rdG_st,clG_st,BLEG_st,TotDZG_st,AcuteDZG_st,ChronicDZG_st))
#colnames(st.data.gen)[colnames(st.data.gen)=="ANALYSIS_SCHEMA"]<-"Stratum"


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
is.data.gen<-Reduce(MyMerge, list(acdG_is,jcdG_is,odG_is,rdG_is,clG_is,bleG_is,TotDZG_is,AcuteDZG_is,ChronicDZG_is))
colnames(is.data.gen)[colnames(is.data.gen)=="DOMAIN_SCHEMA"]<-"Island"



MyMerge <- function(x, y){
  df <- merge(x, y, by= c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
sec.data.gen<-Reduce(MyMerge, list(acdG_sec,jcdG_sec,odG_sec,rdG_sec,clG_sec,bleG_sec,TotDZG_sec,AcuteDZG_sec,ChronicDZG_sec))
colnames(sec.data.gen)[colnames(sec.data.gen)=="DOMAIN_SCHEMA"]<-"Sector"

write.csv(st.data.gen,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicREA_stratadata_GENUS_updated.csv")
write.csv(is.data.gen,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicREA_islanddata_GENUS_updated.csv")
write.csv(sec.data.gen,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicREA_sectordata_GENUS_updated.csv")


# POOLING DATA from Site to Strata and Domain at SPCODE-level---------------------------------------------------
survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

#This function will pool data at Sector scale according to predetermined groups. Protected reef slope is converted to Forereef here
#site.data.sp2<-PoolSecStrat(site.data.sp2)
# rich.data<-PoolSecStrat(rich.data.gen)


#Merge site data with Sector look up table. This table indicates how sectors should be pooled or not

#Merge site data with Sector look up table. This table indicates how sectors should be pooled or not
site.data.sp2<-left_join(site.data.sp2,seclu)
head(site.data.sp2)


#Specific Changes to depth bin and reef zone
site.data.sp2<-site.data.sp2 %>% mutate(REEF_ZONE = if_else(REEF_ZONE %in% c("Protected Slope","Forereef"), "Forereef", as.character(REEF_ZONE))) #Convert PRS to FRF
site.data.sp2<-site.data.sp2 %>% mutate(DEPTH_BIN = if_else(ISLAND =="Kingman" & REEF_ZONE=="Lagoon", "ALL", as.character(DEPTH_BIN)))
site.data.sp2<-site.data.sp2 %>% mutate(DEPTH_BIN = if_else(ISLAND =="Rose" & REEF_ZONE=="Backreef", "ALL", as.character(DEPTH_BIN)))
site.data.sp2<-site.data.sp2 %>% mutate(REEF_ZONE = if_else(ISLAND =="Maug" & REEF_ZONE=="Lagoon", "Forereef", as.character(REEF_ZONE))) #Three sites were miss coded- fix in database

#Remove some sectors or strata that were poorly sampled
site.data.sp2 <- filter(site.data.sp2,!((SEC_NAME %in% c("GUA_ACHANG","GUA_PATI_POINT","GUA_PITI_BOMB","GUA_TUMON")& OBS_YEAR == "2017")))
site.data.sp2 <- filter(site.data.sp2,!((ISLAND =="Johnston" & REEF_ZONE=="Lagoon")))
site.data.sp2 <- filter(site.data.sp2,!((OBS_YEAR == "2018" & ISLAND=="Kingman" & REEF_ZONE=="Backreef")))

#Create Strataname
site.data.sp2$DB_RZ<-paste(substring(site.data.sp2$REEF_ZONE,1,1), substring(site.data.sp2$DEPTH_BIN,1,1), sep="")
site.data.sp2$STRATANAME=paste0(site.data.sp2$PooledSector_Demo_1,"_",site.data.sp2$DB_RZ)

#QC CHECK to make sure the sectors and strata pooled correctly
data.test<-ddply(subset(site.data.sp2,GENUS_CODE=="SSSS"),.(REGION,PooledSector_Demo_1,OBS_YEAR,STRATANAME),summarize,n=length(SITE))
sm.test<-ddply(subset(survey_master,Benthic=="1"&EXCLUDE_FLAG=="0"&OBS_YEAR>=2013),.(REGION,ISLAND,SEC_NAME,OBS_YEAR,REEF_ZONE,DEPTH_BIN),summarize,n=length(SITE))

write.csv(data.test,"tmp_sitedataQC.csv")
write.csv(sm.test,"tmp_sitemasterQC.csv")


#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
site.data.sp2$ANALYSIS_SCHEMA<-site.data.sp2$STRATANAME
site.data.sp2$DOMAIN_SCHEMA<-site.data.sp2$PooledSector_Demo_1


#Calculate metrics at Strata-level-We need to work on combining metrics into 1 function

#Create a vector of columns to subset for strata estimates
c.keep<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","SPCODE",
          "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep2<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","SPCODE",
           "n_h","N_h","D._h","SE_D._h")
c.keep3<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","SPCODE",
           "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep4<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","SPCODE",
           "n_h","N_h","prev","SEprev")

acdSP_st<-Calc_Strata(site.data.sp2,"SPCODE","AdColDen","Adpres.abs");acdSP_st=acdSP_st[,c.keep]
colnames(acdSP_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","SPCODE","n","Ntot","AdColDen","SE_AdColDen","Adult_avp","Adult_seprop","Adult_Abun","Adult_SE_Abun","Adult_CV")

jcdSP_st<-Calc_Strata(site.data.sp2,"SPCODE","JuvColDen","Juvpres.abs");jcdSP_st=jcdSP_st[,c.keep]
colnames(jcdSP_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","SPCODE","n","Ntot","JuvColDen","SE_JuvColDen","Juv_avp","Juv_seprop","Juv_Abun","Juv_SE_Abun","Juv_CV")

odSP_st<-Calc_Strata(site.data.sp2,"SPCODE","Ave.od");odSP_st=odSP_st[,c.keep2]
colnames(odSP_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","SPCODE","n","Ntot","Ave.od","SE_Ave.od")

rdSP_st<-Calc_Strata(site.data.sp2,"SPCODE","Ave.rd");rdSP_st=rdSP_st[,c.keep2]
colnames(rdSP_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","SPCODE","n","Ntot","Ave.rd","SE_Ave.rd")

clSP_st<-Calc_Strata(site.data.sp2,"SPCODE","Ave.size");clSP_st=clSP_st[,c.keep2]
colnames(clSP_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","SPCODE","n","Ntot","Ave.size","SE_Ave.size")

BLESP_st<-Calc_Strata_Prevalence(site.data.sp2,"SPCODE","BLE");BLESP_st=BLESP_st[,c.keep4]
colnames(BLESP_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","SPCODE","n","Ntot","Mean_BLE_Prev","SE_BLE_Prev")

TotDZSP_st<-Calc_Strata_Prevalence(site.data.sp2,"SPCODE","TotDZ");TotDZSP_st=TotDZSP_st[,c.keep4]
colnames(TotDZSP_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","SPCODE","n","Ntot","Mean_TotDZ_Prev","SE_TotDZ_Prev")

AcuteDZSP_st<-Calc_Strata_Prevalence(site.data.sp2,"SPCODE","AcuteDZ");AcuteDZSP_st=AcuteDZSP_st[,c.keep4]
colnames(AcuteDZSP_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","SPCODE","n","Ntot","Mean_AcuteDZ_Prev","SE_AcuteDZ_Prev")

ChronicDZSP_st<-Calc_Strata_Prevalence(site.data.sp2,"SPCODE","ChronicDZ");ChronicDZSP_st=ChronicDZSP_st[,c.keep4]
colnames(ChronicDZSP_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","SPCODE","n","Ntot","Mean_ChronicDZ_Prev","SE_ChronicDZ_Prev")


#Double Check that revised pooling is adding up NH (total sites) correctly
View(acdSP_st)
View(sectors)


#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
site.data.sp2$ANALYSIS_SCHEMA<-site.data.sp2$STRATANAME
site.data.sp2$DOMAIN_SCHEMA<-site.data.sp2$ISLAND


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

TotDZSP_is<-Calc_Domain_Prevalence(site.data.sp2,"SPCODE","TotDZ")
TotDZSP_is<-TotDZSP_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_TotDZ_Prev","SE_TotDZ_Prev")]
AcuteDZSP_is<-Calc_Domain_Prevalence(site.data.sp2,"SPCODE","AcuteDZ")
AcuteDZSP_is<-AcuteDZSP_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_AcuteDZ_Prev","SE_AcuteDZ_Prev")]
ChronicDZSP_is<-Calc_Domain_Prevalence(site.data.sp2,"SPCODE","ChronicDZ")
ChronicDZSP_is<-ChronicDZSP_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_ChronicDZ_Prev","SE_ChronicDZ_Prev")]


#Calculate Sector Estimates
site.data.sp2$ANALYSIS_SCHEMA<-site.data.sp2$STRATANAME
site.data.sp2$DOMAIN_SCHEMA<-site.data.sp2$PooledSector


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

TotDZSP_sec<-Calc_Domain_Prevalence(site.data.sp2,"SPCODE","TotDZ")
TotDZSP_sec<-TotDZSP_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_TotDZ_Prev","SE_TotDZ_Prev")]
AcuteDZSP_sec<-Calc_Domain_Prevalence(site.data.sp2,"SPCODE","AcuteDZ")
AcuteDZSP_sec<-AcuteDZSP_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_AcuteDZ_Prev","SE_AcuteDZ_Prev")]
ChronicDZSP_sec<-Calc_Domain_Prevalence(site.data.sp2,"SPCODE","ChronicDZ")
ChronicDZSP_sec<-ChronicDZSP_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","SPCODE","n","Ntot","Mean_ChronicDZ_Prev","SE_ChronicDZ_Prev")]


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","SPCODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
st.data.sp<-Reduce(MyMerge, list(acdSP_st,jcdSP_st,odSP_st,rdSP_st,clSP_st,BLESP_st,TotDZSP_st,AcuteDZSP_st,ChronicDZSP_st))
#colnames(st.data.sp)[colnames(st.data.sp)=="ANALYSIS_SCHEMA"]<-"Stratum"


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","SPCODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
is.data.sp<-Reduce(MyMerge, list(acdSP_is,jcdSP_is,odSP_is,rdSP_is,clSP_is,bleSP_is,TotDZSP_is,AcuteDZSP_is,ChronicDZSP_is))
colnames(is.data.sp)[colnames(is.data.sp)=="DOMAIN_SCHEMA"]<-"Island"



MyMerge <- function(x, y){
  df <- merge(x, y, by= c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","SPCODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
sec.data.sp<-Reduce(MyMerge, list(acdSP_sec,jcdSP_sec,odSP_sec,rdSP_sec,clSP_sec,bleSP_sec,TotDZSP_sec,AcuteDZSP_sec,ChronicDZSP_sec))
colnames(sec.data.sp)[colnames(sec.data.sp)=="DOMAIN_SCHEMA"]<-"Sector"

write.csv(st.data.sp,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicREA_stratadata_SPCODE_updated.csv")
write.csv(is.data.sp,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicREA_islanddata_SPCODE_updated.csv")
write.csv(sec.data.sp,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicREA_sectordata_SPCODE_updated.csv")


# POOLING DATA from Site to Strata and Domain at TAXONCODE-level---------------------------------------------------
#Merge site data with Sector look up table. This table indicates how sectors should be pooled or not
#For NCRMP viztool data- Keep pooling scheme the same across years
#Merge site data with Sector look up table. This table indicates how sectors should be pooled or not
site.data.tax2<-left_join(site.data.tax2,seclu)
head(site.data.tax2)


#Specific Changes to depth bin and reef zone
site.data.tax2<-site.data.tax2 %>% mutate(REEF_ZONE = if_else(REEF_ZONE %in% c("Protected Slope","Forereef"), "Forereef", as.character(REEF_ZONE))) #Convert PRS to FRF
site.data.tax2<-site.data.tax2 %>% mutate(DEPTH_BIN = if_else(ISLAND =="Kingman" & REEF_ZONE=="Lagoon", "ALL", as.character(DEPTH_BIN)))
site.data.tax2<-site.data.tax2 %>% mutate(DEPTH_BIN = if_else(ISLAND =="Rose" & REEF_ZONE=="Backreef", "ALL", as.character(DEPTH_BIN)))
site.data.tax2<-site.data.tax2 %>% mutate(REEF_ZONE = if_else(ISLAND =="Maug" & REEF_ZONE=="Lagoon", "Forereef", as.character(REEF_ZONE))) #Three sites were miss coded- fix in database

#Remove some sectors or strata that were poorly sampled
site.data.tax2 <- filter(site.data.tax2,!((SEC_NAME %in% c("GUA_ACHANG","GUA_PATI_POINT","GUA_PITI_BOMB","GUA_TUMON")& OBS_YEAR == "2017")))
site.data.tax2 <- filter(site.data.tax2,!((ISLAND =="Johnston" & REEF_ZONE=="Lagoon")))
site.data.tax2 <- filter(site.data.tax2,!((OBS_YEAR == "2018" & ISLAND=="Kingman" & REEF_ZONE=="Backreef")))

#Create Strataname
site.data.tax2$DB_RZ<-paste(substring(site.data.tax2$REEF_ZONE,1,1), substring(site.data.tax2$DEPTH_BIN,1,1), sep="")
site.data.tax2$STRATANAME=paste0(site.data.tax2$PooledSector_Demo_1,"_",site.data.tax2$DB_RZ)

#QC CHECK to make sure the sectors and strata pooled correctly
data.test<-ddply(subset(site.data.tax2,GENUS_CODE=="SSSS"),.(REGION,PooledSector_Demo_1,OBS_YEAR,STRATANAME),summarize,n=length(SITE))
sm.test<-ddply(subset(survey_master,Benthic=="1"&EXCLUDE_FLAG=="0"&OBS_YEAR>=2013),.(REGION,ISLAND,SEC_NAME,OBS_YEAR,REEF_ZONE,DEPTH_BIN),summarize,n=length(SITE))

#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
site.data.tax2$ANALYSIS_SCHEMA<-site.data.tax2$STRATANAME
site.data.tax2$DOMAIN_SCHEMA<-site.data.tax2$PooledSector


#Calculate metrics at Strata-level-We need to work on combining metrics into 1 function

#Create a vector of columns to subset for strata estimates
c.keep<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","TAXONCODE",
          "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep2<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","TAXONCODE",
           "n_h","N_h","D._h","SE_D._h")
c.keep3<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","TAXONCODE",
           "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep4<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","TAXONCODE",
           "n_h","N_h","prev","SEprev")

acdTAX_st<-Calc_Strata(site.data.tax2,"TAXONCODE","AdColDen","Adpres.abs");acdTAX_st=acdTAX_st[,c.keep]
colnames(acdTAX_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","TAXONCODE","n","Ntot","AdColDen","SE_AdColDen","Adult_avp","Adult_seprop","Adult_Abun","Adult_SE_Abun","Adult_CV")

jcdTAX_st<-Calc_Strata(site.data.tax2,"TAXONCODE","JuvColDen","Juvpres.abs");jcdTAX_st=jcdTAX_st[,c.keep]
colnames(jcdTAX_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","TAXONCODE","n","Ntot","JuvColDen","SE_JuvColDen","Juv_avp","Juv_seprop","Juv_Abun","Juv_SE_Abun","Juv_CV")

odTAX_st<-Calc_Strata(site.data.tax2,"TAXONCODE","Ave.od");odTAX_st=odTAX_st[,c.keep2]
colnames(odTAX_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","TAXONCODE","n","Ntot","Ave.od","SE_Ave.od")

rdTAX_st<-Calc_Strata(site.data.tax2,"TAXONCODE","Ave.rd");rdTAX_st=rdTAX_st[,c.keep2]
colnames(rdTAX_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","TAXONCODE","n","Ntot","Ave.rd","SE_Ave.rd")

clTAX_st<-Calc_Strata(site.data.tax2,"TAXONCODE","Ave.size");clTAX_st=clTAX_st[,c.keep2]
colnames(clTAX_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","TAXONCODE","n","Ntot","Ave.size","SE_Ave.size")

BLETAX_st<-Calc_Strata_Prevalence(site.data.tax2,"TAXONCODE","BLE");BLETAX_st=BLETAX_st[,c.keep4]
colnames(BLETAX_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","TAXONCODE","n","Ntot","Mean_BLE_Prev","SE_BLE_Prev")

TotDZTAX_st<-Calc_Strata_Prevalence(site.data.tax2,"TAXONCODE","TotDZ");TotDZTAX_st=TotDZTAX_st[,c.keep4]
colnames(TotDZTAX_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","TAXONCODE","n","Ntot","Mean_TotDZ_Prev","SE_TotDZ_Prev")

AcuteDZTAX_st<-Calc_Strata_Prevalence(site.data.tax2,"TAXONCODE","AcuteDZ");AcuteDZTAX_st=AcuteDZTAX_st[,c.keep4]
colnames(AcuteDZTAX_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","TAXONCODE","n","Ntot","Mean_AcuteDZ_Prev","SE_AcuteDZ_Prev")

ChronicDZTAX_st<-Calc_Strata_Prevalence(site.data.tax2,"TAXONCODE","ChronicDZ");ChronicDZTAX_st=ChronicDZTAX_st[,c.keep4]
colnames(ChronicDZTAX_st)<-c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","TAXONCODE","n","Ntot","Mean_ChronicDZ_Prev","SE_ChronicDZ_Prev")


#Double Check that revised pooling is adding up NH (total sites) correctly
View(acdTAX_st)
View(sectors)


#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
site.data.tax2$ANALYSIS_SCHEMA<-site.data.tax2$STRATANAME
site.data.tax2$DOMAIN_SCHEMA<-site.data.tax2$ISLAND


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

TotDZTAX_is<-Calc_Domain_Prevalence(site.data.tax2,"TAXONCODE","TotDZ")
TotDZTAX_is<-TotDZTAX_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_TotDZ_Prev","SE_TotDZ_Prev")]
AcuteDZTAX_is<-Calc_Domain_Prevalence(site.data.tax2,"TAXONCODE","AcuteDZ")
AcuteDZTAX_is<-AcuteDZTAX_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_AcuteDZ_Prev","SE_AcuteDZ_Prev")]
ChronicDZTAX_is<-Calc_Domain_Prevalence(site.data.tax2,"TAXONCODE","ChronicDZ")
ChronicDZTAX_is<-ChronicDZTAX_is[,c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_ChronicDZ_Prev","SE_ChronicDZ_Prev")]


#Calculate Sector Estimates
site.data.tax2$ANALYSIS_SCHEMA<-site.data.tax2$STRATANAME
site.data.tax2$DOMAIN_SCHEMA<-site.data.tax2$PooledSector


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

TotDZTAX_sec<-Calc_Domain_Prevalence(site.data.tax2,"TAXONCODE","TotDZ")
TotDZTAX_sec<-TotDZTAX_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_TotDZ_Prev","SE_TotDZ_Prev")]
AcuteDZTAX_sec<-Calc_Domain_Prevalence(site.data.tax2,"TAXONCODE","AcuteDZ")
AcuteDZTAX_sec<-AcuteDZTAX_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_AcuteDZ_Prev","SE_AcuteDZ_Prev")]
ChronicDZTAX_sec<-Calc_Domain_Prevalence(site.data.tax2,"TAXONCODE","ChronicDZ")
ChronicDZTAX_sec<-ChronicDZTAX_sec[,c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","TAXONCODE","n","Ntot","Mean_ChronicDZ_Prev","SE_ChronicDZ_Prev")]




MyMerge <- function(x, y){
  df <- merge(x, y, by= c("METHOD","REGION","ISLAND","ANALYSIS_YEAR","SECTOR","Stratum","REEF_ZONE","DB_RZ","TAXONCODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
st.data.tax<-Reduce(MyMerge, list(acdTAX_st,jcdTAX_st,odTAX_st,rdTAX_st,clTAX_st,BLETAX_st,TotDZTAX_st,AcuteDZTAX_st,ChronicDZTAX_st))
#colnames(st.data.tax)[colnames(st.data.tax)=="ANALYSIS_SCHEMA"]<-"Stratum"


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("METHOD","REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","TAXONCODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
is.data.tax<-Reduce(MyMerge, list(acdTAX_is,jcdTAX_is,odTAX_is,rdTAX_is,clTAX_is,bleTAX_is,TotDZTAX_is,AcuteDZTAX_is,ChronicDZTAX_is))
colnames(is.data.tax)[colnames(is.data.tax)=="DOMAIN_SCHEMA"]<-"Island"



MyMerge <- function(x, y){
  df <- merge(x, y, by= c("METHOD","REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","TAXONCODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
sec.data.tax<-Reduce(MyMerge, list(acdTAX_sec,jcdTAX_sec,odTAX_sec,rdTAX_sec,clTAX_sec,bleTAX_sec,TotDZTAX_sec,AcuteDZTAX_sec,ChronicDZTAX_sec))
colnames(sec.data.tax)[colnames(sec.data.tax)=="DOMAIN_SCHEMA"]<-"Sector"

write.csv(st.data.tax,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicREA_stratadata_TAXONCODE_updated.csv")
write.csv(is.data.tax,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicREA_islanddata_TAXONCODE_updated.csv")
write.csv(sec.data.tax,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicREA_sectordata_TAXONCODE_updated.csv")

#Things to work on
#1. put pooling changes into csv file rather than write them out in text, too clunky and easy to get confused with different years
#2. Separate rz/db from sector name so have a column for sector and stratum- this will allow us to subset just certain depths and zone more easily later on.
#3. Make sure that ntot added up correctly across years and domains

