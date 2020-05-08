

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


#Change all special missions to exclude flag =-1, right now they are 0. Then exclude these sites
levels(site.data.gen2$MISSIONID)
site.data.gen2$EXCLUDE_FLAG<-ifelse(site.data.gen2$MISSIONID %in% c("MP1410","MP1512","MP1602","MP2006"),-1,0) #I left SE1602 in (2016 Jarvis and Rose)
head(subset(site.data.gen2,EXCLUDE_FLAG==-1))

#Actually remove special missions.
site.data.gen2<-subset(site.data.gen2,EXCLUDE_FLAG==0);
# this dataframe should be empty
head(subset(site.data.gen2,EXCLUDE_FLAG==-1))

#Taxoncode
levels(site.data.tax2$MISSIONID)
site.data.tax2$EXCLUDE_FLAG<-ifelse(site.data.tax2$MISSIONID %in% c("MP1410","MP1512","MP1602","MP2006"),-1,0) #I left SE1602 in (2016 Jarvis and Rose)
head(subset(site.data.tax2,EXCLUDE_FLAG==-1))

#Actually remove special missions.
site.data.tax2<-subset(site.data.tax2,EXCLUDE_FLAG==0);
# this dataframe should be empty
head(subset(site.data.tax2,EXCLUDE_FLAG==-1))

#Spcode
levels(site.data.sp2$MISSIONID)
site.data.sp2$EXCLUDE_FLAG<-ifelse(site.data.sp2$MISSIONID %in% c("MP1410","MP1512","MP1602","MP2006"),-1,0) #I left SE1602 in (2016 Jarvis and Rose)
head(subset(site.data.sp2,EXCLUDE_FLAG==-1))

#Actually remove special missions.
site.data.sp2<-subset(site.data.sp2,EXCLUDE_FLAG==0);
# this dataframe should be empty
head(subset(site.data.sp2,EXCLUDE_FLAG==-1))


# POOLING DATA from Site to Strata and Domain---------------------------------------------------
survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)
site.data.gen2<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS.csv")

#This function will pool data according to predetermined groups. Protected reef slope is converted to Forereef here
site.data.gen2<-PoolSecStrat(site.data.gen2)
rich.data<-PoolSecStrat(rich.data.gen)

#QC CHECK to make sure the sectors and strata pooled correctly
rich.test<-ddply(rich.data,.(REGION,BEN_SEC,OBS_YEAR,STRATANAME),summarize,n=length(SITE))
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
rich.data$ANALYSIS_SCHEMA<-rich.data$STRATANAME
rich.data$DOMAIN_SCHEMA<-rich.data$ISLAND


#Calculate metrics at Strata-level-We need to work on combining metrics into 1 function

#Create a vector of columns to subset for strata estimates
c.keep<-c("REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
          "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep2<-c("REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
           "n_h","N_h","D._h","SE_D._h")
c.keep3<-c("REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
           "n_h","N_h","D._h","SE_D._h","avp","SEprop","Y._h","SE_Y._h","CV_Y._h")
c.keep4<-c("REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
           "n_h","N_h","prev","SEprev")

acdG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","AdColDen","Adpres.abs");acdG_st=acdG_st[,c.keep]
colnames(acdG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","AdColDen","SE_AdColDen","Adult_avp","Adult_seprop","Adult_Abun","Adult_SE_Abun","Adult_CV")

jcdG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","JuvColDen","Juvpres.abs");jcdG_st=jcdG_st[,c.keep]
colnames(jcdG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","JuvColDen","SE_JuvColDen","Juv_avp","Juv_seprop","Juv_Abun","Juv_SE_Abun","Juv_CV")

odG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.od");odG_st=odG_st[,c.keep2]
colnames(odG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.od","SE_Ave.od")

rdG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.rd");rdG_st=rdG_st[,c.keep2]
colnames(rdG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.rd","SE_Ave.rd")

clG_st<-Calc_Strata(site.data.gen2,"GENUS_CODE","Ave.size");clG_st=clG_st[,c.keep2]
colnames(clG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","Ave.size","SE_Ave.size")

BLEG_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","BLE");BLEG_st=BLEG_st[,c.keep4]
colnames(BLEG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","BLE","SE_BLE")

AcuteDZG_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","AcuteDZ");AcuteDZG_st=AcuteDZG_st[,c.keep4]
colnames(AcuteDZG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","AcuteDZ","SE_AcuteDZ")

ChronicDZG_st<-Calc_Strata_Prevalence(site.data.gen2,"GENUS_CODE","ChronicDZ");ChronicDZG_st=ChronicDZG_st[,c.keep4]
colnames(ChronicDZG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","ChronicDZ","SE_ChronicDZ")


# c.keep<-c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","n_h","N_h","D._h","SE_D._h")
# rich_st<-Calc_Strata_Cover_Rich(rich.data,"Richness");rich_st=rich_st[,c.keep]
# colnames(rich_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Sector","REEF_ZONE","DB_RZ","Stratum","n","Ntot","Richness","SE_Richess") 


#Double Check that revised pooling is adding up NH (total sites) correctly
View(acdG_st)
View(sectors)


#Calculate Island Estimates
acdG_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","AdColDen","Adpres.abs")
acdG_is<-acdG_is[,c("REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AdColDen","SE_AdColDen")]
jcdG_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","JuvColDen","Juvpres.abs")
jcdG_is<-jcdG_is[,c("REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen")]
odG_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.od")
odG_is<-odG_is[,c("REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.od","SE_Ave.od")]
rdG_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.rd")
rdG_is<-rdG_is[,c("REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.rd","SE_Ave.rd")]
clG_is<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.size")
clG_is<-clG_is[,c("REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.size","SE_Ave.size")]
bleG_is<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","BLE")
bleG_is<-bleG_is[,c("REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_BLE_Prev","SE_BLE_Prev")]
AcuteDZG_is<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","AcuteDZ")
AcuteDZG_is<-AcuteDZG_is[,c("REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AcuteDZ_Prev","SE_AcuteDZ_Prev")]
ChronicDZG_is<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","ChronicDZ")
ChronicDZG_is<-ChronicDZG_is[,c("REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_ChronicDZ_Prev","SE_ChronicDZ_Prev")]


#Calculate Sector Estimates
site.data.gen2$ANALYSIS_SCHEMA<-site.data.gen2$STRATANAME
site.data.gen2$DOMAIN_SCHEMA<-site.data.gen2$BEN_SEC
rich.data$ANALYSIS_SCHEMA<-rich.data$STRATANAME
rich.data$DOMAIN_SCHEMA<-rich.data$BEN_SEC

acdG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","AdColDen","Adpres.abs")
acdG_sec<-acdG_sec[,c("REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AdColDen","SE_AdColDen")]
jcdG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","JuvColDen","Juvpres.abs")
jcdG_sec<-jcdG_sec[,c("REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen")]
odG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.od")
odG_sec<-odG_sec[,c("REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.od","SE_Ave.od")]
rdG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.rd")
rdG_sec<-rdG_sec[,c("REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.rd","SE_Ave.rd")]
clG_sec<-Calc_Domain(site.data.gen2,"GENUS_CODE","Ave.size")
clG_sec<-clG_sec[,c("REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_Ave.size","SE_Ave.size")]
bleG_sec<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","BLE")
bleG_sec<-bleG_sec[,c("REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_BLE_Prev","SE_BLE_Prev")]
AcuteDZG_sec<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","AcuteDZ")
AcuteDZG_sec<-AcuteDZG_sec[,c("REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_AcuteDZ_Prev","SE_AcuteDZ_Prev")]
ChronicDZG_sec<-Calc_Domain_Prevalence(site.data.gen2,"GENUS_CODE","ChronicDZ")
ChronicDZG_sec<-ChronicDZG_sec[,c("REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_ChronicDZ_Prev","SE_ChronicDZ_Prev")]




MyMerge <- function(x, y){
  df <- merge(x, y, by= c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
st.data.gen<-Reduce(MyMerge, list(acdG_st,jcdG_st,odG_st,rdG_st,clG_st,BLEG_st,AcuteDZG_st,ChronicDZG_st))
colnames(st.data.gen)[colnames(st.data.gen)=="ANALYSIS_SCHEMA"]<-"Stratum"

write.csv(st.data.gen,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicREA_stratadata_GENUS.csv")
# write.csv(rich_st,"Pacificwide_richness_frf_str3.csv")


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
is.data.gen<-Reduce(MyMerge, list(acdG_is,jcdG_is,odG_is,rdG_is,clG_is,bleG_is,AcuteDZG_is,ChronicDZG_is))
colnames(is.data.gen)[colnames(is.data.gen)=="DOMAIN_SCHEMA"]<-"Island"

write.csv(is.data.gen,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicREA_islanddata_GENUS.csv")


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("REGION","ANALYSIS_YEAR","ISLAND","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot"), all.x= TRUE, all.y= TRUE)
  return(df)
}
sec.data.gen<-Reduce(MyMerge, list(acdG_sec,jcdG_sec,odG_sec,rdG_sec,clG_sec,bleG_sec,AcuteDZG_sec,ChronicDZG_sec))
colnames(sec.data.gen)[colnames(sec.data.gen)=="DOMAIN_SCHEMA"]<-"Sector"

write.csv(sec.data.gen,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicREA_sectordata_GENUS.csv")




#Things to work on
#1. put pooling changes into csv file rather than write them out in text, too clunky and easy to get confused with different years
#2. Separate rz/db from sector name so have a column for sector and stratum- this will allow us to subset just certain depths and zone more easily later on.
#3. Make sure that ntot added up correctly across years and domains

