

rm(list=ls())

#LOAD LIBRARY FUNCTIONS ...
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")

site.data.gen2<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS.csv")
site.data.sp2<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_SPCODE.csv")
site.data.tax2<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_TAXONCODE.csv")


# site.data.gen2<-subset(site.data.gen2,ISLAND=="Maro")
# site.data.sp2<-subset(site.data.sp2,ISLAND=="Maro")
# site.data.tax2<-subset(site.data.tax2,ISLAND=="Maro")

# Remove special missions -------------------------------------------------

#Change all special missions to exclude flag =-1, right now they are 0. Then exclude these sites
levels(as.factor(site.data.gen2$MISSIONID))
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


# POOLING DATA from Site to Strata and Domain at GENUS-level---------------------------------------------------
survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv") #list of all sites
seclu<-read.csv("T:/Benthic/Data/Lookup Tables/PacificNCRMP_CoralDemographic_Sectors_Lookup_v3.csv") #list of SEC_NAME (smallest sector) and corresponding pooled sector scheme

#For older data requests (pre 2022) This function pools data at Sector scale according to predetermined groups. Protected reef slope is converted to Forereef here
#site.data.gen2<-PoolSecStrat(site.data.gen2)
#site.data.sp2<-PoolSecStrat(site.data.sp2)
#site.data.tax2<-PoolSecStrat(site.data.tax2)

#Merge site data with Sector look up table. This table indicates how sectors should be pooled or not
#For NCRMP viztool data- Keep pooling scheme the same across years
site.data.gen2<-left_join(site.data.gen2,seclu)
site.data.sp2<-left_join(site.data.sp2,seclu)
site.data.tax2<-left_join(site.data.tax2,seclu)

##Change Protected Reef Slope to Forereef- we do this for some data requests
# site.data.gen2$REEF_ZONE<-ifelse(site.data.gen2$REEF_ZONE %in% c("Protected Slope","Forereef"),"Forereef",as.character(site.data.gen2$REEF_ZONE)) #combine PRS and forereef similar to what fish team does
# site.data.sp2$REEF_ZONE<-ifelse(site.data.sp2$REEF_ZONE %in% c("Protected Slope","Forereef"),"Forereef",as.character(site.data.sp2$REEF_ZONE)) #combine PRS and forereef similar to what fish team does
# site.data.tax2$REEF_ZONE<-ifelse(site.data.tax2$REEF_ZONE %in% c("Protected Slope","Forereef"),"Forereef",as.character(site.data.tax2$REEF_ZONE)) #combine PRS and forereef similar to what fish team does

head(site.data.gen2)
head(site.data.sp2)
head(site.data.tax2)

#Create columns for Stata name (combo of Sector, reef zone and depth bin) & DB_RZ (depth bin/reef zone)
site.data.gen2$STRATANAME<-paste(site.data.gen2$PooledSector,site.data.gen2$REEF_ZONE,site.data.gen2$DEPTH_BIN,sep="_")
site.data.gen2$DB_RZ<-paste(substring(site.data.gen2$REEF_ZONE,1,1), substring(site.data.gen2$DEPTH_BIN,1,1), sep="")

site.data.tax2$STRATANAME<-paste(site.data.tax2$PooledSector,site.data.tax2$REEF_ZONE,site.data.tax2$DEPTH_BIN,sep="_")
site.data.tax2$DB_RZ<-paste(substring(site.data.tax2$REEF_ZONE,1,1), substring(site.data.tax2$DEPTH_BIN,1,1), sep="")

site.data.sp2$STRATANAME<-paste(site.data.sp2$PooledSector,site.data.sp2$REEF_ZONE,site.data.sp2$DEPTH_BIN,sep="_")
site.data.sp2$DB_RZ<-paste(substring(site.data.sp2$REEF_ZONE,1,1), substring(site.data.sp2$DEPTH_BIN,1,1), sep="")



#QC CHECK to make sure the sectors and strata pooled correctly
data.test<-ddply(subset(site.data.gen2,GENUS_CODE=="SSSS"),.(REGION,PooledSector,OBS_YEAR,STRATANAME),summarize,n=length(SITE))
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



#START WITH REGION METRICS- NOT WORKING

# Genus-Level Metrics -----------------------------------------------------

st.data.gen<-Calc_Strata_Metrics(site.data.gen2,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema="PooledSector")
sec.data.gen<-Calc_IslandorSector_Metrics(site.data.gen2,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema="PooledSector")
is.data.gen<-Calc_IslandorSector_Metrics(site.data.gen2,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema="ISLAND")
r.data.gen<-Calc_Region_Metrics(site.data.gen2,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema = "REGION")

write.csv(st.data.gen,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicREA_stratadata_GENUS.csv")
write.csv(is.data.gen,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicREA_islanddata_GENUS.csv")
write.csv(sec.data.gen,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicREA_sectordata_GENUS.csv")
write.csv(r.data.gen,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Region/BenthicREA_regiondata_GENUS.csv")


# SPCODE-Level Metrics -----------------------------------------------------

st.data.sp<-Calc_Strata_Metrics(site.data.sp2,grouping_field="SPCODE",a_schema ="STRATANAME",d_schema="PooledSector")
sec.data.sp<-Calc_IslandorSector_Metrics(site.data.sp2,grouping_field="SPCODE",a_schema ="STRATANAME",d_schema="PooledSector")
is.data.sp<-Calc_IslandorSector_Metrics(site.data.sp2,grouping_field="SPCODE",a_schema ="STRATANAME",d_schema="ISLAND")
r.data.sp<-Calc_Region_Metrics(site.data.sp2,grouping_field="SPCODE",a_schema ="STRATANAME",d_schema = "REGION")

write.csv(st.data.sp,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicREA_stratadata_SPCODE.csv")
write.csv(is.data.sp,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicREA_islanddata_SPCODE.csv")
write.csv(sec.data.sp,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicREA_sectordata_SPCODE.csv")
write.csv(r.data.sp,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Region/BenthicREA_regiondata_SPCODE.csv")


# TAXONCODE-Level Metrics -----------------------------------------------------

st.data.tax<-Calc_Strata_Metrics(site.data.tax2,grouping_field="TAXONCODE",a_schema ="STRATANAME",d_schema="PooledSector")
sec.data.tax<-Calc_IslandorSector_Metrics(site.data.tax2,grouping_field="TAXONCODE",a_schema ="STRATANAME",d_schema="PooledSector")
is.data.tax<-Calc_IslandorSector_Metrics(site.data.tax2,grouping_field="TAXONCODE",a_schema ="STRATANAME",d_schema="ISLAND")
r.data.tax<-Calc_Region_Metrics(site.data.tax2,grouping_field="TAXONCODE",a_schema ="STRATANAME",d_schema = "REGION")

write.csv(st.data.tax,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicREA_stratadata_TAXONCODE.csv")
write.csv(is.data.tax,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicREA_islanddata_TAXONCODE.csv")
write.csv(sec.data.tax,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicREA_sectordata_TAXONCODE.csv")
write.csv(r.data.tax,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Region/BenthicREA_regiondata_TAXONCODE.csv")



