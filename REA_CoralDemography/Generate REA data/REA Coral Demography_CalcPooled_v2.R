#This script is an updated from from REA Coral Demography_CalcPooled.R
#It takes the site level data from REA Coral Demographic_CalcSite.R at the 3 different taxomonic levels (genus, spcode and taxoncode) and
#summarizes our core demographic metrics to the strata, island, sector and regional level
#Updates from v1: I created this script for the NCRMPViztool data request, but it could be use for other data requests
#1. No longer using the the PoolSecStrat() function to assign sector pooling scheme. Now using a csv file of pooled sector names.
#This csv assumes we aren't changing the pooling scheme each year. 
#2. Now summarizing data at regional level- I'm not a fan of this because we may not survey the same sec/is year year so temporal comparsions at the regional level can be missleading

#CAVEAT- be careful about temporal comparisons- this script does not assume all strata and sectors are sampled each year.

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

#Create columns for Stata name (combo of Sector, reef zone and depth bin) & DB_RZ (depth bin/reef zone)
site.data.gen2$STRATANAME<-paste(site.data.gen2$PooledSector,site.data.gen2$REEF_ZONE,site.data.gen2$DEPTH_BIN,sep="_")
site.data.gen2$DB_RZ<-paste(substring(site.data.gen2$REEF_ZONE,1,1), substring(site.data.gen2$DEPTH_BIN,1,1), sep="")

site.data.tax2$STRATANAME<-paste(site.data.tax2$PooledSector,site.data.tax2$REEF_ZONE,site.data.tax2$DEPTH_BIN,sep="_")
site.data.tax2$DB_RZ<-paste(substring(site.data.tax2$REEF_ZONE,1,1), substring(site.data.tax2$DEPTH_BIN,1,1), sep="")

site.data.sp2$STRATANAME<-paste(site.data.sp2$PooledSector,site.data.sp2$REEF_ZONE,site.data.sp2$DEPTH_BIN,sep="_")
site.data.sp2$DB_RZ<-paste(substring(site.data.sp2$REEF_ZONE,1,1), substring(site.data.sp2$DEPTH_BIN,1,1), sep="")



# Final clean up before pooling -------------------------------------------

#Change island name for Alamagan, Guguan and Sarigan to AGS- small islands never sampled adequately enough 
site.data.gen2$ISLAND<-ifelse(site.data.gen2$ISLAND %in% c("Alamagan","Guguan","Sarigan"),"AGS",as.character(site.data.gen2$ISLAND)) #Combine islands
site.data.sp2$ISLAND<-ifelse(site.data.sp2$ISLAND %in% c("Alamagan","Guguan","Sarigan"),"AGS",as.character(site.data.sp2$ISLAND)) #Combine islands
site.data.tax2$ISLAND<-ifelse(site.data.tax2$ISLAND %in% c("Alamagan","Guguan","Sarigan"),"AGS",as.character(site.data.tax2$ISLAND)) #Combine islands


#Remove NWHI islands only surveyed by PMNM and GUA_MP (sampling too low and patchy in 2014 and 2017)
remove<-c("Laysan","Maro","Midway","GUA_MP")
site.data.gen2<-dplyr::filter(site.data.gen2, !PooledSector %in% remove)
site.data.sp2<-dplyr::filter(site.data.sp2, !PooledSector %in% remove)
site.data.tax2<-dplyr::filter(site.data.tax2, !PooledSector %in% remove)

#Remove PRIA 2016 and 2017 surveys- done off cycle for the bleaching response, and do not have all metrics
site.data.gen2$REGION_YEAR<-paste(site.data.gen2$REGION,site.data.gen2$ANALYSIS_YEAR,sep = "_")
site.data.sp2$REGION_YEAR<-paste(site.data.sp2$REGION,site.data.sp2$ANALYSIS_YEAR,sep = "_")
site.data.tax2$REGION_YEAR<-paste(site.data.tax2$REGION,site.data.tax2$ANALYSIS_YEAR,sep = "_")

remove<-c("PRIAs_2016","PRIAs_2017")
site.data.gen2<-dplyr::filter(site.data.gen2, !REGION_YEAR %in% remove)
site.data.sp2<-dplyr::filter(site.data.sp2, !REGION_YEAR %in% remove)
site.data.tax2<-dplyr::filter(site.data.tax2, !REGION_YEAR %in% remove)

#Change Analysis year for PRIAs- you will need to do this for regional estiamtes that include both wake (2014,2017) and other PRIAs (2015 and 2018)
site.data.gen2$ANALYSIS_YEAR<-ifelse(site.data.gen2$REGION_YEAR %in% c("PRIAs_2014","PRIAs_2015"),"2014-15",site.data.gen2$ANALYSIS_YEAR)
site.data.gen2$ANALYSIS_YEAR<-ifelse(site.data.gen2$REGION_YEAR %in% c("PRIAs_2017","PRIAs_2018"),"2017-18",site.data.gen2$ANALYSIS_YEAR)
site.data.sp2$ANALYSIS_YEAR<-ifelse(site.data.sp2$REGION_YEAR %in% c("PRIAs_2014","PRIAs_2015"),"2014-15",site.data.sp2$ANALYSIS_YEAR)
site.data.sp2$ANALYSIS_YEAR<-ifelse(site.data.sp2$REGION_YEAR %in% c("PRIAs_2017","PRIAs_2018"),"2017-18",site.data.sp2$ANALYSIS_YEAR)
site.data.tax2$ANALYSIS_YEAR<-ifelse(site.data.tax2$REGION_YEAR %in% c("PRIAs_2014","PRIAs_2015"),"2014-15",site.data.tax2$ANALYSIS_YEAR)
site.data.tax2$ANALYSIS_YEAR<-ifelse(site.data.tax2$REGION_YEAR %in% c("PRIAs_2017","PRIAs_2018"),"2017-18",site.data.tax2$ANALYSIS_YEAR)


##Change Protected Reef Slope to Forereef- we do this for some data requests
# site.data.gen2$REEF_ZONE<-ifelse(site.data.gen2$REEF_ZONE %in% c("Protected Slope","Forereef"),"Forereef",as.character(site.data.gen2$REEF_ZONE)) #combine PRS and forereef similar to what fish team does
# site.data.sp2$REEF_ZONE<-ifelse(site.data.sp2$REEF_ZONE %in% c("Protected Slope","Forereef"),"Forereef",as.character(site.data.sp2$REEF_ZONE)) #combine PRS and forereef similar to what fish team does
# site.data.tax2$REEF_ZONE<-ifelse(site.data.tax2$REEF_ZONE %in% c("Protected Slope","Forereef"),"Forereef",as.character(site.data.tax2$REEF_ZONE)) #combine PRS and forereef similar to what fish team does

head(site.data.gen2)
head(site.data.sp2)
head(site.data.tax2)


#QC CHECK to make sure the sectors and strata pooled correctly
data.test<-ddply(subset(site.data.gen2,GENUS_CODE=="SSSS"),.(REGION,PooledSector,OBS_YEAR,STRATANAME),summarize,n=length(SITE))
sm.test<-ddply(subset(survey_master,Benthic=="1"&EXCLUDE_FLAG=="0"&OBS_YEAR>=2013),.(REGION,ISLAND,SEC_NAME,OBS_YEAR,REEF_ZONE,DEPTH_BIN),summarize,n=length(SITE))

write.csv(data.test,"tmp_sitedataQC.csv")
write.csv(sm.test,"tmp_sitemasterQC.csv")

# #Subset just Forereef Sites & just target taxa
# site.data.gen2<-subset(site.data.gen2,REEF_ZONE=="Forereef")
# site.data.gen2<-subset(site.data.gen2,GENUS_CODE %in% c("ACSP", "MOSP", "PAVS", "POCS","POSP","SSSS"))

# #Make sure you everything but forereef are dropped
# table(site.data.gen2$REEF_ZONE,site.data.gen2$GENUS_CODE)


# Genus-Level Metrics -----------------------------------------------------

st.data.gen<-Calc_Strata_Metrics(site.data.gen2,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema="PooledSector")
sec.data.gen<-Calc_IslandorSector_Metrics(site.data.gen2,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema="PooledSector")
is.data.gen<-Calc_IslandorSector_Metrics(site.data.gen2,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema="ISLAND")
r.data.gen<-Calc_Region_Metrics(site.data.gen2,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema = "REGION")

write.csv(st.data.gen,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicREA_stratadata_GENUS.csv",row.names=F)
write.csv(is.data.gen,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicREA_islanddata_GENUS.csv",row.names=F)
write.csv(sec.data.gen,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicREA_sectordata_GENUS.csv",row.names=F)
write.csv(r.data.gen,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Region/BenthicREA_regiondata_GENUS.csv",row.names=F)


# SPCODE-Level Metrics -----------------------------------------------------

st.data.sp<-Calc_Strata_Metrics(site.data.sp2,grouping_field="SPCODE",a_schema ="STRATANAME",d_schema="PooledSector")
sec.data.sp<-Calc_IslandorSector_Metrics(site.data.sp2,grouping_field="SPCODE",a_schema ="STRATANAME",d_schema="PooledSector")
is.data.sp<-Calc_IslandorSector_Metrics(site.data.sp2,grouping_field="SPCODE",a_schema ="STRATANAME",d_schema="ISLAND")
r.data.sp<-Calc_Region_Metrics(site.data.sp2,grouping_field="SPCODE",a_schema ="STRATANAME",d_schema = "REGION")

write.csv(st.data.sp,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicREA_stratadata_SPCODE.csv",row.names=F)
write.csv(is.data.sp,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicREA_islanddata_SPCODE.csv",row.names=F)
write.csv(sec.data.sp,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicREA_sectordata_SPCODE.csv",row.names=F)
write.csv(r.data.sp,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Region/BenthicREA_regiondata_SPCODE.csv",row.names=F)


# TAXONCODE-Level Metrics -----------------------------------------------------

st.data.tax<-Calc_Strata_Metrics(site.data.tax2,grouping_field="TAXONCODE",a_schema ="STRATANAME",d_schema="PooledSector")
sec.data.tax<-Calc_IslandorSector_Metrics(site.data.tax2,grouping_field="TAXONCODE",a_schema ="STRATANAME",d_schema="PooledSector")
is.data.tax<-Calc_IslandorSector_Metrics(site.data.tax2,grouping_field="TAXONCODE",a_schema ="STRATANAME",d_schema="ISLAND")
r.data.tax<-Calc_Region_Metrics(site.data.tax2,grouping_field="TAXONCODE",a_schema ="STRATANAME",d_schema = "REGION")

write.csv(st.data.tax,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicREA_stratadata_TAXONCODE.csv",row.names=F)
write.csv(is.data.tax,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicREA_islanddata_TAXONCODE.csv",row.names=F)
write.csv(sec.data.tax,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicREA_sectordata_TAXONCODE.csv",row.names=F)
write.csv(r.data.tax,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Region/BenthicREA_regiondata_TAXONCODE.csv",row.names=F)



