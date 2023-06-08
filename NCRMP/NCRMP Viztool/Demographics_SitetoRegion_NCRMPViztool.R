#This script is an updated from from REA Coral Demography_CalcPooled.R
#It takes the site level data from REA Coral Demographic_CalcSite.R at the 3 different taxomonic levels (genus, spcode and taxoncode) and
#summarizes our core demographic metrics to the strata, island, sector and regional level
#Updates from v1: I created this script for the NCRMPViztool data request, but it could be use for other data requests
#1. No longer using the the PoolSecStrat() function to assign sector pooling scheme. Now using a csv file of pooled sector names.
#This csv assumes we aren't changing the pooling scheme each year. 
#2. Now summarizing data at regional level- I'm not a fan of this because we may not survey the same sec/is year year so temporal comparsions at the regional level can be missleading

#CAVEAT- be careful about temporal comparisons- this script does not assume all strata and sectors are sampled each year.
# 2023 We have now added a second data frame with poorly sampled strata dropped

rm(list=ls())

#LOAD LIBRARY FUNCTIONS ...
# source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
# source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
# source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

#source("C:/Users/courtney.s.couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
#source("C:/Users/courtney.s.couch/Documents/GitHub/fish-paste/lib/core_functions.R")

source("./Functions/Benthic_Functions_newApp_vTAOfork.R")
source("../fish-paste/lib/core_functions.R")
library(tidyverse)

## LOAD benthic data
site.data.gen2<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS.csv")

#site.data.gen2<-subset(site.data.gen2,ISLAND=="Alamagan")

# Remove special missions -------------------------------------------------

#Change all special missions to exclude flag =-1, right now they are 0. Then exclude these sites
levels(as.factor(site.data.gen2$MISSIONID))
site.data.gen2$EXCLUDE_FLAG<-ifelse(site.data.gen2$MISSIONID %in% c("MP1410","MP1512","MP1602","MP2006"),-1,0) #I left SE1602 in (2016 Jarvis and Rose)
head(subset(site.data.gen2,EXCLUDE_FLAG==-1))

#Actually remove special missions.
site.data.gen2<-subset(site.data.gen2,EXCLUDE_FLAG==0);
# this dataframe should be empty
head(subset(site.data.gen2,EXCLUDE_FLAG==-1))

# end Remove Special Mission

# POOLING DATA from Site to Strata and Domain at GENUS-level---------------------------------------------------
#survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv") #list of all sites
survey_master<-read.csv("M:/Benthic Scripts_LEA/SURVEY MASTER.csv") #list of all sites

#Read in list of SEC_NAME (smallest sector), the corresponding pooled sector scheme PooledSector_Viztool) that we are using for Viztool and strata codes/names. 
#Sectors are pooled when there is inadequate sample size for a given single sector. The pooled sectors are the same across years
#PooledSector_Demo_1 is the coarser pooling that we typically use. Erica and Viztool had difficulty visualize sectors this large especially for the MHI.
seclu<-read.csv("T:/Benthic/Data/Lookup Tables/PacificNCRMP_Benthic_Sectors_Lookup_v4.csv") #list of SEC_NAME (smallest sector) and corresponding pooled sector scheme


#Merge site data with Sector look up table. This table indicates how sectors should be pooled or not
#For NCRMP viztool data- Keep pooling scheme the same across years
site.data.gen2<-left_join(site.data.gen2,seclu)

#Create columns for Stata name (combo of Sector, reef zone and depth bin) & DB_RZ (depth bin/reef zone)
site.data.gen2$STRATANAME<-paste(site.data.gen2$PooledSector_Viztool,site.data.gen2$REEF_ZONE,site.data.gen2$DEPTH_BIN,sep="_")
site.data.gen2$STRATANAME_TRENDS<-paste(site.data.gen2$SEC_NAME,site.data.gen2$REEF_ZONE,site.data.gen2$DEPTH_BIN,sep="_")
site.data.gen2$DB_RZ<-paste(substring(site.data.gen2$REEF_ZONE,1,1), substring(site.data.gen2$DEPTH_BIN,1,1), sep="")

#SAMPLING REGION:REgion to consider whether or not it got sampled
site.data.gen2=site.data.gen2 %>%
  mutate(SAMPLING_REGION= case_when(
    ISLAND =="Wake" ~ "MARIAN",
    TRUE ~ as.character(REGION)))
table(site.data.gen2$SAMPLING_REGION)
table(site.data.gen2$REGION)


#Stable Dataset: site.data.gen2
#write.csv(site.data.gen2,"T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/2023ViztoolSites_Demo_FirstPass.csv")

#2023 MARCH - SUBSET FOR TEMPORALLY COHERENT STRATA LEVEL SAMPLING - NO LESS THAN 2 SAMPLES PER STRATA THE WHOLE TIME
###############################
strat2drop=site.data.gen2 %>% filter(GENUS_CODE=="SSSS")

#This is the data frame of regional-scale "should have been sampled"
#Nested region and strata as single value
REG_STRATA=unique(strat2drop[,c("SAMPLING_REGION","STRATANAME_TRENDS")])
REG_STRATA$SR_STR=paste0(REG_STRATA$SAMPLING_REGION,"_",REG_STRATA$STRATANAME_TRENDS)

#Years in which a region should have been sampled
REGIONAL_SAMPLING_EFFORT_YEARS=table(strat2drop[,c("SAMPLING_REGION","ANALYSIS_YEAR")],useNA = "ifany") %>%
  as.data.frame() %>%   mutate(SR_YR=paste0(SAMPLING_REGION,"_",ANALYSIS_YEAR)) 
REGIONAL_SAMPLING_EFFORT_YEARS_CANON=REGIONAL_SAMPLING_EFFORT_YEARS %>% filter(Freq>50) 

#Highlight special island level sampling
REGIONAL_SAMPLING_EFFORT_YEARS_SP=REGIONAL_SAMPLING_EFFORT_YEARS %>% filter(Freq<=50&Freq>0) 
ISLAND_SAMPLING_EFFORT_YEARS=table(strat2drop[,c("SAMPLING_REGION","ISLAND","ANALYSIS_YEAR")],useNA = "ifany") %>%
  as.data.frame() %>%   
  mutate(SR_YR=paste0(SAMPLING_REGION,"_",ANALYSIS_YEAR)) %>% 
  filter(SR_YR %in% REGIONAL_SAMPLING_EFFORT_YEARS_SP$SR_YR) %>% 
  filter(Freq>0) %>% arrange(SR_YR)


#Actual sampling Described by Stratum
Sample_Table=table(strat2drop[,c("SAMPLING_REGION","STRATANAME_TRENDS","ANALYSIS_YEAR")],useNA = "ifany")
Actual_Sample_Table=Sample_Table %>% 
  as.data.frame() %>% 
  mutate(SR_YR=paste0(SAMPLING_REGION,"_",ANALYSIS_YEAR),
         SR_STR=paste0(SAMPLING_REGION,"_",STRATANAME_TRENDS))%>%
  filter(SR_STR%in%REG_STRATA$SR_STR) # Drop Strata not present in a region

Low_Freq_STR_Canon=Actual_Sample_Table %>% 
  filter(SR_YR %in% REGIONAL_SAMPLING_EFFORT_YEARS_CANON$SR_YR) %>% 
  filter(Freq<=1)

DropSTR_Canon=Low_Freq_STR_Canon$STRATANAME_TRENDS

Low_Freq_STR_SP=Actual_Sample_Table %>%
  filter(SR_YR %in% REGIONAL_SAMPLING_EFFORT_YEARS_SP$SR_YR) 
Low_Freq_STR_SP=Low_Freq_STR_SP %>% 
  mutate(ISLAND=substr(Low_Freq_STR_SP$STRATANAME_TRENDS,1,
                              regexpr("_", Low_Freq_STR_SP$STRATANAME_TRENDS, fixed = TRUE) - 1),
         ISL_YR=paste0(ISLAND,"_",ANALYSIS_YEAR))
SPEC_TARGETS=Low_Freq_STR_SP %>% group_by(ISL_YR) %>% summarize(ISL_N=sum(Freq)) %>% filter(ISL_N>0)
Low_Freq_STR_SP=Low_Freq_STR_SP %>% filter(ISL_YR %in% SPEC_TARGETS$ISL_YR) %>% filter(Freq<=1)

DropSTR_SP=Low_Freq_STR_SP$STRATANAME_TRENDS

DropSTR=union(DropSTR_Canon,DropSTR_SP)

#Which stratum-analysis year combo have only one sample
#Drop the poorly sampled strata and carry the new data.frame forward
singleton_strata=site.data.gen2 %>% 
  filter(GENUS_CODE=="SSSS") %>% 
  group_by(STRATANAME,ANALYSIS_YEAR) %>% 
  count() %>% filter(n==1) %>% 
  mutate(STR_YR=paste0(STRATANAME,"_",ANALYSIS_YEAR)) %>% pull(STR_YR)
site.data.gen2=site.data.gen2 %>% 
  mutate(STR_YR=paste0(STRATANAME,"_",ANALYSIS_YEAR)) %>% 
  filter(!STR_YR%in%singleton_strata)

#Which unpooled stratanames have no sampling or N=1 in regionally sampled years,
#or islands with special year sampling
#Drop the poorly sampled strata and carry the new data.frame forward
site.data.gen2.trends=site.data.gen2 %>% filter(!STRATANAME_TRENDS%in%DropSTR)

## Check Dropped Stratanames
# t=table(sort(apply(table(strat2drop$STRATANAME_TRENDS,strat2drop$ANALYSIS_YEAR),1,min_pos)))
# t/sum(t)
NODROPSTR=site.data.gen2 %>% filter(GENUS_CODE=="SSSS") %>% 
  group_by(STRATANAME,ANALYSIS_YEAR) %>% count() %>% group_by(STRATANAME) %>% summarize(Nmin=min(n))
DROPSTR=site.data.gen2.trends %>% filter(GENUS_CODE=="SSSS") %>%
  group_by(STRATANAME_TRENDS,ANALYSIS_YEAR) %>% count() %>% group_by(STRATANAME_TRENDS) %>% summarize(Nmin=min(n))
 drops.df=data.frame(STRATANAME=DropSTR,DROPIT=DropSTR)
 left_join(left_join(NODROPSTR,DROPSTR,by=c("STRATANAME"="STRATANAME_TRENDS")),drops.df)
###############################
#Moving ahead from here with both "site.data.gen2" and the strata dropped "site.data.gen2.trends"


###############################
# Final clean up before pooling take one for "site.data.gen2" -------------------------------------------
###############################

#Change island name for Alamagan, Guguan and Sarigan to SGA- small islands never sampled adequately enough & mirror's fish data
site.data.gen2$ISLAND<-ifelse(site.data.gen2$ISLAND %in% c("Alamagan","Guguan","Sarigan"),"SGA",as.character(site.data.gen2$ISLAND)) #Combine islands

site.data.gen2 <- site.data.gen2 %>% mutate(site.data.gen2,
                                            REGION= case_when(
                                              ISLAND =="Guam" ~ "GUA",
                                              REGION == "MARIAN" & ISLAND !="Guam" ~ "CNMI",
                                              TRUE ~ REGION))
site.data.gen2 <- site.data.gen2 %>% mutate(site.data.gen2,
                                            REGION_NAME= case_when(
                                              ISLAND =="Guam" ~ "Guam",
                                              REGION == "CNMI"~ "Commonwealth of the Northern Mariana Islands",
                                              TRUE ~ REGION_NAME))

#Remove NWHI islands only surveyed by PMNM and not sampled well
remove<-c("Laysan","Maro","Midway")
site.data.gen2<-dplyr::filter(site.data.gen2, !PooledSector_Viztool %in% remove)

# #Remove PRIA 2016 and 2017 surveys- done off cycle for the bleaching response, and do not have all metrics, but keep Wake
site.data.gen2$REGION_YEAR<-paste(site.data.gen2$REGION,site.data.gen2$ANALYSIS_YEAR,sep = "_")
site.data.gen2$REGION_YEAR<-ifelse((site.data.gen2$ISLAND=="Wake" & site.data.gen2$ANALYSIS_YEAR=="2017"),"PRIAs_2017w",site.data.gen2$REGION_YEAR) #This will help you keep wake 2017 data

remove<-c("PRIAs_2016","PRIAs_2017")
site.data.gen2<-dplyr::filter(site.data.gen2, !REGION_YEAR %in% remove)

####Leave Guam MPA in the Complete Data
#site.data.gen2<-dplyr::filter(site.data.gen2, !(PooledSector_Viztool == "GUA_MP" & ANALYSIS_YEAR == "2017"))

#Change Analysis year for PRIAs- you will need to do this for regional estiamtes that include both wake (2014,2017) and other PRIAs (2015 and 2018)
site.data.gen2$ANALYSIS_YEAR<-ifelse(site.data.gen2$REGION_YEAR %in% c("PRIAs_2014","PRIAs_2015"),"2014-15",as.character(site.data.gen2$ANALYSIS_YEAR))
site.data.gen2$ANALYSIS_YEAR<-ifelse(site.data.gen2$REGION_YEAR %in% c("PRIAs_2017w","PRIAs_2017","PRIAs_2018"),"2017-18",as.character(site.data.gen2$ANALYSIS_YEAR))

##Change Protected Reef Slope to Forereef- we do this for some data requests
# site.data.gen2$REEF_ZONE<-ifelse(site.data.gen2$REEF_ZONE %in% c("Protected Slope","Forereef"),"Forereef",as.character(site.data.gen2$REEF_ZONE)) #combine PRS and forereef similar to what fish team does
###############################
###############################
###############################



###############################
# Final clean up before pooling take TWO for "site.data.gen2.trends" -------------------------------------------
###############################

#Change island name for Alamagan, Guguan and Sarigan to SGA- small islands never sampled adequately enough & mirror's fish data
site.data.gen2.trends$ISLAND<-ifelse(site.data.gen2.trends$ISLAND %in% c("Alamagan","Guguan","Sarigan"),"SGA",as.character(site.data.gen2.trends$ISLAND)) #Combine islands

site.data.gen2.trends <- site.data.gen2.trends %>% mutate(site.data.gen2.trends,
                                                          REGION= case_when(
                                                            ISLAND =="Guam" ~ "GUA",
                                                            REGION == "MARIAN" & ISLAND !="Guam" ~ "CNMI",
                                                            TRUE ~ REGION))
site.data.gen2.trends <- site.data.gen2.trends %>% mutate(site.data.gen2.trends,
                                                          REGION_NAME= case_when(
                                                            ISLAND =="Guam" ~ "Guam",
                                                            REGION == "CNMI"~ "Commonwealth of the Northern Mariana Islands",
                                                            TRUE ~ REGION_NAME))

#Remove NWHI islands only surveyed by PMNM and not sampled well
remove<-c("Laysan","Maro","Midway")
site.data.gen2.trends<-dplyr::filter(site.data.gen2.trends, !PooledSector_Viztool %in% remove)

#Remove PRIA 2016 and 2017 surveys- done off cycle for the bleaching response, and do not have all metrics, but keep Wake
site.data.gen2.trends$REGION_YEAR<-paste(site.data.gen2.trends$REGION,site.data.gen2.trends$ANALYSIS_YEAR,sep = "_")
site.data.gen2.trends$REGION_YEAR<-ifelse((site.data.gen2.trends$ISLAND=="Wake" & site.data.gen2.trends$ANALYSIS_YEAR=="2017"),"PRIAs_2017w",site.data.gen2.trends$REGION_YEAR) #This will help you keep wake 2017 data

remove<-c("PRIAs_2016","PRIAs_2017")
site.data.gen2.trends<-dplyr::filter(site.data.gen2.trends, !REGION_YEAR %in% remove)

#remove ALL Guam MPA data from Trends - not enough sites across all the MPAs
site.data.gen2.trends<-dplyr::filter(site.data.gen2.trends, !(PooledSector_Viztool == "GUA_MP"))

#Change Analysis year for PRIAs- you will need to do this for regional estiamtes that include both wake (2014,2017) and other PRIAs (2015 and 2018)
site.data.gen2.trends$ANALYSIS_YEAR<-ifelse(site.data.gen2.trends$REGION_YEAR %in% c("PRIAs_2014","PRIAs_2015"),"2014-15",as.character(site.data.gen2.trends$ANALYSIS_YEAR))
site.data.gen2.trends$ANALYSIS_YEAR<-ifelse(site.data.gen2.trends$REGION_YEAR %in% c("PRIAs_2017w","PRIAs_2017","PRIAs_2018"),"2017-18",as.character(site.data.gen2.trends$ANALYSIS_YEAR))

##Change Protected Reef Slope to Forereef- we do this for some data requests
# site.data.gen2.trends$REEF_ZONE<-ifelse(site.data.gen2.trends$REEF_ZONE %in% c("Protected Slope","Forereef"),"Forereef",as.character(site.data.gen2.trends$REEF_ZONE)) #combine PRS and forereef similar to what fish team does
###############################
###############################
###############################

###############################
#QC CHECK to make sure the sectors and strata pooled correctly
###############################
data.test<-ddply(subset(site.data.gen2,GENUS_CODE=="SSSS"),.(REGION,PooledSector_Viztool,OBS_YEAR,STRATANAME),summarize,n=length(SITE))
data.test.trends<-ddply(subset(site.data.gen2.trends,GENUS_CODE=="SSSS"),.(REGION,PooledSector_Viztool,OBS_YEAR,STRATANAME),summarize,n=length(SITE))
sm.test<-ddply(subset(survey_master,Benthic=="1"&EXCLUDE_FLAG=="0"&OBS_YEAR>=2013),.(REGION,ISLAND,SEC_NAME,OBS_YEAR,REEF_ZONE,DEPTH_BIN),summarize,n=length(SITE))
write.csv(data.test,"tmp_sitedataQC.csv")
write.csv(data.test.trends,"tmp_sitedatatrendsQC.csv")
write.csv(sm.test,"tmp_sitemasterQC.csv")

# #Subset just Forereef Sites & just target taxa
# site.data.gen2<-subset(site.data.gen2,REEF_ZONE=="Forereef")
# site.data.gen2<-subset(site.data.gen2,GENUS_CODE %in% c("ACSP", "MOSP", "PAVS", "POCS","POSP","SSSS"))

# #Make sure you everything but forereef are dropped
# table(site.data.gen2$REEF_ZONE,site.data.gen2$GENUS_CODE)

# Genus-Level Metrics -----------------------------------------------------
#complete dataset
st.data.gen<-Calc_Strata_Metrics(site.data.gen2,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema="PooledSector_Viztool")
sec.data.gen<-Calc_IslandorSector_Metrics(site.data.gen2,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema="PooledSector_Viztool")
is.data.gen<-Calc_IslandorSector_Metrics(site.data.gen2,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema="ISLAND")
r.data.gen<-Calc_Region_Metrics(site.data.gen2,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema = "REGION")

write.csv(st.data.gen,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/BenthicREA_STRATA_Demo_Viztool_2023.csv",row.names=F)
write.csv(is.data.gen,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/BenthicREA_ISLAND_Demo_Viztool_2023.csv",row.names=F)
write.csv(sec.data.gen,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/BenthicREA_SECTOR_Demo_Viztool_2023.csv",row.names=F)
write.csv(r.data.gen,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/BenthicREA_REGION_Demo_Viztool_2023.csv",row.names=F)

#trends dataset
st.data.gen.trends<-Calc_Strata_Metrics(site.data.gen2.trends,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema="PooledSector_Viztool")
sec.data.gen.trends<-Calc_IslandorSector_Metrics(site.data.gen2.trends,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema="PooledSector_Viztool")
is.data.gen.trends<-Calc_IslandorSector_Metrics(site.data.gen2.trends,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema="ISLAND")

#Drop Special Sampling Years
removePRIA1617<-c("PRIAs_2016","PRIAs_2017")
site.data.gen2.trends_nosp<-dplyr::filter(site.data.gen2.trends, !REGION_YEAR %in% removePRIA1617)
r.data.gen.trends<-Calc_Region_Metrics(site.data.gen2.trends_nosp,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema = "REGION")

write.csv(st.data.gen.trends,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/BenthicREA_STRATA_TRENDS_Demo_Viztool_2023.csv",row.names=F)
write.csv(is.data.gen.trends,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/BenthicREA_ISLAND_TRENDS_Demo_Viztool_2023.csv",row.names=F)
write.csv(sec.data.gen.trends,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/BenthicREA_SECTOR_TRENDS_Demo_Viztool_2023.csv",row.names=F)
write.csv(r.data.gen.trends,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/BenthicREA_REGION_TRENDS_Demo_Viztool_2023.csv",row.names=F)


#QC Checks
st.data.gen=read.csv(file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/COMPLETE/BenthicREA_STRATA_Demo_Viztool_2023.csv")
sec.data.gen=read.csv(file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/COMPLETE/BenthicREA_SECTOR_Demo_Viztool_2023.csv")
is.data.gen=read.csv(file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/COMPLETE/BenthicREA_ISLAND_Demo_Viztool_2023.csv")
r.data.gen=read.csv(file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/COMPLETE/BenthicREA_REGION_Demo_Viztool_2023.csv")
st.data.gen.trends=read.csv(file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicREA_STRATA_TRENDS_Demo_Viztool_2023.csv")
sec.data.gen.trends=read.csv(file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicREA_SECTOR_TRENDS_Demo_Viztool_2023.csv")
is.data.gen.trends=read.csv(file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicREA_ISLAND_TRENDS_Demo_Viztool_2023.csv")
r.data.gen.trends=read.csv(file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicREA_REGION_TRENDS_Demo_Viztool_2023.csv")

summary(st.data.gen)
levels(as.factor(st.data.gen$ANALYSIS_YEAR))
levels(as.factor(st.data.gen$SECTOR))

summary(st.data.gen.trends)
levels(as.factor(st.data.gen.trends$ANALYSIS_YEAR))
levels(as.factor(st.data.gen.trends$SECTOR))

RepTab=sec.data.gen %>% 
  filter(GENUS_CODE=="SSSS") %>% 
  dplyr::select(REGION,PooledSector_Viztool,ANALYSIS_YEAR,n) %>% 
  group_by(REGION,PooledSector_Viztool) %>% 
   pivot_wider(names_from=ANALYSIS_YEAR,values_from = n)
RepTab=RepTab[,c(names(RepTab)[1:2],sort(names(RepTab)[3:length(names(RepTab))]))]
RepTab %>% print(n=999)

RepTabT=sec.data.gen.trends %>% 
  filter(GENUS_CODE=="SSSS") %>% 
  dplyr::select(REGION,PooledSector_Viztool,ANALYSIS_YEAR,n) %>% 
  group_by(REGION,PooledSector_Viztool) %>% 
  pivot_wider(names_from=ANALYSIS_YEAR,values_from = n)
RepTabT=RepTabT[,c(names(RepTabT)[1:2],sort(names(RepTabT)[3:length(names(RepTabT))]))]
RepTabT %>% print(n=999)
dim(RepTab)
dim(RepTabT)
