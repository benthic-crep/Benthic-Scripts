#This script is an updated from from REA Coral Demography_CalcPooled.R
#It takes the site level data from REA Coral Demographic_CalcSite.R at the 3 different taxomonic levels (genus, spcode and taxoncode) and
#summarizes our core demographic metrics to the strata, island, sector and regional level
#Updates from v1: I created this script for the NCRMPViztool data request, but it could be use for other data requests
#1. No longer using the the PoolSecStrat() function to assign sector pooling scheme. Now using a csv file of pooled sector names.
#This csv assumes we aren't changing the pooling scheme each year. 
#2. Now summarizing data at regional level- I'm not a fan of this because we may not survey the same sec/is year year so temporal comparsions at the regional level can be missleading

#CAVEAT- be careful about temporal comparisons- this script does not assume all strata and sectors are sampled each year.
# 2023 We have now added a second data frame with poorly sampled strata dropped

# ---- Section 0: Prep ----
rm(list=ls())
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
# Check which packages are currently loaded
pacman::p_loaded()

#Set Run Flags
DEBUG=FALSE

#LOAD LIBRARY FUNCTIONS ...
source("../fish-paste/lib/core_functions.R")
source("./Functions/Benthic_Functions_newApp_v2025TAOfork.R")
source("./Functions/Core_Benthic_Aggregation_Functions_2025.R")

# ---- Section 1: Load SiteLevelData, Clean Site Level Data ----
site.data.gen=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS_2013_2024.csv")
site.data.tax=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_TAXONCODE_2013_2024.csv")
site.data.sp=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_SPCODE_2013_2024.csv")

#site.data.gen<-subset(site.data.gen,ISLAND=="Alamagan")

# Remove special missions
#Change all special missions to exclude flag =-1, right now they are 0. Then exclude these sites
#levels(as.factor(site.data.gen$MISSIONID))
site.data.gen$EXCLUDE_FLAG<-ifelse(site.data.gen$MISSIONID %in% c("MP1410","MP1512","MP1602","MP2006"),-1,0) #I left SE1602 in (2016 Jarvis and Rose)
site.data.tax$EXCLUDE_FLAG<-ifelse(site.data.tax$MISSIONID %in% c("MP1410","MP1512","MP1602","MP2006"),-1,0) #I left SE1602 in (2016 Jarvis and Rose)
site.data.sp$EXCLUDE_FLAG<-ifelse(site.data.sp$MISSIONID %in% c("MP1410","MP1512","MP1602","MP2006"),-1,0) #I left SE1602 in (2016 Jarvis and Rose)
#site.data.gen %>% dplyr::select(MISSIONID,EXCLUDE_FLAG) %>% distinct()
#head(subset(site.data.gen,EXCLUDE_FLAG==-1))

#Actually remove special missions.
site.data.gen<-subset(site.data.gen,EXCLUDE_FLAG==0);
site.data.tax<-subset(site.data.tax,EXCLUDE_FLAG==0);
site.data.sp<-subset(site.data.sp,EXCLUDE_FLAG==0);
# this dataframe should be empty
#head(subset(site.data.gen,EXCLUDE_FLAG==-1))

# end Remove Special Mission

# POOLING DATA from Site to Strata and Domain at GENUS-level 
survey_master<-read.csv("./NCRMP/FY26 Benthic Pipeline/A. Survey Master Prep/SURVEY_MASTER_2024_benthic.csv")

#Read in list of SEC_NAME (smallest sector), the corresponding pooled sector scheme PooledSector_Viztool) that we are using for Viztool and strata codes/names. 
#Sectors are pooled when there is inadequate sample size for a given single sector. The pooled sectors are the same across years
#PooledSector_Demo_1 is the coarser pooling that we typically use. Erica and Viztool had difficulty visualize sectors this large especially for the MHI.
seclu<-read.csv("T:/Benthic/Data/Lookup Tables/PacificNCRMP_Benthic_Sectors_Lookup_v4.csv") #list of SEC_NAME (smallest sector) and corresponding pooled sector scheme

#Merge site data with Sector look up table. This table indicates how sectors should be pooled or not
#For NCRMP viztool data- Keep pooling scheme the same across years
site.data.gen<-left_join(site.data.gen,seclu)
site.data.tax<-left_join(site.data.tax,seclu)
site.data.sp<-left_join(site.data.sp,seclu)

#Create columns for Stata name (combo of Sector, reef zone and depth bin) & DB_RZ (depth bin/reef zone)
site.data.gen$STRATANAME<-paste(site.data.gen$PooledSector_Viztool,site.data.gen$REEF_ZONE,site.data.gen$DEPTH_BIN,sep="_")
site.data.gen$STRATANAME_TRENDS<-paste(site.data.gen$SEC_NAME,site.data.gen$REEF_ZONE,site.data.gen$DEPTH_BIN,sep="_")
site.data.gen$DB_RZ<-paste(substring(site.data.gen$REEF_ZONE,1,1), substring(site.data.gen$DEPTH_BIN,1,1), sep="")
site.data.tax$STRATANAME<-paste(site.data.tax$PooledSector_Viztool,site.data.tax$REEF_ZONE,site.data.tax$DEPTH_BIN,sep="_")
site.data.tax$STRATANAME_TRENDS<-paste(site.data.tax$SEC_NAME,site.data.tax$REEF_ZONE,site.data.tax$DEPTH_BIN,sep="_")
site.data.tax$DB_RZ<-paste(substring(site.data.tax$REEF_ZONE,1,1), substring(site.data.tax$DEPTH_BIN,1,1), sep="")
site.data.sp$STRATANAME<-paste(site.data.sp$PooledSector_Viztool,site.data.sp$REEF_ZONE,site.data.sp$DEPTH_BIN,sep="_")
site.data.sp$STRATANAME_TRENDS<-paste(site.data.sp$SEC_NAME,site.data.sp$REEF_ZONE,site.data.sp$DEPTH_BIN,sep="_")
site.data.sp$DB_RZ<-paste(substring(site.data.sp$REEF_ZONE,1,1), substring(site.data.sp$DEPTH_BIN,1,1), sep="")

#ADD SAMPLING REGION:REgion to consider whether or not it got sampled
site.data.gen=site.data.gen %>%
  mutate(SAMPLING_REGION= case_when(
    ISLAND =="Wake" ~ "MARIAN",
    TRUE ~ as.character(REGION)))
site.data.tax=site.data.tax %>%
  mutate(SAMPLING_REGION= case_when(
    ISLAND =="Wake" ~ "MARIAN",
    TRUE ~ as.character(REGION)))
site.data.sp=site.data.sp %>%
  mutate(SAMPLING_REGION= case_when(
    ISLAND =="Wake" ~ "MARIAN",
    TRUE ~ as.character(REGION)))
# table(site.data.gen$SAMPLING_REGION)
# table(site.data.gen$REGION)


# ---- Section 2: Modify three site datasets for Viztool planned Pooling ----
# Final clean up before pooling ???? Can we do this/should we before the COMPLETE/TRENDS split???? "site.data.gen"

#Change island name for Alamagan, Guguan and Sarigan to SGA- small islands never sampled adequately enough & mirror's fish data
site.data.gen$ISLAND<-ifelse(site.data.gen$ISLAND %in% c("Alamagan","Guguan","Sarigan"),"SGA",as.character(site.data.gen$ISLAND)) #Combine islands
site.data.tax$ISLAND<-ifelse(site.data.tax$ISLAND %in% c("Alamagan","Guguan","Sarigan"),"SGA",as.character(site.data.tax$ISLAND)) #Combine islands
site.data.sp$ISLAND<-ifelse(site.data.sp$ISLAND %in% c("Alamagan","Guguan","Sarigan"),"SGA",as.character(site.data.sp$ISLAND)) #Combine islands

site.data.gen <- site.data.gen %>% mutate(REGION= case_when(
  ISLAND =="Guam" ~ "GUA",
  REGION == "MARIAN" & ISLAND !="Guam" ~ "CNMI",
  TRUE ~ REGION))
site.data.tax <- site.data.tax %>% mutate(REGION= case_when(
  ISLAND =="Guam" ~ "GUA",
  REGION == "MARIAN" & ISLAND !="Guam" ~ "CNMI",
  TRUE ~ REGION))
site.data.sp <- site.data.sp %>% mutate(REGION= case_when(
  ISLAND =="Guam" ~ "GUA",
  REGION == "MARIAN" & ISLAND !="Guam" ~ "CNMI",
  TRUE ~ REGION))
site.data.gen <- site.data.gen %>% mutate(REGION_NAME= case_when(
  ISLAND =="Guam" ~ "Guam",
  REGION == "CNMI"~ "Commonwealth of the Northern Mariana Islands",
  TRUE ~ REGION_NAME))
site.data.tax <- site.data.tax %>% mutate(REGION_NAME= case_when(
  ISLAND =="Guam" ~ "Guam",
  REGION == "CNMI"~ "Commonwealth of the Northern Mariana Islands",
  TRUE ~ REGION_NAME))
site.data.sp <- site.data.sp %>% mutate(REGION_NAME= case_when(
  ISLAND =="Guam" ~ "Guam",
  REGION == "CNMI"~ "Commonwealth of the Northern Mariana Islands",
  TRUE ~ REGION_NAME))

#Remove NWHI islands only surveyed by PMNM and not sampled well
remove<-c("Laysan","Maro","Midway")
site.data.gen<-dplyr::filter(site.data.gen, !PooledSector_Viztool %in% remove)
site.data.tax<-dplyr::filter(site.data.tax, !PooledSector_Viztool %in% remove)
site.data.sp<-dplyr::filter(site.data.sp, !PooledSector_Viztool %in% remove)

# #Remove PRIA 2016 and 2017 surveys- done off cycle for the bleaching response, and do not have all metrics, but keep Wake
site.data.gen$REGION_YEAR<-paste(site.data.gen$REGION,site.data.gen$ANALYSIS_YEAR,sep = "_")
site.data.gen$REGION_YEAR<-ifelse((site.data.gen$ISLAND=="Wake" & site.data.gen$ANALYSIS_YEAR=="2017"),"PRIAs_2017w",site.data.gen$REGION_YEAR) #This will help you keep wake 2017 data
site.data.tax$REGION_YEAR<-paste(site.data.tax$REGION,site.data.tax$ANALYSIS_YEAR,sep = "_")
site.data.tax$REGION_YEAR<-ifelse((site.data.tax$ISLAND=="Wake" & site.data.tax$ANALYSIS_YEAR=="2017"),"PRIAs_2017w",site.data.tax$REGION_YEAR) #This will help you keep wake 2017 data
site.data.sp$REGION_YEAR<-paste(site.data.sp$REGION,site.data.sp$ANALYSIS_YEAR,sep = "_")
site.data.sp$REGION_YEAR<-ifelse((site.data.sp$ISLAND=="Wake" & site.data.sp$ANALYSIS_YEAR=="2017"),"PRIAs_2017w",site.data.sp$REGION_YEAR) #This will help you keep wake 2017 data

remove<-c("PRIAs_2016","PRIAs_2017")
site.data.gen<-dplyr::filter(site.data.gen, !REGION_YEAR %in% remove)
site.data.tax<-dplyr::filter(site.data.tax, !REGION_YEAR %in% remove)
site.data.sp<-dplyr::filter(site.data.sp, !REGION_YEAR %in% remove)

####Leave Guam MPA in the Complete Data
#site.data.gen<-dplyr::filter(site.data.gen, !(PooledSector_Viztool == "GUA_MP" & ANALYSIS_YEAR == "2017"))

#Change Analysis year for PRIAs- you will need to do this for regional estiamtes that include both wake (2014,2017) and other PRIAs (2015 and 2018)
site.data.gen$ANALYSIS_YEAR<-ifelse(site.data.gen$REGION_YEAR %in% c("PRIAs_2014","PRIAs_2015"),"2014-15",as.character(site.data.gen$ANALYSIS_YEAR))
site.data.gen$ANALYSIS_YEAR<-ifelse(site.data.gen$REGION_YEAR %in% c("PRIAs_2017w","PRIAs_2017","PRIAs_2018"),"2017-18",as.character(site.data.gen$ANALYSIS_YEAR))
site.data.tax$ANALYSIS_YEAR<-ifelse(site.data.tax$REGION_YEAR %in% c("PRIAs_2014","PRIAs_2015"),"2014-15",as.character(site.data.tax$ANALYSIS_YEAR))
site.data.tax$ANALYSIS_YEAR<-ifelse(site.data.tax$REGION_YEAR %in% c("PRIAs_2017w","PRIAs_2017","PRIAs_2018"),"2017-18",as.character(site.data.tax$ANALYSIS_YEAR))
site.data.sp$ANALYSIS_YEAR<-ifelse(site.data.sp$REGION_YEAR %in% c("PRIAs_2014","PRIAs_2015"),"2014-15",as.character(site.data.sp$ANALYSIS_YEAR))
site.data.sp$ANALYSIS_YEAR<-ifelse(site.data.sp$REGION_YEAR %in% c("PRIAs_2017w","PRIAs_2017","PRIAs_2018"),"2017-18",as.character(site.data.sp$ANALYSIS_YEAR))

##Change Protected Reef Slope to Forereef- we do this for some data requests
# site.data.gen$REEF_ZONE<-ifelse(site.data.gen$REEF_ZONE %in% c("Protected Slope","Forereef"),"Forereef",as.character(site.data.gen$REEF_ZONE)) #combine PRS and forereef similar to what fish team does

# ---- Section 3: Determine stable sampling, return COMPLETE and TRENDS datasets ----
#2023 MARCH - SUBSET FOR TEMPORALLY COHERENT STRATA LEVEL SAMPLING - NO LESS THAN 2 SAMPLES PER STRATA THE WHOLE TIME
#Use calculations from GENUS level for other taxa
strat2drop=site.data.gen %>% filter(GENUS_CODE=="SSSS")

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
singleton_strata=site.data.gen %>% 
  filter(GENUS_CODE=="SSSS") %>% 
  group_by(STRATANAME,ANALYSIS_YEAR) %>% 
  count() %>% filter(n==1) %>% 
  mutate(STR_YR=paste0(STRATANAME,"_",ANALYSIS_YEAR)) %>% pull(STR_YR)

#Complete Site.data.xxx
site.data.gen=site.data.gen %>% 
  mutate(STR_YR=paste0(STRATANAME,"_",ANALYSIS_YEAR)) %>% 
  filter(!STR_YR%in%singleton_strata)
site.data.tax=site.data.tax %>% 
  mutate(STR_YR=paste0(STRATANAME,"_",ANALYSIS_YEAR)) %>% 
  filter(!STR_YR%in%singleton_strata)
site.data.sp=site.data.sp %>% 
  mutate(STR_YR=paste0(STRATANAME,"_",ANALYSIS_YEAR)) %>% 
  filter(!STR_YR%in%singleton_strata)

#Which unpooled stratanames have no sampling or N=1 in regionally sampled years,
#or islands with special year sampling
#Drop the poorly sampled strata and carry the new data.frame forward
site.data.gen.trends=site.data.gen %>%
  filter(!STRATANAME_TRENDS%in%DropSTR)
site.data.tax.trends=site.data.tax %>%
  filter(!STRATANAME_TRENDS%in%DropSTR)
site.data.sp.trends=site.data.sp %>%
  filter(!STRATANAME_TRENDS%in%DropSTR)

## Check Dropped Stratanames
# t=table(sort(apply(table(strat2drop$STRATANAME_TRENDS,strat2drop$ANALYSIS_YEAR),1,min_pos)))
# t/sum(t)
NODROPSTR=site.data.gen %>% filter(GENUS_CODE=="SSSS") %>% 
  group_by(STRATANAME,ANALYSIS_YEAR) %>% count() %>% group_by(STRATANAME) %>% summarize(Nmin=min(n))
DROPSTR=site.data.gen.trends %>% filter(GENUS_CODE=="SSSS") %>%
  group_by(STRATANAME_TRENDS,ANALYSIS_YEAR) %>% count() %>% group_by(STRATANAME_TRENDS) %>% summarize(Nmin=min(n))
drops.df=data.frame(STRATANAME=DropSTR,DROPIT=DropSTR)
left_join(left_join(NODROPSTR,DROPSTR,by=c("STRATANAME"="STRATANAME_TRENDS")),drops.df)

#Moving ahead from here with both "site.data.xxx" and the strata dropped "site.data.xxx.trends"

#QC CHECK to make sure the sectors and strata pooled correctly
data.test<-ddply(subset(site.data.gen,GENUS_CODE=="SSSS"),.(REGION,PooledSector_Viztool,OBS_YEAR,STRATANAME),summarize,n=length(SITE))
data.test.trends<-ddply(subset(site.data.gen.trends,GENUS_CODE=="SSSS"),.(REGION,PooledSector_Viztool,OBS_YEAR,STRATANAME),summarize,n=length(SITE))
sm.test<-ddply(subset(survey_master,Benthic=="1"&EXCLUDE_FLAG=="0"&OBS_YEAR>=2013),.(REGION,ISLAND,SEC_NAME,OBS_YEAR,REEF_ZONE,DEPTH_BIN),summarize,n=length(SITE))
write.csv(data.test,"tmp_sitedataQC.csv")
write.csv(data.test.trends,"tmp_sitedatatrendsQC.csv")
write.csv(sm.test,"tmp_sitemasterQC.csv")

# #Subset just Forereef Sites & just target taxa
# site.data.gen<-subset(site.data.gen,REEF_ZONE=="Forereef")
# site.data.gen<-subset(site.data.gen,GENUS_CODE %in% c("ACSP", "MOSP", "PAVS", "POCS","POSP","SSSS"))

# #Make sure you everything but forereef are dropped
# table(site.data.gen$REEF_ZONE,site.data.gen$GENUS_CODE)

# ---- Section 4: Run STR,SEC,ISL,REG roll-ups for  COMPLETE and TRENDS datasets ----

#GENUS_CODE complete
st.data.gen<-Calc_Strata_Metrics_25(site_data = site.data.gen,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema="PooledSector_Viztool")
sec.data.gen<-Calc_IslandorSector_Metrics_25(site_data = site.data.gen,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema="PooledSector_Viztool")
is.data.gen<-Calc_IslandorSector_Metrics_25(site.data.gen,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema="ISLAND")
r.data.gen<-Calc_Region_Metrics_25(site.data.gen,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema = "REGION")
#GENUS_CODE trends 
st.data.gen.trends<-Calc_Strata_Metrics_25(site.data.gen.trends,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema="PooledSector_Viztool")
sec.data.gen.trends<-Calc_IslandorSector_Metrics_25(site.data.gen.trends,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema="PooledSector_Viztool")
is.data.gen.trends<-Calc_IslandorSector_Metrics_25(site.data.gen.trends,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema="ISLAND")
#Drop Special Sampling Years
removePRIA1617<-c("PRIAs_2016","PRIAs_2017")
site.data.gen.trends_nosp<-dplyr::filter(site.data.gen.trends, !REGION_YEAR %in% removePRIA1617)
r.data.gen.trends<-Calc_Region_Metrics_25(site.data.gen.trends_nosp,grouping_field="GENUS_CODE",a_schema ="STRATANAME",d_schema = "REGION")

#TAXONCODE complete
st.data.tax<-Calc_Strata_Metrics_25(site_data = site.data.tax,grouping_field="TAXONCODE",a_schema ="STRATANAME",d_schema="PooledSector_Viztool")
sec.data.tax<-Calc_IslandorSector_Metrics_25(site_data = site.data.tax,grouping_field="TAXONCODE",a_schema ="STRATANAME",d_schema="PooledSector_Viztool")
is.data.tax<-Calc_IslandorSector_Metrics_25(site.data.tax,grouping_field="TAXONCODE",a_schema ="STRATANAME",d_schema="ISLAND")
r.data.tax<-Calc_Region_Metrics_25(site.data.tax,grouping_field="TAXONCODE",a_schema ="STRATANAME",d_schema = "REGION")
#TAXONCODE trends 
st.data.tax.trends<-Calc_Strata_Metrics_25(site.data.tax.trends,grouping_field="TAXONCODE",a_schema ="STRATANAME",d_schema="PooledSector_Viztool")
sec.data.tax.trends<-Calc_IslandorSector_Metrics_25(site.data.tax.trends,grouping_field="TAXONCODE",a_schema ="STRATANAME",d_schema="PooledSector_Viztool")
is.data.tax.trends<-Calc_IslandorSector_Metrics_25(site.data.tax.trends,grouping_field="TAXONCODE",a_schema ="STRATANAME",d_schema="ISLAND")
#Drop Special Sampling Years
removePRIA1617<-c("PRIAs_2016","PRIAs_2017")
site.data.tax.trends_nosp<-dplyr::filter(site.data.tax.trends, !REGION_YEAR %in% removePRIA1617)
r.data.tax.trends<-Calc_Region_Metrics_25(site.data.tax.trends_nosp,grouping_field="TAXONCODE",a_schema ="STRATANAME",d_schema = "REGION")

#SPCODE complete
st.data.sp<-Calc_Strata_Metrics_25(site_data = site.data.sp,grouping_field="SPCODE",a_schema ="STRATANAME",d_schema="PooledSector_Viztool")
sec.data.sp<-Calc_IslandorSector_Metrics_25(site_data = site.data.sp,grouping_field="SPCODE",a_schema ="STRATANAME",d_schema="PooledSector_Viztool")
is.data.sp<-Calc_IslandorSector_Metrics_25(site.data.sp,grouping_field="SPCODE",a_schema ="STRATANAME",d_schema="ISLAND")
r.data.sp<-Calc_Region_Metrics_25(site.data.sp,grouping_field="SPCODE",a_schema ="STRATANAME",d_schema = "REGION")
#SPCODE trends 
st.data.sp.trends<-Calc_Strata_Metrics_25(site.data.sp.trends,grouping_field="SPCODE",a_schema ="STRATANAME",d_schema="PooledSector_Viztool")
sec.data.sp.trends<-Calc_IslandorSector_Metrics_25(site.data.sp.trends,grouping_field="SPCODE",a_schema ="STRATANAME",d_schema="PooledSector_Viztool")
is.data.sp.trends<-Calc_IslandorSector_Metrics_25(site.data.sp.trends,grouping_field="SPCODE",a_schema ="STRATANAME",d_schema="ISLAND")
#Drop Special Sampling Years
removePRIA1617<-c("PRIAs_2016","PRIAs_2017")
site.data.sp.trends_nosp<-dplyr::filter(site.data.sp.trends, !REGION_YEAR %in% removePRIA1617)
r.data.sp.trends<-Calc_Region_Metrics_25(site.data.sp.trends_nosp,grouping_field="SPCODE",a_schema ="STRATANAME",d_schema = "REGION")



# ---- Section 5: Write Out All  STR,SEC,ISL,REG roll-ups for  COMPLETE and TRENDS datasets ----
TDriveOut="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/"
VizToolOut="T:/Benthic/Data/Data Requests/NCRMPViztool/2024/unformatted/"

OutDirectories=c(TDriveOut,VizToolOut)
TaxonRank=c("gen","tax","sp");names(TaxonRank)=c("GENUS_CODE","TAXONCODE","SPCODE")
RollUpLevel=c("site","st","sec","is","r");names(RollUpLevel)=c("SITE","STRATA","SECTOR","ISLAND","REGION")
CompORTrends=c("",".trends");names(CompORTrends)=c("COMPLETE","TRENDS")
OUTPUTYEARS="2013_2024"
for (OUTi in OutDirectories){
  for (TRi in names(TaxonRank)){
    for (RUi in names(RollUpLevel)){
      for (CTi in names(CompORTrends)){
        print(paste0("Writing out ",CTi," data at ",RUi,"-Level and ",TRi," rank to ",OUTi))
        #sets appropriate data.frame to "out.dat"
        out.dat.expr=paste0("out.dat=",RollUpLevel[RUi],".data.",TaxonRank[TRi],CompORTrends[CTi]);
        #old:
        eval(parse(text=out.dat.expr))
        #new:rlang::eval_tidy(rlang::parse_expr(out.dat.expr))
        
        #generates appropriate filename
        outfile=paste0("BenthicREA_",RUi,"_",TRi,"_",CTi,"_",OUTPUTYEARS,".csv")
        outpath=paste0(OUTi,outfile)
        #Write out
        write.csv(out.dat,file=outpath,row.names=F)
        print(paste0("*****   Done With: ",outpath))
      }
    }
  }
}



QC=FALSE
if (QC){
#QC Checks
st.data.gen=read.csv(file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/COMPLETE/BenthicREA_STRATA_Demo_Viztool_2024.csv")
sec.data.gen=read.csv(file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/COMPLETE/BenthicREA_SECTOR_Demo_Viztool_2024.csv")
is.data.gen=read.csv(file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/COMPLETE/BenthicREA_ISLAND_Demo_Viztool_2024.csv")
r.data.gen=read.csv(file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/COMPLETE/BenthicREA_REGION_Demo_Viztool_2024.csv")
st.data.gen.trends=read.csv(file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicREA_STRATA_TRENDS_Demo_Viztool_2024.csv")
sec.data.gen.trends=read.csv(file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicREA_SECTOR_TRENDS_Demo_Viztool_2024.csv")
is.data.gen.trends=read.csv(file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicREA_ISLAND_TRENDS_Demo_Viztool_2024.csv")
r.data.gen.trends=read.csv(file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicREA_REGION_TRENDS_Demo_Viztool_2024.csv")

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
}
