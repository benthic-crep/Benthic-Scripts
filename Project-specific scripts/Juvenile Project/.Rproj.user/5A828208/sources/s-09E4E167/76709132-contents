#This script summarizes juvenile data from NCRMP 2013-2019 at the site, stratum, island and sector level
#It also identifies which sectors and strata have been surveyed in all years
#It calculate delta density


rm(list=ls())

#Set Run Flags
DEBUG=TRUE

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")


library(rcompanion)
library(car)
library("TeachingDemos")
library(gridExtra)
library(RColorBrewer)
library(viridis)
library(dplyr)
library(sp)
library(sf)
library(ggsn)
library(ggspatial)
library(ggrepel)
library(rnaturalearth)
library(rgeos)
library(cowplot)


setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project")

#LOAD DATA
jwd<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Juveniles_raw_CLEANED.csv")



## LOAD data
# site.data.gen2<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS.csv")

#Tweaks before calculating Site-level data-------------------------------------------------
#Colony fragments and scleractinans are subseted in the functions 
#Add a column for adult fragments so we can remove them from the dataset later (-1 indicates fragment)
jwd$Fragment <- 0 # you need to add this column so that you can use the site level functions correctly
jwd$DATE_ <- as.Date(jwd$DATE_, format = "%Y-%m-%d")
jwd$METHOD<-"DIVER"
jwd$ANALYST<-jwd$DIVER
jwd$SEGAREA<-jwd$SEGLENGTH*jwd$SEGWIDTH

#Create a look a table of all of the colony attributes- you will need this the functions below
SURVEY_SITE<-c("METHOD","MISSIONID","DATE_","SITEVISITID", "ANALYSIS_YEAR","OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE","HABITAT_CODE","REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")
survey_site<-unique(jwd[,SURVEY_SITE])

#TEMPORARY WORK AROUND-ASK MICHAEL TO FIX
jwd$REEF_ZONE<-ifelse(jwd$SITE=="HAW-04285","Forereef",as.character(jwd$REEF_ZONE))

#Convert Protected Slope to Forereef
jwd$REEF_ZONE<-ifelse(jwd$REEF_ZONE=="Protected Slope","Forereef",as.character(jwd$REEF_ZONE))

#Remove 2 sites that weren't surveyed for juveniles
jwd<-jwd[!(jwd$SITE %in% c("OFU-01012","PAG-00596")),]


# 
# #Look at the size class data to determine the min colony size cut off for analysis
# #Subset 2019 Hawaii data
# hi<-subset(jwd,OBS_YEAR=="2017")
# 
# ggplot(hi) + 
#   geom_density(aes(x = COLONYLENGTH, fill = ANALYST), alpha = 0.2)+
#   geom_vline(xintercept=1, color = "black")+
#   facet_wrap(~ISLAND)
# 
# ggplot(hi) + 
#   geom_histogram(aes(x = COLONYLENGTH, fill = ANALYST))+
#   geom_vline(xintercept=1, color = "black")+
#   facet_wrap(~ISLAND)
# 
# ggplot(hi) + 
#   geom_density(aes(x = COLONYLENGTH, fill = ISLAND), alpha = 0.2)+
#   geom_vline(xintercept=1, color = "black")+
#   facet_wrap(~ISLAND)
# 
# s.data<-ddply(hi,.(ISLAND,ANALYST),
#               summarise,
#               min=min(COLONYLENGTH,na.rm=T),
#               mean=mean(COLONYLENGTH,na.rm=T),
#               ncol=length(COLONYLENGTH,na.rm=T))
# 
# s.all<-ddply(jwd,.(ANALYST),
#              summarise,
#              min=min(COLONYLENGTH,na.rm=T),
#              mean=mean(COLONYLENGTH,na.rm=T),
#              ncol=length(COLONYLENGTH))

#There are some people that didn't record anything less than 1cm

#Change colonies that are <1cm to NA. I'm not subsetting these data because I need to keep the placeholder in the dataframe in case a site only had colonies <1cm or >5cm
View(subset(jwd,COLONYLENGTH<1))
nrow(subset(jwd,COLONYLENGTH<1))
nrow(jwd)
jwd$S_ORDER<-ifelse(jwd$COLONYLENGTH<1|jwd$COLONYLENGTH==5,NA,as.character(jwd$S_ORDER))
jwd$GENUS_CODE<-ifelse(jwd$COLONYLENGTH<1|jwd$COLONYLENGTH==5,"AAAA",as.character(jwd$GENUS_CODE))
jwd$TAXONCODE<-ifelse(jwd$COLONYLENGTH<1|jwd$COLONYLENGTH==5,"AAAA",as.character(jwd$TAXONCODE))
jwd$SPCODE<-ifelse(jwd$COLONYLENGTH<1|jwd$COLONYLENGTH==5,"AAAA",as.character(jwd$SPCODE))
jwd$COLONYLENGTH<-ifelse(jwd$COLONYLENGTH<1|jwd$COLONYLENGTH==5,NA,jwd$COLONYLENGTH)
jwd$TAXONNAME<-ifelse(jwd$COLONYLENGTH<1|jwd$COLONYLENGTH==5,NA,as.character(jwd$TAXONNAME))

nrow(subset(jwd,COLONYLENGTH>1))
View(subset(jwd,COLONYLENGTH>1))

nrow(jwd)
View(jwd)


# Generate Juvenile Density at the TRANSECT & SITE-LEVEL BY GENUS--------------------------------------------------
jcd.gen<-Calc_ColDen_Transect(jwd,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen";colnames(jcd.gen)[colnames(jcd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_j"

#Drop 2nd transect since we only surveyed 1 transect after 2017 and the 2nd transect wasn't always surveyed consistently prior to 2018
jcd.gen$TRANSECT<-as.factor(jcd.gen$TRANSECT)
site.data.gen<-subset(jcd.gen,TRANSECT %in% c("1","3")) #subseting first transect

site.data.gen2<-subset(site.data.gen, select= -c(TRANSECT)) #Create a copy (it takes a long time to run the ddply function above)

# Merge Site level data with sectors file and export site data ------------
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

#Merge together survey meta data and sector area files and check for missmatches 
meta<-left_join(survey_site,sectors)
meta[which(is.na(meta$AREA_HA)),]
nrow(survey_site)
nrow(meta)


#Merge site level data and meta data
site.data.gen2<-left_join(site.data.gen2,meta)
site.data.gen2$Juvpres.abs<-ifelse(site.data.gen2$JuvColDen>0,1,0)


#Change all special missions to exclude flag =-1, right now they are 0. Then exclude these sites
#Exclude PRIA 2017 sites because we want similar time intervals following bleaching events for all regions
levels(site.data.gen2$MISSIONID)
site.data.gen2<-site.data.gen2[!site.data.gen2$MISSIONID %in% c("MP1410","MP1512","MP1602","SE1602","MP2006"),]
site.data.gen2$Year_Island<-paste(site.data.gen2$OBS_YEAR,site.data.gen2$ISLAND,sep="_")
site.data.gen2<-site.data.gen2[!site.data.gen2$Year_Island %in% c("2017_Baker","2017_Jarvis","2017_Howland"),] 

site.data.gen2<-droplevels(site.data.gen2);levels(site.data.gen2$MISSIONID)
View(site.data.gen2)

#Convert Protected Reef Slope to Forereef and Subset just Forereef sites
site.data.gen2$REEF_ZONE<-ifelse(site.data.gen2$REEF_ZONE=="Protected Slope","Forereef",as.character(site.data.gen2$REEF_ZONE))
site.data.gen2<-subset(site.data.gen2,REEF_ZONE=="Forereef")

#Remove Johnston from analysis- we only have 2015 data
site.data.gen2<-subset(site.data.gen2,ISLAND!="Johnston")


#Change Regions
site.data.gen2$REGION<-ifelse(site.data.gen2$ISLAND %in% c("FDP", "Maug", "Asuncion", "Alamagan", "Pagan", "Agrihan", "Guguan", "Sarigan","Farallon_de_Pajaros")
                              ,"NMARIAN", as.character(site.data.gen2$REGION))
site.data.gen2$REGION<-ifelse(site.data.gen2$ISLAND %in% c("Saipan", "Tinian", "Aguijan", "Rota", "Guam")
                              ,"SMARIAN", as.character(site.data.gen2$REGION))
site.data.gen2$REGION<-ifelse(site.data.gen2$ISLAND %in% c("Howland","Baker")
                              ,"PHOENIX", as.character(site.data.gen2$REGION))
site.data.gen2$REGION<-ifelse(site.data.gen2$ISLAND =="Wake"
                              ,"WAKE", as.character(site.data.gen2$REGION))
site.data.gen2$REGION<-ifelse(site.data.gen2$ISLAND %in% c("Kingman","Palmyra","Jarvis")
                              ,"LINE", as.character(site.data.gen2$REGION))

site.data.gen2$STRATANAME<- paste(site.data.gen2$SEC_NAME,site.data.gen2$REEF_ZONE,site.data.gen2$DEPTH_BIN,sep="_")
site.data.gen2$REGION_YEAR<-paste(site.data.gen2$REGION,site.data.gen2$OBS_YEAR,sep="_")

#Remove the 2013 MHI and 2014 NWHI data
site.data.gen3<-site.data.gen2[!site.data.gen2$REGION_YEAR %in% c("NWHI_2014","MHI_2013"),]

table(site.data.gen2$REGION,site.data.gen2$OBS_YEAR)
table(site.data.gen3$REGION,site.data.gen3$OBS_YEAR)


# 
# # Summarize Post bleaching data for driver analysis -----------------------
# 
# #Subset post bleaching regions and years for downstream driver analysis
# REGION_YEAR<-c("MHI_2019","NWHI_2017","NMARIAN_2017","SMARIAN_2017","PHOENIX_2018","LINE_2018","SAMOA_2018")
# 
# site.data.gen2$REGION_YEAR<-paste(site.data.gen2$REGION,site.data.gen2$OBS_YEAR,sep="_")
# 
# data.gen_pb<-site.data.gen2[site.data.gen2$REGION_YEAR %in% REGION_YEAR,]
# head(data.gen_pb)
# table(data.gen_pb$REGION,data.gen_pb$OBS_YEAR)
# table(data.gen_pb$ISLAND,data.gen_pb$OBS_YEAR)
# 
# #Remove strata that have less than 2 sites/stratum
# st.list<-ddply(data.gen_pb,.(OBS_YEAR,REGION,ISLAND,SEC_NAME,STRATANAME),summarize,n=length(unique(SITE)))
# st.list2<-subset(st.list,n>=2);head(st.list);st.list2<-droplevels(st.list2)
# data.gen_pb<-data.gen_pb[data.gen_pb$STRATANAME %in% c(data.gen_pb$STRATANAME),]
# 
# View(data.gen_pb) #double check that strata were dropped correctly
# 
# #POOL UP
# #Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
# data.gen_pb$ANALYSIS_SCHEMA<-data.gen_pb$STRATANAME
# data.gen_pb$DOMAIN_SCHEMA<-data.gen_pb$ISLAND
# data.gen_pb$ANALYSIS_YEAR<-data.gen_pb$OBS_YEAR
# data.gen_pb$DB_RZ<-paste(data.gen_pb$DEPTH_BIN,data.gen_pb$REEF_ZONE,sep="_")
# 
# #Create a vector of columns to subset for strata estimates
# c.keep<-c("REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
#           "n_h","N_h","D._h","SE_D._h")
# 
# jcdG_st<-Calc_Strata(data.gen_pb,"GENUS_CODE","JuvColDen","Juvpres.abs");jcdG_st=jcdG_st[,c.keep]
# colnames(jcdG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","STRATANAME","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","JuvColDen","SE_JuvColDen")
# 
# #Double Check that revised pooling is adding up NH (total sites) correctly
# View(jcdG_st)
# View(sectors)
# 
# 
# #Calculate Island Estimates
# jcdG_is<-Calc_Domain(data.gen_pb,"GENUS_CODE","JuvColDen","Juvpres.abs")
# jcdG_is<-jcdG_is[,c("REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen")]
# 
# #Calculate Sector Estimates
# data.gen_pb$ANALYSIS_SCHEMA<-data.gen_pb$STRATANAME
# data.gen_pb$DOMAIN_SCHEMA<-data.gen_pb$SEC_NAME
# 
# jcdG_sec<-Calc_Domain(data.gen_pb,"GENUS_CODE","JuvColDen","Juvpres.abs")
# jcdG_sec<-jcdG_sec[,c("REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen")]
# 
# 
# write.csv(data.gen_pb,file="T:/Benthic/Projects/Juvenile Project/JuvProject_pb_SITE.csv")
# write.csv(jcdG_st,file="T:/Benthic/Projects/Juvenile Project/JuvProject_pb_STRATA.csv")
# write.csv(jcdG_is,file="T:/Benthic/Projects/Juvenile Project/JuvProject_pb_ISLAND.csv")
# write.csv(jcdG_sec,file="T:/Benthic/Projects/Juvenile Project/JuvProject_pb_SECTOR.csv")
# 




# CALCULATE DeltaDensity/Year for Correlative Analysis --------------------
#To remove the confounding effect of time, Calculate change since bleaching event or year following bleaching event
#I decided to create new list of strata without 2013 MHI sites- terrible sampling and delta den for MHI is calcualted 2019-2016

# GENERATE DATA FOR TEMPORAL ANALYSIS WITH 2013 MHI---------------------------------------------------
site.data.gen2$STRATANAME<- paste(site.data.gen2$SEC_NAME,site.data.gen2$REEF_ZONE,site.data.gen2$DEPTH_BIN,sep="_")
st.list<-ddply(site.data.gen2,.(METHOD,OBS_YEAR,REGION,ISLAND,SEC_NAME,STRATANAME),summarize,n=length(unique(SITE)))
st.list2<-subset(st.list,n>=2);head(st.list) #drop strata that have < 2 sites/stratum

#Generate list of strata that were surveyed in all years for a given region and had at least 2 sites/stratum
st.list_w<-dcast(st.list2, formula=METHOD+REGION+ISLAND+SEC_NAME+STRATANAME~ OBS_YEAR, value.var="n",fill=0)
dCOLS<-c("2013","2014","2015","2016","2017","2018","2019")
st.list_w$year_n<-rowSums(st.list_w[,dCOLS] > 0, na.rm=T) #count # of years of data
st.list_w2<-subset(st.list_w,REGION %in% c("NMARIAN","SMARIAN","LINE","PHOENIX","WAKE","SAMOA") & year_n>=2)
st.list_w3<-subset(st.list_w,REGION %in% c("NWHI","MHI") & year_n>=3)
st.list_w4<-rbind(st.list_w2,st.list_w3)

head(st.list_w4);st.list_w4<-droplevels(st.list_w4) #generate the list

data.gen_temp<-site.data.gen2[site.data.gen2$STRATANAME %in% c(st.list_w4$STRATANAME),] #Subset juv data to only include strata of intersest 

View(data.gen_temp) #double check that strata were dropped correctly


#POOL UP TO STRATA, SECTOR, ISLAND
#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
data.gen_temp$ANALYSIS_SCHEMA<-data.gen_temp$STRATANAME
data.gen_temp$DOMAIN_SCHEMA<-data.gen_temp$SEC_NAME 
data.gen_temp$ANALYSIS_YEAR<-data.gen_temp$OBS_YEAR
data.gen_temp$DB_RZ<-paste(data.gen_temp$DEPTH_BIN,data.gen_temp$REEF_ZONE,sep="_")

#Create a vector of columns to subset for strata estimates
c.keep<-c("METHOD","REGION","ISLAND","DOMAIN_SCHEMA","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
          "n_h","N_h","D._h","SE_D._h")

jcdG_st<-Calc_Strata(data.gen_temp,"GENUS_CODE","JuvColDen","Juvpres.abs");jcdG_st=jcdG_st[,c.keep]
colnames(jcdG_st)<-c("METHOD","REGION","ISLAND","SEC_NAME","ANALYSIS_YEAR","STRATANAME","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","JuvColDen","SE_JuvColDen")
jcdG_st$ANALYSIS_YEAR<-as.factor(jcdG_st$ANALYSIS_YEAR)
head(jcdG_st)

jcdG_st<-cSplit(jcdG_st, 'DB_RZ', sep="_", type.convert=FALSE);colnames(jcdG_st)[colnames(jcdG_st)=="DB_RZ_1"]<-"DEPTH_BIN"
jcdG_stS<-subset(jcdG_st,GENUS_CODE=="SSSS")


write.csv(jcdG_st,file="T:/Benthic/Projects/Juvenile Project/JuvProject_STRATA_WITH_MHI2013.csv")



# GENERATE DENSITY DATA FOR TEMPORAL ANALYSIS WITHOUT 2013 MHI at stratum and regional level---------------------------------------------------
site.data.gen3$STRATANAME<- paste(site.data.gen3$SEC_NAME,site.data.gen3$REEF_ZONE,site.data.gen3$DEPTH_BIN,sep="_")
st.list<-ddply(site.data.gen3,.(METHOD,OBS_YEAR,REGION,ISLAND,SEC_NAME,STRATANAME),summarize,n=length(unique(SITE)))
st.list2<-subset(st.list,n>=2);head(st.list) #Make sure each stratum has at least 2 sites

#Generate list of strata that were surveyed in all years for a given region and had at least 2 sites/stratum
st.list_w<-dcast(st.list2, formula=METHOD+REGION+ISLAND+SEC_NAME+STRATANAME~ OBS_YEAR, value.var="n",fill=0)
dCOLS<-c("2014","2015","2016","2017","2018","2019")
st.list_w$year_n<-rowSums(st.list_w[,dCOLS] > 0, na.rm=T) #count # of years of data
st.list_w2<-subset(st.list_w,REGION %in% c("NMARIAN","SMARIAN","LINE","PHOENIX","WAKE","SAMOA","MHI") & year_n>=2)
st.list_w3<-subset(st.list_w,REGION %in% c("NWHI") & year_n>=3)
st.list_w4<-rbind(st.list_w2,st.list_w3)

head(st.list_w4);st.list_w4<-droplevels(st.list_w4) #generate the list

data.gen_temp<-site.data.gen3[site.data.gen3$STRATANAME %in% c(st.list_w4$STRATANAME),] #Subset juv data to only include strata of intersest 

###STRATA LEVEL-unweighted
#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
data.gen_temp$ANALYSIS_SCHEMA<-data.gen_temp$STRATANAME
data.gen_temp$DOMAIN_SCHEMA<-data.gen_temp$SEC_NAME 
data.gen_temp$ANALYSIS_YEAR<-data.gen_temp$OBS_YEAR
data.gen_temp$DB_RZ<-paste(data.gen_temp$DEPTH_BIN,data.gen_temp$REEF_ZONE,sep="_")

#Create a vector of columns to subset for strata estimates
c.keep<-c("METHOD","REGION","ISLAND","DOMAIN_SCHEMA","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
          "n_h","N_h","D._h","varD._h","SE_D._h")

jcdG_st<-Calc_Strata(data.gen_temp,"GENUS_CODE","JuvColDen","Juvpres.abs");jcdG_st=jcdG_st[,c.keep]
colnames(jcdG_st)<-c("METHOD","REGION","ISLAND","SEC_NAME","ANALYSIS_YEAR","STRATANAME","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","JuvColDen","Var_JuvColDen","SE_JuvColDen")
jcdG_st$ANALYSIS_YEAR<-as.factor(jcdG_st$ANALYSIS_YEAR)
head(jcdG_st)

jcdG_st<-cSplit(jcdG_st, 'DB_RZ', sep="_", type.convert=FALSE);colnames(jcdG_st)[colnames(jcdG_st)=="DB_RZ_1"]<-"DEPTH_BIN"
jcdG_stS<-subset(jcdG_st,GENUS_CODE=="SSSS")

write.csv(jcdG_st,file="T:/Benthic/Projects/Juvenile Project/JuvProject_STRATA_WITHOUT_MHI2013.csv")




#Identify density data for first and last time point-merge this with the delta density dataframe
#These data may be use as predictors in the correlative analysis 
REGION_YEAR<-c("MHI_2019","NWHI_2017","NMARIAN_2017","SMARIAN_2017","PHOENIX_2018","LINE_2018","SAMOA_2018","WAKE_2017")
jcdG_stS<-as.data.frame(jcdG_stS)

jcdG_stS$REGION_YEAR<-paste(jcdG_stS$REGION,jcdG_stS$ANALYSIS_YEAR,sep="_")
jcdG_stS_last<-jcdG_stS[jcdG_stS$REGION_YEAR %in% REGION_YEAR,]
head(jcdG_stS_last)
jcdG_stS_last<-subset(jcdG_stS_last,select=c(STRATANAME,JuvColDen))
colnames(jcdG_stS_last)[2]<-"T2_JuvColDen"

#Identify first year surveyed
REGION_YEAR<-c("MHI_2016","NWHI_2015","NMARIAN_2014","SMARIAN_2014","PHOENIX_2015","LINE_2015","SAMOA_2015","WAKE_2014")
jcdG_stS<-as.data.frame(jcdG_stS)

jcdG_stS$REGION_YEAR<-paste(jcdG_stS$REGION,jcdG_stS$ANALYSIS_YEAR,sep="_")
jcdG_stS_first<-jcdG_stS[jcdG_stS$REGION_YEAR %in% REGION_YEAR,]
head(jcdG_stS_first)
jcdG_stS_first<-subset(jcdG_stS_first,select=c(STRATANAME,JuvColDen))
colnames(jcdG_stS_first)[2]<-"T1_JuvColDen"


###REGIONAL LEVEL-weighted
data.gen_tempS<-subset(data.gen_temp,GENUS_CODE=="SSSS")
data.gen_tempS$ANALYSIS_SCHEMA<-data.gen_tempS$STRATANAME
data.gen_tempS$DOMAIN_SCHEMA<-data.gen_tempS$REGION 
data.gen_tempS$ANALYSIS_YEAR<-data.gen_tempS$OBS_YEAR
data.gen_tempS$DB_RZ<-paste(data.gen_tempS$DEPTH_BIN,data.gen_tempS$REEF_ZONE,sep="_")

#Create a vector of columns to subset for strata estimates
c.keep<-c("METHOD","REGION","ANALYSIS_YEAR","GENUS_CODE",
          "n","Ntot","Mean_JuvColDen","SE_JuvColDen")

jcdG_rS<-Calc_Domain_Region(data.gen_tempS,"GENUS_CODE","JuvColDen","Juvpres.abs");jcdG_rS=jcdG_rS[,c.keep]
jcdG_rS$ANALYSIS_YEAR<-as.factor(jcdG_rS$ANALYSIS_YEAR)
head(jcdG_rS)




# Calculate Weighted Strata  density -------------------------------------------------

#Calculate weighting factor for strata with REGION as domain
DomainStr_NH=ddply(jcdG_stS,.(REGION,ANALYSIS_YEAR),summarize,DomainSumN_h=sum(Ntot,na.rm=TRUE)) #total possible sites in a domain
jcdG_stS<-left_join(jcdG_stS, DomainStr_NH)# add previous to strata data
jcdG_stS$w_h=jcdG_stS$Ntot/jcdG_stS$DomainSumN_h
jcdG_stS$JuvColDenW<-jcdG_stS$JuvColDen*jcdG_stS$w


# Calculate Weighted and Island Regional Delta density -------------------------------------------------

#There are 2 ways I'm playing around with calculating weighted SE of delta density
#Option 1-
jcdst_delta<-jcdG_stS

#French Frigate Mid FRF area isn't correct- temp fix
jcdst_delta$Ntot<-ifelse(jcdst_delta$STRATANAME=="French Frigate_Forereef_Mid",4787.00,jcdst_delta$Ntot)

#Identify median date that surveys were conducted for each strata and year
data.gen_tempS<-subset(data.gen_temp,GENUS_CODE=="SSSS")
date.sum<-ddply(data.gen_tempS,.(METHOD,ANALYSIS_YEAR,REGION,DEPTH_BIN,STRATANAME),
                summarize,
                DATE_=median(DATE_,na.rm=T))
date.sum$ANALYSIS_YEAR<-as.factor(date.sum$ANALYSIS_YEAR)
jcdst_delta$ANALYSIS_YEAR<-as.factor(jcdst_delta$ANALYSIS_YEAR)
strat.data.new<-left_join(jcdst_delta,date.sum)

#Identify median date that surveys were conducted for region and year
temp.date<-ddply(data.gen_tempS,.(ANALYSIS_YEAR,REGION),
                 summarize,
                 DATE_=median(DATE_,na.rm=T))
temp.date
#Convert date long to wide
strat.data.new$YEAR<-paste("a",strat.data.new$ANALYSIS_YEAR,sep="")
tmp<-strat.data.new[,c("METHOD","REGION","ISLAND","YEAR","STRATANAME","DEPTH_BIN","DATE_")];head(tmp)
n.df <- spread(tmp, YEAR, DATE_);head(n.df)

#Calculate difference between dates
n.df$Tdiff<-NULL
for (i in c(1:nrow(n.df))){ #opening brace
  if(n.df$REGION[i] =="LINE"){ #c&p
    n.df$Tdiff[i] = difftime(as.Date(n.df$a2018[i]) ,as.Date(n.df$a2015[i]) , units = c("weeks")) #c&p
  } #c&p
  if(n.df$REGION[i] =="PHOENIX"){ #c&p
    n.df$Tdiff[i] = difftime(as.Date(n.df$a2018[i]) ,as.Date(n.df$a2015[i]) , units = c("weeks")) #c&p
  } #c&p
  if(n.df$REGION[i] =="SAMOA"){ #c&p
    n.df$Tdiff[i] = difftime(as.Date(n.df$a2018[i]) ,as.Date(n.df$a2015[i]) , units = c("weeks")) #c&p
  } #c&p
  if(n.df$REGION[i] =="WAKE"){ #c&p
    n.df$Tdiff[i] = difftime(as.Date(n.df$a2017[i]) ,as.Date(n.df$a2014[i]) , units = c("weeks")) #c&p
  } #c&p
  if(n.df$REGION[i] =="NMARIAN"){ #c&p
    n.df$Tdiff[i] = difftime(as.Date(n.df$a2017[i]) ,as.Date(n.df$a2014[i]) , units = c("weeks")) #c&p
  } #c&p
  if(n.df$REGION[i] =="SMARIAN"){ #c&p
    n.df$Tdiff[i] = difftime(as.Date(n.df$a2017[i]) ,as.Date(n.df$a2014[i]) , units = c("weeks")) #c&p
  } #c&p
  if(n.df$REGION[i] =="MHI"){ #c&p
    n.df$Tdiff[i] = difftime(as.Date(n.df$a2019[i]) ,as.Date(n.df$a2016[i]) , units = c("weeks")) #c&p
  } #c&p
  if(n.df$REGION[i] =="NWHI"){ #c&p
    n.df$Tdiff[i] = difftime(as.Date(n.df$a2017[i]) ,as.Date(n.df$a2015[i]) , units = c("weeks")) #c&p
  } #c&p
} #closing curly brace for entire forloop
head(n.df)

n.df$years<-n.df$Tdiff/52 #transform Tdiff into months

#Convert long to wide for mean, n and var (have to do in 3 different chunks)
#mean
tmp<-strat.data.new[,c("METHOD","REGION","ISLAND","SEC_NAME","YEAR","STRATANAME","DEPTH_BIN","JuvColDen","Ntot")];head(tmp)
juv.new1 <- spread(tmp, YEAR, JuvColDen);head(juv.new1)
colnames(juv.new1)[8:13] <- c("m2014","m2015","m2016","m2017","m2018","m2019")

#n = number of sites/year
tmp<-strat.data.new[,c("METHOD","REGION","ISLAND","SEC_NAME","YEAR","STRATANAME","DEPTH_BIN","n")];head(tmp)
juv.new2 <- spread(tmp, YEAR,n);head(juv.new2)
colnames(juv.new2)[7:12] <- c("n2014","n2015","n2016","n2017","n2018","n2019")

#Variance
tmp<-strat.data.new[,c("METHOD","REGION","ISLAND","SEC_NAME","YEAR","STRATANAME","DEPTH_BIN","Var_JuvColDen")];head(tmp)
juv.new3 <- spread(tmp, YEAR, Var_JuvColDen);head(juv.new3)
colnames(juv.new3)[7:12] <- c("v2014","v2015","v2016","v2017","v2018","v2019")

#merge N and Var dfs together
juv.new4<-left_join(juv.new2,juv.new3);head(juv.new4)


#Calculate difference in MEAN Juv density between years
juv.new1$DeltaDen<-NULL
for (i in c(1:nrow(juv.new1))){ #opening brace
  if(juv.new1$REGION[i] =="LINE"){ #c&p
    juv.new1$DeltaDen[i] = juv.new1$m2018[i] - juv.new1$m2015[i] #c&p
  } #c&p
  if(juv.new1$REGION[i] =="PHOENIX"){ #c&p
    juv.new1$DeltaDen[i] = juv.new1$m2018[i] - juv.new1$m2015[i] #c&p
  } #c&p
  if(juv.new1$REGION[i] =="SAMOA"){ #c&p
    juv.new1$DeltaDen[i] = juv.new1$m2018[i] - juv.new1$m2015[i] #c&p
  } #c&p
  if(juv.new1$REGION[i] =="WAKE"){ #c&p
    juv.new1$DeltaDen[i] = juv.new1$m2017[i] - juv.new1$m2014[i] #c&p
  } #c&p
  if(juv.new1$REGION[i] =="NMARIAN"){ #c&p
    juv.new1$DeltaDen[i] = juv.new1$m2017[i] - juv.new1$m2014[i] #c&p
  } #c&p
  if(juv.new1$REGION[i] =="SMARIAN"){ #c&p
    juv.new1$DeltaDen[i] = juv.new1$m2017[i] - juv.new1$m2014[i] #c&p
  }  
  if(juv.new1$REGION[i] =="MHI"){ #c&p
      juv.new1$DeltaDen[i] = juv.new1$m2019[i] - juv.new1$m2016[i] #c&p
    } #c&p
  if(juv.new1$REGION[i] =="NWHI"){ #c&p
      juv.new1$DeltaDen[i] = juv.new1$m2017[i] - juv.new1$m2015[i] #c&p
    } #c&p
  } #closing curly brace for entire forloop
  head(juv.new1)
  
#Calculate difference in VARIANCE Juv density between years
  #Calculate difference in MEAN Juv density between years
juv.new4$VarDeltaDen<-NULL
  for (i in c(1:nrow(juv.new4))){ #opening brace
    if(juv.new4$REGION[i] =="LINE"){ #c&p
      juv.new4$VarDeltaDen[i] = sqrt((juv.new4$v2018[i]/juv.new4$n2018[i]) + (juv.new4$v2015[i]/juv.new4$n2015[i])) #c&p
    } #c&p
    if(juv.new4$REGION[i] =="PHOENIX"){ #c&p
      juv.new4$VarDeltaDen[i] =  sqrt((juv.new4$v2018[i]/juv.new4$n2018[i]) + (juv.new4$v2015[i]/juv.new4$n2015[i])) #c&p
    } #c&p
    if(juv.new4$REGION[i] =="SAMOA"){ #c&p
      juv.new4$VarDeltaDen[i] =  sqrt((juv.new4$v2018[i]/juv.new4$n2018[i]) + (juv.new4$v2015[i]/juv.new4$n2015[i])) #c&p
    } #c&p
    if(juv.new4$REGION[i] =="WAKE"){ #c&p
      juv.new4$VarDeltaDen[i] =  sqrt((juv.new4$v2017[i]/juv.new4$n2017[i]) + (juv.new4$v2014[i]/juv.new4$n2014[i])) #c&p
    } #c&p
    if(juv.new4$REGION[i] =="NMARIAN"){ #c&p
      juv.new4$VarDeltaDen[i] = sqrt((juv.new4$v2017[i]/juv.new4$n2017[i]) + (juv.new4$v2014[i]/juv.new4$n2014[i])) #c&p
    } #c&p
    if(juv.new4$REGION[i] =="SMARIAN"){ #c&p
      juv.new4$VarDeltaDen[i] = sqrt((juv.new4$v2017[i]/juv.new4$n2017[i]) + (juv.new4$v2014[i]/juv.new4$n2014[i])) #c&p
    }  
    if(juv.new4$REGION[i] =="MHI"){ #c&p
        juv.new4$VarDeltaDen[i] = sqrt((juv.new4$v2019[i]/juv.new4$n2019[i]) + (juv.new4$v2016[i]/juv.new4$n2016[i])) #c&p
      } #c&p
    if(juv.new4$REGION[i] =="NWHI"){ #c&p
        juv.new4$VarDeltaDen[i] = sqrt((juv.new4$v2017[i]/juv.new4$n2017[i]) + (juv.new4$v2015[i]/juv.new4$n2015[i])) #c&p
      } #c&p
    } #closing curly brace for entire forloop
    head(juv.new4)
  
  
  
#Clean up date and juv datasets and merge & CALCULATE Delta density/year
n.df<-n.df[,c("METHOD","REGION","ISLAND","STRATANAME","DEPTH_BIN","years")];head(n.df)
juv.new1<-juv.new1[,c("METHOD","REGION","ISLAND","SEC_NAME","STRATANAME","DEPTH_BIN","DeltaDen","Ntot")];head(juv.new1)
juv.new4<-juv.new4[,c("METHOD","REGION","ISLAND","SEC_NAME","STRATANAME","DEPTH_BIN","VarDeltaDen")];head(juv.new4)

df<-left_join(juv.new1,juv.new4);head(df)
delta.df<-left_join(df,n.df);head(delta.df)

delta.df$DeltaDen_yr<-delta.df$DeltaDen/delta.df$years
delta.df$VarDeltaDen_yr<-delta.df$VarDeltaDen/delta.df$years

head(delta.df)

#Calcuate island and regional delta density
delta.df$DOMAIN_SCHEMA<-delta.df$ISLAND


#START HERE- THIS FUNCTION ISN'T WORKING
delta_is<-Calc_DomainDelta(delta.df,"DeltaDen_yr","VarDeltaDen_yr","SE_DeltaDen_yr")
colnames(delta_is)[2]<-c("ISLAND")

#Calculate Regional Estimates
delta.df$DOMAIN_SCHEMA<-delta.df$REGION
delta_r<-Calc_DomainDelta(delta.df,"DeltaDen_yr","VarDeltaDen_yr","SE_DeltaDen_yr")


head(delta_is)
head(delta_r)


# Calculate Weighted Strata Delta density for mixed models-------------------------------------------------

#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
delta.df$ANALYSIS_SCHEMA<-delta.df$STRATANAME
delta.df$DOMAIN_SCHEMA<-delta.df$REGION 
delta.df$ANALYSIS_YEAR<-delta.df$OBS_YEAR
delta.df$DB_RZ<-paste(delta.df$DEPTH_BIN,"Forereef",sep="_")
delta.df$GENUS_CODE<-"SSSS"

#Calculate weighting factor for strata with REGION as domain
strat.temp<-ddply(delta.df,.(REGION,STRATANAME,Ntot),summarize,temp=sum(Ntot,na.rm=TRUE)) #calculate # of possible sites in a given stratum
Strata_NH<-ddply(strat.temp,.(REGION,STRATANAME),summarize,N_h.as=sum(Ntot,na.rm=TRUE)) #calculate # of possible sites in a given stratum
Dom_NH<-ddply(Strata_NH,.(REGION),summarize,Dom_N_h=sum(N_h.as,na.rm=TRUE))#calculate # of possible sites in a given domain, use this to calculate weighting factor
Strata_NH_R<-left_join(Strata_NH,Dom_NH) #add Dom_N_h into Strata_NH df
Strata_NH_R$w<-Strata_NH_R$N_h.as/Strata_NH_R$Dom_N_h # add schema weighting factor to schema dataframe

delta.df<-left_join(delta.df,Strata_NH_R)
delta.df$W_DeltaDen<-delta.df$DeltaDen_yr*delta.df$w


#Identify median Latitude that surveys were conducted for each sector and year
lat.sum<-ddply(data.gen_tempS,.(REGION,ISLAND),
               summarize,
               LATITUDE=median(LATITUDE,na.rm=T))

j.sum<-ddply(delta.df,.(REGION,ISLAND),
             summarize,
             DeltaDen=median(DeltaDen_yr,na.rm=T))
deltaden_lat<-left_join(j.sum,lat.sum)


#Merge first and last juvenile density data into delta density dataframe
delta.df2<-left_join(delta.df,jcdG_stS_first)
delta.df2<-left_join(delta.df2,jcdG_stS_last)
head(delta.df2)


write.csv(delta.df2,file="T:/Benthic/Projects/Juvenile Project/JuvProject_deltadensity_STRATA.csv")


# Plotting temporal trends ------------------------------------------------

#Plot delta density by region and depth bin
delta.sumD<-ddply(delta.df,.(REGION,DEPTH_BIN),
                  summarize,
                  DeltaMEAN=mean(DeltaDen,na.rm=T),
                  DeltaSE=std.error(DeltaDen,na.rm=T))


#General thoughts on patterns
#The time after the bleaching event is very important. I removed the 2016 and 2017 juvenile data for jarvis, baker and howland. it may be interesting to look at these separately



# Mixed models - slope method ---------------------------------------------
# Trying to find a way to use the site level data
# The idea is to extract the slope values from the mixed models with random slopes from the before/after bleaching 
#This would allow me to incorporate the site-level varianace rather than losing it by pooling up to stratum level
#Unfortunatley, this method doesn't work well. 1- having issues with assumptions of equal variance regardless of which probablity distrubiton I use
#2- The mixed models are having issues with singularity- which means I need to simplify the random effects- which I can't

head(data.gen_tempS)


data.gen_tempS$REGION_YEAR<-paste(data.gen_tempS$REGION,data.gen_tempS$OBS_YEAR,sep="_")
#Remove the 2013 MHI and 2014 NWHI data
data.gen_tempS<-data.gen_tempS[!data.gen_tempS$REGION_YEAR %in% c("NWHI_2014","MHI_2013"),]
View(data.gen_tempS)

data.gen_tempS<-data.gen_tempS %>% mutate(T1_T2= dplyr::recode(OBS_YEAR,
                                           `2014`="1_Before",
                                           `2015`="1_Before",
                                           `2016`="1_Before",
                                           `2017`="2_After",
                                           `2018`="2_After",
                                           `2019`="2_After"))


#Check for normality and equal variance

#Untransformed
op<-par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plotNormalHistogram(data.gen_tempS$JuvColDen)
mod<-lmer(JuvColDen~T1_T2+ (1+T1_T2|DB_RZ/SEC_NAME),data=data.gen_tempS,REML = FALSE, control = lmerControl(
  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
# qqnorm(residuals(mod),ylab="Sample Quantiles for residuals")
# qqline(residuals(mod), col="red")
leveneTest(residuals(mod) ~ data.gen_tempS$T1_T2)
E2<-resid(mod, type = "pearson") # extract normalized residuals
F2<-fitted(mod) # extract the fitted data
plot(F2, E2, xlab = "fitted values", ylab = "residuals",main ="LMM-untransformed") # plot the relationship


a<-subset(data.gen_tempS,JuvColDen==0)
nrow(a)/nrow(data.gen_tempS) #5% of dataset are zero values


#Try glmm with poisson distribution
#Convert density to counts
data.gen_tempS$JuvCount<-round(data.gen_tempS$JuvColDen*6,0)
head(data.gen_tempS)

mod<-glmer(JuvCount~T1_T2+ (1+T1_T2|DB_RZ/SEC_NAME),family="poisson",data=data.gen_tempS)
leveneTest(residuals(mod) ~ data.gen_tempS$T1_T2)
E2<-resid(mod, type = "pearson") # extract normalized residuals
F2<-fitted(mod) # extract the fitted data
plot(F2, E2, xlab = "fitted values", ylab = "residuals",main="GLMM-Poisson") # plot the relationship


# #negative binomial
# mod<-glm.nb(JuvCount~T1_T2,data=data.gen_tempS)
# 
# mod<-glmer.nb(JuvCount~T1_T2+ (1+T1_T2|DB_RZ/SEC_NAME),data=data.gen_tempS,verbose=TRUE)
# leveneTest(residuals(mod) ~ data.gen_tempS$T1_T2)
# E2<-resid(mod, type = "pearson") # extract normalized residuals
# F2<-fitted(mod) # extract the fitted data
# plot(F2, E2, xlab = "fitted values", ylab = "residuals",main="GLMM-Negative Binomial") # plot the relationship
# 


# #Transformed
# l<-log(data.gen_tempS$JuvColDen+0.5)
# data.gen_tempS$l<-log(data.gen_tempS$JuvColDen+0.5)
# plotNormalHistogram(l)
# 
# 
# mod<-lmer(l~T1_T2+ (1+T1_T2|DB_RZ/SEC_NAME),data=data.gen_tempS,REML = FALSE, control = lmerControl(
#   optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
# qqnorm(residuals(mod),ylab="Sample Quantiles for residuals")
# qqline(residuals(mod), col="red")
# 
# summary(mod)  # Report the results
# par(mfrow = c(1, 1))  # Split the plotting panel into a 2 x 2 grid
# plot(mod) 
# 


slopeden<-tibble::rownames_to_column(coef(mod)[[c("SEC_NAME:DB_RZ")]])
slopeden<-slopeden[c(-2)];colnames(slopeden)<-c("SEC_NAME:DB_RZ","SlopeDen")
slopeden$`SEC_NAME:DB_RZ` <- str_replace_all(slopeden$`SEC_NAME:DB_RZ`, ":", "_")
colnames(slopeden)<-c("STRATANAME2","SlopeDen")
head(slopeden)

#Merge with delta def
delta.df$STRATANAME2<-paste(delta.df$SEC_NAME,delta.df$DEPTH_BIN,"Forereef",sep = "_")
delta.df<-left_join(delta.df,slopeden);head(delta.df)
delta.df$Diff<-delta.df$DeltaDen_yr-delta.df$SlopeDen
delta.df[order(delta.df$Diff),]
View(delta.df)

par(mfrow = c(1, 2))  # Split the plotting panel into a 2 x 2 grid
plot(x=predict(mod), y=data.gen_tempS$l,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Density')
abline(mod)

mod2<-lm(delta.df$SlopeDen~delta.df$DeltaDen_yr)
summary(mod2)
plot(x=delta.df$DeltaDen_yr, y=delta.df$SlopeDen,
     xlab='Delta Density/yr',
     ylab='Model Slope Density',
     main='Slope vs. Delta Density')
abline(mod2)



# MIXED MODELING - Density-------------------------------
#Convert analysis year to YEAR1,2,3 so that the GLMMs don't try to compare  regionxyear combos that don't exist
jcdG_stS<-jcdG_stS %>% mutate(YEAR= dplyr::recode(ANALYSIS_YEAR,
                                                               `2014`="Year1",
                                                               `2015`="Year1",
                                                               `2016`="Year1",
                                                               `2017`="Year2",
                                                               `2018`="Year2",
                                                               `2019`="Year2"))
jcdG_stS$YEAR<-ifelse(jcdG_stS$REGION_YEAR=="NWHI_2016","Year3",as.character(jcdG_stS$YEAR))
View(jcdG_stS)





#Check for normality and equal variance
library(performance)
library(see)
library(patchwork)
plotNormalHistogram(jcdG_stS$JuvColDenW)
mod<-lm(JuvColDenW~ANALYSIS_YEAR,data=jcdG_stS)
qqnorm(residuals(mod),ylab="Sample Quantiles for residuals")
qqline(residuals(mod), col="red")

summary(mod)  # Report the results
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(mod) 


#Gamma distribution
jcdG_stS$JuvColDenW1<-jcdG_stS$JuvColDenW +0.5
mod<-glmer(JuvColDenW1~YEAR*REGION + (1|SEC_NAME),family="Gamma",data=jcdG_stS)
mod<-glm(JuvColDenW1~ REGION,family="Gamma",data=jcdG_stS)

performance::check_model(mod)

E2<-resid(mod, type = "pearson") # extract normalized residuals
F2<-fitted(mod) # extract the fitted data
plot(F2, E2, xlab = "fitted values", ylab = "residuals",main="GLMM-Gamma") # plot the relationship



#Convert density to counts- multiplying by less than 100 changes signficance, but perhaps is more realisitc??
jcdG_stS$JuvCount<-round(jcdG_stS$JuvColDenW*100,0)
head(jcdG_stS)

mod<-glmer(JuvCount~YEAR*REGION + (1|SEC_NAME),family="poisson",data=jcdG_stS)
# mod<-glmer(JuvCount~ANALYSIS_YEAR+ (1|ISLAND),family="poisson",data=jcdG_stS) # can't assume a 

# leveneTest(residuals(mod) ~ jcdG_stS$ANALYSIS_YEAR)
# E2<-resid(mod, type = "pearson") # extract normalized residuals
# F2<-fitted(mod) # extract the fitted data
# plot(F2, E2, xlab = "fitted values", ylab = "residuals",main="GLM-Poisson") # plot the relationship

#Check Diagnostic plots - this is a GREAT set of diagnostic plots!!
#Note that the function gives you a "normality of residuals" plot, but this isn't relavent for glms where you are using other PDFs
plot(mod)
performance::check_model(mod)

# # 
# #Tried log and sqrt - power transformation is the only one that will work.
# colden<-jcdG_stS$JuvColDenW +2
# boxcox(colden~jcdG_stS$ANALYSIS_YEAR)
# boxcox(colden~jcdG_stS$ANALYSIS_YEAR, lambda=seq(-7,1))
# 
# dp<-bct(colden,-4.2)
# mod<-lm(dp~ANALYSIS_YEAR,data=jcdG_stS)
# performance::check_model(mod)
# 
# jcdG_stS$JuvDen_trans=dp
# # 


#Likelihood Ratio Tests
library(emmeans)


nullmod<-glmer(JuvCount~1 + (1|SEC_NAME),family="poisson",data=jcdG_stS)
yearmod<-glmer(JuvCount~YEAR + (1|SEC_NAME),family="poisson",data=jcdG_stS)
regionmod<-glmer(JuvCount~REGION + (1|SEC_NAME),family="poisson",data=jcdG_stS)
fullmod<-glmer(JuvCount~YEAR*REGION + (1|SEC_NAME),family="poisson",data=jcdG_stS)

anova(nullmod,fullmod,test="Chisq")
anova(nullmod,yearmod,test="Chisq")
anova(nullmod,regionmod,test="Chisq")


#Post hoc tests
emmeans::emmeans(fullmod, list(pairwise ~ YEAR*REGION), adjust = "tukey")


#Test for differences between Years for each region
# Decided to run indidivual post hoc tests for each region because I'm not really interested in whether Line 2015 is different from MHI 2016
#NWHI
mod<-glm(JuvCount~ANALYSIS_YEAR,family="poisson",data=subset(jcdG_stS,REGION=="NWHI"))
summary(glht(mod, linfct = mcp(ANALYSIS_YEAR = "Tukey"))) 
# 
# mod<-lm(JuvDen_trans~ANALYSIS_YEAR,data=subset(jcdG_stS,REGION=="NWHI"))
# anova(mod)
#MHI
mod<-glm(JuvCount~ANALYSIS_YEAR,family="poisson",data=subset(jcdG_stS,REGION=="MHI"))
summary(glht(mod, linfct = mcp(ANALYSIS_YEAR = "Tukey"))) 

#WAKE
mod<-glm(JuvCount~ANALYSIS_YEAR,family="poisson",data=subset(jcdG_stS,REGION=="WAKE"))
summary(glht(mod, linfct = mcp(ANALYSIS_YEAR = "Tukey"))) 

#PHOENIX
mod<-glm(JuvCount~ANALYSIS_YEAR,family="poisson",data=subset(jcdG_stS,REGION=="PHOENIX"))
summary(glht(mod, linfct = mcp(ANALYSIS_YEAR = "Tukey"))) 

#LINE
mod<-glm(JuvCount~ANALYSIS_YEAR,family="poisson",data=subset(jcdG_stS,REGION=="LINE"))
summary(glht(mod, linfct = mcp(ANALYSIS_YEAR = "Tukey"))) 


#Samoa
mod<-glm(JuvCount~ANALYSIS_YEAR,family="poisson",data=subset(jcdG_stS,REGION=="SAMOA"))
summary(glht(mod, linfct = mcp(ANALYSIS_YEAR = "Tukey"))) 

#SMARIAN
mod<-glm(JuvCount~ANALYSIS_YEAR,family="poisson",data=subset(jcdG_stS,REGION=="SMARIAN"))
summary(glht(mod, linfct = mcp(ANALYSIS_YEAR = "Tukey"))) 

#NMARIAN
mod<-glm(JuvCount~ANALYSIS_YEAR,family="poisson",data=subset(jcdG_stS,REGION=="NMARIAN"))
summary(glht(mod, linfct = mcp(ANALYSIS_YEAR = "Tukey"))) 


#Calculate adjusted pvalues
pvals<-c(0.000001,0.78,0.000001,0.000001,0.52,0.000001,0.000001,0.6,0.0183,0.000001)
round(p.adjust(pvals, "BH"), 3)


# PLOTTING ----------------------------------------------------------------

#bar plot of juv by region by year with post hoc tests 
jcdG_rS$REGION <- factor(jcdG_rS$REGION, levels = c("NWHI","MHI","WAKE","PHOENIX","LINE","SMARIAN","NMARIAN","SAMOA"))
jcdG_rS$ANALYSIS_YEAR<-as.factor(jcdG_rS$ANALYSIS_YEAR)
#Add Posthoc groupings from LMMs
jcdG_rS<- jcdG_rS[order(jcdG_rS$REGION),];jcdG_rS
jcdG_rS$sig<-c("b","a","b","a","b","","","a","b","a","b","","","a","b","","")


p8 <- ggplot(jcdG_rS, aes(x=ANALYSIS_YEAR, y=Mean_JuvColDen,fill=REGION)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve='single'), width = 1, color="black") +
  geom_errorbar(aes(y=Mean_JuvColDen, x=ANALYSIS_YEAR,ymin=Mean_JuvColDen-SE_JuvColDen, ymax=Mean_JuvColDen+SE_JuvColDen), width=.2)+
  facet_grid(~REGION, scales = "free_x", space = "free") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 12),
        legend.position = "none",
        axis.line = element_line(color = "black"),
        text = element_text(size = 12),
        axis.text.y = element_text(colour="black"),
        axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("#CC79A7","#D55E00","#E69F00","#F0E442","#009E73","#56B4E9","#0072B2","#999999")) +
  xlab("Year") +
  ylab("Mean Juvenile Colonies/m^2") +
  #scale_y_continuous(expand = c(0,0), limits = c(0,15)) +
  geom_text(aes(x=ANALYSIS_YEAR,y=Mean_JuvColDen+SE_JuvColDen,label=sig, group = REGION),
            position = position_dodge(),
            vjust = -0.5) 
p8


#Checking normality/equal variance for UNWEIGHTED Delta Density
plotNormalHistogram(delta.df$DeltaDen_yr)

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
#delta.df$Delta_trans<-sqrt(delta.df$DeltaDen_yr+2.7) #sqrt
delta.df$Delta_trans<-sign(delta.df$DeltaDen_yr+2.7) + abs(delta.df$DeltaDen_yr+2.7)^1/3 #cube root

plotNormalHistogram(delta.df$Delta_trans)
#mod<-lmer(Delta_trans~REGION +(1|SEC_NAME),data=delta.df)
mod<-lm(Delta_trans~REGION,data=delta.df)

leveneTest(Delta_trans~REGION,data=delta.df)
performance::check_model(mod)

#Cube root works best, but not perfect- tried power transformation too


#Checking normality/equal variance for WEIGHTED Delta Density
plotNormalHistogram(delta.df$W_DeltaDen)

delta.df$Delta_trans<-sqrt(delta.df$W_DeltaDen+0.5) #sqrt
delta.df$Delta_trans<-sign(delta.df$W_DeltaDen+0.5) + abs(delta.df$W_DeltaDen+0.3)^1/3 #cube root

plotNormalHistogram(delta.df$Delta_trans)
mod<-lmer(Delta_trans~REGION +(1|SEC_NAME),data=delta.df)
mod<-lm(Delta_trans~REGION,data=delta.df)
performance::check_model(mod)

leveneTest(Delta_trans~REGION,data=delta.df)

#Power transformation
library(rcompanion)

delta.df$T_tuk = transformTukey(delta.df$W_DeltaDen +0.5, plotit=FALSE)
plotNormalHistogram(T_tuk)

mod<-lm(T_tuk~REGION,data=delta.df)

performance::check_model(mod)
leveneTest(T_tuk~REGION,data=delta.df)


#No transformations work!!
#Use non parametric stats
library(multcompView)
kruskal.test(delta.df$W_DeltaDen, delta.df$REGION)
test<-pairwise.wilcox.test(delta.df$W_DeltaDen, delta.df$REGION,
                     p.adjust.method = "BH")
multcompLetters(test$p.value)

#Add Posthoc groupings 
delta_r<- delta_r[order(delta_r$REGION),];delta_r

delta_r$REGION <- factor(delta_r$REGION, levels = c("NWHI","MHI","WAKE","PHOENIX","LINE","SMARIAN","NMARIAN","SAMOA"))
delta_r$sig<-c("a","a","ab","b","a","ab","a","ab")
delta_r$years<-c("(2017-2015)","(2019-2016)","(2017-2014)","(2018-2015)","(2018-2015)","(2018-2015)","(2017-2014)","(2017-2014)")

# 
p9 <- ggplot(delta_r, aes(x=REGION, y=DeltaDen_yr,fill=REGION)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve='single'), width = 1, color="black") +
  geom_errorbar(aes(y=DeltaDen_yr, x=REGION,ymin=DeltaDen_yr-SE_DeltaDen_yr, ymax=DeltaDen_yr+SE_DeltaDen_yr), width=.2)+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 12),
        legend.position = "none",
        axis.line = element_line(color = "black"),
        text = element_text(size = 12),
        axis.text.y = element_text(colour="black"),
        axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("#CC79A7","#D55E00","#E69F00","#F0E442","#009E73","#56B4E9","#0072B2","#999999")) +
  xlab("Region") +
  ylab(expression("Mean"~Delta~"Juvenile Density/Year"))+
geom_text(data=delta_r,aes(x=REGION,y=DeltaDen_yr+SE_DeltaDen_yr,label=sig, group = REGION),position = position_dodge(),vjust = -0.5)+
geom_text(data=delta_r,aes(x=REGION,y=-0.8,label=years, group = REGION),position = position_dodge(),vjust = -0.8,size=3,fontface="bold")
  
p9




#with SE_2
p9b <- ggplot(delta_r, aes(x=REGION, y=DeltaDen_yr,fill=REGION)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve='single'), width = 1, color="black") +
  geom_errorbar(aes(y=DeltaDen_yr, x=REGION,ymin=DeltaDen_yr-SE_v2, ymax=DeltaDen_yr+SE_v2), width=.2)+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 12),
        legend.position = "none",
        axis.line = element_line(color = "black"),
        text = element_text(size = 12),
        axis.text.y = element_text(colour="black"),
        axis.text.x = element_text(angle = 90)) +
  scale_fill_viridis(discrete = TRUE) +
  xlab("Region") +
  ylab(expression("Mean"~Delta~"Juvenile Density/Year"))+
  geom_text(data=delta_r,aes(x=REGION,y=DeltaDen_yr+SE_DeltaDen_yr,label=sig, group = REGION),position = position_dodge(),vjust = -0.5)+
  geom_text(data=delta_r,aes(x=REGION,y=-0.8,label=years, group = REGION),position = position_dodge(),vjust = -0.8,size=3,fontface="bold")

p9b



#Plotting Delta Density/Year- ISLAND

delta_is$REGION <- factor(delta_is$REGION, levels = c("NWHI","MHI","WAKE","PHOENIX","LINE","SMARIAN","NMARIAN","SAMOA"))

p10 <- ggplot(delta_is, aes(x=reorder(ISLAND,-DeltaDen_yr), y=DeltaDen_yr,fill=REGION)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve='single'), width = 1, color="black") +
  geom_errorbar(data=delta_is,aes(y=DeltaDen_yr, x=ISLAND,ymin=DeltaDen_yr-SE_DeltaDen_yr, ymax=DeltaDen_yr+SE_DeltaDen_yr), width=.2)+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 12),
        #legend.position = "none",
        axis.line = element_line(color = "black"),
        text = element_text(size = 12),
        axis.text.y = element_text(colour="black"),
        axis.text.x = element_text(vjust=0,angle = 90)) +
  scale_fill_manual(values = c("#CC79A7","#D55E00","#E69F00","#F0E442","#009E73","#56B4E9","#0072B2","#999999")) +
  xlab("Island") +
  ylab(expression("Mean"~Delta~"Juvenile Density/Year"))

p10




#Identify median lat and long that surveys were conducted for each island and year
lat.sum<-ddply(data.gen_tempS,.(REGION,ISLAND),
               summarize,
               Y=median(LATITUDE,na.rm=T),
               X=median(LONGITUDE,na.rm = T))

j.sum<-ddply(delta_is,.(REGION,ISLAND),
             summarize,
             DeltaDen=mean(DeltaDen_yr,na.rm=T))
deltaden_coords<-left_join(j.sum,lat.sum)
head(deltaden_coords)



# #Use unweighted delta density ~ Latitiude
# plotNormalHistogram(delta.df$DeltaDen)
# 
# plotNormalHistogram(sqrt(delta.df$DeltaDen+2.7))
# mod<-lm(DeltaDen~LATITUDE,data=deltaden_coords)
# qqnorm(residuals(mod),ylab="Sample Quanqqnorm(residuals(mod)")
# qqline(residuals(mod), col="red")
# 
# mod1<-lm(DeltaDen~LATITUDE,data=deltaden_coords)
# anova(mod1);summary(mod1)




write.csv(deltaden_coords,file="T:/Benthic/Projects/Juvenile Project/DeltaJuvenileforMaps.csv")

# Plot Pacific-wide Map of Delta Juvenile Density -------------------------

##Helpful website for plotting maps with ggplot https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html

#https://rpubs.com/valentin/pacific-centered-map-voronoi-tessellation

#Theme for maps
theme_set(
  theme_bw() +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size = 8)))

#Get world spatial polygons from the rnaturalearth package
#Cut out area of world that doesn't include the Pacific and bind E and W Pacific and shift the geographical coordinates for a Pacific view (see website for illustration)
#Note- you will get several warnings about "world is invalid" and an issues with rgeos- ignore these
world <- rnaturalearth::ne_countries(scale = 'medium', returnclass = "sp")
box_cut <- bbox2SP(n = 90, s = -90, w = -150, e = 140, proj4string = world@proj4string) #you can tweak the W and E coords to zoom in and out, but adjust the N and S coords in the function below using ymin and ymax
world_crop <- gDifference(world, box_cut)

pacific_crop <- world_crop %>% 
  st_as_sf() %>% # change from sp to sf object/class
  st_shift_longitude() %>% 
  st_crop(c(xmin = st_bbox(.)[["xmin"]],
            xmax = st_bbox(.)[["xmax"]],
            ymin = -15,
            ymax = 30))

#convert delta juvenile density data to spatial points df
xy <- deltaden_coords[,c(5,4)]
deltaden_coords_sp <- SpatialPointsDataFrame(coords = xy, data = deltaden_coords,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#Crop the delta spdf
delta_shift <- deltaden_coords_sp %>% 
  st_as_sf() %>%
  st_shift_longitude() %>% 
  st_crop(c(xmin = 120, xmax = 250, ymin = -50, ymax = 30)) %>% 
  # Also adds the coordinates to be used for labeling with geom_text_repel
  bind_cols(st_coordinates(.) %>% as.data.frame())

#Create an Inset map using a similar process described above
box_cut2 <- bbox2SP(n = 90, s = -90, w = -110, e = 110, proj4string = world@proj4string)
world_crop2 <- gDifference(world, box_cut2)

pacific_crop2 <- world_crop2 %>% 
  st_as_sf() %>% # change from sp to sf object/class
  st_shift_longitude() %>% 
  st_crop(c(xmin = st_bbox(.)[["xmin"]],
            xmax = st_bbox(.)[["xmax"]],
            ymin = -40,
            ymax = 50))
pacific_crop_bb = st_as_sfc(st_bbox(pacific_crop)) #draw box for the area of main map
pacific_box = st_as_sfc(st_bbox(pacific_crop2)) #draw box for the area of inset map

#plot inset map
insetmap<-ggplot() +
  geom_sf(data = pacific_crop2)+
  geom_sf(data = pacific_crop_bb, fill = NA, color = "black", size = 1.2)+
  geom_sf(data = pacific_box, fill = NA, color = "black", size = 0.4)+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())


#plot main data map
deltamap<-ggplot() +
  geom_sf(data = pacific_crop)+ #basemap
  geom_sf(data = delta_shift,aes(color = DeltaDen), size = 3, shape = 19)+ #data
  geom_text_repel(data = delta_shift, #add island labels 
                  aes(x = X...7, y = Y...8, label = ISLAND),
                  size = 3,
                  fontface = "bold",
                  segment.size = 0.25,
                  box.padding = 0.4,
                  min.segment.length = 0,
                  seed = 2020-5-16)+
  annotation_scale(location = "bl", width_hint = 0.4)+ #add scale bar
  scale_color_gradient2(midpoint = 0.66, #Color scheme
                        high = 'forestgreen',
                        mid = 'yellow2',
                        low = 'red2',
                        na.value = 'gray95',
                        name="")+  #you can add a legend title here if you want
  theme(legend.position = c(0.95,0.2)) #put legend inside the plot
  #ggtitle("Change in Juvenile Colony Density/Year")
  
#Combine main and inset maps
finalmap = ggdraw() +
  draw_plot(deltamap) +
  draw_plot(insetmap, x = 0.02, y = 0.07, width = 0.3, height = 0.3)

finalmap



# Delta Density v. Depth --------------------------------------------------

depth_strat<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Predictor Variables/JuvProject_Depth.csv")
delta.df<-left_join(delta.df,depth_strat)

#Use unweighted delta density
plotNormalHistogram(sqrt(delta.df$DeltaDen_yr+2.7))
mod<-lm(sqrt(DeltaDen_yr+2.7)~MeanMidDepth,data=delta.df)
qqnorm(residuals(mod),ylab="Sample Quanqqnorm(residuals(mod)")
qqline(residuals(mod), col="red")

delta.df$Delta_trans<-sqrt(delta.df$DeltaDen_yr+2.7)


mod1 <- lm(Delta_trans~REGION*MeanMaxDepth,data=delta.df)
anova(mod1);summary(mod1)

mod2 <- lm(Delta_trans~MeanMaxDepth,data=delta.df)
anova(mod2);summary(mod2)

jvd<-delta.df %>%
  ggplot(aes(x=MeanMaxDepth, y=DeltaDen_yr)) + 
  geom_smooth(se=TRUE,method="lm",lwd=1.5,color="black")+
  geom_point()+
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001)
    ,axis.text.x = element_text(angle = 90)) + # adjust x axis to lower the same amount as the genus labels
  labs(x="Mean Max Depth(M)",y="Delta Juvenile Density/Year")+
  geom_label(aes(x=4,y=6),hjust=0,label=paste("R^2=0.169","\n p=0.0458",sep=""))
jvd


#Save plots
ggsave(plot=p8,file="T:/Benthic/Projects/Juvenile Project/Figures/DensityRegionalTemporal.jpg",width=10,height=5)
ggsave(plot=p9,file="T:/Benthic/Projects/Juvenile Project/Figures/WeightedDeltaRegional.jpg",width=10,height=5)
ggsave(plot=p10,file="T:/Benthic/Projects/Juvenile Project/Figures/WeightedDeltaIsland.jpg",width=10,height=5)
ggsave(plot=finalmap,file="T:/Benthic/Projects/Juvenile Project/Figures/WeightedDeltaIslandmap.jpg",width=11,height=7)
ggsave(plot=jvd,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Figures/DeltavDepth.jpg",width=10,height=5)




# Identify dominant adult morphology in each stratum ----------------------
awd<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED.csv")

nw<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/PMNM2017_ADULTCOLONY_QCd.csv")

#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("ISLAND","SITEVISITID","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "MORPH_CODE","GENUS_CODE")


#remove extraneous columns
head(awd[,DATA_COLS])
awd<-awd[,DATA_COLS]
nw<-nw[,DATA_COLS]

all.df<-rbind(awd,nw)

morph<-read.csv("T:/Benthic/Data/Lookup Tables/MorphologyCodeLookup.csv")

all.df<-left_join(all.df,morph)
head(all.df)

#Convert Protected Slope to Forereef
all.df$REEF_ZONE<-ifelse(all.df$REEF_ZONE=="Protected Slope","Forereef",as.character(all.df$REEF_ZONE))

all.df<-subset(all.df,REEF_ZONE=="Forereef")

# Merge Site level data with sectors file and export site data ------------
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

#Merge together survey meta data and sector area files and check for missmatches 
all.df<-left_join(all.df,sectors)

all.df$STRATANAME<-paste(all.df$SEC_NAME,all.df$REEF_ZONE,all.df$DEPTH_BIN,sep="_")

st.list<-ddply(delta.df,.(STRATANAME),summarize,n=length(STRATANAME))
all.df2<-all.df[all.df$STRATANAME %in% c(st.list$STRATANAME),] #Subset adult data to only include strata of intersest 

all.df2<- filter(all.df2, !(GENUS_CODE %in% c("SSSS","AAAA")))

morph<-ddply(all.df2,.(ISLAND,STRATANAME,GENUS_CODE,NEW_MORPH_CODE),summarize,morph_sum=length(GENUS_CODE))
tot<-ddply(all.df2,.(ISLAND,STRATANAME,GENUS_CODE),summarize,total=length(GENUS_CODE))
mt<-left_join(morph,tot)
mt$Morph_prop<-mt$morph_sum/mt$total
head(mt)

write.csv(mt,file="T:/Benthic/Projects/Juvenile Project/Strata_Genus_Morphology.csv")
# write.csv(data.gen_pb,file="T:/Benthic/Projects/Juvenile Project/JuvProject_pb_SITE.csv")
