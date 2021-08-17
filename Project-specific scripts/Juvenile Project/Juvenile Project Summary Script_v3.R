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
survey_site$REEF_ZONE<-ifelse(survey_site$SITE=="HAW-04285","Forereef",as.character(survey_site$REEF_ZONE))

#Remove 2 sites that weren't surveyed for juveniles
jwd<-jwd[!(jwd$SITE %in% c("OFU-01012","PAG-00596")),]

#Look at the size class data to determine the min colony size cut off for analysis
#Subset 2019 Hawaii data
hi<-subset(jwd,OBS_YEAR=="2017")

ggplot(hi) + 
  geom_density(aes(x = COLONYLENGTH, fill = ANALYST), alpha = 0.2)+
  geom_vline(xintercept=1, color = "black")+
  facet_wrap(~ISLAND)

ggplot(hi) + 
  geom_histogram(aes(x = COLONYLENGTH, fill = ANALYST))+
  geom_vline(xintercept=1, color = "black")+
  facet_wrap(~ISLAND)

ggplot(hi) + 
  geom_density(aes(x = COLONYLENGTH, fill = ISLAND), alpha = 0.2)+
  geom_vline(xintercept=1, color = "black")+
  facet_wrap(~ISLAND)

s.data<-ddply(hi,.(ISLAND,ANALYST),
              summarise,
              min=min(COLONYLENGTH,na.rm=T),
              mean=mean(COLONYLENGTH,na.rm=T),
              ncol=length(COLONYLENGTH,na.rm=T))

s.all<-ddply(jwd,.(ANALYST),
             summarise,
             min=min(COLONYLENGTH,na.rm=T),
             mean=mean(COLONYLENGTH,na.rm=T),
             ncol=length(COLONYLENGTH))

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

site.data.gen<-ddply(jcd.gen, .(SITE,SITEVISITID,GENUS_CODE), #calc total colonies by condition
                     summarise,
                     JuvColDen=mean(JuvColDen,na.rm=T))

site.data.gen2<-site.data.gen

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

#Convert Protected Reef Slope to Forreef and Subset just Forereef sites
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



# 
# # Summarize Post bleaching data for driver analysis -----------------------
# 
# #Subset post bleaching regions and years for downstream driver analysis
# REGION_YEAR<-c("MHI_2019","NWHI_2016","NMARIAN_2017","SMARIAN_2017","PHOENIX_2018","LINE_2018","SAMOA_2018")
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
# GENERATE DATA FOR TEMPORAL ANALYSIS---------------------------------------------------
site.data.gen2$STRATANAME<- paste(site.data.gen2$SEC_NAME,site.data.gen2$REEF_ZONE,site.data.gen2$DEPTH_BIN,sep="_")
st.list<-ddply(site.data.gen2,.(METHOD,OBS_YEAR,REGION,ISLAND,SEC_NAME,STRATANAME),summarize,n=length(unique(SITE)))
st.list2<-subset(st.list,n>=2);head(st.list)

#Generate list of strata that were surveyed in all years for a given region and had at least 2 sites/stratum
st.list_w<-dcast(st.list2, formula=METHOD+REGION+ISLAND+SEC_NAME+STRATANAME~ OBS_YEAR, value.var="n",fill=0)
dCOLS<-c("2014","2015","2016","2017","2018","2019")
st.list_w$year_n<-rowSums(st.list_w[,dCOLS] > 0, na.rm=T) #count # of years of data
st.list_w2<-subset(st.list_w,REGION %in% c("NMARIAN","SMARIAN","LINE","PHOENIX","WAKE","SAMOA","MHI") & year_n>=2)
st.list_w3<-subset(st.list_w,REGION %in% c("NWHI") & year_n>=3)
st.list_w4<-rbind(st.list_w2,st.list_w3)

head(st.list_w4);st.list_w4<-droplevels(st.list_w4) #generate the list

data.gen_temp<-site.data.gen2[site.data.gen2$STRATANAME %in% c(st.list_w4$STRATANAME),] #Subset juv data to only include strata of intersest 
data.gen_temp<-data.gen_temp[data.gen_temp$OBS_YEAR >2013,] #Remove 2013 MHI data- we had very poor sampling that year and it's 2 years prior to bleaching event

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

#Calculate weighted strata density - need this for the barplots
jcdG_stW<-Calc_Strata_Weighted(data.gen_temp,"GENUS_CODE","JuvColDen")
colnames(jcdG_stW)<-c("REGION","ISLAND","ANALYSIS_YEAR","SEC_NAME","STRATANAME","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","JuvColDen","SE_JuvColDen","JuvDenW","SEJuvDenW")
jcdG_stW$ANALYSIS_YEAR<-as.factor(jcdG_stW$ANALYSIS_YEAR)
head(jcdG_stW)
jcdG_stWS<-subset(jcdG_stW,GENUS_CODE=="SSSS")




#Subset most recent survey data for each region
REGION_YEAR<-c("MHI_2019","NWHI_2016","NMARIAN_2017","SMARIAN_2017","PHOENIX_2018","LINE_2018","SAMOA_2018","WAKE_2017")
jcdG_stS<-as.data.frame(jcdG_stS)

jcdG_stS$REGION_YEAR<-paste(jcdG_stS$REGION,jcdG_stS$ANALYSIS_YEAR,sep="_")
jcdG_stS_pb<-jcdG_stS[jcdG_stS$REGION_YEAR %in% REGION_YEAR,]
head(jcdG_stS_pb)


#Calculate Island Estimates
data.gen_temp$ANALYSIS_SCHEMA<-data.gen_temp$STRATANAME
data.gen_temp$DOMAIN_SCHEMA<-data.gen_temp$ISLAND
data.gen_temp$ANALYSIS_YEAR<-data.gen_temp$OBS_YEAR
data.gen_temp$DB_RZ<-paste(data.gen_temp$DEPTH_BIN,data.gen_temp$REEF_ZONE,sep="_")

jcdG_is<-Calc_Domain(data.gen_temp,"GENUS_CODE","JuvColDen","Juvpres.abs")
jcdG_is<-jcdG_is[,c("REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen")]
jcdG_isS<-subset(jcdG_is,GENUS_CODE=="SSSS")

#Calculate Regional Estimates
data.gen_temp$ANALYSIS_SCHEMA<-data.gen_temp$STRATANAME
data.gen_temp$ANALYSIS_YEAR<-data.gen_temp$OBS_YEAR
data.gen_temp$DB_RZ<-paste(data.gen_temp$DEPTH_BIN,data.gen_temp$REEF_ZONE,sep="_")

jcdG_r<-Calc_Domain_Region(data.gen_temp,"GENUS_CODE","JuvColDen","Juvpres.abs")
jcdG_r<-jcdG_r[,c("REGION","ANALYSIS_YEAR","GENUS_CODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen")]
jcdG_r$REGION<-ifelse(is.na(jcdG_r$REGION),"Wake",jcdG_r$REGION)
jcdG_rS<-subset(jcdG_r,GENUS_CODE=="SSSS")


# Calculate Weighted Strata Delta density -------------------------------------------------
# jcdst_deltaW<-jcdG_stWS
# 
# #Identify median date that surveys were conducted for each strata and year
# data.gen_tempS<-subset(data.gen_temp,GENUS_CODE=="SSSS")
# date.sum<-ddply(data.gen_tempS,.(METHOD,ANALYSIS_YEAR,REGION,DEPTH_BIN,STRATANAME),
#                 summarize,
#                 DATE_=median(DATE_,na.rm=T))
# date.sum$ANALYSIS_YEAR<-as.factor(date.sum$ANALYSIS_YEAR)
# jcdst_deltaW$ANALYSIS_YEAR<-as.factor(jcdst_deltaW$ANALYSIS_YEAR)
# strat.data.new<-left_join(jcdst_deltaW,date.sum)
# 
# #Identify median date that surveys were conducted for region and year
# temp.date<-ddply(data.gen_tempS,.(ANALYSIS_YEAR,REGION),
#                  summarize,
#                  DATE_=median(DATE_,na.rm=T))
# temp.date
# #Convert date long to wide
# strat.data.new$YEAR<-paste("a",strat.data.new$ANALYSIS_YEAR,sep="")
# tmp<-strat.data.new[,c("METHOD","REGION","ISLAND","YEAR","STRATANAME","DEPTH_BIN","DATE_")];head(tmp)
# n.df <- spread(tmp, YEAR, DATE_);head(n.df)
# 
# #Calculate difference between dates
# n.df$Tdiff<-NULL
# for (i in c(1:nrow(n.df))){ #opening brace
#   if(n.df$REGION[i] =="LINE"){ #c&p
#     n.df$Tdiff[i] = difftime(as.Date(n.df$a2018[i]) ,as.Date(n.df$a2015[i]) , units = c("weeks")) #c&p
#   } #c&p
#   if(n.df$REGION[i] =="PHOENIX"){ #c&p
#     n.df$Tdiff[i] = difftime(as.Date(n.df$a2018[i]) ,as.Date(n.df$a2015[i]) , units = c("weeks")) #c&p
#   } #c&p
#   if(n.df$REGION[i] =="SAMOA"){ #c&p
#     n.df$Tdiff[i] = difftime(as.Date(n.df$a2018[i]) ,as.Date(n.df$a2015[i]) , units = c("weeks")) #c&p
#   } #c&p
#   if(n.df$REGION[i] =="WAKE"){ #c&p
#     n.df$Tdiff[i] = difftime(as.Date(n.df$a2017[i]) ,as.Date(n.df$a2014[i]) , units = c("weeks")) #c&p
#   } #c&p
#   if(n.df$REGION[i] =="NMARIAN"){ #c&p
#     n.df$Tdiff[i] = difftime(as.Date(n.df$a2017[i]) ,as.Date(n.df$a2014[i]) , units = c("weeks")) #c&p
#   } #c&p
#   if(n.df$REGION[i] =="SMARIAN"){ #c&p
#     n.df$Tdiff[i] = difftime(as.Date(n.df$a2017[i]) ,as.Date(n.df$a2014[i]) , units = c("weeks")) #c&p
#   } #c&p
#   if(n.df$REGION[i] =="MHI"){ #c&p
#     n.df$Tdiff[i] = difftime(as.Date(n.df$a2019[i]) ,as.Date(n.df$a2016[i]) , units = c("weeks")) #c&p
#   } #c&p
#   if(n.df$REGION[i] =="NWHI"){ #c&p
#     n.df$Tdiff[i] = difftime(as.Date(n.df$a2016[i]) ,as.Date(n.df$a2014[i]) , units = c("weeks")) #c&p
#   } #c&p
# } #closing curly brace for entire forloop
# head(n.df)
# 
# n.df$years<-n.df$Tdiff/52 #transform Tdiff into months
# 
# tmp<-strat.data.new[,c("METHOD","REGION","ISLAND","SEC_NAME","YEAR","STRATANAME","DEPTH_BIN","JuvDenW")];head(tmp)
# juv.new <- spread(tmp, YEAR, JuvDenW);head(juv.new)
# 
# #Calculate difference in Juv density between years
# juv.new$DeltaDen<-NULL
# for (i in c(1:nrow(juv.new))){ #opening brace
#   if(juv.new$REGION[i] =="LINE"){ #c&p
#     juv.new$DeltaDen[i] = juv.new$a2018[i] - juv.new$a2015[i] #c&p
#   } #c&p
#   if(juv.new$REGION[i] =="PHOENIX"){ #c&p
#     juv.new$DeltaDen[i] = juv.new$a2018[i] - juv.new$a2015[i] #c&p
#   } #c&p
#   if(juv.new$REGION[i] =="SAMOA"){ #c&p
#     juv.new$DeltaDen[i] = juv.new$a2018[i] - juv.new$a2015[i] #c&p
#   } #c&p
#   if(juv.new$REGION[i] =="WAKE"){ #c&p
#     juv.new$DeltaDen[i] = juv.new$a2017[i] - juv.new$a2014[i] #c&p
#   } #c&p
#   if(juv.new$REGION[i] =="NMARIAN"){ #c&p
#     juv.new$DeltaDen[i] = juv.new$a2017[i] - juv.new$a2014[i] #c&p
#   } #c&p
#   if(juv.new$REGION[i] =="SMARIAN"){ #c&p
#     juv.new$DeltaDen[i] = juv.new$a2017[i] - juv.new$a2014[i] #c&p
#   } #c&p
#   if(juv.new$REGION[i] =="MHI"){ #c&p
#     juv.new$DeltaDen[i] = juv.new$a2019[i] - juv.new$a2016[i] #c&p
#   } #c&p
#   if(juv.new$REGION[i] =="NWHI"){ #c&p
#     juv.new$DeltaDen[i] = juv.new$a2016[i] - juv.new$a2014[i] #c&p
#   } #c&p
# } #closing curly brace for entire forloop
# head(juv.new)
# 
# #Clean up date and juv datasets and merge & CALCULATE Delta density/year
# n.df<-n.df[,c("METHOD","REGION","ISLAND","STRATANAME","DEPTH_BIN","years")];head(n.df)
# juv.new<-juv.new[,c("METHOD","REGION","ISLAND","SEC_NAME","STRATANAME","DEPTH_BIN","DeltaDen")];head(juv.new)
# delta.dfW<-left_join(juv.new,n.df);head(delta.dfW)
# delta.dfW$DeltaDen_yr<-delta.dfW$DeltaDen/delta.dfW$years
# View(delta.dfW)
# 
# 
# #Speak with Mary about calculating weighted island and regional delta density
# #Calculating variance of delta density is tricky- delta var = sum of variances/sum of sample sizes?
# 
# #For now just calculate unweighted delta density
# delta.sumI<-ddply(delta.dfW,.(REGION,ISLAND),
#                   summarize,
#                   DeltaMEAN=mean(DeltaDen_yr,na.rm=T),
#                   DeltaSE=std.error(DeltaDen_yr,na.rm=T))
# 
# delta.sumR<-ddply(delta.dfW,.(REGION),
#                   summarize,
#                   DeltaMEAN=mean(DeltaDen_yr,na.rm=T),
#                   DeltaSE=std.error(DeltaDen_yr,na.rm=T))
# 


# Calculate Unweighted Regional Delta density -------------------------------------------------

jcdst_delta<-jcdG_stS #unweighted stata-level data

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
    n.df$Tdiff[i] = difftime(as.Date(n.df$a2016[i]) ,as.Date(n.df$a2014[i]) , units = c("weeks")) #c&p
  } #c&p
} #closing curly brace for entire forloop
head(n.df)

n.df$years<-n.df$Tdiff/52 #transform Tdiff into months

tmp<-strat.data.new[,c("METHOD","REGION","ISLAND","SEC_NAME","YEAR","STRATANAME","DEPTH_BIN","JuvColDen")];head(tmp)
juv.new <- spread(tmp, YEAR, JuvColDen);head(juv.new)

#Calculate difference in Juv density between years
juv.new$DeltaDen<-NULL
for (i in c(1:nrow(juv.new))){ #opening brace
  if(juv.new$REGION[i] =="LINE"){ #c&p
    juv.new$DeltaDen[i] = juv.new$a2018[i] - juv.new$a2015[i] #c&p
  } #c&p
  if(juv.new$REGION[i] =="PHOENIX"){ #c&p
    juv.new$DeltaDen[i] = juv.new$a2018[i] - juv.new$a2015[i] #c&p
  } #c&p
  if(juv.new$REGION[i] =="SAMOA"){ #c&p
    juv.new$DeltaDen[i] = juv.new$a2018[i] - juv.new$a2015[i] #c&p
  } #c&p
  if(juv.new$REGION[i] =="WAKE"){ #c&p
    juv.new$DeltaDen[i] = juv.new$a2017[i] - juv.new$a2014[i] #c&p
  } #c&p
  if(juv.new$REGION[i] =="NMARIAN"){ #c&p
    juv.new$DeltaDen[i] = juv.new$a2017[i] - juv.new$a2014[i] #c&p
  } #c&p
  if(juv.new$REGION[i] =="SMARIAN"){ #c&p
    juv.new$DeltaDen[i] = juv.new$a2017[i] - juv.new$a2014[i] #c&p
  } #c&p
  if(juv.new$REGION[i] =="MHI"){ #c&p
    juv.new$DeltaDen[i] = juv.new$a2019[i] - juv.new$a2016[i] #c&p
  } #c&p
  if(juv.new$REGION[i] =="NWHI"){ #c&p
    juv.new$DeltaDen[i] = juv.new$a2016[i] - juv.new$a2014[i] #c&p
  } #c&p
} #closing curly brace for entire forloop
head(juv.new)

#Clean up date and juv datasets and merge & CALCULATE Delta density/year
n.df<-n.df[,c("METHOD","REGION","ISLAND","STRATANAME","DEPTH_BIN","years")];head(n.df)
juv.new<-juv.new[,c("METHOD","REGION","ISLAND","SEC_NAME","STRATANAME","DEPTH_BIN","DeltaDen")];head(juv.new)
delta.df<-left_join(juv.new,n.df);head(delta.df)
delta.df$DeltaDen_yr<-delta.df$DeltaDen/delta.df$years
View(delta.df)


#Speak with Mary about calculating weighted island and regional delta density
#Calculating variance of delta density is tricky- delta var = sum of variances/sum of sample sizes?

#For now just calculate unweighted delta density
delta.sumI<-ddply(delta.df,.(REGION,ISLAND),
                  summarize,
                  DeltaMEAN=mean(DeltaDen_yr,na.rm=T),
                  DeltaSE=std.error(DeltaDen_yr,na.rm=T))

delta.sumR<-ddply(delta.df,.(REGION),
                   summarize,
                   DeltaMEAN=mean(DeltaDen_yr,na.rm=T),
                   DeltaSE=std.error(DeltaDen_yr,na.rm=T))




#Identify median Latititude that surveys were conducted for each sector and year
lat.sum<-ddply(data.gen_tempS,.(REGION,ISLAND),
               summarize,
               LATITUDE=median(LATITUDE,na.rm=T))

j.sum<-ddply(delta.dfW,.(REGION,ISLAND),
             summarize,
             DeltaDen=median(DeltaDen_yr,na.rm=T))
deltaden_lat<-left_join(j.sum,lat.sum)



# Plotting temporal trends ------------------------------------------------

#Plot delta density by region and depth bin
delta.sumD<-ddply(delta.df,.(REGION,DEPTH_BIN),
                  summarize,
                  DeltaMEAN=mean(DeltaDen,na.rm=T),
                  DeltaSE=std.error(DeltaDen,na.rm=T))


#General thoughts on patterns
#The time after the bleaching event is very important. I removed the 2016 and 2017 juvenile data for jarvis, baker and howland. it may be interesting to look at these separately


# MIXED MODELING -SPATIAL TEMPORAL PATTERNS -------------------------------

#Check for normality and equal variance
plotNormalHistogram(jcdG_stWS$JuvDenW)
mod<-lm(JuvDenW~ANALYSIS_YEAR,data=jcdG_stWS)
qqnorm(residuals(mod),ylab="Sample Quantiles for residuals")
qqline(residuals(mod), col="red")

summary(mod)  # Report the results
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(mod) 

#Tried log and sqrt - power transformation is the only one that will work.
colden<-jcdG_stWS$JuvColDen + 1
boxcox(colden~jcdG_stWS$ANALYSIS_YEAR)
boxcox(colden~jcdG_stWS$ANALYSIS_YEAR, lambda=seq(-1,1))

dp<-bct(colden,0.2)

plotNormalHistogram(dp)
qqnorm(dp, ylab="Sample Quantiles for logDensity")
qqline(dp, col="red")
mod<-lm(dp~ANALYSIS_YEAR,data=jcdG_stWS)
qqnorm(residuals(mod),ylab="Sample Quantiles for residuals")
qqline(residuals(mod), col="red")

jcdG_stWS$JuvDen_trans=dp



#Test for differences between Years for each region
#NWHI
mod1 <- lmer(JuvDen_trans~ANALYSIS_YEAR+(1|SEC_NAME),data=subset(jcdG_stWS,REGION=="NWHI"))
summary(glht(mod1, linfct = mcp(ANALYSIS_YEAR = "Tukey"))) #this works!

#MHI
modnull <- lmer(JuvDen_trans~1+(1|SEC_NAME),data=subset(jcdG_stWS,REGION=="MHI"))
mod1 <- lmer(JuvDen_trans~ANALYSIS_YEAR+(1|SEC_NAME),data=subset(jcdG_stWS,REGION=="MHI"))
anova(mod1,modnull,test="chisq") #p=0.000049

#WAKE
mod1 <- lm(JuvDen_trans~ANALYSIS_YEAR,data=subset(jcdG_stWS,REGION=="WAKE"))
summary(mod1)

#PHOENIX
modnull <- lmer(JuvDen_trans~1+(1|SEC_NAME),data=subset(jcdG_stWS,REGION=="PHOENIX"))
mod1 <- lmer(JuvDen_trans~ANALYSIS_YEAR+(1|SEC_NAME),data=subset(jcdG_stWS,REGION=="PHOENIX"))
anova(mod1,modnull,test="chisq")

#LINE
modnull <- lmer(JuvDen_trans~1+(1|SEC_NAME),data=subset(jcdG_stWS,REGION=="LINE"))
mod1 <- lmer(JuvDen_trans~ANALYSIS_YEAR+(1|SEC_NAME),data=subset(jcdG_stWS,REGION=="LINE"))
anova(mod1,modnull,test="chisq")


#Samoa
modnull <- lmer(JuvDen_trans~1+(1|SEC_NAME),data=subset(jcdG_stWS,REGION=="SAMOA"))
mod1 <- lmer(JuvDen_trans~ANALYSIS_YEAR+(1|SEC_NAME),data=subset(jcdG_stWS,REGION=="SAMOA"))
anova(mod1,modnull,test="chisq")

#SMARIAN
modnull <- lmer(JuvDen_trans~1+(1|SEC_NAME),data=subset(jcdG_stWS,REGION=="SMARIAN"))
mod1 <- lmer(JuvDen_trans~ANALYSIS_YEAR+(1|SEC_NAME),data=subset(jcdG_stWS,REGION=="SMARIAN"))
anova(mod1,modnull,test="chisq")

#NMARIAN
modnull <- lmer(JuvDen_trans~1+(1|SEC_NAME),data=subset(jcdG_stWS,REGION=="NMARIAN"))
mod1 <- lmer(JuvDen_trans~ANALYSIS_YEAR+(1|SEC_NAME),data=subset(jcdG_stWS,REGION=="NMARIAN"))
anova(mod1,modnull,test="chisq")


#Calculate adjusted pvalues
pvals<-c(0.721,0.133,0.484,0.0000304,0.038,0.02687,0.03583,0.00527,0.8311,0.0903)
round(p.adjust(pvals, "BH"), 10)



jcdG_rS$REGION <- factor(jcdG_rS$REGION, levels = c("NWHI","MHI","WAKE","PHOENIX","LINE","SAMOA","SMARIAN","NMARIAN"))
jcdG_rS$ANALYSIS_YEAR<-as.factor(jcdG_rS$ANALYSIS_YEAR)
#Add Posthoc groupings from LMMs
jcdG_rS<- jcdG_rS[order(jcdG_rS$REGION),];jcdG_rS
jcdG_rS$sig<-c("","","","a","b","","","","","","","a","b","","","","")


#bar plot of juv by region by year with post hoc tests 
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
  scale_fill_viridis(discrete = TRUE) +
  xlab("Year") +
  ylab("Mean Juvenile Colonies/m^2") +
  #scale_y_continuous(expand = c(0,0), limits = c(0,15)) +
  geom_text(aes(x=ANALYSIS_YEAR,y=Mean_JuvColDen+SE_JuvColDen,label=sig, group = REGION),
            position = position_dodge(),
            vjust = -0.5) 
p8


#Delta Density


# delta.df$Delta_trans<-sqrt(delta.df$DeltaDen_yr+2.7)
# plotNormalHistogram(delta.df$Delta_trans)
# mod<-lmer(Delta_trans~REGION +(1|SEC_NAME),data=delta.df)
# qqnorm(residuals(mod),ylab="Sample Quantiles for residuals")
# qqline(residuals(mod), col="red")
# 
# 
# summary(mod)  # Report the results
# par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
# plot(mod) 
# leveneTest(Delta_trans~REGION,data=delta.df)


#Can't transform- use non parametric
kruskal.test(Delta_trans~REGION,data=delta.df)

pairwise.wilcox.test(delta.df$Delta_trans, delta.df$REGION,
                     p.adjust.method = "BH")

#Add Posthoc groupings from LMMs
delta.sumR<- delta.sumR[order(delta.sumR$REGION),];delta.sumR

delta.sumR$REGION <- factor(delta.sumR$REGION, levels = c("NWHI","MHI","WAKE","PHOENIX","LINE","SAMOA","SMARIAN","NMARIAN"))
delta.sumR$sig<-c("abc","a","abc","c","abc","ab","bc","abc")
delta.sumR$years<-c("2017-2014","2019-2016","2017-2014","2018-2015","2018-2015","2018-2015","2017-2014","2017-2014")

p9 <- ggplot(delta.sumR, aes(x=REGION, y=DeltaMEAN,fill=REGION)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve='single'), width = 1, color="black") +
  geom_errorbar(aes(y=DeltaMEAN, x=REGION,ymin=DeltaMEAN-DeltaSE, ymax=DeltaMEAN+DeltaSE), width=.2)+
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
geom_text(data=delta.sumR,aes(x=REGION,y=DeltaMEAN+DeltaSE,label=sig, group = REGION),position = position_dodge(),vjust = -0.5)+
geom_text(data=delta.sumR,aes(x=REGION,y=-0.8,label=years, group = REGION),position = position_dodge(),vjust = -0.8,size=3.5,fontface="bold")
  
p9


#Plotting Delta Density/Year- ISLAND

delta.sumI$REGION <- factor(delta.sumI$REGION, levels = c("NWHI","MHI","WAKE","PHOENIX","LINE","SAMOA","SMARIAN","NMARIAN"))

p10 <- ggplot(delta.sumI, aes(x=reorder(ISLAND,-DeltaMEAN), y=DeltaMEAN,fill=REGION)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve='single'), width = 1, color="black") +
  geom_errorbar(data=delta.sumI,aes(y=DeltaMEAN, x=ISLAND,ymin=DeltaMEAN-DeltaSE, ymax=DeltaMEAN+DeltaSE), width=.2)+
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
  scale_fill_viridis(discrete = TRUE) +
  xlab("Island") +
  ylab(expression("Mean"~Delta~"Juvenile Density/Year"))

p10




#Identify median lat and long that surveys were conducted for each island and year
lat.sum<-ddply(data.gen_tempS,.(REGION,ISLAND),
               summarize,
               Y=median(LATITUDE,na.rm=T),
               X=median(LONGITUDE,na.rm = T))

j.sum<-ddply(delta.df,.(REGION,ISLAND),
             summarize,
             DeltaDen=mean(DeltaDen_yr,na.rm=T))
deltaden_coords<-left_join(j.sum,lat.sum)
head(deltaden_coords)



#Use unweighted delta density ~ Latitiude
plotNormalHistogram(delta.df$DeltaDen)

plotNormalHistogram(sqrt(delta.df$DeltaDen+2.7))
mod<-lm(DeltaDen~LATITUDE,data=deltaden_coords)
qqnorm(residuals(mod),ylab="Sample Quanqqnorm(residuals(mod)")
qqline(residuals(mod), col="red")

mod1<-lm(DeltaDen~LATITUDE,data=deltaden_coords)
anova(mod1);summary(mod1)




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
ggsave(plot=p8,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Figures/DensityRegionalTemporal.jpg",width=10,height=5)
ggsave(plot=p9,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Figures/DeltaRegional.jpg",width=10,height=5)
ggsave(plot=p10,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Figures/DeltaIsland.jpg",width=10,height=5)
ggsave(plot=finalmap,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Figures/DeltaIslandmap.jpg",width=11,height=7)
ggsave(plot=jvd,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Figures/DeltavDepth.jpg",width=10,height=5)
