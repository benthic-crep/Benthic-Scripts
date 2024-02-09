### Allocation calculations for reef fish surveys

# Fed 8, 2024
# Questions: Thomas Oliver -derived from original script created by Kaylyn McCoy, edited by Courtney Couch

#__________________________________
# This pulls in the last 2 rounds (or so) of data and calculates variance at the genus level. 
# Proportional Variance for each genus is calculated at the strata level and averaged for each strata. 
# Proportional area is calculated from the sectors file at the island level, and is multiplied by the average variance at each strata. 
# This becomes the proportional weight and should be used in conjunction with the proprotional area to allocate survey effort.
# !!!!! FILES NEEDED: First run data pooling scripts 01 and 02 from GitHub to get site level averages: 
# T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS.csv. 
# Summary data files you need are 
# 1) TMPwsd.Rdata, and 
# 2) TMPsectors.Rdata. 
# You will also need the cruise itinerary and the number of sites that can be surveyed in one day.[Or run an Nstar Analysis]
# If we aren't visiting all sectors during the field mission, remove that sector from this calculation -
# i.e. if we aren't going to all marine protected areas in Guam or we aren't sampling backreef in the NWHI, remove those areas in the beginning.
#__________________________________

# set up  -----------------------------------------------------------------------------------------
rm(list=ls())      # clean working environment

# Load libraries
library(tidyverse) # for summarizing/orgainzing data
library(dplyr)     # for summarizing/orgainzing data- LOAD LAST TO PREVENT MASKING
library(ggrepel)

# set wd to relevant folder
setwd("T:/Benthic/Data/StRS Allocation")

# load sector data
sectors<-read.csv("BenthicSectorsforAllocation.csv")

# load demographic site data
#wsd_T<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_TAXONCODE.csv")
wsd_G<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS.csv")

#Specify Target Genera (see "Select Target Taxa" below)
tgen=c("POSP","PAVS","POCS","MOSP","SSSS")

# filter past data for region and nSPC surveys and the last few years/rounds
AllRegSD<-wsd_G %>% filter(OBS_YEAR>"2010") %>% 
  filter(GENUS_CODE%in%tgen) %>% # group by strata
  filter(!SEC_NAME%in%c("Lahaina")) %>% #Resolve Problem Sectors (i.e. Lahaina)
  group_by(REGION,ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN,GENUS_CODE) %>% 
  summarize(SD = sd(AdColDen)) %>% 
  drop_na() # drop strata with no data

# Focal Regions (i.e. in 24, NWHI and MHI) -------------------------------------------------------------------------
REG = AllRegSD %>% filter(REGION %in% c("NWHI","MHI"))

# # !!!!!! filter for reef zones/islands NOT being surveyed here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # for example, take out Lagoon and backreef
REG<-REG %>% filter(REEF_ZONE != "Lagoon",REEF_ZONE != "Backreef")
REG<-droplevels(REG)

# get total SD per island in order to get proportion of each genera's group's variance
ISL.GEN_SD<-REG %>% group_by(ISLAND,GENUS_CODE) %>%
  summarise(SUM_SD=sum(SD)) 

# join totals back to original dataframe to calculate proportion
STR.GEN_pSD<-left_join(REG,ISL.GEN_SD,by=c("ISLAND","GENUS_CODE")) %>%
  mutate(pSD = (SD/SUM_SD))

# calculate average SD for each strata - first isloate proportion of SD, get rid of sd columns
STR_pSD<-STR.GEN_pSD %>% #mean prop var across four rep. genera (POSP,MOSP,PAVS,POCS) and all taxa (SSSS)
  group_by(REGION,ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>% 
  summarise(pSD=mean(pSD))  # get the mean at the strata level

# get area values for each sector from the 'sectors' dataframe
STR_pSD_A<-sectors %>% select(REGION,ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN, AREA_HA)%>% 
  right_join(STR_pSD, by=c("REGION","ISLAND", "SEC_NAME", "REEF_ZONE", "DEPTH_BIN")) # right joining to the previous dataframe selects values for just this region

ISL_AREA<-sectors %>% group_by(REGION,ISLAND) %>%
  summarize(ISLAND_AREA=sum(AREA_HA)) # summarize area by island

STR_pSD_pA_prodSDA<-STR_pSD_A %>% 
  left_join(ISL_AREA,by=c("REGION","ISLAND")) %>% 
  mutate(pAREA=AREA_HA/ISLAND_AREA) %>% 
  select(REGION,ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN,pSD,pAREA) %>% 
  group_by(REGION,ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN) %>% 
  mutate(prodSDA=prod(c(pSD,pAREA)))# calculate proportional area for each strata

ISL_prodSDA<-STR_pSD_pA_prodSDA %>%
  group_by(ISLAND) %>% 
  summarize(prodSDA_ISL=sum(prodSDA))

STR_pSDA<-STR_pSD_pA_prodSDA %>% 
  left_join(ISL_prodSDA,by="ISLAND") %>% 
  mutate(pSDA=prodSDA/prodSDA_ISL) %>% # calculate the proportional weight for each strata
  select(REGION,ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN,pSD,pAREA,pSDA) # clean up dataframe

# JUST A CHECK: does total variance for each island add up to 1?
test<-STR_pSDA %>% 
  group_by(ISLAND) %>% 
  summarize(sum=sum(pSDA))
test

# to calculate allocation based on weights:
##LOAD DAYS PER ISLAND ALLOCATION
# get list of islands - helpful to build DpI .csv
# data.frame(RI=unique(STR_pSDA[,c("REGION","ISLAND")]))
DpI=read.csv("NCRMP24_DAYSperISLAND_20240208.csv")

#This gives small boat days per island, now need to assume 3, 4 or 5 sfm sites per day
STR_ALLOC=STR_pSDA %>% 
  left_join(DpI,by=c("REGION","ISLAND")) %>% 
  mutate(SpSBD.3=3,SpSBD.4=4,SpSBD.5=5) %>% 
  pivot_longer(cols=c("SpSBD.3","SpSBD.4","SpSBD.5"),values_to = "SITESpSBD",names_to=NULL) %>% 
  mutate(SDxA_ALLOC=round(SMALL_BOAT_DAYS*SITESpSBD*pSDA),
         AREA_ALLOC=round(SMALL_BOAT_DAYS*SITESpSBD*pAREA)) %>% 
  pivot_wider(names_from = "SITESpSBD",values_from = c("SDxA_ALLOC","AREA_ALLOC"))

# save file
write.csv(STR_ALLOC,file="NCRMP24_BOATDAYS_Allocation_20240208.csv",row.names = FALSE)


# #Select Target Taxa: ----------------------------------------------------
NsitesPregion=wsd_G %>% group_by(REGION)  %>% summarize(nSpR=length(unique(SITEVISITID)))
TaxChoice=wsd_T %>% group_by(REGION,TAXONCODE) %>%
  filter(AdColDen>0) %>% 
  summarize(nS=length(AdColDen),mnDen=mean(AdColDen,na.rm=T)) %>% 
  left_join(NsitesPregion,by="REGION") %>% 
  mutate(pS=nS/nSpR)

TaxChoice %>% filter(TAXONCODE!="SSSS") %>%
  ggplot()+
  geom_label(aes(pS,mnDen,label=TAXONCODE),size=3)+
  geom_label(aes(pS,mnDen,label=TAXONCODE),size=3,fill="lightblue",
             data=subset(TaxChoice,TAXONCODE%in%c("PLOB","PVAR","PMVC","MOSP")))+
  facet_wrap("REGION",scales="free")+
  xlab("Widespread (prop. of sites)")+
  ylab("Abundant (mean non-zero abundance)")+
  scale_x_sqrt()+
  scale_y_sqrt()+
  theme_bw()

GenChoice=wsd_G %>% group_by(REGION,GENUS_CODE) %>%
  filter(AdColDen>0) %>% 
  summarize(nS=length(AdColDen),mnDen=mean(AdColDen,na.rm=T)) %>% 
  left_join(NsitesPregion,by="REGION") %>% 
  mutate(pS=nS/nSpR)

GenChoice %>% 
  #filter(GENUS_CODE!="SSSS") %>%
  ggplot()+
  geom_label(aes(pS,mnDen,label=GENUS_CODE),size=3)+
  geom_label(aes(pS,mnDen,label=GENUS_CODE),size=3,fill="lightblue",
             data=subset(GenChoice,GENUS_CODE%in%c("POSP","PAVS","POCS","MOSP")))+
  geom_label(aes(pS,mnDen,label=GENUS_CODE),size=3,fill="gold",
             data=subset(GenChoice,GENUS_CODE%in%c("SSSS")))+
  facet_wrap("REGION",scales="free")+
  xlab("Widespread (prop. of sites)")+
  ylab("Abundant (mean non-zero abundance)")+
  scale_x_sqrt()+
  scale_y_sqrt()+
  theme_bw()