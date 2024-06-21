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
if(!is.null(names(sessionInfo()$otherPkgs))){
  invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
}


# Load libraries
library(tidyverse) # for summarizing/orgainzing data
library(dplyr)     # for summarizing/orgainzing data- LOAD LAST TO PREVENT MASKING
library(ggrepel)

# set wd to relevant folder
setwd("T:/Benthic/Data/StRS Allocation")

# load sector data
sectors<-read.csv("BenthicSectorsforAllocation.csv")
strataarea=read.csv("C:/Users/Thomas.Oliver/WORK/Projects/GitHub Projects/fish-paste/data/Sectors-Strata-Areas.csv")
strataarea=strataarea %>% filter(REGION%in%c("MHI","NWHI"))
# NWHIstrataarea=read.csv("C:/Users/Thomas.Oliver/Downloads/strata_table_NWHI.csv") #original Area in km2 (100 HA)
# NWHIstrataarea=NWHIstrataarea %>% mutate(AREA_HA_2024.B=100*strat_area) %>% select( REGION,ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN,AREA_HA_2024.B)
# strataarea=strataarea %>% left_join(NWHIstrataarea) %>% filter(REGION=="NWHI")

#Changes
# RZlu=c("F","B","L","O","S")
# names(RZlu)=unique(strataarea$REEF_ZONE)
# DBlu=c("M","S","D","O")
# names(DBlu)=unique(strataarea$DEPTH_BIN)
# delarea=strataarea %>% filter(REGION%in%c("MHI","NWHI")) %>% 
#   filter(!is.na(AREA_HA_2024)) %>% 
#   mutate(A2324delta=AREA_HA_2024-AREA_HA_2023,
#          A2324deltaB=AREA_HA_2024.B-AREA_HA_2023,
#          STR_NAME=paste0(SEC_NAME,"_",RZlu[REEF_ZONE],DBlu[DEPTH_BIN])) 
# 
# DAplot=delarea %>% 
#   ggplot(aes(x=AREA_HA_2023,label=STR_NAME,color=A2324delta))+
#   geom_point(aes(y=AREA_HA_2024,color=A2324delta),shape=4,size=6)+
#   geom_point(aes(y=AREA_HA_2024.B,color=A2324deltaB),shape=20,size=6)+
#   facet_wrap("ISLAND",scales="free")+
#   geom_abline()+theme_bw()+
#   geom_text_repel(aes(y=AREA_HA_2024),size=3)+
#   geom_text_repel(aes(y=AREA_HA_2024.B),size=3)+
#   scale_color_viridis_c()#+scale_x_log10()+scale_y_log10()
# ggsave(plot = DAplot,filename = "C:/Users/Thomas.Oliver/Downloads/DAplot.jpg",width=16,height=8)
# load demographic site data
# raw<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED_2023.csv")
# rawraw<-load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_ADULTCORAL_RAW_2013-2023.rdata")
# wsd_T<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_TAXONCODE_2023.csv")
wsd_G<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS_2023.csv")
wsd_SSSS=wsd_G %>% filter(GENUS_CODE=="SSSS")
#wsd_F=load("C:/Users/Thomas.Oliver/WORK/Projects/GitHub Projects/fish-paste/data/All")
cov=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicCover_2010-2023_Tier1_STRATA_updated.csv")
f=load("C:/Users/Thomas.Oliver/WORK/Projects/GitHub Projects/fish-paste/data/ALL_REA_FISH_RAW.rdata")

#Specify Target Genera (see "Select Target Taxa" below)
tgen=c("POSP","PAVS","POCS","MOSP","SSSS")

# filter past data for region and nSPC surveys and the last few years/rounds
# AllRegSD<-wsd_G %>% filter(OBS_YEAR>"2010") %>% 
#   filter(GENUS_CODE%in%tgen) %>% # group by strata
#   filter(!SEC_NAME%in%c("Lahaina")) %>% #Resolve Problem Sectors (i.e. Lahaina) ## Do this by ANYear/REGION selected 19 - 16
#   group_by(REGION,ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN,GENUS_CODE) %>% #Use most recent adequate survey year...
#   summarize(SD = sd(AdColDen)) %>% 
#   drop_na() # drop strata with no data

# Focal Regions (i.e. in 24, NWHI and MHI) -------------------------------------------------------------------------
#Use most recent adequate survey year...
REG1=wsd_G %>% filter(REGION=="MHI",ANALYSIS_YEAR%in%c("2013","2016","2019"))
REG2=wsd_G %>% filter(REGION=="NWHI",ANALYSIS_YEAR%in%c("2013-15","2016","2017"))
AYNlu=c(2013,2014,2016,2017,2019);names(AYNlu)=sort(unique(c(REG1$ANALYSIS_YEAR,REG2$ANALYSIS_YEAR)))
REG_SD=rbind(REG1,REG2) %>%
  mutate(ANALYSIS_YR_NUM=as.numeric(AYNlu[ANALYSIS_YEAR])) %>% 
  group_by(REGION,ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN,GENUS_CODE,ANALYSIS_YR_NUM) %>% 
  filter(GENUS_CODE%in%tgen) %>% #focal taxa only
  dplyr::summarize(SD = sd(AdColDen)) %>% 
  group_by(REGION,ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN,GENUS_CODE) %>% #take most recent non-NA year
  na.omit() %>% 
  filter(ANALYSIS_YR_NUM == max(ANALYSIS_YR_NUM))

#Add back strata with 0-1 samples per year (SD=NA)
REG=rbind(REG1,REG2) %>%
  mutate(ANALYSIS_YR_NUM=as.numeric(AYNlu[ANALYSIS_YEAR])) %>% 
  dplyr::select(setdiff(names(REG_SD),c("SD","ANALYSIS_YEAR_NUM")))%>% 
  filter(GENUS_CODE%in%tgen) %>%
  group_by(REGION,ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN,GENUS_CODE) %>% #take most recent non-NA year
  filter(ANALYSIS_YR_NUM == max(ANALYSIS_YR_NUM)) %>% 
  distinct() %>% left_join(REG_SD)

#Add Maui Sectors we've never sampled (before adding island mean SD)
ExtraMAI=expand.grid(REGION="MHI",
                    ISLAND="Maui",
                    SEC_NAME=c("MAI_HANA","MAI_KAHULUI","MAI_SE"),
                    REEF_ZONE="Forereef",
                    DEPTH_BIN=c("Deep","Mid","Shallow"),
                    GENUS_CODE=c("MOSP","PAVS","POCS","POSP","SSSS"),
                    ANALYSIS_YR_NUM=2019,
                    SD=NA)
ExtraNWHI=data.frame(REGION="NWHI",
                     ISLAND=c("Pearl & Hermes","Pearl & Hermes","Kure","Kure"),
                     SEC_NAME=c("Pearl & Hermes","Pearl & Hermes","Kure","Kure"),
                     REEF_ZONE=c("Lagoon","Backreef","Backreef","Lagoon"),
                     DEPTH_BIN=c("Mid","Shallow","Shallow","Shallow"),
                     GENUS_CODE=c("SSSS"),
                     ANALYSIS_YR_NUM=2019,
                     SD=NA)
REG=rbind(REG,ExtraMAI,ExtraNWHI)

SCENARIO="NoExtra"
if(SCENARIO!="Extra"){
  REG=REG %>% 
    filter(!(SEC_NAME%in%c("MAI_HANA","MAI_KAHULUI","MAI_SE"))) %>% 
    filter(REEF_ZONE=="Forereef")
}
#Add variability estimates that are NA back at ISLAND-DEPTH_BIN mean SD
ISL.GEN_mnSD<-REG %>% group_by(ISLAND,GENUS_CODE) %>%
  summarise(MN_SD=mean(SD,na.rm=T)) 
SD_FILLi=which(is.na(REG$SD))
REG[SD_FILLi,"SD"]=left_join(REG[,setdiff(names(REG),c("SD","ANALYSIS_YEAR"))],ISL.GEN_mnSD)[SD_FILLi,"MN_SD"]
REG$ANALYSIS_YEAR=as.character(REG$ANALYSIS_YR_NUM)
REG[SD_FILLi,"ANALYSIS_YEAR"]="ISL-DB MN"

# # !!!!!! filter for reef zones/islands NOT being surveyed here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Going to run across all habitats
#REG<-REG %>% filter(REEF_ZONE != "Lagoon",REEF_ZONE != "Backreef")
#REG<-droplevels(REG)

#DROP ISLANDS NOT IN ALLOCATION
REG=REG %>% filter(!ISLAND%in%c("Maro","Laysan","Midway"))# %>% filter(ISLAND=="Oahu")

# get total SD per island in order to get proportion of each genera's group's variance
ISL.GEN_SD<-REG %>% group_by(ISLAND,GENUS_CODE) %>%
  summarise(SUM_SD=sum(SD,na.rm=T)) 

# join totals back to original dataframe to calculate proportion
STR.GEN_pSD<-left_join(REG,ISL.GEN_SD,by=c("ISLAND","GENUS_CODE")) %>%
  mutate(pSD = (SD/SUM_SD))

# calculate average SD for each strata - first isloate proportion of SD, get rid of sd columns
STR_pSD<-STR.GEN_pSD %>% #mean prop var across four rep. genera (POSP,MOSP,PAVS,POCS) and all taxa (SSSS)
  group_by(REGION,ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>% 
  summarise(pSD=mean(pSD,na.rm=T))  # get the mean at the strata level

# get area values for each sector from the 'sectors' dataframe
STR_pSD_A<-strataarea %>% dplyr::select(REGION,ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN, AREA_HA_2024)%>% 
  right_join(STR_pSD, by=c("REGION","ISLAND", "SEC_NAME", "REEF_ZONE", "DEPTH_BIN")) # right joining to the previous dataframe selects values for just this region

ISL_AREA<-strataarea %>% group_by(REGION,ISLAND) %>%
  dplyr::summarize(ISLAND_AREA=sum(AREA_HA_2024,na.rm=T)) # summarize area by island

STR_pSD_pA_prodSDA<-STR_pSD_A %>% 
  left_join(ISL_AREA,by=c("REGION","ISLAND")) %>% 
  mutate(pAREA=AREA_HA_2024/ISLAND_AREA) %>% 
  dplyr::select(REGION,ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN,pSD,pAREA) %>% 
  group_by(REGION,ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN) %>% 
  mutate(prodSDA=prod(c(pSD,pAREA)))# calculate proportional area for each strata

ISL_prodSDA<-STR_pSD_pA_prodSDA %>%
  group_by(ISLAND) %>% 
  dplyr::summarize(prodSDA_ISL=sum(prodSDA))

STR_pSDA<-STR_pSD_pA_prodSDA %>% 
  left_join(ISL_prodSDA,by="ISLAND") %>% 
  mutate(pSDA=prodSDA/prodSDA_ISL) %>% # calculate the proportional weight for each strata
  dplyr::select(REGION,ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN,pSD,pAREA,pSDA) # clean up dataframe

# JUST A CHECK: does total variance for each island add up to 1?
test<-STR_pSDA %>% 
  group_by(ISLAND) %>% 
  dplyr::summarize(sum=sum(pSDA))
test

# to calculate allocation based on weights:
##LOAD DAYS PER ISLAND ALLOCATION
# get list of islands - helpful to build DpI .csv
# data.frame(RI=unique(STR_pSDA[,c("REGION","ISLAND")]))
#DpI=read.csv("NCRMP24_DAYSperISLAND_20240618.csv")
#DpI=read.csv("NCRMP24_DAYSperISLAND_20240618.csv")
SpI=read.csv("T:/Benthic/Data/StRS Allocation/NCRMP24_SITESperISLAND_20240620.csv") %>% 
  select(c("REGION","ISLAND","ISLANDCODE","SITES_ITIN"))

#All Sectors (including Maui Extras)
STR_ALLOC=STR_pSDA %>% 
  left_join(SpI,by=c("REGION","ISLAND")) %>% 
  mutate(SDxA_ALLOC=round(SITES_ITIN*pSDA),
         AREA_ALLOC=round(SITES_ITIN*pAREA))  

#Old Way
# STR_ALLOC=STR_pSDA %>% 
#   left_join(SpI,by=c("REGION","ISLAND")) %>% 
#  mutate(SpSBD.2=2,SpSBD.2.5=2.5,SpSBD.3=3,SpSBD.3.5=3.5,SpSBD.4=4,SpSBD.5=5) %>% 
#  pivot_longer(cols=c("SpSBD.2","SpSBD.2.5","SpSBD.3","SpSBD.3.5","SpSBD.4","SpSBD.5"),values_to = "SITESpSBD",names_to=NULL) %>% 
  # mutate(SDxA_ALLOC=round(SITES_ITIN*pSDA),
  #        AREA_ALLOC=round(SITES_ITIN*pAREA)) #%>% 
#  pivot_wider(names_from = "SITESpSBD",values_from = c("SDxA_ALLOC","AREA_ALLOC"))



#Historic
REG1=wsd_SSSS %>% filter(REGION=="MHI")
REG2=wsd_SSSS %>% filter(REGION=="NWHI")
#AYNlu=c(2013,2014,2016,2017,2019);names(AYNlu)=sort(unique(c(REG1$ANALYSIS_YEAR,REG2$ANALYSIS_YEAR)))
REG_History=rbind(REG1,REG2) %>%
#  mutate(ANALYSIS_YR_NUM=as.numeric(AYNlu[ANALYSIS_YEAR])) %>% 
  group_by(REGION,ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN,ANALYSIS_YEAR) %>% 
  summarize(Nsites=length(unique(SITEVISITID))) %>% 
  arrange(ANALYSIS_YEAR) %>%# filter(ANALYSIS_YR_NUM%in%c(2013,2016,2019)) %>% 
  pivot_wider(names_from = ANALYSIS_YEAR,values_from = Nsites,names_prefix = "N_") %>% 
  replace_na(list(N_2013=0,N_2019=0,N_2016=0,`N_2013-15`=0,N_2014=0,N_2017=0)) 
REG_History$N_2013_15=REG_History$'N_2013-15'+REG_History$N_2014+REG_History$N_2013
REG_History$N_2016_17=REG_History$N_2016+REG_History$N_2017
REG_History=REG_History %>%
  select(REGION,ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN,N_2013_15,N_2016_17,N_2019) %>% 
  arrange(REGION,ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN)


#Add History
STR_ALLOC=STR_ALLOC %>% left_join(REG_History)

##KEEP MANUAL FOR NOW
#Tweak Sector allocation, then Island to ensure strata N>2
# STR_ALLOC_MAN=STR_ALLOC
# STR_ALLOC_MAN=STR_ALLOC_MAN %>% filter(!ISLAND%in%c("Oahu"))
# STR_ALLOC_MAN$STR_NAME=paste0(STR_ALLOC_MAN$SEC_NAME,"_",STR_ALLOC_MAN$REEF_ZONE,"_",STR_ALLOC_MAN$DEPTH_BIN)
# col2tweak=grep("SDxA",names(STR_ALLOC))
# for(ci in col2tweak){
#   Man_col=paste0(names(STR_ALLOC_MAN)[ci],"_MAN")
#   STR_ALLOC_MAN[,Man_col]=STR_ALLOC_MAN[,ci]
#   uLT2=which(STR_ALLOC_MAN[,Man_col]<2)
#   for (stri in uLT2){
#     #sec_tot=STR_ALLOC_MAN %>% filter(SEC_NAME==STR_ALLOC_MAN$SEC_NAME[stri])
#     isl_tot=STR_ALLOC_MAN %>% filter(ISLAND==STR_ALLOC_MAN$ISLAND[stri])
#     #check what we need to add to make stri 2
#     ltdiff=2-STR_ALLOC_MAN[stri,Man_col]
#     #take away from highest island allocation if any above 2
#     islmaxstr=isl_tot$STR_NAME[which(isl_tot[,Man_col]==max(isl_tot[,Man_col],na.rm=T))[1]]
#     islmaxi=which(STR_ALLOC_MAN$STR_NAME==islmaxstr)
#     #check if new total will be at least 2
#     if((STR_ALLOC_MAN[islmaxi,Man_col]-ltdiff)>=2){
#       #deduct
#       STR_ALLOC_MAN[islmaxi,Man_col]=STR_ALLOC_MAN[islmaxi,Man_col]-ltdiff  
#       #add
#       STR_ALLOC_MAN[stri,Man_col]=STR_ALLOC_MAN[stri,Man_col]+ltdiff
#     }else{
#       #if can't do it, can't do it - leave as is
#       print(paste0(Man_col,": Ran out of island allocation"))
#     }
#   }
# }

# save file

write.csv(STR_ALLOC,file=paste0("./NCRMP24_SITES_Allocation_",Sys.Date(),"_",SCENARIO,".csv"),row.names = FALSE)


# #Select Target Taxa: ----------------------------------------------------
NsitesPregion=wsd_G %>% group_by(REGION)  %>% dplyr::summarize(nSpR=length(unique(SITEVISITID)))
TaxChoice=wsd_T %>% group_by(REGION,TAXONCODE) %>%
  filter(AdColDen>0) %>% 
  dplyr::summarize(nS=length(AdColDen),mnDen=mean(AdColDen,na.rm=T)) %>% 
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
  dplyr::summarize(nS=length(AdColDen),mnDen=mean(AdColDen,na.rm=T)) %>% 
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