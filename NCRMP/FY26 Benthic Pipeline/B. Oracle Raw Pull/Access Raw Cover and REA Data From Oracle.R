rm(list=ls())
pkgs <- names(sessionInfo()$otherPkgs)
for (package in pkgs) {
  detach(paste0("package:", package), unload = TRUE, character.only = TRUE)
}
source("./Useful Scripts/Oracle_Fetch.R")

#First Draw Cover from Oracle, Deposit on T: Drive in "Raw from Oracle" Folder
# Will pull data from relevant tables, fixed and random cover, ad/juv rea
# In Five regions: "MARIAN","SAMOA","PRIAs","NWHI","MHI"
# From: 2010-2024
# Any further special mission exclusions need to happen in appropriate script downstream
#setwd("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/")

## LOAD Mission Info
MI_2010_2024=fetch_data(base_url = "https://picapex.nmfs.local/gis/esd/esd_data/mission_info",
                        YEAR%in%c(2010:2024))
MI_2010_2024 %>% count(REGION,YEAR)
targetMI_All=MI_2010_2024 %>%
  filter(REGION%in%c("MARIAN","SAMOA","PRIAs","NWHI","MHI"),
         YEAR%in%c(2010:2024)) %>% arrange(YEAR) %>% pull(MISSIONID) %>% unique() 

#########################################################################################################
# Pull Recent Years Misson Data
#########################################################################################################
# CNET Cover Data
targetMI_24=MI_2010_2024 %>% 
  filter(REGION%in%c("MARIAN","SAMOA","PRIAs","NWHI","MHI"),
         YEAR%in%c(2024)) %>% arrange(YEAR) %>% pull(MISSIONID) %>% unique() 

COV_RAN_2024=NULL
COV_FIX_2024=NULL
REA_AD_2024=NULL
REA_JV_2024=NULL
for (mi_i in targetMI_24) {
  print(mi_i)
  #Random
  COV_RAN_2024i=fetch_data(base_url = "https://picapex.nmfs.local/gis/esd/esd_data/benthic_cover_random",
                                MISSIONID==mi_i)
  COV_RAN_2024=rbind(COV_RAN_2024,COV_RAN_2024i)
  #Fixed
  COV_FIX_2024i=fetch_data(base_url = "https://picapex.nmfs.local/gis/esd/esd_data/benthic_cover_fixed",
                                MISSIONID==mi_i)
  COV_FIX_2024=rbind(COV_FIX_2024,COV_FIX_2024i)
  #ADULT
  REA_AD_2024i=fetch_data(base_url = "https://picapex.nmfs.local/gis/esd/esd_data/coral_obs_adu",
                               MISSIONID==mi_i)
  REA_AD_2024=rbind(REA_AD_2024,REA_AD_2024i)
  #JUV
  REA_JV_2024i=fetch_data(base_url = "https://picapex.nmfs.local/gis/esd/esd_data/coral_obs_juv",
                               MISSIONID==mi_i)
  REA_JV_2024=rbind(REA_JV_2024,REA_JV_2024i)
}

#Write out Cover data in .rdata
save(list = "COV_RAN_2024",
     file = "T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/FY26/Raw_COV_RANDOM_CNET_2024.rdata")
save(list = "COV_FIX_2024",
     file = "T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/FY26/Raw_COV_FIXED_CNET_2024.rdata")
#Write out REA data in .rdata
save(list = "REA_AD_2024",
     file = "T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/FY26/Raw_REA_ADULT_2024.rdata")
save(list = "REA_JV_2024",
     file = "T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/FY26/Raw_REA_JUVENILE_2024.rdata")



#########################################################################################################
# Pull All Years Misson Data
#########################################################################################################
#Pull All Mission IDs with Cover data, Sort out which to retain after the fact.
#Get access API URLS from: https://picapex.nmfs.local/gis/r/esd/dash/data-access-api

# CNET Cover Data
COV_RAN_2010_2024=NULL
COV_FIX_2010_2024=NULL
for (mi_i in targetMI_All) {
  print(mi_i)
  #Random
  COV_RAN_2010_2024i=fetch_data(base_url = "https://picapex.nmfs.local/gis/esd/esd_data/benthic_cover_random",
                                MISSIONID==mi_i)
  COV_RAN_2010_2024=rbind(COV_RAN_2010_2024,COV_RAN_2010_2024i)
  #Fixed
  COV_FIX_2010_2024i=fetch_data(base_url = "https://picapex.nmfs.local/gis/esd/esd_data/benthic_cover_fixed",
                                MISSIONID==mi_i)
  COV_FIX_2010_2024=rbind(COV_FIX_2010_2024,COV_FIX_2010_2024i)
}

#CPCE Cover Data
COV_RAN_CPCE_2010_2024=NULL
COV_FIX_CPCE_2010_2024=NULL
for (mi_i in targetMI_All) {
  print(mi_i)
  #Random
  COV_RAN_CPCE_2010_2024i=fetch_data(base_url = "https://picapex.nmfs.local/gis/esd/esd_data/mv_bia_cpce_str",
                                MISSIONID==mi_i)
  COV_RAN_CPCE_2010_2024=rbind(COV_RAN_CPCE_2010_2024,COV_RAN_CPCE_2010_2024i)
  #Fixed
  COV_FIX_CPCE_2010_2024i=fetch_data(base_url = "https://picapex.nmfs.local/gis/esd/esd_data/mv_bia_cpce_cli",
                                MISSIONID==mi_i)
  COV_FIX_CPCE_2010_2024=rbind(COV_FIX_CPCE_2010_2024,COV_FIX_CPCE_2010_2024i)
}

#Pull All Mission IDs with REA data, Sort out which to retain after the fact.
#Get access API URLS from: https://picapex.nmfs.local/gis/r/esd/dash/data-access-api
REA_AD_2010_2024=NULL
REA_JV_2010_2024=NULL
for (mi_i in targetMI_All) {
  print(mi_i)
  #ADULT
  REA_AD_2010_2024i=fetch_data(base_url = "https://picapex.nmfs.local/gis/esd/esd_data/coral_obs_adu",
                               MISSIONID==mi_i)
  REA_AD_2010_2024=rbind(REA_AD_2010_2024,REA_AD_2010_2024i)
  #JUV
  REA_JV_2010_2024i=fetch_data(base_url = "https://picapex.nmfs.local/gis/esd/esd_data/coral_obs_juv",
                               MISSIONID==mi_i)
  REA_JV_2010_2024=rbind(REA_JV_2010_2024,REA_JV_2010_2024i)
}

dim(COV_RAN_2010_2024)
COV_RAN_2010_2024 %>% count(MISSIONID)

dim(COV_FIX_2010_2024)
COV_FIX_2010_2024 %>% count(MISSIONID)

dim(COV_RAN_CPCE_2010_2024)
COV_RAN_CPCE_2010_2024 %>% count(MISSIONID)

dim(COV_FIX_CPCE_2010_2024)
COV_FIX_CPCE_2010_2024 %>% count(MISSIONID)

dim(REA_AD_2010_2024)
REA_AD_2010_2024 %>% count(MISSIONID)

dim(REA_JV_2010_2024)
REA_JV_2010_2024 %>% count(MISSIONID)

#Write out Cover data in .rdata
save(list = "COV_RAN_2010_2024",
     file = "T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/FY26/Raw_COV_RANDOM_CNET_2010-2024.rdata")
save(list = "COV_FIX_2010_2024",
     file = "T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/FY26/Raw_COV_FIXED_CNET_2010-2024.rdata")
save(list = "COV_RAN_CPCE_2010_2024",
     file = "T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/FY26/Raw_COV_RANDOM_CPCE_2010-2014.rdata")
save(list = "COV_FIX_CPCE_2010_2024",
     file = "T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/FY26/Raw_COV_FIXED_CPCE_2010-2014.rdata")

#Write out REA data in .rdata
save(list = "REA_AD_2010_2024",
     file = "T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/FY26/Raw_REA_ADULT_2013-2024.rdata")
save(list = "REA_JV_2010_2024",
     file = "T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/FY26/Raw_REA_JUVENILE_2013-2024.rdata")

######################################################################################