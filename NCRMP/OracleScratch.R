#Clear Data and Packages
rm(list=ls())
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

#Load Libraries
source("./Useful Scripts/Oracle_Fetch.R")

#First Draw Cover from Oracle, Deposit on T: Drive in "Raw from Oracle" Folder
# Will pull data from relevant tables, fixed and random cover, ad/juv rea
# In Five regions: "MARIAN","SAMOA","PRIAs","NWHI","MHI"
# From: 2010-2024
# Any further special mission exclusions need to happen in appropriate script downstream
#setwd("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/")

## LOAD Mission Info
MI_2010_2024=fetch_data(base_url = "https://picapex.nmfs.local/gis/esd/esd_data/mission_info",
                        YEAR%in%c(2010:2025))
MI_2010_2024 %>% count(REGION,YEAR)
targetMI_All=MI_2010_2024 %>%
  filter(REGION%in%c("MARIAN","SAMOA","PRIAs","NWHI","MHI"),
         YEAR%in%c(2010:2025)) %>% arrange(YEAR) %>% pull(MISSIONID) %>% unique() 

#########################################################################################################
# Pull Recent Years Misson Data
#########################################################################################################
# CNET Cover Data
targetMI_24=MI_2010_2024 %>% 
  filter(REGION%in%c("MARIAN","SAMOA","PRIAs","NWHI","MHI"),
         YEAR%in%c(2025)) %>% arrange(YEAR) %>% pull(MISSIONID) %>% unique() 

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
  # #ADULT
  # REA_AD_2024i=fetch_data(base_url = "https://picapex.nmfs.local/gis/esd/esd_data/coral_obs_adu_ncrmp",
  #                         MISSIONID==mi_i)
  # REA_AD_2024=rbind(REA_AD_2024,REA_AD_2024i)
  # #JUV
  # REA_JV_2024i=fetch_data(base_url = "https://picapex.nmfs.local/gis/esd/esd_data/coral_obs_juv_ncrmp",
  #                         MISSIONID==mi_i)
  # REA_JV_2024=rbind(REA_JV_2024,REA_JV_2024i)
}
