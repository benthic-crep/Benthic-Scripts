rm(list = ls()) # clear workspace
library(tidyverse)
source("../fish-paste/lib/core_functions.R")
source("./Useful Scripts/Oracle_Fetch.R")

## LOAD Mission Info
MI_24=fetch_data(base_url = "https://picapex.nmfs.local/gis/esd/esd_data/mission_info",YEAR==2024)
MI_25=fetch_data(base_url = "https://picapex.nmfs.local/gis/esd/esd_data/mission_info",YEAR==2025)
#targetMI=MI_24 %>% filter(REGION%in%c("NWHI","MHI")) %>% pull(MISSIONID) %>% unique()
targetMI=MI_25 %>% filter(REGION%in%c("PRIAs","MARIAN")) %>% pull(MISSIONID) %>% unique()

## LOAD: Site visit table from Oracle database that has sectors defined to each site (by Kisei)
SV_25=NULL
for (mi_i in targetMI) {
  # SV_24i=fetch_data(base_url = "https://picapex.nmfs.local/gis/esd/esd_data/site_visit_info",
  #                   MISSIONID==mi_i)
  SV_25i=fetch_data(base_url = "https://picapex.nmfs.local/gis/esd/esd_data/coral_obs_adu",
                    MISSIONID==mi_i)
  SV_25=rbind(SV_25,SV_25i)
}
dim(SV_25)


View(cb)

