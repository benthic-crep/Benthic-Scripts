
# Using R version 4.1.0 (2021-05-18)

rm(list=ls())
dir = Sys.info()[7]
setwd("T:/Benthic/Data/Data Requests/BexTurner/")

library(dplyr)


bexsites <- read.csv("T:/Benthic/Data/Data Requests/BexTurner/NCRMPBenthicCover_2010-2014_Tier1_SITE.csv") # wave data generated from the 15m depth contour to help fill in the gaps from where the fish sites weren't surveyed
sm <- read.csv("C:/Users/courtney.s.couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv") # wave data from historical fish sites 
sm.new<- sm %>% select(SITEVISITID, Benthic, Fish, new_MIN_DEPTH_M, new_MAX_DEPTH_M)
df<-left_join(bexsites,sm.new)

sh_issues<-df%>% filter(DEPTH_BIN =="Shallow" & new_MIN_DEPTH_M>6)
mid_issues<-df%>% filter((DEPTH_BIN =="Mid") & (new_MAX_DEPTH_M<6 | new_MIN_DEPTH_M >18))
deep_issues<-df%>% filter(DEPTH_BIN =="Deep" & new_MAX_DEPTH_M<18)

View(sh_issues)
View(mid_issues)
View(deep_issues)

site.issues<-rbind(mid_issues,deep_issues)

bad.sites<-unique(site.issues$SITEVISITID)

sm.issues<-sm %>% filter(SITEVISITID %in% bad.sites)
#site.issues<- site.issues %>% select(SITE, Benthic, Fish, new_MIN_DEPTH_M, new_MAX_DEPTH_M,DEPTH_BIN,DATE_)

write.csv(sm.issues,"NCRMPdepthissues.csv")

#save file with depth bin and continuous depths
bexsites.new<- bexsites %>%
  left_join(sm.new)%>%
  select(-c(Benthic,Fish)) %>%
  rename(MIN_DEPTH_M = new_MIN_DEPTH_M, MAX_DEPTH_M = new_MAX_DEPTH_M)
View(bexsites.new)

write.csv(bexsites.new,"NCRMPBenthicCover_2010-2014_Tier1_SITE_wdepth.csv")
