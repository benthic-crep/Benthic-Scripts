
rm(list=ls())

#Set Run Flags
DEBUG=TRUE

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/fish_team_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/Islandwide Mean&Variance Functions.R")



setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project")

#LOAD DATA
jwd_strat<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/JuvProject_pb_STRATA.csv")#Post bleaching strata-level juvenile data
jwd_site<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/JuvProject_pb_SITE.csv")#Post bleaching strata-level juvenile data
d_strat<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Response Variables/JuvProject_deltadensity_STRATA.csv");d_strat<-subset(d_strat,select=-c(X))

depth_strat<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Predictor Variables/JuvProject_Depth.csv")
cover_strat<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Predictor Variables/JuvProject_Cover_STRATA.csv")
sh_strat<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Predictor Variables/JuvProject_SubstrateHeight_STRATA.csv")

cover1<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2019_Tier1_SITE.csv")#Cover from all sites
cover3<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2019_Tier3_SITE.csv")#Cover from all sites
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

#Only use Total Scl juvenile data 
jwd_siteS<-subset(jwd_site,GENUS_CODE=="SSSS")


#Plot Juv Den vs. Depth at Site level- is there a cutoff for where juveniles start to decline?
jwd_siteS$ANALYSIS_YEAR<-as.factor(jwd_siteS$ANALYSIS_YEAR)
  
p1<-jwd_siteS %>%
  mutate(REGION = fct_relevel(REGION,"NWHI","MHI","PHOENIX","LINE","SAMOA","SMARIAN","NMARIAN")) %>% #reorder varibles 
  ggplot(aes(x=MAX_DEPTH_M, y=JuvColDen, color=ANALYSIS_YEAR)) + 
  geom_smooth(se=T,method="lm",lwd=1.5)+
  geom_point()+
  facet_wrap(~REGION,scales = "free_y")+
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001))+ 
  geom_vline(data=jwd_siteS, xintercept = 6.096,  size=0.75,color="grey")+
  geom_vline(data=jwd_siteS,xintercept = 18.288,  size=0.75,color="grey")+
   labs(x="Max Depth (m)",y="Juvenile Density")
p1


#Plot juv v depth at strata level
  
jwd_siteS$REGION<-str_replace(jwd_siteS$REGION,"Line", "LINE")
jwd_siteS$REGION<-str_replace(jwd_siteS$REGION,"Phoenix", "PHOENIX")
depth_strat$REGION<-str_replace(depth_strat$REGION,"Line", "LINE")
depth_strat$REGION<-str_replace(depth_strat$REGION,"Phoenix", "PHOENIX")

stratD<-left_join(d_strat,depth_strat)
head(stratD)



p2<-
  ggplot(stratD,aes(x=MeanMaxDepth, y=DeltaDen)) + 
  geom_smooth(se=T,method="lm",lwd=1.5)+
  geom_point()+
  #geom_errorbar(data=jcd_sum,aes(y=jcdMEAN, x=ANALYSIS_YEAR,ymin=jcdMEAN-jcdSE, ymax=jcdMEAN+jcdSE), width=.2)+
  facet_wrap(~fct_relevel(REGION,"NWHI","MHI","PHOENIX","LINE","SAMOA","SMARIAN","NMARIAN"),scales = "free_y")+
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank())+
  labs(x="Mean Max Depth (m)",y="Juvenile Density")
p2


