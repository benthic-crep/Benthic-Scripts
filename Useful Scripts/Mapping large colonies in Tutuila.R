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


df<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED.csv")

dfTut<-subset(df,ISLAND=="Tutuila")


dfS<-ddply(dfTut,.(ISLAND,OBS_YEAR,SITE,LATITUDE,LONGITUDE,DEPTH_BIN,SEC_NAME,TAXONCODE),
      summarize,
      size = max(COLONYLENGTH,na.rm = TRUE))
head(dfS)

lgdf<-subset(dfS,size>= 800)


#Read in islands shapefile
islands<-st_read("U:/GIS/Data/Pacific/islands.shp")

lgdf$X<-lgdf$LONGITUDE
lgdf$Y<-lgdf$LATITUDE
lgdf$OBS_YEAR<-as.factor(lgdf$OBS_YEAR)


ggplot(data = islands) +
  geom_sf() +
  geom_point(data = subset(lgdf,ISLAND=="Tutuila"),aes(x = X, y = Y,color=OBS_YEAR), size = 4, shape = 20) +
  coord_sf(xlim = c(-170.85, -170.5), ylim = c(-14.4, -14.2), expand = FALSE)+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))
#annotation_scale(location = "bl", width_hint = 0.4)
# geom_text(data = d2, aes(x = X, y = Y,label=SITE), size = 3, hjust=0, vjust=-1)
#geom_text_repel(data = subset(d2,ISLAND==randisland),aes(x = X, y = Y, label = SITE),min.segment.length = 0,size=3)

ggplot(data = islands) +
  geom_sf() +
  geom_point(data = subset(lgdf,ISLAND=="Tutuila"),aes(x = X, y = Y,color=size), size = 4, shape = 20) +
  coord_sf(xlim = c(-170.85, -170.5), ylim = c(-14.4, -14.2), expand = FALSE)+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))+
  scale_color_viridis()+
  annotation_scale(location = "bl", width_hint = 0.4)+
  geom_text(data = lgdf, aes(x = X, y = Y,label=SITE), size = 3, hjust=0, vjust=-1)+
  geom_text_repel(data = subset(lgdf,ISLAND=="Tutuila"),aes(x = X, y = Y, label = SITE),min.segment.length = 0,size=3)
  

Fagaitua<-c("TUT-0745","TUT-05738","TUT-05743","TUT-01858")

fsites<-subset(lgdf,SITE %in% Fagaitua)

View(fsites)

