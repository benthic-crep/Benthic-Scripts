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


df<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS.csv")

dfS<-subset(df,GENUS_CODE=="SSSS"&ISLAND=="Pagan")

# # Oahu:
# pl<-subset(df,(X <-157.705 & X > -157.713) & (Y < 21.263 & Y>21.258))
# #oah<-sample.df(pl[(pl$ISLAND == "Oahu"), ], 25)
# oah$SITE<-1:length(oah$OBJECTID)
# oah$SITE<-paste("PL",oah$SITE,sep="")


#Read in islands shapefile
islands<-st_read("U:/GIS/Data/Pacific/islands.shp")

dfS$X<-dfS$LONGITUDE
dfS$Y<-dfS$LATITUDE
dfS$OBS_YEAR<-as.factor(dfS$OBS_YEAR)

#Helpful website for plotting maps with ggplot https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
Plot_Sites<-function(d1,d2,island="Oahu",xlim1,xlim2,ylim1,ylim2){
  ggplot(data = d1) +
    geom_sf() +
    geom_point(data = subset(d2,ISLAND==island),aes(x = X, y = Y,color=OBS_YEAR), size = 4, shape = 20) +
    coord_sf(xlim = c(xlim1, xlim2), ylim = c(ylim1, ylim2), expand = FALSE)+
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
                                          size = 0.5), panel.background = element_rect(fill = "aliceblue"))
    #annotation_scale(location = "bl", width_hint = 0.4)
    # geom_text(data = d2, aes(x = X, y = Y,label=SITE), size = 3, hjust=0, vjust=-1)
    #geom_text_repel(data = subset(d2,ISLAND==randisland),aes(x = X, y = Y, label = SITE),min.segment.length = 0,size=3)
  
}

Plot_BenData<-function(d1,d2,island="Oahu", metric_field="AdColDen",xlim1,xlim2,ylim1,ylim2){
  d2$METRIC<-d2[,metric_field]
  ggplot(data = d1) +
    geom_sf() +
    geom_point(data = subset(d2,ISLAND==island),aes(x = X, y = Y,color=METRIC), size = 4, shape = 20) +
    coord_sf(xlim = c(xlim1, xlim2), ylim = c(ylim1, ylim2), expand = FALSE)+
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
                                          size = 0.5), panel.background = element_rect(fill = "aliceblue"))+
  scale_color_viridis()
}


p1<-Plot_Sites(islands,dfS,"Pagan",145.7, 145.82,18, 18.17)+ggtitle("Historical Benthic Sites");p1
p2<-Plot_BenData(islands,dfS,"Pagan","AdColDen",145.7, 145.82,18, 18.17)+ggtitle("Historical Adult Colony Density");p2


write.csv(oah,file="M:/SFM Site Maps/Portlock site selection/RandomSites_Portlock.csv")

ggsave(plot=p1,file="M:/SFM Site Maps/Portlock site selection/RandomSites_Portlock.pdf",width=10,height=7)

