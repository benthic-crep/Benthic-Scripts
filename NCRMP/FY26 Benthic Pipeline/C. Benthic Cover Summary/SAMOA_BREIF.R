library(tidyverse)
library(sf)
library(ggspatial)
library(scales)
library(patchwork)

#Load REA and COVER
cov=read.csv("C:/Users/Thomas.Oliver/Desktop/PICBILLFISH_BENTHIC/REA Coral Demography & Cover/Summary Data/Sector/BenthicCover_2010-2024_Tier1_SECTOR_Trends.csv")
rea=read.csv("C:/Users/Thomas.Oliver/Desktop/PICBILLFISH_BENTHIC/REA Coral Demography & Cover/Summary Data/Sector/BenthicREA_SECTOR_GENUS_CODE_TRENDS_2013_2024.csv")

#Restrict to AS
covAS=cov %>% filter(REGION=="SAMOA")
reaAS=rea %>% filter(REGION=="SAMOA",GENUS_CODE=="SSSS")

covDel=covAS %>% filter(REGION=="SAMOA",ANALYSIS_YEAR%in%c(2018,2023)) %>% pivot_wider(id_cols = c(REGION,ISLAND,ANALYSIS_SEC),names_from = ANALYSIS_YEAR,values_from = c(Mean.CORAL,SE.CORAL)) %>% 
  mutate(Mean.CORAL_d18.23=100*(Mean.CORAL_2023-Mean.CORAL_2018)/Mean.CORAL_2018)

reaDel=reaAS %>% filter(REGION=="SAMOA",ANALYSIS_YEAR%in%c(2018,2023),GENUS_CODE=="SSSS") %>% pivot_wider(id_cols = c(REGION,PooledSector_Viztool,GENUS_CODE),names_from = ANALYSIS_YEAR,values_from = c( Mean_AdColDen, SE_AdColDen)) %>% 
  mutate(Mean.AdColDen_d18.23=100*(Mean_AdColDen_2023-Mean_AdColDen_2018)/Mean_AdColDen_2018)

#list.files("C:/Users/Thomas.Oliver/",pattern=".shp",recursive = T,full.names = T)
sec=st_read("C:/Users/Thomas.Oliver/WORK/Projects/GITHUB Projects/PIFSC_VitalRates/Data/Shapefiles/ALLPacific_Sectors_Islands_5km_buffer.shp")
secAS=sec %>% filter(ISLAND_CD%in%c("TUT","OFU","SWA","ROS","TAU")) %>% st_make_valid() %>% dplyr::select(c(-SEC_FISHIN,-SEC_HABITA,-Shape_Leng,-Shape_Area))

secISL <- secAS %>%
  group_by(ISLAND_CD) %>%
  summarise( .groups = 'drop') %>% 
  mutate(SEC_NAME=paste0(ISLAND_CD,"_ALL"),Region="SAMOA") %>% 
  dplyr::select(ISLAND_CD,SEC_NAME,Region,geometry)
secAUNUU <- secAS %>% filter(SEC_NAME%in%c("TUT_AUNUU_A","TUT_AUNUU_B")) %>% 
  group_by(ISLAND_CD) %>%
  summarise( .groups = 'drop') %>% 
  mutate(SEC_NAME=paste0(ISLAND_CD,"_AUNUU"),Region="SAMOA") %>% 
  dplyr::select(ISLAND_CD,SEC_NAME,Region,geometry)

secAScov=rbind(secAS,secISL,secAUNUU) %>%
  filter(SEC_NAME%in%unique(covAS$ANALYSIS_SEC)) %>% 
  left_join(covDel,by = c("SEC_NAME"="ANALYSIS_SEC"))

secASrea=rbind(secAS,secISL,secAUNUU) %>%
  filter(SEC_NAME%in%unique(covAS$ANALYSIS_SEC)) %>% 
  left_join(reaDel,by = c("SEC_NAME"="PooledSector_Viztool"))

##############################################
#Swains Maps/Plots - Coral Density


#Tutuila Maps/Plots

TUT_CC=secAScov %>% 
  filter(ISLAND_CD=="TUT") %>% 
  ggplot()+
  annotation_map_tile(type = "osm", zoom = 11) + # Adds basemap
  geom_sf(aes(fill=Mean.CORAL_2023), color = "white",alpha=.9) + # Plots polygon
  geom_sf_label(aes(label=paste(round(Mean.CORAL_2023,1),"% +/-",round(SE.CORAL_2023,2))),size=5,color="black",fun.geometry=st_centroid)+
  scale_color_continuous(guide=NULL)+
  scale_size_area(guide=NULL)+
  theme_minimal()+
  scale_fill_viridis_c(name = "Live Coral Cover 2023")+
  theme(legend.position = "bottom")+
  ggtitle("Tutuila: Live Coral Cover, 2023")+xlab("")+ylab("")
TUT_CC

TUT_delCC=secAScov %>% filter(ISLAND_CD=="TUT") %>% 
  ggplot()+
  annotation_map_tile(type = "osm", zoom = 11) + # Adds basemap
  geom_sf(aes(fill=oob_squish(Mean.CORAL_d18.23,range=c(-25,150))), color = "white",alpha=.9) + # Plots polygonLive 
  geom_sf_label(aes(label=paste(c(rep("+",4),""),round(Mean.CORAL_d18.23,1),"%")),size=5,color="black",fun.geometry=st_centroid)+
  scale_color_continuous(guide=NULL)+
  scale_size_area(guide=NULL)+
  theme_minimal()+scale_fill_gradient2(name = "Percent Chage in Live \nCoral Cover 2018-2023",midpoint = 0,high="darkblue",low="darkred")+
  theme(legend.position = "bottom")+ggtitle("Tutuila: Live Coral Cover Change by Sector- 2018 to 2023")+xlab("")+ylab("")
TUT_delCC

covAS$ANALYSIS_SEC=factor(covAS$ANALYSIS_SEC,levels=c("TUT_NW_OPEN","TUT_SW_OPEN","TUT_NE_OPEN","TUT_SE_OPEN","TUT_AUNUU","Ofu & Olosega","TAU_ALL","ROS_ALL","SWA_ALL"))
ALL_Timeseries=covAS %>% #filter(ISLANDCODE!="TUT") %>%
  ggplot(aes(x=ANALYSIS_YEAR,y=Mean.CORAL,ymin=Mean.CORAL-SE.CORAL,ymax=Mean.CORAL+SE.CORAL,group=ANALYSIS_SEC,color=ANALYSIS_SEC))+
  geom_point()+
  geom_errorbar(width=.25)+
  geom_line()+
  facet_wrap("ANALYSIS_SEC",scales="free_x")+
  theme_bw()+
  theme(legend.position = "bottom")+ggtitle("American Samoa: Live Coral Cover Change by Sector- 2010 to 2023")+
  scale_color_discrete(name="Sector")+ylab("Live Coral Cover +/- SE")+xlab("Sampling Year")

COV_ALL_PLOT=(TUT_CC+TUT_delCC)/ALL_Timeseries
COV_ALL_PLOT

##############################################
#Tutuila Maps/Plots - Coral Density
TUT_ACD=secASrea %>% 
  filter(ISLAND_CD=="TUT") %>% 
  ggplot()+
  annotation_map_tile(type = "osm", zoom = 11) + # Adds basemap
  geom_sf(aes(fill=Mean_AdColDen_2023), color = "white",alpha=.9) + # Plots polygon
  geom_sf_label(aes(label=paste(round(Mean_AdColDen_2023,1),"+/-",round(SE_AdColDen_2023,2), "per m2")),size=5,color="black",fun.geometry=st_centroid)+
  scale_color_continuous(guide=NULL)+
  scale_size_area(guide=NULL)+
  theme_minimal()+
  scale_fill_viridis_c(name = "Adult Colony Density 2023")+
  theme(legend.position = "bottom")+
  ggtitle("Tutuila: Adult Coral Colony Density (Col/m2), 2023")+xlab("")+ylab("")
TUT_ACD

pluses=c(rep("+",2),"",rep("+",1),"")
pct=c(rep("%",2),"",rep("%",1),"")
TUT_delACD=secASrea %>% filter(ISLAND_CD=="TUT") %>% 
  ggplot()+
  annotation_map_tile(type = "osm", zoom = 11) + # Adds basemap
  geom_sf(aes(fill=oob_squish(Mean.AdColDen_d18.23,range=c(-25,150))), color = "white",alpha=.9) + # Plots polygonLive 
  geom_sf_label(aes(label=paste(pluses,round(Mean.AdColDen_d18.23,1),pct)),size=5,color="black",fun.geometry=st_centroid)+
  scale_color_continuous(guide=NULL)+
  scale_size_area(guide=NULL)+
  theme_minimal()+
  scale_fill_gradient2(name = "Percent Chage in Adult\n Colony Density 2018-2023 (col/m2)",midpoint = 0,high="darkblue",low="darkred")+
  theme(legend.position = "bottom")+ggtitle("Tutuila: Percent Chage in Adult Colony Density by Sector- 2018 to 2023")+xlab("")+ylab("")
TUT_delACD

reaAS$PooledSector_Viztool=factor(reaAS$PooledSector_Viztool,levels=c("TUT_NW_OPEN","TUT_SW_OPEN","TUT_FAGALUA_FAGATELE","TUT_NE_OPEN","TUT_SE_OPEN","TUT_AUNUU","Ofu & Olosega","TAU_ALL","ROS_ALL","SWA_ALL"))
ALL_ReaTimeseries=reaAS %>% #filter(ISLANDCODE!="TUT") %>%
  ggplot(aes(x=ANALYSIS_YEAR,y=Mean_AdColDen,ymin=Mean_AdColDen-SE_AdColDen  ,ymax=Mean_AdColDen+SE_AdColDen,group=PooledSector_Viztool ,color=PooledSector_Viztool ))+
  geom_point()+
  geom_errorbar(width=.25)+
  geom_line()+
  facet_wrap("PooledSector_Viztool",scales="free_x")+
  theme_bw()+
  theme(legend.position = "bottom")+ggtitle("American Samoa: Adult Colony Density Change by Sector- 2015 to 2023")+
  scale_color_discrete(name="Sector")+ylab("Live Coral Cover +/- SE")+xlab("Sampling Year")

REA_ALL_PLOT=(TUT_ACD+TUT_delACD)/ALL_ReaTimeseries

sc=.75
ggsave(filename = "C:/Users/Thomas.Oliver/WORK/Projects/SamoaBrief2025/CoverPlot.jpg",plot = COV_ALL_PLOT)
ggsave(filename = "C:/Users/Thomas.Oliver/WORK/Projects/SamoaBrief2025/DenPlot.jpg",plot = REA_ALL_PLOT)
