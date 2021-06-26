

cover<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicCover_2010-2019_Tier1_SECTOR_forTimesSeries.csv")

mhi<-cover%>% dplyr::filter(REGION=="MHI")

write.csv(mhi,file="T:/Benthic/Data/Data Requests/NOAA_StRS2010-2019Cover_bySector.csv",row.names = F)

plot1<-ggplot(mhi, aes(x=ANALYSIS_YEAR, y=Mean.CORAL, fill=ANALYSIS_SEC)) + 
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(aes(ymin=Mean.CORAL-PooledSE.CORAL, ymax=Mean.CORAL+PooledSE.CORAL),width=.15, position=position_dodge(.9)) + 
  guides(fill=FALSE) + facet_wrap(.~ ANALYSIS_SEC , scales="fixed", labeller=label_parsed,nrow=2) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90,size=12)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none")

plot1


plot2<-ggplot(mhi, aes(x=ANALYSIS_YEAR, y=Mean.CORAL, color=ANALYSIS_SEC,group=ANALYSIS_SEC)) + 
    geom_line() +
    geom_point()+
  geom_errorbar(aes(ymin=Mean.CORAL-PooledSE.CORAL, ymax=Mean.CORAL+PooledSE.CORAL),width=.15, position=position_dodge(.9)) + 
  guides(fill=FALSE) + facet_wrap(.~ ANALYSIS_SEC , scales="fixed", labeller=label_parsed,nrow=2) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90,size=12)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none")

plot2



# convert juv data to spatial points data
cover_site<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2020_Tier1_SITE.csv")


mhiS<-cover_site %>% dplyr::filter(REGION=="MHI")

mhiS[mhiS$REGION %in% c("MHI", "NWHI") & mhiS$OBS_YEAR %in% seq(2010,2012),]$ANALYSIS_YEAR<-"2010-12" #We had poor sampling in both years for all islands so we are pooling them together
mhiS[mhiS$REGION %in% c("MHI", "NWHI") & mhiS$OBS_YEAR %in% seq(2013,2015),]$ANALYSIS_YEAR<-"2013-15"#We had poor sampling in both years for some islands so we are pooling them together- we could probably separate years for some of the islands/sectors- I don't love this

# xy <- mhiS[,c(19,18)]
# mhi_sp <- SpatialPointsDataFrame(coords = xy, data = mhiS,
#                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
# str(mhi_sp)


islands<-st_read("U:/GIS/Data/Pacific/islands.shp")

Plot_Map<-function(d1,d2,sc="MOL_SOUTH",xlim1,xlim2,ylim1,ylim2,title){
  ggplot(data = d1) +
    geom_sf() +
    geom_point(data = subset(d2,SEC_NAME==sc),aes(x = LONGITUDE, y = LATITUDE, color = ANALYSIS_YEAR),size=3) +
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
                                          size = 0.5), panel.background = element_rect(fill = "aliceblue"))+
    coord_sf(xlim = c(xlim1, xlim2), ylim = c(ylim1, ylim2), expand = FALSE)+
    ggtitle(title)

}
#Molokai
summary(subset(mhiS,SEC_NAME=="MOL_SOUTH"))

Plot_Map(islands,mhiS,"MOL_SOUTH",-157.2,-156.7,21.03,21.14)

#Molokai
summary(subset(mhiS,SEC_NAME=="HAW_PUNA"))

puna<-Plot_Map(islands,mhiS,"HAW_PUNA",-155.2,-154.7,19.35,19.89,"Hawaii Island-Puna")

ggsave(plot=puna,
       file="T:/Benthic/Data/Data Requests/PunaCoverMap.jpeg")
