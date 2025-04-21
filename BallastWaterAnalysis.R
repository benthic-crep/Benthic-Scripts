#This is trying to get a handle on ballast water introductions from Caribbean sources in light of SCTLD
#Data from National Ballast Water Management Report database:https://nbic.si.edu/

library(tidyverse)
library(ggmap) 
library(sf)
library(ggspatial)
library(mapview)
library(basemaps)

#Get Leaflet Pacific Centered
shift = function(x) {
  geom = st_geometry(x)
  st_geometry(x) = st_sfc(
    lapply(seq_along(geom), function(i) {
      geom[[i]][1] = ifelse(geom[[i]][1] < 0, geom[[i]][1] + 360, geom[[i]][1])
      return(geom[[i]])
    })
    , crs = st_crs(geom)
  )
  return(x)
}

path="C:/Users/Thomas.Oliver/Downloads/"
BUILD_ORIGINAL=TRUE
if(BUILD_ORIGINAL){
  tank.list=list.files(path="C:/Users/Thomas.Oliver/Downloads/",pattern="_tanks.csv",full.names = T)
  Tanks=NULL
  for(i in 1:length(tank.list)){
    thistank=read.csv(tank.list[i])
    Tanks=rbind(Tanks,thistank)
  }
  dim(Tanks)
  Tanks$Arrival_Date=ymd(Tanks$Arrival_Date)
  Tanks$ArrivalYear=year(Tanks$Arrival_Date)
  table(Tanks$Arrival_State,Tanks$ArrivalYear)
  
  TotD=Tanks %>%
    filter(Disch_Vol_MT>0,
           !is.na(Source_Lat),
           !is.na(Source_Lon))
  
  TotD$SCTLD_Flag=FALSE
  TotD$SCTLD_Flag[which(TotD$Source_Lat>6&
                          TotD$Source_Lat<30&
                          TotD$Source_Lon>(-100)&
                          TotD$Source_Lon<(-60)&
                          TotD$Disch_Lon<(-100))]=TRUE 
  FlaggedDischarge=TotD %>% filter(SCTLD_Flag)
  write.csv(TotD,"C:/Users/Thomas.Oliver/Downloads/SCTLD_BallastWater_Pacific.csv")
}else{
  read.csv(paste0(path,"SCTLD_BallastWater_Pacific.csv"))
}


SourceD=TotD %>%
  st_as_sf(coords=c("Source_Lon","Source_Lat"),crs=4326)
DischFlag=FlaggedDischarge %>%
  st_as_sf(coords=c("Disch_Lon","Disch_Lat"),crs=4326)
SourceFlag=FlaggedDischarge %>%
  st_as_sf(coords=c("Source_Lon","Source_Lat"),crs=4326)

AllStates=mapview(shift(subset(SourceD,Arrival_State%in%c("AS","GU","HI"))),
        zcol="Arrival_State",
        cex="Disch_Vol_MT",
        layer.name="Source By Area 2020-24")
mapshot(x = AllStates,url = paste0(path,"BallastWaterSources_All2020-2025.htm"))
HIsource=mapview(shift(subset(SourceD,Arrival_State=="HI")),zcol="Arrival_State",cex="Disch_Vol_MT",
        layer.name="Hawaii Source 2020-24")
#mapshot(x = HIsource,url = paste0(path,"BallastWaterSources_HI2020-2025.htm"))
GUsource=mapview(shift(subset(SourceD,Arrival_State=="GU")),zcol="Arrival_State",cex="Disch_Vol_MT",
        layer.name="Guam Source 2020-24")
#mapshot(x = GUsource,url = paste0(path,"BallastWaterSources_GU2020-2025.htm"))
ASsource=mapview(shift(subset(SourceD,Arrival_State=="AS")),zcol="Arrival_State",cex="Disch_Vol_MT",
        layer.name="Am. Samoa Source 2020-24")
#mapshot(x = ASsource,url = paste0(path,"BallastWaterSources_AS2020-2025.htm"))

SCTLD_Source=mapview(SourceFlag,zcol="Arrival_State",
        layer.name="SCTLD Source Location 2020-24")
mapshot(x = SCTLD_Source,url = paste0(path,"SCTLD_BallastWaterSources_2020-2025.htm"))

SCTLD_Disch=mapview(DischFlag,zcol="Arrival_State",cex="Disch_Vol_MT",
        layer.name="SCTLD Source Discharge Location 2020-24")
mapshot(x = SCTLD_Disch,url = paste0(path,"SCTLD_BallastWaterDischargeDestination_2020-2025.htm"))

