# WAVE ACTION CALCULATOR #
## this code assigns wave action per juvenile coral site using two wave data files
# 
# 
# 
# [1] spatial_7.3-13  ggrepel_0.8.2   ggspatial_1.1.4 ggsn_0.5.0    
# [5] ggplot2_3.3.2   ncf_1.2-9       raster_3.4-5    sf_0.9-8      
#     sp_1.4-4      
# 
# devtools::install_version("spatial", version = "7.3-13", repos = "http://cran.us.r-project.org")
#     

library(dplyr)
library(sp)
library(sf)
library(raster)
library(ncf) # for gcdist()
library(ggsn)
library(ggspatial)
library(ggrepel)




setwd("M:/Environmental_Data_Summary/Data_Download/WaveEnergySwath")
list.files()

### read in wave data
cont <- read.csv("15m_contours.csv") # wave data generated from the 15m depth contour to help fill in the gaps from where the fish sites weren't surveyed
fish <- read.csv("FISH_waves_1979_2010.csv") # wave data from historical fish sites 

#Data cleanup
head(cont)
colnames(cont)
cont <- cont[ which(cont$BAD_FLAG == 0),] # remove the bad flags
cont$X2011 <- NULL # remove 2011 and 2012 data so both datasets have same year range
cont$X2012 <- NULL
cont$BAD_FLAG <- NULL

head(fish)
colnames(fish)
fish<-fish%>% dplyr::filter(Site!="GUA-01310") #remove this site because it doesn't have a lat and long
fish<-fish%>% dplyr::filter(fish$BAD_FLAG == 0) #remove this site because it doesn't have a lat and long

fish$Wave.Power..kwhr.m. <- NULL
fish$ISL <- substr(fish$Site, 1, 3)
fish$Site <- NULL
fish$BAD_FLAG <- NULL
fish <- fish[,c(1,2,35,3:34)] # reorder

all <- rbind(fish, cont) # combined data sets
nrow(fish)
nrow(cont)


# calculate mean and median per coordinate across all years- Tom has concerns about using mean as a summary statistic
all_2<-all %>% 
  rowwise() %>%
  dplyr::mutate(means = mean(c_across(X1979:X2010), na.rm=T),medians = median(c_across(X1979:X2010), na.rm=T))

head(as.data.frame(all_2))

# save full wave action dataframe as a new data set
write.csv(all_2, "WaveActionPacific_1997_2010.csv")


# convert to spatial points data frame
xy <- all_2[,c(1,2)]
all_sp <- SpatialPointsDataFrame(coords = xy, data = all_2,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

### read in juvenile data
juv<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvProject_SITE_weights_AllYears_wHD.csv")

juvS<-subset(juv,GENUS_CODE=="SSSS") #subset just total hard coral for each site
juvS <- subset(juvS,select=c(ISLAND,SITE,LATITUDE,LONGITUDE)) # remove extra columns -- only need site name + coords
colnames(juvS)


#Read in islands shapefile
islands<-st_read("U:/GIS/Data/Pacific/islands.shp")

#Plotting the wave and juvenile data for a subset of islands to check overlap
#Helpful website for plotting maps with ggplot https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
Plot_WaveJuv<-function(d1,d2,d3,waveISL="OAH",juvISL="Oahu",xlim1,xlim2,ylim1,ylim2){
  ggplot(data = d1) +
    geom_sf() +
    geom_point(data = subset(d2,ISL==waveISL), aes(x = x, y = y), size = 2, shape = 21, fill = "slateblue3") +
    geom_point(data = subset(d3,ISLAND==juvISL),aes(x = LONGITUDE, y = LATITUDE), size = 2, shape = 8, color = "darkorange1") +
    coord_sf(xlim = c(xlim1, xlim2), ylim = c(ylim1, ylim2), expand = FALSE)+
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
                                          size = 0.5), panel.background = element_rect(fill = "aliceblue"))+
    annotation_scale(location = "bl", width_hint = 0.4)
}

extent(subset(all_2,ISL=="OAH")) # identify extent of coordinates
Plot_WaveJuv(islands,all_2,juv,"OAH","Oahu",-158.5, -157.5,21.2, 21.8)

extent(subset(all_2,ISL=="KAH")) # identify extent of coordinates
Plot_WaveJuv(islands,all_2,juv,"KAH","Kahoolawe",-156.72, -156.5,20.5, 20.61)

extent(subset(all_2,ISL=="MAU"))
Plot_WaveJuv(islands,all_2,juv,"MAU","Maug",145.2, 145.25,20, 20.05)

extent(subset(all_2,ISL=="PAL"))
Plot_WaveJuv(islands,all_2,juv,"PAL","Palmyra",-162.2, -161.99,5.85,5.925)

extent(subset(all_2,ISL=="KIN"))
Plot_WaveJuv(islands,all_2,juv,"KIN","Kingman",-162.49, -162.3,6.35,6.47)


# convert juv data to spatial points data
xy_juv <- juvS[,c(4,3)]
juv_sp <- SpatialPointsDataFrame(coords = xy_juv, data = juvS,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
str(juv_sp)


### calculate the mean wave action values within 250m radius of each juv point
# source expanding extract function
source("M:/Environmental_Data_Summary/HelperCode/ExpandingExtract_Flex.R")
sites_waves <- ExpandingExtract_Flex(Data = all_sp, SurveyPts = juv_sp,
                                     Dists = seq(0, 4000, by = 50),Data_Col = "medians",REPORT = T) # you may not want to keep sites that have wave data from 4km away, but you can drop these sites later in the script



#Plot the % of sites that fall within each distance 97.5% of sites are within 1km of wave data
head(sites_waves)
t=table(sites_waves$Dist) #visualize 
st=cumsum(t)
plot(st/sum(t))
st
st/sum(t)
plot(sort(sites_waves$Dist)) # where there are natural breaks in distances
abline(h = 1000)
abline(h=750) # this seems like a reasonable break
abline(h=500)

#Define a distance that is too far to estimate
TooFar=1000

#Remove sites with wave data >1000m away
sites_waves$values[which(sites_waves$Dist>TooFar)]=NA

juv_2 <- cbind(juvS, sites_waves)
nrow(juv_2)
sum(!complete.cases(juv_2)) #35 sites will be dropped


#Spot check specific sites with high distances to make sure you are comfortable using the value chosen
Plot_DistCheck<-function(d1,d2,isl="Oahu",xlim1,xlim2,ylim1,ylim2){
  ggplot(data = d1) +
    geom_sf() +
    geom_point(data = subset(d2,ISLAND==isl),aes(x = LONGITUDE, y = LATITUDE, color = Dist)) +
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
                                          size = 0.5), panel.background = element_rect(fill = "aliceblue"))+
    coord_sf(xlim = c(xlim1, xlim2), ylim = c(ylim1, ylim2), expand = FALSE)+
    scale_color_viridis_c()+
    geom_text(data = juv_2, aes(x = LONGITUDE, y = LATITUDE,label=SITE), size = 3, hjust=0, vjust=-1)
    
}

#Palmyra
Plot_DistCheck(islands,juv_2,"Palmyra",-162.17, -161.99,5.86,5.905)

#Make changes to specific sites that have values that don't make sense 
juv_2$values<-ifelse(juv_2$SITE %in% c("PAL-00775","PAL-01196","PAL-01187","PAL-00763","PAL-00753","PAL-01176"),
                     0,juv_2$values) #Change these to 0, they are all sheltered sites and too far from available wave data.

juv_2$values<-ifelse(juv_2$SITE %in% c("FFS-01266"), 114543.5519,juv_2$values) #No data for this site so substituting with value from next closest site


#Kingman
Plot_DistCheck(islands,juv_2,"Kingman",-162.49, -162.3,6.35,6.47)
#Sites look good-no changes needed

#Lisianski
summary(subset(juv_2,ISLAND=="Lisianski"))

Plot_DistCheck(islands,juv_2,"Lisianski",-174.1,-173.8,25.91,26.15)
Plot_WaveJuv(islands,all_2,juv,"LIS","Lisianski",-174.1,-173.8,25.91,26.15)
juv_2$values<-ifelse(juv_2$SITE %in% c("LIS-04067"), 0,juv_2$values) #Change these to 0
juv_2$values<-ifelse(juv_2$SITE %in% c("LIS-00838"),196027.15,juv_2$values) #Change these be similar to nearby site


#Oahu
Plot_DistCheck(islands,juv_2,"Oahu",-158.3, -157.6,21.2, 21.8)
Plot_WaveJuv(islands,all_2,juv,"OAH","Oahu",-158.3, -157.6,21.2, 21.8)
#Sites look good-no changes needed

#Maug
Plot_WaveJuv(islands,all_2,juv,"MAU","Maug",145.2, 145.25,20, 20.05)
Plot_DistCheck(islands,juv_2,"Maug",145.2, 145.25,20, 20.05)
#Sites look good-no changes needed

#Maui
summary(subset(juv_2,ISLAND=="Maui"))
Plot_WaveJuv(islands,all_2,juv,"MAI","Maui",-156.8,-155.9,20.55, 21.1)
Plot_DistCheck(islands,juv_2,"Maui",-156.8,-155.9,20.55, 21.1)
#Sites look good-no changes needed

#Howland
summary(subset(juv_2,ISLAND=="Howland"))
Plot_WaveJuv(islands,all_2,juv,"HOW","Howland",-176.7,-176.3,0.5, 1.0)
Plot_DistCheck(islands,juv_2,"Howland",-176.7,-176.3,0.5, 1.0)

#Pearl and Hermes
summary(subset(juv_2,ISLAND=="Pearl & Hermes"))
Plot_WaveJuv(islands,all_2,juv,"PHR","Pearl & Hermes",-176.1,-175.3,27.5, 27.9)
Plot_DistCheck(islands,juv_2,"Pearl & Hermes",-176.1,-175.3,27.5, 27.9)


#Cleanup dataframe & Export to the Predictor script "Juvenile Project Predictor Variables_SITE_v2.R"
head(juv_2)
wave<-juv_2%>% dplyr::select(ISLAND:values)
wave<-wave %>% dplyr::rename(WavePower=values)

#save the data
write.csv(wave,file="T:/Benthic/Projects/Juvenile Project/Pacific_WaveActionData_v5.csv",row.names = F)
