# WAVE ACTION CALCULATOR #
## this code assigns wave action per covenile coral site using two wave data files
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

### read in cover data
df<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2023_Tier1_SITE.csv")

cov <- subset(df,select=c(ISLAND,SITE,LATITUDE,LONGITUDE)) # remove extra columns -- only need site name + coords
colnames(cov)


#Read in islands shapefile
islands<-st_read("U:/GIS/Data/Pacific/islands.shp")

#Plotting the wave and covenile data for a subset of islands to check overlap
#Helpful website for plotting maps with ggplot https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
Plot_Wavecov<-function(d1,d2,d3,waveISL="OAH",covISL="Oahu",xlim1,xlim2,ylim1,ylim2){
  ggplot(data = d1) +
    geom_sf() +
    geom_point(data = subset(d2,ISL==waveISL), aes(x = x, y = y), size = 2, shape = 21, fill = "slateblue3") +
    geom_point(data = subset(d3,ISLAND==covISL),aes(x = LONGITUDE, y = LATITUDE), size = 2, shape = 8, color = "darkorange1") +
    coord_sf(xlim = c(xlim1, xlim2), ylim = c(ylim1, ylim2), expand = FALSE)+
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
                                          size = 0.5), panel.background = element_rect(fill = "aliceblue"))+
    annotation_scale(location = "bl", width_hint = 0.4)
}


extent(subset(all_2,ISL=="SWA"))
Plot_Wavecov(islands,all_2,cov,"SWA","Swains",-171.1, -171.06,-11.04,-11.075)


# convert cov data to spatial points data
xy_cov <- cov[,c(4,3)]
cov_sp <- SpatialPointsDataFrame(coords = xy_cov, data = cov,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
str(cov_sp)


### calculate the mean wave action values within 250m radius of each cov point
# source expanding extract function
source("M:/Environmental_Data_Summary/HelperCode/ExpandingExtract_Flex.R")
sites_waves <- ExpandingExtract_Flex(Data = all_sp, SurveyPts = cov_sp,
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
TooFar=4000

#Remove sites with wave data >1000m away
sites_waves$values[which(sites_waves$Dist>TooFar)]=NA

cov_2 <- cbind(cov, sites_waves)
nrow(cov_2)
sum(!complete.cases(cov_2)) #35 sites will be dropped

cov_2<-subset(cov_2,values != "NaN")


#Spot check specific sites with high distances to make sure you are comfortable using the value chosen
Plot_DistCheck<-function(d1,d2,isl="Oahu",xlim1,xlim2,ylim1,ylim2){
  ggplot(data = d1) +
    geom_sf() +
    geom_point(data = subset(d2,ISLAND==isl),aes(x = LONGITUDE, y = LATITUDE, color = Dist)) +
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
                                          size = 0.5), panel.background = element_rect(fill = "aliceblue"))+
    coord_sf(xlim = c(xlim1, xlim2), ylim = c(ylim1, ylim2), expand = FALSE)+
    scale_color_viridis_c()
    #geom_text(data = cov_2, aes(x = LONGITUDE, y = LATITUDE,label=SITE), size = 3, hjust=0, vjust=-1)
  
}


Plot_WaveAction<-function(d1,d2,isl="Oahu",xlim1,xlim2,ylim1,ylim2){
  ggplot(data = d1) +
    geom_sf() +
    geom_point(data = subset(d2,ISLAND==isl),aes(x = LONGITUDE, y = LATITUDE, color = values)) +
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
                                          size = 0.5), panel.background = element_rect(fill = "white"))+
    coord_sf(xlim = c(xlim1, xlim2), ylim = c(ylim1, ylim2), expand = FALSE)+
    scale_color_viridis_c(name="Wave Power")
  #geom_text(data = cov_2, aes(x = LONGITUDE, y = LATITUDE,label=SITE), size = 3, hjust=0, vjust=-1)
  
}


Plot_SiteDepth<-function(d1,d2,isl="Oahu",xlim1,xlim2,ylim1,ylim2){
  ggplot(data = d1) +
    geom_sf() +
    geom_point(data = subset(d2,ISLAND==isl),aes(x = LONGITUDE, y = LATITUDE, color = MedianDepth),size =1.5) +
    facet_wrap(~ OBS_YEAR)+
    theme(panel.grid.major = element_line(), panel.background = element_rect(fill = "white"))+
    coord_sf(xlim = c(xlim1, xlim2), ylim = c(ylim1, ylim2), expand = FALSE)+
    scale_color_viridis_c(direction = -1, name="Depth")
  #geom_text(data = cov_2, aes(x = LONGITUDE, y = LATITUDE,label=SITE), size = 3, hjust=0, vjust=-1)
  
}


#Swains
Plot_DistCheck(islands,cov_2,"Swains",-171.1, -171.06,-11.04,-11.075)
Plot_Wavecov(islands,all_2,cov,"SWA","Swains",-171.1, -171.06,-11.04,-11.075)
Plot_WaveAction(islands,cov_2,"Swains",-171.1, -171.06,-11.04,-11.075)


df<-df %>% 
  rowwise() %>% 
  mutate(MedianDepth = median(c(new_MIN_DEPTH_M,new_MAX_DEPTH_M), na.rm = TRUE))
shal<-subset(df,new_MIN_DEPTH_M >= 3.5 & new_MIN_DEPTH_M < 6.05)
Plot_SiteDepth(islands,shal,"Swains",-171.1, -171.06,-11.04,-11.075)



#plot: 
#points- color gradient of depth - 1 map for each year
#points - gradient of coral cover









ggplot() +
  geom_sf(data=islands) +
  geom_point(cov_2,aes(x = LONGITUDE, y = LATITUDE, color = values)) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))+
  coord_sf(xlim = c(-171.1, -171.06), ylim = c(-11.04, -11.075), expand = FALSE)+
  scale_color_viridis_c()



#Oahu
Plot_DistCheck(islands,cov_2,"Oahu",-158.3, -157.6,21.2, 21.8)
Plot_Wavecov(islands,all_2,cov,"OAH","Oahu",-158.3, -157.6,21.2, 21.8)
#Sites look good-no changes needed

#Maug
Plot_Wavecov(islands,all_2,cov,"MAU","Maug",145.2, 145.25,20, 20.05)
Plot_DistCheck(islands,cov_2,"Maug",145.2, 145.25,20, 20.05)
#Sites look good-no changes needed

#Maui
summary(subset(cov_2,ISLAND=="Maui"))
Plot_Wavecov(islands,all_2,cov,"MAI","Maui",-156.8,-155.9,20.55, 21.1)
Plot_DistCheck(islands,cov_2,"Maui",-156.8,-155.9,20.55, 21.1)
#Sites look good-no changes needed

#Howland
summary(subset(cov_2,ISLAND=="Howland"))
Plot_Wavecov(islands,all_2,cov,"HOW","Howland",-176.7,-176.3,0.5, 1.0)
Plot_DistCheck(islands,cov_2,"Howland",-176.7,-176.3,0.5, 1.0)

#Pearl and Hermes
summary(subset(cov_2,ISLAND=="Pearl & Hermes"))
Plot_Wavecov(islands,all_2,cov,"PHR","Pearl & Hermes",-176.1,-175.3,27.5, 27.9)
Plot_DistCheck(islands,cov_2,"Pearl & Hermes",-176.1,-175.3,27.5, 27.9)


#Cleanup dataframe & Export to the Predictor script "covenile Project Predictor Variables_SITE_v2.R"
head(cov_2)
wave<-cov_2%>% dplyr::select(ISLAND:values)
wave<-wave %>% dplyr::rename(WavePower=values)

#save the data
write.csv(wave,file="T:/Benthic/Projects/covenile Project/Pacific_WaveActionData_v5.csv",row.names = F)
