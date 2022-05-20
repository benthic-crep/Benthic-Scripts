# WAVE ACTION CALCULATOR #
## this code assigns wave action per juv site using two data files

library(dplyr)
library(sp)
library(sf)
library(raster)
library(ncf) # for gcdist()
library(ggsn)
library("rnaturalearth")
library("rnaturalearthdata")
library(ggspatial)

setwd("M:/Environmental Data Summary/DataDownload/WaveEnergySwath")
list.files()

### read in wave data
cont <- read.csv("15m_contours.csv") # 15m contour data
fish <- read.csv("FISH_waves_1979_2010.csv") # fish site data

# modify data
head(cont)
colnames(cont)
cont <- cont[ which(cont$BAD_FLAG == 0),] # remove the bad flags
cont$X2011 <- NULL # remove 2011 and 2012 data so both datasets have same year range
cont$X2012 <- NULL
cont$BAD_FLAG <- NULL

head(fish)
colnames(fish)
fish<-subset(fish,Site!="GUA-01310") #remove this site because it doesn't have a lat and long
fish <- fish[ which(fish$BAD_FLAG == 0),]
fish$Wave.Power..kwhr.m. <- NULL
fish$ISL <- substr(fish$Site, 1, 3)
fish$Site <- NULL
fish$BAD_FLAG <- NULL
fish <- fish[,c(1,2,35,3:34)] # reorder

all <- rbind(fish, cont) # combined data sets
nrow(fish)
nrow(cont)


# calculate mean per coordinate across all years
all_2 <- all %>%
  rowwise() %>%
  mutate(means=mean(X1979:X2010, na.rm=T))

head(as.data.frame(all_2))

# save full wave action dataframe as a new data set
setwd("C:/Users/Courtney.S.Couch/Desktop")
write.csv(all_2, "WaveActionHawaii_1997_2010.csv")

# run pairwise gcdist() function to view distance matrix between all of the points
wave_dist <- gcdist(all_2$x, all_2$y)
wave_dist_f <- gcdist(cont$x, cont$y)

dim(wave_dist)
dim(all_2)

# look at the histogram of that -- focus on smallest size to see how close the points are to one another
hist(wave_dist)
min(wave_dist[ which(wave_dist>0)])
plot(table(round(wave_dist[ which(wave_dist>0 & wave_dist<1)],3)))
plot(table(round(wave_dist_f[ which(wave_dist_f>0 & wave_dist_f<10)],3)))

# convert to spatial points data frame
xy <- all_2[,c(1,2)]
all_sp <- SpatialPointsDataFrame(coords = xy, data = all_2,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))



### read in juvenile data
setwd("T:/Benthic/Projects/Juvenile Project") # set working directory 
juv <- read.csv("JuvProject_temporal_SITE.csv")
juv <- subset(juv,select=c(ISLAND,SITE,LATITUDE,LONGITUDE)) # remove extra columns -- only need site name + coords
colnames(juv)


# #Read in islands shapefile
# islands<-st_read("U:/GIS/Data/Pacific/islands.shp")
# 
# #Plotting the wave and juvenile data for a subset of islands to check overlap 
# ggplot(data = islands) +
#   geom_sf() +
#   geom_point(data = subset(all_2,ISL=="OAH"), aes(x = x, y = y), size = 2, shape = 21, fill = "slateblue3") +
#   geom_point(data = subset(juv,ISLAND=="Oahu"),aes(x = LONGITUDE, y = LATITUDE), size = 2, shape = 8, color = "darkorange1") +
#  coord_sf(xlim = c(-158.5, -157.5), ylim = c(21.2, 21.8), expand = FALSE)+
#   theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
#                                         size = 0.5), panel.background = element_rect(fill = "aliceblue"))+
#   annotation_scale(location = "bl", width_hint = 0.4)
#   
# extent(subset(all_2,ISL=="MAU"))
# 
# ggplot(data = islands) +
#   geom_sf() +
#   geom_point(data = subset(all_2,ISL=="MAU"), aes(x = x, y = y), size = 2, shape = 21, fill = "slateblue3") +
#   geom_point(data = subset(juv,ISLAND=="Maug"),aes(x = LONGITUDE, y = LATITUDE), size = 2, shape = 8, color = "darkorange1") +
#   coord_sf(xlim = c(145.2, 145.25), ylim = c(20, 20.05), expand = FALSE)+
#   theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
#                                         size = 0.5), panel.background = element_rect(fill = "aliceblue"))+
#   annotation_scale(location = "bl", width_hint = 0.4)


extent(all_sp)
table(all_sp$ISL)
cell_size <- 50/1000 # go from meters to km

span_x <- diff(extent(all_sp)[1:2]) # the span of degrees
span_x_km <- span_x*111.111 # converting degrees to km of the span of the points
n_cell_x <- round(span_x_km/cell_size) # the value we want to assign to ncol 

span_y <- diff(extent(all_sp)[3:4]) # the span of degrees
span_y_km <- span_y*111.111
n_cell_y <- round(span_y_km/cell_size) # the value we want to assign to nrow 

# convert from spdf to raster
rast <- raster()
extent(rast) <- extent(all_sp) # this might be unnecessary
ncol(rast) <- n_cell_x # this is one way of assigning cell size / resolution
nrow(rast) <- n_cell_y
rast2 <- rasterize(all_sp, rast, all_sp$means, fun=mean)
rast2
# writeRaster(rast2, "WavesHawaii.nc", format = "CDF") too big to save
# plot(rast2) #waaaaay too large to plot

# convert juv data to spatial points data
xy_juv <- juv[,c(3,4)]
juv_sp <- SpatialPointsDataFrame(coords = xy_juv, data = juv,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
str(juv_sp)


### calculate the mean wave action values within 250m radius of each juv point
# source expanding extract function
source("M:/Environmental Data Summary/HelperCode/ExpandingExtract.R")

sites_waves <- ExpandingExtract(r = rast2, SpDF = juv_sp, Dists = seq(0, 4000, by = 50))
sites_waves

juv_2 <- cbind(juv, sites_waves)

ggplot(juv_2[ which(juv_2$Island_Name == "Lanai"),], aes(x = Longitude_DD, y = Latitude_DD, color = Dist)) + 
  geom_point() +
  scale_color_viridis_c()

plot(sort(juv_2$Dist)) # where there are natural breaks in distances
abline(h = 1000)
abline(h=750) # this seems like a reasonable break
abline(h=500)

# save the data!
setwd("C:/Users/Morgan.Winston/Desktop/MHI NWHI 2019 Coral Bleaching/Data/Bleaching Assessments/Combined/Current Database/For Analysis/Supplemental Data")
write.csv(juv_2, "Hawaii_WaveActionData.csv")
