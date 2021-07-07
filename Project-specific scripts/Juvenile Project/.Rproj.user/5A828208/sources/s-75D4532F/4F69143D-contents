#set up API to connect R with GoogleMaps, etc. 
#instructions for setting up Google API: https://cran.r-project.org/web/packages/ggmap/readme/README.html
#Note: You won't be able to set up an API through your noaa.gov account. Once you enter your credit card info, 
#click enable maps API and it will automatically show you your API key. Save the key in a text file 
#Additional information: https:/www.littlemissdata.com/blog/maps & http:/eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

#https://www.findingyourway.io/blog/2018/12/05/2018-12-05-using-ggmap-after-july-2018/

library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)
library(ggmap)
library(sf)
library(ggnewscale)
library(scales)
library(gridExtra)
library(plyr)

# load extra functions
g_legend<-function(a.gplot){ # function to extract legend from a plot
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# load googlemaps API Key
ggmapAPI = readChar("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Couch_googlemapsAPIkey.txt",nchars = 39)
register_google(key = ggmapAPI)

# set working directory
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project")
# import zone shapefile - readOGR() converts the shapefile into a Spatial Polygons Data Frame
sec <- readOGR("T:/Common/Maps/Pacific/Sector/ALLPacific_Sectors_Islands.shp")
#import sector-level delta juvenile data (difference in juvenile density between 2 years)
delj <- read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Response Variables/JuvProject_deltadensity_SECTOR.csv")
# import all site-level juvenile data
j.site <- read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS.csv")

#Remove 2 sites that weren't surveyed for juveniles
j.site<-j.site[!(j.site$SITE %in% c("OFU-01012","PAG-00596")),]


# convert the site-level juvenile data to a spatial points data frame for mapping
juv.site = SpatialPointsDataFrame(coords=j.site[,c("LONGITUDE","LATITUDE")], data=j.site)
crs(juv.site) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
scale = 1.3 # set the scale by which to expand the map extent (generaly 1.3 works for all islands with exceptions)


## ## ## ## ##
### LANAI ####
## ## ## ## ##

lan <- juv.site[ which(juv.site$ISLAND == "Lanai"),]
isl_bb = scale*extent(lan)

# load sectors shapefile, convert to SF for stable plotting in ggplot
sector <- st_as_sf(sec)

# download base map and check zoom level
isl_base = get_map(location = c(mean(isl_bb[1:2]), mean(isl_bb[3:4])), zoom=11, maptype="satellite")
ggmap(isl_base)
sector_points <- sf::st_point_on_surface(sector)
sector_points <- sf::st_point_on_surface(sector[ which(sector$ISLAND_CD == "LAN"),])
sector_coords <- as.data.frame(sf::st_coordinates(sector_points))
sector_coords$NAME <- c("South", "North")
sector_coords$Y <- c(20.675, 20.95)

# run final plot
lan$OBS_YEAR<-as.factor(lan$OBS_YEAR)
ggmap(isl_base) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, color = OBS_YEAR), data = as.data.frame(lan), size = 4) +
  #scale_color_distiller(palette = "Spectral", name = "Year") +
  geom_sf(data = sector[ which(sector$ISLAND_CD == "LAN"),],fill=NA,color="white",alpha=.5, inherit.aes = FALSE) + # plots zones
  coord_sf(expand = F) +
  geom_text(data = sector_coords, aes(X, Y, label = NAME), colour = "white", size = 8)+ 
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 18, colour = "black"),
    title=element_text(size = 18)) +
  scale_x_continuous(limits = c(-157.1, -156.75), breaks = c(-157, -156.9, -156.8), labels = c(-157, -156.9, -156.8)) +
  scale_y_continuous(limits = c(20.665, 20.97), breaks = c(20.7, 20.75, 20.8, 20.85, 20.9, 20.95), labels = c(20.7, 20.75, 20.8, 20.85, 20.9, 20.95)) +
  ggtitle("Lanai")



## ## ## ## ##
### MHI ####
## ## ## ## ##

mhi <- delj[ which(delj$REGION == "MHI"),]
isl_bb = scale*extent(mhi)

# load sectors shapefile, convert to SF for stable plotting in ggplot
sector <- st_as_sf(sec)

# download base map and check zoom level
isl_base = get_map(location = c(mean(isl_bb[1:2]), mean(isl_bb[3:4])), zoom=11, maptype="satellite")
ggmap(isl_base)
sector_points <- sf::st_point_on_surface(sector)
sector_points <- sf::st_point_on_surface(sector[ which(sector$Region == "MHI"),])
sector_coords <- as.data.frame(sf::st_coordinates(sector_points))
# sector_coords$NAME <- c("South", "North")
# sector_coords$Y <- c(20.675, 20.95)

# run final plot
ggmap(isl_base) +
  geom_polygon(data = delj, aes(x=long, y = lat, group = group), fill = "violet", color = "blue") + 
    #scale_color_distiller(palette = "Spectral", name = "Year") +
  geom_sf(data = sector[ which(sector$ISLAND_CD == "LAN"),],fill=NA,color="white",alpha=.5, inherit.aes = FALSE) + # plots zones
  coord_sf(expand = F) +
  geom_text(data = sector_coords, aes(X, Y, label = NAME), colour = "white", size = 8)+ 
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 18, colour = "black"),
    title=element_text(size = 18)) +
  scale_x_continuous(limits = c(-157.1, -156.75), breaks = c(-157, -156.9, -156.8), labels = c(-157, -156.9, -156.8)) +
  scale_y_continuous(limits = c(20.665, 20.97), breaks = c(20.7, 20.75, 20.8, 20.85, 20.9, 20.95), labels = c(20.7, 20.75, 20.8, 20.85, 20.9, 20.95)) +
  ggtitle("Lanai")
