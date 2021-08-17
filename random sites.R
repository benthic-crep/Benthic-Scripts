# Randomly generate sites

# load appropriate grid file

# ------------------MHI-------------------------------
# connect to T drive, load grid file
df<-read.csv("T:/Fish/GIS/Projects/Gridding/MHI/MHI_grid_points.csv")

# Function to randomly select rows:
sample.df<-function(df,n) df[sample(nrow(df),n), ,drop=FALSE]

# Oahu:
pl<-subset(df,(X <-157.705 & X > -157.713) & (Y < 21.263 & Y>21.258))
#oah<-sample.df(pl[(pl$ISLAND == "Oahu"), ], 25)
oah$SITE<-1:length(oah$OBJECTID)
oah$SITE<-paste("PL",oah$SITE,sep="")

library(dplyr)
library(sp)
library(sf)
library(raster)
library(ncf) # for gcdist()
library(ggsn)
library(ggspatial)
library(ggrepel)

#Read in islands shapefile
islands<-st_read("U:/GIS/Data/Pacific/islands.shp")

#Plotting the wave and juvenile data for a subset of islands to check overlap
#Helpful website for plotting maps with ggplot https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
Plot_RandomSites<-function(d1,d2,randisland="Oahu",xlim1,xlim2,ylim1,ylim2){
  ggplot(data = d1) +
    geom_sf() +
    geom_point(data = subset(d2,ISLAND==randisland),aes(x = X, y = Y), size = 4, shape = 20, color = "darkorange1") +
    coord_sf(xlim = c(xlim1, xlim2), ylim = c(ylim1, ylim2), expand = FALSE)+
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
                                          size = 0.5), panel.background = element_rect(fill = "aliceblue"))+
    annotation_scale(location = "bl", width_hint = 0.4)+
  # geom_text(data = d2, aes(x = X, y = Y,label=SITE), size = 3, hjust=0, vjust=-1)
  geom_text_repel(data = subset(d2,ISLAND==randisland),aes(x = X, y = Y, label = SITE),min.segment.length = 0,size=3)
  
}

p1<-Plot_RandomSites(islands,oah,"Oahu",-157.72, -157.68,21.25, 21.27)+ggtitle("Portlock Site Selection");p1

write.csv(oah,file="M:/SFM Site Maps/Portlock site selection/RandomSites_Portlock.csv")

ggsave(plot=p1,file="M:/SFM Site Maps/Portlock site selection/RandomSites_Portlock.pdf",width=10,height=7)



