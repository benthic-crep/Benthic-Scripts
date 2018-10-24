# RAMP Coral Species Comparison: Compares density & the proportion of divers that observed each species across years
# Version: 10/22/2018
# Written by: Morgan Winston

### INITIALIZATION ###

# load required packages
library(ggplot2)
library(viridis)
require(scales)
library(grid)

# need to source benthic team functions for functions to run throughout this script - change directory to wherever files are located on personal computer
source("C:/Users/Morgan.Winston/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions.R")
source("C:/Users/Morgan.Winston/Documents/GitHub/Benthic-Scripts/Functions/core_functions.R")

# set working directory
setwd("T:/Benthic/Data")

# required files: benthic REA data
# dat <- read.csv("dens_fake.csv") # fake data to play with for now
dat <- read.csv("Benthic_taxa comparison_site.csv")

### DATA MANIPULATION ###
dat$X <- NULL
dat <- dat[ ! dat$GENUS_CODE %in% c("MOAS", "LEPA", "UNKN", "PALS", "TUBA"), ]

dat$ColDen <- as.numeric(dat$ColDen)
dat$OBS_YEAR <- as.factor(dat$OBS_YEAR)

dat$REGION <- as.character(dat$REGION)
dat$SUBREGION_NAME <- as.character(dat$SUBREGION_NAME)
for(z in c(1:nrow(dat))){
  if(dat$REGION[z] == "Mariana Archipelago"){
    dat$REGION[z] <- dat$SUBREGION_NAME[z]
  }
  else{}
}

dat$REGION <- as.factor(dat$REGION)

dat$prop.observed <- as.numeric(dat$prop.observed)

dat$obs.bins <- cut(dat$prop.observed, breaks=seq(0,1,0.1), 
                    labels=c("0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8","0.8-0.9","0.9-1"))

setdiff(dat$GENUS_CODE, lookup$GENUS_CODE)
dat$Genus <- NA
for(i in c(1:nrow(dat))){
  dat$Genus[i] <- gen_name_fun(dat$GENUS_CODE[i])
}

dat$Genus <- as.factor(dat$Genus)
dat$OBS_YEAR <- factor(dat$OBS_YEAR, levels=rev(levels(dat$OBS_YEAR)))
levels(dat$Adult_juv)[levels(dat$Adult_juv)=="Juv"] <- "Juvenile"

### PLOTS ###
# cycle through each region (MHI, NWHI, NMAR, SMAR, PRIA, SAMOA) and produce plots for both adults and juveniles

plot_fun <- function(data){
  
  regions <- levels(unique(data$REGION))
  ages <- levels(unique(data$Adult_juv))
  
 for(q in c(1:length(regions))){
   dat_reg <- data[ which(data$REGION == regions[q]),] # subset to region
   
   for(l in c(1:length(ages))){
     dat_reg_age <- dat_reg[ which(dat_reg$Adult_juv == ages[l]),] # subset to age
  

   fig <- ggplot(dat_reg_age, aes(x = SPCODE, y = ColDen, fill = prop.observed, color = OBS_YEAR)) +
     facet_wrap(~Genus, scales = "free") +
     labs(y = expression(Density~~"(no. colonies"*~m^"-2"*")"), x = "") +
     geom_bar(size = 1, stat = "identity") +
     scale_fill_viridis(name = "Proportion of\nDivers Observed\n") +
     ggtitle(paste(regions[q], ages[l], "Colony Observations", sep = " ")) +
     theme_bw() +
     theme(
       plot.title = element_text(color="black", size=12),
       panel.border = element_rect(colour="dark gray"), 
       panel.grid.major = element_blank(), 
       panel.grid.minor = element_blank(),
       axis.title = element_text(size = 12),
       axis.text.x = element_text(angle = 90, vjust= 1, hjust=0.5, size = 7, colour="black"), 
       axis.text.y = element_text(angle = 0, hjust=0.5, colour="black", size = 9), 
       strip.text.y = element_text(angle = 0, size = 12),
       strip.text = element_text(size=9, face = "italic"))
  
   
   if(regions[q] == "American Samoa"){
     fig <- fig + scale_color_manual(name = "Year", labels = c("2018", "2015-2016"), values = c("black", "gray30"))
      if(ages[l] == "Juvenile"){
        wid = 17
      }
      if(ages[l] == "Adult"){
        wid = 20
      }
      h = 15
   }
   
   if(regions[q] == "Pacific Remote Island Areas"){
     fig <- fig + scale_color_manual(name = "Year", labels = c("2016", "2014-2015"), values = c("black", "gray30"))
     if(ages[l] == "Juvenile"){
       wid = 17
     }
     if(ages[l] == "Adult"){
       wid = 20
     }
     h = 15
   }
   
   if(regions[q] == "Main Hawaiian Islands"){
     fig <- fig + scale_color_manual(name = "Year", labels = c("2016", "2013"), values = c("black", "gray30"))
      wid = 12
      h = 12
   }
   
   if(regions[q] == "Northwestern Hawaiian Islands"){
     fig <- fig + scale_color_manual(name = "Year", labels = "2016", values = "black")
     wid = 12
     h = 12
   }
   
   if(regions[q] == "northern Mariana Islands" | regions[q] == "southern Mariana Islands"){
     fig <- fig + scale_color_manual(name = "Year", labels = c("2017", "2014"), values = c("black", "gray30"))
     if(ages[l] == "Juvenile"){
       wid = 15
       h = 12
     }
     if(ages[l] == "Adult"){
       wid = 17
       h = 15
     }
   }
   
    setwd("T:/Benthic/Projects/Species Comparisons")
    png(paste(regions[q], ages[l], "speciescomp.png", sep = "_"), width = wid, height = h, units = "in", res= 300)
    grid.draw(fig)
    dev.off()

   }  
 }
}
