### TOWED-DIVER SURVEY DATA PRE-PROCESSING
# this script reads in raw TDS data and produces a cleaned csv, retained @ segment level
# written by: morgan winston
# last modified: 5/29/18

#### INITIALIZATION ####
# load libraries & functions
library(plyr)

# set working directory
setwd("T:/Benthic/Data/TDS_Raw")

# read in data
tds <- read.csv("VS_BENT_TDS_DATA_VIEW.csv")

#### CLEAN DATA ####

setwd("T:/Benthic/Data/TDS_Raw/Depths")

# need to assign depths to segments missing this - read in supplemental data:
  # MEAN = MULTI data; MEAN_1 = SAT data

bak <- read.csv("segments_bak_zonal_join.csv")
  bak <- bak[,c("DiveID", "SegmentID", "DiveYear", "Island", "MEAN", "MEAN_1")]
  names(bak) <- c("DIVEID", "SEGMENTID","OBS_YEAR","ISLAND","MEAN_MULT","MEAN_SAT")

jar <- read.csv("segments_jar_zonal_join.csv")
  jar <- jar[,c("DiveID", "SegmentID", "DiveYear", "Island", "MEAN")]
  names(jar) <- c("DIVEID", "SEGMENTID","OBS_YEAR","ISLAND","MEAN_MULT")
  jar$MEAN_SAT <- 0
  
joh <- read.csv("segments_joh_zonal_join.csv")
  joh <- joh[,c("DiveID", "SegmentID", "DiveYear", "Island", "MEAN_1")]
  names(joh) <- c("DIVEID", "SEGMENTID","OBS_YEAR","ISLAND", "MEAN_SAT")
  joh$MEAN_MULT <- 0
  
pal <- read.csv("segments_pal_zonal_join.csv")
  pal <- pal[,c("DiveID", "SegmentID", "DiveYear", "Island", "MEAN", "MEAN_1")]
  names(pal) <- c("DIVEID", "SEGMENTID","OBS_YEAR","ISLAND","MEAN_MULT","MEAN_SAT")
  
wak <- read.csv("segments_wak_zonal_join.csv")
  wak <- wak[,c("DiveID", "SegmentID", "DiveYear", "Island", "MEAN")]
  names(wak) <- c("DIVEID", "SEGMENTID","OBS_YEAR","ISLAND","MEAN_MULT")
  wak$MEAN_SAT <- 0

kin <- read.csv("segments_kin_zonal_join.csv")
  kin <- kin[,c("DiveID", "SegmentID", "DiveYear", "Island", "MEAN", "MEAN_1")]
  names(kin) <- c("DIVEID", "SEGMENTID","OBS_YEAR","ISLAND","MEAN_MULT","MEAN_SAT")
  
isldep <- rbind(bak,jar,kin,wak,joh,pal)

tds$DEPTH[is.na(tds$DEPTH)] = 0 # change NA values to zero
tds_nodep <- tds[ which(tds$DEPTH == 0),] # data missing depths

tds_nodep <- merge(tds_nodep, isldep, by = c("DIVEID","SEGMENTID","OBS_YEAR","ISLAND")) # add missing depths 
tds_nodep$MEAN_MULT[is.na(tds_nodep$MEAN_MULT)] = 0 # change NA to zero
tds_nodep$MEAN_SAT[is.na(tds_nodep$MEAN_SAT)] = 0 # change NA to zero

tds_nodep$MEAN_MULT <- tds_nodep$MEAN_MULT*-1 # make depths positive
tds_nodep$MEAN_SAT <- tds_nodep$MEAN_SAT*-1 # make depths positive

for(i in c(1:nrow(tds_nodep))){ #
  if(tds_nodep$MEAN_MULT[i] == tds_nodep$MEAN_SAT[i]){ # if same value (should only be zero) make depth = 0
    tds_nodep$DEPTH[i] <- 0
  }
  else{
    tds_nodep$DEPTH[i] = max(tds_nodep$MEAN_MULT[i],tds_nodep$MEAN_SAT[i]) # depth should be equal to whichever value is greater
  }
}

tds_nodep$MEAN_MULT <- NULL # now that we have new DEPTH column can remove the MULT and SAT columns
tds_nodep$MEAN_SAT <- NULL

#### note: Tomoko has said that the non-diver recorded depths are ~1 m deeper than divers record -- so we might need to modify the code?

tds_dep <- tds[ which(tds$DEPTH > 0),] # data that has depths
tds_fix <- rbind(tds_dep, tds_nodep) # bind the data back together

# rename centroid lat/long columns 
tds_fix$LATITUDE <- NULL
tds_fix$LONGITUDE <- NULL
colnames(tds_fix)[colnames(tds_fix) == "CENTROIDLAT"] <- "Latitude" 
colnames(tds_fix)[colnames(tds_fix) == "CENTROIDLON"] <- "Longitude" 

# assign depth bins
range(tds_fix$DEPTH)
tds_fix$DEPTH_BIN <- "UNK"

for(i in c(1:nrow(tds_fix))){
  if(tds_fix$DEPTH[i] < 6 & tds_fix$DEPTH[i] > 0){
    tds_fix$DEPTH_BIN[i] <- "S"
  }
  if(tds_fix$DEPTH[i] >= 6 & tds_fix$DEPTH[i] <= 18){
    tds_fix$DEPTH_BIN[i] <- "M"
  }
  if(tds_fix$DEPTH[i] > 18){
    tds_fix$DEPTH_BIN[i] <- "D"
  }
}

  
# sum urchins
tds_fix$URCHIN[is.na(tds_fix$URCHIN)] <- 0

for (i in c(1:nrow(tds_fix))){
  if(tds_fix$OBS_YEAR[i] < 2003){
    tds_fix$URCHIN[i] <- tds_fix$URCHIN[i] # for years 2000-2002, use URCHIN value 
  }
  if(tds_fix$OBS_YEAR[i] > 2002){
    tds_fix$URCHIN[i] <- tds_fix$BORING_URCHIN[i] + tds_fix$FREE_URCHIN[i] # for years 2003-present, use BORING + FREE values
  }
}

tds_fix$BORING_URCHIN <- NULL
tds_fix$FREE_URCHIN <- NULL

# change sea cucumber na values to -1 values (so ArcMap can handle)
tds_fix$SEA_CUCUMBER[is.na(tds_fix$SEA_CUCUMBER)] <- -1

# calculate invert densities (# per 100m2); each segment is ~200m long by 8m wide (1600m2)
tds_fix$URCHIN_dens100m <- (tds_fix$URCHIN * 100)/1600
tds_fix$CROWN_OF_THORN_dens100m <- (tds_fix$CROWN_OF_THORN * 100)/1600
tds_fix$GIANT_CLAM_dens100m <- (tds_fix$GIANT_CLAM * 100)/1600
tds_fix$SEA_CUCUMBER_dens100m <- (tds_fix$SEA_CUCUMBER * 100)/1600

# save this csv for future work
setwd("T:/Benthic/Data/TDS_Raw")
write.csv(tds_fix, "benthic_tds_clean.csv")
