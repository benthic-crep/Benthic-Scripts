rm(list=ls())
#library(gdata)             # needed for drop_levels()
#library(reshape)           # reshape library inclues the cast() function used below

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic Functions.R")

#LOAD THE CLEAN wd 
load("Benthicwd.Rdata")

#get base survey info, calculate average depth+complexity+so on

#ADD SECTOR NAME, ANALYSIS YEAR, ANALYSIS SCHEME EVENTUALLY
SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH","SITE_MAX_DEPTH")
survey_table<-Aggregate_InputTable(wd, SURVEY_INFO)



# GENERATE SUMMARY METRICS at the site-level -this is just a start, more metrics to come --------------------------------------------------
m1<-Calc_ColDen_By_Site(wd)
m2<-Calc_olddead_By_Site(wd)


# GENERATE SUMMARY METRICS at the site & taxon level -this is just a start, more metrics to come --------------------------------------------------
m3<-Calc_ColDen_By_Taxon_Site(wd)

#Merge Site Data with benthic summary metric data 
wsd_site <- Reduce(MyMerge, list(m1, m2,survey_table))

# OUTPUT working_site_data  -----------------------------------
save(wsd_site, file="TMPwsd_site.Rdata")
