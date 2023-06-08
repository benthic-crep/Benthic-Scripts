#### For NOAA computers: in order to use this script you will need to contact ITS to set up the ODBC drivers on your computer. You will not have adminstrative rights
# You will also need to set up an oracle account if you haven't already
##Once everything it set up you can download the individual datasets. If you experience errors that datasets can't be found double check your oracle account to make sure you have access
#if you don't have access you will need to ask someone in ESD Data Management to have you access to whatever views you are interested in.

#ITS will be providing the steps to set up your ODBC drivers, which will be included here soon.


rm(list=ls())
library(RODBC)            # to connect to oracle
#setwd("/Users/c/Documents/CRED/Fish Team/Base R/Base Data Files")

Sys.setenv(ODBCINI = "/library/ODBC/odbc.ini")

##*******## Access the GIS database in Oracle. Add your username and password. If you forget your password then you will need to contact ITS
ch <- odbcConnect("GIS", uid = "CCOUCH", pwd = "XXXX") #from kiteworks
##
## #list available tables
tv<-sqlTables(ch, tableType = "VIEW")

a<-as.vector(tv$TABLE_NAME[grep("V0_", as.character(tv$TABLE_NAME))])
b<-as.vector(tv$TABLE_NAME[grep("V_", as.character(tv$TABLE_NAME))])
c<-as.vector(tv$TABLE_NAME[grep("V_BIA_PERC_COVER_PHOTO_STR", as.character(tv$TABLE_NAME))])
d<-as.vector(tv$TABLE_NAME[grep("V_BIA", as.character(tv$TABLE_NAME))])

## Generate a table of Views that you have access to. If you don't see the view you are looking for then it is probably a permissions issue, check with Micahael to get access.
rawtables<-c(a,b,c)
rawtables

df <- sqlQuery(ch, paste("SELECT * FROM GISDAT.V0_SITE_VISIT_ALL")); head(df)
save(df, file="ALL_SITE_SITEVISIT_EVERYTHING.rdata")
df<-df[,c("REGION", "ISLAND", "SECTOR", "SITE", "LATITUDE_LOV", "LONGITUDE_LOV", "REEF_ZONE", "DEPTH_BIN", "PERM_SITE", "PERM_SITE_ARC", "PERM_TRANSECT", "CLIMATE_STATION_YN", "MIN_Z_M", "MAX_Z_M", "PROBLEMATIC", "OLDSITE", "EXCLUDE_FLAG", "COMMENTS_LOS",              
	"SITEVISITID","TYPE", "FISH_SITEVISITID", "BENTHIC_SITEVISITID", "DATE_", "LOCALTIME", "LATITUDE_SV", "LONGITUDE_SV", "HABITAT_CODE",  "MIN_DEPTH_M", "MAX_DEPTH_M", "VISIBILITY", "VISIBILITY_M", "SITEDESCRIPTION",
 	"MISSIONID", "ROUNDID", "COMMENTS", "COMMENTS_SV", "PERM_MARKER", "FISH_REA_YN", "CORAL_BELT_ADULT_YN", "CORAL_BELT_JUVENILE_YN", "MICROBIAL_YN", "PHOTOMOSAIC_YN", "INVERT_YN", "LPI_YN", "PIN_PHOTOS_YN", "MICROBIAL_SAMPLE", "TRANSECT_PHOTOS", "OTHER_SAMPLE", 
 	"ARMS_DEPLOY_YN", "ARMS_RECOVERED", "ARMS_RETRIEVE_YN", "CAUS_DEPLOYED", "CAUS_DEPLOY_YN", "CAUS_RECOVERED", "CAUS_RETRIEVE_YN", "CORAL_SAMPLE", "CORES", #just added these
 	"NOTES")]                                   
save(df, file="ALL_SITE_SITEVISIT.rdata")

# NOT INCLUDING
# "GEOREG_NUM", "GEO_SEQUENCE" "CREATE_DTM_LOS", "LAST_MOD_BY_LOS", "LAST_MOD_DTM_LOS", "OBJECTID_LOS", "CREATED_BY_LOS", "LAST_MOD_BY_SV", "LAST_MOD_DTM_SV", "OBJECTID_SV", "CREATED_BY_SV", "CREATE_DTM_SV"
# "ALGAE_CORISID", 'FISH_BLT_CORISID", "BENTHIC_LPI_CORISID", "IA_CORISID", "CORAL_CORISID", "CORAL_DZ_CORISID", "FISH_SPC_CORISID"                 
# "DIVE", "OWNER", "BIOGEO", "ANALYZED", "COMPLETED_DATE", "COMPLETED_BY", "TRANSECT_BEARING", "TRANSECT_NOTES", "BEARING"   
# "CORAL_SAMPLE", "CORES", "CTD_CAST"  
# "QC_CHECK_ALGAE_LPI", "QC_CHECK_ALGAE_ROVE", "QC_CHECK_CORAL_A", "QC_CHECK_CORAL_B", "QC_CHECK_INVERT_BELT", "QC_CHECK_INVERT_SIZE", "QC_COMPLETED_DATE", "NEED_PHOTO_QC"  
# "PHOTOS_MOVED", "PHOTOS_MOVED_NOTES", "PHOTO_QC", 
# "DISTRICT", "ARMS_DEPLOY_YN", "ARMS_RECOVERED", "ARMS_RETRIEVE_YN", "ALGAL_ROVING", "ALGAL_SAMPLE", "ARMS_DEPLOYED", "BMUS_DEPLOY_YN", "BMUS_RETRIEVE_YN", "CAUS_DEPLOYED", "CAUS_DEPLOY_YN", "CAUS_RECOVERED", "CAUS_RETRIEVE_YN", "CAU_COMMENTS" 
# "ASSIGNED_DATE", "ASSIGNED_TO"                                

df <- sqlQuery(ch, paste("SELECT * FROM GISDAT.V0_FISH_REA")); head(df)
save(df, file="ALL_REA_FISH_RAW.rdata")

#Raw adult coral colony data from REA surveys using method e
# df <- sqlQuery(ch, paste("SELECT * FROM GISDAT.V0_CORAL_OBS_E")); head(df) #This is the older View that we used prior to 2022
# save(df, file="T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_ADULTCORAL_RAW_2013-2020.rdata")

df <- sqlQuery(ch, paste("SELECT * FROM gisdat.V_NCEI_CORAL_OBS_E")); head(df) #We are now using the NCEI view- only difference is fewer collumns that we don't use any more and adding REPEAT_SEG
save(df, file="T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_ADULTCORAL_RAW_2013-2022.rdata")

#Raw juvenile coral colony data from REA surveys using method f #We are now using the NCEI view- only difference is fewer collumns that we don't use any more and adding REPEAT_SEG
# df <- sqlQuery(ch, paste("SELECT * FROM GISDAT.V0_CORAL_OBS_F")); head(df) #This is the older View that we used prior to 2022
# save(df, file="T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_JUVCORAL_RAW_2013-2020.rdata")

df <- sqlQuery(ch, paste("SELECT * FROM gisdat.V_NCEI_CORAL_OBS_F")); head(df)
save(df, file="T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_JUVCORAL_RAW_2013-2022.rdata")

##
##

#TOW FISH PRESENCE DATA#
df <- sqlQuery(ch, paste("SELECT * FROM GISDAT.VS_FISH_TDS_SIGHTINGS")); head(df)
save(df, file="ALL_TOW_FISH_SIGHTINGS.rdata")

#TOW FISH #
df <- sqlQuery(ch, paste("SELECT * FROM GISDAT.VS_FISH_TDS")); head(df)
save(df, file="ALL_TOW_FISH_RAW.rdata")

#TOW BENTHIC#
df <- sqlQuery(ch, paste("SELECT * FROM GISDAT.VS_BENT_TDS")); head(df)
save(df, file="ALL_TOW_BENT_RAW.rdata")




#Photoquad Data -raw point annotations
#Older CPCe (2010-2014) annotations from StRS sites
bia <- sqlQuery(ch, paste("SELECT * FROM GISDAT.V_BIA_PERC_COVER_PHOTO_STR_")); head(bia)
save(bia, file="T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_CPCe_STR_2010-2014.rdata")

#Older CPCe (2010-2014) annotations from Climate sites
cli <- sqlQuery(ch, paste("SELECT * FROM GISDAT.V_BIA_PERC_COVER_PHOTO_CLI_")); head(cli)
save(cli, file="T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_CPCe_CLIMATE_2010-2014.rdata")

# Coral Net Benthic Data (2015- present & some legacy 2012 imagery) from StRS, Climate and special mission sites
cnet <- sqlQuery(ch, paste("SELECT * FROM GISDAT.MV_BIA_CNET_ANALYSIS_DATA_UNION")); head(cnet)
save(cnet, file="T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_CNET_Annotations.rdata")

#Coral Net Benthic Data (2018) from StRS and Climate sites - Robot only annotations generated immediately after the cruise using 100% alleviation. Should only be used for Tier 1 Coral and CCA

cnet_incR <- sqlQuery(ch, paste("SELECT * FROM GISDAT.MV_BIA_CNET_ANALYSIS_DATA_ALL")); head(cnet_incR)
save(cnet_incR, file="ALL_BIA_STR_CNET_INC_ROBOT.rdata")




