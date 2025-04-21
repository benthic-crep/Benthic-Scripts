library(jsonlite)

#API data
esddata <- fromJSON("https://picmid1.nmfs.local/gis/esd/esd_data/benthic_cover_random")
cov_r=esddata$items
esddata <- fromJSON("https://picmid1.nmfs.local/gis/esd/esd_data/benthic_cover_fixed")
cov_f=esddata$items

#TABLE Data # whole lot of cleaning that happens after these loads, so don't assume this script will work as is...
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_BIA_STR_RAW_NEW.rdata")   #bia
load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_CNET_Annotations.rdata") #load data



table(cov_r$region_name,cov_r$obs_year)

#                             2015 2023
# American Samoa              5168  342
# Pacific Remote Island Areas 4490    0

table(cov_f$region_name,cov_f$obs_year)

#                             2010 2012 2023
# American Samoa              4780    0  577
# Pacific Remote Island Areas    0 4643    0



##THESE ARE ALL THE DATA WE HAVE FROM RANDOM (BIA, CNET, and special NW14_15_17) and FIXED: CLI
#Combine cpc and coralnet
FIELDS_TO_RETAIN<-c("MISSIONID","METHOD", "REGION", "OBS_YEAR","ISLAND", "SITEVISITID","SITE",
                    "LATITUDE", "LONGITUDE", "REEF_ZONE", "DEPTH_BIN", "PERM_SITE", "CLIMATE_STATION_YN",
                    "MIN_DEPTH", "MAX_DEPTH", "HABITAT_CODE", "REP", "IMAGE_NAME", "PHOTOID", "ANALYST",
                    "TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME", "POINTS")

#RANDOM THREE TABLES!
# OLD "Benthic Image Analysis
x<-bia[,FIELDS_TO_RETAIN];
table(x$REGION,x$OBS_YEAR)
#         2010  2011  2012  2013  2014
# CT         0     0     0 15772  1157
# MARIAN     0 34809     0     0 45644
# MHI    17742     0 11898 35107     0
# NWHI       0     0     0  1698     0
# PRIAs      0     0     0     0  7142
# SAMOA  26092     0     0     0     0

# NEW "CORALNET"
y<-cnet[,FIELDS_TO_RETAIN]
table(y$REGION,y$OBS_YEAR)
#          2010   2012   2015   2016   2017   2018   2019   2020   2022   2023
# CT          0   1560   2920      0      0      0      0      0      0      0
# MARIAN      0      0      0      0 163630      0      0      0 153410      0
# MHI         0      0  88330 125750      0      0 152450      0      0      0
# NWHI        0      0      0  83390      0      0  10800      0      0      0
# PRIAs       0  77960 144870  17810  56040  86640      0      0      0  11360
# SAMOA    4780  91520 188260  68700      0  78990      0  17860   5400 101440

#SPECIAL ANALYSIS NWHI 14,15,17
n457=nw[,FIELDS_TO_RETAIN]
table(n457$REGION,n457$OBS_YEAR)
#       2014  2015  2017
# NWHI  5990 19660 35070

#FIXED!
z<-cli[,FIELDS_TO_RETAIN]
table(cli$REGION,cli$OBS_YEAR)
#        2012 2013 2014
# MARIAN    0    0 4866
# MHI       0 3605    0
# NWHI      0 3020    0
# PRIAs  7076    0  838
# SAMOA  9229    0    0


ab<-rbind(x,y,z,n457)
names(ab)
# [1] "MISSIONID"          "METHOD"             "REGION"             "OBS_YEAR"           "ISLAND"             "SITEVISITID"        "SITE"               "LATITUDE"           "LONGITUDE"         
# [10] "REEF_ZONE"          "DEPTH_BIN"          "PERM_SITE"          "CLIMATE_STATION_YN" "MIN_DEPTH"          "MAX_DEPTH"          "HABITAT_CODE"       "REP"                "IMAGE_NAME"        
# [19] "PHOTOID"            "ANALYST"            "TIER_1"             "CATEGORY_NAME"      "TIER_2"             "SUBCATEGORY_NAME"   "TIER_3"             "GENERA_NAME"        "POINTS"            

names(cov_r)
# [1] "roundid"            "missionid"          "region_name"        "island"             "site"               "occ_siteid"         "sitevisitid"        "latitude"           "longitude"         
# [10] "reef_zone"          "depth_bin"          "perm_site"          "climate_station_yn" "min_depth"          "max_depth"          "date_"              "image_name"         "obs_year"          
# [19] "replicate"          "photoid"            "analyst"            "tier_1"             "category_name"      "tier_2"             "subcategory_name"   "tier_3"             "genera_name"       
# [28] "x_pos"              "y_pos"              "survey_type"       

all(names(cov_f)==names(cov_r))
# TRUE

names(ab)[which(!names(ab)%in%toupper(names(cov_r)))]
#"METHOD"       "REGION"       "HABITAT_CODE" "REP"          "POINTS"      
names(cov_r)[which(!names(cov_r)%in%tolower(names(ab)))]
#[1] "roundid"     "region_name" "occ_siteid"  "date_"       "replicate"   "x_pos"       "y_pos"       "survey_type"

table(ab$METHOD)
