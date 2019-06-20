#Revising Benthic REA (demography) Sector and Strata pooling FOR PACIFICWIDE 2018 REPORT
PoolSecStrat=function(site_data){
  
  #Create STRATANAME by idenityfing which ANALAYSIS SCHEME you want to use then concatinating with depth and reef zone that will be used to pool data
  site_data$BEN_SEC<-site_data$SEC_NAME
  
  #Changing sector pooling SAMOA
  site_data$BEN_SEC<-ifelse(site_data$OBS_YEAR=="2018"& site_data$ISLAND =="Tutuila","TUT",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$OBS_YEAR=="2018"&site_data$BEN_SEC %in% c("TAU_OPEN","TAU_SANCTUARY"),"TAU",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$OBS_YEAR=="2018"&site_data$BEN_SEC %in% c("SWA_OPEN","SWA_SANCTUARY"),"SWA",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$OBS_YEAR=="2015"&site_data$BEN_SEC %in% c("TAU_OPEN","TAU_SANCTUARY"),"TAU",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$OBS_YEAR=="2015"&site_data$BEN_SEC %in% c("TUT_NE","TUT_AUNUU_A"),"TUT_NE",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$OBS_YEAR=="2015"&site_data$BEN_SEC %in% c("TUT_SW","TUT_FAGALUA"),"TUT_SW",as.character(site_data$BEN_SEC))
  
  #Changing sector pooling PRIA
  site_data <- site_data[!(site_data$OBS_YEAR == "2018" & site_data$ISLAND=="Kingman" & site_data$REEF_ZONE=="Backreef"),] 
  
  
  #Changing sector pooling structure for GUAM & CNMI
  site_data <- site_data[!(site_data$ISLAND=="Maug" & site_data$REEF_ZONE=="Lagoon"),] 
  site_data$BEN_SEC<-ifelse(site_data$SEC_NAME %in% c("GUA_ACHANG","GUA_EAST_OPEN","GUA_PATI_POINT"),"GUAEAALL",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$SEC_NAME %in% c("GUA_HARBOR","GUA_WEST_OPEN","GUA_PITI_BOMB","GUA_SASA_BAY","GUA_TUMON"),"GUAWEALL",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$SEC_NAME %in% c("Guguan", "Alamagan", "Sarigan"),"AGS",as.character(site_data$BEN_SEC))
  site_data$ISLAND<-ifelse(site_data$SEC_NAME %in% c("Guguan", "Alamagan", "Sarigan"),"AGS",as.character(site_data$ISLAND))
  
  #Changing sector pooling structure for MHI- did not combine other islands and structure together because they did not have more than 1 sector (e.g. HAW_KONA_CR is the only coral rich sector on hawaii island)
  site_data$BEN_SEC<-ifelse(site_data$ISLANDCODE =="HAW"& site_data$bGEN_MHI_STRUCTURE=="CM","HAW_CM",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$ISLANDCODE =="MAI"& site_data$bGEN_MHI_STRUCTURE=="CM","MAI_CM",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$ISLANDCODE =="MAI"& site_data$bGEN_MHI_STRUCTURE=="SI","MAI_SI",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$ISLANDCODE =="MOL"& site_data$bGEN_MHI_STRUCTURE=="SI","MOL_SI",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$ISLANDCODE =="NII"& site_data$bGEN_MHI_STRUCTURE=="SI","NII_SI",as.character(site_data$BEN_SEC))
  
  
  #Specific Changes to depth bin and reef zone
  site_data <- site_data[!(site_data$ISLAND=="Johnston" & site_data$REEF_ZONE=="Lagoon"),] 
  site_data$REEF_ZONE<-ifelse(site_data$REEF_ZONE %in% c("Protected Slope","Forereef"),"Forereef",as.character(site_data$REEF_ZONE))
  site_data$DEPTH_BIN<-ifelse(site_data$ISLAND =="Kingman" & site_data$REEF_ZONE=="Lagoon","ALL",as.character(site_data$DEPTH_BIN))
  site_data$DEPTH_BIN<-ifelse(site_data$OBS_YEAR=="2015" & site_data$ISLAND =="Rose" & site_data$REEF_ZONE=="Backreef","ALL",as.character(site_data$DEPTH_BIN))
  
  #Create Strataname
  site_data$DB_RZ<-paste(substring(site_data$REEF_ZONE,1,1), substring(site_data$DEPTH_BIN,1,1), sep="")
  site_data$STRATANAME=paste0(site_data$BEN_SEC,"_",site_data$DB_RZ)
  
  #Changing STRATA pooling structure for SAMOA
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2015"&site_data$STRATANAME %in% c("TUT_AUNUU_B_FM","TUT_AUNUU_B_FS"),"TUT_AUNUU_B_FMS",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2015"&site_data$BEN_SEC=="ROS_SANCTUARY"&site_data$DB_RZ %in% c("FM","FD"), "ROS_FMD",as.character(site_data$STRATANAME))
  
  #Changing STRATA pooling structure for PRIA
  site_data$STRATANAME<-ifelse(site_data$BEN_SEC=="Johnston"&site_data$DB_RZ %in% c("BM","BD"), "Johnston_BMD",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$BEN_SEC=="Johnston"&site_data$DB_RZ %in% c("FM","FS"), "Johnston_FMS",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$BEN_SEC=="Johnston"&site_data$REEF_ZONE =="Lagoon", "Johnston_LA",as.character(site_data$STRATANAME))
  
  #Changing STRATA pooling structure for Guam and CNMI
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2014"&site_data$STRATANAME %in% c("Aguijan_FD","Aguijan_FM"),"Aguijan_FMD",as.character(site_data$STRATANAME))
  
  #Changing STRATA pooling structure for MHI
  site_data$STRATANAME<-ifelse(site_data$STRATANAME %in% c("LAN_NORTH_FD","LAN_NORTH_FM"),"LAN_NORTH_FMD",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2016"& site_data$BEN_SEC=="NII_SI"&site_data$DB_RZ %in% c("FM","FD"), "NII_SI_FMD",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2016"& site_data$BEN_SEC=="OAH_NE"&site_data$DB_RZ %in% c("FM","FD"), "OAH_NE_FMD",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$BEN_SEC=="KAH_SO"&site_data$DB_RZ %in% c("FM","FD"), "KAH_SO_FMD",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2013"&site_data$BEN_SEC=="MAI_CM"&site_data$DB_RZ %in% c("FM","FD"), "MAI_CM_FMD",as.character(site_data$STRATANAME))
  
  site_data$STRATANAME<-ifelse(site_data$BEN_SEC=="MAI_SI", "MAI_SI_A",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2016"&site_data$BEN_SEC=="MOL_PALI", "MOL_PALI_A",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2016"&site_data$BEN_SEC=="MOL_SOUTH", "MOL_SOUTH_ALL",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2016"&site_data$BEN_SEC=="MOL_SI"&site_data$DB_RZ %in% c("FM","FD"), "MOL_SI_FMD",as.character(site_data$STRATANAME))
  
  #Changing STRATA pooling structure for NWHI
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR %in% c("2014","2015")& site_data$BEN_SEC=="Laysan"&site_data$DB_RZ %in% c("FM","FS"), "Laysan_FMS",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR %in% c("2014","2015")& site_data$BEN_SEC=="Maro"&site_data$DB_RZ %in% c("FM","FD"), "Maro_FMD",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR %in% c("2014","2015")& site_data$BEN_SEC=="Pearl & Hermes"&site_data$DB_RZ %in% c("FM","FS"), "Pearl & Hermes_FMS",as.character(site_data$STRATANAME))
  
  
  #Modify Analysis Year############CORRECT THIS IN THE FUTURE TO MAKE IT MORE STREAMLINE!!!!!!!!!!!!!!!!!
  site_data$ANALYSIS_YEAR<-site_data$OBS_YEAR
  site_data$OBS_YEAR<-ifelse(site_data$OBS_YEAR %in% c("2014","2015")& site_data$REGION=="NWHI", "2014-15",as.character(site_data$OBS_YEAR))
  
  return(site_data)
}



#Revising Benthic REA (demography) Sector and Strata pooling FOR PIRO PMEA 4/19 DATA REQUEST
PoolSecStrat_PIRO=function(site_data){
  
  #Create STRATANAME by idenityfing which ANALAYSIS SCHEME you want to use then concatinating with depth and reef zone that will be used to pool data
  site_data$BEN_SEC<-site_data$SEC_NAME
  
  #Changing sector pooling SAMOA
  site_data$BEN_SEC<-ifelse(site_data$ISLAND =="Tutuila","TUT",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$OBS_YEAR=="2018"&site_data$BEN_SEC %in% c("TAU_OPEN","TAU_SANCTUARY"),"TAU",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$OBS_YEAR=="2018"&site_data$BEN_SEC %in% c("SWA_OPEN","SWA_SANCTUARY"),"SWA",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$OBS_YEAR=="2015"&site_data$BEN_SEC %in% c("TAU_OPEN","TAU_SANCTUARY"),"TAU",as.character(site_data$BEN_SEC))

  
  #Changing sector pooling PRIA
  site_data <- site_data[!(site_data$OBS_YEAR == "2018" & site_data$ISLAND=="Kingman" & site_data$REEF_ZONE=="Backreef"),] 
  site_data <- site_data[!(site_data$ISLAND=="Kingman" & site_data$REEF_ZONE=="Lagoon"),] 
  
  
  #Changing sector pooling structure for GUAM & CNMI
  site_data <- site_data[!(site_data$ISLAND=="Maug" & site_data$REEF_ZONE=="Lagoon"),] 
  site_data$BEN_SEC<-ifelse(site_data$SEC_NAME %in% c("GUA_ACHANG","GUA_EAST_OPEN","GUA_PATI_POINT"),"GUAEAALL",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$SEC_NAME %in% c("GUA_HARBOR","GUA_WEST_OPEN","GUA_PITI_BOMB","GUA_SASA_BAY","GUA_TUMON"),"GUAWEALL",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$SEC_NAME %in% c("Guguan", "Alamagan", "Sarigan"),"AGS",as.character(site_data$BEN_SEC))
  site_data$ISLAND<-ifelse(site_data$SEC_NAME %in% c("Guguan", "Alamagan", "Sarigan"),"AGS",as.character(site_data$ISLAND))
  
  #Changing sector pooling structure for MHI- did not combine other islands and structure together because they did not have more than 1 sector (e.g. HAW_KONA_CR is the only coral rich sector on hawaii island)
  site_data$BEN_SEC<-ifelse(site_data$ISLANDCODE =="HAW"& site_data$bGEN_MHI_STRUCTURE=="CM","HAW_CM",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$ISLANDCODE =="MAI"& site_data$bGEN_MHI_STRUCTURE=="CM","MAI_CM",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$ISLANDCODE =="MAI"& site_data$bGEN_MHI_STRUCTURE=="SI","MAI_SI",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$ISLANDCODE =="MOL"& site_data$bGEN_MHI_STRUCTURE=="SI","MOL_SI",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$ISLANDCODE =="NII"& site_data$bGEN_MHI_STRUCTURE=="SI","NII_SI",as.character(site_data$BEN_SEC))
  
  
  #Specific Changes to depth bin and reef zone
  site_data <- site_data[!(site_data$ISLAND=="Johnston" & site_data$REEF_ZONE=="Lagoon"),] 
  site_data$REEF_ZONE<-ifelse(site_data$REEF_ZONE %in% c("Protected Slope","Forereef"),"Forereef",as.character(site_data$REEF_ZONE))
  site_data$DEPTH_BIN<-ifelse(site_data$ISLAND =="Kingman" & site_data$REEF_ZONE=="Lagoon","ALL",as.character(site_data$DEPTH_BIN))
  site_data$DEPTH_BIN<-ifelse(site_data$OBS_YEAR=="2015" & site_data$ISLAND =="Rose" & site_data$REEF_ZONE=="Backreef","ALL",as.character(site_data$DEPTH_BIN))
  
  #Create Strataname
  site_data$DB_RZ<-paste(substring(site_data$REEF_ZONE,1,1), substring(site_data$DEPTH_BIN,1,1), sep="")
  site_data$STRATANAME=paste0(site_data$BEN_SEC,"_",site_data$DB_RZ)
  
  #Changing STRATA pooling structure for SAMOA
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2015"&site_data$STRATANAME %in% c("TUT_AUNUU_B_FM","TUT_AUNUU_B_FS"),"TUT_AUNUU_B_FMS",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2015"&site_data$BEN_SEC=="ROS_SANCTUARY"&site_data$DB_RZ %in% c("FM","FD"), "ROS_FMD",as.character(site_data$STRATANAME))
  
  #Changing STRATA pooling structure for PRIA
  site_data$STRATANAME<-ifelse(site_data$BEN_SEC=="Johnston"&site_data$DB_RZ %in% c("BM","BD"), "Johnston_BMD",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$BEN_SEC=="Johnston"&site_data$DB_RZ %in% c("FM","FS"), "Johnston_FMS",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$BEN_SEC=="Johnston"&site_data$REEF_ZONE =="Lagoon", "Johnston_LA",as.character(site_data$STRATANAME))
  
  #Changing STRATA pooling structure for Guam and CNMI
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2014"&site_data$STRATANAME %in% c("Aguijan_FD","Aguijan_FM"),"Aguijan_FMD",as.character(site_data$STRATANAME))
  
  #Changing STRATA pooling structure for MHI
  site_data$STRATANAME<-ifelse(site_data$STRATANAME %in% c("LAN_NORTH_FD","LAN_NORTH_FM"),"LAN_NORTH_FMD",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2016"& site_data$BEN_SEC=="NII_SI"&site_data$DB_RZ %in% c("FM","FD"), "NII_SI_FMD",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2016"& site_data$BEN_SEC=="OAH_NE"&site_data$DB_RZ %in% c("FM","FD"), "OAH_NE_FMD",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$BEN_SEC=="KAH_SO"&site_data$DB_RZ %in% c("FM","FD"), "KAH_SO_FMD",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2013"&site_data$BEN_SEC=="MAI_CM"&site_data$DB_RZ %in% c("FM","FD"), "MAI_CM_FMD",as.character(site_data$STRATANAME))
  
  site_data$STRATANAME<-ifelse(site_data$BEN_SEC=="MAI_SI", "MAI_SI_A",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2016"&site_data$BEN_SEC=="MOL_PALI", "MOL_PALI_A",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2016"&site_data$BEN_SEC=="MOL_SOUTH", "MOL_SOUTH_ALL",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2016"&site_data$BEN_SEC=="MOL_SI"&site_data$DB_RZ %in% c("FM","FD"), "MOL_SI_FMD",as.character(site_data$STRATANAME))
  
  #Changing STRATA pooling structure for NWHI
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR %in% c("2014","2015")& site_data$BEN_SEC=="Laysan"&site_data$DB_RZ %in% c("FM","FS"), "Laysan_FMS",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR %in% c("2014","2015")& site_data$BEN_SEC=="Maro"&site_data$DB_RZ %in% c("FM","FD"), "Maro_FMD",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR %in% c("2014","2015")& site_data$BEN_SEC=="Pearl & Hermes"&site_data$DB_RZ %in% c("FM","FS"), "Pearl & Hermes_FMS",as.character(site_data$STRATANAME))
  
  
  #Modify Analysis Year############CORRECT THIS IN THE FUTURE TO MAKE IT MORE STREAMLINE!!!!!!!!!!!!!!!!!
  site_data$ANALYSIS_YEAR<-site_data$OBS_YEAR
  site_data$ANALYSIS_YEAR<-ifelse(site_data$ANALYSIS_YEAR %in% c("2014","2015")& site_data$REGION=="NWHI", "2014-15",as.character(site_data$ANALYSIS_YEAR))

  #Changing Regions as requested
  site_data$REGION<-ifelse(site_data$ISLAND %in% c("Baker","Howland"),"PRIA_Phoenix",as.character(site_data$REGION))
  site_data$REGION<-ifelse(site_data$ISLAND %in% c("Kingman","Palmyra","Jarvis"),"PRIA_Line",as.character(site_data$REGION))
  site_data$REGION<-ifelse(site_data$ISLAND =="Wake","Wake",as.character(site_data$REGION))
  site_data$REGION<-ifelse(site_data$ISLAND =="Johnston","Johnston",as.character(site_data$REGION))
  
  
  return(site_data)
}


#Revising Benthic REA (demography) Sector and Strata pooling FOR PIRO PMEA 4/19 DATA REQUEST
PoolSecStrat_PIRO_noKAH=function(site_data){
  
  #Create STRATANAME by idenityfing which ANALAYSIS SCHEME you want to use then concatinating with depth and reef zone that will be used to pool data
  site_data$BEN_SEC<-site_data$SEC_NAME
  
  #Changing sector pooling SAMOA
  site_data$BEN_SEC<-ifelse(site_data$ISLAND =="Tutuila","TUT",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$OBS_YEAR=="2018"&site_data$BEN_SEC %in% c("TAU_OPEN","TAU_SANCTUARY"),"TAU",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$OBS_YEAR=="2018"&site_data$BEN_SEC %in% c("SWA_OPEN","SWA_SANCTUARY"),"SWA",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$OBS_YEAR=="2015"&site_data$BEN_SEC %in% c("TAU_OPEN","TAU_SANCTUARY"),"TAU",as.character(site_data$BEN_SEC))
  
  
  #Changing sector pooling PRIA
  site_data <- site_data[!(site_data$OBS_YEAR == "2018" & site_data$ISLAND=="Kingman" & site_data$REEF_ZONE=="Backreef"),] 
  site_data <- site_data[!(site_data$ISLAND=="Kingman" & site_data$REEF_ZONE=="Lagoon"),] 
  
  
  #Changing sector pooling structure for GUAM & CNMI
  site_data <- site_data[!(site_data$ISLAND=="Maug" & site_data$REEF_ZONE=="Lagoon"),] 
  site_data$BEN_SEC<-ifelse(site_data$SEC_NAME %in% c("GUA_ACHANG","GUA_EAST_OPEN","GUA_PATI_POINT"),"GUAEAALL",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$SEC_NAME %in% c("GUA_HARBOR","GUA_WEST_OPEN","GUA_PITI_BOMB","GUA_SASA_BAY","GUA_TUMON"),"GUAWEALL",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$SEC_NAME %in% c("Guguan", "Alamagan", "Sarigan"),"AGS",as.character(site_data$BEN_SEC))
  site_data$ISLAND<-ifelse(site_data$SEC_NAME %in% c("Guguan", "Alamagan", "Sarigan"),"AGS",as.character(site_data$ISLAND))
  
  #Changing sector pooling structure for MHI- did not combine other islands and structure together because they did not have more than 1 sector (e.g. HAW_KONA_CR is the only coral rich sector on hawaii island)
  site_data$BEN_SEC<-ifelse(site_data$ISLANDCODE =="HAW"& site_data$bGEN_MHI_STRUCTURE=="CM","HAW_CM",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$ISLANDCODE =="MAI"& site_data$bGEN_MHI_STRUCTURE=="CM","MAI_CM",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$ISLANDCODE =="MAI"& site_data$bGEN_MHI_STRUCTURE=="SI","MAI_SI",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$ISLANDCODE =="MOL"& site_data$bGEN_MHI_STRUCTURE=="SI","MOL_SI",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$ISLANDCODE =="NII"& site_data$bGEN_MHI_STRUCTURE=="SI","NII_SI",as.character(site_data$BEN_SEC))
  
  
  #Specific Changes to depth bin and reef zone
  site_data <- site_data[!(site_data$ISLAND=="Johnston" & site_data$REEF_ZONE=="Lagoon"),] 
  site_data$REEF_ZONE<-ifelse(site_data$REEF_ZONE %in% c("Protected Slope","Forereef"),"Forereef",as.character(site_data$REEF_ZONE))
  site_data$DEPTH_BIN<-ifelse(site_data$ISLAND =="Kingman" & site_data$REEF_ZONE=="Lagoon","ALL",as.character(site_data$DEPTH_BIN))
  site_data$DEPTH_BIN<-ifelse(site_data$OBS_YEAR=="2015" & site_data$ISLAND =="Rose" & site_data$REEF_ZONE=="Backreef","ALL",as.character(site_data$DEPTH_BIN))
  
  #Create Strataname
  site_data$DB_RZ<-paste(substring(site_data$REEF_ZONE,1,1), substring(site_data$DEPTH_BIN,1,1), sep="")
  site_data$STRATANAME=paste0(site_data$BEN_SEC,"_",site_data$DB_RZ)
  
  #Changing STRATA pooling structure for SAMOA
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2015"&site_data$STRATANAME %in% c("TUT_AUNUU_B_FM","TUT_AUNUU_B_FS"),"TUT_AUNUU_B_FMS",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2015"&site_data$BEN_SEC=="ROS_SANCTUARY"&site_data$DB_RZ %in% c("FM","FD"), "ROS_FMD",as.character(site_data$STRATANAME))
  
  #Changing STRATA pooling structure for PRIA
  site_data$STRATANAME<-ifelse(site_data$BEN_SEC=="Johnston"&site_data$DB_RZ %in% c("BM","BD"), "Johnston_BMD",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$BEN_SEC=="Johnston"&site_data$DB_RZ %in% c("FM","FS"), "Johnston_FMS",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$BEN_SEC=="Johnston"&site_data$REEF_ZONE =="Lagoon", "Johnston_LA",as.character(site_data$STRATANAME))
  
  #Changing STRATA pooling structure for Guam and CNMI
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2014"&site_data$STRATANAME %in% c("Aguijan_FD","Aguijan_FM"),"Aguijan_FMD",as.character(site_data$STRATANAME))
  
  #Changing STRATA pooling structure for MHI
  site_data$STRATANAME<-ifelse(site_data$STRATANAME %in% c("LAN_NORTH_FD","LAN_NORTH_FM"),"LAN_NORTH_FMD",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2016"& site_data$BEN_SEC=="NII_SI"&site_data$DB_RZ %in% c("FM","FD"), "NII_SI_FMD",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2016"& site_data$BEN_SEC=="OAH_NE"&site_data$DB_RZ %in% c("FM","FD"), "OAH_NE_FMD",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2013"&site_data$BEN_SEC=="MAI_CM"&site_data$DB_RZ %in% c("FM","FD"), "MAI_CM_FMD",as.character(site_data$STRATANAME))
  
  site_data$STRATANAME<-ifelse(site_data$BEN_SEC=="MAI_SI", "MAI_SI_A",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2016"&site_data$BEN_SEC=="MOL_PALI", "MOL_PALI_A",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2016"&site_data$BEN_SEC=="MOL_SOUTH", "MOL_SOUTH_ALL",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2016"&site_data$BEN_SEC=="MOL_SI"&site_data$DB_RZ %in% c("FM","FD"), "MOL_SI_FMD",as.character(site_data$STRATANAME))
  
  #Changing STRATA pooling structure for NWHI
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR %in% c("2014","2015")& site_data$BEN_SEC=="Laysan"&site_data$DB_RZ %in% c("FM","FS"), "Laysan_FMS",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR %in% c("2014","2015")& site_data$BEN_SEC=="Maro"&site_data$DB_RZ %in% c("FM","FD"), "Maro_FMD",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$OBS_YEAR %in% c("2014","2015")& site_data$BEN_SEC=="Pearl & Hermes"&site_data$DB_RZ %in% c("FM","FS"), "Pearl & Hermes_FMS",as.character(site_data$STRATANAME))
  
  
  #Modify Analysis Year############CORRECT THIS IN THE FUTURE TO MAKE IT MORE STREAMLINE!!!!!!!!!!!!!!!!!
  site_data$ANALYSIS_YEAR<-site_data$OBS_YEAR
  site_data$ANALYSIS_YEAR<-ifelse(site_data$ANALYSIS_YEAR %in% c("2014","2015")& site_data$REGION=="NWHI", "2014-15",as.character(site_data$ANALYSIS_YEAR))
  
  #Changing Regions as requested
  site_data$REGION<-ifelse(site_data$ISLAND %in% c("Baker","Howland"),"PRIA_Phoenix",as.character(site_data$REGION))
  site_data$REGION<-ifelse(site_data$ISLAND %in% c("Kingman","Palmyra","Jarvis"),"PRIA_Line",as.character(site_data$REGION))
  site_data$REGION<-ifelse(site_data$ISLAND =="Wake","Wake",as.character(site_data$REGION))
  site_data$REGION<-ifelse(site_data$ISLAND =="Johnston","Johnston",as.character(site_data$REGION))
  
  
  return(site_data)
}

