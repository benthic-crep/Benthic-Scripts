#D2. Benthic REA Demography_CalculateSiteLevelData_FY26

# This script will reads in the CLEANED/Analysis ready data that was generated using 
# D1. Benthic REA Demography_AddThisYearRaw_FY26
#The script does some final tweaks to the data then generates Site-level data
#These data only include surveys conducted between 2013-2024

# ---- Section 0: Prep ----
rm(list=ls())
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
# Check which packages are currently loaded
pacman::p_loaded()

#Set Run Flags
DEBUG=FALSE

#LOAD LIBRARY FUNCTIONS ...
source("../fish-paste/lib/core_functions.R")
source("./Functions/Benthic_Functions_newApp_v2025TAOfork.R")
source("./Functions/Core_Benthic_Aggregation_Functions_2025.R")

# ---- Section 1: Load Data, Final Data Tweaks ----
## LOAD benthic data
awd<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED_2013.2024.csv")
jwd<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Juveniles_raw_CLEANED_2013.2024.csv")

#If you want to test out script, subset with a single island- the entire script takes forever to run

if(DEBUG){awd=subset(awd, OBS_YEAR=="2024");jwd=subset(jwd, OBS_YEAR=="2024")}

#Final Tweaks before calculating site-level data-
#Colony fragments will be removed when you generate the site level data
#We have denoted fragments differently over the course of our dataset. This code makes sure Fragment is 0 or -1 (-1 indicates it's a fragment)

awd$Fragment<-ifelse(awd$OBS_YEAR <2018 & awd$COLONYLENGTH <5 & awd$S_ORDER=="Scleractinia",-1,awd$Fragment)
head(subset(awd,Fragment==-1& OBS_YEAR<2018)) #double check that pre 2018 fragments create
awd$Fragment[is.na(awd$Fragment)] <- 0
jwd$Fragment = 0 # you need to add this column so that you can use the site level functions correctly
awd$METHOD<-"DIVER"
jwd$METHOD<-"DIVER"

#Simplify Bleaching Severity categories: in 2019 the team decided to simplify the bleaching severity from 1-5 to 1-3 to improve consistency in severity values
#This code converts the severity data collected prior to 2019 to a 1-3 scale
awd$DATE_ <- ymd_hms(awd$DATE_)
jwd$DATE_ <- ymd_hms(jwd$DATE_)

#We simplified bleaching severity ranking from 1-5 to 1-3 on 7/11/2019. We decided to drop severity 1 because there is too much inconsistency between divers
awd_pre <- awd %>% filter(DATE_ < as.Date('2019-07-11'))
awd_post<-awd %>% filter(DATE_ >= as.Date('2019-07-11'))

awd_pre<-Convert_Severity(awd_pre,"SEVERITY_1","SEVERITY_1n")
awd_pre<-Convert_Severity(awd_pre,"SEVERITY_2","SEVERITY_2n")
#awd_pre<-Convert_Severity(awd_pre,"SEVERITY_3","SEVERITY_3n") #There were no severity measurements prior to 2020

head(awd_pre)
#View(awd_pre)

#After checking that severity numbers were changed correctly, convert back to original column names & drop original columns
awd_pre<-subset(awd_pre,select=-c(SEVERITY_1));colnames(awd_pre)[which(colnames(awd_pre) == 'SEVERITY_1n')] <- "SEVERITY_1" #change group to whatever your grouping field is.
awd_pre<-subset(awd_pre,select=-c(SEVERITY_2));colnames(awd_pre)[which(colnames(awd_pre) == 'SEVERITY_2n')] <- "SEVERITY_2" #change group to whatever your grouping field is.
#awd_pre<-subset(awd_pre,select=-c(SEVERITY_3));colnames(awd_pre)[which(colnames(awd_pre) == 'SEVERITY_3n')] <- "SEVERITY_3" #change group to whatever your grouping field is.
if(nrow(awd_pre)>0){awd_pre$SEVERITY_3<-NA}

if(DEBUG){awd_pre %>% filter(!is.na(SEVERITY_1)) %>% View()}

#Combine dataframes before and after 2019 & check that rows weren't dropped
awd.<-rbind(awd_pre,awd_post)
if(DEBUG){View(awd.)}
nrow(awd)
nrow(awd.);head(awd.)
awd<-awd.; rm("awd.") #remove temporary dataframe if all good.

#If bleaching severity is <2, change to NA- we just don't record bleaching consistently enough below severity 2
awd$CONDITION_1<-ifelse(awd$CONDITION_1 %in% c("BLP","BLE") & awd$SEVERITY_1==1,"NONE",awd$CONDITION_1);if(DEBUG){View(awd)}
awd$CONDITION_2<-ifelse(awd$CONDITION_2 %in% c("BLP","BLE") & awd$SEVERITY_2==1,"NONE",awd$CONDITION_2);if(DEBUG){View(awd)}
summary(awd$SEVERITY_3) #if you have values in severity 3 then add the code conversion


#Create a look a table of all of the colony attributes- you will need this the functions below
SURVEY_COL<-c("METHOD","DATE_","SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony<-unique(awd[,SURVEY_COL])

SURVEY_SITE<-c("METHOD","MISSIONID","DATE_","SITEVISITID", "ANALYSIS_YEAR","OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")
survey_siteAd<-unique(awd[,SURVEY_SITE])

SURVEY_SITE<-c("METHOD","MISSIONID","DATE_","SITEVISITID", "ANALYSIS_YEAR","OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")
survey_siteJ<-unique(jwd[,SURVEY_SITE])

write.csv(survey_siteAd,"surveysite.csv")

#We did juvenile only surveys in 2017 in PRIA, this will make sure the SV table has both adult and juv sites.
survey_site<-full_join(survey_siteJ,survey_siteAd,by = c("METHOD","MISSIONID","DATE_","SITEVISITID", "ANALYSIS_YEAR","OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
                                                         "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M"));nrow(survey_site)

survey_site<-survey_site[!duplicated(survey_site[,4]),]

#Remove Transect 2 from 2019-present- these were repeat surveys conducted for the SfM method comparison study
awd$TR_YEAR<-paste(awd$TRANSECT,awd$OBS_YEAR,sep="_")
jwd$TR_YEAR<-paste(jwd$TRANSECT,jwd$OBS_YEAR,sep="_")
drop.data<-c("2_2019","2_2022")

awd<-filter(awd,!TR_YEAR %in% drop.data) 
jwd<-filter(jwd,!TR_YEAR %in% drop.data) 


# ---- Section 2: Generate Transect Then Site-Level Data at Three different levels of Taxonomic Resolution (SP,TAX,GEN) ----
TAXON_FOCI=c("SPCODE","TAXONCODE","GENUS_CODE")
# Loop over taxonomic ranks
for(Ti in 1:length(TAXON_FOCI)){
  TAXON_FOCUS=TAXON_FOCI[Ti]
  #Calc_ColDen_Transect
  acd.tax25<-Calc_ColDen_Transect_25(data = awd,grouping_field = TAXON_FOCUS);
  #RENAME
  acd.tax25=acd.tax25 %>% rename(AdColCount=ColCount,
                                 AdColDen=ColDen,
                                 TRANSECTAREA_ad=TRANSECTAREA)
  jcd.tax25<-Calc_ColDen_Transect_25(jwd,TAXON_FOCUS);
  #RENAME
  jcd.tax25=jcd.tax25 %>% rename(JuvColCount=ColCount,
                                 JuvColDen=ColDen,
                                 TRANSECTAREA_j=TRANSECTAREA)
  
  #Calc_ColMetric_Transect Comparisons
  #Calculate Metrics at Transect Level
  cl.tax25<-Calc_ColMetric_Transect_25(data = awd,grouping_field = TAXON_FOCUS,pool_fields = "COLONYLENGTH"); colnames(cl.tax25)[colnames(cl.tax25)=="Ave.y"]<-"Ave.cl" #Average % old dead
  od.tax25<-Calc_ColMetric_Transect_25(data = awd,grouping_field = TAXON_FOCUS,pool_fields = "OLDDEAD"); colnames(od.tax25)[colnames(od.tax25)=="Ave.y"]<-"Ave.od" #Average % old dead
  rd.tax25<-Calc_ColMetric_Transect_25(data = awd,grouping_field = TAXON_FOCUS,pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.tax25)[colnames(rd.tax25)=="Ave.y"]<-"Ave.rd" #Average % recent dead
  
  #Calculate Disease Metrics at Transect Level
  totdzden.tax25<-Calc_TotDZden_Transect_25(awd,survey_colony,TAXON_FOCUS) # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
  totdzden.tax25=totdzden.tax25 %>% dplyr::select(all_of(c("SITEVISITID","SITE","TRANSECT",TAXON_FOCUS,"TotDZ_den")))
  
  #Calc_RDden_Transect
  rdden.tax25<-Calc_RDden_Transect_25(data = awd,survey_colony_f = survey_colony,grouping_field = TAXON_FOCUS) # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
  acutedz.tax25 = rdden.tax25 %>% dplyr::select(all_of(c("SITEVISITID","SITE","TRANSECT",TAXON_FOCUS,"DZGN_G"))) %>% rename(DZGN_den=DZGN_G)
  
  #Calc_CONDden_Transect
  condden.tax25<-Calc_CONDden_Transect_25(data = awd,survey_colony_f = survey_colony,grouping_field = TAXON_FOCUS)# Density of condition colonies by condition, you will need to subset which ever condition you want
  ble.tax25<-subset(condden.tax25,select = c("SITEVISITID","SITE","TRANSECT",TAXON_FOCUS,"BLE"));colnames(ble.tax25)[colnames(ble.tax25)=="BLE"]<-"BLE_den" #subset just bleached colonies
  chronicdz.tax25<-subset(condden.tax25,select = c("SITEVISITID","SITE","TRANSECT",TAXON_FOCUS,"CHRO"));colnames(chronicdz.tax25)[colnames(chronicdz.tax25)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies
  
  #CHANGE TRANSECT NUMBERS FOR JUVENILES (pre-2018 we used 3 and 4)
  jcd.tax25$TRANSECT[jcd.tax25$TRANSECT==3]<-1
  jcd.tax25$TRANSECT[jcd.tax25$TRANSECT==4]<-2
  
  #Remove METHOD from dataframes before merging
  acd.tax25<-subset(acd.tax25,select=-c(METHOD))
  jcd.tax25<-subset(jcd.tax25,select=-c(METHOD))
  cl.tax25<-subset(cl.tax25,select=-c(METHOD))
  od.tax25<-subset(od.tax25,select=-c(METHOD))
  rd.tax25<-subset(rd.tax25,select=-c(METHOD))
  
  #Merge density and partial mortality data together.You will need to replace the DUMMY field with the one you want
  data.tax25.=acd.tax25 %>%
    full_join(jcd.tax25,by=c("SITE","SITEVISITID","TRANSECT",TAXON_FOCUS)) %>%
    full_join(cl.tax25,by=c("SITE","SITEVISITID","TRANSECT",TAXON_FOCUS)) %>%
    full_join(od.tax25,by=c("SITE","SITEVISITID","TRANSECT",TAXON_FOCUS)) %>%
    full_join(rd.tax25,by=c("SITE","SITEVISITID","TRANSECT",TAXON_FOCUS)) %>%
    full_join(totdzden.tax25,by=c("SITE","SITEVISITID","TRANSECT",TAXON_FOCUS)) %>%
    full_join(acutedz.tax25,by=c("SITE","SITEVISITID","TRANSECT",TAXON_FOCUS)) %>%
    full_join(chronicdz.tax25,by=c("SITE","SITEVISITID","TRANSECT",TAXON_FOCUS)) %>%
    full_join(ble.tax25,by=c("SITE","SITEVISITID","TRANSECT",TAXON_FOCUS))%>%as.data.frame()
  
  #Add METHOD back in#Adata.tax25.dd METHOD back in
  data.tax25.$METHOD<-"DIVER"
  head(data.tax25.)
  
  #Change NaN to NA
  is.nan.data.frame <- function(x){do.call(cbind, lapply(x, is.nan))}
  data.tax25.[is.nan.data.frame(data.tax25.)] <- NA
  
  #There will be some NAs when you merge the juvenile and adult dataframes together because there may be some juvenile taxa that weren't observed as adults or juveniles
  #This code identifies which transects adult and juvenile colonies were recorded at and then converts NAs to 0s if needed
  ssss<-data.tax25. %>% filter(!!sym(TAXON_FOCUS)=="SSSS")
  ssss$Ad_pres<-ifelse(is.na(ssss$AdColCount),"0","-1")
  ssss$Juv_pres<-ifelse(is.na(ssss$JuvColCount),"0","-1")
  head(ssss)
  
  #Get TRANSECT level info on AJpres, and Transect Area
  ssss<-subset(ssss,select = c(SITE,SITEVISITID,TRANSECT,Ad_pres,Juv_pres,TRANSECTAREA_ad,TRANSECTAREA_j)) 
  head(ssss)
  
  #Join Area / Presence back
  data.tax25.<-left_join(subset(data.tax25.,select = -c(TRANSECTAREA_ad,TRANSECTAREA_j)),ssss) #use transect area from ssss because transectareas for some taxa were NA after merging adults and juvs
  
  #Convert NA densities/counts to 0
  data.tax25.$JuvColCount[is.na(data.tax25.$JuvColCount) & data.tax25.$Juv_pres==-1]<-0
  data.tax25.$JuvColDen[is.na(data.tax25.$JuvColDen) & data.tax25.$Juv_pres==-1]<-0
  data.tax25.$AdColCount[is.na(data.tax25.$AdColCount) & data.tax25.$Ad_pres==-1]<-0
  data.tax25.$AdColDen[is.na(data.tax25.$AdColDen) & data.tax25.$Ad_pres==-1]<-0
  
  #Calculate transect level prevalence for acute dz, chronic dz and bleaching
  data.tax25.$TotDZ_prev<-(data.tax25.$TotDZ_den*data.tax25.$TRANSECTAREA_ad)/data.tax25.$AdColCount*100
  data.tax25.$DZGN_prev<-(data.tax25.$DZGN_den*data.tax25.$TRANSECTAREA_ad)/data.tax25.$AdColCount*100
  data.tax25.$BLE_prev<-(data.tax25.$BLE_den*data.tax25.$TRANSECTAREA_ad)/data.tax25.$AdColCount*100
  data.tax25.$CHRO_prev<-(data.tax25.$CHRO_den*data.tax25.$TRANSECTAREA_ad)/data.tax25.$AdColCount*100
  #data.tax25.$ALGA_prev<-(data.tax25.$ALGA_den*data.tax25.$TRANSECTAREA_ad)/data.tax25.$AdColCount*100
  
  #There will be some NAs when you merge the DZ and other dataframes together because there may be some taxa that didn't have disease
  #Convert NA to 0 ONLY for disease density NOT for prevalence
  data.tax25.$TotDZ_den<-ifelse(is.na(data.tax25.$TotDZ_den),0,data.tax25.$TotDZ_den)
  data.tax25.$DZGN_den<-ifelse(is.na(data.tax25.$DZGN_den),0,data.tax25.$DZGN_den)
  data.tax25.$CHRO_den<-ifelse(is.na(data.tax25.$CHRO_den),0,data.tax25.$CHRO_den)
  data.tax25.$BLE_den<-ifelse(is.na(data.tax25.$BLE_den),0,data.tax25.$BLE_den)
  #data.tax25.$ALGA_den<-ifelse(is.na(data.tax25.$ALGA_den),0,data.tax25.$ALGA_den)
  
  #Remove data from transects with less than 5m surveyed for adults and 1m for juvs.
  data.tax25.$TRANSECTAREA_ad<-ifelse(data.tax25.$TRANSECTAREA_ad<5,NA,data.tax25.$TRANSECTAREA_ad)
  data.tax25.[data.tax25.$TRANSECTAREA_ad<5,]
  data.tax25.$TRANSECTAREA_j<-ifelse(data.tax25.$TRANSECTAREA_j<1,NA,data.tax25.$TRANSECTAREA_j)
  data.tax25.[data.tax25.$TRANSECTAREA_j<1,]
  
  #GENERATE SITE-LEVEL DATA BY AVERAGING TRANSECTS 
  #Since we have moved to a 1 stage design, we need to summarize the transects before rolling up to site. Dione suggested that we calculate mean of 2 transects rather than pooling or dropping a transect
  site.data.tax.<-data.tax25. %>% group_by(across(all_of(c("SITE","SITEVISITID",TAXON_FOCUS)))) %>%  #calc total colonies by condition
    summarise(
      AdColCount=mean(AdColCount,na.rm=T),AdColDen=mean(AdColDen,na.rm = T),Ave.od=mean(Ave.od,na.rm = T),
      Ave.rd=mean(Ave.rd,na.rm = T),Ave.size=mean(Ave.cl,na.rm=T),JuvColCount=mean(JuvColCount,na.rm=T),
      JuvColDen=mean(JuvColDen,na.rm=T),BLE=mean(BLE_den,na.rm=T),TotDZ=mean(TotDZ_den,na.rm=T), AcuteDZ=mean(DZGN_den,na.rm=T),ChronicDZ=mean(CHRO_den,na.rm=T),
      BLE_prev=mean(BLE_prev,na.rm=T),TotDZ_prev=mean(TotDZ_prev,na.rm=T), AcuteDZ_prev=mean(DZGN_prev,na.rm=T),ChronicDZ_prev=mean(CHRO_prev,na.rm=T)) %>% 
    as.data.frame()
  
  eval(parse(text=paste0("site.data_",TAXON_FOCUS,"=site.data.tax.")))
}



# ---- Section 3: Merge Site level data with sectors file and export site data ----
sectors<-read.csv("../fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)
sectors<-sectors[,c("REGION","ISLAND","SEC_NAME","REEF_ZONE","DEPTH_BIN","AREA_HA_2024","NH")]

#Merge together survey meta data and sector area files and check for missmatches
meta<-left_join(survey_site,sectors)
meta[which(is.na(meta$AREA_HA)),]
nrow(survey_site)
nrow(meta)


#Merge site level data and meta data
site.data_SPCODE<-left_join(site.data_SPCODE,meta)
site.data_TAXONCODE<-left_join(site.data_TAXONCODE,meta)
site.data_GENUS_CODE<-left_join(site.data_GENUS_CODE,meta)

#Final Tweaks
#Add adult and juvenile pres/ab columns
site.data_SPCODE$Adpres.abs<-ifelse(site.data_SPCODE$AdColDen>0,1,0)
site.data_SPCODE$Juvpres.abs<-ifelse(site.data_SPCODE$JuvColDen>0,1,0)
site.data_TAXONCODE$Adpres.abs<-ifelse(site.data_TAXONCODE$AdColDen>0,1,0)
site.data_TAXONCODE$Juvpres.abs<-ifelse(site.data_TAXONCODE$JuvColDen>0,1,0)
site.data_GENUS_CODE$Adpres.abs<-ifelse(site.data_GENUS_CODE$AdColDen>0,1,0)
site.data_GENUS_CODE$Juvpres.abs<-ifelse(site.data_GENUS_CODE$JuvColDen>0,1,0)

# site.data_GENUS_CODE$ALGA<-ifelse(site.data_GENUS_CODE$OBS_YEAR>= 2019,NA,site.data_GENUS_CODE$ALGA)
# site.data_GENUS_CODE$AlgalOG_prev<-ifelse(site.data_GENUS_CODE$OBS_YEAR>= 2019,NA,site.data_GENUS_CODE$AlgalOG_prev)

metaCOL=c("REGION","REGION_NAME","ISLAND","ISLANDCODE","SEC_NAME","REEF_ZONE","DEPTH_BIN","AREA_HA_2024","NH",
          "SITE","SITEVISITID","LATITUDE","LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","MISSIONID","OBS_YEAR","ANALYSIS_YEAR","DATE_")
dataCOL=c("METHOD","Adpres.abs","AdColCount","AdColDen","Ave.od","Ave.rd","Ave.size",
          "BLE","TotDZ","AcuteDZ","ChronicDZ","BLE_prev","TotDZ_prev","AcuteDZ_prev","ChronicDZ_prev",
          "Juvpres.abs","JuvColCount","JuvColDen")

  #Save Site-level data
write.csv(site.data_SPCODE[,c(metaCOL,"SPCODE",dataCOL)],
          file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_SPCODE_2013_2024.csv",row.names = F)
write.csv(site.data_TAXONCODE[,c(metaCOL,"TAXONCODE",dataCOL)],
          file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_TAXONCODE_2013_2024.csv",row.names = F)
write.csv(site.data_GENUS_CODE[,c(metaCOL,"GENUS_CODE",dataCOL)],
          file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS_2013_2024.csv",row.names = F)


