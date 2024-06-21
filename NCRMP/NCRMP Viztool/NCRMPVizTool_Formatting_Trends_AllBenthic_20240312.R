#read in Tier1 summary tables from Benthic Cover_RawtoEstimates_NCRMPViztool.R script
rm(list=ls())

# Functions ---------------------------------------------------------------
#LOAD LIBRARY FUNCTIONS ...
library(stringr)
library(tidyverse)
library(beepr)
# source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
# source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
# source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")
source("./Functions/Benthic_Functions_newApp_vTAOfork.R")
source("../fish-paste/lib/core_functions.R")
source("../fish-paste/lib/GIS_functions.R")
DEBUG_VIEW=F

# Functions ---------------------------------------------------------------


ChangeAnalysisYear_REG<-function(data){
  data <- data %>% mutate(data,
                          ANALYSIS_YEAR= case_when(
                            REGION =="MHI" & ANALYSIS_YEAR == "2013" ~ "2013-15",
                            REGION =="SAMOA" & ANALYSIS_YEAR == "2015" ~ "2015-16",
                            TRUE ~ ANALYSIS_YEAR))
  return(data$ANALYSIS_YEAR)
}
ChangeAnalysisYear_SEC<-function(data){
  data <- data %>% mutate(data,
                          ANALYSIS_YEAR= case_when(
                            REGION =="MHI" & ANALYSIS_YEAR == "2013" ~ "2013-15",
                            REGION =="SAMOA" & ANALYSIS_YEAR == "2015" ~ "2015-16",
                            SECTOR %in% c("Wake") & ANALYSIS_YEAR == "2014" ~ "2014-15",
                            SECTOR %in% c("Wake") & ANALYSIS_YEAR == "2017" ~ "2017-18",
                            SECTOR %in% c("Johnston Atoll","Baker","Howland","Jarvis","Kingman","Palmyra") & ANALYSIS_YEAR == "2015" ~ "2014-15",
                            SECTOR %in% c("Johnston Atoll","Baker","Howland","Jarvis","Kingman","Palmyra") & ANALYSIS_YEAR == "2018" ~ "2017-18",
                            TRUE ~ ANALYSIS_YEAR))
  return(data$ANALYSIS_YEAR)
}
ChangeAnalysisYear_ISL<-function(data){
  data <- data %>% mutate(data,
                          ANALYSIS_YEAR= case_when(
                            REGION =="MHI" & ANALYSIS_YEAR == "2013" ~ "2013-15",
                            REGION =="SAMOA" & ANALYSIS_YEAR == "2015" ~ "2015-16",
                            ISLAND %in% c("Wake") & ANALYSIS_YEAR == "2014" ~ "2014-15",
                            ISLAND %in% c("Wake") & ANALYSIS_YEAR == "2017" ~ "2017-18",
                            ISLAND %in% c("Johnston Atoll","Baker","Howland","Jarvis","Kingman","Palmyra") & ANALYSIS_YEAR == "2015" ~ "2014-15",
                            ISLAND %in% c("Johnston Atoll","Baker","Howland","Jarvis","Kingman","Palmyra") & ANALYSIS_YEAR == "2018" ~ "2017-18",
                            TRUE ~ ANALYSIS_YEAR))
  return(data$ANALYSIS_YEAR)
}
#Add Columns for year start and end based on analysis year
CalcStartYr<-function(data){
  data<-data %>% mutate(SurveyYearStart=recode(ANALYSIS_YEAR,
                                               `2013-15`="2013",
                                               `2014-15`="2014",
                                               `2015-16`="2015",
                                               `2017-18`="2017",
                                               `2014`="2014",
                                               `2015`="2015",
                                               `2016`="2016",
                                               `2013`="2013",
                                               `2017`="2017",
                                               `2018`="2018",
                                               `2019`="2019"))
  return(data$SurveyYearStart)
  
}

CalcEndYr<-function(data){
  data<-data %>% mutate(SurveyYearEnd=recode(ANALYSIS_YEAR,
                                             `2013-15`="2015",
                                             `2014-15`="2015",
                                             `2015-16`="2016",
                                             `2017-18`="2018",
                                             `2014`="2014",
                                             `2015`="2015",
                                             `2016`="2016",
                                             `2013`="2013",
                                             `2017`="2017",
                                             `2018`="2018",
                                             `2019`="2019"))
  return(data$SurveyYearEnd)
}

CalcNewAnalysisYear<-function(data){
  data<-data %>% mutate(ANALYSIS_YEAR_new=recode(ANALYSIS_YEAR,
                                                 `2013-15`="2013",
                                                 `2014-15`="2014",
                                                 `2015-16`="2015",
                                                 `2017-18`="2017",
                                                 `2014`="2014",
                                                 `2015`="2015",
                                                 `2016`="2016",
                                                 `2013`="2013",
                                                 `2017`="2017",
                                                 `2018`="2018",
                                                 `2019`="2019"))
  return(data$ANALYSIS_YEAR_new)
}
#Change Stata names to match AOI Naming Convention
ChangeStrataCode<-function(data){
  data %>% mutate(STRATAcode=recode(STRATA,
                                    `FD`="FRF_D",
                                    `FM`="FRF_M",
                                    `FS`="FRF_S",
                                    `BD`="BRF_D",
                                    `BM`="BRF_M",
                                    `BS`="BRF_S",
                                    `LD`="LAG_D",
                                    `LM`="LAG_M",
                                    `LS`="LAG_S",
                                    `PD`="PRS_D",
                                    `PM`="PRS_M",
                                    `PS`="PRS_S"))
}

ColumnNameChange<-function(data){
  colnames(data)[colnames(data)=="AdColDen"]<-"AdultDensity_colperm2" 
  colnames(data)[colnames(data)=="JuvColDen"]<-"JuvenileDensity_colperm2"
  colnames(data)[colnames(data)=="Mean_AdColDen"]<-"AdultDensity_colperm2" 
  colnames(data)[colnames(data)=="Mean_JuvColDen"]<-"JuvenileDensity_colperm2"
  colnames(data)[colnames(data)=="Mean_BLE_Prev"]<-"BleachingPrevalence_Pct" 
  colnames(data)[colnames(data)=="Mean_TotDZ_Prev"]<-"DiseasePrevalence_Pct" 
  colnames(data)[colnames(data)=="SE_AdColDen"]<-"AdultDensity_SE" 
  colnames(data)[colnames(data)=="SE_JuvColDen"]<-"JuvenileDensity_SE"
  colnames(data)[colnames(data)=="SE_BLE_Prev"]<-"BleachingPrevalence_SE"
  colnames(data)[colnames(data)=="SE_TotDZ_Prev"]<-"DiseasePrevalence_SE"
  
  colnames(data)[colnames(data)=="Mean.CORAL"]<-"CoralCover_Pct" 
  colnames(data)[colnames(data)=="Mean.CCA"]<-"CrustoseCoralineAlgaeCover_Pct"
  colnames(data)[colnames(data)=="Mean.MA"]<-"MacroalgaeCover_Pct" 
  colnames(data)[colnames(data)=="SE.CORAL"]<-"CoralCover_error" 
  colnames(data)[colnames(data)=="SE.CCA"]<-"CrustoseCoralineAlgaeCover_error" 
  colnames(data)[colnames(data)=="SE.MA"]<-"MacroalgaeCover_error"
  
  colnames(data)[colnames(data)=="REGION_NAME"]<-"JURISDICTIONname" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="REGION"]<-"JURISDICTIONcode" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="ISLAND"]<-"SUBJURISDICTIONname" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="ISLANDCODE"]<-"SUBJURISDICTIONcode" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="SECTOR"]<-"SECTORcode" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="SECTORNAME"]<-"SECTORname" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="ANALYSIS_YEAR_new"]<-"AnalysisYear" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="SITEVISITID"]<-"SiteID" #subset just acute diseased colonies
  data$TaxonomicResolution<-"Genus"
  colnames(data)[colnames(data)=="GENUS_CODE"]<-"TaxonomicCode" #subset just acute diseased colonies
  
  return(data)
}

QCcheck<-function(data,meta.cols.=meta.cols,data.cols.=data.cols){
  # data %>% 
  #   dplyr::select(meta.cols.) %>%
  #   summarise(across(everything(), ~ sum(is.na(.x))))
  # 
  data[,meta.cols.] <- lapply(data[,meta.cols.], as.factor)
  
  m<-sapply(data[,meta.cols.],levels) #Make sure levels of variables are correct
  d<-sapply(data[,data.cols.],summary) #make sure range of data is appropriate
  
  return(list(m,d))
}


# Data Load ---------------------------------------------------------------

#Read in DEMOGRAPHIC data
#Complete
st_demo_CO<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/COMPLETE/BenthicREA_STRATA_Demo_Viztool_2023.csv")
st_demo_CO<-st_demo_CO[,c("REGION","ISLAND", "SECTOR","DB_RZ","ANALYSIS_YEAR","n","GENUS_CODE","AdColDen","JuvColDen","Mean_BLE_Prev","Mean_TotDZ_Prev","SE_AdColDen","SE_JuvColDen","SE_BLE_Prev","SE_TotDZ_Prev")]
colnames(st_demo_CO)[colnames(st_demo_CO)=="DB_RZ"]<-"STRATA"
colnames(st_demo_CO)[colnames(st_demo_CO)=="n"]<-"N_Demo"
#Trends
st_demo_TR<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/TRENDS/BenthicREA_STRATA_TRENDS_Demo_Viztool_2023.csv")
st_demo_TR<-st_demo_TR[,c("REGION","ISLAND", "SECTOR","DB_RZ","ANALYSIS_YEAR","n","GENUS_CODE","AdColDen","JuvColDen","Mean_BLE_Prev","Mean_TotDZ_Prev","SE_AdColDen","SE_JuvColDen","SE_BLE_Prev","SE_TotDZ_Prev")]
colnames(st_demo_TR)[colnames(st_demo_TR)=="DB_RZ"]<-"STRATA"
colnames(st_demo_TR)[colnames(st_demo_TR)=="n"]<-"N_Demo"

#Complete
sec_demo_CO<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/COMPLETE/BenthicREA_SECTOR_Demo_Viztool_2023.csv")
sec_demo_CO<-sec_demo_CO[,c("REGION","PooledSector_Viztool","ANALYSIS_YEAR","n","GENUS_CODE","Mean_AdColDen","Mean_JuvColDen","Mean_BLE_Prev","Mean_TotDZ_Prev","SE_AdColDen","SE_JuvColDen","SE_BLE_Prev","SE_TotDZ_Prev")]
colnames(sec_demo_CO)[colnames(sec_demo_CO)=="PooledSector_Viztool"]<-"SECTOR"
colnames(sec_demo_CO)[colnames(sec_demo_CO)=="n"]<-"N_Demo"
#Trends
sec_demo_TR<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/TRENDS/BenthicREA_SECTOR_TRENDS_Demo_Viztool_2023.csv")
sec_demo_TR<-sec_demo_TR[,c("REGION","PooledSector_Viztool","ANALYSIS_YEAR","n","GENUS_CODE","Mean_AdColDen","Mean_JuvColDen","Mean_BLE_Prev","Mean_TotDZ_Prev","SE_AdColDen","SE_JuvColDen","SE_BLE_Prev","SE_TotDZ_Prev")]
colnames(sec_demo_TR)[colnames(sec_demo_TR)=="PooledSector_Viztool"]<-"SECTOR"
colnames(sec_demo_TR)[colnames(sec_demo_TR)=="n"]<-"N_Demo"

#Complete
isl_demo_CO<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/COMPLETE/BenthicREA_ISLAND_Demo_Viztool_2023.csv")
isl_demo_CO<-isl_demo_CO[,c("REGION","ISLAND","ANALYSIS_YEAR","n","GENUS_CODE","Mean_AdColDen","Mean_JuvColDen","Mean_BLE_Prev","Mean_TotDZ_Prev","SE_AdColDen","SE_JuvColDen","SE_BLE_Prev","SE_TotDZ_Prev")]
colnames(isl_demo_CO)[colnames(isl_demo_CO)=="n"]<-"N_Demo"
#Trends
isl_demo_TR<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/TRENDS/BenthicREA_ISLAND_TRENDS_Demo_Viztool_2023.csv")
isl_demo_TR<-isl_demo_TR[,c("REGION","ISLAND","ANALYSIS_YEAR","n","GENUS_CODE","Mean_AdColDen","Mean_JuvColDen","Mean_BLE_Prev","Mean_TotDZ_Prev","SE_AdColDen","SE_JuvColDen","SE_BLE_Prev","SE_TotDZ_Prev")]
colnames(isl_demo_TR)[colnames(isl_demo_TR)=="n"]<-"N_Demo"

#Complete
r_demo_CO<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/COMPLETE/BenthicREA_REGION_Demo_Viztool_2023.csv")
r_demo_CO<-r_demo_CO[,c("REGION","ANALYSIS_YEAR","n","GENUS_CODE","Mean_AdColDen","Mean_JuvColDen","Mean_BLE_Prev","Mean_TotDZ_Prev","SE_AdColDen","SE_JuvColDen","SE_BLE_Prev","SE_TotDZ_Prev")]
colnames(r_demo_CO)[colnames(r_demo_CO)=="n"]<-"N_Demo"
#Trends
r_demo_TR<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/TRENDS/BenthicREA_REGION_TRENDS_Demo_Viztool_2023.csv")
r_demo_TR<-r_demo_TR[,c("REGION","ANALYSIS_YEAR","n","GENUS_CODE","Mean_AdColDen","Mean_JuvColDen","Mean_BLE_Prev","Mean_TotDZ_Prev","SE_AdColDen","SE_JuvColDen","SE_BLE_Prev","SE_TotDZ_Prev")]
colnames(r_demo_TR)[colnames(r_demo_TR)=="n"]<-"N_Demo"

############ Read in COVER Data
#Read in Tier 1 (functional groups) COVER data
#Complete
st_cover_CO<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/COMPLETE/BenthicCover_2010-2023_Tier1_STRATA_Complete_Viztool.csv")
st_cover_CO<-st_cover_CO[,c("REGION","ISLAND", "ANALYSIS_SEC","STRATA","ANALYSIS_YEAR","N","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]
st_cover_CO$GENUS_CODE<-"SSSS"
colnames(st_cover_CO)[colnames(st_cover_CO)=="N"]<-"N_Cover"
colnames(st_cover_CO)[colnames(st_cover_CO)=="ANALYSIS_SEC"]<-"SECTOR"

#Trends
st_cover_TR<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/TRENDS/BenthicCover_2010-2023_Tier1_STRATA_Trends_Viztool.csv")
st_cover_TR<-st_cover_TR[,c("REGION","ISLAND", "ANALYSIS_SEC","STRATA","ANALYSIS_YEAR","N","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]
st_cover_TR$GENUS_CODE<-"SSSS"
colnames(st_cover_TR)[colnames(st_cover_TR)=="N"]<-"N_Cover"
colnames(st_cover_TR)[colnames(st_cover_TR)=="ANALYSIS_SEC"]<-"SECTOR"

#Complete
sec_cover_CO<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/COMPLETE/BenthicCover_2010-2023_Tier1_SECTOR_Complete_Viztool.csv")
sec_cover_CO<-sec_cover_CO[,c("REGION","ANALYSIS_SEC","ANALYSIS_YEAR","N","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]
sec_cover_CO$GENUS_CODE<-"SSSS"
colnames(sec_cover_CO)[colnames(sec_cover_CO)=="N"]<-"N_Cover"
colnames(sec_cover_CO)[colnames(sec_cover_CO)=="ANALYSIS_SEC"]<-"SECTOR"
#Trends
sec_cover_TR<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/TRENDS/BenthicCover_2010-2023_Tier1_SECTOR_Trends_Viztool.csv")
sec_cover_TR<-sec_cover_TR[,c("REGION","ANALYSIS_SEC","ANALYSIS_YEAR","N","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]
sec_cover_TR$GENUS_CODE<-"SSSS"
colnames(sec_cover_TR)[colnames(sec_cover_TR)=="N"]<-"N_Cover"
colnames(sec_cover_TR)[colnames(sec_cover_TR)=="ANALYSIS_SEC"]<-"SECTOR"

#Complete
isl_cover_CO<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/COMPLETE/BenthicCover_2010-2023_Tier1_ISLAND_Complete_Viztool.csv")
isl_cover_CO<-isl_cover_CO[,c("REGION","ISLAND","ANALYSIS_YEAR","N","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]
isl_cover_CO$GENUS_CODE<-"SSSS"
colnames(isl_cover_CO)[colnames(isl_cover_CO)=="N"]<-"N_Cover"
#Trends
isl_cover_TR<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/TRENDS/BenthicCover_2010-2023_Tier1_ISLAND_Trends_Viztool.csv")
isl_cover_TR<-isl_cover_TR[,c("REGION","ISLAND","ANALYSIS_YEAR","N","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]
isl_cover_TR$GENUS_CODE<-"SSSS"
colnames(isl_cover_TR)[colnames(isl_cover_TR)=="N"]<-"N_Cover"

#Complete
r_cover_CO<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/COMPLETE/BenthicCover_2010-2023_Tier1_REGION_Complete_Viztool.csv")
r_cover_CO<-r_cover_CO[,c("REGION","ANALYSIS_YEAR","N","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]
r_cover_CO$GENUS_CODE<-"SSSS"
colnames(r_cover_CO)[colnames(r_cover_CO)=="N"]<-"N_Cover"
#Trends
r_cover_TR<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/TRENDS/BenthicCover_2010-2023_Tier1_REGION_Trends_Viztool.csv")
r_cover_TR<-r_cover_TR[,c("REGION","ANALYSIS_YEAR","N","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]
r_cover_TR$GENUS_CODE<-"SSSS"
colnames(r_cover_TR)[colnames(r_cover_TR)=="N"]<-"N_Cover"


#Read in Tier 2b (Genus level) COVER data
#STRATA - Complete
st_cover2w_CO<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/COMPLETE/BenthicCover_2010-2023_Tier2b_STRATA_Complete_Viztool.csv")
#st_cover2w_CO<-dplyr::select(st_cover2w_CO,-c(Mean.TOT_AREA_WT))
#convert wide to long
st_cover2_CO<-pivot_longer(st_cover2w_CO,cols = Mean.ACAS:SE.TURS,
                           names_to = c(".value","GENUS_CODE"),
                           names_sep = "\\.")
#reorder and rename columns to match template -add MA and CCA columns so we can properly merge with tier 1 data
st_cover2_CO<-st_cover2_CO[,c("REGION","ISLAND", "ANALYSIS_SEC","STRATA","ANALYSIS_YEAR","N","GENUS_CODE","Mean","SE")]
colnames(st_cover2_CO)[colnames(st_cover2_CO)=="N"]<-"N_Cover"
colnames(st_cover2_CO)[colnames(st_cover2_CO)=="ANALYSIS_SEC"]<-"SECTOR"
colnames(st_cover2_CO)[colnames(st_cover2_CO)=="Mean"]<-"Mean.CORAL"
colnames(st_cover2_CO)[colnames(st_cover2_CO)=="SE"]<-"SE.CORAL"
st_cover2_CO$Mean.MA<-NA
st_cover2_CO$Mean.CCA<-NA
st_cover2_CO$SE.MA<-NA
st_cover2_CO$SE.CCA<-NA

#STRATA - Trends
st_cover2w_TR<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/TRENDS/BenthicCover_2010-2023_Tier2b_STRATA_Trends_Viztool.csv")
#st_cover2w_TR<-dplyr::select(st_cover2w_TR,-c(Mean.TOT_AREA_WT))
#convert wide to long
st_cover2_TR<-pivot_longer(st_cover2w_TR,cols = Mean.ACAS:SE.TURS,
                           names_to = c(".value","GENUS_CODE"),
                           names_sep = "\\.")
#reorder and rename columns to match template -add MA and CCA columns so we can properly merge with tier 1 data
st_cover2_TR<-st_cover2_TR[,c("REGION","ISLAND", "ANALYSIS_SEC","STRATA","ANALYSIS_YEAR","N","GENUS_CODE","Mean","SE")]
colnames(st_cover2_TR)[colnames(st_cover2_TR)=="N"]<-"N_Cover"
colnames(st_cover2_TR)[colnames(st_cover2_TR)=="ANALYSIS_SEC"]<-"SECTOR"
colnames(st_cover2_TR)[colnames(st_cover2_TR)=="Mean"]<-"Mean.CORAL"
colnames(st_cover2_TR)[colnames(st_cover2_TR)=="SE"]<-"SE.CORAL"
st_cover2_TR$Mean.MA<-NA
st_cover2_TR$Mean.CCA<-NA
st_cover2_TR$SE.MA<-NA
st_cover2_TR$SE.CCA<-NA



#SECTOR - Complete
sec_cover2w_CO<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/COMPLETE/BenthicCover_2010-2023_Tier2b_SECTOR_Complete_Viztool.csv")
sec_cover2w_CO<-dplyr::select(sec_cover2w_CO,-c(Mean.TOT_AREA_WT))
#convert wide to long
sec_cover2_CO<-pivot_longer(sec_cover2w_CO,cols = Mean.ACAS:SE.TURS,
                            names_to = c(".value","GENUS_CODE"),
                            names_sep = "\\.")
sec_cover2_CO<-sec_cover2_CO[,c("REGION","ANALYSIS_SEC","ANALYSIS_YEAR","N","GENUS_CODE","Mean","SE")]
colnames(sec_cover2_CO)[colnames(sec_cover2_CO)=="N"]<-"N_Cover"
colnames(sec_cover2_CO)[colnames(sec_cover2_CO)=="ANALYSIS_SEC"]<-"SECTOR"
colnames(sec_cover2_CO)[colnames(sec_cover2_CO)=="Mean"]<-"Mean.CORAL"
colnames(sec_cover2_CO)[colnames(sec_cover2_CO)=="SE"]<-"SE.CORAL"
sec_cover2_CO$Mean.MA<-NA
sec_cover2_CO$Mean.CCA<-NA
sec_cover2_CO$SE.MA<-NA
sec_cover2_CO$SE.CCA<-NA

#SECTOR - Trend
sec_cover2w_TR<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/TRENDS/BenthicCover_2010-2023_Tier2b_SECTOR_Trends_Viztool.csv")
sec_cover2w_TR<-dplyr::select(sec_cover2w_TR,-c(Mean.TOT_AREA_WT))
#convert wide to long
sec_cover2_TR<-pivot_longer(sec_cover2w_TR,cols = Mean.ACAS:SE.TURS,
                            names_to = c(".value","GENUS_CODE"),
                            names_sep = "\\.")
sec_cover2_TR<-sec_cover2_TR[,c("REGION","ANALYSIS_SEC","ANALYSIS_YEAR","N","GENUS_CODE","Mean","SE")]
colnames(sec_cover2_TR)[colnames(sec_cover2_TR)=="N"]<-"N_Cover"
colnames(sec_cover2_TR)[colnames(sec_cover2_TR)=="ANALYSIS_SEC"]<-"SECTOR"
colnames(sec_cover2_TR)[colnames(sec_cover2_TR)=="Mean"]<-"Mean.CORAL"
colnames(sec_cover2_TR)[colnames(sec_cover2_TR)=="SE"]<-"SE.CORAL"
sec_cover2_TR$Mean.MA<-NA
sec_cover2_TR$Mean.CCA<-NA
sec_cover2_TR$SE.MA<-NA
sec_cover2_TR$SE.CCA<-NA


#ISLAND - Complete
isl_cover2w_CO<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/COMPLETE/BenthicCover_2010-2023_Tier2b_ISLAND_Complete_Viztool.csv")
isl_cover2w_CO<-dplyr::select(isl_cover2w_CO,-c(Mean.TOT_AREA_WT))
isl_cover2_CO<-pivot_longer(isl_cover2w_CO,cols = Mean.ACAS:SE.TURS,
                            names_to = c(".value","GENUS_CODE"),
                            names_sep = "\\.")
isl_cover2_CO<-isl_cover2_CO[,c("REGION","ISLAND","ANALYSIS_YEAR","N","GENUS_CODE","Mean","SE")]
colnames(isl_cover2_CO)[colnames(isl_cover2_CO)=="N"]<-"N_Cover"
colnames(isl_cover2_CO)[colnames(isl_cover2_CO)=="Mean"]<-"Mean.CORAL"
colnames(isl_cover2_CO)[colnames(isl_cover2_CO)=="SE"]<-"SE.CORAL"
isl_cover2_CO$Mean.MA<-NA
isl_cover2_CO$Mean.CCA<-NA
isl_cover2_CO$SE.MA<-NA
isl_cover2_CO$SE.CCA<-NA

#ISLAND - Trend
isl_cover2w_TR<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/TRENDS/BenthicCover_2010-2023_Tier2b_ISLAND_Trends_Viztool.csv")
isl_cover2w_TR<-dplyr::select(isl_cover2w_TR,-c(Mean.TOT_AREA_WT))
isl_cover2_TR<-pivot_longer(isl_cover2w_TR,cols = Mean.ACAS:SE.TURS,
                            names_to = c(".value","GENUS_CODE"),
                            names_sep = "\\.")
isl_cover2_TR<-isl_cover2_TR[,c("REGION","ISLAND","ANALYSIS_YEAR","N","GENUS_CODE","Mean","SE")]
colnames(isl_cover2_TR)[colnames(isl_cover2_TR)=="N"]<-"N_Cover"
colnames(isl_cover2_TR)[colnames(isl_cover2_TR)=="Mean"]<-"Mean.CORAL"
colnames(isl_cover2_TR)[colnames(isl_cover2_TR)=="SE"]<-"SE.CORAL"
isl_cover2_TR$Mean.MA<-NA
isl_cover2_TR$Mean.CCA<-NA
isl_cover2_TR$SE.MA<-NA
isl_cover2_TR$SE.CCA<-NA



#REGION - Complete
r_cover2w_CO<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/COMPLETE/BenthicCover_2010-2023_Tier2b_REGION_Complete_Viztool.csv")
r_cover2w_CO<-dplyr::select(r_cover2w_CO,-c(Mean.TOT_AREA_WT))
r_cover2_CO<-pivot_longer(r_cover2w_CO,cols = Mean.ACAS:SE.TURS,
                          names_to = c(".value","GENUS_CODE"),
                          names_sep = "\\.")
r_cover2_CO<-r_cover2_CO[,c("REGION","ANALYSIS_YEAR","N","GENUS_CODE","Mean","SE")]
colnames(r_cover2_CO)[colnames(r_cover2_CO)=="N"]<-"N_Cover"
colnames(r_cover2_CO)[colnames(r_cover2_CO)=="Mean"]<-"Mean.CORAL"
colnames(r_cover2_CO)[colnames(r_cover2_CO)=="SE"]<-"SE.CORAL"
r_cover2_CO$Mean.MA<-NA
r_cover2_CO$Mean.CCA<-NA
r_cover2_CO$SE.MA<-NA
r_cover2_CO$SE.CCA<-NA
#REGION - Trends
r_cover2w_TR<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/TRENDS/BenthicCover_2010-2023_Tier2b_REGION_Trends_Viztool.csv")
r_cover2w_TR<-dplyr::select(r_cover2w_TR,-c(Mean.TOT_AREA_WT))
r_cover2_TR<-pivot_longer(r_cover2w_TR,cols = Mean.ACAS:SE.TURS,
                          names_to = c(".value","GENUS_CODE"),
                          names_sep = "\\.")
r_cover2_TR<-r_cover2_TR[,c("REGION","ANALYSIS_YEAR","N","GENUS_CODE","Mean","SE")]
colnames(r_cover2_TR)[colnames(r_cover2_TR)=="N"]<-"N_Cover"
colnames(r_cover2_TR)[colnames(r_cover2_TR)=="Mean"]<-"Mean.CORAL"
colnames(r_cover2_TR)[colnames(r_cover2_TR)=="SE"]<-"SE.CORAL"
r_cover2_TR$Mean.MA<-NA
r_cover2_TR$Mean.CCA<-NA
r_cover2_TR$SE.MA<-NA
r_cover2_TR$SE.CCA<-NA



# Combine Data ------------------------------------------------------------
# Done Loading Data, Beginning to Combine

#Combine genus-level and Tier 1 cover data: COMPLETE
st_cover_CO<-rbind(st_cover_CO,st_cover2_CO)
sec_cover_CO<-rbind(sec_cover_CO,sec_cover2_CO)
isl_cover_CO<-rbind(isl_cover_CO,isl_cover2_CO)
r_cover_CO<-rbind(r_cover_CO,r_cover2_CO)
#Combine genus-level and Tier 1 cover data: TRENDS
st_cover_TR<-rbind(st_cover_TR,st_cover2_TR)
sec_cover_TR<-rbind(sec_cover_TR,sec_cover2_TR)
isl_cover_TR<-rbind(isl_cover_TR,isl_cover2_TR)
r_cover_TR<-rbind(r_cover_TR,r_cover2_TR)

#Demo Analysis Year
st_demo_CO$ANALYSIS_YEAR<-ChangeAnalysisYear_SEC(st_demo_CO)
sec_demo_CO$ANALYSIS_YEAR<-ChangeAnalysisYear_SEC(sec_demo_CO)
isl_demo_CO$ANALYSIS_YEAR<-ChangeAnalysisYear_ISL(isl_demo_CO)
r_demo_CO$ANALYSIS_YEAR<-ChangeAnalysisYear_REG(r_demo_CO)

st_demo_TR$ANALYSIS_YEAR<-ChangeAnalysisYear_SEC(st_demo_TR)
sec_demo_TR$ANALYSIS_YEAR<-ChangeAnalysisYear_SEC(sec_demo_TR)
isl_demo_TR$ANALYSIS_YEAR<-ChangeAnalysisYear_ISL(isl_demo_TR)
r_demo_TR$ANALYSIS_YEAR<-ChangeAnalysisYear_REG(r_demo_TR)

#Cover Analysis Year
st_cover_CO$ANALYSIS_YEAR<-ChangeAnalysisYear_SEC(st_cover_CO)
sec_cover_CO$ANALYSIS_YEAR<-ChangeAnalysisYear_SEC(sec_cover_CO)
isl_cover_CO$ANALYSIS_YEAR<-ChangeAnalysisYear_ISL(isl_cover_CO)
r_cover_CO$ANALYSIS_YEAR<-ChangeAnalysisYear_REG(r_cover_CO)

st_cover_TR$ANALYSIS_YEAR<-ChangeAnalysisYear_SEC(st_cover_TR)
sec_cover_TR$ANALYSIS_YEAR<-ChangeAnalysisYear_SEC(sec_cover_TR)
isl_cover_TR$ANALYSIS_YEAR<-ChangeAnalysisYear_ISL(isl_cover_TR)
r_cover_TR$ANALYSIS_YEAR<-ChangeAnalysisYear_REG(r_cover_TR)

#Change island from SGA to Sarigan, Alamagan, Guguan
st_demo_CO <- st_demo_CO %>% mutate(st_demo_CO,
                                    ISLAND= case_when(
                                      SECTOR %in% c("Guguan", "Alamagan", "Sarigan") ~ "Sarigan, Alamagan, Guguan",
                                      TRUE ~ ISLAND))
isl_demo_CO <- isl_demo_CO %>% mutate(isl_demo_CO,
                                      ISLAND= case_when(
                                        ISLAND == "SGA" ~ "Sarigan, Alamagan, Guguan",
                                        TRUE ~ ISLAND))
st_cover_CO <- st_cover_CO %>% mutate(st_cover_CO,
                                      ISLAND= case_when(
                                        SECTOR %in% c("Guguan", "Alamagan", "Sarigan") ~ "Sarigan, Alamagan, Guguan",
                                        TRUE ~ ISLAND))
isl_cover_CO <- isl_cover_CO %>% mutate(isl_cover_CO,
                                        ISLAND= case_when(
                                          ISLAND == "SGA" ~ "Sarigan, Alamagan, Guguan",
                                          TRUE ~ ISLAND))

st_demo_TR <- st_demo_TR %>% mutate(st_demo_TR,
                                    ISLAND= case_when(
                                      SECTOR %in% c("Guguan", "Alamagan", "Sarigan") ~ "Sarigan, Alamagan, Guguan",
                                      TRUE ~ ISLAND))
isl_demo_TR <- isl_demo_TR %>% mutate(isl_demo_TR,
                                      ISLAND= case_when(
                                        ISLAND == "SGA" ~ "Sarigan, Alamagan, Guguan",
                                        TRUE ~ ISLAND))
st_cover_TR <- st_cover_TR %>% mutate(st_cover_TR,
                                      ISLAND= case_when(
                                        SECTOR %in% c("Guguan", "Alamagan", "Sarigan") ~ "Sarigan, Alamagan, Guguan",
                                        TRUE ~ ISLAND))
isl_cover_TR <- isl_cover_TR %>% mutate(isl_cover_TR,
                                        ISLAND= case_when(
                                          ISLAND == "SGA" ~ "Sarigan, Alamagan, Guguan",
                                          TRUE ~ ISLAND))



#Merge demographic with Tier 1 and genus-level cover data
st_CO<-full_join(st_demo_CO,st_cover_CO,by=c("REGION","ISLAND","SECTOR","STRATA","ANALYSIS_YEAR","GENUS_CODE"))
sec_CO<-full_join(sec_demo_CO,sec_cover_CO,by=c("REGION","SECTOR","ANALYSIS_YEAR","GENUS_CODE"))
isl_CO<-full_join(isl_demo_CO,isl_cover_CO,by=c("REGION","ISLAND","ANALYSIS_YEAR","GENUS_CODE"))
r_CO<-full_join(r_demo_CO, r_cover_CO,by=c("REGION","ANALYSIS_YEAR","GENUS_CODE"))

st_TR<-full_join(st_demo_TR,st_cover_TR,by=c("REGION","ISLAND","SECTOR","STRATA","ANALYSIS_YEAR","GENUS_CODE"))
sec_TR<-full_join(sec_demo_TR,sec_cover_TR,by=c("REGION","SECTOR","ANALYSIS_YEAR","GENUS_CODE"))
isl_TR<-full_join(isl_demo_TR,isl_cover_TR,by=c("REGION","ISLAND","ANALYSIS_YEAR","GENUS_CODE"))
r_TR<-full_join(r_demo_TR, r_cover_TR,by=c("REGION","ANALYSIS_YEAR","GENUS_CODE"))

# View(st)
# View(sec)
# View(isl)
# View(r)

### Joined All datasets, now have 8 datasets, CO,TR X ST,SEC,ISL,REG

## Care for StrataNames
#Read in look up table of all possible strata and sector names ####
seclu<-read.csv("T:/Benthic/Data/Lookup Tables/PacificNCRMP_Benthic_Sectors_Lookup_v4.csv") #Look up for pooling scheme and table of strata names
secname<-read.csv("T:/Benthic/Data/Lookup Tables/SectorNamelookup.csv") #list of all sectors (both SEC_NAME and different pooled sectors)-look up table of sector names

#Change column names to match data
colnames(secname)[colnames(secname)=="SECTOR"]<-"SECTOR" 
colnames(seclu)[colnames(seclu)=="PooledSector_Viztool"]<-"SECTOR" 
colnames(seclu)[colnames(seclu)=="STRATAcode"]<-"STRATA" 

#Change sarigan, alamagan and guguan's island name
seclu$ISLAND<-ifelse(seclu$ISLAND %in% c("Sarigan","Alamagan","Guguan"),"Sarigan, Alamagan, Guguan",seclu$ISLAND)
seclu$ISLANDCODE<-ifelse(seclu$ISLAND %in% c("Sarigan, Alamagan, Guguan"),"SGA",seclu$ISLANDCODE)

#Remove Maro, Midway and Laysan since they aren't part of the NCRMP islands
remove<-c("Maro","Midway","Laysan")
seclu<-filter(seclu,! ISLAND %in% remove)
#and drop molokini and pagopago
remove2sec<-c("TUT_PAGOPAGO","MAI_MOLOKINI")
seclu<-filter(seclu,! SEC_NAME %in% remove2sec)
#
secname <- secname %>% mutate(secname,
                              REGION=case_when(
                                ISLAND=="Guam"~"GUA",
                                REGION=="MARIAN"~"CNMI",
                                TRUE~REGION),
                              REGION_NAME= case_when(
                                ISLAND =="Guam" ~ "Guam",
                                REGION == "CNMI"~ "Commonwealth of the Northern Mariana Islands",
                                TRUE ~ REGION_NAME))
seclu <- seclu %>% mutate(seclu,
                          REGION=case_when(
                            ISLAND=="Guam"~"GUA",
                            REGION=="MARIAN"~"CNMI",
                            TRUE~REGION),
                          REGION_NAME= case_when(
                            ISLAND =="Guam" ~ "Guam",
                            REGION == "CNMI"~ "Commonwealth of the Northern Mariana Islands",
                            TRUE ~ REGION_NAME))
#Include all possible STRATA and add NA values for the strata that weren't sampled each year ####
st_lu<-unique(seclu[,c("REGION","ISLAND","SECTOR","STRATAname","STRATA")]) #make sure to use unique because we are pooling certain sectors together (e.g. Tut_aunuu)

#add Strataname
st_CO<-left_join(st_CO,st_lu,by=c("REGION","ISLAND","SECTOR","STRATA"))#Adds STRATAname 
st_TR<-left_join(st_TR,st_lu,by=c("REGION","ISLAND","SECTOR","STRATA"))#Adds STRATAname 

#Create list of all possible strata and analysis year
st_CO_lu_years <- st_lu %>% left_join(st_CO %>% distinct(REGION, ANALYSIS_YEAR,GENUS_CODE), by = c("REGION"))
st_TR_lu_years <- st_lu %>% left_join(st_TR %>% distinct(REGION, ANALYSIS_YEAR,GENUS_CODE), by = c("REGION"))

#Add rows for surveys that should have happened, but didn't
demo_st_CO<-full_join(st_CO,st_CO_lu_years) %>%
  left_join(secname,by=c("REGION","ISLAND","SECTOR")) %>%
  mutate(N_Demo = replace_na(N_Demo,0)) %>%
  mutate(N_Cover = replace_na(N_Cover,0))
#SHOLDN"T NEED THIS demo_st_CO<-filter(demo_st_CO, !ISLANDCODE %in% c("SAR","GUG","ALA"))
demo_st_TR<-full_join(st_TR,st_TR_lu_years) %>%
  left_join(secname,by=c("REGION","ISLAND","SECTOR")) %>%
  mutate(N_Demo = replace_na(N_Demo,0)) %>%
  mutate(N_Cover = replace_na(N_Cover,0))
#SHOLDN"T NEED THIS demo_st_TR<-filter(demo_st_TR, !ISLANDCODE %in% c("SAR","GUG","ALA"))

#Include all possible SECTORS and add NA values for the sectors that weren't sampled each year ####
sec_lu<-unique(seclu[,c("REGION","ISLAND","SECTOR")]) #make sure to use unique because we are pooling certain sectors together (e.g. Tut_aunuu)

sec_CO<-left_join(sec_CO,sec_lu,by=c("REGION","SECTOR")) 
sec_TR<-left_join(sec_TR,sec_lu,by=c("REGION","SECTOR")) 

#Create list of all possible strata and analysis year
sec_CO_lu_years <- sec_lu %>% left_join(sec_CO %>% distinct(REGION, ANALYSIS_YEAR,GENUS_CODE), by = c("REGION"))
sec_TR_lu_years <- sec_lu %>% left_join(sec_TR %>% distinct(REGION, ANALYSIS_YEAR,GENUS_CODE), by = c("REGION"))

demo_sec_CO<-full_join(sec_CO,sec_CO_lu_years) %>%
  left_join(secname,by=c("REGION","SECTOR","ISLAND")) %>%
  mutate(N_Demo = replace_na(N_Demo,0)) %>%
  mutate(N_Cover = replace_na(N_Cover,0))
#SHOLDN"T NEED THIS demo_sec_CO<-filter(demo_sec_CO, !ISLANDCODE %in% c("SAR","GUG","ALA"))
demo_sec_TR<-full_join(sec_TR,sec_TR_lu_years) %>%
  left_join(secname,by=c("REGION","SECTOR","ISLAND")) %>%
  mutate(N_Demo = replace_na(N_Demo,0)) %>%
  mutate(N_Cover = replace_na(N_Cover,0))
#SHOLDN"T NEED THIS demo_sec_TR<-filter(demo_sec_TR, !ISLANDCODE %in% c("SAR","GUG","ALA"))
#View(demo_sec)


#Include all possible ISLANDS and add NA values for the islands that weren't sampled each year ####
isl_lu<-unique(seclu[,c("REGION_NAME","REGION","ISLAND","ISLANDCODE")])

isl_CO<-left_join(isl_CO,isl_lu) 
isl_TR<-left_join(isl_TR,isl_lu) 

#Create list of all possible strata and analysis year
isl_CO_lu_years <- isl_lu %>% left_join(isl_CO %>% distinct(REGION, ANALYSIS_YEAR,GENUS_CODE), by = c("REGION"))
isl_TR_lu_years <- isl_lu %>% left_join(isl_TR %>% distinct(REGION, ANALYSIS_YEAR,GENUS_CODE), by = c("REGION"))

demo_isl_CO<-full_join(isl_CO,isl_CO_lu_years) %>%
  mutate(N_Demo = replace_na(N_Demo,0)) %>%
  mutate(N_Cover = replace_na(N_Cover,0))
#SHOLDN"T NEED THIS demo_isl_CO<-filter(demo_isl_CO, !ISLANDCODE %in% c("SAR","GUG","ALA"))
demo_isl_TR<-full_join(isl_TR,isl_TR_lu_years) %>%
  mutate(N_Demo = replace_na(N_Demo,0)) %>%
  mutate(N_Cover = replace_na(N_Cover,0))
#SHOLDN"T NEED THIS demo_isl_TR<-filter(demo_isl_TR, !ISLANDCODE %in% c("SAR","GUG","ALA"))
#View(demo_isl)

demo_r_CO<-r_CO %>% mutate(REGION_NAME=recode(REGION,
                                              "GUA"="Guam",
                                              "CNMI"="Commonwealth of the Northern Mariana Islands",
                                              "MHI"="Main Hawaiian Islands",
                                              "NWHI"="Northwestern Hawaiian Islands",
                                              "SAMOA"="American Samoa",
                                              "PRIAs"="Pacific Remote Island Areas"))
demo_r_TR<-r_TR %>% mutate(REGION_NAME=recode(REGION,
                                              "GUA"="Guam",
                                              "CNMI"="Commonwealth of the Northern Mariana Islands",
                                              "MHI"="Main Hawaiian Islands",
                                              "NWHI"="Northwestern Hawaiian Islands",
                                              "SAMOA"="American Samoa",
                                              "PRIAs"="Pacific Remote Island Areas"))

#Final formatting tweaks ####

#Add scientific name # Check Lookup for Viztool purposes
genlu<-read.csv("T:/Benthic/Data/Lookup Tables/Genus_lookup.csv")
genlu<-genlu[,c("SPCODE","TAXONNAME")]
colnames(genlu)[colnames(genlu)=="SPCODE"]<-"GENUS_CODE"
colnames(genlu)[colnames(genlu)=="TAXONNAME"]<-"ScientificName"

#Make sure it makes sense with taxonmic issues
#LEPT and LPHY ==> LEPT
#ALSP and GOSP and GOAL ==> GOSP
#ASTS and MONS ==> ASTS
#  N_cover column: In the strata, sector and island datasets, the genera that aren't annotated in CoralNet (e.g. Caulastrea) are listed as 0, but these taxa are listed as NA in the region datasets
genviz=unique(data.frame(GENUS_CODE_IN= sort(unique(genlu$GENUS_CODE)),
                         GENUS_CODE_OUT=sort(unique(genlu$GENUS_CODE))))
genviz[which(genviz$GENUS_CODE_IN=="MONS"),"GENUS_CODE_OUT"]="ASTS"
genviz[which(genviz$GENUS_CODE_IN=="ALSP"),"GENUS_CODE_OUT"]="GOSP"
genviz[which(genviz$GENUS_CODE_IN=="LPHY"),"GENUS_CODE_OUT"]="LEPT"
#write.csv(x = genviz,file = "T:/Benthic/Data/Lookup Tables/Genus_Viztool_Pooling.csv")


#ADDS ScientificName
demo_st_CO<-left_join(demo_st_CO,genlu,by="GENUS_CODE")
demo_sec_CO<-left_join(demo_sec_CO,genlu,by="GENUS_CODE")
demo_isl_CO<-left_join(demo_isl_CO,genlu,by="GENUS_CODE")
demo_r_CO<-left_join(demo_r_CO,genlu,by="GENUS_CODE")
demo_st_TR<-left_join(demo_st_TR,genlu,by="GENUS_CODE")
demo_sec_TR<-left_join(demo_sec_TR,genlu,by="GENUS_CODE")
demo_isl_TR<-left_join(demo_isl_TR,genlu,by="GENUS_CODE")
demo_r_TR<-left_join(demo_r_TR,genlu,by="GENUS_CODE")


#remove years before 2013
years<-c("2013","2013-15","2014-15","2014","2015","2016","2015-16","2017-18","2017","2018","2019","2022","2023")

demo_st_CO<-subset(demo_st_CO,ANALYSIS_YEAR %in% years);#View(demo_st_CO)
demo_sec_CO<-subset(demo_sec_CO,ANALYSIS_YEAR %in% years);#View(demo_sec)
demo_isl_CO<-subset(demo_isl_CO,ANALYSIS_YEAR %in% years);#View(demo_isl)
demo_r_CO<-subset(demo_r_CO,ANALYSIS_YEAR %in% years);#View(demo_r)
demo_st_TR<-subset(demo_st_TR,ANALYSIS_YEAR %in% years);#View(demo_st)
demo_sec_TR<-subset(demo_sec_TR,ANALYSIS_YEAR %in% years);#View(demo_sec)
demo_isl_TR<-subset(demo_isl_TR,ANALYSIS_YEAR %in% years);#View(demo_isl)
demo_r_TR<-subset(demo_r_TR,ANALYSIS_YEAR %in% years);#View(demo_r)

demo_st_CO$SurveyYearStart<-CalcStartYr(demo_st_CO)
demo_st_CO$SurveyYearEnd<-CalcEndYr(demo_st_CO)
demo_st_CO$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(demo_st_CO)
demo_st_TR$SurveyYearStart<-CalcStartYr(demo_st_TR)
demo_st_TR$SurveyYearEnd<-CalcEndYr(demo_st_TR)
demo_st_TR$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(demo_st_TR)

demo_sec_CO$SurveyYearStart<-CalcStartYr(demo_sec_CO)
demo_sec_CO$SurveyYearEnd<-CalcEndYr(demo_sec_CO)
demo_sec_CO$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(demo_sec_CO)
demo_sec_TR$SurveyYearStart<-CalcStartYr(demo_sec_TR)
demo_sec_TR$SurveyYearEnd<-CalcEndYr(demo_sec_TR)
demo_sec_TR$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(demo_sec_TR)

demo_isl_CO$SurveyYearStart<-CalcStartYr(demo_isl_CO)
demo_isl_CO$SurveyYearEnd<-CalcEndYr(demo_isl_CO)
demo_isl_CO$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(demo_isl_CO)
demo_isl_TR$SurveyYearStart<-CalcStartYr(demo_isl_TR)
demo_isl_TR$SurveyYearEnd<-CalcEndYr(demo_isl_TR)
demo_isl_TR$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(demo_isl_TR)

demo_r_CO$SurveyYearStart<-CalcStartYr(demo_r_CO)
demo_r_CO$SurveyYearEnd<-CalcEndYr(demo_r_CO)
demo_r_CO$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(demo_r_CO)
demo_r_TR$SurveyYearStart<-CalcStartYr(demo_r_TR)
demo_r_TR$SurveyYearEnd<-CalcEndYr(demo_r_TR)
demo_r_TR$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(demo_r_TR)

demo_st_CO<-ChangeStrataCode(demo_st_CO)
demo_st_TR<-ChangeStrataCode(demo_st_TR)

demo_st_CO<-ColumnNameChange(demo_st_CO)
demo_sec_CO<-ColumnNameChange(demo_sec_CO)
demo_isl_CO<-ColumnNameChange(demo_isl_CO)
demo_r_CO<-ColumnNameChange(demo_r_CO)
demo_st_TR<-ColumnNameChange(demo_st_TR)
demo_sec_TR<-ColumnNameChange(demo_sec_TR)
demo_isl_TR<-ColumnNameChange(demo_isl_TR)
demo_r_TR<-ColumnNameChange(demo_r_TR)


demo_st_CO<-demo_st_CO[,c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode", "SECTORname","SECTORcode","STRATAname","STRATAcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N_Demo","N_Cover",
                          "TaxonomicResolution","TaxonomicCode","ScientificName",
                          "CoralCover_Pct","CoralCover_error","CrustoseCoralineAlgaeCover_Pct","CrustoseCoralineAlgaeCover_error","MacroalgaeCover_Pct",
                          "MacroalgaeCover_error","AdultDensity_colperm2","AdultDensity_SE","JuvenileDensity_colperm2","JuvenileDensity_SE","DiseasePrevalence_Pct",
                          "DiseasePrevalence_SE","BleachingPrevalence_Pct","BleachingPrevalence_SE")]
demo_sec_CO<-demo_sec_CO[,c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode", "SECTORname","SECTORcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N_Demo","N_Cover",
                            "TaxonomicResolution","TaxonomicCode","ScientificName",
                            "CoralCover_Pct","CoralCover_error","CrustoseCoralineAlgaeCover_Pct","CrustoseCoralineAlgaeCover_error","MacroalgaeCover_Pct",
                            "MacroalgaeCover_error","AdultDensity_colperm2","AdultDensity_SE","JuvenileDensity_colperm2","JuvenileDensity_SE","DiseasePrevalence_Pct",
                            "DiseasePrevalence_SE","BleachingPrevalence_Pct","BleachingPrevalence_SE")]
demo_isl_CO<-demo_isl_CO[,c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N_Demo","N_Cover",
                            "TaxonomicResolution","TaxonomicCode","ScientificName",
                            "CoralCover_Pct","CoralCover_error","CrustoseCoralineAlgaeCover_Pct","CrustoseCoralineAlgaeCover_error","MacroalgaeCover_Pct",
                            "MacroalgaeCover_error","AdultDensity_colperm2","AdultDensity_SE","JuvenileDensity_colperm2","JuvenileDensity_SE","DiseasePrevalence_Pct",
                            "DiseasePrevalence_SE","BleachingPrevalence_Pct","BleachingPrevalence_SE")]
demo_r_CO<-demo_r_CO[,c("JURISDICTIONname","JURISDICTIONcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N_Demo","N_Cover",
                        "TaxonomicResolution","TaxonomicCode","ScientificName",
                        "CoralCover_Pct","CoralCover_error","CrustoseCoralineAlgaeCover_Pct","CrustoseCoralineAlgaeCover_error","MacroalgaeCover_Pct",
                        "MacroalgaeCover_error","AdultDensity_colperm2","AdultDensity_SE","JuvenileDensity_colperm2","JuvenileDensity_SE","DiseasePrevalence_Pct",
                        "DiseasePrevalence_SE","BleachingPrevalence_Pct","BleachingPrevalence_SE")]
demo_st_TR<-demo_st_TR[,c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode", "SECTORname","SECTORcode","STRATAname","STRATAcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N_Demo","N_Cover",
                          "TaxonomicResolution","TaxonomicCode","ScientificName",
                          "CoralCover_Pct","CoralCover_error","CrustoseCoralineAlgaeCover_Pct","CrustoseCoralineAlgaeCover_error","MacroalgaeCover_Pct",
                          "MacroalgaeCover_error","AdultDensity_colperm2","AdultDensity_SE","JuvenileDensity_colperm2","JuvenileDensity_SE","DiseasePrevalence_Pct",
                          "DiseasePrevalence_SE","BleachingPrevalence_Pct","BleachingPrevalence_SE")]
demo_sec_TR<-demo_sec_TR[,c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode", "SECTORname","SECTORcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N_Demo","N_Cover",
                            "TaxonomicResolution","TaxonomicCode","ScientificName",
                            "CoralCover_Pct","CoralCover_error","CrustoseCoralineAlgaeCover_Pct","CrustoseCoralineAlgaeCover_error","MacroalgaeCover_Pct",
                            "MacroalgaeCover_error","AdultDensity_colperm2","AdultDensity_SE","JuvenileDensity_colperm2","JuvenileDensity_SE","DiseasePrevalence_Pct",
                            "DiseasePrevalence_SE","BleachingPrevalence_Pct","BleachingPrevalence_SE")]
demo_isl_TR<-demo_isl_TR[,c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N_Demo","N_Cover",
                            "TaxonomicResolution","TaxonomicCode","ScientificName",
                            "CoralCover_Pct","CoralCover_error","CrustoseCoralineAlgaeCover_Pct","CrustoseCoralineAlgaeCover_error","MacroalgaeCover_Pct",
                            "MacroalgaeCover_error","AdultDensity_colperm2","AdultDensity_SE","JuvenileDensity_colperm2","JuvenileDensity_SE","DiseasePrevalence_Pct",
                            "DiseasePrevalence_SE","BleachingPrevalence_Pct","BleachingPrevalence_SE")]
demo_r_TR<-demo_r_TR[,c("JURISDICTIONname","JURISDICTIONcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N_Demo","N_Cover",
                        "TaxonomicResolution","TaxonomicCode","ScientificName",
                        "CoralCover_Pct","CoralCover_error","CrustoseCoralineAlgaeCover_Pct","CrustoseCoralineAlgaeCover_error","MacroalgaeCover_Pct",
                        "MacroalgaeCover_error","AdultDensity_colperm2","AdultDensity_SE","JuvenileDensity_colperm2","JuvenileDensity_SE","DiseasePrevalence_Pct",
                        "DiseasePrevalence_SE","BleachingPrevalence_Pct","BleachingPrevalence_SE")]

#Separate all hard coral taxa and functional cover into 1 set of csv files
st_all_CO<-filter(demo_st_CO,TaxonomicCode =="SSSS")
sec_all_CO<-filter(demo_sec_CO,TaxonomicCode =="SSSS")
isl_all_CO<-filter(demo_isl_CO,TaxonomicCode =="SSSS")
r_all_CO<-filter(demo_r_CO,TaxonomicCode =="SSSS")
st_all_TR<-filter(demo_st_TR,TaxonomicCode =="SSSS")
sec_all_TR<-filter(demo_sec_TR,TaxonomicCode =="SSSS")
isl_all_TR<-filter(demo_isl_TR,TaxonomicCode =="SSSS")
r_all_TR<-filter(demo_r_TR,TaxonomicCode =="SSSS")

st_all_CO[c("TaxonomicResolution","TaxonomicCode","ScientificName")]<-matrix(rep(c("Com","ALL CORALS","ALL RECORDED CORALS"),nrow(st_all_CO)),ncol=3,byrow =T) 
sec_all_CO[c("TaxonomicResolution","TaxonomicCode","ScientificName")]<-matrix(rep(c("Com","ALL CORALS","ALL RECORDED CORALS"),nrow(sec_all_CO)),ncol=3,byrow =T) 
isl_all_CO[c("TaxonomicResolution","TaxonomicCode","ScientificName")]<-matrix(rep(c("Com","ALL CORALS","ALL RECORDED CORALS"),nrow(isl_all_CO)),ncol=3,byrow =T) 
r_all_CO[c("TaxonomicResolution","TaxonomicCode","ScientificName")]<-matrix(rep(c("Com","ALL CORALS","ALL RECORDED CORALS"),nrow(r_all_CO)),ncol=3,byrow =T) 
st_all_TR[c("TaxonomicResolution","TaxonomicCode","ScientificName")]<-matrix(rep(c("Com","ALL CORALS","ALL RECORDED CORALS"),nrow(st_all_TR)),ncol=3,byrow =T) 
sec_all_TR[c("TaxonomicResolution","TaxonomicCode","ScientificName")]<-matrix(rep(c("Com","ALL CORALS","ALL RECORDED CORALS"),nrow(sec_all_TR)),ncol=3,byrow =T) 
isl_all_TR[c("TaxonomicResolution","TaxonomicCode","ScientificName")]<-matrix(rep(c("Com","ALL CORALS","ALL RECORDED CORALS"),nrow(isl_all_TR)),ncol=3,byrow =T) 
r_all_TR[c("TaxonomicResolution","TaxonomicCode","ScientificName")]<-matrix(rep(c("Com","ALL CORALS","ALL RECORDED CORALS"),nrow(r_all_TR)),ncol=3,byrow =T) 

#and genera into another set
st_gen_CO<-filter(demo_st_CO,TaxonomicCode !="SSSS")
sec_gen_CO<-filter(demo_sec_CO,TaxonomicCode !="SSSS")
isl_gen_CO<-filter(demo_isl_CO,TaxonomicCode !="SSSS")
r_gen_CO<-filter(demo_r_CO,TaxonomicCode !="SSSS")
st_gen_TR<-filter(demo_st_TR,TaxonomicCode !="SSSS")
sec_gen_TR<-filter(demo_sec_TR,TaxonomicCode !="SSSS")
isl_gen_TR<-filter(demo_isl_TR,TaxonomicCode !="SSSS")
r_gen_TR<-filter(demo_r_TR,TaxonomicCode !="SSSS")




##### SAVE STATE 4.17.2024
#save(file = "T:/Benthic/Data/Data Requests/NCRMPViztool/2023/DATA2024_READY_FOR_JOINKEY.rdata")
#save.image(file = "T:/Benthic/Data/Data Requests/NCRMPViztool/2023/DATA2024_READY_FOR_JOINKEY.rdata")
#load("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/DATA2024_READY_FOR_JOINKEY.Rdata")


#QC Checks

#strata
meta.cols<-c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode", "SECTORname","SECTORcode",
             "STRATAname","STRATAcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N_Demo","N_Cover")
data.cols<-c("CoralCover_Pct","CrustoseCoralineAlgaeCover_Pct","MacroalgaeCover_Pct","CoralCover_error","CrustoseCoralineAlgaeCover_error",
             "MacroalgaeCover_error","AdultDensity_colperm2","JuvenileDensity_colperm2","BleachingPrevalence_Pct","DiseasePrevalence_Pct","AdultDensity_SE",         
             "JuvenileDensity_SE","BleachingPrevalence_SE","DiseasePrevalence_SE")
QCcheck(data = st_all_CO)
QCcheck(data = st_all_TR)

#sector
meta.cols<-c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode", "SECTORname","SECTORcode",
             "SurveyYearStart","SurveyYearEnd","AnalysisYear","N_Demo","N_Cover")
QCcheck(sec_all_CO)
QCcheck(sec_all_TR)

#island
meta.cols<-c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode",
             "SurveyYearStart","SurveyYearEnd","AnalysisYear","N_Demo","N_Cover")
QCcheck(isl_all_CO)
QCcheck(isl_all_TR)

#region
meta.cols<-c("JURISDICTIONname","JURISDICTIONcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N_Demo","N_Cover")
QCcheck(r_all_CO)
QCcheck(r_all_TR)

####Add JoinKey
####
templ_str0=read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/For Submission/BenthicDataSummaryTable_TEMPLATE_DataTable.csv")
templ_str=read.csv("C:/Users/Thomas.Oliver/WORK/Projects/FY23/Viztool/PacBenthicSec_Trend_NJK.csv")
templ_sec=read.csv("C:/Users/Thomas.Oliver/WORK/Projects/FY23/Viztool/PacBenthicSec_Trend_NJK.csv")
templ_isl=read.csv("C:/Users/Thomas.Oliver/WORK/Projects/FY23/Viztool/PacBenthicSubjur_Trend_NJK.csv")
templ_reg=read.csv("C:/Users/Thomas.Oliver/WORK/Projects/FY23/Viztool/PacBenthicJur_Trend_NJK.csv")

# # #First off, Modify Templates for new JoinKey _StartingYear_EndingYear
# templ_str$JoinKey=paste0(substr(templ_str$JoinKey,1,nchar(templ_str$JoinKey)-4),
#                          templ_str$SurveyYearStart,"_",templ_str$SurveyYearEnd)
# templ_sec$JoinKey=paste0(substr(templ_sec$JoinKey,1,nchar(templ_sec$JoinKey)-4),
#                          templ_sec$SurveyYearStart,"_",templ_sec$SurveyYearEnd)
# templ_isl$JoinKey=paste0(substr(templ_isl$JoinKey,1,nchar(templ_isl$JoinKey)-4),
#                          templ_isl$SurveyYearStart,"_",templ_isl$SurveyYearEnd)
# templ_reg$JoinKey=paste0(substr(templ_reg$JoinKey,1,nchar(templ_reg$JoinKey)-4),
#                          templ_reg$SurveyYearStart,"_",templ_reg$SurveyYearEnd)
# 
# write.csv(x = templ_str,file = "C:/Users/Thomas.Oliver/WORK/Projects/FY23/Viztool/PacBenthicStrat_Trend_NJK.csv",row.names = F)
# write.csv(x = templ_sec,file = "C:/Users/Thomas.Oliver/WORK/Projects/FY23/Viztool/PacBenthicSec_Trend_NJK.csv",row.names = F)
# write.csv(x = templ_isl,file = "C:/Users/Thomas.Oliver/WORK/Projects/FY23/Viztool/PacBenthicSubjur_Trend_NJK.csv",row.names = F)
# write.csv(x = templ_reg,file = "C:/Users/Thomas.Oliver/WORK/Projects/FY23/Viztool/PacBenthicJur_Trend_NJK.csv",row.names = F)

#Check For Existing Dups
# reg_dupsN=table(templ_reg$JoinKey)
# reg_dups=names(reg_dupsN[reg_dupsN>1])
# isl_dupsN=table(templ_isl$JoinKey)
# isl_dups=names(isl_dupsN[isl_dupsN>1])
# sec_dupsN=table(templ_sec$JoinKey)
# sec_dups=names(sec_dupsN[sec_dupsN>1])
# str_dupsN=table(subset(templ_str,TaxonomicRes=="Com")$JoinKey)
# str_dups=data.frame(JoinKey=names(str_dupsN[str_dupsN>1]),Ndups=as.numeric(str_dupsN[str_dupsN>1]))

##################################################
#Develop Lookup Tables
##################################################
#AOI Convention Table
AOItab=read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/AOI Naming Convention_Pacific_Edited.csv")

#REGION aka jurisdiction
REGION_JKlu=sort(c(unique(AOItab$Jurisdiction.Name)))
names(REGION_JKlu)=sort(c(unique(st_all_CO$JURISDICTIONname)))

#Island aka subjurisdiction
ISLAND_JKlu=sort(unique(AOItab$Subjurisdiction.Name))
sj=sort(c(unique(st_all_CO$SUBJURISDICTIONname)))
dropLM_i=which(sj%in%c("Laysan","Midway","Maro"))
if(length(dropLM_i)>0) {sj=sj[-dropLM_i]}# Drop Laysan and Midway and Maro
names(ISLAND_JKlu)=sj

#Sector: Lehua continues to be a pain
# SEC_JKlu=sort(c(unique(templ_sec$AOI),"Maui; Hana",
#                 "Maui; Molokini","Maui; Northwest",
#                 "Tutuila; PagoPago"),na.last = T)
# names(SEC_JKlu)=sort(c(unique(st_all_CO$SECTORname)))
#Move Lehua Manually
#SEC_JKlu.df=read.csv("./NCRMP Viztool/JoinKeySectorLookup_25May2023.csv",header = F)
SEC_JKlu.df=AOItab[,c("Subjurisdiction.Name","Island.Sector.Name")] %>% arrange(Subjurisdiction.Name,Island.Sector.Name)
#SEC_JKlu.df[which(SEC_JKlu.df$Sector.Name=="Achang"),"Sector.Name"]="Marine Protected Areas"
#SEC_JKlu.df=SEC_JKlu.df[-which(SEC_JKlu.df$Sector.Name%in%c("Pati Point","Piti Bomb","Tumon Bay")),]%>% arrange(Subjurisdiction.Name,Sector.Name)
#SEC_JKlu.df=SEC_JKlu.df[-which(SEC_JKlu.df$Sector.Name%in%c("Sanctuary")),]%>% arrange(Subjurisdiction.Name,Sector.Name)
#SEC_JKlu.df[which(SEC_JKlu.df$Sector.Name%in%c("Aunuu A")),"Sector.Name"] = "Aunuu"
#SEC_JKlu.df=SEC_JKlu.df[-which(SEC_JKlu.df$Sector.Name%in%c("Aunuu B")),]%>% arrange(Subjurisdiction.Name,Sector.Name)
#SEC_JKlu.df[which(SEC_JKlu.df$Sector.Name%in%c("Fagalua")),"Sector.Name"] = "Fagalua and Fagatele"
#SEC_JKlu.df=SEC_JKlu.df[-which(SEC_JKlu.df$Sector.Name%in%c("Fagatele")),]%>% arrange(Subjurisdiction.Name,Sector.Name)

#SEC_JKlu.df[SEC_JKlu.df$Subjurisdiction.Name=="Tau","Sector.Name"]="All"
#SEC_JKlu.df[SEC_JKlu.df$Subjurisdiction.Name=="Swains","Sector.Name"]="All"

sec=unique(st_all_CO[,c("SUBJURISDICTIONname","SECTORname")]) %>% arrange(SUBJURISDICTIONname,SECTORname)
dropi=which(sec$SECTORname%in%c("Laysan","Midway","Maui_Molokini","Tutuila_PagoPago"))
if(length(dropi)>0){sec=sec[-dropi,]}

cbind(SEC_JKlu.df,sec)

#Do it
SEC_JKlu=SEC_JKlu.df$Island.Sector.Name
names(SEC_JKlu)=sec$SECTORname

#Strata (i.e. hab and depth)
STRATA_JKlu=c("Back reef, deep (18-30m)","Back reef, mid (6-18m)","Back reef, shallow (0-6m)",
              "Forereef, deep (18-30m)","Forereef, mid (6-18m)","Forereef, shallow (0-6m)",
              "Lagoon, deep (18-30m)","Lagoon, mid (6-18m)","Lagoon, shallow (0-6m)",
              "Protected slope, deep (18-30m)","Protected slope, mid (6-18m)","Protected slope, shallow (0-6m)")
names(STRATA_JKlu)=sort(unique(st_all_CO$STRATAname))

#Scientific Name
VizToolNames=sort(unique(c(templ_str$ScientificName,"Millepora sp.")))
DataSetNames=sort(unique(c(st_all_CO$ScientificName,st_gen_CO$ScientificName,"Alveopora sp.","Montastraea sp.")))
# sp.end=which(substr(DataSetNames,nchar(DataSetNames)-2,nchar(DataSetNames))==" sp")
# DataSetNames[sp.end]=paste0(DataSetNames[sp.end],".")
# DataSetNames[which(DataSetNames=="Gardineroseris")]="Gardineroseris sp."
# DataSetNames[which(DataSetNames=="Tubastraea/Balanophyllia")]="Tubastraea/Balanophyllia sp."
# DataSetNames[which(DataSetNames=="Unknown Scleractinian")]="unidentified corals"
#DataSetNames=DataSetNames[-which(DataSetNames=="Heliopora coerulea")]
setdiff(VizToolNames,DataSetNames)
setdiff(DataSetNames,VizToolNames)
data.frame(DataSetNames,VizToolNames)

SciName_JKlu=VizToolNames
names(SciName_JKlu)=DataSetNames
#View(SciName_JKlu)

#######
#Prep Strata Level Data For Join
#######
st_all_CO$JoinKey=paste0("Strata","_",
                         SEC_JKlu[st_all_CO$SECTORname],"; ",
                         STRATA_JKlu[st_all_CO$STRATAname],"_",
                         st_all_CO$TaxonomicCode,"_",
                         st_all_CO$SurveyYearStart,"_",
                         st_all_CO$SurveyYearEnd)
st_all_CO$AOILabel=paste0(SEC_JKlu[st_all_CO$SECTORname],"; ",
                          STRATA_JKlu[st_all_CO$STRATAname])
st_all_CO=st_all_CO[,c("JoinKey","AOILabel",names(st_all_CO)[!names(st_all_CO)%in%c("JoinKey","AOILabel")])]

st_all_TR$JoinKey=paste0("Strata","_",
                         SEC_JKlu[st_all_TR$SECTORname],"; ",
                         STRATA_JKlu[st_all_TR$STRATAname],"_",
                         st_all_TR$TaxonomicCode,"_",
                         st_all_TR$SurveyYearStart,"_",
                         st_all_TR$SurveyYearEnd)
st_all_TR$AOILabel=paste0(SEC_JKlu[st_all_TR$SECTORname],"; ",
                          STRATA_JKlu[st_all_TR$STRATAname])
st_all_TR=st_all_TR[,c("JoinKey","AOILabel",names(st_all_TR)[!names(st_all_TR)%in%c("JoinKey","AOILabel")])]

st_gen_CO$JoinKey=paste0("Strata","_",
                         SEC_JKlu[st_gen_CO$SECTORname],"; ",
                         STRATA_JKlu[st_gen_CO$STRATAname],"_",
                         st_gen_CO$TaxonomicCode,"_",
                         st_gen_CO$SurveyYearStart,"_",
                         st_gen_CO$SurveyYearEnd)
st_gen_CO$AOILabel=paste0(SEC_JKlu[st_gen_CO$SECTORname],"; ",
                          STRATA_JKlu[st_gen_CO$STRATAname])
st_gen_CO=st_gen_CO[,c("JoinKey","AOILabel",names(st_gen_CO)[!names(st_gen_CO)%in%c("JoinKey","AOILabel")])]

st_gen_TR$JoinKey=paste0("Strata","_",
                         SEC_JKlu[st_gen_TR$SECTORname],"; ",
                         STRATA_JKlu[st_gen_TR$STRATAname],"_",
                         st_gen_TR$TaxonomicCode,"_",
                         st_gen_TR$SurveyYearStart,"_",
                         st_gen_TR$SurveyYearEnd)
st_gen_TR$AOILabel=paste0(SEC_JKlu[st_gen_TR$SECTORname],"; ",
                          STRATA_JKlu[st_gen_TR$STRATAname])
st_gen_TR=st_gen_TR[,c("JoinKey","AOILabel",names(st_gen_TR)[!names(st_gen_TR)%in%c("JoinKey","AOILabel")])]

#Merge 'em
st_gen_CO$TaxonomicRes="Gen"
st_gen_TR$TaxonomicRes="Gen"
st_all_CO$TaxonomicRes="Com"
st_all_TR$TaxonomicRes="Com"
st_both_CO=rbind(st_gen_CO,st_all_CO)
st_both_TR=rbind(st_gen_TR,st_all_TR)

#Modify SciName
st_both_CO$ScientificName=SciName_JKlu[st_both_CO$ScientificName]
st_both_TR$ScientificName=SciName_JKlu[st_both_TR$ScientificName]

#Check Survey Years
st_both_CO$SurveyYearEnd=as.integer(st_both_CO$SurveyYearEnd)
st_both_CO$SurveyYearStart=as.integer(st_both_CO$SurveyYearStart)
st_both_CO$MonitoringCycle=as.integer(st_both_CO$AnalysisYear)

st_both_TR$SurveyYearEnd=as.integer(st_both_TR$SurveyYearEnd)
st_both_TR$SurveyYearStart=as.integer(st_both_TR$SurveyYearStart)
st_both_TR$MonitoringCycle=as.integer(st_both_TR$AnalysisYear)

# #Check Pria "ANALYSIS YEARS"
# st.both$SurveyYearEnd[which(st.both$JURISDICTIONcode=="PRIAs")]=
#   st.both$SurveyYearStart[which(st.both$JURISDICTIONcode=="PRIAs")]+1


# ###Test JoinKey Match Ups
# DataKeys_AC=unique(st_all$JoinKey)
# TempKeys_AC=templ_str %>% 
#   filter(ScientificName=="ALL RECORDED CORALS") %>% 
#   pull(JoinKey) %>% unique()
# 
# InBoth_AC=intersect(DataKeys_AC,TempKeys_AC);length(InBoth_AC)
# InData_AC=sort(setdiff(DataKeys_AC,TempKeys_AC));length(InData_AC)
# InTempl_AC=sort(setdiff(TempKeys_AC,DataKeys_AC));length(InTempl_AC)
# 
# DataKeys=unique(c(st_all$JoinKey,st_gen$JoinKey))
# TempKeys=unique(templ_str$JoinKey)
# 
# InBoth=intersect(DataKeys,unique(templ_str$JoinKey));length(InBoth)
# InData=sort(setdiff(DataKeys,unique(templ_str$JoinKey)));length(InData)
# InTempl=sort(setdiff(unique(templ_str$JoinKey),DataKeys));length(InTempl)
# 
# StrIsl=unique(substr(InData,1,regexpr(InData,pattern = ";")))

#Get Join Ready - By Modifying the Column Names to Match the Template
st_both_rn_CO= st_both_CO %>% dplyr::rename(
  N_demo=N_Demo,
  N_cover=N_Cover,
  CoralCover_SE=CoralCover_error,
  MacroalgaeCover_SE=MacroalgaeCover_error,
  CCA_Pct=CrustoseCoralineAlgaeCover_Pct,
  CCA_SE=CrustoseCoralineAlgaeCover_error)

# Drop any rows with NA/O in all N columns
st_both_rn_CO=st_both_rn_CO %>% filter(!(N_demo==0&N_cover==0))

st_both_rn_TR= st_both_TR %>% dplyr::rename(
  N_demoTREND=N_Demo,
  N_coverTREND=N_Cover,
  CoralCoverTREND_Pct=CoralCover_Pct,
  CoralCoverTREND_SE=CoralCover_error,
  MacroalgaeCoverTREND_Pct=MacroalgaeCover_Pct,
  MacroalgaeCoverTREND_SE=MacroalgaeCover_error,
  BleachingPrevalenceTREND_Pct=BleachingPrevalence_Pct,
  BleachingPrevalenceTREND_SE=BleachingPrevalence_SE,
  DiseasePrevalenceTREND_Pct=DiseasePrevalence_Pct,
  DiseasePrevalenceTREND_SE=DiseasePrevalence_SE,
  CCATREND_Pct=CrustoseCoralineAlgaeCover_Pct,
  CCATREND_SE=CrustoseCoralineAlgaeCover_error,
  AdultDensityTREND_colperm2=AdultDensity_colperm2,
  AdultDensityTREND_SE=AdultDensity_SE,
  JuvenileDensityTREND_colperm2=JuvenileDensity_colperm2,
  JuvenileDensityTREND_SE=JuvenileDensity_SE
)

# Drop any rows with NA/O in all N columns
st_both_rn_TR=st_both_rn_TR %>% filter(!(N_demoTREND==0&N_coverTREND==0))

#Final Purge of any Laysan, Midway, Maro
st_both_rn_CO=st_both_rn_CO %>% filter(!SUBJURISDICTIONname %in% c("Midway","Laysan","Maro"))
st_both_rn_TR=st_both_rn_TR %>% filter(!SUBJURISDICTIONname %in% c("Midway","Laysan","Maro"))

#Get Column Names in and Out For Join
#Output Columns
OutputCols=c(names(templ_str)[1:10],"N_demoTREND","N_coverTREND",names(templ_str)[11:38])

join_cols=c("JoinKey","AOILabel","SurveyYearStart","SurveyYearEnd","MonitoringCycle",
            "ScientificName","TaxonomicCode","TaxonomicRes")
TREND_cols=(OutputCols)[grep(pattern = "TREND",x = (OutputCols))]
COMPLETE_cols=setdiff(OutputCols,c(join_cols,TREND_cols))

# # #Note there are many Repeated Join Keys In Both Data and the Template - i.e. Strata that should be pooled, not pooled
# #Before the join
# COSSSS= st_both_rn_CO %>% filter(TaxonomicResolution=="Com")
# table(table(COSSSS$JoinKey))
# ttCO=table(COSSSS$JoinKey)
# whichrepCO=ttCO[ttCO>1]
# TRSSSS= st_both_rn_TR %>% filter(TaxonomicResolution=="Com")
# table(table(TRSSSS$JoinKey))
# ttTR=table(TRSSSS$JoinKey)
# whichrepTR=ttTR[ttTR>1]
# 
# st_both_rn_CO %>% filter(JoinKey%in%names(whichrepCO))



output_ST_Ljoin= left_join(x=st_both_rn_CO[,c(join_cols,COMPLETE_cols)],
                           y=st_both_rn_TR[,c(join_cols,TREND_cols)],
                           by=join_cols);dim(output_ST_Ljoin)
# output_ST_Fjoin= full_join(x=st_both_rn_CO[,c(join_cols,COMPLETE_cols)],
#                            y=st_both_rn_TR[,c(join_cols,TREND_cols)],
#                            by=join_cols);dim(output_ST_Fjoin)
output_ST_Ljoin=output_ST_Ljoin[,OutputCols]
#output_ST_Fjoin=output_ST_Fjoin[,OutputCols]


write.csv(x = output_ST_Ljoin,row.names = FALSE,
          file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/For Submission/March 2024 Submission/PacificBenthicDataSummaryTable_STRATA_LEFTJOIN_byGenus_03May2024.csv")
# write.csv(x = output_ST_Fjoin,row.names = FALSE,
#           file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/For Submission/March 2024 Submission/PacificBenthicDataSummaryTable_STRATA_FULLJOIN_byGenus_19April2024.csv")
beep()

#######
#Prep Sector Level Data For Join
#######
sec_all_CO$JoinKey=paste0("Sector","_",
                          SEC_JKlu[sec_all_CO$SECTORname],"_",
                          sec_all_CO$TaxonomicCode,"_",
                          sec_all_CO$SurveyYearStart,"_",
                          sec_all_CO$SurveyYearEnd)
sec_all_CO$AOILabel=paste0(SEC_JKlu[sec_all_CO$SECTORname])

sec_all_CO=sec_all_CO[,c("JoinKey","AOILabel",names(sec_all_CO)[!names(sec_all_CO)%in%c("JoinKey","AOILabel")])]

sec_all_TR$JoinKey=paste0("Sector","_",
                          SEC_JKlu[sec_all_TR$SECTORname],"_",
                          sec_all_TR$TaxonomicCode,"_",
                          sec_all_TR$SurveyYearStart,"_",
                          sec_all_TR$SurveyYearEnd)
sec_all_TR$AOILabel=paste0(SEC_JKlu[sec_all_TR$SECTORname])

sec_all_TR=sec_all_TR[,c("JoinKey","AOILabel",names(sec_all_TR)[!names(sec_all_TR)%in%c("JoinKey","AOILabel")])]

sec_gen_CO$JoinKey=paste0("Sector","_",
                          SEC_JKlu[sec_gen_CO$SECTORname],"_",
                          sec_gen_CO$TaxonomicCode,"_",
                          sec_gen_CO$SurveyYearStart,"_",
                          sec_gen_CO$SurveyYearEnd)
sec_gen_CO$AOILabel=paste0(SEC_JKlu[sec_gen_CO$SECTORname])
sec_gen_CO=sec_gen_CO[,c("JoinKey","AOILabel",names(sec_gen_CO)[!names(sec_gen_CO)%in%c("JoinKey","AOILabel")])]

sec_gen_TR$JoinKey=paste0("Sector","_",
                          SEC_JKlu[sec_gen_TR$SECTORname],"_",
                          sec_gen_TR$TaxonomicCode,"_",
                          sec_gen_TR$SurveyYearStart,"_",
                          sec_gen_TR$SurveyYearEnd)
sec_gen_TR$AOILabel=paste0(SEC_JKlu[sec_gen_TR$SECTORname])
sec_gen_TR=sec_gen_TR[,c("JoinKey","AOILabel",names(sec_gen_TR)[!names(sec_gen_TR)%in%c("JoinKey","AOILabel")])]

#Merge 'em
sec_gen_CO$TaxonomicRes="Gen"
sec_all_CO$TaxonomicRes="Com"
sec_both_CO=rbind(sec_gen_CO,sec_all_CO)

sec_gen_TR$TaxonomicRes="Gen"
sec_all_TR$TaxonomicRes="Com"
sec_both_TR=rbind(sec_gen_TR,sec_all_TR)

#Modify SciName
sec_both_CO$ScientificName=SciName_JKlu[sec_both_CO$ScientificName]
sec_both_TR$ScientificName=SciName_JKlu[sec_both_TR$ScientificName]

#Check Survey Years
sec_both_CO$SurveyYearEnd=as.integer(sec_both_CO$SurveyYearEnd)
sec_both_CO$SurveyYearStart=as.integer(sec_both_CO$SurveyYearStart)
sec_both_CO$MonitoringCycle=as.integer(sec_both_CO$AnalysisYear)

sec_both_TR$SurveyYearEnd=as.integer(sec_both_TR$SurveyYearEnd)
sec_both_TR$SurveyYearStart=as.integer(sec_both_TR$SurveyYearStart)
sec_both_TR$MonitoringCycle=as.integer(sec_both_TR$AnalysisYear)


#Get Join Ready - By Modifying the Column Names to Match the Template
sec_both_rn_CO= sec_both_CO %>% dplyr::rename(
  N_demo=N_Demo,
  N_cover=N_Cover,
  CoralCover_SE=CoralCover_error,
  MacroalgaeCover_SE=MacroalgaeCover_error,
  CCA_Pct=CrustoseCoralineAlgaeCover_Pct,
  CCA_SE=CrustoseCoralineAlgaeCover_error)
# Drop any rows with NA/O in all N columns
sec_both_rn_CO=sec_both_rn_CO %>% filter(!(N_demo==0&N_cover==0))

sec_both_rn_TR= sec_both_TR %>% dplyr::rename(
  N_demoTREND=N_Demo,
  N_coverTREND=N_Cover,
  CoralCoverTREND_Pct=CoralCover_Pct,
  CoralCoverTREND_SE=CoralCover_error,
  MacroalgaeCoverTREND_Pct=MacroalgaeCover_Pct,
  MacroalgaeCoverTREND_SE=MacroalgaeCover_error,
  BleachingPrevalenceTREND_Pct=BleachingPrevalence_Pct,
  BleachingPrevalenceTREND_SE=BleachingPrevalence_SE,
  DiseasePrevalenceTREND_Pct=DiseasePrevalence_Pct,
  DiseasePrevalenceTREND_SE=DiseasePrevalence_SE,
  CCATREND_Pct=CrustoseCoralineAlgaeCover_Pct,
  CCATREND_SE=CrustoseCoralineAlgaeCover_error,
  AdultDensityTREND_colperm2=AdultDensity_colperm2,
  AdultDensityTREND_SE=AdultDensity_SE,
  JuvenileDensityTREND_colperm2=JuvenileDensity_colperm2,
  JuvenileDensityTREND_SE=JuvenileDensity_SE
)

# Drop any rows with NA/O in all N columns
sec_both_rn_TR=sec_both_rn_TR %>% filter(!(N_demoTREND==0&N_coverTREND==0))

#Final Purge of any Laysan, Midway, Maro
sec_both_rn_CO=sec_both_rn_CO %>% filter(!SUBJURISDICTIONname %in% c("Midway","Laysan","Maro"))
sec_both_rn_TR=sec_both_rn_TR %>% filter(!SUBJURISDICTIONname %in% c("Midway","Laysan","Maro"))


# # #Note there are many Repeated Join Keys In Both Data and the Template - i.e. Strata that should be pooled, not pooled
# #Before the join
COSSSS= sec_both_rn_CO %>% filter(TaxonomicResolution=="Com")
table(table(COSSSS$JoinKey))
ttCO=table(COSSSS$JoinKey)
whichrepCO=ttCO[ttCO>1]
TRSSSS= sec_both_rn_TR %>% filter(TaxonomicResolution=="Com")
table(table(TRSSSS$JoinKey))
ttTR=table(TRSSSS$JoinKey)
whichrepTR=ttTR[ttTR>1]

sec_both_rn_CO %>% filter(JoinKey%in%names(whichrepCO))


output_SEC_Ljoin= left_join(x=sec_both_rn_CO[,c(join_cols,COMPLETE_cols)],
                            y=sec_both_rn_TR[,c(join_cols,TREND_cols)],
                            by=join_cols);dim(output_SEC_Ljoin)
# output_SEC_Fjoin= full_join(x=sec_both_rn_CO[,c(join_cols,COMPLETE_cols)],
#                             y=sec_both_rn_TR[,c(join_cols,TREND_cols)],
#                             by=join_cols);dim(output_SEC_Fjoin)
output_SEC_Ljoin=output_SEC_Ljoin[,OutputCols]
# output_SEC_Fjoin=output_SEC_Fjoin[,OutputCols]
write.csv(x = output_SEC_Ljoin,row.names = FALSE,
          file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/For Submission/March 2024 Submission/PacificBenthicDataSummaryTable_SECTOR_LEFTJOIN_byGenus_03May2024.csv")
# write.csv(x = output_SEC_Fjoin,row.names = FALSE,
#           file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/For Submission/March 2024 Submission/PacificBenthicDataSummaryTable_SECTOR_FULLJOIN_byGenus_19April2024.csv")
# 
beep()
#######
#Prep Island Level Data For Join
#######
isl_all_CO$JoinKey=paste0("Subjurisdiction","_",
                          ISLAND_JKlu[isl_all_CO$SUBJURISDICTIONname],"_",
                          isl_all_CO$TaxonomicCode,"_",
                          isl_all_CO$SurveyYearStart,"_",
                          isl_all_CO$SurveyYearEnd)

isl_all_CO$AOILabel=paste0(ISLAND_JKlu[isl_all_CO$SUBJURISDICTIONname])

isl_all_CO=isl_all_CO[,c("JoinKey","AOILabel",names(isl_all_CO)[!names(isl_all_CO)%in%c("JoinKey","AOILabel")])]

isl_gen_CO$JoinKey=paste0("Subjurisdiction","_",
                          ISLAND_JKlu[isl_gen_CO$SUBJURISDICTIONname],"_",
                          isl_gen_CO$TaxonomicCode,"_",
                          isl_gen_CO$SurveyYearStart,"_",
                          isl_gen_CO$SurveyYearEnd)
isl_gen_CO$AOILabel=paste0(ISLAND_JKlu[isl_gen_CO$SUBJURISDICTIONname])
isl_gen_CO=isl_gen_CO[,c("JoinKey","AOILabel",names(isl_gen_CO)[!names(isl_gen_CO)%in%c("JoinKey","AOILabel")])]

#Merge 'em
isl_gen_CO$TaxonomicRes="Gen"
isl_all_CO$TaxonomicRes="Com"
isl_both_CO=rbind(isl_gen_CO,isl_all_CO)

#Modify SciName
isl_both_CO$ScientificName=SciName_JKlu[isl_both_CO$ScientificName]

#Check Survey Years
isl_both_CO$SurveyYearEnd=as.integer(isl_both_CO$SurveyYearEnd)
isl_both_CO$SurveyYearStart=as.integer(isl_both_CO$SurveyYearStart)
isl_both_CO$MonitoringCycle=as.integer(isl_both_CO$AnalysisYear)

isl_all_TR$JoinKey=paste0("Subjurisdiction","_",
                          ISLAND_JKlu[isl_all_TR$SUBJURISDICTIONname],"_",
                          isl_all_TR$TaxonomicCode,"_",
                          isl_all_TR$SurveyYearStart,"_",
                          isl_all_TR$SurveyYearEnd)

isl_all_TR$AOILabel=paste0(ISLAND_JKlu[isl_all_TR$SUBJURISDICTIONname])

isl_all_TR=isl_all_TR[,c("JoinKey","AOILabel",names(isl_all_TR)[!names(isl_all_TR)%in%c("JoinKey","AOILabel")])]

isl_gen_TR$JoinKey=paste0("Subjurisdiction","_",
                          ISLAND_JKlu[isl_gen_TR$SUBJURISDICTIONname],"_",
                          isl_gen_TR$TaxonomicCode,"_",
                          isl_gen_TR$SurveyYearStart,"_",
                          isl_gen_TR$SurveyYearEnd)
isl_gen_TR$AOILabel=paste0(ISLAND_JKlu[isl_gen_TR$SUBJURISDICTIONname])
isl_gen_TR=isl_gen_TR[,c("JoinKey","AOILabel",names(isl_gen_TR)[!names(isl_gen_TR)%in%c("JoinKey","AOILabel")])]

#Merge 'em
isl_gen_TR$TaxonomicRes="Gen"
isl_all_TR$TaxonomicRes="Com"
isl_both_TR=rbind(isl_gen_TR,isl_all_TR)

#Modify SciName
isl_both_TR$ScientificName=SciName_JKlu[isl_both_TR$ScientificName]

#Check Survey Years
isl_both_TR$SurveyYearEnd=as.integer(isl_both_TR$SurveyYearEnd)
isl_both_TR$SurveyYearStart=as.integer(isl_both_TR$SurveyYearStart)
isl_both_TR$MonitoringCycle=as.integer(isl_both_TR$AnalysisYear)

#Get Join Ready - By Modifying the Column Names to Match the Template
isl_both_rn_CO= isl_both_CO %>% dplyr::rename(
  N_demo=N_Demo,
  N_cover=N_Cover,
  CoralCover_SE=CoralCover_error,
  MacroalgaeCover_SE=MacroalgaeCover_error,
  CCA_Pct=CrustoseCoralineAlgaeCover_Pct,
  CCA_SE=CrustoseCoralineAlgaeCover_error)
# Drop any rows with NA/O in all N columns
isl_both_rn_CO=isl_both_rn_CO %>% filter(!(N_demo==0&N_cover==0))

isl_both_rn_TR= isl_both_TR %>% dplyr::rename(
  N_demoTREND=N_Demo,
  N_coverTREND=N_Cover,
  CoralCoverTREND_Pct=CoralCover_Pct,
  CoralCoverTREND_SE=CoralCover_error,
  MacroalgaeCoverTREND_Pct=MacroalgaeCover_Pct,
  MacroalgaeCoverTREND_SE=MacroalgaeCover_error,
  BleachingPrevalenceTREND_Pct=BleachingPrevalence_Pct,
  BleachingPrevalenceTREND_SE=BleachingPrevalence_SE,
  DiseasePrevalenceTREND_Pct=DiseasePrevalence_Pct,
  DiseasePrevalenceTREND_SE=DiseasePrevalence_SE,
  CCATREND_Pct=CrustoseCoralineAlgaeCover_Pct,
  CCATREND_SE=CrustoseCoralineAlgaeCover_error,
  AdultDensityTREND_colperm2=AdultDensity_colperm2,
  AdultDensityTREND_SE=AdultDensity_SE,
  JuvenileDensityTREND_colperm2=JuvenileDensity_colperm2,
  JuvenileDensityTREND_SE=JuvenileDensity_SE
)

# Drop any rows with NA/O in all N columns
isl_both_rn_TR=isl_both_rn_TR %>% filter(!(N_demoTREND==0&N_coverTREND==0))

#Final Purge of any Laysan, Midway, Maro
isl_both_rn_CO=isl_both_rn_CO %>% filter(!SUBJURISDICTIONname %in% c("Midway","Laysan","Maro"))
isl_both_rn_TR=isl_both_rn_TR %>% filter(!SUBJURISDICTIONname %in% c("Midway","Laysan","Maro"))


# # #Note there are many Repeated Join Keys In Both Data and the Template - i.e. Strata that should be pooled, not pooled
# #Before the join
COSSSS= isl_both_rn_CO %>% filter(TaxonomicResolution=="Com")
table(table(COSSSS$JoinKey))
ttCO=table(COSSSS$JoinKey)
whichrepCO=ttCO[ttCO>1]
TRSSSS= isl_both_rn_TR %>% filter(TaxonomicResolution=="Com")
table(table(TRSSSS$JoinKey))
ttTR=table(TRSSSS$JoinKey)
whichrepTR=ttTR[ttTR>1]

isl_both_rn_CO %>% filter(JoinKey%in%names(whichrepCO))


output_isl_Ljoin= left_join(x=isl_both_rn_CO[,c(join_cols,COMPLETE_cols)],
                            y=isl_both_rn_TR[,c(join_cols,TREND_cols)],
                            by=join_cols);dim(output_isl_Ljoin)
# output_isl_Fjoin= full_join(x=isl_both_rn_CO[,c(join_cols,COMPLETE_cols)],
#                             y=isl_both_rn_TR[,c(join_cols,TREND_cols)],
#                             by=join_cols);dim(output_isl_Fjoin)
output_isl_Ljoin=output_isl_Ljoin[,OutputCols]
# output_isl_Fjoin=output_isl_Fjoin[,OutputCols]
write.csv(x = output_isl_Ljoin,row.names = FALSE,
          file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/For Submission/March 2024 Submission/PacificBenthicDataSummaryTable_ISLAND_LEFTJOIN_byGenus_03May2024.csv")
# write.csv(x = output_isl_Fjoin,row.names = FALSE,
#           file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/For Submission/March 2024 Submission/PacificBenthicDataSummaryTable_ISLAND_FULLJOIN_byGenus_19April2024.csv")

#######
#Prep Region Level Data For Join
#######
r_all_CO$JoinKey=paste0("Jurisdiction","_",
                        REGION_JKlu[r_all_CO$JURISDICTIONname],"_",
                        r_all_CO$TaxonomicCode,"_",
                        r_all_CO$SurveyYearStart,"_",
                        r_all_CO$SurveyYearEnd)
r_all_CO$AOILabel=paste0(REGION_JKlu[r_all_CO$JURISDICTIONname])

r_all_CO=r_all_CO[,c("JoinKey","AOILabel",names(r_all_CO)[!names(r_all_CO)%in%c("JoinKey","AOILabel")])]

r_gen_CO$JoinKey=paste0("Jurisdiction","_",
                        REGION_JKlu[r_gen_CO$JURISDICTIONname],"_",
                        r_gen_CO$TaxonomicCode,"_",
                        r_gen_CO$SurveyYearStart,"_",
                        r_gen_CO$SurveyYearEnd)
r_gen_CO$AOILabel=paste0(REGION_JKlu[r_gen_CO$JURISDICTIONname])
r_gen_CO=r_gen_CO[,c("JoinKey","AOILabel",names(r_gen_CO)[!names(r_gen_CO)%in%c("JoinKey","AOILabel")])]

#Merge 'em
r_gen_CO$TaxonomicRes="Gen"
r_all_CO$TaxonomicRes="Com"
r_both_CO=rbind(r_gen_CO,r_all_CO)

#Modify SciName
r_both_CO$ScientificName=SciName_JKlu[r_both_CO$ScientificName]

#Check Survey Years
r_both_CO$SurveyYearEnd=as.integer(r_both_CO$SurveyYearEnd)
r_both_CO$SurveyYearStart=as.integer(r_both_CO$SurveyYearStart)
r_both_CO$MonitoringCycle=as.integer(r_both_CO$AnalysisYear)

##
r_all_TR$JoinKey=paste0("Jurisdiction","_",
                        REGION_JKlu[r_all_TR$JURISDICTIONname],"_",
                        r_all_TR$TaxonomicCode,"_",
                        r_all_TR$SurveyYearStart,"_",
                        r_all_TR$SurveyYearEnd)
r_all_TR$AOILabel=paste0(REGION_JKlu[r_all_TR$JURISDICTIONname])

r_all_TR=r_all_TR[,c("JoinKey","AOILabel",names(r_all_TR)[!names(r_all_TR)%in%c("JoinKey","AOILabel")])]

r_gen_TR$JoinKey=paste0("Jurisdiction","_",
                        REGION_JKlu[r_gen_TR$JURISDICTIONname],"_",
                        r_gen_TR$TaxonomicCode,"_",
                        r_gen_TR$SurveyYearStart,"_",
                        r_gen_TR$SurveyYearEnd)
r_gen_TR$AOILabel=paste0(REGION_JKlu[r_gen_TR$JURISDICTIONname])
r_gen_TR=r_gen_TR[,c("JoinKey","AOILabel",names(r_gen_TR)[!names(r_gen_TR)%in%c("JoinKey","AOILabel")])]

#Merge 'em
r_gen_TR$TaxonomicRes="Gen"
r_all_TR$TaxonomicRes="Com"
r_both_TR=rbind(r_gen_TR,r_all_TR)

#Modify SciName
r_both_TR$ScientificName=SciName_JKlu[r_both_TR$ScientificName]

#Check Survey Years
r_both_TR$SurveyYearEnd=as.integer(r_both_TR$SurveyYearEnd)
r_both_TR$SurveyYearStart=as.integer(r_both_TR$SurveyYearStart)
r_both_TR$MonitoringCycle=as.integer(r_both_TR$AnalysisYear)


#Get Join Ready - By Modifying the Column Names to Match the Template
r_both_rn_CO= r_both_CO %>% dplyr::rename(
  N_demo=N_Demo,
  N_cover=N_Cover,
  CoralCover_SE=CoralCover_error,
  MacroalgaeCover_SE=MacroalgaeCover_error,
  CCA_Pct=CrustoseCoralineAlgaeCover_Pct,
  CCA_SE=CrustoseCoralineAlgaeCover_error)
# Drop any rows with NA/O in all N columns
r_both_rn_CO=r_both_rn_CO %>% filter(!(N_demo==0&N_cover==0))

r_both_rn_TR= r_both_TR %>% dplyr::rename(
  N_demoTREND=N_Demo,
  N_coverTREND=N_Cover,
  CoralCoverTREND_Pct=CoralCover_Pct,
  CoralCoverTREND_SE=CoralCover_error,
  MacroalgaeCoverTREND_Pct=MacroalgaeCover_Pct,
  MacroalgaeCoverTREND_SE=MacroalgaeCover_error,
  BleachingPrevalenceTREND_Pct=BleachingPrevalence_Pct,
  BleachingPrevalenceTREND_SE=BleachingPrevalence_SE,
  DiseasePrevalenceTREND_Pct=DiseasePrevalence_Pct,
  DiseasePrevalenceTREND_SE=DiseasePrevalence_SE,
  CCATREND_Pct=CrustoseCoralineAlgaeCover_Pct,
  CCATREND_SE=CrustoseCoralineAlgaeCover_error,
  AdultDensityTREND_colperm2=AdultDensity_colperm2,
  AdultDensityTREND_SE=AdultDensity_SE,
  JuvenileDensityTREND_colperm2=JuvenileDensity_colperm2,
  JuvenileDensityTREND_SE=JuvenileDensity_SE
)

# Drop any rows with NA/O in all N columns
r_both_rn_TR=r_both_rn_TR %>% filter(!(N_demoTREND==0&N_coverTREND==0))


# # #Note there are many Repeated Join Keys In Both Data and the Template - i.e. Strata that should be pooled, not pooled
# #Before the join
COSSSS= r_both_rn_CO %>% filter(TaxonomicResolution=="Com")
table(table(COSSSS$JoinKey))
ttCO=table(COSSSS$JoinKey)
whichrepCO=ttCO[ttCO>1]
TRSSSS= r_both_rn_TR %>% filter(TaxonomicResolution=="Com")
table(table(TRSSSS$JoinKey))
ttTR=table(TRSSSS$JoinKey)
whichrepTR=ttTR[ttTR>1]

r_both_rn_CO %>% filter(JoinKey%in%names(whichrepCO))


output_r_Ljoin= left_join(x=r_both_rn_CO[,c(join_cols,COMPLETE_cols)],
                          y=r_both_rn_TR[,c(join_cols,TREND_cols)],
                          by=join_cols);dim(output_r_Ljoin)
# output_r_Fjoin= full_join(x=r_both_rn_CO[,c(join_cols,COMPLETE_cols)],
#                           y=r_both_rn_TR[,c(join_cols,TREND_cols)],
#                           by=join_cols);dim(output_r_Fjoin)
output_r_Ljoin=output_r_Ljoin[,OutputCols]
# output_r_Fjoin=output_r_Fjoin[,OutputCols]
write.csv(x = output_r_Ljoin,row.names = FALSE,
          file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/For Submission/March 2024 Submission/PacificBenthicDataSummaryTable_REGION_LEFTJOIN_byGenus_03May2024.csv")
# write.csv(x = output_r_Fjoin,row.names = FALSE,
#           file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/For Submission/March 2024 Submission/PacificBenthicDataSummaryTable_REGION_FULLJOIN_byGenus_19April2024.csv")
beep()

##############################################################################################################################################################################################
##############################################################################################################################################################################################
#Run final join key filter...

# 
# 
# #Purge Join Keys from old data
# #things in A not in B
# # setdiff(c(1,2,3,4,5),c(3,4,5,6,7))
# # setdiff(c(3,4,5,6,7),c(1,2,3,4,5))
# 
# #Pull taxon out of the join key  -- REGION
# templ_reg$JoinKey_SSSS=templ_reg$JoinKey %>% str_remove("_[A-Z ]{3,}+")
# output_r_Ljoin$JoinKey_SSSS=output_r_Ljoin$JoinKey %>% str_remove("_[A-Z ]{3,}+")
# # Check for new or missing JoinKeys
# missing_r=sort(setdiff(unique(templ_reg$JoinKey_SSSS),unique(output_r_Ljoin$JoinKey_SSSS)))
# new_r=sort(setdiff(unique(output_r_Ljoin$JoinKey_SSSS),unique(templ_reg$JoinKey_SSSS)))
# missing_r
# new_r
# 
# #Purge new JoinKeys
# I_purgenew_r=which(output_r_Ljoin$JoinKey_SSSS%in%new_r)
# output_r_Ljoin_PN=output_r_Ljoin[-I_purgenew_r,]
# write.csv(x = output_r_Ljoin_PN,row.names = FALSE,
#           file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/For Submission/March 2024 Submission/PacificBenthicDataSummaryTable_REGION_LEFTJOIN_byGenus_PurgeNewJK_19April2024.csv")
# 
# 
# #Pull taxon out of the join key  -- ISLAND
# templ_isl$JoinKey_SSSS=templ_isl$JoinKey %>% str_remove("_[A-Z ]{3,}+")
# output_isl_Ljoin$JoinKey_SSSS=output_isl_Ljoin$JoinKey %>% str_remove("_[A-Z ]{3,}+")
# # Check for new or missing JoinKeys
# missing_i=sort(setdiff(unique(templ_isl$JoinKey_SSSS),unique(output_isl_Ljoin$JoinKey_SSSS)))
# new_i=sort(setdiff(unique(output_isl_Ljoin$JoinKey_SSSS),unique(templ_isl$JoinKey_SSSS)))
# missing_i
# new_i
# 
# #Purge new JoinKeys
# I_purgenew_i=which(output_isl_Ljoin$JoinKey_SSSS%in%new_i)
# output_isl_Ljoin_PN=output_isl_Ljoin[-I_purgenew_i,]
# write.csv(x = output_isl_Ljoin_PN,row.names = FALSE,
#           file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/For Submission/March 2024 Submission/PacificBenthicDataSummaryTable_ISLAND_LEFTJOIN_byGenus_PurgeNewJK_19April2024.csv")
# 
# 
# #Pull taxon out of the join key  -- SECTOR
# templ_sec$JoinKey_SSSS=templ_sec$JoinKey %>% str_remove("_[A-Z ]{3,}+")
# output_SEC_Ljoin$JoinKey_SSSS=output_SEC_Ljoin$JoinKey %>% str_remove("_[A-Z ]{3,}+")
# # Check for new or missing JoinKeys
# missing_sc=sort(setdiff(unique(templ_sec$JoinKey_SSSS),unique(output_SEC_Ljoin$JoinKey_SSSS)))
# new_sc=sort(setdiff(unique(output_SEC_Ljoin$JoinKey_SSSS),unique(templ_sec$JoinKey_SSSS)))
# missing_sc
# new_sc
# 
# secAOI2Region=c("CNMI","CNMI","CNMI","CNMI","PRIAs","CNMI",
#                 "NWHI","GUA","GUA","GUA","CNMI","MHI",
#                 "MHI","MHI","MHI","PRIAs","PRIAs","PRIAs",
#                 "MHI","MHI","MHI","MHI","PRIAs","NWHI",
#                 "MHI","MHI","MHI","NWHI","CNMI","MHI",
#                 "MHI","MHI","MHI","MHI","MHI","MHI",
#                 "MHI","MHI","MHI","MHI","MHI","MHI",
#                 "MHI","MHI","MHI","SAMOA","CNMI","PRIAs",
#                 "NWHI","SAMOA","CNMI","CNMI","CNMI","SAMOA",
#                 "SAMOA","CNMI","SAMOA","SAMOA","SAMOA","SAMOA",
#                 "SAMOA","SAMOA","PRIAs","MHI","MHI","MHI")
# names(secAOI2Region)=c(sort(unique(templ_sec$AOILabel)),c("Maui; Hana","Maui; Molokini","Maui; Northwest"))
# 
# templ_sec$ANALYSIS_YEAR=paste(templ_sec$SurveyYearStart,"-",templ_sec$SurveyYearEnd)
# templ_sec$ANALYSIS_YEAR=factor(templ_sec$ANALYSIS_YEAR,levels=sort(unique(templ_sec$ANALYSIS_YEAR)))
# templ_sec$REGION=secAOI2Region[templ_sec$AOILabel]
# ts_D=templ_sec %>% filter(TaxonomicRes=="Com") %>% group_by(REGION,AOILabel,ANALYSIS_YEAR) %>% dplyr::summarize(N=sum(N_demo)) %>% pivot_wider(names_from = ANALYSIS_YEAR,values_from = N)
# ts_C=templ_sec %>% filter(TaxonomicRes=="Com") %>% group_by(REGION,AOILabel,ANALYSIS_YEAR) %>% dplyr::summarize(N=sum(N_cover)) %>% pivot_wider(names_from = ANALYSIS_YEAR,values_from = N)
# 
# ts_D=ts_D[,c(names(ts_D)[1:2],sort(names(ts_D)[3:length(names(ts_D))]))]
# ts_C=ts_C[,c(names(ts_C)[1:2],sort(names(ts_C)[3:length(names(ts_C))]))]
# 
# output_SEC_Ljoin$ANALYSIS_YEAR=paste(output_SEC_Ljoin$SurveyYearStart,"-",output_SEC_Ljoin$SurveyYearEnd)
# output_SEC_Ljoin$ANALYSIS_YEAR=factor(output_SEC_Ljoin$ANALYSIS_YEAR,levels=sort(unique(output_SEC_Ljoin$ANALYSIS_YEAR)))
# output_SEC_Ljoin$REGION=secAOI2Region[output_SEC_Ljoin$AOILabel]
# os_D=output_SEC_Ljoin %>% filter(TaxonomicRes=="Com") %>% group_by(REGION,AOILabel,ANALYSIS_YEAR) %>% dplyr::summarize(N=sum(N_demo)) %>% pivot_wider(names_from = ANALYSIS_YEAR,values_from = N)
# os_C=output_SEC_Ljoin %>% filter(TaxonomicRes=="Com") %>% group_by(REGION,AOILabel,ANALYSIS_YEAR) %>% dplyr::summarize(N=sum(N_cover)) %>% pivot_wider(names_from = ANALYSIS_YEAR,values_from = N)
# 
# os_D=os_D[,c(names(os_D)[1:2],sort(names(os_D)[3:length(names(os_D))]))]
# os_C=os_C[,c(names(os_C)[1:2],sort(names(os_C)[3:length(names(os_C))]))]
# 
# ts_C %>% print(n=99)
# os_C %>% print(n=99)
# ts_D %>% print(n=99)
# os_D %>% print(n=99)
# 
# 
# ########################Starting HERE!!!!!!
# #Purge new JoinKeys
# I_purgenew_sc=which(output_SEC_Ljoin$JoinKey_SSSS%in%new_sc)
# output_SEC_Ljoin_PN=output_SEC_Ljoin[-I_purgenew_sc,]
# write.csv(x = output_SEC_Ljoin_PN,row.names = FALSE,
#           file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/For Submission/March 2024 Submission/PacificBenthicDataSummaryTable_SECTOR_LEFTJOIN_byGenus_PurgeNewJK_19April2024.csv")
# 
# 
# 
# 
# #Pull taxon out of the join key  -- STRATA
# templ_str$JoinKey_SSSS=templ_str$JoinKey %>% str_remove("_[A-Z ]{3,}+")
# output_ST_Ljoin$JoinKey_SSSS=output_ST_Ljoin$JoinKey %>% str_remove("_[A-Z ]{3,}+")
# # Check for new or missing JoinKeys
# missing_st=sort(setdiff(unique(templ_str$JoinKey_SSSS),unique(output_ST_Ljoin$JoinKey_SSSS)))
# new_st=sort(setdiff(unique(output_ST_Ljoin$JoinKey_SSSS),unique(templ_str$JoinKey_SSSS)))
# missing_st
# new_st
##############################################################################################################################################################################################
##############################################################################################################################################################################################
##############################################################################################################################################################################################
# write.csv(st_all,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/formatted/PacificNCRMPviztool2022_STRATA_All_Taxa_TRENDS_Apr2023.csv",row.names = F)
# write.csv(sec_all,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/formatted/PacificNCRMPviztool2022_SECTOR_All_Taxa_TRENDS_Apr2023.csv",row.names = F)
# write.csv(isl_all,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/formatted/PacificNCRMPviztool2022_ISLAND_All_Taxa_TRENDS_Apr2023.csv",row.names = F)
# write.csv(r_all,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/formatted/PacificNCRMPviztool2022_REGION_All_Taxa_TRENDS_Apr2023.csv",row.names = F)
# 
# write.csv(st_gen,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/formatted/PacificNCRMPviztool2022_STRATA_By_Genus_TRENDS_Apr2023.csv",row.names = F)
# write.csv(sec_gen,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/formatted/PacificNCRMPviztool2022_SECTOR_By_Genus_TRENDS_Apr2023.csv",row.names = F)
# write.csv(isl_gen,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/formatted/PacificNCRMPviztool2022_ISLAND_By_Genus_TRENDS_Apr2023.csv",row.names = F)
# write.csv(r_gen,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/formatted/PacificNCRMPviztool2022_REGION_By_Genus_TRENDS_Apr2023.csv",row.names = F)

# write.csv(site.demo,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/formatted/PacificNCRMPviztool2022_SITE_demo.csv",row.names = F)
# write.csv(site.cover,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/formatted/PacificNCRMPviztool2022_SITE_cover.csv",row.names = F)
# 
# # Check out distinctions
# LL=templ_reg_Ljoin %>% filter(TaxonomicRes=="Com")
# RL=templ_reg_Rjoin %>% filter(TaxonomicRes=="Com")
# ALL=templ_reg_ALLjoin %>% filter(TaxonomicRes=="Com")
# print("Rows in LeftJoin:");dim(LL)[1];print("Rows in RightJoin:");dim(RL)[1];print("Rows in FullJoin:");dim(ALL)[1]
# print("JK in LeftJoin, not in FullJoin");cat(paste(sort(setdiff(LL$JoinKey,ALL$JoinKey)),collapse = "\n"))
# print("JK in RighttJoin, not in FullJoin");cat(paste(sort(setdiff(RL$JoinKey,ALL$JoinKey)),collapse = "\n"))
# print("JK in FullJoin, not in LeftJoin");cat(paste(sort(setdiff(ALL$JoinKey,LL$JoinKey)),collapse = "\n"))
# print("JK in FullJoin, not in RightJoin");cat(paste(sort(setdiff(ALL$JoinKey,RL$JoinKey)),collapse = "\n"))
# tLL=table(LL$JoinKey);print("Duplicated JoinKeys in LeftJoin");tLL[which(tLL>1)]
# tALL=table(ALL$JoinKey);print("Duplicated JoinKeys in FullJoin");tALL[which(tALL>1)]
# #Full Join just has PRIA roll-up we can ignore
# 
# #ISL
# LL=templ_isl_Ljoin %>% filter(TaxonomicRes=="Com")
# RL=templ_isl_Rjoin %>% filter(TaxonomicRes=="Com")
# ALL=templ_isl_ALLjoin %>% filter(TaxonomicRes=="Com")
# print("Rows in LeftJoin:");dim(LL)[1];print("Rows in RightJoin:");dim(RL)[1];print("Rows in FullJoin:");dim(ALL)[1]
# print("JK in LeftJoin, not in FullJoin");cat(paste(sort(setdiff(LL$JoinKey,ALL$JoinKey)),collapse = "\n"))
# print("JK in RightJoin, not in FullJoin");cat(paste(sort(setdiff(RL$JoinKey,ALL$JoinKey)),collapse = "\n"))
# print("JK in FullJoin, not in LeftJoin");cat(paste(sort(setdiff(ALL$JoinKey,LL$JoinKey)),collapse = "\n"))
# print("JK in FullJoin, not in RightJoin");cat(paste(sort(setdiff(ALL$JoinKey,RL$JoinKey)),collapse = "\n"))
# tLL=table(LL$JoinKey);print("Duplicated JoinKeys in LeftJoin");tLL[which(tLL>1)]
# tALL=table(ALL$JoinKey);print("Duplicated JoinKeys in FullJoin");tALL[which(tALL>1)]
# #Full Join Has some interesting data:
# #-Agrihan in 2014
# 
# #SEC
# LL=templ_sec_Ljoin %>% filter(TaxonomicRes=="Com")
# ALL=templ_sec_ALLjoin %>% filter(TaxonomicRes=="Com")
# print("Rows in LeftJoin:");dim(LL)[1];print("Rows in FullJoin:");dim(ALL)[1]
# print("JK in LeftJoin, not in FullJoin");cat(paste(sort(setdiff(LL$JoinKey,ALL$JoinKey)),collapse = "\n"))
# print("JK in FullJoin, not in LeftJoin");cat(paste(sort(setdiff(ALL$JoinKey,LL$JoinKey)),collapse = "\n"))
# tLL=table(LL$JoinKey);print("Duplicated JoinKeys in LeftJoin");tLL[which(tLL>1)]
# tALL=table(ALL$JoinKey);print("Duplicated JoinKeys in FullJoin");tALL[which(tALL>1)]
# 
# 
# #STR
# LL=templ_str_Ljoin %>% filter(TaxonomicRes=="Com")
# ALL=templ_str_ALLjoin %>% filter(TaxonomicRes=="Com")
# print("Rows in LeftJoin:");dim(LL)[1];print("Rows in FullJoin:");dim(ALL)[1]
# print("JK in LeftJoin, not in FullJoin");cat(paste(sort(setdiff(LL$JoinKey,ALL$JoinKey)),collapse = "\n"))
# print("JK in FullJoin, not in LeftJoin");cat(paste(sort(setdiff(ALL$JoinKey,LL$JoinKey)),collapse = "\n"))
# tLL=table(LL$JoinKey);print("Duplicated JoinKeys in LeftJoin");tLL[which(tLL>1)]
# tALL=table(ALL$JoinKey);print("Duplicated JoinKeys in FullJoin");tALL[which(tALL>1)]
# 
# Ldups=tLL[which(tLL>1)]
# templ_str_Ljoin %>% filter(JoinKey%in%names(Ldups)&TaxonomicRes=="Com") %>% arrange(JoinKey) %>% View()
