#read in Tier1 summary tables from Benthic Cover_RawtoEstimates_NCRMPViztool.R script


rm(list=ls())

#LOAD LIBRARY FUNCTIONS ...
# source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
# source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
# source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")
source("./Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("../fish-paste/lib/core_functions.R")
source("../fish-paste/lib/GIS_functions.R")

#Read in DEMOGRAPHIC data ####
st.demo<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/BenthicREA_STRATA_Demo_Viztool.csv")
st.demo<-st.demo[,c("REGION","ISLAND", "SECTOR","DB_RZ","ANALYSIS_YEAR","n","GENUS_CODE","AdColDen","JuvColDen","Mean_BLE_Prev","Mean_TotDZ_Prev","SE_AdColDen","SE_JuvColDen","SE_BLE_Prev","SE_TotDZ_Prev")]
colnames(st.demo)[colnames(st.demo)=="DB_RZ"]<-"STRATA"
colnames(st.demo)[colnames(st.demo)=="n"]<-"N_Demo"


sec.demo<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/BenthicREA_SECTOR_Demo_Viztool.csv")
sec.demo<-sec.demo[,c("REGION","PooledSector_Viztool","ANALYSIS_YEAR","n","GENUS_CODE","Mean_AdColDen","Mean_JuvColDen","Mean_BLE_Prev","Mean_TotDZ_Prev","SE_AdColDen","SE_JuvColDen","SE_BLE_Prev","SE_TotDZ_Prev")]
colnames(sec.demo)[colnames(sec.demo)=="PooledSector_Viztool"]<-"SECTOR"
colnames(sec.demo)[colnames(sec.demo)=="n"]<-"N_Demo"

isl.demo<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/BenthicREA_ISLAND_Demo_Viztool.csv")
isl.demo<-isl.demo[,c("REGION","ISLAND","ANALYSIS_YEAR","n","GENUS_CODE","Mean_AdColDen","Mean_JuvColDen","Mean_BLE_Prev","Mean_TotDZ_Prev","SE_AdColDen","SE_JuvColDen","SE_BLE_Prev","SE_TotDZ_Prev")]
colnames(isl.demo)[colnames(isl.demo)=="n"]<-"N_Demo"

r.demo<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/BenthicREA_REGION_Demo_Viztool.csv")
r.demo<-r.demo[,c("REGION","ANALYSIS_YEAR","n","GENUS_CODE","Mean_AdColDen","Mean_JuvColDen","Mean_BLE_Prev","Mean_TotDZ_Prev","SE_AdColDen","SE_JuvColDen","SE_BLE_Prev","SE_TotDZ_Prev")]
colnames(r.demo)[colnames(r.demo)=="n"]<-"N_Demo"

site.demo<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/Viztool/2022ViztoolSites_Demo.csv")
site.demo<-unique(site.demo[,c("REGION","PooledSector_Viztool","DB_RZ","ANALYSIS_YEAR","SITEVISITID","SITE","LATITUDE","LONGITUDE")])
head(site.demo)


#Read in Tier 1 (functional groups) COVER data
st.cover<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/BenthicCover_2010-2019_Tier1_STRATA_Viztool.csv")
st.cover<-st.cover[,c("REGION","ISLAND", "ANALYSIS_SEC","STRATA","ANALYSIS_YEAR","N","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]
st.cover$GENUS_CODE<-"SSSS"
colnames(st.cover)[colnames(st.cover)=="N"]<-"N_Cover"
colnames(st.cover)[colnames(st.cover)=="ANALYSIS_SEC"]<-"SECTOR"

sec.cover<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/BenthicCover_2010-2019_Tier1_SECTOR_Viztool.csv")
sec.cover<-sec.cover[,c("REGION","ANALYSIS_SEC","ANALYSIS_YEAR","N","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]
sec.cover$GENUS_CODE<-"SSSS"
colnames(sec.cover)[colnames(sec.cover)=="N"]<-"N_Cover"
colnames(sec.cover)[colnames(sec.cover)=="ANALYSIS_SEC"]<-"SECTOR"

isl.cover<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/BenthicCover_2010-2019_Tier1_ISLAND_Viztool.csv")
isl.cover<-isl.cover[,c("REGION","ISLAND","ANALYSIS_YEAR","N","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]
isl.cover$GENUS_CODE<-"SSSS"
colnames(isl.cover)[colnames(isl.cover)=="N"]<-"N_Cover"

r.cover<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/BenthicCover_2010-2019_Tier1_REGION_Viztool.csv")
r.cover<-r.cover[,c("REGION","ANALYSIS_YEAR","N","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]
r.cover$GENUS_CODE<-"SSSS"
colnames(r.cover)[colnames(r.cover)=="N"]<-"N_Cover"

site.cover<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/Viztool/2022ViztoolSites_Cover.csv")
site.cover<-site.cover[,c("REGION","PooledSector_Viztool","STRATAcode","ANALYSIS_YEAR","SITEVISITID","SITE","LATITUDE","LONGITUDE")]
site.cover$GENUS_CODE<-"SSSS"


#Read in Tier 2b (Genus level) COVER data
#STRATA
st.cover2w<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/BenthicCover_2010-2019_Tier2b_STRATA_Viztool.csv")
#st.cover2w<-dplyr::select(st.cover2w,-c(Mean.TOT_AREA_WT))

#convert wide to long
st.cover2<-pivot_longer(st.cover2w,cols = Mean.ACAS:SE.TURS,
                        names_to = c(".value","GENUS_CODE"),
                        names_sep = "\\.")

#reorder and rename columns to match template -add MA and CCA columns so we can properly merge with tier 1 data
st.cover2<-st.cover2[,c("REGION","ISLAND", "ANALYSIS_SEC","STRATA","ANALYSIS_YEAR","N","GENUS_CODE","Mean","SE")]

colnames(st.cover2)[colnames(st.cover2)=="N"]<-"N_Cover"
colnames(st.cover2)[colnames(st.cover2)=="ANALYSIS_SEC"]<-"SECTOR"
colnames(st.cover2)[colnames(st.cover2)=="Mean"]<-"Mean.CORAL"
colnames(st.cover2)[colnames(st.cover2)=="SE"]<-"SE.CORAL"
st.cover2$Mean.MA<-NA
st.cover2$Mean.CCA<-NA
st.cover2$SE.MA<-NA
st.cover2$SE.CCA<-NA


#SECTOR
sec.cover2w<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/BenthicCover_2010-2019_Tier2b_SECTOR_Viztool.csv")
sec.cover2w<-dplyr::select(sec.cover2w,-c(Mean.TOT_AREA_WT))

#convert wide to long
sec.cover2<-pivot_longer(sec.cover2w,cols = Mean.ACAS:SE.TURS,
                        names_to = c(".value","GENUS_CODE"),
                        names_sep = "\\.")


sec.cover2<-sec.cover2[,c("REGION","ANALYSIS_SEC","ANALYSIS_YEAR","N","GENUS_CODE","Mean","SE")]

colnames(sec.cover2)[colnames(sec.cover2)=="N"]<-"N_Cover"
colnames(sec.cover2)[colnames(sec.cover2)=="ANALYSIS_SEC"]<-"SECTOR"
colnames(sec.cover2)[colnames(sec.cover2)=="Mean"]<-"Mean.CORAL"
colnames(sec.cover2)[colnames(sec.cover2)=="SE"]<-"SE.CORAL"
sec.cover2$Mean.MA<-NA
sec.cover2$Mean.CCA<-NA
sec.cover2$SE.MA<-NA
sec.cover2$SE.CCA<-NA

#ISLAND
isl.cover2w<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/BenthicCover_2010-2019_Tier2b_ISLAND_Viztool.csv")
isl.cover2w<-dplyr::select(isl.cover2w,-c(Mean.TOT_AREA_WT))

isl.cover2<-pivot_longer(isl.cover2w,cols = Mean.ACAS:SE.TURS,
                         names_to = c(".value","GENUS_CODE"),
                         names_sep = "\\.")

isl.cover2<-isl.cover2[,c("REGION","ISLAND","ANALYSIS_YEAR","N","GENUS_CODE","Mean","SE")]
colnames(isl.cover2)[colnames(isl.cover2)=="N"]<-"N_Cover"
colnames(isl.cover2)[colnames(isl.cover2)=="Mean"]<-"Mean.CORAL"
colnames(isl.cover2)[colnames(isl.cover2)=="SE"]<-"SE.CORAL"
isl.cover2$Mean.MA<-NA
isl.cover2$Mean.CCA<-NA
isl.cover2$SE.MA<-NA
isl.cover2$SE.CCA<-NA

r.cover2w<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/BenthicCover_2010-2019_Tier2b_REGION_Viztool.csv")
r.cover2w<-dplyr::select(r.cover2w,-c(Mean.TOT_AREA_WT))

r.cover2<-pivot_longer(r.cover2w,cols = Mean.ACAS:SE.TURS,
                         names_to = c(".value","GENUS_CODE"),
                         names_sep = "\\.")

r.cover2<-r.cover2[,c("REGION","ANALYSIS_YEAR","N","GENUS_CODE","Mean","SE")]

colnames(r.cover2)[colnames(r.cover2)=="N"]<-"N_Cover"
colnames(r.cover2)[colnames(r.cover2)=="Mean"]<-"Mean.CORAL"
colnames(r.cover2)[colnames(r.cover2)=="SE"]<-"SE.CORAL"
r.cover2$Mean.MA<-NA
r.cover2$Mean.CCA<-NA
r.cover2$SE.MA<-NA
r.cover2$SE.CCA<-NA


ChangeAnalysisYear<-function(data){
  data <- data %>% mutate(data,
                          ANALYSIS_YEAR= case_when(
                          REGION =="MHI" & ANALYSIS_YEAR == "2013" ~ "2013-15",
                          REGION =="SAMOA" & ANALYSIS_YEAR == "2015" ~ "2015-16",
                          TRUE ~ ANALYSIS_YEAR))
  return(data$ANALYSIS_YEAR)
}

st.demo$ANALYSIS_YEAR<-ChangeAnalysisYear(st.demo)
sec.demo$ANALYSIS_YEAR<-ChangeAnalysisYear(sec.demo)
isl.demo$ANALYSIS_YEAR<-ChangeAnalysisYear(isl.demo)
r.demo$ANALYSIS_YEAR<-ChangeAnalysisYear(r.demo)

#Change island from SGA to Sarigan, Alamagan, Guguan
st.demo <- st.demo %>% mutate(st.demo,
                        ISLAND= case_when(
                          SECTOR %in% c("Guguan", "Alamagan", "Sarigan") ~ "Sarigan, Alamagan, Guguan",
                          TRUE ~ ISLAND))
isl.demo <- isl.demo %>% mutate(isl.demo,
                              ISLAND= case_when(
                                ISLAND == "SGA" ~ "Sarigan, Alamagan, Guguan",
                                TRUE ~ ISLAND))

#Combine genus-level and Tier 1 cover data
st.cover<-rbind(st.cover,st.cover2)
sec.cover<-rbind(sec.cover,sec.cover2)
isl.cover<-rbind(isl.cover,isl.cover2)
r.cover<-rbind(r.cover,r.cover2)

#Merge demographic with Tier 1 and genus-level cover data
st<-full_join(st.demo,st.cover)
sec<-full_join(sec.demo,sec.cover)
isl<-full_join(isl.demo,isl.cover)
r<-full_join(r.demo, r.cover)

# View(st)
# View(sec)
# View(isl)
# View(r)

#Read in look up table of all possible strata and sector names ####
seclu<-read.csv("T:/Benthic/Data/Lookup Tables/PacificNCRMP_Benthic_Sectors_Lookup_v4.csv") #Look up for pooling scheme and table of strata names
secname<-read.csv("T:/Benthic/Data/Lookup Tables/SectorNamelookup.csv") #list of all sectors (both SEC_NAME and different pooled sectors)-look up table of sector names

#Change column names to match data
colnames(secname)[colnames(secname)=="SECTOR"]<-"SECTOR" 
colnames(seclu)[colnames(seclu)=="PooledSector_Viztool"]<-"SECTOR" #subset just acute diseased colonies
colnames(seclu)[colnames(seclu)=="STRATAcode"]<-"STRATA" #subset just acute diseased colonies
colnames(site.demo)[colnames(site.demo)=="PooledSector_Viztool"]<-"SECTOR" #subset just acute diseased colonies
colnames(site.demo)[colnames(site.demo)=="DB_RZ"]<-"STRATA" #subset just acute diseased colonies
colnames(site.cover)[colnames(site.cover)=="PooledSector_Viztool"]<-"SECTOR" #subset just acute diseased colonies
colnames(site.cover)[colnames(site.cover)=="DB_RZ"]<-"STRATA" #subset just acute diseased colonies

#Change sarigan, alamagan and guguan's island name
seclu$ISLAND<-ifelse(seclu$ISLAND %in% c("Sarigan","Alamagan","Guguan"),"Sarigan, Alamagan, Guguan",seclu$ISLAND)
seclu$ISLANDCODE<-ifelse(seclu$ISLAND %in% c("Sarigan, Alamagan, Guguan"),"SGA",seclu$ISLANDCODE)

#Remove Maro, Midway and Laysan since they aren't part of the NCRMP islands
remove<-c("Maro","Midway","Laysan")
seclu<-filter(seclu,! ISLAND %in% remove)
site.cover<-filter(site.cover,! SECTOR %in% remove)
site.demo<-filter(site.demo,! SECTOR %in% remove)


secname <- secname %>% mutate(secname,
                              REGION= case_when(
                                ISLAND =="Guam" ~ "GUA",
                                REGION == "MARIAN" & ISLAND !="Guam" ~ "CNMI",
                                TRUE ~ REGION))
seclu <- seclu %>% mutate(seclu,
                          REGION= case_when(
                            ISLAND =="Guam" ~ "GUA",
                            REGION == "MARIAN" & ISLAND !="Guam" ~ "CNMI",
                            TRUE ~ REGION))
secname <- secname %>% mutate(secname,
                          REGION_NAME= case_when(
                          ISLAND =="Guam" ~ "Guam",
                          REGION == "CNMI"~ "Commonwealth of the Northern Mariana Islands",
                          TRUE ~ REGION_NAME))
seclu <- seclu %>% mutate(seclu,
                          REGION_NAME= case_when(
                            ISLAND =="Guam" ~ "Guam",
                            REGION == "CNMI"~ "Commonwealth of the Northern Mariana Islands",
                            TRUE ~ REGION_NAME))

#Merge sector and strata names to SITE data
st.lu<-unique(seclu[,c("REGION_NAME","REGION","ISLAND","ISLANDCODE","SECTOR","STRATAname","STRATA")]) #make sure to use unique because we are pooling certain sectors together (e.g. Tut_aunuu)


nrow(site.demo)
#add Strataname
site.demo<-left_join(site.demo,st.lu) 
site.demo<-left_join(site.demo,secname)
nrow(site.demo)
#View(site.demo)

nrow(site.cover)
#add Strataname
colnames(site.cover)[colnames(site.cover)=="STRATAcode"]<-"STRATA"
site.cover<-left_join(site.cover,st.lu) 
site.cover<-left_join(site.cover,secname)
nrow(site.cover)
#View(site.cover)


#Include all possible STRATA and add NA values for the strata that weren't sampled each year ####

st.lu<-unique(seclu[,c("REGION","ISLAND","SECTOR","STRATAname","STRATA")]) #make sure to use unique because we are pooling certain sectors together (e.g. Tut_aunuu)

#add Strataname
st<-left_join(st,st.lu) 

#Create list of all possible strata and analysis year
st.lu_years <- st.lu %>% left_join(st %>% distinct(REGION, ANALYSIS_YEAR,GENUS_CODE), by = c("REGION"))

demo.st<-full_join(st,st.lu_years) %>%
  left_join(secname,by=c("REGION","SECTOR","ISLAND")) %>%
  mutate(N_Demo = replace_na(N_Demo,0)) %>%
  mutate(N_Cover = replace_na(N_Cover,0))
  
demo.st<-filter(demo.st, !ISLANDCODE %in% c("SAR","GUG","ALA"))
View(demo.st)


#Include all possible SECTORS and add NA values for the sectors that weren't sampled each year ####

sec.lu<-unique(seclu[,c("REGION","ISLAND","SECTOR")]) #make sure to use unique because we are pooling certain sectors together (e.g. Tut_aunuu)

sec<-left_join(sec,sec.lu) 

#Create list of all possible strata and analysis year
sec.lu_years <- sec.lu %>% left_join(sec %>% distinct(REGION, ANALYSIS_YEAR,GENUS_CODE), by = c("REGION"))

demo.sec<-full_join(sec,sec.lu_years) %>%
  left_join(secname,by=c("REGION","SECTOR","ISLAND")) %>%
  mutate(N_Demo = replace_na(N_Demo,0)) %>%
  mutate(N_Cover = replace_na(N_Cover,0))

demo.sec<-filter(demo.sec, !ISLANDCODE %in% c("SAR","GUG","ALA"))
View(demo.sec)


#Include all possible ISLANDS and add NA values for the islands that weren't sampled each year ####

isl.lu<-unique(seclu[,c("REGION_NAME","REGION","ISLAND","ISLANDCODE")])

isl<-left_join(isl,isl.lu) 

#Create list of all possible strata and analysis year
isl.lu_years <- isl.lu %>% left_join(isl %>% distinct(REGION, ANALYSIS_YEAR,GENUS_CODE), by = c("REGION"))

demo.isl<-full_join(isl,isl.lu_years) %>%
  mutate(N_Demo = replace_na(N_Demo,0)) %>%
  mutate(N_Cover = replace_na(N_Cover,0))

demo.isl<-filter(demo.isl, !ISLANDCODE %in% c("SAR","GUG","ALA"))
View(demo.isl)

demo.r<-r %>% mutate(REGION_NAME=recode(REGION,
                                        `GUA`="Guam",
                                        `CNMI`="Commonwealth of the Northern Mariana Islands",
                                        `MHI`="Main Hawaiian Islands",
                                        `NWHI`="Northwestern Hawaiian Islands",
                                        `SAMOA`="American Samoa",
                                        `PRIAs`="Pacific Remote Island Areas"))

#Final formatting tweaks ####

#Add scientific name
genlu<-read.csv("T:/Benthic/Data/Lookup Tables/Genus_lookup.csv")
genlu<-genlu[,c("SPCODE","TAXONNAME")]
colnames(genlu)[colnames(genlu)=="SPCODE"]<-"GENUS_CODE"
colnames(genlu)[colnames(genlu)=="TAXONNAME"]<-"ScientificName"

demo.st<-left_join(demo.st,genlu)
demo.sec<-left_join(demo.sec,genlu)
demo.isl<-left_join(demo.isl,genlu)
demo.r<-left_join(demo.r,genlu)


#remove years before 2013
years<-c("2013","2013-15","2014-15","2014","2015","2016","2015-16","2017-18","2017","2018","2019")

demo.st<-subset(demo.st,ANALYSIS_YEAR %in% years);View(demo.st)
demo.sec<-subset(demo.sec,ANALYSIS_YEAR %in% years);View(demo.sec)
demo.isl<-subset(demo.isl,ANALYSIS_YEAR %in% years);View(demo.isl)
demo.r<-subset(demo.r,ANALYSIS_YEAR %in% years);View(demo.r)
site.demo<-subset(site.demo,ANALYSIS_YEAR %in% years);View(site.demo)
site.cover<-subset(site.cover,ANALYSIS_YEAR %in% years);View(site.cover)


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
                                                 `2013-15`="2014",
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

demo.st$SurveyYearStart<-CalcStartYr(demo.st)
demo.st$SurveyYearEnd<-CalcEndYr(demo.st)
demo.st$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(demo.st)

demo.sec$SurveyYearStart<-CalcStartYr(demo.sec)
demo.sec$SurveyYearEnd<-CalcEndYr(demo.sec)
demo.sec$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(demo.sec)


demo.isl$SurveyYearStart<-CalcStartYr(demo.isl)
demo.isl$SurveyYearEnd<-CalcEndYr(demo.isl)
demo.isl$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(demo.isl)


demo.r$SurveyYearStart<-CalcStartYr(demo.r)
demo.r$SurveyYearEnd<-CalcEndYr(demo.r)
demo.r$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(demo.r)

site.cover$SurveyYearStart<-CalcStartYr(site.cover)
site.cover$SurveyYearEnd<-CalcEndYr(site.cover)
site.cover$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(site.cover)

site.demo$SurveyYearStart<-CalcStartYr(site.demo)
site.demo$SurveyYearEnd<-CalcEndYr(site.demo)
site.demo$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(site.demo)

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

demo.st<-ChangeStrataCode(demo.st)
site.demo<-ChangeStrataCode(site.demo)
site.cover<-ChangeStrataCode(site.cover)

demo.st<-ColumnNameChange(demo.st)
demo.sec<-ColumnNameChange(demo.sec)
demo.isl<-ColumnNameChange(demo.isl)
demo.r<-ColumnNameChange(demo.r)
site.demo<-ColumnNameChange(site.demo)
site.cover<-ColumnNameChange(site.cover)


demo.st<-demo.st[,c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode", "SECTORname","SECTORcode","STRATAname","STRATAcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N_Demo","N_Cover",
                    "TaxonomicResolution","TaxonomicCode","ScientificName",
                    "CoralCover_Pct","CoralCover_error","CrustoseCoralineAlgaeCover_Pct","CrustoseCoralineAlgaeCover_error","MacroalgaeCover_Pct",
                    "MacroalgaeCover_error","AdultDensity_colperm2","AdultDensity_SE","JuvenileDensity_colperm2","JuvenileDensity_SE","DiseasePrevalence_Pct",
                    "DiseasePrevalence_SE","BleachingPrevalence_Pct","BleachingPrevalence_SE")]

demo.sec<-demo.sec[,c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode", "SECTORname","SECTORcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N_Demo","N_Cover",
                      "TaxonomicResolution","TaxonomicCode","ScientificName",
                      "CoralCover_Pct","CoralCover_error","CrustoseCoralineAlgaeCover_Pct","CrustoseCoralineAlgaeCover_error","MacroalgaeCover_Pct",
                      "MacroalgaeCover_error","AdultDensity_colperm2","AdultDensity_SE","JuvenileDensity_colperm2","JuvenileDensity_SE","DiseasePrevalence_Pct",
                      "DiseasePrevalence_SE","BleachingPrevalence_Pct","BleachingPrevalence_SE")]


demo.isl<-demo.isl[,c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N_Demo","N_Cover",
                      "TaxonomicResolution","TaxonomicCode","ScientificName",
                      "CoralCover_Pct","CoralCover_error","CrustoseCoralineAlgaeCover_Pct","CrustoseCoralineAlgaeCover_error","MacroalgaeCover_Pct",
                      "MacroalgaeCover_error","AdultDensity_colperm2","AdultDensity_SE","JuvenileDensity_colperm2","JuvenileDensity_SE","DiseasePrevalence_Pct",
                      "DiseasePrevalence_SE","BleachingPrevalence_Pct","BleachingPrevalence_SE")]

demo.r<-demo.r[,c("JURISDICTIONname","JURISDICTIONcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N_Demo","N_Cover",
                  "TaxonomicResolution","TaxonomicCode","ScientificName",
                  "CoralCover_Pct","CoralCover_error","CrustoseCoralineAlgaeCover_Pct","CrustoseCoralineAlgaeCover_error","MacroalgaeCover_Pct",
                  "MacroalgaeCover_error","AdultDensity_colperm2","AdultDensity_SE","JuvenileDensity_colperm2","JuvenileDensity_SE","DiseasePrevalence_Pct",
                  "DiseasePrevalence_SE","BleachingPrevalence_Pct","BleachingPrevalence_SE")]

site.cover<-site.cover[,c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode", "SECTORname","SECTORcode","STRATAname","STRATAcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","SiteID",
              "LATITUDE","LONGITUDE")]

site.demo<-site.demo[,c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode", "SECTORname","SECTORcode","STRATAname","STRATAcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","SiteID",
              "LATITUDE","LONGITUDE")]

#Separate all hard coral taxa and functional cover into 1 set of csv files
st.all<-filter(demo.st,TaxonomicCode =="SSSS")
sec.all<-filter(demo.sec,TaxonomicCode =="SSSS")
isl.all<-filter(demo.isl,TaxonomicCode =="SSSS")
r.all<-filter(demo.r,TaxonomicCode =="SSSS")

st.all[c("TaxonomicResolution","TaxonomicCode","ScientificName")]<-NA
sec.all[c("TaxonomicResolution","TaxonomicCode","ScientificName")]<-NA
isl.all[c("TaxonomicResolution","TaxonomicCode","ScientificName")]<-NA
r.all[c("TaxonomicResolution","TaxonomicCode","ScientificName")]<-NA

#and genera into another set
st.gen<-filter(demo.st,TaxonomicCode !="SSSS")
sec.gen<-filter(demo.sec,TaxonomicCode !="SSSS")
isl.gen<-filter(demo.isl,TaxonomicCode !="SSSS")
r.gen<-filter(demo.r,TaxonomicCode !="SSSS")



#QC Checks

QCcheck<-function(data){
  data %>% 
    dplyr::select(meta.cols) %>%
    summarise(across(everything(), ~ sum(is.na(.x))))
  
  data[,meta.cols] <- lapply(st.all[,meta.cols], as.factor)
  
  m<-sapply(data[,meta.cols],levels) #Make sure levels of variables are correct
  d<-sapply(data[,data.cols],summary) #make sure range of data is appropriate
  
  return(list(m,d))
}

#strata
meta.cols<-c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode", "SECTORname","SECTORcode",
             "STRATAname","STRATAcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N_Demo","N_Cover")
data.cols<-c("CoralCover_Pct","CrustoseCoralineAlgaeCover_Pct","MacroalgaeCover_Pct","CoralCover_error","CrustoseCoralineAlgaeCover_error",
             "MacroalgaeCover_error","AdultDensity_colperm2","JuvenileDensity_colperm2","BleachingPrevalence_Pct","DiseasePrevalence_Pct","AdultDensity_SE",         
             "JuvenileDensity_SE","BleachingPrevalence_SE","DiseasePrevalence_SE")
QCcheck(st.all)

#sector
meta.cols<-c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode", "SECTORname","SECTORcode",
             "SurveyYearStart","SurveyYearEnd","AnalysisYear","N_Demo","N_Cover")
QCcheck(sec.all)

#island
meta.cols<-c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode",
             "SurveyYearStart","SurveyYearEnd","AnalysisYear","N_Demo","N_Cover")
QCcheck(isl.all)

#region
meta.cols<-c("JURISDICTIONname","JURISDICTIONcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N_Demo","N_Cover")
QCcheck(r.all)



# 
write.csv(st.all,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/formatted/PacificNCRMPviztool2022_STRATA_All_Taxa_Dec2022.csv",row.names = F)
write.csv(sec.all,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/formatted/PacificNCRMPviztool2022_SECTOR_All_Taxa_Dec2022.csv",row.names = F)
write.csv(isl.all,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/formatted/PacificNCRMPviztool2022_ISLAND_All_Taxa_Dec2022.csv",row.names = F)
write.csv(r.all,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/formatted/PacificNCRMPviztool2022_REGION_All_Taxa_Dec2022.csv",row.names = F)

write.csv(st.gen,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/formatted/PacificNCRMPviztool2022_STRATA_By_Genus_Dec2022.csv",row.names = F)
write.csv(sec.gen,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/formatted/PacificNCRMPviztool2022_SECTOR_By_Genus_Dec2022.csv",row.names = F)
write.csv(isl.gen,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/formatted/PacificNCRMPviztool2022_ISLAND_By_Genus_Dec2022.csv",row.names = F)
write.csv(r.gen,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/formatted/PacificNCRMPviztool2022_REGION_By_Genus_Dec2022.csv",row.names = F)

write.csv(site.demo,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/formatted/PacificNCRMPviztool2022_SITE_demo.csv",row.names = F)
write.csv(site.cover,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/formatted/PacificNCRMPviztool2022_SITE_cover.csv",row.names = F)

