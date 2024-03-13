#read in Tier1 summary tables from Benthic Cover_RawtoEstimates_NCRMPViztool.R script


rm(list=ls())

#LOAD LIBRARY FUNCTIONS ...
# source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
# source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
# source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")
source("./Functions/Benthic_Functions_newApp_vTAOfork.R")
source("../fish-paste/lib/core_functions.R")
source("../fish-paste/lib/GIS_functions.R")

#Read in DEMOGRAPHIC data ####
st.demo<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicREA_STRATA_TRENDS_Demo_Viztool_2023.csv")#BenthicREA_STRATA_Demo_Viztool.csv
st.demo<-st.demo[,c("REGION","ISLAND", "SECTOR","DB_RZ","ANALYSIS_YEAR","n","GENUS_CODE","AdColDen","JuvColDen","Mean_BLE_Prev","Mean_TotDZ_Prev","SE_AdColDen","SE_JuvColDen","SE_BLE_Prev","SE_TotDZ_Prev")]
colnames(st.demo)[colnames(st.demo)=="DB_RZ"]<-"STRATA"
colnames(st.demo)[colnames(st.demo)=="n"]<-"N_Demo"


sec.demo<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicREA_SECTOR_TRENDS_Demo_Viztool_2023.csv")#BenthicREA_SECTOR_Demo_Viztool.csv
sec.demo<-sec.demo[,c("REGION","PooledSector_Viztool","ANALYSIS_YEAR","n","GENUS_CODE","Mean_AdColDen","Mean_JuvColDen","Mean_BLE_Prev","Mean_TotDZ_Prev","SE_AdColDen","SE_JuvColDen","SE_BLE_Prev","SE_TotDZ_Prev")]
colnames(sec.demo)[colnames(sec.demo)=="PooledSector_Viztool"]<-"SECTOR"
colnames(sec.demo)[colnames(sec.demo)=="n"]<-"N_Demo"

isl.demo<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicREA_ISLAND_TRENDS_Demo_Viztool_2023.csv")
isl.demo<-isl.demo[,c("REGION","ISLAND","ANALYSIS_YEAR","n","GENUS_CODE","Mean_AdColDen","Mean_JuvColDen","Mean_BLE_Prev","Mean_TotDZ_Prev","SE_AdColDen","SE_JuvColDen","SE_BLE_Prev","SE_TotDZ_Prev")]
colnames(isl.demo)[colnames(isl.demo)=="n"]<-"N_Demo"

r.demo<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicREA_REGION_TRENDS_Demo_Viztool_2023.csv")
r.demo<-r.demo[,c("REGION","ANALYSIS_YEAR","n","GENUS_CODE","Mean_AdColDen","Mean_JuvColDen","Mean_BLE_Prev","Mean_TotDZ_Prev","SE_AdColDen","SE_JuvColDen","SE_BLE_Prev","SE_TotDZ_Prev")]
colnames(r.demo)[colnames(r.demo)=="n"]<-"N_Demo"

# site.demo<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicREA_REGION_TRENDS_Demo_Viztool_2023.csv")
# site.demo<-unique(site.demo[,c("REGION","PooledSector_Viztool","DB_RZ","ANALYSIS_YEAR","SITEVISITID","SITE","LATITUDE","LONGITUDE")])
# head(site.demo)


#Read in Tier 1 (functional groups) COVER data
st.cover<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicCover_2010-2019_Tier1_STRATA_Trends_Viztool.csv")
st.cover<-st.cover[,c("REGION","ISLAND", "ANALYSIS_SEC","STRATA","ANALYSIS_YEAR","N","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]
st.cover$GENUS_CODE<-"SSSS"
colnames(st.cover)[colnames(st.cover)=="N"]<-"N_Cover"
colnames(st.cover)[colnames(st.cover)=="ANALYSIS_SEC"]<-"SECTOR"

sec.cover<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicCover_2010-2019_Tier1_SECTOR_Trends_Viztool.csv")
sec.cover<-sec.cover[,c("REGION","ANALYSIS_SEC","ANALYSIS_YEAR","N","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]
sec.cover$GENUS_CODE<-"SSSS"
colnames(sec.cover)[colnames(sec.cover)=="N"]<-"N_Cover"
colnames(sec.cover)[colnames(sec.cover)=="ANALYSIS_SEC"]<-"SECTOR"

isl.cover<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicCover_2010-2019_Tier1_ISLAND_Trends_Viztool.csv")
isl.cover<-isl.cover[,c("REGION","ISLAND","ANALYSIS_YEAR","N","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]
isl.cover$GENUS_CODE<-"SSSS"
colnames(isl.cover)[colnames(isl.cover)=="N"]<-"N_Cover"

r.cover<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicCover_2010-2019_Tier1_REGION_Trends_Viztool.csv")
r.cover<-r.cover[,c("REGION","ANALYSIS_YEAR","N","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]
r.cover$GENUS_CODE<-"SSSS"
colnames(r.cover)[colnames(r.cover)=="N"]<-"N_Cover"

# site.cover<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/Viztool/2022ViztoolSites_Cover.csv")
# site.cover<-site.cover[,c("REGION","PooledSector_Viztool","STRATAcode","ANALYSIS_YEAR","SITEVISITID","SITE","LATITUDE","LONGITUDE")]
# site.cover$GENUS_CODE<-"SSSS"


#Read in Tier 2b (Genus level) COVER data
#STRATA
st.cover2w<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicCover_2010-2019_Tier2b_STRATA_Trends_Viztool.csv")
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
sec.cover2w<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicCover_2010-2019_Tier2b_SECTOR_Trends_Viztool.csv")
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
isl.cover2w<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicCover_2010-2019_Tier2b_ISLAND_Trends_Viztool.csv")
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

r.cover2w<-read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2022/unformatted/TRENDS/BenthicCover_2010-2019_Tier2b_REGION_Trends_Viztool.csv")
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
# colnames(site.demo)[colnames(site.demo)=="PooledSector_Viztool"]<-"SECTOR" #subset just acute diseased colonies
# colnames(site.demo)[colnames(site.demo)=="DB_RZ"]<-"STRATA" #subset just acute diseased colonies
# colnames(site.cover)[colnames(site.cover)=="PooledSector_Viztool"]<-"SECTOR" #subset just acute diseased colonies
# colnames(site.cover)[colnames(site.cover)=="DB_RZ"]<-"STRATA" #subset just acute diseased colonies

#Change sarigan, alamagan and guguan's island name
seclu$ISLAND<-ifelse(seclu$ISLAND %in% c("Sarigan","Alamagan","Guguan"),"Sarigan, Alamagan, Guguan",seclu$ISLAND)
seclu$ISLANDCODE<-ifelse(seclu$ISLAND %in% c("Sarigan, Alamagan, Guguan"),"SGA",seclu$ISLANDCODE)

#Remove Maro, Midway and Laysan since they aren't part of the NCRMP islands
remove<-c("Maro","Midway","Laysan")
seclu<-filter(seclu,! ISLAND %in% remove)
# site.cover<-filter(site.cover,! SECTOR %in% remove)
# site.demo<-filter(site.demo,! SECTOR %in% remove)


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


# nrow(site.demo)
# #add Strataname
# site.demo<-left_join(site.demo,st.lu) 
# site.demo<-left_join(site.demo,secname)
# nrow(site.demo)
#View(site.demo)

# nrow(site.cover)
#add Strataname
# colnames(site.cover)[colnames(site.cover)=="STRATAcode"]<-"STRATA"
# site.cover<-left_join(site.cover,st.lu) 
# site.cover<-left_join(site.cover,secname)
# nrow(site.cover)
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
# site.demo<-subset(site.demo,ANALYSIS_YEAR %in% years);View(site.demo)
# site.cover<-subset(site.cover,ANALYSIS_YEAR %in% years);View(site.cover)


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

# site.cover$SurveyYearStart<-CalcStartYr(site.cover)
# site.cover$SurveyYearEnd<-CalcEndYr(site.cover)
# site.cover$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(site.cover)

# site.demo$SurveyYearStart<-CalcStartYr(site.demo)
# site.demo$SurveyYearEnd<-CalcEndYr(site.demo)
# site.demo$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(site.demo)

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
# site.demo<-ChangeStrataCode(site.demo)
# site.cover<-ChangeStrataCode(site.cover)

demo.st<-ColumnNameChange(demo.st)
demo.sec<-ColumnNameChange(demo.sec)
demo.isl<-ColumnNameChange(demo.isl)
demo.r<-ColumnNameChange(demo.r)
#site.demo<-ColumnNameChange(site.demo)
#site.cover<-ColumnNameChange(site.cover)


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

# site.cover<-site.cover[,c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode", "SECTORname","SECTORcode","STRATAname","STRATAcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","SiteID",
#               "LATITUDE","LONGITUDE")]
# 
# site.demo<-site.demo[,c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode", "SECTORname","SECTORcode","STRATAname","STRATAcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","SiteID",
#               "LATITUDE","LONGITUDE")]

#Separate all hard coral taxa and functional cover into 1 set of csv files
st.all<-filter(demo.st,TaxonomicCode =="SSSS")
sec.all<-filter(demo.sec,TaxonomicCode =="SSSS")
isl.all<-filter(demo.isl,TaxonomicCode =="SSSS")
r.all<-filter(demo.r,TaxonomicCode =="SSSS")

st.all[c("TaxonomicResolution","TaxonomicCode","ScientificName")]<-matrix(rep(c("Com","ALL CORALS","ALL RECORDED CORALS"),nrow(st.all)),ncol=3,byrow =T) 
sec.all[c("TaxonomicResolution","TaxonomicCode","ScientificName")]<-matrix(rep(c("Com","ALL CORALS","ALL RECORDED CORALS"),nrow(sec.all)),ncol=3,byrow =T) 
isl.all[c("TaxonomicResolution","TaxonomicCode","ScientificName")]<-matrix(rep(c("Com","ALL CORALS","ALL RECORDED CORALS"),nrow(isl.all)),ncol=3,byrow =T) 
r.all[c("TaxonomicResolution","TaxonomicCode","ScientificName")]<-matrix(rep(c("Com","ALL CORALS","ALL RECORDED CORALS"),nrow(r.all)),ncol=3,byrow =T) 

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

####Add JoinKey
####
templ_str=read.csv("C:/Users/Thomas.Oliver/WORK/Projects/FY23/Viztool/PacBenthicStrat_Trend.csv")
templ_sec=read.csv("C:/Users/Thomas.Oliver/WORK/Projects/FY23/Viztool/PacBenthicSec_Trend.csv")
templ_isl=read.csv("C:/Users/Thomas.Oliver/WORK/Projects/FY23/Viztool/PacBenthicSubjur_Trend.csv")
templ_reg=read.csv("C:/Users/Thomas.Oliver/WORK/Projects/FY23/Viztool/PacBenthicJur_Trend.csv")


##################################################
#Develop Lookup Tables
##################################################
#REGION aka jurisdiction
REGION_JKlu=sort(c(unique(st.all$JURISDICTIONname)))#sort(unique(templ_reg$AOI))
names(REGION_JKlu)=sort(c(unique(st.all$JURISDICTIONname)))

#Island aka subjurisdiction
ISLAND_JKlu=sort(unique(templ_isl$AOI))
names(ISLAND_JKlu)=sort(c(unique(st.all$SUBJURISDICTIONname)))

#Sector: Lehua continues to be a pain
SEC_JKlu=sort(c(unique(templ_sec$AOI),"Maui; Hana",
                "Maui; Molokini","Maui; Northwest",
                "Niihau and Lehua; Lehua","Tutuila; PagoPago"),na.last = T)
names(SEC_JKlu)=sort(c(unique(st.all$SECTORname),"Lehua"))

#Strata (i.e. hab and depth)
STRATA_JKlu=c("Back reef, deep (18-30m)","Back reef, mid (6-18m)","Back reef, shallow (0-6m)",
              "Forereef, deep (18-30m)","Forereef, mid (6-18m)","Forereef, shallow (0-6m)",
              "Lagoon, deep (18-30m)","Lagoon, mid (6-18m)","Lagoon, shallow (0-6m)",
              "Protected slope, deep (18-30m)","Protected slope, mid (6-18m)","Protected slope, shallow (0-6m)")
names(STRATA_JKlu)=sort(unique(st.all$STRATAname))

#Scientific Name
SciName_JKlu=sort(c(unique(templ_str$ScientificName),"Millepora sp."))
names(SciName_JKlu)=sort(unique(c(st.all$ScientificName,st.gen$ScientificName)))


#Output Columns
OutputCols=c(names(templ_str)[1:10],"N_demoTREND","N_coverTREND",names(templ_str)[11:38])



#######
#Prep Strata Level Data For Join
#######
st.all$JoinKey=paste0("Strata","_",
                      SEC_JKlu[st.all$SECTORname],"; ",
                      STRATA_JKlu[st.all$STRATAname],"_",
                      st.all$TaxonomicCode,"_",
                      st.all$SurveyYearStart)
st.all$AOILabel=paste0(SEC_JKlu[st.all$SECTORname],"; ",
                      STRATA_JKlu[st.all$STRATAname])
st.all=st.all[,c("JoinKey","AOILabel",names(st.all)[!names(st.all)%in%c("JoinKey","AOILabel")])]

st.gen$JoinKey=paste0("Strata","_",
                      SEC_JKlu[st.gen$SECTORname],"; ",
                      STRATA_JKlu[st.gen$STRATAname],"_",
                      st.gen$TaxonomicCode,"_",
                      st.gen$SurveyYearStart)
st.gen$AOILabel=paste0(SEC_JKlu[st.gen$SECTORname],"; ",
                       STRATA_JKlu[st.gen$STRATAname])
st.gen=st.gen[,c("JoinKey","AOILabel",names(st.gen)[!names(st.gen)%in%c("JoinKey","AOILabel")])]

#Merge 'em
st.gen$TaxonomicRes="Gen"
st.all$TaxonomicRes="Com"
st.both=rbind(st.gen,st.all)

#Modify SciName
st.both$ScientificName=SciName_JKlu[st.both$ScientificName]

#Check Survey Years
st.both$SurveyYearEnd=as.integer(st.both$SurveyYearEnd)
st.both$SurveyYearStart=as.integer(st.both$SurveyYearStart)
st.both$MonitoringCycle=st.both$SurveyYearStart

# #Check Pria "ANALYSIS YEARS"
# st.both$SurveyYearEnd[which(st.both$JURISDICTIONcode=="PRIAs")]=
#   st.both$SurveyYearStart[which(st.both$JURISDICTIONcode=="PRIAs")]+1


# ###Test JoinKey Match Ups
# DataKeys_AC=unique(st.all$JoinKey)
# TempKeys_AC=templ_str %>% 
#   filter(ScientificName=="ALL RECORDED CORALS") %>% 
#   pull(JoinKey) %>% unique()
# 
# InBoth_AC=intersect(DataKeys_AC,TempKeys_AC);length(InBoth_AC)
# InData_AC=sort(setdiff(DataKeys_AC,TempKeys_AC));length(InData_AC)
# InTempl_AC=sort(setdiff(TempKeys_AC,DataKeys_AC));length(InTempl_AC)
# 
# DataKeys=unique(c(st.all$JoinKey,st.gen$JoinKey))
# TempKeys=unique(templ_str$JoinKey)
# 
# InBoth=intersect(DataKeys,unique(templ_str$JoinKey));length(InBoth)
# InData=sort(setdiff(DataKeys,unique(templ_str$JoinKey)));length(InData)
# InTempl=sort(setdiff(unique(templ_str$JoinKey),DataKeys));length(InTempl)
# 
# StrIsl=unique(substr(InData,1,regexpr(InData,pattern = ";")))

#Get Join Ready - By Modifying the Column Names to Match the Template
st.both.rn=st.both %>% rename(
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
  JuvenileDensityTREND_SE=JuvenileDensity_SE,
  N_demoTREND=N_Demo,
  N_coverTREND=N_Cover)


#Get Column Names in and Out For Join
TREND_cols=names(templ_str)[grep(pattern = "TREND",x = names(templ_str))]
join_cols=c("JoinKey","AOILabel","SurveyYearStart","SurveyYearEnd","MonitoringCycle",
            "ScientificName","TaxonomicCode","TaxonomicRes")
datajoin_names=intersect(names(st.both.rn),OutputCols);datajoin_names

# #Note there are many Repeated Join Keys In Both Data and the Template - i.e. Strata that should be pooled, not pooled
# Ttt=table(templ_str$JoinKey)
# dTtt=Ttt[which(Ttt>1)];length(dTtt)
# unique(substr(names(dTtt),8,6+regexpr(pattern = "_",text = substr(names(dTtt),8,9999))))
# Dtt=table(st.both.rn$JoinKey)
# dDtt=Dtt[which(Dtt>1)];length(dDtt)
# unique(substr(names(dDtt),8,6+regexpr(pattern = "_",text = substr(names(dDtt),8,9999))))

#Knowing there are repeated JoinKeys
# Ltt=table(templ_str_Ljoin$JoinKey)
# dLtt=Ltt[which(Ltt>1)];length(dLtt)
# hist(dLtt)
# templ_str_Ljoin[which(templ_str_Ljoin$JoinKey==names(dLtt[1])),]
# 
# Rtt=table(templ_str_Rjoin$JoinKey)
# length(which(Rtt>1))
# 
# intersect(which(!is.na(templ_str_join$CoralCover_Pct)),which(!is.na(templ_str_join$CoralCoverTREND_Pct)))
# setdiff(which(!is.na(templ_str_join$CoralCover_Pct)),which(!is.na(templ_str_join$CoralCoverTREND_Pct)))
# setdiff(which(!is.na(templ_str_join$CoralCoverTREND_Pct)),which(!is.na(templ_str_join$CoralCover_Pct)))
# 
# setdiff(names(templ_str),names(templ_str_join))
# setdiff(names(templ_str_join),names(templ_str))

templ_str_Ljoin= left_join(x=templ_str[,-which(names(templ_str)%in%TREND_cols)],
                           y=st.both.rn[,datajoin_names],
                           by=join_cols,
                           type = "full");dim(templ_str_Ljoin)
templ_str_Ljoin=templ_str_Ljoin[,OutputCols]
write.csv(x = templ_str_Ljoin,row.names = FALSE,
          file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/For Submission/April 2023 Submission/PacificBenthicDataSummaryTable_STRATA_LEFTJOIN_byGenus_Apr2023.csv")

templ_str_Rjoin= right_join(x=templ_str[,-which(names(templ_str)%in%TREND_cols)],
                            y=st.both.rn[,datajoin_names],
                            by=join_cols,
                            type = "full");dim(templ_str_Rjoin)
templ_str_Rjoin=templ_str_Rjoin[,OutputCols]
write.csv(x = templ_str_Rjoin,row.names = FALSE,
          file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/For Submission/April 2023 Submission/PacificBenthicDataSummaryTable_STRATA_RIGHTJOIN_byGenus_Apr2023.csv")

templ_str_ALLjoin= join_all(dfs = list(templ_str[,-which(names(templ_str)%in%TREND_cols)],
                                       st.both.rn[,datajoin_names]),
                            by=join_cols,
                            type = "full");dim(templ_str_ALLjoin)
templ_str_ALLjoin=templ_str_ALLjoin[,OutputCols]
write.csv(x = templ_str_ALLjoin,row.names = FALSE,
          file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/For Submission/April 2023 Submission/PacificBenthicDataSummaryTable_STRATA_FULLJOIN_byGenus_Apr2023.csv")

#######
#Prep Sector Level Data For Join
#######
sec.all$JoinKey=paste0("Sector","_",
                      SEC_JKlu[sec.all$SECTORname],"_",
                      sec.all$TaxonomicCode,"_",
                      sec.all$SurveyYearStart)
sec.all$AOILabel=paste0(SEC_JKlu[sec.all$SECTORname])

sec.all=sec.all[,c("JoinKey","AOILabel",names(sec.all)[!names(sec.all)%in%c("JoinKey","AOILabel")])]

sec.gen$JoinKey=paste0("Strata","_",
                      SEC_JKlu[sec.gen$SECTORname],"; ",
                      STRATA_JKlu[sec.gen$STRATAname],"_",
                      sec.gen$TaxonomicCode,"_",
                      sec.gen$SurveyYearStart)
sec.gen$AOILabel=paste0(SEC_JKlu[sec.gen$SECTORname])
sec.gen=sec.gen[,c("JoinKey","AOILabel",names(sec.gen)[!names(sec.gen)%in%c("JoinKey","AOILabel")])]

#Merge 'em
sec.gen$TaxonomicRes="Gen"
sec.all$TaxonomicRes="Com"
sec.both=rbind(sec.gen,sec.all)

#Modify SciName
sec.both$ScientificName=SciName_JKlu[sec.both$ScientificName]

#Check Survey Years
sec.both$SurveyYearEnd=as.integer(sec.both$SurveyYearEnd)
sec.both$SurveyYearStart=as.integer(sec.both$SurveyYearStart)
sec.both$MonitoringCycle=sec.both$SurveyYearStart

#Get Join Ready - By Modifying the Column Names to Match the Template
sec.both.rn=sec.both %>% rename(
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
  JuvenileDensityTREND_SE=JuvenileDensity_SE,
  N_demoTREND=N_Demo,
  N_coverTREND=N_Cover)

templ_sec_Ljoin= left_join(x=templ_sec[,-which(names(templ_sec)%in%TREND_cols)],
                           y=sec.both.rn[,datajoin_names],
                           by=join_cols);dim(templ_sec_Ljoin)
templ_sec_Ljoin=templ_sec_Ljoin[,OutputCols]
write.csv(x = templ_sec_Ljoin,row.names = FALSE,
          file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/For Submission/April 2023 Submission/PacificBenthicDataSummaryTable_SECTOR_LEFTJOIN_byGenus_Apr2023.csv")

templ_sec_Rjoin= right_join(x=templ_sec[,-which(names(templ_sec)%in%TREND_cols)],
                            y=sec.both.rn[,datajoin_names],
                            by=join_cols);dim(templ_sec_Rjoin)
templ_sec_Rjoin=templ_sec_Rjoin[,OutputCols]
write.csv(x = templ_sec_Rjoin,row.names = FALSE,
          file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/For Submission/April 2023 Submission/PacificBenthicDataSummaryTable_SECTOR_RIGHTJOIN_byGenus_Apr2023.csv")

templ_sec_ALLjoin= join_all(dfs = list(templ_sec[,-which(names(templ_sec)%in%TREND_cols)],
                                       sec.both.rn[,datajoin_names]),
                            by=join_cols,
                            type = "full");dim(templ_sec_Rjoin)

templ_sec_ALLjoin=templ_sec_ALLjoin[,OutputCols]
write.csv(x = templ_sec_ALLjoin,row.names = FALSE,
          file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/For Submission/April 2023 Submission/PacificBenthicDataSummaryTable_SECTOR_FULLJOIN_byGenus_Apr2023.csv")

#######
#Prep Island Level Data For Join
#######
isl.all$JoinKey=paste0("Subjurisdiction","_",
                       ISLAND_JKlu[isl.all$SUBJURISDICTIONname],"_",
                       isl.all$TaxonomicCode,"_",
                       isl.all$SurveyYearStart)

isl.all$AOILabel=paste0(ISLAND_JKlu[isl.all$SUBJURISDICTIONname])

isl.all=isl.all[,c("JoinKey","AOILabel",names(isl.all)[!names(isl.all)%in%c("JoinKey","AOILabel")])]

isl.gen$JoinKey=paste0("Subjurisdiction","_",
                       ISLAND_JKlu[isl.gen$SUBJURISDICTIONname],"_",
                       isl.gen$TaxonomicCode,"_",
                       isl.gen$SurveyYearStart)
isl.gen$AOILabel=paste0(ISLAND_JKlu[isl.gen$SUBJURISDICTIONname])
isl.gen=isl.gen[,c("JoinKey","AOILabel",names(isl.gen)[!names(isl.gen)%in%c("JoinKey","AOILabel")])]

#Merge 'em
isl.gen$TaxonomicRes="Gen"
isl.all$TaxonomicRes="Com"
isl.both=rbind(isl.gen,isl.all)

#Modify SciName
isl.both$ScientificName=SciName_JKlu[isl.both$ScientificName]

#Check Survey Years
isl.both$SurveyYearEnd=as.integer(isl.both$SurveyYearEnd)
isl.both$SurveyYearStart=as.integer(isl.both$SurveyYearStart)
isl.both$MonitoringCycle=isl.both$SurveyYearStart


#Get Join Ready - By Modifying the Column Names to Match the Template
isl.both.rn=isl.both %>% rename(
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
  JuvenileDensityTREND_SE=JuvenileDensity_SE,
  N_demoTREND=N_Demo,
  N_coverTREND=N_Cover)


templ_isl_Ljoin= left_join(x=templ_isl[,-which(names(templ_isl)%in%TREND_cols)],
                           y=isl.both.rn[,datajoin_names],
                           by=join_cols);dim(templ_isl_Ljoin)
templ_isl_Ljoin=templ_isl_Ljoin[,OutputCols]
write.csv(x = templ_isl_Ljoin,row.names = FALSE,
          file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/For Submission/April 2023 Submission/PacificBenthicDataSummaryTable_ISLAND_LEFTJOIN_byGenus_Apr2023.csv")


templ_isl_Rjoin= right_join(x=templ_isl[,-which(names(templ_isl)%in%TREND_cols)],
                            y=isl.both.rn[,datajoin_names],
                            by=join_cols);dim(templ_isl_Rjoin)
templ_isl_Rjoin=templ_isl_Rjoin[,OutputCols]
write.csv(x = templ_isl_Rjoin,row.names = FALSE,
          file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/For Submission/April 2023 Submission/PacificBenthicDataSummaryTable_ISLAND_RIGHTJOIN_byGenus_Apr2023.csv")

templ_isl_ALLjoin= right_join(x=templ_isl[,-which(names(templ_isl)%in%TREND_cols)],
                              y=isl.both.rn[,datajoin_names],
                              by=join_cols);dim(templ_isl_Rjoin)
templ_isl_ALLjoin=templ_isl_ALLjoin[,OutputCols]
write.csv(x = templ_isl_ALLjoin,row.names = FALSE,
          file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/For Submission/April 2023 Submission/PacificBenthicDataSummaryTable_ISLAND_FULLJOIN_byGenus_Apr2023.csv")

#######
#Prep Region Level Data For Join
#######
r.all$JoinKey=paste0("Jurisdiction","_",
                       REGION_JKlu[r.all$JURISDICTIONname],"_",
                       r.all$TaxonomicCode,"_",
                       r.all$SurveyYearStart)
r.all$AOILabel=paste0(REGION_JKlu[r.all$JURISDICTIONname])

r.all=r.all[,c("JoinKey","AOILabel",names(r.all)[!names(r.all)%in%c("JoinKey","AOILabel")])]

r.gen$JoinKey=paste0("Jurisdiction","_",
                     REGION_JKlu[r.gen$JURISDICTIONname],"_",
                     r.gen$TaxonomicCode,"_",
                     r.gen$SurveyYearStart)
r.gen$AOILabel=paste0(REGION_JKlu[r.gen$JURISDICTIONname])
r.gen=r.gen[,c("JoinKey","AOILabel",names(r.gen)[!names(r.gen)%in%c("JoinKey","AOILabel")])]

#Merge 'em
r.gen$TaxonomicRes="Gen"
r.all$TaxonomicRes="Com"
r.both=rbind(r.gen,r.all)

#Modify SciName
r.both$ScientificName=SciName_JKlu[r.both$ScientificName]

#Check Survey Years
r.both$SurveyYearEnd=as.integer(r.both$SurveyYearEnd)
r.both$SurveyYearStart=as.integer(r.both$SurveyYearStart)
r.both$MonitoringCycle=r.both$SurveyYearStart


#Get Join Ready - By Modifying the Column Names to Match the Template
r.both.rn=r.both %>% rename(
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
  JuvenileDensityTREND_SE=JuvenileDensity_SE,
  N_demoTREND=N_Demo,
  N_coverTREND=N_Cover)



templ_reg_Ljoin= left_join(x=templ_reg[,-which(names(templ_reg)%in%TREND_cols)],
                           y=r.both.rn[,datajoin_names],
                           by=join_cols);dim(templ_reg_Ljoin)
templ_reg_Ljoin=templ_reg_Ljoin[,OutputCols]
write.csv(x = templ_reg_Ljoin,row.names = FALSE,
          file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/For Submission/April 2023 Submission/PacificBenthicDataSummaryTable_REGION_LEFTJOIN_byGenus_Apr2023.csv")

templ_reg_Rjoin= right_join(x=templ_reg[,-which(names(templ_reg)%in%TREND_cols)],
                            y=r.both.rn[,datajoin_names],
                            by=join_cols);dim(templ_reg_Rjoin)
templ_reg_Rjoin=templ_reg_Rjoin[,OutputCols]
write.csv(x = templ_reg_Rjoin,row.names = FALSE,
          file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/For Submission/April 2023 Submission/PacificBenthicDataSummaryTable_REGION_RIGHTJOIN_byGenus_Apr2023.csv")

templ_reg_ALLjoin= right_join(x=templ_reg[,-which(names(templ_reg)%in%TREND_cols)],
                              y=r.both.rn[,datajoin_names],
                              by=join_cols);dim(templ_reg_Rjoin)
templ_reg_ALLjoin=templ_reg_ALLjoin[,OutputCols]
write.csv(x = templ_reg_ALLjoin,row.names = FALSE,
          file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/For Submission/April 2023 Submission/PacificBenthicDataSummaryTable_REGION_FULLJOIN_byGenus_Apr2023.csv")


# write.csv(st.all,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/formatted/PacificNCRMPviztool2022_STRATA_All_Taxa_TRENDS_Apr2023.csv",row.names = F)
# write.csv(sec.all,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/formatted/PacificNCRMPviztool2022_SECTOR_All_Taxa_TRENDS_Apr2023.csv",row.names = F)
# write.csv(isl.all,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/formatted/PacificNCRMPviztool2022_ISLAND_All_Taxa_TRENDS_Apr2023.csv",row.names = F)
# write.csv(r.all,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/formatted/PacificNCRMPviztool2022_REGION_All_Taxa_TRENDS_Apr2023.csv",row.names = F)
# 
# write.csv(st.gen,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/formatted/PacificNCRMPviztool2022_STRATA_By_Genus_TRENDS_Apr2023.csv",row.names = F)
# write.csv(sec.gen,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/formatted/PacificNCRMPviztool2022_SECTOR_By_Genus_TRENDS_Apr2023.csv",row.names = F)
# write.csv(isl.gen,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/formatted/PacificNCRMPviztool2022_ISLAND_By_Genus_TRENDS_Apr2023.csv",row.names = F)
# write.csv(r.gen,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/formatted/PacificNCRMPviztool2022_REGION_By_Genus_TRENDS_Apr2023.csv",row.names = F)

# write.csv(site.demo,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/formatted/PacificNCRMPviztool2022_SITE_demo.csv",row.names = F)
# write.csv(site.cover,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/formatted/PacificNCRMPviztool2022_SITE_cover.csv",row.names = F)

# Check out distinctions
LL=templ_reg_Ljoin %>% filter(TaxonomicRes=="Com")
ALL=templ_reg_ALLjoin %>% filter(TaxonomicRes=="Com")
dim(LL)
dim(ALL)
setdiff(LL$JoinKey,ALL$JoinKey)

tLL=table(LL$JoinKey)
tALL=table(ALL$JoinKey)
max(tLL)
tNE=tALL[which(tALL>1)]

#ISL
LL=templ_isl_Ljoin %>% filter(TaxonomicRes=="Com")
ALL=templ_isl_ALLjoin %>% filter(TaxonomicRes=="Com")
dim(LL)
dim(ALL)
setdiff(LL$JoinKey,ALL$JoinKey)

tLL=table(LL$JoinKey)
tALL=table(ALL$JoinKey)
max(tLL)
tNE=tALL[which(tALL>1)]
tNE


#SEC
LL=templ_sec_Ljoin %>% filter(TaxonomicRes=="Com")
ALL=templ_sec_ALLjoin %>% filter(TaxonomicRes=="Com")
dim(LL)
dim(ALL)
setdiff(LL$JoinKey,ALL$JoinKey)

tLL=table(LL$JoinKey)
tALL=table(ALL$JoinKey)
max(tLL)
tNE=tALL[which(tALL>1)]
tNE
dim(ALL)[1]-dim(LL)[1]
sum(tNE)
unique(substr(names(tNE),8,str_locate(pattern = "_",string = names(tNE))[,1]-1))


#STR
LL=templ_str_Ljoin %>% filter(TaxonomicRes=="Com")
ALL=templ_str_ALLjoin %>% filter(TaxonomicRes=="Com")
dim(LL)
dim(ALL)
setdiff(LL$JoinKey,ALL$JoinKey)

tLL=table(LL$JoinKey)
tALL=table(ALL$JoinKey)
max(tLL)
tNE=tALL[which(tALL>1)]
tNE
dim(ALL)[1]-dim(LL)[1]
unique(substr(names(tNE),8,str_locate(pattern = ";",string = names(tNE))[,1]-1))
