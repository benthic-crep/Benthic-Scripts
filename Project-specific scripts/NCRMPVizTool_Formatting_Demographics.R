#read in Tier1 summary tables from Benthic Cover_RawtoEstimates_NCRMPViztool.R script


rm(list=ls())

#LOAD LIBRARY FUNCTIONS ...
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")


#Read in data ####
st<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicREA_stratadata_GENUS.csv")
st<-st[,c("REGION","ISLAND", "SECTOR","DB_RZ","ANALYSIS_YEAR","n","GENUS_CODE","AdColDen","JuvColDen","Mean_BLE_Prev","Mean_TotDZ_Prev","SE_AdColDen","SE_JuvColDen","SE_BLE_Prev","SE_TotDZ_Prev")]
colnames(st)[colnames(st)=="DB_RZ"]<-"STRATA"
colnames(st)[colnames(st)=="n"]<-"N"


sec<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicREA_sectordata_GENUS.csv")
sec<-sec[,c("REGION","PooledSector_Demo_Viztool","ANALYSIS_YEAR","n","GENUS_CODE","Mean_AdColDen","Mean_JuvColDen","Mean_BLE_Prev","Mean_TotDZ_Prev","SE_AdColDen","SE_JuvColDen","SE_BLE_Prev","SE_TotDZ_Prev")]
colnames(sec)[colnames(sec)=="PooledSector_Demo_Viztool"]<-"SECTOR"
colnames(sec)[colnames(sec)=="n"]<-"N"

isl<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicREA_islanddata_GENUS.csv")
isl<-isl[,c("REGION","ISLAND","ANALYSIS_YEAR","n","GENUS_CODE","Mean_AdColDen","Mean_JuvColDen","Mean_BLE_Prev","Mean_TotDZ_Prev","SE_AdColDen","SE_JuvColDen","SE_BLE_Prev","SE_TotDZ_Prev")]
colnames(isl)[colnames(isl)=="n"]<-"N"

r<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Region/BenthicREA_regiondata_GENUS.csv")
r<-r[,c("REGION","ANALYSIS_YEAR","n","GENUS_CODE","Mean_AdColDen","Mean_JuvColDen","Mean_BLE_Prev","Mean_TotDZ_Prev","SE_AdColDen","SE_JuvColDen","SE_BLE_Prev","SE_TotDZ_Prev")]
colnames(r)[colnames(r)=="n"]<-"N"


site<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/2022ViztoolSites_Demo.csv")
site<-unique(site[,c("REGION","PooledSector_Demo_Viztool","DB_RZ","ANALYSIS_YEAR","SITEVISITID","SITE","LATITUDE","LONGITUDE")])
head(site)


#Read in look up table of all possible strata and sector names ####
seclu<-read.csv("T:/Benthic/Data/Lookup Tables/PacificNCRMP_Benthic_Sectors_Lookup_v4.csv") #Look up for pooling scheme and table of strata names
secname<-read.csv("T:/Benthic/Data/Lookup Tables/SectorNamelookup.csv") #list of all sectors (both SEC_NAME and different pooled sectors)-look up table of sector names

#Change REGION and REGION_NAME for MARIAN for Viztool
seclu <- seclu %>% mutate(seclu,
                              REGION= case_when(
                              ISLAND =="Guam" ~ "GUA",
                              REGION == "MARIAN" & ISLAND !="Guam" ~ "CNMI",
                              TRUE ~ REGION))
seclu <- seclu %>% mutate(seclu,
                          REGION_NAME= case_when(
                            ISLAND =="Guam" ~ "Guam",
                            REGION == "CNMI"~ "Commonwealth of the Northern Mariana Islands",
                            TRUE ~ REGION_NAME))

secname <- secname %>% mutate(secname,
                          REGION= case_when(
                            ISLAND =="Guam" ~ "GUA",
                            REGION == "MARIAN" & ISLAND !="Guam" ~ "CNMI",
                            TRUE ~ REGION))
secname <- secname %>% mutate(secname,
                          REGION_NAME= case_when(
                            ISLAND =="Guam" ~ "Guam",
                            REGION == "CNMI"~ "Commonwealth of the Northern Mariana Islands",
                            TRUE ~ REGION_NAME))
#Change column names to match data
colnames(secname)[colnames(secname)=="SECTOR"]<-"SECTOR" 
colnames(seclu)[colnames(seclu)=="PooledSector_Demo_Viztool"]<-"SECTOR" #subset just acute diseased colonies
colnames(seclu)[colnames(seclu)=="STRATAcode"]<-"STRATA" #subset just acute diseased colonies
colnames(site)[colnames(site)=="PooledSector_Demo_Viztool"]<-"SECTOR" #subset just acute diseased colonies
colnames(site)[colnames(site)=="DB_RZ"]<-"STRATA" #subset just acute diseased colonies

#Change sarigan, alamagan and guguan's island name
seclu$ISLAND<-ifelse(seclu$ISLAND %in% c("Sarigan","Alamagan","Guguan"),"Sarigan, Alamagan, Guguan",seclu$ISLAND)
seclu$ISLANDCODE<-ifelse(seclu$ISLAND %in% c("Sarigan, Alamagan, Guguan"),"SGA",seclu$ISLANDCODE)

#Remove Maro, Midway and Laysan since they aren't part of the NCRMP islands
remove<-c("Maro","Midway","Laysan")
seclu<-filter(seclu,! ISLAND %in% remove)
site<-filter(site,! SECTOR %in% remove)



#Merge sector and strata names to SITE data
st.lu<-unique(seclu[,c("REGION_NAME","REGION","ISLAND","ISLANDCODE","SECTOR","STRATAname","STRATA")]) #make sure to use unique because we are pooling certain sectors together (e.g. Tut_aunuu)

nrow(site.demo)
#add Strataname
site.demo<-left_join(site.demo,st.lu) 
site.demo<-left_join(site.demo,secname)
nrow(site.demo)
View(site.demo)


#Include all possible STRATA and add NA values for the strata that weren't sampled each year ####

st.lu<-unique(seclu[,c("REGION_NAME","REGION","ISLAND","ISLANDCODE","SECTOR","STRATAname","STRATA")]) #make sure to use unique because we are pooling certain sectors together (e.g. Tut_aunuu)

#add Strataname
st<-left_join(st,st.lu) 

#Create list of all possible strata and analysis year
st.lu_years <- st.lu %>% left_join(st %>% distinct(REGION, ANALYSIS_YEAR,GENUS_CODE), by = c("REGION"))

demo.st<-full_join(st,st.lu_years) %>%
          left_join(secname) %>%
          mutate(N = replace_na(N,0))

demo.st<-filter(demo.st, !ISLANDCODE %in% c("SAR","GUG","ALA"))
View(demo.st)


demo.st <- demo.st %>% mutate(demo.st,
         STRATAname= case_when(
         STRATA =="FS" ~ "Forereef Shallow",
         STRATA =="FM" ~ "Forereef Mid",
         STRATA =="FD" ~ "Forereef Deep",
         TRUE ~ STRATAname))


#Include all possible SECTORS and add NA values for the sectors that weren't sampled each year ####

sec.lu<-unique(seclu[,c("REGION_NAME","REGION","ISLAND","ISLANDCODE","SECTOR")]) #make sure to use unique because we are pooling certain sectors together (e.g. Tut_aunuu)

sec<-left_join(sec,sec.lu) 

#Create list of all possible strata and analysis year
sec.lu_years <- sec.lu %>% left_join(sec %>% distinct(REGION, ANALYSIS_YEAR,GENUS_CODE), by = c("REGION"))

demo.sec<-full_join(sec,sec.lu_years) %>%
  left_join(secname) %>%
  mutate(N = replace_na(N,0))

demo.sec<-filter(demo.sec, !ISLANDCODE %in% c("SAR","GUG","ALA"))
View(demo.sec)


#Include all possible ISLANDS and add NA values for the islands that weren't sampled each year ####

isl.lu<-unique(seclu[,c("REGION_NAME","REGION","ISLAND","ISLANDCODE")])

isl<-left_join(isl,isl.lu) 

#Create list of all possible strata and analysis year
isl.lu_years <- isl.lu %>% left_join(isl %>% distinct(REGION, ANALYSIS_YEAR,GENUS_CODE), by = c("REGION"))

demo.isl<-full_join(isl,isl.lu_years) %>%
  mutate(N = replace_na(N,0))

demo.isl<-filter(demo.isl, !ISLANDCODE %in% c("SAR","GUG","ALA"))
View(demo.isl)


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

#Add region names to the regional dataframe
demo.r<-r %>% mutate(REGION_NAME=recode(REGION,
                                        `GUA`="Guam",
                                        `CNMI`="Commonwealth of the Northern Mariana Islands",
                                        `MHI`="Main Hawaiian Islands",
                                        `NWHI`="Northwestern Hawaiian Islands",
                                        `SAMOA`="American Samoa",
                                        `PRIAs`="Pacific Remote Island Areas"))




#remove years before 2013
years<-c("2013","2013-15","2014-15","2014","2015","2016","2015-16","2017-18","2017","2018","2019")

demo.st<-subset(demo.st,ANALYSIS_YEAR %in% years);View(demo.st)
demo.sec<-subset(demo.sec,ANALYSIS_YEAR %in% years);View(demo.sec)
demo.isl<-subset(demo.isl,ANALYSIS_YEAR %in% years);View(demo.isl)
demo.r<-subset(demo.r,ANALYSIS_YEAR %in% years);View(demo.r)
site<-subset(site,ANALYSIS_YEAR %in% years);View(site)


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

site$SurveyYearStart<-CalcStartYr(site)
site$SurveyYearEnd<-CalcEndYr(site)
site$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(site)

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
site<-ChangeStrataCode(site)

demo.st<-ColumnNameChange(demo.st)
demo.sec<-ColumnNameChange(demo.sec)
demo.isl<-ColumnNameChange(demo.isl)
demo.r<-ColumnNameChange(demo.r)
site<-ColumnNameChange(site)


demo.st<-demo.st[,c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode", "SECTORname","SECTORcode","STRATAname","STRATAcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N","TaxonomicResolution","TaxonomicCode","ScientificName",
                    "AdultDensity_colperm2","JuvenileDensity_colperm2","BleachingPrevalence_Pct","DiseasePrevalence_Pct","AdultDensity_SE",         
                    "JuvenileDensity_SE","BleachingPrevalence_SE","DiseasePrevalence_SE")]
demo.sec<-demo.sec[,c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode", "SECTORname","SECTORcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N",
                        "TaxonomicResolution","TaxonomicCode","ScientificName","AdultDensity_colperm2","JuvenileDensity_colperm2","BleachingPrevalence_Pct","DiseasePrevalence_Pct","AdultDensity_SE",         
                      "JuvenileDensity_SE","BleachingPrevalence_SE","DiseasePrevalence_SE")]
demo.isl<-demo.isl[,c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N",
                        "TaxonomicResolution","TaxonomicCode","ScientificName","AdultDensity_colperm2","JuvenileDensity_colperm2","BleachingPrevalence_Pct","DiseasePrevalence_Pct","AdultDensity_SE",         
                      "JuvenileDensity_SE","BleachingPrevalence_SE","DiseasePrevalence_SE")]
demo.r<-demo.r[,c("JURISDICTIONname","JURISDICTIONcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N",
                    "TaxonomicResolution","TaxonomicCode","ScientificName","AdultDensity_colperm2","JuvenileDensity_colperm2","BleachingPrevalence_Pct","DiseasePrevalence_Pct","AdultDensity_SE",         
                  "JuvenileDensity_SE","BleachingPrevalence_SE","DiseasePrevalence_SE")]
site<-site[,c("JURISDICTIONname","JURISDICTIONcode","SUBJURISDICTIONname","SUBJURISDICTIONcode", "SECTORname","SECTORcode","STRATAname","STRATAcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","SiteID",
              "LATITUDE","LONGITUDE")]
# 
write.csv(demo.st,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/PacificNCRMPviztool2022_STRATA_demo_v2.csv",row.names = F)
write.csv(demo.sec,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/PacificNCRMPviztool2022_SECTOR_demo_v2.csv",row.names = F)
write.csv(demo.isl,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/PacificNCRMPviztool2022_ISLAND_demo_v2.csv",row.names = F)
write.csv(demo.r,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/PacificNCRMPviztool2022_REGION_demo_v2.csv",row.names = F)
write.csv(site,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/PacificNCRMPviztool2022_SITE_demo_v2.csv",row.names = F)

