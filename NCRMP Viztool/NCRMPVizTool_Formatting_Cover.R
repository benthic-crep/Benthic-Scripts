#read in Tier1 summary tables from Benthic Cover_RawtoEstimates_NCRMPViztool.R script


rm(list=ls())

#LOAD LIBRARY FUNCTIONS ...
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")


#Read in data ####
st<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicCover_2010-2019_Tier1_STRATA_v2.csv")
st<-st[,c("REGION_NAME","REGION","ISLAND","ISLANDCODE", "ANALYSIS_SEC","STRATA","ANALYSIS_YEAR","N","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]

sec<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicCover_2010-2019_Tier1_SECTOR_v2.csv")
sec<-sec[,c("REGION_NAME","REGION","ISLAND","ISLANDCODE", "ANALYSIS_SEC","ANALYSIS_YEAR","N","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]

isl<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicCover_2010-2019_Tier1_ISLAND_v2.csv")
isl<-isl[,c("REGION_NAME","REGION","ISLAND","ISLANDCODE","ANALYSIS_YEAR","N","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]

r<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Region/BenthicCover_2010-2019_Tier1_REGION_v2.csv")
r<-r[,c("REGION","ANALYSIS_YEAR","N","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]

site<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/2022ViztoolSites_Cover.csv")
site<-site[,c("REGION","PooledSector_Cover_Viztool","STRATAcode","ANALYSIS_YEAR","SITEVISITID","SITE","LATITUDE","LONGITUDE")]

#Read in look up table of all possible strata and sector names ####
seclu<-read.csv("T:/Benthic/Data/Lookup Tables/PacificNCRMP_Benthic_Sectors_Lookup_v4.csv") #Look up for pooling scheme and table of strata names
secname<-read.csv("T:/Benthic/Data/Lookup Tables/SectorNamelookup.csv") #list of all sectors (both SEC_NAME and different pooled sectors)-look up table of sector names

#Change column names to match data
colnames(secname)[colnames(secname)=="SECTOR"]<-"ANALYSIS_SEC" 
colnames(seclu)[colnames(seclu)=="PooledSector_Cover_Viztool"]<-"ANALYSIS_SEC" #subset just acute diseased colonies
colnames(seclu)[colnames(seclu)=="STRATAcode"]<-"STRATA" #subset just acute diseased colonies
colnames(site)[colnames(site)=="STRATAcode"]<-"STRATA" #subset just acute diseased colonies
colnames(site)[colnames(site)=="PooledSector_Cover_Viztool"]<-"ANALYSIS_SEC" #subset just acute diseased colonies

#Change sarigan, alamagan and guguan's island name
seclu$ISLAND<-ifelse(seclu$ISLAND %in% c("Sarigan","Alamagan","Guguan"),"Sarigan, Alamagan, Guguan",seclu$ISLAND)
seclu$ISLANDCODE<-ifelse(seclu$ISLAND %in% c("Sarigan, Alamagan, Guguan"),"SGA",seclu$ISLANDCODE)

#Remove Maro, Midway and Laysan since they aren't part of the NCRMP islands
remove<-c("Maro","Midway","Laysan")
seclu<-filter(seclu,! ISLAND %in% remove)
site<-filter(site,! ANALYSIS_SEC %in% remove)


#Merge sector and strata names to SITE data
st.lu<-unique(seclu[,c("REGION_NAME","REGION","ISLAND","ISLANDCODE","ANALYSIS_SEC","STRATAname","STRATA")]) #make sure to use unique because we are pooling certain sectors together (e.g. Tut_aunuu)

nrow(site)
#add Strataname
site<-left_join(site,st.lu) 
site<-left_join(site,secname)
nrow(site)
View(site)

#Include all possible STRATA and add NA values for the strata that weren't sampled each year ####

st.lu<-unique(seclu[,c("REGION_NAME","REGION","ISLAND","ISLANDCODE","ANALYSIS_SEC","STRATAname","STRATA")]) #make sure to use unique because we are pooling certain sectors together (e.g. Tut_aunuu)

#add Strataname
st<-left_join(st,st.lu) 

#Create list of all possible strata and analysis year
st.lu_years <- st.lu %>% left_join(st %>% distinct(REGION, ANALYSIS_YEAR), by = c("REGION"))

cover.st<-full_join(st,st.lu_years) %>%
          left_join(secname) %>%
          mutate(N = replace_na(N,0))

cover.st<-filter(cover.st, !ISLANDCODE %in% c("SAR","GUG","ALA"))
View(cover.st)


cover.st <- cover.st %>% mutate(cover.st,
         STRATAname= case_when(
         STRATA =="FS" ~ "Forereef Shallow",
         STRATA =="FM" ~ "Forereef Mid",
         STRATA =="FD" ~ "Forereef Deep",
         TRUE ~ STRATAname))


#Include all possible SECTORS and add NA values for the sectors that weren't sampled each year ####

sec.lu<-unique(seclu[,c("REGION_NAME","REGION","ISLAND","ISLANDCODE","ANALYSIS_SEC")]) #make sure to use unique because we are pooling certain sectors together (e.g. Tut_aunuu)

sec<-left_join(sec,sec.lu) 

#Create list of all possible strata and analysis year
sec.lu_years <- sec.lu %>% left_join(sec %>% distinct(REGION, ANALYSIS_YEAR), by = c("REGION"))

cover.sec<-full_join(sec,sec.lu_years) %>%
  left_join(secname) %>%
  mutate(N = replace_na(N,0))

cover.sec<-filter(cover.sec, !ISLANDCODE %in% c("SAR","GUG","ALA"))
View(cover.sec)


#Include all possible ISLANDS and add NA values for the islands that weren't sampled each year ####

isl.lu<-unique(seclu[,c("REGION_NAME","REGION","ISLAND","ISLANDCODE")])

isl<-left_join(isl,isl.lu) 

#Create list of all possible strata and analysis year
isl.lu_years <- isl.lu %>% left_join(isl %>% distinct(REGION, ANALYSIS_YEAR), by = c("REGION"))

cover.isl<-full_join(isl,isl.lu_years) %>%
  mutate(N = replace_na(N,0))

cover.isl<-filter(cover.isl, !ISLANDCODE %in% c("SAR","GUG","ALA"))
View(cover.isl)


#Add region names to the regional dataframe
cover.r<-r %>% mutate(REGION_NAME=recode(REGION,
                                                `MARIAN`="Mariana Archipelago",
                                                `MHI`="Main Hawaiian Islands",
                                                `NWHI`="Northwestern Hawaiian Islands",
                                                `SAMOA`="American Samoa",
                                                `PRIAs`="Pacific Remote Island Areas"))


#Final formatting tweaks ####

#remove years before 2013
years<-c("2013-15","2014-15","2014","2015","2016","2015-16","2017-18","2017","2018","2019")

cover.st<-subset(cover.st,ANALYSIS_YEAR %in% years);View(cover.st)
cover.sec<-subset(cover.sec,ANALYSIS_YEAR %in% years);View(cover.sec)
cover.isl<-subset(cover.isl,ANALYSIS_YEAR %in% years);View(cover.isl)
cover.r<-subset(cover.r,ANALYSIS_YEAR %in% years);View(cover.r)
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

cover.st$SurveyYearStart<-CalcStartYr(cover.st)
cover.st$SurveyYearEnd<-CalcEndYr(cover.st)
cover.st$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(cover.st)

cover.sec$SurveyYearStart<-CalcStartYr(cover.sec)
cover.sec$SurveyYearEnd<-CalcEndYr(cover.sec)
cover.sec$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(cover.sec)


cover.isl$SurveyYearStart<-CalcStartYr(cover.isl)
cover.isl$SurveyYearEnd<-CalcEndYr(cover.isl)
cover.isl$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(cover.isl)

cover.r$SurveyYearStart<-CalcStartYr(cover.r)
cover.r$SurveyYearEnd<-CalcEndYr(cover.r)
cover.r$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(cover.r)

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
  colnames(data)[colnames(data)=="Mean.CORAL"]<-"CoralCover_Pct" 
  colnames(data)[colnames(data)=="Mean.CCA"]<-"CrustoseCoralineAlgaeCover_Pct"
  colnames(data)[colnames(data)=="Mean.MA"]<-"MacroalgaeCover_Pct" 
  colnames(data)[colnames(data)=="SE.CORAL"]<-"CoralCover_error" 
  colnames(data)[colnames(data)=="SE.CCA"]<-"CrustoseCoralineAlgaeCover_error" 
  colnames(data)[colnames(data)=="SE.MA"]<-"MacroalgaeCover_error"
  colnames(data)[colnames(data)=="REGION_NAME"]<-"JURISDICTIONname" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="REGION"]<-"JURISDICTIONcode" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="ISLAND"]<-"SUBREGIONname" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="ISLANDCODE"]<-"SUBREGIONcode" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="ANALYSIS_SEC"]<-"SECTORcode" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="SECTORNAME"]<-"SECTORname" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="ANALYSIS_YEAR_new"]<-"AnalysisYear" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="SITEVISITID"]<-"SiteID" #subset just acute diseased colonies
  data$TaxonomicResolution<-"NA"
  data$TaxonomicCode<-"NA"
  data$ScientificName<-"NA"
  
  return(data)
}

cover.st<-ChangeStrataCode(cover.st)
site<-ChangeStrataCode(site)

cover.st<-ColumnNameChange(cover.st)
cover.sec<-ColumnNameChange(cover.sec)
cover.isl<-ColumnNameChange(cover.isl)
cover.r<-ColumnNameChange(cover.r)
site<-ColumnNameChange(site)

cover.st<-cover.st[,c("JURISDICTIONname","JURISDICTIONcode","SUBREGIONname","SUBREGIONcode", "SECTORname","SECTORcode","STRATAname","STRATAcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N","TaxonomicResolution","TaxonomicCode","ScientificName",
                      "CoralCover_Pct","CrustoseCoralineAlgaeCover_Pct","MacroalgaeCover_Pct","CoralCover_error","CrustoseCoralineAlgaeCover_error","MacroalgaeCover_error")]
cover.sec<-cover.sec[,c("JURISDICTIONname","JURISDICTIONcode","SUBREGIONname","SUBREGIONcode", "SECTORname","SECTORcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N",
                        "TaxonomicResolution","TaxonomicCode","ScientificName","CoralCover_Pct","CrustoseCoralineAlgaeCover_Pct","MacroalgaeCover_Pct","CoralCover_error","CrustoseCoralineAlgaeCover_error","MacroalgaeCover_error")]
cover.isl<-cover.isl[,c("JURISDICTIONname","JURISDICTIONcode","SUBREGIONname","SUBREGIONcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N",
                        "TaxonomicResolution","TaxonomicCode","ScientificName","CoralCover_Pct","CrustoseCoralineAlgaeCover_Pct","MacroalgaeCover_Pct","CoralCover_error","CrustoseCoralineAlgaeCover_error","MacroalgaeCover_error")]
cover.r<-cover.r[,c("JURISDICTIONname","JURISDICTIONcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","N",
                    "TaxonomicResolution","TaxonomicCode","ScientificName","CoralCover_Pct","CrustoseCoralineAlgaeCover_Pct","MacroalgaeCover_Pct","CoralCover_error","CrustoseCoralineAlgaeCover_error","MacroalgaeCover_error")]
site<-site[,c("JURISDICTIONname","JURISDICTIONcode","SUBREGIONname","SUBREGIONcode", "SECTORname","SECTORcode","STRATAname","STRATAcode","SurveyYearStart","SurveyYearEnd","AnalysisYear","SiteID",
              "LATITUDE","LONGITUDE")]

write.csv(cover.st,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/PacificNCRMPviztool2022_STRATA_Cover.csv",row.names = F)
write.csv(cover.sec,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/PacificNCRMPviztool2022_SECTOR_Cover.csv",row.names = F)
write.csv(cover.isl,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/PacificNCRMPviztool2022_ISLAND_Cover.csv",row.names = F)
write.csv(cover.r,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/PacificNCRMPviztool2022_REGION_Cover.csv",row.names = F)
write.csv(site,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/PacificNCRMPviztool2022_SITE_Cover.csv",row.names = F)

