#read in Tier1 summary tables from Benthic Cover_RawtoEstimates_NCRMPViztool.R script


rm(list=ls())

#LOAD LIBRARY FUNCTIONS ...
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")


#Read in data
st<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicCover_2010-2019_Tier1_STRATA_v2.csv")
st<-st[,c("REGIONNAME","REGION","ISLAND","ISLANDCODE", "SECTORNAME","DB_RZ","ANALYSIS_YEAR","n","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]

sec<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicCover_2010-2019_Tier1_SECTOR_v2.csv")
sec<-sec[,c("REGIONNAME","REGION","ISLAND","ISLANDCODE", "SECTORNAME","SECTOR","ANALYSIS_YEAR","n","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]

isl<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicCover_2010-2019_Tier1_ISLAND_v2.csv")
isl<-isl[,c("REGIONNAME","REGION","ISLAND","ISLANDCODE","ANALYSIS_YEAR","n","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]

r<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Region/BenthicCover_2010-2019_Tier1_REGION_v2.csv")
r<-r[,c("REGIONNAME","REGION","ANALYSIS_YEAR","n","Mean.CORAL","Mean.CCA","Mean.MA","SE.CORAL","SE.CCA","SE.MA")]

#remove years before 2013
years<-c("2013-2015","2014-2015","2014","2015","2016","2015-2016","2017-2018","2017","2018","2019")
st<-subset(st,ANALYSIS_YEAR %in% years);View(st)
sec<-subset(sec,ANALYSIS_YEAR %in% years);View(sec)
isl<-subset(isl,ANALYSIS_YEAR %in% years);View(isl)
r<-subset(r,ANALYSIS_YEAR %in% years);View(r)

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

CalcStartEnd<-function(data){
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

st$SurveyYearStart<-CalcStartYr(st)
st$SurveyYearEnd<-CalcEndYr(st)
st$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(st)

sec$SurveyYearStart<-CalcStartYr(sec)
sec$SurveyYearEnd<-CalcEndYr(sec)
sec$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(sec)


isl$SurveyYearStart<-CalcStartYr(isl)
isl$SurveyYearEnd<-CalcEndYr(isl)
isl$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(isl)


r$SurveyYearStart<-CalcStartYr(r)
r$SurveyYearEnd<-CalcEndYr(r)
r$ANALYSIS_YEAR_new<-CalcNewAnalysisYear(r)


#Read in look up table of all possible strata
seclu<-read.csv("T:/Benthic/Data/Lookup Tables/PacificNCRMP_Benthic_Sectors_Lookup_v3.csv")

colnames(seclu)[colnames(seclu)=="PooledSector_Cover_Viztool"]<-"ANALYSIS_SEC" #subset just acute diseased colonies
colnames(seclu)[colnames(seclu)=="DB_RZ"]<-"STRATA" #subset just acute diseased colonies

st.lu<-seclu[,c("ANALYSIS_SEC","STRATAname","STRATAcode")]
sec.lu<-unique(seclu[,c("ANALYSIS_SEC")])
isl.lu<-unique(seclu[,c("ISLAND")])

#Merge strata/sec/isl list with cover data data
#Add NA for all strata/sec/isl that don't have cover data
cover.st<-full_join(st,st.lu)
View(cover.st)
#test<-test[!duplicated(test[,4]),]

cover.sec<-full_join(sec,sec.lu)
View(cover.sec)
#test<-test[!duplicated(test[,4]),]


cover.isl<-full_join(isl,isl.lu)
View(cover.isl)
#test<-test[!duplicated(test[,4]),]


#Change Stata names to match AOI Naming Convention
cover.st<-cover.st %>% mutate(STRATAcode=recode(DB_RZ,
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
                                    
ColumnNameChange<-function(data){
  colnames(data)[colnames(data)=="Mean.CORAL"]<-"CoralCover_Pct" 
  colnames(data)[colnames(data)=="Mean.CCA"]<-"CrustoseCoralineAlgaeCover_Pct"
  colnames(data)[colnames(data)=="Mean.MA"]<-"MacroalgaeCover_Pct" 
  colnames(data)[colnames(data)=="SE.CORAL"]<-"CoralCover_error" 
  colnames(data)[colnames(data)=="SE.CCA"]<-"CrustoseCoralineAlgaeCover_error" 
  colnames(data)[colnames(data)=="SE.MA"]<-"MacroalgaeCover_error"
  colnames(data)[colnames(data)=="REGIONNAME"]<-"JURISDICTIONname" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="REGION"]<-"JURISDICTIONcode" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="ISLAND"]<-"SUBREGIONname" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="ISLANDCODE"]<-"SUBREGIONcode" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="SECTOR"]<-"SECTORcode" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="SECTORNAME"]<-"SECTORname" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="STRATANAME"]<-"STRATAname" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="STRATACODE"]<-"STRATAcode" #subset just acute diseased colonies
  colnames(data)[colnames(data)=="ANALYSIS_YEAR_new"]<-"AnalysisYear" #subset just acute diseased colonies
  data$TaxonomicResolution<-"NA"
  data$TaxonomicCode<-"NA"
  data$ScientificName<-"NA"
  
  return(data)
}

cover.st<-ColumnNameChange(cover.st)
cover.sec<-ColumnNameChange(cover.sec)
cover.isl<-ColumnNameChange(cover.isl)
cover.r<-ColumnNameChange(cover.r)

write.csv(st,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/PacificNCRMPviztool2022_STRATA_Cover.csv",row.names = F)
write.csv(sec,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/PacificNCRMPviztool2022_SECTOR_Cover.csv",row.names = F)
write.csv(isl,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/PacificNCRMPviztool2022_ISLAND_Cover.csv",row.names = F)
write.csv(r,file="T:/Benthic/Data/Data Requests/NCRMPViztool/2022/PacificNCRMPviztool2022_REGION_Cover.csv",row.names = F)

