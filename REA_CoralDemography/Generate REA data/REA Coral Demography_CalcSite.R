# This script will reads in the CLEANED/Analysis ready data that was generated using the following script
#C:\Users\Courtney.S.Couch\Documents\GitHub\Benthic-Scripts\REA_CoralDemography\Generate REA data\REA Coral Demography_DataPrep.R
#The script does some final tweaks to the data then generates Site-level data
#These data only include surveys conducted between 2013-2019
rm(list=ls())

#Set Run Flags
DEBUG=TRUE

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

## LOAD benthic data
awd<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_E_raw_CLEANED.csv")
jwd<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_F_raw_CLEANED.csv")

# awd<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_E_raw_CLEANED.csv")
# jwd<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_F_raw_CLEANED.csv")


#Final Tweaks before calculating Site-level data-------------------------------------------------
#Colony fragments and scleractinans are subseted in the functions 
#Add a column for adult fragments so we can remove them from the dataset later (-1 indicates fragment)
# awd<-CreateFragment(awd)
awd$Fragment<-ifelse(awd$OBS_YEAR <2018 & awd$COLONYLENGTH <5 & awd$S_ORDER=="Scleractinia",-1,awd$Fragment)
head(subset(awd,Fragment==-1& OBS_YEAR<2018)) #double check that pre 2018 fragments create
awd$Fragment[is.na(awd$Fragment)] <- 0
jwd$Fragment <- 0 # you need to add this column so that you can use the site level functions correctly

#Simplify Bleaching Severity categories: in 2019 the team decided to simplify the bleaching severity from 1-5 to 1-3 to improve consistency in severity values
#This code converts the severity data collected prior to 2019 to a 1-3 scale
awd$DATE_ <- as.Date(awd$DATE_, format = "%Y-%m-%d")
jwd$DATE_ <- as.Date(jwd$DATE_, format = "%Y-%m-%d")


awd_pre <- awd %>% filter(DATE_ < as.Date('2019-07-11'))
awd_post<-awd %>% filter(DATE_ >= as.Date('2019-07-11'))
Convert_Severity<-function(data,severity_field,severity_new){
  data$SEV<-data[,severity_field]
  data<-data %>% mutate(sev_new=recode(SEV, 
                                       `1`="NA",
                                       `2`="NA",
                                       `3`="2",
                                       `4`="3",
                                       `5`="3"))
  colnames(data)[which(colnames(data) == 'sev_new')] <- severity_new #change group to whatever your grouping field is.
  data<-subset(data,select=-c(SEV))
return(data)
}

awd_pre<-Convert_Severity(awd_pre,"SEVERITY_1","SEVERITY_1n")
awd_pre<-Convert_Severity(awd_pre,"SEVERITY_2","SEVERITY_2n")
#awd_pre<-Convert_Severity(awd_pre,"SEVERITY_3","SEVERITY_3n") #There were no severity measurements prior to 2020

head(awd_pre)
#View(awd_pre)

#After checking that severity numbers were changed correctly, convert back to original column names & drop original columns
awd_pre<-subset(awd_pre,select=-c(SEVERITY_1));colnames(awd_pre)[which(colnames(awd_pre) == 'SEVERITY_1n')] <- "SEVERITY_1" #change group to whatever your grouping field is.
awd_pre<-subset(awd_pre,select=-c(SEVERITY_2));colnames(awd_pre)[which(colnames(awd_pre) == 'SEVERITY_2n')] <- "SEVERITY_2" #change group to whatever your grouping field is.
#awd_pre<-subset(awd_pre,select=-c(SEVERITY_3));colnames(awd_pre)[which(colnames(awd_pre) == 'SEVERITY_3n')] <- "SEVERITY_3" #change group to whatever your grouping field is.
awd_pre$SEVERITY_3<-NA

head(awd_pre)



#Combine dataframes before and after 2019 & check that rows weren't dropped
awd.<-rbind(awd_pre,awd_post);write.csv(awd.,"test.csv")
#Do we want to remove these there are a lot of them.
#awd.$CONDITION_1<-ifelse(awd.$CONDITION_1 %in% c("BLE","BLP") & is.na(SEVERITY_1),"NDZ",as.character(awd.$CONDITION_1))
nrow(awd)
nrow(awd.);head(awd.)
awd<-awd.; rm("awd.") #remove temporary dataframe if all good. 




#Create a look a table of all of the colony attributes- you will need this the functions below
SURVEY_COL<-c("DATE_","SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony<-unique(awd[,SURVEY_COL])#new_Aggregate_InputTable(awd, SURVEY_INFO)#TAO 2019/10/07

SURVEY_SITE<-c("MISSIONID","DATE_","SITEVISITID", "ANALYSIS_YEAR","OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","HABITAT_CODE")
survey_siteAd<-unique(awd[,SURVEY_SITE])#new_Aggregate_InputTable(awd, SURVEY_INFO)#TAO 2019/10/07

SURVEY_SITE<-c("MISSIONID","DATE_","SITEVISITID", "ANALYSIS_YEAR","OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")
survey_siteJ<-unique(jwd[,SURVEY_SITE])#new_Aggregate_InputTable(awd, SURVEY_INFO)#TAO 2019/10/07

write.csv(survey_siteAd,"surveysite.csv")

#We did juvenile only surveys in 2017 in PRIA, this will make sure the SV table has both adult and juv sites.
survey_site<-left_join(survey_siteJ,survey_siteAd);nrow(survey_site) 

#TEMPORARY WORK AROUND-ASK MICHAEL TO FIX
survey_site$REEF_ZONE<-ifelse(survey_site$SITE=="HAW-04285","Forereef",as.character(survey_site$REEF_ZONE))


# GENERATE SUMMARY METRICS at the transect-leveL BY GENUS--------------------------------------------------
#Calc_ColDen_Transect
acd.gen<-Calc_ColDen_Transect(data = awd,grouping_field = "GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen";colnames(acd.gen)[colnames(acd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_ad"# calculate density at genus level as well as total
jcd.gen<-Calc_ColDen_Transect(jwd,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen";colnames(jcd.gen)[colnames(jcd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_j"

#Calc_ColMetric_Transect
cl.gen<-Calc_ColMetric_Transect(data = awd,grouping_field = "GENUS_CODE",pool_fields = "COLONYLENGTH"); colnames(cl.gen)[colnames(cl.gen)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.gen<-Calc_ColMetric_Transect(data = awd,grouping_field = "GENUS_CODE",pool_fields = "OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.gen<-Calc_ColMetric_Transect(data = awd,grouping_field = "GENUS_CODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead

#Calc_RDden_Transect
rdden.gen<-Calc_RDden_Transect(awd,survey_colony,"GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.gen<-subset(rdden.gen,select = c(SITEVISITID,SITE,TRANSECT,GENUS_CODE,DZGN_G));colnames(acutedz.gen)[colnames(acutedz.gen)=="DZGN_G"]<-"DZGN_den" #subset just acute diseased colonies

#Calc_CONDden_Transect
condden.gen<-Calc_CONDden_Transect(awd,survey_colony,"GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.gen<-subset(condden.gen,select = c(SITEVISITID,SITE,TRANSECT,GENUS_CODE,BLE));colnames(ble.gen)[colnames(ble.gen)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.gen<-subset(condden.gen,select = c(SITEVISITID,SITE,TRANSECT,GENUS_CODE,CHRO));colnames(chronicdz.gen)[colnames(chronicdz.gen)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies

#Calc_Richness_Transect
rich.gen<-Calc_Richness_Transect(awd,"GENUS_CODE")

#ADD CODE TO CHANGE TRANSECT NUMBERS FOR JUVENILES
jcd.gen$TRANSECT[jcd.gen$TRANSECT==3]<-1
jcd.gen$TRANSECT[jcd.gen$TRANSECT==4]<-2


#Merge density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","GENUS_CODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.gen<-Reduce(MyMerge, list(acd.gen,jcd.gen,cl.gen,od.gen,rd.gen,acutedz.gen,chronicdz.gen,ble.gen));
head(data.gen)

#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.gen$JuvColCount[is.na(data.gen$JuvColCount)]<-0;data.gen$JuvColDen[is.na(data.gen$JuvColDen)]<-0
data.gen$AdColCount[is.na(data.gen$AdColCount)]<-0;data.gen$AdColDen[is.na(data.gen$AdColDen)]<-0

#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.gen$DZGN_prev<-(data.gen$DZGN_den*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100
data.gen$BLE_prev<-(data.gen$BLE_den*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100
data.gen$CHRO_prev<-(data.gen$CHRO_den*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100

#Remove data from transects with less than 5m surveyed for adults and 1m for juvs.
data.gen$TRANSECTAREA_ad<-ifelse(data.gen$TRANSECTAREA_ad<5,NA,data.gen$TRANSECTAREA_ad);data.gen[data.gen$TRANSECTAREA_ad<5,]
data.gen$TRANSECTAREA_j<-ifelse(data.gen$TRANSECTAREA_j<1,NA,data.gen$TRANSECTAREA_j);data.gen[data.gen$TRANSECTAREA_j<1,]

#GENERATE SITE-LEVEL DATA BY AVERAGING TRANSECTS-----------------------------------
#Since we are moving to a 1 stage design, we need to summarize the transects before rolling up to site. Dione suggested that we calculate mean of 2 transects rather than pooling or dropping a transect

site.data.gen<-ddply(data.gen, .(SITE,SITEVISITID,GENUS_CODE), #calc total colonies by condition
                     summarise,
                     AdColCount=sum(AdColCount,na.rm=T),AdColDen=mean(AdColDen,na.rm = T),Ave.od=mean(Ave.od,na.rm = T),
                     Ave.rd=mean(Ave.rd,na.rm = T),Ave.size=mean(Ave.cl,na.rm=T),JuvColDen=mean(JuvColDen,na.rm=T),
                     BLE=mean(BLE_den,na.rm=T),AcuteDZ=mean(DZGN_den,na.rm=T),ChronicDZ=mean(CHRO_den,na.rm=T),
                     BLE_prev=mean(BLE_prev,na.rm=T),AcuteDZ_prev=mean(DZGN_prev,na.rm=T),ChronicDZ_prev=mean(CHRO_prev,na.rm=T))

#Duplicate dataframe because the ddply step above takes a while to create. Allows you to tweak code below without having to rerun the ddply step above
site.data.gen2<-site.data.gen


# GENERATE SUMMARY METRICS at the transect-leveL BY SPCODE (finest resolution)--------------------------------------------------
#Calc_ColDen_Transect
acd.sp<-Calc_ColDen_Transect(data = awd,grouping_field = "SPCODE");colnames(acd.sp)[colnames(acd.sp)=="ColCount"]<-"AdColCount";colnames(acd.sp)[colnames(acd.sp)=="ColDen"]<-"AdColDen";colnames(acd.sp)[colnames(acd.sp)=="TRANSECTAREA"]<-"TRANSECTAREA_ad"# calculate density at genus level as well as total
jcd.sp<-Calc_ColDen_Transect(jwd,"SPCODE"); colnames(jcd.sp)[colnames(jcd.sp)=="ColCount"]<-"JuvColCount";colnames(jcd.sp)[colnames(jcd.sp)=="ColDen"]<-"JuvColDen";colnames(jcd.sp)[colnames(jcd.sp)=="TRANSECTAREA"]<-"TRANSECTAREA_j"

#Calc_ColMetric_Transect
cl.sp<-Calc_ColMetric_Transect(data = awd,grouping_field = "SPCODE",pool_fields = "COLONYLENGTH"); colnames(cl.sp)[colnames(cl.sp)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.sp<-Calc_ColMetric_Transect(data = awd,grouping_field = "SPCODE",pool_fields = "OLDDEAD"); colnames(od.sp)[colnames(od.sp)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.sp<-Calc_ColMetric_Transect(data = awd,grouping_field = "SPCODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.sp)[colnames(rd.sp)=="Ave.y"]<-"Ave.rd" #Average % recent dead

#Calc_RDden_Transect
rdden.sp<-Calc_RDden_Transect(awd,survey_colony,"SPCODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.sp<-subset(rdden.sp,select = c(SITEVISITID,SITE,TRANSECT,SPCODE,DZGN_G));colnames(acutedz.sp)[colnames(acutedz.sp)=="DZGN_G"]<-"DZGN_den" #subset just acute diseased colonies

#Calc_CONDden_Transect
condden.sp<-Calc_CONDden_Transect(awd,survey_colony,"SPCODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.sp<-subset(condden.sp,select = c(SITEVISITID,SITE,TRANSECT,SPCODE,BLE));colnames(ble.sp)[colnames(ble.sp)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.sp<-subset(condden.sp,select = c(SITEVISITID,SITE,TRANSECT,SPCODE,CHRO));colnames(chronicdz.sp)[colnames(chronicdz.sp)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies

#Calc_Richness_Transect
rich.sp<-Calc_Richness_Transect(awd,"SPCODE")

#ADD CODE TO CHANGE TRANSECT NUMBERS FOR JUVENILES
jcd.sp$TRANSECT[jcd.sp$TRANSECT==3]<-1
jcd.sp$TRANSECT[jcd.sp$TRANSECT==4]<-2


#Merge density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","SPCODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.sp<-Reduce(MyMerge, list(acd.sp,jcd.sp,cl.sp,od.sp,rd.sp,acutedz.sp,chronicdz.sp,ble.sp));
head(data.sp)

#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.sp$JuvColCount[is.na(data.sp$JuvColCount)]<-0;data.sp$JuvColDen[is.na(data.sp$JuvColDen)]<-0
data.sp$AdColCount[is.na(data.sp$AdColCount)]<-0;data.sp$AdColDen[is.na(data.sp$AdColDen)]<-0

#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.sp$DZGN_prev<-(data.sp$DZGN_den*data.sp$TRANSECTAREA_ad)/data.sp$AdColCount*100
data.sp$BLE_prev<-(data.sp$BLE_den*data.sp$TRANSECTAREA_ad)/data.sp$AdColCount*100
data.sp$CHRO_prev<-(data.sp$CHRO_den*data.sp$TRANSECTAREA_ad)/data.sp$AdColCount*100

#Remove data from transects with less than 5m surveyed for adults and 1m for juvs.
data.sp$TRANSECTAREA_ad<-ifelse(data.sp$TRANSECTAREA_ad<5,NA,data.sp$TRANSECTAREA_ad);data.sp[data.sp$TRANSECTAREA_ad<5,]
data.sp$TRANSECTAREA_j<-ifelse(data.sp$TRANSECTAREA_j<1,NA,data.sp$TRANSECTAREA_j);data.sp[data.sp$TRANSECTAREA_j<1,]

#GENERATE SITE-LEVEL DATA BY AVERAGING TRANSECTS-----------------------------------
#Since we are moving to a 1 stage design, we need to summarize the transects before rolling up to site. Dione suggested that we calculate mean of 2 transects rather than pooling or dropping a transect

site.data.sp<-ddply(data.sp, .(SITE,SITEVISITID,SPCODE), #calc total colonies by condition
                    summarise,
                    AdColCount=sum(AdColCount,na.rm=T),AdColDen=mean(AdColDen,na.rm = T),Ave.od=mean(Ave.od,na.rm = T),
                    Ave.rd=mean(Ave.rd,na.rm = T),Ave.size=mean(Ave.cl,na.rm=T),JuvColDen=mean(JuvColDen,na.rm=T),
                    BLE=mean(BLE_den,na.rm=T),AcuteDZ=mean(DZGN_den,na.rm=T),ChronicDZ=mean(CHRO_den,na.rm=T),
                    BLE_prev=mean(BLE_prev,na.rm=T),AcuteDZ_prev=mean(DZGN_prev,na.rm=T),ChronicDZ_prev=mean(CHRO_prev,na.rm=T))

#Duplicate dataframe because the ddply step above takes a while to create. Allows you to tweak code below without having to rerun the ddply step above
site.data.sp2<-site.data.sp



# GENERATE SUMMARY METRICS at the transect-leveL BY TAXONCODE (finest resolution)--------------------------------------------------
#Calc_ColDen_Transect
acd.tax<-Calc_ColDen_Transect(data = awd,grouping_field = "TAXONCODE");colnames(acd.tax)[colnames(acd.tax)=="ColCount"]<-"AdColCount";colnames(acd.tax)[colnames(acd.tax)=="ColDen"]<-"AdColDen";colnames(acd.tax)[colnames(acd.tax)=="TRANSECTAREA"]<-"TRANSECTAREA_ad"# calculate density at genus level as well as total
jcd.tax<-Calc_ColDen_Transect(jwd,"TAXONCODE"); colnames(jcd.tax)[colnames(jcd.tax)=="ColCount"]<-"JuvColCount";colnames(jcd.tax)[colnames(jcd.tax)=="ColDen"]<-"JuvColDen";colnames(jcd.tax)[colnames(jcd.tax)=="TRANSECTAREA"]<-"TRANSECTAREA_j"

#Calc_ColMetric_Transect
cl.tax<-Calc_ColMetric_Transect(data = awd,grouping_field = "TAXONCODE",pool_fields = "COLONYLENGTH"); colnames(cl.tax)[colnames(cl.tax)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.tax<-Calc_ColMetric_Transect(data = awd,grouping_field = "TAXONCODE",pool_fields = "OLDDEAD"); colnames(od.tax)[colnames(od.tax)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.tax<-Calc_ColMetric_Transect(data = awd,grouping_field = "TAXONCODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.tax)[colnames(rd.tax)=="Ave.y"]<-"Ave.rd" #Average % recent dead

#Calc_RDden_Transect
rdden.tax<-Calc_RDden_Transect(awd,survey_colony,"TAXONCODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.tax<-subset(rdden.tax,select = c(SITEVISITID,SITE,TRANSECT,TAXONCODE,DZGN_G));colnames(acutedz.tax)[colnames(acutedz.tax)=="DZGN_G"]<-"DZGN_den" #subset just acute diseased colonies

#Calc_CONDden_Transect
condden.tax<-Calc_CONDden_Transect(awd,survey_colony,"TAXONCODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.tax<-subset(condden.tax,select = c(SITEVISITID,SITE,TRANSECT,TAXONCODE,BLE));colnames(ble.tax)[colnames(ble.tax)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.tax<-subset(condden.tax,select = c(SITEVISITID,SITE,TRANSECT,TAXONCODE,CHRO));colnames(chronicdz.tax)[colnames(chronicdz.tax)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies

#Calc_Richness_Transect
rich.tax<-Calc_Richness_Transect(awd,"TAXONCODE")

#ADD CODE TO CHANGE TRANSECT NUMBERS FOR JUVENILES
jcd.tax$TRANSECT[jcd.tax$TRANSECT==3]<-1
jcd.tax$TRANSECT[jcd.tax$TRANSECT==4]<-2


#Merge density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","TAXONCODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.tax<-Reduce(MyMerge, list(acd.tax,jcd.tax,cl.tax,od.tax,rd.tax,acutedz.tax,chronicdz.tax,ble.tax));
head(data.tax)

#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.tax$JuvColCount[is.na(data.tax$JuvColCount)]<-0;data.tax$JuvColDen[is.na(data.tax$JuvColDen)]<-0
data.tax$AdColCount[is.na(data.tax$AdColCount)]<-0;data.tax$AdColDen[is.na(data.tax$AdColDen)]<-0

#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.tax$DZGN_prev<-(data.tax$DZGN_den*data.tax$TRANSECTAREA_ad)/data.tax$AdColCount*100
data.tax$BLE_prev<-(data.tax$BLE_den*data.tax$TRANSECTAREA_ad)/data.tax$AdColCount*100
data.tax$CHRO_prev<-(data.tax$CHRO_den*data.tax$TRANSECTAREA_ad)/data.tax$AdColCount*100

#Remove data from transects with less than 5m surveyed for adults and 1m for juvs.
data.tax$TRANSECTAREA_ad<-ifelse(data.tax$TRANSECTAREA_ad<5,NA,data.tax$TRANSECTAREA_ad);data.tax[data.tax$TRANSECTAREA_ad<5,]
data.tax$TRANSECTAREA_j<-ifelse(data.tax$TRANSECTAREA_j<1,NA,data.tax$TRANSECTAREA_j);data.tax[data.tax$TRANSECTAREA_j<1,]

#GENERATE SITE-LEVEL DATA BY AVERAGING TRANSECTS-----------------------------------
#Since we are moving to a 1 stage design, we need to summarize the transects before rolling up to site. Dione suggested that we calculate mean of 2 transects rather than pooling or dropping a transect

site.data.tax<-ddply(data.tax, .(SITE,SITEVISITID,TAXONCODE), #calc total colonies by condition
                     summarise,
                     AdColCount=sum(AdColCount,na.rm=T),AdColDen=mean(AdColDen,na.rm = T),Ave.od=mean(Ave.od,na.rm = T),
                     Ave.rd=mean(Ave.rd,na.rm = T),Ave.size=mean(Ave.cl,na.rm=T),JuvColDen=mean(JuvColDen,na.rm=T),
                     BLE=mean(BLE_den,na.rm=T),AcuteDZ=mean(DZGN_den,na.rm=T),ChronicDZ=mean(CHRO_den,na.rm=T),
                     BLE_prev=mean(BLE_prev,na.rm=T),AcuteDZ_prev=mean(DZGN_prev,na.rm=T),ChronicDZ_prev=mean(CHRO_prev,na.rm=T))

#Duplicate dataframe because the ddply step above takes a while to create. Allows you to tweak code below without having to rerun the ddply step above
site.data.tax2<-site.data.tax



# Merge Site level data with sectors file and export site data ------------
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

#Merge together survey meta data and sector area files and check for missmatches 
meta<-left_join(survey_site,sectors)
meta[which(is.na(meta$AREA_HA)),]
nrow(survey_site)
nrow(meta)


#Merge site level data and meta data
site.data.gen2<-left_join(site.data.gen2,meta)
rich.data.gen<-left_join(rich.gen,meta,meta)
site.data.sp2<-left_join(site.data.sp2,meta)
rich.data.sp<-left_join(rich.sp,meta,meta)
site.data.tax2<-left_join(site.data.tax2,meta)

rich.data.tax<-left_join(rich.tax,meta)

#Add adult and juvenile pres/ab columns
site.data.gen2$Adpres.abs<-ifelse(site.data.gen2$AdColDen>0,1,0)
site.data.gen2$Juvpres.abs<-ifelse(site.data.gen2$JuvColDen>0,1,0)
site.data.sp2$Adpres.abs<-ifelse(site.data.sp2$AdColDen>0,1,0)
site.data.sp2$Juvpres.abs<-ifelse(site.data.sp2$JuvColDen>0,1,0)
site.data.tax2$Adpres.abs<-ifelse(site.data.tax2$AdColDen>0,1,0)
site.data.tax2$Juvpres.abs<-ifelse(site.data.tax2$JuvColDen>0,1,0)


write.csv(site.data.gen2,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS.csv",row.names = F)
# write.csv(site.data.gen2,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS.csv",row.names = F)

write.csv(site.data.sp2,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_SPCODE.csv",row.names = F)
# write.csv(site.data.sp2,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_SPCODE.csv",row.names = F)

write.csv(site.data.tax2,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_TAXONCODE.csv",row.names = F)
# write.csv(site.data.tax2,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_TAXONCODE.csv",row.names = F)


