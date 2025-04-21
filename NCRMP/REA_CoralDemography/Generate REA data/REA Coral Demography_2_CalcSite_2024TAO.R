# This script will reads in the CLEANED/Analysis ready data that was generated using the following script
#C:\Users\Courtney.S.Couch\Documents\GitHub\Benthic-Scripts\REA_CoralDemography\Generate REA data\REA Coral Demography_DataPrep.R
#The script does some final tweaks to the data then generates Site-level data
#These data only include surveys conducted between 2013-2019


rm(list=ls())

#Set Run Flags
DEBUG=TRUE

#LOAD LIBRARY FUNCTIONS ...
source("./Functions/Benthic_Functions_newApp_vTAOfork.R")
source("../fish-paste/lib/core_functions.R")
source("../fish-paste/lib/GIS_functions.R")

## LOAD benthic data
awd<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED_2023.csv")
jwd<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Juveniles_raw_CLEANED_2023.csv")

#If you want to test out script, subset with a single island- the entire script takes forever to run

#awd=subset(awd,ISLAND=="Swains"& OBS_YEAR=="2023")
# jwd=subset(jwd,ISLAND=="Asuncion")

#Final Tweaks before calculating site-level data-------------------------------------------------
#Colony fragments will be removed when you generate the site level data
#We have denoted fragments differently over the course of our dataset. This code makes sure Fragment is 0 or -1 (-1 indicates it's a fragment)

awd$Fragment<-ifelse(awd$OBS_YEAR <2018 & awd$COLONYLENGTH <5 & awd$S_ORDER=="Scleractinia",-1,awd$Fragment)
head(subset(awd,Fragment==-1& OBS_YEAR<2018)) #double check that pre 2018 fragments create
awd$Fragment[is.na(awd$Fragment)] <- 0
jwd$Fragment <- 0 # you need to add this column so that you can use the site level functions correctly
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
awd_pre$SEVERITY_3<-NA

View(awd_pre)



#Combine dataframes before and after 2019 & check that rows weren't dropped
awd.<-rbind(awd_pre,awd_post)
View(awd.)
nrow(awd)
nrow(awd.);head(awd.)
awd<-awd.; rm("awd.") #remove temporary dataframe if all good.

#If bleaching severity is <2, change to NA- we just don't record bleaching consistently enough below severity 2
awd$CONDITION_1<-ifelse(awd$CONDITION_1 %in% c("BLP","BLE") & awd$SEVERITY_1==1,"NONE",awd$CONDITION_1);View(awd)
awd$CONDITION_2<-ifelse(awd$CONDITION_2 %in% c("BLP","BLE") & awd$SEVERITY_2==1,"NONE",awd$CONDITION_2);View(awd)
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

#TEMPORARY WORK AROUND-ASK MICHAEL TO FIX
survey_site$REEF_ZONE<-ifelse(survey_site$SITE=="HAW-04285","Forereef",as.character(survey_site$REEF_ZONE))

#Remove Transect 2 from 2019-present- these were repeat surveys conducted for the SfM method comparison study
awd$TR_YEAR<-paste(awd$TRANSECT,awd$OBS_YEAR,sep="_")
jwd$TR_YEAR<-paste(jwd$TRANSECT,jwd$OBS_YEAR,sep="_")
drop.data<-c("2_2019","2_2022")

awd<-filter(awd,!TR_YEAR %in% drop.data) 
jwd<-filter(jwd,!TR_YEAR %in% drop.data) 


# GENERATE SUMMARY METRICS at the transect-leveL BY GENUS--------------------------------------------------
#Calc_ColDen_Transect
acd.gen<-Calc_ColDen_Transect(data = awd,grouping_field = "GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen";colnames(acd.gen)[colnames(acd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_ad"# calculate density at genus level as well as total
jcd.gen<-Calc_ColDen_Transect(jwd,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen";colnames(jcd.gen)[colnames(jcd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_j"

#Calc_ColMetric_Transect
cl.gen<-Calc_ColMetric_Transect(data = awd,grouping_field = "GENUS_CODE",pool_fields = "COLONYLENGTH"); colnames(cl.gen)[colnames(cl.gen)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.gen<-Calc_ColMetric_Transect(data = awd,grouping_field = "GENUS_CODE",pool_fields = "OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.gen<-Calc_ColMetric_Transect(data = awd,grouping_field = "GENUS_CODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead

#Calc_TotDZden_Transect
totdzden.gen<-Calc_TotDZden_Transect(awd,survey_colony,"GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
totdzden.gen<-subset(totdzden.gen,select = c(SITEVISITID,SITE,TRANSECT,GENUS_CODE,TotDZ_den))


#Calc_RDden_Transect
rdden.gen<-Calc_RDden_Transect(awd,survey_colony,"GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.gen<-subset(rdden.gen,select = c(SITEVISITID,SITE,TRANSECT,GENUS_CODE,DZGN_G));colnames(acutedz.gen)[colnames(acutedz.gen)=="DZGN_G"]<-"DZGN_den" #subset just acute diseased colonies
#alga.gen<-subset(rdden.gen,select = c(SITEVISITID,SITE,TRANSECT,GENUS_CODE,ALGA));colnames(alga.gen)[colnames(alga.gen)=="ALGA"]<-"ALGA_den" #subset just colonies with macroalgae overgrowth

#Calc_CONDden_Transect
condden.gen<-Calc_CONDden_Transect(awd,survey_colony,"GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.gen<-subset(condden.gen,select = c(SITEVISITID,SITE,TRANSECT,GENUS_CODE,BLE));colnames(ble.gen)[colnames(ble.gen)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.gen<-subset(condden.gen,select = c(SITEVISITID,SITE,TRANSECT,GENUS_CODE,CHRO));colnames(chronicdz.gen)[colnames(chronicdz.gen)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies

#CHANGE TRANSECT NUMBERS FOR JUVENILES (pre-2018 we used 3 and 4)
jcd.gen$TRANSECT[jcd.gen$TRANSECT==3]<-1
jcd.gen$TRANSECT[jcd.gen$TRANSECT==4]<-2

#Remove METHOD from dataframes before merging
acd.gen<-subset(acd.gen,select=-c(METHOD))
jcd.gen<-subset(jcd.gen,select=-c(METHOD))
cl.gen<-subset(cl.gen,select=-c(METHOD))
od.gen<-subset(od.gen,select=-c(METHOD))
rd.gen<-subset(rd.gen,select=-c(METHOD))

#Merge density and partial mortality data together.You will need to replace the DUMMY field with the one you want
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","GENUS_CODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.gen<-Reduce(MyMerge, list(acd.gen,jcd.gen,cl.gen,od.gen,rd.gen,totdzden.gen,acutedz.gen,chronicdz.gen,ble.gen));


#Add METHOD back in
data.gen$METHOD<-"DIVER"

head(data.gen)

#Change NaN to NA
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

data.gen[is.nan(data.gen)] <- NA

#There will be some NAs when you merge the juvenile and adult dataframes together because there may be some juvenile taxa that weren't observed as adults or juveniles
#This code identifies which transects adult and juvenile colonies were recorded at and then converts NAs to 0s if needed
ssss<-subset(data.gen,GENUS_CODE=="SSSS")
ssss$Ad_pres<-ifelse(is.na(ssss$AdColCount),"0","-1")
ssss$Juv_pres<-ifelse(is.na(ssss$JuvColCount),"0","-1")
head(ssss)

ssss<-subset(ssss,select = c(SITE,SITEVISITID,TRANSECT,Ad_pres,Juv_pres,TRANSECTAREA_ad,TRANSECTAREA_j)) 
head(ssss)

data.gen<-left_join(subset(data.gen,select = -c(TRANSECTAREA_ad,TRANSECTAREA_j)),ssss) #use transect area from ssss because transectareas for some taxa were NA after merging adults and juvs

data.gen$JuvColCount[is.na(data.gen$JuvColCount) & data.gen$Juv_pres==-1]<-0;data.gen$JuvColDen[is.na(data.gen$JuvColDen) & data.gen$Juv_pres==-1]<-0
data.gen$AdColCount[is.na(data.gen$AdColCount) & data.gen$Ad_pres==-1]<-0;data.gen$AdColDen[is.na(data.gen$AdColDen) & data.gen$Ad_pres==-1]<-0

#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.gen$TotDZ_prev<-(data.gen$TotDZ_den*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100
data.gen$DZGN_prev<-(data.gen$DZGN_den*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100
data.gen$BLE_prev<-(data.gen$BLE_den*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100
data.gen$CHRO_prev<-(data.gen$CHRO_den*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100
#data.gen$ALGA_prev<-(data.gen$ALGA_den*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100

#There will be some NAs when you merge the DZ and other dataframes together because there may be some taxa that didn't have disease
#Convert NA to 0 ONLY for disease density NOT for prevalence
data.gen$TotDZ_den<-ifelse(is.na(data.gen$TotDZ_den),0,data.gen$TotDZ_den)
data.gen$DZGN_den<-ifelse(is.na(data.gen$DZGN_den),0,data.gen$DZGN_den)
data.gen$CHRO_den<-ifelse(is.na(data.gen$CHRO_den),0,data.gen$CHRO_den)
data.gen$BLE_den<-ifelse(is.na(data.gen$BLE_den),0,data.gen$BLE_den)
#data.gen$ALGA_den<-ifelse(is.na(data.gen$ALGA_den),0,data.gen$ALGA_den)


#Remove data from transects with less than 5m surveyed for adults and 1m for juvs.
data.gen$TRANSECTAREA_ad<-ifelse(data.gen$TRANSECTAREA_ad<5,NA,data.gen$TRANSECTAREA_ad);data.gen[data.gen$TRANSECTAREA_ad<5,]
data.gen$TRANSECTAREA_j<-ifelse(data.gen$TRANSECTAREA_j<1,NA,data.gen$TRANSECTAREA_j);data.gen[data.gen$TRANSECTAREA_j<1,]

#GENERATE SITE-LEVEL DATA BY AVERAGING TRANSECTS-----------------------------------
#Since we are moving to a 1 stage design, we need to summarize the transects before rolling up to site. Dione suggested that we calculate mean of 2 transects rather than pooling or dropping a transect

site.data.gen<-ddply(data.gen, .(SITE,SITEVISITID,GENUS_CODE), #calc total colonies by condition
                     summarise,
                     AdColCount=mean(AdColCount,na.rm=T),AdColDen=mean(AdColDen,na.rm = T),Ave.od=mean(Ave.od,na.rm = T),
                     Ave.rd=mean(Ave.rd,na.rm = T),Ave.size=mean(Ave.cl,na.rm=T),JuvColCount=mean(JuvColCount,na.rm=T),
                     JuvColDen=mean(JuvColDen,na.rm=T),BLE=mean(BLE_den,na.rm=T),TotDZ=mean(TotDZ_den,na.rm=T), AcuteDZ=mean(DZGN_den,na.rm=T),ChronicDZ=mean(CHRO_den,na.rm=T),
                     BLE_prev=mean(BLE_prev,na.rm=T),TotDZ_prev=mean(TotDZ_prev,na.rm=T), AcuteDZ_prev=mean(DZGN_prev,na.rm=T),ChronicDZ_prev=mean(CHRO_prev,na.rm=T))


#Duplicate dataframe because the ddply step above takes a while to create. Allows you to tweak code below without having to rerun the ddply step above
site.data.gen2<-site.data.gen

head(site.data.gen2)

# GENERATE SUMMARY METRICS at the transect-leveL BY SPCODE (finest resolution)--------------------------------------------------
#Calc_ColDen_Transect
acd.sp<-Calc_ColDen_Transect(data = awd,grouping_field = "SPCODE");colnames(acd.sp)[colnames(acd.sp)=="ColCount"]<-"AdColCount";colnames(acd.sp)[colnames(acd.sp)=="ColDen"]<-"AdColDen";colnames(acd.sp)[colnames(acd.sp)=="TRANSECTAREA"]<-"TRANSECTAREA_ad"# calculate density at genus level as well as total
jcd.sp<-Calc_ColDen_Transect(jwd,"SPCODE"); colnames(jcd.sp)[colnames(jcd.sp)=="ColCount"]<-"JuvColCount";colnames(jcd.sp)[colnames(jcd.sp)=="ColDen"]<-"JuvColDen";colnames(jcd.sp)[colnames(jcd.sp)=="TRANSECTAREA"]<-"TRANSECTAREA_j"

#Calc_ColMetric_Transect
cl.sp<-Calc_ColMetric_Transect(data = awd,grouping_field = "SPCODE",pool_fields = "COLONYLENGTH"); colnames(cl.sp)[colnames(cl.sp)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.sp<-Calc_ColMetric_Transect(data = awd,grouping_field = "SPCODE",pool_fields = "OLDDEAD"); colnames(od.sp)[colnames(od.sp)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.sp<-Calc_ColMetric_Transect(data = awd,grouping_field = "SPCODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.sp)[colnames(rd.sp)=="Ave.y"]<-"Ave.rd" #Average % recent dead

#Calc_TotDZden_Transect
totdzden.sp<-Calc_TotDZden_Transect(awd,survey_colony,"SPCODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
totdzden.sp<-subset(totdzden.sp,select = c(SITEVISITID,SITE,TRANSECT,SPCODE,TotDZ_den))


#Calc_RDden_Transect
rdden.sp<-Calc_RDden_Transect(awd,survey_colony,"SPCODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.sp<-subset(rdden.sp,select = c(SITEVISITID,SITE,TRANSECT,SPCODE,DZGN_G));colnames(acutedz.sp)[colnames(acutedz.sp)=="DZGN_G"]<-"DZGN_den" #subset just acute diseased colonies

#Calc_CONDden_Transect
condden.sp<-Calc_CONDden_Transect(awd,survey_colony,"SPCODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.sp<-subset(condden.sp,select = c(SITEVISITID,SITE,TRANSECT,SPCODE,BLE));colnames(ble.sp)[colnames(ble.sp)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.sp<-subset(condden.sp,select = c(SITEVISITID,SITE,TRANSECT,SPCODE,CHRO));colnames(chronicdz.sp)[colnames(chronicdz.sp)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies


#ADD CODE TO CHANGE TRANSECT NUMBERS FOR JUVENILES
jcd.sp$TRANSECT[jcd.sp$TRANSECT==3]<-1
jcd.sp$TRANSECT[jcd.sp$TRANSECT==4]<-2

#Remove METHOD from dataframes before merging
acd.sp<-subset(acd.sp,select=-c(METHOD))
jcd.sp<-subset(jcd.sp,select=-c(METHOD))
cl.sp<-subset(cl.sp,select=-c(METHOD))
od.sp<-subset(od.sp,select=-c(METHOD))
rd.sp<-subset(rd.sp,select=-c(METHOD))


#Merge density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","SPCODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.sp<-Reduce(MyMerge, list(acd.sp,jcd.sp,cl.sp,od.sp,rd.sp,totdzden.sp,acutedz.sp,chronicdz.sp,ble.sp));
head(data.sp)

data.sp$METHOD<-"DIVER"

#There will be some NAs when you merge the juvenile and adult dataframes together because there may be some juvenile taxa that weren't observed as adults or juveniles
#This code identifies which transects adult and juvenile colonies were recorded at and then converts NAs to 0s if needed
ssss<-subset(data.sp,SPCODE=="SSSS")
ssss$Ad_pres<-ifelse(is.na(ssss$AdColCount),"0","-1")
ssss$Juv_pres<-ifelse(is.na(ssss$JuvColCount),"0","-1")
head(ssss)

ssss<-subset(ssss,select = c(SITE,SITEVISITID,TRANSECT,Ad_pres,Juv_pres,TRANSECTAREA_ad,TRANSECTAREA_j)) 
head(ssss)

data.sp<-left_join(subset(data.sp,select = -c(TRANSECTAREA_ad,TRANSECTAREA_j)),ssss) #use transect area from ssss because transectareas for some taxa were NA after merging adults and juvs


data.sp$JuvColCount[is.na(data.sp$JuvColCount) & data.sp$Juv_pres==-1]<-0;data.sp$JuvColDen[is.na(data.sp$JuvColDen) & data.sp$Juv_pres==-1]<-0
data.sp$AdColCount[is.na(data.sp$AdColCount) & data.sp$Ad_pres==-1]<-0;data.sp$AdColDen[is.na(data.sp$AdColDen) & data.sp$Ad_pres==-1]<-0

#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.sp$TotDZ_prev<-(data.sp$TotDZ_den*data.sp$TRANSECTAREA_ad)/data.sp$AdColCount*100
data.sp$DZGN_prev<-(data.sp$DZGN_den*data.sp$TRANSECTAREA_ad)/data.sp$AdColCount*100
data.sp$BLE_prev<-(data.sp$BLE_den*data.sp$TRANSECTAREA_ad)/data.sp$AdColCount*100
data.sp$CHRO_prev<-(data.sp$CHRO_den*data.sp$TRANSECTAREA_ad)/data.sp$AdColCount*100

#There will be some NAs when you merge the DZ and other dataframes together because there may be some taxa that didn't have disease
#Convert NA to 0 ONLY for disease density NOT for prevalence
data.sp$TotDZ_den<-ifelse(is.na(data.sp$TotDZ_den),0,data.sp$TotDZ_den)
data.sp$DZGN_den<-ifelse(is.na(data.sp$DZGN_den),0,data.sp$DZGN_den)
data.sp$CHRO_den<-ifelse(is.na(data.sp$CHRO_den),0,data.sp$CHRO_den)
data.sp$BLE_den<-ifelse(is.na(data.sp$BLE_den),0,data.sp$BLE_den)


#Remove data from transects with less than 5m surveyed for adults and 1m for juvs.
data.sp$TRANSECTAREA_ad<-ifelse(data.sp$TRANSECTAREA_ad<5,NA,data.sp$TRANSECTAREA_ad);data.sp[data.sp$TRANSECTAREA_ad<5,]
data.sp$TRANSECTAREA_j<-ifelse(data.sp$TRANSECTAREA_j<1,NA,data.sp$TRANSECTAREA_j);data.sp[data.sp$TRANSECTAREA_j<1,]

#GENERATE SITE-LEVEL DATA BY AVERAGING TRANSECTS-----------------------------------
#Since we are moving to a 1 stage design, we need to summarize the transects before rolling up to site. Dione suggested that we calculate mean of 2 transects rather than pooling or dropping a transect

site.data.sp<-ddply(data.sp, .(SITE,SITEVISITID,SPCODE), #calc total colonies by condition
                    summarise,
                    AdColCount=sum(AdColCount,na.rm=T),AdColDen=mean(AdColDen,na.rm = T),Ave.od=mean(Ave.od,na.rm = T),
                    Ave.rd=mean(Ave.rd,na.rm = T),Ave.size=mean(Ave.cl,na.rm=T),JuvColDen=mean(JuvColDen,na.rm=T),
                    BLE=mean(BLE_den,na.rm=T),TotDZ=mean(TotDZ_den), AcuteDZ=mean(DZGN_den,na.rm=T),ChronicDZ=mean(CHRO_den,na.rm=T),
                    BLE_prev=mean(BLE_prev,na.rm=T),TotDZ_prev= mean(TotDZ_prev,na.rm=T), AcuteDZ_prev=mean(DZGN_prev,na.rm=T),ChronicDZ_prev=mean(CHRO_prev,na.rm=T))

#Duplicate dataframe because the ddply step above takes a while to create. Allows you to tweak code below without having to rerun the ddply step above
site.data.sp2<-site.data.sp

head(site.data.sp2)


# GENERATE SUMMARY METRICS at the transect-leveL BY TAXONCODE--------------------------------------------------
#Calc_ColDen_Transect
acd.tax<-Calc_ColDen_Transect(data = awd,grouping_field = "TAXONCODE");colnames(acd.tax)[colnames(acd.tax)=="ColCount"]<-"AdColCount";colnames(acd.tax)[colnames(acd.tax)=="ColDen"]<-"AdColDen";colnames(acd.tax)[colnames(acd.tax)=="TRANSECTAREA"]<-"TRANSECTAREA_ad"# calculate density at genus level as well as total
jcd.tax<-Calc_ColDen_Transect(jwd,"TAXONCODE"); colnames(jcd.tax)[colnames(jcd.tax)=="ColCount"]<-"JuvColCount";colnames(jcd.tax)[colnames(jcd.tax)=="ColDen"]<-"JuvColDen";colnames(jcd.tax)[colnames(jcd.tax)=="TRANSECTAREA"]<-"TRANSECTAREA_j"

#Calc_ColMetric_Transect
cl.tax<-Calc_ColMetric_Transect(data = awd,grouping_field = "TAXONCODE",pool_fields = "COLONYLENGTH"); colnames(cl.tax)[colnames(cl.tax)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.tax<-Calc_ColMetric_Transect(data = awd,grouping_field = "TAXONCODE",pool_fields = "OLDDEAD"); colnames(od.tax)[colnames(od.tax)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.tax<-Calc_ColMetric_Transect(data = awd,grouping_field = "TAXONCODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.tax)[colnames(rd.tax)=="Ave.y"]<-"Ave.rd" #Average % recent dead

#Calc_TotDZden_Transect
totdzden.tax<-Calc_TotDZden_Transect(awd,survey_colony,"TAXONCODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
totdzden.tax<-subset(totdzden.tax,select = c(SITEVISITID,SITE,TRANSECT,TAXONCODE,TotDZ_den))


#Calc_RDden_Transect
rdden.tax<-Calc_RDden_Transect(awd,survey_colony,"TAXONCODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.tax<-subset(rdden.tax,select = c(SITEVISITID,SITE,TRANSECT,TAXONCODE,DZGN_G));colnames(acutedz.tax)[colnames(acutedz.tax)=="DZGN_G"]<-"DZGN_den" #subset just acute diseased colonies

#Calc_CONDden_Transect
condden.tax<-Calc_CONDden_Transect(awd,survey_colony,"TAXONCODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.tax<-subset(condden.tax,select = c(SITEVISITID,SITE,TRANSECT,TAXONCODE,BLE));colnames(ble.tax)[colnames(ble.tax)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.tax<-subset(condden.tax,select = c(SITEVISITID,SITE,TRANSECT,TAXONCODE,CHRO));colnames(chronicdz.tax)[colnames(chronicdz.tax)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies

#Calc_Richness_Transect
#rich.tax<-Calc_Richness_Transect(awd,"TAXONCODE")

#ADD CODE TO CHANGE TRANSECT NUMBERS FOR JUVENILES
jcd.tax$TRANSECT[jcd.tax$TRANSECT==3]<-1
jcd.tax$TRANSECT[jcd.tax$TRANSECT==4]<-2

#Remove METHOD from dataframes before merging
acd.tax<-subset(acd.tax,select=-c(METHOD))
jcd.tax<-subset(jcd.tax,select=-c(METHOD))
cl.tax<-subset(cl.tax,select=-c(METHOD))
od.tax<-subset(od.tax,select=-c(METHOD))
rd.tax<-subset(rd.tax,select=-c(METHOD))


#Merge density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","TAXONCODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.tax<-Reduce(MyMerge, list(acd.tax,jcd.tax,cl.tax,od.tax,rd.tax,totdzden.tax,acutedz.tax,chronicdz.tax,ble.tax));


#Add METHOD back in
data.tax$METHOD<-"DIVER"

head(data.tax)

#Change NaN to NA
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

data.tax[is.nan(data.tax)] <- NA

#There will be some NAs when you merge the juvenile and adult dataframes together because there may be some juvenile taxa that weren't observed as adults or juveniles
#This code identifies which transects adult and juvenile colonies were recorded at and then converts NAs to 0s if needed
ssss<-subset(data.tax,TAXONCODE=="SSSS")
ssss$Ad_pres<-ifelse(is.na(ssss$AdColCount),"0","-1")
ssss$Juv_pres<-ifelse(is.na(ssss$JuvColCount),"0","-1")
head(ssss)

ssss<-subset(ssss,select = c(SITE,SITEVISITID,TRANSECT,Ad_pres,Juv_pres,TRANSECTAREA_ad,TRANSECTAREA_j)) 
head(ssss)

data.tax<-left_join(subset(data.tax,select = -c(TRANSECTAREA_ad,TRANSECTAREA_j)),ssss) #use transect area from ssss because transectareas for some taxa were NA after merging adults and juvs

data.tax$JuvColCount[is.na(data.tax$JuvColCount) & data.tax$Juv_pres==-1]<-0;data.tax$JuvColDen[is.na(data.tax$JuvColDen) & data.tax$Juv_pres==-1]<-0
data.tax$AdColCount[is.na(data.tax$AdColCount) & data.tax$Ad_pres==-1]<-0;data.tax$AdColDen[is.na(data.tax$AdColDen) & data.tax$Ad_pres==-1]<-0

#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.tax$TotDZ_prev<-(data.tax$TotDZ_den*data.tax$TRANSECTAREA_ad)/data.tax$AdColCount*100
data.tax$DZGN_prev<-(data.tax$DZGN_den*data.tax$TRANSECTAREA_ad)/data.tax$AdColCount*100
data.tax$BLE_prev<-(data.tax$BLE_den*data.tax$TRANSECTAREA_ad)/data.tax$AdColCount*100
data.tax$CHRO_prev<-(data.tax$CHRO_den*data.tax$TRANSECTAREA_ad)/data.tax$AdColCount*100

#There will be some NAs when you merge the DZ and other dataframes together because there may be some taxa that didn't have disease
#Convert NA to 0 ONLY for disease density NOT for prevalence
data.tax$TotDZ_den<-ifelse(is.na(data.tax$TotDZ_den),0,data.tax$TotDZ_den)
data.tax$DZGN_den<-ifelse(is.na(data.tax$DZGN_den),0,data.tax$DZGN_den)
data.tax$CHRO_den<-ifelse(is.na(data.tax$CHRO_den),0,data.tax$CHRO_den)
data.tax$BLE_den<-ifelse(is.na(data.tax$BLE_den),0,data.tax$BLE_den)


#Remove data from transects with less than 5m surveyed for adults and 1m for juvs.
data.tax$TRANSECTAREA_ad<-ifelse(data.tax$TRANSECTAREA_ad<5,NA,data.tax$TRANSECTAREA_ad);data.tax[data.tax$TRANSECTAREA_ad<5,]
data.tax$TRANSECTAREA_j<-ifelse(data.tax$TRANSECTAREA_j<1,NA,data.tax$TRANSECTAREA_j);data.tax[data.tax$TRANSECTAREA_j<1,]

#GENERATE SITE-LEVEL DATA BY AVERAGING TRANSECTS-----------------------------------
#Since we are moving to a 1 stage design, we need to summarize the transects before rolling up to site. Dione suggested that we calculate mean of 2 transects rather than pooling or dropping a transect

site.data.tax<-ddply(data.tax, .(SITE,SITEVISITID,TAXONCODE), #calc total colonies by condition
                     summarise,
                     AdColCount=mean(AdColCount,na.rm=T),AdColDen=mean(AdColDen,na.rm = T),Ave.od=mean(Ave.od,na.rm = T),
                     Ave.rd=mean(Ave.rd,na.rm = T),Ave.size=mean(Ave.cl,na.rm=T),JuvColCount=mean(JuvColCount,na.rm=T),
                     JuvColDen=mean(JuvColDen,na.rm=T),BLE=mean(BLE_den,na.rm=T),TotDZ=mean(TotDZ_den,na.rm=T), AcuteDZ=mean(DZGN_den,na.rm=T),ChronicDZ=mean(CHRO_den,na.rm=T),
                     BLE_prev=mean(BLE_prev,na.rm=T),TotDZ_prev=mean(TotDZ_prev,na.rm=T), AcuteDZ_prev=mean(DZGN_prev,na.rm=T),ChronicDZ_prev=mean(CHRO_prev,na.rm=T))

#Duplicate dataframe because the ddply step above takes a while to create. Allows you to tweak code below without having to rerun the ddply step above
site.data.tax2<-site.data.tax

head(site.data.tax2)

# Merge Site level data with sectors file and export site data ------------
sectors<-read.csv("../fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)
sectors<-sectors[,c("REGION","ISLAND","SEC_NAME","REEF_ZONE","DEPTH_BIN","AREA_HA","NH")]

#Merge together survey meta data and sector area files and check for missmatches
meta<-left_join(survey_site,sectors)
meta[which(is.na(meta$AREA_HA)),]
nrow(survey_site)
nrow(meta)


#Merge site level data and meta data
site.data.gen2<-left_join(site.data.gen2,meta)
site.data.sp2<-left_join(site.data.sp2,meta)
site.data.tax2<-left_join(site.data.tax2,meta)

#Final Tweaks
#Add adult and juvenile pres/ab columns
site.data.gen2$Adpres.abs<-ifelse(site.data.gen2$AdColDen>0,1,0)
site.data.gen2$Juvpres.abs<-ifelse(site.data.gen2$JuvColDen>0,1,0)
site.data.sp2$Adpres.abs<-ifelse(site.data.sp2$AdColDen>0,1,0)
site.data.sp2$Juvpres.abs<-ifelse(site.data.sp2$JuvColDen>0,1,0)
site.data.tax2$Adpres.abs<-ifelse(site.data.tax2$AdColDen>0,1,0)
site.data.tax2$Juvpres.abs<-ifelse(site.data.tax2$JuvColDen>0,1,0)

# site.data.gen2$ALGA<-ifelse(site.data.gen2$OBS_YEAR>= 2019,NA,site.data.gen2$ALGA)
# site.data.gen2$AlgalOG_prev<-ifelse(site.data.gen2$OBS_YEAR>= 2019,NA,site.data.gen2$AlgalOG_prev)

#Save Site-level data
write.csv(site.data.gen2,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS_2023.csv",row.names = F)
write.csv(site.data.sp2,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_SPCODE_2023.csv",row.names = F)
write.csv(site.data.tax2,file="T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_TAXONCODE_2023.csv",row.names = F)


