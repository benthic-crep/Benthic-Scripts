rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")

## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision")

#You will need to run the following scripts to generate the QC'd, cleaned and summarized data for this analysis
#T:\Benthic\Data\SfM\QC\SfMBenthic_QC.R
#C:\Users\Courtney.S.Couch\Documents\GitHub\Benthic-Scripts\SfM\InSitu_v_SfM_demog_dataprep.R
#C:\Users\Courtney.S.Couch\Documents\GitHub\Benthic-Scripts\SfM\InSitu_v_SfM_demog_SUMMARIZE.R

ad_sfm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMAdult_MCLEANED.csv")
j_sfm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMJuv_MCLEANED.csv")


#Create a look up table of all of the colony attributes- you will need this for the functions below
SURVEY_COL<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony<-unique(ad_sfm[,SURVEY_COL])

SURVEY_SITE<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")
survey_site<-unique(ad_sfm[,SURVEY_SITE])

SURVEY<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE","ANALYST", "REEF_ZONE",
          "DEPTH_BIN","HABITAT_CODE", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","METHOD","TRANSECT","SEGMENT")
survey_segment<-unique(ad_sfm[,SURVEY])


# GENERATE SUMMARY METRICS at the Segment-leveL BY GENUS--------------------------------------------------

#Calc_ColDen_Transect
acd.gen<-Calc_ColDen_Seg(data = ad_sfm,grouping_field = "GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen";colnames(acd.gen)[colnames(acd.gen)=="SEGAREA"]<-"SEGAREA_ad"# calculate density at genus level as well as total
jcd.gen<-Calc_ColDen_Seg(j_sfm,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen"
jcd.gen<-subset(jcd.gen,select=-c(SEGAREA))


## This function calculates mean colony length, % recent dead, % old dead, condition severity or condition extent to the segment level
## NOTE: can run both adult & juvenile data with this function for COLONYLENGTH
#c("COLONYLENGTH","RDEXTENT1", "RDEXTENT2", "RDEXTENT3", "OLDDEAD","SEVERITY_1","SEVERITY_2", "SEVERITY_3", "EXTENT_1", "EXTENT_2", "EXTENT_3")
ex.b<-subset(ad_sfm,EX_BOUND==0)
cl.gen<-Calc_ColMetric_Seg(data = ex.b,grouping_field = "GENUS_CODE",pool_fields = "COLONYLENGTH"); colnames(cl.gen)[colnames(cl.gen)=="Ave.y"]<-"Ave.cl" #Average % old dead
od.gen<-Calc_ColMetric_Seg(data = ad_sfm,grouping_field = "GENUS_CODE",pool_fields = "OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.gen<-Calc_ColMetric_Seg(data = ad_sfm,grouping_field = "GENUS_CODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead

#Calc_RDden_Transect
rdden.gen<-Calc_RDden_Seg(data=ad_sfm,grouping_field ="GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.gen<-subset(rdden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,DZGN_G));colnames(acutedz.gen)[colnames(acutedz.gen)=="DZGN_G"]<-"DZGN_G_den" #subset just acute diseased colonies


#Calc_CONDden_Transect
condden.gen<-Calc_CONDden_Seg(data=ad_sfm,grouping_field ="GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,BLE));colnames(ble.gen)[colnames(ble.gen)=="BLE"]<-"BLE_den" #subset just bleached colonies
chronicdz.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,SEGMENT,GENUS_CODE,CHRO));colnames(chronicdz.gen)[colnames(chronicdz.gen)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies

#Calc_Richness_Transect
#rich.gen<-Calc_Richness_Transect(ad_sfm,"GENUS_CODE")


#Join density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
data.gen <- join_all(list(acd.gen,jcd.gen,cl.gen,od.gen,rd.gen,acutedz.gen,chronicdz.gen,ble.gen), 
                     by=c("METHOD","SITE","SITEVISITID","TRANSECT","SEGMENT","GENUS_CODE"), type='full')
head(data.gen)


#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.gen$JuvColCount[is.na(data.gen$JuvColCount)]<-0;data.gen$JuvColDen[is.na(data.gen$JuvColDen)]<-0
data.gen$AdColCount[is.na(data.gen$AdColCount)]<-0;data.gen$AdColDen[is.na(data.gen$AdColDen)]<-0


#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.gen$DZGN_G_prev<-(data.gen$DZGN_G_den*data.gen$SEGAREA_ad)/data.gen$AdColCount*100
data.gen$BLE_prev<-(data.gen$BLE_den*data.gen$SEGAREA_ad)/data.gen$AdColCount*100
data.gen$CHRO_prev<-(data.gen$CHRO_den*data.gen$SEGAREA_ad)/data.gen$AdColCount*100

View(data.gen)

data.gen<-left_join(data.gen,survey_segment)
head(data.gen)

###Summarize Calibration data

#Convert wide to long for plotting
data.l<-gather(data.gen,Metric,Value,AdColDen:CHRO_prev,factor_key=T)
data.l<-subset(data.l,GENUS_CODE=="SSSS")

data.sum<-ddply(data.l,.(ANALYST,GENUS_CODE,Metric),
                summarize,
                mean=mean(Value,na.rm=T),
                se=std.error(Value,na.rm=T),
                n=length(unique(SEGMENT,na.rm=T)))
data.sum<-subset(data.sum,Metric %in% c("AdColDen","JuvColDen","Ave.cl","Ave.od","Ave.rd","DZGN_G_prev","BLE_prev","CHRO_prev"))



#Plot between observer 
p1<-ggplot(data.sum, aes(x=ANALYST, y=mean, fill=Metric)) + 
  geom_bar(position=position_dodge(), stat="identity", color="black") + 
  guides(fill=FALSE) + facet_wrap(~Metric, scales="free_y", labeller=label_parsed) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=.15, position=position_dodge(.9)) +theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none"
  )
p1

ggsave(p1,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/Figures/AllMetrics_100Belt.pdf",width=12,height=10)

t<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMAdult_MCLEANED.csv")


rd.all<-ddply(t,.(ANALYST,RD1),summarize,abundance=length(RD1));rd.all<-subset(rd.all,RD1!="NONE")
rd.all

con.all<-ddply(t,.(ANALYST,CONDITION_1),summarize,abundance=length(CONDITION_1));con.all<-subset(con.all,CONDITION_1!="NONE")
con.all

survey_segment$SS<-paste(survey_segment$SITE,survey_segment$SEGMENT,sep="_")
nseg<-ddply(survey_segment,.(ANALYST),summarize,NSEG=length(unique(SS)))
nseg

all.data<-left_join(rd.all,nseg)
all.data$abun_seg<-all.data$abundance/all.data$NSEG

p1<-ggplot(all.data, aes(x=ANALYST, y=abun_seg, fill=RD1)) + 
  geom_bar(position=position_dodge(), stat="identity", color="black") + 
  guides(fill=FALSE) + facet_wrap(~RD1, scales="free_y", labeller=label_parsed) +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none"
  )
p1

all.data<-left_join(con.all,nseg)
all.data$abun_seg<-all.data$abundance/all.data$NSEG

p2<-ggplot(all.data, aes(x=ANALYST, y=abun_seg, fill=CONDITION_1)) + 
  geom_bar(position=position_dodge(), stat="identity", color="black") + 
  guides(fill=FALSE) + facet_wrap(~CONDITION_1, scales="free_y", labeller=label_parsed) +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none"
  )
p2

ggsave(p1,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/Figures/RDCauses_100Belt.pdf",width=12,height=10)
ggsave(p2,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/Figures/Conditions_100Belt.pdf",width=12,height=10)
