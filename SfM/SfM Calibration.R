rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")

## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")

#You will need to run the following scripts to generate the QC'd, cleaned and summarized data for this analysis
#T:\Benthic\Data\SfM\QC\SfMBenthic_QC.R
#C:\Users\Courtney.S.Couch\Documents\GitHub\Benthic-Scripts\SfM\InSitu_v_SfM_demog_dataprep.R
#C:\Users\Courtney.S.Couch\Documents\GitHub\Benthic-Scripts\SfM\InSitu_v_SfM_demog_SUMMARIZE.R

data.gen<-read.csv("T:/Benthic/Data/SfM/Summarized Data/HARAMP19_SfM_Diver_Summary.csv")


###Summarize Calibration data
#List of segments that were surveyed by all methods and multiple divers
sfm3<-data.gen[data.gen$MethodRep=="SfM_3",]
length(unique(sfm3$SS))
seglist<-unique(sfm3$SS)

df.all<-subset(data.gen,SS %in% seglist)
length(unique(df.all$SS))

#Convert wide to long for plotting
data.l<-gather(data.gen,Metric,Value,AdColCount:CHRO_prev,factor_key=T)

data.sum<-ddply(data.l,.(TRANSECT,GENUS_CODE,Metric),
                summarize,
                mean=mean(Value,na.rm=T),
                se=std.error(Value,na.rm=T))
data.sum<-subset(data.sum,GENUS_CODE=="SSSS")
data.sum<-subset(data.sum,Metric %in% c("AdColDen","JuvColDen","Ave.cl","Ave.od","Ave.rd","DZGN_G_prev","BLE_prev","CHRO_prev"))

#Plot between observer 
p1<-ggplot(data.sum, aes(x=TRANSECT, y=mean, fill=Metric)) + 
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
