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

data.gen<-read.csv("T:/Benthic/Data/SfM/Calibration QC/HARAMP_repeats_GENUS_Summarized Data-CALIBRATION.csv")


###Summarize Calibration data
#Check list of segments that have been annotated by different annotators

sfm<-data.gen[data.gen$METHOD=="SfM",];sfm<-droplevels(sfm)
table(sfm$SS,sfm$MethodRep)

#List of segments that were surveyed by all methods and multiple annotators
sfm3<-data.gen[data.gen$MethodRep=="SfM_3",]
length(unique(sfm3$SS))
seglist<-unique(sfm3$SS)

#Subset dataset to only include the 5 segments annotated for calibration exercise
df.all<-subset(data.gen,SS %in% seglist)
length(unique(df.all$SS))

#Convert wide to long for plotting
data.l<-gather(df.all,Metric,Value,AdColCount:CHRO_prev,factor_key=T)
data.l<-subset(data.l,GENUS_CODE=="SSSS")

data.sum<-ddply(data.l,.(TRANSECT,GENUS_CODE,Metric),
                summarize,
                mean=mean(Value,na.rm=T),
                se=std.error(Value,na.rm=T),
                n=length(unique(SS,na.rm=T)))
data.sum<-subset(data.sum,Metric %in% c("AdColDen","JuvColDen","Ave.cl","Ave.od","Ave.rd","DZGN_G_prev","BLE_prev","CHRO_prev"))

#Convert transect back to analyst for plots
data.sum<-data.sum %>% mutate(ANALYST=recode(TRANSECT,
                                          `1`="Mollie",
                                          `2`="Rhonda",
                                          `3`="Corinne",
                                          `4`="Mia",
                                          `5`="Frances",
                                          `6`="Ari",
                                          `NA`="NA"))


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

ggsave(p1,file="T:/Benthic/Data/SfM/Calibration Plots/AllMetrics_SfM5segCalibration.pdf",width=12,height=10)

t<-read.csv("T:/Benthic/Data/SfM/Calibration QC/HARAMP19_SfMAdult_CLEANED.csv")

t$SS<-paste(t$SITE,t$SEGMENT,sep="_")
t2<-subset(t,SS %in% seglist)

rd.all<-ddply(t2,.(ANALYST,RD1),summarize,abundance=length(RD1));rd.all<-subset(rd.all,RD1!="NONE")
rd.all

con.all<-ddply(t2,.(ANALYST,CONDITION_1),summarize,abundance=length(CONDITION_1));con.all<-subset(con.all,CONDITION_1!="NONE")
con.all


p1<-ggplot(rd.all, aes(x=ANALYST, y=abundance, fill=RD1)) + 
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

p2<-ggplot(con.all, aes(x=ANALYST, y=abundance, fill=CONDITION_1)) + 
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

ggsave(p1,file="T:/Benthic/Data/SfM/Calibration Plots/RDconditionsCalibration.pdf",width=12,height=10)
ggsave(p2,file="T:/Benthic/Data/SfM/Calibration Plots/ConditionsCalibration.pdf",width=12,height=10)
