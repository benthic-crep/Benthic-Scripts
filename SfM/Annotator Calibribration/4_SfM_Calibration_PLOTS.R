rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Jonathan.Charendoff/Documents/GitHub/fish-paste/lib/core_functions.R")
library(tidyr)

data.gen<-read.csv("T:/Benthic/Data/SfM/Calibration QC/2025/MARAMP25_GENUS_Summarized Data-CALIBRATION.csv")


###Summarize Calibration data
#List of segments that were surveyed by all methods and multiple annotators
seglist<-unique(data.gen$SS)

#Subset dataset to only include the 5 segments annotated for calibration exercise
df.all<-subset(data.gen,SS %in% seglist)
length(unique(df.all$SS))

#Convert wide to long for plotting
data.l<-gather(df.all,Metric,Value,AdColCount:CHRO_prev,factor_key=T)
data.l<-subset(data.l,GENUS_CODE=="SSSS")

data.sum<-ddply(data.l,.(TRANSECT,GENUS_CODE,Metric),
                summarize,
                mean=mean(Value,na.rm=T),
                se=plotrix::std.error(Value,na.rm=T),
                n=length(unique(SS,na.rm=T)))
data.sum<-subset(data.sum,Metric %in% c("AdColDen","JuvColDen","Ave.cl","Ave.od","Ave.rd","DZGN_G_prev","BLE_prev","CHRO_prev"))

#Convert transect back to analyst for plots
data.sum<-data.sum %>% mutate(ANALYST=dplyr::recode(TRANSECT,
                                          `1`="Jonny",
                                          `2`= "Mia",
                                          `3`="Corinne",
                                          `4`="Ro",
                                          `5`="Sam"))

data.gen<-data.gen %>% mutate(ANALYST=dplyr::recode(TRANSECT,
                                                    `1`="Jonny",
                                                    `2`= "Mia",
                                                    `3`="Corinne",
                                                    `4`="Ro",
                                                    `5`="Sam"))

##Generate plot by segment
data.l<-data.l %>% mutate(ANALYST=dplyr::recode(TRANSECT,
                                                `1`="Jonny",
                                                `2`= "Mia",
                                                `3`="Corinne",
                                                `4`="Ro",
                                                `5`="Sam"))

data.l<-left_join(data.l, segs, by = 'SS')
data.l<-subset(data.l,Metric %in% c("AdColDen","JuvColDen","Ave.cl","Ave.od","Ave.rd","DZGN_G_prev","BLE_prev","CHRO_prev"))
data.l$Number <- as.character(data.l$Number)

#Plot between observer 
p1<-ggplot(data.l, aes(x=SITE, y=Value, fill = ANALYST)) + 
  geom_bar(position="dodge", stat="identity", color="black") + 
  facet_grid(Metric~SS, scales="free")+# , labeller=label_parsed) +
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=.15, position=position_dodge(.9)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    )+
  labs(x = "Segment")


ggsave(p1,file="T:/Benthic/Data/SfM/Calibration Plots/MARAMP25/20251113_AllMetrics_Calibration.png",width=12,height=10)


t<-read.csv("T:/Benthic/Data/SfM/Calibration QC/2025/MARAMP25_SfMAdult_CLEANED.csv")
t$ANALYST[t$ANALYST == "CGA"] <- "CA"

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


p3 <- ggplot(data.gen[data.gen$GENUS_CODE!= "SSSS",], aes(x = GENUS_CODE, y = AdColDen, fill = ANALYST))+ 
  geom_bar(position=position_dodge(), stat="identity")+
  facet_grid(SS~GENUS_CODE, scales = "free")+
  theme_bw()+theme(
    axis.ticks.x = element_blank(), # no x axis ticks
    axis.text.x = element_blank())


ggsave(p1,file="T:/Benthic/Data/SfM/Calibration Plots/MARAMP25/20251113_RDconditionsCalibration.png",width=12,height=10)
ggsave(p2,file="T:/Benthic/Data/SfM/Calibration Plots/MARAMP25/20251113_ConditionsCalibration.png",width=12,height=10)
ggsave(p3,file="T:/Benthic/Data/SfM/Calibration Plots/MARAMP25/20251113_DiversityCalibration.png",width=18,height=10)
