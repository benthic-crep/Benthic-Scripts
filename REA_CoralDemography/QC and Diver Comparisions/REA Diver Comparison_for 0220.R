#Diver Comparision
rm(list=ls())

#Set Run Flags
DEBUG=TRUE

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

## LOAD benthic data
# awd<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_E_raw_CLEANED.csv")
# jwd<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_F_raw_CLEANED.csv")

awd<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Project-specific scripts/Juvenile Project/CoralBelt_E_raw_CLEANED.csv")
jwd<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Project-specific scripts/Juvenile Project/CoralBelt_F_raw_CLEANED.csv")


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


#Old Dead

#% of colonies with an OLD DEAD measurement >0
od_sub<-subset(awd,S_ORDER=="Scleractinia"&Fragment==0);head(od_sub)

od_sum<-ddply(od_sub,.(REGION,OBS_YEAR,DIVER),summarize,
              totalcount=length(COLONYID))
head(od_sum)
odG0<-subset(od_sub,OLDDEAD==0)
odG0sum<-ddply(odG0,.(REGION,OBS_YEAR,DIVER),summarize,
               G0count=length(COLONYID),
               meanCL=mean(COLONYLENGTH))

od.all<-left_join(od_sum,odG0sum)

od.all$percentGr0<-od.all$G0count/od.all$totalcount*100
head(od.all)

od.all<-subset(od.all,totalcount>=100)

mhi19<-subset(od.all,REGION=="MHI" &OBS_YEAR=="2019")

library(forcats)
p1<-mhi19 %>%
  mutate(DIVER = fct_reorder(DIVER, percentGr0)) %>%
  ggplot(aes(x=DIVER, y=percentGr0, fill=DIVER)) + 
  geom_bar(position=position_dodge(), stat="identity") + 
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001))+
  geom_text(aes(label=totalcount), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(x="Diver",y="% of Colonies 0% Old Dead")

p1

p2<-ggplot(mhi19,aes(x=DIVER, y=percentGr0, fill=DIVER)) + 
  geom_bar(position=position_dodge(), stat="identity") + 
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001))+
  geom_text(aes(label=totalcount), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(x="Diver",y="% of Colonies 0% Old Dead")

p2

p2<-ggplot(mhi19,aes(x=meanCL, y=percentGr0, color=DIVER,label=DIVER)) + 
  geom_point(size=2)+
  theme_bw() +
  geom_text(aes(colour = factor(DIVER)),hjust = 0, nudge_x = 0.05)+
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001)
    ,axis.text.x = element_text(angle = 90)) + # adjust x axis to lower the same amount as the genus labels
  
  labs(x="Mean Colony Length",y="% of Colonies 0% Old Dead")
p2


#Just look at 10cm colonies

od_sub<-subset(awd,S_ORDER=="Scleractinia"&Fragment==0&COLONYLENGTH<15);head(od_sub)

od_sum<-ddply(od_sub,.(REGION,OBS_YEAR,DIVER),summarize,
              totalcount=length(COLONYID))
head(od_sum)
odG0<-subset(od_sub,OLDDEAD==0)
odG0sum<-ddply(odG0,.(REGION,OBS_YEAR,DIVER),summarize,
               G0count=length(COLONYID),
               meanCL=mean(COLONYLENGTH))

od.all<-left_join(od_sum,odG0sum)

od.all$percentGr0<-od.all$G0count/od.all$totalcount*100
head(od.all)

od.all<-subset(od.all,totalcount>=100)

mhi19<-subset(od.all,REGION=="MHI" &OBS_YEAR=="2019")

##Show this figure
p5<-mhi19 %>%
  mutate(DIVER = fct_reorder(DIVER, percentGr0)) %>%
  ggplot(aes(x=DIVER, y=percentGr0, fill=DIVER)) + 
  geom_bar(position=position_dodge(), stat="identity") + 
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001))+
  geom_text(aes(label=totalcount), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(x="Diver",y="% of Colonies 0% Old Dead (5-15cm colonies only)")

p5




# plot this against the average old dead
mhi<-subset(awd,S_ORDER=="Scleractinia"&Fragment==0&REGION=="MHI" & OBS_YEAR=="2019");head(mhi)

mhi$SS=paste0(mhi$SITE,mhi$TRANSECT,mhi$SEGMENT,sep="_")
mhi$SEGAREA=mhi$SEGWIDTH*mhi$SEGLENGTH
mhi.sum<-ddply(subset(mhi,SEGAREA==2.5),.(ISLAND,DIVER),summarize,
               nCOL=length(COLONYLENGTH),
               meanCD=nCOL/(2.5*length(unique(SS))),
               meanCL=mean(COLONYLENGTH),
               seCL=sd(COLONYLENGTH,na.rm=T)/sqrt(length(COLONYLENGTH)),
               meanOD=mean(OLDDEAD),
               seOD=sd(OLDDEAD,na.rm=T)/sqrt(length(OLDDEAD)))




p3<-ggplot(mhi,aes(x=COLONYLENGTH, y=OLDDEAD, color=DIVER)) + 
  geom_point(color="grey")+
  geom_smooth(se=FALSE,method="lm",lwd=1.5)+
  facet_wrap(~ISLAND,scales="free")+
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001)
    ,axis.text.x = element_text(angle = 90)) + # adjust x axis to lower the same amount as the genus labels
  scale_y_continuous(breaks=seq(0,100,25))+
  labs(x="Colony Length",y="% Old Dead")
p3

p4<-ggplot(mhi.sum,aes(x=meanCL, y=meanCD, color=DIVER)) + 
  geom_point(aes(size=nCOL))+
  geom_text(aes(label=DIVER),size=3,color="black")+
#  geom_errorbar(aes(ymax=meanOD+seOD,ymin=meanOD-seOD))+
  geom_errorbarh(aes(xmax=meanCL+seCL,xmin=meanCL-seCL))+
  facet_wrap(~ISLAND,scales="free")+
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001)
    ,axis.text.x = element_text(angle = 90)) + # adjust x axis to lower the same amount as the genus labels
 # scale_y_continuous(breaks=seq(0,100,25))+
  labs(x="Colony Length",y="COlony DEnsity")#y="% Old Dead")
p4





mhi<-subset(awd,S_ORDER=="Scleractinia"&Fragment==0&REGION=="MHI" & OBS_YEAR=="2019"& OLDDEAD==0);head(mhi)

mhi.sum<-ddply(mhi,.(ISLAND,DIVER),summarize,
               meanCL=mean(COLONYLENGTH),
               seCL=sd(COLONYLENGTH,na.rm=T)/sqrt(length(COLONYLENGTH)))

p3<-mhi.sum %>%
  mutate(DIVER = fct_reorder(DIVER, meanCL)) %>%
  ggplot(aes(x=DIVER, y=meanCL, fill=DIVER)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=meanCL-seCL,ymax=meanCL+seCL,height=.75))+
  theme_bw() +
  facet_wrap(~ISLAND)+
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001))+
  labs(x="Diver",y="Mean Colony Size with 0% OD")

p3




