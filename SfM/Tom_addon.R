#This script has NOT been updated to Tom's most recent figures

rm(list=ls())

#source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/SfMvDiver Plotting Functions.R") 
source("C:/Users/Corinne.Amir/Documents/GitHub/Benthic-Scripts/Functions/SfMvDiver Plotting Functions.R") 
#Plot1to1; PlotBioAlt; PlotPair

#data.gen<-read.csv("T:/Benthic/Data/SfM/Summarized Data/HARAMP_repeats_GENUS_Summarized Data.csv")
data.gen<-read.csv("T:/Benthic/Data/SfM/Summarized Data/HARAMP_repeats_GENUS_Summarized Data.csv")

library(plyr)
data.gen$SST=paste0(data.gen$SS,"_",data.gen$GENUS_CODE)
seg4list=ddply(data.gen,.(SST,SS),summarize,NBox=length(unique(MethodRep)))
all4seglist=subset(seg4list,NBox>=4)
length(unique(all4seglist[,"SS"]))

data.sm=subset(data.gen,SST %in% all4seglist$SST&GENUS_CODE=="SSSS"|
                                 all4seglist$SST&GENUS_CODE=="POSP"|
                                 all4seglist$SST&GENUS_CODE=="POCS"|
                                 all4seglist$SST&GENUS_CODE=="MOSP")
                                
                                 
data.sm[is.na(data.sm)]=0
dim(data.sm)
length(unique(data.sm$SS))

dscol=c("AdColDen","JuvColDen","Ave.cl","Ave.od", "Ave.rd","DZGN_G_prev","BLE_prev","CHRO_prev")
#dscol=c("AdColDen","JuvColDen","Ave.cl","Ave.od","BLE_prev")
library(reshape2)
mm=melt(data.sm,measure.vars = dscol)
mm=subset(mm,select=-c(DZGN_G_den,BLE_den,CHRO_den,JuvColCount,AdColCount))
head(mm)

mm.add=ddply(mm,.(MethodRep,variable),summarise,
             value_mn=mean(value,na.rm=T),
             value_se=sd(value,na.rm=T)/sqrt(length(value)),
             value_cv=value_se/value_mn)

library(ggplot2)
plot1<- ggplot(mm,aes(x=MethodRep,y=value))+geom_violin()+
  facet_wrap("variable",scales ="free_y") +
  geom_jitter(height=0,width=.1, col="black")+
  geom_errorbar(data=mm.add, aes(y=value_mn,ymin=value_mn-value_cv,ymax=value_mn-value_cv,width=.75), col="gray")+
  geom_errorbar(data=mm.add, aes(y=value_mn,ymin=value_mn+value_cv,ymax=value_mn+value_cv,width=.75), col="gray")+
  geom_errorbar(data=mm.add, aes(y=value_mn,ymin=value_mn,ymax=value_mn,width=.75), col="red")+
  scale_y_sqrt()+theme_bw()
  
ggsave(plot=plot1,file="T:/Benthic/Data/SfM/ComparisionPlots/SSSS_Comparision_densitydistribution.pdf",width=12,height=12)

plot2<- ggplot(mm,aes(x=MethodRep,y=value))+geom_boxplot()+
  facet_wrap("variable",scales ="free_y") +
  geom_jitter(height=0,width=.1, col="black")+
  geom_errorbar(data=mm.add, aes(y=value_mn,ymin=value_mn,ymax=value_mn),size=.9,width=.75,col="red")+
  scale_y_sqrt()+theme_bw()

plot2
ggsave(plot=plot2,file="T:/Benthic/Data/SfM/ComparisionPlots/SSSS_Comparision_boxplot.pdf",width=12,height=12)
