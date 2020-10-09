#This script reads in the diver and SfM-generated demographic data that has been QC'd and cleaned up
#Then generates segment-level summarized that for methods comparision

rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
library(gridExtra)
library(reshape2)
library(plyr)
#install.packages("ggpmisc")
library(hydroGOF)
library(tidyverse)
library(ggpmisc)
library(lme4)
library(nlme)
library(ggplot2)
library(sjPlot)
library(pander)
library(tidyr)
library(AICcmodavg)
library(MuMIn)
library(knitr)
library(glmmTMB)
library(car)


source("T:/Benthic/Data/SfM/ScriptFiles/SfMvDiver Plotting Functions.R") 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")

setwd("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision")

strat<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMGENUS_STRATA.csv")
sector<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_SfMGENUS_SECTOR.csv")


Plot1to1_new<-function(d,response_variable,predictor_variable,r_name,p_name){
  #sub<-d[d$taxon,]
  d$Y<-d[,response_variable]
  d$X<-d[,predictor_variable]
  mx_val<-max(d$Y, d$X, na.rm = TRUE)
  
  corr<-cor.test(d$X, d$Y, method="pearson")
  rmse<-rmse(d$Y, d$X,na.rm=TRUE)
  r_text<-paste("RMSE = ", round(rmse,digits = 2),"\n r = ", round((corr$estimate),2), sep="")
  
  p1<-ggplot(d, aes(x=X, y=Y)) + 
    geom_point(size=1) + 	geom_abline(slope=1, intercept=0) +
    geom_smooth(method="lm", color="red", linetype="dashed", se=F) +
    geom_label(aes((mx_val/5), (mx_val * 0.9), label=r_text), nudge_y=-0.1, nudge_x=1,label.size=0.35, color="black", fill="white") +
    theme_bw()+
    theme(panel.grid.major = element_blank()
          ,panel.grid.minor = element_blank())+
    scale_x_continuous(limits=c(0,mx_val)) +
    scale_y_continuous(limits=c(0,mx_val)) +
    ylab(r_name) +  xlab(p_name)     
  return(p1)
} # 
PlotMethod<-function(d,grouping_field,metric_field,genus_field,metric_name,x,y,siglabel){
  d$GROUP<-d[,grouping_field]
  d$METRIC<-d[,metric_field]
  s<-subset(d,GROUP==genus_field)
  
  p<-ggplot(s, aes(x=METHOD, y=METRIC, fill=METHOD)) + 
    geom_boxplot() +
    geom_label(label=siglabel, x=x,y=y,label.size = 0.35,color = "black", fill="white")+
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 0)
      ,plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      ,axis.ticks.x = element_blank() # no x axis ticks
      ,axis.title.x = element_text( vjust = -.0001)
      ,legend.position="none")+ # adjust x axis to lower the same amount as the genus labels
    labs(x="Method",y=metric_name)
  return(p)
}
PlotHabitat<-function(d,grouping_field,metric_field,genus_field,metric_name,x,y,siglabel){
  d$GROUP<-d[,grouping_field]
  d$METRIC<-d[,metric_field]
  s<-subset(d,GROUP==genus_field)
  
  p<-ggplot(s, aes(x=HAB_R1, y=METRIC, fill=METHOD)) + 
    geom_boxplot() +
    theme_bw() +
    geom_label(label=siglabel, x=x,y=y,label.size = 0.35,color = "black", fill="white")+
    theme(
      axis.text.x = element_text(angle = 90)
      ,plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      ,axis.ticks.x = element_blank() # no x axis ticks
      ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
      ,legend.position="none")+
    labs(x="Habitat Type",y=metric_name)
  return(p)
}
PlotDepth<-function(d,grouping_field,metric_field,metric_field2,genus_field,metric_name,x,y,siglabel){
  d$GROUP<-d[,grouping_field]
  d$METRIC<-d[,metric_field]
  newdat.lme$METRIC2<-newdat.lme[,metric_field2]
  
  s<-subset(d,GROUP==genus_field)
  
  p<-ggplot(s, aes(x = MAX_DEPTH_M, y = METRIC, color = METHOD) ) +
    geom_point(aes(colour = factor(METHOD))) +
    geom_line(data = newdat.lme, aes(y = METRIC2), size = 1)+
    geom_ribbon(data = newdat.lme, aes(y = NULL, ymin = lower, ymax = upper, 
                                       color = NULL, fill = METHOD),alpha = .15)+
    geom_label(label=siglabel, x=x,y=y,label.size = 0.35,color = "black", fill="white")+
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 0)
      ,plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      ,axis.ticks.x = element_blank() # no x axis ticks
      ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
      ,legend.position="none")+
    labs(x="Max Depth (m)",y=metric_name)
  return(p)
}


# Setting up STRATA dataframe ----------------------------------------------------

#strat<-subset(strat,n>2)

#Select columns to keep in Strata data
strat.new<-dplyr::select(strat, c(METHOD,Stratum,GENUS_CODE,Ave.rd,
                                BLE,AcuteDZ_Prev,ChronicDZ_Prev,Sector,DB_RZ,n))

colnames(strat.new)[which(colnames(strat.new) == 'BLE')] <- "BLE_prev"
colnames(strat.new)[which(colnames(strat.new) == 'AcuteDZ_Prev')] <- "AcuteDZ_prev"
colnames(strat.new)[which(colnames(strat.new) == 'ChronicDZ_Prev')] <- "ChronicDZ_prev"

sfm_strat<-subset(strat.new,METHOD=="SfM")
diver_strat<-subset(strat.new,METHOD=="Diver")


#Set up in wide format
colnames(sfm_strat)[4:7] <- paste("SfM_", colnames(sfm_strat[,c(4:7)]), sep = "");sfm_strat<-dplyr::select(sfm_strat,-c(METHOD))

colnames(diver_strat)[4:7] <- paste("Diver_", colnames(diver_strat[,c(4:7)]), sep = "");diver_strat<-dplyr::select(diver_strat,-c(METHOD,Sector,DB_RZ,n))

strat.wide<-merge(sfm_strat,diver_strat,by=c("Stratum","GENUS_CODE"),all=T)

length(unique(strat.wide$Stratum)) 

#################IMPORTANT- CHECK FOR MERGING ERRORS
head(subset(strat.wide,GENUS_CODE=="SSSS")) #Make sure columns merge properly

#Merge together and remove strats that aren't merging properly
strat.wide<-merge(sfm_strat,diver_strat,by=c("Stratum","GENUS_CODE"))
View(strat.wide) 

head(subset(strat.wide,GENUS_CODE=="SSSS"))


# Setting up SECTOR dataframe ---------------------------------------------
colnames(sector)[which(colnames(sector) == 'DOMAIN_SCHEMA')] <- "Sector"
colnames(sector)[which(colnames(sector) == 'Mean_Ave.rd')] <- "Ave.rd"
colnames(sector)[which(colnames(sector) == 'Mean_BLE_Prev')] <- "BLE_prev"
colnames(sector)[which(colnames(sector) == 'Mean_AcuteDZ_Prev')] <- "AcuteDZ_prev"
colnames(sector)[which(colnames(sector) == 'Mean_ChronicDZ_Prev')] <- "ChronicDZ_prev"

sector.new<-dplyr::select(sector, c(METHOD,Sector,GENUS_CODE,Ave.rd,
                                    BLE_prev,AcuteDZ_prev,ChronicDZ_prev,n))


sfm_sector<-subset(sector.new,METHOD=="SfM")
diver_sector<-subset(sector.new,METHOD=="Diver")

#Set up in wide format
colnames(sfm_sector)[4:7] <- paste("SfM_", colnames(sfm_sector[,c(4:7)]), sep = "");sfm_sector<-dplyr::select(sfm_sector,-c(METHOD))

colnames(diver_sector)[4:7] <- paste("Diver_", colnames(diver_sector[,c(4:7)]), sep = "");diver_sector<-dplyr::select(diver_sector,-c(METHOD,n))

sector.wide<-merge(sfm_sector,diver_sector,by=c("Sector","GENUS_CODE"),all=T)

length(unique(sector.wide$Sector)) 

#################IMPORTANT- CHECK FOR MERGING ERRORS
head(subset(sector.wide,GENUS_CODE=="SSSS")) #Make sure columns merge properly

#Merge together and remove sectors that aren't merging properly
sector.wide<-merge(sfm_sector,diver_sector,by=c("Sector","GENUS_CODE"))
View(sector.wide) 

head(subset(sector.wide,GENUS_CODE=="SSSS"))

#Subset total sclerc.
s<-subset(strat.new,GENUS_CODE=="SSSS")
sec<-subset(sector.new,GENUS_CODE=="SSSS")

s.wide<-subset(strat.wide,GENUS_CODE=="SSSS")
sec.wide<-subset(sector.wide,GENUS_CODE=="SSSS")


#RECENT DEAD

hist(s$Ave.rd)
s$logAve.rd<-log(s$Ave.rd+1)
hist(s$logAve.rd)

mod<-lm(logAve.rd~METHOD,data=s)
plot(mod)
qqnorm(resid(mod)) #plot normal quantile- quantile plot.  Should be close to a straight line

#Can't transform recent dead with sqrt, log or power transformation even after removing strats that have 0 rd in both sfm and diver
wilcox.test(Ave.rd ~ METHOD, data=s) 

t<-subset(s,METHOD=="Diver")

p1<-Plot1to1_new(s.wide,"SfM_Ave.rd","Diver_Ave.rd","SfM Average % Recent Dead","Diver % Recent Dead")
p2<-PlotMethod(strat.new,"GENUS_CODE","Ave.rd","SSSS","Average % Recent Dead",1.5,3.5,"NS")

Ave.rdS<-grid.arrange(p1,p2,nrow=1,ncol=2)

#ggsave(plot<-Ave.rdS,file="T:/Benthic/Data/SfM/Method Comparision/Figures/Ave.rdSSSS_stats.png",width=8,height=6)


s<-subset(sector,GENUS_CODE=="SSSS")
s.wide<-subset(sector.wide,GENUS_CODE=="SSSS")


hist(s$Ave.rd)
s$logAve.rd<-log(s$Ave.rd+1)
hist(s$logAve.rd)

mod<-lm(logAve.rd~METHOD,data=s)
plot(mod)
qqnorm(resid(mod)) #plot normal quantile- quantile plot.  Should be close to a straight line

#Can't transform recent dead with sqrt, log or power transformation even after removing strats that have 0 rd in both sfm and diver
wilcox.test(Ave.rd ~ METHOD, data=s) 

#remove NII-WEST-outlier
#s.wide2<-subset(s.wide,Sector!="NII_WEST")

p1<-Plot1to1_new(s.wide,"SfM_Ave.rd","Diver_Ave.rd","SfM Average % Recent Dead","Diver % Recent Dead")
p2<-PlotMethod(sector,"GENUS_CODE","Ave.rd","SSSS","Average % Recent Dead",1.5,3.5,"NS")

Ave.rdS<-grid.arrange(p1,p2,nrow=1,ncol=2)



#Acute disease prevalence
hist(s$AcuteDZ_prev)
s$logAcuteDZ_prev<-log(s$AcuteDZ_prev+1)
hist(s$logAcuteDZ_prev)

mod<-lm(logAcuteDZ_prev~METHOD,data=s)
plot(mod)
qqnorm(resid(mod)) #plot normal quantile- quantile plot.  Should be close to a straight line



#Can't transform recent dead with bionomial distrubtion, sqrt, log or power transformation even after removing strats that have 0 rd in both sfm and diver
wilcox.test(AcuteDZ_prev ~ METHOD, data=s) 

p1<-Plot1to1_new(s.wide,"SfM_AcuteDZ_prev","Diver_AcuteDZ_prev","SfM Prevalence (%)","Diver Prevalence (%)");p1<-p1+ggtitle("Acute Disease")
p2<-PlotMethod(strat.new,"GENUS_CODE","AcuteDZ_prev","SSSS","Prevalence (%)",1.5,14,"NS")

acutedzS<-grid.arrange(p1,p2,nrow=1,ncol=2)

wilcox.test(AcuteDZ_prev ~ METHOD, data=sec) 

p1<-Plot1to1_new(sec.wide,"SfM_AcuteDZ_prev","Diver_AcuteDZ_prev","SfM Prevalence (%)","Diver Prevalence (%)");p1<-p1+ggtitle("Acute Disease")
p2<-PlotMethod(sector.new,"GENUS_CODE","AcuteDZ_prev","SSSS","Prevalence (%)",1.5,14,"NS")

acutedzSec<-grid.arrange(p1,p2,nrow=1,ncol=2)

#ggsave(plot<-acutedzS,file="T:/Benthic/Data/SfM/Method Comparision/Figures/AcuteDZSSSS_stats.png",width=8,height=6)


#Chronic disease prevalence
hist(s$ChronicDZ_prev)
s$logChronicDZ_prev<-log(s$ChronicDZ_prev+1)
hist(s$logChronicDZ_prev)

mod<-lm(ChronicDZ_prev~METHOD,data=s)
plot(mod)
qqnorm(resid(mod)) #plot normal quantile- quantile plot.  Should be close to a straight line


#Can't transform recent dead with bionomial distrubtion, sqrt, log or power transformation even after removing strats that have 0 rd in both sfm and diver
wilcox.test(ChronicDZ_prev ~ METHOD, data=s) 

p1<-Plot1to1_new(s.wide,"SfM_ChronicDZ_prev","Diver_ChronicDZ_prev","SfM Prevalence (%)","Diver Prevalence (%)");p1<-p1+ggtitle("Chronic Disease")
p2<-PlotMethod(strat.new,"GENUS_CODE","ChronicDZ_prev","SSSS","Prevalence (%)",1.5,9.5,"NS")

ChronicdzS<-grid.arrange(p1,p2,nrow=1,ncol=2)

p1<-Plot1to1_new(sec.wide,"SfM_ChronicDZ_prev","Diver_ChronicDZ_prev","SfM Prevalence (%)","Diver Prevalence (%)");p1<-p1+ggtitle("Chronic Disease")
p2<-PlotMethod(sector.new,"GENUS_CODE","ChronicDZ_prev","SSSS","Prevalence (%)",1.5,9.5,"NS")

ChronicdzSec<-grid.arrange(p1,p2,nrow=1,ncol=2)

#ggsave(plot<-ChronicdzS,file="T:/Benthic/Data/SfM/Method Comparision/Figures/ChronicDZSSSS_stats.png",width=8,height=6)


#Bleaching prevalence
hist(s$BLE_prev)
s$logBLE_prev<-log(s$BLE_prev+1)
hist(s$logBLE_prev)

mod<-lm(logBLE_prev~METHOD,data=s)
plot(mod)
qqnorm(resid(mod)) #plot normal quantile- quantile plot.  Should be close to a straight line

#Can't transform recent dead with bionomial distrubtion, sqrt, log or power transformation even after removing strats that have 0 rd in both sfm and diver
wilcox.test(BLE_prev ~ METHOD, data=s) 


p1<-Plot1to1_new(s.wide,"SfM_BLE_prev","Diver_BLE_prev","SfM Prevalence (%)","Diver Prevalence (%)");p1<-p1+ggtitle("Bleaching")
p2<-PlotMethod(strat.new,"GENUS_CODE","BLE_prev","SSSS"," Prevalence (%)",1.5,65,"Significant")

BLES<-grid.arrange(p1,p2,nrow=1,ncol=2)

p1<-Plot1to1_new(sec.wide,"SfM_BLE_prev","Diver_BLE_prev","SfM Prevalence (%)","Diver Prevalence (%)");p1<-p1+ggtitle("Bleaching")
p2<-PlotMethod(sector.new,"GENUS_CODE","BLE_prev","SSSS"," Prevalence (%)",1.5,65,"Significant")

BLESec<-grid.arrange(p1,p2,nrow=1,ncol=2)

#ggsave(plot<-BLES,file="T:/Benthic/Data/SfM/Method Comparision/Figures/BLEPrevSSSS_stats.png",width=8,height=6)


allplotStrat<-grid.arrange(acutedzS,ChronicdzS,BLES,nrow=3,ncol=1,
                       top = textGrob("Strata",gp=gpar(fontsize=20,font=3)))

allplotSector<-grid.arrange(acutedzSec,ChronicdzSec,BLESec,nrow=3,ncol=1,
                            top = textGrob("Sector",gp=gpar(fontsize=20,font=3)))

ggsave(plot<-allplotStrat,file="T:/Benthic/Data/SfM/Method Comparision/Figures/ConditionsALL_Strat.png",width=8,height=10)
ggsave(plot<-allplotSector,file="T:/Benthic/Data/SfM/Method Comparision/Figures/ConditionsALL_Sec.png",width=8,height=10)

