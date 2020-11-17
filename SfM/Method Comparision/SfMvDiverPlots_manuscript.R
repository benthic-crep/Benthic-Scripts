rm(list=ls())

library(gridExtra)
library(reshape2)
library(plyr)
#install.packages("ggpmisc")
library(hydroGOF)
library(tidyverse)
library(ggpmisc)
library(plotrix)
library(FSA)
library(dplyr)

source("T:/Benthic/Data/SfM/ScriptFiles/SfMvDiver Plotting Functions.R") 

data.gen_tmp<-read.csv("T:/Benthic/Data/SfM/Summarized Data/HARAMP_repeats_GENUS_Summarized Data.csv")
div.data<-read.csv("T:/Benthic/Data/SfM/Summarized Data/HARAMP_repeats_DIVERSITY_Summarized Data.csv")

#Merge genus-level and diversity data- use SSSS for diversity data
div.data$GENUS_CODE<-"SSSS"
div.data<-subset(div.data,select = -c(ANALYST,METHOD.1)) #remove columns we don't need before merging
data.gen_tmp<-subset(data.gen_tmp,select = -c(METHOD.1))
nrow(data.gen_tmp)
data.gen<-left_join(data.gen_tmp,div.data)
nrow(data.gen)

length(unique(div.data$SS))


#List of segments that were surveyed by all methods and multiple divers....UNEQUAL
sfm2<-data.gen[data.gen$MethodRep=="SfM_2",] 
length(unique(sfm2$SS)) #44
sfm1<-data.gen[data.gen$MethodRep=="SfM_1",]
length(unique(sfm1$SS)) #44
diver1<-data.gen[data.gen$MethodRep=="DIVER_1",]
length(unique(diver1$SS)) #689
diver2<-data.gen[data.gen$MethodRep=="DIVER_2",]
length(unique(diver2$SS)) #43


#Create dataframe containing only site_segments that contain all 4 methodreps
data.gen$SST=paste0(data.gen$SS,"_",data.gen$GENUS_CODE)
seg4list=ddply(data.gen,.(SST,SS),summarize,NBox=length(unique(MethodRep)))
all4seglist=subset(seg4list,NBox>=4)
length(unique(all4seglist[,"SS"]))


#Create dataframe containing only sites containing 2 annotators and 2 divers
data.sm=subset(data.gen,SST%in%all4seglist$SST)
dim(data.sm)
length(unique(data.sm$SS))


#Check that all Site_Segments being used for analysis have 2 annotators and 2 divers
ddply(data.sm,.(SITE, SEGMENT), summarize, num.repeats = n_distinct(MethodRep)) #should be 4
ddply(data.sm,.(SITE, SEGMENT), summarize, num.repeats = n_distinct(METHOD)) #should = 2


# Plotting Regressions and Bland-Altman by Taxon --------------------------

### Separate 4 datasets and add the dataset name before the metric columns. Need to set up dataframe this way to make plotting easier.
d1<-data.sm[data.sm$MethodRep=="DIVER_1",];colnames(d1)[8:20] <- paste("d1", colnames(d1[8:20]), sep = "");d1<-subset(d1,select=-c(METHOD,SEGAREA_ad,MethodRep,TRANSECT,METHOD.1))
d2<-data.sm[data.sm$MethodRep=="DIVER_2",];colnames(d2)[8:20] <- paste("d2", colnames(d2[8:20]), sep = "");d2<-subset(d2,select=-c(METHOD,SEGAREA_ad,MethodRep,TRANSECT,METHOD.1))
sfm1<-data.sm[data.sm$MethodRep=="SfM_1",];colnames(sfm1)[8:20] <- paste("SfM1", colnames(sfm1[8:20]), sep = "");sfm1<-subset(sfm1,select=-c(METHOD,SEGAREA_ad,MethodRep,TRANSECT,METHOD.1))
sfm2<-data.sm[data.sm$MethodRep=="SfM_2",];colnames(sfm2)[8:20] <- paste("SfM2", colnames(sfm2[8:20]), sep = "");sfm2<-subset(sfm2,select=-c(METHOD,SEGAREA_ad,MethodRep,TRANSECT,METHOD.1))


# #4 datasets together
# df.all <- join_all(list(d1,d2,sfm1,sfm2), by= c("SITE","SITEVISITID","SEGMENT","GENUS_CODE","SS","OBS_YEAR","REGION","ISLAND","SEC_NAME","REEF_ZONE",
#                                                 "DEPTH_BIN","HABITAT_CODE", "LATITUDE","LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M"), type='full'); 
# head(df.all) 
# nrow(df.all)
# 
# 
# #Plot figures
# #PlotAll(dataframe, variable 1, variable 2, y-axis name 1, y-axis name 2, x-axis name 1, x-axis name 2)
# 
# outpath<- "T:/Benthic/Data/SfM/ComparisonPlots_20200204/Adult Density"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p1<-PlotAll(df.all,"d1AdColDen","SfM1AdColDen","SfM Adult Density","Difference SfM Analyst and Diver", "Diver Adult Density","Mean Adult Density"); p1
# p2<-PlotAll(df.all,"d1AdColDen","d2AdColDen","Diver1 Adult Density","Difference Diver1 and Diver2","Diver2 Adult Density","Mean Adult Density"); p2
# p3<-PlotAll(df.all,"SfM1AdColDen","SfM2AdColDen","SfM1 Adult Density","Difference SfM Analyst1 and SfM Analyst2","SfM2 Adult Density","Adult Density"); p3
# 
# outpath<-"T:/Benthic/Data/SfM/ComparisonPlots_20200204/Juvenile Density"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p4<-PlotAll(df.all,"d1JuvColDen","SfM1JuvColDen","SfM Juvenile Density","Difference SfM Analyst and Diver", "Diver Juvenile Density","Mean Juvenile Density"); p4
# p5<-PlotAll(df.all,"d1JuvColDen","d2JuvColDen","Diver1 Juvenile Density","Difference Diver1 and Diver2","Diver2 Juvenile Density","Mean Juvenile Density"); p5
# p6<-PlotAll(df.all,"SfM1JuvColDen","SfM2JuvColDen","SfM1 Juvenile Density","Difference SfM Analyst1 and SfM Analyst2","SfM2 Juvenile Density","Mean Juvenile Density"); p6
# 
# outpath<-"T:/Benthic/Data/SfM/ComparisonPlots_20200204/Colony Size"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p7<-PlotAll(df.all,"d1Ave.cl","SfM1Ave.cl","SfM Colony Diameter","Difference SfM Analyst and Diver", "Diver Colony Diameter","Mean Colony Diameter")
# p8<-PlotAll(df.all,"d1Ave.cl","d2Ave.cl","Diver1 Colony Diameter","Difference Diver1 and Diver2","Diver2 Colony Diameter","Mean Colony Diameter")
# p9<-PlotAll(df.all,"SfM1Ave.cl","SfM2Ave.cl","SfM1 Colony Diameter","Difference SfM Analyst1 and SfM Analyst2","SfM2 Colony Diameter","Mean Colony Diameter")
# 
# outpath<-"T:/Benthic/Data/SfM/ComparisonPlots_20200204/Old Dead"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p10<-PlotAll(df.all,"d1Ave.od","SfM1Ave.od","SfM Average Old Dead Pct","Difference SfM Analyst and Diver", "Diver Average Old Dead Pct","Average Old Dead Pct")
# p11<-PlotAll(df.all,"d1Ave.od","d2Ave.od","Diver1 Average Old Dead Pct","Difference Diver1 & Diver2", "Diver2 Average Old Dead Pct","Average Old Dead Pct")
# p12<-PlotAll(df.all,"SfM1Ave.od","SfM2Ave.od","SfM1 Average Old Dead Pct","Difference SfM Analyst1 and SfM Analyst2", "SfM2 Average Old Dead Pct","Average Old Dead Pct")
# 
# outpath<-"T:/Benthic/Data/SfM/ComparisonPlots_20200204/Recent Dead"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p13<-PlotAll(df.all,"d1Ave.rd","SfM1Ave.rd","SfM Average Recent Dead Pct","Difference SfM Analyst and Diver", "Diver Average Recent Dead Pct","Average Recent Dead Pct")
# p14<-PlotAll(df.all,"d1Ave.rd","d2Ave.rd","Diver1 Average Recent Dead Pct","Difference Diver1 and Diver2","Diver2 Average Recent Dead Pct","Average Recent Dead Pct")
# p15<-PlotAll(df.all,"SfM1Ave.rd","SfM2Ave.rd","SfM1 Average Recent Dead Pct","Difference SfM Analyst1 and SfM Analyst2","SfM2 Average Recent Dead Pct","Mean Average Recent Dead Pct")
# 
# outpath<-"T:/Benthic/Data/SfM/ComparisonPlots_20200204/Bleaching"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p16<-PlotAll(df.all,"d1BLE_prev","SfM1BLE_prev","SfM Bleaching Prevalence","Difference SfM Analyst and Diver", "Diver Bleaching Prevalence","Mean Bleaching Prevalence")
# p17<-PlotAll(df.all,"d1BLE_prev","d2BLE_prev","Diver1 Bleaching Prevalence","Difference Diver1 and Diver2","Diver2 Bleaching Prevalence","Mean Bleaching Prevalence")
# p18<-PlotAll(df.all,"SfM1BLE_prev","SfM2BLE_prev","SfM1 Bleaching Prevalence","Difference SfM Analyst1 and SfM Analyst2","SfM2 Bleaching Prevalence","Mean Bleaching Prevalence")
# 
# outpath<-"T:/Benthic/Data/SfM/ComparisonPlots_20200204/ChronicDZ"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p19<-PlotAll(df.all,"d1CHRO_prev","SfM1CHRO_prev","SfM Chronic Disease Prevalence","Difference SfM Analyst and Diver", "Diver Chronic Disease Prevalence","Mean Chronic Disease Prevalence")
# p20<-PlotAll(df.all,"d1CHRO_prev","d2CHRO_prev","Diver1 Chronic Disease Prevalence","Difference Diver1 and Diver2","Diver2 Chronic Disease Prevalence","Mean Chronic Disease Prevalence")
# p21<-PlotAll(df.all,"SfM1CHRO_prev","SfM2CHRO_prev","SfM1 Chronic Disease Prevalence","Difference SfM Analyst1 and SfM Analyst2","SfM2 Chronic Disease Prevalence","Mean Chronic Disease Prevalence")
# 
# outpath<-"T:/Benthic/Data/SfM/ComparisonPlots_20200204/AcuteDZ"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p22<-PlotAll(df.all,"d1DZGN_G_prev","SfM1DZGN_G_prev","SfM General Disease Prevalence","Difference SfM Analyst and Diver", "Diver General Disease Prevalence","Mean General Disease Prevalence")
# p23<-PlotAll(df.all,"d1DZGN_G_prev","d2DZGN_G_prev","Diver1 General Disease Prevalence","Difference Diver1 and Diver2","Diver2 General Disease Prevalence","Mean General Disease Prevalence")
# p24<-PlotAll(df.all,"SfM1DZGN_G_prev","SfM2DZGN_G_prev","SfM1 General Disease Prevalence","Difference SfM Analyst1 and SfM Analyst2","SfM2 General Disease Prevalence","Mean General Disease Prevalence")


# Plots for Parsing out method vs. observer error -------------------------

####Parsing out method vs. observer error

#Use long dataframe format
#The ErrorComparison function defines different x and ys depending on which senario you are interested in
#use the grouping field to group by fields like genus code, habitat code, depth bin
ErrorComparision<-function(d,grouping_field="GENUS_CODE",metric_field="AdColDen"){
  d$GROUP<-d[,grouping_field]
  d$METRIC<-d[,metric_field]
  
  d.new<-d[,c("METHOD","SITE","SITEVISITID","TRANSECT","GROUP","METRIC","MethodRep","SS")]
  
  a1<-d.new[d.new$MethodRep %in% c("DIVER_1","SfM_1"),]
  a1 <-dcast(a1, formula=SITE+SITEVISITID+GROUP+SS~METHOD,value.var="METRIC",fill=NA)
  a2<-d.new[d.new$MethodRep %in% c("DIVER_2","SfM_2"),]
  a2 <-dcast(a2, formula=SITE+SITEVISITID+GROUP+SS~METHOD,value.var="METRIC",fill=NA)
  a3<-d.new[d.new$MethodRep %in% c("DIVER_1","SfM_2"),]
  a3 <-dcast(a3, formula=SITE+SITEVISITID+GROUP+SS~METHOD,value.var="METRIC",fill=NA)
  a4<-d.new[d.new$MethodRep %in% c("DIVER_2","SfM_1"),]
  a4 <-dcast(a4, formula=SITE+SITEVISITID+GROUP+SS~METHOD,value.var="METRIC",fill=NA)
  
  a5<-d.new[d.new$MethodRep %in% c("SfM_1","SfM_2"),]
  a5 <-dcast(a5, formula=SITE+SITEVISITID+GROUP+SS~TRANSECT,value.var="METRIC",fill=NA)
  colnames(a5)<-c("SITE","SITEVISITID","GROUP","SS","X","Y")
  a5$Comp<-"S1vS2"
  a6<-d.new[d.new$MethodRep %in% c("DIVER_1","DIVER_2"),]
  a6 <-dcast(a6, formula=SITE+SITEVISITID+GROUP+SS~TRANSECT,value.var="METRIC",fill=NA)
  colnames(a6)<-c("SITE","SITEVISITID","GROUP","SS","X","Y")
  a6$Comp<-"D1vD2"
  
  MO<-rbind(a1,a2,a3,a4); colnames(MO)<-c("SITE","SITEVISITID","GROUP","SS","X","Y"); MO$Comp<-"MO"
  
  Obar<-ddply(d,.(SITE,SITEVISITID,GROUP,SS,METHOD),summarize,METRIC=mean(METRIC))
  Obar.n <-dcast(Obar, formula=SITE+SITEVISITID+GROUP+SS~METHOD,value.var="METRIC",fill=NA)
  colnames(Obar.n)<-c("SITE","SITEVISITID","GROUP","SS","X","Y")
  Obar.n$Comp<-"Obar"
  head(Obar.n)
  
  Mbar<-ddply(d,.(SITE,SITEVISITID,GROUP,SS,TRANSECT),summarize,METRIC=mean(METRIC))
  Mbar.n <-dcast(Mbar, formula=SITE+SITEVISITID+GROUP+SS~TRANSECT,value.var="METRIC",fill=NA)
  colnames(Mbar.n)<-c("SITE","SITEVISITID","GROUP","SS","X","Y")
  Mbar.n$Comp<-"Mbar"
  head(Mbar.n)
  
  all.comp<-rbind(MO,a5,a6,Obar.n,Mbar.n)
  all.comp$Metric<-metric_field
  
  colnames(all.comp)[which(colnames(all.comp) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(all.comp)
}

####
ad.comp<-ErrorComparision(data.sm,"GENUS_CODE","AdColDen")
jd.comp<-ErrorComparision(data.sm,"GENUS_CODE","JuvColDen")
cl.comp<-ErrorComparision(data.sm,"GENUS_CODE","Ave.cl")
od.comp<-ErrorComparision(data.sm,"GENUS_CODE","Ave.od")
ble.comp<-ErrorComparision(data.sm,"GENUS_CODE","BLE_prev")
div.comp<-ErrorComparision(data.sm,"GENUS_CODE","Shannon")
rich.comp<-ErrorComparision(data.sm,"GENUS_CODE","Richness")

all.rmse<-rbind(ad.comp,jd.comp,cl.comp,od.comp,ble.comp,div.comp,rich.comp)

bias_sc=function(X,Y){
  del=(X-Y)
  mn=apply(cbind(X,Y),1,mean,na.rm=T)
  dm=0.5*del/mn
  dm[which(mn==0)]=0
  return(dm)
}
ae_sc=function(X,Y){
  del=abs(X-Y)
  mn=apply(cbind(X,Y),1,mean,na.rm=T)
  dm=0.5*del/mn
  dm[which(mn==0)]=0
  return(dm)
}
meanbias_sc=function(X,Y){
  return(mean(bias_sc(X,Y),na.rm=T))
}
medianbias_sc=function(X,Y){
  return(median(bias_sc(X,Y),na.rm=T))
}
mae.mn=function(X,Y){
  return(mean(abs(X-Y),na.rm=T))
}
mae.md=function(X,Y){
  return(median(abs(X-Y),na.rm=T))
}
mae_sc=function(X,Y){
  return(mean(ae_sc(X,Y),na.rm=T))
}
seae_sc=function(X,Y){
  return(std.error(ae_sc(X,Y),na.rm=T))
}
mdae_sc=function(X,Y){
  return(median(ae_sc(X,Y),na.rm=T))
}

rmse.mn=function(X,Y){
  return(sqrt(mean((X-Y)^2,na.rm=T)))
}
rmse.md=function(X,Y){
  return(sqrt(median((X-Y)^2,na.rm=T)))
}
log2AR=function(X,Y){
  lar=log2(X/Y)
  lar[is.infinite(lar)]=NA
  lar[is.nan(lar)]=NA
  return(lar)
}

all.rmse$delta=(all.rmse$X-all.rmse$Y)
all.rmse$mnXY=(all.rmse$Y+all.rmse$X)/2
all.rmse$L2delta=(log2(all.rmse$X)-log2(all.rmse$Y))
all.rmse$L2mnXY=(log2(all.rmse$Y)+log2(all.rmse$X))/2
all.rmse$Bias_SC=bias_sc(all.rmse$Y,all.rmse$X)
all.rmse$ae_sc=ae_sc(all.rmse$Y,all.rmse$X)
ssss.err=subset(all.rmse,GENUS_CODE=="SSSS"&Comp%in%c("D1vD2","S1vS2","MO"))
ssss.err$Metric=factor(ssss.err$Metric,
                       levels=c("AdColDen","JuvColDen","BLE_prev","Ave.cl","Ave.od","Shannon","Richness"))
ssss.err$Comp=factor(ssss.err$Comp,levels=c("D1vD2","S1vS2","MO"))
ssss.err.mnsd=ddply(ssss.err,.(Metric,Comp),summarize,
                    mean_mnXY=mean(mnXY,na.rm=T),
                    q75_mnXY=quantile(mnXY,probs = .75,na.rm=T),
                    q95_mnXY=quantile(mnXY,probs = .95,na.rm=T),
                    mean_delta=mean(delta,na.rm=T),
                    sd_delta=sd(delta,na.rm=T),
                    p_1t=t.test(delta,mu=0,alternative="two.sided")$p.value,
                    sigtext=if(p_1t<0.05){paste0("* p < ",signif(p_1t,2)," *")}else{"NS"})

# AllBA=ggplot(ssss.err,aes(x=mnXY,y=delta,color=Metric,fill=Metric))+
#   geom_point()+
#   stat_smooth(method="lm",lty=2,fill="gray",alpha=.5)+
#   guides(color=FALSE,fill=FALSE)+
#   geom_abline(intercept = 0,slope=2)+
#   geom_abline(intercept = 0,slope=-2)+
#   geom_hline(yintercept = 0)+
#   geom_hline(data=ssss.err.mnsd,aes(yintercept = mean_delta),color="red",lty=5)+
#   geom_text(data=ssss.err.mnsd,aes(label=sigtext),
#             x=-Inf,y = -Inf,hjust=.25,vjust=1,
#             size=2,color="red")+
#   geom_text_npc(data=ssss.err.mnsd,aes(label=sigtext),
#                 npcx=0.5,npcy = .99,hjust=.25,vjust=1,
#                 size=2,color="red")+
#   geom_hline(data=ssss.err.mnsd,aes(yintercept = mean_delta-1.96*sd_delta),color="pink",lty=5)+
#   geom_hline(data=ssss.err.mnsd,aes(yintercept = mean_delta+1.96*sd_delta),color="pink",lty=5)+
#   facet_wrap(Comp~Metric,scales="free",nrow=3)
# 
# AllMetricOutPath="T:/Benthic/Data/SfM/ComparisonPlots_20200204/AllMetricsComp/"
# if(!dir.exists(AllMetricOutPath)){dir.create(AllMetricOutPath)}
# ggsave(plot = AllBA,
#        filename = paste0(AllMetricOutPath,"AllMetric_OBS_MO_SS_BA.png"),width=17,height=8)
# 
# 
# AllBA_MO=ggplot(subset(ssss.err,Comp=="MO"),aes(x=mnXY,y=delta,color=Metric,fill=Metric))+
#   geom_point()+
#   stat_smooth(method="lm",lty=2,fill="gray",alpha=.5)+
#   guides(color=FALSE,fill=FALSE)+
#   geom_abline(intercept = 0,slope=2)+
#   geom_abline(intercept = 0,slope=-2)+
#   geom_hline(yintercept = 0)+
#   geom_hline(data=subset(ssss.err.mnsd,Comp=="MO"),aes(yintercept = mean_delta),color="red",lty=5)+
#   geom_text(data=subset(ssss.err.mnsd,Comp=="MO"),aes(label=sigtext),
#             x=-Inf,y = -Inf,hjust=.25,vjust=1,
#             size=2,color="red")+
#   geom_text_npc(data=subset(ssss.err.mnsd,Comp=="MO"),aes(label=sigtext),
#                 npcx=0.5,npcy = .99,hjust=.25,vjust=1,
#                 size=2,color="red")+
#   geom_hline(data=subset(ssss.err.mnsd,Comp=="MO"),aes(yintercept = mean_delta-1.96*sd_delta),color="pink",lty=5)+
#   geom_hline(data=subset(ssss.err.mnsd,Comp=="MO"),aes(yintercept = mean_delta+1.96*sd_delta),color="pink",lty=5)+
#   facet_wrap(.~Metric,scales="free",nrow=2)
# 
# AllMetricOutPath="T:/Benthic/Data/SfM/ComparisonPlots_20200204/AllMetricsComp/"
# if(!dir.exists(AllMetricOutPath)){dir.create(AllMetricOutPath)}
# ggsave(plot = AllBA_MO,
#        filename = paste0(AllMetricOutPath,"AllMetric_MOonly_SS_BA.png"),width=17,height=8)
# 


dfError<-ddply(all.rmse,.(Metric,GENUS_CODE,Comp),
               summarize,
               N=length(X),
               Nnz=length(which((X+Y)>0)),
               Nz=length(which((X+Y)==0)),
               Mean..=mean(rbind(Y,X),na.rm=T),
               MeanBias=mean(Y-X,na.rm=T),
               MedianBias=median(Y-X,na.rm=T),
               MeanBias_sc=meanbias_sc(Y,X),
               MedianBias_sc=medianbias_sc(Y,X),
               MAE.mn=mean(abs(Y-X),na.rm=T),
               MAE.md=median(abs(Y-X),na.rm=T),
               MAE_sc.mn=mae_sc(Y,X),
               MAE_sc.se=seae_sc(Y,X),
               MAE_sc.md=mdae_sc(Y,X),
               RMSE.mn=rmse.mn(Y,X),
               RMSE.md=rmse.md(Y,X),
               Log2AR_mn=mean(log2AR(X,Y),na.rm = T),
               Log2AR_se=sd(log2AR(X,Y),na.rm=T)/sqrt(length(X)))

rmse.ssss<-subset(dfError,GENUS_CODE=="SSSS"&Comp%in%c("D1vD2","S1vS2","MO"))

#Rename error comparison names-can't get mutate:recode to work
comp.names<-data.frame(Comp=c("D1vD2","MO","S1vS2"),Comp2=c("In water Observer","Method Error","SfM Observer"))
rmse.ssss<-left_join(rmse.ssss,comp.names)


comporder<-c("In water Observer","SfM Observer","Method Error")

rmse.ssss <- rmse.ssss[ order(match(rmse.ssss$Comp2, comporder)),]

rmse.ssss$Comp2<-as.character(rmse.ssss$Comp2)
rmse.ssss$Comp2<-factor(rmse.ssss$Comp2, levels = comporder)

rmse.ssss$Metric<-as.character(rmse.ssss$Metric)
rmse.ssss$Metric<-factor(rmse.ssss$Metric,
                         levels=c("AdColDen","JuvColDen",
                                  "BLE_prev","Ave.cl","Ave.od","Shannon","Richness"))

#Scaled MAE plot for manuscript
#Changing metric names to make more sense 
metricnames<-data.frame(Metric=c("AdColDen","Ave.cl","Ave.od","BLE_prev","JuvColDen","Richness","Shannon"),
                        Metric.new=c("`Adult Density`","`Average Colony Diameter`","`Average Old Dead`",
                                     "`Bleaching Prevalence`","`Juvenile Density`","`Adult Species Richness`",
                                     "`Adult Shannon Diversity`"))


rmse.ssss<-left_join(rmse.ssss,metricnames)

#reorder metrics in plot
metricoder<- c("`Adult Density`","`Juvenile Density`","`Average Colony Diameter`","`Average Old Dead`",
"`Bleaching Prevalence`","`Adult Species Richness`","`Adult Shannon Diversity`")


rmse.ssss<- rmse.ssss[order(match(rmse.ssss$Metric.new, metricoder)),];head(rmse.ssss)
rmse.ssss$Metric.new<-as.character(rmse.ssss$Metric.new);head(rmse.ssss)
rmse.ssss$Metric.new<-factor(rmse.ssss$Metric.new, levels = metricoder);head(rmse.ssss)


#Identify significant difference between error types
rmse.sub<-subset(all.rmse,GENUS_CODE=="SSSS"&Comp%in%c("D1vD2","S1vS2","MO"))
rmse.sub<-subset(rmse.sub,Metric %in% c("AdColDen","JuvColDen","Ave.cl","Ave.od","BLE_prev","Richness","Shannon"))

comp.names<-data.frame(Comp=c("D1vD2","MO","S1vS2"),Comp2=c("In water Observer","Method Error","SfM Observer"))
rmse.sub<-left_join(rmse.sub,comp.names)

#We tested for normality and equal variance for all metrics- nothing could be transformed
#Ran post hoc tests with multiple test correction
kruskal.test(ae_sc ~ Comp, data=subset(rmse.sub,Metric=="AdColDen"))
dunnTest(ae_sc ~ Comp, data=subset(rmse.sub,Metric=="AdColDen"),method="bh")

kruskal.test(ae_sc ~ Comp, data=subset(rmse.sub,Metric=="JuvColDen"))
dunnTest(ae_sc ~ Comp, data=subset(rmse.sub,Metric=="JuvColDen"),method="bh")

kruskal.test(ae_sc ~ Comp, data=subset(rmse.sub,Metric=="BLE_prev"))
dunnTest(ae_sc ~ Comp, data=subset(rmse.sub,Metric=="BLE_prev"),method="bh")

kruskal.test(ae_sc ~ Comp, data=subset(rmse.sub,Metric=="Ave.cl"))
dunnTest(ae_sc ~ Comp, data=subset(rmse.sub,Metric=="Ave.cl"),method="bh")

kruskal.test(ae_sc ~ Comp, data=subset(rmse.sub,Metric=="Ave.od"))
dunnTest(ae_sc ~ Comp, data=subset(rmse.sub,Metric=="Ave.od"),method="bh")

kruskal.test(ae_sc ~ Comp, data=subset(rmse.sub,Metric=="Richness"))
dunnTest(ae_sc ~ Comp, data=subset(rmse.sub,Metric=="Richness"),method="bh")

kruskal.test(ae_sc ~ Comp, data=subset(rmse.sub,Metric=="Shannon"))
dunnTest(ae_sc ~ Comp, data=subset(rmse.sub,Metric=="Shannon"),method="bh")

#Set up text labels for post hoc tests on plots
#rmse.ssss<- rmse.ssss[order(rmse.ssss$Metric.new),];rmse.ssss
rmse.ssss$sig<-c("a","b","ab",
                 "","","",
                 "a","b","a",
                 "","","",
                 "","","",
                 "","","",
                 "","","")

ScaledMAE_manuscript<-ggplot(rmse.ssss, aes(x=Comp2, y=MAE_sc.mn, fill=Metric.new)) + 
  geom_hline(yintercept = c(0,1))+
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(aes(ymin=MAE_sc.mn-MAE_sc.se, ymax=MAE_sc.mn+MAE_sc.se),width=.15, position=position_dodge(.9)) + 
  guides(fill=FALSE) + facet_wrap(.~ `Metric.new` , scales="fixed", labeller=label_parsed) +
  ylab("Midpoint Scaled Mean Absolute Error")+
  ylim(c(0,.5))+
  xlab("Error Comparison")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none")+
  theme(axis.title.y = element_text(face="bold")
        ,axis.title.x = element_text(face="bold"))+
  geom_text(data=rmse.ssss,aes(x=Comp2,y=MAE_sc.mn+MAE_sc.se,label=sig, group = Metric.new),
            position = position_dodge(),
            vjust = -0.5)
ScaledMAE_manuscript


ggsave(plot=ScaledMAE_manuscript,file="T:/Benthic/Data/SfM/Method Comparision/Figures/MAEScaled_Manuscript_Comparision.jpeg",width=10,height=8)


SSSSmet=subset(data.sm,GENUS_CODE=="SSSS")[,c("SS","MethodRep","AdColDen","JuvColDen",
                                              "BLE_prev","Ave.cl","Ave.od","Richness","Shannon")]



Smm=melt(SSSSmet,id.vars = c("SS","MethodRep"))

#Changing metric names to make more sense 
metricnames<-data.frame(variable=c("AdColDen","Ave.cl","Ave.od","BLE_prev","JuvColDen","Richness","Shannon"),
                        variable2=c("`Adult Density`","`Average Colony Diameter`","`Average Old Dead`",
                                     "`Bleaching Prevalence`","`Juvenile Density`","`Adult Species Richness`",
                                     "`Adult Shannon Diversity`"))
Smm<-left_join(Smm,metricnames)

methodnames<-data.frame(MethodRep=c("DIVER_1","DIVER_2","SfM_1","SfM_2"),
                                    MethodRep2=c("Diver 1","Diver 2","SfM 1","SfM 2"))
Smm<-left_join(Smm,methodnames)


#reorder metrics in plot
Smm<- Smm[order(match(Smm$variable2, metricoder)),];head(Smm)
Smm$variable2<-as.character(Smm$variable2);head(Smm)
Smm$variable2<-factor(Smm$variable2, levels = metricoder);head(Smm)

                        


Boxes=ggplot(Smm,aes(x=MethodRep2,y=value,fill=variable2))+
  geom_violin(draw_quantiles = c(.5),size=.5)+
  geom_jitter(width=.25,height=0,color="gray",alpha=0.5)+
  facet_wrap(.~`variable2`,scale='free_y',nrow=2,labeller=label_parsed)+
  guides(fill=FALSE)+
  ylab("Value")+
  xlab("Method and Observer")+theme_bw()+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none")+
  theme(axis.title.y = element_text(face="bold")
        ,axis.title.x = element_text(face="bold"))

    
    
Boxes

ggsave(plot=Boxes,
       file="T:/Benthic/Data/SfM/Method Comparision/Figures/Violin_Comparision_Manuscript.jpeg",
       width=10,height=6)

