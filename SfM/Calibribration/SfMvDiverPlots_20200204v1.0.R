rm(list=ls())

library(gridExtra)
library(reshape2)
library(plyr)
#install.packages("ggpmisc")
library(hydroGOF)
library(tidyverse)
library(ggpmisc)
source("C:/Users/Thomas.Oliver/WORK/BenthicComparison/SfMvDiver Plotting Functions.R") 
data.gen<-read.csv("T:/Benthic/Data/SfM/Summarized Data/HARAMP_repeats_GENUS_Summarized Data.csv")


#List of segments that were surveyed by all methods and multiple divers....UNEQUAL
sfm2<-data.gen[data.gen$MethodRep=="SfM_2",] 
length(unique(sfm2$SS)) #38 unique SS
sfm1<-data.gen[data.gen$MethodRep=="SfM_1",]
length(unique(sfm1$SS)) #31
diver1<-data.gen[data.gen$MethodRep=="DIVER_1",]
length(unique(diver1$SS)) #24
diver2<-data.gen[data.gen$MethodRep=="DIVER_2",]
length(unique(diver2$SS)) #23


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


#4 datasets together
df.all <- join_all(list(d1,d2,sfm1,sfm2), by= c("SITE","SITEVISITID","SEGMENT","GENUS_CODE","SS","OBS_YEAR","REGION","ISLAND","SEC_NAME","REEF_ZONE",
                           "DEPTH_BIN","HABITAT_CODE", "LATITUDE","LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M"), type='full'); 
head(df.all) 
nrow(df.all)


#Plot figures
#PlotAll(dataframe, variable 1, variable 2, y-axis name 1, y-axis name 2, x-axis name 1, x-axis name 2)

outpath<- "T:/Benthic/Data/SfM/ComparisonPlots_20200204/Adult Density"
if(!dir.exists(outpath)){dir.create(outpath)}
p1<-PlotAll(df.all,"d1AdColDen","SfM1AdColDen","SfM Adult Density","Difference SfM Analyst and Diver", "Diver Adult Density","Mean Adult Density"); p1
p2<-PlotAll(df.all,"d1AdColDen","d2AdColDen","Diver1 Adult Density","Difference Diver1 and Diver2","Diver2 Adult Density","Mean Adult Density"); p2
p3<-PlotAll(df.all,"SfM1AdColDen","SfM2AdColDen","SfM1 Adult Density","Difference SfM Analyst1 and SfM Analyst2","SfM2 Adult Density","Adult Density"); p3

outpath<-"T:/Benthic/Data/SfM/ComparisonPlots_20200204/Juvenile Density"
if(!dir.exists(outpath)){dir.create(outpath)}
p4<-PlotAll(df.all,"d1JuvColDen","SfM1JuvColDen","SfM Juvenile Density","Difference SfM Analyst and Diver", "Diver Juvenile Density","Mean Juvenile Density"); p4
p5<-PlotAll(df.all,"d1JuvColDen","d2JuvColDen","Diver1 Juvenile Density","Difference Diver1 and Diver2","Diver2 Juvenile Density","Mean Juvenile Density"); p5
p6<-PlotAll(df.all,"SfM1JuvColDen","SfM2JuvColDen","SfM1 Juvenile Density","Difference SfM Analyst1 and SfM Analyst2","SfM2 Juvenile Density","Mean Juvenile Density"); p6

outpath<-"T:/Benthic/Data/SfM/ComparisonPlots_20200204/Colony Size"
if(!dir.exists(outpath)){dir.create(outpath)}
p7<-PlotAll(df.all,"d1Ave.cl","SfM1Ave.cl","SfM Colony Length","Difference SfM Analyst and Diver", "Diver Colony Length","Mean Colony Length")
p8<-PlotAll(df.all,"d1Ave.cl","d2Ave.cl","Diver1 Colony Length","Difference Diver1 and Diver2","Diver2 Colony Length","Mean Colony Length")
p9<-PlotAll(df.all,"SfM1Ave.cl","SfM2Ave.cl","SfM1 Colony Length","Difference SfM Analyst1 and SfM Analyst2","SfM2 Colony Length","Mean Colony Length")

outpath<-"T:/Benthic/Data/SfM/ComparisonPlots_20200204/Old Dead"
if(!dir.exists(outpath)){dir.create(outpath)}
p10<-PlotAll(df.all,"d1Ave.od","SfM1Ave.od","SfM Average Old Dead Pct","Difference SfM Analyst and Diver", "Diver Average Old Dead Pct","Average Old Dead Pct")
p11<-PlotAll(df.all,"d1Ave.od","d2Ave.od","Diver1 Average Old Dead Pct","Difference Diver1 & Diver2", "Diver2 Average Old Dead Pct","Average Old Dead Pct")
p12<-PlotAll(df.all,"SfM1Ave.od","SfM2Ave.od","SfM1 Average Old Dead Pct","Difference SfM Analyst1 and SfM Analyst2", "SfM2 Average Old Dead Pct","Average Old Dead Pct")

outpath<-"T:/Benthic/Data/SfM/ComparisonPlots_20200204/Recent Dead"
if(!dir.exists(outpath)){dir.create(outpath)}
p13<-PlotAll(df.all,"d1Ave.rd","SfM1Ave.rd","SfM Average Recent Dead Pct","Difference SfM Analyst and Diver", "Diver Average Recent Dead Pct","Average Recent Dead Pct")
p14<-PlotAll(df.all,"d1Ave.rd","d2Ave.rd","Diver1 Average Recent Dead Pct","Difference Diver1 and Diver2","Diver2 Average Recent Dead Pct","Average Recent Dead Pct")
p15<-PlotAll(df.all,"SfM1Ave.rd","SfM2Ave.rd","SfM1 Average Recent Dead Pct","Difference SfM Analyst1 and SfM Analyst2","SfM2 Average Recent Dead Pct","Mean Average Recent Dead Pct")

outpath<-"T:/Benthic/Data/SfM/ComparisonPlots_20200204/Bleaching"
if(!dir.exists(outpath)){dir.create(outpath)}
p16<-PlotAll(df.all,"d1BLE_prev","SfM1BLE_prev","SfM Bleaching Prevalence","Difference SfM Analyst and Diver", "Diver Bleaching Prevalence","Mean Bleaching Prevalence")
p17<-PlotAll(df.all,"d1BLE_prev","d2BLE_prev","Diver1 Bleaching Prevalence","Difference Diver1 and Diver2","Diver2 Bleaching Prevalence","Mean Bleaching Prevalence")
p18<-PlotAll(df.all,"SfM1BLE_prev","SfM2BLE_prev","SfM1 Bleaching Prevalence","Difference SfM Analyst1 and SfM Analyst2","SfM2 Bleaching Prevalence","Mean Bleaching Prevalence")

outpath<-"T:/Benthic/Data/SfM/ComparisonPlots_20200204/ChronicDZ"
if(!dir.exists(outpath)){dir.create(outpath)}
p19<-PlotAll(df.all,"d1CHRO_prev","SfM1CHRO_prev","SfM Chronic Disease Prevalence","Difference SfM Analyst and Diver", "Diver Chronic Disease Prevalence","Mean Chronic Disease Prevalence")
p20<-PlotAll(df.all,"d1CHRO_prev","d2CHRO_prev","Diver1 Chronic Disease Prevalence","Difference Diver1 and Diver2","Diver2 Chronic Disease Prevalence","Mean Chronic Disease Prevalence")
p21<-PlotAll(df.all,"SfM1CHRO_prev","SfM2CHRO_prev","SfM1 Chronic Disease Prevalence","Difference SfM Analyst1 and SfM Analyst2","SfM2 Chronic Disease Prevalence","Mean Chronic Disease Prevalence")

outpath<-"T:/Benthic/Data/SfM/ComparisonPlots_20200204/AcuteDZ"
if(!dir.exists(outpath)){dir.create(outpath)}
p22<-PlotAll(df.all,"d1DZGN_G_prev","SfM1DZGN_G_prev","SfM General Disease Prevalence","Difference SfM Analyst and Diver", "Diver General Disease Prevalence","Mean General Disease Prevalence")
p23<-PlotAll(df.all,"d1DZGN_G_prev","d2DZGN_G_prev","Diver1 General Disease Prevalence","Difference Diver1 and Diver2","Diver2 General Disease Prevalence","Mean General Disease Prevalence")
p24<-PlotAll(df.all,"SfM1DZGN_G_prev","SfM2DZGN_G_prev","SfM1 General Disease Prevalence","Difference SfM Analyst1 and SfM Analyst2","SfM2 General Disease Prevalence","Mean General Disease Prevalence")


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
rd.comp<-ErrorComparision(data.sm,"GENUS_CODE","Ave.rd")
od.comp<-ErrorComparision(data.sm,"GENUS_CODE","Ave.od")
dz.comp<-ErrorComparision(data.sm,"GENUS_CODE","DZGN_G_prev")
ble.comp<-ErrorComparision(data.sm,"GENUS_CODE","BLE_prev")
chr.comp<-ErrorComparision(data.sm,"GENUS_CODE","CHRO_prev")

all.rmse<-rbind(ad.comp,jd.comp,cl.comp,od.comp,rd.comp,dz.comp,ble.comp,chr.comp)
#all.rmse<-rbind(ad.comp,jd.comp,dz.comp,cl.comp,rd.comp,od.comp)

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
                       levels=c("AdColDen","JuvColDen","DZGN_G_prev","CHRO_prev",
                                "BLE_prev","Ave.cl","Ave.rd","Ave.od"))
ssss.err$Comp=factor(ssss.err$Comp,levels=c("D1vD2","S1vS2","MO"))
ssss.err.mnsd=ddply(ssss.err,.(Metric,Comp),summarize,
                    mean_mnXY=mean(mnXY,na.rm=T),
                    q75_mnXY=quantile(mnXY,probs = .75,na.rm=T),
                    q95_mnXY=quantile(mnXY,probs = .95,na.rm=T),
                    mean_delta=mean(delta,na.rm=T),
                    sd_delta=sd(delta,na.rm=T),
                    p_1t=t.test(delta,mu=0,alternative="two.sided")$p.value,
                    sigtext=if(p_1t<0.05){paste0("* p < ",signif(p_1t,2)," *")}else{"NS"})

AllBA=ggplot(ssss.err,aes(x=mnXY,y=delta,color=Metric,fill=Metric))+
  geom_point()+
  stat_smooth(method="lm",lty=2,fill="gray",alpha=.5)+
  guides(color=FALSE,fill=FALSE)+
  geom_abline(intercept = 0,slope=2)+
  geom_abline(intercept = 0,slope=-2)+
  geom_hline(yintercept = 0)+
  geom_hline(data=ssss.err.mnsd,aes(yintercept = mean_delta),color="red",lty=5)+
  geom_text(data=ssss.err.mnsd,aes(label=sigtext),
            x=-Inf,y = -Inf,hjust=.25,vjust=1,
            size=2,color="red")+
  geom_text_npc(data=ssss.err.mnsd,aes(label=sigtext),
            npcx=0.5,npcy = .99,hjust=.25,vjust=1,
            size=2,color="red")+
  geom_hline(data=ssss.err.mnsd,aes(yintercept = mean_delta-1.96*sd_delta),color="pink",lty=5)+
  geom_hline(data=ssss.err.mnsd,aes(yintercept = mean_delta+1.96*sd_delta),color="pink",lty=5)+
  facet_wrap(Comp~Metric,scales="free",nrow=3)

AllMetricOutPath="T:/Benthic/Data/SfM/ComparisonPlots_20200204/AllMetricsComp/"
if(!dir.exists(AllMetricOutPath)){dir.create(AllMetricOutPath)}
ggsave(plot = AllBA,
       filename = paste0(AllMetricOutPath,"AllMetric_OBS_MO_SS_BA.png"),width=17,height=8)


AllBA_MO=ggplot(subset(ssss.err,Comp=="MO"),aes(x=mnXY,y=delta,color=Metric,fill=Metric))+
  geom_point()+
  stat_smooth(method="lm",lty=2,fill="gray",alpha=.5)+
  guides(color=FALSE,fill=FALSE)+
  geom_abline(intercept = 0,slope=2)+
  geom_abline(intercept = 0,slope=-2)+
  geom_hline(yintercept = 0)+
  geom_hline(data=subset(ssss.err.mnsd,Comp=="MO"),aes(yintercept = mean_delta),color="red",lty=5)+
  geom_text(data=subset(ssss.err.mnsd,Comp=="MO"),aes(label=sigtext),
            x=-Inf,y = -Inf,hjust=.25,vjust=1,
            size=2,color="red")+
  geom_text_npc(data=subset(ssss.err.mnsd,Comp=="MO"),aes(label=sigtext),
                npcx=0.5,npcy = .99,hjust=.25,vjust=1,
                size=2,color="red")+
  geom_hline(data=subset(ssss.err.mnsd,Comp=="MO"),aes(yintercept = mean_delta-1.96*sd_delta),color="pink",lty=5)+
  geom_hline(data=subset(ssss.err.mnsd,Comp=="MO"),aes(yintercept = mean_delta+1.96*sd_delta),color="pink",lty=5)+
  facet_wrap(.~Metric,scales="free",nrow=2)

AllMetricOutPath="T:/Benthic/Data/SfM/ComparisonPlots_20200204/AllMetricsComp/"
if(!dir.exists(AllMetricOutPath)){dir.create(AllMetricOutPath)}
ggsave(plot = AllBA_MO,
       filename = paste0(AllMetricOutPath,"AllMetric_MOonly_SS_BA.png"),width=17,height=8)



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
            MAE_sc.md=mdae_sc(Y,X),
            RMSE.mn=rmse.mn(Y,X),
            RMSE.md=rmse.md(Y,X),
            Log2AR_mn=mean(log2AR(X,Y),na.rm = T),
            Log2AR_se=sd(log2AR(X,Y),na.rm=T)/sqrt(length(X)))

rmse.ssss<-subset(dfError,GENUS_CODE=="SSSS"&Comp%in%c("D1vD2","S1vS2","MO"))

rmse.ssss<-rmse.ssss %>% mutate(Comp=recode(Comp, 
                                           `D1vD2`="In water Observer",
#                                           `Mbar`="NA",
                                           `MO`="Method Error",
 #                                          `Obar`="NA",
                                           `S1vS2`="SfM Observer"))

comporder<-c("In water Observer","SfM Observer","Method Error")

rmse.ssss <- rmse.ssss[ order(match(rmse.ssss$Comp, comporder)),]

rmse.ssss$Comp<-as.character(rmse.ssss$Comp)
rmse.ssss$Comp<-factor(rmse.ssss$Comp, levels = comporder)

rmse.ssss$Metric<-as.character(rmse.ssss$Metric)
rmse.ssss$Metric<-factor(rmse.ssss$Metric,
                         levels=c("AdColDen","JuvColDen","DZGN_G_prev","CHRO_prev",
                                  "BLE_prev","Ave.cl","Ave.rd","Ave.od"))


#B_A

BiasUnscaled<-ggplot(subset(rmse.ssss,Comp=="Method Error"),
                     aes(x=Metric, y=MeanBias,ymin=MeanBias-MAE.mn,ymax=MeanBias+MAE.mn, color=Metric)) + 
  geom_hline(yintercept = 0)+
  geom_point(size=5) +
  geom_linerange(size=2)+
  geom_text(aes(x=2,y=6.5,label="SFM Bigger"),color="gray")+
  geom_text(aes(x=2,y=-6.5,label="Diver Bigger"),color="gray")+
  guides(fill=FALSE) +
  #facet_wrap(~Metric, scales="fixed", labeller=label_parsed) +
  scale_y_continuous(name="Unscaled Mean Bias (+/- MAE)",limits=c(-6.5,6.5))+
  xlab("Metric")+
  ggtitle("Unscaled Bias and Error Between Diver and SFM\n43 segments @ 28 Sites")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none"
  )

BiasUnscaled


BiasScaled<-ggplot(subset(rmse.ssss,Comp=="Method Error"),
                     aes(x=Metric, y=MeanBias_sc,ymin=MeanBias_sc-MAE_sc.mn,ymax=MeanBias_sc+MAE_sc.mn, color=Metric)) + 
  geom_hline(yintercept = 0)+
  geom_point(size=5) +
  geom_linerange(size=2)+
  geom_text(aes(x=2,y=.5,label="SFM Bigger"),color="gray")+
  geom_text(aes(x=2,y=-.5,label="Diver Bigger"),color="gray")+
  guides(fill=FALSE) +
  #facet_wrap(~Metric, scales="fixed", labeller=label_parsed) +
  scale_y_continuous(name="Midpoint Scaled Mean Bias (+/- Scaled MAE)",limits=c(-.5,.5))+
  xlab("Metric")+
  ggtitle("Scaled Bias and Error Between Diver and SFM\n43 segments @ 28 Sites")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none"
  )

BiasScaled


Bias2=grid.arrange(BiasUnscaled,BiasScaled,nrow=1,ncol=2)
Bias2

ggsave(plot=BiasUnscaled,file="T:/Benthic/Data/SfM/ComparisonPlots_20200204/AllMetricsComp/BiasUnscaled_Count00_Comparision.pdf",width=8,height=8)
ggsave(plot=BiasScaled,file="T:/Benthic/Data/SfM/ComparisonPlots_20200204/AllMetricsComp/BiasScaled_Count00_Comparision.pdf",width=8,height=8)
ggsave(plot=Bias2,file="T:/Benthic/Data/SfM/ComparisonPlots_20200204/AllMetricsComp/BiasUn_and_Scaled_Count00_Comparision.pdf",width=16,height=8)




UnscaledMAE<-ggplot(rmse.ssss, aes(x=Comp, y=MAE.mn, fill=Metric)) + 
  geom_hline(yintercept = 0)+
  geom_bar(position="dodge",stat="identity") +
  guides(fill=FALSE) + facet_wrap(~Metric, scales="fixed", labeller=label_parsed) +
  ylab("Mean Absolute Error")+
  xlab("Error Comparison")+
  ggtitle("Unscaled Mean Absolute Error Across Observers and Methods\n43 segments @ 28 Sites")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none"
  )

UnscaledMAE



ScaledMAE<-ggplot(rmse.ssss, aes(x=Comp, y=MAE_sc.mn, fill=Metric)) + 
  geom_hline(yintercept = c(0,1))+
  geom_bar(position="dodge",stat="identity") +
  guides(fill=FALSE) + facet_wrap(~Metric, scales="fixed", labeller=label_parsed) +
  ylab("Midpoint Scaled Mean Absolute Error")+
  ylim(c(0,.5))+
  xlab("Error Comparison")+
  ggtitle("Scaled Mean Absolute Error Across Observers and Methods\n43 segments @ 28 Sites")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none"
  )

ScaledMAE
MAE2=grid.arrange(UnscaledMAE,ScaledMAE,nrow=1,ncol=2)
MAE2

ggsave(plot=UnscaledMAE,file="T:/Benthic/Data/SfM/ComparisonPlots_20200204/AllMetricsComp/MAEUnscaled_Count00_Comparision.pdf",width=8,height=8)
ggsave(plot=ScaledMAE,file="T:/Benthic/Data/SfM/ComparisonPlots_20200204/AllMetricsComp/MAEScaled_Count00_Comparision.pdf",width=8,height=8)
ggsave(plot=MAE2,file="T:/Benthic/Data/SfM/ComparisonPlots_20200204/AllMetricsComp/MAEUn_and_Scaled_Count00_Comparision.pdf",width=16,height=8)


SSSSmet=subset(data.sm,GENUS_CODE=="SSSS")[,c("SS","MethodRep","AdColDen","JuvColDen","DZGN_G_prev","CHRO_prev",
                                              "BLE_prev","Ave.cl","Ave.rd","Ave.od")]
Smm=melt(SSSSmet,id.vars = c("SS","MethodRep"))
Boxes=ggplot(Smm,aes(x=MethodRep,y=value,fill=variable))+
  geom_violin(draw_quantiles = c(.05,.5,.95),size=.5)+
  geom_jitter(width=.25,height=0,color="gray",alpha=0.5)+
  facet_wrap(.~variable,scale='free_y',nrow=2)+
  guides(fill=FALSE)+
  ggtitle("Distibutions of Metrics by Method/Observer")+
  ylab("Value")+theme_bw()
Boxes
ggsave(plot=Boxes,
       file="T:/Benthic/Data/SfM/ComparisonPlots_20200204/AllMetricsComp/Violin_Comparision.pdf",
       width=12,height=8)


p3<-ggplot(rmse.ssss, aes(x=Comp, y=Log2AR_mn,ymin=Log2AR_mn-Log2AR_se,ymax=Log2AR_mn+Log2AR_se, color=Metric)) + 
  geom_hline(yintercept = 0)+
  geom_point() +
  geom_errorbar()+
  guides(fill=FALSE) + facet_wrap(~Metric, scales="fixed", labeller=label_parsed) +
  scale_y_continuous(name = "Log2 Accuracy Ratio (Mean +/- SE)")+
  xlab("Error Comparison")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none"
  )

p3
ggsave(plot=p1,file="T:/Benthic/Data/SfM/ComparisonPlots_20200204/AllLogAR_Comparision.pdf",width=12,height=12)


data.sm.S=subset(data.sm,GENUS_CODE="SSSS")
data.sm.S$OBS_rep=matrix(unlist(strsplit(as.vector(data.sm.S$MethodRep),"_")),ncol=2,byrow=T)[,2]
library(lme4)
library(lmerTest)


head(all.rmse)
all.rmse$delYX=all.rmse$Y-all.rmse$X
ars=subset(all.rmse,GENUS_CODE=="SSSS")

mod=lmer(delYX~Comp*OBS_rep+(1|SS),data=data.sm.S)
summary(mod)

