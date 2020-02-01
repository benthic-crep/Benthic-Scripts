rm(list=ls())

#source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/SfMvDiver Plotting Functions.R") 
source("C:/Users/Corinne.Amir/Documents/GitHub/Benthic-Scripts/Functions/SfMvDiver Plotting Functions.R") 
#Plot1to1; PlotBioAlt; PlotPair

#data.gen<-read.csv("T:/Benthic/Data/SfM/Summarized Data/HARAMP_repeats_GENUS_Summarized Data.csv")
data.gen.a<-read.csv("T:/Benthic/Data/SfM/Summarized Data/HARAMP_repeats_GENUS_Summarized Data.csv")
data.gen<-read.csv("T:/Benthic/Data/SfM/Summarized Data/HARAMP_repeats_GENUS_Summarized Data-CALIBRATION.csv")
seglist<-read.csv("T:/Benthic/Data/SfM/Summarized Data/Comparison_seglist.csv")

dim(data.gen)
dim(data.gen.a)

#List of segments that were surveyed by all methods and multiple divers....UNEQUAL
sfm2<-data.gen[data.gen$MethodRep=="SfM_2",] 
length(unique(sfm2$SS)) #46 unique SS
sfm1<-data.gen[data.gen$MethodRep=="SfM_1",]
length(unique(sfm1$SS)) #44
diver1<-data.gen[data.gen$MethodRep=="DIVER_1",]
length(unique(diver1$SS)) #692
diver2<-data.gen[data.gen$MethodRep=="DIVER_2",]
length(unique(diver2$SS)) #44


sfm2<-data.gen.a[data.gen.a$MethodRep=="SfM_2",] 
length(unique(sfm2$SS)) #46 unique SS
sfm1.a<-data.gen.a[data.gen.a$MethodRep=="SfM_1",]
length(unique(sfm1$SS)) #44
diver1<-data.gen.a[data.gen.a$MethodRep=="DIVER_1",]
length(unique(diver1$SS)) #692
diver2<-data.gen.a[data.gen.a$MethodRep=="DIVER_2",]
length(unique(diver2$SS)) #44


siteseg.a<-
  sfm1$SS=paste0(ad_sfm$SITE,"_",ad_sfm$SEGMENT)
seglista=as.vector(ddply(sfm1.a,.(SS),summarize,length(unique(sfm1.a$SS))))
dim(seglista)
dim(seglist)

seglist$merge<-as.vector(0)
seglista$merge<-as.vector(0)

a<-anti_join(seglist,seglista,by="SS")
#Create dataframe containing only site_segments that contain all 4 methodreps
#data.gen.rm<-subset(data.gen, TRANSECT %in% c(3, 4, 5, 6, "NA"));data.gen[data.gen$TRANSECT] #for calibration
data.gen$SS<-paste0(data.gen$SITE,"_",data.gen$SEGMENT)
data.sm=subset(data.gen,SS%in%seglist$SS)
dim(data.sm)
length(unique(data.sm$SS))
table(data.sm$SS, data.sm$MethodRep) #all columns should be NONZERO = FALSE

sfm2<-data.sm[data.sm$MethodRep=="SfM_2",] 
length(unique(sfm2$SS)) #46 unique SS
sfm1<-data.sm[data.sm$MethodRep=="SfM_1",]
length(unique(sfm1$SS)) #44
diver1<-data.sm[data.sm$MethodRep=="DIVER_1",]
length(unique(diver1$SS)) #692
diver2<-data.sm[data.sm$MethodRep=="DIVER_2",]
length(unique(diver2$SS))


#Check that all Site_Segments being used for analysis have 2 annotators and 2 divers
ddply(data.sm,.(SITE, SEGMENT), summarize, num.repeats = n_distinct(MethodRep)) #should be 4
ddply(data.sm,.(SITE, SEGMENT), summarize, num.repeats = n_distinct(METHOD)) #should = 2


# Plotting Regressions and Bland-Altman by Taxon --------------------------

### Separate 4 datasets and add the dataset name before the metric columns. Need to set up dataframe this way to make plotting easier.
# d1<-data.gen[data.gen$MethodRep=="DIVER_1",];colnames(d1)[8:20] <- paste("d1", colnames(d1[8:20]), sep = "");d1<-subset(d1,select=-c(METHOD,SEGAREA_ad,MethodRep,TRANSECT,METHOD.1))
# d2<-data.gen[data.gen$MethodRep=="DIVER_2",];colnames(d2)[8:20] <- paste("d2", colnames(d2[8:20]), sep = "");d2<-subset(d2,select=-c(METHOD,SEGAREA_ad,MethodRep,TRANSECT,METHOD.1))
# sfm1<-data.gen[data.gen$MethodRep=="SfM_1",];colnames(sfm1)[8:20] <- paste("SfM1", colnames(sfm1[8:20]), sep = "");sfm1<-subset(sfm1,select=-c(METHOD,SEGAREA_ad,MethodRep,TRANSECT,METHOD.1))
# sfm2<-data.gen[data.gen$MethodRep=="SfM_2",];colnames(sfm2)[8:20] <- paste("SfM2", colnames(sfm2[8:20]), sep = "");sfm2<-subset(sfm2,select=-c(METHOD,SEGAREA_ad,MethodRep,TRANSECT,METHOD.1))
d1<-data.sm[data.sm$MethodRep=="DIVER_1",];colnames(d1)[8:20] <- paste("d1", colnames(d1[8:20]), sep = "");d1<-subset(d1,select=-c(METHOD,SEGAREA_ad,MethodRep,TRANSECT,METHOD.1))
d2<-data.sm[data.sm$MethodRep=="DIVER_2",];colnames(d2)[8:20] <- paste("d2", colnames(d2[8:20]), sep = "");d2<-subset(d2,select=-c(METHOD,SEGAREA_ad,MethodRep,TRANSECT,METHOD.1))
sfm1<-data.sm[data.sm$MethodRep=="SfM_1",];colnames(sfm1)[8:20] <- paste("SfM1", colnames(sfm1[8:20]), sep = "");sfm1<-subset(sfm1,select=-c(METHOD,SEGAREA_ad,MethodRep,TRANSECT,METHOD.1))
sfm2<-data.sm[data.sm$MethodRep=="SfM_2",];colnames(sfm2)[8:20] <- paste("SfM2", colnames(sfm2[8:20]), sep = "");sfm2<-subset(sfm2,select=-c(METHOD,SEGAREA_ad,MethodRep,TRANSECT,METHOD.1))


#4 datasets together

df.all <- join_all(list(d1,d2,sfm1,sfm2), by= c("SITE","SITEVISITID","SEGMENT","GENUS_CODE","SS","OBS_YEAR","REGION","ISLAND","SEC_NAME","REEF_ZONE",
                           "DEPTH_BIN","HABITAT_CODE", "LATITUDE","LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M"), type='full'); 
head(data.all) 
nrow(df.all)
ddply(df.all,.(SITE, SEGMENT), summarize, num.repeats = n_distinct(d1AdColCount)) 


#Plot figures
#PlotAll(dataframe, variable 1, variable 2, y-axis name 1, y-axis name 2, x-axis name 1, x-axis name 2)

outpath<- "T:/Benthic/Data/SfM/ComparisionPlots/Adult Density/Adult Density Plots"
p1<-PlotAll(df.all,"d1AdColDen","SfM1AdColDen","SfM Adult Density","Difference SfM Analyst and Diver", "Diver Adult Density","Mean Adult Density"); p1
p2<-PlotAll(df.all,"d1AdColDen","d2AdColDen","Diver1 Adult Density","Difference Diver1 and Diver2","Diver2 Adult Density","Mean Adult Density"); p2
p3<-PlotAll(df.all,"SfM1AdColDen","SfM2AdColDen","SfM1 Adult Density","Difference SfM Analyst1 and SfM Analyst2","SfM2 Adult Density","Mean Adult Density"); p3

outpath<-"T:/Benthic/Data/SfM/ComparisionPlots/Juvenile Density"
p4<-PlotAll(df.all,"d1JuvColDen","SfM1JuvColDen","SfM Juvenile Density","Difference SfM Analyst and Diver", "Diver Juvenile Density","Mean Juvenile Density"); p4
p5<-PlotAll(df.all,"d1JuvColDen","d2JuvColDen","Diver1 Juvenile Density","Difference Diver1 and Diver2","Diver2 Juvenile Density","Mean Juvenile Density"); p5
p6<-PlotAll(df.all,"SfM1JuvColDen","SfM2JuvColDen","SfM1 Juvenile Density","Difference SfM Analyst1 and SfM Analyst2","SfM2 Juvenile Density","Mean Juvenile Density"); p6

outpath<-"T:/Benthic/Data/SfM/ComparisionPlots/Colony Size"
p7<-PlotAll(df.all,"d1Ave.cl","SfM1Ave.cl","SfM Colony Length","Difference SfM Analyst and Diver", "Diver Colony Length","Mean Colony Length")
p8<-PlotAll(df.all,"d1Ave.cl","d2Ave.cl","Diver1 Colony Length","Difference Diver1 and Diver2","Diver2 Colony Length","Mean Colony Length")
p9<-PlotAll(df.all,"SfM1Ave.cl","SfM2Ave.cl","SfM1 Colony Length","Difference SfM Analyst1 and SfM Analyst2","SfM2 Colony Length","Mean Colony Length")

outpath<-"T:/Benthic/Data/SfM/ComparisionPlots/Old Dead"
p10<-PlotAll(df.all,"d1Ave.od","SfM1JuvColDen","SfM Juvenile Density","Difference SfM Analyst and Diver", "Diver Juvenile Density","Mean Juvenile Density")
p11<-PlotAll(df.all,"d1Ave.od","d2JuvColDen","Diver1 Juvenile Density","Difference Diver1 and Diver2","Diver2 Juvenile Density","Mean Juvenile Density")
p12<-PlotAll(df.all,"SfM1Ave.od","SfM2JuvColDen","SfM1 Juvenile Density","Difference SfM Analyst1 and SfM Analyst2","SfM2 Juvenile Density","Mean Juvenile Density")

outpath<-"T:/Benthic/Data/SfM/ComparisionPlots/Recent Dead"
p13<-PlotAll(df.all,"d1JuvColDen","SfM1JuvColDen","SfM Juvenile Density","Difference SfM Analyst and Diver", "Diver Juvenile Density","Mean Juvenile Density")
p14<-PlotAll(df.all,"d1JuvColDen","d2JuvColDen","Diver1 Juvenile Density","Difference Diver1 and Diver2","Diver2 Juvenile Density","Mean Juvenile Density")
p15<-PlotAll(df.all,"SfM1JuvColDen","SfM2JuvColDen","SfM1 Juvenile Density","Difference SfM Analyst1 and SfM Analyst2","SfM2 Juvenile Density","Mean Juvenile Density")

outpath<-"T:/Benthic/Data/SfM/ComparisionPlots/Bleaching"
p16<-PlotAll(df.all,"d1JuvColDen","SfM1JuvColDen","SfM Juvenile Density","Difference SfM Analyst and Diver", "Diver Juvenile Density","Mean Juvenile Density")
p17<-PlotAll(df.all,"d1JuvColDen","d2JuvColDen","Diver1 Juvenile Density","Difference Diver1 and Diver2","Diver2 Juvenile Density","Mean Juvenile Density")
p18<-PlotAll(df.all,"SfM1JuvColDen","SfM2JuvColDen","SfM1 Juvenile Density","Difference SfM Analyst1 and SfM Analyst2","SfM2 Juvenile Density","Mean Juvenile Density")

outpath<-"T:/Benthic/Data/SfM/ComparisionPlots/ChronicDZ"
p19<-PlotAll(df.all,"d1JuvColDen","SfM1JuvColDen","SfM Juvenile Density","Difference SfM Analyst and Diver", "Diver Juvenile Density","Mean Juvenile Density")
p20<-PlotAll(df.all,"d1JuvColDen","d2JuvColDen","Diver1 Juvenile Density","Difference Diver1 and Diver2","Diver2 Juvenile Density","Mean Juvenile Density")
p21<-PlotAll(df.all,"SfM1JuvColDen","SfM2JuvColDen","SfM1 Juvenile Density","Difference SfM Analyst1 and SfM Analyst2","SfM2 Juvenile Density","Mean Juvenile Density")

outpath<-"T:/Benthic/Data/SfM/ComparisionPlots/AcuteDZ"
p22<-PlotAll(df.all,"d1JuvColDen","SfM1JuvColDen","SfM Juvenile Density","Difference SfM Analyst and Diver", "Diver Juvenile Density","Mean Juvenile Density")
p23<-PlotAll(df.all,"d1JuvColDen","d2JuvColDen","Diver1 Juvenile Density","Difference Diver1 and Diver2","Diver2 Juvenile Density","Mean Juvenile Density")
p24<-PlotAll(df.all,"SfM1JuvColDen","SfM2JuvColDen","SfM1 Juvenile Density","Difference SfM Analyst1 and SfM Analyst2","SfM2 Juvenile Density","Mean Juvenile Density")

#PlotAll(dataframe, variable 1, variable 2, y-axis name 1, y-axis name 2, x-axis name 1, x-axis name 2)
outpath<-"T:/Benthic/Data/SfM/ComparisionPlots/Disease General"
p22<-PlotAll(df.all,"d1DZGN_G_prev","SfM1DZGN_G_prev","SfM Disease Prevalence","Difference SfM Analyst and Diver", "Diver Disease Prevalence","Mean Disease Prevalence")
p23<-PlotAll(df.all,"d1DZGN_G_prev","d2DZGN_G_prev","Diver1 Disease Prevalence","Difference Diver1 and Diver2","Diver2 Disease Prevalence","Mean Disease Prevalence")
p24<-PlotAll(df.all,"SfM1DZGN_G_prev","SfM2DZGN_G_prev","SfM1 Disease Prevalence","Difference SfM Analyst1 and SfM Analyst2","SfM2 Disease Prevalence","Mean Disease Prevalence")


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


# ad.comp<-ErrorComparision(data.gen,"GENUS_CODE","AdColDen")
# jd.comp<-ErrorComparision(data.gen,"GENUS_CODE","JuvColDen")
# cl.comp<-ErrorComparision(data.gen,"GENUS_CODE","Ave.cl")
# rd.comp<-ErrorComparision(data.gen,"GENUS_CODE","Ave.rd")
# od.comp<-ErrorComparision(data.gen,"GENUS_CODE","Ave.od")
# dz.comp<-ErrorComparision(data.gen,"GENUS_CODE","DZGN_G_prev")
# ble.comp<-ErrorComparision(data.gen,"GENUS_CODE","BLE_prev")
# chr.comp<-ErrorComparision(data.gen,"GENUS_CODE","CHRO_prev")
ad.comp<-ErrorComparision(data.sm,"GENUS_CODE","AdColDen")
jd.comp<-ErrorComparision(data.sm,"GENUS_CODE","JuvColDen")
cl.comp<-ErrorComparision(data.sm,"GENUS_CODE","Ave.cl")
rd.comp<-ErrorComparision(data.sm,"GENUS_CODE","Ave.rd")
od.comp<-ErrorComparision(data.sm,"GENUS_CODE","Ave.od")
dz.comp<-ErrorComparision(data.sm,"GENUS_CODE","DZGN_G_prev")
ble.comp<-ErrorComparision(data.sm,"GENUS_CODE","BLE_prev")
chr.comp<-ErrorComparision(data.sm,"GENUS_CODE","CHRO_prev")

all.rmse<-rbind(ad.comp,jd.comp,cl.comp,od.comp,rd.comp,dz.comp,ble.comp,chr.comp)
all.rmse<-rbind(ad.comp,jd.comp,cl.comp,od.comp,rd.comp,ble.comp)
all.rmse<-rbind(dz.comp,chr.comp)

rmse<-ddply(all.rmse,.(Metric,GENUS_CODE,Comp),
            summarize,
            RMSE=rmse(Y,X,na.rm=T),
            RMSE_mean=(RMSE/mean(X)),
            RMSE_sd=RMSE/sd(X),
            RMSE_maxmin=RMSE/max(X)-min(X),
            RMSE_iq=RMSE/(quantile(X,0.75)-quantile(X,0.25)))

rmse.ssss<-subset(rmse,GENUS_CODE=="SSSS")

rmse.ssss<-rmse.ssss %>% mutate(Comp=recode(Comp, 
                                           `D1vD2`="In water Observer",
                                           `Mbar`="Observer Overall",
                                           `MO`="Observer and Method",
                                           `Obar`="Method",
                                           `S1vS2`="SfM Observer"))
# rmse.ssss<-rmse.ssss %>% mutate(Metric=recode(Metric,                       
#                                             "AdColDen"="Adult Density",
#                                             `JuvColDen`="Juvenile Density",
#                                             `Ave.cl`="Colony Length",
#                                             `Ave.od`="Old Dead",
#                                             `Ave.rd`="Recent Dead",
#                                             `BLE_prev`="Bleaching Prevalence",
#                                             `DZGN_G_prev`="General Disease",
#                                             `CHRO_prev`="Chronic Disease"))   #Not working

comporder<-c("Observer and Method","Observer Overall","Method","SfM Observer","In water Observer")

rmse.ssss <- rmse.ssss[ order(match(rmse.ssss$Comp, comporder)),]

rmse.ssss$Comp<-as.character(rmse.ssss$Comp)
rmse.ssss$Comp<-factor(rmse.ssss$Comp, levels = comporder)
rmse.ssss$Metric<-as.character(rmse.ssss$Metric)

p1<-ggplot(rmse.ssss, aes(x=Comp, y=RMSE_mean, fill=Metric)) + 
  geom_bar(position=position_dodge(), stat="identity", color="black") + 
  guides(fill=FALSE) + facet_wrap(~Metric, scales="fixed", labeller=label_parsed) +
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

p1
ggsave(plot=p1,file="T:/Benthic/Data/SfM/ComparisionPlots/AllRMSE_Comparision_standardizeRMSE_mean.pdf",width=12,height=12)
ggsave(plot=p1,file="C:/Users/Corinne.Amir/Documents/SfM Stuff/PartialRMSE_Comparision_standardizeRMSE_mean.pdf",width=12,height=12)


#Plot across depth bins....TBC
dim(all.rmse)
all.rmse<-left_join(all.rmse,data.sm[,c(1:7,21:33)])
dim(all.rmse)
rmse<-ddply(all.rmse,.(Metric,GENUS_CODE,Comp, DEPTH_BIN),
            summarize,
            RMSE=rmse(Y,X,na.rm=T),
            RMSE_mean=(RMSE/mean(X)),
            RMSE_sd=RMSE/sd(X),
            RMSE_maxmin=RMSE/max(X)-min(X),
            RMSE_iq=RMSE/(quantile(X,0.75)-quantile(X,0.25)))




