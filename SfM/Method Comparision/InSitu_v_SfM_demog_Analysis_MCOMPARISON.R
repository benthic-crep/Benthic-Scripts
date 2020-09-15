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



source("T:/Benthic/Data/SfM/ScriptFiles/SfMvDiver Plotting Functions.R") 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")

setwd("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision")

#Read in files
seg<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_GENUS_SEGMENT.csv")
site<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_GENUS_SITE.csv")


# #Select columns to keep in segment data
seg<-dplyr::select(seg, c(METHOD,SITE,SITEVISITID,SEGMENT,GENUS_CODE,ANALYST,SEGAREA_ad,SEGAREA_j,AdColCount,AdColDen,JuvColDen,Ave.size,Ave.od,Ave.rd,
                                    BLE_prev,AcuteDZ_prev,ChronicDZ_prev,ISLAND,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,HABITAT_CODE,
                                    MIN_DEPTH_M,MAX_DEPTH_M))

seg<-subset(seg,SITE!="HAW-04285"&SEGMENT!="5") #sckewed by 1 very very large colony
sfm_seg<-subset(seg,METHOD=="SfM")
diver_seg<-subset(seg,METHOD=="Diver")

# #Set up in wide format
colnames(sfm_seg)[9:17] <- paste("SfM_", colnames(sfm_seg[,c(9:17)]), sep = "");sfm_seg<-dplyr::select(sfm_seg,-c(METHOD,HABITAT_CODE))

colnames(diver_seg)[9:17] <- paste("Diver_", colnames(diver_seg[,c(9:17)]), sep = "");diver_seg<-dplyr::select(diver_seg,-c(METHOD,ANALYST,ISLAND,HABITAT_CODE,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,MIN_DEPTH_M,MAX_DEPTH_M))

seg.wide<-merge(sfm_seg,diver_seg,by=c("SITE","SITEVISITID","SEGMENT","GENUS_CODE","SEGAREA_ad","SEGAREA_j"),all=T)

# #################IMPORTANT- CHECK FOR MERGING ERRORS
View(subset(seg.wide,GENUS_CODE=="SSSS")) #Make sure columns merge properly

#Merge together and remove segs that aren't merging properly
seg.wide<-merge(sfm_seg,diver_seg,by=c("SITE","SITEVISITID","SEGMENT","GENUS_CODE","SEGAREA_ad","SEGAREA_j"))
View(seg.wide) ##NOTE- LAN-01819 seg 10 SfM is duplicated for some reason-hopefully this will be fixed with final data

# #Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
seg.wide$Diver_JuvColDen[is.na(seg.wide$Diver_JuvColDen)]<-0
seg.wide$Diver_AdColDen[is.na(seg.wide$Diver_AdColDen)]<-0
seg.wide$SfM_JuvColDen[is.na(seg.wide$SfM_JuvColDen)]<-0
seg.wide$SfM_AdColDen[is.na(seg.wide$SfM_AdColDen)]<-0

head(seg.wide)
#
# head(subset(seg.wide,GENUS_CODE=="SSSS"))


# 
# # Plotting Regressions and Bland-Altman by Taxon --------------------------
# #PlotAll(dataframe, variable 1, variable 2, y-axis name 1, y-axis name 2, x-axis name 1, x-axis name 2)
# 
# outpath<- "T:/Benthic/Data/SfM/Method Comparision/Figures/Adult Density"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p1<-PlotAll(seg.wide,"Diver_AdColDen","SfM_AdColDen","SfM Adult Density","Difference SfM Analyst and Diver", "Diver Adult Density","Mean Adult Density")
# p1
# 
# outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Juvenile Density"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p4<-PlotAll(seg.wide,"Diver_JuvColDen","SfM_JuvColDen","SfM Juvenile Density","Difference SfM Analyst and Diver", "Diver Juvenile Density","Mean Juvenile Density")
# p4
# 
# outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Colony Size"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p7<-PlotAll(seg.wide,"Diver_Ave.size","SfM_Ave.size","SfM Colony Length","Difference SfM Analyst and Diver", "Diver Colony Length","Mean Colony Length")
# p7
# 
# outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Old Dead"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p10<-PlotAll(seg.wide,"Diver_Ave.od","SfM_Ave.od","SfM Average Old Dead Pct","Difference SfM Analyst and Diver", "Diver Average Old Dead Pct","Average Old Dead Pct")
# p10
# 
# outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Recent Dead"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p13<-PlotAll(seg.wide,"Diver_Ave.rd","SfM_Ave.rd","SfM Average Recent Dead Pct","Difference SfM Analyst and Diver", "Diver Average Recent Dead Pct","Average Recent Dead Pct")
# p13
# 
# outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Bleaching"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p16<-PlotAll(seg.wide,"Diver_BLE_prev","SfM_BLE_prev","SfM Bleaching Prevalence","Difference SfM Analyst and Diver", "Diver Bleaching Prevalence","Mean Bleaching Prevalence")
# p16
# 
# outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/ChronicDZ"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p19<-PlotAll(seg.wide,"Diver_ChronicDZ_prev","SfM_ChronicDZ_prev","SfM Chronic Disease Prevalence","Difference SfM Analyst and Diver", "Diver Chronic Disease Prevalence","Mean Chronic Disease Prevalence")
# p19
# 
# outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/AcuteDZ"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p22<-PlotAll(seg.wide,"Diver_AcuteDZ_prev","SfM_AcuteDZ_prev","SfM General Disease Prevalence","Difference SfM Analyst and Diver", "Diver General Disease Prevalence","Mean General Disease Prevalence")
# p22
# 
# 
# #Mixed Models
# table(seg$HABITAT_CODE)
# 
# #Simplify Habitat codes
# seg<-seg %>% mutate(HAB_R1=recode(HABITAT_CODE, 
#                                                   `AGR`="Carbonate Reef",
#                                                   `APR`="Carbonate Reef",
#                                                   `PAV`="Pavement",
#                                                   `PPR`="Pavement",
#                                                   `RRB`="Rubble",
#                                                   `ROB`="Rock & Boulder",
#                                                   `SCR`="Reef with Sand",
#                                                   `PSC`="Reef with Sand"))
# 
# 
# #bar plots of juv desnity by sector by year
# p1<-ggplot(subset(seg,GENUS_CODE=="SSSS"), aes(x=HAB_R1, y=AdColDen, fill=METHOD)) + 
#   geom_boxplot() + 
#   # guides(fill=FALSE) 
#   facet_wrap(~SEC_NAME,scales = "free", labeller=label_parsed) +
#   theme_bw() +
#   theme(
#     axis.text.x = element_text(angle = 90)
#     ,plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,axis.ticks.x = element_blank() # no x axis ticks
#     ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
#     ,legend.position="bottom")+
#   labs(x="Habitat Type",y="Mean Adult Density/m2")
# 
# p1
# 
# p2<-ggplot(subset(seg,GENUS_CODE=="SSSS"), aes(x=HAB_R1, y=AdColDen, fill=METHOD)) + 
#   geom_boxplot() + 
#   # guides(fill=FALSE) 
#   theme_bw() +
#   theme(
#     axis.text.x = element_text(angle = 90)
#     ,plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,axis.ticks.x = element_blank() # no x axis ticks
#     ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
#     ,legend.position="bottom")+
#   labs(x="Habitat Type",y="Mean Adult Density/m2")
# 
# p2
# 
# 
# #Segment Analysis with just RS, AH and MA
# head(seg)
# new.seg<-subset(seg,ANALYST %in% c("RS","MA"))
# new.seg$A_Sec<-paste(new.seg$ANALYST,new.seg$SEC_NAME,sep="_")
# 
# table(new.seg$METHOD,new.seg$SEC_NAME,new.seg$ANALYST)
# 
# SURVEY_SEC<-c("METHOD","ANALYST","SEC_NAME")
# survey_sec<-unique(new.seg[,SURVEY_SEC])
# 
# #Identify which sectors were surveyed by a RS and MA using both methods
# rs<-subset(survey_sec,ANALYST=="RS")
# tmp<-dcast(rs, SEC_NAME ~ METHOD, value.var="SEC_NAME")
# rs.sec<-subset(tmp,Diver==SfM)
# ma<-subset(survey_sec,ANALYST=="MA")
# tmp<-dcast(ma, SEC_NAME ~ METHOD, value.var="SEC_NAME")
# ma.sec<-subset(tmp,Diver==SfM)
# ma.sec$A_Sec<-paste("MA",ma.sec$SEC_NAME,sep="_")
# rs.sec$A_Sec<-paste("RS",rs.sec$SEC_NAME,sep="_")
# sub.sec<-rbind(ma.sec,rs.sec)
# sub.seg<-subset(new.seg,A_Sec %in% c(sub.sec$A_Sec))
# table(sub.seg$METHOD,sub.seg$SEC_NAME,sub.seg$ANALYST)
# 
# #Convert analyst names to numbers
# sub.seg<-sub.seg %>% mutate(ANALYST=recode(ANALYST, 
#                                     `MA`="1",
#                                     `RS`="2"))
# 
# #Adult Colony Density- sqrt transform
# s<-subset(sub.seg,GENUS_CODE=="SSSS")
# hist(sqrt(s$AdColDen))
# s$sqAdColDen<-sqrt(s$AdColDen)
# m<-lmer(sqAdColDen~METHOD*ANALYST + (1|SEC_NAME),data=s)
# 
# DPlots<-function(m,s){
#   par(mfrow=c(2,2)) # make the subplots
#   qqnorm(resid(m))
#   E2<-resid(m, type = "response") # extract normalized residuals
#   F2<-fitted(m) # extract the fitted data
#   plot(F2, E2, xlab = "fitted values", ylab = "residuals") # plot the relationship
#   abline(h = 0, lty = 2) # add a flat line at zerp
#   # test for homogeneity of variances
#   boxplot(E2~s$SEC_NAME, ylab = "residuals")
#   # check for independence. There should be no pattern
#   plot(E2~s$METHOD, ylab = 'residuals', xlab = "METHOD")
# }
# 
# DPlots(m,s)
# 
# mod1<-lmer(sqAdColDen~1 + (1|SEC_NAME),data=s)
# mod2<-lmer(sqAdColDen~METHOD + (1|SEC_NAME),data=s)
# mod3<-lmer(sqAdColDen~METHOD*ANALYST + (1|SEC_NAME),data=s)
# #mod7<-lmer(sqAdColDen~METHOD*HAB_R1*MAX_DEPTH_M + (1|SEC_NAME),data=s)
# 
# anova(mod3,mod2,test="Chisq")
# 
# PlotANALYST<-function(d,grouping_field,metric_field,genus_field,metric_name,x,y,siglabel){
#   d$GROUP<-d[,grouping_field]
#   d$METRIC<-d[,metric_field]
#   s<-subset(d,GROUP==genus_field)
#   
#   p<-ggplot(s, aes(x=ANALYST, y=METRIC, fill=METHOD)) + 
#     geom_boxplot() +
#     theme_bw() +
#     geom_label(label=siglabel, x=x,y=y,label.size = 0.35,color = "black", fill="white")+
#     theme(
#       axis.text.x = element_text(angle = 0)
#       ,plot.background = element_blank()
#       ,panel.grid.major = element_blank()
#       ,panel.grid.minor = element_blank()
#       ,axis.ticks.x = element_blank() # no x axis ticks
#       ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
#       ,legend.position="none")+
#     labs(x="Analyst",y=metric_name)
#   return(p)
# }
# 
# p1<-PlotANALYST(sub.seg,"GENUS_CODE","AdColDen","SSSS","Adult Density",1.5,28,"NS")
# p1
# 
# 
# #Juvenile
# s<-subset(sub.seg,GENUS_CODE=="SSSS")
# hist(sqrt(s$JuvColDen))
# s$sqJuvColDen<-sqrt(s$JuvColDen)
# m<-lmer(sqJuvColDen~METHOD*ANALYST + (1|SEC_NAME),data=s)
# DPlots(m,s)
# 
# mod1<-lmer(sqJuvColDen~1 + (1|SEC_NAME),data=s)
# mod2<-lmer(sqJuvColDen~METHOD + (1|SEC_NAME),data=s)
# mod3<-lmer(sqJuvColDen~METHOD*ANALYST + (1|SEC_NAME),data=s)
# #mod7<-lmer(sqJuvColDen~METHOD*HAB_R1*MAX_DEPTH_M + (1|SEC_NAME),data=s)
# 
# anova(mod3,mod2,test="Chisq")
# 
# p2<-PlotANALYST(sub.seg,"GENUS_CODE","JuvColDen","SSSS","Juvenile Density",1.5,36,"NS")
# #p1<-p1+geom_label(label="Significant", x=1,y=36,label.size = 0.35,color = "black", fill="#00BFC4")
# p2
# 
# #Ave. size
# s<-subset(sub.seg,GENUS_CODE=="SSSS")
# hist(sqrt(s$Ave.size))
# s$sqAve.size<-sqrt(s$Ave.size)
# m<-lmer(sqAve.size~METHOD*ANALYST + (1|SEC_NAME),data=s)
# DPlots(m,s)
# 
# mod1<-lmer(sqAve.size~1 + (1|SEC_NAME),data=s)
# mod2<-lmer(sqAve.size~METHOD + (1|SEC_NAME),data=s)
# mod3<-lmer(sqAve.size~METHOD*ANALYST + (1|SEC_NAME),data=s)
# #mod7<-lmer(sqAve.size~METHOD*HAB_R1*MAX_DEPTH_M + (1|SEC_NAME),data=s)
# 
# anova(mod3,mod2,test="Chisq")
# 
# p3<-PlotANALYST(sub.seg,"GENUS_CODE","Ave.size","SSSS","Average Max Diameter",1.5,36,"NS")
# #p1<-p1+geom_label(label="Significant", x=1,y=36,label.size = 0.35,color = "black", fill="#00BFC4")
# p3
# 
# #Ave. od- can't transform
# s<-subset(sub.seg,GENUS_CODE=="SSSS")
# hist(sqrt(s$Ave.od+1))
# s$sqAve.od<-sqrt(s$Ave.od)
# m<-lmer(sqAve.od~METHOD*ANALYST + (1|SEC_NAME),data=s)
# DPlots(m,s)
# 
# wilcox.test(Ave.od ~ METHOD, data=subset(s,ANALYST=="1")) 
# wilcox.test(Ave.od ~ METHOD, data=subset(s,ANALYST=="2")) 
# 
# 
# 
# p4<-PlotANALYST(sub.seg,"GENUS_CODE","Ave.od","SSSS","Average % Old Dead",1,36,"NS")
# #p1<-p1+geom_label(label="Significant", x=1,y=36,label.size = 0.35,color = "black", fill="#00BFC4")
# p4
# 
# 
# #Ave. rd
# s<-subset(sub.seg,GENUS_CODE=="SSSS")
# hist(sqrt(s$Ave.rd))
# s$sqAve.rd<-sqrt(s$Ave.rd)
# m<-lmer(sqAve.rd~METHOD*ANALYST + (1|SEC_NAME),data=s)
# DPlots(m,s)
# 
# wilcox.test(Ave.rd ~ METHOD, data=subset(s,ANALYST=="1")) 
# wilcox.test(Ave.rd ~ METHOD, data=subset(s,ANALYST=="2")) 
# 
# 
# p5<-PlotANALYST(sub.seg,"GENUS_CODE","Ave.rd","SSSS","Average % Recent Dead",1,10,"Significant")
# p5<-p5+geom_label(label="Significant", x=1,y=10,label.size = 0.35,color = "black", fill="#00BFC4")
# p5
# 
# #Acute disease
# s<-subset(sub.seg,GENUS_CODE=="SSSS")
# hist(sqrt(s$AcuteDZ_prev))
# s$sqAcuteDZ_prev<-sqrt(s$AcuteDZ_prev)
# m<-lmer(sqAcuteDZ_prev~METHOD*ANALYST + (1|SEC_NAME),data=s)
# DPlots(m,s)
# 
# wilcox.test(AcuteDZ_prev ~ METHOD, data=subset(s,ANALYST=="1")) 
# wilcox.test(AcuteDZ_prev ~ METHOD, data=subset(s,ANALYST=="2")) 
# 
# 
# p6<-PlotANALYST(sub.seg,"GENUS_CODE","AcuteDZ_prev","SSSS","Acute Disease Prevalence (%)",1,20,"Significant")
# p6<-p6+geom_label(label="Significant", x=1,y=20,label.size = 0.35,color = "black", fill="#00BFC4")
# p6
# 
# #Chronic disease
# s<-subset(sub.seg,GENUS_CODE=="SSSS")
# hist(sqrt(s$ChronicDZ_prev))
# s$sqChronicDZ_prev<-sqrt(s$ChronicDZ_prev)
# m<-lmer(sqChronicDZ_prev~METHOD*ANALYST + (1|SEC_NAME),data=s)
# DPlots(m,s)
# 
# wilcox.test(ChronicDZ_prev ~ METHOD, data=subset(s,ANALYST=="1")) 
# wilcox.test(ChronicDZ_prev ~ METHOD, data=subset(s,ANALYST=="2")) 
# 
# p7<-PlotANALYST(sub.seg,"GENUS_CODE","ChronicDZ_prev","SSSS","Chronic Disease Prevalence (%)",1.5,20,"NS")
# p7
# 
# #Bleaching
# s<-subset(sub.seg,GENUS_CODE=="SSSS")
# hist(sqrt(s$BLE_prev))
# s$sqBLE_prev<-sqrt(s$BLE_prev)
# m<-lmer(sqBLE_prev~METHOD*ANALYST + (1|SEC_NAME),data=s)
# DPlots(m,s)
# 
# wilcox.test(BLE_prev ~ METHOD, data=subset(s,ANALYST=="1")) 
# wilcox.test(BLE_prev ~ METHOD, data=subset(s,ANALYST=="2")) 
# 
# 
# p8<-PlotANALYST(sub.seg,"GENUS_CODE","BLE_prev","SSSS","Bleaching Prevalence (%)",1,45,"Significant")
# p8<-p8+geom_label(label="Significant", x=1,y=45,label.size = 0.35,color = "black", fill="#00BFC4")
# p8<-p8+geom_label(label="Significant", x=2,y=45,label.size = 0.35,color = "black", fill="#00BFC4")
# p8
# 
# 
# allplots<-grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,nrow=2,ncol=4)
# 
# ggsave(plot<-allplots,file="T:/Benthic/Data/SfM/Method Comparision/Figures/Analystplots_stats.png",width=10,height=5)
# 


#Site level analysis
# Prep Site-level data ----------------------------------------------------

#Select columns to keep in SITE data
site.new<-dplyr::select(site, c(METHOD,SITEVISITID,SITE,GENUS_CODE,TRANSECTAREA_ad,TRANSECTAREA_j,AdColCount,AdColDen,JuvColDen,Ave.size,Ave.od,Ave.rd,
                            BLE_prev,AcuteDZ_prev,ChronicDZ_prev,ISLAND,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,HABITAT_CODE,
                            MIN_DEPTH_M,MAX_DEPTH_M))


#site<-subset(site,SITE!="HAW-04285") #sckewed by 1 very very large colony
sfm_site<-subset(site.new,METHOD=="SfM")
diver_site<-subset(site.new,METHOD=="Diver")

##############################START HERE#####################


#Set up in wide format
colnames(sfm_site)[7:15] <- paste("SfM_", colnames(sfm_site[,c(7:15)]), sep = "");sfm_site<-dplyr::select(sfm_site,-c(METHOD,HABITAT_CODE))

colnames(diver_site)[7:15] <- paste("Diver_", colnames(diver_site[,c(7:15)]), sep = "");diver_site<-dplyr::select(diver_site,-c(METHOD,ISLAND,HABITAT_CODE,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,MIN_DEPTH_M,MAX_DEPTH_M))

site.wide<-merge(sfm_site,diver_site,by=c("SITE","SITEVISITID","GENUS_CODE","TRANSECTAREA_ad","TRANSECTAREA_j"),all=T)

length(unique(site.wide$SITEVISITID)) #should be 104

#################IMPORTANT- CHECK FOR MERGING ERRORS
View(subset(site.wide,GENUS_CODE=="SSSS")) #Make sure columns merge properly

#Merge together and remove sites that aren't merging properly
site.wide<-merge(sfm_site,diver_site,by=c("SITE","SITEVISITID","GENUS_CODE","TRANSECTAREA_ad","TRANSECTAREA_j"))
View(site.wide) ##NOTE- LAN-01819 site 10 SfM is duplicated for some reason-hopefully this will be fixed with final data

# #Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
site.wide$Diver_JuvColDen[is.na(site.wide$Diver_JuvColDen)]<-0
site.wide$Diver_AdColDen[is.na(site.wide$Diver_AdColDen)]<-0
site.wide$SfM_JuvColDen[is.na(site.wide$SfM_JuvColDen)]<-0
site.wide$SfM_AdColDen[is.na(site.wide$SfM_AdColDen)]<-0

head(site.wide)

head(subset(site.wide,GENUS_CODE=="SSSS"))

# 
# # Plotting Regressions and Bland-Altman by Taxon --------------------------
# #PlotAll(dataframe, variable 1, variable 2, y-axis name 1, y-axis name 2, x-axis name 1, x-axis name 2)
# 
# outpath<- "T:/Benthic/Data/SfM/Method Comparision/Figures/Site/Adult Density"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p1<-PlotAll(site.wide,"Diver_AdColDen","SfM_AdColDen","SfM Adult Density","Difference SfM Analyst and Diver", "Diver Adult Density","Mean Adult Density")
# p1
# 
# outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Site/Juvenile Density"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p4<-PlotAll(site.wide,"Diver_JuvColDen","SfM_JuvColDen","SfM Juvenile Density","Difference SfM Analyst and Diver", "Diver Juvenile Density","Mean Juvenile Density")
# p4
# 
# outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Site/Colony Size"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p7<-PlotAll(site.wide,"Diver_Ave.size","SfM_Ave.size","SfM Colony Length","Difference SfM Analyst and Diver", "Diver Colony Length","Mean Colony Length")
# p7
# 
# outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Site/Old Dead"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p10<-PlotAll(site.wide,"Diver_Ave.od","SfM_Ave.od","SfM Average Old Dead Pct","Difference SfM Analyst and Diver", "Diver Average Old Dead Pct","Average Old Dead Pct")
# p10
# 
# outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Site/Recent Dead"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p13<-PlotAll(site.wide,"Diver_Ave.rd","SfM_Ave.rd","SfM Average Recent Dead Pct","Difference SfM Analyst and Diver", "Diver Average Recent Dead Pct","Average Recent Dead Pct")
# p13
# 
# outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Site/Bleaching"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p16<-PlotAll(site.wide,"Diver_BLE_prev","SfM_BLE_prev","SfM Bleaching Prevalence","Difference SfM Analyst and Diver", "Diver Bleaching Prevalence","Mean Bleaching Prevalence")
# p16
# 
# outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Site/ChronicDZ"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p19<-PlotAll(site.wide,"Diver_ChronicDZ_prev","SfM_ChronicDZ_prev","SfM Chronic Disease Prevalence","Difference SfM Analyst and Diver", "Diver Chronic Disease Prevalence","Mean Chronic Disease Prevalence")
# p19
# 
# outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Site/AcuteDZ"
# if(!dir.exists(outpath)){dir.create(outpath)}
# p22<-PlotAll(site.wide,"Diver_AcuteDZ_prev","SfM_AcuteDZ_prev","SfM General Disease Prevalence","Difference SfM Analyst and Diver", "Diver General Disease Prevalence","Mean General Disease Prevalence")
# p22


#Mixed Models
table(site$HABITAT_CODE)

#Simplify Habitat codes
site<-site %>% mutate(HAB_R1=recode(HABITAT_CODE, 
                                    `AGR`="Aggregate Reef",
                                    `APR`="Patch Reef",
                                    `PAV`="Pavement",
                                    `PPR`="Patch Reef",
                                    `RRB`="Rubble",
                                    `ROB`="Rock & Boulder",
                                    `SCR`="Patch Reef",
                                    `PSC`="Patch Reef"))


#bar plots of juv desnity by sector by year
p1<-ggplot(subset(site,GENUS_CODE=="SSSS"), aes(x=HAB_R1, y=AdColDen, fill=METHOD)) + 
  geom_boxplot() + 
  # guides(fill=FALSE) 
  facet_wrap(~SEC_NAME,scales = "free", labeller=label_parsed) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="bottom")+
  labs(x="Habitat Type",y="Mean Adult Density/m2")

p1

p2<-ggplot(subset(site,GENUS_CODE=="SSSS"), aes(x=HAB_R1, y=AdColDen, fill=METHOD)) + 
  geom_boxplot() + 
  # guides(fill=FALSE) 
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="bottom")+
  labs(x="Habitat Type",y="Mean Adult Density/m2")

p2

#https://stats.stackexchange.com/questions/375911/strange-qqplot-for-poisson-gam

# glmerDensity<-function(d,grouping_field="GENUS_CODE",metric_field="AdColDen"){
#   d$GROUP<-d[,grouping_field]
#   d$METRIC<-d[,metric_field]
#   
#   s<-subset(d,GROUP=="SSSS")
#   nullmod<-glmer(METRIC~1 + (1|SEC_NAME)+ offset(siteAREA_ad),family=poisson,data=s)
#   mod1<-glmer(METRIC~METHOD + (1|SEC_NAME)+ offset(siteAREA_ad),family=poisson,data=s)
#   mod4<-glmer(METRIC~METHOD*MAX_DEPTH_M + (1|SEC_NAME)+ offset(siteAREA_ad),family=poisson,data=s)
#   mod5<-glmer(METRIC~METHOD*HAB_R1 + (1|SEC_NAME)+ offset(siteAREA_ad),family=poisson,data=s)
#   
# }
# 
# 
# tab_model(mod2)
# 
# #Model Selection
# 
# glmerDensity<-function(d,grouping_field="GENUS_CODE",genus_field="SSSS",metric_field="AdColCount"){
#   d$GROUP<-d[,grouping_field]
#   d$METRIC<-d[,metric_field]
#   s<-subset(d,GROUP==genus_field)
#   
#   Cand.set <- list( )
#   Cand.set[[1]]<-glmer(METRIC~1 + (1|ISLAND/SEC_NAME),family=poisson,data=s)
#   Cand.set[[2]]<-glmer(METRIC~METHOD + (1|ISLAND/SEC_NAME),family=poisson,data=s)
#   Cand.set[[4]]<-glmer(METRIC~METHOD*MAX_DEPTH_M + (1|ISLAND/SEC_NAME),family=poisson,data=s,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
#   Cand.set[[5]]<-glmer(METRIC~METHOD*HAB_R1 + (1|ISLAND/SEC_NAME),family=poisson,data=s,control=glmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=200000)))
#   
#   Modnames <- c("Null","Method","Method + Depth","Method + Habitat")
#   ##generate AICc table
#   aictab(cand.set = Cand.set, modnames = Modnames, sort = TRUE)
#   ##round to 4 digits after decimal point and give log-likelihood
#   print(aictab(cand.set = Cand.set, modnames = Modnames, sort = TRUE),
#         digits = 3, LL = TRUE)
#   
#   a<-r.squaredGLMM(Cand.set[[1]])
#   b<-r.squaredGLMM(Cand.set[[2]])
#   c<-r.squaredGLMM(Cand.set[[3]])
#   d<-r.squaredGLMM(Cand.set[[4]])
#   e<-r.squaredGLMM(Cand.set[[5]])
#   
#   print(a);print(b);print(c);print(d);print(e)
#   
# }
# 
# #Adults
# glmerDensity(site,"GENUS_CODE","SSSS","AdColCount")
# glmerDensity(site,"GENUS_CODE","POSP","AdColCount")
# glmerDensity(site,"GENUS_CODE","MOSP","AdColCount")
# glmerDensity(site,"GENUS_CODE","POCS","AdColCount")
# 
# glmerDensity(site,"GENUS_CODE","SSSS","JuvColDen")
# glmerDensity(site,"GENUS_CODE","POSP","JuvColDen")
# glmerDensity(site,"GENUS_CODE","MOSP","JuvColDen")
# glmerDensity(site,"GENUS_CODE","POCS","JuvColDen")

#Adult Colony Density- sqrt transform
#also tried gamma and neg binomial- sqrt transform is best
s<-subset(site,GENUS_CODE=="SSSS")
hist(log(s$AdColDen))

s$sqAdColDen<-sqrt(s$AdColDen)

m<-lmer(sqAdColDen~METHOD + (1|SEC_NAME),data=s)
m<-lm(length~model,data=s)


DPlots<-function(m,s){
par(mfrow=c(2,2)) # make the subplots
qqnorm(resid(m))
E2<-resid(m, type = "response") # extract normalized residuals
F2<-fitted(m) # extract the fitted data
plot(F2, E2, xlab = "fitted values", ylab = "residuals") # plot the relationship
abline(h = 0, lty = 2) # add a flat line at zerp
# test for homogeneity of variances
boxplot(E2~s$SEC_NAME, ylab = "residuals")
# check for independence. There should be no pattern
plot(E2~s$METHOD, ylab = 'residuals', xlab = "METHOD")
}

DPlots(m,s)


full.form <- formula(sqAdColDen ~ METHOD*MAX_DEPTH_M*HAB_R1, data=s)

#fit lme using "ML"= maximum likelihood - so can use step fxn
full.lme <-lme(full.form, data=s, random=~1|SEC_NAME, method="ML") #use if needed

#drop 3-way interaction term
FULL.MOD <- update(full.lme, .~. -METHOD:MAX_DEPTH_M:HAB_R1) 
FULL.MOD <- update(FULL.MOD, .~. -MAX_DEPTH_M:HAB_R1) 
anova(FULL.MOD) #from output, drop the 2-way interaction term with the larger p-value first

RED.MOD1 <- update(FULL.MOD, .~. -METHOD:HAB_R1) #drop 2-way interaction term
anova(FULL.MOD, RED.MOD1) #LRT --> move forward w/ whichever model keeps/removes term

RED.MOD2 <- update(RED.MOD1, .~. -METHOD:MAX_DEPTH_M) #drop 2-way interaction term
anova(RED.MOD1, RED.MOD2) #LRT --> move forward w/ whichever model keeps/removes term

RED.MOD3 <- update(RED.MOD2, .~. -METHOD) #drop 2-way interaction term
anova(RED.MOD2, RED.MOD3) #LRT --> move forward w/ whichever model keeps/removes term




#Extract predicted values for Adult density
#https://aosmith.rbind.io/2018/11/16/plot-fitted-lines/
library(nlme)
mod<-lme(sqAdColDen~METHOD*MAX_DEPTH_M, random = ~1|SEC_NAME,data=s)

newdat.lme = data.frame(METHOD = s$METHOD,
                        MAX_DEPTH_M = rep(seq(0,30,by=0.29),2),
                        SEC_NAME = s$SEC_NAME)
head(newdat.lme)
newdat.lme$predlme <- predict(mod, newdata = newdat.lme, level=0)
newdat.lme$AdColDen<-newdat.lme$predlme^2 #back transform predicted values

des = model.matrix(formula(mod)[-2], newdat.lme)
predvar = diag( des %*% vcov(mod) %*% t(des) )
newdat.lme$lower = with(newdat.lme, predlme - 2*sqrt(predvar) )
newdat.lme$upper = with(newdat.lme, predlme + 2*sqrt(predvar) )
newdat.lme$lower<-newdat.lme$lower^2 #back transform predicted values
newdat.lme$upper<-newdat.lme$upper^2 #back transform predicted values


s.wide<-subset(site.wide,GENUS_CODE=="SSSS")
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


p1<-Plot1to1_new(s.wide,"SfM_AdColDen","Diver_AdColDen","SfM Adult Density","Diver Adult Density")
p2<-PlotMethod(site,"GENUS_CODE","AdColDen","SSSS","Adult Density",1.5,28,"NS")
p3<-PlotHabitat(site,"GENUS_CODE","AdColDen","SSSS","Adult Density",3,28,"Method x Habitat NS")
p4<-PlotDepth(site,"GENUS_CODE","AdColDen","AdColDen","SSSS","Adult Density",15,28,"Method x Depth NS")

AdColDenS<-grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)

ggsave(plot<-AdColDenS,file="T:/Benthic/Data/SfM/Method Comparision/Figures/AdColDenSSSS_stats.png",width=8,height=6)

# #Rather than setting up complicated contrasts to test for dif between methods within habitats
# #Run separate models for each habitat then run multiple test corrections
# a<-summary(glht(lmer(sqAdColDen~METHOD + (1|SEC_NAME),data=subset(s,HAB_R1=="Aggregate Reef")),linfct=mcp(METHOD="Tukey")))
# b<-summary(glht(lmer(sqAdColDen~METHOD + (1|SEC_NAME),data=subset(s,HAB_R1=="Pavement")),linfct=mcp(METHOD="Tukey")))
# c<-summary(glht(lmer(sqAdColDen~METHOD + (1|SEC_NAME),data=subset(s,HAB_R1=="Patch Reef")),linfct=mcp(METHOD="Tukey")))
# d<-summary(glht(lmer(sqAdColDen~METHOD + (1|SEC_NAME),data=subset(s,HAB_R1=="Rock & Boulder")),linfct=mcp(METHOD="Tukey")))
# e<-summary(glht(lmer(sqAdColDen~METHOD + (1|SEC_NAME),data=subset(s,HAB_R1=="Rubble")),linfct=mcp(METHOD="Tukey")))
# 
# #Extract values and adjust for multiple tests- 
# pval<-c(a$test$pvalues,b$test$pvalues,c$test$pvalues,d$test$pvalues,e$test$pvalues)
# p.adjust(pval,"BH")

# 
# lsmeans(mod6,~ MH) 
# tmp <-emmeans(mod6, pairwise ~ METHOD | HAB_R1)

#Juvenile Colony Density- sqrt transform
#also tried gamma and neg binomial- sqrt transform is best
s<-subset(site,GENUS_CODE=="SSSS")
hist(sqrt(s$JuvColDen))
s$sqJuvColDen<-sqrt(s$JuvColDen)
m<-lmer(sqJuvColDen~METHOD + (1|SEC_NAME),data=s)

DPlots(m,s)


full.form <- formula(sqJuvColDen ~ METHOD*MAX_DEPTH_M*HAB_R1, data=s)

#fit lme using "ML"= maximum likelihood - so can use step fxn
full.lme <-lme(full.form, data=s, random=~1|SEC_NAME, method="ML") #use if needed

#drop 3-way interaction term & depth x habitat (not interested in these hypotheses)
FULL.MOD <- update(full.lme, .~. -METHOD:MAX_DEPTH_M:HAB_R1) 
FULL.MOD <- update(FULL.MOD, .~. -MAX_DEPTH_M:HAB_R1) 
anova(FULL.MOD) #from output, drop the 2-way interaction term with the larger p-value first

RED.MOD1 <- update(FULL.MOD, .~. -METHOD:HAB_R1) #drop 2-way interaction term
anova(FULL.MOD, RED.MOD1) #LRT --> move forward w/ whichever model keeps/removes term

RED.MOD2 <- update(RED.MOD1, .~. -METHOD:MAX_DEPTH_M) #drop 2-way interaction term
anova(RED.MOD1, RED.MOD2) #LRT --> move forward w/ whichever model keeps/removes term


#Extract predicted values for Adult density
#https://aosmith.rbind.io/2018/11/16/plot-fitted-lines/
library(nlme)
mod<-lme(sqJuvColDen~METHOD*MAX_DEPTH_M, random = ~1|SEC_NAME,data=s)

newdat.lme = data.frame(METHOD = s$METHOD,
                        MAX_DEPTH_M = rep(seq(0,30,by=0.29),2),
                        SEC_NAME = s$SEC_NAME)
head(newdat.lme)
newdat.lme$predlme <- predict(mod, newdata = newdat.lme, level=0)
newdat.lme$JuvColDen<-newdat.lme$predlme^2 #back transform predicted values

des = model.matrix(formula(mod)[-2], newdat.lme)
predvar = diag( des %*% vcov(mod) %*% t(des) )
newdat.lme$lower = with(newdat.lme, predlme - 2*sqrt(predvar) )
newdat.lme$upper = with(newdat.lme, predlme + 2*sqrt(predvar) )
newdat.lme$lower<-newdat.lme$lower^2 #back transform predicted values
newdat.lme$upper<-newdat.lme$upper^2 #back transform predicted values

p1<-Plot1to1_new(s.wide,"SfM_JuvColDen","Diver_JuvColDen","SfM Juvenile Density","Diver Juvenile Density")
p2<-PlotMethod(site,"GENUS_CODE","JuvColDen","SSSS","Juvenile Density",1.5,60,"NS")
p3<-PlotHabitat(site,"GENUS_CODE","JuvColDen","SSSS","Juvenile Density",3,60,"Method x Habitat NS")
p4<-PlotDepth(site,"GENUS_CODE","JuvColDen","JuvColDen","SSSS","Juvenile Density",15,60,"Method x Depth NS")

JuvColDenS<-grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)

ggsave(plot<-JuvColDenS,file="T:/Benthic/Data/SfM/Method Comparision/Figures/JuvColDenSSSS_stats.png",width=8,height=6)




#Ave Size- not perfect transformation, but will work
s<-subset(site,GENUS_CODE=="SSSS")
hist(log(s$Ave.size))
s$logAve.size<-log(s$Ave.size)
m<-lmer(logAve.size~METHOD + (1|SEC_NAME),data=s)

DPlots(m,s)

full.form <- formula(logAve.size ~ METHOD*MAX_DEPTH_M*HAB_R1, data=s)

#fit lme using "ML"= maximum likelihood - so can use step fxn
full.lme <-lme(full.form, data=s, random=~1|SEC_NAME, method="ML",na.action = na.omit) #use if needed

#drop 3-way interaction term & depth x habitat (not interested in these hypotheses)
FULL.MOD <- update(full.lme, .~. -METHOD:MAX_DEPTH_M:HAB_R1) 
FULL.MOD <- update(FULL.MOD, .~. -MAX_DEPTH_M:HAB_R1) 
anova(FULL.MOD) #from output, drop the 2-way interaction term with the larger p-value first

RED.MOD1 <- update(FULL.MOD, .~. -METHOD:HAB_R1) #drop 2-way interaction term
anova(FULL.MOD, RED.MOD1) #LRT --> move forward w/ whichever model keeps/removes term

RED.MOD2 <- update(RED.MOD1, .~. -METHOD:MAX_DEPTH_M) #drop 2-way interaction term
anova(RED.MOD1, RED.MOD2) #LRT --> move forward w/ whichever model keeps/removes term

RED.MOD3 <- update(RED.MOD2, .~. -METHOD) #drop 2-way interaction term
anova(RED.MOD2, RED.MOD3) #LRT --> move forward w/ whichever model keeps/removes term



#Extract predicted values for Adult density
#https://aosmith.rbind.io/2018/11/16/plot-fitted-lines/
library(nlme)
mod<-lme(logAve.size~METHOD*MAX_DEPTH_M, random = ~1|SEC_NAME,data=s,na.action=na.omit)

newdat.lme = data.frame(METHOD = s$METHOD,
                        MAX_DEPTH_M = rep(seq(0,30,by=0.29),2),
                        SEC_NAME = s$SEC_NAME)
head(newdat.lme)
newdat.lme$predlme <- predict(mod, newdata = newdat.lme, level=0)
newdat.lme$Ave.size<-exp(newdat.lme$predlme) #back transform predicted values

des = model.matrix(formula(mod)[-2], newdat.lme)
predvar = diag( des %*% vcov(mod) %*% t(des) )
newdat.lme$lower = with(newdat.lme, predlme - 2*sqrt(predvar) )
newdat.lme$upper = with(newdat.lme, predlme + 2*sqrt(predvar) )
newdat.lme$lower<-exp(newdat.lme$lower) #back transform predicted values
newdat.lme$upper<-exp(newdat.lme$upper) #back transform predicted values

p1<-Plot1to1_new(s.wide,"SfM_Ave.size","Diver_Ave.size","SfM Average Max Diameter (cm)","Diver Average Max Diameter (cm)")
p2<-PlotMethod(site,"GENUS_CODE","Ave.size","SSSS","Average Max Diameter (cm)",1.5,34,"NS")
p3<-PlotHabitat(site,"GENUS_CODE","Ave.size","SSSS","Average Max Diameter (cm)",3,34,"Method x Habitat NS")
p4<-PlotDepth(site,"GENUS_CODE","Ave.size","Ave.size","SSSS","Average Max Diameter (cm)",15,34,"Method x Depth NS")

Ave.sizeS<-grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)

ggsave(plot<-Ave.sizeS,file="T:/Benthic/Data/SfM/Method Comparision/Figures/Ave.sizeSSSS_stats.png",width=8,height=6)


#Old dead- not perfect transformation, but will work
s<-subset(site,GENUS_CODE=="SSSS")
hist(sqrt(s$Ave.od))
s$sqAve.od<-sqrt(s$Ave.od)
mod<-lmer(sqAve.od~METHOD + (1|SEC_NAME),data=s)
DPlots(m,s)

full.form <- formula(sqAve.od ~ METHOD*MAX_DEPTH_M*HAB_R1, data=s)

#fit lme using "ML"= maximum likelihood - so can use step fxn
full.lme <-lme(full.form, data=s, random=~1|SEC_NAME, method="ML",na.action = na.omit) #use if needed

#drop 3-way interaction term & depth x habitat (not interested in these hypotheses)
FULL.MOD <- update(full.lme, .~. -METHOD:MAX_DEPTH_M:HAB_R1) 
FULL.MOD <- update(FULL.MOD, .~. -MAX_DEPTH_M:HAB_R1) 
anova(FULL.MOD) #from output, drop the 2-way interaction term with the larger p-value first

RED.MOD1 <- update(FULL.MOD, .~. -METHOD:MAX_DEPTH_M) #drop 2-way interaction term
anova(FULL.MOD, RED.MOD1) #LRT --> move forward w/ whichever model keeps/removes term

RED.MOD2 <- update(RED.MOD1, .~. -METHOD:HAB_R1) #drop 2-way interaction term
anova(RED.MOD1, RED.MOD2) #LRT --> move forward w/ whichever model keeps/removes term

RED.MOD3 <- update(RED.MOD2, .~. -METHOD) #drop 2-way interaction term
anova(RED.MOD2, RED.MOD3) #LRT --> move forward w/ whichever model keeps/removes term



#Extract predicted values for Adult density
#https://aosmith.rbind.io/2018/11/16/plot-fitted-lines/
library(nlme)
mod<-lme(sqAve.od~METHOD*MAX_DEPTH_M, random = ~1|SEC_NAME,data=s,na.action=na.omit)

newdat.lme = data.frame(METHOD = s$METHOD,
                        MAX_DEPTH_M = rep(seq(0,30,by=0.29),2),
                        SEC_NAME = s$SEC_NAME)
head(newdat.lme)
newdat.lme$predlme <- predict(mod, newdata = newdat.lme, level=0)
newdat.lme$Ave.od<-newdat.lme$predlme^2 #back transform predicted values

des = model.matrix(formula(mod)[-2], newdat.lme)
predvar = diag( des %*% vcov(mod) %*% t(des) )
newdat.lme$lower = with(newdat.lme, predlme - 2*sqrt(predvar) )
newdat.lme$upper = with(newdat.lme, predlme + 2*sqrt(predvar) )
newdat.lme$lower<-newdat.lme$lower^2 #back transform predicted values
newdat.lme$upper<-newdat.lme$upper^2 #back transform predicted values

p1<-Plot1to1_new(s.wide,"SfM_Ave.od","Diver_Ave.od","SfM Average % Old Dead","Diver % Old Dead")
p2<-PlotMethod(site,"GENUS_CODE","Ave.od","SSSS","Average % Old Dead",1.5,49,"Method Significant")
p2<-p2+geom_label(label="Method Significant", x=1.5,y=49,label.size = 0.35,color = "black", fill="#00BFC4")
p3<-PlotHabitat(site,"GENUS_CODE","Ave.od","SSSS","Average % Old Dead",3,49,"Method x Habitat NS")
p4<-PlotDepth(site,"GENUS_CODE","Ave.od","Ave.od","SSSS","Average % Old Dead",15,49,"Method x Depth NS")

Ave.odS<-grid.arrange(p1,p2,p3,p4,nrow=4,ncol=1)
Ave.odS<-grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)

ggsave(plot<-Ave.odS,file="T:/Benthic/Data/SfM/Method Comparision/Figures/Ave.odSSSS_stats.png",width=8,height=6)


#Can't transform recent dead

hist(sqrt(s$Ave.rd))
s$logAve.rd<-log(s$Ave.rd+1)
mod<-lmer(logAve.rd~METHOD + (1|SEC_NAME),data=s)
plot(mod)
qqnorm(resid(mod)) #plot normal quantile- quantile plot.  Should be close to a straight line


#Remove sites that have 0s for both sfM and diver
s.wide<-subset(site.wide,GENUS_CODE=="SSSS")
s<-subset(site,GENUS_CODE=="SSSS");length(unique(s$SITE))
tmp <- s.wide[ which(s.wide$SfM_Ave.rd + s.wide$Diver_Ave.rd>0) , ]
sitelist<-unique(tmp$SITE)
s<-dplyr::filter(s, SITE %in% sitelist);length(unique(s$SITE))

s<-subset(site,GENUS_CODE=="SSSS");length(unique(s$SITE))
  
#Can't transform recent dead with sqrt, log or power transformation even after removing sites that have 0 rd in both sfm and diver
wilcox.test(Ave.rd ~ METHOD, data=s) 
wilcox.test(Ave.rd ~ METHOD, data=subset(s,HAB_R1=="Aggregate Reef")) 
wilcox.test(Ave.rd ~ METHOD, data=subset(s,HAB_R1=="Patch Reef")) 
wilcox.test(Ave.rd ~ METHOD, data=subset(s,HAB_R1=="Pavement")) 
wilcox.test(Ave.rd ~ METHOD, data=subset(s,HAB_R1=="Rock & Boulder")) 
wilcox.test(Ave.rd ~ METHOD, data=subset(s,HAB_R1=="Rubble")) 

t<-subset(s,METHOD=="Diver")
corr <- cor.test(x=t$MAX_DEPTH_M, y=t$Ave.rd, method = 'spearman')
corr

PlotDepth_NP<-function(d,grouping_field,metric_field,genus_field,metric_name){
  d$GROUP<-d[,grouping_field]
  d$METRIC<-d[,metric_field]
  s<-subset(d,GROUP==genus_field)
  
  p1<-ggplot(s, aes(x=MAX_DEPTH_M, y=METRIC,fill = METHOD,color=METHOD)) + 
    geom_smooth(method="lm")+
    geom_point(size=1) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 0)
          ,plot.background = element_blank()
          ,panel.grid.major = element_blank()
          ,panel.grid.minor = element_blank()
          ,axis.ticks.x = element_blank() # no x axis ticks
          ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
          ,legend.position="none")+
    labs(x="Max Depth (m)",y=metric_name)     
  return(p1)
}

  
p1<-Plot1to1_new(s.wide,"SfM_Ave.rd","Diver_Ave.rd","SfM Average % Recent Dead","Diver % Recent Dead")
p2<-PlotMethod(site,"GENUS_CODE","Ave.rd","SSSS","Average % Recent Dead",1.5,3.5,"NS")
p3<-PlotHabitat(site,"GENUS_CODE","Ave.rd","SSSS","Average % Recent Dead",3,34,"Method x Habitat NS")
p4<-PlotDepth_NP(site,"GENUS_CODE","Ave.rd","SSSS","Average % Recent Dead")

Ave.rdS<-grid.arrange(p1,p2,p3,p4,nrow=4,ncol=1)

ggsave(plot<-Ave.rdS,file="T:/Benthic/Data/SfM/Method Comparision/Figures/Ave.rdSSSS_stats.png",width=8,height=6)

allplots<-grid.arrange(Ave.odS,Ave.rdS,nrow=1,ncol=3)
ggsave(plot<-allplots,file="T:/Benthic/Data/SfM/Method Comparision/Figures/PartialMortality_stats.png",width=8,height=10)


#Identify % of sites that had 0 rd for one method but not the other
a<-(s.wide[ which(s.wide$SfM_Ave.rd ==0 & s.wide$Diver_Ave.rd>0) , ])
nrow(a)/104
b<-(s.wide[ which(s.wide$SfM_Ave.rd >0 & s.wide$Diver_Ave.rd==0) , ])
nrow(b)/104


#Acute disease prevalence
s<-subset(site,GENUS_CODE=="SSSS")
s$AcuteDZ<-(s$AcuteDZ_prev/100)*s$AdColCount
s$test<-as.integer(as.character(s$AcuteDZ))
head(s)

s.wide<-subset(site.wide,GENUS_CODE=="SSSS")
s<-subset(site,GENUS_CODE=="SSSS");length(unique(s$SITE))
s$AcuteDZ<-(s$AcuteDZ_prev/100)*s$AdColCount
s$test<-as.integer(as.character(s$AcuteDZ))
head(s)

#try removing sites that had 0 for both methods- transformation doesn't work
tmp <- s.wide[ which(s.wide$SfM_AcuteDZ_prev + s.wide$Diver_AcuteDZ_prev>0) , ]
sitelist<-unique(tmp$SITE)
s<-dplyr::filter(s, SITE %in% sitelist);length(unique(s$SITE))

#try removing sites where you had 0 for either method - only 30 sites remain can't run analyses
tmp <- s.wide[ which(s.wide$SfM_AcuteDZ_prev>0 & s.wide$Diver_AcuteDZ_prev>0) , ]

#Can't transform recent dead with bionomial distrubtion, sqrt, log or power transformation even after removing sites that have 0 rd in both sfm and diver
s<-subset(site,GENUS_CODE=="SSSS")
wilcox.test(AcuteDZ_prev ~ METHOD, data=s) 
wilcox.test(AcuteDZ_prev ~ METHOD, data=subset(s,HAB_R1=="Aggregate Reef")) 
wilcox.test(AcuteDZ_prev ~ METHOD, data=subset(s,HAB_R1=="Patch Reef")) 
wilcox.test(AcuteDZ_prev ~ METHOD, data=subset(s,HAB_R1=="Pavement")) 
wilcox.test(AcuteDZ_prev ~ METHOD, data=subset(s,HAB_R1=="Rock & Boulder")) 
wilcox.test(AcuteDZ_prev ~ METHOD, data=subset(s,HAB_R1=="Rubble")) 

pval<-c(0.155,
        0.0681,
        0.7014,
        0.8823,
        0.173)
p.adjust(pval,"BH")


t<-subset(s,METHOD=="Diver")
corr <- cor.test(x=t$MAX_DEPTH_M, y=t$AcuteDZ_prev, method = 'spearman')
corr

p1<-Plot1to1_new(s.wide,"SfM_AcuteDZ_prev","Diver_AcuteDZ_prev","SfM Prevalence (%)","Diver Prevalence (%)");p1<-p1+ggtitle("Acute Disease")
p2<-PlotMethod(site,"GENUS_CODE","AcuteDZ_prev","SSSS","Prevalence (%)",1.5,14,"NS")
p3<-PlotHabitat(site,"GENUS_CODE","AcuteDZ_prev","SSSS","Prevalence (%)",3,34,"Method x Habitat NS")
p4<-PlotDepth_NP(site,"GENUS_CODE","AcuteDZ_prev","SSSS","Prevalence (%)")

acutedzS<-grid.arrange(p1,p2,p3,p4,nrow=4,ncol=1)

#ggsave(plot<-acutedzS,file="T:/Benthic/Data/SfM/Method Comparision/Figures/AcuteDZSSSS_stats.png",width=8,height=6)

#Identify % of sites that had 0 rd for one method but not the other
a<-(s.wide[ which(s.wide$SfM_AcuteDZ_prev ==0 & s.wide$Diver_AcuteDZ_prev>0) , ])
nrow(a)/104
b<-(s.wide[ which(s.wide$SfM_AcuteDZ_prev >0 & s.wide$Diver_AcuteDZ_prev==0) , ])
nrow(b)/104

#Chronic disease prevalence
s<-subset(site,GENUS_CODE=="SSSS")
s$ChronicDZ<-(s$ChronicDZ_prev/100)*s$AdColCount
s$test<-as.integer(as.character(s$ChronicDZ))
head(s)

s.wide<-subset(site.wide,GENUS_CODE=="SSSS")
s<-subset(site,GENUS_CODE=="SSSS");length(unique(s$SITE))
s$ChronicDZ<-(s$ChronicDZ_prev/100)*s$AdColCount
s$test<-as.integer(as.character(s$ChronicDZ))
head(s)

tmp <- s.wide[ which(s.wide$SfM_ChronicDZ_prev + s.wide$Diver_ChronicDZ_prev>0) , ]
sitelist<-unique(tmp$SITE)
s<-dplyr::filter(s, SITE %in% sitelist);length(unique(s$SITE))

#Can't transform recent dead with bionomial distrubtion, sqrt, log or power transformation even after removing sites that have 0 rd in both sfm and diver
s<-subset(site,GENUS_CODE=="SSSS")
wilcox.test(ChronicDZ_prev ~ METHOD, data=s) 
wilcox.test(ChronicDZ_prev ~ METHOD, data=subset(s,HAB_R1=="Aggregate Reef")) 
wilcox.test(ChronicDZ_prev ~ METHOD, data=subset(s,HAB_R1=="Patch Reef")) 
wilcox.test(ChronicDZ_prev ~ METHOD, data=subset(s,HAB_R1=="Pavement")) 
wilcox.test(ChronicDZ_prev ~ METHOD, data=subset(s,HAB_R1=="Rock & Boulder")) 
wilcox.test(ChronicDZ_prev ~ METHOD, data=subset(s,HAB_R1=="Rubble")) 

pval<-c(0.292,
        0.462,
        0.5963,
        0.2551,
        0.7221)
p.adjust(pval,"BH")

t<-subset(s,METHOD=="SfM")
corr <- cor.test(x=t$MAX_DEPTH_M, y=t$ChronicDZ_prev, method = 'spearman')
corr


p1<-Plot1to1_new(s.wide,"SfM_ChronicDZ_prev","Diver_ChronicDZ_prev","SfM Prevalence (%)","Diver Prevalence (%)");p1<-p1+ggtitle("Chronic Disease")
p2<-PlotMethod(site,"GENUS_CODE","ChronicDZ_prev","SSSS","Prevalence (%)",1.5,9.5,"NS")
p3<-PlotHabitat(site,"GENUS_CODE","ChronicDZ_prev","SSSS","Prevalence (%)",3,34,"Method x Habitat NS")
p4<-PlotDepth_NP(site,"GENUS_CODE","ChronicDZ_prev","SSSS","Prevalence (%)")

ChronicdzS<-grid.arrange(p1,p2,p3,p4,nrow=4,ncol=1)

#ggsave(plot<-ChronicdzS,file="T:/Benthic/Data/SfM/Method Comparision/Figures/ChronicDZSSSS_stats.png",width=8,height=6)

#Identify % of sites that had 0 rd for one method but not the other
a<-(s.wide[ which(s.wide$SfM_ChronicDZ_prev ==0 & s.wide$Diver_ChronicDZ_prev>0) , ])
nrow(a)/104
b<-(s.wide[ which(s.wide$SfM_ChronicDZ_prev >0 & s.wide$Diver_ChronicDZ_prev==0) , ])
nrow(b)/104


#Bleaching prevalence
s<-subset(site,GENUS_CODE=="SSSS")
s$BLE<-(s$BLE_prev/100)*s$AdColCount
s$test<-as.integer(as.character(s$BLE))
head(s)

s.wide<-subset(site.wide,GENUS_CODE=="SSSS")
s<-subset(site,GENUS_CODE=="SSSS");length(unique(s$SITE))
s$BLE<-(s$BLE_prev/100)*s$AdColCount
s$test<-as.integer(as.character(s$BLE))
head(s)

tmp <- s.wide[ which(s.wide$SfM_BLE_prev + s.wide$Diver_BLE_prev>0) , ]
sitelist<-unique(tmp$SITE)
s<-dplyr::filter(s, SITE %in% sitelist);length(unique(s$SITE))

#Can't transform recent dead with bionomial distrubtion, sqrt, log or power transformation even after removing sites that have 0 rd in both sfm and diver
s<-subset(site,GENUS_CODE=="SSSS")
wilcox.test(BLE_prev ~ METHOD, data=s) 
wilcox.test(BLE_prev ~ METHOD, data=subset(s,HAB_R1=="Aggregate Reef")) 
wilcox.test(BLE_prev ~ METHOD, data=subset(s,HAB_R1=="Patch Reef")) 
wilcox.test(BLE_prev ~ METHOD, data=subset(s,HAB_R1=="Pavement")) 
wilcox.test(BLE_prev ~ METHOD, data=subset(s,HAB_R1=="Rock & Boulder")) 
wilcox.test(BLE_prev ~ METHOD, data=subset(s,HAB_R1=="Rubble")) 


pval<-c(0.002771,
        0.6781,
        0.1804,
        0.06162,
        0.413)
p.adjust(pval,"BH")

t<-subset(s,METHOD=="Diver")
corr <- cor.test(x=t$MAX_DEPTH_M, y=t$BLE_prev, method = 'spearman')
corr

p1<-Plot1to1_new(s.wide,"SfM_BLE_prev","Diver_BLE_prev","SfM Prevalence (%)","Diver Prevalence (%)");p1<-p1+ggtitle("Bleaching")
p2<-PlotMethod(site,"GENUS_CODE","BLE_prev","SSSS"," Prevalence (%)",1.5,65,"Significant")
p2<-p2+geom_label(label="Significant", x=1.5,y=65,label.size = 0.35,color = "black", fill="#00BFC4")
p3<-PlotHabitat(site,"GENUS_CODE","BLE_prev","SSSS","Prevalence (%)",1,60,"*")
p3<-p3+geom_label(label="*", x=1,y=60,label.size = 0.35,color = "black", fill="#00BFC4")
p4<-PlotDepth_NP(site,"GENUS_CODE","BLE_prev","SSSS","Prevalence (%)")

BLES<-grid.arrange(p1,p2,p3,p4,nrow=4,ncol=1)

#ggsave(plot<-BLES,file="T:/Benthic/Data/SfM/Method Comparision/Figures/BLEPrevSSSS_stats.png",width=8,height=6)

allplots<-grid.arrange(acutedzS,ChronicdzS,BLES,nrow=1,ncol=3)
ggsave(plot<-allplots,file="T:/Benthic/Data/SfM/Method Comparision/Figures/ConditionsALL_stats.png",width=8,height=10)




#Identify % of sites that had 0 rd for one method but not the other
a<-(s.wide[ which(s.wide$SfM_BLE_prev ==0 & s.wide$Diver_BLE_prev>0) , ])
nrow(a)/104
b<-(s.wide[ which(s.wide$SfM_BLE_prev >0 & s.wide$Diver_BLE_prev==0) , ])
nrow(b)/104


###Adult density for dominant taxa
s<-subset(site,GENUS_CODE=="POSP")
s.wide<-subset(site.wide,GENUS_CODE=="POSP")

hist(log(s$AdColDen))
s$logAdColDen<-log(s$AdColDen+1)
m<-lmer(logAdColDen~METHOD + (1|SEC_NAME),data=s)
DPlots<-function(m,s){
  par(mfrow=c(2,2)) # make the subplots
  qqnorm(resid(m))
  E2<-resid(m, type = "response") # extract normalized residuals
  F2<-fitted(m) # extract the fitted data
  plot(F2, E2, xlab = "fitted values", ylab = "residuals") # plot the relationship
  abline(h = 0, lty = 2) # add a flat line at zerp
  # test for homogeneity of variances
  boxplot(E2~s$SEC_NAME, ylab = "residuals")
  # check for independence. There should be no pattern
  plot(E2~s$METHOD, ylab = 'residuals', xlab = "METHOD")
}
DPlots(m,s)

mod1<-lmer(logAdColDen~1 + (1|SEC_NAME),data=s)
mod2<-lmer(logAdColDen~METHOD + (1|SEC_NAME),data=s)

anova(mod1,mod2,test="chisq")

p1<-Plot1to1_new(s.wide,"SfM_AdColDen","Diver_AdColDen","SfM Adult Density","Diver Adult Density");p1<-p1+ggtitle("Porites")+theme(plot.title = element_text(face = "italic"))
p2<-PlotMethod(site,"GENUS_CODE","AdColDen","POSP","Adult Density",1.5,24,"NS")
POSPcolden<-grid.arrange(p1,p2,nrow=2,ncol=1)

ggsave(plot<-POSPcolden,file="T:/Benthic/Data/SfM/Method Comparision/Figures/AdColDenPOSP_stats.png",width=5,height=5)


##
s<-subset(site,GENUS_CODE=="MOSP")
s.wide<-subset(site.wide,GENUS_CODE=="MOSP")
# tmp <- s.wide[ which(s.wide$SfM_AdColDen + s.wide$Diver_AdColDen>0) , ]
# sitelist<-unique(tmp$SITE)
# s<-dplyr::filter(s, SITE %in% sitelist);length(unique(s$SITE))

library("TeachingDemos")
library(MASS)
colden<-s$AdColDen + 1
boxcox(colden~s$METHOD)
boxcox(colden~s$METHOD, lambda=seq(-1,1))

dp<-bct(colden,-0.6)


hist(log(s$AdColDen))
s$logAdColDen<-log(s$AdColDen+1)
m<-lmer(logAdColDen~METHOD + (1|SEC_NAME),data=s)
m<-lmer(dp~METHOD + (1|SEC_NAME),data=s)

DPlots(m,s)
shapiro.test(dp)

wilcox.test(AdColDen ~ METHOD, data=s) 


p1<-Plot1to1_new(s.wide,"SfM_AdColDen","Diver_AdColDen","SfM Adult Density","Diver Adult Density");p1<-p1+ggtitle("Montipora")+theme(plot.title = element_text(face = "italic"))
p2<-PlotMethod(site,"GENUS_CODE","AdColDen","MOSP","Adult Density",1.5,17,"NS")
MOSPcolden<-grid.arrange(p1,p2,nrow=2,ncol=1)

ggsave(plot<-MOSPcolden,file="T:/Benthic/Data/SfM/Method Comparision/Figures/AdColDenMOSP_stats.png",width=5,height=5)


##
s<-subset(site,GENUS_CODE=="POCS")
s.wide<-subset(site.wide,GENUS_CODE=="POCS")
# tmp <- s.wide[ which(s.wide$SfM_AdColDen + s.wide$Diver_AdColDen>0) , ]
# sitelist<-unique(tmp$SITE)
# s<-dplyr::filter(s, SITE %in% sitelist);length(unique(s$SITE))

library("TeachingDemos")
library(MASS)
colden<-s$AdColDen + 1
boxcox(colden~s$METHOD)
boxcox(colden~s$METHOD, lambda=seq(-1,1))

dp<-bct(colden,-0.6)


hist(sqrt(s$AdColDen))
s$sqAdColDen<-sqrt(s$AdColDen)
m<-lmer(sqAdColDen~METHOD + (1|SEC_NAME),data=s)
m<-lmer(dp~METHOD + (1|SEC_NAME),data=s)

DPlots<-function(m,s){
  par(mfrow=c(2,2)) # make the subplots
  qqnorm(resid(m))
  E2<-resid(m, type = "response") # extract normalized residuals
  F2<-fitted(m) # extract the fitted data
  plot(F2, E2, xlab = "fitted values", ylab = "residuals") # plot the relationship
  abline(h = 0, lty = 2) # add a flat line at zerp
  # test for homogeneity of variances
  boxplot(E2~s$SEC_NAME, ylab = "residuals")
  # check for independence. There should be no pattern
  plot(E2~s$METHOD, ylab = 'residuals', xlab = "METHOD")
}
DPlots(m,s)

wilcox.test(AdColDen ~ METHOD, data=s) 


p1<-Plot1to1_new(s.wide,"SfM_AdColDen","Diver_AdColDen","SfM Adult Density","Diver Adult Density");p1<-p1+ggtitle("Pocillopora")+theme(plot.title = element_text(face = "italic"))
p2<-PlotMethod(site,"GENUS_CODE","AdColDen","POCS","Adult Density",1.5,10,"NS")
POCScolden<-grid.arrange(p1,p2,nrow=2,ncol=1)

ggsave(plot<-POCScolden,file="T:/Benthic/Data/SfM/Method Comparision/Figures/AdColDenPOCS_stats.png",width=5,height=5)


allplots<-grid.arrange(POSPcolden,MOSPcolden,POCScolden,nrow=1,ncol=3)
ggsave(plot<-allplots,file="T:/Benthic/Data/SfM/Method Comparision/Figures/AdColDenDomtaxa_stats.png",width=10,height=8)



###Juvenile density for dominant taxa
s<-subset(site,GENUS_CODE=="POSP")
s.wide<-subset(site.wide,GENUS_CODE=="POSP")

hist(log(s$JuvColDen))
s$logJuvColDen<-log(s$JuvColDen+1)
m<-lmer(logJuvColDen~METHOD + (1|SEC_NAME),data=s)
DPlots<-function(m,s){
  par(mfrow=c(2,2)) # make the subplots
  qqnorm(resid(m))
  E2<-resid(m, type = "response") # extract normalized residuals
  F2<-fitted(m) # extract the fitted data
  plot(F2, E2, xlab = "fitted values", ylab = "residuals") # plot the relationship
  abline(h = 0, lty = 2) # add a flat line at zerp
  # test for homogeneity of variances
  boxplot(E2~s$SEC_NAME, ylab = "residuals")
  # check for independence. There should be no pattern
  plot(E2~s$METHOD, ylab = 'residuals', xlab = "METHOD")
}
DPlots(m,s)

mod1<-lmer(logJuvColDen~1 + (1|SEC_NAME),data=s)
mod2<-lmer(logJuvColDen~METHOD + (1|SEC_NAME),data=s)

anova(mod1,mod2,test="chisq")

p1<-Plot1to1_new(s.wide,"SfM_JuvColDen","Diver_JuvColDen","SfM Juvenile Density","Diver Juvenile Density");p1<-p1+ggtitle("Porites")+theme(plot.title = element_text(face = "italic"))
p2<-PlotMethod(site,"GENUS_CODE","JuvColDen","POSP","Juvenile Density",1.5,30,"Significant")
p2<-p2+geom_label(label="Significant", x=1.5,y=30,label.size = 0.35,color = "black", fill="#00BFC4")

POSPcolden<-grid.arrange(p1,p2,nrow=2,ncol=1)

ggsave(plot<-POSPcolden,file="T:/Benthic/Data/SfM/Method Comparision/Figures/JuvColDenPOSP_stats.png",width=5,height=5)


##
s<-subset(site,GENUS_CODE=="MOSP")
s.wide<-subset(site.wide,GENUS_CODE=="MOSP")
tmp <- s.wide[ which(s.wide$SfM_JuvColDen + s.wide$Diver_JuvColDen>0) , ]
sitelist<-unique(tmp$SITE)
s<-dplyr::filter(s, SITE %in% sitelist);length(unique(s$SITE))

library("TeachingDemos")
library(MASS)
colden<-s$JuvColDen + 1
boxcox(colden~s$METHOD)
boxcox(colden~s$METHOD, lambda=seq(-1,1))

dp<-bct(colden,-0.6)


hist(log(s$JuvColDen))
s$logJuvColDen<-log(s$JuvColDen+1)
m<-lmer(logJuvColDen~METHOD + (1|SEC_NAME),data=s)
#m<-lmer(dp~METHOD + (1|SEC_NAME),data=s)

DPlots(m,s)
shapiro.test(dp)

wilcox.test(JuvColDen ~ METHOD, data=s) 


p1<-Plot1to1_new(s.wide,"SfM_JuvColDen","Diver_JuvColDen","SfM Juvenile Density","Diver Juvenile Density");p1<-p1+ggtitle("Montipora")+theme(plot.title = element_text(face = "italic"))
p2<-PlotMethod(site,"GENUS_CODE","JuvColDen","MOSP","Juvenile Density",1.5,22,"NS")
MOSPcolden<-grid.arrange(p1,p2,nrow=2,ncol=1)

ggsave(plot<-MOSPcolden,file="T:/Benthic/Data/SfM/Method Comparision/Figures/JuvColDenMOSP_stats.png",width=5,height=5)


##
s<-subset(site,GENUS_CODE=="POCS")
s.wide<-subset(site.wide,GENUS_CODE=="POCS")
# tmp <- s.wide[ which(s.wide$SfM_JuvColDen + s.wide$Diver_JuvColDen>0) , ]
# sitelist<-unique(tmp$SITE)
# s<-dplyr::filter(s, SITE %in% sitelist);length(unique(s$SITE))

library("TeachingDemos")
library(MASS)
colden<-s$JuvColDen + 1
boxcox(colden~s$METHOD)
boxcox(colden~s$METHOD, lambda=seq(-1,1))

dp<-bct(colden,-0.6)


hist(sqrt(s$JuvColDen))
s$sqJuvColDen<-sqrt(s$JuvColDen)
m<-lmer(sqJuvColDen~METHOD + (1|SEC_NAME),data=s)
m<-lmer(dp~METHOD + (1|SEC_NAME),data=s)

DPlots(m,s)

wilcox.test(JuvColDen ~ METHOD, data=s) 


p1<-Plot1to1_new(s.wide,"SfM_JuvColDen","Diver_JuvColDen","SfM Juvenile Density","Diver Juvenile Density");p1<-p1+ggtitle("Pocillopora")+theme(plot.title = element_text(face = "italic"))
p2<-PlotMethod(site,"GENUS_CODE","JuvColDen","POCS","Juvenile Density",1.5,13,"NS")
POCScolden<-grid.arrange(p1,p2,nrow=2,ncol=1)

ggsave(plot<-POCScolden,file="T:/Benthic/Data/SfM/Method Comparision/Figures/JuvColDenPOCS_stats.png",width=5,height=5)


allplots<-grid.arrange(POSPcolden,MOSPcolden,POCScolden,nrow=1,ncol=3)
ggsave(plot<-allplots,file="T:/Benthic/Data/SfM/Method Comparision/Figures/JuvColDenDomtaxa_stats.png",width=10,height=8)





