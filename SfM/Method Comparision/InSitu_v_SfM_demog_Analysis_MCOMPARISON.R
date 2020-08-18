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
# seg<-dplyr::select(seg, c(METHOD,SITE,SITEVISITID,SEGMENT,GENUS_CODE,ANALYST,SEGAREA_ad,SEGAREA_j,AdColCount,AdColDen,JuvColDen,Ave.size,Ave.od,Ave.rd,
#                                     BLE_prev,AcuteDZ_prev,ChronicDZ_prev,ISLAND,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,HABITAT_CODE,
#                                     MIN_DEPTH_M,MAX_DEPTH_M))
# 
# seg<-subset(seg,SITE!="HAW-04285"&SEGMENT!="5") #sckewed by 1 very very large colony
# sfm_seg<-subset(seg,METHOD=="SfM")
# diver_seg<-subset(seg,METHOD=="Diver")

#Set up in wide format
# colnames(sfm_seg)[9:17] <- paste("SfM_", colnames(sfm_seg[,c(9:17)]), sep = "");sfm_seg<-dplyr::select(sfm_seg,-c(METHOD,HABITAT_CODE))
# 
# colnames(diver_seg)[9:17] <- paste("Diver_", colnames(diver_seg[,c(9:17)]), sep = "");diver_seg<-dplyr::select(diver_seg,-c(METHOD,ANALYST,ISLAND,HABITAT_CODE,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,MIN_DEPTH_M,MAX_DEPTH_M))
# 
# seg.wide<-merge(sfm_seg,diver_seg,by=c("SITE","SITEVISITID","SEGMENT","GENUS_CODE","SEGAREA_ad","SEGAREA_j"),all=T)

# #################IMPORTANT- CHECK FOR MERGING ERRORS
# View(subset(seg.wide,GENUS_CODE=="SSSS")) #Make sure columns merge properly
# 
# #Merge together and remove segs that aren't merging properly
# seg.wide<-merge(sfm_seg,diver_seg,by=c("SITE","SITEVISITID","SEGMENT","GENUS_CODE","SEGAREA_ad","SEGAREA_j"))
# View(seg.wide) ##NOTE- LAN-01819 seg 10 SfM is duplicated for some reason-hopefully this will be fixed with final data
# 
# # #Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
# seg.wide$Diver_JuvColDen[is.na(seg.wide$Diver_JuvColDen)]<-0
# seg.wide$Diver_AdColDen[is.na(seg.wide$Diver_AdColDen)]<-0
# seg.wide$SfM_JuvColDen[is.na(seg.wide$SfM_JuvColDen)]<-0
# seg.wide$SfM_AdColDen[is.na(seg.wide$SfM_AdColDen)]<-0
# 
# head(seg.wide)
# 
# head(subset(seg.wide,GENUS_CODE=="SSSS"))
# 
# 
# 
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
# glmerDensity<-function(d,grouping_field="GENUS_CODE",metric_field="AdColDen"){
#   d$GROUP<-d[,grouping_field]
#   d$METRIC<-d[,metric_field]
#   
#   s<-subset(d,GROUP=="SSSS")
#   nullmod<-glmer(METRIC~1 + (1|SEC_NAME/SITE)+ offset(SEGAREA_ad),family=poisson,data=s)
#   mod1<-glmer(METRIC~METHOD + (1|SEC_NAME/SITE)+ offset(SEGAREA_ad),family=poisson,data=s)
#   mod2<-glmer(METRIC~METHOD + ANALYST+ (1|SEC_NAME/SITE)+ offset(SEGAREA_ad),family=poisson,data=s)
#   mod3<-glmer(METRIC~ANALYST+ (1|SEC_NAME/SITE)+ offset(SEGAREA_ad),family=poisson,data=s)
#   mod4<-glmer(METRIC~METHOD*MAX_DEPTH_M + (1|SEC_NAME/SITE)+ offset(SEGAREA_ad),family=poisson,data=s)
#   mod5<-glmer(METRIC~METHOD*HAB_R1 + (1|SEC_NAME/SITE)+ offset(SEGAREA_ad),family=poisson,data=s)
#   
#   }
# 
# 
# s<-subset(seg,GENUS_CODE=="SSSS")
# nullmod<-glmer(AdColCount~1 + (1|SEC_NAME/SITE),family=poisson,data=s)
# mod1<-glmer(AdColCount~METHOD + (1|SEC_NAME/SITE),family=poisson,data=s)
# mod2<-glmer(AdColCount~METHOD*ANALYST+ (1|SEC_NAME/SITE),family=poisson,data=s,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
# mod3<-glmer(AdColCount~METHOD*MAX_DEPTH_M + (1|SEC_NAME/SITE),family=poisson,data=s)
# mod4<-glmer(AdColCount~METHOD*HAB_R1 + (1|SEC_NAME/SITE),family=poisson,data=s)
# 
# 
# 
# #s<-s %>% drop_na(SEGAREA_j)
# nullmod<-glmer(JuvColDen~1 + (1|SEC_NAME/SITE),family=poisson,data=s)
# mod1<-glmer(JuvColDen~METHOD + (1|SEC_NAME/SITE),family=poisson,data=s)
# mod2<-glmer(JuvColDen~METHOD+ANALYST+ (1|SEC_NAME/SITE),family=poisson,data=s,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
# mod3<-glmer(JuvColDen~METHOD*MAX_DEPTH_M + (1|SEC_NAME/SITE),family=poisson,data=s,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
# #mod4<-glmer(JuvColDen~METHOD*HAB_R1 + (1|SEC_NAME/SITE),family=poisson,data=s,control=glmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=200000)))
# #Can't use Habitat for juveniles- model convergence that can't be addressed
# #Also having issues with model sigularity -need to simplify random effects (drop SEC_NAME)
# 
# 
# anova(mod1,nullmod,test="chisq")
# anova(mod2,mod1,test="chisq")
# anova(mod2,mod3,test="chisq")
# anova(mod1,mod4,test="chisq")
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
# Cand.set <- list( )
# Cand.set[[1]]<-glmer(METRIC~1 + (1|ISLAND/SITE),family=poisson,data=s)
# Cand.set[[2]]<-glmer(METRIC~METHOD + (1|ISLAND/SITE),family=poisson,data=s)
# Cand.set[[3]]<-glmer(METRIC~METHOD+ANALYST+ (1|ISLAND/SITE),family=poisson,data=s,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
# Cand.set[[4]]<-glmer(METRIC~METHOD*MAX_DEPTH_M + (1|ISLAND/SITE),family=poisson,data=s,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
# Cand.set[[5]]<-glmer(METRIC~METHOD*HAB_R1 + (1|ISLAND/SITE),family=poisson,data=s,control=glmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=200000)))
# 
# Modnames <- c("Null","Method","Method + Analyst","Method + Depth","Method + Habitat")
# ##generate AICc table
# aictab(cand.set = Cand.set, modnames = Modnames, sort = TRUE)
# ##round to 4 digits after decimal point and give log-likelihood
# print(aictab(cand.set = Cand.set, modnames = Modnames, sort = TRUE),
#       digits = 3, LL = TRUE)
# 
# a<-r.squaredGLMM(Cand.set[[1]])
# b<-r.squaredGLMM(Cand.set[[2]])
# c<-r.squaredGLMM(Cand.set[[3]])
# d<-r.squaredGLMM(Cand.set[[4]])
# e<-r.squaredGLMM(Cand.set[[5]])
# 
# print(a);print(b);print(c);print(d);print(e)
# 
# }
# 
# #Adults
# glmerDensity(seg,"GENUS_CODE","SSSS","AdColCount")
# glmerDensity(seg,"GENUS_CODE","POSP","AdColCount")
# glmerDensity(seg,"GENUS_CODE","MOSP","AdColCount")
# glmerDensity(seg,"GENUS_CODE","POCS","AdColCount")
# 
# glmerDensity(seg,"GENUS_CODE","SSSS","JuvColDen")
# glmerDensity(seg,"GENUS_CODE","POSP","JuvColDen")
# glmerDensity(seg,"GENUS_CODE","MOSP","JuvColDen")
# glmerDensity(seg,"GENUS_CODE","POCS","JuvColDen")
# 
# #Ave Size- not perfect transformation, but will work
# s<-subset(seg,GENUS_CODE=="SSSS")
# hist(log(s$Ave.od))
# s$logAve.size<-log(s$Ave.size)
# mod<-lmer(logAve.size~METHOD + (1|SEC_NAME/SITE),data=s)
# plot(mod)
# qqnorm(resid(mod)) #plot normal quantile- quantile plot.  Should be close to a straight line
# 
# #Old dead- not perfect transformation, but will work
# s<-subset(seg,GENUS_CODE=="SSSS")
# hist(sqrt(s$Ave.od))
# s$sqAve.od<-sqrt(s$Ave.od)
# mod<-lmer(sqAve.od~METHOD + (1|SEC_NAME/SITE),data=s)
# plot(mod)
# qqnorm(resid(mod)) #plot normal quantile- quantile plot.  Should be close to a straight line
# 
# #Can't transform recent dead
# hist(log(s$Ave.rd+1))
# s$logAve.rd<-log(s$Ave.rd+1)
# mod<-lmer(logAve.rd~METHOD + (1|SEC_NAME/SITE),data=s)
# plot(mod)
# qqnorm(resid(mod)) #plot normal quantile- quantile plot.  Should be close to a straight line
# 
# #Transform variables
# s<-subset(seg,GENUS_CODE=="SSSS")
# s$logAve.size<-log(s$Ave.size)
# s$sqrtAve.od<-sqrt(s$Ave.od)
# 
# 
# lmerMetric<-function(d,grouping_field="GENUS_CODE",genus_field="SSSS",metric_field="AdColCount"){
#   d$GROUP<-d[,grouping_field]
#   d$METRIC<-d[,metric_field]
#   s<-subset(d,GROUP==genus_field)
#   
#   Cand.set <- list( )
#   Cand.set[[1]]<-lmer(METRIC~1 + (1|SEC_NAME/SITE),data=s,REML=F)
#   Cand.set[[2]]<-lmer(METRIC~METHOD + (1|SEC_NAME/SITE),data=s,REML=F)
#   Cand.set[[3]]<-lmer(METRIC~METHOD+ANALYST+ (1|SEC_NAME/SITE),data=s,REML=F,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
#   Cand.set[[4]]<-lmer(METRIC~METHOD*MAX_DEPTH_M + (1|SEC_NAME/SITE),REML=F,data=s,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
#   Cand.set[[5]]<-lmer(METRIC~METHOD*HAB_R1 + (1|SEC_NAME/SITE),data=s,REML=F,control=lmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=200000)))
#   
#   Modnames <- c("Null","Method","Method + Analyst","Method + Depth","Method + Habitat")
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
# lmerMetric(s,"GENUS_CODE","SSSS","logAve.size")
# lmerMetric(s,"GENUS_CODE","SSSS","sqrtAve.od")
# 
# mod<-lmer(logAve.size~METHOD*HAB_R1 + (1|SEC_NAME/SITE),data=s,REML=F,control=lmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=200000)))
# summary(mod)
# 
# #Prevalence Data
# #Calculate # of cases/type- need this for the the binomial GLMMs 
# s<-subset(seg,GENUS_CODE=="SSSS")
# s$AcuteDZ<-(s$AcuteDZ_prev/100)*s$AdColCount
# s$ChronicDZ<-(s$ChronicDZ_prev/100)*s$AdColCount
# s$BLE<-(s$BLE_prev/100)*s$AdColCount
# head(s)
# 
# glmerPrev<-function(d,grouping_field="GENUS_CODE",genus_field="SSSS",metric_field="AcuteDZ"){
#   d$GROUP<-d[,grouping_field]
#   d$METRIC<-d[,metric_field]
#   s<-subset(d,GROUP==genus_field)
#   
#   Cand.set <- list( )
#   Cand.set[[1]]<-glmer(cbind(METRIC~1 + (1|ISLAND/SITE),family=binomial,data=s)
#   Cand.set[[2]]<-glmer(METRIC~METHOD + (1|ISLAND/SITE),family=binomial,data=s)
#   Cand.set[[3]]<-glmer(METRIC~METHOD+ANALYST+ (1|ISLAND/SITE),family=binomial,data=s,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
#   Cand.set[[4]]<-glmer(METRIC~METHOD*MAX_DEPTH_M + (1|ISLAND/SITE),family=binomial,data=s,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
#   Cand.set[[5]]<-glmer(METRIC~METHOD*HAB_R1 + (1|ISLAND/SITE),family=binomial,data=s,control=glmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=200000)))
#   
#   Modnames <- c("Null","Method","Method + Analyst","Method + Depth","Method + Habitat")
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
# glmerDensity(seg,"GENUS_CODE","SSSS","AdColCount")
# glmerDensity(seg,"GENUS_CODE","POSP","AdColCount")
# glmerDensity(seg,"GENUS_CODE","MOSP","AdColCount")
# glmerDensity(seg,"GENUS_CODE","POCS","AdColCount")


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

glmerDensity<-function(d,grouping_field="GENUS_CODE",metric_field="AdColDen"){
  d$GROUP<-d[,grouping_field]
  d$METRIC<-d[,metric_field]
  
  s<-subset(d,GROUP=="SSSS")
  nullmod<-glmer(METRIC~1 + (1|SEC_NAME)+ offset(siteAREA_ad),family=poisson,data=s)
  mod1<-glmer(METRIC~METHOD + (1|SEC_NAME)+ offset(siteAREA_ad),family=poisson,data=s)
  mod4<-glmer(METRIC~METHOD*MAX_DEPTH_M + (1|SEC_NAME)+ offset(siteAREA_ad),family=poisson,data=s)
  mod5<-glmer(METRIC~METHOD*HAB_R1 + (1|SEC_NAME)+ offset(siteAREA_ad),family=poisson,data=s)
  
}


tab_model(mod2)

#Model Selection

glmerDensity<-function(d,grouping_field="GENUS_CODE",genus_field="SSSS",metric_field="AdColCount"){
  d$GROUP<-d[,grouping_field]
  d$METRIC<-d[,metric_field]
  s<-subset(d,GROUP==genus_field)
  
  Cand.set <- list( )
  Cand.set[[1]]<-glmer(METRIC~1 + (1|ISLAND/SEC_NAME),family=poisson,data=s)
  Cand.set[[2]]<-glmer(METRIC~METHOD + (1|ISLAND/SEC_NAME),family=poisson,data=s)
  Cand.set[[4]]<-glmer(METRIC~METHOD*MAX_DEPTH_M + (1|ISLAND/SEC_NAME),family=poisson,data=s,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
  Cand.set[[5]]<-glmer(METRIC~METHOD*HAB_R1 + (1|ISLAND/SEC_NAME),family=poisson,data=s,control=glmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=200000)))
  
  Modnames <- c("Null","Method","Method + Depth","Method + Habitat")
  ##generate AICc table
  aictab(cand.set = Cand.set, modnames = Modnames, sort = TRUE)
  ##round to 4 digits after decimal point and give log-likelihood
  print(aictab(cand.set = Cand.set, modnames = Modnames, sort = TRUE),
        digits = 3, LL = TRUE)
  
  a<-r.squaredGLMM(Cand.set[[1]])
  b<-r.squaredGLMM(Cand.set[[2]])
  c<-r.squaredGLMM(Cand.set[[3]])
  d<-r.squaredGLMM(Cand.set[[4]])
  e<-r.squaredGLMM(Cand.set[[5]])
  
  print(a);print(b);print(c);print(d);print(e)
  
}

#Adults
glmerDensity(site,"GENUS_CODE","SSSS","AdColCount")
glmerDensity(site,"GENUS_CODE","POSP","AdColCount")
glmerDensity(site,"GENUS_CODE","MOSP","AdColCount")
glmerDensity(site,"GENUS_CODE","POCS","AdColCount")

glmerDensity(site,"GENUS_CODE","SSSS","JuvColDen")
glmerDensity(site,"GENUS_CODE","POSP","JuvColDen")
glmerDensity(site,"GENUS_CODE","MOSP","JuvColDen")
glmerDensity(site,"GENUS_CODE","POCS","JuvColDen")

#Adult Colony Density- sqrt transform
#also tried gamma and neg binomial- sqrt transform is best
s<-subset(site,GENUS_CODE=="SSSS")
hist(log(s$AdColDen))
s$sqAdColDen<-sqrt(s$AdColDen)
m<-lmer(sqAdColDen~METHOD + (1|SEC_NAME),data=s)

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

mod1<-lmer(sqAdColDen~1 + (1|SEC_NAME),data=s)
mod2<-lmer(sqAdColDen~METHOD + (1|SEC_NAME),data=s)
mod3<-lmer(sqAdColDen~MAX_DEPTH_M + (1|SEC_NAME),data=s)
mod4<-lmer(sqAdColDen~HAB_R1 + (1|SEC_NAME),data=s)
mod5<-lmer(sqAdColDen~METHOD*MAX_DEPTH_M + (1|SEC_NAME),data=s)
mod6<-lmer(sqAdColDen~METHOD*HAB_R1 + (1|SEC_NAME),data=s)
#mod7<-lmer(sqAdColDen~METHOD*HAB_R1*MAX_DEPTH_M + (1|SEC_NAME),data=s)

anova(mod6,mod5,test="Chisq")
anova(mod6,mod4,test="Chisq")
anova(mod4,mod3,test="Chisq")
anova(mod4,mod2,test="Chisq")
anova(mod2,mod1,test="Chisq")


#Nope
m<-glmmTMB(AdColCount~METHOD + (1|SEC_NAME),ziformula=~1,
           family=poisson,data=s)
DPlots(m,s)

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
p2<-PlotMethod(site,"GENUS_CODE","AdColDen","SSSS","Adult Density",1.5,29,"NS")
p3<-PlotHabitat(site,"GENUS_CODE","AdColDen","SSSS","Adult Density",3.5,29,"Method x Habitat NS")
p4<-PlotDepth(site,"GENUS_CODE","AdColDen","AdColDen","SSSS","Adult Density",18,29,"Method x Depth NS")

AdColDenS<-grid.arrange(p1,p2,p3,p4,nrow=1,ncol=4)

ggsave(plot<-AdColDenS,file="T:/Benthic/Data/SfM/Method Comparision/Figures/AdColDenSSSS_stats.png",width=10,height=5)

#Juvenile Colony Density- sqrt transform
#also tried gamma and neg binomial- sqrt transform is best
s<-subset(site,GENUS_CODE=="SSSS")
hist(sqrt(s$JuvColDen))
s$sqJuvColDen<-sqrt(s$JuvColDen)
m<-lmer(sqJuvColDen~METHOD + (1|SEC_NAME),data=s)

DPlots(m,s)

mod1<-lmer(sqJuvColDen~1 + (1|SEC_NAME),data=s)
mod2<-lmer(sqJuvColDen~METHOD + (1|SEC_NAME),data=s)
mod3<-lmer(sqJuvColDen~MAX_DEPTH_M + (1|SEC_NAME),data=s)
mod4<-lmer(sqJuvColDen~HAB_R1 + (1|SEC_NAME),data=s)
mod5<-lmer(sqJuvColDen~METHOD*MAX_DEPTH_M + (1|SEC_NAME),data=s)
mod6<-lmer(sqJuvColDen~METHOD*HAB_R1 + (1|SEC_NAME),data=s)
#mod7<-lmer(sqAdColDen~METHOD*HAB_R1*MAX_DEPTH_M + (1|SEC_NAME),data=s)

anova(mod6,mod5,test="Chisq")
anova(mod5,mod4,test="Chisq")
anova(mod5,mod3,test="Chisq")
anova(mod5,mod2,test="Chisq")
anova(mod2,mod1,test="Chisq")


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
p3<-PlotHabitat(site,"GENUS_CODE","JuvColDen","SSSS","Juvenile Density",3.5,60,"Method x Habitat NS")
p4<-PlotDepth(site,"GENUS_CODE","JuvColDen","JuvColDen","SSSS","Juvenile Density",18,60,"Significant Method x Depth")

JuvColDenS<-grid.arrange(p1,p2,p3,p4,nrow=1,ncol=4)

ggsave(plot<-JuvColDenS,file="T:/Benthic/Data/SfM/Method Comparision/Figures/JuvColDenSSSS_stats.png",width=10,height=5)




#Ave Size- not perfect transformation, but will work
s<-subset(site,GENUS_CODE=="SSSS")
hist(log(s$Ave.size))
s$logAve.size<-log(s$Ave.size)
m<-lmer(logAve.size~METHOD + (1|SEC_NAME),data=s)

DPlots(m,s)

mod1<-lmer(logAve.size~1 + (1|SEC_NAME),data=s)
mod2<-lmer(logAve.size~METHOD + (1|SEC_NAME),data=s)
mod3<-lmer(logAve.size~MAX_DEPTH_M + (1|SEC_NAME),data=s)
mod4<-lmer(logAve.size~HAB_R1 + (1|SEC_NAME),data=s)
mod5<-lmer(logAve.size~METHOD*MAX_DEPTH_M + (1|SEC_NAME),data=s)
mod6<-lmer(logAve.size~METHOD*HAB_R1 + (1|SEC_NAME),data=s)
#mod7<-lmer(sqAdColDen~METHOD*HAB_R1*MAX_DEPTH_M + (1|SEC_NAME),data=s)

anova(mod6,mod5,test="Chisq")
anova(mod6,mod4,test="Chisq")
anova(mod5,mod4,test="Chisq")
anova(mod4,mod3,test="Chisq")
anova(mod4,mod2,test="Chisq")



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
p2<-PlotMethod(site,"GENUS_CODE","Ave.size","SSSS","Average Max Diameter (cm)",1.5,35,"NS")
p3<-PlotHabitat(site,"GENUS_CODE","Ave.size","SSSS","Average Max Diameter (cm)",3.5,35,"Method x Habitat NS")
p4<-PlotDepth(site,"GENUS_CODE","Ave.size","Ave.size","SSSS","Average Max Diameter (cm)",18,35,"Method x Depth NS")

Ave.sizeS<-grid.arrange(p1,p2,p3,p4,nrow=1,ncol=4)

ggsave(plot<-Ave.sizeS,file="T:/Benthic/Data/SfM/Method Comparision/Figures/Ave.sizeSSSS_stats.png",width=10,height=5)





#Old dead- not perfect transformation, but will work
s<-subset(site,GENUS_CODE=="SSSS")
hist(sqrt(s$Ave.od))
s$sqAve.od<-sqrt(s$Ave.od)
mod<-lmer(sqAve.od~METHOD + (1|SEC_NAME),data=s)
DPlots(m,s)

mod1<-lmer(sqAve.od~1 + (1|SEC_NAME),data=s)
mod2<-lmer(sqAve.od~METHOD + (1|SEC_NAME),data=s)
mod3<-lmer(sqAve.od~MAX_DEPTH_M + (1|SEC_NAME),data=s)
mod4<-lmer(sqAve.od~HAB_R1 + (1|SEC_NAME),data=s)
mod5<-lmer(sqAve.od~METHOD*MAX_DEPTH_M + (1|SEC_NAME),data=s)
mod6<-lmer(sqAve.od~METHOD*HAB_R1 + (1|SEC_NAME),data=s)
#mod7<-lmer(sqAdColDen~METHOD*HAB_R1*MAX_DEPTH_M + (1|SEC_NAME),data=s)

anova(mod6,mod5,test="Chisq")
anova(mod6,mod4,test="Chisq")
anova(mod6,mod2,test="Chisq")
anova(mod5,mod4,test="Chisq")
anova(mod5,mod3,test="Chisq")
anova(mod5,mod2,test="Chisq")

#Rather than setting up complicated contrasts to test for dif between methods within habitats
#Run separate models for each habitat then run multiple test corrections
a<-summary(glht(lmer(sqAve.od~METHOD + (1|SEC_NAME),data=subset(s,HAB_R1=="Aggregate Reef")),linfct=mcp(METHOD="Tukey")))
b<-summary(glht(lmer(sqAve.od~METHOD + (1|SEC_NAME),data=subset(s,HAB_R1=="Pavement")),linfct=mcp(METHOD="Tukey")))
c<-summary(glht(lmer(sqAve.od~METHOD + (1|SEC_NAME),data=subset(s,HAB_R1=="Patch Reef")),linfct=mcp(METHOD="Tukey")))
d<-summary(glht(lmer(sqAve.od~METHOD + (1|SEC_NAME),data=subset(s,HAB_R1=="Rock & Boulder")),linfct=mcp(METHOD="Tukey")))
e<-summary(glht(lmer(sqAve.od~METHOD + (1|SEC_NAME),data=subset(s,HAB_R1=="Rubble")),linfct=mcp(METHOD="Tukey")))

#Extract values and adjust for multiple tests- 
pval<-c(a$test$pvalues,b$test$pvalues,c$test$pvalues,d$test$pvalues,e$test$pvalues)
p.adjust(pval,"BH")

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
p2<-PlotMethod(site,"GENUS_CODE","Ave.od","SSSS","Average % Old Dead",1.5,45,"NS")
p3<-PlotHabitat(site,"GENUS_CODE","Ave.od","SSSS","Average % Old Dead",3.5,45,"Method x Habitat NS")
p4<-PlotDepth(site,"GENUS_CODE","Ave.od","Ave.od","SSSS","Average % Old Dead",18,45,"Significant Method x Depth")

Ave.odS<-grid.arrange(p1,p2,p3,p4,nrow=1,ncol=4)

ggsave(plot<-Ave.odS,file="T:/Benthic/Data/SfM/Method Comparision/Figures/Ave.odSSSS_stats.png",width=10,height=5)


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

hist(log(s$Ave.rd+1))
hist(sqrt(s$Ave.rd))
m<-lmer(sqrt(s$Ave.rd)~METHOD + (1|SEC_NAME),data=s)

par(mfrow=c(2,2)) # make the subplots
  qqnorm(resid(m))
  E2<-resid(m, type = "response") # extract normalized residuals
  F2<-fitted(m) # extract the fitted data
  plot(F2, E2, xlab = "fitted values", ylab = "residuals") # plot the relationship
  abline(h = 0, lty = 2) # add a flat line at zerp
  
#Can't transform recent dead with sqrt, log or power transformation even after removing sites that have 0 rd in both sfm and diver
wilcox.test(Ave.rd ~ METHOD, data=s) 
  
p1<-Plot1to1_new(s.wide,"SfM_Ave.rd","Diver_Ave.rd","SfM Average % Recent Dead","Diver % Recent Dead")
p2<-PlotMethod(site,"GENUS_CODE","Ave.rd","SSSS","Average % Recent Dead",1.5,4,"NS")

Ave.rdS<-grid.arrange(p1,p2,nrow=1,ncol=2)

ggsave(plot<-Ave.rdS,file="T:/Benthic/Data/SfM/Method Comparision/Figures/Ave.rdSSSS_stats.png",width=5,height=5)

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

tmp <- s.wide[ which(s.wide$SfM_AcuteDZ_prev + s.wide$Diver_AcuteDZ_prev>0) , ]
sitelist<-unique(tmp$SITE)
s<-dplyr::filter(s, SITE %in% sitelist);length(unique(s$SITE))

m<-glmer(AcuteDZ/AdColCount~METHOD + (1|SEC_NAME),family = "binomial",data=s,na.action = na.omit)
par(mfrow=c(2,2)) # make the subplots
qqnorm(resid(m))
E2<-resid(m, type = "response") # extract normalized residuals
F2<-fitted(m) # extract the fitted data
plot(F2, E2, xlab = "fitted values", ylab = "residuals") # plot the relationship
abline(h = 0, lty = 2) # add a flat line at zerp

hist(sqrt(s$AcuteDZ_prev))
s$sqAcuteDZ_prev<-sqrt(s$AcuteDZ_prev)
m<-lmer(sqAcuteDZ_prev~METHOD + (1|SEC_NAME),data=s)

#Can't transform recent dead with bionomial distrubtion, sqrt, log or power transformation even after removing sites that have 0 rd in both sfm and diver
s<-subset(site,GENUS_CODE=="SSSS")
wilcox.test(AcuteDZ_prev ~ METHOD, data=s) 

p1<-Plot1to1_new(s.wide,"SfM_AcuteDZ_prev","Diver_AcuteDZ_prev","SfM Acute Disease Prevalence (%)","Diver Acute Disease Prevalence (%)")
p2<-PlotMethod(site,"GENUS_CODE","AcuteDZ_prev","SSSS","Acute Disease Prevalence (%)",1.5,15,"NS")

acutedzS<-grid.arrange(p1,p2,nrow=1,ncol=2)

ggsave(plot<-acutedzS,file="T:/Benthic/Data/SfM/Method Comparision/Figures/AcuteDZSSSS_stats.png",width=5,height=5)

#Identify % of sites that had 0 rd for one method but not the other
a<-(s.wide[ which(s.wide$SfM_AcuteDZ_prev ==0 & s.wide$Diver_AcuteDZ_prev>0) , ])
nrow(a)/104
b<-(s.wide[ which(s.wide$SfM_AcuteDZ_prev >0 & s.wide$Diver_AcuteDZ_prev==0) , ])
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

m<-glmer(BLE/AdColCount~METHOD + (1|SEC_NAME),family = "binomial",data=s,na.action = na.omit)
par(mfrow=c(2,2)) # make the subplots
qqnorm(resid(m))
E2<-resid(m, type = "response") # extract normalized residuals
F2<-fitted(m) # extract the fitted data
plot(F2, E2, xlab = "fitted values", ylab = "residuals") # plot the relationship
abline(h = 0, lty = 2) # add a flat line at zerp

hist(log(s$BLE_prev+1))
s$logBLE_prev<-log(s$BLE_prev+1)
m<-lmer(logBLE_prev~METHOD + (1|SEC_NAME),data=s)


#Can't transform recent dead with bionomial distrubtion, sqrt, log or power transformation even after removing sites that have 0 rd in both sfm and diver
s<-subset(site,GENUS_CODE=="SSSS")
wilcox.test(BLE_prev ~ METHOD, data=s) 

p1<-Plot1to1_new(s.wide,"SfM_BLE_prev","Diver_BLE_prev","SfM Bleaching Prevalence (%)","Diver Bleaching Prevalence (%)")
p2<-PlotMethod(site,"GENUS_CODE","BLE_prev","SSSS","Bleaching Prevalence (%)",1.5,60,"Significant")
BLES<-grid.arrange(p1,p2,nrow=1,ncol=2)

ggsave(plot<-BLES,file="T:/Benthic/Data/SfM/Method Comparision/Figures/BLEPrevSSSS_stats.png",width=5,height=5)

#Identify % of sites that had 0 rd for one method but not the other
a<-(s.wide[ which(s.wide$SfM_BLE_prev ==0 & s.wide$Diver_BLE_prev>0) , ])
nrow(a)/104
b<-(s.wide[ which(s.wide$SfM_BLE_prev >0 & s.wide$Diver_BLE_prev==0) , ])
nrow(b)/104


###
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
shapiro.test(dp)

wilcox.test(AdColDen ~ METHOD, data=s) 


p1<-Plot1to1_new(s.wide,"SfM_AdColDen","Diver_AdColDen","SfM Adult Density","Diver Adult Density");p1<-p1+ggtitle("Montipora")+theme(plot.title = element_text(face = "italic"))
p2<-PlotMethod(site,"GENUS_CODE","AdColDen","MOSP","Adult Density",1.5,22,"NS")
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
p2<-PlotMethod(site,"GENUS_CODE","AdColDen","POCS","Adult Density",1.5,3.6,"NS")
POCScolden<-grid.arrange(p1,p2,nrow=2,ncol=1)

ggsave(plot<-POCScolden,file="T:/Benthic/Data/SfM/Method Comparision/Figures/AdColDenPOCS_stats.png",width=5,height=5)


allplots<-grid.arrange(POSPcolden,MOSPcolden,POCScolden,nrow=1,ncol=3)
ggsave(plot<-allplots,file="T:/Benthic/Data/SfM/Method Comparision/Figures/AdColDenDomtaxa_stats.png",width=10,height=8)






