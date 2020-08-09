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


# Plotting Regressions and Bland-Altman by Taxon --------------------------
#PlotAll(dataframe, variable 1, variable 2, y-axis name 1, y-axis name 2, x-axis name 1, x-axis name 2)

outpath<- "T:/Benthic/Data/SfM/Method Comparision/Figures/Site/Adult Density"
if(!dir.exists(outpath)){dir.create(outpath)}
p1<-PlotAll(site.wide,"Diver_AdColDen","SfM_AdColDen","SfM Adult Density","Difference SfM Analyst and Diver", "Diver Adult Density","Mean Adult Density")
p1

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Site/Juvenile Density"
if(!dir.exists(outpath)){dir.create(outpath)}
p4<-PlotAll(site.wide,"Diver_JuvColDen","SfM_JuvColDen","SfM Juvenile Density","Difference SfM Analyst and Diver", "Diver Juvenile Density","Mean Juvenile Density")
p4

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Site/Colony Size"
if(!dir.exists(outpath)){dir.create(outpath)}
p7<-PlotAll(site.wide,"Diver_Ave.size","SfM_Ave.size","SfM Colony Length","Difference SfM Analyst and Diver", "Diver Colony Length","Mean Colony Length")
p7

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Site/Old Dead"
if(!dir.exists(outpath)){dir.create(outpath)}
p10<-PlotAll(site.wide,"Diver_Ave.od","SfM_Ave.od","SfM Average Old Dead Pct","Difference SfM Analyst and Diver", "Diver Average Old Dead Pct","Average Old Dead Pct")
p10

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Site/Recent Dead"
if(!dir.exists(outpath)){dir.create(outpath)}
p13<-PlotAll(site.wide,"Diver_Ave.rd","SfM_Ave.rd","SfM Average Recent Dead Pct","Difference SfM Analyst and Diver", "Diver Average Recent Dead Pct","Average Recent Dead Pct")
p13

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Site/Bleaching"
if(!dir.exists(outpath)){dir.create(outpath)}
p16<-PlotAll(site.wide,"Diver_BLE_prev","SfM_BLE_prev","SfM Bleaching Prevalence","Difference SfM Analyst and Diver", "Diver Bleaching Prevalence","Mean Bleaching Prevalence")
p16

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Site/ChronicDZ"
if(!dir.exists(outpath)){dir.create(outpath)}
p19<-PlotAll(site.wide,"Diver_ChronicDZ_prev","SfM_ChronicDZ_prev","SfM Chronic Disease Prevalence","Difference SfM Analyst and Diver", "Diver Chronic Disease Prevalence","Mean Chronic Disease Prevalence")
p19

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Site/AcuteDZ"
if(!dir.exists(outpath)){dir.create(outpath)}
p22<-PlotAll(site.wide,"Diver_AcuteDZ_prev","SfM_AcuteDZ_prev","SfM General Disease Prevalence","Difference SfM Analyst and Diver", "Diver General Disease Prevalence","Mean General Disease Prevalence")
p22


#Mixed Models
table(site$HABITAT_CODE)

#Simplify Habitat codes
site<-site %>% mutate(HAB_R1=recode(HABITAT_CODE, 
                                    `AGR`="Carbonate Reef",
                                    `APR`="Carbonate Reef",
                                    `PAV`="Pavement",
                                    `PPR`="Pavement",
                                    `RRB`="Rubble",
                                    `ROB`="Rock & Boulder",
                                    `SCR`="Reef with Sand",
                                    `PSC`="Reef with Sand"))


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

#Nope
m<-glmmTMB(AdColCount~METHOD + (1|SEC_NAME),ziformula=~1,
           family=poisson,data=s)
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

s.wide<-subset(site.wide,GENUS_CODE=="SSSS")
Plot1to1_new<-function(d,response_variable,predictor_variable){
  #sub<-d[d$taxon,]
  d$X<-d[,response_variable]
  d$Y<-d[,predictor_variable]
  mx_val<-max(d$Y, d$X, na.rm = TRUE)
  
  corr<-cor.test(d$X, d$Y, method="pearson")
  rmse<-rmse(d$Y, d$X,na.rm=TRUE)
  r_text<-paste("RMSE = ", round(rmse,digits = 2),"\n r = ", round((corr$estimate),2), sep="")
  
  p1<-ggplot(d, aes(x=X, y=Y)) + 
    geom_point(size=1) + 	geom_abline(slope=1, intercept=0) +
    geom_smooth(method="lm", color="red", linetype="dashed", se=F) +
    geom_text(aes((mx_val/5), (mx_val * 0.9), label=r_text), nudge_y=-0.1, nudge_x=0.1,size=4, color="red") +
    theme_bw()+
    scale_x_continuous(limits=c(0,mx_val)) +
    scale_y_continuous(limits=c(0,mx_val)) +
    xlab(response_variable) +  ylab(predictor_variable)     
  return(p1)
} # 


p1<-Plot1to1_new(s.wide,"Diver_AdColDen","SfM_AdColDen")
p1

p2<-ggplot(subset(site,GENUS_CODE=="SSSS"), aes(x=METHOD, y=AdColDen, fill=METHOD)) + 
  geom_boxplot() +
  geom_label(label="NS", x=1.5,y=28,label.size = 0.35,color = "black", fill="white")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="bottom")+
  labs(x="Method",y="Mean Adult Density/m2")

p2


p3<-ggplot(subset(site,GENUS_CODE=="SSSS"), aes(x=HAB_R1, y=AdColDen, fill=METHOD)) + 
  geom_boxplot() +
  # guides(fill=FALSE) 
  theme_bw() +
  geom_label(label="NS between methods and habitats", x=4,y=28,label.size = 0.35,color = "black", fill="white")+
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none")+
  labs(x="Habitat Type",y="Mean Adult Density/m2")

p3

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


p4<-ggplot(s, aes(x = MAX_DEPTH_M, y = AdColDen, color = METHOD) ) +
  geom_point(aes(colour = factor(METHOD))) +
  geom_line(data = newdat.lme, aes(y = AdColDen), size = 1)+
  geom_ribbon(data = newdat.lme, aes(y = NULL, ymin = lower, ymax = upper, 
                                     color = NULL, fill = METHOD),alpha = .15)+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none")+
  labs(x="Max Depth (m)",y="Adult Density/m2")
p4

AdColDenS<-grid.arrange(p1,p2,p3,p4,nrow=1,ncol=4)

ggsave(plot<-AdColDenS,file="T:/Benthic/Data/SfM/Method Comparision/Figures/AdColDenSSSS_stats.pdf",width=10,height=5)


#Ave Size- not perfect transformation, but will work
s<-subset(site,GENUS_CODE=="SSSS")
hist(log(s$Ave.od))
s$logAve.size<-log(s$Ave.size)
mod<-lmer(logAve.size~METHOD + (1|ISLAND/SEC_NAME),data=s)
plot(mod)
qqnorm(resid(mod)) #plot normal quantile- quantile plot.  Should be close to a straight line

#Old dead- not perfect transformation, but will work
s<-subset(site,GENUS_CODE=="SSSS")
hist(sqrt(s$Ave.od))
s$sqAve.od<-sqrt(s$Ave.od)
mod<-lmer(sqAve.od~METHOD + (1|ISLAND/SEC_NAME),data=s)
plot(mod)
qqnorm(resid(mod)) #plot normal quantile- quantile plot.  Should be close to a straight line

#Can't transform recent dead
hist(log(s$Ave.rd+1))
s$logAve.rd<-log(s$Ave.rd+1)
mod<-lmer(logAve.rd~METHOD + (1|ISLAND/SEC_NAME),data=s)
plot(mod)
qqnorm(resid(mod)) #plot normal quantile- quantile plot.  Should be close to a straight line

#Transform variables
s<-subset(site,GENUS_CODE=="SSSS")
s$logAve.size<-log(s$Ave.size)
s$sqrtAve.od<-sqrt(s$Ave.od)

library(bbmle)
bbmle::AICctab()

glmerMetric<-function(d,grouping_field="GENUS_CODE",genus_field="SSSS",metric_field="AdColDen"){
  d$GROUP<-d[,grouping_field]
  d$METRIC<-d[,metric_field]
  s<-subset(d,GROUP==genus_field)
  
  m1<-glmer((METRIC+1)~1 + (1|SEC_NAME),family = Gamma(link = log),data=s,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
  m2<-glmer((METRIC+1)~METHOD + (1|SEC_NAME),family=Gamma(link = log),data=s,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
  m3<-glmer((METRIC+1)~METHOD*MAX_DEPTH_M + (1|SEC_NAME),family=Gamma(link = log),data=s,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
  m4<-glmer((METRIC+1)~METHOD*HAB_R1 + (1|SEC_NAME),family=Gamma(link = log),data=s,control=glmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=200000)))
  
  Modnames <- c("Null","Method","Method x Depth","Method x Habitat")
  ##generate AICc table
  AICctab(m1,m2,m3,m4, mnames = Modnames, base=TRUE, weights=TRUE, logLik=TRUE)
  print(AICctab(m1,m2,m3,m4, mnames = Modnames, base=TRUE, weights=TRUE, logLik=TRUE))
  a<-r.squaredGLMM(m1)
  b<-r.squaredGLMM(m2)
  d<-r.squaredGLMM(m3)
  e<-r.squaredGLMM(m4)
  
  print(a);print(b);print(d);print(e)
  
}
glmerMetric(s,"GENUS_CODE","SSSS","AdColDen")
lmerMetric(s,"GENUS_CODE","SSSS","logAve.size")
lmerMetric(s,"GENUS_CODE","SSSS","sqrtAve.od")



#Prevalence Data
#Calculate # of cases/type- need this for the the binomial GLMMs 
s<-subset(site,GENUS_CODE=="SSSS")
s$AcuteDZ<-(s$AcuteDZ_prev/100)*s$AdColCount
s$ChronicDZ<-(s$ChronicDZ_prev/100)*s$AdColCount
s$BLE<-(s$BLE_prev/100)*s$AdColCount
head(s)

glmerPrev<-function(d,grouping_field="GENUS_CODE",genus_field="SSSS",metric_field="AcuteDZ"){
  d$GROUP<-d[,grouping_field]
  d$METRIC<-d[,metric_field]
  s<-subset(d,GROUP==genus_field)
  
  Cand.set <- list( )
  Cand.set[[1]]<-glmer(METRIC~1 + (1|ISLAND/SEC_NAME),family=binomial,data=s)
                       Cand.set[[2]]<-glmer(METRIC~METHOD + (1|ISLAND/SEC_NAME),family=binomial,data=s)
                       Cand.set[[4]]<-glmer(METRIC~METHOD*MAX_DEPTH_M + (1|ISLAND/SEC_NAME),family=binomial,data=s,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
                       Cand.set[[5]]<-glmer(METRIC~METHOD*HAB_R1 + (1|ISLAND/SEC_NAME),family=binomial,data=s,control=glmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=200000)))
                       
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


####Analyst-level analysis
#interaction between method and analyst for the analysts that have data for both method types
seg_sub<-subset(seg,GENUS_CODE=="SSSS" & ANALYST %in% c("RS","MA","AH"))
mod1<-lmer(METRIC~1 + (1|ISLAND/SEC_NAME),data=seg_sub,REML=F)
mod2<-lmer(METRIC~METHOD*ANALYST + (1|ISLAND/SEC_NAME),data=seg_sub,REML=F)


