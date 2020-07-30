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
# 
# 
# #Set up in wide format
# colnames(sfm_seg)[9:17] <- paste("SfM_", colnames(sfm_seg[,c(9:17)]), sep = "");sfm_seg<-dplyr::select(sfm_seg,-c(METHOD,ANALYST,HABITAT_CODE))
#                                                                                                                      
# colnames(diver_seg)[9:17] <- paste("Diver_", colnames(diver_seg[,c(9:17)]), sep = "");diver_seg<-dplyr::select(diver_seg,-c(METHOD,ANALYST,ISLAND,HABITAT_CODE,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,MIN_DEPTH_M,MAX_DEPTH_M))
# 
# seg.wide<-merge(sfm_seg,diver_seg,by=c("SITE","SITEVISITID","SEGMENT","GENUS_CODE","SEGAREA_ad","SEGAREA_j"),all=T)
# 
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


# Prep Site-level data ----------------------------------------------------

#Select columns to keep in SITE data
site<-dplyr::select(site, c(METHOD,SITEVISITID,GENUS_CODE,TRANSECTAREA_ad,TRANSECTAREA_j,AdColCount,AdColDen,JuvColDen,Ave.size,Ave.od,Ave.rd,
                          BLE_prev,AcuteDZ_prev,ChronicDZ_prev,ISLAND,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,HABITAT_CODE,
                          MIN_DEPTH_M,MAX_DEPTH_M))


#site<-subset(site,SITE!="HAW-04285") #sckewed by 1 very very large colony
sfm_site<-subset(site,METHOD=="SfM")
diver_site<-subset(site,METHOD=="Diver")

##############################START HERE#####################


#Set up in wide format
colnames(sfm_seg)[9:17] <- paste("SfM_", colnames(sfm_seg[,c(9:17)]), sep = "");sfm_seg<-dplyr::select(sfm_seg,-c(METHOD,HABITAT_CODE))

colnames(diver_seg)[9:17] <- paste("Diver_", colnames(diver_seg[,c(9:17)]), sep = "");diver_seg<-dplyr::select(diver_seg,-c(METHOD,ANALYST,ISLAND,HABITAT_CODE,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,MIN_DEPTH_M,MAX_DEPTH_M))

seg.wide<-merge(sfm_seg,diver_seg,by=c("SITE","SITEVISITID","SEGMENT","GENUS_CODE","SEGAREA_ad","SEGAREA_j"),all=T)

#################IMPORTANT- CHECK FOR MERGING ERRORS
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

head(subset(seg.wide,GENUS_CODE=="SSSS"))




# Plotting Regressions and Bland-Altman by Taxon --------------------------
#PlotAll(dataframe, variable 1, variable 2, y-axis name 1, y-axis name 2, x-axis name 1, x-axis name 2)

outpath<- "T:/Benthic/Data/SfM/Method Comparision/Figures/Adult Density"
if(!dir.exists(outpath)){dir.create(outpath)}
p1<-PlotAll(seg.wide,"Diver_AdColDen","SfM_AdColDen","SfM Adult Density","Difference SfM Analyst and Diver", "Diver Adult Density","Mean Adult Density")
p1

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Juvenile Density"
if(!dir.exists(outpath)){dir.create(outpath)}
p4<-PlotAll(seg.wide,"Diver_JuvColDen","SfM_JuvColDen","SfM Juvenile Density","Difference SfM Analyst and Diver", "Diver Juvenile Density","Mean Juvenile Density")
p4

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Colony Size"
if(!dir.exists(outpath)){dir.create(outpath)}
p7<-PlotAll(seg.wide,"Diver_Ave.size","SfM_Ave.size","SfM Colony Length","Difference SfM Analyst and Diver", "Diver Colony Length","Mean Colony Length")
p7

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Old Dead"
if(!dir.exists(outpath)){dir.create(outpath)}
p10<-PlotAll(seg.wide,"Diver_Ave.od","SfM_Ave.od","SfM Average Old Dead Pct","Difference SfM Analyst and Diver", "Diver Average Old Dead Pct","Average Old Dead Pct")
p10

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Recent Dead"
if(!dir.exists(outpath)){dir.create(outpath)}
p13<-PlotAll(seg.wide,"Diver_Ave.rd","SfM_Ave.rd","SfM Average Recent Dead Pct","Difference SfM Analyst and Diver", "Diver Average Recent Dead Pct","Average Recent Dead Pct")
p13

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/Bleaching"
if(!dir.exists(outpath)){dir.create(outpath)}
p16<-PlotAll(seg.wide,"Diver_BLE_prev","SfM_BLE_prev","SfM Bleaching Prevalence","Difference SfM Analyst and Diver", "Diver Bleaching Prevalence","Mean Bleaching Prevalence")
p16

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/ChronicDZ"
if(!dir.exists(outpath)){dir.create(outpath)}
p19<-PlotAll(seg.wide,"Diver_ChronicDZ_prev","SfM_ChronicDZ_prev","SfM Chronic Disease Prevalence","Difference SfM Analyst and Diver", "Diver Chronic Disease Prevalence","Mean Chronic Disease Prevalence")
p19

outpath<-"T:/Benthic/Data/SfM/Method Comparision/Figures/AcuteDZ"
if(!dir.exists(outpath)){dir.create(outpath)}
p22<-PlotAll(seg.wide,"Diver_AcuteDZ_prev","SfM_AcuteDZ_prev","SfM General Disease Prevalence","Difference SfM Analyst and Diver", "Diver General Disease Prevalence","Mean General Disease Prevalence")
p22


#Mixed Models
table(seg$HABITAT_CODE)

#Simplify Habitat codes
seg<-seg %>% mutate(HAB_R1=recode(HABITAT_CODE, 
                                                  `AGR`="Carbonate Reef",
                                                  `APR`="Carbonate Reef",
                                                  `PAV`="Pavement",
                                                  `PPR`="Pavement",
                                                  `RRB`="Rubble",
                                                  `ROB`="Rock & Boulder",
                                                  `SCR`="Reef with Sand",
                                                  `PSC`="Reef with Sand"))


#bar plots of juv desnity by sector by year
p1<-ggplot(subset(seg,GENUS_CODE=="SSSS"), aes(x=HAB_R1, y=AdColDen, fill=METHOD)) + 
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

p2<-ggplot(subset(seg,GENUS_CODE=="SSSS"), aes(x=HAB_R1, y=AdColDen, fill=METHOD)) + 
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


glmerDensity<-function(d,grouping_field="GENUS_CODE",metric_field="AdColDen"){
  d$GROUP<-d[,grouping_field]
  d$METRIC<-d[,metric_field]
  
  s<-subset(d,GROUP=="SSSS")
  nullmod<-glmer(METRIC~1 + (1|SEC_NAME/SITE)+ offset(SEGAREA_ad),family=poisson,data=s)
  mod1<-glmer(METRIC~METHOD + (1|SEC_NAME/SITE)+ offset(SEGAREA_ad),family=poisson,data=s)
  mod2<-glmer(METRIC~METHOD + ANALYST+ (1|SEC_NAME/SITE)+ offset(SEGAREA_ad),family=poisson,data=s)
  mod3<-glmer(METRIC~ANALYST+ (1|SEC_NAME/SITE)+ offset(SEGAREA_ad),family=poisson,data=s)
  mod4<-glmer(METRIC~METHOD*MAX_DEPTH_M + (1|SEC_NAME/SITE)+ offset(SEGAREA_ad),family=poisson,data=s)
  mod5<-glmer(METRIC~METHOD*HAB_R1 + (1|SEC_NAME/SITE)+ offset(SEGAREA_ad),family=poisson,data=s)
  
  }


s<-subset(seg,GENUS_CODE=="SSSS")
nullmod<-glmer(AdColCount~1 + (1|SEC_NAME/SITE),family=poisson,data=s)
mod1<-glmer(AdColCount~METHOD + (1|SEC_NAME/SITE),family=poisson,data=s)
mod2<-glmer(AdColCount~METHOD*ANALYST+ (1|SEC_NAME/SITE),family=poisson,data=s,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
mod3<-glmer(AdColCount~METHOD*MAX_DEPTH_M + (1|SEC_NAME/SITE),family=poisson,data=s)
mod4<-glmer(AdColCount~METHOD*HAB_R1 + (1|SEC_NAME/SITE),family=poisson,data=s)



#s<-s %>% drop_na(SEGAREA_j)
nullmod<-glmer(JuvColDen~1 + (1|SEC_NAME/SITE),family=poisson,data=s)
mod1<-glmer(JuvColDen~METHOD + (1|SEC_NAME/SITE),family=poisson,data=s)
mod2<-glmer(JuvColDen~METHOD+ANALYST+ (1|SEC_NAME/SITE),family=poisson,data=s,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
mod3<-glmer(JuvColDen~METHOD*MAX_DEPTH_M + (1|SEC_NAME/SITE),family=poisson,data=s,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
#mod4<-glmer(JuvColDen~METHOD*HAB_R1 + (1|SEC_NAME/SITE),family=poisson,data=s,control=glmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=200000)))
#Can't use Habitat for juveniles- model convergence that can't be addressed
#Also having issues with model sigularity -need to simplify random effects (drop SEC_NAME)


anova(mod1,nullmod,test="chisq")
anova(mod2,mod1,test="chisq")
anova(mod2,mod3,test="chisq")
anova(mod1,mod4,test="chisq")


tab_model(mod2)

#Model Selection

glmerDensity<-function(d,grouping_field="GENUS_CODE",genus_field="SSSS",metric_field="AdColCount"){
  d$GROUP<-d[,grouping_field]
  d$METRIC<-d[,metric_field]
  s<-subset(d,GROUP==genus_field)
  
Cand.set <- list( )
Cand.set[[1]]<-glmer(METRIC~1 + (1|ISLAND/SITE),family=poisson,data=s)
Cand.set[[2]]<-glmer(METRIC~METHOD + (1|ISLAND/SITE),family=poisson,data=s)
Cand.set[[3]]<-glmer(METRIC~METHOD+ANALYST+ (1|ISLAND/SITE),family=poisson,data=s,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
Cand.set[[4]]<-glmer(METRIC~METHOD*MAX_DEPTH_M + (1|ISLAND/SITE),family=poisson,data=s,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
Cand.set[[5]]<-glmer(METRIC~METHOD*HAB_R1 + (1|ISLAND/SITE),family=poisson,data=s,control=glmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=200000)))

Modnames <- c("Null","Method","Method + Analyst","Method + Depth","Method + Habitat")
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
glmerDensity(seg,"GENUS_CODE","SSSS","AdColCount")
glmerDensity(seg,"GENUS_CODE","POSP","AdColCount")
glmerDensity(seg,"GENUS_CODE","MOSP","AdColCount")
glmerDensity(seg,"GENUS_CODE","POCS","AdColCount")

glmerDensity(seg,"GENUS_CODE","SSSS","JuvColDen")
glmerDensity(seg,"GENUS_CODE","POSP","JuvColDen")
glmerDensity(seg,"GENUS_CODE","MOSP","JuvColDen")
glmerDensity(seg,"GENUS_CODE","POCS","JuvColDen")

#Ave Size- not perfect transformation, but will work
s<-subset(seg,GENUS_CODE=="SSSS")
hist(log(s$Ave.od))
s$logAve.size<-log(s$Ave.size)
mod<-lmer(logAve.size~METHOD + (1|SEC_NAME/SITE),data=s)
plot(mod)
qqnorm(resid(mod)) #plot normal quantile- quantile plot.  Should be close to a straight line

#Old dead- not perfect transformation, but will work
s<-subset(seg,GENUS_CODE=="SSSS")
hist(sqrt(s$Ave.od))
s$sqAve.od<-sqrt(s$Ave.od)
mod<-lmer(sqAve.od~METHOD + (1|SEC_NAME/SITE),data=s)
plot(mod)
qqnorm(resid(mod)) #plot normal quantile- quantile plot.  Should be close to a straight line

#Can't transform recent dead
hist(log(s$Ave.rd+1))
s$logAve.rd<-log(s$Ave.rd+1)
mod<-lmer(logAve.rd~METHOD + (1|SEC_NAME/SITE),data=s)
plot(mod)
qqnorm(resid(mod)) #plot normal quantile- quantile plot.  Should be close to a straight line

#Transform variables
s<-subset(seg,GENUS_CODE=="SSSS")
s$logAve.size<-log(s$Ave.size)
s$sqrtAve.od<-sqrt(s$Ave.od)


lmerMetric<-function(d,grouping_field="GENUS_CODE",genus_field="SSSS",metric_field="AdColCount"){
  d$GROUP<-d[,grouping_field]
  d$METRIC<-d[,metric_field]
  s<-subset(d,GROUP==genus_field)
  
  Cand.set <- list( )
  Cand.set[[1]]<-lmer(METRIC~1 + (1|SEC_NAME/SITE),data=s,REML=F)
  Cand.set[[2]]<-lmer(METRIC~METHOD + (1|SEC_NAME/SITE),data=s,REML=F)
  Cand.set[[3]]<-lmer(METRIC~METHOD+ANALYST+ (1|SEC_NAME/SITE),data=s,REML=F,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
  Cand.set[[4]]<-lmer(METRIC~METHOD*MAX_DEPTH_M + (1|SEC_NAME/SITE),REML=F,data=s,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
  Cand.set[[5]]<-lmer(METRIC~METHOD*HAB_R1 + (1|SEC_NAME/SITE),data=s,REML=F,control=lmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=200000)))
  
  Modnames <- c("Null","Method","Method + Analyst","Method + Depth","Method + Habitat")
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

lmerMetric(s,"GENUS_CODE","SSSS","logAve.size")
lmerMetric(s,"GENUS_CODE","SSSS","sqrtAve.od")

mod<-lmer(logAve.size~METHOD*HAB_R1 + (1|SEC_NAME/SITE),data=s,REML=F,control=lmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=200000)))
summary(mod)

#Prevalence Data
#Calculate # of cases/type- need this for the the binomial GLMMs 
s<-subset(seg,GENUS_CODE=="SSSS")
s$AcuteDZ<-(s$AcuteDZ_prev/100)*s$AdColCount
s$ChronicDZ<-(s$ChronicDZ_prev/100)*s$AdColCount
s$BLE<-(s$BLE_prev/100)*s$AdColCount
head(s)

glmerPrev<-function(d,grouping_field="GENUS_CODE",genus_field="SSSS",metric_field="AcuteDZ"){
  d$GROUP<-d[,grouping_field]
  d$METRIC<-d[,metric_field]
  s<-subset(d,GROUP==genus_field)
  
  Cand.set <- list( )
  Cand.set[[1]]<-glmer(cbind(METRIC~1 + (1|ISLAND/SITE),family=binomial,data=s)
  Cand.set[[2]]<-glmer(METRIC~METHOD + (1|ISLAND/SITE),family=binomial,data=s)
  Cand.set[[3]]<-glmer(METRIC~METHOD+ANALYST+ (1|ISLAND/SITE),family=binomial,data=s,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
  Cand.set[[4]]<-glmer(METRIC~METHOD*MAX_DEPTH_M + (1|ISLAND/SITE),family=binomial,data=s,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000)))
  Cand.set[[5]]<-glmer(METRIC~METHOD*HAB_R1 + (1|ISLAND/SITE),family=binomial,data=s,control=glmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=200000)))
  
  Modnames <- c("Null","Method","Method + Analyst","Method + Depth","Method + Habitat")
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
glmerDensity(seg,"GENUS_CODE","SSSS","AdColCount")
glmerDensity(seg,"GENUS_CODE","POSP","AdColCount")
glmerDensity(seg,"GENUS_CODE","MOSP","AdColCount")
glmerDensity(seg,"GENUS_CODE","POCS","AdColCount")
