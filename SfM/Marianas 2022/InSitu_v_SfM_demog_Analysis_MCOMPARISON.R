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
source("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-scripts/Functions/core_functions.R")

setwd("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision")


Plot1to1_new<-function(d,response_variable,predictor_variable,r_name,p_name,x,y){
  #sub<-d[d$taxon,]
  d$Y<-d[,response_variable]
  d$X<-d[,predictor_variable]
  mx_val<-max(d$Y, d$X, na.rm = TRUE)
  
  lm_fit <- lm(Y ~ X, data=d)
  predicted_df <- data.frame(Y_pred = predict(lm_fit, d), X=d$X)
  
  corr<-cor.test(d$X, d$Y, method="pearson")
  rmse<-rmse(d$Y, d$X,na.rm=TRUE)
  r_text<-paste("RMSE = ", round(rmse,digits = 2),"\n r = ", round((corr$estimate),2), sep="")
  
  p1<-ggplot(d, aes(x=X, y=Y)) + 
    geom_point(size=1) + 	
    geom_abline(slope=1, intercept=0) +
    #geom_jitter(width=.25,height=0,color="black",alpha=0.5)+
    #geom_smooth(method="lm", color="red", linetype="dashed", se=F) +
    geom_line(linetype = "dashed", color='red',data = predicted_df, aes(x=X, y=Y_pred))+
    geom_label(label=r_text,x=x,y=y, nudge_y=-0.1, nudge_x=1,label.size=0.35, color="black", fill="white") +
    theme_bw()+
    theme(panel.grid.major = element_blank()
          ,panel.grid.minor = element_blank())+
    scale_x_continuous(limits=c(0,mx_val)) +
    #scale_y_continuous(limits=c(0,mx_val)) +
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
      ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
      ,legend.position="none")+
    scale_x_continuous(breaks=seq(0,30,5))+
    labs(x="Max Depth (m)",y=metric_name)
  return(p)
}

#d<- na.omit(site[site$SITE!= "PAG-01273",-49])
#grouping_field<-"GENUS_CODE"
#metric_field<-"Ave.rd"
#genus_field<-"SSSS"
#metric_name<-"Average % Recent Dead"
PlotDepth_NP<-function(d,grouping_field,metric_field,genus_field,metric_name){
  d$GROUP<-d[,grouping_field]
  d$METRIC<-d[,metric_field]
  s<-subset(d,GROUP==genus_field)
  
  p1<-ggplot(s, aes(x=MAX_DEPTH_M, y=METRIC,fill = METHOD,color=METHOD)) + 
    #geom_smooth(method="lm")+
    geom_point(size=1) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 0)
          ,plot.background = element_blank()
          ,panel.grid.major = element_blank()
          ,panel.grid.minor = element_blank()
          ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
          ,legend.position="none")+
    scale_x_continuous(breaks=seq(0,30,5))+
    labs(x="Max Depth (m)",y=metric_name)     
  return(p1)
}



#Read in files
#seg<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/HARAMP19_GENUS_SEGMENT.csv")
site<-read.csv("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/MARAMP22_GENUS_SITE_Juvs.csv")
site_as <- read.csv("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/ASRAMP23_GENUS_SITE_Juvs.csv")
site <- bind_rows(site, site_as)
site <- subset(site, select = -c(HABITAT_CODE, NOTES))
div<-read.csv("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/MARAMP22_DIVERSITY_SITE_Genus.csv")
div_as<-read.csv("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/ASRAMP23_DIVERSITY_SITE_Genus.csv")
div <- bind_rows(div, div_as)
div <- subset(div, select = -c(HABITAT_CODE, NOTES, NA.))


#divS<-read.csv("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/MARAMP22_DIVERSITY_SPECIES_SITE.csv")

#
## #Select columns to keep in segment data
#seg<-dplyr::select(seg, c(METHOD,SITE,SITEVISITID,SEGMENT,GENUS_CODE,ANALYST,SEGAREA_ad,SEGAREA_j,AdColCount,AdColDen,JuvColDen,Ave.size,Ave.od,Ave.rd,
#                                    BLE_prev,AcuteDZ_prev,ChronicDZ_prev,ISLAND,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,HABITAT_CODE,
#                                    MIN_DEPTH_M,MAX_DEPTH_M))
#
#seg<-subset(seg,SITE!="HAW-04285"&SEGMENT!="5") #sckewed by 1 very very large colony
#sfm_seg<-subset(seg,METHOD=="SfM")
#diver_seg<-subset(seg,METHOD=="Diver")
#
## #Set up in wide format
#colnames(sfm_seg)[9:17] <- paste("SfM_", colnames(sfm_seg[,c(9:17)]), sep = "");sfm_seg<-dplyr::select(sfm_seg,-c(METHOD,HABITAT_CODE))
#
#colnames(diver_seg)[9:17] <- paste("Diver_", colnames(diver_seg[,c(9:17)]), sep = "");diver_seg<-dplyr::select(diver_seg,-c(METHOD,ANALYST,ISLAND,HABITAT_CODE,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,MIN_DEPTH_M,MAX_DEPTH_M))
#
#seg.wide<-merge(sfm_seg,diver_seg,by=c("SITE","SITEVISITID","SEGMENT","GENUS_CODE","SEGAREA_ad","SEGAREA_j"),all=T)
#
## #################IMPORTANT- CHECK FOR MERGING ERRORS
#View(subset(seg.wide,GENUS_CODE=="SSSS")) #Make sure columns merge properly
#
##Merge together and remove segs that aren't merging properly
#seg.wide<-merge(sfm_seg,diver_seg,by=c("SITE","SITEVISITID","SEGMENT","GENUS_CODE","SEGAREA_ad","SEGAREA_j"))
#View(seg.wide) ##NOTE- LAN-01819 seg 10 SfM is duplicated for some reason-hopefully this will be fixed with final data
#
## #Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
#seg.wide$Diver_JuvColDen[is.na(seg.wide$Diver_JuvColDen)]<-0
#seg.wide$Diver_AdColDen[is.na(seg.wide$Diver_AdColDen)]<-0
#seg.wide$SfM_JuvColDen[is.na(seg.wide$SfM_JuvColDen)]<-0
#seg.wide$SfM_AdColDen[is.na(seg.wide$SfM_AdColDen)]<-0
#
#head(seg.wide)


#Site level analysis
# Prep Site-level data ----------------------------------------------------
site$METHOD[site$METHOD == "Diver"] <- "DIVER"
div$METHOD[div$METHOD == "Diver"] <- "DIVER"
#Select columns to keep in SITE data
site.new<-dplyr::select(site, c(METHOD,SITEVISITID,SITE,GENUS_CODE,TRANSECTAREA_ad,TRANSECTAREA_j,AdColCount,AdColDen,JuvColDen,Ave.size,Ave.od,Ave.rd,Predation,
                            BLE_prev,AcuteDZ_prev,ChronicDZ_prev,ISLAND,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,
                            MIN_DEPTH_M,MAX_DEPTH_M))



#site<-subset(site,SITE!="HAW-04285") #sckewed by 1 very very large colony
sfm_site<-subset(site.new,METHOD=="SfM")
diver_site<-subset(site.new,METHOD=="DIVER")

#Set up in wide format
colnames(sfm_site)[7:16] <- paste("SfM_", colnames(sfm_site[,c(7:16)]), sep = "");sfm_site<-dplyr::select(sfm_site,-c(METHOD))

colnames(diver_site)[7:16] <- paste("Diver_", colnames(diver_site[,c(7:16)]), sep = "");diver_site<-dplyr::select(diver_site,-c(METHOD,ISLAND,SEC_NAME,DEPTH_BIN,LATITUDE,LONGITUDE,MIN_DEPTH_M,MAX_DEPTH_M))

site.wide<-merge(sfm_site,diver_site,by=c("SITE","SITEVISITID","GENUS_CODE"),all=T)

length(unique(site.wide$SITEVISITID)) #should be 104

#################IMPORTANT- CHECK FOR MERGING ERRORS
View(subset(site.wide,GENUS_CODE=="SSSS")) #Make sure columns merge properly


# #Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
site.wide$Diver_JuvColDen[is.na(site.wide$Diver_JuvColDen)]<-0
site.wide$Diver_AdColDen[is.na(site.wide$Diver_AdColDen)]<-0
site.wide$SfM_JuvColDen[is.na(site.wide$SfM_JuvColDen)]<-0
site.wide$SfM_AdColDen[is.na(site.wide$SfM_AdColDen)]<-0

head(site.wide)
test.marian <- subset(site.wide,GENUS_CODE=="SSSS")

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
#table(site$HABITAT_CODE)
#
#
##Simplify Habitat codes
#hab<-data.frame("HABITAT_CODE"= c("AGR","APS",
#           "PAV","PPR","RRB","ROB","SCR","PSC", "SAG"),
#           "HAB_R1" = c("Aggregate Reef","Patch Reef","Pavement",
#                        "Patch Reef","Rubble","Rock & Boulder",
#                        "Patch Reef","Patch Reef", "Spur & Groove"))
#nrow(site)
#site<-left_join(site,hab)
#nrow(site)
#
#bar plots of juv desnity by sector by year
p1<-ggplot(subset(site,GENUS_CODE=="SSSS"), aes(x=SEC_NAME, y=AdColDen, fill=METHOD)) + 
  geom_boxplot() + 
  # guides(fill=FALSE) 
  #facet_wrap(~SEC_NAME,scales = "free", labeller=label_parsed) +
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

p2<-ggplot(subset(site,GENUS_CODE=="SSSS"), aes(x=SITE, y=AdColDen, fill=METHOD)) + 
  geom_boxplot() + 
  # guides(fill=FALSE) 
  facet_wrap(~ISLAND,scales = "free", labeller=label_parsed)+
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


# ADULT DENSITY-SSSS ------------------------------------------------------

#also tried gamma and neg binomial- sqrt transform is best
s<-subset(site,GENUS_CODE=="SSSS")
hist(sqrt(s$AdColDen))

s$sqAdColDen<-sqrt(s$AdColDen)

m<-lmer(sqAdColDen~METHOD + (1|SEC_NAME),data=s)
anova(m)

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
plot(E2~as.factor(s$METHOD), ylab = 'residuals', xlab = "METHOD")
}

DPlots(m,s)


full.form <- formula(sqAdColDen ~ METHOD*MAX_DEPTH_M, data=s)

#fit lme using "ML"= maximum likelihood - so can use step fxn
full.lme <-lme(full.form, data=s, random=~1|SEC_NAME, method="ML") #use if needed

#drop 3-way interaction term
FULL.MOD <- update(full.lme, .~. -METHOD:MAX_DEPTH_M) 
#FULL.MOD <- update(FULL.MOD, .~. -MAX_DEPTH_M) 
anova(full.lme, FULL.MOD) #from output, drop the 2-way interaction term with the larger p-value first

RED.MOD2 <- update(FULL.MOD, .~. -MAX_DEPTH_M) #drop 2-way interaction term
anova(FULL.MOD, RED.MOD2) #LRT --> move forward w/ whichever model keeps/removes term


#Extract predicted values for Adult density
#https://aosmith.rbind.io/2018/11/16/plot-fitted-lines/
library(nlme)
mod<-lme(sqAdColDen~METHOD*MAX_DEPTH_M, random = ~1|SEC_NAME,data=s)

newdat.lme = data.frame(METHOD = s$METHOD,
                        MAX_DEPTH_M = s$MAX_DEPTH_M,
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

#I plotted the signficance labels outside of the axis limits for the publication
p1<-Plot1to1_new(s.wide,"SfM_AdColDen","Diver_AdColDen","SfM Adult Density","Diver Adult Density",5,25) +geom_text(x = 0, y = 36, label = "A", color = "black")
p2<-PlotMethod(site,"GENUS_CODE","AdColDen","SSSS","Adult Density",1.5,20,"NS")+geom_text(x = .5, y = 36, label = "B", color = "black")
#p3<-PlotHabitat(site,"GENUS_CODE","AdColDen","SSSS","Adult Density",3,50,"Method x Habitat NS")
#p3<-p3+theme(axis.title.x=element_blank(),axis.text.x=element_blank()) #Remove axis labels for the manuscript
p4<-PlotDepth(site,"GENUS_CODE","AdColDen","AdColDen","SSSS","Adult Density",20,30,"Method x Depth NS")+geom_text(x = 3, y = 36, label = "C", color = "black")
#p4<-p4+theme(axis.title.x=element_blank(),axis.text.x=element_blank())#Remove axis labels for the manuscript

AdColDenS<-grid.arrange(p1,p2,p4,nrow=1,ncol=3)

ggsave(plot<-AdColDenS,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/AdColDenSSSS_stats_update.png",width=12,height=6)

AdColDen_1n2<-grid.arrange(p1,p2,nrow=1,ncol=2)
AdColDen_3n4<-grid.arrange(p3,p4,nrow=1,ncol=2)


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


# JUVENILE DENSITY --------------------------------------------------------

#- sqrt transform
#also tried gamma and neg binomial- sqrt transform is best
s<-subset(site,GENUS_CODE=="SSSS" & JuvColDen < 40)
hist(sqrt(s$JuvColDen))
s$sqJuvColDen<-sqrt(s$JuvColDen)
m<-lmer(sqJuvColDen~METHOD + (1|SEC_NAME),data=s)

DPlots(m,s)


full.form <- formula(sqJuvColDen ~ METHOD*MAX_DEPTH_M, data=s)

#fit lme using "ML"= maximum likelihood - so can use step fxn
full.lme <-lme(full.form, data=s, random=~1|SEC_NAME, method="ML") #use if needed

#drop 3-way interaction term & depth x habitat (not interested in these hypotheses)
FULL.MOD <- update(full.lme, .~. -METHOD:MAX_DEPTH_M) 
anova(full.lme,FULL.MOD) #from output, drop the 2-way interaction term with the larger p-value first

RED.MOD1 <- update(FULL.MOD, .~. -METHOD) #drop 2-way interaction term
anova(FULL.MOD, RED.MOD1) #LRT --> move forward w/ whichever model keeps/removes term


anova(full.lme)

#Extract predicted values for Adult density
#https://aosmith.rbind.io/2018/11/16/plot-fitted-lines/
library(nlme)
mod<-lme(sqJuvColDen~METHOD*MAX_DEPTH_M, random = ~1|SEC_NAME,data=s)
anova(mod)
newdat.lme = data.frame(METHOD = s$METHOD,
                        MAX_DEPTH_M = s$MAX_DEPTH_M,
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

p1<-Plot1to1_new(s.wide,"SfM_JuvColDen","Diver_JuvColDen","SfM Juvenile Density","Diver Juvenile Density",10,40) +geom_text(x = 0, y = 49, label = "A")
p2<-PlotMethod(site,"GENUS_CODE","JuvColDen","SSSS","Juvenile Density",1.5,40,"Method Significant") +geom_text(x = 0.5, y = 49, label = "B")
#p3<-PlotHabitat(site,"GENUS_CODE","JuvColDen","SSSS","Juvenile Density",3,80,"Method x Habitat NS")
#p3<-p3+theme(axis.title.x=element_blank(),axis.text.x=element_blank()) #Remove axis labels for the manuscript
p4<-PlotDepth(site,"GENUS_CODE","JuvColDen","JuvColDen","SSSS","Juvenile Density",10,40,"Method x Depth NS") +geom_text(x = 3, y = 49, label = "C", color = "black")
#p4<-p4+theme(axis.title.x=element_blank(),axis.text.x=element_blank()) #Remove axis labels for the manuscript

JuvColDenS<-grid.arrange(p1,p2,p4,nrow=1)

ggsave(plot<-JuvColDenS,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/JuvColDenSSSS_stats.png",width=12,height=6)

JuvColDen_1n2<-grid.arrange(p1,p2,nrow=1,ncol=2)
JuvColDen_3n4<-grid.arrange(p3,p4,nrow=1,ncol=2)



# ADULT SIZE --------------------------------------------------------------

#not perfect transformation, but will work
s<-subset(site,GENUS_CODE=="SSSS")
hist(log(s$Ave.size))
s$logAve.size<-log(s$Ave.size)
m<-lmer(logAve.size~METHOD + (1|SEC_NAME),data=s)

DPlots(m,s)

full.form <- formula(logAve.size ~ METHOD*MAX_DEPTH_M, data=s)

#fit lme using "ML"= maximum likelihood - so can use step fxn
full.lme <-lme(full.form, data=s, random=~1|SEC_NAME, method="ML",na.action = na.omit) #use if needed

#drop 3-way interaction term & depth x habitat (not interested in these hypotheses)
FULL.MOD <- update(full.lme, .~. -METHOD:MAX_DEPTH_M) 
anova(full.lme, FULL.MOD) #from output, drop the 2-way interaction term with the larger p-value first

RED.MOD1 <- update(FULL.MOD, .~. -METHOD) #drop 2-way interaction term
anova(FULL.MOD, RED.MOD1) #LRT --> move forward w/ whichever model keeps/removes term

anova(full.lme)

#Extract predicted values for Adult density
#https://aosmith.rbind.io/2018/11/16/plot-fitted-lines/
library(nlme)
mod<-lme(logAve.size~METHOD*MAX_DEPTH_M, random = ~1|SEC_NAME,data=s,na.action=na.omit)

newdat.lme = data.frame(METHOD = s$METHOD,
                        MAX_DEPTH_M = s$MAX_DEPTH_M,
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

p1<-Plot1to1_new(na.omit(s.wide),"SfM_Ave.size","Diver_Ave.size","SfM Ave. Max Diameter (cm)","Diver Ave. Max Diameter (cm)",5,28)+geom_text(x = 0, y = 31, label = "A")
p2<-PlotMethod(na.omit(site),"GENUS_CODE","Ave.size","SSSS","Ave. Max Diameter (cm)",1.5,28,"NS")+geom_text(x = 0.5, y = 31, label = "B")
#p3<-PlotHabitat(na.omit(site[,-49]),"GENUS_CODE","Ave.size","SSSS","Ave. Max Diameter (cm)",3,50,"Method x Habitat NS")
p4<-PlotDepth(na.omit(site),"GENUS_CODE","Ave.size","Ave.size","SSSS","Ave. Max Diameter (cm)",15,28,"Method x Depth NS")+geom_text(x = 3, y = 31, label = "C", color = "black")

Ave.sizeS<-grid.arrange(p1,p2,p4,nrow=1)

ggsave(plot<-Ave.sizeS,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/Ave.sizeSSSS_stats.png",width=12,height=6)

Ave.size_1n2<-grid.arrange(p1,p2,nrow=1,ncol=2)
Ave.size_3n4<-grid.arrange(p3,p4,nrow=1,ncol=2)


# OLD DEAD ----------------------------------------------------------------


#not perfect transformation, but will work
s<-subset(site,GENUS_CODE=="SSSS")
hist(sqrt(s$Ave.od))
s$sqAve.od<-sqrt(s$Ave.od)
m<-lmer(sqAve.od~METHOD + (1|SEC_NAME),data=s)
DPlots(m,s)

full.form <- formula(sqAve.od ~ METHOD*MAX_DEPTH_M, data=s)

#fit lme using "ML"= maximum likelihood - so can use step fxn
full.lme <-lme(full.form, data=s, random=~1|SEC_NAME, method="ML",na.action = na.omit) #use if needed

#drop 3-way interaction term & depth x habitat (not interested in these hypotheses)
FULL.MOD <- update(full.lme, .~. -METHOD:MAX_DEPTH_M) 
anova(full.lme, FULL.MOD) #from output, drop the 2-way interaction term with the larger p-value first

RED.MOD3 <- update(FULL.MOD, .~. -METHOD) #drop 2-way interaction term
anova(FULL.MOD, RED.MOD3) #LRT --> move forward w/ whichever model keeps/removes term

anova(full.lme)

#Extract predicted values for Adult density
#https://aosmith.rbind.io/2018/11/16/plot-fitted-lines/
library(nlme)
mod<-lme(sqAve.od~METHOD*MAX_DEPTH_M, random = ~1|SEC_NAME,data=s,na.action=na.omit)

newdat.lme = data.frame(METHOD = s$METHOD,
                        MAX_DEPTH_M = s$MAX_DEPTH_M,
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

p1<-Plot1to1_new(na.omit(s.wide),"SfM_Ave.od","Diver_Ave.od","SfM Ave. % Old Dead","Diver Ave. % Old Dead", 10, 45)+geom_text(x = 0, y = 60, label = "A", color = "black")
p2<-PlotMethod(na.omit(site[,-49]),"GENUS_CODE","Ave.od","SSSS","Ave. % Old Dead",1.5,45,"Method Significant")+geom_text(x = 0.5, y = 60, label = "B", color = "black")
#p3<-PlotHabitat(na.omit(site[,-49]),"GENUS_CODE","Ave.od","SSSS","Ave. % Old Dead",3,100,"Method x Habitat NS")
p4<-PlotDepth(na.omit(site),"GENUS_CODE","Ave.od","Ave.od","SSSS","Ave. % Old Dead",15,55,"Method x Depth NS")+geom_text(x = 3, y = 60, label = "C", color = "black")

Ave.odS<-grid.arrange(p1,p2,p4,nrow=1)

ggsave(plot<-Ave.odS,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/Ave.odSSSS_stats.png",width=12,height=6)

Ave.od_1n2<-grid.arrange(p1,p2,nrow=1,ncol=2)
Ave.od_3n4<-grid.arrange(p3,p4,nrow=1,ncol=2)

# RECENT DEAD -------------------------------------------------------------

hist(log(s$Ave.rd))
s$logAve.rd<-log(s$Ave.rd+1)
mod<-lmer(logAve.rd~METHOD + (1|SEC_NAME),data=s)
plot(mod)
qqnorm(resid(mod)) #plot normal quantile- quantile plot.  Should be close to a straight line
DPlots(mod,s)
shapiro.test(s$logAve.rd)

#Remove sites that have 0s for both sfM and diver
s.wide<-subset(site.wide,GENUS_CODE=="SSSS")
s<-subset(site,GENUS_CODE=="SSSS");length(unique(s$SITE))
tmp <- s.wide[ which(s.wide$SfM_Ave.rd + s.wide$Diver_Ave.rd>0) , ]
sitelist<-unique(tmp$SITE)
s<-dplyr::filter(s, SITE %in% sitelist);length(unique(s$SITE))

s<-subset(site,GENUS_CODE=="SSSS");length(unique(s$SITE))
  
#Can't transform recent dead with sqrt, log or power transformation even after removing sites that have 0 rd in both sfm and diver
wilcox.test(Ave.rd ~ METHOD, data=s) 

t<-subset(s,METHOD=="SfM")
corr <- cor.test(x=t$MAX_DEPTH_M, y=t$Ave.rd, method = 'spearman')
corr

  
p1<-Plot1to1_new(na.omit(s.wide[s.wide$SITE != "PAG-01273",]),"SfM_Ave.rd","Diver_Ave.rd","SfM Average % Recent Dead","Diver % Recent Dead",.5,1.5)+geom_text(x = 0, y = 2, label = "D", color = "black")
p2<-PlotMethod(na.omit(site[site$SITE != "PAG-01273",-49]),"GENUS_CODE","Ave.rd","SSSS","Average % Recent Dead",1.5,1.5,"Method Significant")+geom_text(x = .5, y = 2, label = "E", color = "black")
#p3<-PlotHabitat(na.omit(site[s.wide$SITE != "PAG-01273",-49]),"GENUS_CODE","Ave.rd","SSSS","Average % Recent Dead",3,34,"")
p4<-PlotDepth_NP(na.omit(site[site$SITE != "PAG-01273",-49]),"GENUS_CODE","Ave.rd","SSSS","Average % Recent Dead")+geom_text(x = 3, y = 2, label = "F", color = "black")
    

Ave.rdS<-grid.arrange(p1,p2,p4,nrow=1)

ggsave(plot<-Ave.rdS,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/Ave.rdSSSS_stats_No_Outlier.png",width=12,height=6)

allplots<-grid.arrange(Ave.odS,Ave.rdS,nrow=2)
ggsave(plot<-allplots,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/PartialMortality_stats.png",width=12,height=12)


#Identify % of sites that had 0 rd for one method but not the other
a<-(s.wide[ which(s.wide$SfM_Ave.rd ==0 & s.wide$Diver_Ave.rd>0) , ])
nrow(a)/131
b<-(s.wide[ which(s.wide$SfM_Ave.rd >0 & s.wide$Diver_Ave.rd==0) , ])
nrow(b)/131


s<-subset(site,GENUS_CODE=="SSSS")
s$AcuteDZ<-(s$AcuteDZ_prev/100)*s$AdColCount
s$test<-as.integer(as.character(s$AcuteDZ))
head(s)


# ACUTE DISEASE PREVALENCE ------------------------------------------------

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


t<-subset(s,METHOD=="DIVER")
corr <- cor.test(x=t$MAX_DEPTH_M, y=t$AcuteDZ_prev, method = 'spearman')
corr

p1<-Plot1to1_new(na.omit(s.wide),"SfM_AcuteDZ_prev","Diver_AcuteDZ_prev","SfM Prevalence (%)","Diver Prevalence (%)", 1,2.5);p1<-p1+ggtitle("Acute Disease")+geom_text(x = 0, y = 3.75, label = "A", color = "black")
p2<-PlotMethod(na.omit(site[,-49]),"GENUS_CODE","AcuteDZ_prev","SSSS","Prevalence (%)",1.5,2,"NS")+geom_text(x = 0.5, y = 3.75, label = "B", color = "black")
#p3<-PlotHabitat(na.omit(site[,-49]),"GENUS_CODE","AcuteDZ_prev","SSSS","Prevalence (%)",3,34,"Method x Habitat NS")
p4<-PlotDepth_NP(na.omit(site[,-49]),"GENUS_CODE","AcuteDZ_prev","SSSS","Prevalence (%)")+geom_text(x = 3, y =3.75, label = "C", color = "black")

acutedzS<-grid.arrange(p1,p2,p4,nrow=3)

ggsave(plot<-acutedzS,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/AcuteDZSSSS_stats.png",width=8,height=6)

#Identify % of sites that had 0 rd for one method but not the other
a<-(s.wide[ which(s.wide$SfM_AcuteDZ_prev ==0 & s.wide$Diver_AcuteDZ_prev>0) , ])
nrow(a)/131
b<-(s.wide[ which(s.wide$SfM_AcuteDZ_prev >0 & s.wide$Diver_AcuteDZ_prev==0) , ])
nrow(b)/131


###predation
wilcox.test(Predation ~ METHOD, data=s) 

t<-subset(s,METHOD=="SfM")
corr <- cor.test(x=t$MAX_DEPTH_M, y=t$Predation, method = 'spearman')
corr

a<-(s.wide[ which(s.wide$SfM_Predation ==0 & s.wide$Diver_Predation>0) , ])
nrow(a)/131
b<-(s.wide[ which(s.wide$SfM_Predation >0 & s.wide$Diver_Predation==0) , ])
nrow(b)/131

p1<-Plot1to1_new(na.omit(s.wide),"SfM_Predation","Diver_Predation","SfM Prevalence (%)","Diver Prevalence (%)",1,3.5);p1<-p1+ggtitle("Predation")+geom_text(x = 0, y = 4.5, label = "D", color = "black")
p2<-PlotMethod(na.omit(site[,-49]),"GENUS_CODE","Predation","SSSS","Prevalence (%)",1.5,1.5,"NS")+geom_text(x = 0.5, y = 2.25, label = "E", color = "black")
p4<-PlotDepth_NP(na.omit(site[,-49]),"GENUS_CODE","Predation","SSSS","Prevalence (%)")+geom_text(x = 3, y = 2.25, label = "F", color = "black")

pred<-grid.arrange(p1,p2,p4,nrow=3,ncol=1)
ggsave(pred,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/PredSSSS_stats.png",width=8,height=6)
# CHRONIC DISEASE PREVALENCE ----------------------------------------------

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

t<-subset(s,METHOD=="DIVER")
corr <- cor.test(x=t$MAX_DEPTH_M, y=t$ChronicDZ_prev, method = 'spearman')
corr


p1<-Plot1to1_new(na.omit(s.wide),"SfM_ChronicDZ_prev","Diver_ChronicDZ_prev","SfM Prevalence (%)","Diver Prevalence (%)", 10,25)+geom_text(x = 0, y = 38, label = "G", color = "black");p1<-p1+ggtitle("Chronic Disease")
p2<-PlotMethod(na.omit(site[,-49]),"GENUS_CODE","ChronicDZ_prev","SSSS","Prevalence (%)",1.5,9.5,"NS")+geom_text(x = 0.5, y = 38, label = "H", color = "black")
#p3<-PlotHabitat(na.omit(site[,-49]),"GENUS_CODE","ChronicDZ_prev","SSSS","Prevalence (%)",3,60,"Method x Habitat NS")
p4<-PlotDepth_NP(na.omit(site[,-49]),"GENUS_CODE","ChronicDZ_prev","SSSS","Prevalence (%)")+geom_text(x = 3, y = 38, label = "I", color = "black")

ChronicdzS<-grid.arrange(p1,p2,p4,nrow=3)

ggsave(plot<-ChronicdzS,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/ChronicDZSSSS_stats.png",width=8,height=6)

#Identify % of sites that had 0 rd for one method but not the other
a<-(s.wide[ which(s.wide$SfM_ChronicDZ_prev ==0 & s.wide$Diver_ChronicDZ_prev>0) , ])
nrow(a)/131
b<-(s.wide[ which(s.wide$SfM_ChronicDZ_prev >0 & s.wide$Diver_ChronicDZ_prev==0) , ])
nrow(b)/131


s<-subset(site,GENUS_CODE=="SSSS")
s$BLE<-(s$BLE_prev/100)*s$AdColCount
s$test<-as.integer(as.character(s$BLE))
head(s)


# BLEACHING PREVALENCE ----------------------------------------------------

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


t<-subset(s,METHOD=="SfM")
corr <- cor.test(x=t$MAX_DEPTH_M, y=t$BLE_prev, method = 'spearman')
corr

p1<-Plot1to1_new(na.omit(s.wide),"SfM_BLE_prev","Diver_BLE_prev","SfM Prevalence (%)","Diver Prevalence (%)",10,5);p1<-p1+ggtitle("Bleaching Prevalence")+geom_text(x = 0, y = 6.5, label = "J", color = "black")
p2<-PlotMethod(na.omit(site[,-49]),"GENUS_CODE","BLE_prev","SSSS"," Prevalence (%)",1.5,5,"NS")+geom_text(x = .5, y = 17, label = "K", color = "black")
p4<-PlotDepth_NP(na.omit(site[,-49]),"GENUS_CODE","BLE_prev","SSSS","Prevalence (%)")+geom_text(x = 3, y = 17, label = "L", color = "black")

BLES<-grid.arrange(p1,p2,p4,nrow=3)

ggsave(plot<-BLES,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/BLEPrevSSSS_stats.png",width=8,height=6)

BLE_1n2<-grid.arrange(p1,p2,nrow=1,ncol=2)
BLE_3n4<-grid.arrange(p3,p4,nrow=1,ncol=2)


allplots<-grid.arrange(acutedzS,pred, ChronicdzS,BLES,nrow=1,ncol=4)
ggsave(plot<-allplots,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/ConditionsALL_stats.png",width=10,height=10)


#Identify % of sites that had 0 rd for one method but not the other
a<-(s.wide[ which(s.wide$SfM_BLE_prev ==0 & s.wide$Diver_BLE_prev>0) , ])
nrow(a)/131
b<-(s.wide[ which(s.wide$SfM_BLE_prev >0 & s.wide$Diver_BLE_prev==0) , ])
nrow(b)/131



# ADULT DENSITY- DOMINANT TAXA --------------------------------------------
doms <- site.new %>% 
          dplyr::group_by(GENUS_CODE, METHOD) %>% 
          dplyr::summarise(counts = sum(AdColCount)) %>% 
          dplyr::arrange(desc(counts))
doms.order <- site.new[site.new$GENUS_CODE!="SSSS",] %>% 
  dplyr::group_by(GENUS_CODE) %>% 
  dplyr::summarise(counts = as.numeric(sum(AdColCount))) %>% 
  dplyr::arrange(desc(counts))
doms.order$rank <- c(1:nrow(doms.order))
a.diff <- pivot_wider(doms, names_from = METHOD, values_from = counts)
a.diff$diff <- a.diff$SfM - a.diff$DIVER
a.diff$pct <- (a.diff$diff*100)/a.diff$DIVER

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

x <- anova(mod1,mod2)


p1<-Plot1to1_new(na.omit(s.wide),"SfM_AdColDen","Diver_AdColDen","SfM Adult Density","Diver Adult Density",2.5,10);p1<-p1+ggtitle("Porites")+theme(plot.title = element_text(face = "italic"))+geom_text(x = 0, y = 11.5, label = "A", color = "black")
p2<-PlotMethod(site,"GENUS_CODE","AdColDen","POSP","Adult Density",1.5,8,"NS")+geom_text(x = 0.5, y = 11.5, label = "B", color = "black")
POSPcolden<-grid.arrange(p1,p2,nrow=2,ncol=1)

ggsave(plot<-POSPcolden,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/AdColDenPOSP_stats.png",width=5,height=5)


##
s<-subset(site,GENUS_CODE=="ASSP")
s.wide<-subset(site.wide,GENUS_CODE=="ASSP")
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
s$logAdColDen<-log(s$AdColDen)
m<-lmer(logAdColDen~METHOD + (1|SEC_NAME),data=s)
m<-lmer(dp~METHOD + (1|SEC_NAME),data=s)

DPlots(m,s)
shapiro.test(dp)

wilcox.test(AdColDen ~ METHOD, data=s) 


p1<-Plot1to1_new(s.wide,"SfM_AdColDen","Diver_AdColDen","SfM Adult Density","Diver Adult Density",2.5,10);p1<-p1+ggtitle("Astreopora")+theme(plot.title = element_text(face = "italic"))+geom_text(x = 0, y = 12, label = "E", color = "black")
p2<-PlotMethod(site,"GENUS_CODE","AdColDen","ASSP","Adult Density",1.5,5,"NS")+geom_text(x = 0.5, y = 12, label = "F", color = "black")
ASSPcolden<-grid.arrange(p1,p2,nrow=2,ncol=1)

ggsave(plot<-MOSPcolden,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/AdColDenASSP_stats.png",width=5,height=5)


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
s$logAdColDen<-log(s$AdColDen)
m<-lmer(sqAdColDen~METHOD + (1|SEC_NAME),data=s)
m<-lmer(dp~METHOD + (1|SEC_NAME),data=s)

DPlots(m,s)

x$p.value <- wilcox.test(AdColDen ~ METHOD, data=s) 

p1<-Plot1to1_new(s.wide,"SfM_AdColDen","Diver_AdColDen","SfM Adult Density","Diver Adult Density",3,12);p1<-p1+ggtitle("Montipora")+theme(plot.title = element_text(face = "italic"))+geom_text(x = 0, y = 18, label = "C", color = "black")
p2<-PlotMethod(s,"GENUS_CODE","AdColDen","MOSP","Adult Density",1.5,6,"NS")+geom_text(x = 0.5, y = 18, label = "D", color = "black")
MOSPcolden<-grid.arrange(p1,p2,nrow=2,ncol=1)

ggsave(plot<-POCScolden,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/AdColDenFASP_stats.png",width=5,height=5)


allplots<-grid.arrange(POSPcolden,MOSPcolden,ASSPcolden,nrow=1,ncol=3)
ggsave(plot<-allplots,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/AdColDenDomtaxa_stats.png",width=10,height=8)

#plot all genera

Plot1to1_corr<-function(d,response_variable,predictor_variable,r_name,p_name,x,y){
  #sub<-d[d$taxon,]
  d$Y<-d[,response_variable]
  d$X<-d[,predictor_variable]
  mx_val<-max(d$Y, d$X, na.rm = TRUE)
  
  lm_fit <- lm(Y ~ X, data=d)
  predicted_df <- data.frame(Y_pred = predict(lm_fit, d), X=d$X)
  
  corr<-cor.test(d$X, d$Y, method="pearson")
  return(corr)
}

genus  <- doms$GENUS_CODE
plotlist <- list()
for (i in 1:length(genus)) {
s<-subset(site,GENUS_CODE==genus[i])
s.wide<-subset(site.wide,GENUS_CODE==genus[i])
p1<-Plot1to1_corr(s.wide,"SfM_AdColDen","Diver_AdColDen","SfM Adult Density","Diver Adult Density",50,50)#;p1<-p1+ggtitle(genus[i])+theme(plot.title = element_text(face = "italic"))
#p2<-PlotMethod(site,"GENUS_CODE","AdColDen",genus[i],"Adult Density",1.5,40,"NS")
plotlist[[i]]<-p1
}#i
allplots<-grid.arrange(grobs = plotlist,nrow=7,ncol=7)

ggsave(plot<-allplots,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/AdColDenAlltaxa_stats.png",width=10,height=8)

for (i in 1:nrow(a.diff)) {
s.wide<-subset(site.wide,GENUS_CODE==a.diff$GENUS_CODE[i])
lm_fit <- lm(Diver_AdColDen ~ SfM_AdColDen, data=s.wide)
test <- summary(lm_fit)
a.diff$R[i] <- test$adj.r.squared
}
write.csv(a.diff, "pct_diff.csv")
a.diff <- read.csv("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/pct_diff.csv")[,-1]
a.diff$Type[a.diff$GENUS_CODE == "CYPS"] <- "Mixed"
a.diff$Type[a.diff$GENUS_CODE == "ECHL"] <- "Cryptic"

doms.dens <- site.new %>% 
  dplyr::group_by(GENUS_CODE) %>% 
  dplyr::summarise(counts = mean(AdColDen)) %>% 
  dplyr::arrange(desc(counts))

a.diff <- left_join(a.diff, doms.dens)


  
s<-subset(site,GENUS_CODE!="SSSS")
test <- s[s$GENUS_CODE %in% a.diff$GENUS_CODE[a.diff$DIVER >15],-c(11:20)]
sigs <- data.frame(GENUS_CODE = unique(test$GENUS_CODE))
#par(mfrow = c(3,9))
for (i in 1:length(a.diff$GENUS_CODE[a.diff$DIVER >15])) {
hist(test$AdColDen[test$GENUS_CODE == unique(test$GENUS_CODE)[i]])
  
wtest <- wilcox.test(AdColDen ~ METHOD, data=test[test$GENUS_CODE == unique(test$GENUS_CODE)[i],])
#mod1<-lmer(log(AdColDen+1)~1 + (1|SEC_NAME),data=test[test$GENUS_CODE == unique(test$GENUS_CODE)[i],])
#mod2<-lmer(log(AdColDen+1)~METHOD + (1|SEC_NAME),data=test[test$GENUS_CODE == unique(test$GENUS_CODE)[i],])
#LRT <- anova(mod1,mod2)

sigs$pvals.wil[i] <-wtest$p.value
#sigs$pvals.lrt[i] <- LRT[2,8]
}
write.csv(sigs, "sigs.csv")

a.diff <- left_join(a.diff, sigs)
a.diff$sig <- ifelse(a.diff$pvals.wil > 0.05, "", "*")
a.diff$sig[is.na(a.diff$sig)] <- "NA" 
a.diff <- left_join(a.diff, doms.order[,-2], by = "GENUS_CODE")
order <- a.diff$GENUS_CODE
a.diff$GENUS_CODE <- factor(a.diff$GENUS_CODE, levels = rev(order))

rvals <- ggplot(a.diff[a.diff$DIVER >15 & a.diff$GENUS_CODE != "SSSS",], aes(x = forcats::fct_reorder(GENUS_CODE, Type), y = pct, fill = DIVER))+
  geom_bar(stat = "identity")+
  theme_classic()+
  geom_text(aes(label = sig, vjust = -1), position = "stack", color = "black")+
  geom_text(inherit.aes = F, label = "More Colonies in water", y = -100, x = 16, size = 5, color = "#F8766D")+
  geom_text(inherit.aes = F,label = "More Colonies in SfM", y = 100, x =16, size = 5, color = "#00BFC4")+
  geom_text(inherit.aes = F,label = "Cryptic", y = 75, x =3)+
  geom_text(inherit.aes = F,label = "Mixed", y = 75, x =8)+
  geom_text(inherit.aes = F,label = "Non-Cryptic", y = 75, x =21)+
  labs(x = "Genus Code", y = "% Difference", fill = "Abundance")+
  geom_hline(yintercept = 0, color = "black", linetype = "solid")+
  geom_vline(xintercept = 5.5, color = "black", linetype = "dashed")+
  geom_vline(xintercept = 10.5, color = "black", linetype = "dashed")+
  #ggrepel::geom_label_repel(aes(label = GENUS_CODE, color =  Type, size = counts, fill = sig), position = position_jitter(), max.overlaps = 20)+
  #scale_color_manual(values = c("tan4", "black", "dodgerblue4"))+
  #scale_fill_manual(values = c("gray88","white", "khaki"))+
  ylim(-100, 100)+#+guides(fill = "none")+
  theme(axis.text.x = element_text(angle = 90, vjust = .3, hjust=1))

rvals

ggsave(rvals, file= "C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/AdColcount.png", width = 10, height = 8)

rankabun <- left_join(doms, doms.order[,-2])

ggplot(rankabun[rankabun$GENUS_CODE!="SSSS",], aes(x = rank, y = counts, color = METHOD))+
  geom_line()+
  theme_classic()

# JUVENILE DENSITY- DOMINANT TAXA -----------------------------------------
doms.j <- site %>% 
  dplyr::group_by(GENUS_CODE, METHOD) %>% 
  dplyr::summarise(counts = as.numeric(sum(JuvColCount))) %>% 
  dplyr::arrange(desc(counts))

j.diff <- pivot_wider(doms.j, names_from = METHOD, values_from = counts)
j.diff$diff <- j.diff$SfM - j.diff$DIVER
j.diff$pct <- j.diff$diff*100/j.diff$DIVER

doms.dens <- site.new %>% 
  dplyr::group_by(GENUS_CODE) %>% 
  dplyr::summarise(counts = mean(JuvColDen)) %>% 
  dplyr::arrange(desc(counts))

doms.dens$rank <- c(1:nrow(doms.dens))

j.diff <- left_join(j.diff, doms.dens)


for (i in 1:nrow(j.diff)) {
  s.wide<-subset(site.wide,GENUS_CODE==j.diff$GENUS_CODE[i])
  lm_fit <- lm(Diver_JuvColDen ~ SfM_JuvColDen, data=s.wide)
  test <- summary(lm_fit)
  j.diff$R[i] <- test$adj.r.squared
}

j.diff <- left_join(j.diff, a.diff[,c(1,7,8)])

s<-subset(site,GENUS_CODE!="SSSS")
test <- s[s$GENUS_CODE %in% j.diff$GENUS_CODE[j.diff$DIVER >6],-c(11:20)]
sigs <- data.frame(GENUS_CODE = unique(test$GENUS_CODE))
par(mfrow = c(3,8))
for (i in 1:22) {
  hist(log(test$JuvColDen[test$GENUS_CODE == unique(test$GENUS_CODE)[i]]))
  
  wtest <- wilcox.test(JuvColDen ~ METHOD, data=test[test$GENUS_CODE == unique(test$GENUS_CODE)[i],])
  mod1<-lmer(log(JuvColDen+1)~1 + (1|SEC_NAME),data=test[test$GENUS_CODE == unique(test$GENUS_CODE)[i],])
  mod2<-lmer(log(JuvColDen+1)~METHOD + (1|SEC_NAME),data=test[test$GENUS_CODE == unique(test$GENUS_CODE)[i],])
  LRT <- anova(mod1,mod2)
  
  sigs$pvals.wil[i] <-wtest$p.value
  sigs$pvals.lrt[i] <- LRT[2,8]
}
write.csv(sigs, "sigs.csv")

j.diff <- left_join(j.diff, sigs)
j.diff$sig <- ifelse(j.diff$pvals.lrt > 0.05, "", "*")
#j.diff$sig[is.na(j.diff$sig)] <- "No Test" 
j.diff <- left_join(j.diff, doms.order[,-2], by = "GENUS_CODE")
order <- j.diff$GENUS_CODE
j.diff$GENUS_CODE <- factor(j.diff$GENUS_CODE, levels = rev(order))

rvals.juv <- ggplot(j.diff[j.diff$DIVER >6 & j.diff$GENUS_CODE != "SSSS",], aes(x = forcats::fct_reorder(GENUS_CODE, Type), y = pct, fill = DIVER))+
  geom_bar(stat = "identity")+
  theme_classic()+
  geom_text(aes(label = sig, vjust = -1), position = "stack", color = "black")+
  geom_text(inherit.aes = F, label = "More Colonies in-water", y = -100, x = 13.75, size = 5, color = "#F8766D")+
  geom_text(inherit.aes = F,label = "More Colonies in SfM", y = 110, x =13.5, size = 5, color = "#00BFC4")+
  geom_text(inherit.aes = F,label = "Cryptic", y = -90, x =3)+
  geom_text(inherit.aes = F,label = "Mixed", y = -90, x =8.5)+
  geom_text(inherit.aes = F,label = "Non-Cryptic", y = -90, x =18)+
  labs(x = "Genus Code", y = "% Difference", fill = "Abundance")+
  geom_hline(yintercept = 0, color = "black", linetype = "solid")+
  geom_vline(xintercept = 5.5, color = "black", linetype = "dashed")+
  geom_vline(xintercept = 11.5, color = "black", linetype = "dashed")+
  #ggrepel::geom_label_repel(aes(label = GENUS_CODE, color =  Type, size = counts, fill = sig), position = position_jitter(), max.overlaps = 20)+
  #scale_color_manual(values = c("tan4", "black", "dodgerblue4"))+
  #scale_fill_manual(values = c("gray88","white", "khaki"))+
  ylim(-100, 115)+#+guides(fill = "none")+
  theme(axis.text.x = element_text(angle = 90, vjust = .3, hjust=1))

ggsave(rvals.juv, file= "C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/JuvColcount.png", width = 10, height = 8)

s<-subset(site,GENUS_CODE=="POSP")
s.wide<-subset(site.wide,GENUS_CODE=="POSP")

hist((s$JuvColDen))
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
anova(mod1, mod2)

p1<-Plot1to1_new(s.wide,"SfM_JuvColDen","Diver_JuvColDen","SfM Juvenile Density","Diver Juvenile Density",4.5,1.5);p1<-p1+ggtitle("Porites")+theme(plot.title = element_text(face = "italic"))+geom_text(x = 0, y = 8.3, label = "A", color = "black")
p2<-PlotMethod(site,"GENUS_CODE","JuvColDen","POSP","Juvenile Density",1.5,6,"Significant")+geom_text(x = 0.5, y = 8.3, label = "B", color = "black")


POSPcolden<-grid.arrange(p1,p2,nrow=2,ncol=1)

ggsave(plot<-POSPcolden,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/JuvColDenPOSP_stats.png",width=5,height=5)


##
s<-subset(site,GENUS_CODE=="FASP")
s.wide<-subset(site.wide,GENUS_CODE=="FASP")
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
mod1<-lmer(logJuvColDen~1 + (1|SEC_NAME),data=s)
mod2<-lmer(logJuvColDen~METHOD + (1|SEC_NAME),data=s)
anova(mod1, mod2)

#m<-lmer(dp~METHOD + (1|SEC_NAME),data=s)
summary(m)
DPlots(m,s)
shapiro.test(dp)

wilcox.test(JuvColDen ~ METHOD, data=s) 


p1<-Plot1to1_new(s.wide,"SfM_JuvColDen","Diver_JuvColDen","SfM Juvenile Density","Diver Juvenile Density",7,2.5);p1<-p1+ggtitle("Favia")+theme(plot.title = element_text(face = "italic"))+geom_text(x = 0, y = 11, label = "C", color = "black")
p2<-PlotMethod(site,"GENUS_CODE","JuvColDen","FASP","Juvenile Density",1.5,5,"Significant")+geom_text(x = 0.5, y = 11, label = "D", color = "black")
FASPcolden<-grid.arrange(p1,p2,nrow=2,ncol=1)

ggsave(plot<-FASPcolden,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/JuvColDenFASP_stats.png",width=5,height=5)


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


hist(log(s$JuvColDen))
s$logJuvColDen<-log(s$JuvColDen+1)
mod1<-lmer(logJuvColDen~1 + (1|SEC_NAME),data=s)
mod2<-lmer(logJuvColDen~METHOD + (1|SEC_NAME),data=s)
anova(mod1, mod2)
DPlots(m,s)

wilcox.test(JuvColDen ~ METHOD, data=s) 


p1<-Plot1to1_new(s.wide,"SfM_JuvColDen","Diver_JuvColDen","SfM Juvenile Density","Diver Juvenile Density",4.5,1);p1<-p1+ggtitle("Pocillopora")+theme(plot.title = element_text(face = "italic"))+geom_text(x = 0, y = 5.5, label = "E", color = "black")
p2<-PlotMethod(site,"GENUS_CODE","JuvColDen","POCS","Juvenile Density",1.5,3,"NS")+geom_text(x = 0.5, y = 5.5, label = "F", color = "black")
POCScolden<-grid.arrange(p1,p2,nrow=2,ncol=1)

ggsave(plot<-POCScolden,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/JuvColDenASSP_stats.png",width=5,height=5)

s<-subset(site,GENUS_CODE=="LEPT")
s.wide<-subset(site.wide,GENUS_CODE=="LEPT")
hist(log(s$JuvColDen))
s$logJuvColDen<-log(s$JuvColDen+1)
mod1<-lmer(logJuvColDen~1 + (1|SEC_NAME),data=s)
mod2<-lmer(logJuvColDen~METHOD + (1|SEC_NAME),data=s)
anova(mod1, mod2)

p1<-Plot1to1_new(s.wide,"SfM_JuvColDen","Diver_JuvColDen","SfM Juvenile Density","Diver Juvenile Density",4,2);p1<-p1+ggtitle("Leptastrea")+theme(plot.title = element_text(face = "italic"))+geom_text(x = 0, y = 6.7, label = "G", color = "black")
p2<-PlotMethod(site,"GENUS_CODE","JuvColDen","LEPT","Juvenile Density",1.5,3,"Significant")+geom_text(x = 0.5, y = 6.7, label = "H", color = "black")
LEPTcolden<-grid.arrange(p1,p2,nrow=2,ncol=1)

allplots<-grid.arrange(POSPcolden,FASPcolden,POCScolden, LEPTcolden,nrow=1,ncol=4)
ggsave(plot<-allplots,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/JuvColDenDomtaxa_stats.png",width=16,height=10)



c#
c#genus  <- doms.j$GENUS_CODE
c#plotlist <- list()
c#for (i in 1:length(genus)) {
c#  s<-subset(site,GENUS_CODE==genus[i])
c#  s.wide<-subset(site.wide,GENUS_CODE==genus[i])
c#  p1<-Plot1to1_new(s.wide,"SfM_JuvColDen","Diver_JuvColDen","SfM Juvenile Density","Diver Juvenile Density",100,100);p1<-p1+ggtitle(genus[i])+theme(plot.title = element_text(face = "italic"))
c#  #p2<-PlotMethod(site,"GENUS_CODE","AdColDen",genus[i],"Adult Density",1.5,40,"NS")
c#  plotlist[[i]]<-p1
c#}#i
c#allplots<-grid.arrange(grobs = plotlist,nrow=7,ncol=7)
c#
c#ggsave(plot<-allplots,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/JuvColDenAlltaxa_stats.png",width=10,height=8)

# DIVERSITY ---------------------------------------------------------------

sfm_div<-subset(div,METHOD=="SfM")
diver_div<-subset(div,METHOD=="Diver")

# #Set up in wide format
colnames(sfm_div)[c(4:11,38:41)] <- paste("SfM_", colnames(sfm_div[,c(4:11,38:41)]), sep = "");sfm_div<-dplyr::select(sfm_div,-c(METHOD))

colnames(diver_div)[c(4:11,38:41)] <- paste("Diver_", colnames(diver_div[,c(4:11,38:41)]), sep = "");diver_div<-dplyr::select(diver_div,-c(METHOD))

div.wide<-left_join(sfm_div,diver_div, by = "SITE")
div.marian <- div.wide


hist(div$Adult_Shannon)
m<-lmer(Adult_Shannon~METHOD + (1|SEC_NAME),data=div)
DPlots(m,div)
leveneTest(Adult_Shannon~METHOD, data = div)

hist(div$Adult_Richness)
m<-lmer(Adult_Richness~METHOD + (1|SEC_NAME),data=div)
DPlots(m,div)
leveneTest(Adult_Richness~METHOD, data = div)

hist(div$Adult_Hills)
m<-lmer(logGENSPratio_Adult~METHOD + (1|SEC_NAME),data=div)
DPlots(m,div)
leveneTest(GENSPratio_Adult~METHOD, data = div)

##Shannon
full.form <- formula(Adult_Shannon ~ METHOD*MAX_DEPTH_M, data=div)

#fit lme using "ML"= maximum likelihood - so can use step fxn
full.lme <-lme(full.form, data=div, random=~1|SEC_NAME, method="ML",na.action = na.omit) #use if needed

#drop 3-way interaction term & depth x habitat (not interested in these hypotheses)
FULL.MOD <- update(full.lme, .~. -METHOD:MAX_DEPTH_M) 
anova(full.lme, FULL.MOD) #from output, drop the 2-way interaction term with the larger p-value first

RED.MOD1 <- update(FULL.MOD, .~. -METHOD) #drop 2-way interaction term
anova(FULL.MOD, RED.MOD1) #LRT --> move forward w/ whichever model keeps/removes term

anova(full.lme)


##hills
full.form <- formula(Adult_Hills ~ METHOD*MAX_DEPTH_M, data=div)

#fit lme using "ML"= maximum likelihood - so can use step fxn
full.lme <-lme(full.form, data=div, random=~1|SEC_NAME, method="ML",na.action = na.omit) #use if needed

#drop 3-way interaction term & depth x habitat (not interested in these hypotheses)
FULL.MOD <- update(full.lme, .~. -METHOD:MAX_DEPTH_M) 
anova(full.lme, FULL.MOD) #from output, drop the 2-way interaction term with the larger p-value first

RED.MOD1 <- update(FULL.MOD, .~. -METHOD) #drop 2-way interaction term
anova(FULL.MOD, RED.MOD1) #LRT --> move forward w/ whichever model keeps/removes term

anova(full.lme)


##richness
full.form <- formula(Adult_Richness ~ METHOD*MAX_DEPTH_M, data=div)

#fit lme using "ML"= maximum likelihood - so can use step fxn
full.lme <-lme(full.form, data=div, random=~1|SEC_NAME, method="ML",na.action = na.omit) #use if needed

#drop 3-way interaction term & depth x habitat (not interested in these hypotheses)
FULL.MOD <- update(full.lme, .~. -METHOD:MAX_DEPTH_M) 
anova(full.lme, FULL.MOD) #from output, drop the 2-way interaction term with the larger p-value first

RED.MOD1 <- update(FULL.MOD, .~. -METHOD) #drop 2-way interaction term
anova(FULL.MOD, RED.MOD1) #LRT --> move forward w/ whichever model keeps/removes term

anova(full.lme)



mod1<-lm(Adult_Shannon~METHOD, data = div);summary(mod1)
mod1<-lm(Adult_Richness~METHOD, data = div);summary(mod1)
mod1<-lm(Adult_Hills~METHOD, data = div);summary(mod1)
mod1<-lm(logGENSPratio_Adult~METHOD, data = div);summary(mod1)

p1<-Plot1to1_new(div.wide,"SfM_Adult_Shannon","Diver_Adult_Shannon","SfM Adult Shannon Diversity","Diver Adult Shannon Diversity",0.75,2)+
  geom_text(x = 0, y = 2.7, label = "A", color = "black")
p2<-Plot1to1_new(div.wide,"SfM_Adult_Hills","Diver_Adult_Hills","SfM Adult Diversity (Hills)","Diver Adult Diversity (Hills)",3.5,12)+
  geom_text(x = 0, y = 14.75, label = "C", color = "black")
p3<-Plot1to1_new(div.wide,"SfM_Adult_Richness","Diver_Adult_Richness","SfM Adult Genus Richness","Diver Adult Genus Richness",6,20)+
  geom_text(x = 0, y = 28, label = "E", color = "black")



p5<-ggplot(div, aes(x=METHOD, y=Adult_Shannon, fill=METHOD)) + 
  geom_boxplot() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001)
    ,legend.position="none")+ # adjust x axis to lower the same amount as the genus labels
  labs(x="Method",y="Adult Shannon Diversity")+
  geom_label(y=.3,x=1.5, label="Method Significant", fill = "white")+
  geom_text(x = 0.5, y = 2.8, label = "B", color = "black")

p6<-ggplot(div, aes(x=METHOD, y=Adult_Hills, fill=METHOD)) + 
  geom_boxplot() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001)
    ,legend.position="none")+ # adjust x axis to lower the same amount as the genus labels
  labs(x="Method",y="Adult Diversity (Hills)")+
  geom_label(y=1,x=1.5, label="Method Significant", fill = "white")+
  geom_text(x = 0.5, y = 17, label = "D", color = "black")

p7<-ggplot(div, aes(x=METHOD, y=Adult_Richness, fill=METHOD)) + 
  geom_boxplot() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001)
    ,legend.position="none")+ # adjust x axis to lower the same amount as the genus labels
  labs(x="Method",y="Adult Genus Richness")+
  geom_label(y=1,x=1.5, label="NS", fill = "white")+
  geom_text(x = 0.5, y = 28, label = "F", color = "black")



AdultDiv<-grid.arrange(p1, p2,  p3,p5,p6,p7,nrow=2,ncol=3)
ggsave(plot<-AdultDiv,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/Adult_Diversity_Genus.png",width=12,height=8)

#AdultDiv_Rich<-grid.arrange(p3,p6,nrow=1,ncol=2)
#AdultDiv_Div<-grid.arrange(p1,p5,nrow=2,ncol=1)

hist(div$Juv_Shannon)
m<-lmer(Juv_Shannon~METHOD + (1|SEC_NAME),data=div)
DPlots(m,div)
leveneTest(Adult_Shannon~METHOD, data = div)

hist(div$Juv_Richness)
m<-lmer(Juv_Richness~METHOD + (1|SEC_NAME),data=div)
DPlots(m,div)
leveneTest(Adult_Richness~METHOD, data = div)

hist(div$Juv_Hills)
m<-lmer(Juv_Hills~METHOD + (1|SEC_NAME),data=div)
DPlots(m,div)
leveneTest(Juv_Hills~METHOD, data = div)


full.form <- formula(Juv_Shannon ~ METHOD*MAX_DEPTH_M, data=div)

#fit lme using "ML"= maximum likelihood - so can use step fxn
full.lme <-lme(full.form, data=div, random=~1|SEC_NAME, method="ML",na.action = na.omit) #use if needed

#drop 3-way interaction term & depth x habitat (not interested in these hypotheses)
FULL.MOD <- update(full.lme, .~. -METHOD:MAX_DEPTH_M) 
anova(full.lme, FULL.MOD) #from output, drop the 2-way interaction term with the larger p-value first

RED.MOD1 <- update(FULL.MOD, .~. -METHOD) #drop 2-way interaction term
anova(FULL.MOD, RED.MOD1) #LRT --> move forward w/ whichever model keeps/removes term

anova(full.lme)


##hills
full.form <- formula(Juv_Hills ~ METHOD*MAX_DEPTH_M, data=div)

#fit lme using "ML"= maximum likelihood - so can use step fxn
full.lme <-lme(full.form, data=div, random=~1|SEC_NAME, method="ML",na.action = na.omit) #use if needed

#drop 3-way interaction term & depth x habitat (not interested in these hypotheses)
FULL.MOD <- update(full.lme, .~. -METHOD:MAX_DEPTH_M) 
anova(full.lme, FULL.MOD) #from output, drop the 2-way interaction term with the larger p-value first

RED.MOD1 <- update(FULL.MOD, .~. -METHOD) #drop 2-way interaction term
anova(FULL.MOD, RED.MOD1) #LRT --> move forward w/ whichever model keeps/removes term

anova(full.lme)


##richness
full.form <- formula(Juv_Richness ~ METHOD*MAX_DEPTH_M, data=div)

#fit lme using "ML"= maximum likelihood - so can use step fxn
full.lme <-lme(full.form, data=div, random=~1|SEC_NAME, method="ML",na.action = na.omit) #use if needed

#drop 3-way interaction term & depth x habitat (not interested in these hypotheses)
FULL.MOD <- update(full.lme, .~. -METHOD:MAX_DEPTH_M) 
anova(full.lme, FULL.MOD) #from output, drop the 2-way interaction term with the larger p-value first

RED.MOD1 <- update(FULL.MOD, .~. -METHOD) #drop 2-way interaction term
anova(FULL.MOD, RED.MOD1) #LRT --> move forward w/ whichever model keeps/removes term

anova(full.lme)


mod1<-lm(Juv_Shannon~METHOD, data = div);summary(mod1)
mod1<-lm(Juv_Richness~METHOD, data = div);summary(mod1)
mod1<-lm(Juv_Hills~METHOD, data = div);summary(mod1)


p1<-Plot1to1_new(na.omit(div.wide[,c(7,46)]),"SfM_Juv_Shannon","Diver_Juv_Shannon","SfM Juv Diversity (Shannon)","Diver Juv Diversity (Shannon)",1.8,.25)+
  geom_text(x = 0, y = 2.5, label = "A", color = "black")
p2<-Plot1to1_new(na.omit(div.wide[,c(9, 48)]),"SfM_Juv_Hills","Diver_Juv_Hills","SfM Juv Diversity (Hills)","Diver Juv Diversity (Hills)",2.5,10)+
  geom_text(x = 0, y = 11.8, label = "C", color = "black")
p3<-Plot1to1_new(na.omit(div.wide[,c(10,49)]),"SfM_Juv_Richness","Diver_Juv_Richness","SfM Juv Richness","Diver Juv Richness",4,15)+
  geom_text(x = 0, y = 17.1, label = "E", color = "black")





boxes <- na.omit(div[,c(1,8:11)])

p5<-ggplot(boxes, aes(x=METHOD, y=Juv_Shannon, fill=METHOD)) + 
  geom_boxplot() +
  geom_label(label="ns", x=1.5,y=.5,color = "black", fill="white")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001)
    ,legend.position="none")+ # adjust x axis to lower the same amount as the genus labels
  labs(x="Method",y="Juvenile Diversity (Shannon)")+
  geom_text(x = 0.5, y = 2.5, label = "B", color = "black")

p6<-ggplot(boxes, aes(x=METHOD, y=Juv_Hills, fill=METHOD)) + 
  geom_boxplot() +
  geom_label(label="ns", x=1.5,y=1.5,color = "black", fill="white")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001)
    ,legend.position="none")+ # adjust x axis to lower the same amount as the genus labels
  labs(x="Method",y="Juvenile Diversity (Hills)")+
  geom_text(x = 0.5, y = 11.9, label = "D", color = "black")

p7<-ggplot(boxes, aes(x=METHOD, y=Juv_Richness, fill=METHOD)) + 
  geom_boxplot() +
  geom_label(label="ns", x=1.5,y=1,color = "black", fill="white")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001)
    ,legend.position="none")+ # adjust x axis to lower the same amount as the genus labels
  labs(x="Method",y="Juvenile Genus Richness")+
  geom_text(x = 0.5, y = 17.3, label = "F", color = "black")

p8<-ggplot(na.omit(div[,c(1,41)]), aes(x=METHOD, y=logGENSPratio_Juv, fill=METHOD)) + 
  geom_boxplot() +
  geom_label(label="Method Significant", x=1.5,y=-1,color = "black", fill="white")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001)
    ,legend.position="none")+ # adjust x axis to lower the same amount as the genus labels
  labs(x="Method",y="Juvenile Genus:Species Ratio")+
  geom_text(x = 0.5, y = 5.1, label = "H", color = "black")

JuvDiv<-grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,nrow=2,ncol=4)
ggsave(plot<-JuvDiv,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/Juv_Diversity1to1.png",width=12,height=8)




# Plots for report --------------------------------------------------------
manuscriptplots_col<-grid.arrange(AdColDen_1n2,JuvColDen_1n2,Ave.size_1n2,Ave.od_1n2,
                              BLE_1n2,nrow=5,ncol=1)
manuscriptplots_div<-grid.arrange(AdultDiv_Rich,AdultDiv_Div,nrow=2,ncol=1)

manuscriptplots_col_SEM<-grid.arrange(AdColDen_3n4,JuvColDen_3n4,Ave.size_3n4,Ave.od_3n4,
                                  BLE_3n4,nrow=5,ncol=1)

ggsave(plot<-manuscriptplots_col,file="T:/Benthic/Data/SfM/Method Comparision/Figures/ManuscriptPlots_colonymetrics.png",width=8,height=10)
ggsave(plot<-manuscriptplots_div,file="T:/Benthic/Data/SfM/Method Comparision/Figures/ManuscriptPlots_diversitymetrics.png",width=8,height=6)
ggsave(plot<-manuscriptplots_col_SEM,file="T:/Benthic/Data/SfM/Method Comparision/Figures/ManuscriptPlots_colonymetricsSEM.png",width=8,height=10)

