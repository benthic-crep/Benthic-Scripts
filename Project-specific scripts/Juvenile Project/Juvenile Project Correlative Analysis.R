#Useful websites: https://m-clark.github.io/generalized-additive-models/application.html



rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

library(VCA)
library(forcats)
library(geosphere)
library(rgdal)
library(stringr)
library(mgcv)
detach(package:dplyr) #dplyr has issues if plyr is loaded first
library(dplyr)
library(MASS)
library(performance)
library(see)
library(patchwork)
library(emmeans)
library(rcompanion)

setwd("T:/Benthic/Projects/Juvenile Project")


#LOAD DATA
df<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvDeltaDen_Pred.csv")#Combined juvenile delta density and all predictors

#remove columns
df<-subset(df,select=-c(GENUS_CODE,N_h.as,Dom_N_h, w, ANALYSIS_SEC,ANALYSIS_YEAR, r_y, RUBBLE,SAND,MA,MeanMaxDepth,Carbonate_HC,Sand_Rubble_HC,utmlat_strata_weighted_mean,HUMANS200_mean,SE_MEAN_SH,meankdPAR,MaxMaxDHW10))
df<-subset(df,select=-c(Delta_CORAL,Delta_CCA,Delta_SAND_RUB,Delta_TURF,Basalt_HC))

##Mannually adding in substrate height for Tut_aunuu_a (used all years rather than just most recent survey since no fish surveys were done in this sector in 2018)
df$MEAN_SH<-ifelse(df$STRATANAME=="TUT_AUNUU_A_Forereef_Deep",0.3850,df$MEAN_SH)
df$MEAN_SH<-ifelse(df$STRATANAME=="TUT_AUNUU_A_Forereef_Mid",0.31,df$MEAN_SH)

# 
# jwd_site<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/JuvProject_pb_SITE.csv")#Post bleaching strata-level juvenile data
# 
# #Only use Total Scl juvenile data 
# jwd_siteS<-subset(jwd_site,GENUS_CODE=="SSSS")
# 
# 
# #Plot Juv Den vs. Depth at Site level- is there a cutoff for where juveniles start to decline?
# jwd_siteS$ANALYSIS_YEAR<-as.factor(jwd_siteS$ANALYSIS_YEAR)
#   
# p1<-jwd_siteS %>%
#   mutate(REGION = fct_relevel(REGION,"NWHI","MHI","PHOENIX","LINE","SAMOA","SMARIAN","NMARIAN")) %>% #reorder varibles 
#   ggplot(aes(x=MAX_DEPTH_M, y=JuvColDen, color=ANALYSIS_YEAR)) + 
#   geom_smooth(se=T,method="lm",lwd=1.5)+
#   geom_point()+
#   facet_wrap(~REGION,scales = "free_y")+
#   theme_bw() +
#   theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,axis.ticks.x = element_blank() # no x axis ticks
#     ,axis.title.x = element_text( vjust = -.0001))+ 
#   geom_vline(data=jwd_siteS, xintercept = 6.096,  size=0.75,color="grey")+
#   geom_vline(data=jwd_siteS,xintercept = 18.288,  size=0.75,color="grey")+
#    labs(x="Max Depth (m)",y="Juvenile Density")
# p1
# 
# 
# #Plot juv v depth at strata level
#   
# jwd_siteS$REGION<-str_replace(jwd_siteS$REGION,"Line", "LINE")
# jwd_siteS$REGION<-str_replace(jwd_siteS$REGION,"Phoenix", "PHOENIX")
# depth_strat$REGION<-str_replace(depth_strat$REGION,"Line", "LINE")
# depth_strat$REGION<-str_replace(depth_strat$REGION,"Phoenix", "PHOENIX")
# 
# stratD<-left_join(d_strat,depth_strat)
# head(stratD)
# 
# 
# 
# p2<-
#   ggplot(stratD,aes(x=MeanMaxDepth, y=DeltaDen)) + 
#   geom_smooth(se=T,method="lm",lwd=1.5)+
#   geom_point()+
#   #geom_errorbar(data=jcd_sum,aes(y=jcdMEAN, x=ANALYSIS_YEAR,ymin=jcdMEAN-jcdSE, ymax=jcdMEAN+jcdSE), width=.2)+
#   facet_wrap(~fct_relevel(REGION,"NWHI","MHI","PHOENIX","LINE","SAMOA","SMARIAN","NMARIAN"),scales = "free_y")+
#   theme_bw() +
#   theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank())+
#   labs(x="Mean Max Depth (m)",y="Juvenile Density")
# p2
# 
# 

#General Additive Models
head(df)


#Check for multicolinearity across predictors
fit1<-lm(DeltaDen_yr~CORAL+CCA+TURF+SAND_RUB+MeanDepth+Basalt_HC+lat_strata_weighted_mean+MeanMaxDHW10+Meankd490+
           MEAN_SH+HUMANS20_mean+MeanWavePower,data=df)

fit2<-gam(DeltaDen_yr~s(CORAL)+s(CCA)+s(SAND_RUB)+s(MeanDepth)+s(Basalt_HC)+s(MeanMaxDHW10)+s(Meankd490)+
           s(MEAN_SH)+s(HUMANS20_mean)+s(MeanWavePower),data=df) #overparameterized

fit2<-gam(DeltaDen_yr~s(CCA)+s(SAND_RUB)+s(MeanDepth)+s(MeanMaxDHW10)+
            s(HUMANS20_mean)+s(MeanWavePower),data=df) #I can't include more than 6

# summary(fit2)
# par(mfrow=c(3,2))
# plot(fit2)

#Testing for Multicolinarity
which( colnames(df)=="CORAL" )

preds<-df[,19:length(df)]
library(GGally)
ggpairs(preds)

library(car)
car::vif(fit1)

library(corrplot)

M = cor(preds)
corrplot(M, method = 'number')

#turf cover and CCA cover are correlated and CCA and mean latitude are also correlated
#Dropping turf and latitude
par(mfrow=c(2,2))

mod1<-gam(DeltaDen_mo~s(MeanDepth)+s(MaxMaxDHW10), data=df)
summary(mod1)

mod2<-gam(DeltaDen_mo~s(lat_strata_weighted_mean), data=df)
summary(mod2)
plot(mod2)

mod3<-gam(DeltaDen_mo~s(lat_strata_weighted_mean), data=df)
summary(mod3)
plot(mod3)

mod4<-gam(DeltaDen_mo~s(CCA), data=df)
summary(mod4)
plot(mod4)

# 
# mod1b = update(mod1, .~.-s(MeanMaxDHW10))
# summary(mod1b)

plot(mod1)

testdata = data.frame(MeanDepth = seq(0, 25, length = 108),
                      MaxMaxDHW10 = seq(0, 35, length = 108))

fits = predict(mod1, newdata=testdata, type='response', se=T)
predicts = data.frame(testdata, fits) %>% 
  mutate(lower = fit - 1.96*se.fit,
         upper = fit + 1.96*se.fit)

ggplot(aes(x=MaxMaxDHW10,y=fit), data=predicts) +
  geom_ribbon(aes(ymin = lower, ymax=upper), fill='gray90') +
  geom_line(color='#00aaff')+
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank())



# GLMS - Density ----------------------------------------------------------


#Density from the most recent survey time point -Gamma distribution

df$JuvColDen_pos<-df$T2_JuvColDen +0.5 #Make positive to use Gamma distribution

#Option 1 for predictors
den_mod1<-glm(JuvColDen_pos~ CORAL + CCA + SAND_RUB + MeanDepth + MeanMaxDHW10 + MaxMaxDHW03 + MEAN_SH + HUMANS20_mean + MeanWavePower,
              family = "Gamma",data= df)

#Option 2 for predictors
den_mod2<-glm(JuvColDen_pos~ CORAL + CCA + SAND_RUB + MeanDepth + MaxMaxDHW03 + Mean_Mon_SST_Range + MEAN_SH + HUMANS20_mean + MeanWavePower,
              family = "Gamma",data= df)

# run model first with no interactions to assess VIF
step_mod_bic <- stepAIC(den_mod1, k = log(nrow(df)))
summary(step_mod_bic)

step_mod_bic <- stepAIC(den_mod2, k = log(nrow(df)))
summary(step_mod_bic)



# LMs - Delta Density ----------------------------------------------------------

#Checking normality/equal variance for Delta Density
plotNormalHistogram(df$DeltaDen_yr)

df$Delta_trans<-sqrt(df$DeltaDen_yr+3) #sqrt is best (also tried log and cube root)

plotNormalHistogram(df$Delta_trans)
mod<-lm(Delta_trans~REGION +(1|SEC_NAME),data=df)
mod<-lm(Delta_trans~ T1_JuvColDen + CORAL + CCA + SAND_RUB + MeanDepth + MaxMaxDHW03 + Mean_Mon_SST_Range + MEAN_SH + HUMANS20_mean + MeanWavePower, data=df)
mod<-lm(Delta_trans~REGION,data=df)

performance::check_model(mod)


#Option 1 for predictors
den_mod1<-lm(Delta_trans~T1_JuvColDen + CORAL + CCA + SAND_RUB + MeanDepth + MeanMaxDHW10 + MaxMaxDHW03 + MEAN_SH + HUMANS20_mean + MeanWavePower,
              data= df)

#Option 2 for predictors
den_mod2<-lm(Delta_trans~  CORAL + CCA + SAND_RUB + MeanDepth + MaxMaxDHW03 + Mean_Mon_SST_Range + MEAN_SH + HUMANS20_mean + MeanWavePower+T1_JuvColDen,
              data= df)

# run model first with no interactions to assess VIF
step_mod_bic <- stepAIC(den_mod1, k = log(nrow(df)))
summary(step_mod_bic)

step_mod_bic <- stepAIC(den_mod2, k = log(nrow(df)))
summary(step_mod_bic)

