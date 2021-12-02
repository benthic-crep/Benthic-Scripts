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
library(survey)

setwd("T:/Benthic/Projects/Juvenile Project")


#LOAD DATA
df<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvDeltaDen_Pred.csv")#Combined juvenile delta density and all predictors
jcdG_st<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvProject_STRATA_WITHOUT_MHI2013.csv")

#remove columns
df<-subset(df,select=-c(GENUS_CODE,N_h.as,Dom_N_h, w, SEC_NAME,ANALYSIS_YEAR, r_y, RUBBLE,SAND,MA,MeanMaxDepth,Carbonate_HC,Sand_Rubble_HC,utmlat_strata_weighted_mean,HUMANS200_mean,SE_MEAN_SH,MaxMaxDHW10))
df<-subset(df,select=-c(Delta_CORAL,Delta_SAND_RUB,Delta_TURF,Basalt_HC))

##Mannually adding in substrate height for Tut_aunuu_a (used all years rather than just most recent survey since no fish surveys were done in this sector in 2018)
df$MEAN_SH<-ifelse(df$STRATANAME=="TUT_AUNUU_A_Forereef_Deep",0.3850,df$MEAN_SH)
df$MEAN_SH<-ifelse(df$STRATANAME=="TUT_AUNUU_A_Forereef_Mid",0.31,df$MEAN_SH)

#Change NaN to NA
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

df[is.nan(df)] <- NA
  
#Set up the strata-level data and add survey weights
jcdG_st<- mutate_if(jcdG_st, 
                    is.character, 
                    str_replace_all, pattern = " ", replacement = "_")
jcdG_stS<-subset(jcdG_st,GENUS_CODE=="SSSS")
REGION_YEAR<-c("MHI_2019","NWHI_2017","NMARIAN_2017","SMARIAN_2017","PHOENIX_2018","LINE_2018","SAMOA_2018","WAKE_2017")
jcdG_stS<-as.data.frame(jcdG_stS)

jcdG_stS$REGION_YEAR<-paste(jcdG_stS$REGION,jcdG_stS$ANALYSIS_YEAR,sep="_")
jcdG_stS_last<-jcdG_stS[jcdG_stS$REGION_YEAR %in% REGION_YEAR,]
head(jcdG_stS_last)
jcdG_stS_last$sw<-jcdG_stS_last$Ntot/jcdG_stS_last$n

#Extract predictors and merge with new survey weights dataset
pcols<-c("STRATANAME","CORAL", "CCA","Delta_CCA","TURF","SAND_RUB","MeanDepth","lat_strata_weighted_mean","MeanMaxDHW10",
         "MeanMaxDHW03","MaxMaxDHW03","Mean_Mon_SST_Range","Mean_BW_SST_Range",
         "meanChla05", "MeanWavePower","YearSinceDHW4")

p<-df[,pcols]

#Combine survey weighted juvenile data and predictors
nrow(jcdG_stS_last)
jcdG_stS_last<-left_join(jcdG_stS_last,p)
nrow(jcdG_stS_last);View(jcdG_stS_last)


#Testing for Multicolinarity
which(colnames(jcdG_stS_last)=="CORAL")
test<-subset(jcdG_stS_last,REGION!="NWHI")
preds<-test[,18:length(test)]
# library(GGally)
# ggpairs(preds)

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

# df$JuvColDen_pos<-df$T2_JuvColDen +0.5 #Make positive to use Gamma distribution
# 
# #Option 1 for predictors
# den_mod1<-glm(JuvColDen_pos~ CORAL + CCA + SAND_RUB + MeanDepth + MeanMaxDHW10 + MaxMaxDHW03 + MEAN_SH + HUMANS20_mean + MeanWavePower,
#               family = "Gamma",data= df)
# 
# #Option 2 for predictors
# den_mod2<-glm(JuvColDen_pos~ CORAL + CCA + SAND_RUB + MeanDepth + MaxMaxDHW03 + Mean_Mon_SST_Range + MEAN_SH + HUMANS20_mean + MeanWavePower,
#               family = "Gamma",data= df)
# 
# # run model first with no interactions to assess VIF
# step_mod_bic <- stepAIC(den_mod1, k = log(nrow(df)))
# summary(step_mod_bic)
# 
# step_mod_bic <- stepAIC(den_mod2, k = log(nrow(df)))
# summary(step_mod_bic)

des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+REGION+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=site.swS)

#Calculate regional mean and SE
sw_Rmean<-svyby(~JuvColDen,~ANALYSIS_YEAR+REGION,des,svymean)

#Test fixed effects of region and year
modR<-svyglm(JuvColCount ~ REGION*ANALYSIS_YEAR, design=des,offset= TRANSECTAREA_j, family="quasipoisson")



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

