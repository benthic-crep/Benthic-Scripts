#This script conducts the driver analysis using 2013-2019 survey data


#Run this script with the new svydesign structure without Niihau and see what results look like. Then run with


rm(list=ls())

#LOAD LIBRARY FUNCTIONS ...
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

#Calculate Difference in range of values
rg<-function(m) {
  max(m, na.rm=TRUE) - min(m, na.rm=TRUE)
}


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
library(gridExtra)
library(ggpubr)
library(lemon)
library(arm)
library(purrr)
library(tibble)
library(car)
library(extrafont)
library(remotes)
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# extrafont::font_import()
#loadfonts(device="win")


setwd("T:/Benthic/Projects/Juvenile Project")


#LOAD DATA
df.orig<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvDen_Pred_SITE_AllYears.csv")#Combined juvenile delta density and all predictors
jcdG_st<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvProject_STRATA_WITHOUT_MHI2013.csv")
cover_sec<-read.csv("T:/Benthic/Projects/Juvenile Project/BenthicCover_JuvenileProject_Tier1_SECTOR.csv")

#remove columns
df<-subset(df.orig,select=c(DATE_,OBS_YEAR,REGION,ISLAND,SEC_NAME,DEPTH_BIN,REEF_ZONE,STRATANAME,HABITAT_CODE,SITE,n,NH,sw,TRANSECTAREA_j,JuvColCount,JuvColDen,
                       LATITUDE,LONGITUDE,Depth_Median,CORAL,CORALst,CCA,SAND_RUB,TURF,EMA_MA, YearSinceDHW4, YearSinceDHW8,DHW.MeanMax_Degree_Heating_Weeks_YR01,
                       DHW.MeanMax_Degree_Heating_Weeks_YR03,DHW.MeanMax_Degree_Heating_Weeks_YR05,DHW.MeanMax_Degree_Heating_Weeks_YR10YR01,
                       DHW.MeanMax_Degree_Heating_Weeks_YR10,DHW.MaxMax_Degree_Heating_Weeks_YR10,
                       DHW.Np10y_Major_Degree_Heating_Weeks_YR10,WavePower,CVsst,CVchla,mean_SST_CRW_Daily_YR10,mean_Chlorophyll_A_VIIRS_Monthly_750m_YR05,
                       sd_SST_CRW_Daily_YR10,sd_Chlorophyll_A_VIIRS_Monthly_750m_YR05,mean_biweekly_range_SST_CRW_Daily_YR10,HumanDen))

#Combine site level data with sector cover data
cover_sec$OBS_YEAR<-cover_sec$ANALYSIS_YEAR
df<-left_join(df,cover_sec)

df$Area<-df$NH


df$CORAL_sec<-ifelse(df$SEC_NAME=="Baker",23.62746339,df$CORAL_sec)#no 2018 benthic cover sites so using fish sector data
df$CoralSec_A<-df$Area*df$CORAL_sec

#df<-filter(df,ISLAND !="Niihau")

table(df$REGION,df$OBS_YEAR)

df$DATE_<-ymd(df$DATE_)
#Generate table of date ranges and n for each island
df$OBS_MONTH<-lubridate::month(df$DATE_,label=TRUE)
df$OBS_DAY<-day(df$DATE_)

meta<-df %>%
  group_by(REGION,ISLAND,OBS_YEAR) %>%
  summarize(MinDate=min(DATE_),MaxDate=max(DATE_),n=length(n))

meta<-df %>%
  group_by(REGION,ISLAND,OBS_YEAR) %>%
  summarize(MinMonth=min(OBS_MONTH),MaxMonth=max(OBS_MONTH),
            MinDay=min(OBS_DAY),MaxDay=max(OBS_DAY),
            n=length(n))
# meta$DateMin<-paste(meta$MinMonth,meta$MinDay,sep="/")
# meta$DateMax<-paste(meta$MaxMonth,meta$MaxDay,meta$OBS_YEAR,sep="/")
# meta$DateRange<-paste(meta$DateMin,meta$DateMax,sep=" - ")
# 
meta$Month<-ifelse(meta$MinMonth==meta$MaxMonth,as.character(meta$MaxMonth),paste(meta$MinMonth,meta$MaxMonth,sep=" - "))
meta$DateRange<-paste(meta$Month,meta$OBS_YEAR,sep=" ")

# meta$DateRange<-paste(meta$MinDate,meta$MaxDate,sep = ",")
# meta<-meta[,c("REGION","ISLAND","OBS_YEAR","DateRange","n")]
# head(meta)

meta<-meta[,c("REGION","ISLAND","OBS_YEAR","DateRange","n")]
head(meta)
meta<-meta %>% mutate(T1_T2= dplyr::recode(OBS_YEAR,
                                           `2013`="T1",     
                                           `2014`="T1",
                                                `2015`="T1",
                                                `2016`="T2",
                                                `2017`="T2",
                                                `2018`="T2",
                                                `2019`="T3"))
meta$R_Y<-paste(meta$REGION,meta$OBS_YEAR,sep="-")
meta$T1_T2<-ifelse(meta$R_Y=="NWHI-2017","T3",meta$T1_T2)
View(meta)

coord<-df %>%
  group_by(REGION,ISLAND) %>%
  summarize(Latitude=median(LATITUDE),Longitude=median(LONGITUDE))

wide.date<-meta %>%
  dplyr::select(REGION,ISLAND,T1_T2,DateRange) %>%
  pivot_wider(names_from = T1_T2,values_from = DateRange)

wide.n<-meta %>%
  dplyr::select(REGION,ISLAND,T1_T2,n) %>%
  pivot_wider(names_from = T1_T2,values_from = n)
View(wide.n)

wide<-left_join(wide.date,wide.n,by=c("REGION","ISLAND"))

wide$T1<-paste(wide$T1.x,"(",wide$T1.y,")")
wide$T2<-paste(wide$T2.x,"(",wide$T2.y,")")
wide$T3<-paste(wide$T3.x,"(",wide$T3.y,")")

wide<-left_join(wide,coord,by=c("REGION","ISLAND"))

head(wide)
wide<-wide[,c("REGION","ISLAND","Latitude","Longitude","T1","T2","T3")]

wide$ISLAND <- factor(wide$ISLAND, levels = c("Kure","Pearl_&_Hermes","Lisianski","French_Frigate","Kauai",
                                              "Oahu","Molokai","Maui","Lanai","Kahoolawe","Hawaii","Wake",
                                              "Howland","Baker","Kingman","Palmyra","Jarvis","Saipan","Tinian",
                                              "Rota","Aguijan","Guam","Farallon_de_Pajaros","Maug","Pagan","Asuncion",
                                              "Sarigan","Swains","Tutuila","Ofu_&_Olosega","Tau","Rose"))
wide<- wide[order(wide$ISLAND),];View(wide)


write.csv(wide,file="T:/Benthic/Projects/Juvenile Project/Tables/Table1_v2.csv")


#Dealing with missing Year Since DHW4 data:
#If a site never experienced a >=4 DHW event then set YearSinceDHW4 to the most recent survey date - 1st recorded DHW data (1/1/1985)
df$DATE_<-ymd(df$DATE_)
dhw_start<-ymd("1985-01-01")

#how many NA values for year since = 198 (14.3%)
summary(df$YearSinceDHW4) 

#df$YearSinceDHW4<-ifelse(is.na(df$YearSinceDHW4),difftime(df$DATE_ ,dhw_start, units = c("weeks"))/52,df$YearSinceDHW4)
df$YearSinceDHW4<-ifelse(is.na(df$YearSinceDHW4),32,df$YearSinceDHW4)

subset(df,SEC_NAME=="OAH_NORTH")

#df$YearSinceDHW4<-ifelse(is.na(df$YearSinceDHW4)|df$YearSinceDHW4>=20,20,df$YearSinceDHW4)
# df$logYSD<-log(df$YearSinceDHW4)
# plot(df$JuvColDen~df$logYSD)


View(df)

#Convert latitude to absolute value
df$LATITUDE<-abs(df$LATITUDE)


#Rename Predictors
colnames(df)[colnames(df)=="DHW.MeanMax_Degree_Heating_Weeks_YR01"]<-"MaxDHW1"
colnames(df)[colnames(df)=="DHW.MaxMax_Degree_Heating_Weeks_YR03"]<-"MaxDHW3"
colnames(df)[colnames(df)=="DHW.MeanMax_Degree_Heating_Weeks_YR03"]<-"MeanDHW3"
colnames(df)[colnames(df)=="DHW.MeanMax_Degree_Heating_Weeks_YR05"]<-"MeanDHW5"
colnames(df)[colnames(df)=="DHW.MeanMax_Degree_Heating_Weeks_YR10YR01"]<-"MeanDHW9"
colnames(df)[colnames(df)=="DHW.MeanMax_Degree_Heating_Weeks_YR10"]<-"MeanDHW10"
colnames(df)[colnames(df)=="DHW.MaxMax_Degree_Heating_Weeks_YR10"]<-"MaxDHW10"
colnames(df)[colnames(df)=="mean_SST_CRW_Daily_YR10"]<-"MeanSST"
colnames(df)[colnames(df)=="sd_Chlorophyll_A_VIIRS_Monthly_750m_YR05"]<-"SDchla"
colnames(df)[colnames(df)=="sd_SST_CRW_Daily_YR10"]<-"SDsst"
colnames(df)[colnames(df)=="mean_Chlorophyll_A_VIIRS_Monthly_750m_YR05"]<-"Meanchla"

colnames(df)[colnames(df)=="DHW.Np10y_Major_Degree_Heating_Weeks_YR10"]<-"DHW_Freq"
colnames(df)[colnames(df)=="mean_biweekly_range_SST_CRW_Daily_YR10"]<-"SST_Range"

# #Remove row with NAs for Chla
# nrow(df)
# df<-df %>% drop_na(CVchla)
# nrow(df)

hist(log10(df$HumanDen+0.5))
df$logHumanDen<-log10(df$HumanDen+0.5)


#Extract predictors and merge with new survey weights dataset
pcols<-c("SITE","CORAL","CoralSec_A","CORALst","CCA","TURF","EMA_MA","SAND_RUB","Depth_Median","LATITUDE","MaxDHW1",
         "MeanDHW3","MeanDHW5","MeanDHW9","MeanDHW10","MaxDHW10","DHW_Freq","CVsst", "CVchla","Meanchla","SST_Range","SDsst","SDchla","MeanSST","WavePower","YearSinceDHW4","logHumanDen")

p<-df[,pcols]

#Combine survey weighted juvenile data and predictors
rcols<-c("OBS_YEAR","REGION","SITE","TRANSECTAREA_j","JuvColCount","n","NH","sw")

r<-df[,rcols]

nrow(r)
r<-left_join(r,p)
nrow(r);View(r)


#Testing for Multicolinarity
which(colnames(r)=="CORAL")
preds<-r[,9:ncol(r)]
# library(GGally)
# ggpairs(preds)

#Check that there are no duplicate sites
df %>% 
  group_by(SITE) %>% 
  filter(n()>1)


library(corrplot)

par(mfrow=c(1,1))
M = cor(preds)
png(width = 750, height = 750, filename = "T:/Benthic/Projects/Juvenile Project/Figures/Drivers/JuvenilePredictorsCorPlot_AllYears.png")
corrplot(M, method = 'number')
dev.off()

#Confirmed with VIF - a priori cut off 3, but all less than 2.
fit1 <- lm(JuvColDen ~ CORAL + CoralSec_A +  CCA +  EMA_MA + SAND_RUB + Depth_Median +  
             MeanDHW10 + SST_Range +  Meanchla + 
             WavePower + YearSinceDHW4 + logHumanDen, data = df)

car::vif(fit1)


#Turf and CCA correlated, latitutude and Mean SST correlated, Mean SSt and CVsst correlated

ggplot() + 
  geom_point(data=df, aes(x = LATITUDE, y = CVsst,color=REGION)) + 
    theme_bw()

ggplot() + 
  geom_point(data=df, aes(x = LATITUDE, y = CVchla,color=REGION)) + 
  theme_bw()

ggplot() + 
  geom_point(data=df, aes(x = LATITUDE, y = Meanchla,color=REGION)) + 
  theme_bw()


#Dropping turf, MaxMaxDHW03,SST range,year since event
preds <- scale(preds, center = T, scale = T);colnames(preds)<-paste("scaled",colnames(preds),sep="_")

new.df<-cbind(df,preds)


# Look for and remove outliers --------------------------------------------
#quick plots of top predictors
par(mfrow=c(2,2))
plot(new.df$JuvColDen~new.df$MeanDHW10)
plot(new.df$JuvColDen~new.df$Depth_Median)
#plot(new.df$JuvColDen~new.df$LATITUDE)
plot(new.df$JuvColDen~new.df$CVsst)
plot(new.df$JuvColDen~new.df$CVchla) 
plot(new.df$JuvColDen~new.df$Meanchla) 

plot(new.df$JuvColDen~new.df$scaled_CORAL) 
plot(new.df$JuvColDen~new.df$scaled_CCA) 
plot(new.df$JuvColDen~new.df$scaled_EMA_MA) 
plot(new.df$JuvColDen~new.df$scaled_SAND_RUB) 

plot(new.df$JuvColDen~new.df$CVsst)
plot(new.df$JuvColDen~new.df$YearSinceDHW4)
plot(new.df$JuvColDen~new.df$scaled_SST_Range)
plot(new.df$JuvColDen~new.df$scaled_SST_Range)



par(mfrow=c(1,1))
plot(new.df$JuvColDen~new.df$YearSinceDHW4)
plot(new.df$JuvColDen~new.df$scaled_CoralSec_A) 
plot(new.df$JuvColDen~new.df$scaled_logHumanDen) 

head(new.df)


##Convert Year since HS event into categorical variable
new.df$HS_YN<-ifelse(new.df$YearSinceDHW4<=15,"HS","No_HS")
#new.df$YearSinceDHW4<-ifelse(new.df$YearSinceDHW4<=15,new.df$YearSinceDHW4,NA)

#Also tried setting NA values to max of climatolgoical time series = 32 years
new.df$YearSinceDHW4<-ifelse(is.na(new.df$YearSinceDHW4),32,new.df$YearSinceDHW4)




#Remove outliers -there are 4 2016 FFS sites that are >40 juvs/m2. When I remove them Sector coral cover is no longer signficant. 
#Keeping them in for now, but will probably remove the points or sector coral cover eventually
# drop.site<-c("FFS-01314","FFS-01288","FFS-01328","FFS-01272")
# new.df<-new.df %>% filter(!SITE %in% drop.site)

#write.csv(new.df,file="T:/Benthic/Projects/Juvenile Project/Data/test.new.df.csv")

# #Backwards Model selection with Wald Tests (similar to LRTs) ------------
data.cols<-c("OBS_YEAR","REGION","ISLAND","SEC_NAME","STRATANAME","SITE","TRANSECTAREA_j","JuvColCount","n","NH","sw","SITE","CORAL","CoralSec_A","CCA","EMA_MA","SAND_RUB","Depth_Median",
                    "MeanDHW5","MeanDHW10","DHW_Freq","CVsst", "CVchla","Meanchla","MeanSST","WavePower","YearSinceDHW4","scaled_CORAL","scaled_CoralSec_A","scaled_CCA","scaled_EMA_MA","scaled_SAND_RUB","scaled_Depth_Median",
"scaled_MeanDHW5","scaled_MeanDHW10","scaled_DHW_Freq","scaled_CVsst", "scaled_CVchla","scaled_Meanchla","scaled_SST_Range","scaled_MeanSST","scaled_WavePower","scaled_YearSinceDHW4","scaled_logHumanDen","logHumanDen","HS_YN")

new.df<-new.df[,data.cols]

#StRS design
#concantate all nested variables into 1 column 
#des<-svydesign(id=~1, strata=~ OBS_YEAR + REGION+ISLAND+SEC_NAME+STRATANAME, weights=~sw,data=new.df) #overestimating the variance wit this method only accounting for the first variable

new.df$Strat_conc<-paste(new.df$OBS_YEAR, new.df$REGION,new.df$ISLAND,new.df$STRATANAME,sep = "_")
# rm.st<-c("2015_NWHI_French_Frigate_French_Frigate_Forereef_Deep","2015_NWHI_French_Frigate_French_Frigate_Forereef_Shallow",
#          "2015_NWHI_Lisianski_Lisianski_Forereef_Shallow","2016_MHI_Kauai_KAU_NAPALI_Forereef_Mid")
# new.df<-dplyr::filter(new.df,!Strat_conc %in% rm.st)
des<-svydesign(id=~1, strata=~ Strat_conc, weights=~sw,data=new.df)

#There was an error with depth bin of 1 of the 2015 FFS sites

# Testing for polynomial relationships -----------------------------------------------------

#Testing polynomial relationships with depth
d_poly3<-svyglm(JuvColCount ~  
                  poly(scaled_Depth_Median,3),
                design=des, family="poisson",offset=log(TRANSECTAREA_j))

d_poly2<-svyglm(JuvColCount ~  
                    poly(scaled_Depth_Median,2),
                    design=des, family="poisson",offset=log(TRANSECTAREA_j))
d<-svyglm(JuvColCount ~  
            scaled_Depth_Median,
          design=des, family="poisson",offset=log(TRANSECTAREA_j))

anova(d,d_poly2) 
anova(d_poly3,d_poly2) 

AIC(d_poly3)
AIC(d_poly2)
AIC(d)

#Plotting 2nd order poly
df.d<-new.df
df.d$scaled_Depth_Median<- seq(min(new.df$scaled_Depth_Median),max(new.df$scaled_Depth_Median),
                                  by=round(rg(new.df$scaled_Depth_Median),5)/nrow(new.df))


p <- predict(d_poly3, newdata = df.d, type = "response",se.fit=TRUE)
p<-as.data.frame(p)
colnames(p)<-c("Predicted_Juv","SE_Juv")
newdata<-cbind(df.d,p)
newdata$Predict.lwr <- newdata$Predicted_Juv - 1.96 * newdata$SE_Juv # confidence interval upper bound
newdata$Predict.upr <- newdata$Predicted_Juv + 1.96 * newdata$SE_Juv # confidence interval lower bound
head(newdata)


att <- attributes(scale(new.df$Depth_Median))
mylabels <- seq(0,30,3)
mybreaks <- scale(mylabels, att$`scaled:center`, att$`scaled:scale`)[,1]

#Plot
ggplot(newdata, aes(x = scaled_Depth_Median, y = Predicted_Juv)) +
  geom_line() +
  geom_ribbon(data = newdata,
              aes(ymin = Predict.lwr, ymax = Predict.upr),
              alpha = 0.1)+
  ylab("Predicted Juvenile Abudance") +
  xlab("Median Depth (m)")+ 
  ggtitle("Depth with 3rd order Polynomial")+
    scale_x_continuous(labels=mylabels,breaks=mybreaks)


#Testing polynomial realtionships with Coral
d_poly3<-svyglm(JuvColCount ~  
                  poly(scaled_CORAL,3),
                design=des, family="poisson",offset=log(TRANSECTAREA_j))

d_poly2<-svyglm(JuvColCount ~  
                  poly(scaled_CORAL,2),
                design=des, family="poisson",offset=log(TRANSECTAREA_j))
d<-svyglm(JuvColCount ~  
            scaled_CORAL,
          design=des, family="poisson",offset=log(TRANSECTAREA_j))

anova(d,d_poly2) 
anova(d_poly3,d_poly2) 

AIC(d_poly3)
AIC(d_poly2)
AIC(d)


#Plotting 3rd order poly
df.d<-new.df
df.d$scaled_CORAL<- seq(min(new.df$scaled_CORAL),max(new.df$scaled_CORAL),
                               by=round(rg(new.df$scaled_CORAL),5)/nrow(new.df))


p <- predict(d_poly2, newdata = df.d, type = "response",se.fit=TRUE)
p<-as.data.frame(p)
colnames(p)<-c("Predicted_Juv","SE_Juv")
newdata<-cbind(df.d,p)
newdata$Predict.lwr <- newdata$Predicted_Juv - 1.96 * newdata$SE_Juv # confidence interval upper bound
newdata$Predict.upr <- newdata$Predicted_Juv + 1.96 * newdata$SE_Juv # confidence interval lower bound
head(newdata)


att <- attributes(scale(new.df$CORAL))
mylabels <- seq(0,85,10)
mybreaks <- scale(mylabels, att$`scaled:center`, att$`scaled:scale`)[,1]

#Plot
ggplot(newdata, aes(x = scaled_CORAL, y = Predicted_Juv)) +
  geom_line() +
  geom_ribbon(data = newdata,
              aes(ymin = Predict.lwr, ymax = Predict.upr),
              alpha = 0.1)+
  ylab("Predicted Juvenile Abudance") +
  xlab("% Coral Cover")+ 
  ggtitle("% Coral Cover with 3rd order Polynomial")+
  scale_x_continuous(labels=mylabels,breaks=mybreaks)


#I played with polynomials with the other predictors and depth is the only one that falls out as nonlinear


#CHanges- use 3rd order poly for depth and 2nd order for coral cover


##### MANUSCRIPT- Model selection no chla or cvsst x dhw interactions, but dhw x benthic interactions ####
#There is an error- "all variables must be in design=argument

des<-svydesign(id=~1, strata=~ Strat_conc, weights=~sw,data=new.df)


#Global model with Interactions with MeanMaxDHW10
global.mod<-svyglm(JuvColCount ~
                      poly(scaled_CORAL,2,raw=TRUE)*scaled_MeanDHW10+ 
                      scaled_CCA*poly(scaled_Depth_Median,3,raw=TRUE)+
                      scaled_CoralSec_A*scaled_MeanDHW10 +
                      scaled_EMA_MA*scaled_MeanDHW10 +
                      scaled_SAND_RUB*scaled_MeanDHW10 +
                      poly(scaled_Depth_Median,3,raw=TRUE)*scaled_MeanDHW10 +
                      scaled_Meanchla +
                      scaled_CVsst+
                      scaled_WavePower*scaled_MeanDHW10+
                      scaled_YearSinceDHW4*scaled_MeanDHW10+
                      scaled_logHumanDen*scaled_MeanDHW10,
                    design=des, family="poisson",offset=log(TRANSECTAREA_j))

summary(global.mod)

#Only option to generate a R2 like metric for these kinds of models
cor(global.mod3$y, fitted(global.mod))^2

#Backwards model selection
RED.MOD1 <- update(global.mod, .~. -scaled_MeanDHW10:scaled_logHumanDen) #drop 2-way interaction term
anova(global.mod, RED.MOD1,method="Wald") #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD1)


RED.MOD2 <- update(RED.MOD1, .~. -poly(scaled_CORAL, 2, raw = TRUE):scaled_MeanDHW10) #drop 2-way interaction term
anova(RED.MOD1, RED.MOD2) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD2)

RED.MOD3 <- update(RED.MOD2, .~. -scaled_MeanDHW10:poly(scaled_Depth_Median, 3, raw = TRUE)) #drop 2-way interaction term
anova(RED.MOD2, RED.MOD3,test = "Chisq") #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD3)

RED.MOD4 <- update(RED.MOD3, .~. -scaled_CVsst) #drop 2-way interaction term
anova(RED.MOD3, RED.MOD4) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD4)

RED.MOD5 <- update(RED.MOD4, .~. -scaled_CCA:poly(scaled_Depth_Median, 3, raw = TRUE)) #drop 2-way interaction term
anova(RED.MOD4, RED.MOD5) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD5)

RED.MOD6 <- update(RED.MOD5, .~. -scaled_MeanDHW10:scaled_EMA_MA) #drop 2-way interaction term
anova(RED.MOD5, RED.MOD6) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD6)

RED.MOD7 <- update(RED.MOD6, .~. -scaled_MeanDHW10:scaled_CoralSec_A) #drop 2-way interaction term
anova(RED.MOD6, RED.MOD7) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD7)

RED.MOD8 <- update(RED.MOD7, .~. -scaled_Meanchla) #drop 2-way interaction term
anova(RED.MOD7, RED.MOD8) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD8)

RED.MOD9 <- update(RED.MOD8, .~. -scaled_MeanDHW10:scaled_SAND_RUB) #drop 2-way interaction term
anova(RED.MOD9, RED.MOD8) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD9)

RED.MOD10 <- update(RED.MOD9, .~. -scaled_CCA) #drop 2-way interaction term
anova(RED.MOD9, RED.MOD10) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD10)

RED.MOD11 <- update(RED.MOD10, .~. -scaled_MeanDHW10:scaled_WavePower) #drop 2-way interaction term
anova(RED.MOD11, RED.MOD10) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD11)

RED.MOD12 <- update(RED.MOD11, .~. -scaled_CoralSec_A) #drop 2-way interaction term
anova(RED.MOD11, RED.MOD12) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD12)

RED.MOD13 <- update(RED.MOD12, .~. -scaled_WavePower) #drop 2-way interaction term
anova(RED.MOD12, RED.MOD13) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD13)

AIC(RED.MOD11)
AIC(RED.MOD12)
AIC(RED.MOD13)

best.mod<-RED.MOD12
summary(best.mod)

#Only option to generate a R2 like metric for these kinds of models
cor(best.mod$y, fitted(best.mod))^2


#Also conducted model selection with NAs in YearSinceDHW4 treated as max of climatology (32 years)-
#The top predictors and effect sizes didn't change. 

# PARAMETER ESTIMATES +/- SE ----------------------------------------------

sum <- summary(best.mod)
sum.co <- data.frame(sum$coefficients)
sum.co$Variable <- rownames(sum.co)
sum.co <- data.frame(sum.co, row.names = NULL)
sum.co <- sum.co[ order(abs(sum.co$Estimate), decreasing = T),]
var_ord <- sum.co$Variable


sum.co$lwr.CI <- sum.co$Estimate - 1.96 * sum.co$Std..Error # confidence interval lower bound
sum.co$upr.CI <- sum.co$Estimate + 1.96 * sum.co$Std..Error # confidence interval upper bound
head(sum.co)

sum.co <- sum.co[ which(sum.co$Variable != "(Intercept)"),]

expression(bold('Mean Max '^o*'C-weeks'))

# sum.co$Variable <- factor(sum.co$Variable, levels = var_ord)
# sum.co <- sum.co[order(factor(sum.co$Variable, levels = var_ord)),]
sum.co$Variable_plot <- factor(c("Depth",
                                 "HS_ts x HS_sev",
                                 "Coral Cover^2",
                                 "Coral Cover",
                                 "HS_ts",
                                 "Depth^2",
                                 "Sector-level Coral Cover",
                                 "Macroalgae Cover",
                                 "Human Density",
                                 "Wave Power x HSsev",
                                 "Unconsolidated Cover",
                                 "Coral Cover^3",
                                 "Heat Stress",
                                 "Wave Power"),
                               levels = c("Depth",
                                          "HS_ts x HS_sev",
                                          "Coral Cover^2",
                                          "Coral Cover",
                                          "HS_ts",
                                          "Depth^2",
                                          "Sector-level Coral Cover",
                                          "Macroalgae Cover",
                                          "Human Density",
                                          "Wave Power x HSsev",
                                          "Unconsolidated Cover",
                                          "Coral Cover^3",
                                          "Heat Stress",
                                          "Wave Power"))

write.csv(sum.co,file="T:/Benthic/Projects/Juvenile Project/Tables/Table S2.csv")


sum.co$Sig <- NA
sum.co <- transform(sum.co,
                    Sig=ifelse(Pr...t..<0.05,"p<0.05","p>0.05"))

var_plot <-
  ggplot(sum.co,aes(x = reorder(Variable_plot,Estimate), y = Estimate)) +
  geom_point(aes(color = Sig),size = 3) +
  geom_hline(yintercept = 0, color = "lightgray") +
  geom_errorbar(aes(ymin = lwr.CI, ymax = upr.CI,color = Sig), width=.2,
                position=position_dodge(.9))  +
  coord_flip() +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0, "lines"),
    panel.background = element_rect(colour = "black", fill = "white"),
    text = element_text(size = 18)) +
  xlab("") +
  ylab("\nParameter Estimate") +
  scale_y_continuous(limits = c(-0.6,0.7)) +
  scale_x_discrete(limits = rev(levels(sum.co$Variable_plot))) +
  scale_color_manual(values = c("#009E73","black"))

var_plot


setwd("T:/Benthic/Projects/Juvenile Project/Figures/Drivers/")
png(width = 750, height = 750, filename = "ParameterEstimates_2013-19_v3.png")
var_plot
dev.off()


# function to predict bleaching and create a plot based on variable of interest
Predictplot <- function(mod=best.mod,dat=newdata, us_pred="CORAL",predictor="scaled_CORAL", predictor_name,sigcol="black",bks=2){
  dat$s_X<-dat[,predictor] #scaled predictor
  dat$X<-dat[,us_pred] #unscaled predictor
  
  p <- predict(mod, newdata = dat, type = "response",se.fit=TRUE)
  p<-as.data.frame(p)
  colnames(p)<-c("Predicted_Juv","SE_Juv")
  dat<-cbind(dat,p)
  dat$Predict.lwr <- dat$Predicted_Juv - 1.96 * dat$SE_Juv # confidence interval upper bound
  dat$Predict.upr <- dat$Predicted_Juv + 1.96 * dat$SE_Juv # confidence interval lower bound
  head(dat)
  
  mx_val<-max(dat$X, na.rm = TRUE)
  
  #Unscaling predictor to plot on x axis
  att <- attributes(scale(dat$X))
  mylabels <- seq(0,mx_val,bks)
  mybreaks <- scale(mylabels, att$`scaled:center`, att$`scaled:scale`)[,1]
  
  
  #Try mapping geom_rug(dat2,aes(x=s_X)) - dat2= new.df
  
  #Plot
  plot1<-ggplot(data=dat, aes(x = s_X, y = Predicted_Juv)) + 
    geom_line(color=sigcol,size=1) +
    geom_ribbon(data = dat,
                aes(ymin = Predict.lwr, ymax = Predict.upr),
                alpha = 0.1)+
    theme_bw() +
    theme(
      axis.title.y = element_blank(),
      axis.title = element_text(face = "bold"),
      text = element_text(size = 18),
      panel.grid = element_blank()
    ) +
    ylab("Predicted Juvenile Abudance") +
    xlab(predictor_name)  +
    scale_x_continuous(labels = mylabels,breaks=mybreaks) #This isn't working
  
  return(plot1)
  
}


#10yr Meam Max DHW
newdata <- new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
newdata$scaled_EMA_MA <- mean(new.df$scaled_EMA_MA)
newdata$scaled_WavePower <- mean(new.df$scaled_WavePower)
newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
newdata$scaled_logHumanDen <- mean(new.df$scaled_logHumanDen)
newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
newdata$scaled_MeanDHW10<-seq(min(new.df$scaled_MeanDHW10),max(new.df$scaled_MeanDHW10),
                              by=round(rg(new.df$scaled_MeanDHW10),5)/nrow(new.df))
newdata$scaled_YearSinceDHW4<-mean(new.df$scaled_YearSinceDHW4,na.rm=T)
expression('Module Temperature ('^o*'C)')
 
hs.plot<-Predictplot(best.mod,dat=newdata,"MeanDHW10","scaled_MeanDHW10",expression(bold('Mean Max '^o*'C-weeks')),"black",2)+
  geom_rug(data=new.df,mapping=aes(x=scaled_MeanDHW10,y=0))

hs.plot

#Depth
newdata <- new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
newdata$scaled_EMA_MA <- mean(new.df$scaled_EMA_MA)
newdata$scaled_WavePower <- mean(new.df$scaled_WavePower)
newdata$scaled_Depth_Median<- seq(min(new.df$scaled_Depth_Median),max(new.df$scaled_Depth_Median),
                                  by=round(rg(new.df$scaled_Depth_Median),5)/nrow(new.df))
newdata$scaled_logHumanDen <- mean(new.df$scaled_logHumanDen)
newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
newdata$scaled_YearSinceDHW4<-mean(new.df$scaled_YearSinceDHW4)


depth.plot<-Predictplot(best.mod,newdata,"Depth_Median","scaled_Depth_Median","Median Depth (m)","#009E73",2)+
  geom_rug(data=new.df,mapping=aes(x=scaled_Depth_Median,y=0))

#Coral Cover
newdata <- new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- seq(min(new.df$scaled_CORAL),max(new.df$scaled_CORAL),
                            by=round(rg(new.df$scaled_CORAL),4)/nrow(new.df))
newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
newdata$scaled_EMA_MA <- mean(new.df$scaled_EMA_MA)
newdata$scaled_WavePower <- mean(new.df$scaled_WavePower)
newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
newdata$scaled_logHumanDen <- mean(new.df$scaled_logHumanDen)
newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
newdata$scaled_YearSinceDHW4<-mean(new.df$scaled_YearSinceDHW4)

coral.plot<-Predictplot(best.mod,newdata,"CORAL","scaled_CORAL","% Coral Cover","#009E73",10)+
  geom_rug(data=new.df,mapping=aes(x=scaled_CORAL,y=0))

#Sector-level Coral Cover
newdata <- new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
newdata$scaled_CoralSec_A <- seq(min(new.df$scaled_CoralSec_A),max(new.df$scaled_CoralSec_A),
                                 by=round(rg(new.df$scaled_CoralSec_A),5)/nrow(new.df))
newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
newdata$scaled_EMA_MA <- mean(new.df$scaled_EMA_MA)
newdata$scaled_WavePower <- mean(new.df$scaled_WavePower)
newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
newdata$scaled_logHumanDen <- mean(new.df$scaled_logHumanDen)
newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
newdata$scaled_YearSinceDHW4<-mean(new.df$scaled_YearSinceDHW4)

coralsec.plot<-Predictplot(best.mod,newdata,"CoralSec_A","scaled_CoralSec_A","Sector-level Coral Cover x Area","#009E73",100000)+
  geom_rug(data=new.df,mapping=aes(x=scaled_CoralSec_A,y=0))

#Sand/Rubble Cover
newdata <- new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
newdata$scaled_SAND_RUB <- seq(min(new.df$scaled_SAND_RUB),max(new.df$scaled_SAND_RUB),
                               by=round(rg(new.df$scaled_SAND_RUB),4)/nrow(new.df))
newdata$scaled_EMA_MA <- mean(new.df$scaled_EMA_MA)
newdata$scaled_WavePower <- mean(new.df$scaled_WavePower)
newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
newdata$scaled_logHumanDen <- mean(new.df$scaled_logHumanDen)
newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
newdata$scaled_YearSinceDHW4<-mean(new.df$scaled_YearSinceDHW4)

sandrub.plot<-Predictplot(best.mod,newdata,"SAND_RUB","scaled_SAND_RUB","% Unconsolidated Cover","#009E73",10)+
  geom_rug(data=new.df,mapping=aes(x=scaled_SAND_RUB,y=0))

#Macroalgae Cover
newdata <- new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
newdata$scaled_SAND_RUB <- mean(newdata$scaled_SAND_RUB)
newdata$scaled_EMA_MA <- seq(min(new.df$scaled_EMA_MA),max(new.df$scaled_EMA_MA),
                             by=round(rg(new.df$scaled_EMA_MA),7)/nrow(new.df))
newdata$scaled_WavePower <- mean(new.df$scaled_WavePower)
newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
newdata$scaled_logHumanDen <- mean(new.df$scaled_logHumanDen)
newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
newdata$scaled_YearSinceDHW4<-mean(new.df$scaled_YearSinceDHW4)

ma.plot<-Predictplot(best.mod,newdata,"EMA_MA","scaled_EMA_MA","% Macroalgae Cover","#009E73",10)+
  geom_rug(data=new.df,mapping=aes(x=scaled_EMA_MA,y=0))


#LogHumanDensity
newdata <- new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
newdata$scaled_EMA_MA <- mean(new.df$scaled_EMA_MA)
newdata$scaled_WavePower <- mean(new.df$scaled_WavePower)
newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
newdata$scaled_logHumanDen <- seq(min(new.df$scaled_logHumanDen),max(new.df$scaled_logHumanDen),
                                  by=round(rg(new.df$scaled_logHumanDen),4)/nrow(new.df))
newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
newdata$scaled_YearSinceDHW4<-mean(new.df$scaled_YearSinceDHW4)

human.plot<-Predictplot(best.mod,newdata,"logHumanDen","scaled_logHumanDen",expression(bold('Log Human Density km'^-2)),"#009E73",0.5)+
  geom_rug(data=new.df,mapping=aes(x=scaled_logHumanDen,y=0))

#Wave Power
newdata <- new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
newdata$scaled_EMA_MA <- mean(new.df$scaled_EMA_MA)
newdata$scaled_WavePower <- seq(min(new.df$scaled_WavePower),max(new.df$scaled_WavePower),
                                by=round(rg(new.df$scaled_WavePower),5)/nrow(new.df))
newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
newdata$scaled_logHumanDen <- mean(new.df$scaled_logHumanDen)
newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
newdata$scaled_YearSinceDHW4<-mean(new.df$scaled_YearSinceDHW4)

wave.plot<-Predictplot(best.mod,newdata,"WavePower","scaled_WavePower","Wave Power","black", 60000)+
  geom_rug(data=new.df,mapping=aes(x=scaled_WavePower,y=0))

#Year Since DHW4 
newdata <- new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
newdata$scaled_EMA_MA <- mean(new.df$scaled_EMA_MA)
newdata$scaled_WavePower <- mean(new.df$scaled_WavePower)
newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
newdata$scaled_logHumanDen <- mean(new.df$scaled_logHumanDen)
newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
newdata$scaled_YearSinceDHW4<-seq(min(new.df$scaled_YearSinceDHW4),max(new.df$scaled_YearSinceDHW4),
                                  by=round(rg(new.df$scaled_YearSinceDHW4),6)/nrow(new.df))

tsdhw.plot<-Predictplot(best.mod,newdata,"YearSinceDHW4","scaled_YearSinceDHW4","Years Since Heat Stress Event","black",2)+
  geom_rug(data=new.df,mapping=aes(x=scaled_YearSinceDHW4,y=0))


# save full plot
setwd("T:/Benthic/Projects/Juvenile Project/Figures/Drivers/")
# ytitle <- text_grob("Predicted Juvenile Colonies/m2", size = 18, face = "bold", rot = 90)

ytitle <- text_grob(expression(bold(paste("Predicted Juvenile Colonies",m^-2))), size = 18, face = "bold", rot = 90)
png(width = 1050, height = 950, filename = "Predictions_2013-19_v3.png")
grid.arrange(arrangeGrob(depth.plot + ggtitle("A)"),
                         coral.plot + ggtitle("B)"),
                         tsdhw.plot + ggtitle("C)"), 
                         coralsec.plot + ggtitle("D)"),
                         ma.plot + ggtitle("E)"),
                         human.plot + ggtitle("F)"), 
                         sandrub.plot + ggtitle("G)"), 
                         hs.plot + ggtitle("H)"),
                         wave.plot + ggtitle("I)"),
                         nrow = 3), 
             nrow = 2, heights = c(10,1),
             left = ytitle)
dev.off()


#Figure for Reef Futures Presentation
setwd("T:/Benthic/Projects/Juvenile Project/Figures/Drivers/")
ytitle <- text_grob(expression(bold(paste("Predicted Juvenile Colonies",m^-2))), size = 18, face = "bold", rot = 90)
png(width = 1050, height = 750, filename = "Predictions_2013-19_ReefFutures.png")
grid.arrange(arrangeGrob(depth.plot,
                         coral.plot,
                         coralsec.plot,
                         ma.plot,
                         human.plot, 
                         sandrub.plot, 
                         nrow = 2), 
             nrow = 2, heights = c(10,1),
             left = ytitle)
dev.off()



# Visualizing Interactions----------------------------------------

#Heat Stress Severity x year since heat stress event
r <- subset(new.df,YearSinceDHW4<=3)
newdata1<-r
newdata1$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata1$scaled_CORAL <- mean(r$scaled_CORAL)
newdata1$scaled_CoralSec_A <- mean(r$scaled_CoralSec_A)
newdata1$scaled_SAND_RUB <- mean(r$scaled_SAND_RUB)
newdata1$scaled_EMA_MA <- mean(r$scaled_EMA_MA)
newdata1$scaled_WavePower <- mean(r$scaled_WavePower)
newdata1$scaled_Depth_Median<- mean(r$scaled_Depth_Median)
newdata1$scaled_logHumanDen <- mean(r$scaled_logHumanDen)
newdata1$scaled_CVsst <- mean(r$scaled_CVsst)
newdata1$scaled_MeanDHW10<-seq(min(r$scaled_MeanDHW10),max(r$scaled_MeanDHW10),
                              by=round(rg(r$scaled_MeanDHW10),3)/nrow(r))
newdata1$scaled_YearSinceDHW4<-mean(r$scaled_YearSinceDHW4)


o <- subset(new.df,YearSinceDHW4>3 & YearSinceDHW4 <=15)
newdata2<-o
newdata2$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata2$scaled_CORAL <- mean(o$scaled_CORAL)
newdata2$scaled_CoralSec_A <- mean(o$scaled_CoralSec_A)
newdata2$scaled_SAND_RUB <- mean(o$scaled_SAND_RUB)
newdata2$scaled_EMA_MA <- mean(o$scaled_EMA_MA)
newdata2$scaled_WavePower <- mean(o$scaled_WavePower)
newdata2$scaled_Depth_Median<- mean(o$scaled_Depth_Median)
newdata2$scaled_logHumanDen <- mean(o$scaled_logHumanDen)
newdata2$scaled_CVsst <- mean(o$scaled_CVsst)
newdata2$scaled_MeanDHW10<-seq(min(o$scaled_MeanDHW10),max(o$scaled_MeanDHW10),
                              by=round(rg(o$scaled_MeanDHW10),5)/nrow(o))
newdata2$scaled_YearSinceDHW4<-mean(o$scaled_YearSinceDHW4)


m <- subset(new.df,YearSinceDHW4>15)
newdata3<-m
newdata3$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata3$scaled_CORAL <- mean(m$scaled_CORAL)
newdata3$scaled_CoralSec_A <- mean(m$scaled_CoralSec_A)
newdata3$scaled_SAND_RUB <- mean(m$scaled_SAND_RUB)
newdata3$scaled_EMA_MA <- mean(m$scaled_EMA_MA)
newdata3$scaled_WavePower <- mean(m$scaled_WavePower)
newdata3$scaled_Depth_Median<- mean(m$scaled_Depth_Median)
newdata3$scaled_logHumanDen <- mean(m$scaled_logHumanDen)
newdata3$scaled_CVsst <- mean(m$scaled_CVsst)
newdata3$scaled_MeanDHW10<-seq(min(m$scaled_MeanDHW10),max(m$scaled_MeanDHW10),
                               by=round(rg(m$scaled_MeanDHW10),4)/nrow(m))
newdata3$scaled_YearSinceDHW4<-mean(m$scaled_YearSinceDHW4)




p <- predict(best.mod, newdata = newdata1, type = "response",se.fit=TRUE)
p<-as.data.frame(p)
colnames(p)<-c("Predicted_Juv","SE_Juv")
newdata1<-cbind(newdata1,p)
newdata1$Predict.lwr <- newdata1$Predicted_Juv - 1.96 * newdata1$SE_Juv # confidence interval upper bound
newdata1$Predict.upr <- newdata1$Predicted_Juv + 1.96 * newdata1$SE_Juv # confidence interval lower bound
newdata1$HSts_cat<-"0-3 years"

p <- predict(best.mod, newdata = newdata2, type = "response",se.fit=TRUE)
p<-as.data.frame(p)
colnames(p)<-c("Predicted_Juv","SE_Juv")
newdata2<-cbind(newdata2,p)
newdata2$Predict.lwr <- newdata2$Predicted_Juv - 1.96 * newdata2$SE_Juv # confidence interval upper bound
newdata2$Predict.upr <- newdata2$Predicted_Juv + 1.96 * newdata2$SE_Juv # confidence interval lower bound
newdata2$HSts_cat<-"3-15 years"

p <- predict(best.mod, newdata = newdata3, type = "response",se.fit=TRUE)
p<-as.data.frame(p)
colnames(p)<-c("Predicted_Juv","SE_Juv")
newdata3<-cbind(newdata3,p)
newdata3$Predict.lwr <- newdata3$Predicted_Juv - 1.96 * newdata3$SE_Juv # confidence interval upper bound
newdata3$Predict.upr <- newdata3$Predicted_Juv + 1.96 * newdata3$SE_Juv # confidence interval lower bound
newdata3$HSts_cat<-"> 15 years"

#Merge into 1 dataframe and add column for each HSts category. 
all.newdata<-rbind(newdata1,newdata2,newdata3)


#Unscaling predictor to plot on x axis
att <- attributes(scale(new.df$MeanDHW10))
mylabels <- seq(0,14,2)
mybreaks <- scale(mylabels, att$`scaled:center`, att$`scaled:scale`)[,1]

colors<-c("cyan4","purple3","goldenrod2")

#Reorder HSts variables
all.newdata$HSts_cat <- factor(all.newdata$HSts_cat, levels = c("0-3 years","3-15 years","> 15 years"))
all.newdata<- all.newdata[order(all.newdata$HSts_cat),];head(all.newdata)



#Plot
plot1<-ggplot() +
  geom_line(data=all.newdata,aes(x = scaled_MeanDHW10, y = Predicted_Juv,color=HSts_cat),size=1) +
  geom_ribbon(data = all.newdata,aes(x = scaled_MeanDHW10,ymin = Predict.lwr, ymax = Predict.upr,fill=HSts_cat),alpha = 0.1)+
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key.size = unit(1.5, 'cm'),
    legend.text = element_text(size=18),
    text = element_text(size = 18),
    panel.grid = element_blank()
  ) +
  ylab(expression(bold(paste("Predicted Juvenile Colonies",m^-2)))) +
  xlab(expression(bold('Mean Max '^o*'C-weeks')))  +
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors)+
  scale_x_continuous(labels = comma(mylabels),breaks=mybreaks)+
  scale_y_continuous(limits=c(0,20))+
  geom_rug(data=new.df,mapping=aes(x=scaled_MeanDHW10,y=0))




#### Wave Power x Heat stress- 
l <- subset(new.df,MeanDHW10<4)

newdata1<-l
newdata1$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata1$scaled_CORAL <- mean(l$scaled_CORAL)
newdata1$scaled_CoralSec_A <- mean(l$scaled_CoralSec_A)
newdata1$scaled_SAND_RUB <- mean(l$scaled_SAND_RUB)
newdata1$scaled_EMA_MA <- mean(l$scaled_EMA_MA)
newdata1$scaled_Depth_Median<- mean(l$scaled_Depth_Median)
newdata1$scaled_logHumanDen <- mean(l$scaled_logHumanDen)
newdata1$scaled_WavePower <- seq(min(l$scaled_WavePower),max(l$scaled_WavePower),
                            by=round(rg(l$scaled_WavePower),4)/nrow(l))
newdata1$scaled_MeanDHW10<-mean(l$scaled_MeanDHW10)
newdata1$scaled_YearSinceDHW4<-mean(l$scaled_YearSinceDHW4)


h <- subset(new.df,MeanDHW10>=4)

newdata2<-h
newdata2$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata2$scaled_CORAL <- mean(h$scaled_CORAL)
newdata2$scaled_CoralSec_A <- mean(h$scaled_CoralSec_A)
newdata2$scaled_SAND_RUB <- mean(h$scaled_SAND_RUB)
newdata2$scaled_EMA_MA <- mean(h$scaled_EMA_MA)
newdata2$scaled_Depth_Median<- mean(h$scaled_Depth_Median)
newdata2$scaled_logHumanDen <- mean(h$scaled_logHumanDen)
newdata2$scaled_WavePower <- seq(min(h$scaled_WavePower),max(h$scaled_WavePower),
                                by=round(rg(h$scaled_WavePower),5)/nrow(h))
newdata2$scaled_MeanDHW10<-mean(h$scaled_MeanDHW10)
newdata2$scaled_YearSinceDHW4<-mean(h$scaled_YearSinceDHW4)



p <- predict(best.mod, newdata = newdata1, type = "response",se.fit=TRUE)
p<-as.data.frame(p)
colnames(p)<-c("Predicted_Juv","SE_Juv")
newdata1<-cbind(newdata1,p)
newdata1$Predict.lwr <- newdata1$Predicted_Juv - 1.96 * newdata1$SE_Juv # confidence interval upper bound
newdata1$Predict.upr <- newdata1$Predicted_Juv + 1.96 * newdata1$SE_Juv # confidence interval lower bound
newdata1$HSsev_cat<-"low"

p <- predict(best.mod, newdata = newdata2, type = "response",se.fit=TRUE)
p<-as.data.frame(p)
colnames(p)<-c("Predicted_Juv","SE_Juv")
newdata2<-cbind(newdata2,p)
newdata2$Predict.lwr <- newdata2$Predicted_Juv - 1.96 * newdata2$SE_Juv # confidence interval upper bound
newdata2$Predict.upr <- newdata2$Predicted_Juv + 1.96 * newdata2$SE_Juv # confidence interval lower bound
newdata2$HSsev_cat<-"high"


#Merge into 1 dataframe and add column for each HStsev category. 
all.newdata<-rbind(newdata1,newdata2)


#Unscaling predictor to plot on x axis
att <- attributes(scale(new.df$WavePower))
mylabels <- seq(0,max(new.df$WavePower),75000)
mybreaks <- scale(mylabels, att$`scaled:center`, att$`scaled:scale`)[,1]

colors<-c("steelblue2","royalblue4")

#Reorder HSts variables
all.newdata$HSsev_cat <- factor(all.newdata$HSsev_cat, levels = c("low","high"))
all.newdata<- all.newdata[order(all.newdata$HSsev_cat),];head(all.newdata)

#Plot
plot2<-ggplot() +
  geom_line(data=all.newdata,aes(x = scaled_WavePower, y = Predicted_Juv,color=HSsev_cat),size=1) +
  geom_ribbon(data = all.newdata,aes(x = scaled_WavePower,ymin = Predict.lwr, ymax = Predict.upr,fill=HSsev_cat),alpha = 0.1)+
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.key.size = unit(1.5, 'cm'),
    legend.text = element_text(size=18),
    text = element_text(size = 18),
    panel.grid = element_blank()
  ) +
  ylab(expression(bold(paste("Predicted Juvenile Colonies",m^-2)))) +
  xlab("Wave Power")  +
  scale_color_manual(labels=c(expression('< 4 '^o*'C-weeks'),expression("">= '4 '^o*'C-weeks')),values = colors)+
  scale_fill_manual(labels=c(expression('< 4 '^o*'C-weeks'),expression("">= '4 '^o*'C-weeks')),values = colors)+
  scale_x_continuous(labels = comma(mylabels),breaks=mybreaks)+
  scale_y_continuous(limits=c(0,20))+
  geom_rug(data=new.df,mapping=aes(x=scaled_WavePower,y=0))

plot2




# save full plot
setwd("T:/Benthic/Projects/Juvenile Project/Figures/Final for Manuscript/")
ytitle <- text_grob(expression(bold(paste("Predicted Juvenile Colonies",m^-2))), size = 18,
                    face = "bold", rot = 90,hjust=0.2)
png(width = 1050, height = 600, filename = "Fig.5.png")
grid.arrange(arrangeGrob(plot1 + ggtitle("A) Time Since Heat Stress x Heat Stress Severity"),
                         plot2 + ggtitle("B) Wave Power x Heat Stress Severity"),
                         nrow = 1), 
             nrow = 2, heights = c(10,1),
             left = ytitle)
dev.off()


# Reef Futures Plot
setwd("T:/Benthic/Projects/Juvenile Project/Figures/Final for Manuscript/")
ytitle <- text_grob(expression(bold(paste("Predicted Juvenile Colonies",m^-2))), size = 18,
                    face = "bold",family = "Gill Sans MT", rot = 90,hjust=0.2)
png(width = 1050, height = 600, filename = "Fig.5_ReefFutures.png")
grid.arrange(arrangeGrob(plot1 + ggtitle("Time Since Heat Stress x Heat Stress Severity"),
                         plot2 + ggtitle("Wave Power x Heat Stress Severity"),
                         nrow = 1), 
             nrow = 2, heights = c(10,1),
             left = ytitle)
dev.off()


