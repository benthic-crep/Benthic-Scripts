#This script conducts the driver analysis using 2013-2019 survey data


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
library(MuMIn)
library(arm)
library(purrr)
library(tibble)
library(splines)
setwd("T:/Benthic/Projects/Juvenile Project")


#LOAD DATA
df<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvDen_Pred_SITE_AllYears.csv")#Combined juvenile delta density and all predictors
jcdG_st<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvProject_STRATA_WITHOUT_MHI2013.csv")
cover_sec<-read.csv("T:/Benthic/Projects/Juvenile Project/BenthicCover_JuvenileProject_Tier1_SECTOR.csv")

#remove columns
df<-subset(df,select=c(DATE_,OBS_YEAR,REGION,ISLAND,SEC_NAME,DEPTH_BIN,REEF_ZONE,STRATANAME,HABITAT_CODE,SITE,n,NH,sw,TRANSECTAREA_j,JuvColCount,JuvColDen,
                       LATITUDE,Depth_Median,CORAL,CORALst,CCA,SAND_RUB,TURF,EMA_MA, YearSinceDHW4, YearSinceDHW8,DHW.MeanMax_Degree_Heating_Weeks_YR01,
                       DHW.MeanMax_Degree_Heating_Weeks_YR03,DHW.MeanMax_Degree_Heating_Weeks_YR05,DHW.MeanMax_Degree_Heating_Weeks_YR10YR01,
                       DHW.MeanMax_Degree_Heating_Weeks_YR10,DHW.MaxMax_Degree_Heating_Weeks_YR10,
                       DHW.Np10y_Major_Degree_Heating_Weeks_YR10,WavePower,CVsst,CVchla,mean_SST_CRW_Daily_YR10,mean_Chlorophyll_A_VIIRS_Monthly_750m_YR05,
                       sd_SST_CRW_Daily_YR10,sd_Chlorophyll_A_VIIRS_Monthly_750m_YR05,mean_annual_range_SST_CRW_Daily_YR10,HumanDen))

#Combine site level data with sector cover data
cover_sec$OBS_YEAR<-cover_sec$ANALYSIS_YEAR
df<-left_join(df,cover_sec)

df$Area<-df$NH


df$CORAL_sec<-ifelse(df$SEC_NAME=="Baker",23.62746339,df$CORAL_sec)#no 2018 benthic cover sites so using fish sector data
df$CoralSec_A<-df$Area*df$CORAL_sec


#Remove 2014 NWHI data because we do not have benthic cover data for this year
REGION_YEAR<-c("NWHI_2014")

table(df$REGION,df$OBS_YEAR)
df$REGION_YEAR<-paste(df$REGION,df$OBS_YEAR,sep="_")
df<-df[df$REGION_YEAR != REGION_YEAR,]
table(df$REGION,df$OBS_YEAR)

#Generate table of date ranges and n for each island
df$OBS_MONTH<-month(df$DATE_)
df$OBS_DAY<-day(df$DATE_)

meta<-df %>%
  group_by(REGION,OBS_YEAR) %>%
  summarize(MinDate=min(DATE_),MaxDate=max(DATE_),n=length(n))

meta<-df %>%
  group_by(REGION,OBS_YEAR) %>%
  summarize(MinMonth=min(OBS_MONTH),MaxMonth=max(OBS_MONTH),
            MinDay=min(OBS_DAY),MaxDay=max(OBS_DAY),
            n=length(n))
meta$DateMin<-paste(meta$MinMonth,meta$MinDay,sep="/")
meta$DateMax<-paste(meta$MaxMonth,meta$MaxDay,meta$OBS_YEAR,sep="/")
meta$DateRange<-paste(meta$DateMin,meta$DateMax,sep=" - ")


meta$DateRange<-paste(meta$MinDate,meta$MaxDate,sep = ",")
meta<-meta[,c("REGION","OBS_YEAR","DateRange","n")]
head(meta)

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
colnames(df)[colnames(df)=="mean_monthly_range_SST_CRW_Daily_YR10"]<-"SST_Range"

#Remove row with NAs for Chla
nrow(df)
df<-df %>% drop_na(CVchla)
nrow(df)

hist(log10(df$HumanDen+0.5))
df$logHumanDen<-log10(df$HumanDen+0.5)


#Extract predictors and merge with new survey weights dataset
pcols<-c("SITE","CORAL","CoralSec_A","CORALst","CCA","TURF","EMA_MA","SAND_RUB","Depth_Median","LATITUDE","MaxDHW1",
         "MeanDHW3","MeanDHW5","MeanDHW9","MeanDHW10","MaxDHW10","DHW_Freq","CVsst", "CVchla","Meanchla","SDsst","SDchla","MeanSST","WavePower","YearSinceDHW4","logHumanDen")

p<-df[,pcols]

#Combine survey weighted juvenile data and predictors
rcols<-c("OBS_YEAR","REGION","SITE","TRANSECTAREA_j","JuvColCount","n","NH","sw")

r<-df[,rcols]

nrow(r)
r<-left_join(r,p)
nrow(r);View(r)


#Testing for Multicolinarity
which(colnames(r)=="CORAL")
preds<-r[,8:ncol(r)]
# library(GGally)
# ggpairs(preds)

library(car)
car::vif(fit1)

library(corrplot)

par(mfrow=c(1,1))
M = cor(preds)
png(width = 750, height = 750, filename = "T:/Benthic/Projects/Juvenile Project/Figures/Drivers/JuvenilePredictorsCorPlot_AllYears.png")
corrplot(M, method = 'number')
dev.off()

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
plot(new.df$JuvColDen~new.df$scaled_DHW_Freq)

par(mfrow=c(1,1))
plot(new.df$JuvColDen~new.df$YearSinceDHW4)
plot(new.df$JuvColDen~new.df$scaled_CoralSec_A) 
plot(new.df$JuvColDen~new.df$scaled_logHumanDen) 


##Convert Year since HS event into categorical variable
new.df$HS_YN<-ifelse(new.df$YearSinceDHW4<=15,"HS","No_HS")
new.df$YearSinceDHW4<-ifelse(new.df$YearSinceDHW4<=15,new.df$YearSinceDHW4,NA)




#Remove outliers -there are 4 2016 FFS sites that are >40 juvs/m2. When I remove them Sector coral cover is no longer signficant. 
#Keeping them in for now, but will probably remove the points or sector coral cover eventually
# drop.site<-c("FFS-01314","FFS-01288","FFS-01328","FFS-01272")
# new.df<-new.df %>% filter(!SITE %in% drop.site)

#write.csv(new.df,file="T:/Benthic/Projects/Juvenile Project/Data/test.new.df.csv")

# #Backwards Model selection with Wald Tests (similar to LRTs) ------------
data.cols<-c("OBS_YEAR","REGION","ISLAND","SEC_NAME","STRATANAME","SITE","TRANSECTAREA_j","JuvColCount","n","NH","sw","SITE","CORAL","CoralSec_A","CCA","EMA_MA","SAND_RUB","Depth_Median",
                    "MeanDHW5","MeanDHW10","DHW_Freq","CVsst", "CVchla","Meanchla","MeanSST","WavePower","YearSinceDHW4","scaled_CORAL","scaled_CoralSec_A","scaled_CCA","scaled_EMA_MA","scaled_SAND_RUB","scaled_Depth_Median",
"scaled_MeanDHW5","scaled_MeanDHW10","scaled_DHW_Freq","scaled_CVsst", "scaled_CVchla","scaled_Meanchla","scaled_MeanSST","scaled_WavePower","scaled_YearSinceDHW4","scaled_logHumanDen","logHumanDen","HS_YS")

new.df<-new.df[,data.cols]

#StRS design
des<-svydesign(id=~1, strata=~ OBS_YEAR + REGION+ISLAND+SEC_NAME+STRATANAME, weights=~sw,data=new.df)


# Testing for polynomial relationships -----------------------------------------------------

#Testing polynomial realtionships with depth
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


p <- predict(d_poly2, newdata = df.d, type = "response",se.fit=TRUE)
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


p <- predict(d_poly3, newdata = df.d, type = "response",se.fit=TRUE)
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


# Run Models --------------------------------------------------------------

#MeanMaxDHW5 v MeanMax10
# m10<-svyglm(JuvColCount ~  
#                       scaled_MeanDHW10,
#                     design=des, family="poisson",offset=log(TRANSECTAREA_j))
# m5<-svyglm(JuvColCount ~  
#               scaled_MeanDHW5,
#             design=des, family="poisson",offset=log(TRANSECTAREA_j))
# 
# summary(m10);summary(m5)



#Global model with Interactions with MeanMaxDHW10
global.mod1<-svyglm(JuvColCount ~
                       poly(scaled_CORAL,3,raw=TRUE)+ 
                       scaled_CCA+
                       scaled_CoralSec_A*scaled_MeanDHW10 +
                       scaled_EMA_MA +
                       scaled_SAND_RUB +
                       poly(scaled_Depth_Median,2,raw=TRUE)*scaled_MeanDHW10 +
                       scaled_Meanchla*scaled_MeanDHW10 +
                       scaled_CVsst*scaled_MeanDHW10 +
                       scaled_WavePower*scaled_MeanDHW10+
                       scaled_YearSinceDHW4*scaled_MeanDHW10+
                       HS_YN+
                       scaled_logHumanDen*scaled_MeanDHW10,
                     design=des, family="poisson",offset=log(TRANSECTAREA_j))

summary(global.mod1)

#Only option to generate a R2 like metric for these kinds of models
cor(global.mod1$y, fitted(global.mod1))^2

#Backwards model selection
RED.MOD1 <- update(global.mod1, .~. -scaled_MeanDHW10:scaled_logHumanDen) #drop 2-way interaction term
anova(global.mod1, RED.MOD1,method="Wald") #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD1)


RED.MOD2 <- update(RED.MOD1, .~. -scaled_CoralSec_A:scaled_MeanDHW10) #drop 2-way interaction term
anova(RED.MOD1, RED.MOD2) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD2)

RED.MOD3 <- update(RED.MOD2, .~. -scaled_MeanDHW10:poly(scaled_Depth_Median, 2, raw = TRUE)) #drop 2-way interaction term
anova(RED.MOD2, RED.MOD3,test = "Chisq") #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD3)

RED.MOD4 <- update(RED.MOD3, .~. -scaled_WavePower) #drop 2-way interaction term
anova(RED.MOD3, RED.MOD4) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD4)

RED.MOD5 <- update(RED.MOD4, .~. -scaled_CCA) #drop 2-way interaction term
anova(RED.MOD4, RED.MOD5) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD5)

RED.MOD6 <- update(RED.MOD5, .~. -HS_YN) #drop 2-way interaction term
anova(RED.MOD5, RED.MOD6) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD6)

RED.MOD7 <- update(RED.MOD6, .~. -scaled_MeanDHW10:scaled_WavePower) #drop 2-way interaction term
anova(RED.MOD6, RED.MOD7) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD7)

RED.MOD8 <- update(RED.MOD7, .~. -scaled_EMA_MA) #drop 2-way interaction term
anova(RED.MOD7, RED.MOD8) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD8)

RED.MOD9 <- update(RED.MOD8, .~. -scaled_MeanDHW10:scaled_Meanchla) #drop 2-way interaction term
anova(RED.MOD9, RED.MOD8) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD9)

RED.MOD10 <- update(RED.MOD9, .~. -scaled_Meanchla) #drop 2-way interaction term
anova(RED.MOD9, RED.MOD10) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD10)

RED.MOD11 <- update(RED.MOD10, .~. -scaled_SAND_RUB) #drop 2-way interaction term
anova(RED.MOD11, RED.MOD10) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD11)

AIC(RED.MOD9)
AIC(RED.MOD10)
AIC(RED.MOD11)

best.mod<-RED.MOD10
summary(best.mod)

#Only option to generate a R2 like metric for these kinds of models
cor(best.mod$y, fitted(best.mod))^2


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


# sum.co$Variable <- factor(sum.co$Variable, levels = var_ord)
# sum.co <- sum.co[order(factor(sum.co$Variable, levels = var_ord)),]
sum.co$Variable_plot <- factor(c("SST Variability x Heat Stress",
                                 "Depth",
                                 "Time Since Heat Stress x Heat Stress",
                                 "Coral Cover^2",
                                 "Coral Cover",
                                 "Time Since Heat Stress",
                                 "Sector-level Coral Cover",
                                 "Depth^2",
                                 "Human Density",
                                 "Sand and Rubble Cover",
                                 "Coral Cover^3",
                                 "SST Variability",
                                 "Heat Stress"),
                               levels = c("SST Variability x Heat Stress",
                                          "Depth",
                                          "Time Since Heat Stress x Heat Stress",
                                          "Coral Cover^2",
                                          "Coral Cover",
                                          "Time Since Heat Stress",
                                          "Sector-level Coral Cover",
                                          "Depth^2",
                                          "Human Density",
                                          "Sand and Rubble Cover",
                                          "Coral Cover^3",
                                          "SST Variability",
                                          "Heat Stress"))

write.csv(sum.co,file="Density_best.mod2013-19_svyglm_table_v2.csv")


sum.co$Sig <- NA
sum.co <- transform(sum.co,
                    Sig=ifelse(Pr...t..<0.05,"p<0.05","p>0.05"))
#

# sum.co$SigLeg <- NA
# for(i in c(1:nrow(sum.co))){
#   if(sum.co$Pr...t..[i] > 0.05){
#     sum.co$SigLeg[i] <- "NS"
#   }
#   if(sum.co$Pr...t..[i] <= 0.05 & sum.co$Pr...t..[i] > 0.01){
#     sum.co$SigLeg[i] <- "p < 0.05"
#   }
#   if(sum.co$Pr...t..[i] <= 0.01 & sum.co$Pr...t..[i] > 0.001){
#     sum.co$SigLeg[i] <- "p < 0.01"
#   }
#   if(sum.co$Pr...t..[i] <= 0.001){
#     sum.co$SigLeg[i] <- "p < 0.001"
#   }
# }

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
  scale_y_continuous(limits = c(-0.9,0.8)) +
  scale_x_discrete(limits = rev(levels(sum.co$Variable_plot))) +
  scale_color_manual(values = c("#009E73","black"))

var_plot


setwd("T:/Benthic/Projects/Juvenile Project/Figures/Drivers/")
png(width = 750, height = 750, filename = "ParameterEstimates_2013-19_v2.png")
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
      text = element_text(size = 12),
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
newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
newdata$scaled_logHumanDen <- mean(new.df$scaled_logHumanDen)
newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
newdata$scaled_MeanDHW10<-seq(min(new.df$scaled_MeanDHW10),max(new.df$scaled_MeanDHW10),
                              by=round(rg(new.df$scaled_MeanDHW10),5)/nrow(new.df))
newdata$scaled_YearSinceDHW4<-mean(new.df$scaled_YearSinceDHW4)

hs.plot<-Predictplot(best.mod,dat=newdata,"MeanDHW10","scaled_MeanDHW10","Mean Max degC-weeks","black",2)+
        geom_rug(data=new.df,mapping=aes(x=scaled_MeanDHW10,y=0))
  
hs.plot

#Depth
newdata <- new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
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
                            by=round(rg(new.df$scaled_CORAL),5)/nrow(new.df))
newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
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
                                 by=round(rg(new.df$scaled_CoralSec_A),6)/nrow(new.df))
newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
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
                               by=round(rg(new.df$scaled_SAND_RUB),5)/nrow(new.df))
newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
newdata$scaled_logHumanDen <- mean(new.df$scaled_logHumanDen)
newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
newdata$scaled_YearSinceDHW4<-mean(new.df$scaled_YearSinceDHW4)

sandrub.plot<-Predictplot(best.mod,newdata,"SAND_RUB","scaled_SAND_RUB","% Sand & Rubble Cover","#009E73",10)+
  geom_rug(data=new.df,mapping=aes(x=scaled_SAND_RUB,y=0))

#LogHumanDensity
newdata <- new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
newdata$scaled_logHumanDen <- seq(min(new.df$scaled_logHumanDen),max(new.df$scaled_logHumanDen),
                                  by=round(rg(new.df$scaled_logHumanDen),5)/nrow(new.df))
newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
newdata$scaled_YearSinceDHW4<-mean(new.df$scaled_YearSinceDHW4)

human.plot<-Predictplot(best.mod,newdata,"logHumanDen","scaled_logHumanDen","Log Human Density km-2","#009E73",0.5)+
  geom_rug(data=new.df,mapping=aes(x=scaled_logHumanDen,y=0))

#CVsst 
newdata <- new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
newdata$scaled_logHumanDen <- mean(new.df$scaled_logHumanDen)
newdata$scaled_CVsst <- seq(min(new.df$scaled_CVsst),max(new.df$scaled_CVsst),
                            by=round(rg(new.df$scaled_CVsst),5)/nrow(new.df))
newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
newdata$scaled_YearSinceDHW4<-mean(new.df$scaled_YearSinceDHW4)

cvsst.plot<-Predictplot(best.mod,newdata,"CVsst","scaled_CVsst","CV SST","black", 0.02)+
  geom_rug(data=new.df,mapping=aes(x=scaled_CVsst,y=0))

#Year Since DHW4 
newdata <- new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
newdata$scaled_logHumanDen <- mean(new.df$scaled_logHumanDen)
newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
newdata$scaled_YearSinceDHW4<-seq(min(new.df$scaled_YearSinceDHW4),max(new.df$scaled_YearSinceDHW4),
                                  by=round(rg(new.df$scaled_YearSinceDHW4),2)/nrow(new.df))

tsdhw.plot<-Predictplot(best.mod,newdata,"YearSinceDHW4","scaled_YearSinceDHW4","Years Since Heat Stress Event","black",2)+
  geom_rug(data=new.df,mapping=aes(x=scaled_YearSinceDHW4,y=0))

# save full plot
setwd("T:/Benthic/Projects/Juvenile Project/Figures/Drivers/")
ytitle <- text_grob("Predicted Juvenile Colonies/m2", size = 18, face = "bold", rot = 90)
png(width = 1050, height = 950, filename = "Predictions_2013-19_v2.png")
grid.arrange(arrangeGrob(depth.plot + ggtitle("a)"),
                         coral.plot + ggtitle("b)"),
                         tsdhw.plot + ggtitle("c)"), 
                         coralsec.plot + ggtitle("d)"),
                         human.plot + ggtitle("e)"), 
                         sandrub.plot + ggtitle("f)"), 
                         hs.plot + ggtitle("g)"),
                         cvsst.plot + ggtitle("h)"),
                         nrow = 3), 
             nrow = 2, heights = c(10,1),
             left = ytitle)
dev.off()




# Visualizing Interactions- plan B ----------------------------------------
#interaction surfaces just doesn't make sense so I created binned categories for Year Since DHW4 and generated predictions for each category


PredictplotI <- function(mod,origdat=new.df, dat=newdata,us_pred="CORAL",predictor="scaled_CORAL", predictor_name,sigcol="black",bks=2,mx_Y){
  dat$s_X<-dat[,predictor]
  
  p <- predict(mod, newdata = dat, type = "response",se.fit=TRUE)
  p<-as.data.frame(p)
  colnames(p)<-c("Predicted_Juv","SE_Juv")
  dat<-cbind(dat,p)
  dat$Predict.lwr <- dat$Predicted_Juv - 1.96 * dat$SE_Juv # confidence interval upper bound
  dat$Predict.upr <- dat$Predicted_Juv + 1.96 * dat$SE_Juv # confidence interval lower bound
  head(dat)
  
  origdat$X<-origdat[,us_pred]
  mx_val<-max(origdat$X, na.rm = TRUE)
  
  
  #Unscaling predictor to plot on x axis
  att <- attributes(scale(origdat$X))
  mylabels <- seq(0,mx_val,bks)
  mybreaks <- scale(mylabels, att$`scaled:center`, att$`scaled:scale`)[,1]
  
  # set color for line
  #pcol<-ifelse(sum.co$Sig == "p>0.05","black","#3399FF")
  
  
  #Try mapping geom_rug(dat2,aes(x=s_X)) - dat2= new.df
  
  #Plot
  plot1<-ggplot(dat, aes(x = s_X, y = Predicted_Juv)) + 
    geom_line(color=sigcol,size=1) +
    geom_ribbon(data = dat,
                aes(ymin = Predict.lwr, ymax = Predict.upr),
                alpha = 0.1)+
    #geom_rug() +
    theme_bw() +
    theme(
      axis.title.y = element_blank(),
      axis.title = element_text(face = "bold"),
      text = element_text(size = 12),
      panel.grid = element_blank()
    ) +
    ylab("Predicted Juvenile Abudance") +
    xlab(predictor_name)  +
    scale_x_continuous(labels = mylabels,breaks=mybreaks)+
    scale_y_continuous(limits=c(0,max(mx_Y)))
  
  return(plot1)
  
}

new.df$YScat<-ifelse(new.df$YearSinceDHW4<5,"<5 Years Since Heat Stress Event",">5 Years Since Heat Stress Event")


r <- subset(new.df,YearSinceDHW4<=5)
newdata<-r
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- mean(r$scaled_CORAL)
newdata$scaled_CoralSec_A <- mean(r$scaled_CoralSec_A)
newdata$scaled_SAND_RUB <- mean(r$scaled_SAND_RUB)
newdata$scaled_Depth_Median<- mean(r$scaled_Depth_Median)
newdata$scaled_logHumanDen <- mean(r$scaled_logHumanDen)
newdata$scaled_CVsst <- mean(r$scaled_CVsst)
newdata$scaled_MeanDHW10<-seq(min(r$scaled_MeanDHW10),max(r$scaled_MeanDHW10),
                              by=round(rg(r$scaled_MeanDHW10),3)/nrow(r))
newdata$scaled_YearSinceDHW4<-mean(r$scaled_YearSinceDHW4)

recentdhw.plot<-PredictplotI(best.mod,new.df,newdata,"MeanDHW10","scaled_MeanDHW10","Mean Max degC-weeks","#009E73", 2,16)+
  ggtitle("0 - 5 Years Since Heat Stress Event")+
  geom_rug(data=r,mapping=aes(x=scaled_MeanDHW10,y=0))

recentdhw.plot

o <- subset(new.df,YearSinceDHW4>5)
newdata<-o
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- mean(o$scaled_CORAL)
newdata$scaled_CoralSec_A <- mean(o$scaled_CoralSec_A)
newdata$scaled_SAND_RUB <- mean(o$scaled_SAND_RUB)
newdata$scaled_Depth_Median<- mean(o$scaled_Depth_Median)
newdata$scaled_logHumanDen <- mean(o$scaled_logHumanDen)
newdata$scaled_CVsst <- mean(o$scaled_CVsst)
newdata$scaled_MeanDHW10<-seq(min(o$scaled_MeanDHW10),max(o$scaled_MeanDHW10),
                              by=round(rg(o$scaled_MeanDHW10),6)/nrow(o))
newdata$scaled_YearSinceDHW4<-mean(o$scaled_YearSinceDHW4)

olddhw.plot<-PredictplotI(best.mod,new.df,newdata,"MeanDHW10","scaled_MeanDHW10","Mean Max degC-weeks","#009E73", 0.5,16)+
  ggtitle("5-15 Years Since Heat Stress Event")+
  geom_rug(data=o,mapping=aes(x=scaled_MeanDHW10,y=0))

olddhw.plot

# save full plot
setwd("T:/Benthic/Projects/Juvenile Project/Figures/Drivers/")
ytitle <- text_grob("Predicted Juvenile Colonies/m2", size = 18, face = "bold", rot = 90)
png(width = 1050, height = 600, filename = "HS_YSHS_Predictions_2013-19.png")
grid.arrange(arrangeGrob(recentdhw.plot + ggtitle("a) 0-5 years since heat stress event"),
                         olddhw.plot + ggtitle("b) 5-15 years since heat stress event"),
                         nrow = 1), 
             nrow = 2, heights = c(10,1),
             left = ytitle)
dev.off()


#### CVsst x Heat stress
l <- subset(new.df,CVsst<0.06)
newdata<-l
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- mean(l$scaled_CORAL)
newdata$scaled_CoralSec_A <- mean(l$scaled_CoralSec_A)
newdata$scaled_SAND_RUB <- mean(l$scaled_SAND_RUB)
newdata$scaled_Depth_Median<- mean(l$scaled_Depth_Median)
newdata$scaled_logHumanDen <- mean(l$scaled_logHumanDen)
newdata$scaled_CVsst <- mean(l$scaled_CVsst)
newdata$scaled_MeanDHW10<-seq(min(l$scaled_MeanDHW10),max(l$scaled_MeanDHW10),
                              by=round(rg(l$scaled_MeanDHW10),5)/nrow(l))
newdata$scaled_YearSinceDHW4<-mean(l$scaled_YearSinceDHW4)

lowCV.plot<-PredictplotI(best.mod,new.df,newdata,"MeanDHW10","scaled_MeanDHW10","Mean Max degC-weeks","#009E73", 2,40)+
  ggtitle("Low to moderate CV of SST")+
  geom_rug(data=l,mapping=aes(x=scaled_MeanDHW10,y=0))

lowCV.plot

h <- subset(new.df,CVsst>=0.06)
newdata<-h
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- mean(h$scaled_CORAL)
newdata$scaled_CoralSec_A <- mean(h$scaled_CoralSec_A)
newdata$scaled_SAND_RUB <- mean(h$scaled_SAND_RUB)
newdata$scaled_Depth_Median<- mean(h$scaled_Depth_Median)
newdata$scaled_logHumanDen <- mean(h$scaled_logHumanDen)
newdata$scaled_CVsst <- mean(h$scaled_CVsst)
newdata$scaled_MeanDHW10<-seq(min(h$scaled_MeanDHW10),max(h$scaled_MeanDHW10),
                              by=round(rg(h$scaled_MeanDHW10),5)/nrow(h))
newdata$scaled_YearSinceDHW4<-mean(h$scaled_YearSinceDHW4)

highCV.plot<-PredictplotI(best.mod,new.df,newdata,"MeanDHW10","scaled_MeanDHW10","Mean Max degC-weeks","#009E73", 0.5,40)+
  ggtitle("Moderate to high CV of SST")+
  geom_rug(data=h,mapping=aes(x=scaled_MeanDHW10,y=0))

highCV.plot

# save full plot
setwd("T:/Benthic/Projects/Juvenile Project/Figures/Drivers/")
ytitle <- text_grob("Predicted Juvenile Colonies/m2", size = 18, face = "bold", rot = 90)
png(width = 1050, height = 600, filename = "HS_CVsst_Predictions_2013-19.png")
grid.arrange(arrangeGrob(lowCV.plot + ggtitle("a) Low to moderate CV of SST"),
                         highCV.plot + ggtitle("b) Moderate to high CV of SST"),
                         nrow = 1), 
             nrow = 2, heights = c(10,1),
             left = ytitle)
dev.off()






# Original backwards model selection --------------------------------------

# #Global model with Interactions with MeanMaxDHW10
# global.mod10<-svyglm(JuvColCount ~
#                        poly(scaled_CORAL,3,raw=TRUE)+ 
#                        scaled_CCA+
#                        scaled_CoralSec_A*scaled_MeanDHW10 +
#                        scaled_EMA_MA +
#                        scaled_SAND_RUB +
#                        poly(scaled_Depth_Median,2,raw=TRUE)*scaled_MeanDHW10 +
#                        scaled_CVsst*scaled_MeanDHW10 +
#                        scaled_WavePower*scaled_MeanDHW10+
#                        scaled_YearSinceDHW4*scaled_MeanDHW10+
#                        scaled_logHumanDen*scaled_MeanDHW10,
#                     design=des, family="poisson",offset=log(TRANSECTAREA_j))
# 
# summary(global.mod10)
# 
# 
# #Only option to generate a R2 like metric for these kinds of models
# cor(global.mod10$y, fitted(global.mod10))^2
# 
# #Backwards model selection
# RED.MOD1 <- update(global.mod10, .~. -scaled_MeanDHW10:scaled_logHumanDen) #drop 2-way interaction term
# anova(global.mod10, RED.MOD1,method="Wald") #LRT --> move forward w/ whichever model keeps/removes term
# summary(RED.MOD1)
# 
# 
# RED.MOD2 <- update(RED.MOD1, .~. -scaled_CoralSec_A:scaled_MeanDHW10) #drop 2-way interaction term
# anova(RED.MOD1, RED.MOD2) #LRT --> move forward w/ whichever model keeps/removes term
# summary(RED.MOD2)
# 
# RED.MOD3 <- update(RED.MOD2, .~. -scaled_MeanDHW10:poly(scaled_Depth_Median, 2, raw = TRUE)) #drop 2-way interaction term
# anova(RED.MOD2, RED.MOD3,test = "Chisq") #LRT --> move forward w/ whichever model keeps/removes term
# summary(RED.MOD3)
# 
# RED.MOD4 <- update(RED.MOD3, .~. -scaled_WavePower) #drop 2-way interaction term
# anova(RED.MOD3, RED.MOD4) #LRT --> move forward w/ whichever model keeps/removes term
# summary(RED.MOD4)
# 
# RED.MOD5 <- update(RED.MOD4, .~. -scaled_MeanDHW10:scaled_WavePower) #drop 2-way interaction term
# anova(RED.MOD4, RED.MOD5) #LRT --> move forward w/ whichever model keeps/removes term
# summary(RED.MOD5)
# 
# RED.MOD6 <- update(RED.MOD5, .~. -scaled_CCA) #drop 2-way interaction term
# anova(RED.MOD5, RED.MOD6) #LRT --> move forward w/ whichever model keeps/removes term
# summary(RED.MOD6)
# 
# RED.MOD7 <- update(RED.MOD6, .~. -scaled_EMA_MA) #drop 2-way interaction term
# anova(RED.MOD6, RED.MOD7) #LRT --> move forward w/ whichever model keeps/removes term
# summary(RED.MOD7)
# 
# RED.MOD8 <- update(RED.MOD7, .~. -scaled_SAND_RUB) #drop 2-way interaction term
# anova(RED.MOD7, RED.MOD8) #LRT --> move forward w/ whichever model keeps/removes term
# summary(RED.MOD8)
# 
# AIC(RED.MOD5)
# AIC(RED.MOD6)
# AIC(RED.MOD7)
# AIC(RED.MOD8)
# 
# best.mod<-RED.MOD7
# summary(best.mod)
# 
# 
# #Only option to generate a R2 like metric for these kinds of models
# cor(best.mod$y, fitted(best.mod))^2
# 
# # PARAMETER ESTIMATES +/- SE ----------------------------------------------
# 
# sum <- summary(best.mod)
# sum.co <- data.frame(sum$coefficients)
# sum.co$Variable <- rownames(sum.co)
# sum.co <- data.frame(sum.co, row.names = NULL)
# sum.co <- sum.co[ order(abs(sum.co$Estimate), decreasing = T),]
# var_ord <- sum.co$Variable
# 
# 
# sum.co$lwr.CI <- sum.co$Estimate - 1.96 * sum.co$Std..Error # confidence interval lower bound
# sum.co$upr.CI <- sum.co$Estimate + 1.96 * sum.co$Std..Error # confidence interval upper bound
# head(sum.co)
# 
# sum.co <- sum.co[ which(sum.co$Variable != "(Intercept)"),]
# 
# 
# # sum.co$Variable <- factor(sum.co$Variable, levels = var_ord)
# # sum.co <- sum.co[order(factor(sum.co$Variable, levels = var_ord)),]
# sum.co$Variable_plot <- factor(c("SST Variability x Heat Stress",
#                                  "Time Since Heat Stress x Heat Stress",
#                                  "Depth",
#                                  "Coral Cover^2",
#                                  "Time Since Heat Stress",
#                                  "Coral Cover",
#                                  "Sector-level Coral Cover",
#                                  "Depth^2",
#                                  "Human Density",
#                                  "Sand and Rubble Cover",
#                                  "Heat Stress",
#                                  "Coral Cover^3",
#                                  "SST Variability"), 
#                                levels = c("SST Variability x Heat Stress",
#                                           "Time Since Heat Stress x Heat Stress",
#                                           "Depth",
#                                           "Coral Cover^2",
#                                           "Time Since Heat Stress",
#                                           "Coral Cover",
#                                           "Sector-level Coral Cover",
#                                           "Depth^2",
#                                           "Human Density",
#                                           "Sand and Rubble Cover",
#                                           "Heat Stress",
#                                           "Coral Cover^3",
#                                           "SST Variability"))
# 
# write.csv(sum.co,file="Density_best.mod2013-19_svyglm_table.csv")
# 
# 
# sum.co$Sig <- NA
# sum.co <- transform(sum.co, 
#                     Sig=ifelse(Pr...t..<0.05,"p<0.05","p>0.05"))
# # 
# 
# # sum.co$SigLeg <- NA
# # for(i in c(1:nrow(sum.co))){
# #   if(sum.co$Pr...t..[i] > 0.05){
# #     sum.co$SigLeg[i] <- "NS"
# #   }
# #   if(sum.co$Pr...t..[i] <= 0.05 & sum.co$Pr...t..[i] > 0.01){
# #     sum.co$SigLeg[i] <- "p < 0.05"
# #   }
# #   if(sum.co$Pr...t..[i] <= 0.01 & sum.co$Pr...t..[i] > 0.001){
# #     sum.co$SigLeg[i] <- "p < 0.01"
# #   }
# #   if(sum.co$Pr...t..[i] <= 0.001){
# #     sum.co$SigLeg[i] <- "p < 0.001"
# #   }
# # }
# 
# var_plot <- 
#   ggplot(sum.co,aes(x = reorder(Variable_plot,Estimate), y = Estimate)) + 
#   geom_point(aes(color = Sig),size = 3) +
#   geom_hline(yintercept = 0, color = "lightgray") +
#   geom_errorbar(aes(ymin = lwr.CI, ymax = upr.CI,color = Sig), width=.2,
#                 position=position_dodge(.9))  +
#   coord_flip() +
#   theme_bw() +
#   theme(
#     legend.position = "none",
#     panel.grid.major = element_blank(), 
#     panel.grid.minor = element_blank(),
#     panel.spacing = unit(0, "lines"), 
#     panel.background = element_rect(colour = "black", fill = "white"),
#     text = element_text(size = 18)) +
#   xlab("") +
#   ylab("\nParameter Estimate") +
#   scale_y_continuous(limits = c(-0.9,0.8)) +
#   scale_x_discrete(limits = rev(levels(sum.co$Variable_plot))) +
#   scale_color_manual(values = c("#009E73","black"))
# 
# var_plot
# 
# 
# setwd("T:/Benthic/Projects/Juvenile Project/Figures/Drivers/")
# png(width = 750, height = 750, filename = "ParameterEstimates_2013-19.png")
# var_plot
# dev.off()
# 




# 
# # function to predict bleaching and create a plot based on variable of interest
# Predictplot <- function(mod, dat,us_pred="CORAL",predictor="scaled_CORAL", predictor_name,sigcol="black",bks=2){
#   dat$s_X<-dat[,predictor]
#   
#   p <- predict(mod, newdata = dat, type = "response",se.fit=TRUE)
#   p<-as.data.frame(p)
#   colnames(p)<-c("Predicted_Juv","SE_Juv")
#   dat<-cbind(dat,p)
#   dat$Predict.lwr <- dat$Predicted_Juv - 1.96 * dat$SE_Juv # confidence interval upper bound
#   dat$Predict.upr <- dat$Predicted_Juv + 1.96 * dat$SE_Juv # confidence interval lower bound
#   head(dat)
#   
#   dat$X<-dat[,us_pred]
#   mx_val<-max(dat$X, na.rm = TRUE)
#   
#   
#   #Unscaling predictor to plot on x axis
#   att <- attributes(scale(dat$X))
#   mylabels <- seq(0,mx_val,bks)
#   mybreaks <- scale(mylabels, att$`scaled:center`, att$`scaled:scale`)[,1]
#   
#   # set color for line
#   #pcol<-ifelse(sum.co$Sig == "p>0.05","black","#3399FF")
#   
#   
#   #Try mapping geom_rug(dat2,aes(x=s_X)) - dat2= new.df
#   
#   #Plot
#   plot1<-ggplot(dat, aes(x = s_X, y = Predicted_Juv)) + 
#     geom_line(color=sigcol,size=1) +
#     geom_ribbon(data = dat,
#                 aes(ymin = Predict.lwr, ymax = Predict.upr),
#                 alpha = 0.1)+
#     #geom_rug() +
#     theme_bw() +
#     theme(
#       axis.title.y = element_blank(),
#       axis.title = element_text(face = "bold"),
#       text = element_text(size = 12),
#       panel.grid = element_blank()
#     ) +
#     ylab("Predicted Juvenile Abudance") +
#     xlab(predictor_name)  +
#     scale_x_continuous(labels = mylabels,breaks=mybreaks) #This isn't working
#   
#   return(plot1)
#   
# }
# 
# 
# #10yr Meam Max DHW
# newdata <- new.df
# newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
# newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
# newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
# newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
# newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
# newdata$scaled_logHumanDen <- mean(new.df$scaled_logHumanDen)
# newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
# newdata$scaled_MeanDHW10<-seq(min(new.df$scaled_MeanDHW10),max(new.df$scaled_MeanDHW10),
#                               by=round(rg(new.df$scaled_MeanDHW10),5)/nrow(new.df))
# newdata$scaled_YearSinceDHW4<-mean(new.df$scaled_YearSinceDHW4)
# 
# 
# hs.plot<-Predictplot(best.mod,newdata,"MeanDHW10","scaled_MeanDHW10","Mean Max degC-weeks","black",2)
# hs.plot
# #Depth
# newdata <- new.df
# newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
# newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
# newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
# newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
# newdata$scaled_Depth_Median<- seq(min(new.df$scaled_Depth_Median),max(new.df$scaled_Depth_Median),
#                                   by=round(rg(new.df$scaled_Depth_Median),5)/nrow(new.df))
# newdata$scaled_logHumanDen <- mean(new.df$scaled_logHumanDen)
# newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
# newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
# newdata$scaled_YearSinceDHW4<-mean(new.df$scaled_YearSinceDHW4)
# 
# 
# depth.plot<-Predictplot(best.mod,newdata,"Depth_Median","scaled_Depth_Median","Median Depth (m)","#009E73",2)
# 
# #Coral Cover
# newdata <- new.df
# newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
# newdata$scaled_CORAL <- seq(min(new.df$scaled_CORAL),max(new.df$scaled_CORAL),
#                             by=round(rg(new.df$scaled_CORAL),5)/nrow(new.df))
# newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
# newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
# newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
# newdata$scaled_logHumanDen <- mean(new.df$scaled_logHumanDen)
# newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
# newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
# newdata$scaled_YearSinceDHW4<-mean(new.df$scaled_YearSinceDHW4)
# 
# coral.plot<-Predictplot(best.mod,newdata,"CORAL","scaled_CORAL","% Coral Cover","#009E73",10)
# 
# #Sector-level Coral Cover
# newdata <- new.df
# newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
# newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
# newdata$scaled_CoralSec_A <- seq(min(new.df$scaled_CoralSec_A),max(new.df$scaled_CoralSec_A),
#                                  by=round(rg(new.df$scaled_CoralSec_A),6)/nrow(new.df))
# newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
# newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
# newdata$scaled_logHumanDen <- mean(new.df$scaled_logHumanDen)
# newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
# newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
# newdata$scaled_YearSinceDHW4<-mean(new.df$scaled_YearSinceDHW4)
# 
# coralsec.plot<-Predictplot(best.mod,newdata,"CoralSec_A","scaled_CoralSec_A","Sector-level Coral Cover x Area","#009E73",100000)
# summary(df$CoralSec_A)
# 
# #Sand/Rubble Cover
# newdata <- new.df
# newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
# newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
# newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
# newdata$scaled_SAND_RUB <- seq(min(new.df$scaled_SAND_RUB),max(new.df$scaled_SAND_RUB),
#                                by=round(rg(new.df$scaled_SAND_RUB),5)/nrow(new.df))
# newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
# newdata$scaled_logHumanDen <- mean(new.df$scaled_logHumanDen)
# newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
# newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
# newdata$scaled_YearSinceDHW4<-mean(new.df$scaled_YearSinceDHW4)
# 
# sandrub.plot<-Predictplot(best.mod,newdata,"SAND_RUB","scaled_SAND_RUB","% Sand & Rubble Cover","#009E73",10)
# 
# #LogHumanDensity
# newdata <- new.df
# newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
# newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
# newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
# newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
# newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
# newdata$scaled_logHumanDen <- seq(min(new.df$scaled_logHumanDen),max(new.df$scaled_logHumanDen),
#                                   by=round(rg(new.df$scaled_logHumanDen),5)/nrow(new.df))
# newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
# newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
# newdata$scaled_YearSinceDHW4<-mean(new.df$scaled_YearSinceDHW4)
# 
# human.plot<-Predictplot(best.mod,newdata,"logHumanDen","scaled_logHumanDen","Log Human Density km-2","#009E73",0.5)
# 
# #CVsst 
# newdata <- new.df
# newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
# newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
# newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
# newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
# newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
# newdata$scaled_logHumanDen <- mean(new.df$scaled_logHumanDen)
# newdata$scaled_CVsst <- seq(min(new.df$scaled_CVsst),max(new.df$scaled_CVsst),
#                             by=round(rg(new.df$scaled_CVsst),5)/nrow(new.df))
# newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
# newdata$scaled_YearSinceDHW4<-mean(new.df$scaled_YearSinceDHW4)
# 
# cvsst.plot<-Predictplot(best.mod,newdata,"CVsst","scaled_CVsst","CV SST","black", 0.02)
# 
# #Year Since DHW4 
# newdata <- new.df
# newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
# newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
# newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
# newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
# newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
# newdata$scaled_logHumanDen <- mean(new.df$scaled_logHumanDen)
# newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
# newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
# newdata$scaled_YearSinceDHW4<-seq(min(new.df$scaled_YearSinceDHW4),max(new.df$scaled_YearSinceDHW4),
#                                   by=round(rg(new.df$scaled_YearSinceDHW4),6)/nrow(new.df))
# 
# tsdhw.plot<-Predictplot(best.mod,newdata,"YearSinceDHW4","scaled_YearSinceDHW4","Years Since Heat Stress Event","#009E73",2)
# 
# # save legend as separate plot (use the legend from parameter estimate plots!)
# mylegend<-g_legend(ggplot(sum.co, aes(x = Variable_plot, y = Estimate, color = Sig)) + theme(legend.text = element_text(size = 18), legend.title = element_text(size = 18), legend.position = "bottom", legend.direction = "horizontal") +
#                      geom_line() + scale_color_discrete(name = "Significance"))
# 
# 
# # save full plot
# setwd("T:/Benthic/Projects/Juvenile Project/Figures/Drivers/")
# ytitle <- text_grob("Predicted Juvenile Colonies/m2", size = 18, face = "bold", rot = 90)
# png(width = 1050, height = 950, filename = "Predictions_2013-19.png")
# grid.arrange(arrangeGrob(depth.plot + ggtitle("a)"),
#                          coral.plot + ggtitle("b)"),
#                          tsdhw.plot + ggtitle("c)"), 
#                          coralsec.plot + ggtitle("d)"),
#                          human.plot + ggtitle("e)"), 
#                          sandrub.plot + ggtitle("f)"), 
#                          hs.plot + ggtitle("g)"),
#                          cvsst.plot + ggtitle("h)"),
#                          nrow = 3), 
#              nrow = 2, heights = c(10,1),
#              left = ytitle)
# dev.off()



#Global model with Interactions with MeanMaxDHW5 - original way
#Note- poly() uses raw=False as default which means that it's generating orthoginal polynomials. svyglm()doesn't know how to 
#deal with orthogonal polys so you make sure raw = TRUE. Otherwise your parameters estimates won't be correct and you will not be able to generate predictions
# global.mod5<-svyglm(JuvColCount ~  
#                       scaled_CORAL+ 
#                       scaled_CoralSec_A*scaled_MeanDHW5 +
#                       scaled_CCA +
#                       scaled_EMA_MA +
#                       scaled_SAND_RUB +
#                       poly(scaled_Depth_Median,3,raw=TRUE)*scaled_MeanDHW5 +
#                       scaled_MeanSST*scaled_MeanDHW5 +
#                       scaled_CVsst*scaled_MeanDHW5 +
#                       scaled_Meanchla*scaled_MeanDHW5 +
#                       scaled_WavePower*scaled_MeanDHW5+
#                       scaled_YearSinceDHW4*scaled_MeanDHW5+
#                       scaled_DHW_Freq,
#                     design=des, family="poisson",offset=log(TRANSECTAREA_j))
# 
# summary(global.mod5)
# 
# 
# #Backwards model selection
# #Note- for the interaction terms, you will need to keep the main effects in the model if the interaction term is signficant
# RED.MOD1 <- update(global.mod5, .~. -scaled_WavePower) #drop 2-way interaction term
# anova(global.mod5, RED.MOD1,method="Wald") #LRT --> move forward w/ whichever model keeps/removes term
# summary(RED.MOD1)
# 
# RED.MOD2 <- update(RED.MOD1, .~. -scaled_CCA) #drop 2-way interaction term
# anova(RED.MOD1, RED.MOD2) #LRT --> move forward w/ whichever model keeps/removes term
# summary(RED.MOD2)
# 
# RED.MOD3 <- update(RED.MOD2, .~. -scaled_MeanDHW5:poly(scaled_Depth_Median, 3, raw = TRUE)) #drop 2-way interaction term
# anova(RED.MOD2, RED.MOD3,test = "Chisq") #LRT --> move forward w/ whichever model keeps/removes term
# summary(RED.MOD3)
# 
# RED.MOD4 <- update(RED.MOD3, .~. -scaled_CoralSec_A:scaled_MeanDHW5) #drop 2-way interaction term
# anova(RED.MOD3, RED.MOD4) #LRT --> move forward w/ whichever model keeps/removes term
# summary(RED.MOD4)
# 
# RED.MOD5 <- update(RED.MOD4, .~. -scaled_CoralSec_A) #drop 2-way interaction term
# anova(RED.MOD4, RED.MOD5) #LRT --> move forward w/ whichever model keeps/removes term
# summary(RED.MOD5)
# 
# RED.MOD6 <- update(RED.MOD5, .~. -scaled_MeanDHW5:scaled_WavePower) #drop 2-way interaction term
# anova(RED.MOD5, RED.MOD6) #LRT --> move forward w/ whichever model keeps/removes term
# summary(RED.MOD6) 
# 
# RED.MOD7 <- update(RED.MOD6, .~. -scaled_MeanDHW5:scaled_CVsst) #drop 2-way interaction term
# anova(RED.MOD6, RED.MOD7) #LRT --> move forward w/ whichever model keeps/removes term
# summary(RED.MOD7) 
# 
# RED.MOD8 <- update(RED.MOD7, .~. -scaled_DHW_Freq) #drop 2-way interaction term
# anova(RED.MOD7, RED.MOD8) #LRT --> move forward w/ whichever model keeps/removes term
# summary(RED.MOD8) 
# 
# RED.MOD9 <- update(RED.MOD8, .~. -scaled_MeanDHW5:scaled_MeanSST) #drop 2-way interaction term
# anova(RED.MOD8, RED.MOD9) #LRT --> move forward w/ whichever model keeps/removes term
# summary(RED.MOD9)
# 
# RED.MOD10 <- update(RED.MOD9, .~. -scaled_Meanchla) #drop 2-way interaction term
# anova(RED.MOD9, RED.MOD10) #LRT --> move forward w/ whichever model keeps/removes term
# summary(RED.MOD10)
# 
# 
# 
# AIC(RED.MOD8)
# AIC(RED.MOD9)
# AIC(RED.MOD10)
# 
# best.mod5<-RED.MOD9
# summary(best.mod5)


# 
# 
# 
# # Interaction Surfaces- the plots just don't make sense--------------------------------------
# 
# newdata <- new.df
# newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
# newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
# newdata$scaled_CoralSec_A <- mean(new.df$scaled_CoralSec_A)
# newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
# newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
# newdata$scaled_logHumanDen <- mean(new.df$scaled_logHumanDen)
# newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
# newdata$scaled_MeanDHW10<-seq(min(new.df$scaled_MeanDHW10),max(new.df$scaled_MeanDHW10),
#                               by=round(rg(new.df$scaled_MeanDHW10),5)/nrow(new.df))
# newdata$scaled_YearSinceDHW4<-seq(min(new.df$scaled_YearSinceDHW4),max(new.df$scaled_YearSinceDHW4),
#                                   by=round(rg(new.df$scaled_YearSinceDHW4),6)/nrow(new.df))
# 
# 
# m<-best.mod
# v2="scaled_MeanDHW10"
# v1="scaled_YearSinceDHW4"
# #v1="scaled_CVsst"
# d=newdata
# 
# intSurfplot.fun <- function(var1, var2, mod, dat){
#   m <- mod # set final model
#   v1 = var1 # set interaction variable 1
#   v2 = var2 # set interaction variable 2
#   d = dat # set database of observations
#   
#   v1_seq <- seq(from = min(d[[v1]]), to = max(d[[v1]]), length = 100)
#   v2_seq <- seq(from = min(d[[v2]]), to = max(d[[v2]]), length = 100)
#   
#   # generate list of predictors we need calculate mean values for (everything but the two variables specified)
#   sum.co <- data.frame(summary(m)$coefficients)
#   sum.co$Variable <- rownames(sum.co)
#   sum.co <- data.frame(sum.co, row.names = NULL)
#   sum.co <- sum.co[ which(sum.co$Variable != "(Intercept)"),] # remove intercept
#   vars = sum.co$Variable[!grepl(paste0(":", collapse = "|"), sum.co$Variable)] # remove interaction effects
#   vars2 = vars[!(vars %in% c(v1,v2))] # remove v1 and v2
#   
#   # generate new data frame to hold unique values of variable of interest and mean of other predictors
#   new_temp <- setNames(data.frame(matrix(ncol = length(vars), nrow = 100*100)), vars)
#   new_temp[[v1]] <- rep(v1_seq, each = 100)
#   new_temp[[v2]] <- rep(v2_seq, 100)
#   # calculate means of other values of interest
#   for(i in c(1:length(vars2))){
#     col.name <- vars2[i]
#     new_temp[[col.name]] <- mean(d[[col.name]]) # we want the mean of all other other variables - they stay constant while we vary the variable of interest
#   }
#   
#   #Set polynomial variables to mean of scaled metric
#   new_temp$scaled_CORAL<-mean(d$scaled_CORAL)
#   new_temp$scaled_Depth_Median<-mean(d$scaled_Depth_Median)
#   
#   # make predictions of bleaching on new data
#   new_temp$pred_juv <- predict(m, newdata = new_temp)   # add predicted values of bleaching
#   new_temp$pred_juv<-as.numeric(new_temp$pred_juv) #change from svystat to numeric
#   
#   
#   ggplot(new_temp, aes(x = .data[[v1]], y = .data[[v2]])) + 
#     geom_raster(aes(fill=pred_juv)) + 
#     scale_fill_viridis_c(name = "Predicted Juvenile Density") +
#     geom_point(data = new.df, aes(x = .data[[v1]], y = .data[[v2]]),
#                shape = 21, size = 3, stroke = 1, color = "black") +
#     theme(
#       text = element_text(size = 20),
#       axis.text = element_text(color = "black")
#     )
#   
# }
# 



###Figure out how to overlay raw points on the maps
#https://stackoverflow.com/questions/46745319/plotting-fitted-and-original-data-in-r-with-higher-density-of-x-values-for-fitt