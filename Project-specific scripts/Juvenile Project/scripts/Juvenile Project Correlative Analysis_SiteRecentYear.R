#This script conducts the driver analysis using the most recent survey data (2017-2019)


rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

#Calculate Difference in range of values
rg<-function(m) {
  max(m, na.rm=TRUE) - min(m, na.rm=TRUE)
}


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
library(gridExtra)
library(ggpubr)
library(lemon)
library(MuMIn)
library(arm)
library(purrr)
library(tibble)

setwd("T:/Benthic/Projects/Juvenile Project")


#LOAD DATA
df<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvDen_Pred_SITE_AllYears.csv")#Combined juvenile delta density and all predictors
jcdG_st<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvProject_STRATA_WITHOUT_MHI2013.csv")
cover_sec<-read.csv("T:/Benthic/Projects/Juvenile Project/BenthicCover_JuvenileProject_Tier1_SECTOR.csv")
cover_sec$OBS_YEAR<-cover_sec$ANALYSIS_YEAR

#remove columns
df<-subset(df,select=c(OBS_YEAR,REGION,ISLAND,SEC_NAME,DEPTH_BIN,REEF_ZONE,STRATANAME,HABITAT_CODE,SITE,n,NH,sw,TRANSECTAREA_j,JuvColCount,JuvColDen,
                       LATITUDE,Depth_Median,CORAL,CORALst,CCA,SAND_RUB,TURF,EMA_MA, YearSinceDHW4, YearSinceDHW8,DHW.MeanMax_Degree_Heating_Weeks_YR01,
                       DHW.MeanMax_Degree_Heating_Weeks_YR03,DHW.MeanMax_Degree_Heating_Weeks_YR05,DHW.MeanMax_Degree_Heating_Weeks_YR10YR01,
                       DHW.MeanMax_Degree_Heating_Weeks_YR10,DHW.MaxMax_Degree_Heating_Weeks_YR10,
                       DHW.Np10y_Major_Degree_Heating_Weeks_YR10,WavePower,CVsst,CVchla,mean_SST_CRW_Daily_YR10,mean_Chlorophyll_A_VIIRS_Monthly_750m_YR05,
                       sd_SST_CRW_Daily_YR10,sd_Chlorophyll_A_VIIRS_Monthly_750m_YR05,mean_annual_range_SST_CRW_Daily_YR10,HumanDen))

#Combine site level data with sector cover data
cover_sec$OBS_YEAR<-cover_sec$ANALYSIS_YEAR
df<-left_join(df,cover_sec)

df$Area<-(df$NH*250)/1000

df$CORAL_sec<-ifelse(df$SEC_NAME=="Baker",23.62746339,df$CORAL_sec)#no 2018 benthic cover sites so using fish sector data
df$CoralSec_A<-df$Area*df$CORAL_sec

#Only include the most recent year
REGION_YEAR<-c("MHI_2019","NWHI_2017","NMARIAN_2017","SMARIAN_2017","PHOENIX_2018","LINE_2018","SAMOA_2018","WAKE_2017")

table(df$REGION,df$OBS_YEAR)
df$REGION_YEAR<-paste(df$REGION,df$OBS_YEAR,sep="_")
df<-df[df$REGION_YEAR %in% REGION_YEAR,]
table(df$REGION,df$OBS_YEAR)


#Dealing with missing Year Since DHW4 data:
#If a site never experienced a >=4 DHW event then set YearSinceDHW4 to the most recent survey date - 1st recorded DHW data (1/1/1985)
# all_pred_site$DATE_<-ymd(all_pred_site$DATE_)
# dhw_start<-ymd("1985-01-01") 
# 
# all_pred_site$YearSinceDHW4<-ifelse(is.na(all_pred_site$YearSinceDHW4),difftime(all_pred_site$DATE_ ,dhw_start, units = c("weeks"))/52,all_pred_site$YearSinceDHW4)
# subset(all_pred_site,SEC_NAME=="OAH_NORTH")

df$YearSinceDHW4<-ifelse(is.na(df$YearSinceDHW4)|df$YearSinceDHW4>15,15,df$YearSinceDHW4)
subset(df,SEC_NAME=="OAH_NORTH")


# #Set up the strata-level data to add first strata level estimate of juv density
# jcdG_st<- mutate_if(jcdG_st, 
#                     is.character, 
#                     str_replace_all, pattern = " ", replacement = "_")
# jcdG_stS<-subset(jcdG_st,GENUS_CODE=="SSSS")
# REGION_YEAR<-c("MHI_2016","NWHI_2015","NMARIAN_2014","SMARIAN_2014","PHOENIX_2015","LINE_2015","SAMOA_2015","WAKE_2014")
# jcdG_stS<-as.data.frame(jcdG_stS)
# 
# jcdG_stS$REGION_YEAR<-paste(jcdG_stS$REGION,jcdG_stS$ANALYSIS_YEAR,sep="_")
# jcdG_stS_first<-jcdG_stS[jcdG_stS$REGION_YEAR %in% REGION_YEAR,]
# head(jcdG_stS_first)

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

#Remove row with NAs
nrow(df)
df<-df %>% drop_na(CVchla)
nrow(df)

View(df)

hist(log10(df$HumanDen+0.5))
df$logHumanDen<-log10(df$HumanDen+0.5)



#Extract predictors and merge with new survey weights dataset
pcols<-c("SITE","CORAL","CoralSec_A","CORALst","CCA","TURF","EMA_MA","SAND_RUB","Depth_Median","LATITUDE","MaxDHW1",
         "MeanDHW3","MeanDHW5","MeanDHW9","MeanDHW10","MaxDHW10","DHW_Freq","CVsst", "CVchla","Meanchla","SDsst","SDchla",
         "MeanSST","WavePower","YearSinceDHW4","logHumanDen")


p<-df[,pcols]

#Combine survey weighted juvenile data and predictors
rcols<-c("REGION","SITE","TRANSECTAREA_j","JuvColCount","n","NH","sw")

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
png(width = 750, height = 750, filename = "T:/Benthic/Projects/Juvenile Project/Figures/Drivers/JuvenilePredictorsCorPlot_2017-19.png")
corrplot(M, method = 'number')
dev.off()


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
plot(new.df$JuvColDen~new.df$MeanDHW5)
plot(new.df$JuvColDen~new.df$Depth_Median)
#plot(new.df$JuvColDen~new.df$LATITUDE)
plot(new.df$JuvColDen~new.df$CVsst)
plot(new.df$JuvColDen~new.df$CVchla) #one outlier in CVchla
#new.df<-subset(new.df,SITE!="GUA-02140")
plot(new.df$JuvColDen~new.df$scaled_CORAL) 
#plot(new.df$JuvColDen~new.df$scaled_CCA) 
plot(new.df$JuvColDen~new.df$scaled_EMA_MA) 
plot(new.df$JuvColDen~new.df$scaled_SAND_RUB) 

plot(new.df$JuvColDen~new.df$scaled_CoralSec_A) 
plot(new.df$JuvColDen~new.df$Meanchla)
plot(new.df$JuvColDen~new.df$scaled_YearSinceDHW4)
plot(new.df$JuvColDen~new.df$YearSinceDHW4)
plot(new.df$JuvColDen~new.df$scaled_DHW_Freq)
plot(new.df$JuvColDen~new.df$logHumanDen)

par(mfrow=c(1,1))
plot(new.df$JuvColDen~new.df$scaled_YearSinceDHW4)



# #Backwards Model selection with Wald Tests (similar to LRTs) ------------
data.cols<-c("REGION","ISLAND","SEC_NAME","STRATANAME","SITE","TRANSECTAREA_j","JuvColCount","n","NH","sw","SITE","CORAL","CoralSec_A","CCA","EMA_MA","SAND_RUB","Depth_Median",
             "MeanDHW5","MeanDHW10","CVsst", "CVchla","Meanchla","MeanSST","WavePower","YearSinceDHW4","scaled_CORAL","scaled_CoralSec_A","scaled_CCA","scaled_EMA_MA","scaled_SAND_RUB","scaled_Depth_Median",
             "scaled_MeanDHW5","scaled_MeanDHW10","scaled_DHW_Freq","scaled_CVsst", "scaled_CVchla","scaled_Meanchla","scaled_MeanSST","scaled_WavePower","scaled_YearSinceDHW4","scaled_logHumanDen")

new.df<-new.df[,data.cols]

#StRS design
des<-svydesign(id=~1, strata=~REGION+ISLAND+SEC_NAME+STRATANAME, weights=~sw,data=new.df)


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

#Plotting 3rd order poly
df.d<-new.df
df.d$scaled_Depth_Median<-seq(-1.61,2.5,by=0.008102767)

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
  theme_bw() +
  ylab("Predicted Juvenile Abudance") +
  xlab("Median Depth (m)")+ 
  ggtitle("Depth with 3rd order Polynomial")+
  scale_x_continuous(labels=mylabels,breaks=mybreaks)


#Coral cover
d_poly3<-svyglm(JuvColCount ~  
                  poly(scaled_CORAL,3),
                design=des, family="poisson",offset=log(TRANSECTAREA_j))

d_poly2<-svyglm(JuvColCount ~  
                  poly(scaled_CORAL,2),
                design=des, family="poisson",offset=log(TRANSECTAREA_j))
d<-svyglm(JuvColCount ~  
            scaled_CORAL,
          design=des, family="poisson",offset=log(TRANSECTAREA_j))

anova(d,d_poly3) #LRT --> move forward w/ whichever model keeps/removes term

AIC(d_poly3)
AIC(d_poly2)
AIC(d)

df.d<-new.df
df.d$scaled_CORAL<-seq(min(df.d$scaled_CORAL),max(df.d$scaled_CORAL),
                                               by=round(rg(df.d$scaled_CORAL),7)/nrow(df.d))

p <- predict(d_poly3, newdata = df.d, type = "response",se.fit=TRUE)
p<-as.data.frame(p)
colnames(p)<-c("Predicted_Juv","SE_Juv")
newdata<-cbind(df.d,p)
newdata$Predict.lwr <- newdata$Predicted_Juv - 1.96 * newdata$SE_Juv # confidence interval upper bound
newdata$Predict.upr <- newdata$Predicted_Juv + 1.96 * newdata$SE_Juv # confidence interval lower bound
head(newdata)


att <- attributes(scale(new.df$CORAL))
mylabels <- seq(0,75,10)
mybreaks <- scale(mylabels, att$`scaled:center`, att$`scaled:scale`)[,1]

#Plot
ggplot(newdata, aes(x = scaled_CORAL, y = Predicted_Juv)) +
  geom_line() +
  geom_ribbon(data = newdata,
              aes(ymin = Predict.lwr, ymax = Predict.upr),
              alpha = 0.1)+
  theme_bw() +
  ylab("Predicted Juvenile Abudance") +
  xlab("% Coral Cover")+ 
  ggtitle("Coral Cover with 3rd order Polynomial")+
  scale_x_continuous(labels=mylabels,breaks=mybreaks)


#I played with polynomials with the other predictors, depth and Coral the only ones that fall out as nonlinear



#Global model with Interactions with depth as polynomial
global.mod<-svyglm(JuvColCount ~  
                      poly(scaled_CORAL,3,raw=TRUE)+ 
                      scaled_CCA+
                      scaled_CoralSec_A*scaled_MeanDHW10 +
                      scaled_EMA_MA +
                      scaled_SAND_RUB +
                      poly(scaled_Depth_Median,3,raw=TRUE)*scaled_MeanDHW10 +
                      scaled_MeanSST*scaled_MeanDHW10 +
                      scaled_CVsst*scaled_MeanDHW10 +
                      scaled_WavePower*scaled_MeanDHW10+
                      scaled_YearSinceDHW4*scaled_MeanDHW10+
                      scaled_logHumanDen*scaled_MeanDHW10,
                    design=des, family="poisson",offset=log(TRANSECTAREA_j))

summary(global.mod)


#Backwards model selection
RED.MOD1 <- update(global.mod, .~. -scaled_MeanDHW10:poly(scaled_Depth_Median, 3,raw=TRUE)) #drop 2-way interaction term
anova(global.mod, RED.MOD1,method="Wald") #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD1)


RED.MOD2 <- update(RED.MOD1, .~. -scaled_CoralSec_A) #drop 2-way interaction term
anova(RED.MOD1, RED.MOD2) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD2)

RED.MOD3 <- update(RED.MOD2, .~. -scaled_YearSinceDHW4) #drop 2-way interaction term
anova(RED.MOD2, RED.MOD3,test = "Chisq") #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD3)

RED.MOD4 <- update(RED.MOD3, .~. -scaled_MeanDHW10:scaled_WavePower) #drop 2-way interaction term
anova(RED.MOD3, RED.MOD4) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD4)

RED.MOD5 <- update(RED.MOD4, .~. -scaled_MeanDHW10:scaled_YearSinceDHW4) #drop 2-way interaction term
anova(RED.MOD4, RED.MOD5) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD5)

RED.MOD6 <- update(RED.MOD5, .~. -scaled_WavePower) #drop 2-way interaction term
anova(RED.MOD5, RED.MOD6) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD6) 

RED.MOD7 <- update(RED.MOD6, .~. -scaled_MeanDHW10:scaled_logHumanDen) #drop 2-way interaction term
anova(RED.MOD6, RED.MOD7) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD7) 

RED.MOD8 <- update(RED.MOD7, .~. -scaled_logHumanDen) #drop 2-way interaction term
anova(RED.MOD7, RED.MOD8) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD8) 

RED.MOD9 <- update(RED.MOD8, .~. -scaled_MeanDHW10:scaled_CoralSec_A) #drop 2-way interaction term
anova(RED.MOD8, RED.MOD9) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD9) 

RED.MOD10 <- update(RED.MOD9, .~. -scaled_CCA) #drop 2-way interaction term
anova(RED.MOD9, RED.MOD10) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD10) 

RED.MOD11 <- update(RED.MOD10, .~. -scaled_SAND_RUB) #drop 2-way interaction term
anova(RED.MOD10, RED.MOD11) #LRT --> move forward w/ whichever model keeps/removes term
summary(RED.MOD11) 


AIC(RED.MOD9)
AIC(RED.MOD10)
AIC(RED.MOD11)

summary(RED.MOD10)
best.mod<-RED.MOD10
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
sum.co$Variable_plot <- factor(c("Heat Stress x SST Variability",
                                 "Coral Cover^2",
                                 "Depth^2",
                                 "Heat Stress",
                                 "Heat Stress x Mean SST",
                                 "Macroalgae Cover",
                                 "Depth^3",
                                 "Coral Cover^3",
                                 "Depth",
                                 "Coral Cover",
                                 "Sand & Rubble Cover",
                                 "Mean SST",
                                 "SST Variability"), 
                               levels = c("Heat Stress x SST Variability",
                                          "Coral Cover^2",
                                          "Depth^2",
                                          "Heat Stress",
                                          "Heat Stress x Mean SST",
                                          "Macroalgae Cover",
                                          "Depth^3",
                                          "Coral Cover^3",
                                          "Depth",
                                          "Coral Cover",
                                          "Sand & Rubble Cover",
                                          "Mean SST",
                                          "SST Variability"))
sum.co

write.csv(sum.co,file="Density_best.mod_2017-19_svyglm_table.csv")


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
  ggplot(sum.co,aes(x = reorder(Variable_plot,abs(Estimate)), y = Estimate)) + 
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
  scale_y_continuous(limits = c(-1.2,0.6)) +
  scale_x_discrete(limits = rev(levels(sum.co$Variable_plot))) +
  scale_color_manual(values = c("#3399FF","black"))

var_plot


setwd("T:/Benthic/Projects/Juvenile Project/Figures/Drivers/")
png(width = 750, height = 750, filename = "ParameterEstimates_RecentSurveys.png")
var_plot
dev.off()



# Model predictions for each predictor holding others constant ------------
# If this is the correct approach then generate a function for all variables


# function to predict bleaching and create a plot based on variable of interest
Predictplot <- function(mod, dat,us_pred="CORAL",predictor="scaled_CORAL", predictor_name,sigcol="black",bks=2){
  dat$s_X<-dat[,predictor]
  
  p <- predict(mod, newdata = dat, type = "response",se.fit=TRUE)
  p<-as.data.frame(p)
  colnames(p)<-c("Predicted_Juv","SE_Juv")
  dat<-cbind(dat,p)
  dat$Predict.lwr <- dat$Predicted_Juv - 1.96 * dat$SE_Juv # confidence interval upper bound
  dat$Predict.upr <- dat$Predicted_Juv + 1.96 * dat$SE_Juv # confidence interval lower bound
  #head(dat)
  
  dat$X<-dat[,us_pred]
  mx_val<-max(dat$X, na.rm = TRUE)
  
  
  #Unscaling predictor to plot on x axis
  att <- attributes(scale(dat$X))
  mylabels <- seq(0,mx_val,bks)
  mybreaks <- scale(mylabels, att$`scaled:center`, att$`scaled:scale`)[,1]
  
  # set color for line
  #pcol<-ifelse(sum.co$Sig == "p>0.05","black","#3399FF")
  
  
  #Plot
  plot1<-ggplot(dat, aes(x = s_X, y = Predicted_Juv)) + 
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
    scale_x_continuous(labels = mylabels,breaks=mybreaks)
    #scale_y_continuous(breaks = seq(0,30,5),limits=c(0,30))
  
  return(plot1)
  
}

#10yr Meam Max DHW
newdata <- new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
newdata$scaled_EMA_MA <- mean(new.df$scaled_EMA_MA)
newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
newdata$scaled_MeanSST <- mean(new.df$scaled_MeanSST)
newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
newdata$scaled_MeanDHW10<-seq(min(new.df$scaled_MeanDHW10),max(new.df$scaled_MeanDHW10),
                             by=round(rg(new.df$scaled_MeanDHW10),5)/nrow(new.df))


hs.plot<-Predictplot(mod=best.mod,dat=newdata,us_pred = "MeanDHW10",predictor = "scaled_MeanDHW10",predictor_name = "Mean of Max degC-weeks",sigcol = "#3399FF",bks=2)
#expression(paste("Mean Juvenile Colonies   ",m^-2))

#Depth
newdata <- new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- mean(new.df$scaled_CORAL)
newdata$scaled_EMA_MA <- mean(new.df$scaled_EMA_MA)
newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
newdata$scaled_MeanSST <- mean(new.df$scaled_MeanSST)
newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
newdata$scaled_Depth_Median<- seq(min(new.df$scaled_Depth_Median),max(new.df$scaled_Depth_Median),
                                  by=round(rg(new.df$scaled_Depth_Median),3)/nrow(new.df))

depth.plot<-Predictplot(mod=best.mod,dat=newdata,us_pred="Depth_Median",predictor="scaled_Depth_Median",predictor_name="Median Depth (m)",sigcol="#3399FF",bks=2)

#Coral Cover
newdata <- new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <-seq(min(new.df$scaled_CORAL),max(new.df$scaled_CORAL),
                           by=round(rg(new.df$scaled_CORAL),4)/nrow(new.df))
newdata$scaled_EMA_MA <- mean(new.df$scaled_EMA_MA)
newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
newdata$scaled_MeanSST <- mean(new.df$scaled_MeanSST)
newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)

coral.plot<-Predictplot(mod=best.mod,dat=newdata,us_pred="CORAL",predictor="scaled_CORAL",predictor_name="% Coral Cover",sigcol="#3399FF",bks=10)

#Macroalgae Cover
newdata <- new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_EMA_MA <- seq(min(new.df$scaled_EMA_MA),max(new.df$scaled_EMA_MA),
                             by=round(rg(new.df$scaled_EMA_MA),4)/nrow(new.df))
newdata$scaled_CORAL <-mean(new.df$scaled_CORAL)
newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
newdata$scaled_MeanSST <- mean(new.df$scaled_MeanSST)
newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)

ma.plot<-Predictplot(mod=best.mod,dat=newdata,us_pred="EMA_MA",predictor="scaled_EMA_MA",predictor_name="% Macroalgae Cover",sigcol="#3399FF",bks=10)


#Sand/Rubble Cover
newdata <- new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_SAND_RUB <- seq(min(new.df$scaled_SAND_RUB),max(new.df$scaled_SAND_RUB),
                               by=round(rg(new.df$scaled_SAND_RUB),4)/nrow(new.df))
newdata$scaled_CORAL <-mean(new.df$scaled_CORAL)
newdata$scaled_EMA_MA <- mean(new.df$scaled_EMA_MA)
newdata$scaled_MeanSST <- mean(new.df$scaled_MeanSST)
newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)

sandrub.plot<-Predictplot(mod=best.mod,dat=newdata,us_pred="SAND_RUB",predictor="scaled_SAND_RUB",predictor_name="% Sand & Rubble Cover",sigcol="#3399FF",bks=10)

#MeanSST 
newdata <- new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_MeanSST <- seq(min(new.df$scaled_MeanSST),max(new.df$scaled_MeanSST),
                              by=round(rg(new.df$scaled_MeanSST),5)/nrow(new.df))
newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
newdata$scaled_CORAL <-mean(new.df$scaled_CORAL)
newdata$scaled_EMA_MA <- mean(new.df$scaled_EMA_MA)
newdata$scaled_CVsst <- mean(new.df$scaled_CVsst)
newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)

meansst.plot<-Predictplot(mod=best.mod,dat=newdata,us_pred="MeanSST",predictor="scaled_MeanSST",predictor_name="Mean SST (degC)",sigcol="black",bks=0.5)


#CVsst 
newdata <- new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_SAND_RUB <- mean(new.df$scaled_SAND_RUB)
newdata$scaled_CORAL <-mean(new.df$scaled_CORAL)
newdata$scaled_EMA_MA <- mean(new.df$scaled_EMA_MA)
newdata$scaled_MeanDHW10<-mean(new.df$scaled_MeanDHW10)
newdata$scaled_Depth_Median<- mean(new.df$scaled_Depth_Median)
newdata$scaled_MeanSST <- mean(new.df$scaled_MeanSST)
newdata$scaled_CVsst <- seq(min(new.df$scaled_CVsst),max(new.df$scaled_CVsst),
                            by=round(rg(new.df$scaled_CVsst),3)/nrow(new.df))

cvsst.plot<-Predictplot(mod=best.mod,dat=newdata,us_pred="CVsst",predictor="scaled_CVsst",predictor_name="CV SST",sigcol="black",bks=0.02)



# save legend as separate plot (use the legend from parameter estimate plots!)
mylegend<-g_legend(ggplot(sum.co, aes(x = Variable_plot, y = Estimate, color = Sig)) + theme(legend.text = element_text(size = 18), legend.title = element_text(size = 18), legend.position = "bottom", legend.direction = "horizontal") +
                     geom_line() + scale_color_discrete(name = "Significance"))


# save full plot
setwd("T:/Benthic/Projects/Juvenile Project/Figures/Drivers/")
ytitle <- text_grob(expression(paste("Juvenile Colonies  ",m^-2)), size = 18, face = "bold", rot = 90)
png(width = 1050, height = 950, filename = "Predictions.png")
grid.arrange(arrangeGrob(coral.plot + ggtitle("a)"),
                         depth.plot + ggtitle("b)"), 
                         hs.plot + ggtitle("c)"),
                         ma.plot + ggtitle("d)"), 
                         sandrub.plot + ggtitle("e)"),
                         meansst.plot + ggtitle("f)"),
                         cvsst.plot + ggtitle("g)"),
                         nrow = 3), 
             nrow = 2, heights = c(10,1),
             left = ytitle)
dev.off()



# Model Averaging- doesn't work for svyglm --------------------------------



# #Model Averaging- doesn't work with dredge
# #Tried several strategies- this doesn't work well for svyglm.No longer using this approach
# des<-svydesign(id=~1, strata=~REGION+ISLAND+SEC_NAME, weights=~sw,data=new.df)
# 
# 
# #Generate a list of all possible models since I can't use dredge
# vars <- c("scaled_CORAL","scaled_CORAL_sec", "scaled_CCA", "scaled_EMA_MA",
#             "scaled_SAND_RUB","scaled_Depth_Median","scaled_MeanDHW10","scaled_LATITUDE",
#           "scaled_WavePower","scaled_YearSinceDHW4")
# 
# models <- list()
# 
# for (i in 1:10){
#   vc <- combn(vars,i)
#   for (j in 1:ncol(vc)){
#     model <- as.formula(paste0("JuvColCount ~", paste0(vc[,j], collapse = "+")))
#     models <- c(models, model)
#   }
# }
# #models
# 
# mods<-lapply(models, function(x) svyglm(x,design=des,family="poisson",offset=log(TRANSECTAREA_j)))
# aics<-lapply(models, function(x) as.data.frame(AIC(svyglm(x,design=des,family="poisson")))[2,]) #calcaulte AIC values for each model and consolidate them into a list
# minAIC<- Reduce(min,aics) #smallest AIC values of all models
# 
# delAIC<-lapply(aics, function(x, minval) x- minval, minval = minAIC) #calculate deltaAIC values
# 
# ll<-lapply(delAIC, function(x) exp(-0.5*x))
# sum_ll<-Reduce(sum,ll)
# AICw<-lapply(ll, function(x,sl) exp(-0.5*x)/sl,sl=sum_ll)
# mod.w<-unlist(AICw, use.names = FALSE)
# delAIC<-unlist(delAIC, use.names = FALSE)
# 
# 
# #Top models
# # top.mod<-Filter(function(x) x <2, delAIC) #subset top models (del AIC <2)
# # mod.w<-unlist(top.mod, use.names = FALSE)
# 
# #Top models- need to figure out how to more easily identify top models in the code above without manually digging through previous lists
# #This is a temporary workaround for now
# # m1<-svyglm(JuvColDen ~ scaled_CCA + scaled_MaxMaxDHW03,design=des, family="quasipoisson")
# # m2<-svyglm(JuvColDen ~ scaled_MaxMaxDHW03,design=des, family="quasipoisson")
# # m3<-svyglm(JuvColDen ~ scaled_CCA * scaled_MaxMaxDHW03,design=des, family="quasipoisson")
# # 
# # #extract coefficents and se 
# # coeff<-c(m1$coefficients,m2$coefficients)
# # coeff<-as.numeric(as.character(coeff))
# # SEcoeff<-c(SE(m1),SE(m2))
# # SEcoeff<-as.numeric(as.character(SEcoeff))
# # df.mod<-as.numeric(as.character(m1$df.residual))
# # test<-par.avg(x=coeff,se=SEcoeff,df=df.mod,weight=mod.w)
# 
# as.data.frame(summary(m1)$coefficients[,1])
# summary(mods)$coefficients[,1:2]
# 
# coefs <- lapply(mods, function(x) summary(x)$coefficients[,1:2])
# coefs
# 
# #convert list of all model coefficents to a dataframe
# ID <- seq(1:1023)
# ID<-paste("mod",ID,sep="")
# ID<-as.factor(ID)
# all.coefs<-map2(coefs, ID, ~cbind(.x, Model = .y))
# head(all.coefs)
# all.coefs <- do.call(rbind, all.coefs)
# head(all.coefs)
# 
# #Convert row names to variables
# vars <- rownames(all.coefs)
# all.coefs<-as.data.frame(all.coefs)
# new.coefs<-cbind(vars,all.coefs);head(new.coefs)
# rownames(new.coefs)<-NULL #remove rownames
# 
# new.coefs<-filter(new.coefs,vars !="(Intercept)") #remove the intercept
# colnames(new.coefs)[3]<-"Std.Error"
# 
# Model <- seq(1:1023)
# Model<-as.data.frame(Model)
# M<-cbind(Model,delAIC,mod.w)
# 
# new.coefs<-left_join(new.coefs,M)
# 
# c<-new.coefs %>%
#   dplyr::select(vars,Estimate,Model) %>%
#   pivot_wider(names_from = vars,values_from = Estimate,values_fill = 0)
# 
# e<-new.coefs %>%
#   dplyr::select(vars,Std.Error,Model) %>%
#   pivot_wider(names_from = vars,values_from = Std.Error,values_fill = 0)
# 
# test<-cbind(c,mod.w)




