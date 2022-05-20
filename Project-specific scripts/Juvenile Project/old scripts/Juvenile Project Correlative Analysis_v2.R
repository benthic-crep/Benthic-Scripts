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
library(gridExtra)
library(ggpubr)
library(lemon)
library(MuMIn)
library(arm)
library(purrr)
library(tibble)

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
preds<-test[,19:ncol(test)]
# library(GGally)
# ggpairs(preds)

library(car)
car::vif(fit1)

library(corrplot)

M = cor(preds)
corrplot(M, method = 'number')

#Dropping turf, latitude,MeanMaxDHW10,MeanMaxDHW03, MaxMaxDHW03,Mean biweekly SST range
preds<-jcdG_stS_last[,19:ncol(jcdG_stS_last)]

preds <- scale(preds, center = T, scale = T);colnames(preds)<-paste("scaled",colnames(preds),sep="_")

new.df<-cbind(jcdG_stS_last,preds)


# Survey weighted GLMs - Density ----------------------------------------------------------
#Using a quasipoisson because the data are highly overdisperse and I have count derived data
des<-svydesign(id=~1, strata=~REGION+ISLAND+SEC_NAME, weights=~sw,data=new.df)


mod1<-svyglm(JuvColDen ~  scaled_CCA + scaled_SAND_RUB + scaled_MeanDepth + scaled_MaxMaxDHW03+
             scaled_Mean_Mon_SST_Range + scaled_MeanWavePower + scaled_meanChla05+scaled_CORAL,design=des, family="quasipoisson")
mod2<-svyglm(JuvColDen ~  scaled_CORAL + scaled_CCA + scaled_SAND_RUB + scaled_MeanDepth + scaled_MaxMaxDHW03+
               scaled_Mean_Mon_SST_Range + scaled_MeanWavePower + scaled_meanChla05,design=des, family="quasipoisson")
mod3<-svyglm(JuvColDen ~  scaled_CORAL + scaled_CCA + scaled_SAND_RUB + scaled_MeanDepth + scaled_MaxMaxDHW03+
               scaled_Mean_Mon_SST_Range + scaled_meanChla05 + scaled_MeanWavePower,design=des, family="quasipoisson")
mod4<-svyglm(JuvColDen ~ scaled_CORAL + scaled_CCA + scaled_SAND_RUB + scaled_MeanDepth + scaled_MaxMaxDHW03+
                scaled_MeanWavePower + scaled_meanChla05 + scaled_Mean_Mon_SST_Range,design=des, family="quasipoisson")
mod5<-svyglm(JuvColDen ~ scaled_CORAL + scaled_CCA + scaled_SAND_RUB + scaled_MeanDepth + 
               scaled_Mean_Mon_SST_Range + scaled_MeanWavePower + scaled_meanChla05 + scaled_MaxMaxDHW03,design=des, family="quasipoisson")
mod6<-svyglm(JuvColDen ~ scaled_CORAL + scaled_CCA + scaled_SAND_RUB +  scaled_MaxMaxDHW03+
               scaled_Mean_Mon_SST_Range + scaled_MeanWavePower + scaled_meanChla05 + scaled_MeanDepth,design=des, family="quasipoisson")
mod7<-svyglm(JuvColDen ~ scaled_CORAL + scaled_CCA +  scaled_MeanDepth + scaled_MaxMaxDHW03+
               scaled_Mean_Mon_SST_Range + scaled_MeanWavePower + scaled_meanChla05 + scaled_SAND_RUB,design=des, family="quasipoisson")
mod8<-svyglm(JuvColDen ~ scaled_CORAL +  scaled_SAND_RUB + scaled_MeanDepth + scaled_MaxMaxDHW03+
               scaled_Mean_Mon_SST_Range + scaled_MeanWavePower + scaled_meanChla05 + scaled_CCA,design=des, family="quasipoisson")

mod9<-svyglm(JuvColDen ~ scaled_MaxMaxDHW03
               ,design=des, family="quasipoisson")


summary(mod1);summary(mod2);summary(mod3);summary(mod4);summary(mod5);summary(mod6);summary(mod7);summary(mod8)
psrsq(mod1);psrsq(mod9)
AIC(mod1)


options(scipen=999) #prevents scientific notation

# modlist<-c(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8)
# 
# # manually select model names
# model_names = c("mod1","mod2")
# 
# # create a list based on models names provided
# list_models = lapply(model_names, get)
# 
# # set names
# names(list_models) = model_names
# 
# 
# mod.comb<-NULL
# for(i in 1:length(list_models)){
#   
#   i = 1
#   mod = list_models[i]
#   sum <- summary(mod)
#   sum.co <- data.frame(sum$coefficients)
#   sum.co$Variable <- rownames(sum.co)
#   sum.co <- data.frame(sum.co[c(9),], row.names = NULL)
#   mod.comb<-rbind(mod.comb,sum.co)
# }
# View(mod.comb)

#Can't figure out how to get the for loop to work, so this is temp fix
ExtractLastMetric<-function(mod){
  sum <- summary(mod)
  sum.co <- data.frame(sum$coefficients)
  sum.co$Variable <- rownames(sum.co)
  sum.co <- data.frame(sum.co[c(9),], row.names = NULL)
  return(sum.co)
}

m1<-ExtractLastMetric(mod1)
m2<-ExtractLastMetric(mod2)
m3<-ExtractLastMetric(mod3)
m4<-ExtractLastMetric(mod4)
m5<-ExtractLastMetric(mod5)
m6<-ExtractLastMetric(mod6)
m7<-ExtractLastMetric(mod7)
m8<-ExtractLastMetric(mod8)

m.all<-rbind(m1,m2,m3,m4,m5,m6,m7,m8)
write.csv(m.all,"Density_svyglm_table.csv")

#Backwards selection with Wald Tests (similar to LRTs)

RED.MOD1 <- update(mod1, .~. -scaled_MeanWavePower) #drop 2-way interaction term
anova(mod1, RED.MOD1) #LRT --> move forward w/ whichever model keeps/removes term

RED.MOD2 <- update(RED.MOD1, .~. -scaled_meanChla05) #drop 2-way interaction term
anova(RED.MOD1, RED.MOD2) #LRT --> move forward w/ whichever model keeps/removes term

RED.MOD3 <- update(RED.MOD2, .~. -scaled_CORAL) #drop 2-way interaction term
anova(RED.MOD2, RED.MOD3) #LRT --> move forward w/ whichever model keeps/removes term

RED.MOD4 <- update(RED.MOD3, .~. -scaled_Mean_Mon_SST_Range) #drop 2-way interaction term
anova(RED.MOD3, RED.MOD4) #LRT --> move forward w/ whichever model keeps/removes term

RED.MOD5 <- update(RED.MOD4, .~. -scaled_MeanDepth) #drop 2-way interaction term
anova(RED.MOD4, RED.MOD5) #LRT --> move forward w/ whichever model keeps/removes term

RED.MOD6 <- update(RED.MOD5, .~. -scaled_SAND_RUB) #drop 2-way interaction term
anova(RED.MOD5, RED.MOD6) #LRT --> move forward w/ whichever model keeps/removes term

RED.MOD7 <- update(RED.MOD6, .~. -scaled_CCA) #drop 2-way interaction term
anova(RED.MOD6, RED.MOD7) #LRT --> move forward w/ whichever model keeps/removes term

#With backwards selection- nothing is signficant
mod1<-svyglm(JuvColDen ~  scaled_CCA + scaled_SAND_RUB + scaled_MeanDepth + scaled_MaxMaxDHW03+
               scaled_Mean_Mon_SST_Range + scaled_MeanWavePower + scaled_meanChla05,design=des, family="quasipoisson")
mod2<-svyglm(JuvColDen ~  scaled_CCA + scaled_Mean_Mon_SST_Range + scaled_MeanWavePower + scaled_meanChla05,design=des, family="quasipoisson")

mod3<-svyglm(JuvColDen ~  scaled_CCA,design=des, family="quasipoisson")

AIC(mod1)
AIC(mod2)
AIC(mod3)


new.df$JuvCount<-new.df$JuvColDen*50
#Model Averaging using MuMin
des<-svydesign(id=~1, strata=~REGION+ISLAND+SEC_NAME, weights=~sw,data=new.df)

global.model<-svyglm(JuvColDen ~  scaled_CCA + scaled_SAND_RUB + scaled_MeanDepth + scaled_MaxMaxDHW03+
                       scaled_Mean_Mon_SST_Range + scaled_MeanWavePower + scaled_meanChla05+scaled_CORAL,design=des, family="quasipoisson")
summary(global.model)

global.model<-svyglm(JuvColDen ~  scaled_CCA + scaled_SAND_RUB + scaled_MeanDepth + scaled_MaxMaxDHW03+
                       scaled_Mean_Mon_SST_Range + scaled_MeanWavePower + scaled_meanChla05+scaled_CORAL,design=des, family="poisson")
summary(global.model)
global.model$aic

global.model<-svyglm(JuvColDen ~  scaled_CCA ,design=des, family="poisson")


#Can't standarize with svyglm
stz.model<-standardize(global.model,standardize.y=F)

model.set<-dredge(global.model)


#Generate a list of all possible models since I can't use dredge
vars <- c("scaled_CCA","scaled_SAND_RUB", "scaled_MeanDepth", "scaled_MaxMaxDHW03",
            "scaled_MeanWavePower","scaled_meanChla05","scaled_CORAL")

models <- list()

for (i in 1:7){
  vc <- combn(vars,i)
  for (j in 1:ncol(vc)){
    model <- as.formula(paste0("JuvColDen ~", paste0(vc[,j], collapse = "+")))
    models <- c(models, model)
  }
}
#models

mods<-lapply(models, function(x) svyglm(x,design=des,family="quasipoisson"))
aics<-lapply(models, function(x) as.data.frame(AIC(svyglm(x,design=des,family="quasipoisson")))[2,]) #calcaulte AIC values for each model and consolidate them into a list
minAIC<- Reduce(min,aics) #smallest AIC values of all models

delAIC<-lapply(aics, function(x, minval) x- minval, minval = minAIC) #calculate deltaAIC values

ll<-lapply(delAIC, function(x) exp(-0.5*x))
sum_ll<-Reduce(sum,ll)
AICw<-lapply(ll, function(x,sl) exp(-0.5*x)/sl,sl=sum_ll)
mod.w<-unlist(AICw, use.names = FALSE)
delAIC<-unlist(delAIC, use.names = FALSE)


#Top models
# top.mod<-Filter(function(x) x <2, delAIC) #subset top models (del AIC <2)
# mod.w<-unlist(top.mod, use.names = FALSE)

#Top models- need to figure out how to more easily identify top models in the code above without manually digging through previous lists
#This is a temporary workaround for now
m1<-svyglm(JuvColDen ~ scaled_CCA + scaled_MaxMaxDHW03,design=des, family="quasipoisson")
m2<-svyglm(JuvColDen ~ scaled_MaxMaxDHW03,design=des, family="quasipoisson")
m3<-svyglm(JuvColDen ~ scaled_CCA * scaled_MaxMaxDHW03,design=des, family="quasipoisson")

#extract coefficents and se 
coeff<-c(m1$coefficients,m2$coefficients)
coeff<-as.numeric(as.character(coeff))
SEcoeff<-c(SE(m1),SE(m2))
SEcoeff<-as.numeric(as.character(SEcoeff))
df.mod<-as.numeric(as.character(m1$df.residual))
test<-par.avg(x=coeff,se=SEcoeff,df=df.mod,weight=mod.w)

as.data.frame(summary(m1)$coefficients[,1])
summary(mods)$coefficients[,1:2]

coefs <- lapply(mods, function(x) summary(x)$coefficients[,1:2])
coefs

#convert list of all model coefficents to a dataframe
ID <- seq(1:127)
ID<-paste("mod",ID,sep="")
ID<-as.factor(ID)
all.coefs<-map2(coefs, ID, ~cbind(.x, Model = .y))
head(all.coefs)
all.coefs <- do.call(rbind, all.coefs)
head(all.coefs)

#Convert row names to variables
vars <- rownames(all.coefs)
all.coefs<-as.data.frame(all.coefs)
new.coefs<-cbind(vars,all.coefs)
rownames(new.coefs)<-NULL #remove rownames

new.coefs<-filter(new.coefs,vars !="(Intercept)") #remove the intercept
colnames(new.coefs)[3]<-"Std.Error"
c<-new.coefs %>%
  dplyr::select(vars,Estimate,Model) %>%
  pivot_wider(names_from = vars,values_from = Estimate,values_fill = 0)

e<-new.coefs %>%
  dplyr::select(vars,Std.Error,Model) %>%
  pivot_wider(names_from = vars,values_from = Std.Error,values_fill = 0)

test<-cbind(c,mod.w)

#Partial Regression Plots
m.all$Variable_plot <- factor(c("Coral Cover",
                                 "5 yr Mean Chla",
                                 "Mean Wave Power",
                                 "Mean Mo SST Range",
                                 "3 yr Max DHW",
                                 "Mean Depth",
                                 "Sand and Rubble Cover",
                                 "CCA Cover"), 
                               levels = c("Coral Cover",
                                          "5 yr Mean Chla",
                                          "Mean Wave Power",
                                          "Mean Mo SST Range",
                                          "3 yr Max DHW",
                                          "Mean Depth",
                                          "Sand and Rubble Cover",
                                          "CCA Cover"))

m.all$Sig <- NA
m.all <- transform(m.all, 
                    Sig=ifelse(Pr...t..<0.05,"Significant","Non significant"))

m.all$SigLeg <- NA
for(i in c(1:nrow(m.all))){
  if(m.all$Pr...t..[i] > 0.05){
    m.all$SigLeg[i] <- "NS"
  }
  if(m.all$Pr...t..[i] <= 0.05 & m.all$Pr...t..[i] > 0.01){
    m.all$SigLeg[i] <- "p < 0.05"
  }
  if(m.all$Pr...t..[i] <= 0.01 & m.all$Pr...t..[i] > 0.001){
    m.all$SigLeg[i] <- "p < 0.01"
  }
  if(m.all$Pr...t..[i] <= 0.001){
    m.all$SigLeg[i] <- "p < 0.001"
  }
}



# function to predict bleaching and create a plot based on variable of interest
margResidplot.fun <- function(var, mod, dat){
  m <- mod # set final model
  sum_int_f <- summary(m)$coefficients[ which(row.names(summary(m)$coefficients) == "(Intercept)"),"Estimate"] # intercept of final model
  v = var # set variable of interest
  d = dat
  
  # update model to remove variable of interest & calculate residuals
  var.ints <- grep(v, row.names(summary(m)$coefficients), value = T) # this will give a list elements that the variable appears in and we can use it to update the model
  var.update <- paste(var.ints, collapse = "-") # put the list of elements into one string
  mod.var <- update(m, as.formula(paste0(".~.-",var.update))) # create updated model without the variable of interest & its interaction effects
  d$residuals.var <- residuals(mod.var) # save the residuals from the model
  int_var <- coef(mod.var)[1] # intercept of reduced model without variable of interest
  
  # generate list of predictors we need calculate mean values for
  sum.co <- data.frame(summary(m)$coefficients)
  sum.co$Variable <- rownames(sum.co)
  sum.co <- data.frame(sum.co, row.names = NULL)
  # rem <- sum.co[ which(sum.co$Pr...t.. > 0.05),]$Variable
  sum.co <- sum.co[ which(sum.co$Variable != "(Intercept)"),] # remove intercept
  vars = sum.co$Variable[!grepl(paste0(":", collapse = "|"), sum.co$Variable)] # remove interaction effects if you have them
  
  # generate new data frame to hold unique values of variable of interest and mean of other predictors
  new_temp <- setNames(data.frame(matrix(ncol = length(vars), nrow = 0)), vars)
  add <- as.data.frame(unique(d[v])) # get unique values of variable of interest
  new_temp <- merge(new_temp, add, all = TRUE) # add in unique values of variable of interest
  # calculate means of other values of interest
  for(i in c(1:length(vars))){
    if(vars[i] != v){
      col.name <- vars[i]
      new_temp[[col.name]] <- mean(d[[col.name]]) # we want the mean of all other other variables - they stay constant while we vary the variable of interest
    }
  }
  
  # make predictions of juveniles on new data
  p <- predict(m, newdata = new_temp, interval = "confidence")   # add predicted values of juvenile density
  new_temp<-cbind(new_temp,p)
  new_temp$Predict.fit <- new_temp$link
  new_temp$Predict.lwr <- new_temp$Predict.fit + 1.96 * new_temp$SE # confidence interval upper bound
  new_temp$Predict.upr <- new_temp$Predict.fit - 1.96 * new_temp$SE # confidence interval lower bound
  
  
  print(head(new_temp,10)) # check that newdata generated properly
  
  # set color for line
  pcol <- NA
  pv <- summary(m)$coefficients[,4][[v]]
  
  if(pv > 0.05){pcol <- "#F8766D"}
  if(pv <= 0.05 & pv > 0.01){
    pcol <- "#C77CFF"
  }
  if(pv <= 0.01 & pv > 0.001){
    pcol <- "#00BFC4"
  }
  if(pv <= 0.001){
    pcol <- "#7CAE00"
  }
  
  ggplot() + 
    geom_point(data=d, aes(x = .data[[v]], y =residuals.var)) + 
    geom_ribbon(data=new_temp, aes(x=.data[[v]], ymin=Predict.lwr-int_var, ymax=Predict.upr-int_var), alpha= 0.2, fill="black") +
    geom_line(data=new_temp, aes(x= .data[[v]], y=Predict.fit-int_var), color=pcol, size = 1) +
    theme_bw() +
    theme(
      axis.title.y = element_blank(),
      axis.title = element_text(face = "bold"),
      text = element_text(size = 18),
      panel.grid = element_blank()
    ) +
    scale_size_continuous(guide = F) +
    scale_y_continuous(breaks = c(-7.5,-5,-2.5,0,2.5,5,7.5,10), labels = c(-(7.5^2), -(5^2), -(2.5^2), 0, (2.5^2), (5^2), (7.5^2), (10^2))) 
}

# coral_mn <- mean(new.df$CORAL)
# coral_sd <- sd(new.df$CORAL)
# dhw1_mn <- mean(hcbc_complete$DHW.MeanMax.YR01_mn)
# dhw1_sd <- sd(hcbc_complete$DHW.MeanMax.YR01_mn)
# dhw10_mn <- mean(hcbc_complete$DHW.MeanMax.YR10YR01_mn)
# dhw10_sd <- sd(hcbc_complete$DHW.MeanMax.YR10YR01_mn)
# sus_mn <- mean(log(hcbc_complete$SiteSuscp_mn))
# sus_sd <- sd(log(hcbc_complete$SiteSuscp_mn))
# dep_mn <- mean(hcbc_complete$Depth_ft_mn)
# dep_sd <- sd(hcbc_complete$Depth_ft_mn)
# hist_mn <- mean(hcbc_complete$PctBleached_hist_mn)
# hist_sd <- sd(hcbc_complete$PctBleached_hist_mn)
# eff_mn <- mean(sqrt(hcbc_complete$TotalEffluent_mn))
# eff_sd <- sd(sqrt(hcbc_complete$TotalEffluent_mn))
# urb_mn <- mean(sqrt(hcbc_complete$Urban_runoff_mn))
# urb_sd <- sd(sqrt(hcbc_complete$Urban_runoff_mn))
# tou_mn <- mean(log(hcbc_complete$TourRec_10yrAvgPUD_mn))
# tou_sd <- sd(log(hcbc_complete$TourRec_10yrAvgPUD_mn))


coral_resid_plot <- margResidplot.fun("scaled_CORAL", mod1, new.df) + xlab("% Coral Cover") #+ scale_x_continuous(breaks = c((42.5-coral_mn)/coral_sd, (45-coral_mn)/par_sd, (47.5-par_mn)/par_sd), labels = c(42.5, 45, 47.5))  # unscale x axis
cca_resid_plot <-  margResidplot.fun("scaled_CCA", mod1, new.df) + xlab("% CCA Cover") #+ scale_x_continuous(breaks = c((0-dhw1_mn)/dhw1_sd, (3-dhw1_mn)/dhw1_sd, (6-dhw1_mn)/dhw1_sd, (9-dhw1_mn)/dhw1_sd), labels = c(0,3,6,9)) 
maxdhw3_resid_plot <-  margResidplot.fun("scaled_MaxMaxDHW03", mod1, new.df) + xlab("3yr Max Thermal Stress (DHW)") #+ scale_x_continuous(breaks = c((0-dhw10_mn)/dhw10_sd, (3-dhw10_mn)/dhw10_sd, (6-dhw10_mn)/dhw10_sd, (9-dhw10_mn)/dhw10_sd, (12-dhw10_mn)/dhw10_sd), labels = c(0,3,6,9,12)) 
depth_resid_plot <- margResidplot.fun("scaled_MeanDepth", mod1, new.df) + xlab("Mean Depth (m)") #+  scale_x_continuous(breaks = c((log(2)-sus_mn)/sus_sd, (log(2.5)-sus_mn)/sus_sd, (log(3)-sus_mn)/sus_sd, (log(3.5)-sus_mn)/sus_sd, (log(4)-sus_mn)/sus_sd), labels = seq(2,4,by=.5)) 
wave_resid_plot <- margResidplot.fun("scaled_MeanWavePower", mod1, new.df) + xlab("Wave Power") #+ scale_x_continuous(breaks = c((10-dep_mn)/dep_sd, (30-dep_mn)/dep_sd, (50-dep_mn)/dep_sd, (70-dep_mn)/dep_sd), labels = c(10,30,50,70)) 
chla_resid_plot <-  margResidplot.fun("scaled_meanChla05", mod1, new.df) + xlab("5 yr Mean Chla") #+  scale_x_continuous(breaks = c((10-hist_mn)/hist_sd, (20-hist_mn)/hist_sd, (30-hist_mn)/hist_sd, (40-hist_mn)/hist_sd, (50-hist_mn)/hist_sd, (60-hist_mn)/hist_sd), labels = c(10, 20,30,40,50,60))
sstrange_resid_plot <-  margResidplot.fun("scaled_Mean_Mon_SST_Range", mod1, new.df) + xlab("Mean Mo SST Range") #+  scale_x_continuous(breaks = c((10-hist_mn)/hist_sd, (20-hist_mn)/hist_sd, (30-hist_mn)/hist_sd, (40-hist_mn)/hist_sd, (50-hist_mn)/hist_sd, (60-hist_mn)/hist_sd), labels = c(10, 20,30,40,50,60))
sandrub_resid_plot <-  margResidplot.fun("scaled_SAND_RUB", mod1, new.df) + xlab("Mean Sand and Rubble Cover") #+  scale_x_continuous(breaks = c((10-hist_mn)/hist_sd, (20-hist_mn)/hist_sd, (30-hist_mn)/hist_sd, (40-hist_mn)/hist_sd, (50-hist_mn)/hist_sd, (60-hist_mn)/hist_sd), labels = c(10, 20,30,40,50,60))

# save legend as separate plot (use the legend from parameter estimate plots!)
mylegend<-g_legend(ggplot(m.all, aes(x = Variable_plot, y = Estimate, color = SigLeg)) + 
                     theme(legend.text = element_text(size = 18), 
                           legend.title = element_text(size = 18), 
                           legend.position = "bottom", 
                           legend.direction = "horizontal") +
                     geom_line() + scale_color_discrete(name = "Significance"))

# save full plot
setwd("T:/Benthic/Projects/Juvenile Project/Figures")
ytitle <- text_grob("Partial Residual", size = 18, face = "bold", rot = 90)
jpeg(width = 1050, height = 950, filename = "Drivers_PartialResiduals.jpg")
grid.arrange(arrangeGrob(coral_resid_plot + ggtitle("a)"), 
                         cca_resid_plot + ggtitle("b)"),
                         depth_resid_plot + ggtitle("c)"),
                         maxdhw3_resid_plot + ggtitle("d)"), 
                         wave_resid_plot + ggtitle("e)"),
                         chla_resid_plot + ggtitle("f)"), 
                         sandrub_resid_plot + ggtitle("g)"), 
                         sstrange_resid_plot + ggtitle("h)"),
                         nrow = 3), 
             mylegend, nrow = 2, heights = c(10,1),
             left = ytitle)
dev.off()