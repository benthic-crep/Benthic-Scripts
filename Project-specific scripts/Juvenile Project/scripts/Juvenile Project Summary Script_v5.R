#This script summarizes juvenile data from NCRMP 2013-2019 at the site, stratum, island and sector level
#It also identifies which sectors and strata have been surveyed in all years
#It calculate delta density

#Ends with summarized site-level data between 2015-2019 


rm(list=ls())

#Set Run Flags
DEBUG=TRUE

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")


library(rcompanion)
library(car)
library("TeachingDemos")
library(gridExtra)
library(RColorBrewer)
library(viridis)
library(dplyr)
library(sp)
library(sf)
library(ggsn)
library(ggspatial)
library(ggrepel)
library(rnaturalearth)
library(rgeos)
library(cowplot)
library(survey)
library(svydiags)

setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project")

#LOAD DATA
jwd<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Juveniles_raw_CLEANED.csv")



## LOAD data
# site.data.gen2<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS.csv")

#Tweaks before calculating Site-level data-------------------------------------------------
#Colony fragments and scleractinans are subseted in the functions 
#Add a column for adult fragments so we can remove them from the dataset later (-1 indicates fragment)
jwd$Fragment <- 0 # you need to add this column so that you can use the site level functions correctly
jwd$DATE_ <- as.Date(jwd$DATE_, format = "%Y-%m-%d")
jwd$METHOD<-"DIVER"
jwd$ANALYST<-jwd$DIVER
jwd$SEGAREA<-jwd$SEGLENGTH*jwd$SEGWIDTH

#Create a look a table of all of the colony attributes- you will need this the functions below
SURVEY_SITE<-c("METHOD","MISSIONID","DATE_","SITEVISITID", "ANALYSIS_YEAR","OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE","HABITAT_CODE","REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")
survey_site<-unique(jwd[,SURVEY_SITE])

#TEMPORARY WORK AROUND-ASK MICHAEL TO FIX
jwd$REEF_ZONE<-ifelse(jwd$SITE=="HAW-04285","Forereef",as.character(jwd$REEF_ZONE))

#Convert Protected Slope to Forereef
jwd$REEF_ZONE<-ifelse(jwd$REEF_ZONE=="Protected Slope","Forereef",as.character(jwd$REEF_ZONE))

#Remove 2 sites that weren't surveyed for juveniles
jwd<-jwd[!(jwd$SITE %in% c("OFU-01012","PAG-00596")),]


# 
# #Look at the size class data to determine the min colony size cut off for analysis
# #Subset 2019 Hawaii data
# hi<-subset(jwd,OBS_YEAR=="2017")
# 
# ggplot(hi) + 
#   geom_density(aes(x = COLONYLENGTH, fill = ANALYST), alpha = 0.2)+
#   geom_vline(xintercept=1, color = "black")+
#   facet_wrap(~ISLAND)
# 
# ggplot(hi) + 
#   geom_histogram(aes(x = COLONYLENGTH, fill = ANALYST))+
#   geom_vline(xintercept=1, color = "black")+
#   facet_wrap(~ISLAND)
# 
# ggplot(hi) + 
#   geom_density(aes(x = COLONYLENGTH, fill = ISLAND), alpha = 0.2)+
#   geom_vline(xintercept=1, color = "black")+
#   facet_wrap(~ISLAND)
# 
# s.data<-ddply(hi,.(ISLAND,ANALYST),
#               summarise,
#               min=min(COLONYLENGTH,na.rm=T),
#               mean=mean(COLONYLENGTH,na.rm=T),
#               ncol=length(COLONYLENGTH,na.rm=T))
# 
# s.all<-ddply(jwd,.(ANALYST),
#              summarise,
#              min=min(COLONYLENGTH,na.rm=T),
#              mean=mean(COLONYLENGTH,na.rm=T),
#              ncol=length(COLONYLENGTH))

#There are some people that didn't record anything less than 1cm

#Change colonies that are <1cm to NA. I'm not subsetting these data because I need to keep the placeholder in the dataframe in case a site only had colonies <1cm or >5cm
View(subset(jwd,COLONYLENGTH<1))
nrow(subset(jwd,COLONYLENGTH<1))
nrow(jwd)
jwd$S_ORDER<-ifelse(jwd$COLONYLENGTH<1|jwd$COLONYLENGTH==5,NA,as.character(jwd$S_ORDER))
jwd$GENUS_CODE<-ifelse(jwd$COLONYLENGTH<1|jwd$COLONYLENGTH==5,"AAAA",as.character(jwd$GENUS_CODE))
jwd$TAXONCODE<-ifelse(jwd$COLONYLENGTH<1|jwd$COLONYLENGTH==5,"AAAA",as.character(jwd$TAXONCODE))
jwd$SPCODE<-ifelse(jwd$COLONYLENGTH<1|jwd$COLONYLENGTH==5,"AAAA",as.character(jwd$SPCODE))
jwd$COLONYLENGTH<-ifelse(jwd$COLONYLENGTH<1|jwd$COLONYLENGTH==5,NA,jwd$COLONYLENGTH)
jwd$TAXONNAME<-ifelse(jwd$COLONYLENGTH<1|jwd$COLONYLENGTH==5,NA,as.character(jwd$TAXONNAME))

nrow(subset(jwd,COLONYLENGTH>1))
View(subset(jwd,COLONYLENGTH>1))

nrow(jwd)
View(jwd)


# Generate Juvenile Density at the TRANSECT & SITE-LEVEL BY GENUS--------------------------------------------------
jcd.gen<-Calc_ColDen_Transect(jwd,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen";colnames(jcd.gen)[colnames(jcd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_j"

#Drop 2nd transect since we only surveyed 1 transect after 2017 and the 2nd transect wasn't always surveyed consistently prior to 2018
jcd.gen$TRANSECT<-as.factor(jcd.gen$TRANSECT)
site.data.gen<-subset(jcd.gen,TRANSECT %in% c("1","3")) #subseting first transect

site.data.gen2<-subset(site.data.gen, select= -c(TRANSECT)) #Create a copy (it takes a long time to run the ddply function above)

# Merge Site level data with sectors file and export site data ------------
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

#Merge together survey meta data and sector area files and check for missmatches 
meta<-left_join(survey_site,sectors)
meta[which(is.na(meta$AREA_HA)),]
nrow(survey_site)
nrow(meta)


#Merge site level data and meta data
site.data.gen2<-left_join(site.data.gen2,meta)
site.data.gen2$Juvpres.abs<-ifelse(site.data.gen2$JuvColDen>0,1,0)


#Change all special missions to exclude flag =-1, right now they are 0. Then exclude these sites
#Exclude PRIA 2017 sites because we want similar time intervals following bleaching events for all regions
levels(site.data.gen2$MISSIONID)
site.data.gen2<-site.data.gen2[!site.data.gen2$MISSIONID %in% c("MP1410","MP1512","MP1602","MP2006"),] #We may want to remove SE1602 (2016 Rose and Jarvis data)
site.data.gen2$Year_Island<-paste(site.data.gen2$OBS_YEAR,site.data.gen2$ISLAND,sep="_")
#site.data.gen2<-site.data.gen2[!site.data.gen2$Year_Island %in% c("2017_Baker","2017_Jarvis","2017_Howland"),] 

site.data.gen2<-droplevels(site.data.gen2);levels(site.data.gen2$MISSIONID)
View(site.data.gen2)

#Convert Protected Reef Slope to Forereef and Subset just Forereef sites
site.data.gen2$REEF_ZONE<-ifelse(site.data.gen2$REEF_ZONE=="Protected Slope","Forereef",as.character(site.data.gen2$REEF_ZONE))
site.data.gen2<-subset(site.data.gen2,REEF_ZONE=="Forereef")

#Remove Johnston from analysis- we only have 2015 data
site.data.gen2<-subset(site.data.gen2,ISLAND!="Johnston")


#Change Regions
site.data.gen2$REGION<-ifelse(site.data.gen2$ISLAND %in% c("FDP", "Maug", "Asuncion", "Alamagan", "Pagan", "Agrihan", "Guguan", "Sarigan","Farallon_de_Pajaros")
                              ,"NMARIAN", as.character(site.data.gen2$REGION))
site.data.gen2$REGION<-ifelse(site.data.gen2$ISLAND %in% c("Saipan", "Tinian", "Aguijan", "Rota", "Guam")
                              ,"SMARIAN", as.character(site.data.gen2$REGION))
site.data.gen2$REGION<-ifelse(site.data.gen2$ISLAND %in% c("Howland","Baker")
                              ,"PHOENIX", as.character(site.data.gen2$REGION))
site.data.gen2$REGION<-ifelse(site.data.gen2$ISLAND =="Wake"
                              ,"WAKE", as.character(site.data.gen2$REGION))
site.data.gen2$REGION<-ifelse(site.data.gen2$ISLAND %in% c("Kingman","Palmyra","Jarvis")
                              ,"LINE", as.character(site.data.gen2$REGION))

site.data.gen2$STRATANAME<- paste(site.data.gen2$SEC_NAME,site.data.gen2$REEF_ZONE,site.data.gen2$DEPTH_BIN,sep="_")
site.data.gen2$REGION_YEAR<-paste(site.data.gen2$REGION,site.data.gen2$OBS_YEAR,sep="_")

#Remove the 2014 NWHI data
site.data.gen2<-site.data.gen2[site.data.gen2$REGION_YEAR !="NWHI_2014",]

length(unique(site.data.gen2$SITE))


#Use survey package to calculate mean SE and conduct statistical analyses

#Calculate survey weights (inverse proportion weighting)
w.df<-ddply(site.data.gen2,.(ANALYSIS_YEAR,SEC_NAME,REEF_ZONE,DEPTH_BIN,NH),
            summarize,
            n=length(unique(SITE)))

w.df$sw<-w.df$NH/w.df$n

site.sw<-left_join(site.data.gen2,w.df) #merge weights with site-level data
head(site.sw)
site.swS<-subset(site.sw,GENUS_CODE=="SSSS") #only include total scleractinans

#remove strata that have less than 2 sites
site.swS<-subset(site.swS,n>1)
summary(site.swS$n)

write.csv(site.swS,file="T:/Benthic/Projects/Juvenile Project/JuvProject_SITE_weights_AllYears.csv",row.names = F)

des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+REGION+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=site.swS)

#Calculate regional mean and SE
sw_Rmean<-svyby(~JuvColDen,~ANALYSIS_YEAR+REGION,des,svymean)

#Test fixed effects of region and year
modR<-svyglm(JuvColCount ~ REGION*ANALYSIS_YEAR, design=des,offset= TRANSECTAREA_j, family="quasipoisson")

svystdres(modR,stvar="DB_RZ",doplot=TRUE)
svyCooksD(modR,stvar="DB_RZ",doplot=TRUE)
anova(modR)
AIC(modgaus)

#Run separate post hoc tests for each region to test for differences between years- I don't care about comparing 
library(multcomp)
nwhi.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(site.sw,REGION=="NWHI"))
nwhi<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=nwhi.des,offset= TRANSECTAREA_j, family="quasipoisson")
summary(glht(nwhi, mcp(ANALYSIS_YEAR="Tukey"))) 

mhi.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(site.sw,REGION=="MHI"))
mhi<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=mhi.des,offset= TRANSECTAREA_j, family="quasipoisson")
summary(glht(mhi, mcp(ANALYSIS_YEAR="Tukey"))) 

wake.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(site.sw,REGION=="WAKE"))
wake<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=wake.des,offset= TRANSECTAREA_j, family="quasipoisson")
summary(glht(wake, mcp(ANALYSIS_YEAR="Tukey"))) 

ph.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(site.sw,REGION=="PHOENIX"))
ph<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=ph.des,offset= TRANSECTAREA_j, family="quasipoisson")
summary(glht(ph, mcp(ANALYSIS_YEAR="Tukey"))) 

line.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(site.sw,REGION=="LINE"))
l<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=line.des,offset= TRANSECTAREA_j, family="quasipoisson")
summary(glht(l, mcp(ANALYSIS_YEAR="Tukey"))) 

sam.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(site.sw,REGION=="SAMOA"))
sam<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=sam.des,offset= TRANSECTAREA_j, family="quasipoisson")
summary(glht(sam, mcp(ANALYSIS_YEAR="Tukey"))) 

sm.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(site.sw,REGION=="SMARIAN"))
sm<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=sm.des,offset= TRANSECTAREA_j, family="quasipoisson")
summary(glht(sm, mcp(ANALYSIS_YEAR="Tukey"))) 

nm.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(site.sw,REGION=="NMARIAN"))
nm<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=nm.des,offset= TRANSECTAREA_j, family="quasipoisson")
summary(glht(nm, mcp(ANALYSIS_YEAR="Tukey")))


#Calculate adjusted pvalues
pvals<-c(0.49,0.995,0.407,0.000784,0.287,0.194,0.000001,0.445,0.151,0.000167)
round(p.adjust(pvals, "BH"), 3) #0.544, 0.995, 0.554, 0.003, 0.478, 0.388, 0.000, 0.544, 0.378, 0.001


site.sw2<-site.swS %>% mutate(YEAR= dplyr::recode(ANALYSIS_YEAR,
                                                  `2014`="Year1",
                                                  `2015`="Year1",
                                                  `2016`="Year1",
                                                  `2017`="Year2",
                                                  `2018`="Year2",
                                                  `2019`="Year2"))
jcdG_stS$YEAR<-ifelse(jcdG_stS$REGION_YEAR=="NWHI_2016","Year3",as.character(jcdG_stS$YEAR))
View(jcdG_stS)


des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+REGION+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=site.swS)

#Calculate regional mean and SE
sw_Rmean<-svyby(~JuvColDen,~ANALYSIS_YEAR+REGION,des,svymean)

#Test fixed effects of region and year
modR<-svyglm(JuvColCount ~ REGION*ANALYSIS_YEAR, design=des,offset= TRANSECTAREA_j, family="quasipoisson")


# MIXED MODELING - Density-------------------------------
#Convert analysis year to YEAR1,2,3 so that the GLMMs don't try to compare  regionxyear combos that don't exist
jcdG_stS<-jcdG_stS %>% mutate(YEAR= dplyr::recode(ANALYSIS_YEAR,
                                                  `2014`="Year1",
                                                  `2015`="Year1",
                                                  `2016`="Year1",
                                                  `2017`="Year2",
                                                  `2018`="Year2",
                                                  `2019`="Year2"))
jcdG_stS$YEAR<-ifelse(jcdG_stS$REGION_YEAR=="NWHI_2016","Year3",as.character(jcdG_stS$YEAR))
View(jcdG_stS)




# PLOTTING ----------------------------------------------------------------

round(p.adjust(pvals, "BH"), 3) #0.544, 0.995, 0.554, 0.003, 0.478, 0.388, 0.000, 0.544, 0.378, 0.001

#bar plot of juv by region by year with post hoc tests 
sw_Rmean$REGION <- factor(sw_Rmean$REGION, levels = c("NWHI","MHI","WAKE","PHOENIX","LINE","SMARIAN","NMARIAN","SAMOA"))
sw_Rmean$ANALYSIS_YEAR<-as.factor(sw_Rmean$ANALYSIS_YEAR)
#Add Posthoc groupings from LMMs
sw_Rmean<- sw_Rmean[order(sw_Rmean$REGION),];sw_Rmean
sw_Rmean$sig<-c("","","","a","b","","","","","a","b","","","a","b","","")


p8 <- ggplot(sw_Rmean, aes(x=ANALYSIS_YEAR, y=JuvColDen,fill=REGION)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve='single'), width = 1, color="black") +
  geom_errorbar(aes(y=JuvColDen, x=ANALYSIS_YEAR,ymin=JuvColDen-se, ymax=JuvColDen+se), width=.2)+
  facet_grid(~REGION, scales = "free_x", space = "free") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 12),
        legend.position = "none",
        axis.line = element_line(color = "black"),
        text = element_text(size = 12),
        axis.text.y = element_text(colour="black"),
        axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("#CC79A7","#D55E00","#E69F00","#F0E442","#009E73","#56B4E9","#0072B2","#999999")) +
  xlab("Year") +
  ylab("Mean Juvenile Colonies/m^2") +
  #scale_y_continuous(expand = c(0,0), limits = c(0,15)) +
  geom_text(aes(x=ANALYSIS_YEAR,y=JuvColDen+se,label=sig, group = REGION),
            position = position_dodge(),
            vjust = -0.5) 
p8


#Checking normality/equal variance for UNWEIGHTED Delta Density
plotNormalHistogram(delta.df$DeltaDen_yr)

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
#delta.df$Delta_trans<-sqrt(delta.df$DeltaDen_yr+2.7) #sqrt
delta.df$Delta_trans<-sign(delta.df$DeltaDen_yr+2.7) + abs(delta.df$DeltaDen_yr+2.7)^1/3 #cube root

plotNormalHistogram(delta.df$Delta_trans)
#mod<-lmer(Delta_trans~REGION +(1|SEC_NAME),data=delta.df)
mod<-lm(Delta_trans~REGION,data=delta.df)

leveneTest(Delta_trans~REGION,data=delta.df)
performance::check_model(mod)

#Cube root works best, but not perfect- tried power transformation too


#Checking normality/equal variance for WEIGHTED Delta Density
plotNormalHistogram(delta.df$W_DeltaDen)

delta.df$Delta_trans<-sqrt(delta.df$W_DeltaDen+0.5) #sqrt
delta.df$Delta_trans<-sign(delta.df$W_DeltaDen+0.5) + abs(delta.df$W_DeltaDen+0.3)^1/3 #cube root

plotNormalHistogram(delta.df$Delta_trans)
mod<-lmer(Delta_trans~REGION +(1|SEC_NAME),data=delta.df)
mod<-lm(Delta_trans~REGION,data=delta.df)
performance::check_model(mod)

leveneTest(Delta_trans~REGION,data=delta.df)

#Power transformation
library(rcompanion)

delta.df$T_tuk = transformTukey(delta.df$W_DeltaDen +0.5, plotit=FALSE)
plotNormalHistogram(T_tuk)

mod<-lm(T_tuk~REGION,data=delta.df)

performance::check_model(mod)
leveneTest(T_tuk~REGION,data=delta.df)


#No transformations work!!
#Use non parametric stats
library(multcompView)
kruskal.test(delta.df$W_DeltaDen, delta.df$REGION)
test<-pairwise.wilcox.test(delta.df$W_DeltaDen, delta.df$REGION,
                           p.adjust.method = "BH")
multcompLetters(test$p.value)

#Add Posthoc groupings 
delta_r<- delta_r[order(delta_r$REGION),];delta_r

delta_r$REGION <- factor(delta_r$REGION, levels = c("NWHI","MHI","WAKE","PHOENIX","LINE","SMARIAN","NMARIAN","SAMOA"))
delta_r$sig<-c("a","a","ab","b","a","ab","a","ab")
delta_r$years<-c("(2017-2015)","(2019-2016)","(2017-2014)","(2018-2015)","(2018-2015)","(2018-2015)","(2017-2014)","(2017-2014)")

# 
p9 <- ggplot(delta_r, aes(x=REGION, y=DeltaDen_yr,fill=REGION)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve='single'), width = 1, color="black") +
  geom_errorbar(aes(y=DeltaDen_yr, x=REGION,ymin=DeltaDen_yr-SE_DeltaDen_yr, ymax=DeltaDen_yr+SE_DeltaDen_yr), width=.2)+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 12),
        legend.position = "none",
        axis.line = element_line(color = "black"),
        text = element_text(size = 12),
        axis.text.y = element_text(colour="black"),
        axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("#CC79A7","#D55E00","#E69F00","#F0E442","#009E73","#56B4E9","#0072B2","#999999")) +
  xlab("Region") +
  ylab(expression("Mean"~Delta~"Juvenile Density/Year"))+
  geom_text(data=delta_r,aes(x=REGION,y=DeltaDen_yr+SE_DeltaDen_yr,label=sig, group = REGION),position = position_dodge(),vjust = -0.5)+
  geom_text(data=delta_r,aes(x=REGION,y=-0.8,label=years, group = REGION),position = position_dodge(),vjust = -0.8,size=3,fontface="bold")

p9




#with SE_2
p9b <- ggplot(delta_r, aes(x=REGION, y=DeltaDen_yr,fill=REGION)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve='single'), width = 1, color="black") +
  geom_errorbar(aes(y=DeltaDen_yr, x=REGION,ymin=DeltaDen_yr-SE_v2, ymax=DeltaDen_yr+SE_v2), width=.2)+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 12),
        legend.position = "none",
        axis.line = element_line(color = "black"),
        text = element_text(size = 12),
        axis.text.y = element_text(colour="black"),
        axis.text.x = element_text(angle = 90)) +
  scale_fill_viridis(discrete = TRUE) +
  xlab("Region") +
  ylab(expression("Mean"~Delta~"Juvenile Density/Year"))+
  geom_text(data=delta_r,aes(x=REGION,y=DeltaDen_yr+SE_DeltaDen_yr,label=sig, group = REGION),position = position_dodge(),vjust = -0.5)+
  geom_text(data=delta_r,aes(x=REGION,y=-0.8,label=years, group = REGION),position = position_dodge(),vjust = -0.8,size=3,fontface="bold")

p9b



#Plotting Delta Density/Year- ISLAND

delta_is$REGION <- factor(delta_is$REGION, levels = c("NWHI","MHI","WAKE","PHOENIX","LINE","SMARIAN","NMARIAN","SAMOA"))

p10 <- ggplot(delta_is, aes(x=reorder(ISLAND,-DeltaDen_yr), y=DeltaDen_yr,fill=REGION)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve='single'), width = 1, color="black") +
  geom_errorbar(data=delta_is,aes(y=DeltaDen_yr, x=ISLAND,ymin=DeltaDen_yr-SE_DeltaDen_yr, ymax=DeltaDen_yr+SE_DeltaDen_yr), width=.2)+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 12),
        #legend.position = "none",
        axis.line = element_line(color = "black"),
        text = element_text(size = 12),
        axis.text.y = element_text(colour="black"),
        axis.text.x = element_text(vjust=0,angle = 90)) +
  scale_fill_manual(values = c("#CC79A7","#D55E00","#E69F00","#F0E442","#009E73","#56B4E9","#0072B2","#999999")) +
  xlab("Island") +
  ylab(expression("Mean"~Delta~"Juvenile Density/Year"))

p10




#Identify median lat and long that surveys were conducted for each island and year
lat.sum<-ddply(data.gen_tempS,.(REGION,ISLAND),
               summarize,
               Y=median(LATITUDE,na.rm=T),
               X=median(LONGITUDE,na.rm = T))

j.sum<-ddply(delta_is,.(REGION,ISLAND),
             summarize,
             DeltaDen=mean(DeltaDen_yr,na.rm=T))
deltaden_coords<-left_join(j.sum,lat.sum)
head(deltaden_coords)



# #Use unweighted delta density ~ Latitiude
# plotNormalHistogram(delta.df$DeltaDen)
# 
# plotNormalHistogram(sqrt(delta.df$DeltaDen+2.7))
# mod<-lm(DeltaDen~LATITUDE,data=deltaden_coords)
# qqnorm(residuals(mod),ylab="Sample Quanqqnorm(residuals(mod)")
# qqline(residuals(mod), col="red")
# 
# mod1<-lm(DeltaDen~LATITUDE,data=deltaden_coords)
# anova(mod1);summary(mod1)




write.csv(deltaden_coords,file="T:/Benthic/Projects/Juvenile Project/DeltaJuvenileforMaps.csv")

# Plot Pacific-wide Map of Delta Juvenile Density -------------------------

##Helpful website for plotting maps with ggplot https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html

#https://rpubs.com/valentin/pacific-centered-map-voronoi-tessellation

#Theme for maps
theme_set(
  theme_bw() +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size = 8)))

#Get world spatial polygons from the rnaturalearth package
#Cut out area of world that doesn't include the Pacific and bind E and W Pacific and shift the geographical coordinates for a Pacific view (see website for illustration)
#Note- you will get several warnings about "world is invalid" and an issues with rgeos- ignore these
world <- rnaturalearth::ne_countries(scale = 'medium', returnclass = "sp")
box_cut <- bbox2SP(n = 90, s = -90, w = -150, e = 140, proj4string = world@proj4string) #you can tweak the W and E coords to zoom in and out, but adjust the N and S coords in the function below using ymin and ymax
world_crop <- gDifference(world, box_cut)

pacific_crop <- world_crop %>% 
  st_as_sf() %>% # change from sp to sf object/class
  st_shift_longitude() %>% 
  st_crop(c(xmin = st_bbox(.)[["xmin"]],
            xmax = st_bbox(.)[["xmax"]],
            ymin = -15,
            ymax = 30))

#convert delta juvenile density data to spatial points df
xy <- deltaden_coords[,c(5,4)]
deltaden_coords_sp <- SpatialPointsDataFrame(coords = xy, data = deltaden_coords,
                                             proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#Crop the delta spdf
delta_shift <- deltaden_coords_sp %>% 
  st_as_sf() %>%
  st_shift_longitude() %>% 
  st_crop(c(xmin = 120, xmax = 250, ymin = -50, ymax = 30)) %>% 
  # Also adds the coordinates to be used for labeling with geom_text_repel
  bind_cols(st_coordinates(.) %>% as.data.frame())

#Create an Inset map using a similar process described above
box_cut2 <- bbox2SP(n = 90, s = -90, w = -110, e = 110, proj4string = world@proj4string)
world_crop2 <- gDifference(world, box_cut2)

pacific_crop2 <- world_crop2 %>% 
  st_as_sf() %>% # change from sp to sf object/class
  st_shift_longitude() %>% 
  st_crop(c(xmin = st_bbox(.)[["xmin"]],
            xmax = st_bbox(.)[["xmax"]],
            ymin = -40,
            ymax = 50))
pacific_crop_bb = st_as_sfc(st_bbox(pacific_crop)) #draw box for the area of main map
pacific_box = st_as_sfc(st_bbox(pacific_crop2)) #draw box for the area of inset map

#plot inset map
insetmap<-ggplot() +
  geom_sf(data = pacific_crop2)+
  geom_sf(data = pacific_crop_bb, fill = NA, color = "black", size = 1.2)+
  geom_sf(data = pacific_box, fill = NA, color = "black", size = 0.4)+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())


#plot main data map
deltamap<-ggplot() +
  geom_sf(data = pacific_crop)+ #basemap
  geom_sf(data = delta_shift,aes(color = DeltaDen), size = 3, shape = 19)+ #data
  geom_text_repel(data = delta_shift, #add island labels 
                  aes(x = X...7, y = Y...8, label = ISLAND),
                  size = 3,
                  fontface = "bold",
                  segment.size = 0.25,
                  box.padding = 0.4,
                  min.segment.length = 0,
                  seed = 2020-5-16)+
  annotation_scale(location = "bl", width_hint = 0.4)+ #add scale bar
  scale_color_gradient2(midpoint = 0.66, #Color scheme
                        high = 'forestgreen',
                        mid = 'yellow2',
                        low = 'red2',
                        na.value = 'gray95',
                        name="")+  #you can add a legend title here if you want
  theme(legend.position = c(0.95,0.2)) #put legend inside the plot
#ggtitle("Change in Juvenile Colony Density/Year")

#Combine main and inset maps
finalmap = ggdraw() +
  draw_plot(deltamap) +
  draw_plot(insetmap, x = 0.02, y = 0.07, width = 0.3, height = 0.3)

finalmap



# Delta Density v. Depth --------------------------------------------------

depth_strat<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Predictor Variables/JuvProject_Depth.csv")
delta.df<-left_join(delta.df,depth_strat)

#Use unweighted delta density
plotNormalHistogram(sqrt(delta.df$DeltaDen_yr+2.7))
mod<-lm(sqrt(DeltaDen_yr+2.7)~MeanMidDepth,data=delta.df)
qqnorm(residuals(mod),ylab="Sample Quanqqnorm(residuals(mod)")
qqline(residuals(mod), col="red")

delta.df$Delta_trans<-sqrt(delta.df$DeltaDen_yr+2.7)


mod1 <- lm(Delta_trans~REGION*MeanMaxDepth,data=delta.df)
anova(mod1);summary(mod1)

mod2 <- lm(Delta_trans~MeanMaxDepth,data=delta.df)
anova(mod2);summary(mod2)

jvd<-delta.df %>%
  ggplot(aes(x=MeanMaxDepth, y=DeltaDen_yr)) + 
  geom_smooth(se=TRUE,method="lm",lwd=1.5,color="black")+
  geom_point()+
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001)
    ,axis.text.x = element_text(angle = 90)) + # adjust x axis to lower the same amount as the genus labels
  labs(x="Mean Max Depth(M)",y="Delta Juvenile Density/Year")+
  geom_label(aes(x=4,y=6),hjust=0,label=paste("R^2=0.169","\n p=0.0458",sep=""))
jvd


#Save plots
ggsave(plot=p8,file="T:/Benthic/Projects/Juvenile Project/Figures/DensityRegionalTemporal.jpg",width=10,height=5)
ggsave(plot=p9,file="T:/Benthic/Projects/Juvenile Project/Figures/WeightedDeltaRegional.jpg",width=10,height=5)
ggsave(plot=p10,file="T:/Benthic/Projects/Juvenile Project/Figures/WeightedDeltaIsland.jpg",width=10,height=5)
ggsave(plot=finalmap,file="T:/Benthic/Projects/Juvenile Project/Figures/WeightedDeltaIslandmap.jpg",width=11,height=7)
ggsave(plot=jvd,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Figures/DeltavDepth.jpg",width=10,height=5)




# Identify dominant adult morphology in each stratum ----------------------
awd<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED.csv")

nw<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/PMNM2017_ADULTCOLONY_QCd.csv")

#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("ISLAND","SITEVISITID","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "MORPH_CODE","GENUS_CODE")


#remove extraneous columns
head(awd[,DATA_COLS])
awd<-awd[,DATA_COLS]
nw<-nw[,DATA_COLS]

all.df<-rbind(awd,nw)

morph<-read.csv("T:/Benthic/Data/Lookup Tables/MorphologyCodeLookup.csv")

all.df<-left_join(all.df,morph)
head(all.df)

#Convert Protected Slope to Forereef
all.df$REEF_ZONE<-ifelse(all.df$REEF_ZONE=="Protected Slope","Forereef",as.character(all.df$REEF_ZONE))

all.df<-subset(all.df,REEF_ZONE=="Forereef")

# Merge Site level data with sectors file and export site data ------------
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

#Merge together survey meta data and sector area files and check for missmatches 
all.df<-left_join(all.df,sectors)

all.df$STRATANAME<-paste(all.df$SEC_NAME,all.df$REEF_ZONE,all.df$DEPTH_BIN,sep="_")

st.list<-ddply(delta.df,.(STRATANAME),summarize,n=length(STRATANAME))
all.df2<-all.df[all.df$STRATANAME %in% c(st.list$STRATANAME),] #Subset adult data to only include strata of intersest 

all.df2<- filter(all.df2, !(GENUS_CODE %in% c("SSSS","AAAA")))

morph<-ddply(all.df2,.(ISLAND,STRATANAME,GENUS_CODE,NEW_MORPH_CODE),summarize,morph_sum=length(GENUS_CODE))
tot<-ddply(all.df2,.(ISLAND,STRATANAME,GENUS_CODE),summarize,total=length(GENUS_CODE))
mt<-left_join(morph,tot)
mt$Morph_prop<-mt$morph_sum/mt$total
head(mt)

write.csv(mt,file="T:/Benthic/Projects/Juvenile Project/Strata_Genus_Morphology.csv")
# write.csv(data.gen_pb,file="T:/Benthic/Projects/Juvenile Project/JuvProject_pb_SITE.csv")
