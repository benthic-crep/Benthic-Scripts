
# This script will clean the raw benthic REA data using method E that comes directly from the new data base application.
#Note- these data represent the revised data structure insituted in November 2018. Several recent dead and condition columns were added
#These data only include surveys conducted between 2013-2019
rm(list=ls())

#Set Run Flags
DEBUG=TRUE

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")
library(VCA)
library(forcats)

setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project")

#LOAD DATA
jwd<-read.csv("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_F_raw_CLEANED.csv")


## LOAD data
# site.data.gen2<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS.csv")
# site.data.tax2<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_TAXONCODE.csv")



#Final Tweaks before calculating Site-level data-------------------------------------------------
#Colony fragments and scleractinans are subseted in the functions 
#Add a column for adult fragments so we can remove them from the dataset later (-1 indicates fragment)
jwd$Fragment <- 0 # you need to add this column so that you can use the site level functions correctly
jwd$DATE_ <- as.Date(jwd$DATE_, format = "%Y-%m-%d")
jwd$METHOD<-"DIVER"
jwd$ANALYST<-jwd$DIVER
jwd$SEGAREA<-jwd$SEGLENGTH*jwd$SEGWIDTH

#Create a look a table of all of the colony attributes- you will need this the functions below
SURVEY_SITE<-c("MISSIONID","DATE_","SITEVISITID", "ANALYSIS_YEAR","OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE","HABITAT_CODE","REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")
survey_site<-unique(jwd[,SURVEY_SITE])#new_Aggregate_InputTable(awd, SURVEY_INFO)#TAO 2019/10/07


#TEMPORARY WORK AROUND-ASK MICHAEL TO FIX
survey_site$REEF_ZONE<-ifelse(survey_site$SITE=="HAW-04285","Forereef",as.character(survey_site$REEF_ZONE))


#Look at the size class data to determine the min colony size cut off for analysis
#Subset 2019 Hawaii data
hi<-subset(jwd,OBS_YEAR=="2019")

ggplot(hi) + 
  geom_density(aes(x = COLONYLENGTH, fill = ANALYST), alpha = 0.2)+
  geom_vline(xintercept=1, color = "black")+
  facet_wrap(~ISLAND)

ggplot(hi) + 
  geom_density(aes(x = COLONYLENGTH, fill = ISLAND), alpha = 0.2)+
  facet_wrap(~ISLAND)

s.data<-ddply(hi,.(ISLAND,ANALYST),
              summarise,
         min=min(COLONYLENGTH,na.rm=T),
         mean=mean(COLONYLENGTH,na.rm=T),
         ncol=length(COLONYLENGTH,na.rm=T))

s.all<-ddply(jwd,.(ISLAND,ANALYST),
              summarise,
              min=min(COLONYLENGTH,na.rm=T),
              mean=mean(COLONYLENGTH,na.rm=T),
              ncol=length(COLONYLENGTH))

#Change colonies that are <1cm or >5cm to NA. I'm not subsetting these data because I need to keep the placeholder in the dataframe in case a site only had colonies <1cm or >5cm
View(subset(jwd,COLONYLENGTH<1))
nrow(subset(jwd,COLONYLENGTH<1))
nrow(jwd)
jwd$S_ORDER<-ifelse(jwd$COLONYLENGTH<1|jwd$COLONYLENGTH==5,NA,as.character(jwd$S_ORDER))
jwd$GENUS_CODE<-ifelse(jwd$COLONYLENGTH<1|jwd$COLONYLENGTH==5,NA,as.character(jwd$GENUS_CODE))
jwd$TAXONCODE<-ifelse(jwd$COLONYLENGTH<1|jwd$COLONYLENGTH==5,NA,as.character(jwd$TAXONCODE))
jwd$SPCODE<-ifelse(jwd$COLONYLENGTH<1|jwd$COLONYLENGTH==5,NA,as.character(jwd$SPCODE))
jwd$COLONYLENGTH<-ifelse(jwd$COLONYLENGTH<1|jwd$COLONYLENGTH==5,NA,jwd$COLONYLENGTH)

nrow(subset(jwd,COLONYLENGTH>1))
View(subset(jwd,COLONYLENGTH>1))

nrow(jwd)
View(jwd)



# GENERATE SUMMARY METRICS at the transect-leveL BY GENUS--------------------------------------------------
jcd.gen<-Calc_ColDen_Transect(jwd,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen";colnames(jcd.gen)[colnames(jcd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_j"

site.data.gen<-ddply(jcd.gen, .(SITE,SITEVISITID,GENUS_CODE), #calc total colonies by condition
                     summarise,
                     JuvColDen=mean(JuvColDen,na.rm=T))

site.data.gen2<-site.data.gen
                     
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
levels(site.data.gen2$MISSIONID)
site.data.gen2<-site.data.gen2[!site.data.gen2$MISSIONID %in% c("MP1410","MP1512","MP1602","SE1602","MP2006"),]
site.data.gen2$Year_Island<-paste(site.data.gen2$OBS_YEAR,site.data.gen2$ISLAND,sep="_")
site.data.gen2<-site.data.gen2[!site.data.gen2$Year_Island %in% c("2017_Baker","2017_Jarvis","2017_Howland"),] 

site.data.gen2<-droplevels(site.data.gen2);levels(site.data.gen2$MISSIONID)
View(site.data.gen2)

#Subset just Forereef sites
site.data.gen2$REEF_ZONE<-ifelse(site.data.gen2$REEF_ZONE=="Protected Slope","Forereef",as.character(site.data.gen2$REEF_ZONE))
site.data.gen2<-subset(site.data.gen2,REEF_ZONE=="Forereef")

#Remove Wake from analysis- we don't have enough replicate strata to conduct downstream analyses
site.data.gen2<-subset(site.data.gen2,ISLAND!="Wake")


#Change Regions
site.data.gen2$REGION<-ifelse(site.data.gen2$ISLAND %in% c("FDP", "Maug", "Asuncion", "Alamagan", "Pagan", "Agrihan", "Guguan", "Sarigan","Farallon de Pajaros")
                             ,"NMARIAN", as.character(site.data.gen2$REGION))
site.data.gen2$REGION<-ifelse(site.data.gen2$ISLAND %in% c("Saipan", "Tinian", "Aguijan", "Rota", "Guam")
                             ,"SMARIAN", as.character(site.data.gen2$REGION))
site.data.gen2$REGION<-ifelse(site.data.gen2$ISLAND %in% c("Johnston","Howland","Baker")
                             ,"Phoenix", as.character(site.data.gen2$REGION))
site.data.gen2$REGION<-ifelse(site.data.gen2$ISLAND %in% c("Kingman","Palmyra","Jarvis")
                             ,"Line", as.character(site.data.gen2$REGION))

# Generate data for temporal analysis---------------------------------------------------
site.data.gen2$STRATANAME<- paste(site.data.gen2$SEC_NAME,site.data.gen2$REEF_ZONE,site.data.gen2$DEPTH_BIN,sep="_")
st.list<-ddply(site.data.gen2,.(OBS_YEAR,REGION,ISLAND,SEC_NAME,STRATANAME),summarize,n=length(unique(SITE)))
st.list2<-subset(st.list,n>=2);head(st.list)

#Generate list of strata that were surveyed in all years for a given region and had at least 2 sites/stratum
st.list_w<-dcast(st.list2, formula=REGION+ISLAND+SEC_NAME+STRATANAME~ OBS_YEAR, value.var="n",fill=0)
dCOLS<-c("2013","2014","2015","2016","2017","2018","2019")
st.list_w$year_n<-rowSums(st.list_w[,dCOLS] > 0, na.rm=T) #count # of years of data
st.list_w2<-subset(st.list_w,REGION %in% c("NMARIAN","SMARIAN","Line","Phoenix","SAMOA") & year_n>=2)
st.list_w3<-subset(st.list_w,REGION %in% c("MHI","NWHI") & year_n>=3)
st.list_w4<-rbind(st.list_w2,st.list_w3)

head(st.list_w4);st.list_w4<-droplevels(st.list_w4)


data.gen_temp<-site.data.gen2[site.data.gen2$STRATANAME %in% c(st.list_w4$STRATANAME),]

View(data.gen_temp) #double check that strata were dropped correctly


#POOL UP
#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
data.gen_temp$ANALYSIS_SCHEMA<-data.gen_temp$STRATANAME
data.gen_temp$DOMAIN_SCHEMA<-data.gen_temp$ISLAND
data.gen_temp$ANALYSIS_YEAR<-data.gen_temp$OBS_YEAR
data.gen_temp$DB_RZ<-paste(data.gen_temp$DEPTH_BIN,data.gen_temp$REEF_ZONE,sep="_")

#Create a vector of columns to subset for strata estimates
c.keep<-c("REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
          "n_h","N_h","D._h","SE_D._h")

jcdG_st<-Calc_Strata(data.gen_temp,"GENUS_CODE","JuvColDen","Juvpres.abs");jcdG_st=jcdG_st[,c.keep]
colnames(jcdG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","JuvColDen","SE_JuvColDen")
jcdG_st$ANALYSIS_YEAR<-as.factor(jcdG_st$ANALYSIS_YEAR)

#Double Check that revised pooling is adding up NH (total sites) correctly
View(jcdG_st)
View(sectors)


#Calculate Island Estimates
jcdG_is<-Calc_Domain(data.gen_temp,"GENUS_CODE","JuvColDen","Juvpres.abs")
jcdG_is<-jcdG_is[,c("REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen")]

#Calculate Sector Estimates
data.gen_temp$ANALYSIS_SCHEMA<-data.gen_temp$STRATANAME
data.gen_temp$DOMAIN_SCHEMA<-data.gen_temp$SEC_NAME

jcdG_sec<-Calc_Domain(data.gen_temp,"GENUS_CODE","JuvColDen","Juvpres.abs")
jcdG_sec<-jcdG_sec[,c("REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen")]

# write.csv(jcdG_st,file="T:/Benthic/Projects/Juvenile Project/JuvProject_temporal_STRATA.csv")
# write.csv(jcdG_is,file="T:/Benthic/Projects/Juvenile Project/JuvProject_temporal_ISLAND.csv")
# write.csv(jcdG_sec,file="T:/Benthic/Projects/Juvenile Project/JuvProject_temporal_SECTOR.csv")

write.csv(jcdG_st,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/JuvProject_temporal_STRATA.csv")
write.csv(jcdG_is,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/JuvProject_temporal_ISLAND.csv")
write.csv(jcdG_sec,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/JuvProject_temporal_SECTOR.csv")


###Plotting
jcdG_sec$ANALYSIS_YEAR<-as.factor(jcdG_sec$ANALYSIS_YEAR)

p1<-ggplot(subset(jcdG_sec,GENUS_CODE=="SSSS"), aes(x=DOMAIN_SCHEMA, y=Mean_JuvColDen, fill=ANALYSIS_YEAR)) + 
  geom_bar(position=position_dodge(), stat="identity") + 
  # guides(fill=FALSE) 
  facet_wrap(~REGION, scales="free_x", labeller=label_parsed) +
  geom_errorbar(aes(ymin=Mean_JuvColDen-SE_JuvColDen, ymax=Mean_JuvColDen+SE_JuvColDen),width=.15, position=position_dodge(.9))+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="bottom")+
  labs(x="Sector",y="Mean Juvenile Density/m2")+
  guides(fill=guide_legend(title="Year"))


p1
#ggsave(plot=p1,file="T:/Benthic/Projects/Juvenile Project/Figures/Juv_Temporal.pdf",width=10,height=10)
ggsave(plot=p1,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Figures/Juv_Temporal.pdf",width=10,height=10)



####Absolute Change####

#Wake surveyd in off year- change time point
# jcdG_st$ANALYSIS_YEAR[jcdG_st$ISLAND=="Wake"&jcdG_st$ANALYSIS_YEAR=="2014"] <- "2015"
# jcdG_st$ANALYSIS_YEAR[jcdG_st$ISLAND=="Wake"&jcdG_st$ANALYSIS_YEAR=="2017"] <- "2018"

jcdG_st$YEAR<-paste("a",jcdG_st$ANALYSIS_YEAR,sep="")

new.df_w<-dcast(jcdG_st, formula= REGION + ISLAND + Stratum + REEF_ZONE + DB_RZ + GENUS_CODE~ YEAR, value.var="JuvColDen",fill=NA)
head(new.df_w)

nwhi<-subset(new.df_w,REGION=="NWHI")
mhi<-subset(new.df_w,REGION=="MHI")
nmarian<-subset(new.df_w,REGION=="NMARIAN")
smarian<-subset(new.df_w,REGION=="SMARIAN")
ph<-subset(new.df_w,REGION=="Phoenix")
line<-subset(new.df_w,REGION=="Line")
samoa<-subset(new.df_w,REGION=="SAMOA")

#marian<-subset(new.df_w,REGION=="MARIAN")
#pria<-subset(new.df_w,REGION=="PRIAs")

head(mhi)

mhi$t2013<-0
mhi$t2014<-NA
mhi$t2015<-NA
mhi$t2016<-mhi$a2016-mhi$a2013
mhi$t2017<-NA
mhi$t2018<-NA
mhi$t2019<-mhi$a2019-mhi$a2016

nwhi$t2013<-NA
nwhi$t2014<-0
nwhi$t2015<-nwhi$a2015-nwhi$a2014
nwhi$t2016<-nwhi$a2016-nwhi$a2015
nwhi$t2017<-NA
nwhi$t2018<-NA
nwhi$t2019<-NA

nmarian$t2013<-NA
nmarian$t2014<-0
nmarian$t2015<-NA
nmarian$t2016<-NA
nmarian$t2017<-nmarian$a2017-nmarian$a2014
nmarian$t2018<-NA
nmarian$t2019<-NA

smarian$t2013<-NA
smarian$t2014<-0
smarian$t2015<-NA
smarian$t2016<-NA
smarian$t2017<-smarian$a2017-smarian$a2014
smarian$t2018<-NA
smarian$t2019<-NA


ph$t2013<-NA
ph$t2014<-NA
ph$t2015<-0
ph$t2016<-NA
ph$t2017<-NA
ph$t2018<-ph$a2018-ph$a2015
ph$t2019<-NA

line$t2013<-NA
line$t2014<-NA
line$t2015<-0
line$t2016<-NA
line$t2017<-NA
line$t2018<-line$a2018-line$a2015
line$t2019<-NA

samoa$t2013<-NA
samoa$t2014<-NA
samoa$t2015<-0
samoa$t2016<-NA
samoa$t2017<-NA
samoa$t2018<-samoa$a2018-samoa$a2015
samoa$t2019<-NA
abschange<-rbind(nwhi,mhi,nmarian,smarian,ph,line,samoa)

abschange<-abschange[,-c(7:13)]
pc_long<-gather(abschange,Year,Change,t2013:t2019,factor_key = T)

# change_sum<-ddply(all.df_long,.(REGION,GENUS_CODE,Year),
#                   summarize,
#                   Mean=mean(Change,na.rm=T),
#                   SE=std.error(Change,na.rm=T))

pc_long<-pc_long %>% mutate(Yearn=recode(Year,
                                         `t2013`="2013",
                                         `t2014`="2014",
                                         `t2015`="2015",
                                         `t2016`="2016",
                                         `t2017`="2017",
                                         `t2018`="2018",
                                         `t2019`="2019"))
pc_long$Yearn<-as.numeric(as.character(pc_long$Yearn))
head(pc_long)
pc_long<-cSplit(pc_long, 'DB_RZ', sep="_", type.convert=FALSE);colnames(pc_long)[colnames(pc_long)=="DB_RZ_1"]<-"DEPTH_BIN"

pc_longS<-subset(pc_long,GENUS_CODE=="SSSS")

p1<-pc_longS %>%
  mutate(REGION = fct_relevel(REGION,"NWHI","MHI","Phoenix","Line","SAMOA","SMARIAN","NMARIAN")) %>% #reorder varibles 
  ggplot(aes(x=Yearn, y=Change, color=REGION)) + 
  geom_smooth(se=FALSE,method="loess",lwd=1.5)+
  geom_point()+
  geom_hline(yintercept=0)+
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001)) + # adjust x axis to lower the same amount as the genus labels
  labs(x="Year",y="Abs. Change Mean Density")+
  scale_x_continuous(breaks=seq(2013,2019,1))
p1

#ggsave(plot=p1,file="T:/Benthic/Projects/Juvenile Project/Figures/Abschange_REGION_points.pdf",width=8,height=6)
ggsave(plot=p1,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Figures/Abschange_REGION_points.pdf",width=8,height=6)

p2<-pc_longS %>%
  mutate(DEPTH_BIN = fct_relevel(DEPTH_BIN,"Shallow","Mid","Deep")) %>% #reorder varibles
  mutate(REGION = fct_relevel(REGION,"NWHI","MHI","Phoenix","Line","SAMOA","SMARIAN","NMARIAN")) %>% #reorder varibles 
  ggplot(aes(x=Yearn, y=Change, color=REGION)) + 
  geom_smooth(se=FALSE,method="loess",lwd=1.5)+
  geom_point()+
  geom_hline(yintercept=0)+
  facet_wrap(~DEPTH_BIN)+
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001)
    ,axis.text.x = element_text(angle = 90)) + # adjust x axis to lower the same amount as the genus labels
  labs(x="Year",y="Abs. Change Mean Density")+
  scale_x_continuous(breaks=seq(2013,2019,1))
p2

jcdG_st<-cSplit(jcdG_st, 'DB_RZ', sep="_", type.convert=FALSE);colnames(jcdG_st)[colnames(jcdG_st)=="DB_RZ_1"]<-"DEPTH_BIN"
jcdG_stS<-subset(jcdG_st,GENUS_CODE=="SSSS")
jcdG_stS$ANALYSIS_YEAR<-as.numeric(as.character(jcdG_stS$ANALYSIS_YEAR))

p3<-jcdG_stS %>%
  mutate(REGION = fct_relevel(REGION,"NWHI","MHI","Phoenix","Line","SAMOA","SMARIAN","NMARIAN")) %>% #reorder varibles 
  mutate(DEPTH_BIN = fct_relevel(DEPTH_BIN,"Shallow","Mid","Deep")) %>% #reorder varibles 
  ggplot(aes(x=ANALYSIS_YEAR, y=JuvColDen, color=REGION)) + 
  geom_smooth(se=FALSE,method="loess",lwd=1.5)+
  geom_point()+
  facet_wrap(~DEPTH_BIN)+
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001)
    ,axis.text.x = element_text(angle = 90)) + # adjust x axis to lower the same amount as the genus labels
  labs(x="Year",y="Mean Juvenile Density")+
  scale_x_continuous(breaks=seq(2013,2019,1))
p3



# p3<-ggplot(subset(pc_long,GENUS_CODE=="SSSS"), aes(x=DEPTH_BIN, y=Change, color=Year)) + 
#   geom_boxplot()+
#   geom_hline(yintercept=0)+
#   facet_wrap(~REGION)+
#   theme_bw() +
#   theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,axis.ticks.x = element_blank() # no x axis ticks
#     ,axis.title.x = element_text( vjust = -.0001)) + # adjust x axis to lower the same amount as the genus labels
#   labs(x="Year",y="Abs. Change Mean Density")
# p3


# ggsave(plot=p1,file="T:/Benthic/Projects/Juvenile Project/Figures/Abschange_REGION_points.pdf",width=8,height=6)
# ggsave(plot=p2,file="T:/Benthic/Projects/Juvenile Project/Figures/Abschange_REGION_DEPTH_points.pdf",width=8,height=6)
# ggsave(plot=p3,file="T:/Benthic/Projects/Juvenile Project/Figures/Density_REGION_DEPTH_points.pdf",width=8,height=6)

ggsave(plot=p1,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Figures/Abschange_REGION_points.pdf",width=8,height=6)
ggsave(plot=p2,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Figures/Abschange_REGION_DEPTH_points.pdf",width=8,height=6)
ggsave(plot=p3,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Figures/Density_REGION_DEPTH_points.pdf",width=8,height=6)



#General thoughts on patterns
#The time after the bleaching event is very important. I removed the 2016 and 2017 juvenile data for jarvis, baker and howland. it may be interesting to look at these separately



##Statistical Analyses


head(jcdG_stS)

#Check for normality and equal variance
library(rcompanion)
#Log transform
plotNormalHistogram(jcdG_stS$JuvColDen)
mod<-lm(JuvColDen~Stratum,data=jcdG_stS)
qqnorm(residuals(mod),ylab="Sample Quantiles for residuals")
qqline(residuals(mod), col="red")

jcdG_stS$Den1<-jcdG_stS$JuvColDen+0.5

mod.glm<-glm(Den1~Stratum,data=jcdG_stS,family="gaussian"(link='log'))


summary(mod.glm)  # Report the results
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(mod.glm) 

l<-log(jcdG_stS$JuvColDen+1)
plotNormalHistogram(l)
qqnorm(l, ylab="Sample Quantiles for logDensity")
qqline(l, col="red")
mod<-lm(l~Stratum,data=jcdG_stS)
qqnorm(residuals(mod),ylab="Sample Quantiles for residuals")
qqline(residuals(mod), col="red")
jcdG_stS$logDen<-log(jcdG_stS$JuvColDen+1)

summary(mod)  # Report the results
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(mod) 

#Take home I can either transform or use gamma distribution

#Variance Component Analysis
S<-subset(data.gen_temp,GENUS_CODE=="SSSS")

l<-log(S$JuvColDen+1)
plotNormalHistogram(l)
qqnorm(l, ylab="Sample Quantiles for logDensity")
qqline(l, col="red")
mod<-lm(l~Stratum,data=data.gen_temp)
qqnorm(residuals(mod),ylab="Sample Quantiles for residuals")
qqline(residuals(mod), col="red")
S$logDen<-log(S$JuvColDen+1)


S$DEPTH_BIN<-as.factor(S$DEPTH_BIN)
fit.MS2 <- fitVCA(logDen~DEPTH_BIN/DOMAIN_SCHEMA, S)
print(fit.MS2, digits=4)

mod<-lmer(logDen~1+(1|ANALYSIS_SCHEMA/DOMAIN_SCHEMA/REGION/OBS_YEAR),data=S)
summary(mod)
mod1 <- lmer(logDen~YEAR*REGION+(1|Stratum),data=jcdG_stS)

mod2 <- lmer(logDen~YEAR+(1|Stratum),data=jcdG_stS)
mod3 <- lmer(logDen~REGION+(1|Stratum),data=jcdG_stS)

modnull <- lmer(logDen~1+(1|Stratum),data=jcdG_stS)
anova(mod1,mod2,test="chisq")
anova(mod1,mod3,test="chisq")
anova(mod1,modnull,test="chisq")

#MHI
mod1 <- lmer(logDen~YEAR*+(1|Stratum),data=subset(jcdG_stS,REGION=="MHI"&DEPTH_BIN=="Mid"))
# lsmo <-lsmeans :: lsmeans (mod1, ~ YEAR , adjust = "Tukey") #This is bullshit- don't use lsmeans it says everything is signficant
# lsmo
summary(glht(mod1, linfct = mcp(YEAR = "Tukey"))) #this works!



mod1 <- glmer(logDen~YEAR*REGION+(1|Stratum),data=jcdG_stS)
summary(mod1)

#Next steps
#separate out northern and southern CNMI, phoneix and line island
#weighted density estimates
#Repeated measures anova for weighted means

#Subset post bleaching regions and years for downstream driver analysis
REGION_YEAR<-c("MHI_2016","NWHI_2016","NMARIAN_2017","SMARIAN_2017","Phoenix_2018","Line_2018","SAMOA_2018")

site.data.gen2$REGION_YEAR<-paste(site.data.gen2$REGION,site.data.gen2$OBS_YEAR,sep="_")

data.gen_pb<-site.data.gen2[site.data.gen2$REGION_YEAR %in% REGION_YEAR,]
head(data.gen_pb)

#Remove strata that have less than 2 sites/stratum
st.list<-ddply(data.gen_pb,.(OBS_YEAR,REGION,ISLAND,SEC_NAME,STRATANAME),summarize,n=length(unique(SITE)))
st.list2<-subset(st.list,n>=2);head(st.list);st.list2<-droplevels(st.list2)
data.gen_pb<-data.gen_pb[data.gen_pb$STRATANAME %in% c(data.gen_pb$STRATANAME),]

View(data.gen_pb) #double check that strata were dropped correctly

#POOL UP
#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
data.gen_pb$ANALYSIS_SCHEMA<-data.gen_pb$STRATANAME
data.gen_pb$DOMAIN_SCHEMA<-data.gen_pb$ISLAND
data.gen_pb$ANALYSIS_YEAR<-data.gen_pb$OBS_YEAR
data.gen_pb$DB_RZ<-paste(data.gen_pb$DEPTH_BIN,data.gen_pb$REEF_ZONE,sep="_")

#Create a vector of columns to subset for strata estimates
c.keep<-c("REGION","ISLAND","ANALYSIS_YEAR","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GENUS_CODE",
          "n_h","N_h","D._h","SE_D._h")

jcdG_st<-Calc_Strata(data.gen_pb,"GENUS_CODE","JuvColDen","Juvpres.abs");jcdG_st=jcdG_st[,c.keep]
colnames(jcdG_st)<-c("REGION","ISLAND","ANALYSIS_YEAR","Stratum","REEF_ZONE","DB_RZ","GENUS_CODE","n","Ntot","JuvColDen","SE_JuvColDen")

#Double Check that revised pooling is adding up NH (total sites) correctly
View(jcdG_st)
View(sectors)


#Calculate Island Estimates
jcdG_is<-Calc_Domain(data.gen_pb,"GENUS_CODE","JuvColDen","Juvpres.abs")
jcdG_is<-jcdG_is[,c("REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen")]

#Calculate Sector Estimates
data.gen_pb$ANALYSIS_SCHEMA<-data.gen_pb$STRATANAME
data.gen_pb$DOMAIN_SCHEMA<-data.gen_pb$SEC_NAME

jcdG_sec<-Calc_Domain(data.gen_pb,"GENUS_CODE","JuvColDen","Juvpres.abs")
jcdG_sec<-jcdG_sec[,c("REGION","ANALYSIS_YEAR","DOMAIN_SCHEMA","GENUS_CODE","n","Ntot","Mean_JuvColDen","SE_JuvColDen")]

# write.csv(jcdG_st,file="T:/Benthic/Projects/Juvenile Project/JuvProject_pb_STRATA.csv")
# write.csv(jcdG_is,file="T:/Benthic/Projects/Juvenile Project/JuvProject_pb_ISLAND.csv")
# write.csv(jcdG_sec,file="T:/Benthic/Projects/Juvenile Project/JuvProject_pb_SECTOR.csv")

write.csv(jcdG_st,"JuvProject_pb_STRATA.csv")
write.csv(jcdG_is,"JuvProject_pb_ISLAND.csv")
write.csv(jcdG_sec,"JuvProject_pb_SECTOR.csv")


#Plot Juv Density by habitat type

#Identify sites with NA in HABITAT_CODE
data.gen_pb[is.na(data.gen_pb$HABITAT_CODE),]


#Convert habitat codes to a consolidate list of codes

data.gen_pb<-data.gen_pb %>% mutate(HAB_R1=recode(HABITAT_CODE, 
                                     `AGR`="Carbonate Reef",
                                     `APR`="Carbonate Reef",
                                     `APS`="Carbonate Reef",
                                     `WAL`="Carbonate Reef",
                                     `PAV`="Pavement",
                                     `PPR`="Pavement",
                                     `RRB`="Rubble",
                                     `ROB`="Rock & Boulder",
                                     `SAG`="Spur & Groove",
                                     `SCR`="Reef with Sand",
                                     `PSC`="Reef with Sand"))

data.gen_pb<-data.gen_pb %>% mutate(HAB_R2=recode(HABITAT_CODE, 
                                                  `AGR`="Carbonate Reef",
                                                  `APR`="Carbonate Reef",
                                                  `APS`="Carbonate Reef",
                                                  `WAL`="Carbonate Reef",
                                                  `PAV`="Basalt",
                                                  `PPR`="Basalt",
                                                  `RRB`="Rubble",
                                                  `ROB`="Basalt",
                                                  `SAG`="Carbonate Reef",
                                                  `SCR`="Carbonate Reef",
                                                  `PSC`="Basalt"))

data.gen_pb<-data.gen_pb %>% mutate(HAB_COMPLEX=recode(HABITAT_CODE, 
                                                  `AGR`="2",
                                                  `APR`="2",
                                                  `APS`="2",
                                                  `WAL`="2",
                                                  `PAV`="1",
                                                  `PPR`="2",
                                                  `RRB`="3",
                                                  `ROB`="1",
                                                  `SAG`="2",
                                                  `SCR`="2",
                                                  `PSC`="1"))
data.gen_pb$HAB_COMPLEX<-as.numeric(data.gen_pb$HAB_COMPLEX)

hab<-ddply(subset(data.gen_pb,GENUS_CODE=="SSSS"),.(OBS_YEAR,ISLAND,DEPTH_BIN,HABITAT_CODE),
           summarize,
           aveJuvDen=mean(JuvColDen),
           SEJuvDen=std.error(JuvColDen),
           n=length(SITE))

plot2<-ggplot(hab, aes(x=HABITAT_CODE, y=aveJuvDen, fill=DEPTH_BIN)) + 
  geom_bar(position=position_dodge(), stat="identity") + 
  # guides(fill=FALSE) 
  facet_wrap(~ISLAND, scales="free_x") +
  geom_errorbar(aes(ymin=aveJuvDen-SEJuvDen, ymax=aveJuvDen+SEJuvDen),width=.15, position=position_dodge(.9))+
  theme_bw() +
  geom_text(data = hab, aes(x = factor(HABITAT_CODE),  y=aveJuvDen+SEJuvDen+5, label = n), position = position_dodge(1), size = 2.25, color = "black", hjust = 'left')+
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="bottom")+
  labs(x="Habitat Code",y="Mean Juvenile Density/m2")

plot2


hab<-ddply(subset(data.gen_pb,GENUS_CODE=="SSSS"),.(OBS_YEAR,REGION,DEPTH_BIN,HAB_R1),
           summarize,
           aveJuvDen=mean(JuvColDen),
           SEJuvDen=std.error(JuvColDen),
           n=length(SITE))

hab2<-ggplot(hab, aes(x=HAB_R1, y=aveJuvDen, fill=DEPTH_BIN)) + 
  geom_bar(position=position_dodge(), stat="identity") + 
  # guides(fill=FALSE) 
  facet_wrap(~REGION, scales="free_x") +
  geom_errorbar(aes(ymin=aveJuvDen-SEJuvDen, ymax=aveJuvDen+SEJuvDen),width=.15, position=position_dodge(.9))+
  geom_text(data = hab, aes(x = factor(HAB_R1),  y=aveJuvDen+SEJuvDen+5, label = n), position = position_dodge(1), size = 2.25, color = "black", hjust = 'left')+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="bottom")+
  labs(x="Habitat Code",y="Mean Juvenile Density/m2")

hab2

hab<-ddply(subset(data.gen_pb,GENUS_CODE=="SSSS"),.(OBS_YEAR,REGION,DEPTH_BIN,HAB_R2),
           summarize,
           aveJuvDen=mean(JuvColDen),
           SEJuvDen=std.error(JuvColDen),
           n=length(SITE))

hab3<-ggplot(hab, aes(x=HAB_R2, y=aveJuvDen, fill=DEPTH_BIN)) + 
  geom_bar(position=position_dodge(), stat="identity") + 
  # guides(fill=FALSE) 
  facet_wrap(~REGION, scales="free_x") +
  geom_errorbar(aes(ymin=aveJuvDen-SEJuvDen, ymax=aveJuvDen+SEJuvDen),width=.15, position=position_dodge(.9))+
  geom_text(data = hab, aes(x = factor(HAB_R2),  y=aveJuvDen+SEJuvDen+2, label = n), position = position_dodge(1), size = 2.25, color = "black", hjust = 'left')+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="bottom")+
  labs(x="Habitat Code",y="Mean Juvenile Density/m2")

hab3

ggsave(plot=hab2,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Figures/DensityvGeneralHabtype1.pdf",width=8,height=8)
ggsave(plot=hab3,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Figures/DensityvGeneralHabtype2.pdf",width=8,height=8)



#Take home: There really is no diserable differences across habitat and regions that would allow me to convert habitat to continuous variable

#Tried plotting density vs. general habitat complexity based on habitat code- nope, don't use this method
hab<-ddply(data.gen_pb,.(OBS_YEAR,REGION,DEPTH_BIN,GENUS_CODE,STRATANAME),
           summarize,
           aveJuvDen=mean(JuvColDen),
           habcomplex=mean(HAB_COMPLEX))

habS<-subset(hab,GENUS_CODE=="SSSS")

hab4<-habS %>%
  mutate(DEPTH_BIN = fct_relevel(DEPTH_BIN,"Shallow","Mid","Deep")) %>% #reorder varibles
  mutate(REGION = fct_relevel(REGION,"NWHI","MHI","Phoenix","Line","SAMOA","SMARIAN","NMARIAN")) %>% #reorder varibles 
  ggplot(aes(x=habcomplex, y=aveJuvDen, color=REGION)) + 
  geom_smooth(se=FALSE,method="loess",lwd=1.5)+
  geom_point()+
  facet_wrap(~DEPTH_BIN)+
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001)
    ,axis.text.x = element_text(angle = 90)) + # adjust x axis to lower the same amount as the genus labels
  labs(x="Year",y="Abs. Change Mean Density")+
  scale_x_continuous(breaks=seq(2013,2019,1))
p2

