#This script summarizes juvenile data from NCRMP 2013-2019 at the site, stratum, island and sector level
#It also identifies which sectors and strata have been surveyed in all years
#It calculate delta density

#Ends with summarized site-level data between 2015-2019 


rm(list=ls())

#Set Run Flags
DEBUG=TRUE

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
#source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
#source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")


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
library(multcompView)

setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project")

#LOAD DATA
jwd<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Juveniles_raw_CLEANED.csv")

tmp<-unique(jwd[,c("OBS_YEAR","REGION","ISLAND","REEF_ZONE","DEPTH_BIN","SITE","TRANSECT")])

table(tmp$REGION,tmp$OBS_YEAR)


#Tweaks before calculating Site-level data-------------------------------------------------
#Colony fragments and scleractinans are subsetted in the functions 
#Add a column for adult fragments so we can remove them from the dataset later (-1 indicates fragment)
jwd$Fragment <- 0 # you need to add this column so that you can use the site level functions correctly
jwd$DATE_ <- as.Date(jwd$DATE_, format = "%Y-%m-%d")
jwd$METHOD<-"DIVER"
jwd$ANALYST<-jwd$DIVER
jwd$SEGAREA<-jwd$SEGLENGTH*jwd$SEGWIDTH

#Create a table of all of the colony attributes- you will need this the functions below
SURVEY_SITE<-c("METHOD","MISSIONID","DATE_","SITEVISITID", "ANALYSIS_YEAR","OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE","HABITAT_CODE","REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")
survey_site<-unique(jwd[,SURVEY_SITE])

#TEMPORARY WORK AROUND-ASK MICHAEL TO FIX
jwd$REEF_ZONE<-ifelse(jwd$SITE=="HAW-04285","Forereef",as.character(jwd$REEF_ZONE))

#Convert Protected Slope to Forereef
jwd$REEF_ZONE<-ifelse(jwd$REEF_ZONE=="Protected Slope","Forereef",as.character(jwd$REEF_ZONE))

#Remove 2 sites that weren't surveyed for juveniles
jwd<-jwd[!(jwd$SITE %in% c("OFU-01012","PAG-00596")),]


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

site.data.gen<-dplyr::filter(jcd.gen,TRANSECT %in% c("1","3"))#subseting first transect (different, different transect numbering was used over the years)
summary(site.data.gen$TRANSECT)

site.data.gen2<-dplyr::select(site.data.gen, -c(TRANSECT)) #Create a copy (it takes a long time to run the transect function above)

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

#Make tweaks to pooling sector pooling and drop specific islands
isl.drop<-c("Alamagan","Midway","Maro","Niihau","Johnston","Guguan","Agrihan") 
site.data.gen2<-site.data.gen2[!site.data.gen2$ISLAND %in% c(isl.drop),] #Remove islands that don't have enough strata sampled across the years 

#Not enough sampling in each sector- pool them together
site.data.gen2$SEC_NAME<-ifelse(site.data.gen2$SEC_NAME %in% c("TAU_OPEN","TAU_SANCTUARY"),"TAU",as.character(site.data.gen2$SEC_NAME))
site.data.gen2$SEC_NAME<-ifelse(site.data.gen2$SEC_NAME %in% c("SWA_OPEN","SWA_SANCTUARY"),"SWA",as.character(site.data.gen2$SEC_NAME))
View(site.data.gen2)


#Change all special missions to exclude flag =-1, right now they are 0. Then exclude these sites
#Exclude PRIA 2017 sites because we want similar time intervals following bleaching events for all regions
levels(site.data.gen2$MISSIONID)
site.data.gen2<-site.data.gen2[!site.data.gen2$MISSIONID %in% c("MP1410","MP1512","MP1602","SE1602","MP2006"),] 
site.data.gen2$Year_Island<-paste(site.data.gen2$OBS_YEAR,site.data.gen2$ISLAND,sep="_")
site.data.gen2<-site.data.gen2[!site.data.gen2$Year_Island %in% c("2017_Baker","2017_Jarvis","2017_Howland"),] 

site.data.gen2<-droplevels(site.data.gen2);levels(site.data.gen2$MISSIONID)
View(site.data.gen2)

#Convert Protected Reef Slope to Forereef and Subset just Forereef sites
site.data.gen2$REEF_ZONE<-ifelse(site.data.gen2$REEF_ZONE=="Protected Slope","Forereef",as.character(site.data.gen2$REEF_ZONE))
site.data.gen2<-subset(site.data.gen2,REEF_ZONE=="Forereef")

#Add up NH for forereef and protected reef sites
tmp<-unique(site.data.gen2[,c("OBS_YEAR","SEC_NAME","REEF_ZONE","DEPTH_BIN","NH")])

new.NH<-ddply(tmp,.(OBS_YEAR,SEC_NAME,REEF_ZONE,DEPTH_BIN),
              summarize, new.NH=sum(NH))
nrow(site.data.gen2) #double check that the same number of rows are present before and after joining
site.data.gen2<-left_join(site.data.gen2,new.NH)
site.data.gen2<-subset(site.data.gen2,select=-c(NH)) #remove old NH column
site.data.gen2<-site.data.gen2 %>% rename(NH=new.NH) #rename NH
nrow(site.data.gen2)#double check that the same number of rows are present before and after joining

table(site.data.gen2$ISLAND,site.data.gen2$OBS_YEAR)


#Change Region Names -breaking up marianas and PRIAs into subregions because of broad geography and thermal history
site.data.gen2$REGION<-ifelse(site.data.gen2$ISLAND %in% c("Maug", "Asuncion", "Alamagan", "Pagan", "Agrihan", "Guguan", "Sarigan","Farallon de Pajaros")
                              ,"NMI", as.character(site.data.gen2$REGION))
site.data.gen2$REGION<-ifelse(site.data.gen2$ISLAND %in% c("Saipan", "Tinian", "Aguijan", "Rota", "Guam")
                              ,"SMI", as.character(site.data.gen2$REGION))
site.data.gen2$REGION<-ifelse(site.data.gen2$ISLAND %in% c("Howland","Baker")
                              ,"PHOENIX", as.character(site.data.gen2$REGION))
site.data.gen2$REGION<-ifelse(site.data.gen2$ISLAND =="Wake"
                              ,"WAKE", as.character(site.data.gen2$REGION))
site.data.gen2$REGION<-ifelse(site.data.gen2$ISLAND %in% c("Kingman","Palmyra","Jarvis")
                              ,"LINE", as.character(site.data.gen2$REGION))

site.data.gen2$STRATANAME<- paste(site.data.gen2$SEC_NAME,site.data.gen2$REEF_ZONE,site.data.gen2$DEPTH_BIN,sep="_")
site.data.gen2$REGION_YEAR<-paste(site.data.gen2$REGION,site.data.gen2$OBS_YEAR,sep="_")
site.data.gen2$DB_RZ<- paste(site.data.gen2$REEF_ZONE,site.data.gen2$DEPTH_BIN,sep="_")

# #Remove the 2014 NWHI data
# site.data.gen2<-site.data.gen2[site.data.gen2$REGION_YEAR !="NWHI_2014",]

length(unique(site.data.gen2$SITE))


#Calculate survey weights (inverse proportion weighting)
w.df<-ddply(site.data.gen2,.(ANALYSIS_YEAR,SEC_NAME,REEF_ZONE,DEPTH_BIN,NH),
            summarize,
            n=length(unique(SITE))) #quantify # of sites/stratum

w.df$sw<-w.df$NH/w.df$n #calculate survey weights for each site

site.sw<-left_join(site.data.gen2,w.df) #merge weights with site-level data
head(site.sw)
site.swS<-dplyr::filter(site.sw,GENUS_CODE=="SSSS")#only include total scleractinans

tmp<-unique(site.swS[,c("OBS_YEAR","REGION","ISLAND","REEF_ZONE","DEPTH_BIN","SITE")])
View(subset(tmp,OBS_YEAR=="2018"&ISLAND=="Swains"))


#remove strata that have less than 2 sites
site.swS<-subset(site.swS,n>1)
summary(site.swS$n)

tmp<-unique(site.swS[,c("OBS_YEAR","REGION","ISLAND","REEF_ZONE","DEPTH_BIN","SITE")])
View(subset(tmp,OBS_YEAR=="2018"&ISLAND=="Swains"))


write.csv(site.swS,file="T:/Benthic/Projects/Juvenile Project/JuvProject_SITE_weights_AllYears.csv",row.names = F)


# GENERATE DATA FOR TEMPORAL ANALYSIS WITH 2013 MHI---------------------------------------------------
site.swS$STRATANAME<- paste(site.swS$SEC_NAME,site.swS$REEF_ZONE,site.swS$DEPTH_BIN,sep="_")
st.list<-ddply(site.swS,.(OBS_YEAR,REGION,ISLAND,SEC_NAME,STRATANAME),summarize,n=length(unique(SITE)))

#Generate list of strata that were surveyed in all years for a given region and had at least 2 sites/stratum
st.list_w<-dcast(st.list, formula=REGION+ISLAND+SEC_NAME+STRATANAME~ OBS_YEAR, value.var="n",fill=0)
dCOLS<-c("2013","2014","2015","2016","2017","2018","2019")
st.list_w$year_n<-rowSums(st.list_w[,dCOLS] > 0, na.rm=T) #count # of years of data
st.list_w2<-subset(st.list_w,REGION %in% c("NMI","SMI","LINE","PHOENIX","WAKE","SAMOA") & year_n>=2)
st.list_w3<-subset(st.list_w,REGION %in% c("NWHI","MHI") & year_n>=3)
st.list_w4<-rbind(st.list_w2,st.list_w3)

head(st.list_w4);st.list_w4<-droplevels(st.list_w4) #generate the list

data.gen_temp<-site.swS[site.swS$STRATANAME %in% c(st.list_w4$STRATANAME),] #Subset juv data to only include strata of intersest 
isl.drop<-c("Maui","Niihau","Kure","French Frigate","Midway","Pearl & Hermes","Lisianski") #excluding all NWHI islands- poor sampling across years
data.temporal<-data.gen_temp[!data.gen_temp$ISLAND %in% c(isl.drop),] #Remove islands that don't have enough strata sampled across the years 
View(data.temporal)

write.csv(data.temporal,file="T:/Benthic/Projects/Juvenile Project/JuvDen_Temporal.csv",row.names = FALSE)

# Temporal Trends in Juveniles --------------------------------------------

#Use survey package to calculate mean SE and conduct statistical analyses
data.temporal$ANALYSIS_YEAR<-as.factor(data.temporal$ANALYSIS_YEAR)
des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+REGION+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=data.temporal)

#Calculate regional mean and SE
temp_Rmean<-svyby(~JuvColDen,~ANALYSIS_YEAR+REGION,des,svymean)

#Test fixed effects of region and year
modR<-svyglm(JuvColCount ~ REGION*ANALYSIS_YEAR, design=des,offset= TRANSECTAREA_j, family="poisson")

svystdres(modR,stvar="DB_RZ",doplot=TRUE)
anova(modR)

#Run separate post hoc tests for each region to test for differences between years- I don't care about comparing all possible combinations of year and region
library(multcomp)

mhi.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(data.temporal,REGION=="MHI"))
mhi<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=mhi.des,offset= TRANSECTAREA_j, family="poisson")
summary(glht(mhi, mcp(ANALYSIS_YEAR="Tukey"))) 

wake.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(data.temporal,REGION=="WAKE"))
wake<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=wake.des,offset= TRANSECTAREA_j, family="poisson")
summary(glht(wake, mcp(ANALYSIS_YEAR="Tukey"))) 

ph.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(data.temporal,REGION=="PHOENIX"))
ph<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=ph.des,offset= TRANSECTAREA_j, family="poisson")
summary(glht(ph, mcp(ANALYSIS_YEAR="Tukey"))) 

line.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(data.temporal,REGION=="LINE"))
l<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=line.des,offset= TRANSECTAREA_j, family="poisson")
summary(glht(l, mcp(ANALYSIS_YEAR="Tukey"))) 

sm.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(data.temporal,REGION=="SMI"))
sm<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=sm.des,offset= TRANSECTAREA_j, family="poisson")
summary(glht(sm, mcp(ANALYSIS_YEAR="Tukey"))) 

nm.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(data.temporal,REGION=="NMI"))
nm<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=nm.des,offset= TRANSECTAREA_j, family="poisson")
summary(glht(nm, mcp(ANALYSIS_YEAR="Tukey")))

sam.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(data.temporal,REGION=="SAMOA"))
sam<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=sam.des,offset= TRANSECTAREA_j, family="poisson")
summary(glht(sam, mcp(ANALYSIS_YEAR="Tukey"))) 


#Calculate adjusted pvalues for multiple test corrections
pvals<-c(0.54424,0.03636,0.00345,0.348,0.299,0.000001,0.263,0.000295,0.391)
round(p.adjust(pvals, "BH"), 3) #0.568 0.082 0.010 0.447 0.447 0.000 0.447 0.001 0.568


# PLOTTING ----------------------------------------------------------------

#bar plot of juv by region by year with post hoc tests 
temp_Rmean$REGION <- factor(temp_Rmean$REGION, levels = c("MHI","NMI","WAKE","SMI","SAMOA","LINE","PHOENIX"))
temp_Rmean$ANALYSIS_YEAR<-as.factor(temp_Rmean$ANALYSIS_YEAR)
#Add Posthoc groupings from glms
temp_Rmean<- temp_Rmean[order(temp_Rmean$REGION),];temp_Rmean
temp_Rmean$sig<-c("ab","a","b","a","b","","","","","","","a","b","","")

#scale_fill_manual(values = c("#CC79A7","#D55E00","#E69F00","#F0E442","#009E73","#56B4E9","#0072B2","#999999")) +
  
p8 <- ggplot(temp_Rmean, aes(x=ANALYSIS_YEAR, y=JuvColDen,fill=REGION)) +
  #geom_bar(stat = "identity", position = position_dodge2(preserve='single'), width = 1, color="black") +
  geom_errorbar(aes(y=JuvColDen, x=ANALYSIS_YEAR,ymin=JuvColDen-se, ymax=JuvColDen+se), width=.2)+
  geom_point(color="black",pch=21,size=4)+
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
        axis.text.x = element_text(angle = 90,size=14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold")) +
  scale_fill_manual(values = c("#D55E00","#E69F00","#F0E442","#009E73","#56B4E9","#0072B2","#999999")) +
  labs(x="",y=expression(paste("Mean Juvenile Colonies ",m^-2)))+
  scale_y_continuous(expand = c(0,0), limits = c(0,16)) +
  geom_text(aes(x=ANALYSIS_YEAR,y=JuvColDen+se,label=sig, group = REGION),
            position = position_dodge(),
            vjust = -0.5) 
p8

ggsave(plot=p8,file="T:/Benthic/Projects/Juvenile Project/Figures/DensityRegionalTemporal.jpg",width=10,height=5)



# Spatial Trends in Juveniles averaged across years --------------------------------------------
# R_Y<-c("MHI_2019","NWHI_2017","NMI_2017","SMI_2017","PHOENIX_2018","LINE_2018","SAMOA_2018","WAKE_2017")
# 
# site.swS$REGION_YEAR<-paste(site.swS$REGION,site.swS$OBS_YEAR,sep="_")
# site.swS<-site.swS[site.swS$REGION_YEAR %in% R_Y,] 


#Remove 2014 NWHI data because we do not have benthic cover data for this year
REGION_YEAR<-c("NWHI_2014")

table(site.swS$REGION,site.swS$OBS_YEAR)
site.swS$REGION_YEAR<-paste(site.swS$REGION,site.swS$OBS_YEAR,sep="_")
site.swS<-site.swS[site.swS$REGION_YEAR != REGION_YEAR,]
table(site.swS$REGION,site.swS$OBS_YEAR)


table(site.swS$ISLAND,site.swS$DB_RZ)
nrow(site.swS) #total number of sites used in the spatial analysis

#Use survey package to calculate mean SE and conduct statistical analyses
site.swS$ANALYSIS_YEAR<-as.factor(site.swS$ANALYSIS_YEAR)
site.swS$REGION<-as.factor(site.swS$REGION)

des<-svydesign(id=~1, strata=~OBS_YEAR+REGION+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=site.swS)

#Calculate regional mean and SE
spatial_Rmean<-svyby(~JuvColDen,~REGION,des,svymean)

#Test fixed effects of region and year
modR<-svyglm(JuvColCount ~ REGION, design=des,offset= TRANSECTAREA_j, family="poisson")
summary(modR)
tuk2<-glht(modR, mcp(REGION="Tukey")) 
tuk.cld2 <- cld(tuk2)
sig<-c("d","bd","a","ab","c","e","a","c")
#sig<-c("a","a","c","ac","b","d","c","b")

spatial_Rmean<-cbind(spatial_Rmean,sig)


svystdres(modR,stvar="DB_RZ",doplot=TRUE)
null.mod<-svyglm(JuvColCount ~ 1, design=des,offset= TRANSECTAREA_j, family="poisson")
anova(null.mod,modR)


#bar plot of juv by region by year with post hoc tests 
spatial_Rmean$REGION <- factor(spatial_Rmean$REGION, levels = c("NWHI","MHI","NMI","WAKE","SMI","SAMOA","LINE","PHOENIX"))
#Add Posthoc groupings from glms
spatial_Rmean<- spatial_Rmean[order(spatial_Rmean$REGION),];spatial_Rmean


spatialR <- ggplot(spatial_Rmean, aes(x=REGION, y=JuvColDen,fill=REGION)) +
  #geom_bar(stat = "identity", position = position_dodge2(preserve='single'), width = 1, color="black") +
  geom_errorbar(aes(y=JuvColDen, x=REGION,ymin=JuvColDen-se, ymax=JuvColDen+se), width=.2)+
  geom_point(color="black",pch=21,size=5)+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 12),
        legend.position = "none",
        axis.line = element_line(color = "black"),
        text = element_text(size = 14),
        axis.text.y = element_text(colour="black"),
        axis.text.x = element_text(angle = 90,size=14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold")) +
  scale_fill_manual(name=NULL, values = c("#CC79A7","#D55E00","#E69F00","#F0E442","#009E73","#56B4E9","#0072B2","#999999")) +
  labs(x="",y=expression(bold(paste("Mean Juvenile Colonies ",m^-2))),title = "B")+
  scale_y_continuous(expand = c(0,0), limits = c(0,16)) +
  geom_text(aes(x=REGION,y=JuvColDen+se,label=sig, group = REGION),
            position = position_dodge(),
            vjust = -0.5,size = 8) 
spatialR

ggsave(plot=spatialR,file="T:/Benthic/Projects/Juvenile Project/Figures/DensityRegionalSpatial.jpg",width=10,height=5)


spatial_Imean<-svyby(~JuvColDen,~REGION + ISLAND,des,svymean)
spatial_Imean$REGION <- factor(spatial_Imean$REGION, levels = c("NWHI","MHI","NMI","WAKE","SMI","SAMOA","LINE","PHOENIX"))


p10 <- ggplot(spatial_Imean, aes(x=reorder(ISLAND,-JuvColDen), y=JuvColDen,color=REGION)) +
  #geom_bar(stat = "identity", position = position_dodge2(preserve='single'), width = 1, color="black") +
  geom_point(size=2)+
  geom_errorbar(aes(y=JuvColDen, x=ISLAND,ymin=JuvColDen-se, ymax=JuvColDen+se), width=.2)+
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
  scale_y_continuous(expand = c(0,0), limits = c(0,25)) +
  scale_color_manual(name=NULL,values = c("#CC79A7","#D55E00","#E69F00","#F0E442","#009E73","#56B4E9","#0072B2","#999999")) +
  labs(x="Island",y=expression(paste("Mean Juvenile Colonies  ",m^-2)))
p10


ggsave(plot=p10,file="T:/Benthic/Projects/Juvenile Project/Figures/DensityIslandSpatial.jpg",width=10,height=5)


# Plot Pacific-wide Map most recent Juvenile Density -------------------------

##Helpful website for plotting maps with ggplot https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html

#https://rpubs.com/valentin/pacific-centered-map-voronoi-tessellation

#Identify median lat and long that surveys were conducted for each island and year
lat.sum<-ddply(site.swS,.(REGION,ISLAND),
               summarize,
               Y=median(LATITUDE,na.rm=T),
               X=median(LONGITUDE,na.rm = T))

j.sum<-ddply(spatial_Imean,.(REGION,ISLAND),
             summarize,
             JuvColDen=mean(JuvColDen,na.rm=T))
juv_coords<-left_join(j.sum,lat.sum)
head(juv_coords)


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
xy <- juv_coords[,c(5,4)]
juv_coords_sp <- SpatialPointsDataFrame(coords = xy, data = juv_coords,
                                        proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#Crop the juv spdf
delta_shift <- juv_coords_sp %>% 
  st_as_sf() %>%
  st_shift_longitude() %>% 
  st_crop(c(xmin = 120, xmax = 250, ymin = -50, ymax = 30)) %>% 
  # Also adds the coordinates to be used for labeling with geom_text_repel
  bind_cols(st_coordinates(.) %>% as.data.frame())

#Create an Inset map using a similar process described above
box_cut2 <- bbox2SP(n = 90, s = -90, w = -120, e = 110, proj4string = world@proj4string)
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


#plot main data map with island colors = gradient of juvenile density
deltamap<-ggplot() +
  geom_sf(data = pacific_crop)+ #basemap
  geom_sf(data = delta_shift,aes(color = JuvColDen), size = 3, shape = 19)+ #data
  geom_text_repel(data = delta_shift, #add island labels 
                  aes(x = X...7, y = Y...8, label = ISLAND),
                  size = 3,
                  fontface = "bold",
                  segment.size = 0.25,
                  box.padding = 0.4,
                  min.segment.length = 0,
                  seed = 2020-5-16)+
  annotation_scale(location = "bl", width_hint = 0.4)+ #add scale bar
  scale_color_gradient2(midpoint = 8, #Color scheme
                        high = 'forestgreen',
                        mid = 'yellow2',
                        low = 'red2',
                        na.value = 'gray95',
                        name=expression(paste("Mean Juvenile Colonies  ",m^-2)))+  #you can add a legend title here if you want
  #theme(legend.position = c(0.9,0.15))
  theme(legend.position = "bottom")
  

#Combine main and inset maps
finalmap = ggdraw() +
  draw_plot(deltamap) +
  draw_plot(spatialR, x = 0.05, y = 0.15, width = 0.3, height = 0.3)+
  #draw_plot(insetmap, x = 0.02, y = 0.07, width = 0.3, height = 0.3)
  draw_plot(insetmap, x = 0.77, y = 0.1, width = 0.23, height = 0.23)
  
finalmap

ggsave(plot=finalmap,file="T:/Benthic/Projects/Juvenile Project/Figures/RecentJuvMap_colorsJuvCol.jpg",width=13,height=9)


#plot main data map with island colors = REGION
delta_shift$REGION <- factor(delta_shift$REGION, levels = c("NWHI","MHI","WAKE","PHOENIX","LINE","SMI","NMI","SAMOA"))
delta_shift<- delta_shift[order(delta_shift$REGION),];delta_shift


deltamap<-ggplot() +
  geom_sf(data = pacific_crop)+ #basemap
  geom_sf(data = delta_shift, aes(fill = REGION),color="black",size = 3, pch=21)+ #data
  geom_text_repel(data = delta_shift, #add island labels 
                  aes(x = X...7, y = Y...8, label = ISLAND),
                  size = 3,
                  fontface = "bold",
                  segment.size = 0.25,
                  box.padding = 0.4,
                  min.segment.length = 0,
                  seed = 2020-5-16)+
  labs(title="A")+
  scale_fill_manual(name=NULL,values = c("#CC79A7","#D55E00","#E69F00","#F0E442","#009E73","#56B4E9","#0072B2","#999999")) +
  annotation_scale(location = "bl", width_hint = 0.4)+ #add scale bar
  #theme(legend.position = c(0.9,0.15))
  theme(legend.position = "none")


#Combine main and inset maps
finalmap = ggdraw() +
  draw_plot(deltamap) +
  draw_plot(spatialR, x = 0.05, y = 0.08, width = 0.5, height = 0.5)+
  #draw_plot(insetmap, x = 0.02, y = 0.07, width = 0.3, height = 0.3)
  draw_plot(insetmap, x = 0.77, y = 0.1, width = 0.23, height = 0.23)

finalmap

ggsave(plot=finalmap,file="T:/Benthic/Projects/Juvenile Project/Figures/Final for Manuscript/Figure 1.jpg",width=13,height=9)

