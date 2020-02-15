
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

## LOAD data
site.data.gen2<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS.csv")
site.data.tax2<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_TAXONCODE.csv")


#Change all special missions to exclude flag =-1, right now they are 0. Then exclude these sites
levels(site.data.gen2$MISSIONID)
site.data.gen2<-site.data.gen2[!site.data.gen2$MISSIONID %in% c("MP1410","MP1512","MP1602","SE1602"),]
site.data.gen2$Year_Island<-paste(site.data.gen2$OBS_YEAR,site.data.gen2$ISLAND,sep="_")
site.data.gen2<-site.data.gen2[!site.data.gen2$Year_Island %in% c("2017_Baker","2017_Jarvis","2017_Howland"),] 

site.data.gen2<-droplevels(site.data.gen2);levels(site.data.gen2$MISSIONID)
View(site.data.gen2)


#Subset forereef and protected reefs slope
site.data.gen2<-subset(site.data.gen2,REEF_ZONE %in% c("Forereef","Protected Slope"))

# Generate data for temporal analysis---------------------------------------------------
sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)
site.data.gen2$STRATANAME<- paste(site.data.gen2$SEC_NAME,site.data.gen2$REEF_ZONE,site.data.gen2$DEPTH_BIN,sep="_")
st.list<-ddply(site.data.gen2,.(OBS_YEAR,REGION,ISLAND,SEC_NAME,STRATANAME),summarize,n=length(unique(SITE)))
st.list2<-subset(st.list,n>=2);head(st.list)

#Generate list of strata that were surveyed in all years for a given region and had at least 2 sites/stratum
st.list_w<-dcast(st.list2, formula=REGION+ISLAND+SEC_NAME+STRATANAME~ OBS_YEAR, value.var="n",fill=0)
dCOLS<-c("2013","2014","2015","2016","2017","2018","2019")
st.list_w$year_n<-rowSums(st.list_w[,dCOLS] > 0, na.rm=T) #count # of years of data
st.list_w2<-subset(st.list_w,REGION %in% c("MARIAN","PRIAs","SAMOA") & year_n>=2)
st.list_w3<-subset(st.list_w,REGION %in% c("MHI","NWHI") & year_n>=3)
st.list_w4<-rbind(st.list_w2,st.list_w3)

head(st.list_w4);st.list_w4<-droplevels(st.list_w4)


data.gen_temp<-site.data.gen2[site.data.gen2$STRATANAME %in% c(st.list_w4$STRATANAME),]

View(data.gen_temp) #double check that strata were dropped correctly


#Change Regions
# NMARorder <- c("FDP", "Maug", "Asuncion", "Alamagan", "Pagan", "Agrihan", "Guguan", "Sarigan")
# 
# SMARorder <- c("Saipan", "Tinian", "Aguijan", "Rota", "Guam")


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



write.csv(jcdG_st,file="T:/Benthic/Projects/Juvenile Project/JuvProject_temporal_STRATA.csv")
write.csv(jcdG_is,file="T:/Benthic/Projects/Juvenile Project/JuvProject_temporal_ISLAND.csv")
write.csv(jcdG_sec,file="T:/Benthic/Projects/Juvenile Project/JuvProject_temporal_SECTOR.csv")


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
ggsave(plot=p1,file="T:/Benthic/Projects/Juvenile Project/Figures/Juv_Temporal.pdf",width=10,height=10)



####Absolute Change####

#Wake surveyd in off year- change time point
jcdG_st$ANALYSIS_YEAR[jcdG_st$ISLAND=="Wake"&jcdG_st$ANALYSIS_YEAR=="2014"] <- "2015"
jcdG_st$ANALYSIS_YEAR[jcdG_st$ISLAND=="Wake"&jcdG_st$ANALYSIS_YEAR=="2017"] <- "2018"

jcdG_st$YEAR<-paste("a",jcdG_st$ANALYSIS_YEAR,sep="")

new.df_w<-dcast(jcdG_st, formula= REGION + ISLAND + Stratum + REEF_ZONE + DB_RZ + GENUS_CODE~ YEAR, value.var="JuvColDen",fill=NA)
head(new.df_w)

nwhi<-subset(new.df_w,REGION=="NWHI")
mhi<-subset(new.df_w,REGION=="MHI")
marian<-subset(new.df_w,REGION=="MARIAN")
pria<-subset(new.df_w,REGION=="PRIAs")
samoa<-subset(new.df_w,REGION=="SAMOA")

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

marian$t2013<-NA
marian$t2014<-0
marian$t2015<-NA
marian$t2016<-NA
marian$t2017<-marian$a2017-marian$a2014
marian$t2018<-NA
marian$t2019<-NA

pria$t2013<-NA
pria$t2014<-NA
pria$t2015<-0
pria$t2016<-NA
pria$t2017<-NA
pria$t2018<-pria$a2018-pria$a2015
pria$t2019<-NA

samoa$t2013<-NA
samoa$t2014<-NA
samoa$t2015<-0
samoa$t2016<-NA
samoa$t2017<-NA
samoa$t2018<-samoa$a2018-samoa$a2015
samoa$t2019<-NA
abschange<-rbind(nwhi,mhi,pria,marian,samoa)

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

p1<-ggplot(pc_longS, aes(x=Yearn, y=Change, color=REGION)) + 
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

ggsave(plot=p1,file="T:/Benthic/Projects/Juvenile Project/Figures/Abschange_REGION_points.pdf",width=8,height=6)

p2<-pc_longS %>%
  mutate(DEPTH_BIN = fct_relevel(DEPTH_BIN,"Shallow","Mid","Deep")) %>% #reorder varibles 
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


ggsave(plot=p1,file="T:/Benthic/Projects/Juvenile Project/Figures/Abschange_REGION_points.pdf",width=8,height=6)
ggsave(plot=p2,file="T:/Benthic/Projects/Juvenile Project/Figures/Abschange_REGION_DEPTH_points.pdf",width=8,height=6)
ggsave(plot=p3,file="T:/Benthic/Projects/Juvenile Project/Figures/Density_REGION_DEPTH_points.pdf",width=8,height=6)

#General thoughts on patterns
#The time after the bleaching event is very important. I removed the 2016 and 2017 juvenile data for jarvis, baker and howland. it may be interesting to look at these separately



##Statistical Analyses
head(jcdG_stS)

#Check for normality and equal variance
library(rcompanion)
#Log transform
plotNormalHistogram(jcdG_stS$JuvColDen)
l<-log(jcdG_stS$JuvColDen+1)
plotNormalHistogram(l)
qqnorm(l, ylab="Sample Quantiles for logDensity")
qqline(l, col="red")
mod<-lm(l~Stratum,data=jcdG_stS)
qqnorm(residuals(mod),ylab="Sample Quantiles for residuals")
qqline(residuals(mod), col="red")
jcdG_stS$logDen<-log(jcdG_stS$JuvColDen+1)



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
#Remove TUSP
#Subset juvs >2cm
#weighted density estimates
#Repeated measures anova for weighted means

# Generate post bleaching data --------------------------------------------
data.gen_pb<-subset(site.data.gen2,OBS_YEAR>=2016)

st.list<-ddply(data.gen_pb,.(OBS_YEAR,REGION,ISLAND,SEC_NAME,STRATANAME),summarize,n=length(unique(SITE)))
st.list2<-subset(st.list,n>=2);head(st.list);st.list2<-droplevels(st.list2)
data.gen_pb<-site.data.gen2[site.data.gen2$STRATANAME %in% c(st.list2$STRATANAME),]

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

write.csv(jcdG_st,file="T:/Benthic/Projects/Juvenile Project/JuvProject_pb_STRATA.csv")
write.csv(jcdG_is,file="T:/Benthic/Projects/Juvenile Project/JuvProject_pb_ISLAND.csv")
write.csv(jcdG_sec,file="T:/Benthic/Projects/Juvenile Project/JuvProject_pb_SECTOR.csv")




