
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

df<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvDen_Pred_SITE_AllYears.csv")#Combined juvenile delta density and all predictors
df<-filter(df,ISLAND !="Guguan") #only 1 year

RI<-unique(df[,c("REGION","ISLAND")])


hs.all<-read.csv("T:/Benthic/Projects/Juvenile Project/Juvenile_HSTimeline.csv")

hs.all <- mutate_if(hs.all, 
                    is.character, 
                    str_replace_all, pattern = " ", replacement = "_")

hs.all<-left_join(RI,hs.all)

table(hs.all$ISLAND,hs.all$REGION)

class(df$DATE_)
df$DATE_<-ymd(df$DATE_)
juvR.date <- df %>%
  group_by(REGION,OBS_YEAR) %>%
  summarise(DATE_=median(DATE_))

juvR.date$ANALYSIS_YEAR<-juvR.date$OBS_YEAR

# Temporal Trends in Juveniles --------------------------------------------
data.temporal <-read.csv("T:/Benthic/Projects/Juvenile Project/JuvDen_Temporal.csv")
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

#Merge regional mean and median dates
temp_Rmean$ANALYSIS_YEAR<-as.factor(temp_Rmean$ANALYSIS_YEAR)
juvR.date$ANALYSIS_YEAR<-as.factor(juvR.date$ANALYSIS_YEAR)

temp_Rmean<-left_join(temp_Rmean,juvR.date)
head(temp_Rmean)

#bar plot of juv by region by year with post hoc tests 
temp_Rmean$REGION <- factor(temp_Rmean$REGION, levels = c("MHI","WAKE","PHOENIX","LINE","SMI","NMI","SAMOA"))
temp_Rmean$ANALYSIS_YEAR<-as.factor(temp_Rmean$ANALYSIS_YEAR)
#Add Posthoc groupings from glms
temp_Rmean<- temp_Rmean[order(temp_Rmean$REGION),];temp_Rmean
temp_Rmean$sig<-c("ab","b","a","","","","","a","b","","","a","b","","")


p8 <- ggplot(temp_Rmean, aes(x=ANALYSIS_YEAR, y=JuvColDen,color=REGION)) +
  geom_point()+
  #geom_bar(stat = "identity", position = position_dodge2(preserve='single'), width = 1, color="black") +
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
  scale_fill_manual(values = c("#D55E00","#E69F00","#F0E442","#009E73","#56B4E9","#0072B2","#999999")) +
  xlab("Year") +
  ylab("Mean Juvenile Colonies/m^2") +
  scale_y_continuous(expand = c(0,0), limits = c(0,16)) +
  geom_text(aes(x=ANALYSIS_YEAR,y=JuvColDen+se,label=sig, group = REGION),
            position = position_dodge(),
            vjust = -0.5) 
p8

hs.allR<-hs.all %>%
  group_by(REGION,DATE_) %>%
  summarise(MeanDHW=mean(MeanDHW))

hs.allR$DATE_<-ymd(hs.allR$DATE_)

#Some dates are missing so there are outlier points- remove these
#Line Islands
jar<-unique(subset(hs.all,ISLAND=="Jarvis")[,c("REGION","DATE_")])
pal<-unique(subset(hs.all,ISLAND=="Palmyra")[,c("REGION","DATE_")])
kin<-unique(subset(hs.all,ISLAND=="Kingman")[,c("REGION","DATE_")])

li<- jar   %>% 
  inner_join(pal,) %>%
  inner_join(kin)

li$DATE_<-ymd(li$DATE_)

ll<-subset(hs.allR,REGION=="LINE")
line.date<-ll %>% filter(DATE_ %in% li$DATE_)
hs.allR<-subset(hs.allR,REGION!="LINE")
hs.allR<-rbind(hs.allR,line.date)

#NWHI
ffs<-unique(subset(hs.all,ISLAND=="French_Frigate")[,c("REGION","DATE_")])
kur<-unique(subset(hs.all,ISLAND=="Kure")[,c("REGION","DATE_")])
lis<-unique(subset(hs.all,ISLAND=="Lisianski")[,c("REGION","DATE_")])
phr<-unique(subset(hs.all,ISLAND=="Pearl_&_Hermes")[,c("REGION","DATE_")])

nwhi<- ffs   %>% 
  inner_join(kur,) %>%
  inner_join(lis) %>%
  inner_join(phr)
  
nwhi$DATE_<-ymd(nwhi$DATE_)

ll<-subset(hs.allR,REGION=="NWHI")
nwhi.date<-ll %>% filter(DATE_ %in% nwhi$DATE_)
hs.allR<-subset(hs.allR,REGION!="NWHI")
hs.allR<-rbind(hs.allR,nwhi.date)


coeff=1


tl.plot<-
  ggplot() +
  facet_wrap( ~ REGION, nrow = 8) +
  geom_point(data=temp_Rmean, aes(y=JuvColDen,x=DATE_)) +
  geom_errorbar(data=temp_Rmean,aes(y=JuvColDen, x=DATE_,ymin=JuvColDen-se, ymax=JuvColDen+se), width=1,size=1)+
  geom_line(data=hs.allR, aes(y=MeanDHW, x= DATE_), size=1) +
  geom_hline(yintercept=4,linetype = "dashed",color="orange")+
  geom_hline(yintercept=8,linetype = "dashed",color="red")+
    theme_bw()+
  scale_x_date(breaks=date_breaks("1 year"))+
  scale_y_continuous(
    # Features of the first axis
    name = "Mean Juvenile Colonies/m2",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Degree C-Weeks")
  )

tl.plot



tl.plot<-
  ggplot() +
  facet_wrap( ~ REGION, nrow = 8) +
  geom_line(data=hs.allR, aes(y=MeanDHW, x= DATE_), size=1) +
  geom_hline(yintercept=4,linetype = "dashed",color="orange")+
  geom_hline(yintercept=8,linetype = "dashed",color="red")+
  theme_bw()+
  scale_x_date(breaks=date_breaks("1 year"))

tl.plot

setwd("T:/Benthic/Projects/Juvenile Project/Figures/Patterns/")
png(width = 650, height = 750, filename = "HS_Juv_Timeline.png")
tl.plot
dev.off()


hs.allR$REGION <- factor(hs.allR$REGION, levels = c("NWHI","MHI","WAKE","PHOENIX","LINE","SMI","NMI","SAMOA"))
#Add Posthoc groupings from glms
hs.allR<- hs.allR[order(hs.allR$REGION),];hs.allR

juvR.date$REGION <- factor(juvR.date$REGION, levels = c("NWHI","MHI","WAKE","PHOENIX","LINE","SMI","NMI","SAMOA"))
#Add Posthoc groupings from glms
juvR.date<- juvR.date[order(juvR.date$REGION),];juvR.date


juvR.date$MeanDHW<-1
hs.tl<-
  ggplot() +
  facet_wrap( ~ REGION, nrow = 8) +
  geom_point(data=juvR.date, aes(y=MeanDHW,x=DATE_),shape=4,size=3) +
  geom_line(data=hs.allR, aes(y=MeanDHW, x= DATE_), size=1) +
  geom_hline(yintercept=4,linetype = "dashed",color="orange")+
  geom_hline(yintercept=8,linetype = "dashed",color="red")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "none",
        axis.line = element_line(color = "black"),
        text = element_text(size = 12),
        axis.text.y = element_text(colour="black"))+
  scale_x_date(breaks=date_breaks("1 year"))+
  ylab("Mean Degree C-weeks")+
  xlab("")

hs.tl


#bar plot of juv by region by year with post hoc tests 
data.temporal <-read.csv("T:/Benthic/Projects/Juvenile Project/JuvDen_Temporal.csv")
#Use survey package to calculate mean SE and conduct statistical analyses
data.temporal$ANALYSIS_YEAR<-as.factor(data.temporal$ANALYSIS_YEAR)
des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+REGION+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=data.temporal)

#Calculate regional mean and SE
temp_Rmean<-svyby(~JuvColDen,~ANALYSIS_YEAR+REGION,des,svymean)

temp_Rmean<-as.data.frame(temp_Rmean);rownames(temp_Rmean) <- NULL

nw<-NULL
nw$ANALYSIS_YEAR<-c("2015","2016","2017")
nw$REGION<-c("NWHI","NWHI","NWHI")
nw$JuvColDen<-NA
nw$se<-NA
nw<-as.data.frame(nw)
nw

temp_Rmean<-rbind(temp_Rmean,nw)

temp_Rmean$REGION <- factor(temp_Rmean$REGION, levels = c("NWHI","MHI","WAKE","PHOENIX","LINE","SMI","NMI","SAMOA"))
temp_Rmean$ANALYSIS_YEAR<-as.factor(temp_Rmean$ANALYSIS_YEAR)
#Add Posthoc groupings from glms
temp_Rmean<- temp_Rmean[order(temp_Rmean$REGION),];temp_Rmean
temp_Rmean$sig<-c("","","","ab","b","a","","","","","a","b","","","a","b","","")

juv.tl <- ggplot(temp_Rmean, aes(x=ANALYSIS_YEAR, y=JuvColDen)) +
  geom_point(size=3)+
  geom_errorbar(aes(y=JuvColDen, x=ANALYSIS_YEAR,ymin=JuvColDen-se, ymax=JuvColDen+se), width=.1)+
  facet_wrap(~REGION,scales= "free_x", nrow=8,strip.position = "right") +
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
        axis.text.y = element_text(colour="black"))+
  xlab("") +
  ylab("Mean Juvenile Colonies/m^2") +
  scale_y_continuous(expand = c(0,0), limits = c(0,16)) +
  geom_text(aes(x=ANALYSIS_YEAR,y=JuvColDen+se,label=sig, group = REGION),
            position = position_dodge(),
            vjust = -0.5) 
juv.tl

# save full plot
setwd("T:/Benthic/Projects/Juvenile Project/Figures/Patterns/")
#ytitle <- text_grob("Predicted Juvenile Colonies/m2", size = 18, face = "bold", rot = 90)
png(width = 1050, height = 950, filename = "HS_Juv_timelinev2.png")
grid.arrange(arrangeGrob(hs.tl + ggtitle("a)"),
                         juv.tl + ggtitle("b)"),
                         ncol = 2,widths=c(2,1)))
dev.off()
