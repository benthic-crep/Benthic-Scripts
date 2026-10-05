cov=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2024_Tier1_SITE.csv")
cov3=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2024_Tier3_SITE.csv")

library(tidyverse)
library(lubridate)
cov$DATE_=ymd(cov$DATE_)
cov$OBS_YEAR
cov3$DATE_=ymd(cov3$DATE_)
cov$ISLAND

cov %>% filter(ISLAND%in%c("Howland","Baker","Jarvis")) %>% ggplot(aes(x=DATE_))+
  geom_point(aes(y=CORAL),color="blue")+
  geom_point(aes(y=I),color="gold")+
  geom_point(aes(y=CCA),color="pink")+
  stat_smooth(aes(y=CORAL),method="loess",color="blue")+
  stat_smooth(aes(y=I),method="loess",color="gold")+
  stat_smooth(aes(y=CCA),method="loess",color="pink")+
  facet_grid(.~ISLAND)+
  theme_bw()

cov %>% filter(ISLAND%in%c("Howland","Baker","Jarvis")) %>% ggplot(aes(x=factor(OBS_YEAR)))+
  geom_point(aes(y=CORAL),color="blue")+
  geom_point(aes(y=I),color="gold")+
  geom_point(aes(y=CCA),color="pink")+
  geom_violin(aes(y=CORAL),fill="blue",alpha=.25)+
  geom_violin(aes(y=I),fill="gold",alpha=.25)+
  geom_violin(aes(y=CCA),fill="pink",alpha=.25)+
  # stat_smooth(aes(y=CORAL),method="loess",color="blue")+
  # stat_smooth(aes(y=I),method="loess",color="gold")+
  # stat_smooth(aes(y=CCA),method="loess",color="pink")+
  facet_grid(.~ISLAND)+
  theme_bw()

cov3 %>% filter(ISLAND%in%c("Howland")) %>% ggplot(aes(x=LONGITUDE_LOV))+geom_point(aes(y=CMOR))
cov3$cov100=100
cov$cov100=100


cov$OBS_YEAR_POOL=cov$OBS_YEAR
cov$OBS_YEAR_POOL[cov$OBS_YEAR_POOL>=2017]=2025
cov %>% filter(ISLAND%in%c("Howland","Baker","Jarvis")) %>%
  ggplot(aes(x=LONGITUDE_LOV,y=LATITUDE_LOV))+
  geom_point(aes(size=cov100),color="gray")+
  geom_point(aes(size=TURF),color="brown")+
  geom_point(aes(size=CORAL),color="blue")+
  geom_point(aes(size=MA),color="green")+
  geom_point(aes(size=I),color="gold")+
  facet_wrap(ISLAND~OBS_YEAR_POOL,scales="free",ncol=3)+
  theme_bw()+coord_cartesian()
cov3=left_join(cov3,cov[c("SITEVISITID","CORAL","CCA")],by="SITEVISITID")

benpca=cov3 %>% filter(REGION=="PRIAs") %>% select(11:114)
benpca=cov %>% filter(ISLAND=="Howland",LONGITUDE_LOV<(-176.6175)) %>% select(11:19)
benpca=cov %>% filter(REGION=="PRIAs") %>% select(11:19)
pca=benpca %>% princomp()

benpca$RUBBBLY=benpca$CCA+benpca$TURFR
pca=benpca %>% select(-RUB,-CCAR,-TURFR) %>% princomp()
plot(pca)
plot(pca$loadings["RUBBBLY",])

iR=1#which.max(abs(pca$loadings["CCAH",]))
iC=2#which.max(abs(pca$loadings["CMOR",]))
scores=as.data.frame(pca$scores)
scores$XAXIS=scores[,iR]
scores$YAXIS=scores[,iC]
load=data.frame(XAXIS=pca$loadings[,c(iR)],YAXIS=pca$loadings[,c(iC)]);load$Name=as.vector(row.names(load))
sc=50
ggplot(scores,aes(x=XAXIS,y=YAXIS))+
  geom_point()+
  geom_segment(aes(x = 0,xend=sc*XAXIS,y=0,yend = sc*YAXIS),data=load)+
  geom_text(aes(x = sc*XAXIS,y=sc*YAXIS,label=Name),data=load,color="gold",size=10)

cov3 %>pcacov3 %>% filter(REGION=="PRIAs") %>%
ggplot(aes(x=CMOR,y=CCA))+
  facet_wrap("REGION")+
  geom_point(size=4)


cov %>% filter(ISLAND%in%c("Howland","Baker","Jarvis")) %>%
  ggplot(aes(x=LONGITUDE_LOV,y=LATITUDE_LOV))+
  geom_point(aes(size=cov100),color="gray")+
  geom_point(aes(size=CORAL+I),color="blue")+
  facet_wrap(ISLAND~OBS_YEAR,scales="free",ncol=5)+
  theme_bw()+coord_cartesian()




cov3 %>% filter(ISLAND%in%c("Howland","Baker","Jarvis")) %>% ggplot(aes(x=DATE_))+
  geom_point(aes(y=POCS),color="blue")+
  geom_point(aes(y=POMA),color="lightblue")+
  geom_point(aes(y=MOFO),color="darkblue")+
  geom_point(aes(y=CMOR),color="gold")+
  geom_point(aes(y=CCAH),color="pink")+
  stat_smooth(aes(y=POCS),method="loess",color="blue")+
  stat_smooth(aes(y=POMA),method="loess",color="lightblue")+
  stat_smooth(aes(y=MOFO),method="loess",color="darkblue")+
  stat_smooth(aes(y=CMOR),method="loess",color="gold")+
  stat_smooth(aes(y=CCAH),method="loess",color="pink")+
  facet_grid(.~ISLAND)+
  theme_bw()


