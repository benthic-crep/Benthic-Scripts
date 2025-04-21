library(tidyverse)
library(ggrepel)
DT17=read.csv("C:/Users/Thomas.Oliver/Downloads/DT2017- Table S2 Species abundance summary.csv")
names(DT17)[c(5:11)]=c("Occ.Pct","Abund.MN","Abund.OA","Abund.OA.deep","Abund.OA.shallow","Pct.Diff","ER.Occ.Pct")

head(DT17)
Adt17=DT17 %>% filter(Genus=="Acropora")
sum(Adt17$Petition)

Adt17$PorT=factor(2*Adt17$Listed+Adt17$Petition)

Adt17 %>% ggplot(aes(x=Abund.MN,y=ER.Occ.Pct))+
  geom_point()+
  geom_point(shape=21,fill="blue",size=3,data=Adt17 %>% filter(Petition==1),alpha=.5)+
  geom_point(shape=21,fill="red",size=3,data=Adt17 %>% filter(Listed==1),alpha=.5)+
  geom_hline(yintercept = max(Adt17 %>% filter(Listed==1) %>% pull((ER.Occ.Pct))),color="red",lty=2)+
  geom_vline(xintercept = max(Adt17 %>% filter(Listed==1) %>% pull((Abund.MN))),color="red",lty=2)+
  geom_text_repel(aes(label=Species.name,color=PorT),
                  data=Adt17 %>% filter(Petition==1|Listed==1))+
  scale_color_manual(guide="none",values=c("blue","red"))+
  scale_x_sqrt()+
  scale_y_sqrt()+
  xlab("Mean Abundance (Devantier and Turk 2017")+
  ylab("Distribution Size (% of Ecoregions Found) (Devantier and Turak 2017)")+coord_flip()+
  theme_bw()


DT17 %>% 
  filter(Genus%in%c("Acropora","Porites","Pocillopora","Oxypora")) %>% 
  ggplot(aes(Occ.Pct,ER.Occ.Pct,color=Genus))+
  geom_point(aes(size=Abund.OA))+
  geom_point(shape=21,fill="blue",size=3,data=DT17 %>% filter(Petition==1),alpha=.5)+
  geom_point(shape=21,fill="red",size=3,data=DT17 %>% filter(Listed==1),alpha=.5)+
  scale_x_sqrt()+
  scale_y_sqrt()+
  scale_color_discrete(guide="none")+
  facet_wrap("Genus")+
  geom_label_repel(aes(label=Species.name),alpha=.1)+
  theme_bw()

DT17 %>% 
  filter(Genus%in%c("Acropora","Porites","Pocillopora","Oxypora")) %>% 
  ggplot(aes(Occ.Pct,ER.Occ.Pct,color=Genus))+
  geom_point(aes(size=Abund.OA))+
  geom_point(shape=21,fill="blue",size=3,data=DT17 %>% filter(Petition==1),alpha=.5)+
  geom_point(shape=21,fill="red",size=3,data=DT17 %>% filter(Listed==1),alpha=.5)+
  scale_x_sqrt()+
  scale_y_sqrt()+
  scale_color_discrete(guide="none")+
  facet_wrap("Genus")+
  geom_label_repel(aes(label=Species.name),alpha=.1)+
  theme_bw()



DT17 %>% 
  filter(Genus%in%c("Acropora","Porites","Pocillopora","Oxypora")) %>% 
  ggplot(aes(Abund.MN,Occ.Pct,color=Genus))+
  geom_point(aes(size=ER.Occ.Pct))+
  geom_point(shape=21,fill="blue",size=3,data=DT17 %>% filter(Petition==1),alpha=.5,color="black")+
  geom_point(shape=21,fill="red",size=3,data=DT17 %>% filter(Listed==1),alpha=.5,color="black")+
  scale_x_sqrt()+
  scale_y_sqrt()+
  scale_color_discrete(guide="none")+
  facet_wrap("Genus")+
  geom_label_repel(aes(label=Species.name),alpha=.75,size=2)+
  theme_bw()


DT17 %>% 
  filter(Genus%in%c("Acropora","Porites","Pocillopora","Oxypora")) %>% 
  ggplot(aes(ER.Occ.Pct,Occ.Pct,color=Genus))+
  geom_point(aes(size=Abund.MN))+
  geom_point(shape=21,fill="blue",size=3,data=DT17 %>% filter(Petition==1),alpha=.5,color="black")+
  geom_point(shape=21,fill="red",size=3,data=DT17 %>% filter(Listed==1),alpha=.5,color="black")+
  scale_x_sqrt()+
  scale_y_sqrt()+
  scale_color_discrete(guide="none")+
  facet_wrap("Genus")+
  geom_label_repel(aes(label=Species.name),alpha=.75,size=2)+
  theme_bw()

DT17 %>% ggplot(aes(x=Abund.OA,fill=Genus))+geom_histogram()

DT17 %>% ggplot(aes(x=Abund.OA,fill=Genus))+geom_histogram()
