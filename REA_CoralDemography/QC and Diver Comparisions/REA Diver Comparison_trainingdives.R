#Diver Comparision
rm(list=ls())

#Set Run Flags
DEBUG=TRUE

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

dz<-read.csv("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/REA_CoralDemography/QC and Diver Comparisions/February 2022/Feb2022DiverComparison_DZ.csv")

df<-read.csv("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/REA_CoralDemography/QC and Diver Comparisions/February 2022/Feb2022DiverComparison.csv")
df<-df[,c(1:11)]
df$SegTax<-paste(df$Segment,df$Taxon,sep="_")

#Formatting tweaks-------------------------------------------------
a<-filter(df,AdJuv=="A")
j<-filter(df,AdJuv=="J")


#Counts
p1<-
  ggplot(data.gen,aes(x=Diver, y=Count, fill=Taxon)) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~Segment,scales="free_x")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank())+
  ggtitle("Adult Count By Taxon")

p1

p2<-
  ggplot(a,aes(x=Diver, y=Count, fill=Diver)) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~Segment,scales="free_x")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank()
    ,legend.position = "none")+
  ggtitle("Adult Total Count")

p2

#p3<-
  ggplot(data.gen[data.gen$GENUS_CODE != "SSSS" & data.gen$AdColCount > 0,],aes(x=ANALYST, y=AdColCount, fill=ANALYST)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_wrap(~GENUS_CODE,scales="free_y") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank())+
  ggtitle("Adult Count By Taxon")

p3

p4<-
  ggplot(j,aes(x=Diver, y=Count, fill=Taxon)) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~Segment,scales="free_x")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank())+
  ggtitle("Juv Count By Taxon")

p4

size<-a[,c(2,3,4,6,7,12)]
size<-pivot_longer(size,cols=c("MinSize","MaxSize"),names_to = "Max_Min",values_to = "Size_cm")
p5<-size %>%
  mutate(Max_Min = fct_relevel(Max_Min, 
                            "MinSize","MaxSize")) %>%
  ggplot(aes(x=Diver, y=Size_cm, fill=Max_Min)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_wrap(~SegTax,scales="free_x")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank())+
  ggtitle("Adult Size By Taxon")

p5

mort<-a[,c(2,3,4,8,9,12)]
mort2<-pivot_longer(mort,cols=c("MinDead","MaxDead"),names_to = "Min_Max",values_to = "Perct_Dead")
p6<-mort2 %>%
  mutate(Min_Max = fct_relevel(Min_Max, 
                               "MinDead","MaxDead")) %>%
  ggplot(aes(x=Diver, y=Perct_Dead, fill=Min_Max)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_wrap(~SegTax,scales="free_x")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank())+
  ggtitle("Adult % Dead By Taxon")

p6

mort3 <- mort2 %>% 
  group_by(Segment,Diver,Min_Max)%>% 
  summarise(meanDead=mean(Perct_Dead,na.rm=T)) 

p7<-mort3 %>%
  mutate(Min_Max = fct_relevel(Min_Max, 
                               "MinDead","MaxDead")) %>%
  ggplot(aes(x=Diver, y=meanDead, fill=Min_Max)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_wrap(~Segment,scales="free_x")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank())+
  ggtitle("Adult % Dead")

p7

#Disease
dz<-pivot_longer(dz,cols=c(DZ1:DZ4),names_to = "DZ",values_to = "DZType")
dz$DZType<-as.factor(dz$DZType)
dz<-subset(dz,DZType !="")

dz$DZType<-ifelse(dz$DZType %in% c("FISH","GAST","PRED"),"PRED",as.character(dz$DZType))

dz_sum<-ddply(dz,.(Segment,Diver,DZType),summarize,Count=length(DZType))

p8<-
  ggplot(dz_sum,aes(x=Diver, y=Count, fill=DZType)) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~Segment,scales="free_x")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank())+
  ggtitle("DZ/Condition")

p8


ggsave(p1,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/REA_CoralDemography/QC and Diver Comparisions/February 2022/StackAdultCountByTaxon.png",width=8,height=8)
ggsave(p2,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/REA_CoralDemography/QC and Diver Comparisions/February 2022/AdultTotalCount.png",width=8,height=8)
ggsave(p3,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/REA_CoralDemography/QC and Diver Comparisions/February 2022/AdultCountByTaxon.png",width=8,height=8)
ggsave(p4,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/REA_CoralDemography/QC and Diver Comparisions/February 2022/StackJuvenileCountByTaxon.png",width=8,height=8)
ggsave(p5,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/REA_CoralDemography/QC and Diver Comparisions/February 2022/AdultSizeByTaxon.png",width=8,height=8)
ggsave(p6,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/REA_CoralDemography/QC and Diver Comparisions/February 2022/AdultDeadByTaxon.png",width=8,height=8)
ggsave(p7,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/REA_CoralDemography/QC and Diver Comparisions/February 2022/AdultDeadTotal.png",width=8,height=8)
ggsave(p8,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/REA_CoralDemography/QC and Diver Comparisions/February 2022/Disease.png",width=8,height=8)
