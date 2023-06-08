#Diver Comparision
rm(list=ls())

df<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/REA_CoralDemography/QC and Diver Comparisions/April 2022/Apr2022REA_TrainingDives.csv")
df$SiteSeg<-paste(df$Site,df$Segment,sep="_")
df$DateTax<-paste(df$DATE_,df$Taxon,sep="_")

#Formatting tweaks-------------------------------------------------
a<-filter(df,AdJuv=="Adult")
j<-filter(df,AdJuv=="Juv")

#Calculate Density- adults
a.sum<-ddply(a,.(DATE_,DateTax,Diver,Taxon),
             summarize,
             Count=sum(Count))
a.area<-ddply(a,.(DATE_,Diver),
              summarize,
              nseg=length(unique(SiteSeg)),
              TotalArea=nseg*2.5)

a.sum<-left_join(a.sum,a.area)
a.sum$ColDensity<-a.sum$Count/a.sum$TotalArea

#Calculate Density- juvs

j.sum<-ddply(j,.(DATE_,DateTax,Diver,Taxon),
             summarize,
             Count=sum(Count))
j.area<-ddply(j,.(DATE_,Diver),
              summarize,
              nseg=length(unique(SiteSeg)),
              TotalArea=nseg*2.5)

j.sum<-left_join(j.sum,j.area)
j.sum$ColDensity<-j.sum$Count/j.sum$TotalArea



#Counts
a.sum<-subset(a.sum,Taxon %in% c("MCAP","MPAT","PLOB","PMEA","PVAR"))
a<-subset(a,Taxon %in% c("MCAP","MPAT","PLOB","PMEA","PVAR"))


p1<-
  ggplot(a.sum,aes(x=Diver, y=ColDensity, fill=Taxon)) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~DATE_,scales="free_x")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank())+
  ggtitle("Adult Density By Dominant Taxon")

p1

p2<-
  ggplot(a.sum,aes(x=Diver, y=ColDensity, fill=Diver)) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~DATE_,scales="free_x")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank()
    ,legend.position = "none")+
  ggtitle("Adult Density")

p2

p3<-
  ggplot(a.sum,aes(x=Diver, y=ColDensity, fill=Diver)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_wrap(~DateTax,scales="free_x")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank())+
  ggtitle("Adult Density By Dominant Taxon")

p3

p4<-
  ggplot(j.sum,aes(x=Diver, y=ColDensity, fill=Taxon)) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~DATE_,scales="free_x")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank())+
  ggtitle("Juv Count By Taxon")

p4

size<-a[,c(1,2,3,5,9,10,17)]
size<-pivot_longer(size,cols=c("MinSize","MaxSize"),names_to = "Max_Min",values_to = "Size_cm")
p5<-size %>%
  mutate(Max_Min = fct_relevel(Max_Min, 
                            "MinSize","MaxSize")) %>%
  ggplot(aes(x=Diver, y=Size_cm, fill=Max_Min)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_wrap(~DateTax,scales="free_x")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank())+
  ggtitle("Adult Size By Taxon")

p5

mort<-a[,c(1,2,3,5,11,12,17)]

mort2<-pivot_longer(mort,cols=c("MinDead","MaxDead"),names_to = "Min_Max",values_to = "Perct_Dead")
p6<-mort2 %>%
  mutate(Min_Max = fct_relevel(Min_Max, 
                               "MinDead","MaxDead")) %>%
  ggplot(aes(x=Diver, y=Perct_Dead, fill=Min_Max)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_wrap(~DateTax,scales="free_x")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank())+
  ggtitle("Adult % Dead By Taxon")

p6


ggsave(p1,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/REA_CoralDemography/QC and Diver Comparisions/April 2022/StackAdultCountByTaxon.png",width=8,height=8)
ggsave(p2,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/REA_CoralDemography/QC and Diver Comparisions/April 2022/AdultTotalCount.png",width=8,height=8)
ggsave(p3,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/REA_CoralDemography/QC and Diver Comparisions/April 2022/AdultCountByTaxon.png",width=8,height=8)
ggsave(p4,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/REA_CoralDemography/QC and Diver Comparisions/April 2022/StackJuvenileCountByTaxon.png",width=8,height=8)
ggsave(p5,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/REA_CoralDemography/QC and Diver Comparisions/April 2022/AdultSizeByTaxon.png",width=8,height=8)
ggsave(p6,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/REA_CoralDemography/QC and Diver Comparisions/April 2022/AdultDeadByTaxon.png",width=8,height=8)
