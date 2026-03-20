rm(list=ls())
library(tidyverse)

isl=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicCover_2010-2024_Tier1_ISLAND_Trends.csv")
isl2b=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicCover_2010-2024_Tier2b_ISLAND_Trends.csv")
sec=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicCover_2010-2024_Tier1_SECTOR_Trends.csv")
str=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicCover_2010-2024_Tier1_STRATA_Trends.csv")

isl_C=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicCover_2010-2024_Tier1_ISLAND_Complete.csv")
isl2b_C=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicCover_2010-2024_Tier2b_ISLAND_Complete.csv")
sec_C=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Sector/BenthicCover_2010-2024_Tier1_SECTOR_Complete.csv")
str_C=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Stratum/BenthicCover_2010-2024_Tier1_STRATA_Trends.csv")



isl_m = isl %>% filter(REGION=="MHI") %>% mutate(
  Mean.Calc_Cov=Mean.CORAL+Mean.CCA,
  SE.Calc_Cov=sqrt(SE.CORAL^2+SE.CCA^2),
  Mean.Alg_Cov=Mean.TURF+Mean.MA,
  SE.Alg_Cov=sqrt(SE.CORAL^2+SE.CCA^2),
  Mean.ReefBuilderRatio=(Mean.CORAL+Mean.CCA)/(Mean.MA+Mean.TURF),
  SE.ReefBuilderRatio=(Mean.Calc_Cov/Mean.Alg_Cov)*sqrt((SE.Calc_Cov/Mean.Calc_Cov)^2+(SE.Alg_Cov/Mean.Alg_Cov)^2),
)
col=names(isl_m)[c(1:5,15,26,27,30,31)]
isl_m=isl_m %>% select(all_of(col))

col=c(col[1:2],"ANALYSIS_SEC",col[3:length(col)])
sec_m = sec %>% filter(REGION=="MHI")%>% mutate(
  Mean.Calc_Cov=Mean.CORAL+Mean.CCA,
  SE.Calc_Cov=sqrt(SE.CORAL^2+SE.CCA^2),
  Mean.Alg_Cov=Mean.TURF+Mean.MA,
  SE.Alg_Cov=sqrt(SE.CORAL^2+SE.CCA^2),
  Mean.ReefBuilderRatio=(Mean.CORAL+Mean.CCA)/(Mean.MA+Mean.TURF),
  SE.ReefBuilderRatio=(Mean.Calc_Cov/Mean.Alg_Cov)*sqrt((SE.Calc_Cov/Mean.Calc_Cov)^2+(SE.Alg_Cov/Mean.Alg_Cov)^2),
) %>% select(all_of(col))

col=c(col[1:3],"STRATA",col[4:length(col)])
str_m = str %>% filter(REGION=="MHI")%>% mutate(
  Mean.Calc_Cov=Mean.CORAL+Mean.CCA,
  SE.Calc_Cov=sqrt(SE.CORAL^2+SE.CCA^2),
  Mean.Alg_Cov=Mean.TURF+Mean.MA,
  SE.Alg_Cov=sqrt(SE.CORAL^2+SE.CCA^2),
  Mean.ReefBuilderRatio=(Mean.CORAL+Mean.CCA)/(Mean.MA+Mean.TURF),
  SE.ReefBuilderRatio=(Mean.Calc_Cov/Mean.Alg_Cov)*sqrt((SE.Calc_Cov/Mean.Calc_Cov)^2+(SE.Alg_Cov/Mean.Alg_Cov)^2),
) %>% select(all_of(col))

write.csv(x = isl_m,file = "T:/Benthic/Data/Data Requests/GOVE_2026/ISLAND_MHI_CORAL.CALCCOVER.RBR.csv")
write.csv(x = sec_m,file = "T:/Benthic/Data/Data Requests/GOVE_2026/SECTOR_MHI_CORAL.CALCCOVER.RBR.csv")
write.csv(x = str_m,file = "T:/Benthic/Data/Data Requests/GOVE_2026/STRATA_MHI_CORAL.CALCCOVER.RBR.csv")

#######################################################

isl_mC = isl_C %>% filter(REGION=="MHI") %>% mutate(
  Mean.Calc_Cov=Mean.CORAL+Mean.CCA,
  SE.Calc_Cov=sqrt(SE.CORAL^2+SE.CCA^2),
  Mean.Alg_Cov=Mean.TURF+Mean.MA,
  SE.Alg_Cov=sqrt(SE.CORAL^2+SE.CCA^2),
  Mean.ReefBuilderRatio=(Mean.CORAL+Mean.CCA)/(Mean.MA+Mean.TURF),
  SE.ReefBuilderRatio=(Mean.Calc_Cov/Mean.Alg_Cov)*sqrt((SE.Calc_Cov/Mean.Calc_Cov)^2+(SE.Alg_Cov/Mean.Alg_Cov)^2),
)
col=names(isl_mC)[c(1:5,15,26,27,30,31)]
isl_mC=isl_mC %>% select(all_of(col))

col=c(col[1:2],"ANALYSIS_SEC",col[3:length(col)])
sec_mC = sec_C %>% filter(REGION=="MHI")%>% mutate(
  Mean.Calc_Cov=Mean.CORAL+Mean.CCA,
  SE.Calc_Cov=sqrt(SE.CORAL^2+SE.CCA^2),
  Mean.Alg_Cov=Mean.TURF+Mean.MA,
  SE.Alg_Cov=sqrt(SE.CORAL^2+SE.CCA^2),
  Mean.ReefBuilderRatio=(Mean.CORAL+Mean.CCA)/(Mean.MA+Mean.TURF),
  SE.ReefBuilderRatio=(Mean.Calc_Cov/Mean.Alg_Cov)*sqrt((SE.Calc_Cov/Mean.Calc_Cov)^2+(SE.Alg_Cov/Mean.Alg_Cov)^2),
) %>% select(all_of(col))

col=c(col[1:3],"STRATA",col[4:length(col)])
str_mC = str_C %>% filter(REGION=="MHI")%>% mutate(
  Mean.Calc_Cov=Mean.CORAL+Mean.CCA,
  SE.Calc_Cov=sqrt(SE.CORAL^2+SE.CCA^2),
  Mean.Alg_Cov=Mean.TURF+Mean.MA,
  SE.Alg_Cov=sqrt(SE.CORAL^2+SE.CCA^2),
  Mean.ReefBuilderRatio=(Mean.CORAL+Mean.CCA)/(Mean.MA+Mean.TURF),
  SE.ReefBuilderRatio=(Mean.Calc_Cov/Mean.Alg_Cov)*sqrt((SE.Calc_Cov/Mean.Calc_Cov)^2+(SE.Alg_Cov/Mean.Alg_Cov)^2),
) %>% select(all_of(col))

write.csv(x = isl_mC,file = "T:/Benthic/Data/Data Requests/GOVE_2026/ISLAND_MHI_COMPLETE_CORAL.CALCCOVER.RBR.csv")
write.csv(x = sec_mC,file = "T:/Benthic/Data/Data Requests/GOVE_2026/SECTOR_MHI_COMPLETE_CORAL.CALCCOVER.RBR.csv")
write.csv(x = str_mC,file = "T:/Benthic/Data/Data Requests/GOVE_2026/STRATA_MHI_COMPLETE_CORAL.CALCCOVER.RBR.csv")

#######################################################

sc=.9
ggplot(isl_m,aes(x=ANALYSIS_YEAR))+
  geom_point(aes(y=Mean.CORAL),color="blue")+
  geom_errorbar(aes(ymax=Mean.CORAL+SE.CORAL,ymin=Mean.CORAL-SE.CORAL),color="blue")+
  geom_point(aes(y=Mean.Calc_Cov),color="pink")+
  geom_errorbar(aes(ymax=Mean.Calc_Cov+SE.Calc_Cov,ymin=Mean.Calc_Cov-SE.Calc_Cov),color="pink")+
  facet_wrap("ISLAND")+theme_bw()

ggplot(isl_m,aes(x=ANALYSIS_YEAR))+
  geom_point(aes(y=(Mean.ReefBuilderRatio)),color="gold")+
  geom_errorbar(aes(ymax=Mean.ReefBuilderRatio+SE.ReefBuilderRatio,ymin=Mean.ReefBuilderRatio-SE.ReefBuilderRatio),color="gold")+
  facet_wrap("ISLAND")+theme_bw()


ggplot(sec_m,aes(x=ANALYSIS_YEAR))+
  geom_point(aes(y=Mean.CORAL),color="blue")+
  geom_errorbar(aes(ymax=Mean.CORAL+SE.CORAL,ymin=Mean.CORAL-SE.CORAL),color="blue")+
  geom_point(aes(y=Mean.Calc_Cov),color="pink")+
  geom_errorbar(aes(ymax=Mean.Calc_Cov+SE.Calc_Cov,ymin=Mean.Calc_Cov-SE.Calc_Cov),color="pink")+
  geom_point(aes(y=100*Mean.ReefBuilderRatio),color="gold")+
  geom_errorbar(aes(ymax=100*Mean.ReefBuilderRatio+100*SE.ReefBuilderRatio,ymin=100*Mean.ReefBuilderRatio-100*SE.ReefBuilderRatio),color="gold")+
  facet_wrap("ISLAND")+theme_bw()

sec_m$xoff=0.1*rnorm(nrow(sec_m))
CORALplot=ggplot()+
  geom_violin(aes(x=ANALYSIS_YEAR,y=Mean.CORAL),data=str_m,fill="gold")+
  geom_point(aes(x=as.numeric(as.factor(ANALYSIS_YEAR))+xoff,y=Mean.CORAL),data=sec_m,color="darkblue")+
  geom_errorbar(aes(x=as.numeric(as.factor(ANALYSIS_YEAR))+xoff,ymax=Mean.CORAL+SE.CORAL,ymin=Mean.CORAL-SE.CORAL,alpha=.5),
                data=sec_m,color="darkblue",size=1,alpha=.5,width = .25)+
  geom_point(aes(x=ANALYSIS_YEAR,y=Mean.CORAL),data=isl_m,color="darkgreen",size=3)+
  geom_errorbar(aes(x=ANALYSIS_YEAR,ymax=Mean.CORAL+SE.CORAL,ymin=Mean.CORAL-SE.CORAL),
                data=isl_m,color="darkgreen",size=2,width = .25)+
  facet_wrap("ISLAND")+theme_bw()+ggtitle("Coral Cover, at Strata (gold violin), Sector (blue jittered), and Island means (Green), +/- SE")
ggsave(plot = CORALplot,filename = "T:/Benthic/Data/Data Requests/GOVE_2026/CORAL_DescriptivePlot.jpg",width=11*sc,height=8.5*sc)


Calc_Covplot=ggplot()+
  geom_violin(aes(x=ANALYSIS_YEAR,y=Mean.Calc_Cov),data=str_m,fill="gold")+
  geom_point(aes(x=as.numeric(as.factor(ANALYSIS_YEAR))+xoff,y=Mean.Calc_Cov),data=sec_m,color="darkblue")+
  geom_errorbar(aes(x=as.numeric(as.factor(ANALYSIS_YEAR))+xoff,ymax=Mean.Calc_Cov+SE.Calc_Cov,ymin=Mean.Calc_Cov-SE.Calc_Cov,alpha=.5),
                data=sec_m,color="darkblue",size=1,alpha=.5,width = .25)+
  geom_point(aes(x=ANALYSIS_YEAR,y=Mean.Calc_Cov),data=isl_m,color="darkgreen",size=3)+
  geom_errorbar(aes(x=ANALYSIS_YEAR,ymax=Mean.Calc_Cov+SE.Calc_Cov,ymin=Mean.Calc_Cov-SE.Calc_Cov),
                data=isl_m,color="darkgreen",size=2,width = .25)+
  facet_wrap("ISLAND")+theme_bw()+ggtitle("Calicfied Cover (CORAL + CCA), at Strata (gold violin), Sector (blue jittered), and Island means (Green), +/- SE")
ggsave(plot = Calc_Covplot,filename = "T:/Benthic/Data/Data Requests/GOVE_2026/Calc_Cov_DescriptivePlot.jpg",width=11*sc,height=8.5*sc)

ReefBuilderRatioplot=ggplot()+
  geom_violin(aes(x=ANALYSIS_YEAR,y=Mean.ReefBuilderRatio),data=str_m,fill="gold")+
  geom_point(aes(x=as.numeric(as.factor(ANALYSIS_YEAR))+xoff,y=Mean.ReefBuilderRatio),data=sec_m,color="darkblue")+
  geom_errorbar(aes(x=as.numeric(as.factor(ANALYSIS_YEAR))+xoff,ymax=Mean.ReefBuilderRatio+SE.ReefBuilderRatio,ymin=Mean.ReefBuilderRatio-SE.ReefBuilderRatio,alpha=.5),
                data=sec_m,color="darkblue",size=1,alpha=.5,width = .25)+
  geom_point(aes(x=ANALYSIS_YEAR,y=Mean.ReefBuilderRatio),data=isl_m,color="darkgreen",size=3)+
  geom_errorbar(aes(x=ANALYSIS_YEAR,ymax=Mean.ReefBuilderRatio+SE.ReefBuilderRatio,ymin=Mean.ReefBuilderRatio-SE.ReefBuilderRatio),
                data=isl_m,color="darkgreen",size=2,width = .25)+
  facet_wrap("ISLAND")+theme_bw()+ggtitle("Reef Builder Ratio (CORAL + CCA)/(TURF + MA), at Strata (gold violin), Sector (blue jittered), and Island means (Green), +/- SE")
ggsave(plot = ReefBuilderRatioplot,filename = "T:/Benthic/Data/Data Requests/GOVE_2026/ReefBuilderRatio_DescriptivePlot.jpg",width=11*sc,height=8.5*sc)

#######################################################


ggplot(isl_mC,aes(x=ANALYSIS_YEAR))+
  geom_point(aes(y=Mean.CORAL),color="blue")+
  geom_errorbar(aes(ymax=Mean.CORAL+SE.CORAL,ymin=Mean.CORAL-SE.CORAL),color="blue")+
  geom_point(aes(y=Mean.Calc_Cov),color="pink")+
  geom_errorbar(aes(ymax=Mean.Calc_Cov+SE.Calc_Cov,ymin=Mean.Calc_Cov-SE.Calc_Cov),color="pink")+
  facet_wrap("ISLAND")+theme_bw()

ggplot(isl_mC,aes(x=ANALYSIS_YEAR))+
  geom_point(aes(y=(Mean.ReefBuilderRatio)),color="gold")+
  geom_errorbar(aes(ymax=Mean.ReefBuilderRatio+SE.ReefBuilderRatio,ymin=Mean.ReefBuilderRatio-SE.ReefBuilderRatio),color="gold")+
  facet_wrap("ISLAND")+theme_bw()


ggplot(sec_mC,aes(x=ANALYSIS_YEAR))+
  geom_point(aes(y=Mean.CORAL),color="blue")+
  geom_errorbar(aes(ymax=Mean.CORAL+SE.CORAL,ymin=Mean.CORAL-SE.CORAL),color="blue")+
  geom_point(aes(y=Mean.Calc_Cov),color="pink")+
  geom_errorbar(aes(ymax=Mean.Calc_Cov+SE.Calc_Cov,ymin=Mean.Calc_Cov-SE.Calc_Cov),color="pink")+
  geom_point(aes(y=100*Mean.ReefBuilderRatio),color="gold")+
  geom_errorbar(aes(ymax=100*Mean.ReefBuilderRatio+100*SE.ReefBuilderRatio,ymin=100*Mean.ReefBuilderRatio-100*SE.ReefBuilderRatio),color="gold")+
  facet_wrap("ISLAND")+theme_bw()

sc=.9
sec_mC$xoff=0.1*rnorm(nrow(sec_mC))
CORALplotC=ggplot()+
  geom_violin(aes(x=ANALYSIS_YEAR,y=Mean.CORAL),data=str_mC,fill="gold")+
  geom_point(aes(x=as.numeric(as.factor(ANALYSIS_YEAR))+xoff,y=Mean.CORAL),data=sec_mC,color="darkblue")+
  geom_errorbar(aes(x=as.numeric(as.factor(ANALYSIS_YEAR))+xoff,ymax=Mean.CORAL+SE.CORAL,ymin=Mean.CORAL-SE.CORAL,alpha=.5),
                data=sec_mC,color="darkblue",size=1,alpha=.5,width = .25)+
  geom_point(aes(x=ANALYSIS_YEAR,y=Mean.CORAL),data=isl_mC,color="darkgreen",size=3)+
  geom_errorbar(aes(x=ANALYSIS_YEAR,ymax=Mean.CORAL+SE.CORAL,ymin=Mean.CORAL-SE.CORAL),
                data=isl_mC,color="darkgreen",size=2,width = .25)+
  facet_wrap("ISLAND")+theme_bw()+ggtitle("Coral Cover, at Strata (gold violin), Sector (blue jittered), and Island means (Green), +/- SE")
ggsave(plot = CORALplotC,filename = "T:/Benthic/Data/Data Requests/GOVE_2026/CORAL_COMPLETE_DescriptivePlot.jpg",width=11*sc,height=8.5*sc)


Calc_CovplotC=ggplot()+
  geom_violin(aes(x=ANALYSIS_YEAR,y=Mean.Calc_Cov),data=str_mC,fill="gold")+
  geom_point(aes(x=as.numeric(as.factor(ANALYSIS_YEAR))+xoff,y=Mean.Calc_Cov),data=sec_mC,color="darkblue")+
  geom_errorbar(aes(x=as.numeric(as.factor(ANALYSIS_YEAR))+xoff,ymax=Mean.Calc_Cov+SE.Calc_Cov,ymin=Mean.Calc_Cov-SE.Calc_Cov,alpha=.5),
                data=sec_mC,color="darkblue",size=1,alpha=.5,width = .25)+
  geom_point(aes(x=ANALYSIS_YEAR,y=Mean.Calc_Cov),data=isl_mC,color="darkgreen",size=3)+
  geom_errorbar(aes(x=ANALYSIS_YEAR,ymax=Mean.Calc_Cov+SE.Calc_Cov,ymin=Mean.Calc_Cov-SE.Calc_Cov),
                data=isl_mC,color="darkgreen",size=2,width = .25)+
  facet_wrap("ISLAND")+theme_bw()+ggtitle("Calicfied Cover (CORAL + CCA), at Strata (gold violin), Sector (blue jittered), and Island means (Green), +/- SE")
ggsave(plot = Calc_CovplotC,filename = "T:/Benthic/Data/Data Requests/GOVE_2026/Calc_COMPLETE_Cov_DescriptivePlot.jpg",width=11*sc,height=8.5*sc)

ReefBuilderRatioplotC=ggplot()+
  geom_violin(aes(x=ANALYSIS_YEAR,y=Mean.ReefBuilderRatio),data=str_mC,fill="gold")+
  geom_point(aes(x=as.numeric(as.factor(ANALYSIS_YEAR))+xoff,y=Mean.ReefBuilderRatio),data=sec_mC,color="darkblue")+
  geom_errorbar(aes(x=as.numeric(as.factor(ANALYSIS_YEAR))+xoff,ymax=Mean.ReefBuilderRatio+SE.ReefBuilderRatio,ymin=Mean.ReefBuilderRatio-SE.ReefBuilderRatio,alpha=.5),
                data=sec_mC,color="darkblue",size=1,alpha=.5,width = .25)+
  geom_point(aes(x=ANALYSIS_YEAR,y=Mean.ReefBuilderRatio),data=isl_mC,color="darkgreen",size=3)+
  geom_errorbar(aes(x=ANALYSIS_YEAR,ymax=Mean.ReefBuilderRatio+SE.ReefBuilderRatio,ymin=Mean.ReefBuilderRatio-SE.ReefBuilderRatio),
                data=isl_mC,color="darkgreen",size=2,width = .25)+
  facet_wrap("ISLAND")+theme_bw()+ggtitle("Reef Builder Ratio (CORAL + CCA)/(TURF + MA), at Strata (gold violin), Sector (blue jittered), and Island means (Green), +/- SE")
ggsave(plot = ReefBuilderRatioplotC,filename = "T:/Benthic/Data/Data Requests/GOVE_2026/ReefBuilderRatio_COMPLETE_DescriptivePlot.jpg",width=11*sc,height=8.5*sc)

