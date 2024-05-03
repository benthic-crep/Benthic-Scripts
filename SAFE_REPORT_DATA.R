library(tidyverse)
#Read Complete and TimeSeries Cover Data
isl_cover_CO=read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/COMPLETE/BenthicCover_2010-2023_Tier1_ISLAND_Complete_Viztool.csv")
isl_cover_TR=read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/TRENDS/BenthicCover_2010-2023_Tier1_ISLAND_Trends_Viztool.csv")
# aaa=load("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/BIA_2010-2023_CLEANED.RData")
# nab=subset(ab,REGION=="NWHI")
# (table(nab$ISLAND,nab$OBS_YEAR))

#Prepare propoer geographic order for REGION, REGION_NAME, and ISLAND Factors; Modify Island Names for Plotting (ISLANDp)
REGION_order=c("NWHI","MHI","CNMI","GUA","PRIAs","SAMOA")
REGION_NAME_order=c("Northwestern Hawaiian Islands","Main Hawaiian Islands","Commonwealth of the Northern Mariana Islands","Guam","American Samoa")
ISL_order=c("Kure","Midway","Pearl & Hermes","Lisianski","Laysan","Maro","Gardner","French Frigate","Necker","Nihoa",
            "Kauai","Niihau","Oahu","Molokai","Maui","Lanai","Kahoolawe","Hawaii",
            "Farallon de Pajaros","Maug","Asuncion","Agrihan","Pagan","Sarigan, Alamagan, Guguan","Saipan","Tinian","Aguijan","Rota","Guam",
            "Wake","Johnston","Kingman","Palmyra","Howland","Baker","Jarvis",
            "Swains","Ofu & Olosega","Tau","Tutuila","Rose")
ISLp_order=ISL_order
ISLp_order[ISLp_order=="Ofu & Olosega"]="O&O"
ISLp_order[ISLp_order=="Farallon de Pajaros"]="FDP"
ISLp_order[ISLp_order=="Sarigan, Alamagan, Guguan"]="AGS"
ISLp_order[ISLp_order=="Pearl & Hermes"]="P&H"
ISLp_order[ISLp_order=="French Frigate"]="FFS"
uI_lu=ISLp_order;names(uI_lu)=ISL_order

#Order the factors
isl_cover_CO$REGION=factor(isl_cover_CO$REGION,levels=REGION_order)
isl_cover_TR$REGION=factor(isl_cover_TR$REGION,levels=REGION_order)
isl_cover_CO$REGION_NAME=factor(isl_cover_CO$REGION_NAME,levels=REGION_NAME_order)
isl_cover_TR$REGION_NAME=factor(isl_cover_TR$REGION_NAME,levels=REGION_NAME_order)
isl_cover_CO$ISLAND=factor(isl_cover_CO$ISLAND,levels=ISL_order)
isl_cover_TR$ISLAND=factor(isl_cover_TR$ISLAND,levels=ISL_order)
isl_cover_CO$ISLANDp=factor(uI_lu[isl_cover_CO$ISLAND],levels=ISLp_order)
isl_cover_TR$ISLANDp=factor(uI_lu[isl_cover_TR$ISLAND],levels=ISLp_order)

#Convert ANALYSIS_YEARs into NCRMP_PERIODs
#Build LookUp
NCRMP_PERIODS=c("2010-12",
                "2013-15",
                "2016-18",
                "2019-23")
NP_lu=NCRMP_PERIODS[c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4)]
names(NP_lu)=sort(unique(isl_cover_CO$ANALYSIS_YEAR))
#Convert
isl_cover_TR$NCRMP_PERIOD=NP_lu[isl_cover_TR$ANALYSIS_YEAR]
isl_cover_CO$NCRMP_PERIOD=NP_lu[isl_cover_CO$ANALYSIS_YEAR]

#Generate All Island Cover Averages, rolling up SE across ANALYSIS_YEARS
reg_cover_COallt=isl_cover_CO %>% group_by(REGION) %>% 
  dplyr::summarize(N=sum(N),Mean.CORAL=mean(Mean.CORAL),SE.CORAL=sqrt(sum(SE.CORAL^2)))
reg_cover_TRallt=isl_cover_TR %>% group_by(REGION) %>% 
  dplyr::summarize(N=sum(N),Mean.CORAL=mean(Mean.CORAL),SE.CORAL=sqrt(sum(SE.CORAL^2)))
isl_cover_COallt=isl_cover_CO %>% group_by(REGION,ISLANDp) %>% 
  dplyr::summarize(N=sum(N),Mean.CORAL=mean(Mean.CORAL),SE.CORAL=sqrt(sum(SE.CORAL^2)))

#Generate Island By NCRMP_PERIOD Cover Averages, rolling up SE across ANALYSIS_YEARS
isl_cover_TRnp=isl_cover_TR %>% group_by(REGION,REGION_NAME,ISLAND,ISLANDp,NCRMP_PERIOD) %>% 
  dplyr::summarize(N=sum(N),Mean.CORAL=mean(Mean.CORAL),SE.CORAL=sqrt(sum(SE.CORAL^2)))
isl_cover_COnp=isl_cover_CO %>% group_by(REGION,REGION_NAME,ISLAND,ISLANDp,NCRMP_PERIOD) %>% 
  dplyr::summarize(N=sum(N),Mean.CORAL=mean(Mean.CORAL),SE.CORAL=sqrt(sum(SE.CORAL^2)))


###PLOTTING
#Island-scale mean coral cover 2010-2023 by latitude
Alltr=isl_cover_COallt %>% 
  ggplot(aes(x=ISLANDp,y=Mean.CORAL,ymin=Mean.CORAL-SE.CORAL,ymax=Mean.CORAL+SE.CORAL))+
  geom_col(aes(fill=REGION),width=1,color="black")+
  geom_errorbar(width=.1)+
  geom_hline(aes(yintercept=Mean.CORAL),color="darkred",data=reg_cover_COallt)+
  facet_grid("Hard Coral"~REGION,drop=T,scales="free_x", space = "free_x")+
  ylab("Hard Coral Cover (%)")+
  xlab("")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90),strip.background = element_rect(fill="gray75",color = "gray75"))+
  scale_fill_brewer(guide="none",palette = "YlOrRd")
Alltr

sc=1
ggsave(plot = Alltr,
       filename = "T:/Benthic/Data/Data Requests/SAFE_2023/All_Islands_Mean_CORAL_Cover_2010-2023.jpg",
       width = sc*8.5,height = sc*11/3)

#REGIONAL PLOTS - MHI
MHI_cover_TR=isl_cover_TRnp %>% filter(REGION=="MHI")
MHI_cover_TR=rbind(MHI_cover_TR,subset(isl_cover_COnp,ISLAND=="Kahoolawe"))

MHItr=MHI_cover_TR %>% 
  ggplot(aes(x=NCRMP_PERIOD,y=Mean.CORAL,ymin=Mean.CORAL-SE.CORAL,ymax=Mean.CORAL+SE.CORAL))+
  geom_col(aes(fill=ISLANDp),color="black")+
  geom_errorbar(width=.2)+
  geom_hline(aes(yintercept=Mean.CORAL),color="darkred",data=subset(reg_cover_TRallt,REGION=="MHI"))+
  facet_grid("Hard Coral"~ISLANDp)+
  ylab("Hard Coral Cover (%)")+
  xlab("")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90),strip.background = element_rect(fill="gray75",color = "gray75"))+
  scale_fill_brewer(guide="none",palette = "YlOrRd")
MHItr

sc=1
ggsave(plot = MHItr,
       filename = "T:/Benthic/Data/Data Requests/SAFE_2023/MHI_Mean_CORAL_Cover_2010-2023.jpg",
       width = sc*8.5,height = sc*11/3)

#REGIONAL PLOTS - NWHI
NWHItr=isl_cover_COnp %>% filter(REGION=="NWHI") %>% 
  ggplot(aes(x=NCRMP_PERIOD,y=Mean.CORAL,ymin=Mean.CORAL-SE.CORAL,ymax=Mean.CORAL+SE.CORAL))+
  geom_col(aes(fill=ISLANDp),color="black")+
  geom_errorbar(width=.2)+
  geom_hline(aes(yintercept=Mean.CORAL),color="darkred",data=subset(reg_cover_TRallt,REGION=="MHI"))+
  facet_grid("Hard Coral"~ISLANDp)+
  ylab("Hard Coral Cover (%)")+
  xlab("")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90),strip.background = element_rect(fill="gray75",color = "gray75"))+
  scale_fill_brewer(guide="none",palette = "YlOrRd")
NWHItr

sc=1
ggsave(plot = NWHItr,
       filename = "T:/Benthic/Data/Data Requests/SAFE_2023/NWHI_Mean_CORAL_Cover_2010-2023.jpg",
       width = sc*8.5,height = sc*11/3)

#REGIONAL PLOTS - CNMI

CNMItrp=isl_cover_TRnp %>% filter(REGION=="CNMI") %>% 
  ggplot(aes(x=NCRMP_PERIOD,y=Mean.CORAL,ymin=Mean.CORAL-SE.CORAL,ymax=Mean.CORAL+SE.CORAL))+
  geom_col(aes(fill=ISLANDp),color="black")+
  geom_errorbar(width=.1)+
  geom_hline(aes(yintercept=Mean.CORAL),color="darkred",data=subset(reg_cover_TRallt,REGION=="MHI"))+
  facet_grid("Hard Coral"~ISLANDp)+
  ylab("Hard Coral Cover (%)")+
  xlab("")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90),strip.background = element_rect(fill="gray75",color = "gray75"))+
  scale_fill_brewer(guide="none",palette = "YlOrRd")
CNMItrp

sc=1
ggsave(plot = CNMItrp,
       filename = "T:/Benthic/Data/Data Requests/SAFE_2023/CNMI_Mean_CORAL_Cover_2010-2023.jpg",
       width = sc*8.5,height = sc*11/3)


#REGIONAL PLOTS - Guam
GUAtrp=isl_cover_TRnp %>% filter(REGION=="GUA") %>% 
  ggplot(aes(x=NCRMP_PERIOD,y=Mean.CORAL,ymin=Mean.CORAL-SE.CORAL,ymax=Mean.CORAL+SE.CORAL))+
  geom_col(aes(fill=ISLANDp),color="black")+
  geom_errorbar(width=.1)+
  geom_hline(aes(yintercept=Mean.CORAL),color="darkred",data=subset(reg_cover_TRallt,REGION=="MHI"))+
  facet_grid("Hard Coral"~ISLANDp)+
  ylab("Hard Coral Cover (%)")+
  xlab("")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90),strip.background = element_rect(fill="gray75",color = "gray75"))+
  scale_fill_brewer(guide="none",palette = "YlOrRd")
GUAtrp

sc=1
ggsave(plot = GUAtrp,
       filename = "T:/Benthic/Data/Data Requests/SAFE_2023/GUA_Mean_CORAL_Cover_2010-2023.jpg",
       width = sc*8.5,height = sc*11/3)


#REGIONAL PLOTS - AS
AStrp=isl_cover_TRnp %>% filter(REGION=="SAMOA") %>% 
  ggplot(aes(x=NCRMP_PERIOD,y=Mean.CORAL,ymin=Mean.CORAL-SE.CORAL,ymax=Mean.CORAL+SE.CORAL))+
  geom_col(aes(fill=ISLANDp),color="black")+
  geom_errorbar(width=.1)+
  geom_hline(aes(yintercept=Mean.CORAL),color="darkred",data=subset(reg_cover_TRallt,REGION=="MHI"))+
  facet_grid("Hard Coral"~ISLANDp)+
  ylab("Hard Coral Cover (%)")+
  xlab("")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90),strip.background = element_rect(fill="gray75",color = "gray75"))+
  scale_fill_brewer(guide="none",palette = "YlOrRd")
AStrp

sc=1
ggsave(plot = AStrp,
       filename = "T:/Benthic/Data/Data Requests/SAFE_2023/AS_Mean_CORAL_Cover_2010-2023.jpg",
       width = sc*8.5,height = sc*11/3)

