#Mumby 2024 PNAS https://doi.org/10.1073/pnas.2418314121
#Paper says Allee effect bad at 10 m spacing, severe at 15 m
#Paper argues against using density and for Nearest Conspecific Neighbor

#Pull Colony, Site and Island Level data, then calculate distance by segment comparison
rea=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED_2013.2024.csv")
rea_site=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_SITE_TAXONCODE_COMPLETE_2013_2024.csv")
rea_isl=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicREA_ISLAND_TAXONCODE_COMPLETE_2013_2024.csv")


jar=rea %>% filter(ISLANDCODE=="JAR")
jar_site=rea_site %>% filter(ISLAND=="Jarvis")
jar_isl=rea_isl %>% filter(ISLAND=="Jarvis")

jar_tax=jar# %>% filter(TAXONCODE!="SSSS")#%>% filter(TAXONCODE%in%c("MOSP","POCS","PGWC","PMVC","PVAR","LMYC","PNIE","PDUE","PCHI"))

#Prep NN Loops
uY=unique(jar_tax$ANALYSIS_YEAR)
uS=unique(jar_tax$SITE)
uT=unique(jar_tax$TAXONCODE)

#Test for median offset within segment
npts=10000
mat=data.frame(LON=runif(npts,min=0,max=2.5),LAT=runif(npts,min=0,max=1))
matsf=st_as_sf(mat,coords=c("LON","LAT"))
dmat=st_distance(matsf)
hist(dmat)
plot(mat$LON,mat$LAT)
median_intrasegment_offset=median(dmat)

#Loop to build Year-Site Dist Matrices
library(sf)
jar_sf=jar_tax %>% select(SITE,ANALYSIS_YEAR,LONGITUDE,LATITUDE) %>% distinct()
jar_sf=st_as_sf(jar_sf,coords = c("LONGITUDE","LATITUDE"),crs=4326)
jar_Site_Year_Dlist=list()
for(iY in 1:length(uY)){
  thisY=jar_sf %>% filter(ANALYSIS_YEAR==uY[iY])
  jar_Di=as.matrix(st_distance(thisY))
  diag(jar_Di)<-NA
  rownames(jar_Di)=thisY$SITE
  colnames(jar_Di)=thisY$SITE
  infD=which(is.infinite(jar_Di),arr.ind = T)
  jar_Site_Year_Dlist[[iY]]=jar_Di
}
names(jar_Site_Year_Dlist)=uY

#Loop to find colony-level conspecific NN dists and proportions of colonies within set NN dists
Nruns=length(uY)*length(uS)*length(uT)
NAstack=rep(NA,Nruns)
NNest=data.frame(ANALYSIS_YEAR=NAstack,
                 SITE=NAstack,
                 TAXONCODE=NAstack,
                 NN_mean=NAstack,
                 Pwin01=NAstack,
                 Pwin06=NAstack,
                 Pwin11=NAstack,
                 Pwin16=NAstack)
skipcount=1;calccount=1
#loop year, site, taxon
for(iY in 1:length(uY)){
  for(iS in 1:length(uS)){
    for(iT in 1:length(uT)){
      #iY=1;iS=1;iT=3
      #Colonies from same year,site,taxon
      thisj=jar_tax %>% filter(ANALYSIS_YEAR==uY[iY],SITE==uS[iS],TAXONCODE==uT[iT])
      #if none, skip
      if(nrow(thisj)==0){skipcount=skipcount+1;next}
      #build intra-site, inter-colony dist matrix
      thisD=as.matrix(dist(thisj$SEGMENT)+median_intrasegment_offset)
      diag(thisD)<-NA
      #get colony NN
      NN=apply(thisD,1,min,na.rm=TRUE)
      #if there are 2 or more colonies at the site, get mean NN and prop of colonies with x dist
      if(length(NN)>0){
        NNest_this=data.frame(ANALYSIS_YEAR=uY[iY],
                              SITE=uS[iS],
                              TAXONCODE=uT[iT],
                              NN_mean=mean(NN,na.rm=T),
                              Pwin01=length(which(NN<=01))/length(NN),
                              Pwin06=length(which(NN<=02))/length(NN),
                              Pwin11=length(which(NN<=05))/length(NN),
                              Pwin16=length(which(NN<=10))/length(NN))}
      #if not flag as NA meters, with 0 PropWithin
      else{
        NNest_this=data.frame(ANALYSIS_YEAR=uY[iY],SITE=uS[iS],TAXONCODE=uT[iT],NN_mean=NA,Pwin01=0,Pwin06=0,Pwin11=0,Pwin16=0)
      }
      #if dist is NA, add inter-site distance, NA if not on island
      if(is_na(NNest_this$NN_mean)){
        ConspSites=jar_tax %>% filter(ANALYSIS_YEAR==uY[iY],TAXONCODE==uT[iT]) %>% pull(SITE) %>% unique()
        InterSiteD=jar_Site_Year_Dlist[[uY[iY]]]
        InterFocalSiteD=InterSiteD[uS[iS],ConspSites]
        MinInterSiteD=min(InterFocalSiteD,na.rm=T)
        #On horns of whether Infinite makes sense for inter-island distances?
        #if(!is.infinite(MinInterSiteD)){NNest_this$NN_mean=MinInterSiteD}else{NNest_this$NN_mean=NA}
        NNest_this$NN_mean=MinInterSiteD #for now, leave Inf as Inf
      }
      #Add to site-level data structure
      NNest[calccount,]=NNest_this
      #track progress
      totcount=calccount+skipcount
      print(paste0(totcount," of ",Nruns," (",calccount," calc'd. ",skipcount," skipped.)"))
      calccount=calccount+1
    }
  }
}
#truncate unused NNest space
NNest=NNest[1:calccount,]
#report final progress #s
totcount=calccount+skipcount-2
print(paste0(totcount," of ",Nruns," completed. (",calccount-1," calc'd. ",skipcount-1," skipped.)"))

#Join to Site,Year,Taxon Data -Level
NNest$OBS_YEAR=as.integer(NNest$ANALYSIS_YEAR)
jar_site=left_join(jar_site,NNest %>% select(-ANALYSIS_YEAR),by=c("OBS_YEAR","SITE","TAXONCODE"))
jar_site %>% filter(AdColCount>0)
area=read.csv("../fish-paste/data/Sectors-Strata-Areas.csv") 
ja=area %>% filter(ISLAND=="Jarvis") %>% select(SEC_NAME,DEPTH_BIN,REEF_ZONE,AREA_HA_2023)
jar_sec=jar_site %>% left_join(ja) %>% 
  select(SEC_NAME,REEF_ZONE,DEPTH_BIN,OBS_YEAR,TAXONCODE,AREA_HA_2023,AdColDen,Pwin06) %>% 
  group_by(SEC_NAME,REEF_ZONE,DEPTH_BIN,OBS_YEAR,TAXONCODE,AREA_HA_2023) %>%
  summarize(mean_AdColDen=mean(AdColDen,na.rm=T),
            mean_Pwin06=mean(Pwin06,na.rm=T)) %>% 
  mutate(Ncol_str=mean_AdColDen*AREA_HA_2023*10000,
         Nwin06_str=mean_Pwin06*Ncol_str) %>%
  mutate(Ncol_str=replace_na(Ncol_str,0),
         Nwin06_str=replace_na(Nwin06_str,0)) %>% 
  group_by(SEC_NAME,OBS_YEAR,TAXONCODE) %>%
  summarize(Ncol_sec=sum(Ncol_str),Nwin06_sec=sum(Nwin06_str)) %>% 
  pivot_wider(names_from = OBS_YEAR,values_from = c(Ncol_sec,Nwin06_sec)) %>% 
  arrange(desc(Ncol_sec_2015 )) %>% 
  mutate(PctNcol=(Ncol_sec_2018-Ncol_sec_2015)/Ncol_sec_2015,
         PctNwin06=(Nwin06_sec_2018-Nwin06_sec_2015)/Nwin06_sec_2015,
         LRNcol=log2(Ncol_sec_2018/Ncol_sec_2015),
         LRNwin06=log2(Nwin06_sec_2018/Nwin06_sec_2015))

library(ggrepel)
jar_sec %>% ggplot(aes(y=LRNwin06,x=LRNcol))+geom_point()+geom_label_repel(aes(label=TAXONCODE))+
  xlab("LogRatio of 2015:2018\nIsland Estimate of N. Colonies")+
  ylab("LogRatio of 2015:2018\nIsland Estimate of N. Colonies with NN within 6 m")+
  geom_abline()

space2den=function(x,boxside=10){
  bign=(boxside/x+1)^2
  den=bign/(boxside^2)
  return(den)
}
space2den_l=function(x,boxside=10){
  bign=(boxside/x)^2
  den=bign/(boxside^2)
  return(den)
}

#jar_site=jar_site %>% mutate(Pwin06=replace_na(Pwin06,0))
jar_site$Pwin06[which(jar_site$AdColDen==0)]=0
jar_site$Pwin01[which(jar_site$AdColDen==0)]=0
NNjar=jar_site %>% ggplot(aes(x=AdColDen,y=Pwin06))+
  geom_jitter(width=.05,height=0.01)+
  stat_smooth(method="loess")+
  geom_vline(xintercept = c(.25,2),color="red")+
#  geom_vline(xintercept = c(space2den(1:10)),color="blue")+
#  annotate(x=space2den(1:10),y=rep(.1,10),label=1:10,geom = "text")+
#  geom_vline(xintercept = c(space2den_l(1:10)),color="lightblue")+
#  annotate(x=space2den_l(1:10),y=rep(.1,10),label=1:10,geom = "text")+
  scale_x_log10(limits=c(0.003,20))+
  ylim(c(0,1))+theme_bw()+xlab("Adult Colony Density")+ylab("Prop. of Colonies with\nNearest Neighbor within 6 m")+
  ggtitle("Allee Effect: Jarvis 2015-2018\n Site-Level Adult Density vs. Prop. of Colonies within 6 m")


DensDist=rea_site %>% filter(TAXONCODE!="SSSS") %>% 
  ggplot(aes(x=AdColDen,fill=REGION))+geom_histogram(bins=20)+scale_x_log10(limits=c(0.003,20))+
  geom_vline(xintercept = c(.25,2),color="red")+
  theme_bw()+xlab("Adult Colony Density")+
  ggtitle("Observed Site-Level Taxon Densities 2013-2024")#+facet_wrap("OBS_YEAR")

rea_site %>% filter(TAXONCODE!="SSSS") %>% 
  ggplot(aes(x=AdColCount,fill=REGION))+geom_histogram(bins=20)+scale_x_log10()

library(patchwork)

ProblemHere=NNjar+DensDist
ggsave(filename = "C:/Users/Thomas.Oliver/Desktop/DoWeHaveAProblem.jpg",plot = ProblemHere)


jar_site %>% ggplot(aes(x=Nper10,y=Nwin06))+geom_point()+geom_abline()+scale_x_log10()+scale_y_log10()

hist(jar_sec$LRNcol,10)
hist(jar_sec$LRNwin06,10)

library(tidyverse)
hibefore=jar_site %>% filter(OBS_YEAR==2015) %>% filter(TAXONCODE!="SSSS") %>% select(TAXONCODE,AdColDen) %>%
  group_by(TAXONCODE) %>% summarize(mean_AdColDen=mean(AdColDen,na.rm=T)) %>% arrange(desc(mean_AdColDen)) %>%
  slice(1:11)
hiafter=jar_site %>% filter(OBS_YEAR==2018) %>% filter(TAXONCODE!="SSSS") %>% select(TAXONCODE,AdColDen) %>%
  group_by(TAXONCODE) %>% summarize(mean_AdColDen=mean(AdColDen,na.rm=T)) %>% arrange(desc(mean_AdColDen)) %>%
  slice(1:12)
targettax=unique(c(hibefore$TAXONCODE,hiafter$TAXONCODE));length(targettax)
jar_site %>% filter(TAXONCODE%in%targettax)%>% mutate(TAXONCODE=factor(TAXONCODE,levels=targettax)) %>%
  ggplot(aes(x=OBS_YEAR,y=Nper10,size=AdColCount))+geom_jitter(width=.1,height=0,shape=1) + facet_wrap("TAXONCODE")
jar_site %>% filter(TAXONCODE%in%targettax)%>%mutate(TAXONCODE=factor(TAXONCODE,levels=targettax)) %>% 
  ggplot(aes(x=OBS_YEAR,y=Nout06,size=Nper10))+geom_jitter(width=.1,height=0,shape=1) + facet_wrap("TAXONCODE")
jar_site %>% filter(TAXONCODE%in%targettax,OBS_YEAR==2018)%>%mutate(TAXONCODE=factor(TAXONCODE,levels=targettax)) %>% 
  ggplot(aes(x=Nper10,y=Pout06,size=Nper10))+geom_jitter(width=.1,height=0,shape=1) + facet_wrap("TAXONCODE")

NNest %>% ggplot(aes(x=ANALYSIS_YEAR,y=Pwin06))+geom_jitter()
NNest %>% ggplot(aes(x=ANALYSIS_YEAR,y=NN_mean))+geom_jitter()+
  scale_y_log10(limits=c(median_intrasegment_offset,max(NNest$NN_mean)))





NNest_isl=NNest %>% group_by(TAXONCODE,ANALYSIS_YEAR) %>% summarize(NN_mean=mean(NN_mean))

NNest_isl %>% ggplot(aes(x=ANALYSIS_YEAR,y=NN_mean))+geom_jitter()


table(jar_isl$ANALYSIS_YEAR)
jar_isl9=jar_isl %>% 
  select(TAXONCODE,ANALYSIS_YEAR,Mean_AdColDen) %>%
  filter(TAXONCODE!="SSSS",
         TAXONCODE%in%c("MOSP","POCS","PGWC","PMVC","PVAR",
                        "LMYC","PNIE","PDUE","PCHI")) %>% 
  arrange(desc(Mean_AdColDen))

ggplot(jar_isl9,aes(x=(TAXONCODE),y=Mean_AdColDen,color=ANALYSIS_YEAR))+
  geom_jitter(width = .1,height=0)+scale_y_log10()

## Simulation

#100 by 100 "playing field"
nrun=30
boxside=223.6068#sqrt(113)
ColCounts=ceiling(boxside^2*10^seq(-3.5,-1,length.out=30))#c(30:100,200,500,1000,2000,5000)
NN_Den=data.frame(Ncol=rep(x=ColCounts,nrun),
                  Run=NA,medNN=NA,Den=NA)
count=1
for(runi in 1:nrun){
  for(coli in 1:length(ColCounts)){
    #coli=2
  df=data.frame(x=runif(n=ColCounts[coli],min=0,max=boxside),
                y=runif(n=ColCounts[coli],min=0,max=boxside))
  dfsf=st_as_sf(df,coords=c("x","y"),crs=32601)
  D=as.matrix(st_distance(dfsf));diag(D)=NA
  NN=apply(D,1,min,na.rm=TRUE)
  NN_Den$Run[count]=runi
  NN_Den$Pwin06[count]=length(which(NN<=6))/length(NN)
  NN_Den$medNN[count]=median(NN)
  NN_Den$q5NN[count]=quantile(NN,.05)
  NN_Den$q95NN[count]=quantile(NN,.95)
  NN_Den$Den[count]=ColCounts[coli]/(boxside^2)
  print(paste0(runi,":",coli))
  count=count+1
  }
}


boxsim=ggplot(NN_Den,aes(x=Den,y=Pwin06))+
  geom_point()+
  scale_x_log10(limits=c(min(NN_Den$Den),.20))+
  geom_hline(yintercept = c(.5,.9),color="red")+
  geom_vline(xintercept = c(0.00625,0.0215),color="red")+
  geom_vline(xintercept = 1:3/113,color="blue")+
  stat_smooth(method="loess",span=.15)+
#  scale_y_log10()+
  theme_bw()+ggtitle("Simulation with Random Position of Colonies within 25x25 Meter Box")+
  xlab("Adult Colony Density")+ylab("Prop. of Colonies with\nNearest Neighbor within 6 m")
boxsim

library(mgcv)
den2NN6=gam(formula = Pwin06~(s(Den)),data=NN_Den)
plot(den2NN6)
predict(den2NN6,newdata = data.frame(Den=c(0.00625,0.0215)))
1/c(0.00625,0.0215,.1,1)

inverse.predict(den2NN6, newdata = .5)


NoProblemHere=boxsim/NNjar/DensDist

boxsim=ggplot(NN_Den,aes(x=Den,y=medNN))+#Pwin06))+#
  geom_jitter(width=0,height=0)+
  scale_x_log10(limits=c(min(NN_Den$Den),2))+
  scale_y_log10()+
  geom_vline(xintercept = c(.25,2),color="red")+
  stat_smooth(method="gam")+
  #  scale_y_log10()+
  theme_bw()+ggtitle("Simulation with Random Position of Colonies within 25x25 Meter Box")+
  xlab("Adult Colony Density")+
#  ylab("Prop. of Colonies with\nNearest Neighbor within 6 m")
  ylab("Median Site-Level Nearest Neighbor")
boxsim



#consider spatial clustering and how to describe it...
