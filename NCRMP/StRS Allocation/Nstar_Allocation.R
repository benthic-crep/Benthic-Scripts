#Pulling from Smith, Steven G., Dione W. Swanson, Mark Chiappone, Steven L. Miller, and Jerald S. Ault. "Probability sampling of stony coral populations in the Florida Keys." Environmental Monitoring and Assessment 183 (2011): 121-138.
#Given that we now use a single-stage design, SSU = 10 m2, PSU = 2500 m2; Mhi = 250
#Nstarh = optimal allocation for a given stratum is...
#Nstarh = Nstar * ( Wh * Suh  )/( SUMh (Wh * Suh) )
#Where ... 
#Nstar is the "number of PSUs required to achieve a specified variance !!! (not just how many sites you actually have)
#Wh is the Stratum h weighting factor (aka proportional area)
#... i.e. (Nh*Mh / SUMh( Nh*Mh )); Nh*Mh = Total possible SSUs in Strat h (Area)
#Suh is the sample standard deviation in stratum h (SQRT(VARh))

#So, in english, the optimal allocation per stratum across a given domain is
#    the Number of total sites needed to hit a desired variance (Nstar) times
#    the ratio of prop area * stratum sd AND the summed prop area by stratum sd for all strata.

#Currently, we just estimate Nstar - I want to actually calculate it, starting at island scale.

#Working Backward:
#1: Nstar_h = Nstar * ( w_h * s_uh  )/( SUM_h(w_h * s_uh) )
#2: Nstar = ( SUM_h(w_h * s_uh) * ( SUM_h(w_h * s_uh) + SUM_h( ( w_h^2 * s_2h2 ) / (mstar_h * w_h * s_uh) ) ) ) /...
#                                       ( V[D--_st] + SUM_h( ( w_h^2 * s_1h2 ) / (N_h) ))
#Nstar_h ~ Nstar, w_h, s_uh 
#Nstar ~ w_h, s_uh, s_2h2, mstar_h, V[D--_st], s_1h2, N_h 

# LIBRARIES  -----------------------------------------------------------------------------------------
rm(list=ls())      # clean working environment

# Load libraries
library(tidyverse) # for summarizing/orgainzing data

# FUNCTIONS ---------------------------------------------------------------
c_Nstar_h=function(Nstar,w_h,s_uh){ #Optimal Strata Level Allocation
  Nstar_h=Nstar*(w_h*s_uh)/(sum(w_h*s_uh))
  return(Nstar_h)
}

c_Nstar=function(w_h, s_uh, s_1h2, s_2h2, mstar_h, V.D.._st, N_h,STAGE=1){ #N PSU to hit variance target
  if(STAGE==1){
    num = sum(w_h*s_uh)^2
  }else{
    num = sum(w_h*s_uh)*(sum(w_h*s_uh) + sum((w_h^2*s_2h2)/(mstar_h*w_h*s_uh)))
  }
  denom = (V.D.._st + sum( ( w_h^2 * s_1h2 ) / (N_h) ))
  Nstar=num/denom
  return(Nstar)
}

c_w_h = function(N_h,M_h){ #Strat Weighting factor (aka proportional area across domain)
  w_h = (N_h*M_h)/sum(N_h*M_h)
  return(w_h)
}

c_s_uh = function(s_1h2,s_2h2,M_h,STAGE=1){ #Sample standard deviation in stratum h
  if(STAGE==1){
    s_uh = sqrt( s_1h2 )
  }else{
    s_uh = sqrt( s_1h2 - (s_2h2/M_h))}
  return(s_uh)
}

c_s_1h2 = function(n_h,P._hi){ #Sample variance among PSUs in stratum h
  s_1h2 = sum((P._hi-P.._h)^2) / (n_h - 1)
  return(s_1h2)
}

c_s_2h2 = function(n_h,m_hi,P._hi){ #Sample variance among SSUs in stratum h
  s_2h2 = (1/n_h) * ( (sum(m_hi/(m_hi-1))) * (P._hi) * (1 - P._hi) )
  return(s_2h2)
}

c_mstar_h = function (s_2h2,s_uh) {# Optimum number of SSU per PSU in stratum h (i think I'll just set this to 1?)
  mstar_h = sqrt(s_2h2)/s_uh
  return(mstar_h)
}

c_D.._st=function(w_h,D.._h){#Target Variance for survey wide mean density #Can I set this?
  D.._st = sum(w_h*D.._h)
  return(D.._st)
}

c_V.D.._st=function(D.._h,D.._st){#Target Variance for survey wide mean density #Can I set this?
  V.D.._st = (CV(D.._h)*D.._st)^2
  return(V.D.._st)
}

c_N_h=function(A_h.m2,PSU_Am2){
  N_h=floor(A_h.m2/PSU_Am2)
  reuturn(N_h)
}

SE=function(x){return(sd(x)/sqrt(length(x)))}
CV=function(x){return(SE(x)/mean(x))}


#OK Now if I want to calcualte Nstar_h: I'm going to calculate each component and add a ".c" suffix

# DATA: Get Allocation Ready Data ---------------------------------------------------------------
#First get PSU-level data for the MHI and strata Areas.

####Run lines 33 - 107 of Benthic Allocation Generation, returns StrataLevelData mhi_a; Imma call it DF_h
# Get StrataLevelData with # Has N_h, n_h, D.._h, s_1h2, s_2h2=NA, and s_uh ~ sqrt(s_1h2)
setwd("T:/Benthic/Data/StRS Allocation/")
str=read.csv("../REA Coral Demography & Cover/Summary Data/Stratum/BenthicREA_stratadata_GENUS.csv")
site=read.csv("../REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS_2023.csv")

######################################
HA2M2=10000
A_PSU.M2=50^2
A_SSU.M2=10
M_h=A_PSU.M2/A_SSU.M2

#Strata Level Metrics
#N_h: Total possible number of PSUs units in stratum h
#n_h: Number of PSUs sampled in stratum h
#D.._h: Mean density in stratum h
#s_1h2: Sample variance among PSUs in stratum h
#s_2h2=NA
#s_uh ~ sqrt(s_1h2) #Sample standard deviation in stratum h

# load sector data
sectors<-read.csv("BenthicSectorsforAllocation.csv")
sec_N_h=sectors %>% 
  select(REGION,ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN, AREA_HA) %>% 
  mutate(AREA_M2=AREA_HA*HA2M2,
         N_h=AREA_M2/A_PSU.M2) %>% 
  rename(SECTOR=SEC_NAME) %>% 
  select(REGION,ISLAND,SECTOR,REEF_ZONE,DEPTH_BIN,N_h)

DBlu=c("Shallow","Mid","Deep");names(DBlu)=c("S","M","D")
str$DEPTH_BIN=DBlu[substr(str$DB_RZ,2,2)]

tgen=c("POSP","PAVS","POCS","MOSP","SSSS")

DF_h=str %>% 
  filter(REGION%in%c("MHI","NWHI"))  # Has N_h, n_h, D.._h, s_1h2, s_2h2=NA, and s_uh ~ sqrt(s_1h2)
DF_hl= DF_h %>% 
  select(METHOD,REGION,ISLAND,ANALYSIS_YEAR,SECTOR,Stratum,REEF_ZONE,DEPTH_BIN,DB_RZ,GENUS_CODE,n,Ntot,AdColDen,SE_AdColDen) %>% 
  rename(D.._h=AdColDen,s_uh=SE_AdColDen,n_h=n) %>%
  mutate(s_1h2=s_uh^2,DEPTH_BIN=DBlu[substr(DB_RZ,2,2)]) %>%
  left_join(sec_N_h,by=c("REGION","ISLAND","SECTOR","REEF_ZONE","DEPTH_BIN")) %>% 
  group_by(ISLAND,GENUS_CODE,ANALYSIS_YEAR) %>% 
  mutate(m._h=1,
         n_h.m_h=n_h*m._h,
         var.D.._h.= ((1-(n_h/N_h))/(n_h))*s_1h2,
         w_h=c_w_h(N_h = N_h,M_h=M_h))

TargetCV=0.25

#Domain scale estimates (ISLAND)
DF_stl=DF_hl %>% 
  group_by(REGION,ISLAND,GENUS_CODE,ANALYSIS_YEAR) %>% 
  reframe(
    n=sum(n_h),
    nm=sum(n_h.m_h),
    D.._st=sum(w_h*D.._h),
    var.D.._st.= sum(w_h^2*var.D.._h.),
    SE.D.._st.=sqrt(var.D.._st.),
    CV.D.._st.=SE.D.._st./D.._st,
    V.D.._st.=(CV.D.._st.*D.._st)^2,
    Nstar_obs=c_Nstar(w_h=w_h,
                      s_uh = s_uh,
                      s_1h2 = s_1h2,
                      s_2h2 = 0,
                      mstar_h = 1,
                      V.D.._st = V.D.._st.,
                      N_h = N_h),
    Nstar_05=c_Nstar(w_h=w_h,
                     s_uh = s_uh,
                     s_1h2 = s_1h2,
                     s_2h2 = 0,
                     mstar_h = 1,
                     V.D.._st = (.05*D.._st)^2,
                     N_h = N_h),
    Nstar_10=c_Nstar(w_h=w_h,
                     s_uh = s_uh,
                     s_1h2 = s_1h2,
                     s_2h2 = 0,
                     mstar_h = 1,
                     V.D.._st = (.10*D.._st)^2,
                     N_h = N_h),
    Nstar_20=c_Nstar(w_h=w_h,
                     s_uh = s_uh,
                     s_1h2 = s_1h2,
                     s_2h2 = 0,
                     mstar_h = 1,
                     V.D.._st = (.20*D.._st)^2,
                     N_h = N_h),
    Nstar_30=c_Nstar(w_h=w_h,
                     s_uh = s_uh,
                     s_1h2 = s_1h2,
                     s_2h2 = 0,
                     mstar_h = 1,
                     V.D.._st = (.30*D.._st)^2,
                     N_h = N_h),
    Nstar_50=c_Nstar(w_h=w_h,
                     s_uh = s_uh,
                     s_1h2 = s_1h2,
                     s_2h2 = 0,
                     mstar_h = 1,
                     V.D.._st = (.50*D.._st)^2,
                     N_h = N_h),
    Nstar_99=c_Nstar(w_h=w_h,
                     s_uh = s_uh,
                     s_1h2 = s_1h2,
                     s_2h2 = 0,
                     mstar_h = 1,
                     V.D.._st = (.99*D.._st)^2,
                     N_h = N_h)
    
  ) %>% pivot_longer(cols=all_of(c("Nstar_05","Nstar_10","Nstar_20","Nstar_30","Nstar_50","Nstar_99")),
                     names_to = "Target_CV",values_to = "Nstar",names_prefix = "Nstar_")
DF_stl$Target_CV=as.numeric(DF_stl$Target_CV)

#Generate Mean CV Across Target Taxa
DF_stl.=DF_stl %>% 
  filter(GENUS_CODE%in%tgen) %>%
  filter(!ISLAND%in%c("Midway","Laysan","Maro")) %>% 
  group_by(REGION,ISLAND,Target_CV) %>% 
  filter(ANALYSIS_YEAR==max(ANALYSIS_YEAR)) %>% 
  group_by(REGION,ISLAND,ANALYSIS_YEAR,Target_CV) %>% 
  #mean across the 5 target taxa (akin to old school allocation)
  summarize(n=mean(n,na.rm=T),
            nm=mean(nm,na.rm=T),
            D.._st=mean(D.._st,na.rm=T),
            var.D.._st.=mean(var.D.._st.,na.rm=T),
            SE.D.._st.=mean(SE.D.._st.,na.rm=T),
            CV.D.._st.=mean(CV.D.._st.,na.rm=T),
            V.D.._st.=mean(V.D.._st.,na.rm=T),
            Nstar_obs=mean(Nstar_obs,na.rm=T),
            Nstar=mean(Nstar,na.rm=T)
  )



Old_Style_Neyman_Allocation=read.csv("./NCRMP24_SITES_Allocation_2024-06-20_Extra.csv")
Old_Style_Neyman_Allocation=Old_Style_Neyman_Allocation%>% mutate(Stratum=paste(SEC_NAME,REEF_ZONE,DEPTH_BIN,sep="_"))
ISL_OSNA=Old_Style_Neyman_Allocation %>%
  filter(REGION%in%c("MHI","NWHI")) %>%
  filter(!ISLAND%in%c("Midway","Laysan","Maro")) %>% 
  group_by(REGION,ISLAND) %>% 
  reframe(N=sum(SDxA_ALLOC ))



RefCV=25
RefCV2=10
NstarPlots=DF_stl. %>% 
  ggplot(aes(x=Nstar,y=Target_CV,color=ANALYSIS_YEAR))+
  geom_segment(aes(x=Nstar_obs,xend=n,y=CV.D.._st.*100,yend=CV.D.._st.*100))+
  geom_point(aes(x=Nstar_obs,y=CV.D.._st.*100),shape=8)+
  geom_point(aes(x=n,y=CV.D.._st.*100),shape=19)+
  geom_line(aes())+
  scale_shape_manual(values=c("Nstar @ observed CV"=8,"N Sampled"=19))+
  facet_wrap(c("REGION","ISLAND"),nrow=5)+
  geom_hline(yintercept = RefCV,color="purple",lty=2)+
  geom_hline(yintercept = RefCV2,color="gold",lty=2)+
  geom_vline(aes(xintercept = N),color="gray",data=ISL_OSNA)+
  geom_text(aes(x = N+5,y=RefCV+5,label=N),color="gray",data=ISL_OSNA)+
  scale_x_log10(limits=c(1,200))+theme_bw()+
  scale_y_sqrt()+theme_bw()+
  ylab("Co. of Variation: Adult Colony Density")+
  xlab("Nstar - Optimal Number of Sites for given CV target")+
  ggtitle(paste0("Nstar Analysis - Island Scale, Mean of Total and 4 Target Genera"))
sc=1.25
ggsave(plot = NstarPlots,filename = "./NstarPlots_SSSS_withNeyman_NCRMP24_20240221.jpg",width=sc*11,height=sc*8.5)


#

#Strata scale estimates (STRATA)
DF_strata=DF_hl %>% 
  group_by(REGION,ISLAND,SECTOR,Stratum,GENUS_CODE,ANALYSIS_YEAR) %>% 
  reframe(
    n=sum(n_h),
    nm=sum(n_h.m_h),
    D.._st=sum(w_h*D.._h),
    var.D.._st.= sum(w_h^2*var.D.._h.),
    SE.D.._st.=sqrt(var.D.._st.),
    CV.D.._st.=SE.D.._st./D.._st,
    V.D.._st.=(CV.D.._st.*D.._st)^2,
    Nstar_obs=c_Nstar(w_h=w_h,
                      s_uh = s_uh,
                      s_1h2 = s_1h2,
                      s_2h2 = 0,
                      mstar_h = 1,
                      V.D.._st = V.D.._st.,
                      N_h = N_h),
    Nstar_05=c_Nstar(w_h=w_h,
                     s_uh = s_uh,
                     s_1h2 = s_1h2,
                     s_2h2 = 0,
                     mstar_h = 1,
                     V.D.._st = (.05*D.._st)^2,
                     N_h = N_h),
    Nstar_10=c_Nstar(w_h=w_h,
                     s_uh = s_uh,
                     s_1h2 = s_1h2,
                     s_2h2 = 0,
                     mstar_h = 1,
                     V.D.._st = (.10*D.._st)^2,
                     N_h = N_h),
    Nstar_20=c_Nstar(w_h=w_h,
                     s_uh = s_uh,
                     s_1h2 = s_1h2,
                     s_2h2 = 0,
                     mstar_h = 1,
                     V.D.._st = (.20*D.._st)^2,
                     N_h = N_h),
    Nstar_25=c_Nstar(w_h=w_h,
                     s_uh = s_uh,
                     s_1h2 = s_1h2,
                     s_2h2 = 0,
                     mstar_h = 1,
                     V.D.._st = (.25*D.._st)^2,
                     N_h = N_h),
    Nstar_30=c_Nstar(w_h=w_h,
                     s_uh = s_uh,
                     s_1h2 = s_1h2,
                     s_2h2 = 0,
                     mstar_h = 1,
                     V.D.._st = (.30*D.._st)^2,
                     N_h = N_h),
    Nstar_50=c_Nstar(w_h=w_h,
                     s_uh = s_uh,
                     s_1h2 = s_1h2,
                     s_2h2 = 0,
                     mstar_h = 1,
                     V.D.._st = (.50*D.._st)^2,
                     N_h = N_h),
    Nstar_99=c_Nstar(w_h=w_h,
                     s_uh = s_uh,
                     s_1h2 = s_1h2,
                     s_2h2 = 0,
                     mstar_h = 1,
                     V.D.._st = (.99*D.._st)^2,
                     N_h = N_h)
    
  ) %>% pivot_longer(cols=all_of(c("Nstar_05","Nstar_10","Nstar_20","Nstar_25","Nstar_30","Nstar_50","Nstar_99")),
                     names_to = "Target_CV",values_to = "Nstar",names_prefix = "Nstar_")
DF_strata$Target_CV=as.numeric(DF_strata$Target_CV)
class(DF_strata$Target_CV)

#Generate Mean CV Across Target Taxa
DF_strata.=DF_strata %>% 
  filter(GENUS_CODE%in%tgen) %>%
  filter(!ISLAND%in%c("Midway","Laysan","Maro")) %>% 
  group_by(REGION,ISLAND,SECTOR,Stratum,Target_CV) %>% 
  filter(ANALYSIS_YEAR==max(ANALYSIS_YEAR)) %>% 
  group_by(REGION,ISLAND,SECTOR,Stratum,ANALYSIS_YEAR,Target_CV) %>% 
  #mean across the 5 target taxa (akin to old school allocation)
  summarize(n=mean(n,na.rm=T),
            nm=mean(nm,na.rm=T),
            D.._st=mean(D.._st,na.rm=T),
            var.D.._st.=mean(var.D.._st.,na.rm=T),
            SE.D.._st.=mean(SE.D.._st.,na.rm=T),
            CV.D.._st.=mean(CV.D.._st.,na.rm=T),
            V.D.._st.=mean(V.D.._st.,na.rm=T),
            Nstar_obs=mean(Nstar_obs,na.rm=T),
            Nstar=mean(Nstar,na.rm=T)
  )



RefCV=30
RefCV2=10
uI=DF_strata. %>% ungroup() %>% 
  filter(!ISLAND%in%c("Midway","Laysan","Maro")) %>%
  distinct(REGION,ISLAND) %>% select(REGION,ISLAND)

for (i in 1:nrow(uI)){
  STR_OSNA=Old_Style_Neyman_Allocation %>% filter(ISLAND==uI$ISLAND[i]) 
  NstarPlots_STR=DF_strata. %>% 
    filter(ISLAND==uI$ISLAND[i]) %>% 
    group_by() %>% 
    droplevels() %>% 
    ggplot(aes(x=Nstar,y=Target_CV,color=ANALYSIS_YEAR))+
    geom_segment(aes(x=Nstar_obs,xend=n,y=CV.D.._st.*100,yend=CV.D.._st.*100))+
    geom_point(aes(x=Nstar_obs,y=CV.D.._st.*100),shape=8)+
    geom_point(aes(x=n,y=CV.D.._st.*100),shape=19)+
    geom_line()+
    scale_shape_manual(values=c("Nstar @ observed CV"=8,"N Sampled"=19))+
    facet_wrap(c("REGION","ISLAND","Stratum"),scales="free",ncol=3)+
    geom_hline(yintercept = RefCV,color="purple",lty=2)+
    geom_hline(yintercept = RefCV2,color="gold",lty=2)+
    geom_vline(aes(xintercept = SDxA_ALLOC ),color="black",data=STR_OSNA)+
    geom_text(aes(x = SDxA_ALLOC+5,y=RefCV+5,label=SDxA_ALLOC),color="gray",data=STR_OSNA)+
    scale_x_log10(limits=c(1,30),breaks=c(2:12,15,20,25,30))+theme_bw()+
    scale_y_log10()+theme_bw()+
    ylab("Co. of Variation: Adult Colony Density")+
    xlab("Nstar - Optimal Number of Sites for given CV target")+
    ggtitle(paste0("Nstar Analysis - Strata Scale, Mean of Total and 4 Target Genera"))
  
  sc=1.25
  ggsave(plot = NstarPlots_STR,
         filename = paste0("./NCRMP2024_StrataNstar_By_Island/NstarPlots_TargetGenera_withNeyman_NCRMP24_",uI$ISLAND[i],"_20240221.jpg"),
         width=sc*11,height=sc*8.5)
  
  
}  

DF_str30=DF_strata. %>% filter(Target_CV==30) %>% select(REGION,ISLAND,SECTOR,Stratum,Nstar)
OSNA.nstar=Old_Style_Neyman_Allocation %>% 
  left_join(DF_str30,by=join_by(REGION,ISLAND,SEC_NAME==SECTOR,Stratum)) %>% 
  rename(Nstar_30=Nstar)
write.csv(x = OSNA.nstar,file = paste0("./NCRMP24_SITES_Allocation_",Sys.Date(),"_ExtraMaui_NWHIall_Nstar30.csv"))

# #ADD ISLAND DOMAIN ESTIMATES BACK INTO STRATA LEVEL
# DF_hl=DF_hl %>% left_join(DF_stl,by=c("ISLAND","GENUS_CODE","ANALYSIS_YEAR"))

#Merge Both Back
NoEx=read.csv("./NCRMP24_SITES_Allocation_2024-06-20_NoExtra.csv")
names(NoEx)
OSNA.2.nstar=OSNA.nstar %>% 
  left_join(NoEx[,c("REGION","ISLAND","SEC_NAME","REEF_ZONE","DEPTH_BIN","SDxA_ALLOC","AREA_ALLOC")],by=c("REGION","ISLAND","SEC_NAME","REEF_ZONE","DEPTH_BIN")) %>% 
  rename(AREA_ALLOC_Ex=AREA_ALLOC.x,
         SDxA_ALLOC_Ex=SDxA_ALLOC.x,
         AREA_ALLOC_NoEx=AREA_ALLOC.y,
         SDxA_ALLOC_NoEx=SDxA_ALLOC.y)
write.csv(x = OSNA.2.nstar,file = paste0("./NCRMP24_SITES_Allocation_",Sys.Date(),"_AllScenarios_Nstar30.csv"))
