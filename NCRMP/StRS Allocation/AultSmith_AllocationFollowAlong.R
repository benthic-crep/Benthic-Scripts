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
# set wd to relevant folder
setwd("T:/Benthic/Data/StRS Allocation")
# load sector data
sectors<-read.csv("BenthicSectorsforAllocation.csv")
wsd=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_TAXONCODE.csv")

nwhi<-wsd %>% filter(REGION == "NWHI", OBS_YEAR>"2010", TAXONCODE%in%c("PLOB","PCOM","PMVC","PVAR","SSSS")) %>% 
  select(SITEVISITID,ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN,TAXONCODE,AdColDen) %>% 
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>% 
  pivot_wider(names_from = TAXONCODE,values_from = AdColDen) %>% # group by strata
  summarize(SD_SSSS = sd(SSSS),SD_PLOB = sd(PLOB),SD_PCOM = sd(PCOM),SD_PMVC = sd(PMVC),SD_PVAR = sd(PVAR)) %>% # calculate variance (sd) for each trophic level
  drop_na() # drop strata with no data

# # !!!!!! filter for reef zones/islands NOT being surveyed here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # for example, take out Lagoon and backreef
nwhi<-nwhi %>% filter(REEF_ZONE != "Lagoon",REEF_ZONE != "Backreef")
nwhi<-droplevels(nwhi)

# get total variance per island in order to get proportion of each torhpic group's variance
a<-nwhi %>% group_by(ISLAND) %>%
  summarise(PLOB=sum(SD_PLOB),PCOM=sum(SD_PCOM),PMVC=sum(SD_PMVC),PVAR=sum(SD_PVAR),SSSS=sum(SD_SSSS)) 

# join totals back to original dataframe to calculate proportion
b<-full_join(nwhi,a,by="ISLAND") %>%
  mutate(PROP_PLOB = (SD_PLOB/PLOB),PROP_PCOM = (SD_PCOM/PCOM),PROP_PMVC = (SD_PMVC/PMVC),PROP_PVAR = (SD_PVAR/PVAR),PROP_SSSS = (SD_SSSS/SSSS))

# calculate average variance for each strata - first isloate proportion of variance, get rid of sd columns
names(b)
c<-b[,c("ISLAND","SEC_NAME","REEF_ZONE" , "DEPTH_BIN","PROP_PLOB" , "PROP_PCOM", "PROP_PMVC","PROP_PVAR","PROP_SSSS" )]
#gather(c, troph, prop, -c(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN)) %>% # melt data with 'gather' function to get all data into columns rather than rows
d<- c %>% pivot_longer(cols=c("PROP_PLOB","PROP_PCOM","PROP_PMVC","PROP_PVAR","PROP_SSSS"),names_to="TAXONCODE",values_to = "PROP") %>% 
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>%
  summarise(mean_v=mean(PROP)) # get the mean at the strata level

# get area values for each sector from the 'sectors' dataframe
sec<-sectors %>% select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN, AREA_HA) %>%
  right_join(d, by=c("ISLAND", "SEC_NAME", "REEF_ZONE", "DEPTH_BIN")) # right joining to the previous dataframe selects values for just the NWHI

e<-sec %>% group_by(ISLAND) %>%
  summarize(area=sum(AREA_HA)) # summarize area by island

f<-full_join(sec,e,by="ISLAND") %>% 
  mutate(AREA_PCT=AREA_HA/area) %>% 
  select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN,mean_v,AREA_PCT) # calculate proportional area for each strata

#gather(f, var, value, -c(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN)) %>% # melt data with 'gather' function to get all data into columns rather than rows
g<- f %>%  
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>% 
  summarise(mean_v=mean_v*AREA_PCT) # multiply the mean variance times the proportion of area of each strata

h<-g %>% group_by(ISLAND) %>% 
  summarize(sum=sum(mean_v))

i<-full_join(g,h,by="ISLAND") %>% 
  mutate(AREA_VAR_PCT=mean_v/sum) %>% # calculate the proportional weight for each strata
  select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN,AREA_VAR_PCT) # clean up dataframe

# JUST A CHECK: does total variance for each island add up to 1?
test<-i %>% 
  group_by(ISLAND) %>% 
  summarize(sum=sum(AREA_VAR_PCT))
summary(test)

# add proportion of each strata by area along with the proportional weight 
j<-i %>% full_join(f,by=c("ISLAND", "SEC_NAME", "REEF_ZONE", "DEPTH_BIN"))
k<-select(j,-mean_v)

# to calculate allocation based on weights:
# get list of islands
unique(k$ISLAND)

# Value (n) is number of sites we can survey in 1 day (this comes from team lead, based on small boats and number of divers)
n<-6
# Plug in number of days we have at each island here (using each island's 3-letter code), and multiply based on number of sites we can survey in 1 day (n)

ffs<-3*n
kur<-3*n
lay<-3*n
lis<-3*n
mid<-3*n
phr<-3*n

#create a field for sites and give a dummy variable = 1
k$TOTSITES<-1
# for each island, plug in total sites we can survey during this visit

k[k$ISLAND %in% "French Frigate",]$TOTSITES<-ffs
k[k$ISLAND %in% "Kure",]$TOTSITES<-kur
k[k$ISLAND %in% "Laysan",]$TOTSITES<-lay
k[k$ISLAND %in% "Lisianski",]$TOTSITES<-lis
k[k$ISLAND %in% "Midway",]$TOTSITES<-mid
k[k$ISLAND %in% "Pearl & Hermes",]$TOTSITES<-phr

# to get the allocation, multiply the number of total sites we can survey at each island by the 2 different weighted values (area and variance, and just area)
k$WEIGHTED_ALLOCATION<-round(k$TOTSITES*k$AREA_VAR_PCT)
k$AREA_ALLOCATION<-round(k$TOTSITES*k$AREA_PCT)

# save file
write.csv(k,file="NWHI_allocation.csv")mhi_a=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_TAXONCODE.csv")



######################################
A_PSU.M2=50^2
A_SSU.M2=10
M_h=A_PSU.M2/A_SSU.M2

#Strata Level Metrics
DF_h=mhi_a # Has N_h, n_h, D.._h, s_1h2, s_2h2=NA, and s_uh ~ sqrt(s_1h2)
DF_hl= DF_h %>% pivot_longer(cols=all_of(names(DF_h)[8:22]),
                             names_to = c("Metric","TAXONCODE"),
                             names_pattern = "(\\S*_\\S*)_(\\w*)",
                             values_to = "Values") %>% 
  pivot_wider(names_from = "Metric",values_from = "Values") %>%
  group_by(ISLAND,TAXONCODE,ANALYSIS_YEAR) %>% 
  mutate(m._h=1,
         n_h.m_h=n_h*m._h,
         var.D.._h.= ((1-(n_h/N_h))/(n_h))*s_1h2,
         s_uh=sqrt(s_1h2),
         w_h=c_w_h(N_h = N_h,M_h=M_h))

TargetCV=0.25
#Domain scale estimates (ISLAND)
DF_stl=DF_hl %>% 
  group_by(ISLAND,TAXONCODE,ANALYSIS_YEAR) %>% 
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

Old_Style_Neyman_Allocation=read.csv("./MHI_NWHI_allocation_2024_MOD.csv")
ISL_OSNA=Old_Style_Neyman_Allocation %>%
  filter(REGION=="MHI") %>% 
  group_by(REGION,ISLAND) %>% 
  reframe(N=sum(WEIGHTED_ALLOCATION_MOD))

RefCV=25
NstarPlots=DF_stl %>% filter(TAXONCODE=="SSSS") %>% 
  ggplot(aes(x=Nstar,y=Target_CV,color=ANALYSIS_YEAR))+
  geom_segment(aes(x=Nstar_obs,xend=n,y=CV.D.._st.*100,yend=CV.D.._st.*100))+
  geom_point(aes(x=Nstar_obs,y=CV.D.._st.*100),shape=8)+
  geom_point(aes(x=n,y=CV.D.._st.*100),shape=19)+
  geom_line()+
  scale_shape_manual(values=c("Nstar @ observed CV"=8,"N Sampled"=19))+
  facet_wrap(TAXONCODE~ISLAND,nrow=5)+
  geom_hline(yintercept = RefCV,color="purple",lty=2)+
  geom_vline(aes(xintercept = N),color="gray",data=ISL_OSNA)+
  geom_text(aes(x = N+5,y=RefCV+5,label=N),color="gray",data=ISL_OSNA)+
  scale_x_log10(limits=c(1,200))+theme_bw()+
  ylab("Co. of Variation: Adult Colony Density")+
  xlab("Nstar - Optimal Sampling for given CV target")
sc=1.25
ggsave(plot = NstarPlots,filename = "./NstarPlots_SSSS_withNeyman_MHI.jpg",width=sc*11,height=sc*8.5)

#ADD ISLAND DOMAIN ESTIMATES BACK INTO STRATA LEVEL
DF_hl=DF_hl %>% left_join(DF_stl,by=c("ISLAND","TAXONCODE","ANALYSIS_YEAR"))

SSSS_st=DF_hl %>% filter(TAXONCODE=="SSSS") %>% group_by("ISLAND","ANALYSIS_YEAR")
View(SSSS_st)

test=DF_hl %>% filter(TAXONCODE=="PCOM"&ANALYSIS_YEAR=="2019"&ISLAND=="Hawaii") %>% 
  mutate(Nstar_obs=c_Nstar(w_h=w_h,
                           s_uh = s_uh,
                           s_1h2 = s_1h2,
                           s_2h2 = 0,
                           mstar_h = 1,
                           V.D.._st = V.D.._st.,
                           N_h = N_h)) 
sum(test$w_h*test$s_uh)*(sum(test$w_h*test$s_uh) + sum((test$w_h^2*0)/(1*test$w_h*test$s_uh)))
SSSS_st=SSSS_st %>% 
  group_by(ISLAND,ANALYSIS_YEAR) %>% 
  mutate(Nstar_h=c_Nstar_h(Nstar=Nstar,
                           w_h=w_h,
                           s_uh=s_uh))
#View(SSSS_st)
c_Nstar_h()
SSSS_h = DF_hl %>% filter(TAXONCODE=="SSSS")
#sum(SSSS_h$w_h) #check that all weighting factors (for a given taxon) sum to 1


V.D.._st.c=c_V.D.._st(D.._h = SSSS_h$D.._)


Nstar_h.c=c_Nstar_h(Nstar = c_Nstar(w_h = ),
                    w_h = ,
                    s_uh =)


regexp(names(DF_h))






