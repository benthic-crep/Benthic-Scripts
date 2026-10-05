library(tidyverse)
library(anytime)
s=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_TAXONCODE_2013_2024.csv")
s$DATE=ymd(s$)
s$DATE_[is.na(s$DATE)]


loadname=load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/FY26/Raw_REA_ADULT_2024.rdata")

save(list = "REA_AD_2024",
     file = )

spday=s %>% group_by(OBS_YEAR) %>% 
  summarize(Nsites=length(unique(SITE)),
            Ndays=length(unique(DATE)),
            Nsites.p.day=Nsites/Ndays)

REA_AD_2024$DATE=as_date(ymd_hms(REA_AD_2024$DATE_))
length(unique(REA_AD_2024$DATE))
REA_AD_2024 %>% group_by(OBS_YEAR) %>% 
  summarize(Nsites=length(unique(SITE)),
            Ndays=length(unique(DATE)),
            Nsites.p.day=Nsites/Ndays)



library(tidyverse)
ca=read.csv("C:/Users/Thomas.Oliver/Downloads/Cruise Activities 02.csv")
head(ca)
MAT=ca %>% mutate(Date=dmy(Date)) %>% group_by(Date) %>% summarize(
  nSPC=length(which(Fish.Rea.Survey.Yn=="YES")),
  nPQ=length(which(Photoquad.Yn=="YES")),
  nSFM=length(which(Sfm.Yn=="YES"&Type!="Oceanography")),
  nSTRr=length(which(Str.Rec.Activity.Yn=="YES")),
  nSTRd=length(which(Str.Dep.Activity.Yn=="YES")),
  nCAUr=length(which(Caus.Retrieve.Yn=="YES")),
  nCAUd=length(which(Caus.Deploy.Yn=="YES")),
  nBMUr=length(which(Bmus.Retrieve.Yn=="YES")),
  nBMUd=length(which(Bmus.Deploy.Yn=="YES")),
  nCTD=length(which(Ctd.Activity.Yn=="YES")),
  nH2O=length(which(H2o.Activity.Yn=="YES")),
  nDS=length(which(Ds.Suite.Activity.Yn=="YES")),
  nCB=length(which(Cb.Activity.Yn=="YES"))
) %>% filter(Date>"2026-05-19",Date<"2026-06-10")

for(i in 1:nrow(MAT)){
  strout=MAT[i,] %>% mutate(STROUT=paste0("Work completed: ",nSPC," SPC sites, ",nPQ," PQ sites, ",nSFM," SfM belts, ",nSTRr," STRs recovered/ ",nSTRd,
  " deployed, ",nCAUd," CAU site, ",nBMUd," BMU site,",nCTD," CTD casts/water samples, ",nCB," carbonate budget survey, ",nDS," Diel Suite deployed/recovered.")) %>% 
    select(Date,STROUT)
  print(strout$Date)
  print(strout$STROUT)
} 

Work completed: 9 SPC sites, 12 PQ sites, 9 SfM belts, 2 STRs recovered/ 3 deployed, 1 CAU site, 1 BMU site,
8 CTD casts/water samples, 1 carbonate budget survey, 3 IKMT tows, 3 eDNA samples, 12 genetic voucher specimens