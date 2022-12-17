#Data Request for Bex Turner (Bangor University)
#She wants tier 1 data from 2010-2014
#read in data created from BenthicCover_RawtoEstimates_NCRMPViztool

wsd<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2020_Tier1_SITE.csv")

#remove permanent sites, climate sites and special projects
wsd$PERM_SITE[is.na(wsd$PERM_SITE)]<-"0"
wsd$TRANSECT_PHOTOS[is.na(wsd$TRANSECT_PHOTOS)]<-"0"
wsd<-wsd[!wsd$MISSIONID%in% c("MP1410","MP1512","MP1602","MP2006","FAGAALU1","FAGAALU2"),] #I left SE1602 in (2016 Jarvis and Rose)

#Remove occ sites,sites with exclude flag =-1 and permanent sites
wsd<-subset(wsd,is.na(OCC_SITEID)& EXCLUDE_FLAG!="-1"& PERM_SITE!=-1 & Oceanography !=1)

wsd<-subset(wsd,OBS_YEAR<2015)

View(wsd)

wsd<-subset(wsd,select=-c(OCC_SITEID,ANALYSIS_YEAR,EXCLUDE_FLAG,TRANSECT_PHOTOS,Oceanography,PERM_SITE,CLIMATE_STATION_YN,N,new.N,CCA_CORAL,BSR))

write.csv(wsd, file="T:/Benthic/Data/Data Requests/Bex Turner/NCRMPBenthicCover_2010-2014_Tier1_SITE.csv",row.names=F)
