#Data request from Erin Reed. 
#Data generated using C:\Users\courtney.s.couch\Documents\GitHub\Benthic-Scripts\NCRMP Viztool\Benthic Cover_RawtoEstimates_NCRMPViztool.R
#Algae summarized up to Class (brown, green, red)- excluding ema and cca


wsd<- read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2020_Tier2b_SITE.csv")

codes_lu<-read.csv("T:/Benthic/Data/Lookup Tables/All_Photoquad_codes.csv")

algae<-subset(codes_lu,TIER_1 %in% c("TURF","MA"))
cols.keep<-levels(as.factor(algae$TIER_2b))


meta.cols<-c("SITE","OBS_YEAR","DATE_","REGION","ISLAND","LATITUDE","LONGITUDE","DEPTH_BIN")
meta.cols<-wsd[meta.cols]
data.cols<-wsd[cols.keep]

all.cols<-cbind(meta.cols,data.cols)
all.cols<-subset(all.cols,select= -c(HARD,RUB,SG))

all.cols<-subset(all.cols,OBS_YEAR>=2013)
write.csv(all.cols,"T:/Benthic/Data/Data Requests/ErinReed_Alga/NCRMP_AlgalCover_Tier2b.csv")