rm(list=ls())
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic Functions.R")

#LOAD THE CLEAN wd 
load("Benthicwd.Rdata")

###Subset scleractinans and rows without colonies to include zeros
scl<-subset(x,S_ORDER =="Scleractinia"|SPCODE=="AAAA")


# GENERATE SUMMARY METRICS -this is just a start, more metrics to come --------------------------------------------------
m1<-Calc_ColDen_By_Site(scl)
head(m1)

m2<-Calc_olddead_By_Site(scl)
head(m2)


m3<-Calc_ColDen_By_Taxon_Site(scl)
head(m3)

#Merge Site Data and Count Data Per Site  
wsd<-merge(m1,m2,by=c("MISSIONID","REGION","ISLANDCODE","OBS_YEAR","SITE"),all=TRUE)

# OUTPUT working_site_data  -----------------------------------
save(wsd, file="TMPwsd.Rdata")
