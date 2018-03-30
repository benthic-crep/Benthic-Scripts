rm(list=ls())

### Load Libraries, Data and Make Selection of What Data to Include ---------------
# Load Libraries ----------------------------------------------------------
library(mgcv)
library(ggplot2)
library(plyr)
library(gridExtra)
library(reshape2)

##--------------------------------------------------------------------------------------------------------------------------------------------##
# Load Benthic REA Data, Adults, Juveniles, CoralNet Points  ----------------------------------------

#ADULT: wd is adult
load("/Users/thomas.oliver/WORK/CRED_WORK/Projects/NCRMP Power Analysis/BenthicPower/DataFiles/TMPBenthicREA_Adultwd.Rdata")
Ad_col=wd
#purge shitty sites
ss=c("HAW-01631","HAW-01643","MAU-00621","TAU-00675","TUT-01792")
purge=which(Ad_col$SITE%in%ss)
if(length(purge)>0){Ad_col=Ad_col[-purge,]}
rm(list=c("wd"))
Ad_col[Ad_col==-9]<-NA # Change -9 to NA
Ad_col$RDall <- rowSums(Ad_col[,c("RDEXTENT1", "RDEXTENT2")], na.rm=TRUE) #add up to 2 recent dead columns

#JUVENILE: wd is juv
load("/Users/thomas.oliver/WORK/CRED_WORK/Projects/NCRMP Power Analysis/BenthicPower/DataFiles/TMPBenthicREA_Juvwd.Rdata")
Ju_col=wd
rm(list=c("wd"))

#COVER: cnet is Cover
load("/Users/thomas.oliver/WORK/CRED_WORK/Projects/NCRMP Power Analysis/BenthicPower/DataFiles/ALL_BIA_STR_CNET.rdata")
Co_pt=cnet
rm(list=c("cnet"))


##--------------------------------------------------------------------------------------------------------------------------------------------##
# Load Area Files  ---------------------------------------------------------
sitemaster=read.csv("/Users/thomas.oliver/WORK/CRED_WORK/Projects/NCRMP Power Analysis/BenthicPower/DataFiles/Benthic2013-17_SiteMaster_v4.csv")
sitemaster_iw=read.csv("/Users/thomas.oliver/WORK/CRED_WORK/Projects/NCRMP Power Analysis/BenthicPower/DataFiles/SITE MASTER.csv")

areas=read.csv("/Users/thomas.oliver/WORK/CRED_WORK/Projects/NCRMP Power Analysis/BenthicPower/DataFiles/Benthic_SectorArea_v4.csv")

##--------------------------------------------------------------------------------------------------------------------------------------------##
#Convert to TAXONCODE - Waiting for Year by Region Canon codes. Until then using SPCODE/SPECIES AS IS 
##--------------------------------------------------------------------------------------------------------------------------------------------##
Ad_col$TAXONCODE=as.factor(Ad_col$SPCODE)
Ju_col$TAXONCODE=as.factor(Ju_col$SPECIES)
#Ad_col$TAXONCODE=as.factor(CanonSPCODE_LU[Ad_col$SPCODE])
#Ju_col$TAXONCODE=as.factor(CanonSPCODE_LU[Ju_col$SPECIES])

# Take Actions to Make all Dataset Play Nice ------------------------------
#lookups to code strata the same in area and wd
SiteLU=sitemaster$SEC_NAME
names(SiteLU)=sitemaster$BENTHIC_BASIC

#Reset Site name to match 
site2fivedigit=function(sitenames){
  dashspot=regexpr("-",sitenames)
  first=substr(sitenames,1,dashspot)
  last=formatC(as.numeric(substr(sitenames,dashspot+1,nchar(as.character(sitenames)))),flag=0,width=5)
  out=paste0(first,last)
  return(out)
}
Co_pt$SITE=site2fivedigit(Co_pt$SITE)
length(Co_pt$SITE[which(!Co_pt$SITE%in%sitemaster$SITE)])

SITE2SEC_LU=sitemaster_iw$SEC_NAME
names(SITE2SEC_LU)=sitemaster_iw$SITE
Co_pt$SEC_NAME=SITE2SEC_LU[Co_pt$SITE]

#Missing Sites are just weird. Gone.
missing=unique(sort(Co_pt$SITE[which(!Co_pt$SITE%in%sitemaster$SITE)]))
Co_pt=Co_pt[-which(Co_pt$SITE%in%missing),]

#Transform CODE to NAME
Ad_col$SEC_NAME=SiteLU[as.vector(Ad_col$BENTHIC_BASIC)]

#Build STRATANAME
Ad_col$STRATANAME=paste0(Ad_col$SEC_NAME,"_",Ad_col$DEPTH_BIN,"_",Ad_col$REEF_ZONE)
Ju_col$STRATANAME=paste0(Ju_col$SEC_NAME,"_",Ju_col$DEPTH_BIN,"_",Ju_col$REEF_ZONE)
Co_pt$STRATANAME=paste0(Co_pt$SEC_NAME,"_",Co_pt$DEPTH_BIN,"_",Co_pt$REEF_ZONE)
areas$STRATANAME=paste0(areas$SEC_NAME,"_",areas$DEPTH_BIN,"_",areas$REEF_ZONE)
not_matched_Ad=Ad_col$STRATANAME[which(is.na(match(Ad_col$STRATANAME,areas$STRATANAME)))]
unique(not_matched_Ad)
unique(Ad_col[which(Ad_col$STRATANAME%in%unique(not_matched_Ad)),"SITE"])
not_matched_Ju=Ju_col$STRATANAME[which(is.na(match(Ju_col$STRATANAME,areas$STRATANAME)))]
unique(not_matched_Ju)
not_matched_Co=Co_pt$STRATANAME[which(is.na(match(Co_pt$STRATANAME,areas$STRATANAME)))]
unique(not_matched_Co)

#Purge Not Matched Strata
purge_ad=which(Ad_col$STRATANAME%in%unique(not_matched_Ad))
if(length(purge_ad)>0) Ad_col=Ad_col[-purge_ad,]
purge_ju=which(Ju_col$STRATANAME%in%unique(not_matched_Ju))
if(length(purge_ju)>0) Ju_col=Ju_col[-purge_ju,]
purge_co=which(Co_pt$STRATANAME%in%unique(not_matched_Co))
if(length(purge_co)>0) Co_pt=Co_pt[-purge_co,]

#Join Areas to Data Files - Adult
matchup_ADi=match(Ad_col$STRATANAME,areas$STRATANAME)
Ad_col.=cbind(Ad_col,areas[matchup_ADi,c("AREA_HA","NH")])
Ad_col.$AREA_M2=Ad_col.$AREA_HA*10000

#Join Areas to Data Files - Juv
matchup_JUi=match(Ju_col$STRATANAME,areas$STRATANAME)
Ju_col.=cbind(Ju_col,areas[matchup_JUi,c("AREA_HA","NH")])
Ju_col.$AREA_M2=Ju_col.$AREA_HA*10000

#Join Areas to Data Files - Cov
matchup_COi=match(Co_pt$STRATANAME,areas$STRATANAME)
Co_pt.=cbind(Co_pt,areas[matchup_COi,c("AREA_HA","NH")])
Co_pt.$AREA_M2=Co_pt.$AREA_HA*10000


#Set initial ANALYSIS_SCHEMA to STRATA
Ad_col.$ANALYSIS_SCHEMA=Ad_col.$STRATANAME
Ju_col.$ANALYSIS_SCHEMA=Ju_col.$STRATANAME
Co_pt.$ANALYSIS_SCHEMA=Co_pt.$STRATANAME
Ad_col.$DOMAIN_SCHEMA=Ad_col.$ISLANDCODE
Ju_col.$DOMAIN_SCHEMA=Ju_col.$ISLANDCODE
Co_pt.$DOMAIN_SCHEMA=Co_pt.$ISLANDCODE

# Define Functions for Ault/Smith Metrics ---------------------------------

AllFactorToChar <- function(sourceDF) {
  Classes <- sapply(sourceDF, class)
  FactorCol=which(Classes=="factor")
  sourceDF[,FactorCol]=sapply(sourceDF[FactorCol],as.vector)
  return(sourceDF)
}

#Given AdultColony-Level REA Data, generate segment level data.
Ad_ColonytoSegment=function(raw){
  ###summarize by segment - Takes Care of Zeros and Scleractinians
  #First get area of each segment.
  a=ddply(raw,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE,TRANSECT,SEGMENT),summarize,
          SegArea=median(SEGWIDTH)*median(SEGLENGTH));
  ###subset just hard corals and calculate abundance
  raw_scl=subset(raw,S_ORDER=="Scleractinia")
  #Get Metrics for Each Taxon
  seg_taxa_each=ddply(raw_scl,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE,TRANSECT,SEGMENT,TAXONCODE),summarize,
                      NCol=length(TAXONCODE),PRD_All=mean(RDall,na.rm=TRUE),POD=mean(OLDDEAD,na.rm=TRUE),PBD=mean(RDall+OLDDEAD,na.rm=TRUE))
  #Get Metrics for All Taxa Pooled
  seg_taxa_all= ddply(raw_scl,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE,TRANSECT,SEGMENT),summarize,
                      NCol=length(TAXONCODE),PRD_All=mean(RDall,na.rm=TRUE),POD=mean(OLDDEAD,na.rm=TRUE),PBD=mean(RDall+OLDDEAD,na.rm=TRUE))
  seg_taxa_all$TAXONCODE="All"
  #Correct Column Order
  seg_taxa_all=seg_taxa_all[,names(seg_taxa_each)]
  #bind long form DF
  seg_taxa=rbind(seg_taxa_all,seg_taxa_each)
  
  ## Merge DF of all segments with All taxa metrics - Make sure there are "zero" segments for _each_level of TAXONCODE
  uSCC=unique(seg_taxa$TAXONCODE)
  #Pre Allocate Long DF
  seg_long_list = vector('list', length(uSCC))
  for(i in 1:length(uSCC)){
    this_taxa=subset(seg_taxa,TAXONCODE==uSCC[i])
    this_seg=merge(a,this_taxa, by=c("DOMAIN_SCHEMA","REGION","ISLANDCODE","SEC_NAME","STRATANAME","ANALYSIS_SCHEMA","NH","ANALYSIS_YEAR","SITE","TRANSECT","SEGMENT"),
                   all.x=TRUE)
    this_seg$TAXONCODE=uSCC[i]
    this_seg$NCol[which(is.na(this_seg$NCol))]=0
    this_seg$ColDen=this_seg$NCol/this_seg$SegArea
    seg_long_list[[i]]=this_seg
  }
  seg_long = do.call('rbind', seg_long_list)

  return(seg_long)
}

#Given AdultColony-Level REA Data, generate segment data.
Ju_ColonytoSegment=function(raw){
  ###summarize by segment - Takes Care of Zeros and Scleractinians
  #First get area of each segment.
  a=ddply(raw,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE,TRANSECT,SEGMENT),summarize,
          SegArea=median(SEGWIDTH)*median(SEGLENGTH));
  ###subset just hard corals and calculate abundance
  raw_scl=subset(raw,S_ORDER=="Scleractinia")
  #Get Metrics for Each Taxon
  seg_taxa_each=ddply(raw_scl,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE,TRANSECT,SEGMENT,TAXONCODE),summarize,
                      NCol=length(TAXONCODE))
  #Get Metrics for All Taxa Pooled
  seg_taxa_all= ddply(raw_scl,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE,TRANSECT,SEGMENT),summarize,
                      NCol=length(TAXONCODE))
  seg_taxa_all$TAXONCODE="All"
  #Correct Column Order
  seg_taxa_all=seg_taxa_all[,names(seg_taxa_each)]
  #bind long form DF
  seg_taxa=rbind(seg_taxa_all,seg_taxa_each)
  
  ## Merge DF of all segments with All taxa metrics - Make sure there are "zero" segments for _each_level of TAXONCODE
  uSCC=unique(seg_taxa$TAXONCODE)
  #Pre Allocate Long DF
  seg_long_list = vector('list', length(uSCC))
  for(i in 1:length(uSCC)){
    this_taxa=subset(seg_taxa,TAXONCODE==uSCC[i])
    this_seg=merge(a,this_taxa, by=c("DOMAIN_SCHEMA","REGION","ISLANDCODE","SEC_NAME","STRATANAME","ANALYSIS_SCHEMA","NH","ANALYSIS_YEAR","SITE","TRANSECT","SEGMENT"),
                   all.x=TRUE)
    this_seg$TAXONCODE=uSCC[i]
    this_seg$NCol[which(is.na(this_seg$NCol))]=0
    this_seg$ColDen=this_seg$NCol/this_seg$SegArea
    seg_long_list[[i]]=this_seg
  }
  seg_long = do.call('rbind', seg_long_list)
  
  return(seg_long)
}

#Given AdultColony-Level REA Data, generate transect level data.
Ad_ColonytoTransect=function(raw){
  ###summarize by transect - Takes Care of Zeros and Scleractinians
  #First get area of each transect.
  a=ddply(raw,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE,TRANSECT,SEGMENT),summarize,
          SegArea=median(SEGWIDTH)*median(SEGLENGTH))
  aa=ddply(a,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE,TRANSECT),summarize,
          TranArea=sum(SegArea,na.rm=TRUE))
  ###subset just hard corals and calculate abundance
  raw_scl=subset(raw,S_ORDER=="Scleractinia")
  #Get Metrics for Each Taxon
  tran_taxa_each=ddply(raw_scl,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE,TRANSECT,TAXONCODE),summarize,
                      NCol=length(TAXONCODE),PRD_All=mean(RDall,na.rm=TRUE),POD=mean(OLDDEAD,na.rm=TRUE),PBD=mean(RDall+OLDDEAD,na.rm=TRUE))
  #Get Metrics for All Taxa Pooled
  tran_taxa_all= ddply(raw_scl,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE,TRANSECT),summarize,
                      NCol=length(TAXONCODE),PRD_All=mean(RDall,na.rm=TRUE),POD=mean(OLDDEAD,na.rm=TRUE),PBD=mean(RDall+OLDDEAD,na.rm=TRUE))
  tran_taxa_all$TAXONCODE="All"
  #Correct Column Order
  tran_taxa_all=tran_taxa_all[,names(tran_taxa_each)]
  #bind long form DF
  tran_taxa=rbind(tran_taxa_all,tran_taxa_each)
  
  ## Merge DF of all transects with All taxa metrics - Make sure there are "zero" transects for _each_level of TAXONCODE
  uSCC=unique(tran_taxa$TAXONCODE)
  #Pre Allocate Long DF
  tran_long_list = vector('list', length(uSCC))
  for(i in 1:length(uSCC)){
    this_taxa=subset(tran_taxa,TAXONCODE==uSCC[i])
    this_tran=merge(aa,this_taxa, by=c("DOMAIN_SCHEMA","REGION","ISLANDCODE","SEC_NAME","STRATANAME","ANALYSIS_SCHEMA","NH","ANALYSIS_YEAR","SITE","TRANSECT"),
                   all.x=TRUE)
    this_tran$TAXONCODE=uSCC[i]
    this_tran$NCol[which(is.na(this_tran$NCol))]=0
    this_tran$ColDen=this_tran$NCol/this_tran$TranArea
    tran_long_list[[i]]=this_tran
  }
  tran_long = do.call('rbind', tran_long_list)
  
  return(tran_long)
}

#Given Juvenile Colony-Level REA Data, generate transect data.
Ju_ColonytoTransect=function(raw){
  ###summarize by transect - Takes Care of Zeros and Scleractinians
  #First get area of each transect.
  a=ddply(raw,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE,TRANSECT,SEGMENT),summarize,
          SegArea=median(SEGWIDTH)*median(SEGLENGTH))
  aa=ddply(a,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE,TRANSECT),summarize,
           TranArea=sum(SegArea,na.rm=TRUE))
  ###subset just hard corals and calculate abundance
  raw_scl=subset(raw,S_ORDER=="Scleractinia")
  #Get Metrics for Each Taxon
  tran_taxa_each=ddply(raw_scl,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE,TRANSECT,TAXONCODE),summarize,
                       NCol=length(TAXONCODE))
  #Get Metrics for All Taxa Pooled
  tran_taxa_all= ddply(raw_scl,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE,TRANSECT),summarize,
                       NCol=length(TAXONCODE))
  tran_taxa_all$TAXONCODE="All"
  #Correct Column Order
  tran_taxa_all=tran_taxa_all[,names(tran_taxa_each)]
  #bind long form DF
  tran_taxa=rbind(tran_taxa_all,tran_taxa_each)
  
  ## Merge DF of all transects with All taxa metrics - Make sure there are "zero" transects for _each_level of TAXONCODE
  uSCC=unique(tran_taxa$TAXONCODE)
  #Pre Allocate Long DF
  tran_long_list = vector('list', length(uSCC))
  for(i in 1:length(uSCC)){
    this_taxa=subset(tran_taxa,TAXONCODE==uSCC[i])
    this_tran=merge(aa,this_taxa, by=c("DOMAIN_SCHEMA","REGION","ISLANDCODE","SEC_NAME","STRATANAME","ANALYSIS_SCHEMA","NH","ANALYSIS_YEAR","SITE","TRANSECT"),
                    all.x=TRUE)
    this_tran$TAXONCODE=uSCC[i]
    this_tran$NCol[which(is.na(this_tran$NCol))]=0
    this_tran$ColDen=this_tran$NCol/this_tran$TranArea
    tran_long_list[[i]]=this_tran
  }
  tran_long = do.call('rbind', tran_long_list)
  
  return(tran_long)
}

#Given AdultColony-Level REA Data, generate site level data.
Ad_ColonytoSite=function(raw){
  ###summarize by site - Takes Care of Zeros and Scleractinians
  #First get area surveyed of each site
  a=ddply(raw,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE,TRANSECT,SEGMENT),summarize,
          SegArea=median(SEGWIDTH)*median(SEGLENGTH))
  aa=ddply(a,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE),summarize,
           SiteArea=sum(SegArea,na.rm=TRUE))
  ###subset just hard corals and calculate abundance
  raw_scl=subset(raw,S_ORDER=="Scleractinia")
  #Get Metrics for Each Taxon
  site_taxa_each=ddply(raw_scl,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE,TAXONCODE),summarize,
                       NCol=length(TAXONCODE),PRD_All=mean(RDall,na.rm=TRUE),POD=mean(OLDDEAD,na.rm=TRUE),PBD=mean(RDall+OLDDEAD,na.rm=TRUE))
  #Get Metrics for All Taxa Pooled
  site_taxa_all= ddply(raw_scl,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE),summarize,
                       NCol=length(TAXONCODE),PRD_All=mean(RDall,na.rm=TRUE),POD=mean(OLDDEAD,na.rm=TRUE),PBD=mean(RDall+OLDDEAD,na.rm=TRUE))
  site_taxa_all$TAXONCODE="All"
  #Correct Column Order
  site_taxa_all=site_taxa_all[,names(site_taxa_each)]
  #bind long form DF
  site_taxa=rbind(site_taxa_all,site_taxa_each)
  
  ## Merge DF of all sites with All taxa metrics - Make sure there are "zero" sites for _each_level of TAXONCODE
  uSCC=unique(site_taxa$TAXONCODE)
  #Pre Allocate Long DF
  site_long_list = vector('list', length(uSCC))
  for(i in 1:length(uSCC)){
    this_taxa=subset(site_taxa,TAXONCODE==uSCC[i])
    this_site=merge(aa,this_taxa, by=c("DOMAIN_SCHEMA","REGION","ISLANDCODE","SEC_NAME","STRATANAME","ANALYSIS_SCHEMA","NH","ANALYSIS_YEAR","SITE"),
                    all.x=TRUE)
    this_site$TAXONCODE=uSCC[i]
    this_site$NCol[which(is.na(this_site$NCol))]=0
    this_site$ColDen=this_site$NCol/this_site$SiteArea
    site_long_list[[i]]=this_site
  }
  site_long = do.call('rbind', site_long_list)
  
  return(site_long)
}

Ad_ColonytoSite2s=function(raw){
  ###summarize by site - Takes Care of Zeros and Scleractinians
  #First get area surveyed of each site
  a=ddply(raw,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE,TRANSECT,SEGMENT),summarize,
          SegArea=median(SEGWIDTH)*median(SEGLENGTH))
  aa=ddply(a,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE),summarize,
           SiteArea=sum(SegArea,na.rm=TRUE))
  
  ###subset just hard corals and calculate abundance
  raw_scl=subset(raw,S_ORDER=="Scleractinia")
  #First roll to Transect
  Tran=Ad_ColonytoTransect(raw_scl)

    #Get Metrics for Each Taxon
  site_taxa=ddply(Tran,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE,TAXONCODE),summarize,
                       Ntran=length(unique(TRANSECT)),NCol=sum(NCol,na.rm=TRUE),PRD_All=mean(PRD_All,na.rm=TRUE),POD=mean(POD,na.rm=TRUE),PBD=mean(PBD,na.rm=TRUE))

  ## Merge DF of all sites with All taxa metrics - Make sure there are "zero" sites for _each_level of TAXONCODE
  uSCC=unique(site_taxa$TAXONCODE)
  #Pre Allocate Long DF
  site_long_list = vector('list', length(uSCC))
  for(i in 1:length(uSCC)){
    this_taxa=subset(site_taxa,TAXONCODE==uSCC[i])
    this_site=merge(aa,this_taxa, by=c("DOMAIN_SCHEMA","REGION","ISLANDCODE","SEC_NAME","STRATANAME","ANALYSIS_SCHEMA","NH","ANALYSIS_YEAR","SITE"),
                    all.x=TRUE)
    this_site$TAXONCODE=uSCC[i]
    this_site$NCol[which(is.na(this_site$NCol))]=0
    this_site$ColDen=this_site$NCol/this_site$SiteArea
    site_long_list[[i]]=this_site
  }
  site_long = do.call('rbind', site_long_list)
  
  return(site_long)
}

#Given AdultColony-Level REA Data, generate site level data.
Ad_CovertoSite=function(raw,Category="CATEGORY_CODE"){
  ###summarize by site - Takes Care of Zeros and Scleractinians
  #First get area surveyed of each site
  t=table(raw$SITE,raw$CATEGORY_CODE)
  tdf=as.data.frame.matrix(t)
  tdf$All=rowSums(tdf)
  purge=which(tdf$All==0)
  
  if(length(purge)>0){tdf=tdf[-purge,]}
  
  pdf=tdf/tdf$All
  pdf$SITE=rownames(pdf)
  pdf$REGION=raw$REGION[match(pdf$SITE,as.vector(raw$SITE))]
  pdf$ISLANDCODE=raw$ISLANDCODE[match(pdf$SITE,as.vector(raw$SITE))]
  pdf$ANALYSIS_SCHEMA=raw$ANALYSIS_SCHEMA[match(pdf$SITE,as.vector(raw$SITE))]
  pdf$ANALYSIS_YEAR=raw$ANALYSIS_YEAR[match(pdf$SITE,as.vector(raw$SITE))]
  pdf$DOMAIN_SCHEMA=raw$DOMAIN_SCHEMA[match(pdf$SITE,as.vector(raw$SITE))]
  pdf$STRATANAME=raw$STRATANAME[match(pdf$SITE,as.vector(raw$SITE))]
  pdf$NH=raw$NH[match(pdf$SITE,as.vector(raw$SITE))]
  return(pdf)
}

#Given Juvenile Colony-Level REA Data, generate site level data.
Ju_ColonytoSite=function(raw){
  ###summarize by site - Takes Care of Zeros and Scleractinians
  #First get area surveyed of each site
  a=ddply(raw,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE,TRANSECT,SEGMENT),summarize,
          SegArea=median(SEGWIDTH)*median(SEGLENGTH))
  aa=ddply(a,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE),summarize,
           SiteArea=sum(SegArea,na.rm=TRUE))
  ###subset just hard corals and calculate abundance
  raw_scl=subset(raw,S_ORDER=="Scleractinia")
  #Get Metrics for Each Taxon
  site_taxa_each=ddply(raw_scl,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE,TAXONCODE),summarize,
                       NCol=length(TAXONCODE))
  #Get Metrics for All Taxa Pooled
  site_taxa_all= ddply(raw_scl,.(DOMAIN_SCHEMA,REGION,ISLANDCODE,SEC_NAME,STRATANAME,ANALYSIS_SCHEMA,NH,ANALYSIS_YEAR,SITE),summarize,
                       NCol=length(TAXONCODE))
  site_taxa_all$TAXONCODE="All"
  #Correct Column Order
  site_taxa_all=site_taxa_all[,names(site_taxa_each)]
  #bind long form DF
  site_taxa=rbind(site_taxa_all,site_taxa_each)
  
  ## Merge DF of all sites with All taxa metrics - Make sure there are "zero" sites for _each_level of TAXONCODE
  uSCC=unique(site_taxa$TAXONCODE)
  #Pre Allocate Long DF
  site_long_list = vector('list', length(uSCC))
  for(i in 1:length(uSCC)){
    this_taxa=subset(site_taxa,TAXONCODE==uSCC[i])
    this_site=merge(aa,this_taxa, by=c("DOMAIN_SCHEMA","REGION","ISLANDCODE","SEC_NAME","STRATANAME","ANALYSIS_SCHEMA","NH","ANALYSIS_YEAR","SITE"),
                    all.x=TRUE)
    this_site$TAXONCODE=uSCC[i]
    this_site$NCol[which(is.na(this_site$NCol))]=0
    this_site$ColDen=this_site$NCol/this_site$SiteArea
    site_long_list[[i]]=this_site
  }
  site_long = do.call('rbind', site_long_list)
  
  return(site_long)
}


#Define parameters ----
#M_hi= Total possible number of SSUs(transects) in PSU (site) i in stratum h
#N_h= Total possible number of PSUs (site) in stratum h
#Dom_N_h=Total possible number of PSUs (site) in domain h
#w_h=stratum h weighting factor(proportion of available habitat)
#D_h= mean site density of colonies in stratum h

analysis_schema_rollup1S=function(Col_Data,Ad_Ju="Ad",M_hi=250){
  #First roll Colony to Site
  if(Ad_Ju=="Ad"){
    Site_roll=Ad_ColonytoSite(Col_Data) 
  }else{
    Site_roll=Ju_ColonytoSite(Col_Data) 
  }
  #For a Given ANALYSIS_SCHEMA, we need to pool N_h, and generate w_h
  Strata_NHroll=ddply(subset(Site_roll,TAXONCODE=="All"),.(DOMAIN_SCHEMA,ANALYSIS_SCHEMA,STRATANAME),summarize,N_h=median(NH,na.rm=TRUE)) #calculate # of possible sites in a given stratum
  Schema_NHroll=ddply(Strata_NHroll,.(DOMAIN_SCHEMA,ANALYSIS_SCHEMA),summarize,N_h=sum(N_h,na.rm=TRUE))#calculate # of possible sites in a given schema
  Dom_NHroll=ddply(Schema_NHroll,.(DOMAIN_SCHEMA),summarize,Dom_N_h=sum(N_h,na.rm=TRUE))#calculate # of possible sites in a given domain
  Schema_NHroll$Dom_N_h=Dom_NHroll$Dom_N_h[match(Schema_NHroll$DOMAIN_SCHEMA,Dom_NHroll$DOMAIN_SCHEMA)]# add Dom_N_h to schema dataframe
  Schema_NHroll$w_h=Schema_NHroll$N_h/Schema_NHroll$Dom_N_h # add schema weighting factor to schema dataframe
  
  #Now add back the Analysis_Schema Nh and wh to Site_roll
  Site_roll$N_h.as=Schema_NHroll$N_h[match(Site_roll$ANALYSIS_SCHEMA,Schema_NHroll$ANALYSIS_SCHEMA)]
  Site_roll$w_h.as=Schema_NHroll$w_h[match(Site_roll$ANALYSIS_SCHEMA,Schema_NHroll$ANALYSIS_SCHEMA)]
  
  #Roll up the Schema from site 
  Schema_roll=ddply(Site_roll,.(DOMAIN_SCHEMA,ANALYSIS_SCHEMA,TAXONCODE),summarize,
                    n_h=length(NCol),# No. of Sites surveyed in a Strata
                    N_h=median(N_h.as,na.rm=T),# Strata Area (as N 50x50 grids)
                    w_h=median(w_h.as,na.rm=T),# Strata Area (as N 50x50 grids)
                    D._h=mean(ColDen,na.rm=T), # Mean of Site-Level Density in a Stratum
                    S1_h=var(ColDen,na.rm=T), #sample variance in density between sites
                    varD._h=(1-(n_h/N_h))*S2_h/n_h, #Strata level  variance of mean density
                    Y._h=D._h*N_h^2,
                    varY._h=varD._h*N_h^2,
                    SE_D._h=sqrt(varD._h),
                    CV_D._h=SE_D._h/D._h,
                    SE_Y._h=sqrt(varY._h),
                    CV_Y._h=SE_Y._h/Y._h)
  Schema_roll$M_hi=250
  Schema_roll=Schema_roll[,c("DOMAIN_SCHEMA","ANALYSIS_SCHEMA","TAXONCODE",
                             "M_hi","n_h","N_h","w_h",
                             "D._h","S2_h","varD._h","SE_D._h","CV_D._h",
                             "Y._h","varY._h","SE_Y._h","CV_Y._h")]
  return(Schema_roll)
}

analysis_schema_rollup2S=function(Col_Data,Ad_Ju="Ad",M_hi=250){
  #First roll Colony to Site
  if(Ad_Ju=="Ad"){
    Site_roll=Ad_ColonytoSite(Col_Data) 
  }else{
    Site_roll=Ju_ColonytoSite(Col_Data) 
  }
  #For a Given ANALYSIS_SCHEMA, we need to pool N_h, and generate w_h
  Strata_NHroll=ddply(subset(Site_roll,TAXONCODE=="All"),.(DOMAIN_SCHEMA,ANALYSIS_SCHEMA,STRATANAME),summarize,N_h=median(NH,na.rm=TRUE))
  Schema_NHroll=ddply(Strata_NHroll,.(DOMAIN_SCHEMA,ANALYSIS_SCHEMA),summarize,N_h=sum(N_h,na.rm=TRUE))
  Dom_NHroll=ddply(Schema_NHroll,.(DOMAIN_SCHEMA),summarize,Dom_N_h=sum(N_h,na.rm=TRUE))
  Schema_NHroll$Dom_N_h=Dom_NHroll$Dom_N_h[match(Schema_NHroll$DOMAIN_SCHEMA,Dom_NHroll$DOMAIN_SCHEMA)]
  Schema_NHroll$w_h=Schema_NHroll$N_h/Schema_NHroll$Dom_N_h
  #Now add back the Analysis_Schema Nh and wh to Site_roll
  Site_roll$N_h.as=Schema_NHroll$N_h[match(Site_roll$ANALYSIS_SCHEMA,Schema_NHroll$ANALYSIS_SCHEMA)]
  Site_roll$w_h.as=Schema_NHroll$w_h[match(Site_roll$ANALYSIS_SCHEMA,Schema_NHroll$ANALYSIS_SCHEMA)]
  #Roll up the Schema
  Schema_roll=ddply(Site_roll,.(DOMAIN_SCHEMA,ANALYSIS_SCHEMA,TAXONCODE),summarize,
                    n_h=length(NCol),# No. of Sites in a Strata
                    N_h=median(N_h.as,na.rm=T),# Strata Area (as N 50x50 grids)
                    w_h=median(w_h.as,na.rm=T),# Strata Area (as N 50x50 grids)
                    D._h=mean(ColDen,na.rm=T), # Mean of Site-Level Density in a Stratum
                    S2_h=var(ColDen,na.rm=T), #Strata level sample variance of mean density
                    varD._h=(1-(n_h/N_h))*S2_h/n_h, #Strata level  variance of mean density
                    Y._h=D._h*N_h^2,
                    varY._h=varD._h*N_h^2,
                    SE_D._h=sqrt(varD._h),
                    CV_D._h=SE_D._h/D._h,
                    SE_Y._h=sqrt(varY._h),
                    CV_Y._h=SE_Y._h/Y._h)
  Schema_roll$M_hi=250
  Schema_roll=Schema_roll[,c("DOMAIN_SCHEMA","ANALYSIS_SCHEMA","TAXONCODE",
                             "M_hi","n_h","N_h","w_h",
                             "D._h","S2_h","varD._h","SE_D._h","CV_D._h",
                             "Y._h","varY._h","SE_Y._h","CV_Y._h")]
  return(Schema_roll)
}



stratum_rollup2S=function(SSU,SEG.TRANS="TRAN",y="ColDen_All"){
  eval(parse(text=paste("SSU$Y=SSU$",y,sep="")))
  Site_roll=site_rollup(SSU,y=y)
  Site_roll$ST=SEG.TRANS
  Strata_roll=ddply(Site_roll,.(REGION,ANALYSIS_SCHEMA),summarize,
                    M_hi=if(ST=="SEG"){1000}else{250},
                    N_h=median(N_h,na.rm=TRUE),
                    N_hM_h=N_h*M_hi,
                    n_h=length(m_hi),
                    n_hm_h=sum(m_hi),na.rm=TRUE,
                    D.._h=mean(D._hi,na.rm=TRUE),
                    m._h=mean(m_hi,na.rm=TRUE),
                    S2_1h=var(D._hi,na.rm=TRUE),
                    S2_2h=mean(SSUvar_hi,na.rm=TRUE),
                    varD.._h=((1-n_h/N_h)/n_h)*S2_1h+((n_h/N_h*(1-(m._h/M_hi)))/n_hm_h)*S2_2h,
                    SE_D.._h=sqrt(varD.._h),
                    CV_D.._h=SE_D.._h/D.._h,
                    Y.._h=D.._h*N_hM_h^2,
                    varY.._h=varD.._h*N_hM_h^2,
                    s_uh=sqrt(S2_1h-(S2_2h/M_hi)),
                    mstar_h=sqrt(S2_2h)/s_uh)
  Strata_roll$w_h=with(Strata_roll, N_hM_h/sum(N_hM_h))
  Strata_roll=Strata_roll[,c("REGION","ANALYSIS_SCHEMA",
                             "M_hi","N_h","N_hM_h","w_h","n_h","D.._h","S2_1h",
                             "S2_2h","m._h","n_hm_h","varD.._h","SE_D.._h","CV_D.._h","s_uh","mstar_h")]#
  return(Strata_roll)
}



# Performance Functions ---------------------------------------------------
analysis_schema_rollup_cover_1S=function(Cov_Data){
  #First roll Colony to Site
  Site_roll=Ad_CovertoSite(Cov_Data) 
  #For a Given ANALYSIS_SCHEMA, we need to pool N_h, and generate w_h
  Strata_NHroll=ddply(Site_roll,.(DOMAIN_SCHEMA,ANALYSIS_SCHEMA,STRATANAME),summarize,N_h=median(NH,na.rm=TRUE))
  Schema_NHroll=ddply(Strata_NHroll,.(DOMAIN_SCHEMA,ANALYSIS_SCHEMA),summarize,N_h=sum(N_h,na.rm=TRUE))
  Dom_NHroll=ddply(Schema_NHroll,.(DOMAIN_SCHEMA),summarize,Dom_N_h=sum(N_h,na.rm=TRUE))
  Schema_NHroll$Dom_N_h=Dom_NHroll$Dom_N_h[match(Schema_NHroll$DOMAIN_SCHEMA,Dom_NHroll$DOMAIN_SCHEMA)]
  Schema_NHroll$w_h=Schema_NHroll$N_h/Schema_NHroll$Dom_N_h
  #Now add back the Analysis_Schema Nh and wh to Site_roll
  Site_roll$N_h.as=Schema_NHroll$N_h[match(Site_roll$ANALYSIS_SCHEMA,Schema_NHroll$ANALYSIS_SCHEMA)]
  Site_roll$w_h.as=Schema_NHroll$w_h[match(Site_roll$ANALYSIS_SCHEMA,Schema_NHroll$ANALYSIS_SCHEMA)]
  #Roll up the Schema
  Schema_roll=ddply(Site_roll,.(DOMAIN_SCHEMA,ANALYSIS_SCHEMA),summarize,
                    n_h=length(SITE),# No. of Sites in a Strata
                    N_h=median(N_h.as,na.rm=T),# Strata Area (as N 50x50 grids)
                    w_h=median(w_h.as,na.rm=TRUE),
                    CORAL_D._h=mean(CORAL,na.rm=T), # Mean of Site-Level Density in a Stratum
                    CORAL_S2_h=var(CORAL,na.rm=T), #Strata level sample variance of mean density
                    CORAL_varD._h=(1-(n_h/N_h))*CORAL_S2_h/n_h, #Strata level  variance of mean density
                    CORAL_SE_D._h=sqrt(CORAL_varD._h),
                    CORAL_CV_D._h=CORAL_SE_D._h/CORAL_D._h,
                    CCA_D._h=mean(CCA,na.rm=T), # Mean of Site-Level Density in a Stratum
                    CCA_S2_h=var(CCA,na.rm=T), #Strata level sample variance of mean density
                    CCA_varD._h=(1-(n_h/N_h))*CCA_S2_h/n_h, #Strata level  variance of mean density
                    CCA_SE_D._h=sqrt(CCA_varD._h),
                    CCA_CV_D._h=CCA_SE_D._h/CCA_D._h,
                    MA_D._h=mean(MA,na.rm=T), # Mean of Site-Level Density in a Stratum
                    MA_S2_h=var(MA,na.rm=T), #Strata level sample variance of mean density
                    MA_varD._h=(1-(n_h/N_h))*MA_S2_h/n_h, #Strata level  variance of mean density
                    MA_SE_D._h=sqrt(MA_varD._h),
                    MA_CV_D._h=MA_SE_D._h/MA_D._h)
  Schema_roll=Schema_roll[,c("DOMAIN_SCHEMA","ANALYSIS_SCHEMA",
                             "n_h","N_h","w_h",
                             "CORAL_D._h","CORAL_S2_h","CORAL_varD._h","CORAL_SE_D._h","CORAL_CV_D._h",
                             "CCA_D._h","CCA_S2_h","CCA_varD._h","CCA_SE_D._h","CCA_CV_D._h",
                             "MA_D._h","MA_S2_h","MA_varD._h","MA_SE_D._h","MA_CV_D._h")]
  return(Schema_roll)
}

domain_rollup1S=function(Col_Data,Ad_Ju="Ad"){
  Aschema_data=analysis_schema_rollup1S(Col_Data,Ad_Ju=Ad_Ju)
  DomainStr_NHroll=ddply(subset(Aschema_data,TAXONCODE=="All"),.(DOMAIN_SCHEMA),summarize,N_h=sum(N_h,na.rm=TRUE))
  Aschema_data$DomainSumN_h=DomainStr_NHroll$N_h[match(Aschema_data$DOMAIN_SCHEMA,DomainStr_NHroll$DOMAIN_SCHEMA)]
  Aschema_data$w_h=Aschema_data$N_h/Aschema_data$DomainSumN_h
  
  domain_data=ddply(Aschema_data,.(DOMAIN_SCHEMA,TAXONCODE),summarize,
                      D._st=sum(w_h*D._h,na.rm=TRUE),
                      varD._st=sum(w_h^2*varD._h,na.rm=TRUE),
                      Y._st=sum(Y._h,na.rm=TRUE),
                      varY._st=sum(varY._h,na.rm=TRUE),
                      n=sum(n_h,na.rm=TRUE),
                      N=sum(N_h,na.rm=TRUE),
                      SE_varD._st=sqrt(varD._st),
                      CV_varD._st=SE_varD._st/D._st,
                      SE_varY._st=sqrt(varY._st),
                      CV_varY._st=SE_varY._st/Y._st)
  return(domain_data)
}

domain_rollup_cover_1S=function(Cov_Data){
  Aschema_data=analysis_schema_rollup_cover_1S(Cov_Data)
  DomainStr_NHroll=ddply(Aschema_data,.(DOMAIN_SCHEMA),summarize,N_h=sum(N_h,na.rm=TRUE))
  Aschema_data$DomainSumN_h=DomainStr_NHroll$N_h[match(Aschema_data$DOMAIN_SCHEMA,DomainStr_NHroll$DOMAIN_SCHEMA)]
  Aschema_data$w_h=Aschema_data$N_h/Aschema_data$DomainSumN_h
  
  domain_data=ddply(Aschema_data,.(DOMAIN_SCHEMA),summarize,
                    n=sum(n_h,na.rm=TRUE),
                    N=sum(N_h,na.rm=TRUE),
                    CORAL_D._st=sum(w_h*CORAL_D._h,na.rm=TRUE),
                    CORAL_varD._st=sum(w_h^2*CORAL_varD._h,na.rm=TRUE),
                    CORAL_SE_varD._st=sqrt(CORAL_varD._st),
                    CORAL_CV_varD._st=CORAL_SE_varD._st/CORAL_D._st,
                    CCA_D._st=sum(w_h*CCA_D._h,na.rm=TRUE),
                    CCA_varD._st=sum(w_h^2*CCA_varD._h,na.rm=TRUE),
                    CCA_SE_varD._st=sqrt(CCA_varD._st),
                    CCA_CV_varD._st=CCA_SE_varD._st/CCA_D._st,
                    MA_D._st=sum(w_h*MA_D._h,na.rm=TRUE),
                    MA_varD._st=sum(w_h^2*MA_varD._h,na.rm=TRUE),
                    MA_SE_varD._st=sqrt(MA_varD._st),
                    MA_CV_varD._st=MA_SE_varD._st/MA_D._st)
  return(domain_data)
}


NstarCurve1S=function(schema_data,CVvec=seq(.01,.50,by=0.01)){
  dom=ddply(schema_data,.(DOMAIN_SCHEMA,TAXONCODE),summarize, D._st=sum(w_h*D._h,na.rm=TRUE),N=sum(N_h))
  NstarList=vector('list', nrow(dom))
  for(i in 1:nrow(dom)){
    str_sub=subset(schema_data,DOMAIN_SCHEMA==dom$DOMAIN_SCHEMA[i]&TAXONCODE==dom$TAXONCODE[i])
    TVvec=(CVvec*dom$D._st[i])^2
    Swhsh2=sum(str_sub$w_h*sqrt(str_sub$S2_h),na.rm=T)^2
    Swhs2h=sum((str_sub$w_h*str_sub$S2_h),na.rm=T)
    NstarVals=(Swhsh2)/(TVvec+Swhs2h/dom$N[i])
    NstarList[[i]]=data.frame(DOMAIN_SCHEMA=dom$DOMAIN_SCHEMA[i],TAXONCODE=dom$TAXONCODE[i],CV=CVvec,Nstar=NstarVals)
  }
  AllNstarVals=do.call('rbind',NstarList)
  return(AllNstarVals)
}


NstarCurve_cover_1S=function(schema_data,CVvec=seq(.01,.50,by=0.01),mn="CORAL_D._h",var="CORAL_S2_h"){
  eval(parse(text=paste0("dom=ddply(schema_data,.(DOMAIN_SCHEMA),summarize, D._st=sum(w_h*",mn,",na.rm=TRUE),N=sum(N_h))")))
  eval(parse(text=paste0("schema_data$S2_h=schema_data$",var)))
  eval(parse(text=paste0("schema_data$S2_h=schema_data$",var)))
  NstarList=vector('list', nrow(dom))
  for(i in 1:nrow(dom)){
    str_sub=subset(schema_data,DOMAIN_SCHEMA==dom$DOMAIN_SCHEMA)
    TVvec=(CVvec*dom$D._st[i])^2
    Swhsh2=sum(str_sub$w_h*sqrt(str_sub$S2_h),na.rm=T)^2
    Swhs2h=sum((str_sub$w_h*str_sub$S2_h),na.rm=T)
    NstarVals=(Swhsh2)/(TVvec+Swhs2h/dom$N[i])
    NstarList[[i]]=data.frame(DOMAIN_SCHEMA=dom$DOMAIN_SCHEMA[i],CV=CVvec,Nstar=NstarVals)
  }
  AllNstarVals=do.call('rbind',NstarList)
  return(AllNstarVals)
}


NrealCurve1S=function(schema_data,site_data,CVvec=seq(.01,.50,by=0.01),Path="DESIGN"){
  if(Path=="DESIGN"){
    dom=ddply(schema_data,.(DOMAIN_SCHEMA,TAXONCODE),summarize,D._st=sum(w_h*D._h,na.rm=TRUE),varD._st=sum(w_h^2*varD._h,na.rm=TRUE),N=sum(N_h,na.rm=TRUE)) 
  }else{
    dom=ddply(site_data,.(DOMAIN_SCHEMA,TAXONCODE),summarize,D._st=mean(ColDen,na.rm=TRUE),varD._st=var(ColDen,na.rm=TRUE),N=length(ColDen)) 
  }
  NrealList=vector('list', nrow(dom))
  for(i in 1:nrow(dom)){
    NrealVals=dom$varD._st[i]/(dom$D._st[i]*CVvec)^2
    NrealList[[i]]=data.frame(DOMAIN_SCHEMA=dom$DOMAIN_SCHEMA[i],TAXONCODE=dom$TAXONCODE[i],CV=CVvec,Nreal=NrealVals)
  }
  AllNrealVals=do.call('rbind',NrealList)
  return(AllNrealVals)
}

NstarH1S=function(strata_data,CV=0.15){
  dom=ddply(strata_data,.(DOMAIN_SCHEMA,TAXONCODE),summarize,D._st=sum(w_h*D._h,na.rm=TRUE),N=sum(N_h),n=sum(n_h))
  if(nrow(dom)>1) print("########WARNING TOO MANY DOMAINS SPECIFIED...")
  TV=(CV*dom$D._st)^2
  Swhsh2=sum(strata_data$w_h*sqrt(strata_data$S2_h),na.rm=T)^2
  Swhs2h=sum((strata_data$w_h*strata_data$S2_h),na.rm=T)
  Nstar=(Swhsh2)/(TV+Swhs2h/dom$N)
  NstarH=dom$n*((strata_data$w_h*sqrt(strata_data$S2_h))/(sum((strata_data$w_h*sqrt(strata_data$S2_h)),na.rm=T)))
  strata_data=cbind(strata_data,NstarH)
  return(strata_data)
}


NstarH1S_cover=function(strata_data,CV=0.15){
  dom=ddply(strata_data,.(DOMAIN_SCHEMA,TAXONCODE),summarize, D._st=sum(w_h*D._h,na.rm=TRUE),N=sum(N_h),n=sum(n_h))
  TV=(CV*dom$D._st)^2
  Swhsh2=sum(strata_data$w_h*sqrt(strata_data$S2_h),na.rm=T)^2
  Swhs2h=sum((strata_data$w_h*strata_data$S2_h),na.rm=T)
  Nstar=(Swhsh2)/(TV+Swhs2h/dom$N)
  NstarH=dom$n*((strata_data$w_h*sqrt(strata_data$S2_h))/(sum((strata_data$w_h*sqrt(strata_data$S2_h)),na.rm=T)))
  strata_data=cbind(strata_data,NstarH)
  return(strata_data)
}


CV2N=function(mn,sd,CVvec=seq(.01,.50,by=0.01)){
  N=(sd/(mn*CVvec))^2
  df=data.frame(N,CV=CVvec)
  return(df)
}

CV2TargetN=function(mn,sd,CVvec=seq(.01,.50,by=0.01),TargetCV=0.10){
  N=(sd/(mn*CVvec))^2
  df=data.frame(N,CV=CVvec)
  TargetN=df$N[which.min((df$CV-TargetCV)^2)]
  return(TargetN)
}


# Subset Data By Region----------------------------
Ad_col.SA15=subset(Ad_col.,OBS_YEAR>=2015&REGION=="SAMOA"&TRANSECT==1&MISSIONID=="HA1501")
Ju_col.SA15=subset(Ju_col.,OBS_YEAR>=2015&REGION=="SAMOA"&TRANSECT==3&MISSIONID=="HA1501")
Co_pt.SA15=subset(Co_pt.,OBS_YEAR==2015&REGION=="SAMOA"&MISSIONID=="HA1501")

Ad_col.HI16=subset(Ad_col.,OBS_YEAR>=2016&REGION=="MHI"&TRANSECT==1&MISSIONID=="HA1606")
Ju_col.HI16=subset(Ju_col.,OBS_YEAR>=2016&REGION=="MHI"&TRANSECT==3&MISSIONID=="HA1606")
Co_pt.HI16=subset(Co_pt.,OBS_YEAR==2016&REGION=="MHI"&MISSIONID=="HA1606")

Ad_col.NW16=subset(Ad_col.,OBS_YEAR>=2016&REGION=="NWHI"&TRANSECT==1&MISSIONID=="HA1606")
Ju_col.NW16=subset(Ju_col.,OBS_YEAR>=2016&REGION=="NWHI"&TRANSECT==3&MISSIONID=="HA1606")
Co_pt.NW16=subset(Co_pt.,OBS_YEAR==2016&REGION=="NWHI"&MISSIONID=="HA1606")

# Rollup Regions and Datasets to Define Domains ####
domSA.a=domain_rollup1S(Ad_col.SA15,Ad_Ju="Ad")
domSA.j=domain_rollup1S(Ju_col.SA15,Ad_Ju="Ju")
domSA.c=domain_rollup_cover_1S(Co_pt.SA15)
domHI.a=domain_rollup1S(Ad_col.HI16,Ad_Ju="Ad")
domHI.j=domain_rollup1S(Ju_col.HI16,Ad_Ju="Ju")
domHI.c=domain_rollup_cover_1S(Co_pt.HI16)
domNW.a=domain_rollup1S(Ad_col.NW16,Ad_Ju="Ad")
domNW.j=domain_rollup1S(Ju_col.NW16,Ad_Ju="Ju")
domNW.c=domain_rollup_cover_1S(Co_pt.NW16)

# Run Taxa, Plot Sampling Curves ####
Target=c("All","FSTE","MCUR","PVER","AMYR","LPUR","PDUE","ICRA","PLIC","PDAM")
AbunLU=c("All",rep("Abundant",3),rep("Common",3),rep("Rare",3))
names(AbunLU)=Target

strSA.a=analysis_schema_rollup1S(Ad_col.SA15,Ad_Ju = "Ad")
strSA.a.tx=subset(strSA.a,TAXONCODE%in%Target)
strSA.a.tx$Abundance_Class=AbunLU[strSA.a.tx$TAXONCODE]
strSA.a.tx$DOMAIN_SCHEMA="SAMOA"
tx.out=NstarCurve1S(strSA.a.tx)
tx.out$Abundance_Class=AbunLU[tx.out$TAXONCODE]

ggplot(subset(tx.out,TAXONCODE!="ICRA"),aes(x=Nstar,y=CV,color=Abundance_Class,lty=TAXONCODE))+
  geom_line()+
  geom_hline(yintercept = 0.15,color="black",lty=2)+
  xlim(c(0,100))+
  ylim(c(0,.50))+
  xlab("Benthic Boat-Days (3.5 Sites/Boat-Day)")+
  ggtitle(paste0("Adult Coral Colony Density: Across Taxa Classes:\n Lines = Regional Scale, Dot = Realized at Island-Scale"))


subset(domSA.a,TAXONCODE=="All")
subset(domSA.j,TAXONCODE=="All")
domSA.c

subset(domHI.a,TAXONCODE=="All")
subset(domHI.j,TAXONCODE=="All")
domHI.c

subset(domNW.a,TAXONCODE=="All")
subset(domNW.j,TAXONCODE=="All")
domNW.c


domSA.a$REGION="SAMOA"
domHI.a$REGION="MHI"
domNW.a$REGION="NWHI"
TXCD="MCUR"
dom.a=rbind(subset(domSA.a,TAXONCODE==TXCD),subset(domHI.a,TAXONCODE==TXCD),subset(domNW.a,TAXONCODE==TXCD))
dom.a$DATATYPE="ADULT"

Ad_col.SA15.d=Ad_col.SA15
Ad_col.SA15.d$DOMAIN_SCHEMA="SAMOA"
ASdomSA.a=analysis_schema_rollup1S(Ad_col.SA15.d,Ad_Ju = "Ad")
strNh.AS=NstarCurve1S(subset(ASdomSA.a,TAXONCODE==TXCD),CV=seq(0.01,0.30,by=0.01))

Ad_col.HI16.d=Ad_col.HI16
Ad_col.HI16.d$DOMAIN_SCHEMA="MHI"
ASdomHI.a=analysis_schema_rollup1S(Ad_col.HI16.d,Ad_Ju = "Ad")
strNh.HI=NstarCurve1S(subset(ASdomHI.a,TAXONCODE==TXCD),CV=seq(0.01,0.30,by=0.01))
#IslNh.HI=ddply(strNh,.(substr(strNh$ANALYSIS_SCHEMA,1,3)),summarize,sumNstarH=sum(NstarH,na.rm=TRUE))

Ad_col.NW16.d=Ad_col.NW16
Ad_col.NW16.d$DOMAIN_SCHEMA="NWHI"
ASdomNW.a=analysis_schema_rollup1S(Ad_col.NW16.d,Ad_Ju = "Ad")
strNh.NW=NstarCurve1S(subset(ASdomNW.a,TAXONCODE==TXCD),CV=seq(0.01,0.30,by=0.01))
#IslNh.NW=ddply(strNh,.(substr(strNh$ANALYSIS_SCHEMA,1,3)),summarize,sumNstarH=sum(NstarH,na.rm=TRUE))

Nstar.a=rbind(strNh.AS,strNh.HI,strNh.NW)

TARGETCV=0.15
ACD=ggplot()+
  geom_line(data=Nstar.a,aes(x=Nstar/3.5,y=CV,color=DOMAIN_SCHEMA))+
  geom_point(data=dom.a,aes(x=n/3.5,y=CV_varD._st,color=REGION))+
  geom_hline(yintercept = median(dom.a$CV_varD._st),color="gray")+
  geom_hline(yintercept = TARGETCV,color="black",lty=2)+
  xlim(c(0,26))+
  ylim(c(0,.25))+
  xlab("Benthic Boat-Days (3.5 Sites/Boat-Day)")+
  ggtitle(paste0("Adult Coral Colony Density: ",TXCD,"\n Lines = Regional Scale, Dot = Realized at Island-Scale"))

domSA.j$REGION="SAMOA"
domHI.j$REGION="MHI"
domNW.j$REGION="NWHI"
dom.j=rbind(subset(domSA.j,TAXONCODE=="All"),subset(domHI.j,TAXONCODE=="All"),subset(domNW.j,TAXONCODE=="All"))
dom.j$DATATYPE="JUVENILE"

Ju_col.SA15.d=Ju_col.SA15
Ju_col.SA15.d$DOMAIN_SCHEMA="SAMOA"
ASdomSA.j=analysis_schema_rollup1S(Ju_col.SA15.d,Ad_Ju = "Ju")
strNh.AS.j=NstarCurve1S(subset(ASdomSA.j,TAXONCODE=="All"),CV=seq(0.01,0.30,by=0.01))

Ju_col.HI16.d=Ju_col.HI16
Ju_col.HI16.d$DOMAIN_SCHEMA="MHI"
ASdomHI.j=analysis_schema_rollup1S(Ju_col.HI16.d,Ad_Ju = "Ju")
strNh.HI.j=NstarCurve1S(subset(ASdomHI.j,TAXONCODE=="All"),CV=seq(0.01,0.30,by=0.01))
#IslNh.HI=ddply(strNh,.(substr(strNh$ANALYSIS_SCHEMA,1,3)),summarize,sumNstarH=sum(NstarH,na.rm=TRUE))

Ju_col.NW16.d=Ju_col.NW16
Ju_col.NW16.d$DOMAIN_SCHEMA="NWHI"
ASdomNW.j=analysis_schema_rollup1S(Ju_col.NW16.d,Ad_Ju = "Ju")
strNh.NW.j=NstarCurve1S(subset(ASdomNW.j,TAXONCODE=="All"),CV=seq(0.01,0.30,by=0.01))
#IslNh.NW=ddply(strNh,.(substr(strNh$ANALYSIS_SCHEMA,1,3)),summarize,sumNstarH=sum(NstarH,na.rm=TRUE))

Nstar.j=rbind(strNh.AS.j,strNh.HI.j,strNh.NW.j)


JCD=ggplot()+
  geom_line(data=Nstar.j,aes(x=Nstar/3.5,y=CV,color=DOMAIN_SCHEMA))+
  geom_point(data=dom.j,aes(x=n/3.5,y=CV_varD._st,color=REGION))+
  geom_hline(yintercept = median(dom.j$CV_varD._st),color="gray")+
  geom_hline(yintercept = TARGETCV,color="black",lty=2)+
  xlim(c(0,26))+
  ylim(c(0,.25))+
  xlab("Benthic Boat-Days (3.5 Sites/Boat-Day)")+
  ggtitle("Juvenile Coral Colony Density:\n Lines = Regional Scale, Dot = Realized at Island-Scale")



domSA.c$REGION="SAMOA"
domHI.c$REGION="MHI"
domNW.c$REGION="NWHI"
dom.c=rbind(domSA.c,domHI.c,domNW.c)
dom.c$DATATYPE="COVER"

Co_pt.SA15.d=Co_pt.SA15
Co_pt.SA15.d$DOMAIN_SCHEMA="SAMOA"
ASdomSA.c=analysis_schema_rollup_cover_1S(Co_pt.SA15.d)
strNh.AS.c=NstarCurve_cover_1S(ASdomSA.c,CV=seq(0.01,0.30,by=0.01))

Co_pt.HI16.d=Co_pt.HI16
Co_pt.HI16.d$DOMAIN_SCHEMA="MHI"
ASdomHI.c=analysis_schema_rollup_cover_1S(Co_pt.HI16.d)
strNh.HI.c=NstarCurve_cover_1S(ASdomHI.c,CV=seq(0.01,0.30,by=0.01))

Co_pt.NW16.d=Co_pt.NW16
Co_pt.NW16.d$DOMAIN_SCHEMA="NWHI"
ASdomNW.c=analysis_schema_rollup_cover_1S(Co_pt.NW16.d)
strNh.NW.c=NstarCurve_cover_1S(ASdomNW.c,CV=seq(0.01,0.30,by=0.01))

Nstar.c=rbind(strNh.AS.c,strNh.HI.c,strNh.NW.c)


CC=ggplot()+
  geom_line(data=Nstar.c,aes(x=Nstar/4,y=CV,color=DOMAIN_SCHEMA))+
  geom_point(data=dom.c,aes(x=n/4,y=CORAL_CV_varD._st,color=REGION))+
  geom_hline(yintercept = median(dom.c$CORAL_CV_varD._st),color="gray")+
  geom_hline(yintercept = TARGETCV,color="black",lty=2)+
  xlim(c(0,26))+
  ylim(c(0,.25))+
  xlab("Fish & Benthic Boat-Days (4 Sites/Boat-Day)")+
  ggtitle("Live Coral Cover Density (%):\n Lines = Regional Scale, Dot = Realized at Island-Scale")

library(gridExtra)

All=grid.arrange(CC,ACD,JCD)
ggsave(filename = "/Users/thomas.oliver/WORK/CRED_WORK/Projects/NCRMP Power Analysis/All_Benthic_Sampling Curves.jpg",plot=All,height = 11,width = 8.5)
cover=dom.c[,c("REGION","DOMAIN_SCHEMA","CORAL_D._st","n","CORAL_SE_varD._st","CORAL_CV_varD._st","DATATYPE")]
cover$METRIC="CORAL_COVER"
cover

aden=dom.a[,c("REGION","DOMAIN_SCHEMA","D._st","n","SE_varD._st","CV_varD._st","DATATYPE")]
aden$METRIC="ADULT_DENSITY"
aden

jden=dom.j[,c("REGION","DOMAIN_SCHEMA","D._st","n","SE_varD._st","CV_varD._st","DATATYPE")]
jden$METRIC="JUV_DENSITY"
jden




####Summarize to Segment ####
Ad_col.SA15=subset(Ad_col.,OBS_YEAR>=2015&REGION=="SAMOA"&TRANSECT==1&MISSIONID=="HA1501")
Ju_col.SA15=subset(Ju_col.,OBS_YEAR>=2015&REGION=="SAMOA"&TRANSECT==3&MISSIONID=="HA1501")

segSA.a=Ad_ColonytoSegment(Ad_col.SA15)
tranSA.a=Ad_ColonytoTransect(Ad_col.SA15)
siteSA.a=Ad_ColonytoSite(Ad_col.SA15)

strSA.aa=ddply(subset(siteSA.a,TAXONCODE=="All"),.(ANALYSIS_SCHEMA),summarize,u=mean(ColDen),v=var(ColDen))

schemaSA.a=analysis_schema_rollup1S(Ad_col.SA15,Ad_Ju = "Ad")
schSA.aa=subset(schemaSA.a,TAXONCODE=="All")
strSA.aa$Dv=schSA.aa$varD._h[match(schSA.aa$ANALYSIS_SCHEMA,strSA.aa$ANALYSIS_SCHEMA)]


#old_schema=Ad_col.SA15$DOMAIN_SCHEMA
#Ad_col.SA15$DOMAIN_SCHEMA="SAMOA"
domSA.a=domain_rollup1S(Ad_col.SA15,Ad_Ju = "Ad")
domSA.j=domain_rollup1S(Ju_col.SA15,Ad_Ju = "Ju")
subset(domSA.a,TAXONCODE=="All")
subset(domSA.j,TAXONCODE=="All")

tax="MCUR"
var(subset(segSA.a,TAXONCODE==tax)$ColDen)
var(subset(tranSA.a,TAXONCODE==tax)$ColDen)
var(subset(siteSA.a,TAXONCODE==tax)$ColDen)
median(subset(schemaSA.a,TAXONCODE==tax)$varD._h)

subset(domSA.a,TAXONCODE==tax)$varD._st

Ad_col.SA15$DOMAIN_SCHEMA="SAMOA"
Ju_col.SA15$DOMAIN_SCHEMA="SAMOA"
domSA.j=domain_rollup1S(Ju_col.SA15,Ad_Ju = "Ju")

Target=c("All","FSTE","MCUR","PVER","AMYR","LPUR","PDUE","ICRA","PLIC","PDAM")
AbunLU=c("All",rep("Abundant",3),rep("Common",3),rep("Rare",3))
names(AbunLU)=Target

Stt=subset(schemaSA.a,TAXONCODE%in%Target)
Stt_site=subset(siteSA.a,TAXONCODE%in%Target)
Stt$DOMAIN_SCHEMA="SAMOA"#substr(Stt$ANALYSIS_SCHEMA,1,3)#"SAMOA"#
Stt_site$DOMAIN_SCHEMA="SAMOA"
Ns=NstarCurve1S(schema_data = Stt,CVvec = seq(0.01,0.30,by=0.01))
Ns$Abundance_Class=factor(AbunLU[as.vector(Ns$TAXONCODE)],levels=c("All","Abundant","Common","Rare"))
Nr=NrealCurve1S(schema_data = Stt,site_data = Stt_site,CVvec = seq(0.01,0.30,by=0.01),Path = "DESIGN")
Nr$Abundance_Class=factor(AbunLU[as.vector(Nr$TAXONCODE)],levels=c("All","Abundant","Common","Rare"))
Nr$OPTIMUM="Regional Estimate"
Ns$OPTIMUM="Optimum"
names(Nr)[4]="Nstar"
Nlines=rbind(Ns,Nr)
cv15=subset(Nlines,CV=="0.15")
cv15[order(cv15$Abundance_Class,cv15$OPTIMUM,cv15$Nstar),]

Ad_col.SA15$DOMAIN_SCHEMA="SAMOA"#substr(Ad_col.SA15$ANALYSIS_SCHEMA,1,3)
D=domain_rollup1S(Col_Data = subset(Ad_col.SA15,TAXONCODE%in%Target),Ad_Ju = "Ad")
D$Abundance_Class=factor(AbunLU[as.vector(D$TAXONCODE)],levels=c("All","Abundant","Common","Rare"))
D$Nstar=D$n
D$CV=D$CV_varD._st
ggplot(Ns,aes(x=Nstar,y=CV,color=TAXONCODE))+
  geom_line(aes(lty=OPTIMUM))+
  geom_point(data=D)+
  facet_grid(DOMAIN_SCHEMA~Abundance_Class)+
  xlim(c(0,200))+
  ylim(c(0,.30))


NsH=NstarH1S(strata_data =schemaSA.a,CV = 0.15 )



# NCRMP Tables ------------------------------------------------------------


Co_pt.SA15=subset(Co_pt,OBS_YEAR==2015&REGION=="SAMOA"&MISSIONID=="HA1501")
Co_site_SA15=Ad_CovertoSite(Co_pt.SA15)
Co_pt.HI16=subset(Co_pt,OBS_YEAR==2016&REGION=="MHI"&MISSIONID=="HA1606")
Co_site_HI16=Ad_CovertoSite(Co_pt.HI16)
Co_pt.NW16=subset(Co_pt,OBS_YEAR==2016&REGION=="NWHI"&MISSIONID=="HA1606")
Co_site_NW16=Ad_CovertoSite(Co_pt.NW16)

Co_pt.reg=rbind(Co_pt.SA15,Co_pt.HI16,Co_pt.NW16)
Co_Reg=rbind(Co_site_SA15,Co_site_HI16,Co_site_NW16)

CCtab.=ddply(Co_Reg,.(REGION,ISLANDCODE,STRATANAME),summarize,
            N=length(CORAL),
            CORAL_mn=round(100*mean(CORAL,na.rm=TRUE),1),
            CORAL_sd=round(100*sd(CORAL,na.rm=TRUE),1),
            CORAL_cv=round(100*((sd(CORAL,na.rm=TRUE)/sqrt(N))/mean(CORAL,na.rm=TRUE)),1),
            CCA_mn=round(100*mean(CCA,na.rm=TRUE),1),
            CCA_sd=round(100*sd(CCA,na.rm=TRUE),1),
            CCA_cv=round(100*((sd(CCA,na.rm=TRUE)/sqrt(N))/mean(CCA,na.rm=TRUE)),1),
            MALG_mn=round(100*mean(MA,na.rm=TRUE),1),
            MALG_sd=round(100*sd(MA,na.rm=TRUE),1),
            MALG_cv=round(100*((sd(MA,na.rm=TRUE)/sqrt(N))/mean(MA,na.rm=TRUE)),1))

CCtab=CCtab.
CCtab$TARGET_CV=15
for(i in 1:nrow(CCtab)){
  CCtab$CORAL_TARGET_N[i]=round(CV2TargetN(mn = CCtab$CORAL_mn[i]/100,sd = CCtab$CORAL_sd[i]/100,TargetCV = CCtab$TARGET_CV[i]/100))
  CCtab$CCA_TARGET_N[i]=round(CV2TargetN(mn = CCtab$CCA_mn[i]/100,sd = CCtab$CCA_sd[i]/100,TargetCV = CCtab$TARGET_CV[i]/100))
  CCtab$MALG_TARGET_N[i]=round(CV2TargetN(mn = CCtab$MALG_mn[i]/100,sd = CCtab$MALG_sd[i]/100,TargetCV = CCtab$TARGET_CV[i]/100))
  CCtab$MinBoatDays[i]=ceiling(max(CCtab$CORAL_TARGET_N[i])/4)
  CCtab$MinDays[i]=CCtab$MinBoatDays[i]/4
}
CCtab

CCreg=ddply(Co_Reg,.(REGION),summarize,
            N=length(CORAL),
            CORAL_mn=round(100*mean(CORAL,na.rm=TRUE),1),
            CORAL_sd=round(100*sd(CORAL,na.rm=TRUE),1),
            CORAL_cv=round(100*((sd(CORAL,na.rm=TRUE)/sqrt(N))/mean(CORAL,na.rm=TRUE)),1),
            CCA_mn=round(100*mean(CCA,na.rm=TRUE),1),
            CCA_sd=round(100*sd(CCA,na.rm=TRUE),1),
            CCA_cv=round(100*((sd(CCA,na.rm=TRUE)/sqrt(N))/mean(CCA,na.rm=TRUE)),1),
            MALG_mn=round(100*mean(MA,na.rm=TRUE),1),
            MALG_sd=round(100*sd(MA,na.rm=TRUE),1),
            MALG_cv=round(100*((sd(MA,na.rm=TRUE)/sqrt(N))/mean(MA,na.rm=TRUE)),1))
CCreg$TARGET_CV=15
for(i in 1:nrow(CCreg)){
  CCreg$CORAL_TARGET_N[i]=round(CV2TargetN(mn = CCreg$CORAL_mn[i]/100,sd = CCreg$CORAL_sd[i]/100,TargetCV = CCreg$TARGET_CV[i]/100))
  CCreg$CCA_TARGET_N[i]=round(CV2TargetN(mn = CCreg$CCA_mn[i]/100,sd = CCreg$CCA_sd[i]/100,TargetCV = CCreg$TARGET_CV[i]/100))
  CCreg$MALG_TARGET_N[i]=round(CV2TargetN(mn = CCreg$MALG_mn[i]/100,sd = CCreg$MALG_sd[i]/100,TargetCV = CCreg$TARGET_CV[i]/100))
  CCreg$MinBoatDays[i]=ceiling(max(CCreg$CORAL_TARGET_N[i])/4)
  CCreg$MinDays[i]=CCreg$MinBoatDays[i]/4
}
CCreg$ISLANDCODE="ALL"
CCreg=CCreg[,names(CCtab)]

CCtab$ISLANDCODE=as.character(CCtab$ISLANDCODE)
CCreg
CCall=rbind(CCreg,CCtab)
CCall
write.csv(CCall,"/Users/thomas.oliver/WORK/CRED_WORK/Projects/NCRMP Power Analysis/BenthicPower/DataFiles/CoverTable_SAMOA_MHI_NWHI.csv")




#Should we do One-Stage or Two-Stage? ####



#How many Segments Should We Run?

#How many Photo-Points Should We Run?