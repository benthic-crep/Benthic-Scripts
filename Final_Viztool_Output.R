indir="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/For Submission/March 2024 Submission/"
outdir="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/For Submission/March 2024 Submission/Files_Split_By_Tier_Region_MonitoringCycle/"
infiles=list.files(indir)
L_files=sort(infiles[grep(pattern = "PacificBenthic",x = infiles)])

RL_=read.csv(paste0(indir,L_files[2]))
IL_=read.csv(paste0(indir,L_files[1]))
SecL_=read.csv(paste0(indir,L_files[3]))
StrL_=read.csv(paste0(indir,L_files[4]))

# Be in Excel or CSV (comma separated value) format.
# Contain data from only one data theme (Benthic, Climate, Fish, or Socio).
# Contain data from only one Jurisdiction.
# Contain data from the same AOI Tier, although the data can represent multiple AOIs within that Tier.
# For example, a Florida Subjurisdiction file can contain data for Southeast Florida, Florida Keys, and Dry Tortugas. It should not contain any data at the higher Florida Jurisdiction level, or the lower Sector or Strata levels.
# Each data theme per basin has a folder, in which there are subfolders for each Jurisdiction (and a subfolder for Sampled Sites for Benthic and Fish).
# Within the Jurisdiction and Sampled Sites folders are subfolders for monitoring cycle year (2020, 2021, 2022, etc.). If a folder doesn’t exist for the year you are submitting data, please create one.  

#Hacky Patch for NA values in REgional N_Cover
RL_$N_cover[is.na(RL_$N_cover)]=0
RL_$N_coverTREND[is.na(RL_$N_coverTREND)]=0
#IL_$N_cover[is.na(IL_$N_cover)]

#I.E. BASIN,BENTHIC,JURISDICTION,AOITIER,YEAR
#Don't sweat JK purge (i.e include old data)

#So, split each AOI tier by jurisdiction and year

uAOITier=c("REGION","ISLAND","SECTOR","STRATA")
uJUR=sort(unique(RL_$AOILabel))
uMC=sort(unique(StrL_$MonitoringCycle))

AOIlu=read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/AOI Naming Convention_Pacific_Edited.csv")########################
#AOIluUE=read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/AOI Naming Convention_Pacific.csv")########################
#library(data.table)
# ssl=str_split(StrL_$AOILabel,pattern = ";")
# jj <- (as.data.frame(do.call(rbind, ssl)))
# StrL_[which(jj$V1=="Marine Protected Areas"),]

YearFolder=c("2023/","2022/","Other Years/")
for(j in 1:length(uJUR)){
  for(m in 1:length(uMC)){
    #j=1;m=3
    if(m==9){ThisYearFolder=YearFolder[1]}else if(m==8){ThisYearFolder=YearFolder[2]}else {ThisYearFolder=YearFolder[3]}
    Reg_RegName=AOIlu$Jurisdiction.Name[match(RL_$AOILabel,AOIlu$Jurisdiction.Name)]
    Reg_jm=RL_ %>% filter(Reg_RegName==uJUR[j] & MonitoringCycle == uMC[m])
    if(nrow(Reg_jm)>0){write.csv(x = Reg_jm,file = paste0(outdir,ThisYearFolder,"VizTool_REGION_",uJUR[j],"_",uMC[m],".csv"))}

    Isl_RegName=AOIlu$Jurisdiction.Name[match(IL_$AOILabel,AOIlu$Subjurisdiction.Name)]
    Isl_jm=IL_ %>% filter(Isl_RegName==uJUR[j] & MonitoringCycle == uMC[m])
    if(nrow(Isl_jm)>0){write.csv(x = Isl_jm,file = paste0(outdir,ThisYearFolder,"VizTool_ISLAND_",uJUR[j],"_",uMC[m],".csv"))}

    Sec_RegName=AOIlu$Jurisdiction.Name[match(SecL_$AOILabel,AOIlu$Island.Sector.Name)]
    Sec_jm=SecL_ %>% filter(Sec_RegName==uJUR[j] & MonitoringCycle == uMC[m])
    if(nrow(Sec_jm)>0){write.csv(x = Sec_jm,file = paste0(outdir,ThisYearFolder,"VizTool_SECTOR_",uJUR[j],"_",uMC[m],".csv"))}

    Str_IslSplit=sapply(strsplit(StrL_$AOILabel,";"),"[[",1)
    Str_RegName=AOIlu$Jurisdiction.Name[match(Str_IslSplit,AOIlu$Subjurisdiction.Name)]
    Str_jm=StrL_ %>% filter(Str_RegName==uJUR[j] & MonitoringCycle == uMC[m])
    if(nrow(Str_jm)>0){write.csv(x = Str_jm,file = paste0(outdir,ThisYearFolder,"VizTool_STRATA_",uJUR[j],"_",uMC[m],".csv"))}
    
    print(paste("Done writing",uJUR[j],"for monitoring year",uMC[m]))
  }
  print(paste("Done writing ALL",uJUR[j],"."))
}
beep()


#Output Sampled Sites
ss_names_cov=c(SiteVisitID="SITEVISITID",SiteID="SITE",Latitude="LATITUDE_LOV",Longitude="LONGITUDE_LOV",SampleYear="ANALYSIS_YEAR",
           Jurisdiction="REGION",Subjurisdiction="ISLAND",Sector="SEC_NAME",Strata="STRATA_NAME",Comments="Comments")
cov.ss=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicCover_2010-2023_Tier1_SITE.csv")
cov.ss$STRATA_NAME=paste0(cov.ss$SEC_NAME,"_",cov.ss$REEF_ZONE,"_",cov.ss$DEPTH_BIN)
cov.ss22=cov.ss %>% filter(ANALYSIS_YEAR=="2022") %>% 
  select(SITEVISITID,SITE,LATITUDE_LOV,LONGITUDE_LOV,ANALYSIS_YEAR,REGION,ISLAND,SEC_NAME,STRATA_NAME) %>%
  mutate(Comments="") %>% 
  rename(all_of(ss_names_cov))%>%
  distinct()
dim(cov.ss22)

cov.ss23=cov.ss %>% filter(ANALYSIS_YEAR=="2023") %>% 
  select(SITEVISITID,SITE,LATITUDE_LOV,LONGITUDE_LOV,ANALYSIS_YEAR,REGION,ISLAND,SEC_NAME,STRATA_NAME) %>%
  mutate(Comments="") %>% 
  rename(all_of(ss_names_cov)) %>%
  distinct()
dim(cov.ss23)

ss_names_demo=c(SiteVisitID="SITEVISITID",SiteID="SITE",Latitude="LATITUDE",Longitude="LONGITUDE",SampleYear="ANALYSIS_YEAR",
           Jurisdiction="REGION",Subjurisdiction="ISLAND",Sector="SEC_NAME",Strata="STRATA_NAME",Comments="Comments")
demo.ss=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS_2023.csv")
demo.ss$STRATA_NAME=paste0(demo.ss$SEC_NAME,"_",demo.ss$REEF_ZONE,"_",demo.ss$DEPTH_BIN)
demo.ss22=demo.ss %>% filter(ANALYSIS_YEAR=="2022") %>% 
  select(SITEVISITID,SITE,LATITUDE,LONGITUDE,ANALYSIS_YEAR,REGION,ISLAND,SEC_NAME,STRATA_NAME) %>%
  mutate(Comments="") %>% 
  rename(all_of(ss_names_demo))%>%
  distinct()
dim(demo.ss23)

demo.ss23=demo.ss %>% filter(ANALYSIS_YEAR=="2023") %>% 
  select(SITEVISITID,SITE,LATITUDE,LONGITUDE,ANALYSIS_YEAR,REGION,ISLAND,SEC_NAME,STRATA_NAME) %>%
  mutate(Comments="") %>% 
  rename(all_of(ss_names_demo))%>%
  distinct()
dim(demo.ss23)

write.csv(cov.ss22,paste0(outdir,"NCRMP_Pacific_Benthic_SurveyedSites_Cover_2022.csv"))
write.csv(cov.ss23,paste0(outdir,"NCRMP_Pacific_Benthic_SurveyedSites_Cover_2023.csv"))
write.csv(demo.ss22,paste0(outdir,"NCRMP_Pacific_Benthic_SurveyedSites_Demo_2022.csv"))
write.csv(demo.ss23,paste0(outdir,"NCRMP_Pacific_Benthic_SurveyedSites_Demo_2023.csv"))