indir="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/For Submission/March 2024 Submission/"
outdir="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/For Submission/March 2024 Submission/Files_Split_By_Tier_Region_MonitoringCycle/"
infiles=list.files(indir)
L_files=infiles[1+c(5,2,8,11)]

RL_=read.csv(paste0(indir,L_files[1]))
IL_=read.csv(paste0(indir,L_files[2]))
SecL_=read.csv(paste0(indir,L_files[3]))
StrL_=read.csv(paste0(indir,L_files[4]))

# Be in Excel or CSV (comma separated value) format.
# Contain data from only one data theme (Benthic, Climate, Fish, or Socio).
# Contain data from only one Jurisdiction.
# Contain data from the same AOI Tier, although the data can represent multiple AOIs within that Tier.
# For example, a Florida Subjurisdiction file can contain data for Southeast Florida, Florida Keys, and Dry Tortugas. It should not contain any data at the higher Florida Jurisdiction level, or the lower Sector or Strata levels.
# Each data theme per basin has a folder, in which there are subfolders for each Jurisdiction (and a subfolder for Sampled Sites for Benthic and Fish).
# Within the Jurisdiction and Sampled Sites folders are subfolders for monitoring cycle year (2020, 2021, 2022, etc.). If a folder doesn’t exist for the year you are submitting data, please create one.  

#I.E. BASIN,BENTHIC,JURISDICTION,AOITIER,YEAR
#Don't sweat JK purge (i.e include old data)

#So, split each AOI tier by jurisdiction and year

uAOITier=c("REGION","ISLAND","SECTOR","STRATA")
uJUR=sort(unique(RL_$AOILabel))
uMC=sort(unique(StrL_$MonitoringCycle))

AOIlu=read.csv("T:/Benthic/Data/Data Requests/NCRMPViztool/2023/unformatted/AOI Naming Convention_Pacific.csv")

for(j in 1:length(uJUR)){
  for(m in 1:length(uMC)){
    #j=1;m=3
    Reg_RegName=AOIlu$Jurisdiction.Name[match(RL_$AOILabel,AOIlu$Jurisdiction.Name)]
    Reg_jm=RL_ %>% filter(Reg_RegName==uJUR[j] & MonitoringCycle == uMC[m])
    if(nrow(Reg_jm)>0){write.csv(x = Reg_jm,file = paste0(outdir,"VizTool_REGION_",uJUR[j],"_",uMC[m],".csv"))}

    Isl_RegName=AOIlu$Jurisdiction.Name[match(IL_$AOILabel,AOIlu$Subjurisdiction.Name)]
    Isl_jm=IL_ %>% filter(Isl_RegName==uJUR[j] & MonitoringCycle == uMC[m])
    if(nrow(Isl_jm)>0){write.csv(x = Isl_jm,file = paste0(outdir,"VizTool_ISLAND_",uJUR[j],"_",uMC[m],".csv"))}

    Sec_RegName=AOIlu$Jurisdiction.Name[match(SecL_$AOILabel,AOIlu$Sector.Name)]
    Sec_jm=SecL_ %>% filter(Sec_RegName==uJUR[j] & MonitoringCycle == uMC[m])
    if(nrow(Sec_jm)>0){write.csv(x = Sec_jm,file = paste0(outdir,"VizTool_SECTOR_",uJUR[j],"_",uMC[m],".csv"))}

    Str_SecSplit=matrix(unlist(strsplit(StrL_$AOILabel,";")),ncol=2,byrow = T)[,1]
    Str_RegName=AOIlu$Jurisdiction.Name[match(Str_SecSplit,AOIlu$Sector.Name)]
    Str_jm=StrL_ %>% filter(Str_RegName==uJUR[j] & MonitoringCycle == uMC[m])
    if(nrow(Str_jm)>0){write.csv(x = Str_jm,file = paste0(outdir,"VizTool_STRATA_",uJUR[j],"_",uMC[m],".csv"))}
    
    print(paste("Done writing",uJUR[j],"for monitoring year",uMC[m]))
  }
  print(paste("Done writing ALL",uJUR[j],"."))
}
beep()
