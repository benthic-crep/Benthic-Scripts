VT23dir="T:/Benthic/Data/Data Requests/NCRMPViztool/2023/For Submission/March 2024 Submission/Files_Split_By_Tier_Region_MonitoringCycle/"
VT23fl=list.files(VT23dir,full.names = T)

#Strata Files
VT23fl_str=VT23fl[grep("STRATA",VT23fl)]
#file.remove(VT23fl_str)

thisone=read.csv(VT23fl_str[1])
thisone %>% filter(TaxonomicCode=="ALL CORALS") %>% group_by(AOILabel) %>% dplyr::select(N_demo,N_demoTREND) %>% View()
thisone %>% filter(TaxonomicCode=="ALL CORALS") %>% group_by(AOILabel) %>% dplyr::select(N_cover,N_coverTREND)


length(VT23fl)
