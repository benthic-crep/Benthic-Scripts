TAXON_FOCI=c("SPCODE","TAXONCODE","GENUS_CODE")

# GENERATE SUMMARY METRICS at the transect-level by SPCODE,TAXONCODE, then GENUS_CODE--------------------------------------------------
for(Ti in 1:length(TAXON_FOCI)){
  TAXON_FOCUS=TAXON_FOCI[Ti]
  #Calc_ColDen_Transect
  acd.tax25<-Calc_ColDen_Transect_25(data = awd,grouping_field = TAXON_FOCUS);
  #RENAME
  acd.tax25=acd.tax25 %>% rename(AdColCount=ColCount,
                                 AdColDen=ColDen,
                                 TRANSECTAREA_ad=TRANSECTAREA)
  jcd.tax25<-Calc_ColDen_Transect_25(jwd,TAXON_FOCUS);
  #RENAME
  jcd.tax25=jcd.tax25 %>% rename(JuvColCount=ColCount,
                                 JuvColDen=ColDen,
                                 TRANSECTAREA_j=TRANSECTAREA)
  
  #Calc_ColMetric_Transect Comparisons
  #Calculate Metrics at Transect Level
  cl.tax25<-Calc_ColMetric_Transect_25(data = awd,grouping_field = TAXON_FOCUS,pool_fields = "COLONYLENGTH"); colnames(cl.tax25)[colnames(cl.tax25)=="Ave.y"]<-"Ave.cl" #Average % old dead
  od.tax25<-Calc_ColMetric_Transect_25(data = awd,grouping_field = TAXON_FOCUS,pool_fields = "OLDDEAD"); colnames(od.tax25)[colnames(od.tax25)=="Ave.y"]<-"Ave.od" #Average % old dead
  rd.tax25<-Calc_ColMetric_Transect_25(data = awd,grouping_field = TAXON_FOCUS,pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.tax25)[colnames(rd.tax25)=="Ave.y"]<-"Ave.rd" #Average % recent dead
  
  #Calculate Disease Metrics at Transect Level
  totdzden.tax25<-Calc_TotDZden_Transect_25(awd,survey_colony,TAXON_FOCUS) # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
  totdzden.tax25=totdzden.tax25 %>% dplyr::select(all_of(c("SITEVISITID","SITE","TRANSECT",TAXON_FOCUS,"TotDZ_den")))
  
  #Calc_RDden_Transect
  rdden.tax25<-Calc_RDden_Transect_25(data = awd,survey_colony_f = survey_colony,grouping_field = TAXON_FOCUS) # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
  acutedz.tax25 = rdden.tax25 %>% dplyr::select(all_of(c("SITEVISITID","SITE","TRANSECT",TAXON_FOCUS,"DZGN_G"))) %>% rename(DZGN_den=DZGN_G)
  
  #Calc_CONDden_Transect
  condden.tax25<-Calc_CONDden_Transect_25(awd,survey_colony,TAXON_FOCUS)# Density of condition colonies by condition, you will need to subset which ever condition you want
  ble.tax25<-subset(condden.tax25,select = c("SITEVISITID","SITE","TRANSECT",TAXON_FOCUS,"BLE"));colnames(ble.tax25)[colnames(ble.tax25)=="BLE"]<-"BLE_den" #subset just bleached colonies
  chronicdz.tax25<-subset(condden.tax25,select = c("SITEVISITID","SITE","TRANSECT",TAXON_FOCUS,"CHRO"));colnames(chronicdz.tax25)[colnames(chronicdz.tax25)=="CHRO"]<-"CHRO_den" #subset just chronic diseased colonies
  
  #CHANGE TRANSECT NUMBERS FOR JUVENILES (pre-2018 we used 3 and 4)
  jcd.tax25$TRANSECT[jcd.tax25$TRANSECT==3]<-1
  jcd.tax25$TRANSECT[jcd.tax25$TRANSECT==4]<-2
  
  #Remove METHOD from dataframes before merging
  acd.tax25<-subset(acd.tax25,select=-c(METHOD))
  jcd.tax25<-subset(jcd.tax25,select=-c(METHOD))
  cl.tax25<-subset(cl.tax25,select=-c(METHOD))
  od.tax25<-subset(od.tax25,select=-c(METHOD))
  rd.tax25<-subset(rd.tax25,select=-c(METHOD))
  
  #Merge density and partial mortality data together.You will need to replace the DUMMY field with the one you want
  data.tax25.=acd.tax25 %>%
    full_join(jcd.tax25,by=c("SITE","SITEVISITID","TRANSECT",TAXON_FOCUS)) %>%
    full_join(cl.tax25,by=c("SITE","SITEVISITID","TRANSECT",TAXON_FOCUS)) %>%
    full_join(od.tax25,by=c("SITE","SITEVISITID","TRANSECT",TAXON_FOCUS)) %>%
    full_join(rd.tax25,by=c("SITE","SITEVISITID","TRANSECT",TAXON_FOCUS)) %>%
    full_join(totdzden.tax25,by=c("SITE","SITEVISITID","TRANSECT",TAXON_FOCUS)) %>%
    full_join(acutedz.tax25,by=c("SITE","SITEVISITID","TRANSECT",TAXON_FOCUS)) %>%
    full_join(chronicdz.tax25,by=c("SITE","SITEVISITID","TRANSECT",TAXON_FOCUS)) %>%
    full_join(ble.tax25,by=c("SITE","SITEVISITID","TRANSECT",TAXON_FOCUS))%>%as.data.frame()
  
  #Add METHOD back in#Adata.tax25.dd METHOD back in
  data.tax25.$METHOD<-"DIVER"
  head(data.tax25.)
  
  #Change NaN to NA
  is.nan.data.frame <- function(x){do.call(cbind, lapply(x, is.nan))}
  data.tax25.[is.nan.data.frame(data.tax25.)] <- NA
  
  #There will be some NAs when you merge the juvenile and adult dataframes together because there may be some juvenile taxa that weren't observed as adults or juveniles
  #This code identifies which transects adult and juvenile colonies were recorded at and then converts NAs to 0s if needed
  ssss<-data.tax25. %>% filter(!!sym(TAXON_FOCUS)=="SSSS")
  ssss$Ad_pres<-ifelse(is.na(ssss$AdColCount),"0","-1")
  ssss$Juv_pres<-ifelse(is.na(ssss$JuvColCount),"0","-1")
  head(ssss)
  
  #Get TRANSECT level info on AJpres, and Transect Area
  ssss<-subset(ssss,select = c(SITE,SITEVISITID,TRANSECT,Ad_pres,Juv_pres,TRANSECTAREA_ad,TRANSECTAREA_j)) 
  head(ssss)
  
  #Join Area / Presence back
  data.tax25.<-left_join(subset(data.tax25.,select = -c(TRANSECTAREA_ad,TRANSECTAREA_j)),ssss) #use transect area from ssss because transectareas for some taxa were NA after merging adults and juvs
  
  #Convert NA densities/counts to 0
  data.tax25.$JuvColCount[is.na(data.tax25.$JuvColCount) & data.tax25.$Juv_pres==-1]<-0
  data.tax25.$JuvColDen[is.na(data.tax25.$JuvColDen) & data.tax25.$Juv_pres==-1]<-0
  data.tax25.$AdColCount[is.na(data.tax25.$AdColCount) & data.tax25.$Ad_pres==-1]<-0
  data.tax25.$AdColDen[is.na(data.tax25.$AdColDen) & data.tax25.$Ad_pres==-1]<-0
  
  #Calculate transect level prevalence for acute dz, chronic dz and bleaching
  data.tax25.$TotDZ_prev<-(data.tax25.$TotDZ_den*data.tax25.$TRANSECTAREA_ad)/data.tax25.$AdColCount*100
  data.tax25.$DZGN_prev<-(data.tax25.$DZGN_den*data.tax25.$TRANSECTAREA_ad)/data.tax25.$AdColCount*100
  data.tax25.$BLE_prev<-(data.tax25.$BLE_den*data.tax25.$TRANSECTAREA_ad)/data.tax25.$AdColCount*100
  data.tax25.$CHRO_prev<-(data.tax25.$CHRO_den*data.tax25.$TRANSECTAREA_ad)/data.tax25.$AdColCount*100
  #data.tax25.$ALGA_prev<-(data.tax25.$ALGA_den*data.tax25.$TRANSECTAREA_ad)/data.tax25.$AdColCount*100
  
  #There will be some NAs when you merge the DZ and other dataframes together because there may be some taxa that didn't have disease
  #Convert NA to 0 ONLY for disease density NOT for prevalence
  data.tax25.$TotDZ_den<-ifelse(is.na(data.tax25.$TotDZ_den),0,data.tax25.$TotDZ_den)
  data.tax25.$DZGN_den<-ifelse(is.na(data.tax25.$DZGN_den),0,data.tax25.$DZGN_den)
  data.tax25.$CHRO_den<-ifelse(is.na(data.tax25.$CHRO_den),0,data.tax25.$CHRO_den)
  data.tax25.$BLE_den<-ifelse(is.na(data.tax25.$BLE_den),0,data.tax25.$BLE_den)
  #data.tax25.$ALGA_den<-ifelse(is.na(data.tax25.$ALGA_den),0,data.tax25.$ALGA_den)
  
  #Remove data from transects with less than 5m surveyed for adults and 1m for juvs.
  data.tax25.$TRANSECTAREA_ad<-ifelse(data.tax25.$TRANSECTAREA_ad<5,NA,data.tax25.$TRANSECTAREA_ad)
  data.tax25.[data.tax25.$TRANSECTAREA_ad<5,]
  data.tax25.$TRANSECTAREA_j<-ifelse(data.tax25.$TRANSECTAREA_j<1,NA,data.tax25.$TRANSECTAREA_j)
  data.tax25.[data.tax25.$TRANSECTAREA_j<1,]
  
  #GENERATE SITE-LEVEL DATA BY AVERAGING TRANSECTS-----------------------------------
  #Since we have moved to a 1 stage design, we need to summarize the transects before rolling up to site. Dione suggested that we calculate mean of 2 transects rather than pooling or dropping a transect
  site.data.tax.<-data.tax25. %>% group_by(across(all_of(c("SITE","SITEVISITID",TAXON_FOCUS)))) %>%  #calc total colonies by condition
    summarise(
      AdColCount=mean(AdColCount,na.rm=T),AdColDen=mean(AdColDen,na.rm = T),Ave.od=mean(Ave.od,na.rm = T),
      Ave.rd=mean(Ave.rd,na.rm = T),Ave.size=mean(Ave.cl,na.rm=T),JuvColCount=mean(JuvColCount,na.rm=T),
      JuvColDen=mean(JuvColDen,na.rm=T),BLE=mean(BLE_den,na.rm=T),TotDZ=mean(TotDZ_den,na.rm=T), AcuteDZ=mean(DZGN_den,na.rm=T),ChronicDZ=mean(CHRO_den,na.rm=T),
      BLE_prev=mean(BLE_prev,na.rm=T),TotDZ_prev=mean(TotDZ_prev,na.rm=T), AcuteDZ_prev=mean(DZGN_prev,na.rm=T),ChronicDZ_prev=mean(CHRO_prev,na.rm=T)) %>% 
    as.data.frame()
  
  eval(parse(text=paste0("site.data_",TAXON_FOCUS,"=site.data.tax.")))
}

site.data_
