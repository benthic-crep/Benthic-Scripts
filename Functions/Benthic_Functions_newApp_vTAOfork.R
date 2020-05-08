# reshape library inclues the cast() function used below
library(reshape2)
library(ggplot2) ## to create the diver vs diver graphs
library(data.table)
library(plyr)
library(dplyr)
library(gdata)
library(tidyr)
library(plotrix)
library(scales)  # for pretty_breaks() function
library(splitstackshape)
library(lme4)
library(nlme)
library(lsmeans)
library(multcomp)
library(lubridate)
# GENERAL FUNCTIONS -------------------------------------------------------
#convert segment numbers from 1,3,5,7 to 0,5,10,15 to reduce confusion
ConvertSegNumber<-function(data){
  data$SEGMENT<-as.factor(data$SEGMENT)
  data<-data %>% mutate(SEGMENT.new=recode(SEGMENT, 
                                           `1`="0",
                                           `3`="5",
                                           `5`="10",
                                           `7`="15",
                                           `NA`="NA"))
  return(data$SEGMENT.new)
}

# #Create General Recent Dead Cause code based on specific cause code
CreateGenRDCode<-function(data,rdcode_field,gencode_name,lookup){
  data$CODE<-data[,rdcode_field] #assign a grouping field for taxa
  data$CODE<-as.factor(data$CODE)
  data<-left_join(data,lookup, by = "CODE")
  colnames(data)[which(colnames(data) == 'GENCODE')] <- gencode_name #change group to whatever your grouping field is.
  data<-subset(data,select=-c(CODE))
  return(data)
}

#Create GENUS_CODE from SPCODE (you will need to do this for the SfM data)
CreateGenusCode<-function(data,taxamaster){
  data<-left_join(data,taxamaster, by = "SPCODE")
  return(data)
}


#Convert SPCODE in raw colony data to taxoncode.We use taxoncode because some taxa can not be reliably identified 
#to species-level across observers and need to be rolled up to genus. -generates a look up table
#NOT WORKING PROPERLY- ADDS NA FOR TAXONNAME AFTER MERGING. 
Convert_to_Taxoncode<-function(data,taxamaster){
  a<-ddply(data,.(REGION,OBS_YEAR,S_ORDER,GENUS_CODE,SPCODE), #create a list of Genera and Species by region and year
           summarise,
           count=length(COLONYID))
  b<-left_join(a,taxamaster,by=c("REGION","OBS_YEAR","SPCODE")) 
  b$TAXONCODE<-ifelse(b$S_ORDER!="Scleractinia",as.character(b$SPCODE),
                      ifelse(is.na(b$TAXON_NAME), as.character(b$GENUS_CODE),as.character(b$SPCODE))) #Change spcode to genus code if we do not uniformly id that taxon to species level
  b$TAXONCODE[b$TAXONCODE==""] <- "UNKN" #Convert unknown species or codes that aren't in our taxa list to unknown
  out<-left_join(data,b,by=c("REGION","OBS_YEAR","GENUS_CODE","SPCODE","S_ORDER"))
  out<-subset(out,select=-c(count,TAXON_NAME,TAXAGROUP)) #merge to master taxa list
  return(out$TAXONCODE)
}

scl_genus_list<-function(data){
  data<-subset(data,S_ORDER=="Scleractinia")
  a<-ddply(data,.(GENUS_CODE,GENUS), #create a list of Genera and Species by region and year
           summarize,
           dummy=length(COLONYID))
  a<-subset(a,select=-c(dummy))
  df<-data.frame("SSSS","All Scleractinia")
  names(df)<-c("GENUS_CODE","GENUS")
  a <- rbind(a, df)
  a<-a[complete.cases(a), ]
  
  return(a)
}

scl_taxonname_list<-function(data){
  data<-subset(data,S_ORDER=="Scleractinia")
  a<-ddply(data,.(SPECIES,TAXONNAME), #create a list of Genera and Species by region and year
           summarize,
           dummy=length(COLONYID))
  a<-subset(a,select=-c(dummy))
  colnames(a)[colnames(a)=="SPECIES"]<-"TAXONCODE" #Change column name
  df<-data.frame("SSSS","All Scleractinia")
  names(df)<-c("TAXONCODE","TAXONNAME")
  a <- rbind(a, df)
  return(a)
}

scl_taxonname_list_ISLAND<-function(data){
  data<-subset(data,S_ORDER=="Scleractinia")
  a<-ddply(data,.(ISLAND,SPECIES,TAXONNAME), #create a list of Genera and Species by region and year
           summarize,
           dummy=length(COLONYID))
  a<-subset(a,select=-c(dummy))
  colnames(a)[colnames(a)=="SPECIES"]<-"TAXONCODE" #Change column name
  return(a)
}

##Calcuate segment and transect area and add column for transect area for methods c,e,f
Transectarea<-function(data){
  data$SEGAREA<-data$SEGLENGTH*data$SEGWIDTH # Calculate segment area
  
  #Calculate total transect area then merge back to a dataframe
  s.df<-ddply(data, .(MISSIONID,REGION,ISLAND,OBS_YEAR,SITE,TRANSECT,SEGMENT,SITEVISITID),
              summarise,
              SEGAREA=median(SEGAREA))
  tr.df<-ddply(s.df, .(MISSIONID,REGION,ISLAND,OBS_YEAR,SITE,TRANSECT,SITEVISITID),
               summarise,
               TRANSECTAREA=sum(SEGAREA))
  
  data<-left_join(data,tr.df, by=c("MISSIONID","REGION","ISLAND","OBS_YEAR","SITE","SITEVISITID","TRANSECT"))
  
  
  return(data$TRANSECTAREA)
}


##Calcuate transect area and add column for transect area for methods a and b
Transectarea_old<-function(data,s.df){
  data$tmp<-data$TRANWIDTH*data$TRANLENGTH # Calculate segment area
  
  #Calculate total transect area then merge back to a dataframe
  tr.df<-ddply(data, .(MISSIONID,REGION,ISLAND,OBS_YEAR,SITE,TRANSECT,SITEVISITID),
               summarise,
               TRANSECTAREA=median(tmp))
  data<-merge(data,tr.df, by=c("MISSIONID","REGION","ISLAND","OBS_YEAR","SITE","SITEVISITID","TRANSECT"),all=TRUE)
  
  
  return(data)
}




####Functions for benthic summary metrics

#This function calculates total area surveyed per transect
Calc_SurveyArea_By_Transect<-function(data){
  
  tr.df<-ddply(data, .(SITE,SITEVISITID,TRANSECT),
               summarise,
               TRANSECTAREA=median(TRANSECTAREA))
  
  return(tr.df)
}


#This function calculates total area surveyed per site
Calc_SurveyArea_By_Site<-function(data){
  
  tr.df<-ddply(data, .(SITE,TRANSECT,SITEVISITID),
               summarise,
               TRANSECTAREA=median(TRANSECTAREA))
  
  tr.df2<-ddply(tr.df, .(SITE,SITEVISITID),
                summarise,
                TRANSECTAREA=sum(TRANSECTAREA))
  return(tr.df2)
}

#### SEGMENT-LEVEL SUMMARY FUNCTIONS ####

## This function calculates total area per segment. Using median will help you with situations where someone recorded different seg areas by mistake.
Calc_SurveyArea_By_Seg<-function(data) {
  
  tr.df<-ddply(data, .(SITE,SITEVISITID,TRANSECT,SEGMENT),
               summarize,
               SEGAREA=median(SEGAREA))
  
  return(tr.df)
}

#updated 2/4/20
## This function calculates colony density at the segment scale by first calculating the total survey area (using Calc_SurveyArea_By_Seg) then calculating colony density
Calc_ColDen_Seg<-function(data, grouping_field="GENUS_CODE"){
  
  data$GROUP<-data[,grouping_field] #assign a grouping field for taxa
  
  #Calculate # of colonies for each variable. You need to have S_ORDER and Fragment here so you can incorporate zeros properly later in the code
  a<-ddply(data, .(METHOD,SITE,SITEVISITID,TRANSECT,ANALYST,SEGMENT,S_ORDER,GROUP,Fragment),
           summarise,
           ColCount=length(COLONYID)) #change to count
  
  #Convert from long to wide and insert 0s for taxa that weren't found at each site. 
  ca0=spread(a,key=GROUP,value=ColCount,fill=0) #Keepin' it TIDYR
  data.cols<-names(ca0[8:dim(ca0)[2]]) #define your data coloumns- note you need to index by column number not by names-you may have situations where there are no AAAA
  field.cols<-c("METHOD","SITE", "SITEVISITID", "TRANSECT","SEGMENT","Fragment") #define field columns
  
  ### Drop all fragments, but don't drop a fragment-only transect... ###
  #change colony counts for fragments to 0 so that we account for the transects that only had fragments
  ca0[which(ca0$Fragment <0), data.cols]<-0 
  
  #At this point you will have multiple rows for each site/transect so sum data by site and transect. This will help you properly insert 0s
  field.cols<-c("METHOD","SITE", "SITEVISITID", "TRANSECT","SEGMENT")
  ca1<-aggregate(ca0[,data.cols], by=ca0[,field.cols], sum) 
  rm(list='ca0')
  
  #Create a list of scleractinian taxa that are in the dataframe as columns then sum across just those taxa to get total scl
  b<-subset(data,S_ORDER=="Scleractinia");taxalist<-as.character(unique(b$GROUP))
  ca1$SSSS<-rowSums(ca1[,taxalist,drop=FALSE]) #calculate total colony density
  ca2 <- gather(ca1, GROUP, ColCount, names(ca1[6:dim(ca1)[2]]), factor_key=TRUE) #convert wide to long format
  rm(list='ca1')
  
  #Remove everything that isn't a scleractinian
  taxalist2<-c(taxalist,"SSSS")
  ca3<-ca2[ca2$GROUP %in% taxalist2,]
  rm(list='ca2')
  
  #Calculate SEGMENT area surveyed and colony density***
  uTA=unique(data[,c("METHOD","SITE","SITEVISITID","TRANSECT","SEGMENT","SEGAREA")])
  out<-join(uTA,ca3, by=c("METHOD","SITE","SITEVISITID","TRANSECT","SEGMENT"))
  out$ColDen<-out$ColCount/out$SEGAREA
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  #if(nrow(out)!=nrow(ca3)) {cat("WARNING:Segment-level data not merging properly")}   # should be 0
  
  return(out)
}


## This function calculates mean colony length, % recent dead, % old dead, condition severity or condition extent to the segment level
## NOTE: can run both adult & juvenile data with this function for COLONYLENGTH
#may be worth changing to "Calc_ColMetric_Seg<-function(data, grouping_field, pool_fields)" and finding a workaround for the dcast warning before it turns into an error
Calc_ColMetric_Seg<-function(data, grouping_field="GENUS_CODE", pool_fields=c("COLONYLENGTH","RDEXTENT1", "RDEXTENT2","OLDDEAD","SEVERITY","EXTENT")) {
  
  scl<-subset(data,Fragment==0&S_ORDER=="Scleractinia") #excludes fragments and anything that isn't a hard coral
  scl$GROUP<-scl[,grouping_field]
  scl$y <- rowSums(scl[,pool_fields,drop=FALSE], na.rm=TRUE) #this will allow you to add the 2 recent dead columns if you are looking at this metric
  
  rd<-ddply(scl, .(METHOD,SITE,SITEVISITID,TRANSECT,SEGMENT,GROUP),
            summarise,
            Ave.y=mean(y, na.rm=TRUE))
  
  rdtot<-ddply(scl, .(METHOD,SITE,SITEVISITID,TRANSECT,SEGMENT),
               summarise,
               Ave.y=mean(y, na.rm=TRUE))
  rdtot$GROUP<-"SSSS"; rdtot <- rdtot[c(1,2,3,4,5,7,6)]
  rd_wide<-dcast(rd, formula=METHOD+ SITE + SITEVISITID +TRANSECT+SEGMENT~ GROUP, value.var="Ave.y",fill=0)
  rd_long <- gather(rd_wide, GROUP, Ave.y, names(rd_wide[6:dim(rd_wide)[2]]), factor_key=TRUE) #convert wide to long format
  rd_long<-rbind(rd,rdtot)
  
  colnames(rd_long)[which(colnames(rd_long) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(rd_long)
}

#Updated 12/27/19
Calc_RDden_Seg<-function(data, survey_colony_f=survey_colony, grouping_field="S_ORDER"){
  scl<-subset(data,Fragment==0 &S_ORDER=="Scleractinia")
  
  #add and "_G" to the general cause code so that we distiguish it from specific cause codes
  scl$GENRD1<-paste(scl$GENRD1,"_G",sep="");scl$GENRD2<-paste(scl$GENRD2,"_G",sep="");scl$GENRD3<-paste(scl$GENRD3,"_G",sep="")
  
  #Change varibles to factors
  factor_cols <- c("GENRD1","GENRD2","GENRD3","RD1","RD2","RD3")
  scl[factor_cols] <- lapply(scl[factor_cols], as.factor)
  
  #collapse all general and specific cause code columns into 1 column so that we can count up # of colonies with each condition
  #this step will spit out an error message about attributes not being identical-ignore this. you did not lose data
  scl_l <- gather(data = scl, key = RDcat, value = RDtype, c(GENRD1,RD1,GENRD2,RD2,GENRD3,RD3), factor_key=TRUE)
  
  #convert from long to wide and fill in 0s
  # Tom gave up trying to make this from dcast to spread to 'keep dependencies down', maybe another day...
  rd<-dcast(scl_l, formula=METHOD+SITEVISITID + SITE+TRANSECT+SEGMENT+COLONYID ~ RDtype, value.var="RDtype",length,fill=0)
  MDcol=c("METHOD","SITEVISITID","SITE","TRANSECT","SEGMENT","COLONYID")
  DATAcol=setdiff(names(rd),MDcol)
  rd.new=rd;  rd.new[,DATAcol][rd.new[,DATAcol]>1]=1  
  
  #merge data with colony level metadata and sum conditions by transect and taxoncode
  allrd3<-left_join(rd.new,survey_colony_f,by=MDcol)
  ConditionsWeCareAbout=names(allrd3[(length(MDcol)+1):dim(rd.new)[2]])
  allrd3_l <- gather(data = allrd3, key = RDCond, value = abun,
                     ConditionsWeCareAbout,
                     factor_key=TRUE) #convert wide to long format by condition
  allrd3_l$GROUP<-allrd3_l[,grouping_field]
  allrd3_lsum<-ddply(allrd3_l, .(METHOD,SITE,SITEVISITID,TRANSECT,SEGMENT,GROUP,RDCond), #calc total colonies by taxon and condition
                     summarise,
                     RDabun=sum(abun))
  out1<-ddply(allrd3_lsum, .(METHOD,SITE,SITEVISITID,TRANSECT,SEGMENT,RDCond), #calc total colonies by condition
              summarise,
              RDabun=sum(RDabun,na.rm=T))
  out1$GROUP<-"SSSS"; out1 <- out1[c(1,2,3,4,5,8,6,7)] #add total colony code
  a<-subset(rbind(allrd3_lsum,out1),!RDCond %in% c("NONE_G","NONE"))
  
  #Convert back to wide format
  abun<-dcast(a, formula=METHOD+SITEVISITID +SITE + TRANSECT+SEGMENT+GROUP~ RDCond, value.var="RDabun",sum,fill=0)
  
  uTA=unique(data[,c("METHOD","SITEVISITID","SITE","TRANSECT","SEGMENT","SEGAREA")])
  ab.tr<-merge(x = uTA,y = abun,by=c("METHOD","SITEVISITID","SITE","TRANSECT","SEGMENT"))
  #ab.tr<-join(x = uTA,y = abun,by=c("SITEVISITID","SITE","TRANSECT","SEGMENT"))#adding NA.1.....
  #Check NAs - Should be empty...
  new_DF <- sum(rowSums(is.na(ab.tr))) # should be 0
  if(new_DF > 0) {cat("WARNING:NAs in dataframe")}   

  #calcualte density of each condition, for output
  out<-ab.tr
  out[ ,which(names(out)==DATAcol[1]):ncol(out)]=out[ ,which(names(out)==DATAcol[1]):ncol(out)]/out$SEGAREA # selects every row and 2nd to last columns
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(out)
}

#Updated 2/4/20
Calc_CONDden_Seg<-function(data,survey_colony_f=survey_colony, grouping_field="S_ORDER"){
  scl<-subset(data,Fragment==0 &S_ORDER=="Scleractinia")
  
  #Add a column that indicates (1= yes, 0= no) whether the colony had a chronic disease
  scl$Chronic<-ifelse(scl$CONDITION_1 %in% c("SGA","PTR","FUG")|scl$CONDITION_2 %in% c("SGA","PTR","FUG")|scl$CONDITION_3 %in% c("SGA","PTR","FUG"),"CHRO","NONE")
  
  #Change varibles to factors
  factor_cols <- c("CONDITION_1","CONDITION_2","CONDITION_3","Chronic")
  scl[factor_cols] <- lapply(scl[factor_cols], as.factor)
  
  #collapse all general and specific cause code columns into 1 column so that we can count up # of colonies with each condition
  long <- gather(data= scl, key= CONDcat,value=CONDtype, c(CONDITION_1,CONDITION_2,CONDITION_3,Chronic), factor_key=TRUE)
  
  #convert from long to wide and fill in 0s
  rd<-dcast(long, formula=METHOD+SITEVISITID + SITE+TRANSECT+SEGMENT+COLONYID ~ CONDtype, value.var="CONDtype",length,fill=0)
  x<-c("CHRO");rd[x[!(x %in% colnames(rd))]] = 0 #if data does not have CHRO, add this column with 0s
  
  MDcol=c("METHOD","SITEVISITID","SITE","TRANSECT","SEGMENT","COLONYID")
  DATAcol=setdiff(names(rd),MDcol)
  rd.new=rd;  rd.new[,DATAcol][rd.new[,DATAcol]>1]=1  
  
  #merge data with colony level metadata and sum conditions by transect and taxoncode
  allrd3<-left_join(rd.new,survey_colony_f)
  ConditionsWeCareAbout=names(allrd3[(length(MDcol)+1):dim(rd.new)[2]])
  allrd3_l <- gather(data = allrd3, key = Cond, value = abun,
                     ConditionsWeCareAbout,
                     factor_key=TRUE) #convert wide to long format by condition
  allrd3_l$GROUP<-allrd3_l[,grouping_field]
  allrd3_lsum<-ddply(allrd3_l, .(METHOD,SITE,SITEVISITID,TRANSECT,SEGMENT,GROUP,Cond), #calc total colonies by taxon and condition
                     summarise,
                     CONDabun=sum(abun))
  out1<-ddply(allrd3_lsum, .(METHOD,SITE,SITEVISITID,TRANSECT,SEGMENT,Cond), #calc total colonies by condition
              summarise,
              CONDabun=sum(CONDabun,na.rm=T))
  out1$GROUP<-"SSSS"; out1 <- out1[c(1,2,3,4,5,8,6,7)] #add total colony code
  a<-subset(rbind(allrd3_lsum,out1),!Cond %in% c("NONE_G","NONE"))
  
  #Convert back to wide format
  abun<-dcast(a, formula=METHOD+SITEVISITID +SITE + SEGMENT+TRANSECT+GROUP~ Cond, value.var="CONDabun",sum,fill=0)
  
  uTA=unique(data[,c("METHOD","SITEVISITID","SITE","TRANSECT","SEGMENT","SEGAREA")])
  ab.tr<-merge(x = uTA,y = abun,by=c("METHOD","SITEVISITID","SITE","TRANSECT","SEGMENT"))
  #ab.tr<-join(x = uTA,y = abun,by=c("SITEVISITID","SITE","TRANSECT","SEGMENT"))#adding NA.1.....
  #Check NAs - Should be empty...
  new_DF <- sum(rowSums(is.na(ab.tr))) # should be 0
  if(new_DF > 0) {cat("WARNING:NAs in dataframe")}   
  
  #Check NAs - Should be empty...
  new_DF <- sum(rowSums(is.na(ab.tr))) # should be 0
  if(new_DF > 0) {cat("WARNING:NAs in dataframe")}   
  
  #calcualte density of each condition, for output
  out<-ab.tr
  out[ ,which(names(out)==DATAcol[1]):ncol(out)]=out[ ,which(names(out)==DATAcol[1]):ncol(out)]/out$SEGAREA # selects every row and 2nd to last columns
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(out)
}

#SEGMENT DIVER-LEVEL COMPARISON
#updated 2/4/20
## This function calculates colony density at the segment scale by first calculating the total survey area (using Calc_SurveyArea_By_Seg) then calculating colony density
Calc_ColDen_Seg_DIVER<-function(data, grouping_field="GENUS_CODE"){
  
  data$GROUP<-data[,grouping_field] #assign a grouping field for taxa
  
  #Calculate # of colonies for each variable. You need to have S_ORDER and Fragment here so you can incorporate zeros properly later in the code
  a<-ddply(data, .(METHOD,SITE,SITEVISITID,TRANSECT, SEGMENT,ANALYST,S_ORDER,GROUP,Fragment),
           summarise,
           ColCount=length(COLONYID)) #change to count
  
  #Convert from long to wide and insert 0s for taxa that weren't found at each site. 
  ca0=spread(a,key=GROUP,value=ColCount,fill=0) #Keepin' it TIDYR
  data.cols<-names(ca0[9:dim(ca0)[2]]) #define your data coloumns- note you need to index by column number not by names-you may have situations where there are no AAAA
  field.cols<-c("METHOD","SITE", "SITEVISITID", "TRANSECT","SEGMENT","ANALYST","Fragment") #define field columns
  
  ### Drop all fragments, but don't drop a fragment-only transect... ###
  #change colony counts for fragments to 0 so that we account for the transects that only had fragments
  ca0[which(ca0$Fragment <0), data.cols]<-0 
  
  #At this point you will have multiple rows for each site/transect so sum data by site and transect. This will help you properly insert 0s
  field.cols<-c("METHOD","SITE", "SITEVISITID", "TRANSECT","SEGMENT","ANALYST")
  ca1<-aggregate(ca0[,data.cols], by=ca0[,field.cols], sum) 
  rm(list='ca0')
  
  #Create a list of scleractinian taxa that are in the dataframe as columns then sum across just those taxa to get total scl
  b<-subset(data,S_ORDER=="Scleractinia");taxalist<-as.character(unique(b$GROUP))
  ca1$SSSS<-rowSums(ca1[,taxalist,drop=FALSE]) #calculate total colony density
  ca2 <- gather(ca1, GROUP, ColCount, names(ca1[7:dim(ca1)[2]]), factor_key=TRUE) #convert wide to long format
  rm(list='ca1')
  
  #Remove everything that isn't a scleractinian
  taxalist2<-c(taxalist,"SSSS")
  ca3<-ca2[ca2$GROUP %in% taxalist2,]
  rm(list='ca2')
  
  #Calculate SEGMENT area surveyed and colony density***
  uTA=unique(data[,c("METHOD","SITE","SITEVISITID","TRANSECT","SEGMENT","ANALYST","SEGAREA")])
  out<-join(uTA,ca3, by=c("METHOD","SITE","SITEVISITID","TRANSECT","SEGMENT","ANALYST"))
  out$ColDen<-out$ColCount/out$SEGAREA
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  #if(nrow(out)!=nrow(ca3)) {cat("WARNING:Segment-level data not merging properly")}   # should be 0
  
  return(out)
}


## This function calculates mean colony length, % recent dead, % old dead, condition severity or condition extent to the segment level
## NOTE: can run both adult & juvenile data with this function for COLONYLENGTH
Calc_ColMetric_Seg_DIVER<-function(data, grouping_field="GENUS_CODE", pool_fields=c("COLONYLENGTH","RDEXTENT1", "RDEXTENT2","OLDDEAD","SEVERITY","EXTENT")) {
  
  scl<-subset(data,Fragment==0&S_ORDER=="Scleractinia") #excludes fragments and anything that isn't a hard coral
  scl$GROUP<-scl[,grouping_field]
  scl$y <- rowSums(scl[,pool_fields,drop=FALSE], na.rm=TRUE) #this will allow you to add the 2 recent dead columns if you are looking at this metric
  
  rd<-ddply(scl, .(METHOD,SITE,SITEVISITID,TRANSECT,SEGMENT,ANALYST,GROUP),
            summarise,
            Ave.y=mean(y, na.rm=TRUE))
  
  rdtot<-ddply(scl, .(METHOD,SITE,SITEVISITID,TRANSECT,SEGMENT,ANALYST),
               summarise,
               Ave.y=mean(y, na.rm=TRUE))
  rdtot$GROUP<-"SSSS"; rdtot <- rdtot[c(1,2,3,4,5,6,8,7)]
  rd_wide<-dcast(rd, formula=METHOD+ SITE + SITEVISITID +TRANSECT+SEGMENT+ANALYST ~ GROUP, value.var="Ave.y",fill=0)
  rd_long <- gather(rd_wide, GROUP, Ave.y, names(rd_wide[7:dim(rd_wide)[2]]), factor_key=TRUE) #convert wide to long format
  rd_long<-rbind(rd,rdtot)
  
  colnames(rd_long)[which(colnames(rd_long) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(rd_long)
}

#Updated 2/4/20
Calc_RDden_Seg_DIVER<-function(data, survey_colony_f=survey_colony, grouping_field="S_ORDER"){
  scl<-subset(data,Fragment==0 &S_ORDER=="Scleractinia")
  
  #add and "_G" to the general cause code so that we distiguish it from specific cause codes
  scl$GENRD1<-paste(scl$GENRD1,"_G",sep="");scl$GENRD2<-paste(scl$GENRD2,"_G",sep="");scl$GENRD3<-paste(scl$GENRD3,"_G",sep="")
  
  #Change varibles to factors
  factor_cols <- c("GENRD1","GENRD2","GENRD3","RD1","RD2","RD3")
  scl[factor_cols] <- lapply(scl[factor_cols], as.factor)
  
  #collapse all general and specific cause code columns into 1 column so that we can count up # of colonies with each condition
  #this step will spit out an error message about attributes not being identical-ignore this. you did not lose data
  scl_l <- gather(data = scl, key = RDcat, value = RDtype, c(GENRD1,RD1,GENRD2,RD2,GENRD3,RD3), factor_key=TRUE)
  
  #convert from long to wide and fill in 0s
  # Tom gave up trying to make this from dcast to spread to 'keep dependencies down', maybe another day...
  rd<-dcast(scl_l, formula=METHOD+SITEVISITID + SITE+TRANSECT+SEGMENT+ANALYST+COLONYID ~ RDtype, value.var="RDtype",length,fill=0)
  MDcol=c("METHOD","SITEVISITID","SITE","TRANSECT","SEGMENT","ANALYST","COLONYID")
  DATAcol=setdiff(names(rd),MDcol)
  rd.new=rd;  rd.new[,DATAcol][rd.new[,DATAcol]>1]=1  
  
  #merge data with colony level metadata and sum conditions by transect and taxoncode
  allrd3<-left_join(rd.new,survey_colony_f,by=MDcol)
  ConditionsWeCareAbout=names(allrd3[(length(MDcol)+1):dim(rd.new)[2]])
  allrd3_l <- gather(data = allrd3, key = RDCond, value = abun,
                     ConditionsWeCareAbout,
                     factor_key=TRUE) #convert wide to long format by condition
  allrd3_l$GROUP<-allrd3_l[,grouping_field]
  allrd3_lsum<-ddply(allrd3_l, .(METHOD,SITE,SITEVISITID,TRANSECT,SEGMENT,ANALYST,GROUP,RDCond), #calc total colonies by taxon and condition
                     summarise,
                     RDabun=sum(abun))
  out1<-ddply(allrd3_lsum, .(METHOD,SITE,SITEVISITID,TRANSECT,SEGMENT,ANALYST,RDCond), #calc total colonies by condition
              summarise,
              RDabun=sum(RDabun,na.rm=T))
  out1$GROUP<-"SSSS"; out1 <- out1[c(1,2,3,4,5,6,9,7,8)] #add total colony code
  a<-subset(rbind(allrd3_lsum,out1),!RDCond %in% c("NONE_G","NONE"))
  
  #Convert back to wide format
  abun<-dcast(a, formula=METHOD+SITEVISITID +SITE + TRANSECT+SEGMENT+ANALYST+GROUP~ RDCond, value.var="RDabun",sum,fill=0)
  
  uTA=unique(data[,c("METHOD","SITEVISITID","SITE","TRANSECT","SEGMENT","ANALYST","SEGAREA")])
  ab.tr<-merge(x = uTA,y = abun,by=c("METHOD","SITEVISITID","SITE","TRANSECT","SEGMENT","ANALYST"))
  #ab.tr<-join(x = uTA,y = abun,by=c("SITEVISITID","SITE","TRANSECT","SEGMENT"))#adding NA.1.....
  #Check NAs - Should be empty...
  new_DF <- sum(rowSums(is.na(ab.tr))) # should be 0
  if(new_DF > 0) {cat("WARNING:NAs in dataframe")}   
  
  #calcualte density of each condition, for output
  out<-ab.tr
  out[ ,which(names(out)==DATAcol[1]):ncol(out)]=out[ ,which(names(out)==DATAcol[1]):ncol(out)]/out$SEGAREA # selects every row and 2nd to last columns
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(out)
}

#Updated 2/4/20
Calc_CONDden_Seg_DIVER<-function(data,survey_colony_f=survey_colony, grouping_field="S_ORDER"){
  scl<-subset(data,Fragment==0 &S_ORDER=="Scleractinia")
  
  #Add a column that indicates (1= yes, 0= no) whether the colony had a chronic disease
  scl$Chronic<-ifelse(scl$CONDITION_1 %in% c("SGA","PTR","FUG")|scl$CONDITION_2 %in% c("SGA","PTR","FUG")|scl$CONDITION_3 %in% c("SGA","PTR","FUG"),"CHRO","NONE")
  
  #Change varibles to factors
  factor_cols <- c("CONDITION_1","CONDITION_2","CONDITION_3","Chronic")
  scl[factor_cols] <- lapply(scl[factor_cols], as.factor)
  
  #collapse all general and specific cause code columns into 1 column so that we can count up # of colonies with each condition
  long <- gather(data= scl, key= CONDcat,value=CONDtype, c(CONDITION_1,CONDITION_2,CONDITION_3,Chronic), factor_key=TRUE)
  
  #convert from long to wide and fill in 0s
  rd<-dcast(long, formula=METHOD+SITEVISITID + SITE+TRANSECT+SEGMENT+ANALYST+COLONYID ~ CONDtype, value.var="CONDtype",length,fill=0)
  x<-c("CHRO");rd[x[!(x %in% colnames(rd))]] = 0 #if data does not have CHRO, add this column with 0s
  
  MDcol=c("METHOD","SITEVISITID","SITE","TRANSECT","SEGMENT","ANALYST","COLONYID")
  DATAcol=setdiff(names(rd),MDcol)
  rd.new=rd;  rd.new[,DATAcol][rd.new[,DATAcol]>1]=1  
  
  #merge data with colony level metadata and sum conditions by transect and taxoncode
  allrd3<-left_join(rd.new,survey_colony_f)
  ConditionsWeCareAbout=names(allrd3[(length(MDcol)+1):dim(rd.new)[2]])
  allrd3_l <- gather(data = allrd3, key = Cond, value = abun,
                     ConditionsWeCareAbout,
                     factor_key=TRUE) #convert wide to long format by condition
  allrd3_l$GROUP<-allrd3_l[,grouping_field]
  allrd3_lsum<-ddply(allrd3_l, .(METHOD,SITE,SITEVISITID,TRANSECT,SEGMENT,ANALYST,GROUP,Cond), #calc total colonies by taxon and condition
                     summarise,
                     CONDabun=sum(abun))
  out1<-ddply(allrd3_lsum, .(METHOD,SITE,SITEVISITID,TRANSECT,SEGMENT,ANALYST,Cond), #calc total colonies by condition
              summarise,
              CONDabun=sum(CONDabun,na.rm=T))
  out1$GROUP<-"SSSS"; out1 <- out1[c(1,2,3,4,5,6,9,7,8)] #add total colony code
  a<-subset(rbind(allrd3_lsum,out1),!Cond %in% c("NONE_G","NONE"))
  
  #Convert back to wide format
  abun<-dcast(a, formula=METHOD+SITEVISITID +SITE +TRANSECT+SEGMENT+ANALYST+GROUP~ Cond, value.var="CONDabun",sum,fill=0)
  
  uTA=unique(data[,c("METHOD","SITEVISITID","SITE","TRANSECT","SEGMENT","ANALYST","SEGAREA")])
  ab.tr<-merge(x = uTA,y = abun,by=c("METHOD","SITEVISITID","SITE","TRANSECT","SEGMENT","ANALYST"))
  #ab.tr<-join(x = uTA,y = abun,by=c("SITEVISITID","SITE","TRANSECT","SEGMENT"))#adding NA.1.....
  #Check NAs - Should be empty...
  new_DF <- sum(rowSums(is.na(ab.tr))) # should be 0
  if(new_DF > 0) {cat("WARNING:NAs in dataframe")}   
  
  #Check NAs - Should be empty...
  new_DF <- sum(rowSums(is.na(ab.tr))) # should be 0
  if(new_DF > 0) {cat("WARNING:NAs in dataframe")}   
  
  #calcualte density of each condition, for output
  out<-ab.tr
  out[ ,which(names(out)==DATAcol[1]):ncol(out)]=out[ ,which(names(out)==DATAcol[1]):ncol(out)]/out$SEGAREA # selects every row and 2nd to last columns
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(out)
}



## TRANSECT LEVEL SUMMARY FUNCTIONS #######

#This function calculates colony density at the transect scale by first calculating the total survey area (using Calc_SurveyArea_By_Transect) then calcuating colony density
Calc_ColDen_Transect<-function(data, grouping_field="GENUS_CODE"){
  #require(tidyr)
  
  data$GROUP<-data[,grouping_field] #assign a grouping field for taxa
  
  #Remove Tubastrea
  data$S_ORDER<-ifelse(data$GROUP=="TUSP",NA,as.character(data$S_ORDER))
  data$GROUP<-ifelse(data$GROUP=="TUSP","AAAA",as.character(data$GROUP))


  #Calculate # of colonies for each variable. You need to have S_ORDER and Fragment here so you can incorporate zeros properly later in the code
  a<-ddply(data, .(SITE,SITEVISITID,TRANSECT, S_ORDER,GROUP,Fragment),
           summarise,
           ColCount=length(COLONYID)) #change to count
  
  #Convert from long to wide and insert 0s for taxa that weren't found at each site. 
  #ca<-dcast(a, formula=SITE + SITEVISITID +TRANSECT +Fragment+S_ORDER~ GROUP, value.var="ColCount",fill=0)
  ca0=spread(a,key=GROUP,value=ColCount,fill=0) #Keepin' it TIDYR
  data.cols<-names(ca0[6:dim(ca0)[2]]) #define your data coloumns- note you need to index by column number not by names-you may have situations where there are no AAAA
  field.cols<-c("SITE", "SITEVISITID", "TRANSECT","Fragment") #define field columns
  
  ### Drop all fragments, but don't drop a fragment-only transect... ###
  #change colony counts for fragments to 0 so that we account for the transects that only had fragments
  ca0[which(ca0$Fragment <0), data.cols]<-0 
  
  #At this point you will have multiple rows for each site/transect so sum data by site and transect. This will help you properly insert 0s
  field.cols<-c("SITE", "SITEVISITID", "TRANSECT")
  ca1<-aggregate(ca0[,data.cols], by=ca0[,field.cols], sum) 
  rm(list='ca0')
  
  #Create a list of scleractinian taxa that are in the dataframe as columns then sum across just those taxa to get total scl
  b<-subset(data,S_ORDER=="Scleractinia");taxalist<-as.character(unique(b$GROUP))
  ca1$SSSS<-rowSums(ca1[,taxalist,drop=FALSE]) #calculate total colony density
  ca2 <- gather(ca1, GROUP, ColCount, names(ca1[4:dim(ca1)[2]]), factor_key=TRUE) #convert wide to long format
  rm(list='ca1')
  
  #Remove everything that isn't a scleractinian
  taxalist2<-c(taxalist,"SSSS")
  ca3<-ca2[ca2$GROUP %in% taxalist2,]
  rm(list='ca2')
  
  #Calculate transect area surveyed and colony density***
  #trarea<-Calc_SurveyArea_By_Transect(data)
  uTA=unique(data[,c("SITE","SITEVISITID","TRANSECT","TRANSECTAREA")])
  out<-join(uTA,ca3, by=c("SITE","SITEVISITID","TRANSECT"))
  out$ColDen<-out$ColCount/out$TRANSECTAREA
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
    return(out)
}


#Change percent to proportion
##This function calculates mean colony legnth, % recent dead, % old dead, condition severity or condition extent to the transect level
Calc_ColMetric_Transect<-function(data, grouping_field="S_ORDER",pool_fields=c("COLONYLENGTH","RDEXTENT1", "RDEXTENT2","RDEXTENT3","OLDDEAD","SEVERITY1","EXTENT1","SEVERITY2","EXTENT2","SEVERITY3","EXTENT3")){
  
  data$GROUP<-data[,grouping_field]
  
  scl<-subset(data,Fragment==0&S_ORDER=="Scleractinia" & GROUP!="TUSP") #excludes fragments and anything that isn't a hard coral
  
  scl$y <- rowSums(scl[,pool_fields,drop=FALSE], na.rm=TRUE) #this will allow you to add the 2 recent dead columns if you are looking at this metric
  
  rd<-ddply(scl, .(SITE,SITEVISITID,TRANSECT,GROUP),
            summarise,
            Ave.y=mean(y, na.rm=TRUE))
  
  rdtot<-ddply(scl, .(SITE,SITEVISITID,TRANSECT),
               summarise,
               Ave.y=mean(y, na.rm=TRUE))
  rdtot$GROUP<-"SSSS"; rdtot <- rdtot[c(1,2,3,5,4)]
  rd_wide<-dcast(rd, formula=SITE + SITEVISITID +TRANSECT~ GROUP, value.var="Ave.y",fill=0)
  rd_long <- gather(rd_wide, GROUP, Ave.y, names(rd_wide[4:dim(rd_wide)[2]]), factor_key=TRUE) #convert wide to long format
  rd_long<-rbind(rd,rdtot)
  
  colnames(rd_long)[which(colnames(rd_long) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(rd_long)
}



#Updated 1/24/20
Calc_RDden_Transect<-function(data, survey_colony_f=survey_colony, grouping_field="S_ORDER"){

  data$GROUP<-data[,grouping_field]
  
  scl<-subset(data,Fragment==0&S_ORDER=="Scleractinia" & GROUP!="TUSP") #excludes fragments and anything that isn't a hard coral
  
  #add and "_G" to the general cause code so that we distiguish it from specific cause codes
  scl$GENRD1<-paste(scl$GENRD1,"_G",sep="");scl$GENRD2<-paste(scl$GENRD2,"_G",sep="");scl$GENRD3<-paste(scl$GENRD3,"_G",sep="")
  
  #Change varibles to factors
  factor_cols <- c("GENRD1","GENRD2","GENRD3","RD1","RD2","RD3")
  scl[factor_cols] <- lapply(scl[factor_cols], as.factor)

  #collapse all general and specific cause code columns into 1 column so that we can count up # of colonies with each condition
  #this step will spit out an error message about attributes not being identical-ignore this. you did not lose data
  scl_l <- gather(data = scl, key = RDcat, value = RDtype, c(GENRD1,RD1,GENRD2,RD2,GENRD3,RD3), factor_key=TRUE)
  
  #convert from long to wide and fill in 0s
  # Tom gave up trying to make this from dcast to spread to 'keep dependencies down', maybe another day...
  rd<-dcast(scl_l, formula=SITEVISITID + SITE+TRANSECT+COLONYID ~ RDtype, value.var="RDtype",length,fill=0)
  MDcol=c("SITEVISITID","SITE","TRANSECT","COLONYID")
  DATAcol=setdiff(names(rd),MDcol)
  rd.new=rd;  rd.new[,DATAcol][rd.new[,DATAcol]>1]=1  

  #merge data with colony level metadata and sum conditions by transect and taxoncode
  allrd3<-left_join(rd.new,survey_colony_f)
  ConditionsWeCareAbout=names(allrd3[(length(MDcol)+1):dim(rd.new)[2]])
  allrd3_l <- gather(data = allrd3, key = RDCond, value = abun,
                     ConditionsWeCareAbout,
                     factor_key=TRUE) #convert wide to long format by condition
  allrd3_l$GROUP<-allrd3_l[,grouping_field]
  allrd3_lsum<-ddply(allrd3_l, .(SITE,SITEVISITID,TRANSECT,GROUP,RDCond), #calc total colonies by taxon and condition
                 summarise,
                 RDabun=sum(abun))
  out1<-ddply(allrd3_lsum, .(SITE,SITEVISITID,TRANSECT,RDCond), #calc total colonies by condition
              summarise,
              RDabun=sum(RDabun,na.rm=T))
  out1$GROUP<-"SSSS"; out1 <- out1[c(1,2,3,6,4,5)] #add total colony code
  a<-subset(rbind(allrd3_lsum,out1),!RDCond %in% c("NONE_G","NONE"))
  
  #Convert back to wide format
  abun<-dcast(a, formula=SITEVISITID +SITE + TRANSECT+GROUP~ RDCond, value.var="RDabun",sum,fill=0)
  
  #trarea<-Calc_SurveyArea_By_Transect(data) #calculate survey area/site
  uTA=unique(scl[,c("SITEVISITID","SITE","TRANSECT","TRANSECTAREA")])
  ab.tr<-left_join(x = uTA,y = abun)
  #ab.tr<-left_join(x = uTA,y = abun,by=c("SITEVISITID","SITE","TRANSECT"))
  ab.tr[is.na(ab.tr),]<-0

  #Check NAs - Should be empty...
  new_DF <- sum(rowSums(is.na(ab.tr))) # should be 0
  if(new_DF > 0) {cat("WARNING:NAs in dataframe")}   
  
  #calcualte density of each condition, for output
  out<-ab.tr
  out[ ,which(names(out)==DATAcol[1]):ncol(out)]=out[ ,which(names(out)==DATAcol[1]):ncol(out)]/out$TRANSECTAREA # selects every row and 2nd to last columns
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(out)
}



#Updated 1/24/20
Calc_CONDden_Transect<-function(data,survey_colony_f=survey_colony, grouping_field="S_ORDER"){
  
  data$GROUP<-data[,grouping_field]
  
  scl<-subset(data,Fragment==0&S_ORDER=="Scleractinia" & GROUP!="TUSP") #excludes fragments and anything that isn't a hard coral
  
  #Add a column that indicates (1= yes, 0= no) whether the colony had a chronic disease
  scl$Chronic<-ifelse(scl$CONDITION_1 %in% c("SGA","PTR","FUG")|scl$CONDITION_2 %in% c("SGA","PTR","FUG")|scl$CONDITION_3 %in% c("SGA","PTR","FUG"),"CHRO","NONE")
  
  #Change varibles to factors
  factor_cols <- c("CONDITION_1","CONDITION_2","CONDITION_3","Chronic")
  scl[factor_cols] <- lapply(scl[factor_cols], as.factor)
  
  #collapse all general and specific cause code columns into 1 column so that we can count up # of colonies with each condition
  long <- gather(data= scl, key= CONDcat,value=CONDtype, c(CONDITION_1,CONDITION_2,CONDITION_3,Chronic), factor_key=TRUE)
  
  #convert from long to wide and fill in 0s
  rd<-dcast(long, formula=SITEVISITID + SITE+TRANSECT+COLONYID ~ CONDtype, value.var="CONDtype",length,fill=0)
  x<-c("CHRO");rd[x[!(x %in% colnames(rd))]] = 0 #if data does not have CHRO, add this column with 0s
  MDcol=c("SITEVISITID","SITE","TRANSECT","COLONYID")
  DATAcol=setdiff(names(rd),MDcol)
  rd.new=rd;  rd.new[,DATAcol][rd.new[,DATAcol]>1]=1  
  
  #merge data with colony level metadata and sum conditions by transect and taxoncode
  allrd3<-left_join(rd.new,survey_colony_f)
  ConditionsWeCareAbout=names(allrd3[(length(MDcol)+1):dim(rd.new)[2]])
  allrd3_l <- gather(data = allrd3, key = Cond, value = abun,
                     ConditionsWeCareAbout,
                     factor_key=TRUE) #convert wide to long format by condition
  allrd3_l$GROUP<-allrd3_l[,grouping_field]
  allrd3_lsum<-ddply(allrd3_l, .(SITE,SITEVISITID,TRANSECT,GROUP,Cond), #calc total colonies by taxon and condition
                     summarise,
                     CONDabun=sum(abun))
  out1<-ddply(allrd3_lsum, .(SITE,SITEVISITID,TRANSECT,Cond), #calc total colonies by condition
              summarise,
              CONDabun=sum(CONDabun,na.rm=T))
  out1$GROUP<-"SSSS"; out1 <- out1[c(1,2,3,6,4,5)] #add total colony code
  a<-subset(rbind(allrd3_lsum,out1),!Cond %in% c("NONE_G","NONE"))
  
  #Convert back to wide format
  abun<-dcast(a, formula=SITEVISITID +SITE + TRANSECT+GROUP~ Cond, value.var="CONDabun",sum,fill=0)
  
  uTA=unique(scl[,c("SITEVISITID","SITE","TRANSECT","TRANSECTAREA")])
  ab.tr<-left_join(x = uTA,y = abun)
  ab.tr[is.na(ab.tr)]<-0
  
  #Check NAs - Should be empty...
  new_DF <- sum(rowSums(is.na(ab.tr))) # should be 0
  if(new_DF > 0) {cat("WARNING:NAs in dataframe")}   
  
  #calcualte density of each condition, for output
  out<-ab.tr
  out[,which(names(out)==DATAcol[1]):ncol(out)]=out[,which(names(out)==DATAcol[1]):ncol(out)]/out$TRANSECTAREA # selects every row and 2nd to last columns
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(out)
}


#Richness pooled at the strata level- no SE at strata 
#total island counts of genera
#strata richness then weight by island or region

#This function calculates hard coral richness at the transect scale by
Calc_Richness_Transect<-function(data,grouping_field="GENUS_CODE"){
  
  data$GROUP<-data[,grouping_field] #assign a grouping field for taxa){
  
  #Subset 1st 3 segments on transect 1. Remove transect 2 so that we can have a uniform plot size for calculating richness
  data<-subset(data,TRANSECT==1&SEGMENT!=7)
  
  #Calculate # of colonies for each variable. You need to have S_ORDER and Fragment here so you can incorporate zeros properly later in the code
  a<-ddply(data, .(SITE,SITEVISITID,S_ORDER),
           summarise,
          Richness=length(unique(GROUP))) #change to count
  
  a$Richness<-ifelse(a$S_ORDER!="Scleractinia",0,a$Richness)
  b<-ddply(a, .(SITE,SITEVISITID),
           summarise,
           Richness=sum(Richness)) #change to count
  
  return(b)
}


#Revising Benthic REA (demography) Sector and Strata pooling
PoolSecStrat=function(site_data){
  
  ############CORRECT THIS IN THE FUTURE TO MAKE IT MORE STREAMLINE!!!!!!!!!!!!!!!!
  
  #Create STRATANAME by idenityfing which ANALAYSIS SCHEME you want to use then concatinating with depth and reef zone that will be used to pool data
  site_data$BEN_SEC<-site_data$SEC_NAME
  
  #Changing sector pooling SAMOA
  site_data$BEN_SEC<-ifelse(site_data$OBS_YEAR=="2018"&site_data$BEN_SEC %in% c("TUT_NE_OPEN","TUT_AUNUU_A"),"TUT_NE",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$OBS_YEAR=="2018"&site_data$BEN_SEC %in% c("TUT_FAGALUA","TUT_FAGATELE"),"TUT_FAGALUA_FAGATELE",as.character(site_data$BEN_SEC))
  #site_data$BEN_SEC<-ifelse(site_data$OBS_YEAR=="2018"& site_data$ISLAND =="Tutuila","TUT",as.character(site_data$BEN_SEC)) #Dione suggested we just pool up to island for sector- don't agree
  site_data$BEN_SEC<-ifelse(site_data$OBS_YEAR=="2018"&site_data$BEN_SEC %in% c("TAU_OPEN","TAU_SANCTUARY"),"TAU",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$OBS_YEAR=="2018"&site_data$BEN_SEC %in% c("SWA_OPEN","SWA_SANCTUARY"),"SWA",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$OBS_YEAR=="2015"&site_data$BEN_SEC %in% c("TAU_OPEN","TAU_SANCTUARY"),"TAU",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$OBS_YEAR=="2015"&site_data$BEN_SEC %in% c("TUT_NE_OPEN","TUT_AUNUU_A"),"TUT_NE",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$OBS_YEAR=="2015"&site_data$BEN_SEC %in% c("TUT_SW_OPEN","TUT_FAGALUA"),"TUT_SW",as.character(site_data$BEN_SEC))
  
  #Changing sector pooling PRIA
  site_data <- site_data[!(site_data$OBS_YEAR == "2018" & site_data$ISLAND=="Kingman" & site_data$REEF_ZONE=="Backreef"),] 
  
  
  #Changing sector pooling structure for GUAM & CNMI
  site_data <- site_data[!(site_data$ISLAND=="Maug" & site_data$REEF_ZONE=="Lagoon"),] 
  site_data$BEN_SEC<-ifelse(site_data$SEC_NAME %in% c("GUA_ACHANG","GUA_EAST_OPEN","GUA_PATI_POINT"),"GUAEAALL",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$SEC_NAME %in% c("GUA_HARBOR","GUA_WEST_OPEN","GUA_PITI_BOMB","GUA_SASA_BAY","GUA_TUMON"),"GUAWEALL",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$SEC_NAME %in% c("Guguan", "Alamagan", "Sarigan"),"AGS",as.character(site_data$BEN_SEC))
  site_data$ISLAND<-ifelse(site_data$SEC_NAME %in% c("Guguan", "Alamagan", "Sarigan"),"AGS",as.character(site_data$ISLAND))
  
  #Changing sector pooling structure for MHI- did not combine other islands and structure together because they did not have more than 1 sector (e.g. HAW_KONA_CR is the only coral rich sector on hawaii island)
  site_data$BEN_SEC<-ifelse(site_data$ISLANDCODE =="HAW"& site_data$bGEN_MHI_STRUCTURE=="CM","HAW_CM",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$ISLANDCODE =="MAI"& site_data$bGEN_MHI_STRUCTURE=="CM","MAI_CM",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$ISLANDCODE =="MAI"& site_data$bGEN_MHI_STRUCTURE=="SI","MAI_SI",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$ISLANDCODE =="MOL"& site_data$bGEN_MHI_STRUCTURE=="SI","MOL_SI",as.character(site_data$BEN_SEC))
  site_data$BEN_SEC<-ifelse(site_data$ISLANDCODE =="NII"& site_data$bGEN_MHI_STRUCTURE=="SI","NII_SI",as.character(site_data$BEN_SEC))
  
  
  #Specific Changes to depth bin and reef zone
  site_data <- site_data[!(site_data$ISLAND=="Johnston" & site_data$REEF_ZONE=="Lagoon"),] 
  site_data$REEF_ZONE<-ifelse(site_data$REEF_ZONE %in% c("Protected Slope","Forereef"),"Forereef",as.character(site_data$REEF_ZONE))
  site_data$DEPTH_BIN<-ifelse(site_data$ISLAND =="Kingman" & site_data$REEF_ZONE=="Lagoon","ALL",as.character(site_data$DEPTH_BIN))
  site_data$DEPTH_BIN<-ifelse(site_data$OBS_YEAR=="2015" & site_data$ISLAND =="Rose" & site_data$REEF_ZONE=="Backreef","ALL",as.character(site_data$DEPTH_BIN))
  
  #Create Strataname
  site_data$DB_RZ<-paste(substring(site_data$REEF_ZONE,1,1), substring(site_data$DEPTH_BIN,1,1), sep="")
  site_data$STRATANAME=paste0(site_data$BEN_SEC,"_",site_data$DB_RZ)
  
  #Changing STRATA pooling structure for SAMOA
  # site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2015"&site_data$STRATANAME %in% c("TUT_AUNUU_B_FM","TUT_AUNUU_B_FS"),"TUT_AUNUU_B_FMS",as.character(site_data$STRATANAME))
  # site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2015"&site_data$BEN_SEC=="ROS_SANCTUARY"&site_data$DB_RZ %in% c("FM","FD"), "ROS_FMD",as.character(site_data$STRATANAME))
  # site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2018"&site_data$STRATANAME %in% c("TUT_NE_FM","TUT_NE_FS"),"TUT_NE_FMS",as.character(site_data$STRATANAME))
  # site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2018"&site_data$STRATANAME %in% c("TUT_SW_FD","TUT_SW_FM"),"TUT_SW_FDM",as.character(site_data$STRATANAME))
  # 
  # #Changing STRATA pooling structure for PRIA
  # site_data$STRATANAME<-ifelse(site_data$BEN_SEC=="Johnston"&site_data$DB_RZ %in% c("BM","BD"), "Johnston_BMD",as.character(site_data$STRATANAME))
  # site_data$STRATANAME<-ifelse(site_data$BEN_SEC=="Johnston"&site_data$DB_RZ %in% c("FM","FS"), "Johnston_FMS",as.character(site_data$STRATANAME))
  site_data$STRATANAME<-ifelse(site_data$BEN_SEC=="Johnston"&site_data$REEF_ZONE =="Lagoon", "Johnston_LA",as.character(site_data$STRATANAME))
  
  # #Changing STRATA pooling structure for Guam and CNMI
  # site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2014"&site_data$STRATANAME %in% c("Aguijan_FD","Aguijan_FM"),"Aguijan_FMD",as.character(site_data$STRATANAME))
  # 
  # #Changing STRATA pooling structure for MHI
  # site_data$STRATANAME<-ifelse(site_data$STRATANAME %in% c("LAN_NORTH_FD","LAN_NORTH_FM"),"LAN_NORTH_FMD",as.character(site_data$STRATANAME))
  # site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2016"& site_data$BEN_SEC=="NII_SI"&site_data$DB_RZ %in% c("FM","FD"), "NII_SI_FMD",as.character(site_data$STRATANAME))
  # site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2016"& site_data$BEN_SEC=="OAH_NE"&site_data$DB_RZ %in% c("FM","FD"), "OAH_NE_FMD",as.character(site_data$STRATANAME))
  # site_data$STRATANAME<-ifelse(site_data$BEN_SEC=="KAH_SO"&site_data$DB_RZ %in% c("FM","FD"), "KAH_SO_FMD",as.character(site_data$STRATANAME))
  # site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2013"&site_data$BEN_SEC=="MAI_CM"&site_data$DB_RZ %in% c("FM","FD"), "MAI_CM_FMD",as.character(site_data$STRATANAME))
  # 
  # site_data$STRATANAME<-ifelse(site_data$BEN_SEC=="MAI_SI", "MAI_SI_A",as.character(site_data$STRATANAME))
  # site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2016"&site_data$BEN_SEC=="MOL_PALI", "MOL_PALI_A",as.character(site_data$STRATANAME))
  # site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2016"&site_data$BEN_SEC=="MOL_SOUTH", "MOL_SOUTH_ALL",as.character(site_data$STRATANAME))
  # site_data$STRATANAME<-ifelse(site_data$OBS_YEAR=="2016"&site_data$BEN_SEC=="MOL_SI"&site_data$DB_RZ %in% c("FM","FD"), "MOL_SI_FMD",as.character(site_data$STRATANAME))
  # 
  # #Changing STRATA pooling structure for NWHI
  # site_data$STRATANAME<-ifelse(site_data$OBS_YEAR %in% c("2014","2015")& site_data$BEN_SEC=="Laysan"&site_data$DB_RZ %in% c("FM","FS"), "Laysan_FMS",as.character(site_data$STRATANAME))
  # site_data$STRATANAME<-ifelse(site_data$OBS_YEAR %in% c("2014","2015")& site_data$BEN_SEC=="Maro"&site_data$DB_RZ %in% c("FM","FD"), "Maro_FMD",as.character(site_data$STRATANAME))
  # site_data$STRATANAME<-ifelse(site_data$OBS_YEAR %in% c("2014","2015")& site_data$BEN_SEC=="Pearl & Hermes"&site_data$DB_RZ %in% c("FM","FS"), "Pearl & Hermes_FMS",as.character(site_data$STRATANAME))
  
  #Create a new depth bin column that has the combined depths
  site_data$DB<-site_data$DB_RZ
  removeRZ<-"(B|F|L)"
  site_data$DB<-gsub(removeRZ, "", site_data$DB)
  
  
  #Modify Analysis Year
  site_data$ANALYSIS_YEAR<-site_data$OBS_YEAR
  site_data$OBS_YEAR<-ifelse(site_data$OBS_YEAR %in% c("2014","2015")& site_data$REGION=="NWHI", "2014-15",as.character(site_data$OBS_YEAR))
  
  return(site_data)
}




## POOLING FUNCTIONS TO GENERATE ESTIMATES #######

#STRATA ROLL UP FUNCTION-This function calculates mean, var, SE and CV at the strata level. I've built in flexilbity to use either genus or taxoncode
# You can input any metric you would like (eg. adult density, mean % old dead,etc). Note that for any metric that does not involve density of colonies, 
# Y._h (total colony abundance in stratum),varY._h (variance in total abundance), SE_Y._h and CV_Y._h are meaningless-DO NOT USE
#Note: for whatever reason, the grouping, other and metric fields need to be in this order. If you don't want to include an other field then add "DUMMY" as the second variable when you are running this function.
#e.g. st<-Calc_Strata(data.mon,"GENUS_CODE","DUMMY","ColDen")

Calc_Strata=function(site_data,grouping_field,metric_field,pres.abs_field="Adpres.abs",M_hi=250){
  
  #Build in flexibility to look at genus or taxon level
  site_data$GROUP<-site_data[,grouping_field]
  
  #Build in flexibility to summarized different metrics
  site_data$METRIC<-site_data[,metric_field]
  site_data$METRIC<-as.numeric(site_data$METRIC)
  
  site_data$PRES.ABS<-site_data[,pres.abs_field]
  
  #For a Given ANALYSIS_SCHEMA, we need to pool N_h, and generate w_h
  strat.temp<-ddply(subset(site_data,GROUP=="SSSS"),.(REGION,ISLAND,ANALYSIS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA,NH),summarize,temp=sum(NH,na.rm=TRUE)) #calculate # of possible sites in a given stratum
  Strata_NH<-ddply(strat.temp,.(REGION,ISLAND,ANALYSIS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA),summarize,N_h.as=sum(NH,na.rm=TRUE)) #calculate # of possible sites in a given stratum
  Dom_NH<-ddply(Strata_NH,.(REGION,ISLAND,ANALYSIS_YEAR,DOMAIN_SCHEMA),summarize,Dom_N_h=sum(N_h.as,na.rm=TRUE))#calculate # of possible sites in a given domain, use this to calculate weighting factor
  Strata_NH<-left_join(Strata_NH,Dom_NH) #add Dom_N_h into Strata_NH df
  Strata_NH$w_h.as<-Strata_NH$N_h.as/Strata_NH$Dom_N_h # add schema weighting factor to schema dataframe
  
  #Now add back the Analysis_Schema Dom_N_h, Nh and wh to site_data - can't use match because we need to merge based on analysis scheme and analysis year
  site_data<-left_join(site_data,Strata_NH)
  
  #Calculate summary metrics at the stratum level (rolled up from site level)
  Strata_roll=ddply(site_data,.(REGION,ISLAND,ANALYSIS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA,REEF_ZONE,DB_RZ,GROUP,Dom_N_h),summarize,
                    n_h=length(SITE),# No. of Sites surveyed in a Strata
                    N_h=median(N_h.as,na.rm=T),# Strata Area (as N 50x50 grids) - median allows you to pick 1 value
                    w_h=median(w_h.as,na.rm=T),# weigting factor for a given stratum- median allows you to pick 1 value
                    D._h=mean(METRIC,na.rm=T), # Mean of Site-Level metric in a Stratum
                    S1_h=var(METRIC,na.rm=T), #sample variance in metric between sites
                    varD._h=(1-(n_h/N_h))*S1_h/n_h, #Strata level  variance of mean density
                    nmtot=(N_h*250), #total possible area
                    th=10, #minimum sampling unit
                    Y._h=D._h*nmtot*th,#total colony abundance in stratum **corrected using diones code
                    varY._h=((nmtot^2)*varD._h*(th^2)), #variance in total abundance- corrected using diones code
                    SE_D._h=sqrt(varD._h),
                    CV_D._h=(SE_D._h/D._h)*100,
                    SE_Y._h=sqrt(varY._h),
                    CV_Y._h=(SE_Y._h/Y._h)*100,
                    avp=sum(PRES.ABS)/n_h,
                    var_prop=(n_h/(n_h-1)*avp*(1-avp)),
                    SEprop=sqrt(var_prop))
  
  Strata_roll$M_hi=250 #define total possible transects in a site
  Strata_roll=Strata_roll[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ","GROUP",
                             "M_hi","n_h","N_h","w_h",
                             "D._h","S1_h","varD._h","SE_D._h","CV_D._h",
                             "Y._h","varY._h","SE_Y._h","CV_Y._h","avp","var_prop","SEprop")]
  
  #remove strata that have only 1 site because you can't calculate variance
  Strata_roll<-Strata_roll[Strata_roll$n_h>1,]
  
  colnames(Strata_roll)[which(colnames(Strata_roll) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  colnames(Strata_roll)[which(colnames(Strata_roll) == 'prop.occur')] <- pres.abs_field #change group to whatever your grouping field is.
  
  
  return(Strata_roll)
}

#This function is very similar to Calc_Strata, but calculates the Domain NH by adding up all possible strata in a domain rather than just the ones that were sampled to calculate 
#total domain area and strata weights
#This is an older function so make sure it's up to date with changes that have been made to rest of script 

# Calc_Analysis_Strata=function(site_data,sec,grouping_field,metric_field,pres.abs_field="Adpres.abs",M_hi=250){
#   
#   #Build in flexibility to look at genus or taxon level
#   site_data$GROUP<-site_data[,grouping_field]
#   
#   #Build in flexibility to summarized different metrics
#   site_data$METRIC<-site_data[,metric_field]
#   site_data$METRIC<-as.numeric(site_data$METRIC)
#   
#   site_data$PRES.ABS<-site_data[,pres.abs_field]
#   
#   #For a Given ANALYSIS_SCHEMA, we need to pool N_h, and generate w_h
#   strat.temp<-ddply(subset(site_data,GROUP=="SSSS"),.(REGION,ISLAND,ANALYSIS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA,NH),summarize,temp=sum(NH,na.rm=TRUE)) #calculate # of possible sites in a given stratum
#   Strata_NH<-ddply(strat.temp,.(REGION,ISLAND,ANALYSIS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA),summarize,N_h=sum(NH,na.rm=TRUE)) #calculate # of possible sites in a given stratum
#   Dom_NH<-ddply(sec,.(DOMAIN_SCHEMA),summarize,Dom_N_h=sum(NH,na.rm=TRUE))#calculate # of possible sites in a given domain (entire domain even if you didn't sample some of the strata), use this to calculate weighting factor
#   Strata_NH$Dom_N_h<-Dom_NH$Dom_N_h[match(Strata_NH$DOMAIN_SCHEMA,Dom_NH$DOMAIN_SCHEMA)]# add Dom_N_h to schema dataframe
#   Strata_NH$w_h<-Strata_NH$N_h/Strata_NH$Dom_N_h # add schema weighting factor to schema dataframe
#   
#   #Now add back the Analysis_Schema Nh and wh to site_data
#   site_data$N_h.as<-Strata_NH$N_h[match(site_data$ANALYSIS_SCHEMA,Strata_NH$ANALYSIS_SCHEMA)]
#   site_data$w_h.as<-Strata_NH$w_h[match(site_data$ANALYSIS_SCHEMA,Strata_NH$ANALYSIS_SCHEMA)]
#   site_data$Dom_N_h<-Strata_NH$Dom_N_h[match(site_data$ANALYSIS_SCHEMA,Strata_NH$ANALYSIS_SCHEMA)]
#   
#   #Calculate summary metrics at the stratum level (rolled up from site level)
#   Strata_roll=ddply(site_data,.(REGION,ISLAND,ANALYSIS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA,GROUP,Dom_N_h),summarize,
#                     n_h=length(SITE),# No. of Sites surveyed in a Strata
#                     N_h=median(N_h.as,na.rm=T),# Strata Area (as N 50x50 grids) - median allows you to pick 1 value
#                     w_h=median(w_h.as,na.rm=T),# weigting factor for a given stratum- median allows you to pick 1 value
#                     D._h=mean(METRIC,na.rm=T), # Mean of Site-Level metric in a Stratum
#                     S1_h=var(METRIC,na.rm=T), #sample variance in metric between sites
#                     varD._h=(1-(n_h/N_h))*S1_h/n_h, #Strata level  variance of mean density
#                     nmtot=(N_h*250), #total possible area
#                     th=10, #minimum sampling unit
#                     Y._h=D._h*nmtot*th,#total colony abundance in stratum **corrected using diones code
#                     varY._h=((nmtot^2)*varD._h*(th^2)), #variance in total abundance- corrected using diones code
#                     SE_D._h=sqrt(varD._h),
#                     CV_D._h=(SE_D._h/D._h)*100,
#                     SE_Y._h=sqrt(varY._h),
#                     CV_Y._h=(SE_Y._h/Y._h)*100,
#                     avp=sum(PRES.ABS)/n_h,
#                     var_prop=(n_h/(n_h-1)*avp*(1-avp)),
#                     SEprop=sqrt(var_prop))
#   
#   Strata_roll$M_hi=250 #define total possible transects in a site
#   Strata_roll=Strata_roll[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","GROUP","Dom_N_h",
#                              "M_hi","n_h","N_h","w_h",
#                              "D._h","S1_h","varD._h","SE_D._h","CV_D._h",
#                              "Y._h","varY._h","SE_Y._h","CV_Y._h","avp","var_prop","SEprop")]
#   
#   #remove strata that have only 1 site because you can't calculate variance
#   Strata_roll<-Strata_roll[Strata_roll$n_h>1,]
#   
#   colnames(Strata_roll)[which(colnames(Strata_roll) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
#   colnames(Strata_roll)[which(colnames(Strata_roll) == 'prop.occur')] <- pres.abs_field #change group to whatever your grouping field is.
#   
#   
#   return(Strata_roll)
# }


#DOMAIN ROLL UP FUNCTION-This function calculates mean, var, SE and CV at the DOMAIN level. I've built in flexilbity to use either genus or taxoncode as well as other metrics (size class, morph)
# You can input any metric you would like (eg. adult density, mean % old dead,etc). Note that for any metric that does not involve density of colonies, 
# Y._h (total colony abundance in stratum),varY._h (variance in total abundance), SE_Y._h and CV_Y._h are meaningless-DO NOT USE
Calc_Domain=function(site_data,grouping_field="S_ORDER",metric_field,pres.abs_field="Adpres.abs"){
  
  Strata_data=Calc_Strata(site_data,grouping_field,metric_field,pres.abs_field)
  
  #Build in flexibility to look at genus or taxon level
  Strata_data$GROUP<-Strata_data[,grouping_field]

  DomainStr_NH=ddply(subset(Strata_data,GROUP=="SSSS"),.(REGION,ANALYSIS_YEAR,DOMAIN_SCHEMA),summarize,DomainSumN_h=sum(N_h,na.rm=TRUE)) #total possible sites in a domain
  Strata_data<-left_join(Strata_data, DomainStr_NH)# add previous to strata data
  Strata_data$w_h=Strata_data$N_h/Strata_data$DomainSumN_h
  
  Domain_roll=ddply(Strata_data,.(REGION,ANALYSIS_YEAR,ISLAND,DOMAIN_SCHEMA,GROUP),summarize,
                    D._st=sum(w_h*D._h,na.rm=TRUE), #Domain weighted estimate (sum of Weighted strata density)
                    varD._st=sum(w_h^2*varD._h,na.rm=TRUE), #Domain weighted variance estimate
                    Y._st=sum(Y._h,na.rm=TRUE), #Domain total abundance (sum of extrapolated strata abundance)
                    varY._st=sum(varY._h,na.rm=TRUE),#Domain variance total abundance (sum of extrapolated strata varaiance abundance)
                    n=sum(n_h,na.rm=TRUE), #total sites surveyed in domain
                    Ntot=sum(N_h,na.rm=TRUE), #total possible sites in domain
                    SE_D._st=sqrt(varD._st), #SE of domain metric estimate
                    CV_D._st=(SE_D._st/D._st)*100, #CV of domain metric estimate
                    SE_Y._st=sqrt(varY._st),#SE of domain abundance estimate
                    CV_Y._st=(SE_Y._st/Y._st)*100,#CV of domain abundnace estimate
                    po._st=sum(w_h*avp,na.rm=TRUE), #Domain weighted estimate 
                    varpo._st=sum(w_h^2*var_prop,na.rm=TRUE), #Domain weighted variance estimate
                    SE_po._st=sqrt(varpo._st), #SE of domain metric estimate
                    CV_po._st=SE_po._st/po._st) #CV of domain metric estimate
                    
#need to double check calculations with Dione
  # Domain_roll=Domain_roll[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GROUP",
  #                            "n","Ntot","D._st","SE_D._st")]
  # 
  colnames(Domain_roll)[which(colnames(Domain_roll) == 'D._st')] <- paste0("Mean","_",metric_field) 
  colnames(Domain_roll)[which(colnames(Domain_roll) == 'SE_D._st')] <- paste0("SE","_",metric_field) 
  colnames(Domain_roll)[which(colnames(Domain_roll) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  
  return(Domain_roll)
}

# Calc_Analysis_Domain=function(site_data,sec,grouping_field="S_ORDER",metric_field,pres.abs_field="Adpres.abs"){
#   
#   Strata_data=Calc_Analysis_Strata(site_data,sec,grouping_field,metric_field,pres.abs_field)
#   
#   #Build in flexibility to look at genus or taxon level
#   Strata_data$GROUP<-Strata_data[,grouping_field]
#   
#   Domain_roll=ddply(Strata_data,.(ANALYSIS_YEAR,DOMAIN_SCHEMA,GROUP),summarize,
#                     D._st=sum(w_h*D._h,na.rm=TRUE), #Domain weighted estimate (sum of Weighted strata density)
#                     varD._st=sum(w_h^2*varD._h,na.rm=TRUE), #Domain weighted variance estimate
#                     Y._st=sum(Y._h,na.rm=TRUE), #Domain total abundance (sum of extrapolated strata abundance)
#                     varY._st=sum(varY._h,na.rm=TRUE),#Domain variance total abundance (sum of extrapolated strata varaiance abundance)
#                     n=sum(n_h,na.rm=TRUE), #total sites surveyed in domain
#                     Ntot=median(Dom_N_h),
#                     SE_D._st=sqrt(varD._st), #SE of domain metric estimate
#                     CV_D._st=(SE_D._st/D._st)*100, #CV of domain metric estimate
#                     SE_Y._st=sqrt(varY._st),#SE of domain abundance estimate
#                     CV_Y._st=(SE_Y._st/Y._st)*100,#CV of domain abundnace estimate
#                     po._st=sum(w_h*avp,na.rm=TRUE), #Domain weighted estimate 
#                     varpo._st=sum(w_h^2*var_prop,na.rm=TRUE), #Domain weighted variance estimate
#                     SE_po._st=sqrt(varpo._st), #SE of domain metric estimate
#                     CV_po._st=SE_po._st/po._st) #CV of domain metric estimate
#   
#   #need to double check calculations with Dione
#   # Domain_roll=Domain_roll[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GROUP",
#   #                            "n","Ntot","D._st","SE_D._st")]
#   # 
#   colnames(Domain_roll)[which(colnames(Domain_roll) == 'D._st')] <- paste0("Mean","_",metric_field) 
#   colnames(Domain_roll)[which(colnames(Domain_roll) == 'SE_D._st')] <- paste0("SE","_",metric_field) 
#   colnames(Domain_roll)[which(colnames(Domain_roll) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
#   
#   
#   return(Domain_roll)
# }
# 

###POOLING FUNCTIONS FOR COVER and Richness DATA----
#need to eventually modify this code to summarize tier 2 and 3 data
Calc_Strata_Cover_Rich=function(site_data,metric_field=c("CORAL","CCA","MA","TURF","Richness"),M_hi=250){
  
  #Build in flexibility to summarized different metrics
  site_data$METRIC<-site_data[,metric_field]
  site_data$METRIC<-as.numeric(site_data$METRIC)
  
  #For a Given ANALYSIS_SCHEMA, we need to pool N_h, and generate w_h
  strat.temp<-ddply(site_data,.(REGION,ISLAND,ANALYSIS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA,NH),summarize,temp=sum(NH,na.rm=TRUE)) #calculate # of possible sites in a given stratum
  Strata_NH<-ddply(strat.temp,.(REGION,ISLAND,ANALYSIS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA),summarize,N_h=sum(NH,na.rm=TRUE)) #calculate # of possible sites in a given stratum
  Dom_NH<-ddply(Strata_NH,.(REGION,ISLAND,ANALYSIS_YEAR,DOMAIN_SCHEMA),summarize,Dom_N_h=sum(N_h,na.rm=TRUE))#calculate # of possible sites in a given domain
  Strata_NH$Dom_N_h<-Dom_NH$Dom_N_h[match(Strata_NH$DOMAIN_SCHEMA,Dom_NH$DOMAIN_SCHEMA)]# add Dom_N_h to schema dataframe
  Strata_NH$w_h<-Strata_NH$N_h/Strata_NH$Dom_N_h # add schema weighting factor to schema dataframe
  
  #Now add back the Analysis_Schema Nh and wh to site_data
  site_data$N_h.as<-Strata_NH$N_h[match(site_data$ANALYSIS_SCHEMA,Strata_NH$ANALYSIS_SCHEMA)]
  site_data$w_h.as<-Strata_NH$w_h[match(site_data$ANALYSIS_SCHEMA,Strata_NH$ANALYSIS_SCHEMA)]
  
  #Calculate summary metrics at the stratum level (rolled up from site level)
  Strata_roll=ddply(site_data,.(REGION,ISLAND,ANALYSIS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA,REEF_ZONE,DB_RZ),summarize,
                    n_h=length(SITE),# No. of Sites surveyed in a Strata
                    N_h=median(N_h.as,na.rm=T),# Strata Area (as N 50x50 grids) - median allows you to pick 1 value
                    w_h=median(w_h.as,na.rm=T),# weigting factor for a given stratum- median allows you to pick 1 value
                    D._h=mean(METRIC,na.rm=T), # Mean of Site-Level metric in a Stratum
                    S1_h=var(METRIC,na.rm=T), #sample variance in metric between sites
                    varD._h=(1-(n_h/N_h))*S1_h/n_h, #Strata level  variance of mean density
                    SE_D._h=sqrt(varD._h),
                    CV_D._h=SE_D._h/D._h)
  
  Strata_roll$M_hi=250 #define total possible transects in a site
  Strata_roll=Strata_roll[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","REEF_ZONE","DB_RZ",
                             "M_hi","n_h","N_h","w_h",
                             "D._h","S1_h","varD._h","SE_D._h","CV_D._h")]
  
  #remove strata that have only 1 site because you can't calculate variance
  Strata_roll<-Strata_roll[Strata_roll$n_h>1,]
  
  return(Strata_roll)
}


Calc_Domain_Cover_Rich=function(site_data,metric_field=c("CORAL","CCA","MA","TURF","Richness")){
  
  Strata_data=Calc_Strata_Cover_Rich(site_data,metric_field)
  
  DomainStr_NH=ddply(Strata_data,GROUP=="SSSS",.(ANALYSIS_YEAR,DOMAIN_SCHEMA),summarize,DomainSumN_h=sum(N_h,na.rm=TRUE)) #total possible sites in a domain
  Strata_data<-merge(Strata_data, DomainStr_NH,by=c("ANALYSIS_YEAR","DOMAIN_SCHEMA"))# add previous to strata data
  Strata_data$w_h=Strata_data$N_h/Strata_data$DomainSumN_h
  
  Domain_roll=ddply(Strata_data,.(REGION,ISLAND,ANALYSIS_YEAR,DOMAIN_SCHEMA),summarize,
                    D._st=sum(w_h*D._h,na.rm=TRUE), #Domain weighted estimate (sum of Weighted strata density)
                    varD._st=sum(w_h^2*varD._h,na.rm=TRUE), #Domain weighted variance estimate
                    n=sum(n_h,na.rm=TRUE), #total sites surveyed in domain
                    nstrat=length(n_h), #total number of strata in a domain
                    Ntot=sum(N_h,na.rm=TRUE), #total possible sites in domain
                    SE_D._st=sqrt(varD._st), #SE of domain metric estimate
                    CV_D._st=SE_D._st/D._st) #CV of domain metric estimate
  
  Domain_roll=Domain_roll[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA",
                             "n","Ntot","D._st","SE_D._st")]
  
  colnames(Domain_roll)[which(colnames(Domain_roll) == 'D._st')] <- paste0("Mean","_",metric_field) 
  colnames(Domain_roll)[which(colnames(Domain_roll) == 'SE_D._st')] <- paste0("SE","_",metric_field) 
  
  
  return(Domain_roll)
}


###POOLING FUNCTIONS FOR PREVALENCE DATA----

#STRATA ROLL UP FUNCTION-This function calculates mean, var, SE and CV at the strata level. I've built in flexilbity to use either genus or taxoncode
# You can input any RD cause or condition metric you would like . 

Calc_Strata_Prevalence=function(site_data,grouping_field,metric_field){
  
  #Build in flexibility to look at genus or taxon level
  site_data$GROUP<-site_data[,grouping_field]
  
  #Build in flexibility to summarized different metrics
  site_data$METRIC<-site_data[,metric_field]
  site_data$METRIC<-as.numeric(site_data$METRIC)
  
  #For a Given ANALYSIS_SCHEMA, we need to pool N_h, and generate w_h
  strat.temp<-ddply(subset(site_data,GROUP=="SSSS"),.(REGION,ISLAND,ANALYSIS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA,NH),summarize,temp=sum(NH,na.rm=TRUE)) #calculate # of possible sites in a given stratum
  Strata_NH<-ddply(strat.temp,.(REGION,ISLAND,ANALYSIS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA),summarize,N_h.as=sum(NH,na.rm=TRUE)) #calculate # of possible sites in a given stratum
  Dom_NH<-ddply(Strata_NH,.(REGION,ISLAND,ANALYSIS_YEAR,DOMAIN_SCHEMA),summarize,Dom_N_h=sum(N_h.as,na.rm=TRUE))#calculate # of possible sites in a given domain, use this to calculate weighting factor
  Strata_NH<-left_join(Strata_NH,Dom_NH) #add Dom_N_h into Strata_NH df
  Strata_NH$w_h.as<-Strata_NH$N_h.as/Strata_NH$Dom_N_h # add schema weighting factor to schema dataframe
  
  #Now add back the Analysis_Schema Dom_N_h, Nh and wh to site_data - can't use match because we need to merge based on analysis scheme and analysis year
  site_data<-left_join(site_data,Strata_NH)

  #Calculate summary metrics at the stratum level (rolled up from site level)
  Strata_roll=ddply(site_data,.(REGION,ISLAND,ANALYSIS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA,REEF_ZONE,DB_RZ,GROUP),summarize,
                    n_h=length(SITE),# No. of Sites surveyed in a Strata
                    N_h=median(N_h.as,na.rm=T),# Strata Area (as N 50x50 grids) - median allows you to pick 1 value
                    w_h=median(w_h.as,na.rm=T),# weigting factor for a given stratum- median allows you to pick 1 value
                    C_h=mean(METRIC,na.rm=T), # Mean density colonies with specific RD cause or condition in a Stratum
                    S1C_h=var(METRIC,na.rm=T), #sample variance in RD cause or condition density between sites
                    varC_h=(1-(n_h/N_h))*S1C_h/n_h, #Strata level  variance of mean condition density
                    nmtot=(N_h*250), #total possible area
                    th=10, #minimum sampling unit
                    C_abun_h=C_h*nmtot*th, # abundance of colonies with a condition in stratum 
                    varC_abun_h=((nmtot^2)*varC_h*(th^2)), #variance in total abundance of condition
                    SE_C_abun_h=sqrt(varC_abun_h),#SE of total abundance of condition
                    acd_h=mean(AdColDen,na.rm=T), # Mean of Site-Level all colonies in a Stratum
                    acd_abun_h=acd_h*nmtot*th, #strata-level abundnace of all colonies
                    prev=(C_abun_h/acd_abun_h)*100, # prevalence of condition at stratum level
                    SEprev=(SE_C_abun_h/acd_abun_h)*100,#SE of condition at stratum level 
                    CVprev=(SEprev/prev)*100) #CV of prevalence
  
  Strata_roll$M_hi=250 #define total possible transects in a site
  Strata_roll=Strata_roll[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","GROUP","REEF_ZONE","DB_RZ",
                             "M_hi","n_h","N_h","w_h","C_h","acd_h","varC_h","C_abun_h","varC_abun_h","acd_h","acd_abun_h",
                             "prev","SEprev")]
  
  #remove strata that have only 1 site because you can't calculate variance
  Strata_roll<-Strata_roll[Strata_roll$n_h>1,]
  
  colnames(Strata_roll)[which(colnames(Strata_roll) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  
  return(Strata_roll)
}


#DOMAIN ROLL UP FUNCTION-This function calculates mean, var, SE and CV at the DOMAIN level. I've built in flexilbity to use either genus or taxoncode as well as other metrics (size class, morph)
# You can input any metric you would like (eg. adult density, mean % old dead,etc). Note that for any metric that does not involve density of colonies, 
# Y._h (total colony abundance in stratum),varY._h (variance in total abundance), SE_Y._h and CV_Y._h are meaningless-DO NOT USE
Calc_Domain_Prevalence=function(site_data,grouping_field="S_ORDER",metric_field){
  
  Strata_data=Calc_Strata_Prevalence(site_data,grouping_field,metric_field)
  
  #Build in flexibility to look at genus or taxon level
  Strata_data$GROUP<-Strata_data[,grouping_field]
  
  DomainStr_NH=ddply(subset(Strata_data,GROUP=="SSSS"),.(REGION,ANALYSIS_YEAR,DOMAIN_SCHEMA),summarize,DomainSumN_h=sum(N_h,na.rm=TRUE)) #total possible sites in a domain
  Strata_data<-left_join(Strata_data, DomainStr_NH)# add previous to strata data
  Strata_data$w_h=Strata_data$N_h/Strata_data$DomainSumN_h
  
  Domain_roll=ddply(Strata_data,.(REGION,ISLAND,ANALYSIS_YEAR,DOMAIN_SCHEMA,GROUP),summarize,
                    C_st=sum(w_h*C_h,na.rm=TRUE), #Domain weighted estimate (sum of Weighted strata density)
                    varC_st=sum(w_h^2*varC_h,na.rm=TRUE), #Domain weighted variance estimate
                    C_abun_st=sum(C_abun_h,na.rm=TRUE), #Domain total abundance of colonies with a given condition (sum of extrapolated strata abundance)
                    varC_abun_st=sum(varC_abun_h,na.rm=TRUE),#Domain variance total abundance of colonies with a given condition (sum of extrapolated strata varaiance abundance)
                    n=sum(n_h,na.rm=TRUE), #total sites surveyed in domain
                    Ntot=sum(N_h,na.rm=TRUE), #total possible sites in domain
                    SE_varC_st=sqrt(varC_st), #SE of domain metric estimate
                    CV_varC_st=SE_varC_st/C_st, #CV of domain metric estimate
                    SE_varC_abun_st=sqrt(varC_abun_st),#SE of domain abundance estimate
                    CV_varC_abun_st=SE_varC_abun_st/C_abun_st,#CV of domain abundnace estimate
                    acd_st=sum(w_h*acd_h,na.rm=TRUE), # sum of all colony densities across all strata in a given domain
                    acd_abun_st=sum(acd_abun_h,na.rm=TRUE), #domain abundnace of all colonies
                    prev=(C_abun_st/acd_abun_st)*100, # prevalence of condition at domain level
                    SEprev=(SE_varC_abun_st/acd_abun_st)*100,#SE of condition at domain level 
                    CVprev=SEprev/prev) #CV of prevalence
  
  Domain_roll=Domain_roll[,c("REGION","ISLAND","ANALYSIS_YEAR","DOMAIN_SCHEMA","GROUP",
                             "n","Ntot","prev","SEprev")]
  
  colnames(Domain_roll)[which(colnames(Domain_roll) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  colnames(Domain_roll)[which(colnames(Domain_roll) == 'prev')] <- paste0("Mean","_",metric_field,"_Prev") 
  colnames(Domain_roll)[which(colnames(Domain_roll) == 'SEprev')] <- paste0("SE","_",metric_field,"_Prev") 

  return(Domain_roll)
}





####
#BSR-UNWEIGHTED FUNCTIONS####
####


Calc_Sitemetrics_BSR<-function(data, grouping_field){
  a<-merge(data,survey_transect,by=c("SITE","SITEVISITID","TRANSECT"))
  a$GROUP<-a[,grouping_field]
  out<-ddply(a, .(OBS_YEAR,REGION_NAME,ISLAND,SITE,LATITUDE,LONGITUDE,REEF_ZONE,DEPTH_BIN,SITE_MAX_DEPTH,GROUP),
             summarise,
             ACD=mean(AdColDen,na.rm = TRUE),
             JCD=mean(JuvColDen,na.rm = TRUE),
             BLE=mean(BLEprev,na.rm = TRUE),
             COTS=mean(COTSprev,na.rm=TRUE),
             DZ=mean(TotDZprev,na.rm = TRUE),
             AcuteDZ=mean(AcuteDZprev,na.rm = TRUE),
             ChrDZ=mean(ChronicDZprev,na.rm = TRUE))
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(out)
}
# 
Calc_Islmetrics_BSR<-function(data, grouping_field="GENUS_CODE"){
  data$GROUP<-data[,grouping_field]
  out<-ddply(data, .(OBS_YEAR,REGION_NAME,ISLAND,GROUP),
             summarise,
             meanAdultColDen=mean(ACD,na.rm = TRUE),
             meanJuvColDen=mean(JCD,na.rm = TRUE),
             meanBLE=mean(BLE,na.rm = TRUE),
             meanAcuteDZ=mean(AcuteDZ,na.rm = TRUE),
             meanChrDZ=mean(ChrDZ,na.rm = TRUE),
             meanDZ=mean(DZ,na.rm = TRUE),
             meanCOTS=mean(COTS,na.rm = TRUE),
             seCOTS=std.error(COTS,na.rm=TRUE),
             seAdultColDen=std.error(ACD,na.rm=TRUE),
             seJuvColDen=std.error(JCD,na.rm=TRUE),
             seBLE=std.error(BLE,na.rm=TRUE),
             seAcuteDZ=std.error(AcuteDZ,na.rm=TRUE),
             seChrDZ=std.error(ChrDZ,na.rm=TRUE),
             seDZ=std.error(DZ,na.rm=TRUE),
             ntot=length(SITE))
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(out)
}

Calc_IslDepthmetrics_BSR<-function(data, grouping_field="GENUS_CODE"){
  data$GROUP<-data[,grouping_field]
  out<-ddply(data, .(OBS_YEAR,REGION_NAME,ISLAND,DEPTH_BIN,GROUP),
             summarise,
             meanAdultColDen=mean(ACD,na.rm=TRUE),
             meanJuvColDen=mean(JCD,na.rm=TRUE),
             meanBLE=mean(BLE,na.rm=TRUE),
             meanAcuteDZ=mean(AcuteDZ,na.rm=TRUE),
             meanChrDZ=mean(ChrDZ,na.rm=TRUE),
             meanDZ=mean(DZ,na.rm=TRUE),
             meanCOTS=mean(COTS,na.rm = TRUE),
             seCOTS=std.error(COTS,na.rm=TRUE),
             seAdultColDen=std.error(ACD,na.rm=TRUE),
             seJuvColDen=std.error(JCD,na.rm=TRUE),
             seBLE=std.error(BLE,na.rm=TRUE),
             seAcuteDZ=std.error(AcuteDZ,na.rm=TRUE),
             seChrDZ=std.error(ChrDZ,na.rm=TRUE),
             seDZ=std.error(DZ,na.rm=TRUE),
             ntot=length(SITE))
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(out)
}

Calc_IslReefZonemetrics_BSR<-function(data, grouping_field="GENUS_CODE"){
  data$GROUP<-data[,grouping_field]
  out<-ddply(data, .(OBS_YEAR,REGION,ISLAND,REEF_ZONE,GROUP),
             summarise,
             meanAdultColDen=mean(ACD,na.rm=TRUE),
             meanJuvColDen=mean(JCD,na.rm=TRUE),
             meanBLE=mean(BLE,na.rm=TRUE),
             meanAcuteDZ=mean(AcuteDZ,na.rm=TRUE),
             meanChrDZ=mean(ChrDZ,na.rm=TRUE),
             meanDZ=mean(DZ,na.rm=TRUE),
             meanCOTS=mean(COTS,na.rm = TRUE),
             seCOTS=std.error(COTS,na.rm=TRUE),
             seAdultColDen=std.error(ACD,na.rm=TRUE),
             seJuvColDen=std.error(JCD,na.rm=TRUE),
             seBLE=std.error(BLE,na.rm=TRUE),
             seAcuteDZ=std.error(AcuteDZ,na.rm=TRUE),
             seChrDZ=std.error(ChrDZ,na.rm=TRUE),
             seDZ=std.error(DZ,na.rm=TRUE),
             ntot=length(SITE))
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(out)
}


####
#BSR-TEMPLATE FUNCTIONS####
####

zone_ord <- c("Forereef", "Backreef", "Lagoon")

reg_name <- function(x){ # this function returns the full name of an abbreviated region
  if(x == "PRIAs"){y = "Pacific Remote Islands Areas"}
  if(x == "MARIAN"){y = "Mariana Archipelago"}
  if(x == "MHI"){y = "Main Hawaiian Islands"}
  if(x == "NWHI"){y = "Northwestern Hawaiian Islands"}
  if(x == "SAMOA"){y = "American Samoa"}
  if(x == "NMAR"){y = "northern Mariana Archipelago"}
  if(x == "SMAR"){y = "southern Mariana Archipelago"}
  
  return(y)
}

island_order <- function(x){ # this function will return a list of the island names properly ordered for figures
  PRIAorder <- c("Johnston", "Baker", "Howland", "Jarvis", "Kingman", "Palmyra")
  NMARorder <- c("FDP", "Maug", "Asuncion", "Alamagan", "Pagan", "Agrihan", "Guguan", "Sarigan")
  SMARorder <- c("Saipan", "Tinian", "Aguijan", "Rota", "Guam")
  samoaorder <- c("Ofu & Olosega", "Rose", "Swains", "Tau", "Tutuila")
  MHIorder <- c("Hawaii", "Maui", "Kahoolawe", "Lanai", "Molokai", "Oahu", "Kauai", "Niihau")
  NWHIorder <- c("Necker", "French Frigate", "Gardner", "Maro", "Laysan", "Lisianski", "Pearl & Hermes", "Midway", "Kure")
  MARIANorder <- c("FDP", "Maug", "Asuncion", "Alamagan", "Pagan", "Agrihan", "Guguan", "Sarigan", "Saipan", "Tinian", "Aguijan", "Rota", "Guam")
  
  if(x == "Pacific Remote Island Areas" | x == "PRIAs"){y = PRIAorder}
  if(x == "northern Mariana Islands" | x == "NMAR"){y = NMARorder}
  if(x == "southern Mariana Islands" | x == "SMAR"){y = SMARorder}
  if(x == "American Samoa" | x == "SAMOA"){y = samoaorder}
  if(x == "Main Hawaiian Islands" | x == "MHI"){y = MHIorder}
  if(x == "Northwestern Hawaiian Islands" | x == "NWHI"){y = NWHIorder}
  if(x == "Mariana Archipelago" | x == "MARIAN"){y = NWHIorder}
  
  
  return(y)
} 


gen_name_fun <- function(gen){ # function to return genera name given abbreviation
  
  gen <- as.character(gen)
  lookup <-  read.csv("T:/Benthic/Data/SpGen_Reference/AllGenList.csv")
  genera <- as.character(lookup[ which(lookup$Genus.code == gen),]$Genus)
  return(genera)  
  
}

spe_name_fun <- function(spe){ # function to return genera name given abbreviation
  
  spe <- as.character(spe)
  lookup <-  read.csv("T:/Benthic/Data/SpGen_Reference/AllSpList.csv")
  spec <- as.character(lookup[ which(lookup$Species.code == spe),]$Species)
  return(spec)  
  
}
