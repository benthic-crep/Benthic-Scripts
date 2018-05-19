# reshape library inclues the cast() function used below
library(reshape2)
library(ggplot2) ## to create the diver vs diver graphs
library(data.table)
library(plyr)
library(gdata)
library(tidyr)
library(plotrix)




# GENERAL FUNCTIONS -------------------------------------------------------

#merging more than 2 dataframes together. use df <- Reduce(MyMerge, list(m1, m2, m3)) to use function
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","GENUS_CODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}

#Convert SPCODE in raw colony data to taxoncode.We use taxoncode because some taxa can not be reliably identified 
#to species-level across observers and need to be rolled up to genus. -generates a look up table
Convert_to_Taxoncode<-function(data){
  a<-ddply(data,.(REGION,OBS_YEAR,S_ORDER,GENUS_CODE,SPCODE), #create a list of Genera and Species by region and year
           summarise,
           count=length(COLONYID))
  b<-merge(a,taxa,by=c("REGION","OBS_YEAR","SPCODE"),all.x=T)
  b$TAXONCODE<-ifelse(b$S_ORDER!="Scleractinia",as.character(b$SPCODE),ifelse(is.na(b$TAXON_NAME), as.character(b$GENUS_CODE),as.character(b$SPCODE))) #Change spcode to genus code if we do not uniformly id that taxon to species level
  b$TAXONCODE[b$TAXONCODE==""] <- "UNKN" #Convert unknown species or codes that aren't in our taxa list to unknown
  out<-merge(data,b,by=c("REGION","OBS_YEAR","GENUS_CODE","SPCODE","S_ORDER"))
  out<-subset(out,select=-c(count,TAXON_NAME,TAXAGROUP)) #merge to master taxa list
  return(out)
}

##Calcuate segment and transect area and add column for transect area for methods c,e,f
Transectarea<-function(data,s.df){
data$SEGAREA<-data$SEGLENGTH*data$SEGWIDTH # Calculate segment area

#Calculate total transect area then merge back to a dataframe
s.df<-ddply(data, .(MISSIONID,REGION,ISLAND,OBS_YEAR,SITE,TRANSECT,SEGMENT,SITEVISITID),
            summarise,
            SEGAREA=median(SEGAREA))
tr.df<-ddply(s.df, .(MISSIONID,REGION,ISLAND,OBS_YEAR,SITE,TRANSECT,SITEVISITID),
             summarise,
             TRANSECTAREA=sum(SEGAREA))

data<-merge(data,tr.df, by=c("MISSIONID","REGION","ISLAND","OBS_YEAR","SITE","SITEVISITID","TRANSECT"),all=TRUE)


return(data)
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



## TRANSECT LEVEL SUMMARY FUNCTIONS #######

#This function calculates colony density at the transect scale by first calculating the total survey area (using Calc_SurveyArea_By_Transect) then calcuating colony density
Calc_ColDen_Transect<-function(data, grouping_field="GENUS_CODE"){

  data$GROUP<-data[,grouping_field] #assign a grouping field for taxa
  
  #Calculate # of colonies for each variable. You need to have S_ORDER and Fragment here so you can incorporate zeros properly
  a<-ddply(data, .(SITE,SITEVISITID,TRANSECT, S_ORDER,GROUP,Fragment),
           summarise,
           ColCount=length(COLONYID)) #change to count
  
  #Convert from long to wide and insert 0s for taxa that weren't found at each site. 
  ca<-dcast(a, formula=SITE + SITEVISITID +TRANSECT +Fragment+S_ORDER~ GROUP, value.var="ColCount",fill=0)
  data.cols<-names(ca[6:dim(ca)[2]]) #define your data coloumns
  field.cols<-c("SITE", "SITEVISITID", "TRANSECT","Fragment") #define field columns
  
  #change colony counts for fragments to 0 so that we account for the transects that only had fragments
  ca[which(ca$Fragment <0), data.cols]<-0 

  #At this point you will have multiple rows for each site/transect so sum data by site and transect. This will help you properly insert 0s
  field.cols<-c("SITE", "SITEVISITID", "TRANSECT")
  ca<-aggregate(ca[,data.cols], by=ca[,field.cols], sum) 
  
  #Create a list of scleractinian taxa that are in the dataframe as columns then sum across just those taxa to get total scl
  b<-subset(data,S_ORDER=="Scleractinia");taxalist<-as.character(unique(b$GROUP))
  ca$SSSS<-rowSums(ca[,taxalist,drop=FALSE]) #calculate total colony density
  ca <- gather(ca, GROUP, ColCount, names(ca[4:dim(ca)[2]]), factor_key=TRUE) #convert wide to long format
  
  #Remove everything that isn't a scleractinian
  taxalist2<-c(taxalist,"SSSS")
  ca<-ca[ca$GROUP %in% taxalist2,]

  #Calculate transect area surveyed and colony density
  trarea<-Calc_SurveyArea_By_Transect(data)
  out<-merge(trarea,ca, by=c("SITE","SITEVISITID","TRANSECT"),all.x=TRUE)
  out$ColDen<-out$ColCount/out$TRANSECTAREA
  out<-subset(out,select=-c(TRANSECTAREA))#remove transect area column
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  
  #Add in proportion occurance- add colomn 0 or 1
  return(out)
}


#Change percent to proportion
##This function calculates mean colony legnth, % recent dead, % old dead, condition severity or condition extent to the transect level
Calc_ColMetric_Transect<-function(data, grouping_field="S_ORDER",pool_fields=c("COLONYLENGTH","RDEXTENT1", "RDEXTENT2","OLDDEAD","SEVERITY","EXTENT")){
  
  scl<-subset(data,COLONYLENGTH>5&S_ORDER=="Scleractinia")
  scl$GROUP<-scl[,grouping_field]
  scl$y <- rowSums(scl[,pool_fields,drop=FALSE], na.rm=TRUE) #this will allow you to add the 2 recent dead columns if you are looking at this metric
  
  rd<-ddply(scl, .(SITE,SITEVISITID,TRANSECT,GROUP),
            summarise,
            Ave.y=mean(y, na.rm=TRUE))
  
  rdtot<-ddply(scl, .(SITE,SITEVISITID,TRANSECT),
               summarise,
               Ave.y=mean(y, na.rm=TRUE))
  rdtot$GROUP<-"SSSS"; rdtot <- rdtot[c(1,2,3,5,4)]
  #rd_wide<-dcast(rd, formula=SITE + SITEVISITID +OTHER +TRANSECT~ GROUP, value.var="Ave.y",fill=0)
  #rd_long <- gather(rd_wide, GROUP, Ave.y, names(rd_wide[5:dim(rd_wide)[2]]), factor_key=TRUE) #convert wide to long format
  rd_long<-rbind(rd,rdtot)
  
  colnames(rd_long)[which(colnames(rd_long) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(rd_long)
}

#This function calculate abundance of recent dead conditions by transect and taxonomic group
#Transform RD1 and RD2 from long to wide format, remove other site AND TRANSECT level info
# 
# Calc_RDden_Transect<-function(data, grouping_field="S_ORDER"){
# scl<-subset(data,COLONYLENGTH>5&S_ORDER=="Scleractinia")
# 
# rd1<-dcast(scl, formula=SITEVISITID + SITE+TRANSECT+COLONYID ~ RD1, value.var="RD1",length,fill=0);names(rd1)<-gsub("DZGN","DZGNS",names(rd1),fixed = TRUE)#; rd1<-rd1[,-c(1:3)]
# rd2<-dcast(scl, formula=SITEVISITID + SITE+TRANSECT+COLONYID ~ RD2, value.var="RD2",length,fill=0) ;names(rd2)<-gsub("DZGN","DZGNS",names(rd2),fixed = TRUE)#; rd2<-rd2[,-c(1:3)]
# rd3<-dcast(scl, formula=SITEVISITID + SITE+TRANSECT+COLONYID ~ GENRD1, value.var="GENRD1",length,fill=0);colnames(rd3)[5:ncol(rd3)]<-paste("All",colnames(rd3[,c(5:ncol(rd3))]),sep="_") 
# rd4<-dcast(scl, formula=SITEVISITID + SITE+TRANSECT+COLONYID ~ GENRD2, value.var="GENRD2",length,fill=0);colnames(rd4)[5:ncol(rd4)]<-paste("All",colnames(rd4[,c(5:ncol(rd4))]),sep="_") 
# 
# #merge all dataframes together
# a<-merge(rd1,rd2,by=c("SITEVISITID","SITE","TRANSECT","COLONYID"),all=TRUE)
# b<-merge(a,rd3,by=c("SITEVISITID","SITE","TRANSECT","COLONYID"),all=TRUE)
# allrd<-merge(b,rd4,by=c("SITEVISITID","SITE","TRANSECT","COLONYID"),all=TRUE)
# 
# allrd<-allrd[,-c(1:3)] #remove all metadata except colonyid
# 
# #R will add .x and .y to column names because there are columns that are identically names
# # this will remove .x and .y so that we can sum identifically named columns
# names(allrd)<-gsub(".x","",names(allrd),fixed = TRUE)
# names(allrd)<-gsub(".y","",names(allrd),fixed = TRUE)
# head(allrd)
# 
# #Sum identically named columns and remove the no data column
# allrd2<-as.data.frame(sapply(unique(colnames(allrd)), 
#                              function(x) rowSums(allrd[, colnames(allrd) == x, drop = FALSE])));allrd2<-allrd2[,!(colnames(allrd2) =="All_NONE")]
# 
# #merge data with colony level metadata and sum conditions by transect and taxoncode
# allrd3<-merge(survey_colony,allrd2, by="COLONYID")
# long <- gather(allrd3, RDCond, abun, names(allrd3[22:dim(allrd3)[2]]), factor_key=TRUE) #convert wide to long format by condition
# long$GROUP<-long[,grouping_field]
# longsum<-ddply(long, .(SITE,SITEVISITID,TRANSECT,GROUP,RDCond), #calc total colonies by taxon and condition
#            summarise,
#            RDabun=sum(abun))
# out1<-ddply(longsum, .(SITE,SITEVISITID,TRANSECT,RDCond), #calc total colonies by condition
#            summarise,
#            RDabun=sum(RDabun))
# out1$GROUP<-"SSSS"; out1 <- out1[c(1,2,3,6,4,5)] #add total colony code
# a<-rbind(longsum,out1)
# a<-subset(a,RDCond!="NONE")
# 
# #Convert back to wide format
# abun<-dcast(a, formula=SITEVISITID +SITE + TRANSECT+GROUP~ RDCond, value.var="RDabun",sum,fill=0)
# 
# trarea<-Calc_SurveyArea_By_Transect(data) #calculate survey area/site
# 
# #merge dataframes
# ab.tr<-merge(trarea,abun,by=c("SITEVISITID","SITE","TRANSECT"),all=TRUE)
# ab.tr[is.na(ab.tr)]<-0
# new_DF <- ab.tr[rowSums(is.na(ab.tr)) > 0,] #identify which rows have NAs
# 
# 
# #calcualte density of each condition
# cd<-ab.tr[, 6:ncol(ab.tr)]/ab.tr$TRANSECTAREA # selects every row and 2nd to last columns
# out<-cbind(ab.tr[,c(1:5)],cd) #cbind the transect info to data.
# 
# colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
# 
# return(out)
# }

#This function calculate abundance of recent dead conditions by transect and taxonomic group
#Transform RD1 and RD2 from long to wide format, remove other site AND TRANSECT level info

Calc_RDden_Transect<-function(data, grouping_field="S_ORDER"){
  scl<-subset(awd,COLONYLENGTH>5&S_ORDER=="Scleractinia")
  
  #add and "S" to the general cause code so that we distiguish it from specific cause codes
  scl$GENRD1<-paste(scl$GENRD1,"S",sep="");scl$GENRD2<-paste(scl$GENRD2,"S",sep="")
  
  #collapse all general and specific cause code columns into 1 column so that we can count up # of colonies with each condition
  long <- gather(scl, RDcat, RDtype, c(GENRD1,RD1,GENRD2,RD2), factor_key=TRUE)
  
  #convert from long to wide and fill in 0s
  rd<-dcast(long, formula=SITEVISITID + SITE+TRANSECT+COLONYID ~ RDtype, value.var="RDtype",length,fill=0)
  
  d<-  rd[,-c(1:4)] #remove all metadata
  d[d>1] <- 1 #Change values greater than 1 to 1 so that you don't double count colonies
  meta<-rd[,c(1:4)] #Subset just metadata
  rd.new<-cbind(meta,d) #combine metadata back with data
  
  #merge data with colony level metadata and sum conditions by transect and taxoncode
  allrd3<-merge(survey_colony,rd.new, by=c("SITEVISITID","SITE","TRANSECT","COLONYID"))
  long <- gather(allrd3, RDCond, abun, names(allrd3[21:dim(allrd3)[2]]), factor_key=TRUE) #convert wide to long format by condition
  long$GROUP<-long[,grouping_field]
  longsum<-ddply(long, .(SITE,SITEVISITID,TRANSECT,GROUP,RDCond), #calc total colonies by taxon and condition
                 summarise,
                 RDabun=sum(abun))
  out1<-ddply(longsum, .(SITE,SITEVISITID,TRANSECT,RDCond), #calc total colonies by condition
              summarise,
              RDabun=sum(RDabun))
  out1$GROUP<-"SSSS"; out1 <- out1[c(1,2,3,6,4,5)] #add total colony code
  a<-rbind(longsum,out1)
  a<-subset(a,!RDCond %in% c("NONES","NONE"))
  
  #Convert back to wide format
  abun<-dcast(a, formula=SITEVISITID +SITE + TRANSECT+GROUP~ RDCond, value.var="RDabun",sum,fill=0)
  
  trarea<-Calc_SurveyArea_By_Transect(data) #calculate survey area/site
  
  #merge dataframes
  ab.tr<-merge(trarea,abun,by=c("SITEVISITID","SITE","TRANSECT"),all=TRUE)
  ab.tr[is.na(ab.tr)]<-0
  new_DF <- ab.tr[rowSums(is.na(ab.tr)) > 0,] #identify which rows have NAs
  
  
  #calcualte density of each condition
  cd<-ab.tr[, 6:ncol(ab.tr)]/ab.tr$TRANSECTAREA # selects every row and 2nd to last columns
  out<-cbind(ab.tr[,c(1:5)],cd) #cbind the transect info to data.
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(out)
}


#This function calculate abundance of recent dead conditions by transect and taxonomic group
#Transform RD1 and RD2 from long to wide format, remove other site AND TRANSECT level info

Calc_RDabun_Transect<-function(data, grouping_field="S_ORDER"){
  scl<-subset(data,COLONYLENGTH>5&S_ORDER=="Scleractinia")
  
  #add and "S" to the general cause code so that we distiguish it from specific cause codes
  scl$GENRD1<-paste(scl$GENRD1,"S",sep="");scl$GENRD2<-paste(scl$GENRD2,"S",sep="")
  
  #collapse all general and specific cause code columns into 1 column so that we can count up # of colonies with each condition
  long <- gather(scl, RDcat, RDtype, c(GENRD1,RD1,GENRD2,RD2), factor_key=TRUE)
  
  #convert from long to wide and fill in 0s
  rd<-dcast(long, formula=SITEVISITID + SITE+TRANSECT+COLONYID ~ RDtype, value.var="RDtype",length,fill=0)
  
  d<-  rd[,-c(1:4)] #remove all metadata
  d[d>1] <- 1 #Change values greater than 1 to 1 so that you don't double count colonies
  meta<-rd[,c(1:4)] #Subset just metadata
  rd.new<-cbind(meta,d) #combine metadata back with data
  
  #merge data with colony level metadata and sum conditions by transect and taxoncode
  allrd3<-merge(survey_colony,rd.new, by=c("SITEVISITID","SITE","TRANSECT","COLONYID"))
  long <- gather(allrd3, RDCond, abun, names(allrd3[21:dim(allrd3)[2]]), factor_key=TRUE) #convert wide to long format by condition
  long$GROUP<-long[,grouping_field]
  longsum<-ddply(long, .(SITE,SITEVISITID,TRANSECT,GROUP,RDCond), #calc total colonies by taxon and condition
                 summarise,
                 RDabun=sum(abun))
  out1<-ddply(longsum, .(SITE,SITEVISITID,TRANSECT,RDCond), #calc total colonies by condition
              summarise,
              RDabun=sum(RDabun))
  out1$GROUP<-"SSSS"; out1 <- out1[c(1,2,3,6,4,5)] #add total colony code
  a<-rbind(longsum,out1)
  a<-subset(a,!RDCond %in% c("NONES","NONE"))
  
  #Convert back to wide format
  out<-dcast(a, formula=SITEVISITID +SITE + TRANSECT+GROUP~ RDCond, value.var="RDabun",sum,fill=0)
  colnames(out)[colnames(out)=="DZGNS"]<-"AcuteDZ" #rename column 
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  cd<-Calc_ColDen_Transect(data,grouping_field)[,-c(6)]
  out2<-merge(out,cd,by=c("SITEVISITID","SITE","TRANSECT",grouping_field))
  
  return(out2)
}



#This function calculates DENSITY of conditions conditions by transect and taxonomic group
Calc_Condden_Transect<-function(data, grouping_field="S_ORDER"){
  #Remove scleractinan adult colony fragments
  scl<-subset(data,COLONYLENGTH>5&S_ORDER=="Scleractinia")
  
  c<-dcast(scl, formula=SITEVISITID + SITE+TRANSECT+COLONYID ~ COND, value.var="COND",length,fill=0)
  c <- subset(c, select = -c(NDZ)) #remove columns
  a<-merge(survey_colony,c, by=c("SITE","SITEVISITID","TRANSECT","COLONYID"))
  long <- gather(a, COND, Condabun, names(a[21:dim(a)[2]]), factor_key=TRUE) #convert wide to long format by condition
  
  #merge data with colony level metadata and sum conditions by transect and taxoncode
  long$GROUP<-long[,grouping_field]
  longsum<-ddply(long, .(SITE,SITEVISITID,TRANSECT,GROUP,COND),
             summarise,
             Condabun=sum(Condabun))
  out1<-ddply(longsum, .(SITE,SITEVISITID,TRANSECT,COND), #calc total colonies by condition
              summarise,
              Condabun=sum(Condabun))
  out1$GROUP<-"SSSS"; out1 <- out1[c(1,2,3,6,4,5)] #add total colony code
  a<-rbind(longsum,out1)
  a<-subset(a,COND!="NONE")
  
  #convert wide to long
  abun<-dcast(a, formula=SITEVISITID +SITE+ TRANSECT+GROUP~ COND, value.var="Condabun",sum,fill=0)
  
  trarea<-Calc_SurveyArea_By_Transect(data) #calculate survey area/site
  
  #merge dataframes
  ab.tr<-merge(trarea,abun,by=c("SITEVISITID","SITE","TRANSECT"),all=TRUE)
  ab.tr[is.na(ab.tr)]<-0
  
  #calcualte density of each condition
  cd<-ab.tr[, 6:ncol(ab.tr)]/ab.tr$TRANSECTAREA # selects every row and 2nd to last columns
  out<-cbind(ab.tr[,c(1:5)],cd) #cbind the transect info to data.
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(out)
}

#This function calculate ABUNDANCE of conditions conditions by transect and taxonomic group
Calc_Condabun_Transect<-function(data, grouping_field="S_ORDER"){
  #Remove scleractinan adult colony fragments
  scl<-subset(data,COLONYLENGTH>5&S_ORDER=="Scleractinia")
  
  c<-dcast(scl, formula=SITEVISITID + SITE+TRANSECT+COLONYID ~ COND, value.var="COND",length,fill=0)
  c <- subset(c, select = -c(NDZ)) #remove columns
  a<-merge(survey_colony,c, by=c("SITE","SITEVISITID","TRANSECT","COLONYID"))
  long <- gather(a, COND, Condabun, names(a[21:dim(a)[2]]), factor_key=TRUE) #convert wide to long format by condition
  
  #merge data with colony level metadata and sum conditions by transect and taxoncode
  long$GROUP<-long[,grouping_field]
  longsum<-ddply(long, .(SITE,SITEVISITID,TRANSECT,GROUP,COND),
                 summarise,
                 Condabun=sum(Condabun))
  out1<-ddply(longsum, .(SITE,SITEVISITID,TRANSECT,COND), #calc total colonies by condition
              summarise,
              Condabun=sum(Condabun))
  out1$GROUP<-"SSSS"; out1 <- out1[c(1,2,3,6,4,5)] #add total colony code
  a<-rbind(longsum,out1)
  a<-subset(a,COND!="NONE")
  
  
  #convert wide to long
  abun<-dcast(a, formula=SITEVISITID +SITE+ TRANSECT+GROUP~ COND, value.var="Condabun",sum,fill=0)
  
  colnames(abun)[which(colnames(abun) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  cd<-Calc_ColDen_Transect(data,grouping_field)[,-c(6)]
  out<-merge(abun,cd,by=c("SITEVISITID","SITE","TRANSECT",grouping_field))
  
  return(out)
}


## POOLING FUNCTIONS TO GENERATE ESTIMATES #######

#STRATA ROLL UP FUNCTION-This function calculates mean, var, SE and CV at the strata level. I've built in flexilbity to use either genus or taxoncode
# You can input any metric you would like (eg. adult density, mean % old dead,etc). Note that for any metric that does not involve density of colonies, 
# Y._h (total colony abundance in stratum),varY._h (variance in total abundance), SE_Y._h and CV_Y._h are meaningless-DO NOT USE
#Note: for whatever reason, the grouping, other and metric fields need to be in this order. If you don't want to include an other field then add "DUMMY" as the second variable when you are running this function.
#e.g. st<-Calc_Strata(data.mon,"GENUS_CODE","DUMMY","ColDen")

Calc_Strata=function(site_data,grouping_field,metric_field=c("AdColDen","JuvColDen","Ave.od","Ave.rd","Condabun","CORAL"),M_hi=250){
  
  #Build in flexibility to look at genus or taxon level
  site_data$GROUP<-site_data[,grouping_field]
  
  #Build in flexibility to summarized different metrics
  site_data$METRIC<-site_data[,metric_field]
  site_data$METRIC<-as.numeric(site_data$METRIC)
  
  #For a Given ANALYSIS_SCHEMA, we need to pool N_h, and generate w_h
  Strata_NH<-ddply(subset(site_data,GROUP=="SSSS"),.(ANALYSIS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA,STRATANAME),summarize,N_h=median(NH,na.rm=TRUE)) #calculate # of possible sites in a given stratum
  Schema_NH<-ddply(Strata_NH,.(ANALYSIS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA),summarize,N_h=sum(N_h,na.rm=TRUE))#calculate # of possible sites in a given schema
  Dom_NH<-ddply(Schema_NH,.(ANALYSIS_YEAR,DOMAIN_SCHEMA),summarize,Dom_N_h=sum(N_h,na.rm=TRUE))#calculate # of possible sites in a given domain
  Schema_NH$Dom_N_h<-Dom_NH$Dom_N_h[match(Schema_NH$DOMAIN_SCHEMA,Dom_NH$DOMAIN_SCHEMA)]# add Dom_N_h to schema dataframe
  Schema_NH$w_h<-Schema_NH$N_h/Schema_NH$Dom_N_h # add schema weighting factor to schema dataframe
  
  #Now add back the Analysis_Schema Nh and wh to site_data
  site_data$N_h.as<-Schema_NH$N_h[match(site_data$ANALYSIS_SCHEMA,Schema_NH$ANALYSIS_SCHEMA)]
  site_data$w_h.as<-Schema_NH$w_h[match(site_data$ANALYSIS_SCHEMA,Schema_NH$ANALYSIS_SCHEMA)]

  #Calculate summary metrics at the stratum level (rolled up from site level)
  Strata_roll=ddply(site_data,.(ANALYSIS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA,GROUP),summarize,
                    n_h=length(SITE),# No. of Sites surveyed in a Strata
                    N_h=median(N_h.as,na.rm=T),# Strata Area (as N 50x50 grids) - median allows you to pick 1 value
                    w_h=median(w_h.as,na.rm=T),# weigting factor for a given stratum- median allows you to pick 1 value
                    D._h=mean(METRIC,na.rm=T), # Mean of Site-Level metric in a Stratum
                    S1_h=var(METRIC,na.rm=T), #sample variance in metric between sites
                    varD._h=(1-(n_h/N_h))*S1_h/n_h, #Strata level  variance of mean density
                    Y._h=D._h*N_h*250, #total colony abundance in stratum (Mhi=250) 
                    varY._h=varD._h*N_h^2, #variance in total abundance 
                    SE_D._h=sqrt(varD._h),
                    CV_D._h=SE_D._h/D._h,
                    SE_Y._h=sqrt(varY._h),
                    CV_Y._h=SE_Y._h/Y._h)
  
  Strata_roll$M_hi=250 #define total possible transects in a site
  Strata_roll=Strata_roll[,c("ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","GROUP",
                             "M_hi","n_h","N_h","w_h",
                             "D._h","S1_h","varD._h","SE_D._h","CV_D._h",
                             "Y._h","varY._h","SE_Y._h","CV_Y._h")]
  
  #remove strata that have only 1 site because you can't calculate variance
  Strata_roll<-Strata_roll[Strata_roll$n_h>1,]
  
  colnames(Strata_roll)[which(colnames(Strata_roll) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.


  return(Strata_roll)
}


#DOMAIN ROLL UP FUNCTION-This function calculates mean, var, SE and CV at the DOMAIN level. I've built in flexilbity to use either genus or taxoncode as well as other metrics (size class, morph)
# You can input any metric you would like (eg. adult density, mean % old dead,etc). Note that for any metric that does not involve density of colonies, 
# Y._h (total colony abundance in stratum),varY._h (variance in total abundance), SE_Y._h and CV_Y._h are meaningless-DO NOT USE
Calc_Domain=function(site_data,grouping_field="S_ORDER",metric_field=c("AdColDen","JuvColDen","Ave.od","Ave.rd")){
 
  Strata_data=Calc_Strata(site_data,grouping_field,metric_field)
  
  #Build in flexibility to look at genus or taxon level
  Strata_data$GROUP<-Strata_data[,grouping_field]
  
  DomainStr_NH=ddply(subset(Strata_data,GROUP=="SSSS"),.(ANALYSIS_YEAR,DOMAIN_SCHEMA),summarize,N_h=sum(N_h,na.rm=TRUE)) #total possible sites in a domain
  Strata_data$DomainSumN_h=DomainStr_NH$N_h[match(Strata_data$DOMAIN_SCHEMA,DomainStr_NH$DOMAIN_SCHEMA)] # add previous to strata data
  Strata_data$w_h=Strata_data$N_h/Strata_data$DomainSumN_h
  
  Domain_roll=ddply(Strata_data,.(ANALYSIS_YEAR,DOMAIN_SCHEMA,GROUP),summarize,
                    D._st=sum(w_h*D._h,na.rm=TRUE), #Domain weighted estimate (sum of Weighted strata density)
                    varD._st=sum(w_h^2*varD._h,na.rm=TRUE), #Domain weighted variance estimate
                    Y._st=sum(Y._h,na.rm=TRUE), #Domain total abundance (sum of extrapolated strata abundance)
                    varY._st=sum(varY._h,na.rm=TRUE),#Domain variance total abundance (sum of extrapolated strata varaiance abundance)
                    n=sum(n_h,na.rm=TRUE), #total sites surveyed in domain
                    Ntot=sum(N_h,na.rm=TRUE), #total possible sites in domain
                    SE_D._st=sqrt(varD._st), #SE of domain metric estimate
                    CV_D._st=SE_varD._st/D._st, #CV of domain metric estimate
                    SE_Y._st=sqrt(varY._st),#SE of domain abundance estimate
                    CV_Y._st=SE_varY._st/Y._st.)#CV of domain abundnace estimate
      #Add Weighted proportion occurance, se and cv)
  
  colnames(Domain_roll)[which(colnames(Domain_roll) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  

  return(Domain_roll)
}

###POOLING FUNCTIONS FOR COVER DATA----
Calc_Strata_Cover=function(site_data,metric_field=c("CORAL","CCA","MA","TURF"),M_hi=250){
  
  #Build in flexibility to summarized different metrics
  site_data$METRIC<-site_data[,metric_field]
  site_data$METRIC<-as.numeric(site_data$METRIC)
  
  #For a Given ANALYSIS_SCHEMA, we need to pool N_h, and generate w_h
  Strata_NH<-ddply(site_data,.(OBS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA,STRATANAME),summarize,N_h=median(NH,na.rm=TRUE)) #calculate # of possible sites in a given stratum
  Schema_NH<-ddply(Strata_NH,.(OBS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA),summarize,N_h=sum(N_h,na.rm=TRUE))#calculate # of possible sites in a given schema
  Dom_NH<-ddply(Schema_NH,.(OBS_YEAR,DOMAIN_SCHEMA),summarize,Dom_N_h=sum(N_h,na.rm=TRUE))#calculate # of possible sites in a given domain
  Schema_NH$Dom_N_h<-Dom_NH$Dom_N_h[match(Schema_NH$DOMAIN_SCHEMA,Dom_NH$DOMAIN_SCHEMA)]# add Dom_N_h to schema dataframe
  Schema_NH$w_h<-Schema_NH$N_h/Schema_NH$Dom_N_h # add schema weighting factor to schema dataframe
  
  #Now add back the Analysis_Schema Nh and wh to site_data
  site_data$N_h.as<-Schema_NH$N_h[match(site_data$ANALYSIS_SCHEMA,Schema_NH$ANALYSIS_SCHEMA)]
  site_data$w_h.as<-Schema_NH$w_h[match(site_data$ANALYSIS_SCHEMA,Schema_NH$ANALYSIS_SCHEMA)]
  
  #Calculate summary metrics at the stratum level (rolled up from site level)
  Strata_roll=ddply(site_data,.(OBS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA),summarize,
                    n_h=length(SITE),# No. of Sites surveyed in a Strata
                    N_h=median(N_h.as,na.rm=T),# Strata Area (as N 50x50 grids) - median allows you to pick 1 value
                    w_h=median(w_h.as,na.rm=T),# weigting factor for a given stratum- median allows you to pick 1 value
                    D._h=mean(METRIC,na.rm=T), # Mean of Site-Level metric in a Stratum
                    S1_h=var(METRIC,na.rm=T), #sample variance in metric between sites
                    varD._h=(1-(n_h/N_h))*S1_h/n_h, #Strata level  variance of mean density
                    SE_D._h=sqrt(varD._h),
                    CV_D._h=SE_D._h/D._h)
  
  Strata_roll$M_hi=250 #define total possible transects in a site
  Strata_roll=Strata_roll[,c("OBS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA",
                             "M_hi","n_h","N_h","w_h",
                             "D._h","S1_h","varD._h","SE_D._h","CV_D._h")]
  
  #remove strata that have only 1 site because you can't calculate variance
  Strata_roll<-Strata_roll[Strata_roll$n_h>1,]
  
  return(Strata_roll)
}


Calc_Domain_Cover=function(site_data,metric_field=c("CORAL","CCA","MA","TURF")){
  
  Strata_data=Calc_Strata_Cover(site_data,metric_field)
  
  DomainStr_NH=ddply(Strata_data,.(OBS_YEAR,DOMAIN_SCHEMA),summarize,N_h=sum(N_h,na.rm=TRUE)) #total possible sites in a domain
  Strata_data$DomainSumN_h=DomainStr_NH$N_h[match(Strata_data$DOMAIN_SCHEMA,DomainStr_NH$DOMAIN_SCHEMA)] # add previous to strata data
  Strata_data$w_h=Strata_data$N_h/Strata_data$DomainSumN_h
  
  Domain_roll=ddply(Strata_data,.(OBS_YEAR,DOMAIN_SCHEMA),summarize,
                    D._st=sum(w_h*D._h,na.rm=TRUE), #Domain weighted estimate (sum of Weighted strata density)
                    varD._st=sum(w_h^2*varD._h,na.rm=TRUE), #Domain weighted variance estimate
                    n=sum(n_h,na.rm=TRUE), #total sites surveyed in domain
                    Ntot=sum(N_h,na.rm=TRUE), #total possible sites in domain
                    SE_D._st=sqrt(varD._st), #SE of domain metric estimate
                    CV_D._st=SE_D._st/D._st) #CV of domain metric estimate
  
  return(Domain_roll)
}


###POOLING FUNCTIONS FOR PREVALENCE DATA----

#STRATA ROLL UP FUNCTION-This function calculates mean, var, SE and CV at the strata level. I've built in flexilbity to use either genus or taxoncode
# You can input any metric you would like (eg. adult density, mean % old dead,etc). Note that for any metric that does not involve density of colonies, 
# Y._h (total colony abundance in stratum),varY._h (variance in total abundance), SE_Y._h and CV_Y._h are meaningless-DO NOT USE
#Note: for whatever reason, the grouping, other and metric fields need to be in this order. If you don't want to include an other field then add "DUMMY" as the second variable when you are running this function.
#e.g. st<-Calc_Strata(data.mon,"GENUS_CODE","DUMMY","ColDen")

Calc_Strata_Prevalence=function(site_data,grouping_field,metric_field){
  
  #Build in flexibility to look at genus or taxon level
  site_data$GROUP<-site_data[,grouping_field]
  
  #Build in flexibility to summarized different metrics
  site_data$METRIC<-site_data[,metric_field]
  site_data$METRIC<-as.numeric(site_data$METRIC)
  
  #For a Given ANALYSIS_SCHEMA, we need to pool N_h, and generate w_h
  Strata_NH<-ddply(subset(site_data,GROUP=="SSSS"),.(ANALYSIS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA,STRATANAME),summarize,N_h=median(NH,na.rm=TRUE)) #calculate # of possible sites in a given stratum
  Schema_NH<-ddply(Strata_NH,.(ANALYSIS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA),summarize,N_h=sum(N_h,na.rm=TRUE))#calculate # of possible sites in a given schema
  Dom_NH<-ddply(Schema_NH,.(ANALYSIS_YEAR,DOMAIN_SCHEMA),summarize,Dom_N_h=sum(N_h,na.rm=TRUE))#calculate # of possible sites in a given domain
  Schema_NH$Dom_N_h<-Dom_NH$Dom_N_h[match(Schema_NH$DOMAIN_SCHEMA,Dom_NH$DOMAIN_SCHEMA)]# add Dom_N_h to schema dataframe
  Schema_NH$w_h<-Schema_NH$N_h/Schema_NH$Dom_N_h # add schema weighting factor to schema dataframe
  
  #Now add back the Analysis_Schema Nh and wh to site_data
  site_data$N_h.as<-Schema_NH$N_h[match(site_data$ANALYSIS_SCHEMA,Schema_NH$ANALYSIS_SCHEMA)]
  site_data$w_h.as<-Schema_NH$w_h[match(site_data$ANALYSIS_SCHEMA,Schema_NH$ANALYSIS_SCHEMA)]
  
  #site_data$Mhi<-250 - Still having issues using Mhi as a variable in dataframe
  #Calculate summary metrics at the stratum level (rolled up from site level)
  Strata_roll=ddply(site_data,.(ANALYSIS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA,GROUP),summarize,
                    n_h=length(SITE),# No. of Sites surveyed in a Strata
                    N_h=median(N_h.as,na.rm=T),# Strata Area (as N 50x50 grids) - median allows you to pick 1 value
                    w_h=median(w_h.as,na.rm=T),# weigting factor for a given stratum- median allows you to pick 1 value
                    C_h=mean(BLE,na.rm=T), # Mean density colonies with specific condition in a Stratum
                    S1C_h=var(BLE,na.rm=T), #sample variance in condition density between sites
                    varC_h=(1-(n_h/N_h))*S1C_h/n_h, #Strata level  variance of mean condition density
                    C_abun_h=C_h*N_h*250, # abundance of colonies with a condition in stratum 
                    varC_abun_h=varC_h*N_h^2, #variance in total abundance 
                    SE_C_abun_h=sqrt(varC_abun_h),
                    acd_h=mean(AdColDen,na.rm=T), # Mean of Site-Level all colonies in a Stratum
                    acd_abun_h=acd_h*N_h*250, #strata-level abundnace of all colonies
                    prev=C_abun_h/acd_abun_h, # prevalence of condition at stratum level
                    SEprev=SE_C_abun_h/acd_abun_h)#SE of condition at stratum level 

  Strata_roll$M_hi=250 #define total possible transects in a site
  Strata_roll=Strata_roll[,c("ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","GROUP",
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
  
  Strata_data=Calc_Strata(site_data,grouping_field,metric_field)
  
  #Build in flexibility to look at genus or taxon level
  Strata_data$GROUP<-Strata_data[,grouping_field]
  
  DomainStr_NH=ddply(subset(Strata_data,GROUP=="SSSS"),.(ANALYSIS_YEAR,DOMAIN_SCHEMA),summarize,N_h=sum(N_h,na.rm=TRUE)) #total possible sites in a domain
  Strata_data$DomainSumN_h=DomainStr_NH$N_h[match(Strata_data$DOMAIN_SCHEMA,DomainStr_NH$DOMAIN_SCHEMA)] # add previous to strata data
  Strata_data$w_h=Strata_data$N_h/Strata_data$DomainSumN_h
  
  Domain_roll=ddply(Strata_data,.(ANALYSIS_YEAR,DOMAIN_SCHEMA,GROUP),summarize,
                    C_st=sum(w_h*C_h,na.rm=TRUE), #Domain weighted estimate (sum of Weighted strata density)
                    varC_st=sum(w_h^2*varC_h,na.rm=TRUE), #Domain weighted variance estimate
                    C_abun_st=sum(C_abun_h,na.rm=TRUE), #Domain total abundance (sum of extrapolated strata abundance)
                    varC_abun_st=sum(varC_abun_h,na.rm=TRUE),#Domain variance total abundance (sum of extrapolated strata varaiance abundance)
                    n=sum(n_h,na.rm=TRUE), #total sites surveyed in domain
                    N=sum(N_h,na.rm=TRUE), #total possible sites in domain
                    SE_varC_st=sqrt(varC_st), #SE of domain metric estimate
                    CV_varC_st=SE_varC_st/C_st, #CV of domain metric estimate
                    SE_varC_abun_st=sqrt(varC_abun_st),#SE of domain abundance estimate
                    CV_varC_abun_st=SE_varC_abun_st/C_abun_st,#CV of domain abundnace estimate
                    acd_st=sum(w_h*acd_h,na.rm=TRUE), # Mean of Site-Level all colonies in a Stratum
                    acd_abun_abun_st=sum(acd_abun_h,na.rm=TRUE), #strata-level abundnace of all colonies
                    prev=C_abun_st/acd_abun_h, # prevalence of condition at stratum level
                    SEprev=SE_C_abun_h/acd_abun_h,#SE of condition at stratum level 
                    CVprev=SEprev/prev) #CV of prevalence
  
  colnames(Domain_roll)[which(colnames(Domain_roll) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(Domain_roll)
}





####
#BSR-UNWEIGHTED FUNCTIONS####
####


Calc_Sitemetrics_BSR<-function(data, grouping_field){
  a<-merge(data,survey_transect,by=c("SITE","SITEVISITID","TRANSECT"))
  a$GROUP<-a[,grouping_field]
  out<-ddply(a, .(OBS_YEAR,REGION,ISLAND,SITE,LATITUDE,LONGITUDE,REEF_ZONE,DEPTH_BIN,GROUP),
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
  out<-ddply(data, .(OBS_YEAR,REGION,ISLAND,GROUP),
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
  out<-ddply(data, .(OBS_YEAR,REGION,ISLAND,DEPTH_BIN,GROUP),
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
