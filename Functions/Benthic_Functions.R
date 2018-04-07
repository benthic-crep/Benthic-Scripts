# reshape library inclues the cast() function used below
library(reshape2)
library(ggplot2) ## to create the diver vs diver graphs
library(data.table)
library(plyr)
library(gdata)
library(tidyr)



# GENERAL FUNCTIONS -------------------------------------------------------

#merging more than 2 dataframes together. use df <- Reduce(MyMerge, list(m1, m2, m3)) to use function
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","GENUS_CODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}

#standard error
se<-function(e) {sd(e)/sqrt(length(e))}


#Convert SPCODE in raw colony data to taxoncode.We use taxoncode because some taxa can not be reliably identified 
#to species-level across observers and need to be rolled up to genus. -generates a look up table
Convert_to_Taxoncode<-function(data){
  a<-ddply(data,.(REGION,OBS_YEAR,S_ORDER,GENUS_CODE,SPCODE), #create a list of Genera and Species by region and year
           summarise,
           count=length(COLONYID))
  b<-merge(a,taxa,by=c("REGION","OBS_YEAR","SPCODE"),all.x=T)
  b$TAXONCODE<-ifelse(b$S_ORDER!="Scleractinia",as.character(b$SPCODE),ifelse(is.na(b$TAXON_NAME), as.character(b$GENUS_CODE),as.character(b$SPCODE))) #Change spcode to genus code if we do not uniformly id that taxon to species level
  b$TAXONCODE[b$TAXONCODE==""] <- "UNKN" #Convert unknown species or codes that aren't in our taxa list to unknown
  out<-merge(x,b,by=c("REGION","OBS_YEAR","GENUS_CODE","SPCODE","S_ORDER"))
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
Calc_ColDen_Transect<-function(data, grouping_field="S_ORDER"){

trarea<-Calc_SurveyArea_By_Transect(data)

scl<-subset(data,S_ORDER=="Scleractinia")

scl$GROUP<-scl[,grouping_field]
colden<-ddply(scl, .(SITE,SITEVISITID,TRANSECT, GROUP),
              summarise,
              Colabun=length(COLONYID)) #change to count

colden2<-merge(trarea,colden, by=c("SITE","SITEVISITID","TRANSECT"),all.x=TRUE)

colden2$Colabun[is.na(colden2$Colabun)]<-0

colden2$ColDen<-colden2$Colabun/colden2$TRANSECTAREA

cd<-dcast(colden2, formula=SITE + SITEVISITID + TRANSECT ~ GROUP, value.var="ColDen",fill=0)
ca<-dcast(colden2, formula=SITE + SITEVISITID + TRANSECT ~ GROUP, value.var="Colabun",fill=0)
cd<-cd[,-grep("NA",colnames(cd))] #remove NA column
ca<-ca[,-grep("NA",colnames(ca))] #remove NA column

cd$SSSS<-rowSums(cd[,names(cd[4:dim(cd)[2]]),drop=FALSE]) #calculate total colony density
cd <- gather(cd, GROUP, ColDen, names(cd[4:dim(cd)[2]]), factor_key=TRUE) #convert wide to long format

ca$SSSS<-rowSums(ca[,names(ca[4:dim(ca)[2]]),drop=FALSE]) #calculate total colony density
ca <- gather(ca, GROUP, Colabun, names(ca[4:dim(ca)[2]]), factor_key=TRUE) #convert wide to long format

out<-merge(cd,ca, by=c("SITE","SITEVISITID","TRANSECT","GROUP"))

colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.

return(out)
}

##This function calculates mean % recent dead, % old dead, condition severity or condition extent to the transect level
Calc_Dead_Sev_Ext_Transect<-function(data, grouping_field="S_ORDER",pool_fields=c("RDEXTENT1", "RDEXTENT2","OLDDEAD","SEVERITY","EXTENT")){
  
  scl<-subset(data,S_ORDER=="Scleractinia")
  scl$GROUP<-scl[,grouping_field]
  scl$y <- rowSums(scl[,pool_fields,drop=FALSE], na.rm=TRUE) #this will allow you to add the 2 recent dead columns if you are looking at this metric
  #scl$y <- rowSums(awd[,c("OLDDEAD")], na.rm=TRUE) #this will allow you to add the 2 recent dead columns if you are looking at this metric
  
  rd<-ddply(scl, .(SITE,SITEVISITID,TRANSECT,GROUP),
                 summarise,
                 Ave.y=mean(y))
  rdtot<-ddply(scl, .(SITE,SITEVISITID,TRANSECT),
               summarise,
               Ave.y=mean(y))
  rdtot$GROUP<-"SSSS"; rdtot <- rdtot[c(1,2,3,5,4)]
  rd_wide<-dcast(rd, formula=SITE + SITEVISITID + TRANSECT ~ GROUP, value.var="Ave.y",fill=0)
  rd_long <- gather(rd_wide, GROUP, Ave.y, names(rd_wide[4:dim(rd_wide)[2]]), factor_key=TRUE) #convert wide to long format
  rd_long<-rbind(rd_long,rdtot)
  
  colnames(rd_long)[which(colnames(rd_long) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(rd_long)
}

#This function calculate abundance of recent dead conditions by transect and taxonomic group
#Transform RD1 and RD2 from long to wide format, remove other site AND TRANSECT level info
#WORK WITH IVOR TO BUILD IN MORE FLEXIBILITY FOR CONDITIONS
Calc_RDabun_Transect<-function(data, grouping_field="S_ORDER"){
scl<-subset(data,S_ORDER=="Scleractinia")

rd1<-dcast(scl, formula=SITEVISITID + SITE+TRANSECT+COLONYID ~ RD1, value.var="RD1",length,fill=0);names(rd1)<-gsub("DZGN","DZGNS",names(rd1),fixed = TRUE)#; rd1<-rd1[,-c(1:3)]
rd2<-dcast(scl, formula=SITEVISITID + SITE+TRANSECT+COLONYID ~ RD2, value.var="RD2",length,fill=0) ;names(rd2)<-gsub("DZGN","DZGNS",names(rd2),fixed = TRUE)#; rd2<-rd2[,-c(1:3)]
rd3<-dcast(scl, formula=SITEVISITID + SITE+TRANSECT+COLONYID ~ GENRD1, value.var="GENRD1",length,fill=0);colnames(rd3)[5:ncol(rd3)]<-paste("All",colnames(rd3[,c(5:ncol(rd3))]),sep="_") 
rd4<-dcast(scl, formula=SITEVISITID + SITE+TRANSECT+COLONYID ~ GENRD2, value.var="GENRD2",length,fill=0);colnames(rd4)[5:ncol(rd4)]<-paste("All",colnames(rd4[,c(5:ncol(rd4))]),sep="_") 

#merge all dataframes together
a<-merge(rd1,rd2,by=c("SITEVISITID","SITE","TRANSECT","COLONYID"),all=TRUE)
b<-merge(a,rd3,by=c("SITEVISITID","SITE","TRANSECT","COLONYID"),all=TRUE)
allrd<-merge(b,rd4,by=c("SITEVISITID","SITE","TRANSECT","COLONYID"),all=TRUE)

allrd<-allrd[,-c(1:3)] #remove all metadata except colonyid

#R will add .x and .y to column names because there are columns that are identically names
# this will remove .x and .y so that we can sum identifically named columns
names(allrd)<-gsub(".x","",names(allrd),fixed = TRUE)
names(allrd)<-gsub(".y","",names(allrd),fixed = TRUE)
head(allrd)

#Sum identically named columns and remove the no data column
allrd2<-as.data.frame(sapply(unique(colnames(allrd)), 
                             function(x) rowSums(allrd[, colnames(allrd) == x, drop = FALSE])));allrd2<-allrd2[,!(colnames(allrd2) =="All_NODATA")]

#merge data with colony level metadata and sum conditions by transect and taxoncode
allrd3<-merge(survey_colony,allrd2, by="COLONYID")
long <- gather(allrd3, RDCond, abun, names(allrd3[20:dim(allrd3)[2]]), factor_key=TRUE) #convert wide to long format by condition
long$GROUP<-long[,grouping_field]
longsum<-ddply(long, .(SITE,SITEVISITID,TRANSECT,GROUP,RDCond), #calc total colonies by taxon and condition
           summarise,
           RDabun=sum(abun))
out1<-ddply(longsum, .(SITE,SITEVISITID,TRANSECT,RDCond), #calc total colonies by condition
           summarise,
           RDabun=sum(RDabun))
out1$GROUP<-"SSSS"; out1 <- out1[c(1,2,3,6,4,5)] #add total colony code
out<-rbind(longsum,out1)
out<-subset(out,RDCond!="NODATA")

colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.

return(out)
}


#This function calculate abundance of conditions conditions by transect and taxonomic group
#Transform RD1 and RD2 from long to wide format, remove other site AND TRANSECT level info
#WORK WITH IVOR TO BUILD IN MORE FLEXIBILITY FOR CONDITIONS
Calc_Condabun_Transect<-function(data, grouping_field="S_ORDER"){
  scl<-subset(data,S_ORDER=="Scleractinia")
  
  c<-dcast(scl, formula=SITEVISITID + SITE+TRANSECT+COLONYID ~ COND, value.var="COND",length,fill=0)
  c$ChronicDZ<-c$PDS+c$FUG+c$SGA
  c <- subset(c, select = -c(NDZ)) #remove columns
  a<-merge(survey_colony,c, by=c("SITE","SITEVISITID","TRANSECT","COLONYID"))
  long <- gather(a, COND, Condabun, names(a[20:dim(a)[2]]), factor_key=TRUE) #convert wide to long format by condition
  
  #merge data with colony level metadata and sum conditions by transect and taxoncode
  long$GROUP<-long[,grouping_field]
  longsum<-ddply(long, .(SITE,SITEVISITID,TRANSECT,GROUP,COND),
             summarise,
             Condabun=sum(Condabun))
  out1<-ddply(longsum, .(SITE,SITEVISITID,TRANSECT,COND), #calc total colonies by condition
              summarise,
              Condabun=sum(Condabun))
  out1$GROUP<-"SSSS"; out1 <- out1[c(1,2,3,6,4,5)] #add total colony code
  out<-rbind(longsum,out1)
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(out)
}


## SITE LEVEL SUMMARY FUNCTIONS #######

#This function calculates colony density at the site scale by first calculating the total survey area (using Calc_SurveyArea_By_Transect) then calcuating colony density
#Use the grouping field to identify whether you want to look at GENUS_CODE or TAXONCODE
#Use the other field to for summarizing data such as colony size bins, morphology, etc. Note: you will need to add a DUMMY column to the dataframe prior to using this funciton.
#If you don't want to pass a variable into the other_field then the function will just spit out a DUMMY collumn that can be removed later. 
Calc_ColDen_Site<-function(data, grouping_field="S_ORDER",other_field="DUMMY"){
  
  trarea<-Calc_SurveyArea_By_Site(data)
  
  scl<-subset(data,S_ORDER=="Scleractinia")
  
  scl$GROUP<-scl[,grouping_field]
  scl$OTHER<-scl[,other_field]
  
  colden<-ddply(scl, .(SITE,SITEVISITID, OTHER,GROUP),
                summarise,
                Colabun=length(COLONYID)) #change to count
  
  colden2<-merge(trarea,colden, by=c("SITE","SITEVISITID"),all.x=TRUE)
  
  colden2$Colabun[is.na(colden2$Colabun)]<-0
  
  colden2$ColDen<-colden2$Colabun/colden2$TRANSECTAREA
  
  cd<-dcast(colden2, formula=SITE + SITEVISITID +OTHER ~ GROUP, value.var="ColDen",fill=0)
  ca<-dcast(colden2, formula=SITE + SITEVISITID +OTHER ~ GROUP, value.var="Colabun",fill=0)

  cd$SSSS<-rowSums(cd[,names(cd[4:dim(cd)[2]]),drop=FALSE]) #calculate total colony density
  cd <- gather(cd, GROUP, ColDen, names(cd[4:dim(cd)[2]]), factor_key=TRUE) #convert wide to long format
  
  ca$SSSS<-rowSums(ca[,names(ca[4:dim(ca)[2]]),drop=FALSE]) #calculate total colony density
  ca <- gather(ca, GROUP, Colabun, names(ca[4:dim(ca)[2]]), factor_key=TRUE) #convert wide to long format
  
  out<-merge(cd,ca, by=c("SITE","SITEVISITID","OTHER","GROUP"))
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  colnames(out)[which(colnames(out) == 'OTHER')] <- other_field #change group to whatever your other field is.
  
  return(out)
}

##This function calculates mean colony legnth, % recent dead, % old dead, condition severity or condition extent to the transect level
Calc_ColMetric_Site<-function(data, grouping_field="S_ORDER",pool_fields=c("COLONYLENGTH","RDEXTENT1", "RDEXTENT2","OLDDEAD","SEVERITY","EXTENT"),other_field="DUMMY"){
  
  scl<-subset(data,S_ORDER=="Scleractinia")
  scl$GROUP<-scl[,grouping_field]
  scl$OTHER<-scl[,other_field]
  scl$y <- rowSums(scl[,pool_fields,drop=FALSE], na.rm=TRUE) #this will allow you to add the 2 recent dead columns if you are looking at this metric
  
  rd<-ddply(scl, .(SITE,SITEVISITID,OTHER,GROUP),
            summarise,
            Ave.y=mean(y, na.rm=TRUE))
  
  rdtot<-ddply(scl, .(SITE,SITEVISITID,OTHER),
               summarise,
               Ave.y=mean(y, na.rm=TRUE))
  rdtot$GROUP<-"SSSS"; rdtot <- rdtot[c(1,2,3,5,4)]
  rd_wide<-dcast(rd, formula=SITE + SITEVISITID +OTHER ~ GROUP, value.var="Ave.y",fill=0)
  rd_long <- gather(rd_wide, GROUP, Ave.y, names(rd_wide[4:dim(rd_wide)[2]]), factor_key=TRUE) #convert wide to long format
  rd_long<-rbind(rd_long,rdtot)
  
  colnames(rd_long)[which(colnames(rd_long) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  colnames(rd_long)[which(colnames(rd_long) == 'OTHER')] <- other_field #change group to whatever your other field is.
  
  return(rd_long)
}

#This function calculate abundance of recent dead conditions by transect and taxonomic group
#Transform RD1 and RD2 from long to wide format, remove other site AND TRANSECT level info
#WORK WITH IVOR TO BUILD IN MORE FLEXIBILITY FOR CONDITIONS
Calc_RDabun_Site<-function(data, grouping_field="S_ORDER"){
  scl<-subset(data,S_ORDER=="Scleractinia")
  
  rd1<-dcast(scl, formula=SITEVISITID + SITE+COLONYID ~ RD1, value.var="RD1",length,fill=0);names(rd1)<-gsub("DZGN","DZGNS",names(rd1),fixed = TRUE)#; rd1<-rd1[,-c(1:3)]
  rd2<-dcast(scl, formula=SITEVISITID + SITE+COLONYID ~ RD2, value.var="RD2",length,fill=0) ;names(rd2)<-gsub("DZGN","DZGNS",names(rd2),fixed = TRUE)#; rd2<-rd2[,-c(1:3)]
  rd3<-dcast(scl, formula=SITEVISITID + SITE+COLONYID ~ GENRD1, value.var="GENRD1",length,fill=0);colnames(rd3)[4:ncol(rd3)]<-paste("All",colnames(rd3[,c(4:ncol(rd3))]),sep="_") 
  rd4<-dcast(scl, formula=SITEVISITID + SITE+COLONYID ~ GENRD2, value.var="GENRD2",length,fill=0);colnames(rd4)[4:ncol(rd4)]<-paste("All",colnames(rd4[,c(4:ncol(rd4))]),sep="_") 
  
  #merge all dataframes together
  a<-merge(rd1,rd2,by=c("SITEVISITID","SITE","COLONYID"),all=TRUE)
  b<-merge(a,rd3,by=c("SITEVISITID","SITE","COLONYID"),all=TRUE)
  allrd<-merge(b,rd4,by=c("SITEVISITID","SITE","COLONYID"),all=TRUE)
  
  allrd<-allrd[,-c(1:2)] #remove all metadata except colonyid
  
  #R will add .x and .y to column names because there are columns that are identically names
  # this will remove .x and .y so that we can sum identifically named columns
  names(allrd)<-gsub(".x","",names(allrd),fixed = TRUE)
  names(allrd)<-gsub(".y","",names(allrd),fixed = TRUE)
  head(allrd)
  
  #Sum identically named columns and remove the no data column
  allrd2<-as.data.frame(sapply(unique(colnames(allrd)), 
                               function(x) rowSums(allrd[, colnames(allrd) == x, drop = FALSE])));allrd2<-allrd2[,!(colnames(allrd2) =="All_NODATA")]
  
  #merge data with colony level metadata and sum conditions by transect and taxoncode
  allrd3<-merge(survey_colony,allrd2, by="COLONYID")
  long <- gather(allrd3, RDCond, abun, names(allrd3[20:dim(allrd3)[2]]), factor_key=TRUE) #convert wide to long format by condition
  long$GROUP<-long[,grouping_field]
  longsum<-ddply(long, .(SITE,SITEVISITID,GROUP,RDCond), #calc total colonies by taxon and condition
                 summarise,
                 RDabun=sum(abun))
  out1<-ddply(longsum, .(SITE,SITEVISITID,RDCond), #calc total colonies by condition
              summarise,
              RDabun=sum(RDabun))
  out1$GROUP<-"SSSS"; out1 <- out1[c(1,2,3,6,4,5)] #add total colony code
  out<-rbind(longsum,out1)
  out<-subset(out,RDCond!="NODATA")
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(out)
}


#This function calculate abundance of conditions conditions by transect and taxonomic group
#Transform RD1 and RD2 from long to wide format, remove other site AND TRANSECT level info
#WORK WITH IVOR TO BUILD IN MORE FLEXIBILITY FOR CONDITIONS
Calc_Condabun_Site<-function(data, grouping_field="S_ORDER"){
  scl<-subset(data,S_ORDER=="Scleractinia")

  c<-dcast(scl, formula=SITEVISITID + SITE+COLONYID ~ COND, value.var="COND",length,fill=0)
  c$ChronicDZ<-c$PDS+c$FUG+c$SGA
  c <- subset(c, select = -c(NDZ)) #remove columns
  a<-merge(survey_colony,c, by=c("SITE","SITEVISITID","COLONYID"))
  long <- gather(a, COND, Condabun, names(a[22:dim(a)[2]]), factor_key=TRUE) #convert wide to long format by condition
  
  #merge data with colony level metadata and sum conditions by transect and taxoncode
  long$GROUP<-long[,grouping_field]
  longsum<-ddply(long, .(SITE,SITEVISITID,GROUP,COND),
                 summarise,
                 Condabun=sum(Condabun))
  out1<-ddply(longsum, .(SITE,SITEVISITID,COND), #calc total colonies by condition
              summarise,
              Condabun=sum(Condabun))
  out1$GROUP<-"SSSS"; out1 <- out1[c(1,2,3,5,4)] #add total colony code
  out<-rbind(longsum,out1)
  
  out<-dcast(out, formula=SITEVISITID +SITE +GROUP~ COND, value.var="Condabun",sum,fill=0)
  head(out)
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(out)
}

## POOLING FUNCTIONS TO GENERATE ESTIMATES #######

#STRATA ROLL UP FUNCTION-This function calculates mean, var, SE and CV at the strata level. I've built in flexilbity to use either genus or taxoncode
# You can input any metric you would like (eg. adult density, mean % old dead,etc). Note that for any metric that does not involve density of colonies, 
# Y._h (total colony abundance in stratum),varY._h (variance in total abundance), SE_Y._h and CV_Y._h are meaningless-DO NOT USE
#Note: for whatever reason, the grouping, other and metric fields need to be in this order. If you don't want to include an other field then add "DUMMY" as the second variable when you are running this function.
#e.g. st<-Calc_Strata(data.mon,"GENUS_CODE","DUMMY","ColDen")

Calc_Strata=function(site_data,grouping_field,other_field="DUMMY",metric_field=c("AdColDen","JuvColDen","Ave.od","Ave.rd"),M_hi=250){
  
  #Build in flexibility to look at genus or taxon level
  site_data$GROUP<-site_data[,grouping_field]
  
  #Build in flexibility to look at other variables (e.g. morphology, size_class)
  site_data$OTHER<-site_data[,other_field]
  
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
  Strata_roll=ddply(site_data,.(ANALYSIS_YEAR,DOMAIN_SCHEMA,ANALYSIS_SCHEMA,OTHER,GROUP),summarize,
                    n_h=length(SITE),# No. of Sites surveyed in a Strata
                    N_h=median(N_h.as,na.rm=T),# Strata Area (as N 50x50 grids)
                    w_h=median(w_h.as,na.rm=T),# weigting factor for a given stratum
                    D._h=mean(METRIC,na.rm=T), # Mean of Site-Level Density in a Stratum
                    S1_h=var(METRIC,na.rm=T), #sample variance in density between sites
                    varD._h=(1-(n_h/N_h))*S1_h/n_h, #Strata level  variance of mean density
                    Y._h=D._h*N_h*250, #total colony abundance in stratum (Mhi=250)
                    varY._h=varD._h*N_h^2, #variance in total abundance 
                    SE_D._h=sqrt(varD._h),
                    CV_D._h=SE_D._h/D._h,
                    SE_Y._h=sqrt(varY._h),
                    CV_Y._h=SE_Y._h/Y._h)
  
  Strata_roll$M_hi=250 #define total possible transects in a site
  Strata_roll=Strata_roll[,c("ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","OTHER","GROUP",
                             "M_hi","n_h","N_h","w_h",
                             "D._h","S1_h","varD._h","SE_D._h","CV_D._h",
                             "Y._h","varY._h","SE_Y._h","CV_Y._h")]
  
  #remove strata that have only 1 site because you can't calculate variance
  Strata_roll<-Strata_roll[Strata_roll$n_h>1,]
  
  colnames(Strata_roll)[which(colnames(Strata_roll) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.

  colnames(Strata_roll)[which(colnames(Strata_roll) == 'OTHER')] <- other_field #change group to whatever your grouping field is.


  return(Strata_roll)
}


#DOMAIN ROLL UP FUNCTION-This function calculates mean, var, SE and CV at the DOMAIN level. I've built in flexilbity to use either genus or taxoncode as well as other metrics (size class, morph)
# You can input any metric you would like (eg. adult density, mean % old dead,etc). Note that for any metric that does not involve density of colonies, 
# Y._h (total colony abundance in stratum),varY._h (variance in total abundance), SE_Y._h and CV_Y._h are meaningless-DO NOT USE
Calc_Domain=function(site_data,grouping_field="S_ORDER",metric_field=c("AdColDen","JuvColDen","Ave.od","Ave.rd"),other_field="DUMMY"){
 
  Strata_data=Calc_Strata(site_data,grouping_field,other_field,metric_field)
  
  #Build in flexibility to look at genus or taxon level
  Strata_data$GROUP<-Strata_data[,grouping_field]
  
  #Build in flexibility to look at other variables (e.g. morphology, size_class)
  Strata_data$OTHER<-Strata_data[,other_field]
  
  DomainStr_NH=ddply(subset(Strata_data,GROUP=="SSSS"),.(ANALYSIS_YEAR,DOMAIN_SCHEMA),summarize,N_h=sum(N_h,na.rm=TRUE)) #total possible sites in a domain
  Strata_data$DomainSumN_h=DomainStr_NH$N_h[match(Strata_data$DOMAIN_SCHEMA,DomainStr_NH$DOMAIN_SCHEMA)] # add previous to strata data
  Strata_data$w_h=Strata_data$N_h/Strata_data$DomainSumN_h
  
  Domain_roll=ddply(Strata_data,.(ANALYSIS_YEAR,DOMAIN_SCHEMA,OTHER,GROUP),summarize,
                    D._st=sum(w_h*D._h,na.rm=TRUE), #Domain weighted estimate (sum of Weighted strata density)
                    varD._st=sum(w_h^2*varD._h,na.rm=TRUE), #Domain weighted variance estimate
                    Y._st=sum(Y._h,na.rm=TRUE), #Domain total abundance (sum of extrapolated strata abundance)
                    varY._st=sum(varY._h,na.rm=TRUE),#Domain variance total abundance (sum of extrapolated strata varaiance abundance)
                    n=sum(n_h,na.rm=TRUE), #total sites surveyed in domain
                    N=sum(N_h,na.rm=TRUE), #total possible sites in domain
                    SE_varD._st=sqrt(varD._st), #SE of domain metric estimate
                    CV_varD._st=SE_varD._st/D._st, #CV of domain metric estimate
                    SE_varY._st=sqrt(varY._st),#SE of domain abundance estimate
                    CV_varY._st=SE_varY._st/Y._st)#CV of domain abundnace estimate
  
  colnames(Domain_roll)[which(colnames(Domain_roll) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  colnames(Domain_roll)[which(colnames(Domain_roll) == 'OTHER')] <- other_field #change group to whatever your grouping field is.
  
  return(Domain_roll)
}





####
#BSR-UNWEIGHTED FUNCTIONS####
####


Calc_Sitemetrics_BSR<-function(data, grouping_field){
  a<-merge(data,survey_transect,by=c("DEPTH_BIN","REEF_ZONE","SITE","SITEVISITID","TRANSECT"))
  a$GROUP<-a[,grouping_field]
  out<-ddply(a, .(OBS_YEAR,REGION,ISLAND,SITE,LATITUDE,LONGITUDE,DEPTH_BIN,GROUP),
                summarise,
                ACD=mean(AdultColDen),
                JCD=mean(JuvColDen),
                BLE=mean(BLEprev),
                DZ=mean(TotDZprev),
                AcuteDZ=mean(AcuteDZprev),
                ChrDZ=mean(ChronicDZprev))
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(out)
}
# 
Calc_Islmetrics_BSR<-function(data, grouping_field="GENUS_CODE"){
  data$GROUP<-data[,grouping_field]
  out<-ddply(data, .(OBS_YEAR,REGION,ISLAND,GROUP),
                summarise,
                meanAdultColDen=mean(ACD),
                meanJuvColDen=mean(JCD),
                meanBLE=mean(BLE),
                meanAcuteDZ=mean(AcuteDZ),
                meanChrDZ=mean(ChrDZ),
                meanDZ=mean(DZ),
                seAdultColDen=se(ACD),
                seJuvColDen=se(JCD),
                seBLE=se(BLE),
                seAcuteDZ=se(AcuteDZ),
                seChrDZ=se(ChrDZ),
                seDZ=se(DZ),
                ntot=length(SITE))
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(out)
}

Calc_IslDepthmetrics_BSR<-function(data, grouping_field="GENUS_CODE"){
  data$GROUP<-data[,grouping_field]
  out<-ddply(data, .(OBS_YEAR,REGION,ISLAND,DEPTH_BIN,GROUP),
             summarise,
             meanAdultColDen=mean(ACD),
             meanJuvColDen=mean(JCD),
             meanBLE=mean(BLE),
             meanAcuteDZ=mean(AcuteDZ),
             meanChrDZ=mean(ChrDZ),
             meanDZ=mean(DZ),
             seAdultColDen=se(ACD),
             seJuvColDen=se(JCD),
             seBLE=se(BLE),
             seAcuteDZ=se(AcuteDZ),
             seChrDZ=se(ChrDZ),
             seDZ=se(DZ),
             ntot=length(SITE))
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(out)
}




