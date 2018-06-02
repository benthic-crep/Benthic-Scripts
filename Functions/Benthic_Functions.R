# reshape library inclues the cast() function used below
library(reshape2)
library(ggplot2) ## to create the diver vs diver graphs
library(data.table)
library(plyr)
library(gdata)
library(tidyr)
library(plotrix)
library(scales)  # for pretty_breaks() function


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

#### Functions for benthic summary metrics (segment-level for inter-diver comparisons) ####

## This function calculates total area per segment. Using median will help you with situations where someone recorded different seg areas by mistake.
Calc_SurveyArea_By_Seg<-function(data) {
  
  tr.df<-ddply(data, .(SITE,SITEVISITID,TRANSECT,SEGMENT),
               summarize,
               segarea=median(SEGAREA))
  
  return(tr.df)
}


## This function calculates colony density at the segment scale by first calculating the total survey area (using Calc_SurveyArea_By_Seg) then calculating colony density
Calc_ColDen_Seg<-function(data, grouping_field="GENUS_CODE"){
  
  data$GROUP<-data[,grouping_field] #assign a grouping field for taxa
  
  #Calculate # of colonies for each variable. You need to have S_ORDER and Fragment here so you can incorporate zeros properly
  a<-ddply(data, .(DATE_,SITE,SITEVISITID,TRANSECT,SEGMENT,DIVER,S_ORDER,GROUP,Fragment),
           summarise,
           ColCount=length(COLONYID)) #change to count
  
  #Convert from long to wide and insert 0s for taxa that weren't found at each site. 
  ca<-dcast(a, formula=DATE_ + SITE + SITEVISITID + TRANSECT + SEGMENT + DIVER + Fragment + S_ORDER ~ GROUP, value.var="ColCount",fill=0)
  data.cols<-names(ca[9:dim(ca)[2]]) #define your data columns
  #SEGMENT is the base unit .. so pool up colony density at segment level, for the field of interest
  field.cols<-c("DATE_","SITE", "SITEVISITID", "TRANSECT","SEGMENT","DIVER","Fragment") #define field columns; minimum set of fields to build up from
  
  #change colony counts for fragments to 0 so that we account for the transects that only had fragments
  ca[which(ca$Fragment < 0), data.cols] <- 0
  
  #At this point you will have multiple rows for each site/transect/segment so sum data by site, transect and segment. This will help you properly insert 0s
  field.cols<-c("DATE_", "SITE", "SITEVISITID", "TRANSECT","SEGMENT","DIVER")
  ca<-aggregate(ca[,data.cols], by=ca[,field.cols], sum) 
  
  #Create a list of scleractinian taxa that are in the dataframe as columns then sum across just those taxa to get total scl
  b<-subset(data, S_ORDER=="Scleractinia"); taxalist<-as.character(unique(b$GROUP))
  ca$SSSS<-rowSums(ca[,taxalist,drop=FALSE])  # calculate total colony density
  ca <- gather(ca, GROUP, ColCount, names(ca[7:dim(ca)[2]]), factor_key=TRUE)  # convert wide to long format
  
  #Remove everything that isn't a scleractinian
  taxalist2<-c(taxalist,"SSSS")
  ca<-ca[ca$GROUP %in% taxalist2,]
  
  #Calculate segment area surveyed and colony density
  sgarea<-Calc_SurveyArea_By_Seg(data)
  out<-merge(sgarea,ca, by=c("SITE","SITEVISITID","TRANSECT","SEGMENT"),all.x=TRUE)
  out$ColDen<-out$ColCount/out$segarea
  out<-subset(out,select=-c(segarea))  # remove segment area column
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  return(out)
}


## This function calculates mean colony length, % recent dead, % old dead, condition severity or condition extent to the segment level
## NOTE: can run both adult & juvenile data with this function for COLONYLENGTH
Calc_ColMetric_Seg<-function(data, grouping_field="GENUS_CODE", pool_fields=c("COLONYLENGTH","RDEXTENT1", "RDEXTENT2","OLDDEAD","SEVERITY","EXTENT")) {
  
  scl<-subset(data, Fragment >= 0 & S_ORDER == "Scleractinia")  # Note: Calc_ColMetric_Transect() function has COLONYLENGTH>5 but changed to Fragment>=0 so can run juvenile data w/ this function ( for colony length)
  scl$GROUP<-scl[,grouping_field]
  scl$y <- rowSums(scl[,pool_fields,drop=FALSE], na.rm=TRUE) #this will allow you to add the 2 recent dead columns if you are looking at this metric
  
  rd<-ddply(scl, .(DATE_,SITE,SITEVISITID,TRANSECT,SEGMENT,DIVER,GROUP),
            summarise,
            Ave.y=mean(y, na.rm=TRUE))
  
  rdtot<-ddply(scl, .(DATE_,SITE,SITEVISITID,TRANSECT,SEGMENT,DIVER),
               summarise,
               Ave.y=mean(y, na.rm=TRUE))
  
  rdtot$GROUP<-"SSSS"; rdtot <- rdtot[c(1,2,3,4,5,6,8,7)]
  #rd_wide<-dcast(rd, formula=SITE + SITEVISITID +OTHER +TRANSECT~ GROUP, value.var="Ave.y",fill=0)
  #rd_long <- gather(rd_wide, GROUP, Ave.y, names(rd_wide[5:dim(rd_wide)[2]]), factor_key=TRUE) #convert wide to long format
  rd_long<-rbind(rd,rdtot)
  
  colnames(rd_long)[which(colnames(rd_long) == 'GROUP')] <- grouping_field  # change group to whatever your grouping field is.
  colnames(rd_long)[which(colnames(rd_long) == 'Ave.y')] <- paste("Avg", pool_fields, sep="")  # change Ave.y to whatever your pooling field is.
  
  return(rd_long)
}


## This function calculate abundance of recent dead conditions by segment/diver and taxonomic group
## Transform RD1 and RD2 from long to wide format
Calc_RDabun_Segment<-function(data, grouping_field="GENUS_CODE"){
  scl<-subset(data,COLONYLENGTH>5 & S_ORDER=="Scleractinia")
  
  # add an "S" to the general cause code so that we distinguish it from specific cause codes
  scl$GENRD1<-paste(scl$GENRD1,"S",sep=""); scl$GENRD2<-paste(scl$GENRD2,"S",sep="")
  
  # collapse all general and specific cause code columns into 1 column so that we can count up # of colonies with each condition
  long <- gather(scl, RDcat, RDtype, c(GENRD1,RD1,GENRD2,RD2), factor_key=TRUE)
  
  # convert from long to wide and fill in 0s
  rd<-dcast(long, formula=DATE_ + SITE + SITEVISITID + TRANSECT + SEGMENT + DIVER + COLONYID ~ RDtype, value.var="RDtype", length, fill=0)
  
  d <-  rd[,-c(1:7)]  # remove all metadata
  d[d>1] <- 1  # Change values greater than 1 to 1 so that you don't double count colonies
  meta <- rd[,c(1:7)]  # Subset just metadata
  rd.new <- cbind(meta,d)  # combine metadata back with data
  
  # merge data with colony level metadata and sum conditions by segment/diver and taxoncode
  allrd3<-merge(survey_colony,rd.new, by=c("DATE_","SITEVISITID","SITE","TRANSECT","SEGMENT","DIVER","COLONYID"))
  long <- gather(allrd3, RDCond, abun, names(allrd3[23:dim(allrd3)[2]]), factor_key=TRUE) #convert wide to long format by condition
  long$GROUP<-long[,grouping_field]
  longsum<-ddply(long, .(DATE_,SITE,SITEVISITID,TRANSECT,SEGMENT,DIVER,GROUP,RDCond), #calc total colonies by taxon and condition
                 summarise,
                 RDabun=sum(abun))
  out1<-ddply(longsum, .(DATE_,SITE,SITEVISITID,TRANSECT,SEGMENT,DIVER,RDCond), #calc total colonies by condition
              summarise,
              RDabun=sum(RDabun))
  out1$GROUP<-"SSSS"; out1 <- out1[c(1:6,9,7,8)] #add total colony code
  a<-rbind(longsum,out1)
  a<-subset(a,!RDCond %in% c("NONES","NONE"))
  
  #Convert back to wide format
  out<-dcast(a, formula= DATE_ + SITEVISITID + SITE + TRANSECT + SEGMENT + DIVER + GROUP ~ RDCond, value.var="RDabun",sum,fill=0)
  colnames(out)[colnames(out)=="DZGNS"]<-"AcuteDZ" #rename column 
  
  colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  cd<-Calc_ColDen_Seg(data,grouping_field)[,-c(9)]
  out2<-merge(out,cd,by=c("DATE_","SITEVISITID","SITE","TRANSECT","SEGMENT","DIVER",grouping_field))
  
  return(out2)
}


## This function calculate ABUNDANCE of conditions by segment/diver and taxonomic group
Calc_Condabun_Segment<-function(data, grouping_field="GENUS_CODE") {
  # Remove scleractinan adult colony fragments
  scl<-subset(data,COLONYLENGTH>5&S_ORDER=="Scleractinia")
  
  c<-dcast(scl, formula= DATE_ + SITE + SITEVISITID + TRANSECT + SEGMENT + DIVER + COLONYID ~ COND, value.var="COND",length,fill=0)
  c <- subset(c, select = -c(NDZ)) #remove columns
  a<-merge(survey_colony,c, by=c("DATE_","SITEVISITID","SITE","TRANSECT","SEGMENT","DIVER","COLONYID"))
  long <- gather(a, COND, Condabun, names(a[23:dim(a)[2]]), factor_key=TRUE) #convert wide to long format by condition
  
  # merge data with colony level metadata and sum conditions by segment/diver and taxoncode
  long$GROUP<-long[,grouping_field]
  longsum<-ddply(long, .(DATE_,SITE,SITEVISITID,TRANSECT,SEGMENT,DIVER,GROUP,COND),
                 summarise,
                 Condabun=sum(Condabun))
  out1<-ddply(longsum, .(DATE_,SITE,SITEVISITID,TRANSECT,SEGMENT,DIVER,COND), #calc total colonies by condition
              summarise,
              Condabun=sum(Condabun))
  out1$GROUP<-"SSSS"; out1 <- out1[c(1:6,9,7,8)] #add total colony code
  a<-rbind(longsum,out1)
  a<-subset(a,COND!="NONE")
  
  # convert wide to long
  abun<-dcast(a, formula=DATE_ + SITEVISITID + SITE + TRANSECT + SEGMENT + DIVER + GROUP ~ COND, value.var="Condabun",sum,fill=0)
  
  colnames(abun)[which(colnames(abun) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.
  
  cd<-Calc_ColDen_Seg(data,grouping_field)[,-c(9)]
  out<-merge(abun,cd,by=c("DATE_","SITEVISITID","SITE","TRANSECT","SEGMENT","DIVER",grouping_field))
  
  return(out)
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
                    CV_Y._h=SE_Y._h/Y._h,
                    Adprop.occur=sum(Adpres.abs)/n_h,
                    Juvprop.occur=sum(Juvpres.abs)/n_h)
  
  Strata_roll$M_hi=250 #define total possible transects in a site
  Strata_roll=Strata_roll[,c("ANALYSIS_YEAR","DOMAIN_SCHEMA","ANALYSIS_SCHEMA","GROUP",
                             "M_hi","n_h","N_h","w_h",
                             "D._h","S1_h","varD._h","SE_D._h","CV_D._h",
                             "Y._h","varY._h","SE_Y._h","CV_Y._h","Adprop.occur","Juvprop.occur")]
  
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
                    nstrat=length(n_h), #total number of strata in a domain
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



### DIVERVSDIVER FUNCTION: ADULTS + JUVS ###

# May need to add 'x_range' within function argument list to manipulate x-axis on plots if outliers too big (will need to turn on scale_y_continuous() in each ggplot within function)
# For conditions, divervsdiver function currently plots for: BLEprev, TotDZprev, AcuteDZprev, ChronicDZprev, COTSprev. If want to look at additional conditions, will need to add these levels within divervsdiver function to plot

divervsdiver<-function(data, date1, date2, date3) {  
  #data<-compdata.all
  #date1-3<-"2015-02-26","2015-02-27","2015-02-28"  
  #x_range<-10
  x<-data[data$DATE_ == date1,] ## select date
  y<-data[data$DATE_ == date2,] ## select date
  z<-data[data$DATE_ == date3,] ## select date
  size_comp<-rbind(x,y,z)
  size_comp<-droplevels(size_comp)
  
  divers<-levels(size_comp$DIVER)
  ######## diver vs diver: ADULT coral colony density ########
  
  scr<-size_comp[,c("SITEVISITID", "DIVER", "ColDen.ad")] ### stripped down
  msd<-data.frame(with(scr, tapply(ColDen.ad, list(SITEVISITID, DIVER), mean))) ## mean colony density estimate per diver per site
  
  thenas<-data.frame(is.na(msd)) ## id all the locations of nas in dataset
  
  toplot<-list()
  
  for(i in 1:length(divers)){
    ##i<-(divers)[7]
    diver<-which(thenas[,i] == FALSE)
    
    ##select out the sites for diver from the dataframe
    diverdata<-msd[diver,i]
    
    ## get the other diver estimates per site (take mean b/c sometimes more than one other diver at a site)
    otherdiver<-rowMeans(msd[diver, !(colnames(msd) %in% divers[i])], na.rm = TRUE)
    
    ## for diver 1 create a site level colony density ratio, the density of diver 1 / the density of the other divers
    x<-as.vector(diverdata - otherdiver)
    toplot[[i]]<-x
    
  }
  
  toplot<-(melt(toplot))
  
  names(toplot)<-c("colonydensity_ratio", "diver")
  toplot$diver<-as.factor(toplot$diver)
  
  # Plot difference in density ratio between divers
  dens <- ggplot(toplot, aes(factor(diver), colonydensity_ratio))
  dens <- dens + 
    geom_boxplot() + 
    coord_flip() + 
    geom_jitter(colour = "red", size =1, alpha = 0.4) + 
    theme_bw() +
    #scale_y_continuous(limits=c(-x_range,x_range))+
    labs(y = expression(paste("Difference in adult colony density (count/ ", m^-2,") relative to buddy")), x = "Diver") + 
    scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  suppressMessages(ggsave(filename =paste("divervsdiver_adultcolonydensity_",date1,",",substr(date2, 9, 10),",",substr(date3, 9, 10), ".png", sep = ""), width=13, height = 14.0, units = c("cm")))
  
  ######## diver vs diver: ADULT colony size ########
  
  scr<-size_comp[,c("SITEVISITID", "DIVER", "AvgCOLONYLENGTH.ad")] ### stripped down
  msd<-data.frame(with(scr, tapply(AvgCOLONYLENGTH.ad, list(SITEVISITID, DIVER), mean))) ## mean size estimate per diver per site
  
  thenas<-data.frame(is.na(msd)) ## id all the locations of nas in dataset
  
  toplot<-list()
  
  for(i in 1:length(divers)){
    ##i<-(divers)[7]
    diver<-which(thenas[,i] == FALSE)
    
    ##select out the sites for diver from the dataframe
    diverdata<-msd[diver,i]
    
    ## get the other diver estimates per site (take mean b/c sometimes more than one other diver at a site)
    otherdiver<-rowMeans(msd[diver, !(colnames(msd) %in% divers[i])], na.rm = TRUE)
    
    ## for diver 1 create a site level size ratio, the size of diver 1 / the size of the other divers
    x<-as.vector(diverdata - otherdiver)
    toplot[[i]]<-x
    
  }
  
  toplot<-(melt(toplot))
  
  names(toplot)<-c("size_ratio", "diver")
  toplot$diver<-as.factor(toplot$diver)
  
  # Plot difference in size ratio between divers
  size <- ggplot(toplot, aes(factor(diver), size_ratio))
  size <- size + 
    geom_boxplot() + 
    coord_flip() + 
    geom_jitter(colour = "red", size =1, alpha = 0.4) + 
    theme_bw() +
    #scale_y_continuous(limits=c(-x_range,x_range))+
    labs(y = expression(paste("Difference in average adult size (cm) relative to buddy")), x = "Diver") + 
    scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  suppressMessages(ggsave(filename =paste("divervsdiver_adultsize_",date1,",",substr(date2, 9, 10),",",substr(date3, 9, 10), ".png", sep = ""), width=13, height = 14.0, units = c("cm")))
  
  ######## diver vs diver: ADULT old dead ########
  
  scr<-size_comp[,c("SITEVISITID", "DIVER", "AvgOLDDEAD.ad")] ### stripped down
  msd<-data.frame(with(scr, tapply(AvgOLDDEAD.ad, list(SITEVISITID, DIVER), mean))) ## mean old dead estimate per diver per site
  
  thenas<-data.frame(is.na(msd)) ## id all the locations of nas in dataset
  
  toplot<-list()
  
  for(i in 1:length(divers)){
    ##i<-(divers)[7]
    diver<-which(thenas[,i] == FALSE)
    
    ##select out the sites for diver from the dataframe
    diverdata<-msd[diver,i]
    
    ## get the other diver estimates per site (take mean b/c sometimes more than one other diver at a site)
    otherdiver<-rowMeans(msd[diver, !(colnames(msd) %in% divers[i])], na.rm = TRUE)
    
    ## for diver 1 create a site level old dead ratio, the old dead of diver 1 / the old dead of the other divers
    x<-as.vector(diverdata - otherdiver)
    toplot[[i]]<-x
    
  }
  
  toplot<-(melt(toplot))
  
  names(toplot)<-c("olddead_ratio", "diver")
  toplot$diver<-as.factor(toplot$diver)
  
  # Plot difference in old dead ratio between divers
  od <- ggplot(toplot, aes(factor(diver), olddead_ratio))
  od <- od + 
    geom_boxplot() + 
    coord_flip() + 
    geom_jitter(colour = "red", size =1, alpha = 0.4) + 
    theme_bw() +
    #scale_y_continuous(limits=c(-x_range,x_range))+
    labs(y = expression(paste("Difference in average old dead (%) relative to buddy")), x = "Diver") + 
    scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  suppressMessages(ggsave(filename =paste("divervsdiver_olddead_",date1,",",substr(date2, 9, 10),",",substr(date3, 9, 10), ".png", sep = ""), width=13, height = 14.0, units = c("cm")))
  
  ######## diver vs diver: ADULT recent dead ########
  
  scr<-size_comp[,c("SITEVISITID", "DIVER", "AvgRDEXTENT1.ad")] ### stripped down
  msd<-data.frame(with(scr, tapply(AvgRDEXTENT1.ad, list(SITEVISITID, DIVER), mean))) ## mean recent dead estimate per diver per site
  
  thenas<-data.frame(is.na(msd)) ## id all the locations of nas in dataset
  
  toplot<-list()
  
  for(i in 1:length(divers)){
    ##i<-(divers)[7]
    diver<-which(thenas[,i] == FALSE)
    
    ##select out the sites for diver from the dataframe
    diverdata<-msd[diver,i]
    
    ## get the other diver estimates per site (take mean b/c sometimes more than one other diver at a site)
    otherdiver<-rowMeans(msd[diver, !(colnames(msd) %in% divers[i])], na.rm = TRUE)
    
    ## for diver 1 create a site level recent dead ratio, the recent dead of diver 1 / the recent dead of the other divers
    x<-as.vector(diverdata - otherdiver)
    toplot[[i]]<-x
    
  }
  
  toplot<-(melt(toplot))
  
  names(toplot)<-c("rctdead_ratio", "diver")
  toplot$diver<-as.factor(toplot$diver)
  
  # Plot difference in recent dead ratio between divers
  rd <- ggplot(toplot, aes(factor(diver), rctdead_ratio))
  rd <- rd + 
    geom_boxplot() + 
    coord_flip() + 
    geom_jitter(colour = "red", size =1, alpha = 0.4) + 
    theme_bw() +
    #scale_y_continuous(limits=c(-x_range,x_range))+
    labs(y = expression(paste("Difference in average recent dead (%) relative to buddy")), x = "Diver") + 
    scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  suppressMessages(ggsave(filename =paste("divervsdiver_recentdead_",date1,",",substr(date2, 9, 10),",",substr(date3, 9, 10), ".png", sep = ""), width=13, height = 14.0, units = c("cm")))
  
  
  ######## diver vs diver: JUV coral colony density ########
  
  scr<-size_comp[,c("SITEVISITID", "DIVER", "ColDen.juv")] ### stripped down
  msd<-data.frame(with(scr, tapply(ColDen.juv, list(SITEVISITID, DIVER), mean))) ## mean colony density estimate per diver per site
  
  thenas<-data.frame(is.na(msd)) ## id all the locations of nas in dataset
  
  toplot<-list()
  
  for(i in 1:length(divers)){
    ##i<-(divers)[7]
    diver<-which(thenas[,i] == FALSE)
    
    ##select out the sites for diver from the dataframe
    diverdata<-msd[diver,i]
    
    ## get the other diver estimates per site (take mean b/c sometimes more than one other diver at a site)
    otherdiver<-rowMeans(msd[diver, !(colnames(msd) %in% divers[i])], na.rm = TRUE)
    
    ## for diver 1 create a site level colony density ratio, the density of diver 1 / the density of the other divers
    x<-as.vector(diverdata - otherdiver)
    toplot[[i]]<-x
    
  }
  
  toplot<-(melt(toplot))
  
  names(toplot)<-c("colonydensity_ratio", "diver")
  toplot$diver<-as.factor(toplot$diver)
  
  # Plot difference in density ratio between divers
  dens <- ggplot(toplot, aes(factor(diver), colonydensity_ratio))
  dens <- dens + 
    geom_boxplot() + 
    coord_flip() + 
    geom_jitter(colour = "red", size =1, alpha = 0.4) + 
    theme_bw() +
    #scale_y_continuous(limits=c(-x_range,x_range))+
    labs(y = expression(paste("Difference in juv colony density (count/ ", m^-2,") relative to buddy")), x = "Diver") + 
    scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  suppressMessages(ggsave(filename =paste("divervsdiver_juvcolonydensity_",date1,",",substr(date2, 9, 10),",",substr(date3, 9, 10), ".png", sep = ""), width=13, height = 14.0, units = c("cm")))
  
  
  ######## diver vs diver: JUV colony size ########
  
  scr<-size_comp[,c("SITEVISITID", "DIVER", "AvgCOLONYLENGTH.juv")] ### stripped down
  msd<-data.frame(with(scr, tapply(AvgCOLONYLENGTH.juv, list(SITEVISITID, DIVER), mean))) ## mean size estimate per diver per site
  
  thenas<-data.frame(is.na(msd)) ## id all the locations of nas in dataset
  
  toplot<-list()
  
  for(i in 1:length(divers)){
    ##i<-(divers)[7]
    diver<-which(thenas[,i] == FALSE)
    
    ##select out the sites for diver from the dataframe
    diverdata<-msd[diver,i]
    
    ## get the other diver estimates per site (take mean b/c sometimes more than one other diver at a site)
    otherdiver<-rowMeans(msd[diver, !(colnames(msd) %in% divers[i])], na.rm = TRUE)
    
    ## for diver 1 create a site level size ratio, the size of diver 1 / the size of the other divers
    x<-as.vector(diverdata - otherdiver)
    toplot[[i]]<-x
    
  }
  
  toplot<-(melt(toplot))
  
  names(toplot)<-c("size_ratio", "diver")
  toplot$diver<-as.factor(toplot$diver)
  
  # Plot difference in size ratio between divers
  size <- ggplot(toplot, aes(factor(diver), size_ratio))
  size <- size + 
    geom_boxplot() + 
    coord_flip() + 
    geom_jitter(colour = "red", size =1, alpha = 0.4) + 
    theme_bw() +
    #scale_y_continuous(limits=c(-x_range,x_range))+
    labs(y = expression(paste("Difference in average juv size (cm) relative to buddy")), x = "Diver") + 
    scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  suppressMessages(ggsave(filename =paste("divervsdiver_juvsize_",date1,",",substr(date2, 9, 10),",",substr(date3, 9, 10),".png", sep = ""), width=13, height = 14.0, units = c("cm")))
  
  
  ######## diver vs diver: BLE prevalence ########
  
  scr<-size_comp[,c("SITEVISITID", "DIVER", "BLEprev")] ### stripped down
  msd<-data.frame(with(scr, tapply(BLEprev, list(SITEVISITID, DIVER), mean))) ## mean BLE prevalence estimate per diver per site
  
  thenas<-data.frame(is.na(msd)) ## id all the locations of nas in dataset
  
  toplot<-list()
  
  for(i in 1:length(divers)){
    ##i<-(divers)[7]
    diver<-which(thenas[,i] == FALSE)
    
    ##select out the sites for diver from the dataframe
    diverdata<-msd[diver,i]
    
    ## get the other diver estimates per site (take mean b/c sometimes more than one other diver at a site)
    otherdiver<-rowMeans(msd[diver, !(colnames(msd) %in% divers[i])], na.rm = TRUE)
    
    ## for diver 1 create a site level size ratio, the size of diver 1 / the size of the other divers
    x<-as.vector(diverdata - otherdiver)
    toplot[[i]]<-x
    
  }
  
  toplot<-(melt(toplot))
  
  names(toplot)<-c("BLE", "diver")
  toplot$diver<-as.factor(toplot$diver)
  
  # Plot difference in BLE ratio between divers
  ble <- ggplot(toplot, aes(factor(diver), BLE))
  ble <- ble + 
    geom_boxplot() + 
    coord_flip() + 
    geom_jitter(colour = "red", size =1, alpha = 0.4) + 
    theme_bw() +
    #scale_y_continuous(limits=c(-x_range,x_range))+
    labs(y = expression(paste("Difference in average BLE prevalence relative to buddy")), x = "Diver") + 
    scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  suppressMessages(ggsave(filename =paste("divervsdiver_BLEprev_",date1,",",substr(date2, 9, 10),",",substr(date3, 9, 10),".png", sep = ""), width=13, height = 14.0, units = c("cm")))
  
  
  ######## diver vs diver: Tot DZ prevalence ########
  
  scr<-size_comp[,c("SITEVISITID", "DIVER", "TotDZprev")] ### stripped down
  msd<-data.frame(with(scr, tapply(TotDZprev, list(SITEVISITID, DIVER), mean))) ## mean Tot DZ prevalence estimate per diver per site
  
  thenas<-data.frame(is.na(msd)) ## id all the locations of nas in dataset
  
  toplot<-list()
  
  for(i in 1:length(divers)){
    ##i<-(divers)[7]
    diver<-which(thenas[,i] == FALSE)
    
    ##select out the sites for diver from the dataframe
    diverdata<-msd[diver,i]
    
    ## get the other diver estimates per site (take mean b/c sometimes more than one other diver at a site)
    otherdiver<-rowMeans(msd[diver, !(colnames(msd) %in% divers[i])], na.rm = TRUE)
    
    ## for diver 1 create a site level size ratio, the size of diver 1 / the size of the other divers
    x<-as.vector(diverdata - otherdiver)
    toplot[[i]]<-x
    
  }
  
  toplot<-(melt(toplot))
  
  names(toplot)<-c("TotDZ", "diver")
  toplot$diver<-as.factor(toplot$diver)
  
  # Plot difference in Tot DZ ratio between divers
  dz <- ggplot(toplot, aes(factor(diver), TotDZ))
  dz <- dz + 
    geom_boxplot() + 
    coord_flip() + 
    geom_jitter(colour = "red", size =1, alpha = 0.4) + 
    theme_bw() +
    #scale_y_continuous(limits=c(-x_range,x_range))+
    labs(y = expression(paste("Difference in average tot DZ prevalence relative to buddy")), x = "Diver") + 
    scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  suppressMessages(ggsave(filename =paste("divervsdiver_TotDZprev_",date1,",",substr(date2, 9, 10),",",substr(date3, 9, 10),".png", sep = ""), width=13, height = 14.0, units = c("cm")))
  
  
  ######## diver vs diver: Acute DZ prevalence ########
  
  scr<-size_comp[,c("SITEVISITID", "DIVER", "AcuteDZprev")] ### stripped down
  msd<-data.frame(with(scr, tapply(AcuteDZprev, list(SITEVISITID, DIVER), mean))) ## mean Acute DZ prevalence estimate per diver per site
  
  thenas<-data.frame(is.na(msd)) ## id all the locations of nas in dataset
  
  toplot<-list()
  
  for(i in 1:length(divers)){
    ##i<-(divers)[7]
    diver<-which(thenas[,i] == FALSE)
    
    ##select out the sites for diver from the dataframe
    diverdata<-msd[diver,i]
    
    ## get the other diver estimates per site (take mean b/c sometimes more than one other diver at a site)
    otherdiver<-rowMeans(msd[diver, !(colnames(msd) %in% divers[i])], na.rm = TRUE)
    
    ## for diver 1 create a site level size ratio, the size of diver 1 / the size of the other divers
    x<-as.vector(diverdata - otherdiver)
    toplot[[i]]<-x
    
  }
  
  toplot<-(melt(toplot))
  
  names(toplot)<-c("AcuteDZ", "diver")
  toplot$diver<-as.factor(toplot$diver)
  
  # Plot difference in Acute DZ ratio between divers
  adz <- ggplot(toplot, aes(factor(diver), AcuteDZ))
  adz <- adz + 
    geom_boxplot() + 
    coord_flip() + 
    geom_jitter(colour = "red", size =1, alpha = 0.4) + 
    theme_bw() +
    #scale_y_continuous(limits=c(-x_range,x_range))+
    labs(y = expression(paste("Difference in average acute DZ prevalence relative to buddy")), x = "Diver") + 
    scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  suppressMessages(ggsave(filename =paste("divervsdiver_AcuteDZprev_",date1,",",substr(date2, 9, 10),",",substr(date3, 9, 10),".png", sep = ""), width=13, height = 14.0, units = c("cm")))
  
  
  ######## diver vs diver: Chronic DZ prevalence ########
  
  scr<-size_comp[,c("SITEVISITID", "DIVER", "ChronicDZprev")] ### stripped down
  msd<-data.frame(with(scr, tapply(ChronicDZprev, list(SITEVISITID, DIVER), mean))) ## mean Chronic DZ prevalence estimate per diver per site
  
  thenas<-data.frame(is.na(msd)) ## id all the locations of nas in dataset
  
  toplot<-list()
  
  for(i in 1:length(divers)){
    ##i<-(divers)[7]
    diver<-which(thenas[,i] == FALSE)
    
    ##select out the sites for diver from the dataframe
    diverdata<-msd[diver,i]
    
    ## get the other diver estimates per site (take mean b/c sometimes more than one other diver at a site)
    otherdiver<-rowMeans(msd[diver, !(colnames(msd) %in% divers[i])], na.rm = TRUE)
    
    ## for diver 1 create a site level size ratio, the size of diver 1 / the size of the other divers
    x<-as.vector(diverdata - otherdiver)
    toplot[[i]]<-x
    
  }
  
  toplot<-(melt(toplot))
  
  names(toplot)<-c("ChronicDZ", "diver")
  toplot$diver<-as.factor(toplot$diver)
  
  # Plot difference in Chronic DZ ratio between divers
  cdz <- ggplot(toplot, aes(factor(diver), ChronicDZ))
  cdz <- cdz + 
    geom_boxplot() + 
    coord_flip() + 
    geom_jitter(colour = "red", size =1, alpha = 0.4) + 
    theme_bw() +
    #scale_y_continuous(limits=c(-x_range,x_range))+
    labs(y = expression(paste("Difference in average chronic DZ prevalence relative to buddy")), x = "Diver") + 
    scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  suppressMessages(ggsave(filename =paste("divervsdiver_chronicDZprev_",date1,",",substr(date2, 9, 10),",",substr(date3, 9, 10),".png", sep = ""), width=13, height = 14.0, units = c("cm")))
  
  
  ######## diver vs diver: COTS prevalence ########
  
  scr<-size_comp[,c("SITEVISITID", "DIVER", "COTSprev")] ### stripped down
  msd<-data.frame(with(scr, tapply(COTSprev, list(SITEVISITID, DIVER), mean))) ## mean COTS prevalence estimate per diver per site
  
  thenas<-data.frame(is.na(msd)) ## id all the locations of nas in dataset
  
  toplot<-list()
  
  for(i in 1:length(divers)){
    ##i<-(divers)[7]
    diver<-which(thenas[,i] == FALSE)
    
    ##select out the sites for diver from the dataframe
    diverdata<-msd[diver,i]
    
    ## get the other diver estimates per site (take mean b/c sometimes more than one other diver at a site)
    otherdiver<-rowMeans(msd[diver, !(colnames(msd) %in% divers[i])], na.rm = TRUE)
    
    ## for diver 1 create a site level size ratio, the size of diver 1 / the size of the other divers
    x<-as.vector(diverdata - otherdiver)
    toplot[[i]]<-x
    
  }
  
  toplot<-(melt(toplot))
  
  names(toplot)<-c("COTS", "diver")
  toplot$diver<-as.factor(toplot$diver)
  
  # Plot difference in COTSprev ratio between divers
  cots <- ggplot(toplot, aes(factor(diver), COTS))
  cots <- cots + 
    geom_boxplot() + 
    coord_flip() + 
    geom_jitter(colour = "red", size =1, alpha = 0.4) + 
    theme_bw() +
    #scale_y_continuous(limits=c(-x_range,x_range))+
    labs(y = expression(paste("Difference in average COTS prevalence relative to buddy")), x = "Diver") + 
    scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  suppressMessages(ggsave(filename =paste("divervsdiver_COTSprev_",date1,",",substr(date2, 9, 10),",",substr(date3, 9, 10),".png", sep = ""), width=13, height = 14.0, units = c("cm")))
  
}        ## divervsdiver 





