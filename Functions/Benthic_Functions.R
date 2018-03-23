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
  
  tr.df<-ddply(data, .(DEPTH_BIN,REEF_ZONE,SITE,SITEVISITID,TRANSECT),
               summarise,
               TRANSECTAREA=unique(TRANSECTAREA))

  return(tr.df)
}


#This function calculates total area surveyed per site
Calc_SurveyArea_By_Site<-function(data){
  
  tr.df<-ddply(data, .(SITE,TRANSECT,SITEVISITID),
               summarise,
               TRANSECTAREA=unique(TRANSECTAREA))
  
  tr.df2<-ddply(tr.df, .(SITE,SITEVISITID),
                summarise,
                TRANSECTAREA=sum(TRANSECTAREA))
  return(tr.df2)
}



#This function calculates colony density at the transect scale by first calculating the total survey area (using Calc_SurveyArea_By_Transect) then calcuating colony density
Calc_ColDen_Transect<-function(data, grouping_field="S_ORDER"){

trarea<-Calc_SurveyArea_By_Transect(data)

scl<-subset(data,S_ORDER=="Scleractinia")

scl$GROUP<-scl[,grouping_field]
colden<-ddply(scl, .(DEPTH_BIN,REEF_ZONE,SITE,SITEVISITID,TRANSECT, GROUP),
              summarise,
              Colabun=length(COLONYID)) #change to count

colden2<-merge(trarea,colden, by=c("DEPTH_BIN","REEF_ZONE","SITE","SITEVISITID","TRANSECT"),all.x=TRUE)

colden2$Colabun[is.na(colden2$Colabun)]<-0

colden2$ColDen<-colden2$Colabun/colden2$TRANSECTAREA

cd<-dcast(colden2, formula=DEPTH_BIN+REEF_ZONE+SITE + SITEVISITID + TRANSECT ~ GROUP, value.var="ColDen",fill=0)
ca<-dcast(colden2, formula=DEPTH_BIN+REEF_ZONE+SITE + SITEVISITID + TRANSECT ~ GROUP, value.var="Colabun",fill=0)
cd<-cd[,-grep("NA",colnames(cd))] #remove NA column
ca<-ca[,-grep("NA",colnames(ca))] #remove NA column

cd$SSSS<-rowSums(cd[,names(cd[6:dim(cd)[2]])]) #calculate total colony density
cd <- gather(cd, GROUP, ColDen, names(cd[6:dim(cd)[2]]), factor_key=TRUE) #convert wide to long format

ca$SSSS<-rowSums(ca[,names(ca[6:dim(ca)[2]])]) #calculate total colony density
ca <- gather(ca, GROUP, Colabun, names(ca[6:dim(ca)[2]]), factor_key=TRUE) #convert wide to long format

out<-merge(cd,ca, by=c("DEPTH_BIN","REEF_ZONE","SITE","SITEVISITID","TRANSECT","GROUP"))

colnames(out)[which(colnames(out) == 'GROUP')] <- grouping_field #change group to whatever your grouping field is.

return(out)
}


##This function calculates mean % old dead
Calc_olddead_Transect<-function(data, grouping_field="S_ORDER"){
  
  scl<-subset(data,S_ORDER=="Scleractinia")
  scl$GROUP<-scl[,grouping_field]
  od<-ddply(scl, .(DEPTH_BIN,REEF_ZONE,SITE,SITEVISITID,TRANSECT,GROUP), #ave old dead by taxon
                summarise,
                Ave_OldDead=mean(OLDDEAD))
  
  odtot<-ddply(scl, .(DEPTH_BIN,REEF_ZONE,SITE,SITEVISITID,TRANSECT), #ave old data for all colonies
            summarise,
            Ave_OldDead=mean(OLDDEAD))
  odtot$GROUP<-"SSSS"; odtot <- odtot[c(1,2,3,5,4)]
  od_wide<-dcast(od, formula=DEPTH_BIN+REEF_ZONE+SITE + SITEVISITID + TRANSECT ~ GROUP, value.var="Ave_OldDead",fill=0)
  od_long <- gather(od_wide, GROUP, Ave_OldDead, names(od_wide[4:dim(od_wide)[2]]), factor_key=TRUE) #convert wide to long format
  od_long<-rbind(od_long,odtot)
  
  return(od_long)
}

##This function calculates mean % recent dead
#combine old and recent dead into 1 function 
Calc_recentdead_Transect<-function(data, grouping_field="S_ORDER",pool_fields=c("RDEXTENT1", "RDEXTENT2")){
  
  scl<-subset(data,S_ORDER=="Scleractinia")
  scl$GROUP<-scl[,grouping_field]
  scl$RDall <- rowSums(scl[,c("RDEXTENT1", "RDEXTENT2")], na.rm=TRUE) #add up to 2 recent dead columns
  
  rd<-ddply(scl, .(DEPTH_BIN,REEF_ZONE,SITE,SITEVISITID,TRANSECT,GROUP),
                 summarise,
                 Ave_RD=mean(RDall))
  rdtot<-ddply(scl, .(DEPTH_BIN,REEF_ZONE,SITE,SITEVISITID,TRANSECT),
               summarise,
               Ave_RD=mean(RDall))
  rdtot$GROUP<-"SSSS"; rdtot <- rdtot[c(1,2,3,5,4)]
  rd_wide<-dcast(rd, formula=DEPTH_BIN+REEF_ZONE+SITE + SITEVISITID + TRANSECT ~ GROUP, value.var="Ave_RD",fill=0)
  rd_long <- gather(rd_wide, GROUP, Ave_RD, names(rd_wide[4:dim(rd_wide)[2]]), factor_key=TRUE) #convert wide to long format
  rd_long<-rbind(rd_long,rdtot)
  
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

#ASK IVOR HOW TO INCORPORATE THE COLONY DEN FUNCTION INTO THE RD FUNCTION
# #merge disease and colony abundance dataframes
# cd<-Calc_ColDen_Transect(data, grouping_field="S_ORDER")
# allrd3<-merge(cd,alldz, by=c("SITE","SITEVISITID","TRANSECT","TAXONCODE"),all.x=TRUE);allrd3[is.na(allrd3)]<-0
# allrd3$dzprev<-allrd3$All_DZGN/allrd3$Colabun*100 #calculate prevalence


#This function calculate abundance of conditions conditions by transect and taxonomic group
#Transform RD1 and RD2 from long to wide format, remove other site AND TRANSECT level info
#WORK WITH IVOR TO BUILD IN MORE FLEXIBILITY FOR CONDITIONS
Calc_Condabun_Transect<-function(data, grouping_field="S_ORDER"){
  scl<-subset(data,S_ORDER=="Scleractinia")
  
  c<-dcast(scl, formula=SITEVISITID + SITE+TRANSECT+COLONYID ~ COND, value.var="COND",length,fill=0)
  c$ChronicDZ<-c$PDS+c$FUG+c$SGA
  c <- subset(c, select = -c(NDZ,NODATA,TLS,WSY)) #remove columns
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

# ########Functions for BSR Summaries to Site - unweighted
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




