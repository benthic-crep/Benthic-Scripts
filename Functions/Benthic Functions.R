# reshape library inclues the cast() function used below
library(reshape2)
library(ggplot2) ## to create the diver vs diver graphs
library(data.table)
library(plyr)
library(gdata)
library(tidyr)



# GENERAL FUNCTIONS -------------------------------------------------------

#merging more than 2 dataframes together
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID"), all.x= TRUE, all.y= TRUE)
  return(df)
}


######################################################
# function SiteNumLeadingZeros, adds leasing zeros to SITE code numeric parts to make eg OAH-1 become OAH-001
# this function therefore makes it easier to sort site names meaningfully and also removes the problem of eg MAR-22 site being treated in csv output as if it means March 22nd
# some site names have letter in the second 3 portion eg GAR-R3 .. those sites are not changed, because there re very few of those and those are generally well sorted anyway
# (and, it seems harder to work out which situations those are and how to deal with all possible variants .. therefore code just runs for situations where there are only digits in the part of the site name after the hyphen) 
#####################################################
SiteNumLeadingZeros <- function(site_names)
{	
  tmp<-levels(site_names)
  for (i in 1:length(tmp)) {
    s<-tmp[i]
    if (nchar(s)<9) {   # only change values where name length is too short ()
      ss<-strsplit(as.character(tmp[i]),"-")
      s1<-ss[[1]][1]
      s2<-ss[[1]][2]
      if (length(x=grep("[A-Z]",unlist(strsplit(toupper(s2),""))))==0) 
      {
        tmp[i]<-paste(s1, formatC(as.numeric(s2), width=5, flag="0"), sep="-")
      }
    }
  }
  levels(site_names)<-tmp
  
  return(site_names)
} #SiteNumLeadingZeros






##Calcuate segment and transect area and add column for transect area
Transectarea<-function(data,s.df){
data$SEGAREA<-data$SEGLENGTH*data$SEGWIDTH # Calculate segment area

#Calculate total transect area then merge back to a dataframe
s.df<-ddply(data, .(MISSIONID,REGION,ISLANDCODE,OBS_YEAR,SITE,TRANSECT,SEGMENT,SITEVISITID),
            summarise,
            SEGAREA=unique(SEGAREA))
tr.df<-ddply(s.df, .(MISSIONID,REGION,ISLANDCODE,OBS_YEAR,SITE,TRANSECT,SITEVISITID),
             summarise,
             TRANSECTAREA=sum(SEGAREA))

data<-merge(data,tr.df, by=c("MISSIONID","REGION","ISLANDCODE","OBS_YEAR","SITE","SITEVISITID","TRANSECT"),all=TRUE)


return(data)
}



Aggregate_InputTable<-function(x, field_list){  
  # function assumes that x is a data frame looking like our standard input
  # field_list is the list of fields to include (could be everything relating to each survey, or everything relating to a coral taxon)
  # function returns a data frame 
  
  y<-aggregate(x$COLONYLENGTH,by=x[,field_list], sum)  # aggregate sums colony lenth per record, using field_list 
  y<-y[,field_list]                             # drop the length - was just using that to generate a summary table
  
  return(y)
  
} # end Aggregate_InputTables


####Functions for benthic summary metrics

#This function calculates total area surveyed per transect
Calc_SurveyArea_By_Transect<-function(data){
  
  tr.df<-ddply(data, .(SITE,SITEVISITID,TRANSECT),
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



#This function calculates colony density at the site scale by first calculating the total survey area (using Calc_SurveyArea_By_SIte) then calcuating colony density
Calc_ColDen_By_Transect<-function(data, grouping_field="S_ORDER"){

trarea<-Calc_SurveyArea_By_Transect(data)

#scl<-subset(data,S_ORDER==s_order)

data$GROUP<-data[,grouping_field]
colden<-ddply(data, .(SITE,SITEVISITID,TRANSECT, GROUP),
              summarise,
              Colabun=length(COLONYLENGTH))

colden2<-merge(trarea,colden, by=c("SITE","SITEVISITID","TRANSECT"),all.x=TRUE)

colden2[is.na(colden2)]<-0

colden2$AdultColDen<-colden2$Colabun/colden2$TRANSECTAREA

colden2<-dcast(colden2, formula=SITE + SITEVISITID + TRANSECT ~ GROUP, value="AdultColDen",fill=0)


return(colden2)
}



##This function calculates mean % old dead
Calc_olddead_By_Site<-function(data){
  
  scl<-subset(data,S_ORDER=="Scleractinia")

  olddead<-ddply(data, .(SITE,SITEVISITID),
                summarise,
                Ave_OldDead=mean(OLDDEAD))
  
  return(olddead)
}


##This function calculates mean % recent mortality. 
#Note: RECENT_SPECIFIC_CAUSE_CODE 1 and 2 were changed to COND1 AND COND2 to improve coding efficency

Calc_recentdead_By_Site<-function(data){
  data<-subset(data,RDEXTENT1>=0 & RDEXTENT1!="NA")
  scl<-subset(data,S_ORDER=="Scleractinia")
  scl$RD<-scl$RDEXTENT1+scl$RDEXTENT2
  recentdead<-ddply(scl, .(SITE,SITEVISITID),
                 summarise,
                 Ave_RecentDead=mean(RD))
  
  return(recentdead)
}





###playing around modifying fish code for adult colony density- still working on this


Calc_Abundm2<-function(x){
  # IDW return y .. do not modify x inside the function .. just pass out biomassgm2
  # do this elsewhere - keep this function doing one thing - calculating biomassgm2 y$Srvy.Yr<-as.factor(y$Srvy.Yr)
  Area<-ifelse(x$METHOD %in% c("nSPC", "nSPC-CCR"), pi*(7.5^2), ifelse(x$SIZE_ < 20, 50, 100))
  return(x$COUNT/Area)
  
} #end Calc_Abundm2

Calc_TR_AdultDen<-function(x){  
  # function assumes that x is a data frame with at least the columns/elements listed in base_cols, plus the field_of_interest, in this case CommonFamily
  # prop_size is proportion of max size, min_size is minimum size included in mean size calculation, set at 10 cm
  
  #Base unit will be the entire survey
  base_cols=c("SITEVISITID", "TRANSECT") 
  pool_cols<-c(base_cols, "SIZE_")                          
  
  #set count to zero for all sizes smaller than min size to exclude recruits
  x[x$SIZE_< (prop_size*x$LMAX),]$COUNT<-0
  
  
  # set count to zero for all sizes smaller than 15 cm
  x[x$SIZE_< min_size,]$COUNT<-0
  
  #sum total number offishes per SIZE_
  y<-aggregate(x$COUNT,by=x[,pool_cols], sum)
  names(y)<-c(pool_cols, "COUNT")
  y$CS<-y$COUNT*y$SIZE_
  
  #now format this more or less as a crosstab, with field of interest as column variable
  y<-aggregate(y[,c("COUNT", "CS")],by=y[,base_cols], sum)
  y$MEAN_SIZE<-y$CS/y$COUNT
  
  return(y[,c(base_cols, "MEAN_SIZE")])
  
} # end Calc_Site_MeanLength
