# reshape library inclues the cast() function used below
library(reshape2)
library(ggplot2) ## to create the diver vs diver graphs





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
s.df<-ddply(data, .(MISSIONID,REGION,ISLANDCODE,OBS_YEAR,SITE,TRANSECT,SEGMENT),
            summarise,
            SEGAREA=unique(SEGAREA))
tr.df<-ddply(s.df, .(MISSIONID,REGION,ISLANDCODE,OBS_YEAR,SITE,TRANSECT),
             summarise,
             TRANAREA=sum(SEGAREA))

data<-merge(data,tr.df, by=c("MISSIONID","REGION","ISLANDCODE","OBS_YEAR","SITE","TRANSECT"),all=TRUE)


return(data)
}

####Functions for benthic summary metrics

#This function calculates total area surveyed per site
Calc_SurveyArea_By_Site<-function(data){
  
  tr.df<-ddply(data, .(MISSIONID,REGION,ISLANDCODE,OBS_YEAR,SITE,TRANSECT),
               summarise,
               TRANAREA=unique(TRANAREA))
  
  tr.df2<-ddply(tr.df, .(MISSIONID,REGION,ISLANDCODE,OBS_YEAR,SITE),
                summarise,
                TRANAREA=sum(TRANAREA))
  return(tr.df2)
}



#This function calculates colony density at the site scale by first calculating the total survey area (using Calc_SurveyArea_By_SIte) then calcuating colony density
Calc_ColDen_By_Site<-function(data){

trarea<-Calc_SurveyArea_By_Site(data)

colden<-ddply(data, .(MISSIONID,REGION,ISLANDCODE,OBS_YEAR,SITE),
              summarise,
              Colabun=length(COLONYLENGTH))

colden2<-merge(colden,trarea, by=c("MISSIONID","REGION","ISLANDCODE","OBS_YEAR","SITE"),all=TRUE)

colden2$ColonyDensity<-colden2$Colabun/colden2$TRANAREA

return(colden2)
}

#This function calculates colony density at the site scale for each species by first calculating the total survey area (using Calc_SurveyArea_By_SIte) then calcuating colony density
Calc_ColDen_By_Taxon_Site<-function(data){
  
  trarea<-Calc_SurveyArea_By_Site(data)
  
  colden<-ddply(data, .(MISSIONID,REGION,ISLANDCODE,OBS_YEAR,SITE,SPCODE),
                summarise,
                Colabun=length(COLONYLENGTH))
  
  colden2<-merge(colden,trarea, by=c("MISSIONID","REGION","ISLANDCODE","OBS_YEAR","SITE"),all=TRUE)
  
  colden2$ColonyDensity<-colden2$Colabun/colden2$TRANAREA
  
  return(colden2)
}


##This function calculates mean % old dead
Calc_olddead_By_Site<-function(data){

  olddead<-ddply(data, .(MISSIONID,REGION,ISLANDCODE,OBS_YEAR,SITE),
                summarise,
                OLDDEAD=mean(OLDDEAD))
  
  return(olddead)
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
