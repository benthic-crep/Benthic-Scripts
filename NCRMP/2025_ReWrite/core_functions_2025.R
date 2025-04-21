# gdata library inclues the drop.levels() function used below
library(gdata)
library(tidyverse) #includes dplyr
library(tictoc)

# sample_data=wsdComp
# data_cols=data.cols
# pooling_level=c(POOLING_LEVEL, "AREA_HA_correct")
#dpsComp<-Calc_PerStrata(wsdComp, data.cols, c(POOLING_LEVEL, "AREA_HA_correct"))

#############################################################################################################################
# Updated to remove old dependencies  (and speed up by roughly factor of 2)
# See Calc_PerStrata to see comparison of aggreate,tidyverse and data.table. Tidyverse hard failed the speed tests, but data.table wins.
#
#Function calculates mean, variance and N for passed-in data_cols at whatever requested pooling_level (can be single or multiple fields)
# This function is similar to Calc_PooledStrata_MeanVarianceCount, but is much less restricted .. allows user to pool at any level 
# SITEVISITID is base sample level (ie generally a SURVEY DIVE)
# returns a list with three dfs: - 1st df is means per data col per strata, 2nd is var, 3rd is SE
#############################################################################################################################
Calc_PerStrata_2025 <- function (sample_data, data_cols, pooling_level = c("ISLAND", "STRATA", "OBS_YEAR", "METHOD"))
{
  #  NEW 2025-01-27
  BASE_DATA_COLS<-c(pooling_level, "SITEVISITID", data_cols)
  
  #first clean up sample data to just have the data that will be used for generating islandwide mean, var, N values (per strata, method, year)
  sample_dataDT=data.table(sample_data[,BASE_DATA_COLS])
  
  #Calculate aggregate Mean, Var, N, SE
  
  #MEAN
  strata.means.dt=sample_dataDT[,lapply(.SD,mean),by=pooling_level,.SDcols = data_cols]
  strata.means=as.data.frame(strata.means.dt)
  
  #VAR
  strata.vars.dt=sample_dataDT[,lapply(.SD,var),by=pooling_level,.SDcols = data_cols]
  strata.vars=as.data.frame(strata.vars.dt)

  #N
  N.dt=sample_dataDT[,.N,by=pooling_level]#;strata.vars=strata.vars.dt
  
  #Add N to tables
  strata.means$N<-N.dt$N
  strata.vars$N<-N.dt$N
  
  #SE
  strata.se<-strata.vars
  strata.se[,data_cols]<-sqrt(strata.vars[,data_cols])/sqrt(N.dt$N)

  out.x<-list(strata.means[,c(pooling_level,"N", data_cols)],
              strata.se[,c(pooling_level,"N", data_cols)],
              strata.vars[,c(pooling_level,"N", data_cols)])
  names(out.x)<-list("Mean", "SampleSE", "SampleVar")
  return(out.x)
}
# end Calc_PerStrata_2025

#Don't try to improve Calc_PooledSimple it's fine.


#OLD CODE FOR REFERENCE BELOW


#############################################################################################################################
# Function calculates mean, variance and N for passed-in data_cols at whatever requested pooling_level (can be single or multiple fields)
# This funcrion is similat to Calc_PooledStrata_MeanVarianceCount, but is much less restricted .. allows user to pool at any level 
# SITEVISITID is base sample level (ie generally a SURVEY DIVE)
# returns a list with three dfs: - 1st df is means per data col per strata, 2nd is var, 3rd is SE
#############################################################################################################################
Calc_PerStrata_OLD <- function (sample_data, data_cols, pooling_level = c("ISLAND", "STRATA", "OBS_YEAR", "METHOD"))
{
  BASE_DATA_COLS<-c(pooling_level, "SITEVISITID", data_cols)
  
  #first clean up sample data to just have the data that will be used for generating islandwide mean, var, N values (per strata, method, year)
  sample_data<-sample_data[,BASE_DATA_COLS]
  
  # Calculate aggregate Mean, Var, N	
  strata.means<-stats::aggregate(sample_data[,data_cols],by=sample_data[,pooling_level], mean)
  strata.vars<-stats::aggregate(sample_data[,data_cols],by=sample_data[,pooling_level], var)
  #Modified TAO 2024-03-12 - there appears to be some weird context-dependent behavior here. I'm trying to fix with stats::3-13; 3-14 stats didn't work, back to SITEVISITID
  #3-15 back to X working better. Sooo weird.
  #N<-stats::aggregate(sample_data[,"SITEVISITID"],by=sample_data[,pooling_level], length)$x
  #N<-stats::aggregate(sample_data[,"SITEVISITID"],by=sample_data[,pooling_level], length)$SITEVISITID
  Ndf<-stats::aggregate(sample_data[,"SITEVISITID"],by=sample_data[,pooling_level], length)
  N<-Ndf[,ncol(Ndf)]
  strata.means$N<-strata.vars$N<-N
  strata.se<-strata.vars
  strata.se[,data_cols]<-sqrt(strata.vars[,data_cols])/sqrt(N)
  
  out.x<-list(strata.means[,c(pooling_level,"N", data_cols)], strata.se[,c(pooling_level,"N", data_cols)], strata.vars[,c(pooling_level,"N", data_cols)])
  names(out.x)<-list("Mean", "SampleSE", "SampleVar")
  return(out.x)
  
}
# end Calc_PerStrataOLD

#############################################################################################################################
# Updated to remove old dependencies  (and hopefully speed up) tidyverse fail, but data.table wins.
#Function calculates mean, variance and N for passed-in data_cols at whatever requested pooling_level (can be single or multiple fields)
# This function is similar to Calc_PooledStrata_MeanVarianceCount, but is much less restricted .. allows user to pool at any level 
# SITEVISITID is base sample level (ie generally a SURVEY DIVE)
# returns a list with three dfs: - 1st df is means per data col per strata, 2nd is var, 3rd is SE
#############################################################################################################################
Calc_PerStrata_TestMethod <- function (sample_data, data_cols, pooling_level = c("ISLAND", "STRATA", "OBS_YEAR", "METHOD"))
{
  BASE_DATA_COLS<-c(pooling_level, "SITEVISITID", data_cols)
  
  #first clean up sample data to just have the data that will be used for generating islandwide mean, var, N values (per strata, method, year)
  sample_data<-sample_data[,BASE_DATA_COLS]
  sample_dataDT=data.table(sample_data)
  
  #Calculate aggregate Mean, Var, N, SE
  
  #MEAN
  tic("old strata.means")
  strata.means.ag<-stats::aggregate(sample_data[,data_cols],by=sample_data[,pooling_level], mean)
  toc()
  tic("tv strata.means")
  strata.means.tv=sample_data %>%
    group_by_at(pooling_level) %>%
    dplyr::summarise(across(all_of(data_cols), mean,na.rm=TRUE))
  toc()
  tic("DT strata.means")
  strata.means.dt=sample_dataDT[,lapply(.SD,mean),by=pooling_level,.SDcols = data_cols];strata.means=strata.means.dt
  toc()
  strata.means.ag %>% dplyr::arrange(across(pooling_level)) %>% head()
  strata.means.tv %>% as.data.frame() %>% dplyr::arrange(across(pooling_level)) %>% head()
  strata.means.dt %>% dplyr::arrange(across(pooling_level)) %>% head()

  #VAR
  tic("agg")
  strata.vars.ag<-stats::aggregate(sample_data[,data_cols],by=sample_data[,pooling_level], var)
  toc()
  tic("tv")
  strata.vars.tv=sample_data %>%
    group_by_at(pooling_level) %>%
    dplyr::summarise(across(all_of(data_cols), var,na.rm=TRUE))
  toc()
  tic("dt")
  strata.vars.dt=sample_dataDT[,lapply(.SD,var),by=pooling_level,.SDcols = data_cols]#;strata.vars=strata.vars.dt
  toc()
  strata.vars %>% dplyr::arrange(across(pooling_level)) %>% head()
  strata.vars.tv %>% as.data.frame() %>% dplyr::arrange(across(pooling_level)) %>% head()
  strata.vars.dt %>% dplyr::arrange(across(pooling_level)) %>% head()

  #  NEW 2025-01-27

  #Modified TAO 2024-03-12 - there appears to be some weird context-dependent behavior here. I'm trying to fix with stats::3-13; 3-14 stats didn't work, back to SITEVISITID
  #3-15 back to X working better. Sooo weird.
  #N<-stats::aggregate(sample_data[,"SITEVISITID"],by=sample_data[,pooling_level], length)$x
  #N<-stats::aggregate(sample_data[,"SITEVISITID"],by=sample_data[,pooling_level], length)$SITEVISITID
  tic()
  Ndf<-stats::aggregate(sample_data[,"SITEVISITID"],by=sample_data[,pooling_level], length)
  Ndf %>% dplyr::arrange(across(pooling_level)) %>% head()
  toc()
  tic()
  N.dt=sample_dataDT[,.N,by=pooling_level]#;strata.vars=strata.vars.dt
  N.dt %>% dplyr::arrange(across(pooling_level)) %>% head()
  toc()
  
  strata.means$N<-N.dt$N
  strata.vars$N<-N.dt$N
  strata.se<-strata.vars
  strata.se[,data_cols]<-sqrt(strata.vars[,data_cols])/sqrt(N)
  
  
  
  out.x<-list(strata.means[,c(pooling_level,"N", data_cols)], strata.se[,c(pooling_level,"N", data_cols)], strata.vars[,c(pooling_level,"N", data_cols)])
  names(out.x)<-list("Mean", "SampleSE", "SampleVar")
  return(out.x)
  
}
# end Calc_PerStrata_TestMethod