# CREATE UNWEIGHTED ESTIMATES BSR STEP 2 #
# This script will calculate unweighted estimates for benthic summary reports at the site, strata, and island level

bsr <- "single" # choose single or multi -- if single, enter island name; multi, enter region name
isl <- "Wake"
reg <- "PRIAs" # choose region (options: MHI, MARIAN, PRIAs, NWHI, SAMOA)
  # reg_mar <- "SMAR" or "NMAR" -- if choosing the Mariana Archipelago, specify if you want to focus on the northern or southern islands ** OPTIONAL **
yr <- 2017 # choose year
targ_sp <- c("ABRE", "FMAT", "PMEA", "PVAR") # choose target species
targ_gen <- c("FAVS", "GONS", "MOSP", "POCS", "POSP") # choose target genera

# set working directory
setwd("T:/Benthic/Data/REA Coral Demography/Raw from Oracle")

# load library functions
source("C:/Users/Morgan.Winston/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions.R")
source("C:/Users/Morgan.Winston/Documents/GitHub/fish-paste/lib/core_functions.R")

# load data
awd_c <- read.csv("ALL_REA_ADULTCORAL_RAW_cleanWD.csv")
jwd_c <- read.csv("ALL_REA_JUVCORAL_RAW_cleanWD.csv")

# change Farallon de Pajaros to FDP (for figures)
awd_c$ISLAND <- as.character(awd_c$ISLAND)
awd_c$ISLAND[awd_c$ISLAND == "Farallon de Pajaros"] <- "FDP"
awd_c$ISLAND <- as.factor(awd_c$ISLAND)
jwd_c$ISLAND <- as.character(jwd_c$ISLAND)
jwd_c$ISLAND[jwd_c$ISLAND == "Farallon de Pajaros"] <- "FDP"
jwd_c$ISLAND <- as.factor(jwd_c$ISLAND)

# subset to REGION/ISLAND and YEAR of interest
if(bsr == "multi"){
awd_c <- awd_c[ (awd_c$REGION == reg & awd_c$OBS_YEAR == yr),]
jwd_c <- jwd_c[ (jwd_c$REGION == reg & jwd_c$OBS_YEAR == yr),]
  if(reg_mar == "SMAR"){
    awd_c <- awd_c[ (awd_c$ISLAND %in% island_order(SMAR_order)),]
  }
  if(reg_mar == "NMAR"){
    awd_c <- awd_c[ (awd_c$ISLAND %in% island_order(NMAR_order)),]
  }
}

if(bsr == "single"){
  awd_c <- awd_c[ (awd_c$ISLAND == isl & awd_c$OBS_YEAR == yr),]
  jwd_c <- jwd_c[ (jwd_c$ISLAND == isl & jwd_c$OBS_YEAR == yr),]
}


# create table of all of the colony attributes - need this for the Calc_RDden and Calc_Condden functions
SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SITE", "DATE_", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT","TRANSECT","COLONYID","GENUS_CODE","SPCODE","MORPH_CODE","COLONYLENGTH")
survey_colony<-Aggregate_InputTable(awd_c, SURVEY_INFO)

SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE", "SITE", "DATE_", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT","TRANSECT")
survey_transect<-Aggregate_InputTable(awd_c, SURVEY_INFO)

SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE", "SITE", "DATE_", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT")
survey_site<-Aggregate_InputTable(awd_c, SURVEY_INFO)

setwd("T:/Benthic/Data/REA Coral Demography/WorkingData_forBSRs")
if(bsr == "single"){write.csv(survey_site, paste(isl, yr, "surveySite.csv", sep = "_"))}
if(bsr == "multi"){write.csv(survey_site, paste(reg, yr, "surveySite.csv", sep = "_"))}

## subset to forereef
awd_c <- awd_c[ which(awd_c$REEF_ZONE == "Forereef"),]
jwd_c <- jwd_c[ which(jwd_c$REEF_ZONE == "Forereef"),]

# GENERATE SUMMARY METRICS at the transect-level for genus and spcode -------- 
acd.gen<-Calc_ColDen_Transect(awd_c,"GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen"# calculate density at genus level as well as total
rdabun.gen<-Calc_RDabun_Transect(awd_c,"GENUS_CODE") # abundance of recent dead colonies by condition, you will need to subset which ever condition you want
condabun.gen<-Calc_Condabun_Transect(awd_c,"GENUS_CODE")# abundance of condition colonies by condition, you will need to subset which ever condition you want
jcd.gen<-Calc_ColDen_Transect(jwd_c,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen"

  # at SPCODE level
acd.tax<-Calc_ColDen_Transect(awd_c,"SPCODE");colnames(acd.tax)[colnames(acd.tax)=="ColCount"]<-"AdColCount";colnames(acd.tax)[colnames(acd.tax)=="ColDen"]<-"AdColDen"# calculate density at genus level as well as total
rdabun.tax<-Calc_RDabun_Transect(awd_c,"SPCODE") # abundance of recent dead colonies by condition, you will need to subset which ever condition you want
condabun.tax<-Calc_Condabun_Transect(awd_c,"SPCODE")# abundance of condition colonies by condition, you will need to subset which ever condition you want
jcd.tax<-Calc_ColDen_Transect(jwd_c,"SPCODE"); colnames(jcd.tax)[colnames(jcd.tax)=="ColCount"]<-"JuvColCount";colnames(jcd.tax)[colnames(jcd.tax)=="ColDen"]<-"JuvColDen"

# Calculate Chronic disease abudance 
#### this is where I am encoutering problems-- Calc_Condabun_Transect does not seem to be working properly
      # I am getting rows of NAs, and the following workaround I made for calculating total chronic disease prevalence is not working
      # I had to make the workaround to begin with because Calc_Condabun only creates columns with conditions that were observed in the awd data
      # However to calculate the total chronic disease prev we need there to be columns containing zero values for the conditions that were not observed
      # This worked when I was calculating condition prev for PRIAs 2015 as a whole, but my test for Wake 2017 is not working.
      # I can see that there were occurences of FUG at Wake in 2017, but Calc_Condabun does not seem to be recognizing them because they are not showing up in the condabun.gen/tax data frames
      # ^ this issue is also happening for other condition codes like ALG

chrdiz <- c("FUG", "SGA", "PDS", "PTR")

for(i in c(1:length(chrdiz))){
  print(awd_c[ which(awd_c$COND == chrdiz[i]),])
  if(nrow(awd_c[ which(awd_c$COND == chrdiz[i]),]) == 0){
    condabun.gen[chrdiz[i]] <- 0 # if no occurences of a chronic disease condition -- need to add in a column of zeros for the code to work
    condabun.tax[chrdiz[i]] <- 0 # if no occurences of a chronic disease condition -- need to add in a column of zeros for the code to work
  }
}
condabun.gen$ChronicDZ<-condabun.gen$FUG+condabun.gen$SGA+condabun.gen$PDS+condabun.gen$PTR
condabun.tax$ChronicDZ<-condabun.tax$FUG+condabun.tax$SGA+condabun.tax$PDS+condabun.tax$PTR

## Calculate Prevalence of whatever conditions you want [genus level]
rdabun.gen$AcuteDZprev<-rdabun.gen$AcuteDZ/rdabun.gen$ColCount*100 #calculate prevalence
rdabun.gen$COTSprev<-rdabun.gen$COTS/rdabun.gen$ColCount*100 #calculate prevalence
condabun.gen$ChronicDZprev<-condabun.gen$ChronicDZ/condabun.gen$ColCount*100 #calculate prevalence
condabun.gen$BLEprev<-condabun.gen$BLE/condabun.gen$ColCount*100 #calculate prevalence

## Calculate Prevalence of whatever conditions you want [species level]
rdabun.tax$AcuteDZprev<-rdabun.tax$AcuteDZ/rdabun.tax$ColCount*100 #calculate prevalence
rdabun.tax$COTSprev<-rdabun.tax$COTS/rdabun.tax$ColCount*100 #calculate prevalence
condabun.tax$ChronicDZprev<-condabun.tax$ChronicDZ/condabun.tax$ColCount*100 #calculate prevalence
condabun.tax$BLEprev<-condabun.tax$BLE/condabun.tax$ColCount*100 #calculate prevalence


# Merging together genus-level metrics
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","GENUS_CODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.gen<-Reduce(MyMerge, list(acd.gen,rdabun.gen,condabun.gen,jcd.gen))
data.gen$TotDZprev<-(data.gen$AcuteDZ+data.gen$ChronicDZ)/data.gen$ColCount.x #Calculate total disease prevalence

#Subset columns needed for analysis
data.gen<-subset(data.gen,select=c("SITE","SITEVISITID","TRANSECT","GENUS_CODE","AdColDen",
                                   "JuvColDen","BLEprev","TotDZprev","AcuteDZprev","ChronicDZprev","COTSprev"))

#Merging together species-level metrics
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","SPCODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.tax<-Reduce(MyMerge, list(acd.tax,rdabun.tax,condabun.tax,jcd.tax))
data.tax$TotDZprev<-(data.tax$AcuteDZ+data.tax$ChronicDZ)/data.tax$ColCount.x #Calculate total disease prevalence

#Subset columns needed for analysis
data.tax<-subset(data.tax,select=c("SITE","SITEVISITID","TRANSECT","SPCODE","AdColDen",
                                   "JuvColDen","BLEprev","TotDZprev","AcuteDZprev","ChronicDZprev","COTSprev"))


## Create genus-level summaries for target genera
bsrSITEg<-Calc_Sitemetrics_BSR(data.gen,"GENUS_CODE")
bsrISg<-Calc_Islmetrics_BSR(bsrSITEg,"GENUS_CODE")
bsrDEPTHg<-Calc_IslDepthmetrics_BSR(bsrSITEg,"GENUS_CODE")
bsrREEFZONEg<-Calc_IslReefZonemetrics_BSR(bsrSITEg,"GENUS_CODE")
TargGenera_Is<-bsrISg[ which(bsrISg$GENUS_CODE %in% targ_gen),]

## create species-level summaries for target species
bsrSITEt<-Calc_Sitemetrics_BSR(data.tax,"SPCODE")
bsrISt<-Calc_Islmetrics_BSR(bsrSITEt,"SPCODE")
bsrDEPTHt<-Calc_IslDepthmetrics_BSR(bsrSITEt,"SPCODE")
bsrREEFZONEt<-Calc_IslReefZonemetrics_BSR(bsrSITEt,"SPCODE") 
TargTaxon_Is<-bsrISt[ which(bsrISt$SPCODE %in% targ_sp),]

## create table for island level summaries for total sclerarctinans
TotalScl_SITE<-subset(bsrSITEg,GENUS_CODE=="SSSS")
TotalScl_IS<-subset(bsrISg,GENUS_CODE=="SSSS")
TotalScl_ISDEPTH<-subset(bsrDEPTHg,GENUS_CODE=="SSSS") 

# get strata and sectors data and subset it for the regions you need
setwd("T:/Benthic/Data")
sectors<-read.csv("Benthic_SectorArea_v5.csv", stringsAsFactors=FALSE)

### write out tables to csv files
setwd("T:/Benthic/Data/REA Coral Demography/WorkingData_forBSRs")

write.csv(subset(sectors,REGION==reg), paste(reg, "sectorArea_frf.csv", sep = "_"))write.csv(pria,"PRIA_Stratareas.csv") # sector area file
write.csv(TotalScl_IS, paste(reg, yr, "Islandmetrics_totalscl_frf.csv", sep = "_")) # total scleractinian data by region/island/depth
write.csv(TotalScl_ISDEPTH, paste(reg, yr, "IslandDepth_metrics_totalscl_frf.csv", sep = "_")) # total scleractinian data by region/island
write.csv(TargGenera_Is, paste(reg, yr, "Islandmetrics_targetgenera_frf.csv", sep = "_"))
write.csv(TargTaxon_Is, paste(reg, yr, "Islandmetrics_targettaxon_frf.csv", sep = "_"))
