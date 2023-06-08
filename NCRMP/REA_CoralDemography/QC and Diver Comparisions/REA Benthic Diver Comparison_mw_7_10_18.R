#### Benthic DIVERVSDIVER function - to compare benthic REA estimates (density, colony length, old dead, recent dead, condition/disease prevalence) of one diver versus their buddy
## Version: 05/18/2018
## Written by: Marie Ferguson & Courtney Couch
## Note: benthic divervsdiver functions based off of fish REA divervsdiver functions

# required files: ALL_REA_ADULTCORAL_RAW.rdata, ALL_REA_JUVCORAL_RAW.rdata
# required function files: Benthic_functions.R, core_functions.R
# currently this is set up to specify the following data: data, date 1, date 2, date 3; e.g. divervsdiver(working.data, date1="2015-03-27", date2="2015-03-28", date3="2015-03-29")
# divervsdiver function saves one png containing 1 graph for each benthic REA summary metric in the working directory

# clear workspace 
rm(list=ls()) # clear all variables

setwd("C:/Users/Morgan.Winston/Downloads")

# Need to source benthic team functions for functions to run throughout this script. Change directory to wherever files are located on personal computer
source("C:/Users/Morgan.Winston/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_old.R")
source("C:/Users/Morgan.Winston/Documents/GitHub/Benthic-Scripts/Functions/core_functions.R")
  
######################################################################
######################################################################

## CREATE ADULT CLEAN ANALYSIS READY DATA ----------------------------------------
# This script will clean the raw benthic REA data using method E (2013-present) and prepare it for analysis
######################################################################
## LOAD benthic data
load("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA/ALL_REA_ADULTCORAL_RAW.rdata") #from oracle
x<-df #leave this as df

x$SITE<-SiteNumLeadingZeros(x$SITE) # Change site number such as MAR-22 to MAR-0022

### Use these functions to look at data
head(x)
tail(x)


#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","REGION_NAME","ISLAND","ISLANDCODE","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","SITE_MIN_DEPTH","SITE_MAX_DEPTH","SITEVISITID","HABITAT_CODE","DIVER","TRANSECT","SEGMENT","TRANWIDTH",
             "TRANLENGTH","EXCLUDE_FLAG","NO_SURVEY_YN","COLONYID","SPECIES","MORPH_CODE","COLONYLENGTH","OLDDEAD",
             "RECENTDEAD","RECENT_GENERAL_CAUSE_CODE","RECENT_SPECIFIC_CAUSE_CODE",
             "RECENTDEAD_2",	"RECENT_GENERAL_CAUSE_CODE_2","RECENT_SPECIFIC_CAUSE_CODE_2","DZCODE",
             "EXTENT",	"SEVERITY","GENUS_CODE","S_ORDER","TAXONNAME")

#remove extraneous columns
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

#Double check level and class of variables to make sure there aren't any errors
sapply(x,levels)
sapply(x,class)##Change column names to make code easier to code

colnames(x)[colnames(x)=="TRANWIDTH"]<-"SEGWIDTH" #Change column name
colnames(x)[colnames(x)=="TRANLENGTH"]<-"SEGLENGTH" #Change column name
colnames(x)[colnames(x)=="SPECIES"]<-"SPCODE" #Change column name
colnames(x)[colnames(x)=="RECENTDEAD"]<-"RDEXTENT1" #Change column name
colnames(x)[colnames(x)=="RECENT_GENERAL_CAUSE_CODE"]<-"GENRD1" #Change column name
colnames(x)[colnames(x)=="RECENT_SPECIFIC_CAUSE_CODE"]<-"RD1" #Change column name
colnames(x)[colnames(x)=="RECENTDEAD_2"]<-"RDEXTENT2" #Change column name
colnames(x)[colnames(x)=="RECENT_GENERAL_CAUSE_CODE_2"]<-"GENRD2" #Change column name
colnames(x)[colnames(x)=="RECENT_SPECIFIC_CAUSE_CODE_2"]<-"RD2" #Change column name
colnames(x)[colnames(x)=="SITE_MIN_DEPTH"]<-"SITE_MIN_DEPTH_FT" #Change column name
colnames(x)[colnames(x)=="SITE_MAX_DEPTH"]<-"SITE_MAX_DEPTH_FT" #Change column name
colnames(x)[colnames(x)=="DZCODE"]<-"COND" #Change column name

head(x)

# #Create a list of sites only surveyed for photoquads
# SURVEY_INFO<-c("NO_SURVEY_YN","SITEVISITID","OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT")
# pq_only<-Aggregate_InputTable(x, SURVEY_INFO)
# pq<-subset(pq_only,NO_SURVEY_YN==-1)
# 


#There are some SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genus to the spcode and the remaining NAs in the Taxon and genus code to AAAA
x$GENUS_CODE<-as.character(x$GENUS_CODE)
x$SPCODE<-as.character(x$SPCODE)

x$GENUS_CODE<-ifelse(is.na(x$GENUS_CODE)&x$S_ORDER=="Scleractinia",x$SPCODE,x$GENUS_CODE)

x$GENUS_CODE[is.na(x$GENUS_CODE)]<-"AAAA"#change nas to AAAA
#utils::View(x.) #view data in separate window

#Check that Unknown scl were changed correctly
test<-subset(x,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia");head(test)
test<-subset(x,GENUS_CODE=="AAAA");head(test)



#Test whether there are missing values in the NO_SURVEY_YN column. The value should be 0 or -1
x.na<-x[is.na(x$NO_SURVEY_YN),]
test<-ddply(x.na,.(SITE),
            summarize,
            SEG=length(unique(SEGMENT)))
test

#Convert NAs to 0 and remove trasects with no surveys
x$EXCLUDE_FLAG<-is.na(x$EXCLUDE_FLAG)<-0 #Change NAs (blank cells) to 0
x$NO_SURVEY_YN<-is.na(x$NO_SURVEY_YN)<-0 #Change NAs (blank cells) to 0
x<-subset(x,NO_SURVEY_YN>-1) #Exclude rows -1
x<-subset(x,SEGLENGTH!="NA") #Remove segments that were not surveyed for coral demography



##Calcuating segment and transect area and add column for transect area
x<-Transectarea(x)
sapply(x,levels)
head(x)
nrow(x)


## CLEAN UP NAs ##

x[x=="."]<-NA #fix this
x[x=="-9"]<-NA
tmp.lev<-levels(x$GENRD1); head(tmp.lev)
levels(x$GENRD1)<-c(tmp.lev, "NONE") # change to NONE
x[is.na(x$GENRD1),"GENRD1"]<-"NONE"

tmp.lev<-levels(x$RD1); head(tmp.lev)
levels(x$RD1)<-c(tmp.lev, "NONE")
x[is.na(x$RD1),"RD1"]<-"NONE"

tmp.lev<-levels(x$GENRD2); head(tmp.lev)
levels(x$GENRD2)<-c(tmp.lev, "NONE")
x[is.na(x$GENRD2),"GENRD2"]<-"NONE"

tmp.lev<-levels(x$RD2); head(tmp.lev)
levels(x$RD2)<-c(tmp.lev, "NONE")
x[is.na(x$RD2),"RD2"]<-"NONE"

tmp.lev<-levels(x$SITE_MIN_DEPTH_FT); head(tmp.lev)
levels(x$SITE_MIN_DEPTH_FT)<-c(tmp.lev, "NONE")
x[is.na(x$SITE_MIN_DEPTH_FT),"SITE_MIN_DEPTH_FT"]<-"NONE"

tmp.lev<-levels(x$SITE_MAX_DEPTH_FT); head(tmp.lev)
levels(x$SITE_MAX_DEPTH_FT)<-c(tmp.lev, "NONE")
x[is.na(x$SITE_MAX_DEPTH_FT),"SITE_MAX_DEPTH_FT"]<-"NONE"

tmp.lev<-levels(x$Estab_Yr); head(tmp.lev)
levels(x$Estab_Yr)<-c(tmp.lev, "NONE")
x[is.na(x$Estab_Yr),"Estab_Yr"]<-"NONE"

head(x)

awd<-droplevels(x)



## CREATE JUVENILE CLEAN ANALYSIS READY DATA ----

load("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA/ALL_REA_JUVCORAL_RAW.rdata")
x<-df
x$SITE<-SiteNumLeadingZeros(x$SITE)


#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","REGION_NAME","ISLAND","ISLANDCODE","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","SITE_MIN_DEPTH","SITE_MAX_DEPTH","SITEVISITID","HABITAT_CODE","DIVER","EXCLUDE_FLAG","TRANSECT","SEGMENT","TRANWIDTH",
             "TRANLENGTH","COLONYID","SPECIES","MORPH_CODE","COLONYLENGTH","COLONYWIDTH","GENUS_CODE","S_ORDER")

head(x[,DATA_COLS])
x<-x[,DATA_COLS]


#Double check level and class of variables to make sure there aren't any errors
sapply(x,levels)
sapply(x,class)##Change column names to make code easier to code
colnames(x)[colnames(x)=="SPECIES"]<-"SPCODE" #Change column name
colnames(x)[colnames(x)=="TRANWIDTH"]<-"SEGWIDTH" #Change column name
colnames(x)[colnames(x)=="TRANLENGTH"]<-"SEGLENGTH" #Change column name
colnames(x)[colnames(x)=="SITE_MIN_DEPTH"]<-"SITE_MIN_DEPTH_FT" #Change column name
colnames(x)[colnames(x)=="SITE_MAX_DEPTH"]<-"SITE_MAX_DEPTH_FT" #Change column name


#There are some SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genus to the spcode and the remaining NAs in the Taxon and genus code to AAAA
x$GENUS_CODE<-as.character(x$GENUS_CODE)
x$SPCODE<-as.character(x$SPCODE)

x$GENUS_CODE<-ifelse(is.na(x$GENUS_CODE)&x$S_ORDER=="Scleractinia",x$SPCODE,x$GENUS_CODE)

x$GENUS_CODE[is.na(x$GENUS_CODE)]<-"AAAA"#change nas to AAAA
#utils::View(x.) #view data in separate window

#Check that Unknown scl were changed correctly
test<-subset(x,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia");head(test)
test<-subset(x,GENUS_CODE=="AAAA");head(test)



#Remove specfic colonies and segments
x$EXCLUDE_FLAG<-is.na(x$EXCLUDE_FLAG)<-0 #Change NAs (blank cells) to 0
x<-subset(x,EXCLUDE_FLAG>-1);summary(x$EXCLUDE_FLAG) #Exclude rows -1
x<-subset(x,SEGLENGTH!="NA") #Remove segments that were not surveyed for juveniles
nrow(x)


##Calcuating segment and transect area and add column for transect area
x<-Transectarea(x)
sapply(x,levels)
head(x)
nrow(x)


## CLEAN UP NAs 

tmp.lev<-levels(x$SITE_MIN_DEPTH_FT); head(tmp.lev)
levels(x$SITE_MIN_DEPTH_FT)<-c(tmp.lev, "NONE")
x[is.na(x$SITE_MIN_DEPTH_FT),"SITE_MIN_DEPTH_FT"]<-"NONE"

tmp.lev<-levels(x$SITE_MAX_DEPTH_FT); head(tmp.lev)
levels(x$SITE_MAX_DEPTH_FT)<-c(tmp.lev, "NONE")
x[is.na(x$SITE_MAX_DEPTH_FT),"SITE_MAX_DEPTH_FT"]<-"NONE"

head(x)

jwd<-droplevels(x)

#######################################################

### Final Tweaks before generating diver vs. diver comparisons -------------------------------------------------

## Colony fragments and scleractinans are subseted in the functions 
# Double check that there are no NAs in GENUS_CODE- change
new_DF <- awd[is.na(awd$GENUS_CODE),] 
new_DF
new_DF2 <- jwd[is.na(jwd$GENUS_CODE),] 
new_DF2

# Add a column for adult fragments so we can remove them from the dataset later (-1 indicates fragment)
awd$Fragment<-ifelse(awd$COLONYLENGTH < 5 & awd$S_ORDER == "Scleractinia", -1, 0)
jwd$Fragment<-0 # you need to add this column so that you can use the site level functions correctly

# Remove transects with less than 5m surveyed and check how many rows were removed
nrow(awd)
nrow(jwd)
awd<-subset(awd,TRANSECTAREA >= 5) 
jwd<-subset(jwd,TRANSECTAREA >= 1)
nrow(awd)
nrow(jwd)

## Change Transects 3 and 4 in the juvenile data to 1 and 2 so we can merge with adult data
# BE CAREFUL- because we survey different areas for adults and juveniles you can not merge together ad and juvs until AFTER you calculate density
jwd$TRANSECT[jwd$TRANSECT == "3"] <- "1"
jwd$TRANSECT[jwd$TRANSECT == "4"] <- "2"

############################################################

# test functions on SAMOA data to test code
awd2 <- awd[which(awd$REGION == "SAMOA"), ]
jwd2 <- jwd[which(jwd$REGION == "SAMOA"), ]

#Create a look a table of all of the colony attributes- you will need this for the Calc_RDden and Calc_Condden functions
SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SITE", "DATE_", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT","TRANSECT","SEGMENT","DIVER","COLONYID","GENUS_CODE","SPCODE","MORPH_CODE","COLONYLENGTH")
survey_colony<-Aggregate_InputTable(awd2, SURVEY_INFO)

SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE", "SITE", "DATE_", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT","TRANSECT","SEGMENT")
survey_segment<-Aggregate_InputTable(awd2, SURVEY_INFO)

SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE", "SITE", "DATE_", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT")
survey_site<-Aggregate_InputTable(awd2, SURVEY_INFO)

###################################################################


### GENERATE SUMMARY METRICS at the segment-level for genus  --------

## Calculate density for adults
compdata2 <- Calc_ColDen_Seg(data=awd2, grouping_field="GENUS_CODE")
setnames(compdata2, old = c("ColCount","ColDen"), new = c("ColCount.ad","ColDen.ad"))

## Calculate density for juvs
compdata2.juv <- Calc_ColDen_Seg(data=jwd2, grouping_field="GENUS_CODE")
setnames(compdata2.juv, old = c("ColCount","ColDen"), new = c("ColCount.juv","ColDen.juv"))

## Calculate colony length for adults
compdata3 <- Calc_ColMetric_Seg(data=awd2, grouping_field="GENUS_CODE", pool_fields="COLONYLENGTH") 
setnames(compdata3, old = c("AvgCOLONYLENGTH"), new = c("AvgCOLONYLENGTH.ad"))

## Calculate colony length for juvs
compdata3.juv <- Calc_ColMetric_Seg(data=jwd2, grouping_field="GENUS_CODE", pool_fields="COLONYLENGTH") 
setnames(compdata3.juv, old = c("AvgCOLONYLENGTH"), new = c("AvgCOLONYLENGTH.juv"))

## Calculate % recent dead for adults
compdata4 <- Calc_ColMetric_Seg(data=awd2, grouping_field="GENUS_CODE", pool_fields="RDEXTENT1") 
setnames(compdata4, old = c("AvgRDEXTENT1"), new = c("AvgRDEXTENT1.ad"))


## Calculate % old dead for adults
compdata5 <- Calc_ColMetric_Seg(data=awd2, grouping_field="GENUS_CODE", pool_fields="OLDDEAD") 
setnames(compdata5, old = c("AvgOLDDEAD"), new = c("AvgOLDDEAD.ad"))

## Calculate abundance of recent dead colonies by condition for adults
rdabun.gen<-Calc_RDabun_Segment(awd2,"GENUS_CODE")  #Note: you will need to subset which ever condition you want

## Calculate abundance of condition colonies by condition for adults
condabun.gen<-Calc_Condabun_Segment(awd2,"GENUS_CODE")  #Note: you will need to subset which ever condition you want

## Calculate Chronic disease abundance
condabun.gen$ChronicDZ<-condabun.gen$FUG+condabun.gen$SGA+condabun.gen$PTR

## Calculate Prevalence of whatever conditions you want
rdabun.gen$AcuteDZprev<-rdabun.gen$AcuteDZ/rdabun.gen$ColCount*100  #calculate prevalence
rdabun.gen$COTSprev<-rdabun.gen$COTS/rdabun.gen$ColCount*100  #calculate prevalence
condabun.gen$ChronicDZprev<-condabun.gen$ChronicDZ/condabun.gen$ColCount*100  #calculate prevalence
condabun.gen$BLEprev<-condabun.gen$BLE/condabun.gen$ColCount*100  #calculate prevalence


## Merging together genus-level metrics
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("DATE_","SITEVISITID","SITE","TRANSECT","SEGMENT","DIVER","GENUS_CODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}

data.gen<-Reduce(MyMerge, list(compdata2,compdata2.juv,compdata3,compdata3.juv,compdata4,compdata5,rdabun.gen,condabun.gen))  # merge all summary metric dataframes together
data.gen$TotDZprev<-(data.gen$AcuteDZ+data.gen$ChronicDZ)/data.gen$ColCount.x  # Calculate total disease prevalence

## Subset columns needed for inter-diver comparisons (and only need SSSS)
data.gen<-subset(data.gen, GENUS_CODE == "SSSS", select=c("DATE_","SITEVISITID","SITE","TRANSECT","SEGMENT","DIVER","GENUS_CODE","ColDen.ad","ColDen.juv","AvgCOLONYLENGTH.ad","AvgCOLONYLENGTH.juv","AvgOLDDEAD.ad","AvgRDEXTENT1.ad","BLEprev","TotDZprev","AcuteDZprev","ChronicDZprev","COTSprev"));  head(data.gen)


###################################################################

## RUN DIVER VS DIVER FUNCTION ON BENTHIC SUMMARY METRICS ##

## TEST function on range of dates:
divervsdiver(data=data.gen, date1="2015-03-27", date2="2015-03-28", date3="2015-03-29")
#divervsdiver(data=data.gen, date1="2015-03-27", date2="2015-03-28", date3="2015-03-29", x_range=20)  # may need to adjust x_range to match scaling for each benthic metric



###################################################################
########################-------------------------------------

## FROM IVOR: code below allows you to do size comparisons for a given species between divers (usually run after whole leg of a cruise...the more data points the more accurate estimates will be & can make proper comparisons between divers). Should do this for common species/genus of interest. E.g. can do for Porites lobata (MD). There is more room to make errors with this species b/c of their morphology and size make them difficult to measure.

####################################################
### ADD'TL SIZE COMPARISONS ###################
####################################################

# Test on data: TUT 2015
awd3 <- awd2[which(awd2$ISLANDCODE == "TUT" & awd2$OBS_YEAR == 2015), ]

# Identify & designate adult species of interest (may be different for each region)
levels(as.factor(awd3$SPCODE))
SPECIES_OF_INTEREST<-c("MOSP","PLOB","PRUS")
wd<-awd3[awd3$SPCODE %in% SPECIES_OF_INTEREST,]  # subset dataframe so only looking at species of interest

## Prep data so only looking at:  MOSP (all encrusting forms), PLOB (MD morph), PRUS (all morph)
wd$MORPH_CODE_BROAD <- gsub("EC|EM|EF", "ENC", wd$MORPH_CODE); head(wd)  # combine morph codes EC, EM, EF and designate as 'ENC' in new MORPH_CODE_BROAD
x <- wd[which(wd$SPCODE == "MOSP" & wd$MORPH_CODE_BROAD == "ENC"), ] # Subset data so just MOSP-ENC
y <- wd[which(wd$SPCODE == "PLOB" & wd$MORPH_CODE_BROAD == "MD"), ] # Subset data so just PLOB-MD
z <- wd[which(wd$SPCODE == "PRUS"), ] # Subset data so just PRUS
wd<-rbind(x,y,z)  # combine dataframes

# Calculate # of colonies for each diver/species/morph code/colony length.
wd <- ddply(wd, .(DIVER,SPCODE,MORPH_CODE,COLONYLENGTH),
          summarise,
          COUNT=length(COLONYID)) # change to count

divers<-levels(wd$DIVER); divers

wd2<-wd[1, c("DIVER", "SPCODE", "MORPH_CODE", "COLONYLENGTH")]

## Convert from wide to long format (row for each individual colony)
if(wd[1, "COUNT"]>1)
  for (j in 2:wd[1, "COUNT"])
    wd2<-rbind(wd2,wd[1,c("DIVER", "SPCODE", "MORPH_CODE", "COLONYLENGTH")])

for(i in 2:dim(wd)[1]) {
  for (j in 1:wd[i, "COUNT"])
    wd2<-rbind(wd2,wd[i,c("DIVER", "SPCODE", "MORPH_CODE", "COLONYLENGTH")])
}

## Generate plot with inter-diver size comparisons for each species/morph
## Plots will be saved in working directory
for(i in 1:length(SPECIES_OF_INTEREST))
{
  sp<-SPECIES_OF_INTEREST[i]
  wd_sp<-wd2[wd2$SPCODE==sp,]
  
  tmp_name<-paste(sp, ".png", collapse="")
  png(filename=tmp_name)
  #	boxplot(COLONYLENGTH ~ DIVER, data=wd_sp, main=sp)
  
  ggplot(wd_sp, aes(x = DIVER, y = COLONYLENGTH)) + 
    geom_boxplot()+ 
    scale_y_continuous(limits = c(0,max(wd_sp$COLONYLENGTH)*1.1), breaks=pretty_breaks(n=5)) +
    #geom_jitter(colour = "blue", size =4, alpha = 0.4)+
    labs(x = sp) 
  
  ggsave(filename=tmp_name)
  
  dev.off()
  graphics.off()
}



##################### END ########################################
###################################################################
