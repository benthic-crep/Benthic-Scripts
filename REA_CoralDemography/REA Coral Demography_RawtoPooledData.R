
#CREATE ADULT CLEAN ANALYSIS READY DATA----------------------------------------
# This script will clean the raw benthic REA data using method E (2013-present) and prepare it for analysis

rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")

## LOAD benthic data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")
load("ALL_REA_ADULTCORAL_RAW.rdata") #from oracle
x<-df #leave this as df

x$SITE<-SiteNumLeadingZeros(x$SITE) # Change site number such as MAR-22 to MAR-0022


### Use these functions to look at data
head(x)
tail(x)


# load site master to merge with sector names later in the script
# See REA Generate Benthic Site Master and Sectors script for more details on how site master file was created.
site_master<-read.csv("Benthic2013-17_SiteMaster_v5.csv");nrow(site_master)
site_master$SITE<-SiteNumLeadingZeros(site_master$SITE)


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


#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
taxa<-read.csv("2013-17_Taxa_MASTER.csv")

nrow(x)

#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
x.<-Convert_to_Taxoncode(x)

#There are some SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genus or taxoncodes to the spcode and the remaining NAs in the Taxon and genus code to AAAA
x.$GENUS_CODE<-as.character(x.$GENUS_CODE)
x.$SPCODE<-as.character(x.$SPCODE)
x.$TAXONCODE<-as.character(x.$TAXONCODE)

x.$GENUS_CODE<-ifelse(is.na(x.$GENUS_CODE)&x.$S_ORDER=="Scleractinia",x.$SPCODE,x.$GENUS_CODE)
x.$TAXONCODE<-ifelse(is.na(x.$TAXONCODE)&x.$S_ORDER=="Scleractinia",x.$SPCODE,x.$TAXONCODE)

x.$TAXONCODE[is.na(x.$TAXONCODE)]<-"AAAA" #change nas to AAAA
x.$GENUS_CODE[is.na(x.$GENUS_CODE)]<-"AAAA"#change nas to AAAA
#utils::View(x) #view data in separate window

#Check that Unknown scl were changed correctly
test<-subset(x.,TAXONCODE=="UNKN"&S_ORDER=="Scleractinia");head(test)
test<-subset(x.,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia");head(test)
test<-subset(x.,GENUS_CODE=="AAAA");head(test)

#Confirm that no rows were dropped during merge
nrow(x)
nrow(x.)
x<-x.



#Create new colummns that combine species, genus and morphology
x$TAXMORPH<-paste(x$TAXONCODE,x$MORPH_CODE,sep="")
x$GENMORPH<-paste(x$GENUS_CODE,x$MORPH_CODE,sep="")


#add SITE MASTER information to x 
#x<-merge(x, site_master[,c("SITE", "SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE) #Fish team's original code, we may want to create analysis scheme later in the 
nrow(site_master);colnames(site_master)
x<-merge(x, site_master[,c("OBS_YEAR","SITEVISITID","SITE","SEC_NAME","BENTHIC_SEC_CODE", "PROTECTION_DS","ANALYSIS_YEAR")], by=c("OBS_YEAR","SITEVISITID","SITE"))  
length(unique(x$SITEVISITID)) #double check that sites weren't dropped
head(x)

#CHECK THAT all SEC_CODES are present in the site_master file
test<-x[is.na(x$BENTHIC_SEC_CODE), c("REGION", "SITE","OBS_YEAR"),]
if(dim(test)[1]>0) {cat("sites with MISSING SECTORS present")}   # should be 0

#If there are missing sectors, generate a table of missing sites, lat, long, reef zone and depth bins. Manually correct Site Master file
a<-subset(x,is.na(x$BENTHIC_SEC_CODE))
a<-a[c("OBS_YEAR","SITE","LATITUDE", "LONGITUDE","REEF_ZONE","DEPTH_BIN")]
test<-unique(a)
write.csv(test,"missingsectors.csv")


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

#save(awd, file="TMPBenthicREA_Adultwd_V2.Rdata")  #Save clean working data



## CREATE JUVENILE CLEAN ANALYSIS READY DATA ----

load("ALL_REA_JUVCORAL_RAW.rdata")
x<-df
x$SITE<-SiteNumLeadingZeros(x$SITE)

# get strata and sectors data NOTE: we need these files
sectors<-read.csv("Benthic_SectorArea_v5.csv", stringsAsFactors=FALSE)

# load site master to merge with sector names later in the script
# See REA Generate Benthic Site Master and Sectors script for more details on how site master file was created.
site_master<-read.csv("Benthic2013-17_SiteMaster_v5.csv");nrow(site_master)
site_master$SITE<-SiteNumLeadingZeros(site_master$SITE)


#Create vector of column names to include then exclude unwanted columns from dataframe
DATA_COLS<-c("MISSIONID","REGION","REGION_NAME","ISLAND","ISLANDCODE","SITE","LATITUDE",	"LONGITUDE","REEF_ZONE","DEPTH_BIN","OBS_YEAR",
             "DATE_","SITE_MIN_DEPTH","SITE_MAX_DEPTH","SITEVISITID","HABITAT_CODE","DIVER","EXCLUDE_FLAG","TRANSECT","SEGMENT","TRANWIDTH",
             "TRANLENGTH","COLONYID","SPECIES","MORPH_CODE","COLONYLENGTH","COLONYWIDTH","GENUS_CODE","S_ORDER")

head(x[,DATA_COLS])
x<-x[,DATA_COLS]
x$Adult_juv<-"J" #add a column that indicates these are adults. This will be useful later on if we merge adult and juv datasets and also useful for Calc_ColDen_Site function


#Double check level and class of variables to make sure there aren't any errors
sapply(x,levels)
sapply(x,class)##Change column names to make code easier to code
colnames(x)[colnames(x)=="SPECIES"]<-"SPCODE" #Change column name
colnames(x)[colnames(x)=="TRANWIDTH"]<-"SEGWIDTH" #Change column name
colnames(x)[colnames(x)=="TRANLENGTH"]<-"SEGLENGTH" #Change column name
colnames(x)[colnames(x)=="SITE_MIN_DEPTH"]<-"SITE_MIN_DEPTH_FT" #Change column name
colnames(x)[colnames(x)=="SITE_MAX_DEPTH"]<-"SITE_MAX_DEPTH_FT" #Change column name


#add SITE MASTER information to x 
#x<-merge(x, site_master[,c("SITE", "SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE) #Fish team's original code, we may want to create analysis scheme later in the 
nrow(site_master)
x<-merge(x, site_master[,c("OBS_YEAR","SITEVISITID","SITE","SEC_NAME","BENTHIC_SEC_CODE", "PROTECTION_DS","ANALYSIS_YEAR")], by=c("OBS_YEAR","SITEVISITID","SITE"), all.y=TRUE)  
length(unique(x$SITEVISITID)) #double check that sites weren't dropped
head(x)


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

#read in list of taxa that we feel comfortable identifying to species or genus level. Note, taxa lists vary by year and region. This will need to be updated through time.
taxa<-read.csv("2013-17_Taxa_MASTER.csv")

#Convert SPCODE in raw colony data to TAXONCODE -generates a look up table
x.<-Convert_to_Taxoncode(x)

#There are some SPCODES that were a combination of taxa and weren't included in the complete taxa list
#Change these unknown genus or taxoncodes to the spcode and the remaining NAs in the Taxon and genus code to AAAA
x.$GENUS_CODE<-as.character(x.$GENUS_CODE)
x.$SPCODE<-as.character(x.$SPCODE)
x.$TAXONCODE<-as.character(x.$TAXONCODE)

x.$GENUS_CODE<-ifelse(is.na(x.$GENUS_CODE)&x.$S_ORDER=="Scleractinia",x.$SPCODE,x.$GENUS_CODE)
x.$TAXONCODE<-ifelse(is.na(x.$TAXONCODE)&x.$S_ORDER=="Scleractinia",x.$SPCODE,x.$TAXONCODE)

x.$TAXONCODE[is.na(x.$TAXONCODE)]<-"AAAA" #change nas to AAAA
x.$GENUS_CODE[is.na(x.$GENUS_CODE)]<-"AAAA"#change nas to AAAA
#utils::View(x.) #view data in separate window

#Check that Unknown scl were changed correctly
test<-subset(x.,TAXONCODE=="UNKN"&S_ORDER=="Scleractinia");head(test)
test<-subset(x.,GENUS_CODE=="UNKN"&S_ORDER=="Scleractinia");head(test)
test<-subset(x.,GENUS_CODE=="AAAA");head(test)

#Confirm that no rows were dropped during merge
nrow(x)
nrow(x.)
x<-x.


#CHECK THAT all SEC_CODES are present in the site_master file
test<-x[is.na(x$BENTHIC_SEC_CODE), c("REGION", "SITE","OBS_YEAR"),]
if(dim(test)[1]>0) {cat("sites with MISSING SECTORS present")}   # should be 0

#If there are missing sectors, generate a table of missing sites, lat, long, reef zone and depth bins. Manually correct Site Master file
a<-subset(x,is.na(x$BENTHIC_SEC_CODE))
a<-a[c("OBS_YEAR","SITE","LATITUDE", "LONGITUDE","REEF_ZONE","DEPTH_BIN")]
test<-unique(a)
write.csv(test,"missingsectors.csv")


## CLEAN UP NAs 

tmp.lev<-levels(x$SITE_MIN_DEPTH_FT); head(tmp.lev)
levels(x$SITE_MIN_DEPTH_FT)<-c(tmp.lev, "NONE")
x[is.na(x$SITE_MIN_DEPTH_FT),"SITE_MIN_DEPTH_FT"]<-"NONE"

tmp.lev<-levels(x$SITE_MAX_DEPTH_FT); head(tmp.lev)
levels(x$SITE_MAX_DEPTH_FT)<-c(tmp.lev, "NONE")
x[is.na(x$SITE_MAX_DEPTH_FT),"SITE_MAX_DEPTH_FT"]<-"NONE"

head(x)

jwd<-droplevels(x)



#Final Tweaks before calculating Site-level data-------------------------------------------------
#Colony fragments and scleractinans are subseted in the functions 

#Double check that there are no NAs in GENUS_CODE
new_DF <- awd[is.na(awd$GENUS_CODE),] 

# #Add a DUMMY column to data frames. This will allow you to have greater flexibility in the subsequent functions to include different variables (e.g. morphology, size class, etc)
# #you can easily remove this column after pooling to the domain level
# awd$DUMMY<-"DUMMY"
# jwd$DUMMY<-"DUMMY"

#Add a column for adult fragments we we can remove them from the dataset later (-1 indicates fragment)
awd$Fragment<-ifelse(awd$COLONYLENGTH<5&awd$S_ORDER=="Scleractinia",-1,0)
jwd$Fragment<-0 # you need to add this column so that you can use the site level functions correctly

#Remove transects with less than 5m surveyed and check how many rows were removed
nrow(awd)
awd<-subset(awd,TRANSECTAREA>=5) 
jwd<-subset(jwd,TRANSECTAREA>=1)
nrow(awd)
head(awd)

##Change Transects 3 and 4 in the juvenile data to 1 and 2 so we can merge with adult data
#BE CAREFUL- because we survey different areas for adults and juveniles you can not merge together ad and juvs until AFTER you calculate density
jwd$TRANSECT[jwd$TRANSECT == "3"] <- "1"
jwd$TRANSECT[jwd$TRANSECT == "4"] <- "2"


#test functions on HAWAII ISLAND data to test code
awd2<-subset(awd,REGION=="MHI")
jwd2<-subset(jwd,REGION=="MHI")

#Create a look a table of all of the colony attributes- you will need this for the Calc_RDden and Calc_Condden functions
SURVEY_INFO<-c("SITEVISITID", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "DATE_", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT","TRANSECT","COLONYID","GENUS_CODE","TAXONCODE","MORPH_CODE","COLONYLENGTH")
survey_colony<-Aggregate_InputTable(awd2, SURVEY_INFO)


# GENERATE SUMMARY METRICS at the transect-level  --------------------------------------------------

acd.gen<-Calc_ColDen_Transect(awd,"GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen"# calculate density at genus level as well as total
size.gen<-Calc_ColMetric_Transect(awd2,"GENUS_CODE","COLONYLENGTH"); colnames(size.gen)[colnames(size.gen)=="Ave.y"]<-"Ave.size" #Average colony length
od.gen<-Calc_ColMetric_Transect(awd2,"GENUS_CODE","OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.gen<-Calc_ColMetric_Transect(awd2,"GENUS_CODE",c("RDEXTENT1", "RDEXTENT2")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead
rdden.gen<-Calc_RDden_Transect(awd2,"GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want
condden.gen<-Calc_Condden_Transect(awd2,"GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.gen<-subset(condden.gen,select = c(SITEVISITID,SITE,TRANSECT,GENUS_CODE,BLE)) #subset just bleached colonies
jcd.gen<-Calc_ColDen_Transect(jwd2,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen"


#at TAXONCODE level
acd.tax<-Calc_ColDen_Transect(awd2,"TAXONCODE");colnames(acd.tax)[colnames(acd.tax)=="ColCount"]<-"AdColCount";colnames(acd.tax)[colnames(acd.tax)=="ColDen"]<-"AdColDen"# calculate density at genus level as well as total
size.tax<-Calc_ColMetric_Transect(awd2,"TAXONCODE","COLONYLENGTH"); colnames(size.tax)[colnames(size.tax)=="Ave.y"]<-"Ave.size" #Average colony length
od.tax<-Calc_ColMetric_Transect(awd2,"TAXONCODE","OLDDEAD"); colnames(od.tax)[colnames(od.tax)=="Ave.y"]<-"Ave.od"
rd.tax<-Calc_ColMetric_Transect(awd2,"TAXONCODE",c("RDEXTENT1", "RDEXTENT2")); colnames(rd.tax)[colnames(rd.tax)=="Ave.y"]<-"Ave.rd"
#rdden.tax<-Calc_RDden_Transect(awd2,"GENUS_CODE") 
condden.tax<-Calc_Condden_Transect(awd2,"GENUS_CODE")
jcd.tax<-Calc_ColDen_Transect(jwd2,"TAXONCODE"); colnames(jcd.tax)[colnames(jcd.tax)=="ColCount"]<-"JuvColCount";colnames(jcd.tax)[colnames(jcd.tax)=="ColDen"]<-"JuvColDen"


#Merge density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","GENUS_CODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.gen<-Reduce(MyMerge, list(acd.gen,size.gen,od.gen,rd.gen,jcd.gen));

###CHECK WITH DIONE- taxa that don't have a condition/rd should be NA NOT zero, correct?
head(data.gen)

#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.gen$JuvColCount[is.na(data.gen$JuvColCount)]<-0;data.gen$JuvColDen[is.na(data.gen$JuvColDen)]<-0
data.gen$AdColCount[is.na(data.gen$AdColCount)]<-0;data.gen$AdColDen[is.na(data.gen$AdColDen)]<-0

MyMerge <- function(x, y){
  df <- merge(x, y, by= c("SITE","SITEVISITID","TRANSECT","TAXONCODE"), all.x= TRUE, all.y= TRUE)
  return(df)
}
data.tax<-Reduce(MyMerge, list(acd.tax,size.tax,od.tax,rd.tax,jcd.tax))
data.tax$JuvColCount[is.na(data.tax$JuvColCount)]<-0;data.tax$JuvColDen[is.na(data.tax$JuvColDen)]<-0
data.tax$AdColCount[is.na(data.tax$AdColCount)]<-0;data.tax$AdColDen[is.na(data.tax$AdColDen)]<-0


#GENERATE SITE-LEVEL DATA BY AVERAGING TRANSECTS-----------------------------------
#Since we are moving to a 1 stage design, we need to summarize the transects before rolling up to site.
#Dione suggested that we calculate mean of 2 transects rather than pooling or dropping a transect

#define columns you would like to average
data.cols<-c("AdColDen","Ave.size","Ave.od","Ave.rd","JuvColDen")

#now average metrics to site level
#Can't get the aggregate to work properly because we have NAs in the data and we need to retain the 0 values in the density columns
# site.data.gen<-aggregate(data.gen[,data.cols], by = data.gen[,c("SITEVISITID", "SITE", "GENUS_CODE")],mean,na.action=na.pass, na.rm=TRUE)

#This is still clunky, but works for now.
site.data.gen<-ddply(data.gen, .(SITE,SITEVISITID,TRANSECT,GENUS_CODE), #calc total colonies by condition
            summarise,
            AdColDen=mean(AdColDen,na.rm = T),Ave.size=mean(Ave.size,na.rm = T),Ave.od=mean(Ave.od,na.rm = T),
            Ave.rd=mean(Ave.rd,na.rm = T),JuvColDen=mean(JuvColDen,na.rm=T))

#This is still clunky, but works for now.
site.data.tax<-ddply(data.tax, .(SITE,SITEVISITID,TRANSECT,TAXONCODE), #calc total colonies by condition
                     summarise,
                     AdColDen=mean(AdColDen,na.rm = T),Ave.size=mean(Ave.size,na.rm = T),Ave.od=mean(Ave.od,na.rm = T),
                     Ave.rd=mean(Ave.rd,na.rm = T),JuvColDen=mean(JuvColDen,na.rm=T))


# # GENERATE SITE-LEVEL BY POOLING TRANSECTS---------------------------------------------------
# # at GENUS-level
# acd.gen<-Calc_ColDen_Site(awd2,"GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="Colabun"]<-"AdColabun";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen"# calculate density at genus level as well as total
# size.gen<-Calc_ColMetric_Site(awd2,"GENUS_CODE","COLONYLENGTH"); colnames(size.gen)[colnames(size.gen)=="Ave.y"]<-"Ave.size" #Average colony length
# od.gen<-Calc_ColMetric_Site(awd2,"GENUS_CODE","OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
# rd.gen<-Calc_ColMetric_Site(awd2,"GENUS_CODE",c("RDEXTENT1", "RDEXTENT2")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead
# rdden.gen<-Calc_RDden_Site(awd2,"GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want
# condden.gen<-Calc_Condden_Site(awd2,"GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
# ble.gen<-subset(condden.gen,select = c(SITEVISITID,SITE,GENUS_CODE,BLE)) #subset just bleached colonies
# jcd.gen<-Calc_ColDen_Site(jwd2,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="Colabun"]<-"JuvColabun";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen"
# 
# #at TAXONCODE level
# acd.tax<-Calc_ColDen_Site(awd2,"TAXONCODE");colnames(acd.tax)[colnames(acd.tax)=="Colabun"]<-"AdColabun";colnames(acd.tax)[colnames(acd.tax)=="ColDen"]<-"AdColDen"# calculate density at genus level as well as total
# od.tax<-Calc_ColMetric_Site(awd2,"TAXONCODE","OLDDEAD"); colnames(od.tax)[colnames(od.tax)=="Ave.y"]<-"Ave.od"
# rd.tax<-Calc_ColMetric_Site(awd2,"TAXONCODE",c("RDEXTENT1", "RDEXTENT2")); colnames(rd.tax)[colnames(rd.tax)=="Ave.y"]<-"Ave.rd"
# rdden.tax<-Calc_RDden_Site(awd2,"GENUS_CODE") 
# condden.tax<-Calc_Condden_Site(awd2,"GENUS_CODE")
# 
# jcd.tax<-Calc_ColDen_Site(jwd2,"TAXONCODE"); colnames(jcd.tax)[colnames(jcd.tax)=="Colabun"]<-"JuvColabun";colnames(jcd.tax)[colnames(jcd.tax)=="ColDen"]<-"JuvColDen"
# 
# 
# #Merge density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
# MyMerge <- function(x, y){
#   df <- merge(x, y, by= c("SITE","SITEVISITID","DUMMY","GENUS_CODE"), all.x= TRUE, all.y= TRUE)
#   return(df)
# }
# data.gen<-Reduce(MyMerge, list(acd.gen,od.gen,rd.gen,jcd.gen));
# 
# #Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
# data.gen$JuvColabun[is.na(data.gen$JuvColabun)]<-0;data.gen$JuvColDen[is.na(data.gen$JuvColDen)]<-0
# data.gen$AdColabun[is.na(data.gen$AdColabun)]<-0;data.gen$AdColDen[is.na(data.gen$AdColDen)]<-0
# 
# MyMerge <- function(x, y){
#   df <- merge(x, y, by= c("SITE","SITEVISITID","DUMMY","TAXONCODE"), all.x= TRUE, all.y= TRUE)
#   return(df)
# }
# data.tax<-Reduce(MyMerge, list(acd.tax,od.tax,rd.tax,jcd.tax))
# data.tax$JuvColabun[is.na(data.tax$JuvColabun)]<-0;data.tax$JuvColDen[is.na(data.tax$JuvColDen)]<-0
# data.tax$AdColabun[is.na(data.tax$AdColabun)]<-0;data.tax$AdColDen[is.na(data.tax$AdColDen)]<-0
# 


# POOLING DATA from Site to Strata and Domain---------------------------------------------------

# get strata and sectors data. Note, this is the benthic sector/area file. we are still working on properly merging fish and benthic files.
sectors<-read.csv("Benthic_SectorArea_v5.csv", stringsAsFactors=FALSE)


#Generate a table of metadata at the transect and site level for ADULTS
# SURVEY_INFO<-c("SITEVISITID", "ANALYSIS_YEAR","OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME","SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT","TRANSECT")
# survey_transect<-Aggregate_InputTable(awd2, SURVEY_INFO)

SURVEY_INFO<-c("SITEVISITID","ANALYSIS_YEAR", "OBS_YEAR", "REGION", "REGION_NAME", "ISLAND","ISLANDCODE","SEC_NAME", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE","SITE_MIN_DEPTH_FT","SITE_MAX_DEPTH_FT")
survey_site<-Aggregate_InputTable(awd2, SURVEY_INFO)



#Merge together survey meta data and sector area files and check for missmatches 
meta<-merge(survey_site,sectors,by=c("REGION","SEC_NAME","ISLAND","REEF_ZONE","DEPTH_BIN"),all.x=TRUE)
meta[which(is.na(meta$AREA_HA)),]


#Merge site level data and meta data
site.data.gen<-merge(site.data.gen,meta,by=c("SITEVISITID","SITE"),all.x=TRUE)
site.data.tax<-merge(site.data.tax,meta,by=c("SITEVISITID","SITE"),all.x=TRUE)

#Create STRATANAME by idenityfing which ANALAYSIS SCHEME you want to use then concatinating with depth and reef zone that will be used to pool data
site.data.gen$STRATANAME=paste0(site.data.gen$BENTHIC_BASIC,"_",site.data.gen$DEPTH_BIN,"_",site.data.gen$REEF_ZONE)
site.data.tax$STRATANAME=paste0(site.data.tax$BENTHIC_BASIC,"_",site.data.tax$DEPTH_BIN,"_",site.data.tax$REEF_ZONE)

#create a table of total number of sites surveyed by strataname then read out file csv and mess with pooling

#Mess with Backreef and depth pooling here import columns for stratum name for pooling
#Domain at Islandcode
#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
site.data.gen$ANALYSIS_SCHEMA<-site.data.gen$STRATANAME
site.data.gen$DOMAIN_SCHEMA<-site.data.gen$ISLANDCODE

site.data.tax$ANALYSIS_SCHEMA<-site.data.tax$STRATANAME
site.data.tax$DOMAIN_SCHEMA<-site.data.tax$ISLANDCODE

#Calculate metrics at Strata-level
acdG_st<-Calc_Strata(site.data.gen,"GENUS_CODE","AdColDen") 
acdG_is<-Calc_Domain(site.data.gen,"GENUS_CODE","AdColDen")
jcdG_st<-Calc_Strata(site.data.gen,"GENUS_CODE","JuvColDen") 
jcdG_is<-Calc_Domain(site.data.gen,"GENUS_CODE","JuvColDen")

#Calculate metrics at Strata-level
acdT_st<-Calc_Strata(site.data.tax,"TAXONCODE","AdColDen") 
acdT_is<-Calc_Domain(site.data.tax,"TAXONCODE","AdColDen")
jcdT_st<-Calc_Strata(site.data.tax,"TAXONCODE","JuvColDen") 
jcdT_is<-Calc_Domain(site.data.tax,"TAXONCODE","JuvColDen")

#Domain at REGION
#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
site.data.gen$ANALYSIS_SCHEMA<-site.data.gen$STRATANAME
site.data.gen$DOMAIN_SCHEMA<-site.data.gen$REGION

site.data.tax$ANALYSIS_SCHEMA<-site.data.tax$STRATANAME
site.data.tax$DOMAIN_SCHEMA<-site.data.tax$REGION

#Calculate metrics at Strata-level
acdG_st<-Calc_Strata(site.data.gen,"GENUS_CODE","AdColDen")
acdG_is<-Calc_Domain(site.data.gen,"GENUS_CODE","AdColDen")
jcdG_st<-Calc_Strata(site.data.gen,"GENUS_CODE","JuvColDen")
jcdG_is<-Calc_Domain(site.data.gen,"GENUS_CODE","JuvColDen")

#Calculate metrics at Strata-level
acdT_st<-Calc_Strata(site.data.tax,"TAXONCODE","AdColDen")
acdT_is<-Calc_Domain(site.data.tax,"TAXONCODE","AdColDen")
jcdT_st<-Calc_Strata(site.data.tax,"TAXONCODE","JuvColDen") 
jcdT_is<-Calc_Domain(site.data.tax,"TAXONCODE","JuvColDen")




#Calculate metrics at Strata-level
ble_st<-Calc_Strata_Prevalence(site.data.gen,"GENUS_CODE","BLE") #Dummy variable can be ignored, but needs to be listed for this function.
head(ble_st)

#Calculate metrics at Domain-level
ble_is<-Calc_Domain_Prevalence(site.data.gen,"GENUS_CODE","BLE")#Dummy variable can be ignored, but needs to be listed for this function. We still need to tweak this function since it's providing NAs

