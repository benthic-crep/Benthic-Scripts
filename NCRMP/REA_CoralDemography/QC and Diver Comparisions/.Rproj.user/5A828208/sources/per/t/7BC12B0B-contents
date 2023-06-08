#This script QCs the PMNM 2017 REA data from the PMNM Acess database


setwd("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle")

#LOAD LIBRARY FUNCTIONS ... 
library(tidyr)
library(reshape2)
library(dplyr)



source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_BSR.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")


##read benthic data downloaded from Access
ad<-read.csv("PMNM 2017_V0_CORAL_OBS_E.csv") #read in adult data
j<-read.csv("PMNM 2017_V0_CORAL_OBS_F.csv") #read in adult data


load("ALL_REA_ADULTCORAL_RAW_2013-2020.Rdata")

#Change to uppercase
names(ad) <- toupper(names(ad))
names(j) <- toupper(names(j))


# Column Names Changes... -------------------------------------------------
colnames(ad)[colnames(ad)=="MAX_DEPTH"]<-"MAX_DEPTH_FT" #Change column name
colnames(ad)[colnames(ad)=="MIN_DEPTH"]<-"MIN_DEPTH_FT" 
colnames(ad)[colnames(ad)=="MORPHOLOGY"]<-"MORPH_CODE" #Change column name
colnames(ad)[colnames(ad)=="TRAN"]<-"TRANSECTNUM" #Change column name
colnames(ad)[colnames(ad)=="SEG"]<-"SEGMENT" #Change column name
colnames(ad)[colnames(ad)=="TRLEN"]<-"SEGLENGTH" #Change column name
colnames(ad)[colnames(ad)=="TRWID"]<-"SEGWIDTH" #Change column name
colnames(ad)[colnames(ad)=="SPECIES"]<-"SPCODE" #Change column name
colnames(ad)[colnames(ad)=="GENUS"]<-"GENUS_CODE" #Change column name
colnames(ad)[colnames(ad)=="RECENTDEAD"]<-"RECENTDEAD_1" #Change column name
colnames(ad)[colnames(ad)=="RECENTGENERALCAUSECODE"]<-"RECENT_GENERAL_CAUSE_CODE_1" #Change column name
colnames(ad)[colnames(ad)=="RECENTSPECIFICCAUSECODE"]<-"RECENT_SPECIFIC_CAUSE_CODE_1" #Change column name
colnames(ad)[colnames(ad)=="DZCODE"]<-"COND" #Change column name
colnames(ad)[colnames(ad)=="FRAGMENT_YN"]<-"Fragment" #Change column name
colnames(ad)[colnames(ad)=="COND"]<-"CONDITION_1" #Change column name

colnames(j)[colnames(j)=="MAX_DEPTH"]<-"MAX_DEPTH_FT" #Change column name
colnames(j)[colnames(j)=="MIN_DEPTH"]<-"MIN_DEPTH_FT" 
colnames(j)[colnames(j)=="MORPHOLOGY"]<-"MORPH_CODE" #Change column name
colnames(j)[colnames(j)=="TRAN"]<-"TRANSECTNUM" #Change column name
colnames(j)[colnames(j)=="SEG"]<-"SEGMENT" #Change column name
colnames(j)[colnames(j)=="TRLEN"]<-"SEGLENGTH" #Change column name
colnames(j)[colnames(j)=="TRWID"]<-"SEGWIDTH" #Change column name
colnames(j)[colnames(j)=="SPECIES"]<-"SPCODE" #Change column name
colnames(j)[colnames(j)=="GENUS"]<-"GENUS_CODE" #Change column name

#Convert depth data to meters
ad$MAX_DEPTH_M<-ad$MAX_DEPTH_FT*0.3048
ad$MIN_DEPTH_M<-ad$MIN_DEPTH_FT*0.3048

j$MAX_DEPTH_M<-j$MAX_DEPTH_FT*0.3048
j$MIN_DEPTH_M<-j$MIN_DEPTH_FT*0.3048

ad$OBS_YEAR<-2017
j$OBS_YEAR<-2017

#Add a colony id (temporary)
ad$COLONYID<-seq(1:length(ad$COLONYID))

# Prepping Adult Data -----------------------------------------------------

#Change NAs in the Taxon and genus code to AAAA & PEVE to PLUT
ad$GENUS_CODE<-as.character(ad$GENUS_CODE)
ad$SPCODE<-as.character(ad$SPCODE)

ad <- ad %>% 
  mutate(GENUS_CODE = replace(GENUS_CODE, GENUS_CODE == "", "AAAA"))  %>%
  mutate(SPCODE = replace(SPCODE, SPCODE == "PEVE", "PLUT"))  %>%
  mutate(TAXONNAME = replace(TAXONNAME, TAXONNAME == "Porites evermanni", "Porites lutea"))  %>%
  mutate(SPCODE = replace(SPCODE, SPCODE == "", "AAAA"))
View(ad)


##Calculating segment and transect area and add column for transect area
ad<-Transectarea(ad)


#create a site table
detach(package:plyr)
sv<-ad %>%
  select (ISLAND, OBS_YEAR, SITE, LATITUDE, LONGITUDE, REEF_ZONE, DEPTH_BIN, DATE_, HABITAT_CODE, SITE_MIN_DEPTH, SITE_MAX_DEPTH) %>%
  distinct(SITE, .keep_all= TRUE)
View(sv)

site_by_island <- sv %>%
  group_by(ISLAND) %>%
  summarize (n_distinct(SITE))
View(site_by_island)




# Prep Juvenile data ------------------------------------------------------

#Create colony id that doesn't overlap with adults
j$COLONYID <- seq(1:length(j$COLONYID))
j$COLONYID <-  j$COLONYID + length(ad$COLONYID)



#Change NAs in the Taxon and genus code to AAAA & PEVE to PLUT
j$GENUS_CODE<-as.character(j$GENUS_CODE)
j$SPCODE<-as.character(j$SPCODE)

j <- j %>% 
  mutate(GENUS_CODE = replace(GENUS_CODE, GENUS_CODE == "", "AAAA"))  %>%
  mutate(SPCODE = replace(SPCODE, SPCODE == "PEVE", "PLUT"))  %>%
  mutate(TAXONNAME = replace(TAXONNAME, TAXONNAME == "Porites evermanni", "Porites lutea"))  %>%
  mutate(SPCODE = replace(SPCODE, SPCODE == "", "AAAA"))
View(j)

##Calcuating segment and transect area and add column for transect area
library(plyr)
j<-Transectarea(j)



# QC Checks ---------------------------------------------------------------
#Set up output csv file that reports the status of the qc checks
output<-data.frame(
  QC_check<-character(),
  Status<-character(),stringsAsFactors = FALSE)

output<-data.frame(
  QC_check,
  Status,stringsAsFactors = FALSE)

#QC checks on transect 1 data only
ad <- ad %>% filter (TRANSECT==1)
j <- j %>% filter (TRANSECT==1)



#4. Make sure that all sites have all segments. If segments are missing double check datasheets and keep a record of segments that weren't surveyed
test2<-ddply(ad,.(SITE,TRANSECTNUM,SEGMENT,DIVER),summarize,temp=length(SEGMENT))
test2$SS<-paste(test2$SITE,test2$TRANSECTNUM,test2$SEGMENT,sep="_")
eval<-ddply(test2,.(SS),summarize,n=length(unique(SS))) #look for duplicates
if(eval$n>1) {cat("duplicate segments")}  

eval2<-ddply(test2,.(SITE,TRANSECTNUM),summarize,n=length(unique(SEGMENT))) #look at how many sites 




#5. Check that depths are not null and Add a new column to sitevisit table and flags any sites that don't match depth ranges for a given depth bin
sv<-ad %>%
  select (ISLAND, OBS_YEAR, SITE, LATITUDE, LONGITUDE, REEF_ZONE, DEPTH_BIN, DATE_, HABITAT_CODE, MAX_DEPTH_M , MIN_DEPTH_M ) %>%
  distinct(SITE, .keep_all= TRUE)
View(sv)

# sv<-j %>%
#   select (ISLAND, OBS_YEAR, SITE, LATITUDE, LONGITUDE, REEF_ZONE, DEPTH_BIN, DATE_, HABITAT_CODE, MAX_DEPTH_M , MIN_DEPTH_M ) %>%
#   distinct(SITE, .keep_all= TRUE)
# View(sv)


sv$db_ok<-NULL
for (i in c(1:nrow(sv))){ #opening brace
  if(sv$DEPTH_BIN[i] =="Deep"& sv$MIN_DEPTH_M[i]>=18){ #c&p
    sv$db_ok[i] = "ok" #c&p
  } #c&p
  if(sv$DEPTH_BIN[i] =="Mid"& sv$MIN_DEPTH_M[i]>6&sv$MAX_DEPTH_M[i]<18){ #c&p
    sv$db_ok[i] = "ok" #c&p
  } #c&p
  if(sv$DEPTH_BIN[i] =="Shallow"& sv$MAX_DEPTH_M[i]<=6){ #c&p
    sv$db_ok[i] = "ok" #c&p
  } #c&p
} #closing curly brace for entire for loop
sv$db_ok[is.na(sv$db_ok)]<-"error"
subset(sv,db_ok=="error") #identify the sites that have incorrect depth bins


#6.Check for incorrect species
ddply(ad,.(GENUS_CODE,SPCODE),summarize,temp=length(SPCODE))
ddply(j,.(GENUS_CODE,SPCODE),summarize,temp=length(SPCODE))



#7.That the columns have the appropripate type of data (e.g. numeric vs. text) & no errant codes (e.g. ble instead of BLE)
#BH: changed to unique...which shows the current values in the subset plus all possible levels
sapply(ad,unique)
sapply(j, unique)




#8. Identify colonies <5cm in the adult database but not fragments 
ad$COLONYLENGTH[ad$COLONYLENGTH=="-"]<-NA 
ad$COLONYLENGTH <-  as.numeric(as.character(ad$COLONYLENGTH))
sm_colonies <- ad %>% filter(COLONYLENGTH < 5 & Fragment.yn != "-1" & SPCODE != "PBER")
View(sm_colonies)

output[8,]<-c("No adult colonies less than 5cm that aren't fragments","OK")


#9. Identify juvenile colonies that are >= 5cm
j[j$COLONY_LENGTH>=5]

output[9,]<-c("Juvenile size range is correct","OK")


#10.Identify colonies with 0% recent dead, but PRED or DZGEN as general cause code- This check should result in 0 records
ad[ad$RDEXTENT1=="0"& ad$GENRD1!="-",]
ad[ad$RDEXTENT1=="0"& ad$RD1!="-",]
ad[ad$RDEXTENT2=="0"& ad$GENRD2!="-",]
ad[ad$RDEXTENT2=="0"& ad$RD2!="-",]
ad[ad$RDEXTENT3=="0"& ad$GENRD3!="-",]
ad[ad$RDEXTENT3=="0"& ad$RD3!="-",]
ad[ad$EXTENT_1=="0"& ad$CONDITION_1!="-",]
ad[ad$EXTENT_2=="0"& ad$CONDITION_2!="-",]
ad[ad$EXTENT_3=="0"& ad$CONDITION_3!="-",]

output[10,]<-c("Recent Dead cause is present, but %RD = 0","OK")

#11.Identify colonies with 0% recent dead, but PRED or DZGEN as general cause code- This check should result in 0 records
ad[ad$GENRD1!="-"& ad$RDEXTENT1=="-",]
ad[ad$GENRD2!="-"& ad$RDEXTENT2=="-",]
ad[ad$GENRD3!="-"& ad$RDEXTENT3=="-",]

output[11,]<-c("% Recent dead included if a cause was recorded","OK")

#12. Identify colonies with NO % recent dead. Double check that these shouldn't be 0
ad[ad$RDEXTENT1=="-" & ad$SPCODE!="AAAA",]
ad[ad$RDEXTENT2=="-" & ad$SPCODE!="AAAA",]
ad[ad$RDEXTENT3=="-" & ad$SPCODE!="AAAA",]

#Write files with the needed fixes.
RD_extent2 <- ad[ad$RDEXTENT2=="-" & ad$SPCODE!="AAAA",]
write.csv(RD_extent2, "RD2_fix.csv")output[12,]<-c("No missing %RD values","Fixes needed to RD2")

#13. Identify colonies with NO % EXTENT, but a condition. Double check that these shouldn't be 0
ad[ad$EXTENT=="-"& ad$CONDITION_1!="-",]
ad[ad$EXTENT_2=="-"& ad$CONDITION_2!="-",]
ad[ad$EXTENT_3=="-"& ad$CONDITION_3!="-",]

output[13,]<-c("All colonies with a condition have extent","Fixes needed to EXTENT1")

extent <- ad[ad$EXTENT=="-"& ad$CONDITION_1!="-",]
write.csv(extent, "Extente_fix.csv")

#14. Identify colonies witch nothing in condition column, but a value in extent
ad[ad$CONDITION_1=="-"& ad$EXTENT!="-",]
ad[ad$CONDITION_2=="-"& ad$EXTNET_2!="-",]
ad[ad$CONDITION_3=="-"& ad$EXTENT_3!="-",]

output[14,]<-c("Identify colonies with nothing in condition column, but a value in extent","OK")

#15. Identify colonies with nothing in condition column, but a value in severity. Double check that these shouldn't be 0
ad[ad$CONDITION_1=="-"& ad$SEVERITY!="-",]
ad[ad$CONDITION_2=="-"& ad$SEVERITY_2!="-",]
ad[ad$CONDITION_3=="-"& ad$SEVERITY_3!="-",]

output[15,]<-c("Identify colonies with nothing in condition column, but a value in severity","OK")

#16. Identify colonies with NO % SEVERITY, but a condition. Double check that these shouldn't be 0
ad[ad$SEVERITY=="-"& ad$CONDITION_1 %in% c("BLE","BLP"),]
ad[ad$SEVERITY2=="-"& ad$CONDITION_2%in% c("BLE","BLP"),]
ad[ad$SEVERITY3=="-"& ad$CONDITION_3%in% c("BLE","BLP"),]

#If these should be 1-5 then ask Michael to change them- it's faster than doing it manually
output[16,]<-c("BLE cause is present, but severity is null","OK")

#17. RD + OD is not greater than 100%
ad$OLDDEAD<-as.numeric(ad$OLDDEAD)
ad$RDEXTENT2<-as.numeric(ad$RDEXTENT2)
ad$RDEXTENT1<-as.numeric(ad$RDEXTENT1)
ad$totaldead<=ad$RDEXENT1+ad$RDEXTENT2+ad$OLDDEAD
ad[ad$totaldead>100]

output[17,]<-c("RD + OD <=100%","OK")


#18 Create maps of sites and confirm there are no errant sites
#for now import the site visit table into Arc.I'm working on a script to do this directly in R
output[18,]<-c("Site coordinates look good", "pending")


#19. Merge together adult and juvenile datasets to make sure there aren't missing adult or juvenile data- 
#you should have already figured this out using the above script, but this is a triple check.

#WRITE SCRIPT


#Export QC output table with appropriate file name
write.csv(output,"HARAMP2019_Leg2QC_output.csv")

####Generate a list of repeat segments
write.csv(rep.seg,"HARAMP_Leg2_repeatsegments.csv")

