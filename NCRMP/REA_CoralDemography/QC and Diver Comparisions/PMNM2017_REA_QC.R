#This script QCs the PMNM 2017 REA data from the PMNM Acess database


setwd("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle")

#LOAD LIBRARY FUNCTIONS ...
library(tidyr)
library(reshape2)
library(dplyr)
library(sp)
library(sf)
library(raster)
library(ncf) # for gcdist()
library(ggsn)
library(ggspatial)
library(ggrepel)

source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_BSR.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")


##read benthic data downloaded from Access
ad<-read.csv("PMNM 2017_V0_CORAL_OBS_E.csv") #read in adult data
j<-read.csv("PMNM 2017_V0_CORAL_OBS_F.csv") #read in adult data
site<-read.csv("PMNM 2017_SITEVISIT.csv")

load("ALL_REA_ADULTCORAL_RAW_2013-2020.Rdata") #View what the data structure should look like
#load("ALL_REA_JUVCORAL_RAW_2013-2020.Rdata") #View what the data structure should look like

#Change to uppercase
names(ad) <- toupper(names(ad))
names(j) <- toupper(names(j))


# Column Names Changes... -------------------------------------------------
colnames(ad)[colnames(ad)=="MAX_DEPTH"]<-"SITE_MAX_DEPTH" #Change column name
colnames(ad)[colnames(ad)=="MIN_DEPTH"]<-"SITE_MIN_DEPTH" 
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
colnames(ad)[colnames(ad)=="COND"]<-"CONDITION_1" #Change column name
colnames(ad)[colnames(ad)=="EXTENT"]<-"EXTENT_1" #Change column name
colnames(ad)[colnames(ad)=="SEVERITY"]<-"SEVERITY_1" #Change column name

colnames(j)[colnames(j)=="MAX_DEPTH"]<-"SITE_MAX_DEPTH" #Change column name
colnames(j)[colnames(j)=="MIN_DEPTH"]<-"SITE_MIN_DEPTH" 
colnames(j)[colnames(j)=="MORPHOLOGY"]<-"MORPH_CODE" #Change column name
colnames(j)[colnames(j)=="TRAN"]<-"TRANSECTNUM" #Change column name
colnames(j)[colnames(j)=="SEG"]<-"SEGMENT" #Change column name
colnames(j)[colnames(j)=="TRLEN"]<-"SEGLENGTH" #Change column name
colnames(j)[colnames(j)=="TRWID"]<-"SEGWIDTH" #Change column name
colnames(j)[colnames(j)=="SPECIES"]<-"SPCODE" #Change column name
colnames(j)[colnames(j)=="GENUS"]<-"GENUS_CODE" #Change column name



ad$OBS_YEAR<-2017
j$OBS_YEAR<-2017

ad$MISSIONID<-"HA1704"
j$MISSIONID<-"HA1704"


# Prepping Adult Data -----------------------------------------------------

#Change NAs in the Taxon and genus code to AAAA & PEVE to PLUT - wait to change AGEM or AHUM to AGLO until cleaning script
levels(as.factor(ad$SPCODE))
ad$GENUS_CODE<-as.character(ad$GENUS_CODE)
ad$SPCODE<-as.character(ad$SPCODE)

ad <- ad %>% 
  mutate(GENUS_CODE = replace(GENUS_CODE, GENUS_CODE == "", "AAAA"))  %>%
  mutate(SPCODE = replace(SPCODE, SPCODE == "PEVE", "PLUT"))  %>%
  mutate(TAXONNAME = replace(TAXONNAME, TAXONNAME == "Porites evermanni", "Porites lutea"))  %>%
  mutate(SPCODE = replace(SPCODE, SPCODE == "", "AAAA"))
View(ad)


# ##Calculating segment and transect area and add column for transect area
# ad<-Transectarea(ad)


#create a site table
detach(package:plyr)
sv<-ad %>%
  dplyr::select (ISLAND, OBS_YEAR, SITE, LATITUDE, LONGITUDE, REEF_ZONE, DEPTH_BIN, DATE_, HABITAT_CODE, SITE_MIN_DEPTH, SITE_MAX_DEPTH) %>%
  distinct(SITE, .keep_all= TRUE)
View(sv)

site_by_island <- sv %>%
  group_by(ISLAND) %>%
  summarize (n_distinct(SITE))
View(site_by_island)


#Change . to "-"
ad <- ad %>% 
  mutate(RECENT_GENERAL_CAUSE_CODE_1 = replace(RECENT_GENERAL_CAUSE_CODE_1, RECENT_GENERAL_CAUSE_CODE_1 == ".","-"))  %>%
  mutate(RECENT_SPECIFIC_CAUSE_CODE_1 = replace(RECENT_SPECIFIC_CAUSE_CODE_1, RECENT_SPECIFIC_CAUSE_CODE_1 == ".", "-")) %>%
  mutate(MORPH_CODE = replace(MORPH_CODE, MORPH_CODE == ".", "-")) %>%
  mutate(COLONYLENGTH = replace(COLONYLENGTH, COLONYLENGTH == ".", "-")) %>%
  mutate(OLDDEAD = replace(OLDDEAD, OLDDEAD == ".", "-")) %>%
  mutate(RECENTDEAD_1 = replace(RECENTDEAD_1, RECENTDEAD_1 == ".", "-")) %>%
  mutate(CONDITION_1 = replace(CONDITION_1, CONDITION_1 == ".", "-")) %>%
  mutate(SEVERITY_1 = replace(SEVERITY_1, SEVERITY_1 == "-9", "-"))  %>%
  mutate(EXTENT_1 = replace(EXTENT_1, EXTENT_1 == "-9", "-"))

View(ad)


# Prep Juvenile data ------------------------------------------------------

#Change NAs in the Taxon and genus code to AAAA & PEVE to PLUT
levels(as.factor(j$SPCODE))
j$GENUS_CODE<-as.character(j$GENUS_CODE)
j$SPCODE<-as.character(j$SPCODE)

j <- j %>% 
  mutate(GENUS_CODE = replace(GENUS_CODE, GENUS_CODE == "", "AAAA"))  %>%
  mutate(SPCODE = replace(SPCODE, SPCODE == "PEVE", "PLUT"))  %>%
  mutate(TAXONNAME = replace(TAXONNAME, TAXONNAME == "Porites evermanni", "Porites lutea"))  %>%
  mutate(SPCODE = replace(SPCODE, SPCODE == "", "AAAA"))
View(j)


#Change . to "-"
j <- j %>% 
  mutate(MORPH_CODE = replace(MORPH_CODE, MORPH_CODE == ".", "-")) %>%
  mutate(COLONYLENGTH = replace(COLONYLENGTH, COLONYLENGTH == ".", "-"))%>%
  mutate(COLONYWIDTH = replace(COLONYWIDTH, COLONYWIDTH == ".", "-"))

View(j)


##Calcuating segment and transect area and add column for transect area
# j<-Transectarea(j)



# QC Checks ---------------------------------------------------------------
#Set up output csv file that reports the status of the qc checks
output<-data.frame(
  QC_check<-character(),
  Status<-character(),stringsAsFactors = FALSE)

output<-data.frame(
  QC_check,
  Status,stringsAsFactors = FALSE)

#QC checks on transect 1 data only
levels(as.factor(ad$TRANSECTNUM))
levels(as.factor(j$TRANSECTNUM))

#Convert Segment numbers
ad$SEGMENT<-ConvertSegNumber(ad)
j$SEGMENT<-ConvertSegNumber(j)

ad %>% distinct(LATITUDE) %>% pull()
ad %>% distinct(LONGITUDE) %>% pull()
ad %>% distinct(DIVER) %>% pull()
ad %>% distinct(SITE) %>% pull()
j %>% distinct(SITE) %>% pull()


summary(ad$SITE_MAX_DEPTH)
summary(ad$SITE_MIN_DEPTH)
range(ad$SITE_MAX_DEPTH-ad$SITE_MIN_DEPTH) #
range(j$SITE_MAX_DEPTH-j$SITE_MIN_DEPTH) #



#4. Make sure that all sites have all segments. If segments are missing double check datasheets and keep a record of segments that weren't surveyed
library(plyr)
test2<-ddply(ad,.(SITE,TRANSECTNUM,SEGMENT,DIVER),summarize,temp=length(SEGMENT))
test2$SS<-paste(test2$SITE,test2$TRANSECTNUM,test2$SEGMENT,sep="_")
eval<-ddply(test2,.(SITE,SS),summarize,n=length(SS)) #look for duplicates
subset(eval,n>1)  

test2<-ddply(j,.(SITE,TRANSECTNUM,SEGMENT,DIVER),summarize,temp=length(SEGMENT))
test2$SS<-paste(test2$SITE,test2$TRANSECTNUM,test2$SEGMENT,sep="_")
eval<-ddply(test2,.(SITE,SS),summarize,n=length(SS)) #look for duplicates
subset(eval,n>1)  


#5. Check that depths are not null and Add a new column to sitevisit table and flags any sites that don't match depth ranges for a given depth bin
sv<-ad %>%
  dplyr::select (ISLAND, OBS_YEAR, SITE, LATITUDE, LONGITUDE, REEF_ZONE, DEPTH_BIN, DATE_, SITE_MAX_DEPTH , SITE_MIN_DEPTH) %>%
  distinct(SITE, .keep_all= TRUE)
View(sv)

sv<-j %>%
  dplyr::select (ISLAND, OBS_YEAR, SITE, LATITUDE, LONGITUDE, REEF_ZONE, DEPTH_BIN, DATE_, SITE_MAX_DEPTH, SITE_MIN_DEPTH) %>%
  distinct(SITE, .keep_all= TRUE)
View(sv)


sv$db_ok<-NULL
for (i in c(1:nrow(sv))){ #opening brace
  if(sv$DEPTH_BIN[i] =="Deep"& sv$SITE_MIN_DEPTH[i]>=60){ #c&p
    sv$db_ok[i] = "ok" #c&p
  } #c&p
  if(sv$DEPTH_BIN[i] =="Midway"& sv$SITE_MIN_DEPTH[i]>20&sv$SITE_MAX_DEPTH[i]<60){ #c&p
    sv$db_ok[i] = "ok" #c&p
  } #c&p
  if(sv$DEPTH_BIN[i] =="Shallow"& sv$SITE_MAX_DEPTH[i]<=20){ #c&p
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
sm_colonies <- ad %>% filter(COLONYLENGTH < 5 & FRAGMENT_YN != "-1" & SPCODE != "PBER")
View(sm_colonies)

output[8,]<-c("No adult colonies less than 5cm that aren't fragments","OK")


#9. Identify juvenile colonies that are >= 5cm
j[j$COLONY_LENGTH>=5]

output[9,]<-c("Juvenile size range is correct","OK")


#10.Identify colonies with 0% recent dead, but PRED or DZGEN as general cause code- This check should result in 0 records
ad[ad$RECENTDEAD_1=="0"& ad$RECENT_GENERAL_CAUSE_CODE_1!="-",]
ad[ad$RECENTDEAD_1=="0"& ad$RECENT_SPECIFIC_CAUSE_CODE_1!="-",]
ad[ad$EXTENT_1=="0" & ad$CONDITION_1!="-",]

output[10,]<-c("Recent Dead cause is present, but %RD = 0","OK")

#11.Identify colonies with 0% recent dead, but PRED or DZGEN as general cause code- This check should result in 0 records
ad[ad$RECENT_GENERAL_CAUSE_CODE_1!="-"& ad$RECENTDEAD_1=="-",]


output[11,]<-c("% Recent dead included if a cause was recorded","OK")

#12. Identify colonies with NO % recent dead. Double check that these shouldn't be 0
ad[ad$RECENTDEAD_1=="-" & ad$SPCODE!="AAAA",]


#13. Identify colonies with NO % EXTENT, but a condition. Double check that these shouldn't be 0
ad[ad$EXTENT_1!="-"& ad$CONDITION_1=="NDZ",]

output[13,]<-c("All colonies with a condition have extent","Fixes needed to EXTENT1")

#14. Identify colonies witch nothing in condition column, but a value in extent
ad[ad$CONDITION_1=="-"& ad$EXTENT_1!="-",]

output[14,]<-c("Identify colonies with nothing in condition column, but a value in extent","OK")

#15. Identify colonies with nothing in condition column, but a value in severity. Double check that these shouldn't be 0
ad[ad$CONDITION_1=="-"& ad$SEVERITY_1!="-",]

output[15,]<-c("Identify colonies with nothing in condition column, but a value in severity","OK")

#16. Identify colonies with NO % SEVERITY, but a condition. Double check that these shouldn't be 0
ad[ad$SEVERITY_1 =="-"& ad$CONDITION_1 %in% c("BLE","BLP"),]


#If these should be 1-5 then ask Michael to change them- it's faster than doing it manually
output[16,]<-c("BLE cause is present, but severity is null","OK")

#17. RD + OD is not greater than 100%
ad$OLDDEAD<-as.numeric(ad$OLDDEAD)
ad$RECENTDEAD_1<-as.numeric(ad$RECENTDEAD_1)
ad$totaldead<=ad$RECENTDEAD_1+ad$OLDDEAD
ad[ad$totaldead>100]

output[17,]<-c("RD + OD <=100%","OK")


#18 Create maps of sites and confirm there are no errant sites
#for now import the site visit table into Arc.I'm working on a script to do this directly in R
#Read in islands shapefile
islands<-st_read("U:/GIS/Data/Pacific/islands.shp")

ad$x<-ad$LONGITUDE
ad$y<-ad$LATITUDE
ad$Stratum<-paste(ad$DEPTH_BIN,ad$REEF_ZONE,sep="_")

j$x<-j$LONGITUDE
j$y<-j$LATITUDE
j$Stratum<-paste(j$DEPTH_BIN,j$REEF_ZONE,sep="_")

#Plotting the wave and juvenile data for a subset of islands to check overlap
#Helpful website for plotting maps with ggplot https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
Plot_SiteCheck<-function(d1,d2,isl="Oahu",xlim1,xlim2,ylim1,ylim2){
  ggplot(data = d1) +
    geom_sf() +
    geom_point(data = subset(d2,ISLAND==isl), aes(x = x, y = y,color=Stratum), size = 2, shape = 16, fill = "slateblue3") +
    coord_sf(xlim = c(xlim1, xlim2), ylim = c(ylim1, ylim2), expand = FALSE)+
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
                                          size = 0.5), panel.background = element_rect(fill = "aliceblue"))+
    annotation_scale(location = "bl", width_hint = 0.4)
}


levels(as.factor(ad$ISLAND))
table(sv$ISLAND)#CHECK NUMBER OF SITES

summary(subset(ad,ISLAND=="French Frigate")) # identify extent of coordinates
Plot_SiteCheck(islands,ad,"French Frigate",-166.5, -166.0,23.6, 23.88)
Plot_SiteCheck(islands,j,"French Frigate",-166.5, -166.0,23.6, 23.88)

summary(subset(ad,ISLAND=="Kure")) # identify extent of coordinates
Plot_SiteCheck(islands,ad,"Kure",-178.36, -178.25,28.37, 28.43)
Plot_SiteCheck(islands,j,"Kure",-178.36, -178.25,28.37, 28.43)

summary(subset(ad,ISLAND=="Laysan")) # identify extent of coordinates
Plot_SiteCheck(islands,ad,"Laysan",-171.8, -171.7,25.72, 25.81)
Plot_SiteCheck(islands,j,"Laysan",-171.8, -171.7,25.72, 25.81)

summary(subset(ad,ISLAND=="Lisianski")) # identify extent of coordinates
Plot_SiteCheck(islands,ad,"Lisianski",-174.1, -173.85,25.9, 26.1)
Plot_SiteCheck(islands,j,"Lisianski",-174.1, -173.85,25.9, 26.1)

summary(subset(ad,ISLAND=="Midway")) # identify extent of coordinates
Plot_SiteCheck(islands,ad,"Midway",-177.42, -177.2,28.18, 28.26)
Plot_SiteCheck(islands,j,"Midway",-177.42, -177.2,28.18, 28.26)

summary(subset(ad,ISLAND=="Pearl & Hermes")) # identify extent of coordinates
Plot_SiteCheck(islands,ad,"Pearl & Hermes",-175.9, -175.3,27.45, 27.86)
Plot_SiteCheck(islands,j,"Pearl & Hermes",-175.9, -175.3,27.45, 27.86)


output[18,]<-c("Site coordinates look good", "pending")

#Merge with SITEVISITIDs and remove columns we don't want before exporting
ad<-left_join(ad,site)
j<-left_join(j,site)

#Change - to NA
ad <- ad %>% 
  mutate(RECENT_GENERAL_CAUSE_CODE_1 = replace(RECENT_GENERAL_CAUSE_CODE_1, RECENT_GENERAL_CAUSE_CODE_1 =="-",NA))  %>%
  mutate(RECENT_SPECIFIC_CAUSE_CODE_1 = replace(RECENT_SPECIFIC_CAUSE_CODE_1, RECENT_SPECIFIC_CAUSE_CODE_1 == "-",NA)) %>%
  mutate(MORPH_CODE = replace(MORPH_CODE, MORPH_CODE == "-",NA)) %>%
  mutate(COLONYLENGTH = replace(COLONYLENGTH, COLONYLENGTH == "-",NA)) %>%
  mutate(OLDDEAD = replace(OLDDEAD, OLDDEAD == "-",NA)) %>%
  mutate(RECENTDEAD_1 = replace(RECENTDEAD_1, RECENTDEAD_1 == "-",NA)) %>%
  mutate(CONDITION_1 = replace(CONDITION_1, CONDITION_1 == "-",NA)) %>%
  mutate(SEVERITY_1 = replace(SEVERITY_1, SEVERITY_1 == "-",NA))  %>%
  mutate(EXTENT_1 = replace(EXTENT_1, EXTENT_1 == "-",NA))

View(ad)

#Change "-" TO na
j <- j %>% 
  mutate(MORPH_CODE = replace(MORPH_CODE, MORPH_CODE == ".", "-")) %>%
  mutate(COLONYLENGTH = replace(COLONYLENGTH, COLONYLENGTH == ".", "-"))%>%
  mutate(COLONYWIDTH = replace(COLONYWIDTH, COLONYWIDTH == ".", "-"))

View(j)


ad<-dplyr::select(ad,-c(x,y,Stratum,MIN_Z,MAX_Z))
j<-dplyr::select(j,-c(x,y,Stratum,MIN_Z,MAX_Z))

head(ad)

write.csv(ad,"PMNM2017_ADULTCOLONY_QCd.csv")
write.csv(j,"PMNM2017_JUVENILECOLONY_QCd.csv")



