#This script QCs the SfM data pulled directly from the geodatabse
#It is designed to merge the geodatabase with the site visit table and help flag errors to be corrected 
#Completed by Corinne Amir 12/18/2019
#updated by Courtney Couch 1/2/2020


rm=ls() #removes all objects from the current workspace

setwd("T:/Benthic/Data/SfM/QC")

#Upload necessary functions (not opening on my computer)
source("C:/Users/Corinne.Amir/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Corinne.Amir/Documents/GitHub/Benthic-Scripts/Functions/core_functions.R")

##read benthic data downloaded from Mission app and subset to the leg you need to QC
sfm.raw <- read.csv("HARAMP2019_demographic_v2_feb192020.csv") #for comparison (previously used)

head(sfm.raw);nrow(sfm.raw)



# Prepping The Data -----------------------------------------------------

#Turn "no" into 0
sfm.raw$NO_COLONY <- ifelse(sfm.raw$NO_COLONY == "No", "0", as.character(sfm.raw$NO_COLONY))
sfm.raw$EX_BOUND <- ifelse(sfm.raw$EX_BOUND == "No", "0", as.character(sfm.raw$EX_BOUND))
sfm.raw$JUVENILE <- ifelse(sfm.raw$JUVENILE == "No", "0", as.character(sfm.raw$JUVENILE))
sfm.raw$FRAGMENT <- ifelse(sfm.raw$FRAGMENT == "No", "0", as.character(sfm.raw$FRAGMENT))
sfm.raw$REMNANT <- ifelse(sfm.raw$REMNANT == "No", "0", as.character(sfm.raw$REMNANT))



#### If some column classes = logical, Run this function that removes logical NAs ####
RemoveLogicalNA <- function(b)
{	
  if (is.logical(b) == "TRUE") {   
    b[is.logical(b)] <- "NA"
    b <- as.factor(b)
  }
  return(b)
}
#### end function ####

#Add "NA" to columns with class = logical (whole column filled with italized, shaded "NA")
sfm.raw$RDCAUSE3 <- RemoveLogicalNA(sfm.raw$RDCAUSE3)
sfm.raw$CON_3 <- RemoveLogicalNA(sfm.raw$CON_3)



#Remove turn blank cells into NA or 0 depending on column class
sfm.factor <- sfm.raw[,sapply(sfm.raw, is.factor)]
sfm.num <- sfm.raw[,sapply(sfm.raw, is.integer)]
sfm.num <- cbind(sfm.num, (sfm.raw[,sapply(sfm.raw, is.character)]))


#Turn blanks into NA...not working
b <- ifelse(sfm.raw == "", "NA", as.factor(sfm.raw))

a <- function(data) {
  for(i in ncol(data)) {
    ifelse(data[i] == "", "NA", as.factor(i))
  }
  # for(i in ncol(data)) {
  #   ifelse(is.factor(i)== T & data[i] == " ", "NA", as.factor(i))
  # }
  data <-droplevels(data) 
  return(data)
}
 
sfm.raw <- a(sfm.raw)

sfm.raw[sfm.raw == " "] <- "NA"
sfm.raw[sfm.raw == "" & is.factor == T] <- "NA"
sfm.raw<-droplevels(sfm.raw)
str(sfm.raw)

#Fix columns that contain both NA and 0
sfm.raw <- as.factor(c(sfm.raw$CON_3, sfm.raw$CON_2)) #columns that should have integers
#columns that should have factors
#If column is a factor, turn 0 into NA NAs found alongsize 0 in SEV, EXTENT, 
#If column is a factor, turn 0 into NA NAs found alongsize 0 in SEV, EXTENT, 


#### Run sfm-specific SiteNumLeadingZeros function - we needed to modify this from the Core functions script because we have both - and _ in site name ####
# library(stringr)
# sfm.raw$SITE = str_replace(as.character(sfm.raw$SITE),"_","-")
# View(sfm.raw)

SiteNumLeadingZeros_SfM <- function(site_names)
{
  tmp<-levels(site_names)
  for (i in 1:length(tmp)) {
    s<-tmp[i]
    if (nchar(s)<9) {   # only change values where name length is too short ()
      ss<-strsplit(as.character(tmp[i]), split="[_ |,-]")
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
}
#### end function ####

# Change site numbers such as MAR-22 to MAR-0022
sfm.raw$SITE<-SiteNumLeadingZeros_SfM(sfm.raw$SITE)

sfm.raw$SEGMENT<-as.factor(sfm.raw$SEGMENT)
table(sfm.raw$SITE,sfm.raw$SEGMENT)


#Create a dataframe that houses all rows that have not been completely filled out (not including RD and CON-related columns)
sfm.missing.duplicate.rows <- rbind(
  analyst.missing <- filter(sfm.raw, ANALYST %in% c("NA")),
  site.missing <- filter(sfm.raw, SITE %in% c("NA")),
  seglength.missing <-filter(sfm.raw, SEGLENGTH %in% c("0")),
  segwidths.missing <- filter(sfm.raw, SEGWIDTH %in% c("0")),
  spcode.missing <-  filter(sfm.raw, SPCODE %in% c("NA")),
  morphcode.missing <-  filter(sfm.raw, MORPH_CODE %in% c("NA"))) 

sfm.missing <- sfm.missing.duplicate.rows[!duplicated(sfm.missing.duplicate.rows),] 
dim(sfm.missing)
View(sfm.missing)


#Identify all rows where NO_COLONY_ is -1 and all values beforehand are also filled in. These values are ok and should NOT be placed in the sfm.missing dataframe
no.colony.present <- sfm.missing %>%
  filter(NO_COLONY == "-1" & ANALYST != "NA" & SITE != "NA" & SEGLENGTH != "0" & SEGWIDTH != "0")
head(no.colony.present)


#Remove rows with no colony present from the sfm.missing dataframe
sfm.missing <- droplevels(anti_join(sfm.missing, no.colony.present))

sfm.missing$SEGMENT <- as.factor(sfm.missing$SEGMENT)
sfm.missing$SITE <- as.factor(sfm.missing$SITE)

#Save dataframe with missing values 
write.csv(sfm.missing, "sfm_missing_rows.csv") #get these rows repopulated (if missing metadata) or annotated before moving forward


#If charging forward and leaving rows with missing data behind, create a new dataframe where all rows with missing data have been removed
sfm <- droplevels(anti_join(sfm.raw, sfm.missing))

View(sfm)
nrow(sfm)

sfm$SEGMENT <- as.factor(sfm$SEGMENT)
sfm$SITE <- as.factor(sfm$SITE)


#Add column for segment area
levels(as.factor(sfm$SEGLENGTH))
sfm$SEGAREA <- sfm$SEGLENGTH*sfm$SEGWIDTH

#Still want this column?
# #Add column that differentiates adults from juveniles
# sfm$SHAPE_Leng <-  as.numeric(sfm$SHAPE_Leng)
# #sfm$Adult_Juvenile <- ifelse(sfm$SHAPE_Leng < 0.05 & sfm$FRAGMENT_Y != -1 | sfm$SEGAREA==1 & sfm$SPCODE != "PBER", "J", "A")
# sfm$Adult_Juvenile <- ifelse(sfm$SHAPE_Leng<0.05 & sfm$SPCODE != "PBER", "J", "A")


#How many site/segments were annotated 
sfm$site_seg<-paste(sfm$SITE,sfm$SEGMENT)
length(unique(sfm$site_seg)) #Feb 19 = 124 unique sites

# QC Checks ---------------------------------------------------------------
#Set up output csv file that reports the status of the qc checks
output<-data.frame(
  QC_check<-character(),
  Status<-character(),stringsAsFactors = FALSE)

output<-data.frame(
  QC_check,
  Status,stringsAsFactors = FALSE)



#1. Check if only part of a site-segment was removed and placed in the sfm.missing dataframe while the other part was placed in the sfm dataframe. Remove these site-segments.
partial_SiteSeg_removal <- inner_join(sfm.missing, sfm, by = c("SITE", "SEGMENT", "ANALYST")) 
head(partial_SiteSeg_removal) # a dataframe with no data will be displayed if site-segment pairs were NOT split between missing and populated dataframes = good

output[,1] <- c("Sites have been completely annotated", "YES")

#if dataframe is populated, export csv and fix the error
write.csv(partial_SiteSeg_removal, "Error_partial_filled_segments.csv")




#2.Check that the columns have the appropripate type of data (e.g. numeric vs. text) & no errant codes (e.g. ble instead of BLE) 
sapply(sfm,unique)
str(sfm) 
sapply(sfm, class)

output[2,]<-c("No errant codes","Yes") #change depending on output from previous lines of code



#3. All TRANSECT within v2 of the geodabase should = A (B is for repeats)
levels(sfm$TRANSECT)

output[3,]<-c("All transects = A","Yes") #change depending on output from previous lines of code


#3. Make sure that if NO_COLONY=-1 none of the following columns have been populated
sfm %>% filter(sfm$SPCODE == "NA" & sfm$NO_COLONY != -1)
sfm %>% filter(sfm$SPCODE != "NA" & sfm$NO_COLONY == -1)
sfm %>% filter(sfm$SPCODE == -1 & sfm$NO_COLONY == -1)
sfm %>% filter(sfm$JUVENILE == -1 & sfm$NO_COLONY == -1)
sfm %>% filter(sfm$FRAGMENT  == -1 & sfm$NO_COLONY == -1)
sfm %>% filter(sfm$REMNANT == -1 & sfm$NO_COLONY == -1)
sfm %>% filter(sfm$MORPH_CODE == "NA" & sfm$NO_COLONY != -1)
sfm %>% filter(sfm$MORPH_CODE != "NA" & sfm$NO_COLONY == -1)

output[3,]<-c("NO_COLONY segments filled correctly","Yes") 



#3. Calculate the number of annotated segments per site
##Create a summary table of #segments per site and check against tracking data sheet
seg.per.site <- ddply(sfm,.(SITE, SEGMENT), summarize, num.annotated = n_distinct(SEGMENT))
eval.seg.per.site <- as.data.frame(acast(seg.per.site, SITE~SEGMENT, length))
eval.seg.per.site$Total <- rowSums(eval.seg.per.site)
View(eval.seg.per.site) 

#use this file to evaluate where segments may be missing
write.csv(eval.seg.per.site, "Missing_seg_eval.csv")

output[3,]<-c("All segments within each site have been annotated","Some sites have unannotated segments -- ok") #change depending on output from previous line of code





#4. Check how many anotators exist for each segment within a site 
analyst.per.seg<-sfm %>% filter(ANALYST=="RS" | ANALYST=="MW" | ANALYST=="MA") #for comparison plots NOT calibration plots
analyst.per.seg <- ddply(sfm,.(SITE, SEGMENT), summarize, num.analyst = n_distinct(ANALYST))
analyst.multiple <- filter(analyst.per.seg, num.analyst>1)
analyst.multiple.names <- left_join(analyst.multiple, sfm[,1:6]) 
analyst.multiple.names <- analyst.multiple.names %>% unite(JOIN, SITE, SEGMENT, sep = "--")
analyst.multiple.names <-ddply(analyst.multiple.names,.(JOIN, ANALYST), summarize, num.colonies = n_distinct(FID))
View(analyst.multiple.names)

#use this file to evaluate where we are or are not expecting there to be additional analysts
write.csv(analyst.multiple.names, "Duplicate_analyst_eval.csv") 

output[4,]<-c("All segments have been annotated by one individuals","Multiple segments with > 1 annotator -- ok") #change depending on output from previous line of code

#Jan17=44 site-segs with at least 2 annotators




#5.Check for incorrect species-V:\PHOTOMOSAIC (1)\HARAMP\HARAMP_2019_codes.csv
ddply(sfm,.(SPCODE),summarize,temp=length(SPCODE))

output[5,]<-c("Species codes are correct","OK -- although several species present that aren't scored to species, only genus")





#6. Check that SEGWIDTH is correct (should have been apparent in qc #1).
levels(as.factor(sfm$SEGAREA)) #should all be 1 OR 2.5, unless otherwise stated 

output[6,]<-c("All segment widths are correct","Yes") #change depending on output from previous line of code


# Check if all segments contain both seglengths 
seg.length <-ddply(sfm,.(SITE, SEGMENT, ANALYST, SEGLENGTH), summarize, num.seglengths = n_distinct(SEGLENGTH)) 
eval.seg.length <- acast(seg.length, SITE~SEGMENT~ANALYST, length)   
View(eval.seg.length) #all cells that have been annotated should be "2" 

output[7,]<-c("All segments have seglengths 1.0 and 2.5","No errors in MA, ML, RS--ok") #change depending on output from previous line of code

# if there are errors, export a csv file for further analysis
write.csv(eval.seg.length, "Missing_seglength_eval.csv")




#8. Identify colonies flagged as Juveniles or Adults, but have the innocorrect segment area. make sure j = 1 and A = 2.5
sm.colonies.eval <- sfm %>% filter(Adult_Juvenile=="J",SEGAREA != 1, NO_COLONY_ !=-1) 
View(sm.colonies.eval)

lg.colonies.eval <- sfm %>% filter(Adult_Juvenile=="A",SEGAREA==1) 
View(lg.colonies.eval)

output[8,]<-c("Juveniles and Adult colonies have correct labeling","2 errors (1 PBER)")

#If rows have been flagged, export sm_colonies dataframe into a csv file for further QC
write.csv(sm.colonies.eval, "Juveniles_eval.csv")
write.csv(lg.colonies.eval, "Adults_eval.csv")



#9. Identify colonies with 0% recent dead, but has an RDCAUSE code - This check should result in 0 records   
sfm[sfm$RD_1=="0"& sfm$RDCAUSE1!="NA",]
sfm[sfm$RD_2=="0"& sfm$RDCAUSE2!="NA",]
sfm[sfm$RD_3=="0"& sfm$RDCAUSE3!="NA",]

output[9,]<-c("0% Recent Dead corals do NOT have an RDCAUSE code","Errors for AH--ok")



#10. Identify colonies with recent dead >0%, but there is no RDCAUSE code - This check should result in 0 records   
sfm[sfm$RD_1 >0 & sfm$RDCAUSE1=="NA",] #,rowSums(is.na(sfm)) != ncol(sfm),]
sfm[sfm$RD_2 >0 & sfm$RDCAUSE2=="NA",] #,rowSums(is.na(a)) != ncol(a), ]
sfm[sfm$RD_3 >0 & sfm$RDCAUSE3=="NA",] #,rowSums(is.na(a)) != ncol(a), ]

output[10,]<-c("All corals with RD >0 have an RDCAUSE code","YES")



#11. Identify colonies with NO % EXTENT, but a condition - This check should result in 0 records    
sfm[sfm$EXTENT_1=="0"& sfm$CON_1!="NA",]
sfm[sfm$EXTENT_2=="0"& sfm$CON_2!="NA",]
sfm[sfm$EXTENT_3=="0"& sfm$CON_3!="NA",] 

output[11,]<-c("All colonies with a condition have an extent","Errors for AH--ok")



#12. Identify colonies that have no condition, but a value in extent - This check should result in 0 records   
sfm[sfm$CON_1=="NA"& sfm$EXTENT_1!="0",] #rowSums(is.na(a)) != ncol(a),]
sfm[sfm$CON_2=="NA"& sfm$EXTNET_2!="0",]
sfm[sfm$CON_3=="NA"& sfm$EXTENT_3!="0",] #rowSums(is.na(a)) != ncol(a),]

output[12,]<-c("All colonies with NO condition also have NO extent","Yes")



#13. Identify colonies with nothing in condition column, but a value in severity. Double check that these shouldn't be 0  
sfm[sfm$EXTENT_1=="0"& sfm$SEV_1!="0"] #,rowSums(is.na(a)) != ncol(a),]
sfm[sfm$EXTENT_2=="0"& sfm$SEV_2!="0",]
sfm[sfm$EXTENT_3=="0"& sfm$SEV_3!="0",]

output[13,]<-c("Identify colonies with nothing in condition column, but a value in severity","Yes")



#14. make sure that the only rows with severity filled contain BLE or BLP in condition
sfm[sfm$SEV_1=="0"& sfm$CON_1 %in% c("BLE","BLP"),]
sfm[sfm$SEV_2=="0"& sfm$CON_2%in% c("BLE","BLP"),]
sfm[sfm$SEV_3=="0"& sfm$CON_3%in% c("BLE","BLP"),]

`%notin%` <- Negate(`%in%`)
sfm[sfm$SEV_1!="0"& sfm$CON_1 %notin% c("BLE","BLP"),]
sfm[sfm$SEV_2!="0"& sfm$CON_2 %notin% c("BLE","BLP"),]
sfm[sfm$SEV_3!="0"& sfm$CON_3 %notin% c("BLE","BLP"),]

output[14,]<-c("Severity value is present only in colonies with CON = BLE and BLP","Errors for AH--ok")



#15. RD + OD is not greater than 100%
sfm$OLDDEAD<-as.numeric(sfm$OLDDEAD)
sfm$RD_2<-as.numeric(sfm$RD_2)
sfm$RD_1<-as.numeric(sfm$RD_1)
sfm$RD_3<-as.numeric(sfm$RD_3)
sfm$totaldead = sfm$RD_1+sfm$RD_2+sfm$RD_3 + sfm$OLDDEAD
sfm[sfm$totaldead>100,]

output[15,]<-c("RD + OD <=100%","Yes")



#16. Write files for qc #9-15 that have errors (optional)
SEV_1_error <- sfm[sfm$SEV_1=="0"& sfm$CON_1 %in% c("BLE","BLP"),]
write.csv(SEV_1_error, "SEV_1_error.csv") 



#Export QC output table with appropriate file name
setwd("C:/Users/Corinne.Amir/Documents/GitHub/Benthic-Scripts/SfM")
write.csv(output,"HARAMP2019_sfm_output.csv")



# Export QC'd data ----------------------------------------------------------------------------

ad<-subset(sfm,Adult_Juvenile=="A"|NO_COLONY_==-1 & SEGLENGTH==2.5)
ad<-subset(ad,select=-c(Adult_Juvenile,totaldead))
j<-subset(sfm,Adult_Juvenile=="J"&SEGLENGTH!=2.5) # includes segments where NO_COLONY = -1
j<-subset(j,select=c(FID,ANALYST,OBS_YEAR,SITE,SEGMENT,SEGLENGTH,SEGWIDTH,NO_COLONY_,SPCODE,FRAGMENT_Y,MORPH_CODE,EX_BOUND,SHAPE_Leng,SEGAREA))

analyst.per.seg.j<-j %>% filter(ANALYST=="RS" | ANALYST=="MW" | ANALYST=="MA") #for comparison plots NOT calibration plots
analyst.per.seg.j <- ddply(j,.(SITE, SEGMENT), summarize, num.analyst = n_distinct(ANALYST))
analyst.multiple.j <- filter(analyst.per.seg.j, num.analyst>1) 

analyst.per.seg.ad<-ad %>% filter(ANALYST=="RS" | ANALYST=="MW" | ANALYST=="MA") #for comparison plots NOT calibration plots
analyst.per.seg.ad$ANALYST<-droplevels(analyst.per.seg.ad$ANALYST)
analyst.per.seg.ad <- ddply(analyst.per.seg.ad,.(SITE, SEGMENT), summarize, num.analyst = n_distinct(ANALYST))
analyst.multiple.ad <- filter(analyst.per.seg.ad, num.analyst>1) 




#Make sure that you have all the segments that are reported as annotated in the tracking datasheet
seglist<-read.csv("INSERT FILE PATH TO SEGMENT LIST PULLED FROM THE TRACKING SHEET") #We haven't been recording this information yet. 
ad_e<-ddply(ad,.(SITE),summarize,n=length(unique(SEGMENT)))
adseglist<-merge(ad,seglist,by=c(SITE,n),all=T)


#Export QC'd data
#Data ends up in "T:/Benthic/Data/SfM/QC" NOT within Benthic-Scripts Github folder
setwd('T:/Benthic/Data/SfM/QC/')
write.csv(ad,"HARAMP2019_QCdsfm_ADULT.csv",row.names = F)
write.csv(j,"HARAMP2019_QCdsfm_JUV.csv",row.names = F)




# Join the sitevisit table with the QC'd sfm geodatabase table ---------------------------------------------------------------

#Make sure columns being merged between both dataframes have the same class
sfm.sitevisit$SITE <- as.factor(sfm.sitevisit$SITE)
sfm$SITE <- as.factor(sfm$SITE)

sfm.sitevisit$SEGMENT <- as.factor(sfm.sitevisit$SEGMENT)
sfm$SEGMENT <- as.factor(sfm$SEGMENT)



#Remove logical NAs from the sitevisit table if they exist
#function is housed within the "Prepping the data" section 
str(sfm.sitevisit) 
sfm.sitevisit$SITE_MAX_DEPTH <- RemoveLogicalNA(sfm.sitevisit$SITE_MAX_DEPTH)
sfm.sitevisit$SITE_MIN_DEPTH <- RemoveLogicalNA(sfm.sitevisit$SITE_MIN_DEPTH)



#Join geodatabase with site visit table 
meta.geo.merge <- inner_join(sfm.sitevisit, sfm)
meta.geo.merge$SITE <- as.factor(meta.geo.merge$SITE) #turn SITE back into factor



#Make a table of all sites/segments from the site visit table that have not been annotated/did not merge with the geodatabse dataframe
sitevisit.not.annotated <- anti_join(sfm.sitevisit, meta.geo.merge)
write.csv(sitevisit.not.annotated, "Sites_without_annotation.csv")



#Make a table of all sites/segments from the geodatabase that did not merge with the site visit table -- should = 0 observations
not_merged <- anti_join(sfm, meta.geo.merge)
sapply(not_merged,unique) #check if it is a specific SITE, SEGMENT, etc that is not merging with the site visit table
write.csv(not_merged, "Geodatabase_output_incompatible_wSiteVisit.csv")



#Dataframe for number of sites within a given island
site_by_island <- meta.geo.merge %>%
  group_by(ISLAND) %>%
  summarize (n_distinct(SITE)) 
View(site_by_island)


#Dataframe for number of transects within a given site
segment_per_site <- meta.geo.merge %>%
  group_by(SITE) %>%
  summarize (n_distinct(SEGMENT)) 
View(segment_per_site) 

