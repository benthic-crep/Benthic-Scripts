#By Jonathan Charendoff

rm(list=ls())

library(dplyr)
source("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/Functions/core_functions.R")

# Read dataframes pulled directly from geodatabases
setwd("N:/StRS_Sites_Projects/REA Belts/Data Output/Demography Data/2025")



# Read in V2 geodatabase --------------------------------------------------
v2 <- read.csv("SE2503_Demographics_20260619.csv")
sitelist <- read.csv("SE2503_sitelist_20260619.csv")[,1:4]


# Prep the v2 geodatabase data -----------------------------------------------------

#Reformat v2 Site names
v2$SITE<-gsub("_", "-", v2$SITE)
levels(as.factor(v2$SITE))

sfm.raw <- v2
dim(sfm.raw)

# Change site numbers such as MAR-22 to MAR-0022
sfm.raw$SITE<-SiteNumLeadingZeros(as.factor(sfm.raw$SITE))


#If not already present from running the v1-v2 merge script, add column for site_segment
sfm.raw$site_seg<-paste(sfm.raw$SITE,sfm.raw$SEGMENT)


# Check to see if any sites are missing (should have 106 sites)
sitespresent <- data.frame(unique(sfm.raw$SITE)) #Get list of sites exported from gdb
sitespresent <- sitespresent %>% filter(unique.sfm.raw.SITE.!=" " & unique.sfm.raw.SITE.!="A"
                                        &unique.sfm.raw.SITE.!="" & unique.sfm.raw.SITE.!="SE2503") #Remove erroneous site names
sitespresent$df <- rep("present",times = nrow(sitespresent)) #Add column to differentiate from "master" site list
colnames(sitespresent) <- c("SITE", "df")

sitelist$df <- rep("expected",times = nrow(sitelist))
sitelist$SITE<-SiteNumLeadingZeros(as.factor(sitelist$SITE))

sites <- full_join(sitespresent,sitelist, by="SITE")
sites <- sites %>% filter(is.na(df.x) | is.na(df.y)) #Flag sites that are missing in the site list or gdb export
View(sites) # still missing 5 sites....?
write.csv(sites, "./QC Reports/missing_sites.csv") # MOL-2266 and HAW-4224 were dropped because they weren't completed


seglist<-ddply(sfm.raw,.(SITE),summarize,n=length(unique(SEGMENT))) #there should be no sites that have less than 3 segments


#Fill in known values if data are missing
colnames(sfm.raw)[1] <- "ANALYST"

sfm.raw$SEGMENT<-as.factor(sfm.raw$SEGMENT)
table(sfm.raw$SITE,sfm.raw$SEGMENT)


#Create a dataframe that houses all rows that have not been completely filled out (not including RD and CON-related columns)
sfm.raw$ANALYST <- as.factor(sfm.raw$ANALYST)
sfm.raw$SITE <- as.factor(sfm.raw$SITE)
sfm.raw$SEGMENT <- as.factor(sfm.raw$SEGMENT)

sfm.missing.duplicate.rows <- rbind(
  analyst.missing <- filter(sfm.raw, ANALYST %in% c(""," ", NA)),
  site.missing <- filter(sfm.raw, SITE %in% c("NA-   NA", "-   NA", "A-   NA", NA)),
  transect.missing <- filter(sfm.raw, TRANSECT %in% c(" ", NA)),
  seglength.missing <-filter(sfm.raw, SEGLENGTH %in% c(0.0, "0","NA", NA)),
  segwidths.missing <- filter(sfm.raw, SEGWIDTH %in% c("0", "NA", NA)),
  spcode.missing <-  filter(sfm.raw, SPCODE %in% c("NA", " ", "", NA)),
  morphcode.missing <-  filter(sfm.raw, MORPH_CODE %in% c("NA", " ", "", NA)),
  transect.missing <-  filter(sfm.raw, TRANSECT %in% c(0, "<Null>", " ", NA)),
  segmennt.missing <-  filter(sfm.raw, SEGMENT %in% c("NA", "<NA>", NA)),
  year.missing <-  filter(sfm.raw, OBS_YEAR != 2025),
  mission.missing <-  filter(sfm.raw, MISSION_ID != "SE2503"))

sfm.missing <- sfm.missing.duplicate.rows[!duplicated(sfm.missing.duplicate.rows),] 
dim(sfm.missing)
View(sfm.missing)


#Identify all rows where NO_COLONY_ is -1 and all values beforehand are also filled in. These values are ok and should NOT be placed in the sfm.missing dataframe
no.colony.present <- sfm.missing %>%
  filter(NO_COLONY == "-1" & ANALYST != "NA" & SITE != "NA" & SEGLENGTH != "0" & SEGWIDTH != "0")
head(no.colony.present)


#Remove rows with no colony present from the sfm.missing dataframe IF they aren't missing anything else important
sfm.missing <- droplevels(anti_join(sfm.missing, no.colony.present))
dim(sfm.missing)

#Save dataframe with missing values 
write.csv(sfm.missing, "./QC Reports/sfm_missing_rows.csv") #get these rows repopulated (if missing metadata) or annotated before moving forward


#If charging forward and leaving rows with missing data behind, create a new dataframe where all rows with missing data have been removed
sfm <- droplevels(anti_join(sfm.raw, sfm.missing))

nrow(sfm) 


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
str(sfm) # no columns are logical = skip this step

sfm$EXTENT_3 <- RemoveLogicalNA(sfm$EXTENT_3)
sfm$CON_3 <- RemoveLogicalNA(sfm$CON_3)
sfm$SEV_3 <- RemoveLogicalNA(sfm$SEV_3)

#Fix columns with incorrect factor levels
sapply(sfm,unique)

#sfm <- sfm %>% filter(SITE != "-   NA" & SITE != "SE1902-   NA") #cant find where these errors are in the gdb
sfm$OLD_DEAD <- as.numeric(sfm$OLD_DEAD)



sfm<-sfm %>% mutate(REMNANT = mapvalues(REMNANT, c(NA), c(0)),
                    JUVENILE = mapvalues(JUVENILE, c(NA), c(0)),
                    EX_BOUND = mapvalues(EX_BOUND, c(NA), c(0)),
                    NO_COLONY = mapvalues(NO_COLONY, c(NA), c(0)),
                    RDCAUSE1 = mapvalues(RDCAUSE1, c("NA", "", " "), c(NA, NA, NA)),
                    RDCAUSE2 = mapvalues(RDCAUSE2, c("NA", "", " "), c(NA, NA, NA)),
                    RDCAUSE3 = mapvalues(RDCAUSE3, c("NA", "", " "), c(NA, NA, NA)),
                    RD_1 = mapvalues(RD_1, c(NA), c(0)),
                    RD_2 = mapvalues(RD_2, c(NA), c(0)),
                    RD_3 = mapvalues(RD_3, c(NA), c(0)),
                    CON_1 = mapvalues(CON_1, c("", " "), c(NA, NA)),
                    CON_2 = mapvalues(CON_2, c("", " "), c(NA, NA)),
                    CON_3 = mapvalues(CON_3, c("NA", "", " "), c(NA, NA, NA)),
                    EXTENT_3 = mapvalues(EXTENT_3, c("NA", "", 0), c(NA, NA, NA)),
                    SEV_3 = mapvalues(SEV_3, c("NA", "", 0), c(NA, NA, NA)),
                    EXTENT_2 = mapvalues(EXTENT_2, c("NA", "", 0), c(NA, NA, NA)),
                    EXTENT_1 = mapvalues(EXTENT_1, c("NA", "", 0), c(NA, NA, NA)),
                    SEV_2 = mapvalues(SEV_2, c("NA", "", 0), c(NA, NA, NA)),
                    SEV_1 = mapvalues(SEV_1, c("NA", "", 0), c(NA, NA, NA)),
                    OLD_DEAD = mapvalues(OLD_DEAD, c(NA), c(0)),
                    SPCODE = mapvalues(SPCODE, c(""), c(NA)),
                    MORPH_CODE = mapvalues(MORPH_CODE, c(""), c(NA))) 

#old dead isn't recorded for Juvs or remnants
View(sfm[which(sfm$REMNANT == -1 & sfm$OLD_DEAD !=0),])
sfm$OLD_DEAD[which(sfm$JUVENILE == -1 | sfm$REMNANT == -1)] <- NA

#Seglength for juvs is 1m not 2.5 CHANGE WHEN ACTUALLY DEALING WITH DATA 
View(sfm[which(sfm$JUVENILE == -1 & sfm$SEGLENGTH !=1),])
#sfm$SEGLENGTH[which(sfm$JUVENILE == -1)] <- 1

#Add column for segment area
levels(as.factor(sfm$SEGLENGTH))
sfm$SEGAREA <- sfm$SEGLENGTH*sfm$SEGWIDTH


#Miscellaneous changes needed 
sapply(sfm,unique)

length(unique(sfm$SITE)) # 
length(unique(sfm$site_seg)) #


# QC Checks -------------------------------------------------------------------------------------
#Set up output csv file that reports the status of the qc checks
output<-data.frame(
  QC_check<-character(),
  Status<-character(),stringsAsFactors = FALSE)



#1. Check if only part of a site-segment was removed and placed in the sfm.missing dataframe while the other part was placed in the sfm dataframe. Remove these site-segments.
partial_SiteSeg_removal <- inner_join(sfm.missing, sfm, by = c("SITE", "SEGMENT")) 
head(partial_SiteSeg_removal) # a dataframe with no data will be displayed if site-segment pairs were NOT split between missing and populated dataframes = good
partial_SiteSeg_removal <- droplevels(partial_SiteSeg_removal)
unique(partial_SiteSeg_removal$SITE)

output[1,] <- c("Sites have been completely annotated", "YES") #change depending on previous lines of code


#if dataframe is populated, export csv and fix the error
write.csv(partial_SiteSeg_removal, "QC Reports/Error_partial_filled_segments.csv")



#2.Check that the columns have the appropripate type of data (e.g. numeric vs. text) & no errant codes (e.g. SEV and/or RD columns contain NA) 
sapply(sfm,levels)
str(sfm) 
sapply(sfm, class)

output[2,]<-c("No errant codes", "some logical NAs and blanks throughout -- ok")



# #3. All TRANSECT within v2 of the geodabase should = A (B is for repeats)
 filter(sfm, TRANSECT != "A")
 output[3,]<-c("All transects = A","YES") #change depending on output from previous lines of code


#4. Make sure that if NO_COLONY=-1 none of the following columns have been populated
sfm %>% filter(sfm$SPCODE == "NA" & sfm$NO_COLONY != -1)
sfm %>% filter(sfm$SPCODE != "NA" & sfm$NO_COLONY == -1)
sfm %>% filter(sfm$REMNANT == -1 & sfm$NO_COLONY == -1)
sfm %>% filter(sfm$MORPH_CODE == "NA" & sfm$NO_COLONY != -1)
sfm %>% filter(sfm$MORPH_CODE != "NA" & sfm$NO_COLONY == -1)

output[4,]<-c("NO_COLONY segments filled correctly","YES") 



#5. Calculate the number of annotated segments per site and check that all segments contain both seglengths (except segment 15)
##Create a summary table of #segments per site and check against tracking data sheet
seg.per.site <- ddply(sfm,.(SITE, SEGMENT, SEGLENGTH), summarize, num.annotated = n_distinct(SEGLENGTH))
eval.seg.per.site <- as.data.frame(acast(seg.per.site, SITE~SEGMENT, length))
eval.seg.per.site$Total <- rowSums(eval.seg.per.site)
View(eval.seg.per.site) 

#use this file to evaluate where segments may be missing
write.csv(eval.seg.per.site, "QC Reports/Missing_seg_eval.csv")

output[5,]<-c("All annotated segments have correct seg counts","YES") #change depending on output from previous line of code


# #6. Make sure only 1 annotator exists per site_seg-no longer applicable since multiple people did belt corrections after cross checking
analyst.per.site.seg <- ddply(sfm,.(site_seg), summarize, num.analyst = n_distinct(ANALYST))
filter(analyst.per.site.seg, num.analyst>1)
# 
 output[6,]<-c("All site_segs annotated by one person","YES")
# 


#7.Check for incorrect species-V:\PHOTOMOSAIC (1)\HARAMP\HARAMP_2019_codes.csv
coral.counts <- ddply(sfm,.(SPCODE),summarize,temp=length(SPCODE))
suspect <- sfm[sfm$SPCODE %in% coral.counts$SPCODE[coral.counts$temp <10],]
write.csv(unique(sfm[c("SPCODE", "MORPH_CODE")]), "QC Reports/SP-MORPH_combo.csv")##export then check for any weird combos that dont make logical sense like PLIC-BR if probably PLIG-BR
sus.morph <- read.csv("QC Reports/SP-MORPH_combo_checked.csv")
sus.morph$combo <- paste(sus.morph$SPCODE, sus.morph$MORPH_CODE, sep = "_")
sfm.suspect <- sfm; sfm.suspect$combo <- paste(sfm.suspect$SPCODE, sfm.suspect$MORPH_CODE, sep = "_")
sfm.suspect <-  filter(sfm.suspect, combo %in% sus.morph$combo)


write.csv(suspect, "QC Reports/suspect_corals.csv")
write.csv(sfm.suspect, "QC Reports/suspect_morphs.csv")

output[7,]<-c("Species codes are correct","TBD")


#8. Check that SEGWIDTH is correct (should have been apparent in qc #1).
levels(as.factor(sfm$SEGAREA)) #should all be 1 OR 2.5, unless otherwise stated 
area <- subset(sfm, SEGAREA != 2.5 & SEGAREA != 1)

output[8,]<-c("All segment widths are correct","YES") #change depending on output from previous line of code


#9. Identify colonies flagged as Juveniles or Adults, but have the innocorrect segment area. make sure j = 1 and A = 2.5
sm.colonies.eval <- sfm %>% filter(JUVENILE== -1,SEGAREA != 1); sm.colonies.eval
lg.colonies.eval <- sfm %>% filter(JUVENILE==0,SEGAREA==1, NO_COLONY==0); lg.colonies.eval

output[9,]<-c("Juveniles and Adult colonies have correct labeling", "YES")


#If rows have been flagged, export sm_colonies dataframe into a csv file for further QC
write.csv(sm.colonies.eval, "QC Reports/Juveniles_eval.csv")
write.csv(lg.colonies.eval, "QC Reports/Adults_eval.csv")


#10. Identify colonies have the same CON code across multiple CON columns
sfm$CON_3 <- as.character(sfm$CON_3)
CON_dup <- sfm %>% filter(!is.na(CON_1)) %>% filter(!is.na(CON_2))
levels(CON_dup$CON_1) # come up with complete list of codes used in CON column
levels(CON_dup$CON_2)
levels(CON_dup$CON_3)

CON_dup$CON_1 <-factor(CON_dup$CON_1, levels=
                         c("ALG", "BLE","BLP"," ","NA","DAMG" ,"DIS" ,"FUG","PRS","PTR" ,"SGA","TIN"))
CON_dup$CON_2 <-factor(CON_dup$CON_2, levels=
                         c("ALG", "BLE","BLP"," " ,"NA","DAMG" ,"DIS" ,"FUG","PRS","PTR" ,"SGA","TIN")) #Give columns the full list of codes used in the dataaset
CON_dup$CON_3 <-factor(CON_dup$CON_3, levels=
                         c("ALG", "BLE","BLP", " " ,"NA","DAMG" ,"DIS" ,"FUG","PRS","PTR" ,"SGA","TIN"))

CON_check1 <- CON_dup %>% filter(CON_1==CON_2);nrow(CON_check1) # extract rows with duplicate levels among CON_1,2, and 3
CON_check2 <- CON_dup %>% filter(CON_1==CON_3);nrow(CON_check2) 
CON_check3 <- CON_dup %>% filter(CON_2==CON_3);nrow(CON_check3) 

output[10,]<-c("Corals do not have duplicate CON codes","YES")



#11. Identify colonies that have the same RDCAUSE code across multiple RDCAUSE columns
sfm$RDCAUSE3 <- as.factor(sfm$RDCAUSE3)
RD_dup <- sfm %>% filter(!is.na(RDCAUSE1)) %>% filter(!is.na(RDCAUSE2))

levels(RD_dup$RDCAUSE1) # come up with complete list of codes used in RD column
levels(RD_dup$RDCAUSE2)
levels(RD_dup$RDCAUSE3)

RD_dup$RDCAUSE1 <-factor(RD_dup$RDCAUSE1, levels= c("CIL","COTS" ,"DAMG", "DZGN", "FISH" ,"GAST","MACA"," ",
                                                    "NA","OVRG","PRED","SEDI","TLS" ,"TUNI","UNKN")) 
RD_dup$RDCAUSE2 <-factor(RD_dup$RDCAUSE2, levels= c("CIL","COTS" ,"DAMG", "DZGN", "FISH" ,"GAST","MACA"," ",
                                                    "NA","OVRG","PRED","SEDI","TLS" ,"TUNI","UNKN")) #Give columns the full list of codes used in the dataaset
RD_dup$RDCAUSE3 <-factor(RD_dup$RDCAUSE3, levels= c("CIL","COTS" ,"DAMG", "DZGN", "FISH" ,"GAST", "MACA"," ",
                                                    "NA","OVRG","PRED","SEDI","TLS" ,"TUNI","UNKN"))

RD_check1 <- RD_dup %>% filter(RDCAUSE1==RDCAUSE2);nrow(RD_check1) # extract rows with duplicate levels among RDCAUSE1,2, and 3
RD_check2 <- RD_dup %>% filter(RDCAUSE1==RDCAUSE3);nrow(RD_check2) # should be empty
RD_check3 <- RD_dup %>% filter(RDCAUSE2==RDCAUSE3);nrow(RD_check3) 

output[11,]<-c("Corals do not have duplicate RD codes","YES")



#12. Identify colonies with 0% recent dead, but has an RDCAUSE code - This check should result in 0 records   
RD_NAcheck1 <- sfm[which(sfm$RD_1== 0 | is.na(sfm$RD_1) & !is.na(sfm$RDCAUSE1)),]#; unique(a$site_seg)
RD_NAcheck2 <- sfm[which(sfm$RD_2==0 | is.na(sfm$RD_2) & !is.na(sfm$RDCAUSE2)),]
RD_NAcheck3 <- sfm[which(sfm$RD_3== 0 | is.na(sfm$RD_3) & !is.na(sfm$RDCAUSE3)),]
RD_NACheck <- rbind(RD_NAcheck1, RD_NAcheck2, RD_NAcheck3)

write.csv(RD_NACheck, "QC Reports/RD_eval.csv")
output[12,]<-c("0% Recent Dead corals do NOT have an RDCAUSE code","YES")



 #13. Identify colonies with recent dead >0%, but there is no RDCAUSE code - This check should result in 0 records   
RD_Cause_check1 <- sfm[which(sfm$RD_1 >0 & is.na(sfm$RDCAUSE1)),] #,rowSums(is.na(sfm)) != ncol(sfm),]
RD_Cause_check2 <- sfm[which(sfm$RD_2 >0 & is.na(sfm$RDCAUSE2)),] #,rowSums(is.na(a)) != ncol(a), ]
RD_Cause_check3 <- sfm[which(sfm$RD_3 >0 & is.na(sfm$RDCAUSE3)),] #,rowSums(is.na(a)) != ncol(a), ]
RD_Cause_Check <- rbind(RD_Cause_check1, RD_Cause_check2, RD_Cause_check3)

write.csv(RD_Cause_Check, "QC Reports/RD_cause_eval.csv")
output[13,]<-c("All corals with RD >0 have an RDCAUSE code","YES")



#14. Identify colonies with NO % EXTENT, but a condition - This check should result in 0 records    
EXTENT_check1 <- sfm[which(is.na(sfm$EXTENT_1) & !is.na(sfm$CON_1)),]
EXTENT_check2 <- sfm[which(is.na(sfm$EXTENT_2) & !is.na(sfm$CON_2)),]
EXTENT_check3 <- sfm[which(is.na(sfm$EXTENT_3) & !is.na(sfm$CON_3)),] 
EXTENT_Check <- rbind(EXTENT_check1, EXTENT_check2, EXTENT_check3)

write.csv(EXTENT_Check, "QC Reports/Extent_eval.csv")
output[14,]<-c("All colonies with a condition have an extent", "YES")



#15. Identify colonies that have no condition, but a value in extent - This check should result in 0 records   
sfm[which(is.na(sfm$CON_1) & sfm$EXTENT_1!=0),] 
sfm[which(is.na(sfm$CON_2) & sfm$EXTNET_2!=0),]
sfm[which(is.na(sfm$CON_3) & sfm$EXTENT_3!=0),] #rowSums(is.na(a)) != ncol(a),]

output[15,]<-c("All colonies with NO condition also have NO extent","YES")


#16. Identify colonies with nothing in condition column, but a value in severity. Double check that these shouldn't be 0  
sfm[which(is.na(sfm$EXTENT_1)& !is.na(sfm$SEV_1)),] #,rowSums(is.na(a)) != ncol(a),]
sfm[which(is.na(sfm$EXTENT_2)& !is.na(sfm$SEV_2)),]
sfm[which(is.na(sfm$EXTENT_3)& !is.na(sfm$SEV_3)),]

output[16,]<-c("All colonies with NO extent have NO severity","YES")


#17. Make sure that the only rows with severity filled contain BLE or BLP in condition
sfm[which(is.na(sfm$SEV_1) & sfm$CON_1 =="BLE"),]
sfm[which(is.na(sfm$SEV_2) & sfm$CON_2 =="BLE"),]
sfm[which(is.na(sfm$SEV_3) & sfm$CON_3 =="BLE"),]


sfm[which(sfm$SEV_1 > 0 & sfm$CON_1 !="BLE"),]
sfm[which(sfm$SEV_2 > 0 & sfm$CON_2 !="BLE"),]
sfm[which(as.numeric(sfm$SEV_3) > 0 & sfm$CON_3 !="BLE"),]

output[17,]<-c("Severity value is present only in colonies with CON = BLE", "YES")



#18. Make sure that values in SEV are only NA, 2, or 3 
sfm[which(sfm$SEV_1>3 | sfm$SEV_1<2),]
sfm[which(sfm$SEV_2>3 | sfm$SEV_2<2),]
sfm[which(as.numeric(sfm$SEV_3)>3 | as.numeric(sfm$SEV_3)<2),]

output[18,]<-c("Severity values are whole numbers between 0-3","YES")



#19. Check if there are any EX_BOUND colonies that have a conspicuously small shape length
sfm[which(sfm$EX_BOUND == -1 & sfm$Shape_Length < 0.25),]

output[19,]<-c("EX_BOUND colonies have a non-conspicuous shape length", "YES")


#20. RD + OD is not greater than 100%
sfm$OLD_DEAD<-as.numeric(sfm$OLD_DEAD)
sfm$RD_2<-as.numeric(sfm$RD_2)
sfm$RD_1<-as.numeric(sfm$RD_1)
sfm$RD_3<-as.numeric(sfm$RD_3)
sfm$totaldead = sfm$RD_1+sfm$RD_2+sfm$RD_3 + sfm$OLD_DEAD
sfm[which(sfm$totaldead>100),] #RD + OD can equal 100, but not >100

output[20,]<-c("RD + OD <=100%","YES")


#21. Check for duplicate rows in dataframe - it may be ok that colonies are exactly the same length
a<-sfm %>% group_by(Shape_Length,site_seg) %>% filter(n()>1)

output[21,]<-c("No duplicate rows","YES")



#22. Make sure that if a coral is a remnant, that SEGLENGTH = 2.5 regardless of max diameter
remnant.seglegth <- sfm %>% filter(REMNANT == "-1" & SEGLENGTH != 2.5) 
remnant.olddead <- sfm %>% filter(REMNANT == "-1" & OLD_DEAD != 0)
remnant.error <- rbind(remnant.seglegth,remnant.olddead); nrow(remnant.error)

output[22,]<-c("REMNANT filled out correctly","YES")


#23. Make sure that if a coral is a juvenile, they don't have a value in OLDDEAD, or any RD or CON columns
juv.olddead <- sfm %>% filter(JUVENILE == -1 & !is.na(OLD_DEAD) | JUVENILE == -1 & !is.na(CON_1) | 
                                JUVENILE == -1 & !is.na(CON_2) | JUVENILE == -1 & !is.na(CON_3) | 
                                JUVENILE == -1 & !is.na(RDCAUSE1) |JUVENILE == -1 & !is.na(RDCAUSE2) |
                                JUVENILE == -1 & !is.na(RDCAUSE3)); nrow(juv.olddead)

rem.olddead <- sfm %>% filter(REMNANT == -1 & !is.na(OLD_DEAD) | 
                                REMNANT == -1 & !is.na(RDCAUSE1) |REMNANT == -1 & !is.na(RDCAUSE2) |
                                REMNANT == -1 & !is.na(RDCAUSE3)); nrow(rem.olddead)

output[23,]<-c("Juvenile colonies have no OLDDEAD, RD, or CON","YES")

#24 Make sure that juveniles and remnants are smaller than 5cm
sm.size <- sfm %>% filter(JUVENILE == -1 & Shape_Length >= .05|
                             REMNANT == -1 & Shape_Length >= .05|
                            JUVENILE != -1 & REMNANT != -1 & Shape_Length < .05) %>% 
                    filter(NO_COLONY != -1)

write.csv(sm.size, "QC Reports/juv_rem_error.csv")
output[24,]<-c("Juvenile and remnants are < 5cm","Fixing")

#Export QC output table with appropriate file name
write.csv(output,"QC Reports/SE2503_QC_output_20260619.csv")



# Export QC'd data ----------------------------------------------------------------------------

sfm$COLONYID<-c(1:length(sfm$ANALYST))
sfm<-subset(sfm,select=-totaldead)

#Separate by adults and juveniles
ad<-subset(sfm,JUVENILE==0|REMNANT==-1)

j<-subset(sfm,JUVENILE=="-1") # includes segments where NO_COLONY = -1
j<-subset(j,select=c(COLONYID,ANALYST,OBS_YEAR,MISSION_ID,SITE,TRANSECT,SEGMENT,SEGLENGTH,SEGWIDTH,NO_COLONY,SPCODE,MORPH_CODE,
                     EX_BOUND,JUVENILE,REMNANT,Shape_Length,SEGAREA))

# #For annotator comparison study
# analyst.per.seg.j<-j %>% filter(ANALYST=="RS" | ANALYST=="MW" | ANALYST=="MA") #for comparison plots NOT calibration plots
# analyst.per.seg.j <- ddply(j,.(SITE, SEGMENT), summarize, num.analyst = n_distinct(ANALYST))
# analyst.multiple.j <- filter(analyst.per.seg.j, num.analyst>1) 
# 
# analyst.per.seg.ad<-ad %>% filter(ANALYST=="RS" | ANALYST=="MW" | ANALYST=="MA") #for comparison plots NOT calibration plots
# analyst.per.seg.ad$ANALYST<-droplevels(analyst.per.seg.ad$ANALYST)
# analyst.per.seg.ad <- ddply(analyst.per.seg.ad,.(SITE, SEGMENT), summarize, num.analyst = n_distinct(ANALYST))
# analyst.multiple.ad <- filter(analyst.per.seg.ad, num.analyst>1) 
# 


#Make sure that you have all the segments that are reported as annotated in the tracking datasheet (checked in beginning of script but not official)
seglist<-read.csv("INSERT FILE PATH TO SEGMENT LIST PULLED FROM THE TRACKING SHEET") #We haven't been recording this information yet. 
ad_e<-ddply(ad,.(SITE),summarize,n=length(unique(SEGMENT)))
adseglist<-merge(ad,seglist,by=c(SITE,n),all=T)


#Export QC'd data
#Data ends up in "T:/Benthic/Data/SfM/QC" NOT within Benthic-Scripts Github folder
setwd('T:/Benthic/Data/SfM/QC/')
write.csv(ad,"SfM_Adult_demographic_SE2406.csv",row.names = F)
write.csv(j,"SfM_Juvenile_Demographic_SE2406.csv",row.names = F)
write.csv(sfm,"SfM_Demographic_SE2406.csv",row.names = F)


# Prepare for InPort-Merge together survey master table and Inport ready (Corinne ran final updates) colony-level data ------------------------------------
j <- read.csv("T:/Benthic/Data/SfM/QC/SfM_Juvenile_Demographic_MHI_2019.csv");j<-subset(j,select= -c(X))
ad <- read.csv("T:/Benthic/Data/SfM/QC/SfM_Adult_demographic_MHI_2019.csv");ad<-subset(ad,select= -c(site_seg)) 

#SfM/ADULT: Column Names Changes -------------------------------------------------
colnames(ad)[colnames(ad)=="RD_1"]<-"RECENTDEAD_1" #Change column name
colnames(ad)[colnames(ad)=="RDCAUSE1"]<-"RECENT_SPECIFIC_CAUSE_CODE_1" #Change column name
colnames(ad)[colnames(ad)=="RD_2"]<-"RECENTDEAD_2" #Change column name
colnames(ad)[colnames(ad)=="RD_3"]<-"RECENTDEAD_3" #Change column name
colnames(ad)[colnames(ad)=="RDCAUSE2"]<-"RECENT_SPECIFIC_CAUSE_CODE_2" #Change column name
colnames(ad)[colnames(ad)=="RDCAUSE3"]<-"RECENT_SPECIFIC_CAUSE_CODE_3" #Change column name
colnames(ad)[colnames(ad)=="REMNANT"]<-"REMNANT_YN" #Change column name
colnames(ad)[colnames(ad)=="CON_1"]<-"CONDITION_1" #Change column name
colnames(ad)[colnames(ad)=="CON_2"]<-"CONDITION_2" #Change column name
colnames(ad)[colnames(ad)=="CON_3"]<-"CONDITION_3" #Change column name
colnames(ad)[colnames(ad)=="SEV_1"]<-"SEVERITY_1" #Change column name
colnames(ad)[colnames(ad)=="SEV_2"]<-"SEVERITY_2" #Change column name
colnames(ad)[colnames(ad)=="SEV_3"]<-"SEVERITY_3" #Change column name
colnames(ad)[colnames(ad)=="Shape_Length"]<-"COLONYLENGTH" #Change column name


colnames(j)[colnames(j)=="Shape_Length"]<-"COLONYLENGTH" #Change column name


#Modify colunns to match standard REA data
ad$COLONYLENGTH<-ad$COLONYLENGTH*100 #convert from m to cm
ad$COLONYLENGTH<-ifelse(ad$NO_COLONY==-1,NA,ad$COLONYLENGTH) #make sure that the segements that had no colonies have a colony length = 0
ad$S_ORDER<-ifelse(ad$NO_COLONY==0 ,"Scleractinia",NA) #add S_order column
ad$SPCODE<-ifelse(ad$NO_COLONY==-1 ,"AAAA",as.character(ad$SPCODE)) #Change spcode to AAAA if there are no colonies observed in the segment
ad$COLONYID<-ifelse(ad$NO_COLONY==-1 ,NA,ad$COLONYID) #Change colonyid to NA if there are no colonies observed in the segment. COLONYID is a placeholder until Data Services can integrate it properly

#Create Genuscode and taxonname column from spcode
genlookup<-read.csv("T:/Benthic/Data/Lookup Tables/Genus_lookup.csv")
ad<-CreateGenusCode(ad,genlookup) 
colnames(ad)[colnames(ad)=="SPCODE"]<-"TAXONCODE_2024" #Change column name
head(ad)


j$COLONYLENGTH<-j$COLONYLENGTH*100 #convert from m to cm
j$COLONYLENGTH<-ifelse(j$NO_COLONY==-1,NA,j$COLONYLENGTH) #make sure that the segements that had no colonies have a colony length = 0
j$S_ORDER<-ifelse(j$NO_COLONY==0,"Scleractinia",NA) #add S_order column
j$SPCODE<-ifelse(j$NO_COLONY==-1 ,"AAAA",as.character(j$SPCODE)) #Change spcode to AAAA if there are no colonies observed in the segment
j$COLONYID<-ifelse(j$NO_COLONY==-1 ,NA,j$COLONYID) #Change colonyid to NA if there are no colonies observed in the segment

j<-CreateGenusCode(j,genlookup) 
colnames(j)[colnames(j)=="SPCODE"]<-"TAXONCODE_2024" #Change column name
head(j)

ad$SITE<-SiteNumLeadingZeros(ad$SITE)
j$SITE<-SiteNumLeadingZeros(j$SITE)

#Merge with Survey Master
survey_master<-read.csv("C:/Users/Jonathan.Charendoff/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")

colnames(survey_master)[colnames(survey_master)=="new_MIN_DEPTH_M"]<-"MIN_DEPTH_M" #Change column name
colnames(survey_master)[colnames(survey_master)=="new_MAX_DEPTH_M"]<-"MAX_DEPTH_M" #Change column name
colnames(survey_master)[colnames(survey_master)=="LATITUDE_SV"]<-"LATITUDE" #Change column name
colnames(survey_master)[colnames(survey_master)=="LONGITUDE_SV"]<-"LONGITUDE" #Change column name


ad<-left_join(ad,survey_master[,c("MISSIONID","REGION","OBS_YEAR","ISLAND","SITEVISITID","SITE","SEC_NAME",
                                "REEF_ZONE","DEPTH_BIN","HABITAT_CODE","LATITUDE","LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")])
head(ad)
if(nrow(ad)!=nrow(ad)) {cat("WARNING:Data were dropped")} #Check that adult data weren't dropped  


j<-left_join(j,survey_master[,c("MISSIONID","REGION","OBS_YEAR","ISLAND","SITEVISITID","SITE","SEC_NAME",
                                  "REEF_ZONE","DEPTH_BIN","HABITAT_CODE","LATITUDE","LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")])

head(j)
if(nrow(j)!=nrow(j)) {cat("WARNING:Data were dropped")} #Check that adult data weren't dropped  

#Write out dataframes
write.csv(j,file="T:/Benthic/Data/SfM/QC/SfM_Juvenile_Demographic_SE2406_forInPort.csv")
write.csv(ad,file="T:/Benthic/Data/SfM/QC/SfM_Adult_demographic_SE2406_forInPort.csv")



