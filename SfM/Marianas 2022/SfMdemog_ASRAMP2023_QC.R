#Merging v1 and v2 geodatabase and ensuring that no site_segs are missing
#By Corinne Amir
#Modified 2/14/23 by Jonathan Charendoff
rm(list=ls())

source("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/Functions/core_functions.R")

# Read dataframes pulled directly from geodatabases
setwd("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Geodatabase QC")
#setwd("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022")


require(dplyr)
require(plyr)
require(reshape2)



# Read in V2 geodatabase --------------------------------------------------

v2 <- read.csv("RA2301_demographics.csv")
sitelist <- na.omit(read.csv("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/ASRAMP2023_SfM_Meta.csv"))


# Prep the v2 geodatabase data -----------------------------------------------------

# # Merge annotator geodatabases together (all are v2 geodatabase)
# 
# dim(MA); dim(ML); dim(AH); dim(RS); dim(CA); dim(FL)
# str(MA); str(ML); str(AH); str(RS); str(CA); str(FL)
# 
# sfm.raw <- rbind(MA, ML, AH, RS, CA, FL, v1, stringsAsFactors = FALSE)

#Reformat v2 Site names
v2$SITE<-gsub("_", "-", v2$SITE)


#Merge v1 and v2 geodatabases together
sfm.raw <- v2
#sfm.raw <- rbind(v1,v2)
dim(sfm.raw)

# Change site numbers such as MAR-22 to MAR-0022



sfm.raw$SITE <- as.factor(sfm.raw$SITE)
sfm.raw$SITE<-SiteNumLeadingZeros(sfm.raw$SITE) ##NOT WORKING
#If not already present from running the v1-v2 merge script, add column for site_segment
#sfm.raw$SEGMENT[which(sfm.raw$SEGMENT == 10 & sfm.raw$SITE == "PAG-1323")] <- 5
#sfm.raw$SEGMENT[which(sfm.raw$SEGMENT == 10 & sfm.raw$SITE == "MAU-1222")] <- 15
sfm.raw$site_seg<-paste(sfm.raw$SITE,sfm.raw$SEGMENT)



# Check to see if any sites are missing (should have 106 sites)
sitespresent <- data.frame(unique(sfm.raw$SITE)) #Get list of sites exported from gdb
sitespresent <- sitespresent %>% filter(unique.sfm.raw.SITE.!=" " & unique.sfm.raw.SITE.!="A"
                                        &unique.sfm.raw.SITE.!="" & unique.sfm.raw.SITE.!="RA2301") #Remove erroneous site names
sitespresent$df <- rep("present",times = nrow(sitespresent)) #Add column to differentiate from "master" site list
colnames(sitespresent) <- c("SITE", "df")

sitelist$df <- rep("expected",times = nrow(sitelist))
sitelist$SITE <- as.factor(sitelist$SITE)
sitelist$SITE <- SiteNumLeadingZeros(sitelist$SITE)

sites <- full_join(sitespresent,sitelist, by="SITE")
sites <- sites %>% filter(is.na(df.x) | is.na(df.y)) #Flag sites that are missing in the site list or gdb export
View(sites) # still missing 5 sites....?
#write.csv(sites, "HARAMP_missing_sites.csv") # MOL-2266 and HAW-4224 were dropped because they weren't completed


seglist<-ddply(sfm.raw,.(SITE),summarize,n=length(unique(SEGMENT))) #there should be no sites that have less than 3 segments


#Fill in known values if data are missing
sfm.raw$OBS_YEAR <- rep(2023, times=nrow(sfm.raw))
sfm.raw$MISSION_ID <- rep("RA2301", times=nrow(sfm.raw))


sfm.raw$SEGMENT<-as.factor(sfm.raw$SEGMENT)
table(sfm.raw$SITE,sfm.raw$SEGMENT)


#Create a dataframe that houses all rows that have not been completely filled out (not including RD and CON-related columns)
sfm.raw$ANALYST <- as.factor(sfm.raw$ANALYST)
sfm.raw$SITE <- as.factor(sfm.raw$SITE)
sfm.raw$SEGMENT <- as.factor(sfm.raw$SEGMENT)

sfm.missing.duplicate.rows <- rbind(
  analyst.missing <- filter(sfm.raw, ANALYST %in% c("NA", " ",NA)),
  site.missing <- filter(sfm.raw, SITE %in% c("NA-   NA", "-   NA", "A-   NA",NA)),
  transect.missing <- filter(sfm.raw, TRANSECT %in% c(" ", "NA", "",NA)),
  seglength.missing <-filter(sfm.raw, SEGLENGTH %in% c(0.0, "0","NA",NA)),
  segwidths.missing <- filter(sfm.raw, SEGWIDTH %in% c("0", "NA", "", " ",NA)),
  spcode.missing <-  filter(sfm.raw, SPCODE %in% c("NA", " ", "",NA)),
  morphcode.missing <-  filter(sfm.raw, MORPH_CODE %in% c("NA", " ", "",NA)),
  transect.missing <-  filter(sfm.raw, TRANSECT %in% c(0, "<Null>", " ",NA)),
  segmennt.missing <-  filter(sfm.raw, SEGMENT %in% c("NA", "<NA>",NA)))

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
View(sfm.missing)

#Save dataframe with missing values 
#write.csv(sfm.missing, "sfm_missing_rows.csv") #get these rows repopulated (if missing metadata) or annotated before moving forward


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

sfm$RDCAUSE_3 <- RemoveLogicalNA(sfm$RDCAUSE_3)
sfm$RD_3 <- RemoveLogicalNA(sfm$RD_3)
sfm$EXTENT_1 <- RemoveLogicalNA(sfm$EXTENT_1)
sfm$EXTENT_2 <- RemoveLogicalNA(sfm$EXTENT_2)
sfm$EXTENT_3 <- RemoveLogicalNA(sfm$EXTENT_3)
sfm$CON_2 <- RemoveLogicalNA(sfm$CON_2)
sfm$CON_3 <- RemoveLogicalNA(sfm$CON_3)
sfm$SEV_2 <- RemoveLogicalNA(sfm$SEV_2)
sfm$SEV_3 <- RemoveLogicalNA(sfm$SEV_3)

#Fix columns with incorrect factor levels
sapply(sfm,unique)

#sfm <- sfm %>% filter(SITE != "-   NA" & SITE != "SE1902-   NA") #cant find where these errors are in the gdb
sfm$OLDDEAD <- as.numeric(sfm$OLDDEAD)


sfm<-sfm %>% mutate(FRAGMENT = mapvalues(FRAGMENT, c(NA), c(0)),
                    REMNANT = mapvalues(REMNANT, c(NA), c(0)),
                    JUVENILE = mapvalues(JUVENILE, c(NA), c(0)),
                    EX_BOUND = mapvalues(EX_BOUND, c(NA), c(0)),
                    NO_COLONY = mapvalues(NO_COLONY, c(NA), c(0)),
                    SPCODE = mapvalues(SPCODE, c(""), c(NA)),
                    MORPH_CODE = mapvalues(MORPH_CODE, c(""), c(NA)),
                    RDCAUSE_1 = mapvalues(RDCAUSE_1, c("NA", ""),  c(NA, NA)),
                    RDCAUSE_2 = mapvalues(RDCAUSE_2, c("NA", ""),  c(NA, NA)),
                    RDCAUSE_3 = mapvalues(RDCAUSE_3, c("NA", ""), c(NA, NA)),
                    RD_3 = mapvalues(RD_3, c("NA"), c(NA)),
                    EXTENT_2 = mapvalues(EXTENT_2, c("NA"), c(NA)),
                    EXTENT_3 = mapvalues(EXTENT_3, c("NA"), c(NA)),
                    SEV_2 = mapvalues(SEV_2, c("NA"), c(NA)),
                    SEV_3 = mapvalues(SEV_3, c("NA"), c(NA)),
                    CON_1 = mapvalues(CON_1, c("NA", ""), c(NA, NA)),
                    CON_2 = mapvalues(CON_2, c("NA", ""),  c(NA, NA)),
                    CON_3 = mapvalues(CON_3, c("NA", ""), c(NA, NA)),
                    OLDDEAD = mapvalues(OLDDEAD, c(NA), c(0)),
                    JUV_SUBSTRATE = mapvalues(JUV_SUBSTRATE, c(""), c(NA))) 

#dead isn't recorded for Juvs
#sfm[which(sfm$JUVENILE == -1 & sfm$OLDDEAD>0),]
#sfm[which(sfm$JUVENILE == -1 & sfm$RD_1>0),] <- NA
#sfm$RD_1[which(sfm$JUVENILE == -1 & sfm$RD_1>0)] <- NA

#sfm[which(sfm$JUVENILE == -1 & sfm$Shape_Length >= 0.0475 & sfm$SEGLENGTH > 1),]


#Seglength for juvs is 1m not 2.5 CHANGE WHEN ACTUALLY DEALING WITH DATA 
#sfm$SEGLENGTH[which(sfm$JUVENILE == -1& sfm$SEGLENGTH > 1)] <- 1
#sfm$SEGWIDTH[is.na(sfm$SEGWIDTH)] <-1

#Add column for segment area
sfm$SEGLENGTH <- as.numeric(sfm$SEGLENGTH)
sfm$SEGWIDTH <- as.numeric(sfm$SEGWIDTH)
sfm$SEGAREA <- sfm$SEGLENGTH*sfm$SEGWIDTH


#Miscellaneous changes needed 
sapply(sfm,unique)

length(unique(sfm$SITE)) # 103 unique sites 
length(unique(sfm$site_seg)) # 391 unique site_segs

colnames(sfm)[c(19,21,23)] <- c("RDCAUSE1", "RDCAUSE2", "RDCAUSE3")

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
#write.csv(partial_SiteSeg_removal, "Error_partial_filled_segments.csv")



#2.Check that the columns have the appropripate type of data (e.g. numeric vs. text) & no errant codes (e.g. SEV and/or RD columns contain NA) 
sapply(sfm,levels)
str(sfm) 
sapply(sfm, class)

sfm$CON_2 <- as.character(sfm$CON_2)
sfm$CON_3 <- as.character(sfm$CON_3)
sfm$SEV_2 <- as.numeric(sfm$SEV_2)
sfm$SEV_3 <- as.numeric(sfm$SEV_3)
sfm$EXTENT_2 <- as.numeric(sfm$EXTENT_2)
sfm$EXTENT_3 <- as.numeric(sfm$EXTENT_3)

output[2,]<-c("No errant codes", "ok")



# #3. All TRANSECT within v2 of the geodabase should = A (B is for repeats)
filter(sfm, TRANSECT != "A")
 
output[3,]<-c("All transects = A","ok") #change depending on output from previous lines of code


#4. Make sure that if NO_COLONY=-1 none of the following columns have been populated
sfm %>% filter(sfm$SPCODE == "NA" & sfm$NO_COLONY != -1)
sfm %>% filter(sfm$SPCODE != "NA" & sfm$NO_COLONY == -1) 
sfm %>% filter(sfm$FRAGMENT == -1 & sfm$NO_COLONY == -1)
sfm %>% filter(sfm$REMNANT == -1 & sfm$NO_COLONY == -1)
sfm %>% filter(sfm$MORPH_CODE == "NA" & sfm$NO_COLONY != -1)
sfm %>% filter(sfm$MORPH_CODE != "NA" & sfm$NO_COLONY == -1)


output[4,]<-c("NO_COLONY segments filled correctly","YES") 


#5. Calculate the number of annotated segments per site and check that all segments contain both seglengths (except segment 15)
##Create a summary table of #segments per site and check against tracking data sheet
seg.per.site <- ddply(sfm,.(SITE, SEGMENT, SEGAREA), summarize, num.annotated = n_distinct(SEGLENGTH))
eval.seg.per.site <- dcast(seg.per.site, SITE~SEGMENT, length)
eval.seg.per.site <- left_join(eval.seg.per.site, sitelist[,3:4])
#eval.seg.per.site$Total <- rowSums(eval.seg.per.site)
View(eval.seg.per.site)
View(seg.per.site)

#use this file to evaluate where segments may be missing
write.csv(eval.seg.per.site, "2023_Missing_seg_eval.csv")

output[5,]<-c("All annotated segments have correct #seglengths","need to double check some sites") #change depending on output from previous line of code
####stopped here 102623

# #6. Make sure only 1 annotator exists per site_seg-no longer applicable since multiple people did belt corrections after cross checking
 analyst.per.site.seg <- ddply(sfm,.(site_seg), summarize, num.analyst = n_distinct(ANALYST))
 filter(analyst.per.site.seg, num.analyst>1)
# 
output[6,]<-c("All site_segs annotated by one person","YES")
# 


#7.Check for incorrect species-V:\PHOTOMOSAIC (1)\HARAMP\HARAMP_2019_codes.csv
codes <- ddply(sfm,.(SPCODE),summarize,temp=length(SPCODE))
View(codes)

sfm$SPCODE[sfm$SPCODE == "PGRC"] <- "PGWC"

#suspect codes
suspect <- c(codes$SPCODE[codes$temp <= 5], "CYSP")

Full.suspect <- sfm[sfm$SPCODE %in% suspect,]

output[7,]<-c("Species codes are correct","Some NA and blank -- OK")


#8. Check that SEGWIDTH is correct (should have been apparent in qc #1).
levels(as.factor(sfm$SEGWIDTH)) #should all be 1 OR 2.5, unless otherwise stated 


output[8,]<-c("All segment widths are correct","YES") #change depending on output from previous line of code


#9. Identify colonies flagged as Juveniles or Adults, but have the innocorrect segment area. make sure j = 1 and A = 2.5
sm.colonies.eval <- sfm %>% filter(JUVENILE== 0,Shape_Length<=0.045, REMNANT == 0, FRAGMENT == 0); View(sm.colonies.eval)
lg.colonies.eval <- sfm %>% filter(JUVENILE==-1,Shape_Length>0.0475); View(lg.colonies.eval)


output[9,]<-c("Juveniles and Adult colonies have correct labeling", "YES")


#If rows have been flagged, export sm_colonies dataframe into a csv file for further QC
#write.csv(sm.colonies.eval, "Juveniles_eval.csv")
#write.csv(lg.colonies.eval, "Adults_eval.csv")


#10. Identify colonies have the same CON code across multiple CON columns
sfm$CON_3 <- as.character(sfm$CON_3)
CON_dup <- sfm %>% filter(CON_1!="NA") %>% filter(CON_2!="NA")
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
RD_dup <- sfm %>% filter(RDCAUSE1!="NA") %>% filter(RDCAUSE2!="NA")

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
sfm[which(sfm$RD_1== 0 & sfm$RDCAUSE1!="NA"),]#; unique(a$site_seg)
sfm[which(sfm$RD_2=="0" & sfm$RDCAUSE2!= "NA"),]
sfm[which(sfm$RD_3=="0"& sfm$RDCAUSE3!= "NA"),]

output[12,]<-c("0% Recent Dead corals do NOT have an RDCAUSE code","YES")



#13. Identify colonies with recent dead >0%, but there is no RDCAUSE code - This check should result in 0 records   
sfm[which(sfm$RD_1 >0 & sfm$RDCAUSE1=="NA"),] #,rowSums(is.na(sfm)) != ncol(sfm),]
sfm[which(sfm$RD_2 >0 & sfm$RDCAUSE2=="NA"),] #,rowSums(is.na(a)) != ncol(a), ]
sfm[which(sfm$RD_3 !=0 & sfm$RDCAUSE3=="NA"),] #,rowSums(is.na(a)) != ncol(a), ]

output[13,]<-c("All corals with RD >0 have an RDCAUSE code","YES")



#14. Identify colonies with NO % EXTENT, but a condition - This check should result in 0 records    
sfm[which(sfm$EXTENT_1=="0"& sfm$CON_1!="NA"),]
sfm[which(sfm$EXTENT_2=="0"& sfm$CON_2!="NA"),]
sfm[which(sfm$EXTENT_3=="0"& sfm$CON_3!="NA"),] 

output[14,]<-c("All colonies with a condition have an extent", "YES")



#15. Identify colonies that have no condition, but a value in extent - This check should result in 0 records   
sfm[which(sfm$CON_1=="NA" & sfm$EXTENT_1!=0),] 
sfm[which(sfm$CON_2=="NA" & sfm$EXTNET_2!=0),]
sfm[which(sfm$CON_3=="NA" & sfm$EXTENT_3!=0),] #rowSums(is.na(a)) != ncol(a),]

output[15,]<-c("All colonies with NO condition also have NO extent","YES")


#16. Identify colonies with nothing in condition column, but a value in severity. Double check that these shouldn't be 0  
sfm[which(sfm$EXTENT_1==0& sfm$SEV_1!=0),] #,rowSums(is.na(a)) != ncol(a),]
sfm[which(sfm$EXTENT_2==0& sfm$SEV_2!=0),]
sfm[which(sfm$EXTENT_3==0& sfm$SEV_3!=0),]

output[16,]<-c("All colonies with NO extent have NO severity","YES")


#17. Make sure that the only rows with severity filled contain BLE or BLP in condition
sfm[which(sfm$SEV_1==0& sfm$CON_1 %in% c("BLE","BLP")),]
sfm[which(sfm$SEV_2==0& sfm$CON_2%in% c("BLE","BLP")),]
sfm[which(sfm$SEV_3==0& sfm$CON_3%in% c("BLE","BLP")),]

`%notin%` <- Negate(`%in%`)
sfm[which(sfm$SEV_1!=0& sfm$CON_1 %notin% c("BLE","BLP")),]
sfm[which(sfm$SEV_2!=0& sfm$CON_2 %notin% c("BLE","BLP")),]
sfm[which(sfm$SEV_3!=0& sfm$CON_3 %notin% c("BLE","BLP")),]

output[17,]<-c("Severity value is present only in colonies with CON = BLE and BLP", "YES")



#18. Make sure that values in SEV are only NA, 2, or 3 
View(sfm[which(sfm$SEV_1>3 | sfm$SEV_1<2),])
sfm[which(sfm$SEV_2>3 | sfm$SEV_2<2),]
sfm[which(sfm$SEV_3>3 | sfm$SEV_3<2),]

sfm[which(sfm$EXTENT_1<5),]
sfm[which(sfm$EXTENT_2<5),]
sfm[which(sfm$EXTENT_3<5),]


output[18,]<-c("Severity values are whole numbers between 2-3","YES")



#19. Check if there are any EX_BOUND colonies that have a conspicuously small shape length
sfm[which(sfm$EX_BOUND == "-1"& sfm$Shape_Length < 0.25),]


output[19,]<-c("EX_BOUND colonies have a non-conspicuous shape length", "YES")


#20. RD + OD is not greater than 100%
sfm$OLDDEAD<-as.numeric(sfm$OLDDEAD)
sfm$RD_2<-as.numeric(sfm$RD_2)
sfm$RD_1<-as.numeric(sfm$RD_1)
sfm$RD_3<-as.numeric(sfm$RD_3)
sfm$totaldead = sfm$RD_1+sfm$RD_2+sfm$RD_3 + sfm$OLDDEAD
sfm[which(sfm$totaldead>100),] #RD + OD can equal 100, but not >100

output[20,]<-c("RD + OD <=100%","YES")


#21. Check for duplicate rows in dataframe - it may be ok that colonies are exactly the same length
a<-sfm %>% group_by(Shape_Length,site_seg) %>% filter(n()>1)

output[21,]<-c("No duplicate rows","YES")



#22. Make sure that if a coral is a remnant, that SEGLENGTH > 1 regardless of max diameter
remnant.seglegth <- sfm %>% filter(REMNANT == "-1" & SEGLENGTH ==1) 
remnant.olddead <- sfm %>% filter(REMNANT == "-1" & OLDDEAD != 0)
remnant.error <- rbind(remnant.seglegth,remnant.olddead); nrow(remnant.error)


output[22,]<-c("REMNANT filled out correctly","YES")


#23. Make sure that if a coral is a juvenile, they don't have a value in OLDDEAD, or any RD or CON columns
juv.olddead <- sfm %>% filter(JUVENILE == "-1" & OLDDEAD != 0 | JUVENILE == "-1" & CON_1 != "NA" | 
                                JUVENILE == "-1" & CON_2 != "NA" | JUVENILE == "-1" & CON_3 != "NA" | 
                                JUVENILE == "-1" & RDCAUSE1 != "NA" |JUVENILE == "-1" & RDCAUSE2 != "NA" |
                                JUVENILE == "-1" & RDCAUSE3 != "NA"); nrow(juv.olddead)

juv.sub <- sfm %>% filter(JUVENILE == -1 & is.na(JUV_SUBSTRATE), NO_COLONY == 0)

output[23,]<-c("Juvenile colonies have no OLDDEAD, RD, or CON","YES")



#Export QC output table with appropriate file name
write.csv(output,"ASRAMP2023_sfm_output.csv")



# Export QC'd data ----------------------------------------------------------------------------

sfm$COLONYID<-c(1:nrow(sfm))

#Separate by adults and juveniles
ad<-subset(sfm,JUVENILE==0|REMNANT==-1)
ad<-subset(ad,select=-c(ï..OID_,totaldead, JUV_SUBSTRATE))
j<-subset(sfm,JUVENILE=="-1") # includes segments where NO_COLONY = -1
j<-subset(j,select=c(COLONYID,ANALYST,OBS_YEAR,MISSION_ID,SITE,TRANSECT,SEGMENT,SEGLENGTH,SEGWIDTH,NO_COLONY,SPCODE,FRAGMENT,MORPH_CODE,
                     EX_BOUND,JUVENILE,FRAGMENT,REMNANT, JUV_SUBSTRATE,Shape_Length,SEGAREA))

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
write.csv(ad,"ASRAMP2023_QCdsfm_ADULT.csv",row.names = F)
write.csv(j,"ASRAMP2023_QCdsfm_JUV.csv",row.names = F)
write.csv(sfm,"ASRAMP2023_output_FINAL.csv",row.names = F)


# Prepare for InPort-Merge together survey master table and Inport ready (Corinne ran final updates) colony-level data ------------------------------------
j <- read.csv("T:/Benthic/Data/SfM/QC/ASRAMP2023_QCdsfm_JUV.csv")
ad <- read.csv("T:/Benthic/Data/SfM/QC/ASRAMP2023_QCdsfm_ADULT.csv")
ad<-subset(ad,select= -c(site_seg, MOSAIC_ISSUES, SEGAREA)) 
survey_master<-read.csv("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/SURVEY MASTER.csv")

stringi::stri_sub(ad$SITE[which(nchar(ad$SITE) == 7)], 5, 4) <- "00"
stringi::stri_sub(ad$SITE[which(nchar(ad$SITE) == 8)], 5, 4) <- "0"
colnames(ad)[colnames(ad)=="Shape_Length"]<-"Shape_Leng" #Change column name

#Modify columns to match standard REA data
ad$Shape_Leng[which(ad$NO_COLONY == -1)]<- NA#make sure that the segements that had no colonies have a colony length = NA
ad$SPCODE<-ifelse(ad$NO_COLONY==-1 ,"AAAA",as.character(ad$SPCODE)) #Change spcode to AAAA if there are no colonies observed in the segment
ad$COLONYID<-ifelse(ad$NO_COLONY==-1 ,NA,ad$COLONYID) #Change colonyid to NA if there are no colonies observed in the segment. COLONYID is a placeholder until Data Services can integrate it properly

#Merge with Survey Master
colnames(survey_master)[colnames(survey_master)=="new_MIN_DEPTH_M"]<-"MIN_DEPTH_M" #Change column name
colnames(survey_master)[colnames(survey_master)=="new_MAX_DEPTH_M"]<-"MAX_DEPTH_M" #Change column name
colnames(survey_master)[colnames(survey_master)=="LATITUDE_SV"]<-"LATITUDE" #Change column name
colnames(survey_master)[colnames(survey_master)=="LONGITUDE_SV"]<-"LONGITUDE" #Change column name

ad<-left_join(ad,survey_master[,c("REGION","OBS_YEAR","ISLAND","SITEVISITID","SITE",
                                "REEF_ZONE","DEPTH_BIN","HABITAT_CODE","LATITUDE","LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M", "DATE_")])
head(ad)
if(nrow(ad)!=nrow(ad)) {cat("WARNING:Data were dropped")} #Check that adult data weren't dropped  
ad$DATE_ <- substr(ad$DATE_, 1, 10)
ad$SITE_SEG <- paste(ad$SITE, ad$SEGMENT, sep = "_")


#check if cols need to be renamed or dropped/added
col_order <- colnames(read.csv("T:/DataManagement/NCEI Archive Packages/SFM_Demography/Data/2022/SfM_Adult_demographic_MARI_2022.csv"))
test <- colnames(ad)
test[which(!(test %in% col_order))]


colnames(ad)[colnames(ad)=="OLD_DEAD"]<-"OLDDEAD"
colnames(ad)[colnames(ad)=="LATITUDE"]<-"LATITUDE_LOV"
colnames(ad)[colnames(ad)=="LONGITUDE"]<-"LONGITUDE_LOV"
ad <- ad[, col_order]

write.csv(ad,file="T:/DataManagement/NCEI Archive Packages/SFM_Demography/SfM_Adult_demographic_AS-PRIA_2023.csv")


#JUV
j <- read.csv("T:/Benthic/Data/SfM/QC/MARAMP2022_QCdsfm_JUV.csv")
j<-subset(j,select= -c(FRAGMENT.1))

colnames(j)[colnames(j)=="Shape_Length"]<-"Shape_Leng" #Change column name


j$Shape_Leng<-ifelse(j$NO_COLONY==-1,NA,j$Shape_Leng) #make sure that the segements that had no colonies have a colony length = 0
j$SPCODE<-ifelse(j$NO_COLONY==-1 ,"AAAA",as.character(j$SPCODE)) #Change spcode to AAAA if there are no colonies observed in the segment
j$COLONYID<-ifelse(j$NO_COLONY==-1 ,NA,j$COLONYID) #Change colonyid to NA if there are no colonies observed in the segment



j<-left_join(j,survey_master[,c("REGION","OBS_YEAR","ISLAND","SITEVISITID","SITE","DATE_",
                                  "REEF_ZONE","DEPTH_BIN","HABITAT_CODE","LATITUDE","LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")])


col_order <- colnames(read.csv("T:/DataManagement/NCEI Archive Packages/SFM_Demography/SfM_Juvenile_demographic_MHI_2019.csv"))
test <- colnames(j)
test[which(!(test %in% col_order))]
colnames(j)[colnames(j)=="LATITUDE"]<-"LATITUDE_LOV"
colnames(j)[colnames(j)=="LONGITUDE"]<-"LONGITUDE_LOV"
j <- j[, col_order]


head(j)
if(nrow(j)!=nrow(j)) {cat("WARNING:Data were dropped")} #Check that adult data weren't dropped  

#Write out dataframes
write.csv(j,file="T:/DataManagement/NCEI Archive Packages/SFM_Demography/SfM_Juvenile_demographic_AS-PRIA_2023.csv")


