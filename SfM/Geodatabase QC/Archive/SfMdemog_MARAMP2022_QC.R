#Merging v1 and v2 geodatabase and ensuring that no site_segs are missing
#By Corinne Amir
#Modified 12/1/20 by Courtney Couch

source("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/Functions/core_functions.R")

# Read dataframes pulled directly from geodatabases
#setwd("C:/Users/Corinne.Amir/Documents/GitHub/Benthic-Scripts/SfM/Geodatabase QC")
setwd("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Geodatabase QC")

v1 <- read.csv("HARAM2019_annotations_v1_MA_july242020.csv") 
v2 <- read.csv("MARAMP2022_Calibration_2022-08-25.csv") 

require(dplyr)
require(plyr)
require(reshape2)


#Fill in empty rows with NA
for(i in 1:ncol(v1)) if(is.factor(v1[,i])) levels(v1[,i]) <- c(levels(v1[,i]),"NA")
v1[v1 == " "] <- "NA"
v1[v1 == ""] <- "NA"

v1<-droplevels(v1) #drop levels
sapply(v1,levels)# check that all " " were converted
View(v1)


#Fix SITE numbers
v1$SITE<-gsub("_", "-", v1$SITE)
levels(as.factor(v1$SITE))


#Convert all segments to 0,5,10,15 segment label scheme
v1$SEGMENT
table(v1$SITE,v1$SEGMENT)
v1$SEGMENT<-ifelse(v1$SITE=="NII-2584"&v1$SEGMENT=="10","5",v1$SEGMENT)

v1 <- v1 %>% mutate(SEGMENT=recode(SEGMENT, 
                                   "0"="0",
                                   '1'="0",
                                   '3'="5",
                                   '5'="10",
                                   "7"="15",
                                   "NA"="NA"))
#Check that segments were changed correctly
v1<-droplevels(v1)
table(v1$SITE,v1$SEGMENT)


#Does the v1 geodatabase have identical site-segs as the v2 geodatabase? (should be no)
v1$site_seg<-paste(v1$SITE, v1$SEGMENT)
v1$SEGMENT <- as.factor(v1$SEGMENT)
v2$SEGMENT <- as.factor(v2$SEGMENT)

partial_SiteSeg_removal <- inner_join(v1, v2, by = c("SITE", "SEGMENT")) # should have zero rows
head(partial_SiteSeg_removal)
unique(partial_SiteSeg_removal$site_seg) #error sites = "KAU-2166 0"  "KAU-2166 5"  "KAU-2166 10" "KAU-2166 15"


#Check for logical NAs
sapply(v1,class) #RDCAUSE3, CON_3 = logical 


#### Run function that removes logical NAs ####
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
v1$RDCAUSE3 <- RemoveLogicalNA(v1$RDCAUSE3)
v1$CON_3 <- RemoveLogicalNA(v1$CON_3)

sapply(v1,class) #RDCAUSE3 and CON_3 = factor


#Create a dataframe that houses all rows that have not been completely filled out (not including RD and CON-related columns)
sfm.missing.duplicate.rows <- rbind(
  analyst.missing <- filter(v1, ANALYST %in% c("NA")),
  site.missing <- filter(v1, SITE %in% c("NA")),
  seglength.missing <-filter(v1, SEGLENGTH %in% c("0")),
  segwidths.missing <- filter(v1, SEGWIDTH %in% c("0")),
  spcode.missing <-  filter(v1, SPCODE %in% c("NA")),
  morphcode.missing <-  filter(v1, MORPH_CODE %in% c("NA"))) 

sfm.missing <- sfm.missing.duplicate.rows[!duplicated(sfm.missing.duplicate.rows),] 


#Identify all rows where NO_COLONY_ is -1 and remove from sfm.missing dataframe
no.colony.present <- sfm.missing %>%
  filter(NO_COLONY_ == "-1" & ANALYST != "NA" & SITE != "NA" & SEGLENGTH != "0" & SEGWIDTH != "0")

sfm.missing <- droplevels(anti_join(sfm.missing, no.colony.present))


#Make v1 columns match v2 columns
colnames(v2)
colnames(v1) #change: NO_COLONY_ = NO_COLONY, FRAGMENT_Y = FRAGMENT, SHAPE_Leng = Shape_Leng
             #add: TRANSECT, JUVENILE, REMNANT
             

#Fix column names
v1 <- v1 %>% dplyr::rename(Shape_Leng = SHAPE_Leng)


# #Add column for segment area
# v1$SEGAREA <- v1$SEGLENGTH*v1$SEGWIDTH


#Add a column for juveniles
v1$JUVENILE <- ifelse(v1$Shape_Leng<0.05 & v1$SPCODE != "PBER" & v1$SEGLENGTH == 1.0, -1, 0)
            


#Add a dummy column for remnant- we didnt record this in the v1 geodatabase
v1$REMNANT <- rep(0, times = nrow(v1))


#Add column for Transect -B indicates that these data are from the v1 version of the geodatabase
v1$TRANSECT <- rep("B",times = nrow(v1))


#Make v1 columns have the same class as v2
v1$NO_COLONY <- as.factor(v1$NO_COLONY)
v1$FRAGMENT <- as.factor(v1$FRAGMENT)
v1$EX_BOUND <- as.factor(v1$EX_BOUND)
v1$REMNANT <- as.factor(v1$REMNANT)
v1$JUVENILE <- as.factor(v1$JUVENILE)


#Make v1 columns have the same order as v2
col_order <- c("FID","ANALYST","OBS_YEAR","MISSION_ID" ,"SITE" ,"TRANSECT","SEGMENT","SEGLENGTH",
               "SEGWIDTH","NO_COLONY","SPCODE","EX_BOUND","JUVENILE" ,"FRAGMENT","REMNANT" , "OLDDEAD",
               "MORPH_CODE", "RDCAUSE1","RD_1" ,"RDCAUSE2" , "RD_2" ,"RDCAUSE3","RD_3" , "CON_1","EXTENT_1",
               "SEV_1", "CON_2","EXTENT_2" ,"SEV_2","CON_3" ,"EXTENT_3","SEV_3","Shape_Leng") #,"SEGAREA", "site_seg")
v1 <- v1[,col_order]



#Export formatted v1 geodatabase
#write.csv(v1, "HARAMP2019_v1_reformat_FINAL_jul242020.csv",row.names = F)


# Read in V2 geodatabase --------------------------------------------------

v2 <- read.csv("MARAMP2022_Calibration_2022-08-25.csv")
sitelist <- read.csv("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/MARAMP2022_SfM_Calib_Meta.csv")[1:5,]


# Prep the v2 geodatabase data -----------------------------------------------------

# # Merge annotator geodatabases together (all are v2 geodatabase)
# 
# dim(MA); dim(ML); dim(AH); dim(RS); dim(CA); dim(FL)
# str(MA); str(ML); str(AH); str(RS); str(CA); str(FL)
# 
# sfm.raw <- rbind(MA, ML, AH, RS, CA, FL, v1, stringsAsFactors = FALSE)

#Reformat v2 Site names
v2$SITE<-gsub("_", "-", v2$SITE)
levels(as.factor(v2$SITE))

#Merge v1 and v2 geodatabases together
sfm.raw <- v2
#sfm.raw <- rbind(v1,v2)
dim(sfm.raw)

# Change site numbers such as MAR-22 to MAR-0022
#sfm.raw$SITE<-SiteNumLeadingZeros(sfm.raw$SITE)


#If not already present from running the v1-v2 merge script, add column for site_segment
sfm.raw$SEGMENT[which(sfm.raw$SEGMENT == 10 & sfm.raw$SITE == "PAG-1323")] <- 5
sfm.raw$site_seg<-paste(sfm.raw$SITE,sfm.raw$SEGMENT)


#Remove incomplete sites
#sfm.raw <-sfm.raw %>% filter(!SITE %in% c("HAW-04224","MOL-02266"))

#Fix annotator name
#sfm.raw$ANALYST<-ifelse(sfm.raw$ANALYST=="rs","RS",as.character(sfm.raw$ANALYST));head(sfm.raw)


# Check to see if any sites are missing (should have 106 sites)
sitespresent <- data.frame(unique(sfm.raw$SITE)) #Get list of sites exported from gdb
sitespresent <- sitespresent %>% filter(unique.sfm.raw.SITE.!=" " & unique.sfm.raw.SITE.!="A"
                                        &unique.sfm.raw.SITE.!="" & unique.sfm.raw.SITE.!="RA2201") #Remove erroneous site names
sitespresent$df <- rep("present",times = nrow(sitespresent)) #Add column to differentiate from "master" site list
colnames(sitespresent) <- c("SITE", "df")

sitelist$df <- rep("expected",times = nrow(sitelist))

sites <- full_join(sitespresent,sitelist, by="SITE")
sites <- sites %>% filter(is.na(df.x) | is.na(df.y)) #Flag sites that are missing in the site list or gdb export
View(sites) # still missing 5 sites....?
write.csv(sites, "HARAMP_missing_sites.csv") # MOL-2266 and HAW-4224 were dropped because they weren't completed


seglist<-ddply(sfm.raw,.(SITE),summarize,n=length(unique(SEGMENT))) #there should be no sites that have less than 3 segments


#Fill in known values if data are missing
sfm.raw$OBS_YEAR <- rep(2022, times=nrow(sfm.raw))
sfm.raw$MISSION_ID <- rep("RA2201", times=nrow(sfm.raw))


sfm.raw$SEGMENT<-as.factor(sfm.raw$SEGMENT)
table(sfm.raw$SITE,sfm.raw$SEGMENT)


#Create a dataframe that houses all rows that have not been completely filled out (not including RD and CON-related columns)
sfm.raw$ANALYST <- as.factor(sfm.raw$ANALYST)
sfm.raw$SITE <- as.factor(sfm.raw$SITE)
sfm.raw$SEGMENT <- as.factor(sfm.raw$SEGMENT)

sfm.missing.duplicate.rows <- rbind(
  analyst.missing <- filter(sfm.raw, ANALYST %in% c("NA", " ")),
  site.missing <- filter(sfm.raw, SITE %in% c("NA-   NA", "-   NA", "A-   NA")),
  transect.missing <- filter(sfm.raw, TRANSECT %in% c(" ")),
  seglength.missing <-filter(sfm.raw, SEGLENGTH %in% c(0.0, "0","NA")),
  segwidths.missing <- filter(sfm.raw, SEGWIDTH %in% c("0", "NA")),
  spcode.missing <-  filter(sfm.raw, SPCODE %in% c("NA", " ", "")),
  morphcode.missing <-  filter(sfm.raw, MORPH_CODE %in% c("NA", " ", "")),
  transect.missing <-  filter(sfm.raw, TRANSECT %in% c(0, "<Null>", " ")),
  segmennt.missing <-  filter(sfm.raw, SEGMENT %in% c("NA", "<NA>")))

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
write.csv(sfm.missing, "sfm_missing_rows.csv") #get these rows repopulated (if missing metadata) or annotated before moving forward


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

sfm$RDCAUSE3 <- RemoveLogicalNA(sfm$RDCAUSE3)
sfm$CON_3 <- RemoveLogicalNA(sfm$CON_3)


#Fix columns with incorrect factor levels
sapply(sfm,unique)

#sfm <- sfm %>% filter(SITE != "-   NA" & SITE != "SE1902-   NA") #cant find where these errors are in the gdb
sfm$OLD_DEAD <- as.numeric(sfm$OLD_DEAD)


sfm<-sfm %>% mutate(FRAGMENT = mapvalues(FRAGMENT, c(NA), c(0)),
                    REMNANT = mapvalues(REMNANT, c(NA), c(0)),
                    JUVENILE = mapvalues(JUVENILE, c(NA), c(0)),
                    EX_BOUND = mapvalues(EX_BOUND, c(NA), c(0)),
                    NO_COLONY = mapvalues(NO_COLONY, c(NA), c(0)),
                    RDCAUSE1 = mapvalues(RDCAUSE1, c(""), c(NA)),
                    RDCAUSE2 = mapvalues(RDCAUSE2, c(""), c(NA)),
                    RDCAUSE3 = mapvalues(RDCAUSE3, c("NA
                                                     ", ""), c(NA, NA)),
                    CON_1 = mapvalues(CON_1, c(""), c(NA)),
                    CON_2 = mapvalues(CON_2, c(""), c(NA)),
                    CON_3 = mapvalues(CON_3, c("NA", ""), c(NA, NA)),
                    OLD_DEAD = mapvalues(OLD_DEAD, c(NA), c(0))) 

#old dead isn't recorded for Juvs
sfm$OLD_DEAD[which(sfm$JUVENILE == -1)] <- NA

#Seglength for juvs is 1m not 2.5 CHANGE WHEN ACTUALLY DEALING WITH DATA 
sfm$SEGLENGTH[which(sfm$JUVENILE == -1)] <- 1

#Add column for segment area
levels(as.factor(sfm$SEGLENGTH))
sfm$SEGAREA <- sfm$SEGLENGTH*sfm$SEGWIDTH


#Miscellaneous changes needed 
sapply(sfm,unique)

length(unique(sfm$SITE)) # 103 unique sites 
length(unique(sfm$site_seg)) # 391 unique site_segs


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
write.csv(partial_SiteSeg_removal, "Error_partial_filled_segments.csv")



#2.Check that the columns have the appropripate type of data (e.g. numeric vs. text) & no errant codes (e.g. SEV and/or RD columns contain NA) 
sapply(sfm,levels)
str(sfm) 
sapply(sfm, class)

output[2,]<-c("No errant codes", "some logical NAs and blanks throughout -- ok")



# #3. All TRANSECT within v2 of the geodabase should = A (B is for repeats)
# filter(sfm, TRANSECT != "A")
# 
# output[3,]<-c("All transects = A","B sites = v1 gdb -- OK") #change depending on output from previous lines of code


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
seg.per.site <- ddply(sfm,.(SITE, SEGMENT, SEGLENGTH), summarize, num.annotated = n_distinct(SEGLENGTH))
eval.seg.per.site <- as.data.frame(acast(seg.per.site, SITE~SEGMENT, length))
#eval.seg.per.site$Total <- rowSums(eval.seg.per.site)
View(eval.seg.per.site) 

#use this file to evaluate where segments may be missing
write.csv(eval.seg.per.site, "Missing_seg_eval.csv")

output[5,]<-c("All annotated segments have correct #seglengths","23 sites don't have 15m seglength") #change depending on output from previous line of code


# #6. Make sure only 1 annotator exists per site_seg-no longer applicable since multiple people did belt corrections after cross checking
# analyst.per.site.seg <- ddply(sfm,.(site_seg), summarize, num.analyst = n_distinct(ANALYST))
# filter(analyst.per.site.seg, num.analyst>1)
# 
# output[6,]<-c("All site_segs annotated by one person","YES")
# 


#7.Check for incorrect species-V:\PHOTOMOSAIC (1)\HARAMP\HARAMP_2019_codes.csv
ddply(sfm,.(SPCODE),summarize,temp=length(SPCODE))

#these are specifically from the Calib exercise
sfm$SPCODE[which(sfm$SPCODE == "SYSP")] <-"STSP"
sfm$SPCODE[which(sfm$SPCODE == "GPLA")] <-"GEDW"
sfm$SPCODE[which(sfm$SPCODE == "AHYA")] <-"ACSP"

output[7,]<-c("Species codes are correct","Some NA and blank -- OK")


#8. Check that SEGWIDTH is correct (should have been apparent in qc #1).
levels(as.factor(sfm$SEGAREA)) #should all be 1 OR 2.5, unless otherwise stated 

output[8,]<-c("All segment widths are correct","YES") #change depending on output from previous line of code


#9. Identify colonies flagged as Juveniles or Adults, but have the innocorrect segment area. make sure j = 1 and A = 2.5
sm.colonies.eval <- sfm %>% filter(JUVENILE== -1,SEGAREA != 1); sm.colonies.eval
lg.colonies.eval <- sfm %>% filter(JUVENILE==0,SEGAREA==1, NO_COLONY==0); lg.colonies.eval

output[9,]<-c("Juveniles and Adult colonies have correct labeling", "YES")


#If rows have been flagged, export sm_colonies dataframe into a csv file for further QC
write.csv(sm.colonies.eval, "Juveniles_eval.csv")
write.csv(lg.colonies.eval, "Adults_eval.csv")


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
sfm[which(sfm$RD_3 >0 & sfm$RDCAUSE3=="NA"),] #,rowSums(is.na(a)) != ncol(a), ]

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
sfm[which(sfm$EXTENT_1=="0"& sfm$SEV_1!="0"),] #,rowSums(is.na(a)) != ncol(a),]
sfm[which(sfm$EXTENT_2=="0"& sfm$SEV_2!="0"),]
sfm[which(sfm$EXTENT_3=="0"& sfm$SEV_3!="0"),]

output[16,]<-c("All colonies with NO extent have NO severity","YES")


#17. Make sure that the only rows with severity filled contain BLE or BLP in condition
sfm[which(sfm$SEV_1=="0"& sfm$CON_1 %in% c("BLE","BLP")),]
sfm[which(sfm$SEV_2=="0"& sfm$CON_2%in% c("BLE","BLP")),]
sfm[which(sfm$SEV_3=="0"& sfm$CON_3%in% c("BLE","BLP")),]

`%notin%` <- Negate(`%in%`)
sfm[which(sfm$SEV_1!="0"& sfm$CON_1 %notin% c("BLE","BLP")),]
sfm[which(sfm$SEV_2!="0"& sfm$CON_2 %notin% c("BLE","BLP")),]
sfm[which(sfm$SEV_3!="0"& sfm$CON_3 %notin% c("BLE","BLP")),]

output[17,]<-c("Severity value is present only in colonies with CON = BLE and BLP", "YES")



#18. Make sure that values in SEV are only NA, 2, or 3 
sfm[which(sfm$SEV_1>3 | sfm$SEV_1<2),]
sfm[which(sfm$SEV_2>3 | sfm$SEV_1<2),]
sfm[which(sfm$SEV_3>3 | sfm$SEV_1<2),]

output[18,]<-c("Severity values are whole numbers between 0-3","YES")



#19. Check if there are any EX_BOUND colonies that have a conspicuously small shape length
sfm[which(sfm$EX_BOUND == "-1"& sfm$Shape_Length < 0.25),]

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

output[21,]<-c("No duplicate rows","1 potential duplicate")



#22. Make sure that if a coral is a remnant, that SEGLENGTH = 2.5 regardless of max diameter
remnant.seglegth <- sfm %>% filter(REMNANT == "-1" & SEGLENGTH != 2.5) 
remnant.olddead <- sfm %>% filter(REMNANT == "-1" & OLD_DEAD != 0)
remnant.error <- rbind(remnant.seglegth,remnant.olddead); nrow(remnant.error)

sfm$SEGLENGTH[which(sfm$REMNANT == -1 & sfm$SEGLENGTH != 2.5)] <- 2.5 #fix seglength error
sfm$SEGAREA <- sfm$SEGLENGTH*sfm$SEGWIDTH

sfm$OLD_DEAD[which(sfm$REMNANT == -1 & sfm$OLD_DEAD != 0)] <- NA #fix olddead error

output[22,]<-c("REMNANT filled out correctly","YES")


#23. Make sure that if a coral is a juvenile, they don't have a value in OLDDEAD, or any RD or CON columns
juv.olddead <- sfm %>% filter(JUVENILE == "-1" & OLD_DEAD != 0 | JUVENILE == "-1" & CON_1 != "NA" | 
                                JUVENILE == "-1" & CON_2 != "NA" | JUVENILE == "-1" & CON_3 != "NA" | 
                                JUVENILE == "-1" & RDCAUSE1 != "NA" |JUVENILE == "-1" & RDCAUSE2 != "NA" |
                                JUVENILE == "-1" & RDCAUSE3 != "NA"); nrow(juv.olddead)

output[23,]<-c("Juvenile colonies have no OLDDEAD, RD, or CON","YES")



#Export QC output table with appropriate file name
write.csv(output,"MARAMP2022_sfm_Calib_output.csv")



# Export QC'd data ----------------------------------------------------------------------------

sfm$COLONYID<-c(1:length(sfm$OID_))

#Separate by adults and juveniles
ad<-subset(sfm,JUVENILE==0|REMNANT==-1)
ad<-subset(ad,select=-c(OID_,totaldead))
j<-subset(sfm,JUVENILE=="-1") # includes segments where NO_COLONY = -1
j<-subset(j,select=c(COLONYID,ANALYST,OBS_YEAR,MISSION_ID,SITE,TRANSECT,SEGMENT,SEGLENGTH,SEGWIDTH,NO_COLONY,SPCODE,FRAGMENT,MORPH_CODE,
                     EX_BOUND,JUVENILE,FRAGMENT,REMNANT,Shape_Length,SEGAREA))

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
# setwd('T:/Benthic/Data/SfM/QC/')
write.csv(ad,"MARAMP2022_QCdsfm_ADULT_CALIB.csv",row.names = F)
write.csv(j,"MARAMP2022_QCdsfm_JUV_CALIB.csv",row.names = F)
write.csv(sfm,"MARAMP2022_output_FINAL_CALIB.csv",row.names = F)


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
colnames(ad)[colnames(ad)=="FRAGMENT"]<-"FRAGMENT_YN" #Change column name
colnames(ad)[colnames(ad)=="CON_1"]<-"COND" #Change column name
colnames(ad)[colnames(ad)=="CON_2"]<-"CONDITION_2" #Change column name
colnames(ad)[colnames(ad)=="CON_3"]<-"CONDITION_3" #Change column name
colnames(ad)[colnames(ad)=="SEV_1"]<-"SEVERITY_1" #Change column name
colnames(ad)[colnames(ad)=="SEV_2"]<-"SEVERITY_2" #Change column name
colnames(ad)[colnames(ad)=="SEV_3"]<-"SEVERITY_3" #Change column name
colnames(ad)[colnames(ad)=="Shape_Leng"]<-"COLONYLENGTH" #Change column name

colnames(j)[colnames(j)=="FRAGMENT"]<-"FRAGMENT_YN" #Change column name
colnames(j)[colnames(j)=="Shape_Leng"]<-"COLONYLENGTH" #Change column name


#Modify colunns to match standard REA data
ad$COLONYLENGTH<-ad$COLONYLENGTH*100 #convert from m to cm
ad$COLONYLENGTH<-ifelse(ad$NO_COLONY==-1,NA,ad$COLONYLENGTH) #make sure that the segements that had no colonies have a colony length = 0
ad$S_ORDER<-ifelse(ad$NO_COLONY==0 ,"Scleractinia",NA) #add S_order column
ad$SPCODE<-ifelse(ad$NO_COLONY==-1 ,"AAAA",as.character(ad$SPCODE)) #Change spcode to AAAA if there are no colonies observed in the segment
ad$COLONYID<-ifelse(ad$NO_COLONY==-1 ,NA,ad$COLONYID) #Change colonyid to NA if there are no colonies observed in the segment. COLONYID is a placeholder until Data Services can integrate it properly

#Create Genuscode and taxonname column from spcode
genlookup<-read.csv("T:/Benthic/Data/Lookup Tables/Genus_lookup.csv")
ad<-CreateGenusCode(ad,genlookup) 
colnames(ad)[colnames(ad)=="SPCODE"]<-"TAXONCODE" #Change column name
head(ad)


j$COLONYLENGTH<-j$COLONYLENGTH*100 #convert from m to cm
j$COLONYLENGTH<-ifelse(j$NO_COLONY==-1,NA,j$COLONYLENGTH) #make sure that the segements that had no colonies have a colony length = 0
j$S_ORDER<-ifelse(j$NO_COLONY==0,"Scleractinia",NA) #add S_order column
j$SPCODE<-ifelse(j$NO_COLONY==-1 ,"AAAA",as.character(j$SPCODE)) #Change spcode to AAAA if there are no colonies observed in the segment
j$COLONYID<-ifelse(j$NO_COLONY==-1 ,NA,j$COLONYID) #Change colonyid to NA if there are no colonies observed in the segment

j<-CreateGenusCode(j,genlookup) 
colnames(j)[colnames(j)=="SPCODE"]<-"TAXONCODE" #Change column name
head(j)


#Merge with Survey Master
survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")

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
write.csv(j,file="T:/Benthic/Data/SfM/QC/SfM_Juvenile_Demographic_MHI_2019_forInPort.csv")
write.csv(ad,file="T:/Benthic/Data/SfM/QC/SfM_Adult_demographic_MHI_2019_forInPort.csv")






# Join the sitevisit table with the QC'd sfm geodatabase table (NOT UPDATED) ---------------------------------------------------------------

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



