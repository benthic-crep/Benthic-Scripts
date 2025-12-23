#Merging v1 and v2 geodatabase and ensuring that no site_segs are missing
#By Corinne Amir
#Modified 7/24/20 by Courtney Couch

# Read dataframes pulled directly from geodatabases
#setwd("C:/Users/Corinne.Amir/Documents/GitHub/Benthic-Scripts/SfM/Geodatabase QC")
setwd("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Geodatabase QC")

v1 <- read.csv("HARAM2019_annotations_v1_MA_july242020.csv") 
v2 <- read.csv("HARAM2019_annotations_v2_ALL_july242020.csv") 

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


# #For now, remove these sites from the v1 csv file, but in future, re-export v1gdb without these site_segs
# v1 <-v1 %>% filter(v1$SITE !="KAU-2166")


#QC v1 geodatabase similar to v2 

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


# #### Run sfm-specific SiteNumLeadingZeros function #### old function
# SiteNumLeadingZeros_SfM <- function(site_names)
# {
#   tmp<-levels(site_names)
#   for (i in 1:length(tmp)) {
#     s<-tmp[i]
#     if (nchar(s)<9) {   # only change values where name length is too short ()
#       ss<-strsplit(as.character(tmp[i]), split="[_ |,-]")
#       s1<-ss[[1]][1]
#       s2<-ss[[1]][2]
#       if (length(x=grep("[A-Z]",unlist(strsplit(toupper(s2),""))))==0)
#       {
#         tmp[i]<-paste(s1, formatC(as.numeric(s2), width=5, flag="0"), sep="-")
#       }
#     }
#   }
#   levels(site_names)<-tmp
# 
#   return(site_names)
# }
# #### end function ####



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
            


#Add a dummy column for remnant
v1$REMNANT <- rep(0, times = nrow(v1))


#Add column for Transect
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
write.csv(v1, "HARAMP2019_v1_reformat_FINAL_jul242020.csv",row.names = F)

# 
# 
# #Merge v1 and v1 geodatabases (v1 was already QC'd)
# sfm <- rbind(v1,v2)
# dim(sfm) #should have dim = 10188, 35....DOESNT
# 
# 
# #Check if any site_segs are missing
# seg.per.site <- plyr::ddply(sfm,.(SITE, SEGMENT, SEGLENGTH), summarize, num.annotated = n_distinct(SEGLENGTH))
# eval.seg.per.site <- as.data.frame(acast(seg.per.site, SITE~SEGMENT, length))
# #eval.seg.per.site$Total <- rowSums(eval.seg.per.site)
# View(eval.seg.per.site) 


#23 sites without 15m segment
#12 sites with 2 values for 15m segment
#1 site missing all but 15m segment = just repeat site = remove?
#7 sites missing 2 values for 10m segment
#2 sites missing 2 values for 5m segment (same sites as 0m missing)
#4 sites missing 2 values for 0m segment
#1 site missing 5m segment (LAN-1819)

#Problem Sites
#           0 5 10 15
# HAW-04264 2 2  2  2 #MA Server QC -- OK
# HAW-04287 2 2  2  2 #MA Server QC -- OK
# HAW-04292 2 2  2  2 #MA Server QC -- OK
# HAW-04299 2 2  2  2 #MA Server QC -- OK
# HAW-03433 2 2  2  2 #Mia QC -- OK
# HAW-03445 2 2  2  2 #Mia QC -- OK
# HAW-03465 2 2  2  2 #Mia QC -- OK
# KAU-02133 2 2  2  2 #MA Server QC -- OK
# LAN-01811 2 2  2  2 #MA Server QC -- OK
# OAH-03235 2 2  2  2 #MA Server QC -- OK
# LAN-01814 2 2  2  2 #Mia QC -- OK
# MAI-02529 2 2  2  2 #Mia QC -- OK

# HAW-04224 2 2  1  0 #Missing 1+ segment
# HAW-04267 2 2  1  1 #RS QC -- OK
# KAH-00619 1 2  2  0 #FL QC -- OK
# KAH-00632 2 2  1  1 #FL QC -- OK
# KAH-00659 2 2  1  1 #AH QC -- OK
# KAU-02016 0 0  0  1 #MA QC -- OK
# LAN-01819 2 0  2  1 #FL QC -- OK
# MAI-02520 1 1  1  0 #CA QC -- OK
# MAI-02534 1 1  1  1 #FL QC -- OK
# MOL-02260 2 2  1  1 #FL QC -- OK
# MOL-02276 1 2  2  1 #FL QC -- OK

# MOL-02263 2 2  2  0 #RS QC -- OK
# MOL-02240 2 2  2  0 #ML QC -- OK
# MOL-02291 2 2  2  0 #Doesn't exist -- OK
# MOL-02265 2 2  2  0 #Doesn't exist -- OK
# MAI-02544 2 2  2  0 #Doesn't exist -- OK
# MOL-02221 2 2  2  0 #Doesn't exist -- OK
# MOL-02254 2 2  2  0 #Doesn't exist -- OK
# MAI-02466 2 2  2  0 #Doesn't exist -- OK
# MAI-02515 2 2  2  0 #Doesn't exist -- OK
# LAN-01795 2 2  2  0 #Doesn't exist -- OK
# HAW-04304 2 2  2  0 #Doesn't exist -- OK
# KAH-00608 2 2  2  0 #Doesn't exist -- OK
# HAW-04300 2 2  2  0 #Doesn't exist -- OK
# HAW-04220 2 2  2  0 #Doesn't exist -- OK
# HAW-03434 2 2  2  0 #Doesn't exist -- OK
# HAW-03436 2 2  2  0 #Doesn't exist -- OK
# HAW-04296 2 2  2  0 #Doesn't exist -- OK
# HAW-04259 2 2  2  0 #Doesn't exist -- OK

# -   NA    2 2  2  1 #Wrong name (14 rows) -- recorded