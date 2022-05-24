#Coral demography archiving QC
#Corinne Amir
#Last updated: 10-22-2020

library(dplyr)
install.packages('httr');require('httr')
install.packages('worms');require('worms')

setwd("C:/Users/Corinne.Amir/Documents/Archiving/HARAMP2019 SfM Belts")

ad <- read.csv("HARAMP19_SfM_Adult_CLEANED.csv")
# juv <- read.csv("JUVENILE_CORAL_OBS_F_2019.csv")
# taxa <- read.csv("TAXA_LIST_MHI_2019.csv")

#### General QC...look at the data ####

#Check colnames -- ok
colnames(ad) # has MISSIONID, REGION, REGION_NAME,ISLAND, ISLAND_CODE, LATITUDE, LONGITUDE, DEPTH_BIN, SIT_MIN_DEPTH, SITE_MAX_DEPTH, DATE_, DIVER, METHODNAME
colnames(juv) # has MISSIONID, REGION, REGION_NAME,ISLAND, ISLAND_CODE, LATITUDE, LONGITUDE, DEPTH_BIN, SIT_MIN_DEPTH, SITE_MAX_DEPTH, DATE_, DIVER, METHODNAME
colnames(pres)



#Do a general check of values within columns 
sapply(ad,unique)
#OLDDEAD:  valueS = 92, 7, 12
#RDEXTENT1: values = 12, 7
#EXTENT_1: NA and 0 used (just note in data dictionary), values include 7
#SEVERITY_1: NA and 0 used (just note in data dictionary)
#EXTENT_2: NA and 0 used (just note in data dictionary)
#SEVERITY_2: NA and 0 used (just note in data dictionary)


sapply(juv,unique)


#Certain columns should NOT have blanks or NAs? Any look incorrect (some checked earlier with sapply)?
ad %>% distinct(LATITUDE) %>% pull()
ad %>% distinct(LONGITUDE) %>% pull()
ad %>% distinct(DIVER) %>% pull()
ad %>% distinct(SITE) %>% pull()
juv %>% distinct(SITE) %>% pull()
pres %>% distinct(SITE) %>% pull()

#### end ####

#### Specialized QC checks ####
#Make dataframe for output questions
output<-data.frame(Error<-character(),stringsAsFactors = FALSE)


# 1. Check if TAXONNAME = blank, NO_COLONY_OBSERVED should = -1
ad.1 <- ad %>% filter(TAXONNAME == "" & NO_COLONY_OBSERVED != -1)
juv.1 <- juv %>% filter(TAXONNAME == "" & NO_COLONY_OBSERVED != -1)

output[1,] <-"2  errors in ad and 3 in juvand pres where there is no colony but NO_COLONY_OBSERVED = 0" # add error to output list
ad_errors = ad.1 # create dataframe of errors to export
juv_errors = juv.1 # create dataframe of errors to export


# 2. Check if site_max_depth is less than site_min_depth for any sites
depth.errors.ad <- ad %>% filter(SITE_MAX_DEPTH - SITE_MIN_DEPTH < 0) %>% 
                select(SITE, SITE_MAX_DEPTH, SITE_MIN_DEPTH,DIVER) #125 errors = MAI-2520 and HAW-4221
ad.2 <- ad %>% filter(SITE_MAX_DEPTH - SITE_MIN_DEPTH < 0)
          
depth.errors.juv <- juv %>% filter(SITE_MAX_DEPTH - SITE_MIN_DEPTH < 0) %>% 
                    select(SITE, SITE_MAX_DEPTH, SITE_MIN_DEPTH,DIVER) #44 errors = MAI-2520 and HAW-4221
juv.2 <- juv %>% filter(SITE_MAX_DEPTH - SITE_MIN_DEPTH < 0)

pres.2 <- pres %>% filter(SITE_MAX_DEPTH - SITE_MIN_DEPTH < 0)


output[2,] <- "no depth issues"
ad_errors <- rbind(ad_errors,ad.2)
juv_errors <- rbind(juv_errors,juv.2)
pres_errors <- pres.2



#3. Does the depth range seem correct?
range(ad$SITE_MAX_DEPTH) # 5-90
range(ad$SITE_MIN_DEPTH) # 4-87
range(ad$SITE_MAX_DEPTH-ad$SITE_MIN_DEPTH) #0-12
ad %>% mutate(SITE_MAX_DEPTH - SITE_MIN_DEPTH) %>% pull() 
ad.3 <- ad %>% filter(SITE_MAX_DEPTH - SITE_MIN_DEPTH >=10)

range(juv$SITE_MAX_DEPTH) # 5-90
range(juv$SITE_MIN_DEPTH) # 4-87
range(juv$SITE_MAX_DEPTH-juv$SITE_MIN_DEPTH) #-4-12
juv %>% mutate(SITE_MAX_DEPTH - SITE_MIN_DEPTH) %>% pull() #identical to adults
juv.3 <- juv %>% filter(SITE_MAX_DEPTH - SITE_MIN_DEPTH >=10)


output[3,] <- "some sites have depth change between 10-12 = ok? (not incl. in error csvs)"
ad_errors <- rbind(ad_errors,ad.3)
juv_errors <- rbind(juv_errors,juv.3)


#4. Are rows with shallow/mid/deep paired accordingly with depth range?
depth.ad.s <- ad %>% select(SITE, DEPTH_BIN,SITE_MAX_DEPTH, SITE_MIN_DEPTH) %>% filter(DEPTH_BIN == "Shallow")
range(depth.ad.s$SITE_MAX_DEPTH) # 5-24
depth.ad.m <- ad %>% select(SITE, DEPTH_BIN, SITE_MAX_DEPTH, SITE_MIN_DEPTH) %>% filter(DEPTH_BIN == "Mid")
range(depth.ad.m$SITE_MAX_DEPTH) # 21-59
depth.ad.d <- ad %>% select(SITE, DEPTH_BIN, SITE_MAX_DEPTH, SITE_MIN_DEPTH) %>% filter(DEPTH_BIN ==  "Deep")
range(depth.ad.d$SITE_MAX_DEPTH) # 61-90
ad.4 <- ad %>% filter(DEPTH_BIN == "Mid" & SITE_MIN_DEPTH < 25 | DEPTH_BIN == "Mid" & SITE_MAX_DEPTH < 25)


depth.juv.s <- juv %>% select(SITE, DEPTH_BIN, DEPTH_CODE, SITE_MAX_DEPTH, SITE_MIN_DEPTH) %>% filter(DEPTH_CODE == "S")
range(depth.juv.s$SITE_MAX_DEPTH) # 5-24
depth.juv.m <- juv %>% select(SITE, DEPTH_BIN, DEPTH_CODE, SITE_MAX_DEPTH, SITE_MIN_DEPTH) %>% filter(DEPTH_CODE == "M")
range(depth.juv.m$SITE_MAX_DEPTH) # 21-59
depth.juv.d <- juv %>% select(SITE, DEPTH_BIN, DEPTH_CODE, SITE_MAX_DEPTH, SITE_MIN_DEPTH) %>% filter(DEPTH_CODE == "D")
range(depth.juv.d$SITE_MAX_DEPTH) # 61-90
juv.4 <- juv %>% filter(DEPTH_BIN == "Mid" & SITE_MIN_DEPTH < 25 | DEPTH_BIN == "Mid" & SITE_MAX_DEPTH < 25)

pres.4 <- pres %>% filter(DEPTH_BIN == "Mid" & SITE_MIN_DEPTH < 25 | DEPTH_BIN == "Mid" & SITE_MAX_DEPTH < 25)

output[4,] <- "Shallow and Mid site depths overlap (max shallow depth = 24 and min mid depth = 21) = ok"
ad_errors <- rbind(ad_errors,ad.4)
juv_errors <- rbind(juv_errors,juv.4)
pres_errors <- rbind(pres_errors,pres.4) 


#5. Does the date range make sense
ad.5<-ad %>% group_by(MISSIONID,DATE_) %>% summarise() # ok
juv.5<-juv %>% group_by(MISSIONID,DATE_) %>% summarise() # ok
pres.5<-pres %>% group_by(MISSIONID,DATE_) %>% summarise() # ok


#6. Check if Latitude and Longitude make sense -- range OK
range(ad$LATITUDE)
range(ad$LONGITUDE)

range(juv$LATITUDE)
range(juv$LONGITUDE)



#7.  Check range of colony lengths
ad.small.colony.error <- ad %>% filter(COLONYLENGTH < 5 & FRAGMENT_YN != -1 & TAXONCODE != "PBER") %>% 
                         select(SITE,DIVER,TAXONCODE,TAXONNAME,FRAGMENT_YN) 
#adult colony length = 1-750 cm = ok
#juv colony length = 0.2-4.9 = ok
ad.7 <- ad %>% filter(COLONYLENGTH > 250) 


output[7,] <- "1 colony is 750cm and next highest is 250cm = ok?"
ad_errors <- rbind(ad_errors,ad.7)


#8. Does old dead and recent dead equal </= 100?
ad.8 <- ad %>% filter(OLDDEAD + RECENTDEAD_1 + RECENTDEAD_2 + RECENTDEAD_3 > 100) %>%
               select(SITE, OLDDEAD, RECENTDEAD_1, RECENTDEAD_2 ,RECENTDEAD_3,  DIVER) # OK

output[8,] <- "one colony has TLS and BLE recorded in recent dead and condition"


#9. Are all taxa listed in the datasets found in the master taxa list?
levels(ad$TAXONCODE) #Ad Corals not found in 2019 taxa list: BASP (3x, Balanophyllia sp.), PMOL (1x, Pocillopora molokensis), PODU (2x, Porites duerdeni), PSOL (4x, Porites solida)
levels(juv$TAXONCODE) 
levels(taxa$SPCODE) #Juv corals not found in 2019 taxa list: CVAU (12x, Cycloseris vaughani), PBRI (1x, Porites brighami), PCAP (1x, Pocillopora capitata)

ad.9 <- ad %>% filter(TAXONCODE == "BASP" | TAXONCODE == "PMOL" | TAXONCODE == "PODU" | TAXONCODE == "PSOL" |TAXONCODE == "PBRI" | TAXONCODE == "PCAP")
juv.9 <- juv %>% filter(TAXONCODE == "CVAU" | TAXONCODE == "PBRI" | TAXONCODE == "PCAP")                   

output[9,] <- "BASP, PMOL, PODU, PSOL, CVAU, PBRI, PCAP found in datasests, but not taxa list"
ad_errors <- rbind(ad_errors, ad.9)
juv_errors <- rbind(juv_errors, juv.9)



#10. Do all corals with bleaching have a severity and extent recorded?
ad[ad$SEVERITY_1=="0"& ad$CONDITION_1%in% c("BLE","BLP"),] # all ok
ad[ad$SEVERITY_2=="0"& ad$CONDITION_2%in% c("BLE","BLP"),]
ad[ad$SEVERITY_3=="0"& ad$CONDITION_3%in% c("BLE","BLP"),]
`%notin%` <- Negate(`%in%`)
ad[ad$SEVERITY_1!="0"& ad$CONDITION_1 %notin% c("BLE","BLP"),]
ad[ad$SEVERITY_2!="0"& ad$CONDITION_2 %notin% c("BLE","BLP"),]
ad[ad$SEVERITY_3!="0"& ad$CONDITION_3 %notin% c("BLE","BLP"),]



#11. Make sure that datasets only includes survey rounds 133, 134, and 135
check.round <- ad %>% select(ROUNDID); sapply(check.round,unique)
check.round <- juv %>% select(ROUNDID); sapply(check.round,unique)



#12. Do condition codes look erroneous?
check.con <- ad %>% select(CONDITION_1,CONDITION_2,CONDITION_3, CONDITION_1_DESC,CONDITION_2_DESC,CONDITION_3_DESC)
sapply(check.con,unique)
ad.12 <- ad %>% filter(CONDITION_2 == "MACA" | CONDITION_1 == "MACA")

output[12,] <-"MACA listed as a condition and recent dead = ok?"
ad_errors <- rbind(ad_errors,ad.12)



#13. Do recent dead cause codes look erroneous?
check.rdcause <- ad %>% select(RECENT_SPECIFIC_CAUSE_CODE_1,RECENT_SPECIFIC_CAUSE_CODE_2,RECENT_SPECIFIC_CAUSE_CODE_3,
                               RECENT_SPECIFIC_CAUSE_DESC_1,RECENT_SPECIFIC_CAUSE_DESC_2,RECENT_SPECIFIC_CAUSE_DESC_3)
sapply(check.rdcause,unique)
ad.13 <- ad %>% filter(RECENT_SPECIFIC_CAUSE_CODE_1=="MACA")



#14. Are any recent_specific_cause_codes missing an extent?
ad %>% filter(RECENT_SPECIFIC_CAUSE_CODE_1 == "" & RECENTDEAD_1 != 0) # all ok
ad %>% filter(RECENT_SPECIFIC_CAUSE_CODE_2 == "" & RECENTDEAD_2 != 0)
ad %>% filter(RECENT_SPECIFIC_CAUSE_CODE_3 == "" & RECENTDEAD_3 != 0)
ad %>% filter(RECENTDEAD_1 != 0 & RECENT_SPECIFIC_CAUSE_CODE_1 == "")
ad %>% filter(RECENTDEAD_2 != 0 & RECENT_SPECIFIC_CAUSE_CODE_2 == "")
ad %>% filter(RECENTDEAD_3 != 0 & RECENT_SPECIFIC_CAUSE_CODE_3 == "")



#15. Are any recent cause codes missing a condition description?
sapply(check.rdcause,unique)
a<-ad %>% filter(RECENT_SPECIFIC_CAUSE_CODE_1 != "" & RECENT_SPECIFIC_CAUSE_DESC_1 == "") %>% 
          select(COLONYID,RECENT_GENERAL_CAUSE_CODE_1,RECENT_GENERAL_CAUSE_DESC_1,RECENT_SPECIFIC_CAUSE_CODE_1,RECENT_SPECIFIC_CAUSE_DESC_1,RECENTDEAD_1,
                 RECENT_GENERAL_CAUSE_CODE_2,RECENT_GENERAL_CAUSE_DESC_2,RECENT_SPECIFIC_CAUSE_CODE_2,RECENT_SPECIFIC_CAUSE_DESC_2,RECENTDEAD_2,
                 RECENT_GENERAL_CAUSE_CODE_3,RECENT_GENERAL_CAUSE_DESC_3,RECENT_SPECIFIC_CAUSE_CODE_3,RECENT_SPECIFIC_CAUSE_DESC_3,RECENTDEAD_3,)
b<-ad %>% filter(RECENT_GENERAL_CAUSE_CODE_1 != "" & RECENT_GENERAL_CAUSE_DESC_1 == "") %>% 
select(COLONYID,RECENT_GENERAL_CAUSE_CODE_1,RECENT_GENERAL_CAUSE_DESC_1,RECENT_SPECIFIC_CAUSE_CODE_1,RECENT_SPECIFIC_CAUSE_DESC_1,RECENTDEAD_1,
       RECENT_GENERAL_CAUSE_CODE_2,RECENT_GENERAL_CAUSE_DESC_2,RECENT_SPECIFIC_CAUSE_CODE_2,RECENT_SPECIFIC_CAUSE_DESC_2,RECENTDEAD_2,
       RECENT_GENERAL_CAUSE_CODE_3,RECENT_GENERAL_CAUSE_DESC_3,RECENT_SPECIFIC_CAUSE_CODE_3,RECENT_SPECIFIC_CAUSE_DESC_3,RECENTDEAD_3,)
c<-ad %>% filter(RECENT_SPECIFIC_CAUSE_CODE_2 != "" & RECENT_SPECIFIC_CAUSE_DESC_2 == "") %>% 
select(COLONYID,RECENT_GENERAL_CAUSE_CODE_1,RECENT_GENERAL_CAUSE_DESC_1,RECENT_SPECIFIC_CAUSE_CODE_1,RECENT_SPECIFIC_CAUSE_DESC_1,RECENTDEAD_1,
       RECENT_GENERAL_CAUSE_CODE_2,RECENT_GENERAL_CAUSE_DESC_2,RECENT_SPECIFIC_CAUSE_CODE_2,RECENT_SPECIFIC_CAUSE_DESC_2,RECENTDEAD_2,
       RECENT_GENERAL_CAUSE_CODE_3,RECENT_GENERAL_CAUSE_DESC_3,RECENT_SPECIFIC_CAUSE_CODE_3,RECENT_SPECIFIC_CAUSE_DESC_3,RECENTDEAD_3,)
d<-ad %>% filter(RECENT_GENERAL_CAUSE_CODE_2 != "" & RECENT_GENERAL_CAUSE_DESC_2 == "") %>% 
select(COLONYID,RECENT_GENERAL_CAUSE_CODE_1,RECENT_GENERAL_CAUSE_DESC_1,RECENT_SPECIFIC_CAUSE_CODE_1,RECENT_SPECIFIC_CAUSE_DESC_1,RECENTDEAD_1,
       RECENT_GENERAL_CAUSE_CODE_2,RECENT_GENERAL_CAUSE_DESC_2,RECENT_SPECIFIC_CAUSE_CODE_2,RECENT_SPECIFIC_CAUSE_DESC_2,RECENTDEAD_2,
       RECENT_GENERAL_CAUSE_CODE_3,RECENT_GENERAL_CAUSE_DESC_3,RECENT_SPECIFIC_CAUSE_CODE_3,RECENT_SPECIFIC_CAUSE_DESC_3,RECENTDEAD_3,)
e<-ad %>% filter(RECENT_SPECIFIC_CAUSE_CODE_3 != "" & RECENT_SPECIFIC_CAUSE_DESC_3 == "") %>% 
select(COLONYID,RECENT_GENERAL_CAUSE_CODE_1,RECENT_GENERAL_CAUSE_DESC_1,RECENT_SPECIFIC_CAUSE_CODE_1,RECENT_SPECIFIC_CAUSE_DESC_1,RECENTDEAD_1,
       RECENT_GENERAL_CAUSE_CODE_2,RECENT_GENERAL_CAUSE_DESC_2,RECENT_SPECIFIC_CAUSE_CODE_2,RECENT_SPECIFIC_CAUSE_DESC_2,RECENTDEAD_2,
       RECENT_GENERAL_CAUSE_CODE_3,RECENT_GENERAL_CAUSE_DESC_3,RECENT_SPECIFIC_CAUSE_CODE_3,RECENT_SPECIFIC_CAUSE_DESC_3,RECENTDEAD_3,)
f<-ad %>% filter(RECENT_GENERAL_CAUSE_CODE_3 != "" & RECENT_GENERAL_CAUSE_DESC_3 == "") %>% 
select(COLONYID,RECENT_GENERAL_CAUSE_CODE_1,RECENT_GENERAL_CAUSE_DESC_1,RECENT_SPECIFIC_CAUSE_CODE_1,RECENT_SPECIFIC_CAUSE_DESC_1,RECENTDEAD_1,
       RECENT_GENERAL_CAUSE_CODE_2,RECENT_GENERAL_CAUSE_DESC_2,RECENT_SPECIFIC_CAUSE_CODE_2,RECENT_SPECIFIC_CAUSE_DESC_2,RECENTDEAD_2,
       RECENT_GENERAL_CAUSE_CODE_3,RECENT_GENERAL_CAUSE_DESC_3,RECENT_SPECIFIC_CAUSE_CODE_3,RECENT_SPECIFIC_CAUSE_DESC_3,RECENTDEAD_3,)

recent.dead.errors<-rbind(a,b,c,d,e,f) #many errors = separate from the other ad error list



#16. Are any conditions missing an extent?
ad %>% filter(CONDITION_1 == "" & EXTENT_1 != 0) # all ok
ad %>% filter(CONDITION_2 == "" & EXTENT_2 != 0)
ad %>% filter(CONDITION_3 == "" & EXTENT_3 != 0)
ad %>% filter(EXTENT_3 == 0 & CONDITION_3 != "")
ad %>% filter(EXTENT_2 == 0 & CONDITION_2 != "")
ad %>% filter(EXTENT_1 == 0 & CONDITION_1 != "")


#17. Are any condition codes missing a condition description?
a<- ad %>% filter(CONDITION_1 != "" & CONDITION_1_DESC == "" | CONDITION_1 == "" & CONDITION_1_DESC != "") %>% 
           select(COLONYID,CONDITION_1,CONDITION_1_DESC,CONDITION_2,CONDITION_2_DESC,CONDITION_3,CONDITION_3_DESC)
b<- ad %>% filter(CONDITION_2 == "" & CONDITION_2_DESC != "" | CONDITION_2 != "" & CONDITION_2_DESC == "") %>% 
  select(COLONYID,CONDITION_1,CONDITION_1_DESC,CONDITION_2,CONDITION_2_DESC,CONDITION_3,CONDITION_3_DESC)
c<- ad %>% filter(CONDITION_3 == "" & CONDITION_3_DESC != "" | CONDITION_3 != "" & CONDITION_3_DESC == "") %>% 
  select(COLONYID,CONDITION_1,CONDITION_1_DESC,CONDITION_2,CONDITION_2_DESC,CONDITION_3,CONDITION_3_DESC)

ad.17 <- rbind(a,b,c) 
ad_errors <- rbind(ad_errors,ad.17)


#18. Make sure that if NO_COLONY=-1 none of the following columns have been populated
a<-ad %>% filter(ad$TAXONCODE != "AAAA" & ad$NO_COLONY_OBSERVED == -1) # 51 errors: present corals with NO_COLONY = -1
a<-ad %>% filter(ad$TAXONCODE == "AAAA" & ad$NO_COLONY_OBSERVED != -1) # 2 errors: SPCODE = AAAA, but NO_COLONY = 0
ad %>% filter(ad$FRAGMENT_YN == -1 & ad$NO_COLONY_OBSERVED == -1)
a<-ad %>% filter(ad$MORPHOLOGY == "" & ad$NO_COLONY_OBSERVED != -1) # 2 errors: same as second line of code
b<-ad %>% filter(ad$MORPHOLOGY != "" & ad$NO_COLONY_OBSERVED == -1) # 51 errors: [prob] same as first line of code
ad.8 <- ad %>% filter(ad$TAXONCODE != "AAAA" & ad$NO_COLONY_OBSERVED == -1 |ad$TAXONCODE == "AAAA" & ad$NO_COLONY_OBSERVED != -1)

a<-juv %>% filter(juv$TAXONCODE != "AAAA" & juv$NO_COLONY_OBSERVED == -1) # 7 errors: present corals with NO_COLONY = -1
a<-juv %>% filter(juv$TAXONCODE == "AAAA" & juv$NO_COLONY_OBSERVED != -1) # 3 errors: SPCODE = AAAA, but NO_COLONY = 0
a<-juv %>% filter(juv$MORPHOLOGY == "" & juv$NO_COLONY_OBSERVED != -1) # 3 errors: same as second line of code
a<-juv %>% filter(juv$MORPHOLOGY != "" & juv$NO_COLONY_OBSERVED == -1) # 7 errors: present corals with NO_COLONY = -1
juv.8 <- juv %>% filter(juv$TAXONCODE != "AAAA" & juv$NO_COLONY_OBSERVED == -1 |juv$TAXONCODE == "AAAA" & juv$NO_COLONY_OBSERVED != -1)


output[18,] <-"51 errors in adult where NO_COLONY = -1, but a coral exists; 2 errors in adult where NO_COLONY = 0, but taxoncode = AAAA and morphology = blank;
              7 errors in juv where NO_COLONY = -1, but coral exists; 3 errors in juv where TAXONCODE = AAAA, morphology = blank, but NO_COLONY = 0"
juv_errors <- rbind(juv_errors,juv.18)
ad_errors <- rbind(ad_errors,ad.18)


#19. Identify colonies have the same CON code across multiple CON columns
CON_dup <- ad %>% select(CONDITION_1,CONDITION_2,CONDITION_3, CONDITION_1_DESC,CONDITION_2_DESC,CONDITION_3_DESC) #subset dataset for ease
sapply(CON_dup,unique)

CON_dup$CONDITION_1 <-factor(CON_dup$CONDITION_1, levels=
                         c("ALG", "BL", "BIN", "tre","BLE","BLP","MACA","OVRG", "DAMG", 
                           "DIS","TLS","FUG","PRS","PTR", "SEDI","SGA","TIN"))
CON_dup$CONDITION_2 <-factor(CON_dup$CONDITION_2, levels=
                         c("ALG", "BL", "BIN", "tre","BLE","BLP","MACA","OVRG", "DAMG", 
                           "DIS","TLS" ,"FUG","PRS","PTR", "SEDI","SGA","TIN")) #Give columns the full list of codes used in the dataaset
CON_dup$CONDITION_3 <-factor(CON_dup$CONDITION_3, levels=
                         c("ALG", "BL", "BIN", "tre","BLE","BLP","MACA","OVRG", "DAMG", 
                           "DIS","TLS" ,"FUG","PRS","PTR", "SEDI","SGA","TIN"))

CON_check1 <- CON_dup %>% filter(CONDITION_1==CONDITION_2);nrow(CON_check1) # extract rows with duplicate levels among CON_1,2, and 3
CON_check2 <- CON_dup %>% filter(CONDITION_1==CONDITION_3);nrow(CON_check2) 
CON_check3 <- CON_dup %>% filter(CONDITION_2==CONDITION_3);nrow(CON_check3) 

output[19,]<-"MACA and TLS found in CON_1/2 = correct? (not in ad_errors.csv)"



#20. Identify colonies have the same RDCAUSE code across multiple RDCAUSE columns
RD_dup <- ad %>% select(RECENT_SPECIFIC_CAUSE_CODE_1,RECENT_SPECIFIC_CAUSE_CODE_2,RECENT_SPECIFIC_CAUSE_CODE_3) 
sapply(RD_dup,unique)

RD_dup$RECENT_SPECIFIC_CAUSE_CODE_1 <-factor(RD_dup$RECENT_SPECIFIC_CAUSE_CODE_1, level = c(
                                     "BFI", "COTS", "DAMG", "FISH", "GAST", "MACA", "PRED", "TLS", "UNKN", "WSY"))
RD_dup$RECENT_SPECIFIC_CAUSE_CODE_2 <-factor(RD_dup$RECENT_SPECIFIC_CAUSE_CODE_2, level = c(
                                     "BFI", "COTS", "DAMG", "FISH", "GAST", "MACA", "PRED", "TLS", "UNKN", "WSY"))
RD_dup$RECENT_SPECIFIC_CAUSE_CODE_3 <-factor(RD_dup$RECENT_SPECIFIC_CAUSE_CODE_3, level = c(
                                     "BFI", "COTS", "DAMG", "FISH", "GAST", "MACA", "PRED", "TLS", "UNKN", "WSY"))

RD_check1 <- RD_dup %>% filter(RECENT_SPECIFIC_CAUSE_CODE_1==RECENT_SPECIFIC_CAUSE_CODE_2);nrow(RD_check1) # extract rows with duplicate levels among RD_1,2, and 3
RD_check2 <- RD_dup %>% filter(RECENT_SPECIFIC_CAUSE_CODE_1==RECENT_SPECIFIC_CAUSE_CODE_3);nrow(RD_check2) 
RD_check3 <- RD_dup %>% filter(RECENT_SPECIFIC_CAUSE_CODE_2==RECENT_SPECIFIC_CAUSE_CODE_3);nrow(RD_check3) 



#21. Identify colonies with 0% recent dead, but has an RDCAUSE code - This check should result in 0 records   
ad[ad$RECENTDEAD_1 == 0 & ad$RECENT_SPECIFIC_CAUSE_CODE_1 != "",] # all ok
ad[ad$RECENTDEAD_2 == 0 & ad$RECENT_SPECIFIC_CAUSE_CODE_2 != "",]
ad[ad$RECENTDEAD_3 == 0 & ad$RECENT_SPECIFIC_CAUSE_CODE_3 != "",]

                           


#22. Identify colonies with recent dead >0%, but there is no RDCAUSE code - This check should result in 0 records  
ad[ad$RECENTDEAD_1 > 0 & ad$RECENT_SPECIFIC_CAUSE_CODE_1 =="",]
ad[ad$RECENTDEAD_2 > 0 & ad$RECENT_SPECIFIC_CAUSE_CODE_2 =="",]
ad[ad$RECENTDEAD_3 > 0 & ad$RECENT_SPECIFIC_CAUSE_CODE_3 =="",]
ad.22 <- ad[ad$RECENTDEAD_1 > 0 & ad$RECENT_SPECIFIC_CAUSE_CODE_1 =="",]

output[22,]<-"10 errors in RD_1 (4 have general cause, but specific = blank. All others don't have general or specific codes, but have recent dead percentage)"
ad_errors <- rbind(ad.22,ad_errors)


#23. Identify colonies with NO % EXTENT, but a condition - This check should result in 0 records   
ad[ad$EXTENT_1=="0"& ad$CONDITION_1!="",]
ad[ad$EXTENT_2=="0"& ad$CONDITION_2!="",]
ad[ad$EXTENT_3=="0"& ad$CONDITION_3!="",]

check.extent <- ad %>% select(COLONYID,CONDITION_1,CONDITION_1_DESC,EXTENT_1,CONDITION_2,CONDITION_2_DESC,EXTENT_2,CONDITION_3,CONDITION_3_DESC, EXTENT_3) #take a closer look


#24. Identify colonies that have no condition, but a value in extent - This check should result in 0 records   
a<-ad[ad$CONDITION_1=="" & ad$EXTENT_1!="0",]
a<-ad[ad$CONDITION_1=="" & ad$EXTENT_1!="0",]
a<-ad[ad$CONDITION_1=="" & ad$EXTENT_1!="0",]



#25. Identify colonies with nothing in condition column, but a value in severity. Double check that these shouldn't be 0  
a<-ad[ad$EXTENT_1=="0"& ad$SEVERITY_1!="0",]
a<-ad[ad$EXTENT_2=="0"& ad$SEVERITY_2!="0",]
a<-ad[ad$EXTENT_3=="0"& ad$SEVERITY_3!="0",]



#26. Make sure that values in SEV are only 0,1,2, or 3
a<-ad[ad$SEVERITY_1>5,]
a<-ad[ad$SEVERITY_2>5,]
a<-ad[ad$SEVERITY_3>5,]



#27. Check for duplicate rows in dataframe
ad %>% group_by(COLONYID) %>% filter(n()>1) # ok



#28. Check that X_Bound is only 0 or -1

#29. If EX_BOUND = -1, ______ should be blank

# Check that Fragment is only 0 or -1

# 31. If Fragment = -1, ____ should be blank



#28. Find which rows are missing an APHIAID (that are not UNKN or AAAA)
ad.aph <- ad %>% select(SCIENTIFIC_NAME,TAXONNAME,TAXONCODE,APHIAID)
ad.aph <- distinct(ad.aph) #rows with only genus level missing APHIAID
ad.28 <- ad.aph[ad.aph$SCIENTIFIC_NAME=="",]
ad.28 <- ad.28 %>% filter(TAXONCODE!="AAAA")

juv.aph <- juv %>% select(SCIENTIFIC_NAME,TAXONNAME,TAXONCODE,APHIAID)
juv.aph <- distinct(juv.aph) #rows with only genus level missing APHIAID
juv.28 <- juv.aph[juv.aph$SCIENTIFIC_NAME=="",]
juv.28 <- juv.28 %>% filter(TAXONCODE!="AAAA"&TAXONCODE !="UNKN")


#Find APHIA ID for rows with missing values
ad.juv.28 <- full_join(juv.28,ad.28)
taxonnames <- as.vector(ad.juv.28$TAXONNAME)
taxalist<-wormsbymatchnames(taxonnames)
View(list)
#write.csv(taxalist,"WoRMS_APHIAIDs_missing_unabridged.csv") #full writeup of missing aphia IDs and their descriptions (double check before adding)

#Consolidate list of rows/COLONYIDs that need to APHIAID added....FIX UP LATER
aphiaid.add <- taxalist[,c(1,3)]
sapply(aphiaid.add,unique) #get list to add to following line of code
aphiaid.add <- aphiaid.add %>% mutate(scientificnames, levels = c("Porites" , "Pocillopora"  ,"Cyphastrea", "Leptoseris" ,"Leptastrea",   "Montipora" , "Pavona","Psammocora" , "Cycloseris" ,"Tubastraea" , "Fungia", "Balanophyllia"),
                                      TAXONNAME = c("Porites sp" , "Pocillopora sp"  ,"Cyphastrea sp",  "Leptoseris sp"  ,"Leptastrea sp",  "Montipora sp" , "Pavona sp","Psammocora sp" , "Cycloseris sp" ,"Tubastraea sp" , "Fungia sp", "Balanophyllia sp")) 
colnames(aphiaid.add) <- c("APHIAID" ,"SCIENTIFIC_NAME", "levels" , "TAXONNAME")

#adults
ad.aph.miss <- ad %>% select(COLONYID,SCIENTIFIC_NAME,TAXONNAME,TAXONCODE,APHIAID)
ad.aph.miss <- ad.aph.miss[ad.aph.miss$SCIENTIFIC_NAME=="",]
ad.aph.miss <- ad.aph.miss %>% filter(TAXONCODE!="AAAA")                                  

ad.aphiaid.needed <- left_join(ad.aph.miss,aphiaid.add, by = "TAXONNAME")
 
colnames(ad.aphiaid.needed)
ad.aphiaid.needed <- select(ad.aphiaid.needed, c("COLONYID" ,"TAXONNAME" ,  "TAXONCODE"  ,  "APHIAID.y"))

#juveniles
juv.aph.miss <- juv %>% select(COLONYID,SCIENTIFIC_NAME,TAXONNAME,TAXONCODE,APHIAID)
juv.aph.miss <- juv.aph.miss[juv.aph.miss$SCIENTIFIC_NAME=="",]
juv.aph.miss <- juv.aph.miss %>% filter(TAXONCODE!="AAAA")  
juv.aph.miss <- juv.aph.miss %>% filter(TAXONCODE!="UNKN")  

juv.aphiaid.needed <- left_join(juv.aph.miss,aphiaid.add, by = "TAXONNAME")
juv.aphiaid.needed <- select(juv.aphiaid.needed, c("COLONYID" ,"TAXONNAME" ,  "TAXONCODE"  ,  "APHIAID.y"))


# Export aphia ID list
write.csv(ad.aphiaid.needed,"ADULT_CORAL_OBS_E_2019_APHIAIDs.csv")
write.csv(juv.aphiaid.needed,"JUVENILE_CORAL_OBS_F_2019_APHIAIDs.csv")


#### end ####

#### Export error csv files ####
# Export error list
write.csv(output,"Archive_QC_Errors_Jul72020.csv")

#remove duplicate rows from error dataframes
ad_errors <- ad_errors[!duplicated(ad_errors$COLONYID),]
juv_errors <- juv_errors[!duplicated(juv_errors$COLONYID),]
pres_errors <- pres_errors[!duplicated(pres_errors$SEGMENTID),]
recent.dead.errors<-recent.dead.errors[!duplicated(recent.dead.errors$COLONYID),]
con.errors<-con.errors[!duplicated(con.errors$COLONYID),]

write.csv(ad_errors,"adult_errors.csv")
write.csv(juv_errors,"juvenile_errors.csv")
write.csv(pres_errors,"presence_absence_errors.csv")
#write.csv(recent.dead.errors,"recent_dead_errors.csv")
#write.csv(con.errors,"condition_errors.csv")


#### end ####
