#This script reads in the diver and SfM-generated demographic data that has been QC'd and cleaned up
#Then generates site-level or segment-level summarized that for methods comparision
#Script created by Courtney Couch. updated on 10/27/20

rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

#Read in files
ad_sfm<-read.csv("T:/Benthic/Data/SfM/Analysis Ready/MARAMP22_SfM_Adult_CLEANED.csv")
ad_<-read.csv("T:/Benthic/Data/SfM/Analysis Ready/ASRAMP23_SfM_Adult_CLEANED.csv")
ad_sfm <- bind_rows(ad_sfm, ad_)
j_sfm<-read.csv("T:/Benthic/Data/SfM/Analysis Ready/MARAMP22_SfM_Juv_CLEANED.csv") 
sectors<-read.csv("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)


#Double check that the number of segments in the geodatabase matches what annoators said they completed 
#metadata file manually assembled from the tracking sheet pulled from google drive 
meta<-read.csv("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/MARAMP2022_SfM_Meta.csv")
meta<-meta[,c("ISLAND","SITE","Mosaic_Issues","Segments_Annotated","Rugosity")]
head(meta)
levels(as.factor(meta$Mosaic_Issues))

table(meta$Mosaic_Issues)

# 
# #Missing segments from geodatabase FOR ADULTS
# seg_tally<-ddply(ad_sfm,.(ISLAND,SITE),
#                  summarize,
#                  Segments_inGD=length(unique(SEGMENT)))
# 
# tmp.seg<-full_join(meta,seg_tally)
# View(tmp.seg)
# 
# miss.seg<-dplyr::filter(tmp.seg, Segments_Annotated !=Segments_inGD);miss.seg #identify sites that have missing segments
# miss.site<-dplyr::filter(tmp.seg, is.na(Segments_inGD));miss.site #identify sites that have missing segments
# 
# 
# #Missing segments from geodatabase FOR JUV
# seg_tally<-ddply(j_sfm,.(ISLAND,SITE),
#                  summarize,
#                  Segments_inGD=length(unique(SEGMENT)))
# 
# 
# tmp.seg<-full_join(meta,seg_tally)
# View(tmp.seg)
# 
# miss.seg<-dplyr::filter(tmp.seg, Segments_Annotated !=Segments_inGD);miss.seg #identify sites that have missing segments
# miss.site<-dplyr::filter(tmp.seg, is.na(Segments_inGD));miss.site #identify sites that have missing segments


#Check if any site-segments have been dropped 
ad_sfm$SITE_SEG<-paste(ad_sfm$SITE,ad_sfm$SEGMENT,sep ="_")
j_sfm$SITE_SEG<-paste(j_sfm$SITE,j_sfm$SEGMENT,sep ="_")
length(unique(ad_sfm$SITE_SEG));length(unique(ad_sfm$SITE)) #should be 389 ss and 104 sites
length(unique(j_sfm$SITE_SEG));length(unique(j_sfm$SITE)) #should be 312 ss and 104 sites

ad_sfm <- subset(ad_sfm, TRANSECT == "A")
j_sfm <- subset(j_sfm, TRANSECT == "A")

ad_sfm
# PREP VISUAL DIVER DATA ---------------------------------------------
## LOAD benthic data
awd_<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED.csv")
jwd_<-read.csv("T:/Benthic/Data/SfM/Analysis Ready/MARAMP22_DIVERJuv_CLEANED.csv")

awd<-subset(awd_,OBS_YEAR>="2022"&TRANSECT==1)
jwd<-subset(jwd_,TRANSECT==1)


#Changing Analyst names to match SfM
awd$DIVER<-ifelse(awd$DIVER=="J_E","JE",as.character(awd$DIVER))
awd$DIVER<-ifelse(awd$DIVER=="J_C","JC",as.character(awd$DIVER))
jwd$DIVER<-ifelse(jwd$DIVER=="J_E","JE",as.character(jwd$DIVER))
jwd$DIVER<-ifelse(jwd$DIVER=="J_C","JC",as.character(jwd$DIVER))


#Colony fragments and scleractinans are subseted in the functions 
#Add a column for adult fragments so we can remove them from the dataset later (-1 indicates fragment)
awd$FRAGMENT[is.na(awd$FRAGMENT)] <- 0
jwd$Fragment <- 0 # you need to add this column so that you can use the site level functions correctly
colnames(jwd)[colnames(jwd)=="Fragment"]<-"FRAGMENT"

#Simplify Bleaching Severity categories: in 2019 the team decided to simplify the bleaching severity from 1-5 to 1-3 to improve consistency in severity values
#This code converts the severity data collected prior to 2019 to a 1-3 scale
awd$DATE_ <- as.Date(awd$DATE_, format = "%Y-%m-%d")
jwd$DATE_ <- as.Date(jwd$DATE_, format = "%Y-%m-%d")

#awd_pre <- awd %>% filter(DATE_ < as.Date('2019-07-11'))
#awd_post<-awd %>% filter(DATE_ >= as.Date('2019-07-11'))
#
#awd_pre<-Convert_Severity(awd_pre,"SEVERITY_1","SEVERITY_1n")
#awd_pre<-Convert_Severity(awd_pre,"SEVERITY_2","SEVERITY_2n")
##awd_pre<-Convert_Severity(awd_pre,"SEVERITY_3","SEVERITY_3n") #There were no severity measurements prior to 2020
#
#head(awd_pre)
#
##After checking that severity numbers were changed correctly, convert back to original column names & drop original columns
#awd_pre<-subset(awd_pre,select=-c(SEVERITY_1));colnames(awd_pre)[which(colnames(awd_pre) == 'SEVERITY_1n')] <- "SEVERITY_1" #change group to whatever your grouping field is.
#awd_pre<-subset(awd_pre,select=-c(SEVERITY_2));colnames(awd_pre)[which(colnames(awd_pre) == 'SEVERITY_2n')] <- "SEVERITY_2" #change group to whatever your grouping field is.
##awd_pre<-subset(awd_pre,select=-c(SEVERITY_3));colnames(awd_pre)[which(colnames(awd_pre) == 'SEVERITY_3n')] <- "SEVERITY_3" #change group to whatever your grouping field is.
#awd_pre$SEVERITY_3<-NA
#
#head(awd_pre)
#
##Combine dataframes before and after 2019 & check that rows weren't dropped
#awd.<-rbind(awd_pre,awd_post);write.csv(awd.,"test.csv")
#
##Change bleaching severity = 1 to NA
#awd.<-awd. %>% mutate_at(.vars = c("CONDITION_1", "EXTENT_1", "SEVERITY_1"), 
#                         list(~replace(.,CONDITION_1 =='BLE' & SEVERITY_1=='1', NA)));View(awd.)
#awd.<-awd. %>% mutate_at(.vars = c("CONDITION_2", "EXTENT_2", "SEVERITY_2"), 
#                         list(~replace(.,CONDITION_2 =='BLE' & SEVERITY_2=='1', NA)))
#awd.<-awd. %>% mutate_at(.vars = c("CONDITION_3", "EXTENT_3", "SEVERITY_3"), 
#                         list(~replace(.,CONDITION_3 =='BLE' & SEVERITY_3=='1', NA)))
#
#nrow(awd)
#nrow(awd.);head(awd.)
#awd<-awd.; rm("awd.") #remove temporary dataframe if all good. 

#Change columns to merge with sfm data
colnames(awd)[which(colnames(awd) == 'DIVER')] <- "ANALYST"
colnames(jwd)[which(colnames(jwd) == 'DIVER')] <- "ANALYST"
awd$EX_BOUND<-0;awd$EX_BOUND<-as.numeric(awd$EX_BOUND) #add ex_bound columns to match SfM
jwd$EX_BOUND<-0;jwd$EX_BOUND<-as.numeric(jwd$EX_BOUND)




#Calculate segment area
awd$SEGAREA<-awd$SEGLENGTH*awd$SEGWIDTH
jwd$SEGAREA<-jwd$SEGLENGTH*jwd$SEGWIDTH

#Only include sites and segments surveyed by divers during HARAMP 2019
awd$SITE_SEG<-paste(awd$SITE,awd$SEGMENT,sep="_")
jwd$SITE_SEG<-paste(jwd$SITE,jwd$SEGMENT,sep="_")

#remove columns that aren't needed & 
awd<-dplyr::select(awd,-c(TRANSECTAREA,bANALYSIS_SCHEME,ANALYSIS_YEAR,EXCLUDE_FLAG,REGION_NAME,NO_SURVEY_YN,DATE_,ISLANDCODE))
jwd<-dplyr::select(jwd,-c(TRANSECTAREA,bANALYSIS_SCHEME,ANALYSIS_YEAR,EXCLUDE_FLAG,REGION_NAME,NO_SURVEY_YN,DATE_,ISLANDCODE))
awd<-dplyr::filter(awd, SITE_SEG %in% c(ad_sfm$SITE_SEG));head(awd) 
jwd<-dplyr::filter(jwd, SITE_SEG %in% c(j_sfm$SITE_SEG));head(jwd) 

#Drop segments that have <2.5 segarea for adults and <1 for juveniles
#awd<-dplyr::filter(awd, SEGAREA==2.5);View(awd) 
#jwd<-dplyr::filter(jwd, SEGAREA==1);View(jwd) 

#visually check and fix that partial segments were the same

x<- unique(ad_sfm[c("SITE_SEG","SEGAREA")])
y<- unique(awd[c("SITE_SEG","SEGAREA")])
z<- merge(x,y, by = "SITE_SEG")
View(z[which(z$SEGAREA.x != z$SEGAREA.y),])

ad_sfm$SEGLENGTH[which(ad_sfm$SITE_SEG == "ASC-00612_15")] <-2.4
ad_sfm$SEGLENGTH[which(ad_sfm$SITE_SEG == "ASC-00627_5")] <-1.85
ad_sfm$SEGLENGTH[which(ad_sfm$SITE_SEG == "GUA-02781_10")] <-2.0
ad_sfm$SEGLENGTH[which(ad_sfm$SITE_SEG == "MAU-01233_5")] <-2.2
ad_sfm$SEGLENGTH[which(ad_sfm$SITE_SEG == "TIN-00884_10")] <-2.0


ad_sfm <- ad_sfm[-2870,]
ad_sfm <- ad_sfm[-which(ad_sfm$SITE_SEG == "GUA-02681_10"),]
ad_sfm$SITE_SEG[which(ad_sfm$SITE_SEG == "SAI-01921_0" & ad_sfm$SEGAREA == 1.8)] <- "SAI-01921_5"
ad_sfm$SEGLENGTH <- ad_sfm$SEGLENGTH[which(ad_sfm$SITE == "ROT-00830")] <- 2.5
ad_sfm$SEGWIDTH <- ad_sfm$SEGWIDTH[which(ad_sfm$SITE == "ROT-00830")] <- 1

ad_sfm$SEGAREA <- ad_sfm$SEGLENGTH*ad_sfm$SEGWIDTH

j_sfm$SEGLENGTH <- j_sfm$SEGLENGTH[which(j_sfm$SITE == "ROT-00830")] <- 1
j_sfm$SEGWIDTH <- j_sfm$SEGWIDTH[which(j_sfm$SITE == "ROT-00830")] <- 1

j_sfm$SEGAREA <- j_sfm$SEGLENGTH*j_sfm$SEGWIDTH

x<- unique(j_sfm[c("SITE_SEG","SEGAREA")])
y<- unique(jwd[c("SITE_SEG","SEGAREA")])
z<- merge(x,y, by = "SITE_SEG")
View(z[which(z$SEGAREA.x != z$SEGAREA.y),])

j_sfm$SEGAREA[which(j_sfm$SITE_SEG == "PAG-01301_10" |
                    j_sfm$SITE_SEG == "SAI-01888_0" |
                    j_sfm$SITE_SEG == "SAI-01888_5")] <- .5

##
length(unique(awd$SITE))
length(unique(jwd$SITE))

awd<-dplyr::filter(awd, SEGAREA==2.5)
jwd<-dplyr::filter(jwd, SEGAREA==1)
ad_sfm<-dplyr::filter(ad_sfm, SEGAREA==2.5)
j_sfm<-dplyr::filter(j_sfm, SEGAREA==1)

#Remove segments that were annoated in SfM, but not surveyd by divers
ad_sfm_sub<-dplyr::filter(ad_sfm, SITE_SEG %in% c(awd$SITE_SEG));nrow(ad_sfm);nrow(ad_sfm_sub)
j_sfm_sub<-dplyr::filter(j_sfm, SITE_SEG %in% c(jwd$SITE_SEG));nrow(j_sfm);nrow(j_sfm_sub)

awd_sub<-dplyr::filter(awd, SITE_SEG %in% c(ad_sfm_sub$SITE_SEG))
jwd_sub<-dplyr::filter(jwd, SITE_SEG %in% c(j_sfm_sub$SITE_SEG))

length(unique(ad_sfm_sub$SITE));length(unique(ad_sfm_sub$SITE_SEG))
length(unique(j_sfm_sub$SITE));length(unique(j_sfm_sub$SITE_SEG))

# Add Method column to diver data
awd$METHOD<-"Diver";awd$METHOD<-as.factor(awd$METHOD)
jwd$METHOD<-"Diver";jwd$METHOD<-as.factor(jwd$METHOD)

awd_esa<-dplyr::filter(awd_sub, SPCODE %in% c("PDIF", "ICRA", "AGLO"))
ad_sfm_esa<-dplyr::filter(ad_sfm_sub, SPCODE %in% c("PDIF", "ICRA", "AGLO"))


#Merge diver and SfM data
sort(colnames(awd))
sort(colnames(ad_sfm_sub))

sort(colnames(jwd))
sort(colnames(j_sfm_sub))

sapply(awd,class)
sapply(ad_sfm,class)

awd.all<-rbind(ad_sfm_sub,awd_sub)
jwd.all<-rbind(j_sfm_sub,jwd_sub)

#Drop segments that have <2.5 segarea for adults and <1 for juveniles
awd.all<-dplyr::filter(awd.all, SEGAREA==2.5)
jwd.all<-dplyr::filter(jwd.all, SEGAREA==1)

#Calculate transect area
awd.all$TRANSECTAREA<-TransectareaMETHOD(awd.all)
jwd.all$TRANSECTAREA<-TransectareaMETHOD(jwd.all)

#Double check transect areas
summary(jwd.all$TRANSECTAREA)
summary(awd.all$TRANSECTAREA)

#Remove juvenile colonies <0.7cm
tmp<-jwd.all %>% mutate_at(.vars = c("GENUS_CODE", "SPCODE","TAXONCODE"), 
                           list(~replace(.,COLONYLENGTH <0.7, "AAAA")));View(tmp)
tmp<-tmp %>% mutate_at(.vars = c("S_ORDER", "COLONYID","TAXONNAME"), 
                       list(~replace(.,TAXONCODE=="AAAA", NA)));View(tmp)
jwd.all<-tmp %>% mutate_at(.vars = c("COLONYLENGTH"), 
                           list(~replace(.,TAXONCODE=="AAAA", 0)));View(jwd.all)

#remove juveniles >4.5
tmp<-jwd.all %>% mutate_at(.vars = c("GENUS_CODE", "SPCODE","TAXONCODE"), 
                           list(~replace(.,COLONYLENGTH >4.5, "AAAA")));View(tmp)
tmp<-tmp %>% mutate_at(.vars = c("S_ORDER", "COLONYID","TAXONNAME"), 
                       list(~replace(.,TAXONCODE=="AAAA", NA)));View(tmp)
jwd.all<-tmp %>% mutate_at(.vars = c("COLONYLENGTH"), 
                           list(~replace(.,TAXONCODE=="AAAA", 0)));View(jwd.all)

jwd.all <- subset(jwd.all, COLONYLENGTH >1 & COLONYLENGTH < 4.5)

##juv size class
hist.j <- ggplot(na.omit(jwd.all), aes(x = COLONYLENGTH))+
  facet_wrap(~METHOD)+
  geom_histogram(bins = 10)+
  #geom_density()+
  theme_classic()

ggsave(plot<-hist.j,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/JuvColsize.png",width=10,height=8)

genus  <- unique(doms.order$GENUS_CODE)
plotlist <- list()
plotlist[[1]] <- ggplot(na.omit(awd.all[,1:30]), aes(x = COLONYLENGTH, color = METHOD))+
  geom_density(aes(y=after_stat(count)))+
  theme_classic()+
  theme(legend.position="none")+
  ggtitle(paste(genus[1], "SfM_n = ", doms$counts[doms$GENUS_CODE == genus[1] & doms$METHOD == "SfM"],"Diver_n = ", doms$counts[doms$GENUS_CODE == genus[1] & doms$METHOD == "DIVER"]))+theme(plot.title = element_text(face = "italic"))

for (i in 2:length(genus)) {
  hist <- ggplot(na.omit(awd.all[awd.all$GENUS_CODE == genus[i],1:30]), aes(x = COLONYLENGTH, color = METHOD))+
    #facet_wrap(~METHOD)+
    #geom_histogram(binwidth = 5, fill = NA)+
    geom_density(aes(y=after_stat(count)))+
    theme_classic()+
    theme(legend.position="none")+
    ggtitle(paste(genus[i], "SfM_n = ", doms$counts[doms$GENUS_CODE == genus[i] & doms$METHOD == "SfM"],"Diver_n = ", doms$counts[doms$GENUS_CODE == genus[i] & doms$METHOD == "DIVER"]))+theme(plot.title = element_text(face = "italic"))
  plotlist[[i]]<-hist
}#i
allplots<-grid.arrange(grobs = plotlist,nrow=7,ncol=7)
ggsave(plot<-allplots,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/AdColsize_All.png",width=22,height=15)

#Create a look up table of all of the colony attributes- you will need this for the functions below
SURVEY_COL<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony<-unique(awd.all[,SURVEY_COL])

SURVEY_SITE<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN","HABITAT_CODE", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")
survey_site<-unique(awd.all[,SURVEY_SITE])

SURVEY<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN","HABITAT_CODE", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","METHOD","TRANSECT","SEGMENT")
survey_segment<-unique(awd.all[,SURVEY])

nrow(survey_site)

#TEMPORARY WORK AROUND-ASK MICHAEL TO FIX
#survey_site$REEF_ZONE<-ifelse(survey_site$SITE=="HAW-04285","Forereef",as.character(survey_site$REEF_ZONE))
#survey_segment$REEF_ZONE<-ifelse(survey_segment$SITE=="HAW-04285","Forereef",as.character(survey_segment$REEF_ZONE))
colnames(awd.all)[colnames(awd.all)=="FRAGMENT"]<-"Fragment"
colnames(jwd.all)[colnames(jwd.all)=="FRAGMENT"]<-"Fragment"

# GENERATE SUMMARY METRICS at the Segment-leveL BY GENUS--------------------------------------------------

#Calc_ColDen_Transect
acd.gen<-Calc_ColDen_Transect(data = awd.all,grouping_field = "GENUS_CODE");colnames(acd.gen)[colnames(acd.gen)=="ColCount"]<-"AdColCount";colnames(acd.gen)[colnames(acd.gen)=="ColDen"]<-"AdColDen";colnames(acd.gen)[colnames(acd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_ad"# calculate density at genus level as well as total
jcd.gen<-Calc_ColDen_Transect(jwd.all,"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen";colnames(jcd.gen)[colnames(jcd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_j"


## This function calculates mean colony length, % recent dead, % old dead, condition severity or condition extent to the segment level
## NOTE: can run both adult & juvenile data with this function for COLONYLENGTH
#c("COLONYLENGTH","RDEXTENT1", "RDEXTENT2", "RDEXTENT3", "OLDDEAD","SEVERITY_1","SEVERITY_2", "SEVERITY_3", "EXTENT_1", "EXTENT_2", "EXTENT_3")
ex_b<-subset(awd.all,EX_BOUND==0)
cl.gen<-Calc_ColMetric_Transect(data = ex_b,grouping_field = "GENUS_CODE",pool_fields = "COLONYLENGTH"); colnames(cl.gen)[colnames(cl.gen)=="Ave.y"]<-"Ave.size" #Average % old dead
od.gen<-Calc_ColMetric_Transect(data = ex_b,grouping_field = "GENUS_CODE",pool_fields = "OLDDEAD"); colnames(od.gen)[colnames(od.gen)=="Ave.y"]<-"Ave.od" #Average % old dead
rd.gen<-Calc_ColMetric_Transect(data = ex_b,grouping_field = "GENUS_CODE",pool_fields = c("RDEXTENT1", "RDEXTENT2","RDEXTENT3")); colnames(rd.gen)[colnames(rd.gen)=="Ave.y"]<-"Ave.rd" #Average % recent dead


#Calc_RDden_Transect
awd.all$RD1[which(awd.all$RD1=="")]<- "NONE"
awd.all$RDEXTENT1[which(awd.all$RD1=="NONE")]<-0
awd.all$RD2[which(awd.all$RD2=="")]<- "NONE"
awd.all$RDEXTENT2[which(awd.all$RD2=="NONE")]<-0
awd.all$RD3[which(awd.all$RD3=="")]<- "NONE"
awd.all$RDEXTENT3[which(awd.all$RD3=="NONE")]<-0
awd.all$RD3[which(is.na(awd.all$RD3))]<- "NONE"
awd.all$SEGWIDTH[which(is.na(awd.all$SEGWIDTH))] <- 1
awd.all$CONDITION_1[which(awd.all$CONDITION_1=="")]<- "NONE"
awd.all$EXTENT_1[which(awd.all$CONDITION_1=="NONE")]<-0
awd.all$CONDITION_2[which(awd.all$CONDITION_2=="")]<- "NONE"
awd.all$EXTENT_2[which(awd.all$CONDITION_2=="NONE")]<-0
awd.all$CONDITION_3[which(awd.all$CONDITION_3=="")]<- "NONE"
awd.all$EXTENT_3[which(awd.all$CONDITION_3=="NONE")]<-0

rdden.gen<-Calc_RDden_Transect(data=awd.all,grouping_field ="GENUS_CODE") # Density of recent dead colonies by condition, you will need to subset which ever condition you want. The codes ending in "S" are the general categories
acutedz.gen<-subset(rdden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,GENUS_CODE,DZGN_G));colnames(acutedz.gen)[colnames(acutedz.gen)=="DZGN_G"]<-"AcuteDZ" #subset just acute diseased colonies
pred.gen <- subset(rdden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,GENUS_CODE,PRED_G));colnames(pred.gen)[colnames(pred.gen)=="PRED_G"]<-"Predation"

#Calc_CONDden_Transect
condden.gen<-Calc_CONDden_Transect(data=awd.all,grouping_field ="GENUS_CODE")# Density of condition colonies by condition, you will need to subset which ever condition you want
ble.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,GENUS_CODE,BLE));colnames(ble.gen)[colnames(ble.gen)=="BLE"]<-"BLE" #subset just bleached colonies
chronicdz.gen<-subset(condden.gen,select = c(METHOD,SITEVISITID,SITE,TRANSECT,GENUS_CODE,CHRO));colnames(chronicdz.gen)[colnames(chronicdz.gen)=="CHRO"]<-"ChronicDZ"


#Join density and partial moratlity data together.You will need to replace the DUMMY field with the one you want
data.gen <- join_all(list(acd.gen,jcd.gen,cl.gen,od.gen,rd.gen,pred.gen, acutedz.gen,chronicdz.gen,ble.gen), 
                by=c("METHOD","SITE","SITEVISITID","TRANSECT","GENUS_CODE"), type='full')
head(data.gen)
length(unique(data.gen$SITE))

#Change NAs for abunanance and density metrics to 0. Don't change NAs in the partial mortality columns to 0
data.gen$JuvColCount[is.na(data.gen$JuvColCount)]<-0;data.gen$JuvColDen[is.na(data.gen$JuvColDen)]<-0
data.gen$AdColCount[is.na(data.gen$AdColCount)]<-0;data.gen$AdColDen[is.na(data.gen$AdColDen)]<-0


#Calculate transect level prevalence for acute dz, chronic dz and bleaching
data.gen$AcuteDZ_prev<-(data.gen$AcuteDZ*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100
data.gen$BLE_prev<-(data.gen$BLE*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100
data.gen$ChronicDZ_prev<-(data.gen$ChronicDZ*data.gen$TRANSECTAREA_ad)/data.gen$AdColCount*100

View(data.gen)

#Add adult and juvenile pres/ab columns
data.gen$Adpres.abs<-ifelse(data.gen$AdColDen>0,1,0)
data.gen$Juvpres.abs<-ifelse(data.gen$JuvColDen>0,1,0)

ad.transect <- unique(data.gen[c("SITE","TRANSECTAREA_ad", "METHOD")])
ad.transect <- na.omit(ad.transect)

test <- left_join(data.gen, ad.transect, by = c("METHOD", "SITE"))
test$TRANSECTAREA_ad.x <- test$TRANSECTAREA_ad.y
test <- test[,-24]
colnames(test)[colnames(test)=="TRANSECTAREA_ad.x"]<-"TRANSECTAREA_ad"

j.transect <- unique(data.gen[c("SITE","TRANSECTAREA_j", "METHOD")])
j.transect <- na.omit(j.transect)

test <- left_join(test, j.transect, by = c("METHOD", "SITE"))
test$TRANSECTAREA_j.x <- test$TRANSECTAREA_j.y
test <- test[,-24]
colnames(test)[colnames(test)=="TRANSECTAREA_j.x"]<-"TRANSECTAREA_j"

data.gen <- test

#We only surveyed 1 transect/site so data.gen is site-level data
if(length(unique(data.gen$TRANSECT))>1) {cat("WARNING:MORE THAN 1 TRANSECT/SITE IN DF")} #Check that adult data weren't dropped  
site.data.gen2<-dplyr::select(data.gen,-(TRANSECT))


#Calc_Diversity_Transect - for taxoncode includes both genus and species
acd.div_G<-Calc_Diversity_Transect(awd.all,"GENUS_CODE");colnames(acd.div_G)[4:7] <- paste("Adult", colnames(acd.div_G[,c(4:7)]), sep = "_")
jcd.div_G<-Calc_Diversity_Transect(jwd.all,"GENUS_CODE");colnames(jcd.div_G)[4:7] <- paste("Juv", colnames(jcd.div_G[,c(4:7)]), sep = "_")
all.div_G<-left_join(acd.div_G,jcd.div_G);View(all.div_G) #Combine adult and juvenile diversity dfs

#Calc_Diversity_Transect- at just species level
taxa<-read.csv("T:/Benthic/Data/Lookup Tables/2013-23_Taxa_MASTER.csv")
taxa$TAXONCODE<-taxa$SPCODE
h22tax<-subset(taxa,OBS_YEAR=="2023")

#Merge data with taxa list to identify taxon group
awd.all$OBS_YEAR<-as.factor(awd.all$OBS_YEAR)
tmpA<-merge(awd.all,h22tax, by= c("REGION","OBS_YEAR","TAXONCODE"))

jwd.all$OBS_YEAR<-as.factor(jwd.all$OBS_YEAR)
tmpJ<-merge(jwd.all,h22tax, by= c("REGION","OBS_YEAR","TAXONCODE"))

acd.divS<-Calc_Diversity_Transect(awd.all,"TAXONCODE");colnames(acd.divS)[4:7] <- paste("Adult", colnames(acd.divS[,c(4:7)]), sep = "_")
jcd.divS<-Calc_Diversity_Transect(jwd.all,"TAXONCODE");colnames(jcd.divS)[4:7] <- paste("Juv", colnames(jcd.divS[,c(4:7)]), sep = "_")
all.divS<-left_join(acd.divS,jcd.divS);View(all.divS) #Combine adult and juvenile diversity dfs


# Calculate the Ratio of genus to species for adults and juvs at the site level
spgen<-ddply(tmpA,.(METHOD,SITEVISITID,SITE,TAXAGROUP),summarize,
             n=length(TAXAGROUP))

spgenW_Adult<-dcast(spgen, formula=METHOD+ SITE + SITEVISITID ~ TAXAGROUP, value.var="n",fill=0)
spgenW_Adult$GENSPratio_Adult<-spgenW_Adult$GENUS/spgenW_Adult$SPECIES
spgenW_Adult$logGENSPratio_Adult<-log(spgenW_Adult$GENSPratio_Adult)
spgenW_Adult<-subset(spgenW_Adult, select= -c(GENUS,SPECIES))

#Juvs
spgen<-ddply(tmpJ,.(METHOD,SITEVISITID,SITE,TAXAGROUP),summarize,
             n=length(TAXAGROUP))

spgenW_Juv<-dcast(spgen, formula=METHOD+ SITE + SITEVISITID ~ TAXAGROUP, value.var="n",fill=0)
spgenW_Juv$GENSPratio_Juv<-spgenW_Juv$GENUS/spgenW_Juv$SPECIES
spgenW_Juv$logGENSPratio_Juv<-log(spgenW_Juv$GENSPratio_Juv)
spgenW_Juv<-subset(spgenW_Juv, select= -c(GENUS,SPECIES))


#Merge site data with metadata
# Merge Site level data with sectors file and export site data ------------
#sectors<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)

#Merge together survey meta data and sector area files and check for missmatches 
meta<-left_join(survey_site,sectors)
meta[which(is.na(meta$AREA_HA)),]
nrow(survey_site)
nrow(meta)


#Merge site level data and meta data
site.data.gen2<-left_join(site.data.gen2,meta);head(site.data.gen2)
all.diversity_G<-left_join(all.div_G,meta);head(all.diversity_G)
all.diversity_G<-left_join(all.diversity_G,spgenW_Adult)
all.diversity_G<-left_join(all.diversity_G,spgenW_Juv)
is.na(all.diversity_G) <- sapply(all.diversity_G, is.infinite) #Change infinite values to NAs

all.diversityS<-left_join(all.divS,meta);head(all.diversityS)
all.diversityS<-left_join(all.diversityS,spgenW_Adult)
all.diversityS<-left_join(all.diversityS,spgenW_Juv)
is.na(all.diversityS) <- sapply(all.diversityS, is.infinite) #Change infinite values to NAs


#Set ANALYSIS_SCHEMA to STRATA and DOMAIN_SCHEMA to whatever the highest level you want estimates for (e.g. sector, island, region)
site.data.gen2$DB_RZ<-paste(site.data.gen2$DEPTH_BIN,site.data.gen2$REEF_ZONE,sep="_")
site.data.gen2$STRATANAME<-paste(site.data.gen2$SEC_NAME,site.data.gen2$DB_RZ,sep="_")
site.data.gen2$ANALYSIS_SCHEMA<-site.data.gen2$STRATANAME
site.data.gen2$DOMAIN_SCHEMA<-site.data.gen2$SEC_NAME
site.data.gen2$ANALYSIS_YEAR<-site.data.gen2$OBS_YEAR



# #Save file for method comparsion
write.csv(site.data.gen2,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/ASRAMP23_GENUS_SITE_Juvs.csv",row.names = F)
write.csv(data.gen,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/ASRAMP23_GENUS_SEGMENT.csv",row.names = F)
write.csv(all.diversity_G,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/ASRAMP23_DIVERSITY_SITE_Genus.csv",row.names = F)
write.csv(all.diversityS,file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/ASRAMP23_DIVERSITY_SPECIES_SITE.csv",row.names = F)



