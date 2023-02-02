####This script combines historical REA data for Hawaii using Methods A, C and E datasets
# These data were pulled from T:\DataManagement\NODC Archive Packages\Coral Demographics (pre-NCRMP)\Data\Excel &
# T:\DataManagement\NODC Archive Packages\Coral Demographics\Data\Excel

#This script only includes code to calcuate temporal patterns in 
#adult colony density and partial mortality, I haven't included the code for juveniles (method F)yet



rm(list=ls())
#Load raw data
setwd("C:/Users/courtney.s.couch/Documents/Courtney's Files 102122/R Files/ESD/ReportCard")#set directory
a<- read.csv("Data/HistoricalREA_V0_CORAL_OBS_A.csv")

library(ggplot2)
require(gridExtra)
library(reshape2)
library(plyr)
library(grid)
library(data.table) 
library(tidyr)


###FUNCTIONS
maxrange<-function(e) {max(e)-min(e)}### Don't use this because there are several taxa that have a very small range
se<-function(e) {sd(e)/sqrt(length(e))}

##Create a function to generate theme in gFgplot
theme_my <- theme_bw() + theme(
  axis.line        = element_line(colour = "black"),
  panel.grid.major = element_blank(), 
  axis.title= element_text(face="bold"),
  panel.grid.minor = element_blank(),
  strip.background = element_blank(),
  legend.key       = element_blank()
)


##Create a function to generate theme in ggplot
theme_my2 <-  theme_bw()+theme(axis.title.x=element_blank(),
                               axis.text.x=element_blank(),
                               axis.ticks.x=element_blank(),
                               axis.title= element_text(face="bold"),
                               plot.title = element_text(size = 11),legend.key=element_blank(),
                               legend.title=element_blank(),panel.grid=element_blank(),legend.position = "none")


# HOUSEKEEPING ------------------------------------------------------------
DATA_COLS<-c("REGION","ISLANDCODE","DEPTH_BIN","OBS_YEAR","SITE","TRANSECT","TRANWIDTH","TRANLENGTH","TAXONCODE","SIZECLASS","COUNT","S_ORDER")
head(a[,DATA_COLS])
a<-a[,DATA_COLS]

a<-subset(a,REGION %in% c("MHI","NWHI"))
a<-subset(a,S_ORDER=="Scleractinia")
a<-subset(a,DEPTH_BIN=="Mid")
head(a)

###Create new column for island group
a$ISLANDGROUP<-NA
a$ISLANDGROUP<-as.character(a$ISLANDGROUP)
for (i in 1:length(a$ISLANDCODE)){ #opening brace
  
  if(a$ISLANDCODE[i] =="HAW"){ #c&p
    a[i,13] = "HAW" #c&p
  } #c&p
  if(a$ISLANDCODE[i] =="OAH"){ #c&p
    a[i,13] = "OAH" #c&p
  } #c&p
  if(a$ISLANDCODE[i] =="MOL"|a$ISLANDCODE[i] =="MAI"|a$ISLANDCODE[i] =="LAN"){ #c&p
    a[i,13] = "MAUINUI" #c&p
  } #c&p
  if(a$ISLANDCODE[i] =="KAU"|a$ISLANDCODE[i] =="NII"){ #c&p
    a[i,13] = "KAUNII" #c&p
  } #c&p
  if(a$ISLANDCODE[i] =="KUR"){ #c&p
    a[i,13] = "KUR" #c&p
  } #c&p
  if(a$ISLANDCODE[i] =="MID"){ #c&p
    a[i,13] = "MID" #c&p
  } #c&p
  if(a$ISLANDCODE[i] =="PHR"){ #c&p
    a[i,13] = "PHR" #c&p
  } #c&p
  if(a$ISLANDCODE[i] =="LIS"){ #c&p
    a[i,13] = "LIS" #c&p
  } #c&p
  if(a$ISLANDCODE[i] =="LAY"){ #c&p
    a[i,13] = "LAY" #c&p
  } #c&p
  if(a$ISLANDCODE[i] =="MAR"){ #c&p
    a[i,13] = "MAR" #c&p
  } #c&p
  if(a$ISLANDCODE[i] =="FFS"){ #c&p
    a[i,13] = "FFS" #c&p
  } #c&p
  
} #closing curly brace for entire forloop

levels(as.factor(a$ISLANDGROUP))

a<-subset(a,ISLANDGROUP!="NA")



a$TRANAREA<-a$TRANLENGTH*a$TRANWIDTH
nrow(a)
a<-subset(a,TRANAREA>=5)
nrow(a)
head(a)

###Add new column for adult vs. juv
###Create new column for island group
a$SIZEGROUP<-NA
a$SIZEGROUP<-as.character(a$SIZEGROUP)
for (i in 1:length(a$SIZECLASS)){ #opening brace
  
  if(a$SIZECLASS[i] =="0-5 cm"){ #c&p
    a[i,15] = "JUVENILE" #c&p
  } #c&p
  if(a$SIZECLASS[i] !="0-5 cm"){ #c&p
    a[i,15] = "ADULT" #c&p
  } #c&p
} #closing curly brace for entire forloop

head(a)
levels(as.factor(a$SIZEGROUP))



####Calculate total Sclerictinan abundance of adults and juvs by ISLANDCODE, and sizegroup
###NOTE: There were no transects with 0 colonies- make sure to include 0 values if you use this on another dataset
abun_sum<-ddply(a, .(REGION,ISLANDCODE,OBS_YEAR,SITE,TRANSECT,TRANAREA,SIZEGROUP),
          summarise,
          ABUN=sum(COUNT))

abun_sum$DENSITY<-abun_sum$ABUN/abun_sum$TRANAREA
head(abun_sum)

den_summary<-ddply(abun_sum, .(REGION,ISLANDCODE,OBS_YEAR,SIZEGROUP),
                summarise,
                mean_den=mean(DENSITY),
                se_den=se(DENSITY))

write.csv(den_summary,"HI_HistoricalSummary_A.csv")



# Summarizing dataset C- 2008-2012 ----------------------------------------

################
#####
a <- read.csv("Data/HistoricalREA_V0_CORAL_OBS_C.csv")

head(a)

#NA values for depth bin- need to look into, but now change to mid depth
a$DEPTH_BIN<-ifelse(a$DEPTH_BIN=="","Mid",as.character(a$DEPTH_BIN))
View(a)

# HOUSEKEEPING ------------------------------------------------------------
DATA_COLS<-c("REGION","ISLANDCODE","DEPTH_BIN","OBS_YEAR","SITE","TRANSECT","SEGMENT","SEGWIDTH","SEGLENGTH","OLDDEAD","TAXONCODE","COLONYLENGTH","S_ORDER")
head(a[,DATA_COLS])
a<-a[,DATA_COLS]

a<-subset(a,REGION %in% c("MHI"))
a<-subset(a,DEPTH_BIN=="Mid")
a<-subset(a,SEGLENGTH!="NA") #Remove segments that were not surveyed for coral dem
head(a)
tail(a)


###Add new column COUNT (This will allow us to include sites that didn't have any colonies)
ncol(a)
a$COUNT<-NA
a$COUNT<-as.character(a$COUNT)
for (i in 1:length(a$S_ORDER)){ #opening brace
  
  if(a$S_ORDER[i] =="Scleractinia"){ #c&p
    a[i,c("COUNT")] = "1" #c&p
  } #c&p
  if(a$S_ORDER[i] !="Scleractinia"){ #c&p
    a[i,c("COUNT")] = "0" #c&p
  } #c&p
} #closing curly brace for entire forloop

head(a)
levels(as.numeric(a$COUNT))


#Add column for adult v juveniles
a <- a %>%
    mutate(AdJuv = case_when(
      COLONYLENGTH < 5 ~ "Juv",
      COLONYLENGTH >= 5~ "Ad",
      COLONYLENGTH ==NA ~ "NA"))

a$SEGAREA<-a$SEGLENGTH*a$SEGWIDTH # Calculate segment area

#Calculate total transect area then merge back to a dataframe
S.df<-ddply(a, .(REGION,ISLANDCODE,OBS_YEAR,SITE,TRANSECT,SEGMENT,AdJuv),
             summarise,
             SEGAREA=unique(SEGAREA))
TR.DF<-ddply(S.df, .(REGION,ISLANDCODE,OBS_YEAR,SITE,TRANSECT,AdJuv),
             summarise,
             TRANAREA=sum(SEGAREA))

new.df<-merge(a,TR.DF, by=c("REGION","ISLANDCODE","OBS_YEAR","SITE","TRANSECT","AdJuv"),all=TRUE)
str(new.df)
head(new.df)
nrow(new.df)

SURVEY_SITE<-c("SITE","TRANSECT","TRANAREA","AdJuv")
survey_site<-unique(new.df[,SURVEY_SITE])
View(survey_site)

new.df<-subset(new.df,TRANAREA>=5) #Remove transects with less than 5m surveyed and check how many rows were removed
nrow(new.df)
head(new.df)

# ##Create new df for partial mortality
# summary(new.df)
# pm<-subset(new.df,OLDDEAD>=0&OLDDEAD!="NA") #remove rows that old dead wasn't recorded and transects with no coral
# pm<-subset(pm,COLONYLENGTH>5)
# summary(pm$OLDDEAD)
# levels(pm$OBS_YEAR)
# 
# pm$OLDDEAD<-as.numeric(pm$OLDDEAD)
# pm_sum<-ddply(pm, .(REGION,ISLANDCODE,OBS_YEAR,SITE,TRANSECT),
#               summarise,
#               partmort=mean(OLDDEAD))
# head(pm_sum)
# levels(pm_sum$OBS_YEAR)
# 
# pm_sum$partmort<-as.numeric(pm_sum$partmort)
# 
# pm_sum2<-ddply(pm_sum, .(REGION,ISLANDCODE,OBS_YEAR),
#                summarise,
#                mean_pm=mean(partmort),
#                se_pm=se(partmort))
# levels(pm_sum2$OBS_YEAR)

###Create new dfs foR adults and make sure we include sites that didn't have colonies
new.df$COUNT<-as.numeric(new.df$COUNT)


####Calculate total Sclerictinan abundance of adults and juvs by ISLANDCODE, and sizegroup
abun_sum<-ddply(new.df, .(REGION,ISLANDCODE,OBS_YEAR,SITE,TRANSECT,TRANAREA,AdJuv),
                      summarise,
                      abun=sum(COUNT))

abun_sum$DENSITY<-abun_sum$abun/abun_sum$TRANAREA
head(abun_sum)

#Make sure 0 values are in both adult and juv datasets- this is a very clunky way to code this, but it works
tmpJ<-subset(abun_sum,DENSITY==0)
tmpJ$AdJuv<-"Juv"
tmpA<-subset(abun_sum,DENSITY==0)
tmpA$AdJuv<-"Ad"
abun_sum<-subset(abun_sum,DENSITY>0)
abun_sum<-rbind(abun_sum,tmpJ,tmpA)
View(abun_sum)

den_site_summary<-ddply(abun_sum, .(REGION,ISLANDCODE,OBS_YEAR,SITE,AdJuv),
                        summarise,
                        Mean_ColonyDensity=mean(DENSITY))

den_site_summary$OLD_SITE<-den_site_summary$SITE


#merge survey master file

survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")


#Use SM coordinates-some coordinates are wrong in data and need to be updated
colnames(survey_master)[colnames(survey_master)=="LATITUDE_LOV"]<-"LATITUDE" #Change column name- we will eventually change this column back to "taxoncode" after we modify the spcode names to match the taxalist we all feel comfortable identifying
colnames(survey_master)[colnames(survey_master)=="LONGITUDE_LOV"]<-"LONGITUDE" #Change column name- we will eventually change this column back to "taxoncode" after we modify the spcode names to match the taxalist we all feel comfortable identifying


#merge 'em NOTE: left-join will spit out a Warning message that you are joining on factors that have different levels. Basically you have more sites in survey master than x. This is correct and can be ignored here.
den_site_summary<-left_join(den_site_summary, survey_master[,c("OBS_YEAR","OLD_SITE","DEPTH_BIN","LATITUDE","LONGITUDE","SEC_NAME","new_MIN_DEPTH_M","new_MAX_DEPTH_M")])


colnames(den_site_summary)[colnames(den_site_summary)=="new_MIN_DEPTH_M"]<-"MIN_DEPTH_M" #Change column name
colnames(den_site_summary)[colnames(den_site_summary)=="new_MAX_DEPTH_M"]<-"MAX_DEPTH_M" #Change column name

den_site_summary<-subset(den_site_summary,select = -c(OLD_SITE))
View(den_site_summary)

write.csv(den_site_summary,"MHI_ColonyDensity_site_2008-2010.csv")


write.csv(adultden_summary,"HI_HistoricalSummary_C_adultden.csv")
write.csv(pm_sum2,"HI_HistoricalSummary_C_PM.csv")


# Summarizing dataset E- 2013 and 2016 ----------------------------------------

################
a <- read.csv("HistoricalREA_V0_CORAL_OBS_E.csv")

a$OBS_YEAR<-as.factor(a$OBS_YEAR)

sapply(a,levels)
summary(a)

library(ggplot2)
require(gridExtra)
library(reshape2)
library(plyr)
library(grid)
library(data.table) 
library(tidyr)


###FUNCTIONS
maxrange<-function(e) {max(e)-min(e)}### Don't use this because there are several taxa that have a very small range
se<-function(e) {sd(e)/sqrt(length(e))}

##Create a function to generate theme in gFgplot
theme_my <- theme_bw() + theme(
  axis.line        = element_line(colour = "black"),
  panel.grid.major = element_blank(), 
  axis.title= element_text(face="bold"),
  panel.grid.minor = element_blank(),
  strip.background = element_blank(),
  legend.key       = element_blank()
)


##Create a function to generate theme in ggplot
theme_my2 <-  theme_bw()+theme(axis.title.x=element_blank(),
                               axis.text.x=element_blank(),
                               axis.ticks.x=element_blank(),
                               axis.title= element_text(face="bold"),
                               plot.title = element_text(size = 11),legend.key=element_blank(),
                               legend.title=element_blank(),panel.grid=element_blank(),legend.position = "none")


# HOUSEKEEPING ------------------------------------------------------------
DATA_COLS<-c("REGION","ISLANDCODE","DEPTH_BIN","OBS_YEAR","SITE","TRANSECT","SEGMENT","SEGWIDTH","SEGLENGTH","OLDDEAD","TAXONCODE","COLONYLENGTH","S_ORDER")
head(a[,DATA_COLS])
a<-a[,DATA_COLS]

a<-subset(a,REGION %in% c("MHI"))
a<-subset(a,DEPTH_BIN=="Mid")
a<-subset(a,SEGLENGTH!="NA") #Remove segments that were not surveyed for coral dem
head(a)
tail(a)
sapply(a,levels)
summary(a)

###Create new column for island group
a$ISLANDGROUP<-NA
a$ISLANDGROUP<-as.character(a$ISLANDGROUP)
for (i in 1:length(a$ISLANDCODE)){ #opening brace
  
  if(a$ISLANDCODE[i] =="HAW"){ #c&p
    a[i,c("ISLANDGROUP")] = "HAW" #c&p
  } #c&p
  if(a$ISLANDCODE[i] =="OAH"){ #c&p
    a[i,c("ISLANDGROUP")] = "OAH" #c&p
  } #c&p
  if(a$ISLANDCODE[i] =="MOL"|a$ISLANDCODE[i] =="MAI"|a$ISLANDCODE[i] =="LAN"){ #c&p
    a[i,c("ISLANDGROUP")] = "MAUINUI" #c&p
  } #c&p
  if(a$ISLANDCODE[i] =="KAU"|a$ISLANDCODE[i] =="NII"){ #c&p
    a[i,c("ISLANDGROUP")] = "KAUNII" #c&p
  } #c&p
  if(a$ISLANDCODE[i] =="KUR"){ #c&p
    a[i,c("ISLANDGROUP")] = "KUR" #c&p
  } #c&p
  if(a$ISLANDCODE[i] =="MID"){ #c&p
    a[i,c("ISLANDGROUP")] = "MID" #c&p
  } #c&p
  if(a$ISLANDCODE[i] =="PHR"){ #c&p
    a[i,c("ISLANDGROUP")] = "PHR" #c&p
  } #c&p
  if(a$ISLANDCODE[i] =="LIS"){ #c&p
    a[i,c("ISLANDGROUP")] = "LIS" #c&p
  } #c&p
  if(a$ISLANDCODE[i] =="LAY"){ #c&p
    a[i,c("ISLANDGROUP")] = "LAY" #c&p
  } #c&p
  if(a$ISLANDCODE[i] =="MAR"){ #c&p
    a[i,c("ISLANDGROUP")] = "MAR" #c&p
  } #c&p
  if(a$ISLANDCODE[i] =="FFS"){ #c&p
    a[i,c("ISLANDGROUP")] = "FFS" #c&p
  } #c&p
  
} #closing curly brace for entire forloop

levels(as.factor(a$ISLANDGROUP))

a<-subset(a,ISLANDGROUP!="NA")

###Add new column COUNT (This will allow us to include sites that didn't have any colonies)
ncol(a)
a$COUNT<-NA
a$COUNT<-as.character(a$COUNT)
for (i in 1:length(a$S_ORDER)){ #opening brace
  
  if(a$S_ORDER[i] =="Scleractinia"){ #c&p
    a[i,c("COUNT")] = "1" #c&p
  } #c&p
  if(a$S_ORDER[i] !="Scleractinia"){ #c&p
    a[i,c("COUNT")] = "0" #c&p
  } #c&p
} #closing curly brace for entire forloop

head(a)
levels(as.numeric(a$COUNT))


a$SEGAREA<-a$SEGLENGTH*a$SEGWIDTH # Calculate segment area

#Calculate total transect area then merge back to a dataframe
S.df<-ddply(a, .(REGION,ISLANDCODE,OBS_YEAR,SITE,TRANSECT,SEGMENT),
            summarise,
            SEGAREA=unique(SEGAREA))
TR.DF<-ddply(S.df, .(REGION,ISLANDCODE,OBS_YEAR,SITE,TRANSECT),
             summarise,
             TRANAREA=sum(SEGAREA))

new.df<-merge(a,TR.DF, by=c("REGION","ISLANDCODE","OBS_YEAR","SITE","TRANSECT"),all=TRUE)
sapply(new.df,levels)
head(new.df)
nrow(new.df)

new.df<-subset(new.df,TRANAREA>=5) #Remove transects with less than 5m surveyed and check how many rows were removed
nrow(new.df)
head(new.df)
levels(new.df$OBS_YEAR)

##Create new df for partial mortality
summary(new.df)
pm<-subset(new.df,OLDDEAD>=0&OLDDEAD!="NA") #remove rows that old dead wasn't recorded and transects with no coral
pm<-subset(pm,COLONYLENGTH>5)
summary(pm$OLDDEAD)
levels(pm$OBS_YEAR)

pm$OLDDEAD<-as.numeric(pm$OLDDEAD)
pm_sum<-ddply(pm, .(REGION,ISLANDCODE,OBS_YEAR,SITE,TRANSECT),
                      summarise,
                      partmort=mean(OLDDEAD))
head(pm_sum)
levels(pm_sum$OBS_YEAR)

pm_sum$partmort<-as.numeric(pm_sum$partmort)

pm_sum2<-ddply(pm_sum, .(REGION,ISLANDCODE,OBS_YEAR),
                        summarise,
                        mean_pm=mean(partmort),
                        se_pm=se(partmort))
levels(pm_sum2$OBS_YEAR)


###Create new dfs foR adults and make sure we include sites that didn't have colonies
new.df$COUNT<-as.numeric(new.df$COUNT)
adult<-subset(new.df,COLONYLENGTH>5|TAXONCODE=="AAAA")
summary(adult)

####Calculate total Sclerictinan abundance of adults and juvs by ISLANDCODE, and sizegroup
#add up all adults
adult_abun_sum<-ddply(adult, .(REGION,ISLANDCODE,OBS_YEAR,SITE,TRANSECT,TRANAREA),
                      summarise,
                      abun=sum(COUNT))

adult_abun_sum$DENSITY<-adult_abun_sum$abun/adult_abun_sum$TRANAREA
head(adult_abun_sum)

adultden_summary<-ddply(adult_abun_sum, .(REGION,ISLANDCODE,OBS_YEAR),
                        summarise,
                        mean_den=mean(DENSITY),
                        se_den=se(DENSITY))


write.csv(adultden_summary,"HI_HistoricalSummary_E_adultden.csv")
write.csv(pm_sum2,"HI_HistoricalSummary_E_PM.csv")



# Combine all time series data --------------------------------------------
e <- read.csv("HI_HistoricalSummary_E_adultden.csv")
c<- read.csv("HI_HistoricalSummary_C_adultden.csv")
a<- read.csv("HI_HistoricalSummary_a.csv")

ee <- read.csv("HI_HistoricalSummary_E_PM.csv")
cc<- read.csv("HI_HistoricalSummary_C_PM.csv")

a<-subset(a,SIZEGROUP=="ADULT")
head(a)
head(c)
head(e)

a<-a[c(1,2,3,4,6,7)] #reorganize columns so we can rbind all dataframes


all<-rbind(a,c,e)
head(all)
all$OBS_YEAR<-as.numeric(all$OBS_YEAR)

mhi_adult<-subset(all,REGION=="MHI")

mhi_adult$ISLANDCODE<-factor(mhi_adult$ISLANDCODE,levels=c("NII","KAU","OAH","MOL","MAI","LAN","HAW"))

all<-rbind(ee,cc)
head(all)
all$OBS_YEAR<-as.numeric(all$OBS_YEAR)

mhi_pm<-subset(all,REGION=="MHI")

mhi_pm$ISLANDCODE<-factor(mhi_pm$ISLANDCODE,levels=c("NII","KAU","OAH","MOL","MAI","LAN","HAW"))

library(ggplot2)
require(gridExtra)
library(reshape2)
library(plyr)
library(grid)
library(data.table) 
library(tidyr)


###FUNCTIONS
maxrange<-function(e) {max(e)-min(e)}### Don't use this because there are several taxa that have a very small range
se<-function(e) {sd(e)/sqrt(length(e))}

##Create a function to generate theme in gFgplot
theme_my <- theme_bw() + theme(
  axis.line        = element_line(colour = "black"),
  panel.grid.major = element_blank(), 
  axis.title= element_text(face="bold"),
  panel.grid.minor = element_blank(),
  strip.background = element_blank(),
  legend.key       = element_blank()
)


##Create a function to generate theme in ggplot
theme_my2 <-  theme_bw()+theme(axis.title.x=element_blank(),
                               axis.text.x=element_blank(),
                               axis.ticks.x=element_blank(),
                               axis.title= element_text(face="bold"),
                               plot.title = element_text(size = 11),legend.key=element_blank(),
                               legend.title=element_blank(),panel.grid=element_blank(),legend.position = "none")



##plot PM
p<-ggplot(mhi_pm, aes(x = OBS_YEAR, y = mean_pm)) + geom_point()+geom_line(lwd=1.5) +
  geom_errorbar(aes(ymin=mean_pm-se_pm, ymax=mean_pm+se_pm), width=.1) +
  scale_x_continuous(breaks = seq(2005, 2016, by = 2)) +
  facet_wrap(~ISLANDCODE,ncol=3) + 
  labs(x="Year",y="Mean Adult Colonies/m-2")
p <- p + theme_my + theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(p)
ggsave(file="MHI_Historical_PM.pdf",width=10,height=8,p)
dev.off()

##plot just adults
p<-ggplot(mhi_adult, aes(x = OBS_YEAR, y = mean_den)) + geom_point()+geom_line(lwd=1.5) +
  geom_errorbar(aes(ymin=mean_den-se_den, ymax=mean_den+se_den), width=.1) +
  scale_x_continuous(breaks = seq(2005, 2016, by = 2)) +
  facet_wrap(~ISLANDCODE,ncol=3) + 
  labs(x="Year",y="Mean Adult Colonies/m-2")
p <- p + theme_my + theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(p)
ggsave(file="MHI_Historical_Adult.pdf",width=10,height=8,p)
dev.off()



