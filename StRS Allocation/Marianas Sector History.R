
rm(list=ls())
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")

survey_master<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")

marB<-subset(survey_master,REGION=="MARIAN"&OBS_YEAR>=2014& Benthic==1)
head(marB)
table(marB$OBS_YEAR)

marF<-subset(survey_master,REGION=="MARIAN"&OBS_YEAR>=2010& Fish==1)
head(marF)
table(marF$OBS_YEAR)

sec_B<-ddply(marB,.(OBS_YEAR,ISLAND,SEC_NAME),summarize,N=sum(Benthic))
sec_B$OBS_YEAR<-paste(sec_B$OBS_YEAR,"Benthic",sep="_")
sec_F<-ddply(marF,.(OBS_YEAR,ISLAND,SEC_NAME),summarize,N=sum(Fish))
sec_F$OBS_YEAR<-paste(sec_F$OBS_YEAR,"Fish",sep="_")

sec_table<-rbind(sec_B,sec_F)
View(sec_table)

sec_tableW<-dcast(sec_table, formula=ISLAND+SEC_NAME ~ OBS_YEAR, value.var="N",fill=0)
View(sec_tableW)

write.csv(sec_tableW,file="T:/Cruise/CruisePreps/2020/RA-20-01 (MARAMP)/Team Plans/2011-2017_Marianas_allocationbysector.csv")

